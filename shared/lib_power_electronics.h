/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as “System Advisor Model” or “SAM”. Except
*  to comply with the foregoing, the terms “System Advisor Model”, “SAM”, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

#ifndef _power_electronics_h_
#define _power_electronics_h_

#include "lib_battery_dispatch.h"
#include "lib_battery_powerflow.h"
#include "lib_sandia.h"
#include "lib_pvinv.h"

class inverter
{
public:
	inverter(int inverter_type, int) {
		_inverter_type = inverter_type;
	}

	void set_sandia(sandia_inverter_t * sandia_inverter) { _sandia_inverter = sandia_inverter; }
	void set_partlaod(partload_inverter_t * partload_inverter) { _partload_inverter = partload_inverter; }


	void ac_power(double P_dc, double V_dc, double &P_ac)
	{
		double P_par, P_lr, Eff, P_cliploss, P_soloss, P_ntloss;
		if (_inverter_type == SANDIA_INVERTER)
			_sandia_inverter->acpower(P_dc, V_dc, &P_ac, &P_par, &P_lr, &Eff, &P_cliploss, &P_soloss, &P_ntloss);
		else if (_inverter_type == PARTLOAD_INVERTER)
			_partload_inverter->acpower(P_dc, &P_ac, &P_lr, &P_par, &Eff, &P_cliploss, &P_ntloss);
	}
	enum { SANDIA_INVERTER, DATASHEET_INVERTER, PARTLOAD_INVERTER, COEFFICIENT_GENERATOR, NONE };

protected:
	int _inverter_type;
	int _num_inverters;

	sandia_inverter_t * _sandia_inverter;
	partload_inverter_t * _partload_inverter;
};


class bidirectional_inverter
{
public:

	bidirectional_inverter(double ac_dc_efficiency, double dc_ac_efficiency)
	{
		_dc_ac_efficiency = 0.01*dc_ac_efficiency;
		_ac_dc_efficiency = 0.01*ac_dc_efficiency;
	}

	double dc_ac_efficiency() { return _dc_ac_efficiency; }
	double ac_dc_efficiency() { return _ac_dc_efficiency; }


	// return power loss [kW]
	double convert_to_dc(double P_ac, double * P_dc);
	double convert_to_ac(double P_dc, double * P_ac);

	// return increased power required, i.e 9 kWac may require 10 kWdc
	double compute_dc_from_ac(double P_ac);


protected:
	double _dc_ac_efficiency;
	double _ac_dc_efficiency;

	double _loss_dc_ac;
	double _loss_ac_dc;
};

class dc_dc_charge_controller
{
public:

	dc_dc_charge_controller(double batt_dc_dc_bms_efficiency, double pv_dc_dc_mppt_efficiency)
	{
		_batt_dc_dc_bms_efficiency = 0.01*batt_dc_dc_bms_efficiency;
		_pv_dc_dc_mppt_efficiency = 0.01*pv_dc_dc_mppt_efficiency;

	}

	double batt_dc_dc_bms_efficiency() { return _batt_dc_dc_bms_efficiency; };
	double pv_dc_dc_mppt_efficiency() { return _pv_dc_dc_mppt_efficiency; };


protected:
	double _batt_dc_dc_bms_efficiency;
	double _pv_dc_dc_mppt_efficiency;
	double _loss_dc_dc;
};

class rectifier
{
public:
	// don't know if I need this component in AC or DC charge controllers
	rectifier(double ac_dc_efficiency) { _ac_dc_efficiency = 0.01 * ac_dc_efficiency; }
	double ac_dc_efficiency() { return _ac_dc_efficiency; }

	// return power loss [kW]
	double convert_to_dc(double P_ac, double * P_dc);

protected:
	double _ac_dc_efficiency;
	double _loss_dc_ac;
};

class ACBatteryController
{
public:
	ACBatteryController(dispatch_t * dispatch, battery_metrics_t * battery_metrics, double efficiency_1, double efficiency_2);
	~ACBatteryController();

	void initialize(double P_pv, double P_load, double P_pv_clipped, size_t index);

	// function to determine appropriate pv and load to send to battery
	virtual void preprocess_pv_load() = 0;
	virtual void run(size_t year, size_t hour_of_year, size_t step_of_hour, size_t index, double P_pv, double P_load, double P_pv_clipped) = 0;
	virtual void compute_to_batt_load_grid(double P_battery_ac_or_dc, double P_pv_ac_or_dc, double P_load_ac, double inverter_efficiency) = 0;

	// return power loss [kW]
	virtual double gen_ac() = 0;
	virtual double update_gen_ac(double P_gen_ac) = 0;

	void finalize();

	// ac outputs
	double power_tofrom_battery() { return m_batteryPower->powerBattery; }
	double power_tofrom_grid() { return m_batteryPower->powerGrid; }
	double power_gen() { return m_batteryPower->powerGeneratedBySystem; }
	double power_pv_to_load() { return m_batteryPower->powerPVToLoad; }
	double power_battery_to_load() { return m_batteryPower->powerBatteryToLoad; }
	double power_grid_to_load() { return m_batteryPower->powerGridToLoad; }
	double power_pv_to_batt() { return m_batteryPower->powerPVToBattery; }
	double power_grid_to_batt() { return m_batteryPower->powerGridToBattery; }
	double power_pv_to_grid() { return m_batteryPower->powerPVToGrid; }
	double power_battery_to_grid() { return m_batteryPower->powerBatteryToGrid; }
	double power_conversion_loss() { return m_batteryPower->powerConversionLoss; }
	double power_system_loss() { return m_batteryPower->powerSystemLoss; }

private:
	
	// allocated internally
	std::unique_ptr<BatteryPowerFlow> m_batteryPowerFlow;
	std::shared_ptr<BatteryPower> m_batteryPower;
	std::unique_ptr<dispatch_t> m_dispatchInitial;

	// passed in
	dispatch_t * m_dispatch;
	battery_metrics_t *m_batteryMetrics;
	bidirectional_inverter * m_bidirectionalInverter;
};


#endif