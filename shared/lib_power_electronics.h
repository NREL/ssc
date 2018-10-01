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
#include "lib_ondinv.h"
#include "lib_shared_inverter.h"



class BatteryBidirectionalInverter
{
public:

	BatteryBidirectionalInverter(double ac_dc_efficiency, double dc_ac_efficiency)
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

class Battery_DC_DC_ChargeController
{
public:

	Battery_DC_DC_ChargeController(double batt_dc_dc_bms_efficiency, double pv_dc_dc_mppt_efficiency)
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

class BatteryRectifier
{
public:
	// don't know if I need this component in AC or DC charge controllers
	BatteryRectifier(double ac_dc_efficiency) { _ac_dc_efficiency = 0.01 * ac_dc_efficiency; }
	double ac_dc_efficiency() { return _ac_dc_efficiency; }

	// return power loss [kW]
	double convert_to_dc(double P_ac, double * P_dc);

protected:
	double _ac_dc_efficiency;
	double _loss_dc_ac;
};

/**
*
* \class ChargeController
*
*  A ChargeController is a mostly abstract base class which defines the structure of battery charge controllers
*  The ChargeController requires information about the battery dispatch.  At every time step, the charge controller
*  runs the dispatch model and returns to the calling function.  While the ChargeController may seem to be an unnecessary
*  step in the calculation, it provides a location to store the initial dispatch in a timestep, and iterate upon that dispatch
*  if necessary.  It also houses the framework of battery power electronics components, which currently have single-point
*  efficiences, but could be expanded to have more detailed models.
*/
class ChargeController
{
public:
	/// Construct an ChargeController with a dispatch object, battery metrics object
	ChargeController(dispatch_t * dispatch, battery_metrics_t * battery_metrics);

	/// Virtual destructor for ChargeController
	virtual ~ChargeController() {};

	/// Virtual method to run the charge controller given the current timestep, PV production, and load
	virtual void run(size_t year, size_t hour_of_year, size_t step_of_hour, size_t index, double P_pv, double V_pv, double P_load, double P_clipped) = 0;

	/// The supported configurations of a battery system
	enum { DC_CONNECTED, AC_CONNECTED };

protected:

	// allocated and managed internally
	std::unique_ptr<dispatch_t> m_dispatchInitial;	/// An internally managed copy of the initial dispatch of the timestep

	// memory managed elsewhere
	BatteryPower * m_batteryPower;		/// A structure containing all of the components in the battery power flow calculations
	battery_metrics_t *m_batteryMetrics;    /// An object that tracks battery metrics for later analysis
        dispatch_t * m_dispatch;		/// An object containing the framework to run a battery and check operational constraints
};

/**
*
* \class ACBatteryController
*
*  A ACBatteryController is derived from the ChargeController, and contains information specific to a battery connected on the AC-side
*  of a power generating source.  It requires information about the efficiency to convert power from AC to DC and back to AC.
*/
class ACBatteryController : public ChargeController
{
public:
	/// Construct an ACBatteryController with a dispatch object, battery metrics object, and single-point efficiencies for the battery bidirectional inverter
	ACBatteryController(dispatch_t * dispatch, battery_metrics_t * battery_metrics, double efficiencyACToDC, double efficiencyDCToAC);

	/// Destroy the ACBatteryController
	~ACBatteryController() {};

	/// Runs the battery dispatch model with the current PV and Load information
	void run(size_t year, size_t hour_of_year, size_t step_of_hour, size_t index, double P_pv, double V_pv=0, double P_load=0, double P_clipped=0);

private:
	// allocated and managed internally
	std::unique_ptr<BatteryBidirectionalInverter> m_bidirectionalInverter;  /// Model for the battery bi-directional inverter
};

/**
*
* \class DCBatteryController
*
*  A DCBatteryController is derived from the ChargeController, and contains information specific to a battery connected on the DC-side
*  of a power generating source.  It requires information about the efficiency to convert power from DC to DC between the battery and DC source
*/
class DCBatteryController : public ChargeController
{
public:
	/// Construct a DCBatteryController with a dispatch object, battery metrics object, and single-point efficiency for the battery charge controller
	DCBatteryController(dispatch_t * dispatch, battery_metrics_t * battery_metrics, double efficiencyDCToDC);

	/// Destroy the DCBatteryController object
	~DCBatteryController() {};

	/// Sets the shared inverter used by the PV and battery system
	void setSharedInverter(SharedInverter * sharedInverter);

	/// Runs the battery dispatch model with the current PV and Load information
	void run(size_t year, size_t hour_of_year, size_t step_of_hour, size_t index, double P_pv, double V_pv, double P_load=0, double P_pv_clipped = 0);

private:
	
	// allocated and managed internally
	std::unique_ptr<Battery_DC_DC_ChargeController> m_DCDCChargeController;  /// Model for the battery DC/DC charge controller with Battery Management System

};


#endif
