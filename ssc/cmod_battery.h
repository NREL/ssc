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

#ifndef _CMOD_BATTERY_COMMON_
#define _CMOD_BATTERY_COMMON_ 1

#include "core.h"
#include "lib_power_electronics.h"
#include "lib_sandia.h"
#include "lib_pvinv.h"


extern var_info vtab_battery_inputs[];
extern var_info vtab_battery_outputs[];

struct batt_variables
{
	bool system_use_lifetime_output;
	bool en_batt;
	int analysis_period;
	int batt_chem;
	int batt_dispatch;
	int batt_voltage_choice;
	int batt_current_choice;
	int batt_meter_position;
	int batt_pv_choice;
	int batt_target_choice;
	int batt_loss_choice;
	int batt_calendar_choice;

	size_t ncharge;
	size_t ndischarge;
	size_t ndischarge_percent;
	size_t ngridcharge_percent;
	size_t ngridcharge;
	size_t nsched;
	size_t msched;

	ssc_number_t *pcharge = 0;
	ssc_number_t *pdischarge = 0;
	ssc_number_t *pdischarge_percent = 0;
	ssc_number_t *pgridcharge_percent = 0;
	ssc_number_t *pgridcharge = 0;
	ssc_number_t *psched = 0;
	ssc_number_t *psched_weekend = 0;

	util::matrix_t<float> schedule;
	util::matrix_t<double>  batt_lifetime_matrix;
	util::matrix_t<double> batt_calendar_lifetime_matrix;
	util::matrix_t<double> batt_voltage_matrix;

	std::vector<double> target_power_monthly;
	std::vector<double> target_power;

	std::vector<double> batt_losses_charging;
	std::vector<double> batt_losses_discharging;
	std::vector<double> batt_losses_idle;
	std::vector<double> batt_losses;

	int batt_computed_series;
	int batt_computed_strings;

	double batt_kw;
	double batt_kwh;

	double batt_Vnom_default;
	double batt_Vfull;
	double batt_Vexp;
	double batt_Vnom;
	double batt_Qfull;
	double batt_Qfull_flow;
	double batt_Qexp;
	double batt_Qnom;
	double batt_C_rate;
	double batt_resistance;

	double batt_replacement_capacity;
	util::matrix_t<double> cap_vs_temp;
	double batt_mass;
	double batt_length;
	double batt_width;
	double batt_height;
	double batt_Cp;
	double batt_h_to_ambient;
	double T_room;

	double LeadAcid_q20_computed;
	double LeadAcid_tn;
	double LeadAcid_qn_computed;
	double LeadAcid_q10_computed;

	double batt_initial_SOC;
	double batt_maximum_SOC;
	double batt_minimum_SOC;
	double batt_current_charge_max;
	double batt_current_discharge_max;
	double batt_power_charge_max;
	double batt_power_discharge_max;
	double batt_minimum_modetime;

	int batt_topology;
	double batt_ac_dc_efficiency;
	double batt_dc_ac_efficiency;
	double batt_dc_dc_bms_efficiency;
	double pv_dc_dc_mppt_efficiency;

	int inverter_model;
	double inv_snl_eff_cec;
	double inv_cec_cg_eff_cec;
	double inv_ds_eff;
	double inv_pd_eff;
	double inverter_efficiency;

	double batt_calendar_q0;
	double batt_calendar_a;
	double batt_calendar_b;
	double batt_calendar_c;
};


struct battstor
{

	battstor( compute_module &cm, bool setup_model, int replacement_option, size_t nrec, double dt_hr, batt_variables *batt_vars=0);
	void initialize_automated_dispatch(ssc_number_t *pv=0, ssc_number_t *load=0);
	~battstor();

	void initialize_time(size_t year, size_t hour_of_year, size_t step);
	void advance(compute_module &cm, double P_pv, double P_load);
	void outputs_fixed(compute_module &cm);
	void outputs_topology_dependent(compute_module &cm);
	void metrics(compute_module &cm);
	void update_post_inverted(compute_module &cm, double P_gen_ac);
	void update_grid_power(compute_module &cm, double P_gen_ac, double P_load_ac, size_t index);
	bool check_iterate(size_t count);
	void process_messages(compute_module &cm);

	// for user schedule
	void force_replacement();
	void check_replacement_schedule(int batt_replacement_option, size_t count_batt_replacement, ssc_number_t *batt_replacement);
	void calculate_monthly_and_annual_outputs( compute_module &cm );


	// time quantities
	size_t step_per_hour;
	size_t nyears;
	size_t total_steps;
	double _dt_hour;

	size_t year;
	size_t hour;
	size_t step;
	size_t index; // lifetime_index (0 - nyears * steps_per_hour * 8760)
	size_t year_index; // index for one year (0- steps_per_hour * 8760)

	// member data
	voltage_t *voltage_model;
	lifetime_t * lifetime_model;
	lifetime_cycle_t *lifetime_cycle_model;
	lifetime_calendar_t *lifetime_calendar_model;
	thermal_t *thermal_model;
	capacity_t *capacity_model;
	battery_t *battery_model;
	battery_metrics_t *battery_metrics;
	dispatch_manual_t *dispatch_model;
	losses_t *losses_model;
	charge_controller *charge_control;

	bool en;
	int chem;

	batt_variables * batt_vars;
	bool make_vars;
	
	bool dm_charge[6], dm_discharge[6], dm_gridcharge[6]; // manual dispatch
	std::map<int, double> dm_percent_discharge; // <profile, discharge_percent>
	std::map<int, double> dm_percent_gridcharge; // <profile, gridcharge_percent>
	util::matrix_t<float> dm_dynamic_sched;
	util::matrix_t<float> dm_dynamic_sched_weekend;
	std::vector<double> target_power;
	std::vector<double> target_power_monthly;
	
	int topology;
	double dc_dc, ac_dc, dc_ac;

	double e_charge;
	double e_discharge;

	std::vector<double> pv_prediction;
	std::vector<double> load_prediction;
	int prediction_index;

	// outputs
	ssc_number_t
		*outTotalCharge,
		*outAvailableCharge,
		*outBoundCharge,
		*outMaxChargeAtCurrent,
		*outMaxCharge,
		*outMaxChargeThermal,
		*outSOC,
		*outDOD,
		*outCurrent,
		*outCellVoltage,
		*outBatteryVoltage,
		*outCapacityPercent,
		*outCycles,
		*outBatteryBankReplacement,
		*outBatteryTemperature,
		*outCapacityThermalPercent,
		*outDispatchMode,
		*outBatteryPower,
		*outGenPower,
		*outGridPower,
		*outPVToLoad,
		*outBatteryToLoad,
		*outGridToLoad,
		*outGridPowerTarget,
		*outPVToBatt,
		*outGridToBatt,
		*outPVToGrid,
		*outBatteryToGrid,
		*outBatteryConversionPowerLoss,
		*outBatterySystemLoss,
		*outAnnualPVChargeEnergy,
		*outAnnualGridChargeEnergy,
		*outAnnualChargeEnergy,
		*outAnnualDischargeEnergy,
		*outAnnualGridImportEnergy,
		*outAnnualGridExportEnergy,
		*outAnnualEnergySystemLoss,
		*outAnnualEnergyLoss;

	double outAverageCycleEfficiency;
	double outAverageRoundtripEfficiency;
	double outPVChargePercent;
};


#endif
