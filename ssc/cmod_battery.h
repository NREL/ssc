/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (�Alliance�) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
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
*  the underlying software originally provided by Alliance as �System Advisor Model� or �SAM�. Except
*  to comply with the foregoing, the terms �System Advisor Model�, �SAM�, or any confusingly similar
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

#include <map>

#include "core.h"

// forward declarations to speed up build
class SharedInverter;
class voltage_t;
class lifetime_t;
class lifetime_cycle_t;
class lifetime_calendar_t;
class thermal_t;
class capacity_t;
class battery_t;
class battery_metrics_t;
class dispatch_t;
class losses_t;
class ChargeController;
class UtilityRate;

extern var_info vtab_battery_inputs[];
extern var_info vtab_battery_outputs[];

struct batt_variables
{
	bool system_use_lifetime_output;
	bool en_batt;
	bool en_fuelcell;
	int analysis_period;
	int batt_chem;
	int batt_dispatch;
	int batt_voltage_choice;
	int batt_current_choice;
	int batt_meter_position;
	int batt_target_choice;
	int batt_loss_choice;
	int batt_calendar_choice;

	ssc_number_t *pcharge = 0;
	ssc_number_t *pdischarge = 0;
	ssc_number_t *pdischarge_percent = 0;
	ssc_number_t *pgridcharge_percent = 0;
	ssc_number_t *pgridcharge = 0;
	ssc_number_t *psched = 0;
	ssc_number_t *psched_weekend = 0;

	/*! The custom dispatch power input by user (<0 = charging, >0 = discharging) in kW */
	std::vector<double> batt_custom_dispatch;

	/*! Determines if the battery is allowed to charge from the grid using automated control*/
	bool batt_dispatch_auto_can_gridcharge;

	/*! Determines if the battery is allowed to charge from the RE using automated control*/
	bool batt_dispatch_auto_can_charge;

	/*! Determines if the battery is allowed to charge from PV clipping using automated control*/
	bool batt_dispatch_auto_can_clipcharge;

	/*! Determines if the battery is allowed to charge from fuel cell using automated control*/
	bool batt_dispatch_auto_can_fuelcellcharge;

	/*! Vector of periods and if battery can charge from PV*/
	std::vector<bool> batt_can_charge;

	/*! Vector of periods if battery can charge from Fuel Cell*/
	std::vector<bool> batt_can_fuelcellcharge;

	/*! Vector of periods and if battery can discharge*/
	std::vector<bool> batt_can_discharge;

	/*! Vector of periods and if battery can charge from the grid*/
	std::vector<bool> batt_can_gridcharge;

	/*! Vector of percentages that battery is allowed to charge for periods*/
	std::vector<double> batt_discharge_percent;

	/*! Vector of percentages that battery is allowed to gridcharge for periods*/
	std::vector<double> batt_gridcharge_percent;

	/*! Schedule of manual discharge for weekday*/
	util::matrix_t<size_t> batt_discharge_schedule_weekday;

	/*! Schedule of manual discharge for weekend*/
	util::matrix_t<size_t> batt_discharge_schedule_weekend;

	/*! The number of hours to look-ahead in automated dispatch */
	size_t batt_look_ahead_hours;

	/*! The frequency to update the look-ahead automated dispatch */
	double batt_dispatch_update_frequency_hours;

	util::matrix_t<double>  batt_lifetime_matrix;
	util::matrix_t<double> batt_calendar_lifetime_matrix;
	util::matrix_t<double> batt_voltage_matrix;

	std::vector<double> target_power_monthly;
	std::vector<double> target_power;
	std::vector<double> pv_clipping_forecast;
	std::vector<double> pv_dc_power_forecast;


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
	std::vector<double> T_room;

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

	size_t inverter_model;
	double inverter_efficiency;
	double inverter_paco;
	size_t inverter_count;

	double batt_calendar_q0;
	double batt_calendar_a;
	double batt_calendar_b;
	double batt_calendar_c;

	/*! Battery costs */
	double batt_cost_per_kwh;

	/*! PPA Time-of-Delivery factors for periods 1-9 */
	std::vector<double> ppa_factors;
	util::matrix_t<size_t> ppa_weekday_schedule;
	util::matrix_t<size_t> ppa_weekend_schedule;

	/*! Energy rates */
	bool ec_rate_defined;
	util::matrix_t<size_t> ec_weekday_schedule;
	util::matrix_t<size_t> ec_weekend_schedule;
	util::matrix_t<double> ec_tou_matrix;

	/* Battery replacement options */
	int batt_replacement_option;
	std::vector<int> batt_replacement_schedule;

	/* Battery cycle costs */
	int batt_cycle_cost_choice;
	double batt_cycle_cost;
};


struct battstor
{

	battstor( compute_module &cm, bool setup_model, size_t nrec, double dt_hr, batt_variables *batt_vars=0);
	void parse_configuration();
	void initialize_automated_dispatch(std::vector<ssc_number_t> pv= std::vector<ssc_number_t>(), 
									   std::vector<ssc_number_t> load= std::vector<ssc_number_t>(), 
									   std::vector<ssc_number_t> cliploss= std::vector<ssc_number_t>());
	~battstor();

	void initialize_time(size_t year, size_t hour_of_year, size_t step);

	/// Run the battery for the current timestep, given the PV power, load, and clipped power
	void advance(compute_module &cm, double P_pv, double V_pv=0, double P_load=0, double P_pv_clipped=0);

	/// Given a DC connected battery, set the shared PV and battery invertr
	void setSharedInverter(SharedInverter * sharedInverter);

	void outputs_fixed(compute_module &cm);
	void outputs_topology_dependent(compute_module &cm);
	void metrics(compute_module &cm);
	void update_grid_power(compute_module &cm, double P_gen_ac, double P_load_ac, size_t index);
	void process_messages(compute_module &cm);

	/*! Manual dispatch*/
	bool manual_dispatch = false;

	/*! Automated dispatch look ahead*/
	bool look_ahead = false;

	/*! Automated dispatch look behind*/
	bool look_behind = false;

	/*! Automated dispatch use custom input forecast (look ahead)*/
	bool input_forecast = false;

	/*! Automated dispatch override algorithm grid target calculation*/
	bool input_target = false;

	/*! Use user-input battery dispatch */
	bool input_custom_dispatch = false;

	// for user schedule
	void force_replacement();
	void check_replacement_schedule();
	void calculate_monthly_and_annual_outputs( compute_module &cm );

	// time quantities
	size_t step_per_hour;
	size_t step_per_year;
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
	dispatch_t *dispatch_model;
	losses_t *losses_model;
	ChargeController *charge_control;
	UtilityRate * utilityRate;
	
	bool en;
	int chem;

	batt_variables * batt_vars;
	bool make_vars;
	
	/*! Map of profile to discharge percent */
	std::map<size_t, double> dm_percent_discharge; 

	/*! Map of profile to gridcharge percent*/
	std::map<size_t, double> dm_percent_gridcharge; 

	std::vector<double> target_power;
	std::vector<double> target_power_monthly;
	
	double e_charge;
	double e_discharge;

	/*! Variables to store forecast data */
	std::vector<double> pv_prediction;
	std::vector<double> load_prediction;
	std::vector<double> cliploss_prediction;
	int prediction_index;

	/*! If fuel cell is attached */
	std::vector<double> fuelcellPower;

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
		*outFuelCellToLoad,
		*outGridPowerTarget,
		*outBattPowerTarget,
		*outPVToBatt,
		*outGridToBatt,
		*outFuelCellToBatt,
		*outPVToGrid,
		*outBatteryToGrid,
		*outFuelCellToGrid,
		*outBatteryConversionPowerLoss,
		*outBatterySystemLoss,
		*outAnnualPVChargeEnergy,
		*outAnnualGridChargeEnergy,
		*outAnnualChargeEnergy,
		*outAnnualDischargeEnergy,
		*outAnnualGridImportEnergy,
		*outAnnualGridExportEnergy,
		*outAnnualEnergySystemLoss,
		*outAnnualEnergyLoss,
		*outCostToCycle;

	double outAverageCycleEfficiency;
	double outAverageRoundtripEfficiency;
	double outPVChargePercent;
};


#endif
