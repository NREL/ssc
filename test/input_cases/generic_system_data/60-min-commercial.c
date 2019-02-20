#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "sscapi.h"

ssc_bool_t my_handler(ssc_module_t p_mod, ssc_handler_t p_handler, int action,
	float f0, float f1, const char *s0, const char *s1, void *user_data)
{
	if (action == SSC_LOG)
	{
		// print log message to console
		switch ((int)f0)
		{
		case SSC_NOTICE: printf("Notice: %s", s0); break;
		case SSC_WARNING: printf("Warning: %s", s0); break;
		case SSC_ERROR: printf("Error: %s", s0); break;
		}
		return 1;
	}
	else if (action == SSC_UPDATE)
	{
		// print status update to console
		printf("(%.2f %) %s", f0, s0);
		return 1; // return 0 to abort simulation as needed.
	}
	else
		return 0;
}

int set_array(ssc_data_t p_data, const char *name, const char* fn, int len)
{
	char buffer[1024];
	char *record, *line;
	int i = 0;
	ssc_number_t *ary;
	FILE *fp = fopen(fn, "r");
	if (fp == NULL)
	{
		printf("file opening failed ");
		return 0;
	}
	ary = (ssc_number_t *)malloc(len * sizeof(ssc_number_t));
	while ((line = fgets(buffer, sizeof(buffer), fp)) != NULL)
	{
		record = strtok(line, ",");
		while ((record != NULL) && (i < len))
		{
			ary[i] = atof(record);
			record = strtok(NULL, ",");
			i++;
		}
	}
	fclose(fp);
	ssc_data_set_array(p_data, name, ary, len);
	free(ary);
	return 1;
}

int set_matrix(ssc_data_t p_data, const char *name, const char* fn, int nr, int nc)
{
	char buffer[1024];
	char *record, *line;
	ssc_number_t *ary;
	int i = 0, len = nr*nc;
	FILE *fp = fopen(fn, "r");
	if (fp == NULL)
	{
		printf("file opening failed ");
		return 0;
	}
	ary = (ssc_number_t *)malloc(len * sizeof(ssc_number_t));
	while ((line = fgets(buffer, sizeof(buffer), fp)) != NULL)
	{
		record = strtok(line, ",");
		while ((record != NULL) && (i < len))
		{
			ary[i] = atof(record);
			record = strtok(NULL, ",");
			i++;
		}
	}
	fclose(fp);
	ssc_data_set_matrix(p_data, name, ary, nr, nc);
	free(ary);
	return 1;
}

int main(int argc, char *argv[])
{
	printf("Current folder = C:/Projects/ssc/test/input_cases/generic_system_data\n");
	printf("SSC version = %d\n", ssc_version());
	printf("SSC build information = %s\n", ssc_build_info());
	ssc_module_exec_set_print(0);
	ssc_data_t data = ssc_data_create();
	if (data == NULL)
	{
		printf("error: out of memory.");
		return -1;
	}
	ssc_module_t module;

	ssc_data_set_number( data, "spec_mode", 1 );
	ssc_data_set_number( data, "derate", 4 );
	ssc_data_set_number( data, "system_capacity", 1.7090300321578979 );
	ssc_data_set_number( data, "user_capacity_factor", 43.599998474121094 );
	ssc_data_set_number( data, "heat_rate", 10 );
	ssc_data_set_number( data, "conv_eff", 34.118049621582031 );
	set_array( data, "energy_output_array", "C:/Projects/ssc/test/input_cases/generic_system_data/energy_output_array.csv", 8760);
	ssc_data_set_number( data, "system_use_lifetime_output", 0 );
	ssc_data_set_number( data, "analysis_period", 25 );
	ssc_number_t p_generic_degradation[1] ={ 0 };
	ssc_data_set_array( data, "generic_degradation", p_generic_degradation, 1 );
	ssc_data_set_number( data, "adjust:constant", 0 );
	module = ssc_module_create("generic_system"); 
	if (NULL == module)
	{
		printf("error: could not create 'generic_system' module."); 
		ssc_data_free(data); 
		return -1; 
	}
	if (ssc_module_exec(module, data) == 0)
	{
		printf("error during simulation."); 
		ssc_module_free(module); 
		ssc_data_free(data); 
		return -1; 
	}
	ssc_module_free(module);
	ssc_data_set_number( data, "en_batt", 1 );
	set_array( data, "load", "C:/Projects/ssc/test/input_cases/generic_system_data/load.csv", 8760);
	ssc_data_set_number( data, "batt_replacement_option", 0 );
	ssc_data_set_number( data, "batt_chem", 1 );
	ssc_data_set_number( data, "batt_ac_or_dc", 1 );
	ssc_data_set_number( data, "batt_dc_dc_efficiency", 98 );
	ssc_data_set_number( data, "batt_dc_ac_efficiency", 96 );
	ssc_data_set_number( data, "batt_ac_dc_efficiency", 96 );
	ssc_data_set_number( data, "batt_meter_position", 0 );
	ssc_number_t p_batt_losses[1] ={ 0 };
	ssc_data_set_array( data, "batt_losses", p_batt_losses, 1 );
	ssc_number_t p_batt_losses_charging[1] ={ 0 };
	ssc_data_set_array( data, "batt_losses_charging", p_batt_losses_charging, 1 );
	ssc_number_t p_batt_losses_discharging[1] ={ 0 };
	ssc_data_set_array( data, "batt_losses_discharging", p_batt_losses_discharging, 1 );
	ssc_number_t p_batt_losses_idle[1] ={ 0 };
	ssc_data_set_array( data, "batt_losses_idle", p_batt_losses_idle, 1 );
	ssc_data_set_number( data, "batt_loss_choice", 0 );
	ssc_data_set_number( data, "batt_current_choice", 1 );
	ssc_data_set_number( data, "batt_computed_strings", 89 );
	ssc_data_set_number( data, "batt_computed_series", 139 );
	ssc_data_set_number( data, "batt_computed_bank_capacity", 100.20510101318359 );
	ssc_data_set_number( data, "batt_current_charge_max", 100.125 );
	ssc_data_set_number( data, "batt_current_discharge_max", 100.125 );
	ssc_data_set_number( data, "batt_power_charge_max", 50.102550506591797 );
	ssc_data_set_number( data, "batt_power_discharge_max", 50.102550506591797 );
	ssc_data_set_number( data, "batt_voltage_choice", 0 );
	ssc_data_set_number( data, "batt_Vfull", 4.0999999046325684 );
	ssc_data_set_number( data, "batt_Vexp", 4.0500001907348633 );
	ssc_data_set_number( data, "batt_Vnom", 3.4000000953674316 );
	ssc_data_set_number( data, "batt_Vnom_default", 3.5999999046325684 );
	ssc_data_set_number( data, "batt_Qfull", 2.25 );
	ssc_data_set_number( data, "batt_Qfull_flow", 200.25 );
	ssc_data_set_number( data, "batt_Qexp", 0.040049999952316284 );
	ssc_data_set_number( data, "batt_Qnom", 2.0002501010894775 );
	ssc_data_set_number( data, "batt_C_rate", 0.20000000298023224 );
	ssc_data_set_number( data, "batt_resistance", 0.20000000298023224 );
	ssc_number_t p_batt_voltage_matrix[12] ={ 0, 1.2000000476837158, 20, 1.1000000238418579, 40, 1.0499999523162842, 60, 1, 80, 0.94999998807907104, 100, 0.5 };
	ssc_data_set_matrix( data, "batt_voltage_matrix", p_batt_voltage_matrix, 6, 2 );
	ssc_data_set_number( data, "LeadAcid_q20_computed", 200.25 );
	ssc_data_set_number( data, "LeadAcid_q10_computed", 186.23249816894531 );
	ssc_data_set_number( data, "LeadAcid_qn_computed", 120.15000152587891 );
	ssc_data_set_number( data, "LeadAcid_tn", 1 );
	ssc_data_set_number( data, "batt_initial_SOC", 50 );
	ssc_data_set_number( data, "batt_minimum_SOC", 15 );
	ssc_data_set_number( data, "batt_maximum_SOC", 95 );
	ssc_data_set_number( data, "batt_minimum_modetime", 10 );
	ssc_number_t p_batt_lifetime_matrix[18] ={ 20, 0, 100, 20, 5000, 80, 20, 10000, 60, 80, 0, 100, 80, 1000, 80, 80, 2000, 60 };
	ssc_data_set_matrix( data, "batt_lifetime_matrix", p_batt_lifetime_matrix, 6, 3 );
	ssc_data_set_number( data, "batt_calendar_choice", 0 );
	ssc_number_t p_batt_calendar_lifetime_matrix[6] ={ 0, 100, 3650, 80, 7300, 50 };
	ssc_data_set_matrix( data, "batt_calendar_lifetime_matrix", p_batt_calendar_lifetime_matrix, 3, 2 );
	ssc_data_set_number( data, "batt_calendar_q0", 1.0199999809265137 );
	ssc_data_set_number( data, "batt_calendar_a", 0.0026599999982863665 );
	ssc_data_set_number( data, "batt_calendar_b", -7280 );
	ssc_data_set_number( data, "batt_calendar_c", 930 );
	ssc_data_set_number( data, "batt_replacement_capacity", 0 );
	ssc_number_t p_batt_replacement_schedule[1] ={ 0 };
	ssc_data_set_array( data, "batt_replacement_schedule", p_batt_replacement_schedule, 1 );
	ssc_data_set_number( data, "batt_replacement_cost", 600 );
	ssc_data_set_number( data, "batt_mass", 507.8046875 );
	ssc_data_set_number( data, "batt_length", 0.58471626043319702 );
	ssc_data_set_number( data, "batt_width", 0.58471626043319702 );
	ssc_data_set_number( data, "batt_height", 0.58471626043319702 );
	ssc_data_set_number( data, "batt_Cp", 4183 );
	ssc_data_set_number( data, "batt_h_to_ambient", 5 );
	ssc_data_set_number( data, "T_room", 20 );
	ssc_number_t p_cap_vs_temp[8] ={ -15, 65, 0, 85, 25, 100, 40, 104 };
	ssc_data_set_matrix( data, "cap_vs_temp", p_cap_vs_temp, 4, 2 );
	ssc_number_t p_dispatch_manual_charge[6] ={ 1, 1, 1, 0, 0, 0 };
	ssc_data_set_array( data, "dispatch_manual_charge", p_dispatch_manual_charge, 6 );
	ssc_number_t p_dispatch_manual_discharge[6] ={ 0, 0, 1, 0, 0, 0 };
	ssc_data_set_array( data, "dispatch_manual_discharge", p_dispatch_manual_discharge, 6 );
	ssc_number_t p_dispatch_manual_gridcharge[6] ={ 0, 1, 0, 0, 0, 0 };
	ssc_data_set_array( data, "dispatch_manual_gridcharge", p_dispatch_manual_gridcharge, 6 );
	ssc_number_t p_dispatch_manual_percent_discharge[2] ={ 25, 0 };
	ssc_data_set_array( data, "dispatch_manual_percent_discharge", p_dispatch_manual_percent_discharge, 2 );
	ssc_number_t p_dispatch_manual_percent_gridcharge[2] ={ 100, 0 };
	ssc_data_set_array( data, "dispatch_manual_percent_gridcharge", p_dispatch_manual_percent_gridcharge, 2 );
	ssc_number_t p_dispatch_manual_sched[288] ={ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1 };
	ssc_data_set_matrix( data, "dispatch_manual_sched", p_dispatch_manual_sched, 12, 24 );
	ssc_number_t p_dispatch_manual_sched_weekend[288] ={ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1 };
	ssc_data_set_matrix( data, "dispatch_manual_sched_weekend", p_dispatch_manual_sched_weekend, 12, 24 );
	ssc_number_t p_batt_target_power[1] ={ 15 };
	ssc_data_set_array( data, "batt_target_power", p_batt_target_power, 1 );
	ssc_number_t p_batt_target_power_monthly[1] ={ 0 };
	ssc_data_set_array( data, "batt_target_power_monthly", p_batt_target_power_monthly, 1 );
	ssc_data_set_number( data, "batt_target_choice", 0 );
	set_array( data, "batt_custom_dispatch", "C:/Projects/ssc/test/input_cases/generic_system_data/batt_custom_dispatch.csv", 8760);
	ssc_data_set_number( data, "batt_dispatch_choice", 3 );
	ssc_data_set_number( data, "batt_dispatch_auto_can_gridcharge", 0 );
	ssc_data_set_number( data, "batt_dispatch_auto_can_charge", 1 );
	ssc_number_t p_ur_ec_sched_weekday[288] ={ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
	ssc_data_set_matrix( data, "ur_ec_sched_weekday", p_ur_ec_sched_weekday, 12, 24 );
	ssc_number_t p_ur_ec_sched_weekend[288] ={ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
	ssc_data_set_matrix( data, "ur_ec_sched_weekend", p_ur_ec_sched_weekend, 12, 24 );
	ssc_number_t p_ur_ec_tou_mat[30] ={ 1, 1, 9.9999996802856925e+37, 0, 0.094169996678829193, 0, 2, 1, 400, 0, 0.096869997680187225, 0, 2, 2, 800, 0, 0.13817000389099121, 0, 2, 3, 3000, 0, 0.16166999936103821, 0, 2, 4, 9.9999996802856925e+37, 0, 0.17257000505924225, 0 };
	ssc_data_set_matrix( data, "ur_ec_tou_mat", p_ur_ec_tou_mat, 5, 6 );
	module = ssc_module_create("battery"); 
	if (NULL == module)
	{
		printf("error: could not create 'battery' module."); 
		ssc_data_free(data); 
		return -1; 
	}
	if (ssc_module_exec(module, data) == 0)
	{
		printf("error during simulation."); 
		ssc_module_free(module); 
		ssc_data_free(data); 
		return -1; 
	}
	ssc_module_free(module);
	ssc_data_set_number( data, "inflation_rate", 2.5 );
	ssc_number_t p_degradation[1] ={ 0 };
	ssc_data_set_array( data, "degradation", p_degradation, 1 );
	ssc_number_t p_load_escalation[1] ={ 0 };
	ssc_data_set_array( data, "load_escalation", p_load_escalation, 1 );
	ssc_number_t p_rate_escalation[1] ={ 0 };
	ssc_data_set_array( data, "rate_escalation", p_rate_escalation, 1 );
	ssc_data_set_number( data, "ur_metering_option", 2 );
	ssc_data_set_number( data, "ur_nm_yearend_sell_rate", 0.027890000492334366 );
	ssc_data_set_number( data, "ur_monthly_fixed_charge", 8.5500001907348633 );
	ssc_data_set_number( data, "ur_monthly_min_charge", 0 );
	ssc_data_set_number( data, "ur_annual_min_charge", 0 );
	ssc_data_set_number( data, "ur_en_ts_sell_rate", 0 );
	set_array( data, "ur_ts_sell_rate", "C:/Projects/ssc/test/input_cases/generic_system_data/ur_ts_sell_rate.csv", 8760);
	ssc_data_set_number( data, "ur_dc_enable", 1 );
	ssc_number_t p_ur_dc_sched_weekday[288] ={ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
	ssc_data_set_matrix( data, "ur_dc_sched_weekday", p_ur_dc_sched_weekday, 12, 24 );
	ssc_number_t p_ur_dc_sched_weekend[288] ={ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };
	ssc_data_set_matrix( data, "ur_dc_sched_weekend", p_ur_dc_sched_weekend, 12, 24 );
	ssc_number_t p_ur_dc_tou_mat[4] ={ 1, 1, 0, 0 };
	ssc_data_set_matrix( data, "ur_dc_tou_mat", p_ur_dc_tou_mat, 1, 4 );
	ssc_number_t p_ur_dc_flat_mat[48] ={ 0, 1, 0, 0, 1, 1, 0, 0, 2, 1, 0, 0, 3, 1, 0, 0, 4, 1, 0, 0, 5, 1, 0, 0, 6, 1, 0, 0, 7, 1, 0, 0, 8, 1, 0, 0, 9, 1, 0, 0, 10, 1, 0, 0, 11, 1, 0, 0 };
	ssc_data_set_matrix( data, "ur_dc_flat_mat", p_ur_dc_flat_mat, 12, 4 );
	module = ssc_module_create("utilityrate5"); 
	if (NULL == module)
	{
		printf("error: could not create 'utilityrate5' module."); 
		ssc_data_free(data); 
		return -1; 
	}
	if (ssc_module_exec(module, data) == 0)
	{
		printf("error during simulation."); 
		ssc_module_free(module); 
		ssc_data_free(data); 
		return -1; 
	}
	ssc_module_free(module);
	ssc_number_t p_federal_tax_rate[1] ={ 21 };
	ssc_data_set_array( data, "federal_tax_rate", p_federal_tax_rate, 1 );
	ssc_number_t p_state_tax_rate[1] ={ 7 };
	ssc_data_set_array( data, "state_tax_rate", p_state_tax_rate, 1 );
	ssc_data_set_number( data, "property_tax_rate", 0 );
	ssc_data_set_number( data, "prop_tax_cost_assessed_percent", 100 );
	ssc_data_set_number( data, "prop_tax_assessed_decline", 0 );
	ssc_data_set_number( data, "real_discount_rate", 6.4000000953674316 );
	ssc_data_set_number( data, "insurance_rate", 0.5 );
	ssc_data_set_number( data, "loan_term", 25 );
	ssc_data_set_number( data, "loan_rate", 7.5 );
	ssc_data_set_number( data, "debt_fraction", 100 );
	ssc_number_t p_om_fixed[1] ={ 0 };
	ssc_data_set_array( data, "om_fixed", p_om_fixed, 1 );
	ssc_data_set_number( data, "om_fixed_escal", 0 );
	ssc_number_t p_om_production[1] ={ 0 };
	ssc_data_set_array( data, "om_production", p_om_production, 1 );
	ssc_data_set_number( data, "om_production_escal", 0 );
	ssc_number_t p_om_capacity[1] ={ 25 };
	ssc_data_set_array( data, "om_capacity", p_om_capacity, 1 );
	ssc_data_set_number( data, "om_capacity_escal", 0 );
	ssc_number_t p_om_fuel_cost[1] ={ 8 };
	ssc_data_set_array( data, "om_fuel_cost", p_om_fuel_cost, 1 );
	ssc_data_set_number( data, "om_fuel_cost_escal", 0 );
	ssc_data_set_number( data, "depr_fed_type", 1 );
	ssc_data_set_number( data, "depr_fed_sl_years", 7 );
	ssc_number_t p_depr_fed_custom[1] ={ 0 };
	ssc_data_set_array( data, "depr_fed_custom", p_depr_fed_custom, 1 );
	ssc_data_set_number( data, "depr_sta_type", 1 );
	ssc_data_set_number( data, "depr_sta_sl_years", 7 );
	ssc_number_t p_depr_sta_custom[1] ={ 0 };
	ssc_data_set_array( data, "depr_sta_custom", p_depr_sta_custom, 1 );
	ssc_data_set_number( data, "itc_fed_amount", 0 );
	ssc_data_set_number( data, "itc_fed_amount_deprbas_fed", 1 );
	ssc_data_set_number( data, "itc_fed_amount_deprbas_sta", 1 );
	ssc_data_set_number( data, "itc_sta_amount", 0 );
	ssc_data_set_number( data, "itc_sta_amount_deprbas_fed", 0 );
	ssc_data_set_number( data, "itc_sta_amount_deprbas_sta", 0 );
	ssc_data_set_number( data, "itc_fed_percent", 0 );
	ssc_data_set_number( data, "itc_fed_percent_maxvalue", 9.9999996802856925e+37 );
	ssc_data_set_number( data, "itc_fed_percent_deprbas_fed", 1 );
	ssc_data_set_number( data, "itc_fed_percent_deprbas_sta", 1 );
	ssc_data_set_number( data, "itc_sta_percent", 0 );
	ssc_data_set_number( data, "itc_sta_percent_maxvalue", 9.9999996802856925e+37 );
	ssc_data_set_number( data, "itc_sta_percent_deprbas_fed", 0 );
	ssc_data_set_number( data, "itc_sta_percent_deprbas_sta", 0 );
	ssc_number_t p_ptc_fed_amount[1] ={ 0 };
	ssc_data_set_array( data, "ptc_fed_amount", p_ptc_fed_amount, 1 );
	ssc_data_set_number( data, "ptc_fed_term", 10 );
	ssc_data_set_number( data, "ptc_fed_escal", 0 );
	ssc_number_t p_ptc_sta_amount[1] ={ 0 };
	ssc_data_set_array( data, "ptc_sta_amount", p_ptc_sta_amount, 1 );
	ssc_data_set_number( data, "ptc_sta_term", 10 );
	ssc_data_set_number( data, "ptc_sta_escal", 0 );
	ssc_data_set_number( data, "ibi_fed_amount", 0 );
	ssc_data_set_number( data, "ibi_fed_amount_tax_fed", 1 );
	ssc_data_set_number( data, "ibi_fed_amount_tax_sta", 1 );
	ssc_data_set_number( data, "ibi_fed_amount_deprbas_fed", 0 );
	ssc_data_set_number( data, "ibi_fed_amount_deprbas_sta", 0 );
	ssc_data_set_number( data, "ibi_sta_amount", 0 );
	ssc_data_set_number( data, "ibi_sta_amount_tax_fed", 1 );
	ssc_data_set_number( data, "ibi_sta_amount_tax_sta", 1 );
	ssc_data_set_number( data, "ibi_sta_amount_deprbas_fed", 0 );
	ssc_data_set_number( data, "ibi_sta_amount_deprbas_sta", 0 );
	ssc_data_set_number( data, "ibi_uti_amount", 0 );
	ssc_data_set_number( data, "ibi_uti_amount_tax_fed", 1 );
	ssc_data_set_number( data, "ibi_uti_amount_tax_sta", 1 );
	ssc_data_set_number( data, "ibi_uti_amount_deprbas_fed", 0 );
	ssc_data_set_number( data, "ibi_uti_amount_deprbas_sta", 0 );
	ssc_data_set_number( data, "ibi_oth_amount", 0 );
	ssc_data_set_number( data, "ibi_oth_amount_tax_fed", 1 );
	ssc_data_set_number( data, "ibi_oth_amount_tax_sta", 1 );
	ssc_data_set_number( data, "ibi_oth_amount_deprbas_fed", 0 );
	ssc_data_set_number( data, "ibi_oth_amount_deprbas_sta", 0 );
	ssc_data_set_number( data, "ibi_fed_percent", 0 );
	ssc_data_set_number( data, "ibi_fed_percent_maxvalue", 9.9999996802856925e+37 );
	ssc_data_set_number( data, "ibi_fed_percent_tax_fed", 1 );
	ssc_data_set_number( data, "ibi_fed_percent_tax_sta", 1 );
	ssc_data_set_number( data, "ibi_fed_percent_deprbas_fed", 0 );
	ssc_data_set_number( data, "ibi_fed_percent_deprbas_sta", 0 );
	ssc_data_set_number( data, "ibi_sta_percent", 0 );
	ssc_data_set_number( data, "ibi_sta_percent_maxvalue", 9.9999996802856925e+37 );
	ssc_data_set_number( data, "ibi_sta_percent_tax_fed", 1 );
	ssc_data_set_number( data, "ibi_sta_percent_tax_sta", 1 );
	ssc_data_set_number( data, "ibi_sta_percent_deprbas_fed", 0 );
	ssc_data_set_number( data, "ibi_sta_percent_deprbas_sta", 0 );
	ssc_data_set_number( data, "ibi_uti_percent", 0 );
	ssc_data_set_number( data, "ibi_uti_percent_maxvalue", 9.9999996802856925e+37 );
	ssc_data_set_number( data, "ibi_uti_percent_tax_fed", 1 );
	ssc_data_set_number( data, "ibi_uti_percent_tax_sta", 1 );
	ssc_data_set_number( data, "ibi_uti_percent_deprbas_fed", 0 );
	ssc_data_set_number( data, "ibi_uti_percent_deprbas_sta", 0 );
	ssc_data_set_number( data, "ibi_oth_percent", 0 );
	ssc_data_set_number( data, "ibi_oth_percent_maxvalue", 9.9999996802856925e+37 );
	ssc_data_set_number( data, "ibi_oth_percent_tax_fed", 1 );
	ssc_data_set_number( data, "ibi_oth_percent_tax_sta", 1 );
	ssc_data_set_number( data, "ibi_oth_percent_deprbas_fed", 0 );
	ssc_data_set_number( data, "ibi_oth_percent_deprbas_sta", 0 );
	ssc_data_set_number( data, "cbi_fed_amount", 0 );
	ssc_data_set_number( data, "cbi_fed_maxvalue", 9.9999996802856925e+37 );
	ssc_data_set_number( data, "cbi_fed_tax_fed", 1 );
	ssc_data_set_number( data, "cbi_fed_tax_sta", 1 );
	ssc_data_set_number( data, "cbi_fed_deprbas_fed", 0 );
	ssc_data_set_number( data, "cbi_fed_deprbas_sta", 0 );
	ssc_data_set_number( data, "cbi_sta_amount", 0 );
	ssc_data_set_number( data, "cbi_sta_maxvalue", 9.9999996802856925e+37 );
	ssc_data_set_number( data, "cbi_sta_tax_fed", 1 );
	ssc_data_set_number( data, "cbi_sta_tax_sta", 1 );
	ssc_data_set_number( data, "cbi_sta_deprbas_fed", 0 );
	ssc_data_set_number( data, "cbi_sta_deprbas_sta", 0 );
	ssc_data_set_number( data, "cbi_uti_amount", 0 );
	ssc_data_set_number( data, "cbi_uti_maxvalue", 9.9999996802856925e+37 );
	ssc_data_set_number( data, "cbi_uti_tax_fed", 1 );
	ssc_data_set_number( data, "cbi_uti_tax_sta", 1 );
	ssc_data_set_number( data, "cbi_uti_deprbas_fed", 0 );
	ssc_data_set_number( data, "cbi_uti_deprbas_sta", 0 );
	ssc_data_set_number( data, "cbi_oth_amount", 0 );
	ssc_data_set_number( data, "cbi_oth_maxvalue", 9.9999996802856925e+37 );
	ssc_data_set_number( data, "cbi_oth_tax_fed", 1 );
	ssc_data_set_number( data, "cbi_oth_tax_sta", 1 );
	ssc_data_set_number( data, "cbi_oth_deprbas_fed", 0 );
	ssc_data_set_number( data, "cbi_oth_deprbas_sta", 0 );
	ssc_number_t p_pbi_fed_amount[1] ={ 0 };
	ssc_data_set_array( data, "pbi_fed_amount", p_pbi_fed_amount, 1 );
	ssc_data_set_number( data, "pbi_fed_term", 0 );
	ssc_data_set_number( data, "pbi_fed_escal", 0 );
	ssc_data_set_number( data, "pbi_fed_tax_fed", 1 );
	ssc_data_set_number( data, "pbi_fed_tax_sta", 1 );
	ssc_number_t p_pbi_sta_amount[1] ={ 0 };
	ssc_data_set_array( data, "pbi_sta_amount", p_pbi_sta_amount, 1 );
	ssc_data_set_number( data, "pbi_sta_term", 0 );
	ssc_data_set_number( data, "pbi_sta_escal", 0 );
	ssc_data_set_number( data, "pbi_sta_tax_fed", 1 );
	ssc_data_set_number( data, "pbi_sta_tax_sta", 1 );
	ssc_number_t p_pbi_uti_amount[1] ={ 0 };
	ssc_data_set_array( data, "pbi_uti_amount", p_pbi_uti_amount, 1 );
	ssc_data_set_number( data, "pbi_uti_term", 0 );
	ssc_data_set_number( data, "pbi_uti_escal", 0 );
	ssc_data_set_number( data, "pbi_uti_tax_fed", 1 );
	ssc_data_set_number( data, "pbi_uti_tax_sta", 1 );
	ssc_number_t p_pbi_oth_amount[1] ={ 0 };
	ssc_data_set_array( data, "pbi_oth_amount", p_pbi_oth_amount, 1 );
	ssc_data_set_number( data, "pbi_oth_term", 0 );
	ssc_data_set_number( data, "pbi_oth_escal", 0 );
	ssc_data_set_number( data, "pbi_oth_tax_fed", 1 );
	ssc_data_set_number( data, "pbi_oth_tax_sta", 1 );
	ssc_data_set_number( data, "battery_per_kWh", 300 );
	ssc_data_set_number( data, "batt_replacement_cost_escal", 0 );
	ssc_data_set_number( data, "total_installed_cost", 86827.921875 );
	ssc_data_set_number( data, "salvage_percentage", 0 );
	module = ssc_module_create("cashloan"); 
	if (NULL == module)
	{
		printf("error: could not create 'cashloan' module."); 
		ssc_data_free(data); 
		return -1; 
	}
	if (ssc_module_exec(module, data) == 0)
	{
		printf("error during simulation."); 
		ssc_module_free(module); 
		ssc_data_free(data); 
		return -1; 
	}
	ssc_module_free(module);
	ssc_number_t annual_energy;
	ssc_data_get_number(data, "annual_energy", &annual_energy);
	printf("%s = %.17g\n", "Annual energy (year 1)", (double)annual_energy);
	ssc_number_t capacity_factor;
	ssc_data_get_number(data, "capacity_factor", &capacity_factor);
	printf("%s = %.17g\n", "Capacity factor (year 1)", (double)capacity_factor);
	ssc_number_t average_battery_roundtrip_efficiency;
	ssc_data_get_number(data, "average_battery_roundtrip_efficiency", &average_battery_roundtrip_efficiency);
	printf("%s = %.17g\n", "Battery efficiency (incl. converter + ancillary)", (double)average_battery_roundtrip_efficiency);
	ssc_number_t lcoe_nom;
	ssc_data_get_number(data, "lcoe_nom", &lcoe_nom);
	printf("%s = %.17g\n", "Levelized COE (nominal)", (double)lcoe_nom);
	ssc_number_t lcoe_real;
	ssc_data_get_number(data, "lcoe_real", &lcoe_real);
	printf("%s = %.17g\n", "Levelized COE (real)", (double)lcoe_real);
	ssc_number_t elec_cost_without_system_year1;
	ssc_data_get_number(data, "elec_cost_without_system_year1", &elec_cost_without_system_year1);
	printf("%s = %.17g\n", "Electricity bill without system (year 1)", (double)elec_cost_without_system_year1);
	ssc_number_t elec_cost_with_system_year1;
	ssc_data_get_number(data, "elec_cost_with_system_year1", &elec_cost_with_system_year1);
	printf("%s = %.17g\n", "Electricity bill with system (year 1)", (double)elec_cost_with_system_year1);
	ssc_number_t savings_year1;
	ssc_data_get_number(data, "savings_year1", &savings_year1);
	printf("%s = %.17g\n", "Net savings with system (year 1)", (double)savings_year1);
	ssc_number_t npv;
	ssc_data_get_number(data, "npv", &npv);
	printf("%s = %.17g\n", "Net present value", (double)npv);
	ssc_number_t payback;
	ssc_data_get_number(data, "payback", &payback);
	printf("%s = %.17g\n", "Simple payback period", (double)payback);
	ssc_number_t discounted_payback;
	ssc_data_get_number(data, "discounted_payback", &discounted_payback);
	printf("%s = %.17g\n", "Discounted payback period", (double)discounted_payback);
	ssc_number_t adjusted_installed_cost;
	ssc_data_get_number(data, "adjusted_installed_cost", &adjusted_installed_cost);
	printf("%s = %.17g\n", "Net capital cost", (double)adjusted_installed_cost);
	ssc_number_t first_cost;
	ssc_data_get_number(data, "first_cost", &first_cost);
	printf("%s = %.17g\n", "Equity", (double)first_cost);
	ssc_number_t loan_amount;
	ssc_data_get_number(data, "loan_amount", &loan_amount);
	printf("%s = %.17g\n", "Debt", (double)loan_amount);
	ssc_data_free(data);
	return 0;
}
