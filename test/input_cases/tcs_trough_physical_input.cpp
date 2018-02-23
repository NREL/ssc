#include <string.h>
#include "sscapi.h"
#include "vartab.h"
#include "core.h"

#include "tcs_trough_physical_input.h"

using namespace std;

/**
 * Custom Testing Handler that does nothing
 */
ssc_bool_t my_handler(ssc_module_t, ssc_handler_t, int , float, float, const char *, const char *, void *){
	return true;
}

void var(var_table* vt, string name, string value){
	var_data* vd = new var_data(value);
	vt->assign(name, *vd);
}

void var(var_table* vt, string name, double value){
	var_data* vd = new var_data((ssc_number_t)value);
	vt->assign(name, *vd);
}

void var(var_table* vt, string name, float* array, int length){
	var_data* vd = new var_data(array, length);
	vt->assign(name, *vd);
}

void var(var_table* vt, string name, float* matrix, int nr, int nc){
	var_data* vd = new var_data(matrix, nr, nc);
	vt->assign(name, *vd);
}

void assign_default_variables(var_table* vt){
	var(vt, "file_name", "weather.csv");
	var(vt, "track_mode", 1);
	var(vt, "tilt", 0);
	var(vt, "azimuth", 0);
	var(vt, "system_capacity", 99899.9921875);
	var(vt, "nSCA", 8);
	var(vt, "nHCEt", 4);
	var(vt, "nColt", 4);
	var(vt, "nHCEVar", 4);
	var(vt, "nLoops", 181);
	var(vt, "eta_pump", 0.85000002384185791);
	var(vt, "HDR_rough", 4.5699998736381531e-005);
	var(vt, "theta_stow", 170);
	var(vt, "theta_dep", 10);
	var(vt, "Row_Distance", 15);
	var(vt, "FieldConfig", 2);
	var(vt, "T_startup", 300);
	var(vt, "P_ref", 111);
	var(vt, "m_dot_htfmin", 1);
	var(vt, "m_dot_htfmax", 12);
	var(vt, "T_loop_in_des", 293);
	var(vt, "T_loop_out", 391);
	var(vt, "Fluid", 21);
	var(vt, "T_fp", 150);
	var(vt, "I_bn_des", 950);
	var(vt, "V_hdr_max", 3);
	var(vt, "V_hdr_min", 2);
	var(vt, "Pipe_hl_coef", 0.44999998807907104);
	var(vt, "SCA_drives_elec", 125);
	var(vt, "fthrok", 1);
	var(vt, "fthrctrl", 2);
	var(vt, "water_usage_per_wash", 0.69999998807907104);
	var(vt, "washing_frequency", 63);
	var(vt, "accept_mode", 0);
	var(vt, "accept_init", 0);
	var(vt, "accept_loc", 1);
	var(vt, "solar_mult", 2);
	var(vt, "mc_bal_hot", 0.20000000298023224);
	var(vt, "mc_bal_cold", 0.20000000298023224);
	var(vt, "mc_bal_sca", 4.5);

}

void create_default(){
	var_table* vartab = new var_table;
	assign_default_variables(vartab);

	ssc_data_t* p_data = reinterpret_cast<ssc_data_t*>(vartab);
	// copy vartab data into p_data

	std::string name = "tcstrough_physical";
	//ssc_module_exec_simple(name.c_str(), p_data);
}