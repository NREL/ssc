#include "csp_solver_pc_sco2.h"
#include "csp_solver_core.h"

#include "htf_props.h"

C_pc_sco2::C_pc_sco2()
{
	abc = 1.23;
}

void C_pc_sco2::init(C_csp_power_cycle::S_solved_params &solved_params)
{
	throw(C_csp_exception("C_pc_sco2::init() is not complete"));
}

int C_pc_sco2::get_operating_state()
{
	throw(C_csp_exception("C_pc_sco2::get_operating_state() is not complete"));
	
	return -1;		//[-]
}

double C_pc_sco2::get_cold_startup_time()
{
	throw(C_csp_exception("C_pc_sco2::get_cold_startup_time() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();
}
double C_pc_sco2::get_warm_startup_time()
{
	throw(C_csp_exception("C_pc_sco2::get_warm_startup_time() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();
}
double C_pc_sco2::get_hot_startup_time()
{
	throw(C_csp_exception("C_pc_sco2::get_hot_startup_time() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();
}
double C_pc_sco2::get_standby_energy_requirement()
{
	throw(C_csp_exception("C_pc_sco2::get_standby_energy_requirement() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();	//[MWt]
}
double C_pc_sco2::get_cold_startup_energy(double step /*sec*/)
{
	throw(C_csp_exception("C_pc_sco2::get_cold_startup_energy() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();	//[MWh]
}
double C_pc_sco2::get_warm_startup_energy(double step /*sec*/)
{
	throw(C_csp_exception("C_pc_sco2::get_warm_startup_energy() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();	//[MWh]
}
double C_pc_sco2::get_hot_startup_energy(double step /*sec*/)
{
	throw(C_csp_exception("C_pc_sco2::get_hot_startup_energy() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();	//[MWh]
}
double C_pc_sco2::get_max_thermal_power()
{
	throw(C_csp_exception("C_pc_sco2::get_max_thermal_power() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();	//[MW]
}
double C_pc_sco2::get_min_thermal_power()
{
	throw(C_csp_exception("C_pc_sco2::get_min_thermal_power() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();	//[MW]
}
double C_pc_sco2::get_efficiency_at_TPH(double T_degC, double P_atm, double relhum_pct)
{
	throw(C_csp_exception("C_pc_sco2::get_efficiency_at_TPH() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();
}
double C_pc_sco2::get_efficiency_at_load(double load_frac)
{
	throw(C_csp_exception("C_pc_sco2::get_efficiency_at_load() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();
}

// This can vary between timesteps for Type224, depending on remaining startup energy and time
double C_pc_sco2::get_max_q_pc_startup()
{
	throw(C_csp_exception("C_pc_sco2::get_max_q_pc_startup() is not complete"));

	return std::numeric_limits<double>::quiet_NaN();	//[MWt]
}

void C_pc_sco2::call(const C_csp_weatherreader::S_outputs &weather,
	C_csp_solver_htf_1state &htf_state_in,
	const C_csp_power_cycle::S_control_inputs &inputs,
	C_csp_power_cycle::S_csp_pc_out_solver &out_solver,
	C_csp_power_cycle::S_csp_pc_out_report &out_report,
	const C_csp_solver_sim_info &sim_info)
{
	throw(C_csp_exception("C_pc_sco2::call() is not complete"));
}

void C_pc_sco2::converged()
{
	throw(C_csp_exception("C_pc_sco2::converged() is not complete"));
}