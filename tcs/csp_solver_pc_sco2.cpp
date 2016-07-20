#include "csp_solver_pc_sco2.h"
#include "csp_solver_core.h"

#include "htf_props.h"

C_pc_sco2::C_pc_sco2()
{
	m_startup_energy_required = 
		m_startup_time_remain_prev = m_startup_energy_remain_prev =
		m_startup_time_remain_calc = m_startup_energy_remain_calc = std::numeric_limits<double>::quiet_NaN();

	m_standby_control_prev = m_standby_control_calc = -1;
}

void C_pc_sco2::init(C_csp_power_cycle::S_solved_params &solved_params)
{
	// Call the sCO2 Recompression Cycle class to design the cycle
	mc_sco2_recomp.design(ms_params.ms_mc_sco2_recomp_params);

	
	// Set solved paramaters and calculate timestep dependent information
	solved_params.m_W_dot_des = mc_sco2_recomp.get_design_solved()->ms_rc_cycle_solved.m_W_dot_net / 1.E3;	//[MWe] convert from kWe
	solved_params.m_eta_des = mc_sco2_recomp.get_design_solved()->ms_rc_cycle_solved.m_eta_thermal;			//[-]
	solved_params.m_q_dot_des = solved_params.m_W_dot_des / solved_params.m_eta_des;						//[MWt]
	
	// Calculate the startup energy needed
	m_startup_energy_required = ms_params.m_startup_frac*solved_params.m_q_dot_des*1.E3;			//[kWt-hr]	
	solved_params.m_q_startup = m_startup_energy_required/1.E3;										//[MWt-hr]
	solved_params.m_max_frac = ms_params.m_cycle_max_frac;					//[-]
	solved_params.m_cutoff_frac = ms_params.m_cycle_cutoff_frac;			//[-]
	solved_params.m_sb_frac = ms_params.m_q_sby_frac;						//[-]
	solved_params.m_T_htf_hot_ref = ms_params.ms_mc_sco2_recomp_params.m_T_htf_hot_in - 273.15;	//[C]
	solved_params.m_m_dot_design = mc_sco2_recomp.get_phx_des_par()->m_m_dot_hot_des*3600.0;	//[kg/hr]

	// Finally, set member model-timestep-tracking variables
	m_standby_control_prev = OFF;			// Assume power cycle is off when simulation begins
	m_startup_energy_remain_prev = m_startup_energy_required;		//[kWt-hr]
	m_startup_time_remain_prev = ms_params.m_startup_time;			//[hr]

}

int C_pc_sco2::get_operating_state()
{
	if( ms_params.m_startup_frac == 0.0 && ms_params.m_startup_time == 0.0 )
	{
		return C_csp_power_cycle::ON;
	}

	return m_standby_control_prev;
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