#include "csp_solver_fresnel_collector_receiver.h"

C_csp_fresnel_collector_receiver::C_csp_fresnel_collector_receiver()
{
    int x = 0;
}

void C_csp_fresnel_collector_receiver::init(const C_csp_collector_receiver::S_csp_cr_init_inputs init_inputs,
    C_csp_collector_receiver::S_csp_cr_solved_params& solved_params)
{

    
    return;
}


bool C_csp_fresnel_collector_receiver::init_fieldgeom()
{
    return true;
}


C_csp_collector_receiver::E_csp_cr_modes C_csp_fresnel_collector_receiver::get_operating_state()
{
    return E_csp_cr_modes::OFF;	//[-]
}


double C_csp_fresnel_collector_receiver::get_startup_time()
{
    return 3600;    
}
double C_csp_fresnel_collector_receiver::get_startup_energy()
{
    return 1.e-6;
}
double C_csp_fresnel_collector_receiver::get_pumping_parasitic_coef()
{
    return 1.e-6;

}

double C_csp_fresnel_collector_receiver::get_min_power_delivery()
{
    return 0;
}

double C_csp_fresnel_collector_receiver::get_max_power_delivery(double T_cold_in /*C*/)
{
    return 0;
}

double C_csp_fresnel_collector_receiver::get_tracking_power()
{
    return 0;
}

double C_csp_fresnel_collector_receiver::get_col_startup_power()
{
    return 0;
}


void C_csp_fresnel_collector_receiver::get_design_parameters(C_csp_collector_receiver::S_csp_cr_solved_params& solved_params)
{
    return;
}


void C_csp_fresnel_collector_receiver::off(const C_csp_weatherreader::S_outputs& weather,
    const C_csp_solver_htf_1state& htf_state_in,
    C_csp_collector_receiver::S_csp_cr_out_solver& cr_out_solver,
    const C_csp_solver_sim_info& sim_info)
{
    
    return;
}

void C_csp_fresnel_collector_receiver::startup(const C_csp_weatherreader::S_outputs& weather,
    const C_csp_solver_htf_1state& htf_state_in,
    C_csp_collector_receiver::S_csp_cr_out_solver& cr_out_solver,
    const C_csp_solver_sim_info& sim_info)
{
}



void C_csp_fresnel_collector_receiver::on(const C_csp_weatherreader::S_outputs& weather,
    const C_csp_solver_htf_1state& htf_state_in,
    double q_dot_elec_to_CR_heat /*MWt*/, double field_control,
    C_csp_collector_receiver::S_csp_cr_out_solver& cr_out_solver,
    //C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
    const C_csp_solver_sim_info& sim_info)
{
    return;
}

void C_csp_fresnel_collector_receiver::steady_state(const C_csp_weatherreader::S_outputs& weather,
    const C_csp_solver_htf_1state& htf_state_in,
    double W_dot_elec_to_CR_heat /*MWe*/, double field_control,
    C_csp_collector_receiver::S_csp_cr_out_solver& cr_out_solver,
    const C_csp_solver_sim_info& sim_info)
{
    return;
}


void C_csp_fresnel_collector_receiver::estimates(const C_csp_weatherreader::S_outputs& weather,
    const C_csp_solver_htf_1state& htf_state_in,
    C_csp_collector_receiver::S_csp_cr_est_out& est_out,
    const C_csp_solver_sim_info& sim_info)
{
    return;
}


void C_csp_fresnel_collector_receiver::converged()
{
    return;
}

void C_csp_fresnel_collector_receiver::write_output_intervals(double report_time_start,
    const std::vector<double>& v_temp_ts_time_end, double report_time_end)
{
}

double C_csp_fresnel_collector_receiver::calculate_optical_efficiency(const C_csp_weatherreader::S_outputs& weather, const C_csp_solver_sim_info& sim)
{
    return 0;
}

double C_csp_fresnel_collector_receiver::calculate_thermal_efficiency_approx(const C_csp_weatherreader::S_outputs& weather, double q_incident /*MW*/)
{
    return 0;
}

double C_csp_fresnel_collector_receiver::get_collector_area()
{
    return 0;
}

