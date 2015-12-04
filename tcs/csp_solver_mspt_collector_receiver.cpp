#include "csp_solver_mspt_collector_receiver.h"
#include <algorithm>

C_csp_mspt_collector_receiver::C_csp_mspt_collector_receiver(C_pt_heliostatfield & pt_heliostatfield,
	C_mspt_receiver_222 & mspt_receiver_222)
{
	mc_pt_heliostatfield = pt_heliostatfield;
	mc_mspt_receiver_222 = mspt_receiver_222;
}

C_csp_mspt_collector_receiver::~C_csp_mspt_collector_receiver()
{}

void C_csp_mspt_collector_receiver::init()
{
	mc_pt_heliostatfield.init();
	mc_mspt_receiver_222.init();
	return;
}

int C_csp_mspt_collector_receiver::get_operating_state()
{
	return mc_mspt_receiver_222.get_operating_state();
}

double C_csp_mspt_collector_receiver::get_startup_time()
{
    return mc_mspt_receiver_222.m_rec_su_delay * 3600.; //sec   
}

double C_csp_mspt_collector_receiver::get_startup_energy(double step /*sec*/) //MWh
{
    return mc_mspt_receiver_222.m_rec_qf_delay * step/3600. * mc_mspt_receiver_222.m_q_rec_des*1.e-6;
}

double C_csp_mspt_collector_receiver::get_pumping_parasitic_coef()  //MWe/MWt
{
    HTFProperties *htf = mc_mspt_receiver_222.get_htf_property_object();

    C_mspt_receiver_222 *R = &mc_mspt_receiver_222;

    double Tavg = (R->m_T_htf_cold_des + R->m_T_htf_hot_des)/2.;

    double mu_coolant = htf->visc(Tavg);					//[kg/m-s] Absolute viscosity of the coolant
	double k_coolant = htf->cond(Tavg);					//[W/m-K] Conductivity of the coolant
	double rho_coolant = htf->dens(Tavg, 1.0);			//[kg/m^3] Density of the coolant
    double c_p_coolant = htf->Cp(Tavg)*1e3;                 //[J/kg-K] Specific heat

    double m_dot_salt = R->m_q_rec_des / (c_p_coolant * (R->m_T_htf_hot_des - R->m_T_htf_cold_des) );

	double n_t = (int)(CSP::pi*R->m_d_rec / (R->m_od_tube*R->m_n_panels));	// The number of tubes per panel, as a function of the number of panels and the desired diameter of the receiver
	double id_tube = R->m_od_tube - 2 * R->m_th_tube;			//[m] Inner diameter of receiver tube


	double u_coolant = m_dot_salt / (n_t*rho_coolant*pow((id_tube / 2.0), 2)*CSP::pi);	//[m/s] Average velocity of the coolant through the receiver tubes
	double Re_inner = rho_coolant*u_coolant*id_tube / mu_coolant;				//[-] Reynolds number of internal flow
	double Pr_inner = c_p_coolant*mu_coolant / k_coolant;							//[-] Prandtl number of internal flow
	double Nusselt_t, f;
    double LoverD = R->m_h_rec / id_tube;
	double RelRough = (4.5e-5) / id_tube;	//[-] Relative roughness of the tubes. http:www.efunda.com/formulae/fluids/roughness.cfm
	CSP::PipeFlow(Re_inner, Pr_inner, LoverD, RelRough, Nusselt_t, f);

    double deltap, wdot;
    mc_mspt_receiver_222.calc_pump_performance(rho_coolant, m_dot_salt, f, deltap, wdot );

    return wdot / R->m_q_rec_des;

}

double C_csp_mspt_collector_receiver::get_min_power_delivery()    //MWt
{
    return mc_mspt_receiver_222.m_f_rec_min * mc_mspt_receiver_222.m_q_rec_des*1.e-6;
}

void C_csp_mspt_collector_receiver::get_design_parameters(C_csp_collector_receiver::S_csp_cr_solved_params & solved_params)
{
	solved_params.m_T_htf_cold_des = mc_mspt_receiver_222.m_T_htf_cold_des;
	solved_params.m_q_dot_rec_on_min = mc_mspt_receiver_222.m_q_rec_min/1.E6;	//[MW]
	solved_params.m_q_dot_rec_des = mc_mspt_receiver_222.m_q_rec_des/1.E6;		//[MW]
}

void C_csp_mspt_collector_receiver::call(const C_csp_weatherreader::S_outputs &weather,
	C_csp_solver_htf_state &htf_state,
	const C_csp_collector_receiver::S_csp_cr_inputs &inputs,
	C_csp_collector_receiver::S_csp_cr_outputs &cr_outputs,
	const C_csp_solver_sim_info &sim_info)
{
	// What about catching errors here?
	
	// First call heliostat field class: 'csp_solver_pt_heliostat'
	// Then use its outputs as inputs to receiver class: 'csp_solver_mspt_receiver_222'

	// Set heliostat field call() parameters and solve
	double heliostat_field_control = inputs.m_field_control;
	mc_pt_heliostatfield.call(weather, heliostat_field_control, sim_info);

	// Get heliostat field outputs and set corresponding receiver inputs
	C_mspt_receiver_222::S_inputs receiver_inputs;
	receiver_inputs.m_field_eff = mc_pt_heliostatfield.ms_outputs.m_eta_field;
	receiver_inputs.m_input_operation_mode = inputs.m_input_operation_mode;
	receiver_inputs.m_flux_map_input = &mc_pt_heliostatfield.ms_outputs.m_flux_map_out;
	mc_mspt_receiver_222.call(weather, htf_state, receiver_inputs, sim_info);
		
	// Set collector/receiver parent class outputs and return
	cr_outputs.m_eta_field = mc_pt_heliostatfield.ms_outputs.m_eta_field;				//[-]
	cr_outputs.m_q_dot_field_inc = mc_pt_heliostatfield.ms_outputs.m_q_dot_field_inc;	//[MWt]

	cr_outputs.m_eta_thermal = mc_mspt_receiver_222.ms_outputs.m_eta_therm;				//[-]
	cr_outputs.m_q_thermal = mc_mspt_receiver_222.ms_outputs.m_Q_thermal;				//[MW]
	cr_outputs.m_q_startup = mc_mspt_receiver_222.ms_outputs.m_q_startup;				//[MWt-hr]
	cr_outputs.m_q_dot_piping_loss = mc_mspt_receiver_222.ms_outputs.m_q_dot_piping_loss;	//[MWt]
	cr_outputs.m_m_dot_salt_tot = mc_mspt_receiver_222.ms_outputs.m_m_dot_salt_tot;		//[kg/hr]
	cr_outputs.m_T_salt_hot = mc_mspt_receiver_222.ms_outputs.m_T_salt_hot;				//[C]
	cr_outputs.m_W_dot_htf_pump = mc_mspt_receiver_222.ms_outputs.m_W_dot_pump;			//[MWe]
	cr_outputs.m_W_dot_col_tracking = mc_pt_heliostatfield.ms_outputs.m_pparasi;		//[MWe]

	cr_outputs.m_time_required_su = mc_mspt_receiver_222.ms_outputs.m_time_required_su;	//[s]
}


double C_csp_mspt_collector_receiver::calculate_optical_efficiency( const C_csp_weatherreader::S_outputs &weather, const C_csp_solver_sim_info &sim )
{
    /*
    Evaluate optical efficiency. This is a required function for the parent class, 
    but doesn't do much other than simply call the optical efficiency model in this case.
    */

    mc_pt_heliostatfield.call(weather, 1., sim );

    return mc_pt_heliostatfield.ms_outputs.m_eta_field;
}

double C_csp_mspt_collector_receiver::get_collector_area()
{
    //C_pt_heliostatfield::S_params *p = &mc_pt_heliostatfield.ms_params;

    //return p->m_dens_mirror * p->m_helio_height * p->m_helio_width * (double)p->m_helio_positions.nrows();

    return mc_mspt_receiver_222.m_A_sf;
}

double C_csp_mspt_collector_receiver::calculate_thermal_efficiency_approx( const C_csp_weatherreader::S_outputs &weather, double q_inc )
{
    /* 
    A very approximate thermal efficiency used for quick optimization performance projections
    */

    double T_eff = (mc_mspt_receiver_222.m_T_htf_cold_des + mc_mspt_receiver_222.m_T_htf_hot_des)*.55;

    double T_amb = weather.m_tdry + 273.15;
    double T_eff4 = T_eff * T_eff;
    T_eff4 *= T_eff4;
    double T_amb4 = T_amb * T_amb;
    T_amb4 *= T_amb4;

    double Arec = mc_mspt_receiver_222.m_d_rec * 3.1415 * mc_mspt_receiver_222.m_h_rec;

    double q_rad = 5.67e-8*mc_mspt_receiver_222.m_epsilon * Arec * (T_eff4 - T_amb4) * 1.e-6;   //MWt

    double v = weather.m_wspd;
    double v2 = v*v;
    double v3 = v2*v;

    double q_conv = q_rad/2. * (-0.001129*v3 + 0.031229*v2 - 0.01822*v +0.962476);  //convection is about half radiation, scale by wind speed. surrogate regression from molten salt run.

    return max(1. - (q_rad + q_conv)/q_inc, 0.);

}


void C_csp_mspt_collector_receiver::converged()
{
	mc_pt_heliostatfield.converged();
	mc_mspt_receiver_222.converged();
}