/*
BSD 3-Clause License

Copyright (c) Alliance for Sustainable Energy, LLC. See also https://github.com/NREL/ssc/blob/develop/LICENSE
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "csp_solver_falling_particle_collector_receiver.h"
#include "sam_csp_util.h"
#include <algorithm>

static C_csp_reported_outputs::S_output_info S_output_info[] =
{
	{C_csp_falling_particle_collector_receiver::E_FIELD_Q_DOT_INC, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_falling_particle_collector_receiver::E_FIELD_ETA_OPT, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_falling_particle_collector_receiver::E_FIELD_ADJUST, C_csp_reported_outputs::TS_WEIGHTED_AVE},
    {C_csp_falling_particle_collector_receiver::E_IS_FIELD_TRACKING_FINAL, C_csp_reported_outputs::TS_LAST},

    {C_csp_falling_particle_collector_receiver::E_REC_DEFOCUS, C_csp_reported_outputs::TS_WEIGHTED_AVE},
    {C_csp_falling_particle_collector_receiver::E_Q_DOT_INC, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_falling_particle_collector_receiver::E_ETA_THERMAL, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_falling_particle_collector_receiver::E_Q_DOT_THERMAL, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_falling_particle_collector_receiver::E_M_DOT_HTF, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_falling_particle_collector_receiver::E_Q_DOT_STARTUP, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_falling_particle_collector_receiver::E_T_HTF_IN, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_falling_particle_collector_receiver::E_T_HTF_OUT, C_csp_reported_outputs::TS_WEIGHTED_AVE},
    {C_csp_falling_particle_collector_receiver::E_T_CAV_WALL_MAX, C_csp_reported_outputs::TS_WEIGHTED_AVE},
    {C_csp_falling_particle_collector_receiver::E_T_CAV_WALL_AVG, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_csp_falling_particle_collector_receiver::E_Q_DOT_PIPE_LOSS, C_csp_reported_outputs::TS_WEIGHTED_AVE},
    {C_csp_falling_particle_collector_receiver::E_Q_DOT_LOSS, C_csp_reported_outputs::TS_WEIGHTED_AVE},
    {C_csp_falling_particle_collector_receiver::E_Q_DOT_REFL_LOSS, C_csp_reported_outputs::TS_WEIGHTED_AVE},
    {C_csp_falling_particle_collector_receiver::E_W_DOT_TRACKING, C_csp_reported_outputs::TS_WEIGHTED_AVE},
    {C_csp_falling_particle_collector_receiver::E_W_DOT_PUMP, C_csp_reported_outputs::TS_WEIGHTED_AVE},

	{ C_csp_falling_particle_collector_receiver::E_CLEARSKY, C_csp_reported_outputs::TS_WEIGHTED_AVE },
    { C_csp_falling_particle_collector_receiver::E_REC_OP_MODE_FINAL, C_csp_reported_outputs::TS_LAST},
    { C_csp_falling_particle_collector_receiver::E_REC_STARTUP_TIME_REMAIN_FINAL, C_csp_reported_outputs::TS_LAST },
    { C_csp_falling_particle_collector_receiver::E_REC_STARTUP_ENERGY_REMAIN_FINAL, C_csp_reported_outputs::TS_LAST },

    // Outputs for individual receivers
    {C_csp_falling_particle_collector_receiver::E_Q_DOT_INC_1, C_csp_reported_outputs::TS_WEIGHTED_AVE},
    {C_csp_falling_particle_collector_receiver::E_Q_DOT_INC_2, C_csp_reported_outputs::TS_WEIGHTED_AVE},
    {C_csp_falling_particle_collector_receiver::E_Q_DOT_INC_3, C_csp_reported_outputs::TS_WEIGHTED_AVE},
    {C_csp_falling_particle_collector_receiver::E_Q_DOT_INC_4, C_csp_reported_outputs::TS_WEIGHTED_AVE},
    {C_csp_falling_particle_collector_receiver::E_Q_DOT_THERMAL_1, C_csp_reported_outputs::TS_WEIGHTED_AVE},
    {C_csp_falling_particle_collector_receiver::E_Q_DOT_THERMAL_2, C_csp_reported_outputs::TS_WEIGHTED_AVE},
    {C_csp_falling_particle_collector_receiver::E_Q_DOT_THERMAL_3, C_csp_reported_outputs::TS_WEIGHTED_AVE},
    {C_csp_falling_particle_collector_receiver::E_Q_DOT_THERMAL_4, C_csp_reported_outputs::TS_WEIGHTED_AVE},
    {C_csp_falling_particle_collector_receiver::E_ETA_THERMAL_1, C_csp_reported_outputs::TS_WEIGHTED_AVE},
    {C_csp_falling_particle_collector_receiver::E_ETA_THERMAL_2, C_csp_reported_outputs::TS_WEIGHTED_AVE},
    {C_csp_falling_particle_collector_receiver::E_ETA_THERMAL_3, C_csp_reported_outputs::TS_WEIGHTED_AVE},
    {C_csp_falling_particle_collector_receiver::E_ETA_THERMAL_4, C_csp_reported_outputs::TS_WEIGHTED_AVE},
    {C_csp_falling_particle_collector_receiver::E_FIELD_ETA_OPT_1, C_csp_reported_outputs::TS_WEIGHTED_AVE},
    {C_csp_falling_particle_collector_receiver::E_FIELD_ETA_OPT_2, C_csp_reported_outputs::TS_WEIGHTED_AVE},
    {C_csp_falling_particle_collector_receiver::E_FIELD_ETA_OPT_3, C_csp_reported_outputs::TS_WEIGHTED_AVE},
    {C_csp_falling_particle_collector_receiver::E_FIELD_ETA_OPT_4, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	csp_info_invalid	
};

C_csp_falling_particle_collector_receiver::C_csp_falling_particle_collector_receiver(std::vector<C_pt_sf_perf_interp*> pt_heliostatfields,
    std::vector<std::shared_ptr<C_pt_receiver>> pt_receivers):
	mc_pt_heliostatfields(pt_heliostatfields),
	mc_pt_receivers(pt_receivers)
{
	mc_reported_outputs.construct(S_output_info);
    m_fix_mode_from_estimates = true;
    m_fixed_mode_mflow_method = 0;  // TODO: Hard-coded here and in falling particle receiver class for now
}

C_csp_falling_particle_collector_receiver::~C_csp_falling_particle_collector_receiver()
{}

void C_csp_falling_particle_collector_receiver::init(const C_csp_collector_receiver::S_csp_cr_init_inputs init_inputs,
				C_csp_collector_receiver::S_csp_cr_solved_params & solved_params)
{
    double qdes_total = 0.0;
    double A_aper_total = 0.0;
    for (size_t i = 0; i < mc_pt_receivers.size(); i++)
    {
        mc_pt_heliostatfields.at(i)->init();
        mc_pt_receivers.at(i)->init();
        qdes_total += mc_pt_receivers.at(i)->get_q_dot_rec_des();
        A_aper_total += mc_pt_heliostatfields.at(i)->ms_params.m_A_sf;
    }
    solved_params.m_T_htf_cold_des = mc_pt_receivers.at(0)->get_T_htf_cold_des();  //[K] 
    solved_params.m_q_dot_rec_des = qdes_total;   		                          //[MW]
    solved_params.m_A_aper_total = A_aper_total;	                              //[m^2]

    size_t nrec = mc_pt_receivers.size();
    m_qthermal_from_estimates.resize(nrec);
    m_mdot_from_estimates.resize(nrec);
    m_is_on_from_estimates.resize(nrec);
    m_n_on_from_estimates = 0;
    m_approx_min_focus.resize(nrec);
    m_approx_min_focus_at_T.resize(nrec);
    std::fill(m_approx_min_focus.begin(), m_approx_min_focus.end(), 0.0);
    std::fill(m_approx_min_focus_at_T.begin(), m_approx_min_focus_at_T.end(), 0.0);

	return;
}

C_csp_collector_receiver::E_csp_cr_modes C_csp_falling_particle_collector_receiver::get_operating_state()
{
    // Report ON if any receiver is ON 
    // Report STARTUP if no receiver is ON, but at least one receiver is starting up
    // Report OFF_NO_SU_REQ if no receiver is ON and any receiver has no startup requirements (all receivers are assumed to have the same startup requirements)
    // Report OFF otherwise

    C_csp_collector_receiver::E_csp_cr_modes op_mode_per_rec, op_mode;
    op_mode = E_csp_cr_modes::OFF;
    for (size_t i = 0; i < mc_pt_receivers.size(); i++)
    {
        op_mode_per_rec = mc_pt_receivers.at(i)->get_operating_state();  // Individual receiver operating state
        if (op_mode_per_rec == E_csp_cr_modes::ON || op_mode_per_rec == E_csp_cr_modes::STEADY_STATE) 
            op_mode = op_mode_per_rec;
        else if (op_mode_per_rec == E_csp_cr_modes::STARTUP && op_mode != E_csp_cr_modes::ON)
            op_mode = E_csp_cr_modes::STARTUP;
        else if (op_mode_per_rec == E_csp_cr_modes::OFF_NO_SU_REQ && op_mode != E_csp_cr_modes::ON) 
            op_mode = E_csp_cr_modes::OFF_NO_SU_REQ;
    }
	return op_mode;
}

double C_csp_falling_particle_collector_receiver::get_startup_time()
{
    // Used by the dispatch model for startup time approximation.
    // Report the minimum startup time for any receiver 
    double startup_time = 1e10;;
    for (size_t i = 0; i < mc_pt_receivers.size(); i++)
    {
        startup_time = fmin(startup_time, mc_pt_receivers.at(i)->get_startup_time());
    }
    return startup_time;   //[s]
}

double C_csp_falling_particle_collector_receiver::get_startup_energy()
{
    // Used by the dispatch model for startup energy approximation.
    // Report the minimum startup energy for any receiver 
    double startup_energy = 1e10;;
    for (size_t i = 0; i < mc_pt_receivers.size(); i++)
    {
        startup_energy = fmin(startup_energy, mc_pt_receivers.at(i)->get_startup_energy());
    }
    return startup_energy; //[MWh]
}

double C_csp_falling_particle_collector_receiver::get_pumping_parasitic_coef()  //MWe/MWt
{
    // Parasitic coefficient (MWe / MWt) used in the dispatch model
    double lift_power = 0.0;
    double thermal_power = 0.0;
    for (size_t i = 0; i < mc_pt_receivers.size(); i++)
    {
        lift_power += mc_pt_receivers.at(i)->get_pumping_parasitic_coef() * mc_pt_receivers.at(i)->get_q_dot_rec_des(); //[MWe]
        thermal_power += mc_pt_receivers.at(i)->get_q_dot_rec_des(); //[MWt]
    }
    return lift_power / thermal_power;
}

double C_csp_falling_particle_collector_receiver::get_min_power_delivery()    //MWt
{
    // Minimum (aggregated) receiver thermal output used by the dispatch model
    double min_power = 0.0;
    for (size_t i = 0; i < mc_pt_receivers.size(); i++)
    {  
        min_power += mc_pt_receivers.at(i)->get_min_power_delivery(); //[MWt] 
    }
    return min_power; 
}

double C_csp_falling_particle_collector_receiver::get_max_power_delivery(double T_htf_cold_in /*C*/)    //MWt
{
    // Maximum (aggregated) receiver thermal output used by the dispatch model
    double max_power = 0.0;
    for (size_t i = 0; i < mc_pt_receivers.size(); i++)
    {
        max_power += mc_pt_receivers.at(i)->get_max_power_delivery(); //[MWt] 
    }
    return max_power; 
}

double C_csp_falling_particle_collector_receiver::get_tracking_power()
{
    double ptrack = 0.0;
    for (size_t i = 0; i < mc_pt_receivers.size(); i++)
    {
        ptrack += mc_pt_heliostatfields.at(i)->ms_params.m_p_track * mc_pt_heliostatfields.at(i)->ms_params.m_N_hel * 1.e-3;	//MWe
    }
    return ptrack;  //MWe
}

double C_csp_falling_particle_collector_receiver::get_col_startup_power()
{
    double pstart = 0.0;
    for (size_t i = 0; i < mc_pt_receivers.size(); i++)
    {
        pstart += mc_pt_heliostatfields.at(i)->ms_params.m_p_start * mc_pt_heliostatfields.at(i)->ms_params.m_N_hel * 1.e-3;	//MWe-hr
    }
    return pstart;  //MWe-hr
}


void C_csp_falling_particle_collector_receiver::combine_results()
{
    // Combine results from the individual field/receivers into aggregated values for the controller and for reporting
    m_combined_outputs.clear();  // Reset all values to zero
    size_t nrec = mc_pt_receivers.size();
    int n_rec_on = 0;
    for (size_t i = 0; i < nrec; i++)
    {
        m_combined_outputs.q_dot_field_inc += mc_pt_heliostatfields.at(i)->ms_outputs.m_q_dot_field_inc;     // [MWt], total solar power incident on all fields
        m_combined_outputs.q_dot_rec_inc += mc_pt_receivers.at(i)->ms_outputs.m_q_dot_rec_inc;               // [MWt], total solar power incident on all receivers
        m_combined_outputs.q_thermal += mc_pt_receivers.at(i)->ms_outputs.m_Q_thermal;	                     // [MWt], total thermal power to particles over all receivers (currently defined without including transport loss)
        m_combined_outputs.q_dot_transport_loss += mc_pt_receivers.at(i)->ms_outputs.m_q_dot_piping_loss;    // [MWt], total thermal loss for particle transport over all receivers
        m_combined_outputs.q_dot_refl_loss += mc_pt_receivers.at(i)->ms_outputs.m_q_dot_refl_loss;           // [MWt], total solar reflection loss over all receivers
        m_combined_outputs.q_dot_thermal_loss += mc_pt_receivers.at(i)->ms_outputs.m_q_rad_sum + mc_pt_receivers.at(i)->ms_outputs.m_q_conv_sum;   // [MWt], total convection and IR emission loss over all receivers

        m_combined_outputs.q_startup += mc_pt_receivers.at(i)->ms_outputs.m_q_startup;                                                                          // [MWt-hr], total startup thermal energy over all receivers
        m_combined_outputs.q_dot_startup += mc_pt_receivers.at(i)->ms_outputs.m_q_startup / (mc_pt_receivers.at(i)->ms_outputs.m_time_required_su / 3600.0);    // [MWt], total startup thermal power over all receivers  TODO: What if not all receivers are starting up?
        m_combined_outputs.time_required_su += fmax(m_combined_outputs.time_required_su, mc_pt_receivers.at(i)->ms_outputs.m_time_required_su);                 // [s], max startup time required for any single receiver

        m_combined_outputs.eta_field += mc_pt_heliostatfields.at(i)->ms_outputs.m_eta_field * mc_pt_heliostatfields.at(i)->ms_outputs.m_q_dot_field_inc;        // [-] Weighted-average efficiency over fields. TODO: This will only be the average of fields that are operating, should a lower efficiency be reported if an individual field isn't operating?
        m_combined_outputs.eta_thermal += mc_pt_receivers.at(i)->ms_outputs.m_eta_therm * mc_pt_receivers.at(i)->ms_outputs.m_q_dot_rec_inc;                    // [-] Weighted-average over receivers. TODO: This will only be the average of receivers that are operating, should a lower efficiency be reported if an individual field isn't operating?

        m_combined_outputs.m_dot_salt_tot += mc_pt_receivers.at(i)->ms_outputs.m_m_dot_salt_tot;	                                              // [kg/hr], total mass flow over all receivers
        m_combined_outputs.T_salt_hot += mc_pt_receivers.at(i)->ms_outputs.m_m_dot_salt_tot * mc_pt_receivers.at(i)->ms_outputs.m_T_salt_hot;     // [C] Mass-weighted exit average over all receivers
        m_combined_outputs.T_salt_cold += mc_pt_receivers.at(i)->ms_outputs.m_m_dot_salt_tot * mc_pt_receivers.at(i)->ms_outputs.m_T_salt_cold;   // [C] Mass-weighted exit average over all receivers, but all receivers have same inlet T

        m_combined_outputs.sf_adjust += mc_pt_heliostatfields.at(i)->ms_outputs.m_sf_adjust_out;                 // [-] Average over fields  TODO: Does this need to be weighted by field output?
        m_combined_outputs.component_defocus += mc_pt_receivers.at(i)->ms_outputs.m_component_defocus/nrec;      // [-] Average receiver defocus  TODO: Figure out a better way to combine this

        m_combined_outputs.W_dot_tracking += mc_pt_heliostatfields.at(i)->ms_outputs.m_pparasi;                  // [MWe], total parasitic load for all heliostat fields
        m_combined_outputs.W_dot_transport += mc_pt_receivers.at(i)->ms_outputs.m_W_dot_pump;                    // [MWe], total particle lift parasitics

        if (mc_pt_receivers.at(i)->ms_outputs.m_Q_thermal > 0)
        {
            n_rec_on += 1;
            m_combined_outputs.T_cav_wall_max = fmax(m_combined_outputs.T_cav_wall_max, mc_pt_receivers.at(i)->ms_outputs.m_max_T_cav_wall);
            m_combined_outputs.T_cav_wall_avg += mc_pt_receivers.at(i)->ms_outputs.m_avg_T_cav_wall;
        }

    }

    m_combined_outputs.T_salt_hot /= fmax(1e-6, m_combined_outputs.m_dot_salt_tot);         //[C]
    m_combined_outputs.T_salt_cold /= fmax(1e-6, m_combined_outputs.m_dot_salt_tot);        //[C]  
    m_combined_outputs.eta_field /= fmax(1e-6, m_combined_outputs.q_dot_field_inc);         //[-]  TODO: Would a simple average be better to account for receivers that are off?
    m_combined_outputs.eta_thermal /= fmax(1e-6, m_combined_outputs.q_dot_rec_inc);         //[-]  TODO: Would a simple average be better to account for receivers that are off?
    m_combined_outputs.W_dot_elec_in_tot = m_combined_outputs.W_dot_tracking + m_combined_outputs.W_dot_transport;
    if (n_rec_on>0)
        m_combined_outputs.T_cav_wall_avg /= n_rec_on;
    return;
}

void C_csp_falling_particle_collector_receiver::set_outputs(C_csp_collector_receiver::S_csp_cr_out_solver& cr_out_solver)
{
    // Set collector/receiver parent class outputs 
    cr_out_solver.m_q_thermal = m_combined_outputs.q_thermal;	                //[MWt]
    cr_out_solver.m_q_startup = m_combined_outputs.q_startup;				    //[MWt-hr]
    cr_out_solver.m_m_dot_salt_tot = m_combined_outputs.m_dot_salt_tot;		    //[kg/hr]
    cr_out_solver.m_T_salt_hot = m_combined_outputs.T_salt_hot;				    //[C]
    cr_out_solver.m_component_defocus = m_combined_outputs.component_defocus;	//[-]
    cr_out_solver.m_W_dot_elec_in_tot = m_combined_outputs.W_dot_elec_in_tot;   //[MWe]
    cr_out_solver.m_time_required_su = m_combined_outputs.time_required_su;	    //[s]
    cr_out_solver.m_q_dot_heater = 0.0;

    // Set reported outputs
    mc_reported_outputs.value(E_FIELD_Q_DOT_INC, m_combined_outputs.q_dot_field_inc);	    //[MWt]
    mc_reported_outputs.value(E_FIELD_ETA_OPT, m_combined_outputs.eta_field);			    //[-]
    mc_reported_outputs.value(E_FIELD_ADJUST, m_combined_outputs.sf_adjust);			    //[-]
    mc_reported_outputs.value(E_REC_DEFOCUS, m_combined_outputs.component_defocus);         //[-]
    mc_reported_outputs.value(E_Q_DOT_INC, m_combined_outputs.q_dot_rec_inc);	            //[MWt]
    mc_reported_outputs.value(E_ETA_THERMAL, m_combined_outputs.eta_thermal);		        //[-]
    mc_reported_outputs.value(E_Q_DOT_THERMAL, m_combined_outputs.q_thermal);	            //[MWt]
    mc_reported_outputs.value(E_M_DOT_HTF, m_combined_outputs.m_dot_salt_tot);	            //[kg/hr]
        // If startup, then timestep may have changed (why not report this from 222 in MWt?)
    mc_reported_outputs.value(E_Q_DOT_STARTUP, m_combined_outputs.q_dot_startup);		    //[MWt]
    mc_reported_outputs.value(E_T_HTF_IN, m_combined_outputs.T_salt_cold);			        //[C]
    mc_reported_outputs.value(E_T_HTF_OUT, m_combined_outputs.T_salt_hot);		            //[C]
    mc_reported_outputs.value(E_T_CAV_WALL_MAX, m_combined_outputs.T_cav_wall_max);			//[C]
    mc_reported_outputs.value(E_T_CAV_WALL_AVG, m_combined_outputs.T_cav_wall_avg);		    //[C]

    mc_reported_outputs.value(E_Q_DOT_PIPE_LOSS, m_combined_outputs.q_dot_transport_loss);  //[MWt]
    mc_reported_outputs.value(E_Q_DOT_REFL_LOSS, m_combined_outputs.q_dot_refl_loss);       //[MWt]
    mc_reported_outputs.value(E_Q_DOT_LOSS, m_combined_outputs.q_dot_thermal_loss);         //MWt
    mc_reported_outputs.value(E_W_DOT_TRACKING, m_combined_outputs.W_dot_tracking);         //[MWe]
    mc_reported_outputs.value(E_W_DOT_PUMP, m_combined_outputs.W_dot_transport);            //[MWe]


    mc_reported_outputs.value(E_CLEARSKY, mc_pt_heliostatfields.at(0)->ms_outputs.m_clearsky_dni);  // All fields have same clear-sky model and same clear-sky DNI

    // Individual receiver outputs
    int nrec = mc_pt_receivers.size();
    mc_reported_outputs.value(E_Q_DOT_INC_1, mc_pt_receivers.at(0)->ms_outputs.m_q_dot_rec_inc);	                //[MWt]
    mc_reported_outputs.value(E_Q_DOT_INC_2, nrec > 1 ? mc_pt_receivers.at(1)->ms_outputs.m_q_dot_rec_inc : 0.0);	//[MWt]
    mc_reported_outputs.value(E_Q_DOT_INC_3, nrec > 2 ? mc_pt_receivers.at(2)->ms_outputs.m_q_dot_rec_inc : 0.0);	//[MWt]
    mc_reported_outputs.value(E_Q_DOT_INC_4, nrec > 3 ? mc_pt_receivers.at(3)->ms_outputs.m_q_dot_rec_inc : 0.0);	//[MWt]
    mc_reported_outputs.value(E_Q_DOT_THERMAL_1, mc_pt_receivers.at(0)->ms_outputs.m_Q_thermal);	                //[MWt]
    mc_reported_outputs.value(E_Q_DOT_THERMAL_2, nrec > 1 ? mc_pt_receivers.at(1)->ms_outputs.m_Q_thermal : 0.0);	 //[MWt]
    mc_reported_outputs.value(E_Q_DOT_THERMAL_3, nrec > 2 ? mc_pt_receivers.at(2)->ms_outputs.m_Q_thermal : 0.0);	//[MWt]
    mc_reported_outputs.value(E_Q_DOT_THERMAL_4, nrec > 3 ? mc_pt_receivers.at(3)->ms_outputs.m_Q_thermal : 0.0);	//[MWt]
    mc_reported_outputs.value(E_ETA_THERMAL_1, mc_pt_receivers.at(0)->ms_outputs.m_eta_therm);
    mc_reported_outputs.value(E_ETA_THERMAL_2, nrec > 1 ? mc_pt_receivers.at(1)->ms_outputs.m_eta_therm : 0.0);
    mc_reported_outputs.value(E_ETA_THERMAL_3, nrec > 2 ? mc_pt_receivers.at(2)->ms_outputs.m_eta_therm : 0.0);
    mc_reported_outputs.value(E_ETA_THERMAL_4, nrec > 3 ? mc_pt_receivers.at(3)->ms_outputs.m_eta_therm : 0.0);
    mc_reported_outputs.value(E_FIELD_ETA_OPT_1, mc_pt_heliostatfields.at(0)->ms_outputs.m_eta_field);
    mc_reported_outputs.value(E_FIELD_ETA_OPT_2, nrec > 1 ? mc_pt_heliostatfields.at(1)->ms_outputs.m_eta_field : 0.0);
    mc_reported_outputs.value(E_FIELD_ETA_OPT_3, nrec > 2 ? mc_pt_heliostatfields.at(2)->ms_outputs.m_eta_field : 0.0);
    mc_reported_outputs.value(E_FIELD_ETA_OPT_4, nrec > 3 ? mc_pt_heliostatfields.at(3)->ms_outputs.m_eta_field : 0.0);

    return;
}

std::vector<double> C_csp_falling_particle_collector_receiver::split_focus(double avg_focus, std::vector<bool>& is_rec_on)
{
    // 'avg_defocus' = average focus fraction for receivers that are on (defined by is_rec_on).

    size_t nrec = mc_pt_receivers.size();
    std::vector<double> heliostat_field_control_per_rec(nrec, 0.0);


    int n_rec_on = 0;
    double mtot_estimates = 0.0;  // Total mass flow for subset of operating receivers from estimates (no defocus)
    for (size_t i = 0; i < nrec; i++)
    {
        if (is_rec_on.at(i))
        {
            n_rec_on += 1;
            mtot_estimates += m_mdot_from_estimates.at(i);
        }
    }

    bool is_uniform_split = false;
    if (is_uniform_split || n_rec_on == 1 || avg_focus > 0.95) // Split defocus signal uniformly between operating receivers, without accounting for potential known operational minima
    {
        for (size_t i = 0; i < nrec; i++)
            heliostat_field_control_per_rec.at(i) = is_rec_on.at(i) ? avg_focus : 0.0;
    }

    else  // Split defocus signal between operating receiver, accounting for known operational minima
    {
        bool is_uniform_split = (avg_focus < 0.5);  // Uniform initial split it likely better if we're just trying to keep receivers on, non-uniform split likely better if we're trying to keep all receivers at temperature
        double focus_min = 0.4;

        std::vector<int> low_to_high_order = order_receivers(is_rec_on);
        std::vector<bool>is_at_limit(nrec, false);
        double mtot_estimates_new = mtot_estimates;
        int n_at_low_limit = 0;
        int n_at_high_limit = 0;
        double focus_sum = 0.0;
        for (size_t j = 0; j < n_rec_on; j++)
        {
            int n_remain = n_rec_on - j;
            double avg_focus_new = (avg_focus * n_rec_on - focus_sum) / n_remain;
            int i;
            double focus;
            if (is_uniform_split)
            {
                i = low_to_high_order.at(j);  // Worst to best order (e.g. lowest to highest minimum focus)
                focus = avg_focus_new;
            }
            else
            {
                i = low_to_high_order.at(n_rec_on - 1 - j);  // Best to worst order (i.e. lowest to highest focus fraction if split based on relative mass flows)
                focus = 1.0 - (m_mdot_from_estimates.at(i) / mtot_estimates_new) * n_remain * (1.0 - avg_focus_new);
            }

            if (focus < m_approx_min_focus.at(i))   // Enforce known minimum values per receiver 
            {
                heliostat_field_control_per_rec.at(i) = m_approx_min_focus.at(i);
                is_at_limit.at(i) = true;
                n_at_low_limit += 1;
            }
            else
                heliostat_field_control_per_rec.at(i) = fmax(focus_min, fmin(focus, 1.0));
            focus_sum += heliostat_field_control_per_rec.at(i);
            mtot_estimates_new -= m_mdot_from_estimates.at(i);
        }


        // Check if applied focus fraction is equal to the target focus fraction
        double df = focus_sum - avg_focus * n_rec_on;  // Difference between applied focus fraction and target focus fraction
        double dftol = 1e-4;
        if (df > dftol && n_at_low_limit < n_rec_on)   // Need to reduce focus fraction and there are receivers not already at their lower limits
        {
            for (size_t j = 0; j < n_rec_on; j++)
            {
                int i = low_to_high_order.at(n_rec_on-1-j);
                if (!is_at_limit.at(i))
                {
                    double dfi = fmin(df, heliostat_field_control_per_rec.at(i) - m_approx_min_focus.at(i));
                    heliostat_field_control_per_rec.at(i) -= dfi;
                    df -= dfi;
                    if (df <= dftol)
                        break;
                }
            }
        }
        else if (df < -dftol)   // Need to increase focus fractions
        {
            for (size_t j = 0; j < n_rec_on; j++)
            {
                int i = low_to_high_order.at(n_rec_on - 1 - j);
                if (heliostat_field_control_per_rec.at(i)<1.0)
                {
                    double dfi = fmin(df, 1.0 - heliostat_field_control_per_rec.at(i));
                    heliostat_field_control_per_rec.at(i) += dfi;
                    df += dfi;
                    if (df >= -dftol)
                        break;
                }
            }
        }

        // If defocus split above was unsuccessful, revert back to a uniform split, even though this might violate minimum values
        if (fabs(df) >= dftol)
        {
            for (size_t i = 0; i < nrec; i++)
                heliostat_field_control_per_rec.at(i) = is_rec_on.at(i) ? avg_focus : 0.0;
        }
    }

    return heliostat_field_control_per_rec;
}


void C_csp_falling_particle_collector_receiver::update_limits(std::vector<double>& focus_fractions, std::vector<bool>& is_on, std::vector<bool>& is_at_T, double bound_tol,
    std::vector<double>& focus_min_sim_on, std::vector<double>& focus_max_sim_off,
    std::vector<double>& focus_min_sim_at_T, std::vector<double>& focus_max_sim_under_T)
{
    // Update min/max focus fractions where receiver can/can't operate and can/can't achieve exit T

    size_t nrec = mc_pt_receivers.size();
    for (size_t i = 0; i < nrec; i++)
    {
        if (is_on.at(i))
            focus_min_sim_on.at(i) = fmin(focus_min_sim_on.at(i), focus_fractions.at(i));
        else
            focus_max_sim_off.at(i) = fmax(focus_max_sim_off.at(i), focus_fractions.at(i));

        if (focus_min_sim_on.at(i) - focus_max_sim_off.at(i) < bound_tol)
        {
            // Update approximate min focus for this time step if we don't have one already, or if we know that the receiver will be off at the current value
            if (m_approx_min_focus.at(i) == 0.0 || (m_approx_min_focus.at(i) > 0.0 && m_approx_min_focus.at(i) <= focus_max_sim_off.at(i)))
                m_approx_min_focus.at(i) = focus_min_sim_on.at(i);
        }

        if (is_at_T.at(i))
            focus_min_sim_at_T.at(i) = fmin(focus_min_sim_at_T.at(i), focus_fractions.at(i));
        else
            focus_max_sim_under_T.at(i) = fmax(focus_max_sim_under_T.at(i), focus_fractions.at(i));

        if (focus_min_sim_at_T.at(i) - focus_max_sim_under_T.at(i) < bound_tol)
        {
            if (m_approx_min_focus_at_T.at(i) == 0.0 || (m_approx_min_focus_at_T.at(i) > 0.0 && m_approx_min_focus_at_T.at(i) <= focus_max_sim_under_T.at(i)))
                m_approx_min_focus_at_T.at(i) = focus_min_sim_at_T.at(i);
        }
    }
    return;
}

std::vector<int> C_csp_falling_particle_collector_receiver::order_receivers(std::vector<bool>& is_rec_on)
{
    std::vector<int> low_to_high_order(0);
    for (size_t i = 0; i < mc_pt_receivers.size(); i++)
    {
        if (is_rec_on.at(i))
        {
            size_t n = low_to_high_order.size();
            for (size_t j = 0; j < n; j++)
            {
                int k = low_to_high_order.at(j);
                if (m_mdot_from_estimates.at(i) < m_mdot_from_estimates.at(k))
                {
                    low_to_high_order.insert(low_to_high_order.begin() + j, i);
                    break;
                }
            }
            if (low_to_high_order.size() == n)
                low_to_high_order.push_back(i);
        }
    }
    return low_to_high_order;
}



bool C_csp_falling_particle_collector_receiver::update_focus_split(std::vector<double>& focus_fractions, double focus_step, std::vector<int>& low_to_high_order,
    std::vector<bool>& is_expect_on, std::vector<bool>& is_on, std::vector<bool>& is_at_T,
    std::vector<double>& focus_min_sim_on, std::vector<double>& focus_max_sim_off,
    std::vector<double>& focus_min_sim_at_T, std::vector<double>& focus_max_sim_under_T)
{
    bool skip_to_next_iter = false;

    //--- Count number of receivers in each state
    size_t nrec = mc_pt_receivers.size();
    int n_should_operate = 0;
    int n_on = 0;
    int n_at_target_T = 0;
    for (size_t i = 0; i < nrec; i++)
    {
        if (is_expect_on.at(i))
            n_should_operate += 1;
        if (is_on.at(i))
            n_on += 1;
        if (is_at_T.at(i))
            n_at_target_T += 1;
    }
    int n_turned_off = n_should_operate - n_on;         // Number of receivers that were intended to be "on", but that turned off under current defocus conditions
    int n_under_T = n_should_operate - n_at_target_T;  // Number of receivers that were intended to be "on", but that are operating under the target temperature at the current defocus conditions


    //--- Update next defocus split

     // All receivers are off, or more are off than on -> unlikely to find a good solution without shutting one receiver off
    if (n_turned_off == n_should_operate || n_turned_off > n_on) 
        skip_to_next_iter = true;

    // At least one receiver turned off.  Turn up receivers that were off (targeting their minimum operation level) and turn down receivers that were on
    else if (n_turned_off > 0)
    {
        // Turn up receivers that were off to minimum operational level (if known), or step up focus fraction if min operational level is unkonwn
        int n_increase = n_turned_off;
        double df = 0.0;
        double focus_new;
        for (size_t i = 0; i < nrec; i++)
        {
            if (is_expect_on.at(i) && !is_on.at(i))
            {
                if (m_approx_min_focus.at(i) > 0.0 && m_approx_min_focus.at(i) > focus_max_sim_off.at(i))  // Known minimum operational level
                    focus_new = m_approx_min_focus.at(i);
                else  // No known minimum operational level, step up focus fraction limited by the current minimum focus fraction at which the receiver was on
                {
                    focus_new = fmin(focus_fractions.at(i) + focus_step, 1.0);
                    if (focus_new >= focus_min_sim_on.at(i))
                        focus_new = 0.5 * (focus_min_sim_on.at(i) + focus_max_sim_off.at(i));
                }
                df += focus_new - focus_fractions.at(i);
                focus_fractions.at(i) = focus_new;
            }
        }

        // Turn down receivers that were on to compensate. 
        int n_decrease = n_should_operate - n_turned_off;
        int n_turned_down = 0;
        int n = 0;
        for (size_t i = 0; i < nrec; i++)
        {
            if (is_expect_on.at(i) && is_on.at(i))
            {
                focus_new = 0.0;
                if (is_at_T.at(i) && n_at_target_T < n_decrease)  // This receiver is at T but there is at least one more receiver operating that is not. Only turn down this receiver to the point where it can maintain the target exit T
                {
                    if (m_approx_min_focus_at_T.at(i) > 0.0 && focus_fractions.at(i) > m_approx_min_focus_at_T.at(i)) 
                        focus_new = focus_fractions.at(i) - fmin(df, focus_fractions.at(i) - m_approx_min_focus_at_T.at(i));
                    else if (m_approx_min_focus_at_T.at(i) == 0.0) 
                    {
                        focus_new = focus_fractions.at(i) - df / (n_on - n);
                        if (focus_new <= focus_max_sim_under_T.at(i))
                            focus_new = 0.5 * (focus_min_sim_at_T.at(i) + focus_max_sim_under_T.at(i));
                    }
                }
                else  // This receiver is not at T, or it's the only receiver that can be turned down
                {
                    focus_new = focus_fractions.at(i) - df / (n_on - n);
                    if (focus_new <= m_approx_min_focus.at(i))
                        focus_new = m_approx_min_focus.at(i);
                    else if (focus_new <= focus_max_sim_off.at(i))
                        focus_new = 0.5 * (focus_min_sim_on.at(i) + focus_max_sim_off.at(i));
                }

                if (focus_new > 0.0) // Receiver i turned down
                {
                    df -= (focus_fractions.at(i) - focus_new);
                    focus_fractions.at(i) = focus_new;
                    n_turned_down += 1;
                }
                n += 1;
            }
        }

        if (n_turned_down == 0)
            skip_to_next_iter = true;
        else if (df > 0.001) 
        {
            int j = low_to_high_order.at(n_should_operate - 1);
            focus_fractions.at(j) -= df;
        }
    }

    // All receivers that should be operating are on, but all are below the target temperature.
    // Typically can find a better exit temperature by minimizing focus of the lowest-performing receivers (targeting limits at which they can remain on)
    else if (n_under_T == n_should_operate)
    {
        int n_decrease = floor(n_should_operate / 2);
        double df = 0.0;
        int n = 0;
        int k = 0;
        double focus_new;
        while (k < n_should_operate - 1 && n < n_decrease)
        {
            int j = low_to_high_order.at(k);
            focus_new = 0.0;
            if (m_approx_min_focus.at(j) > 0.0 && focus_fractions.at(j) > m_approx_min_focus.at(j))
                focus_new = m_approx_min_focus.at(j);
            else if (m_approx_min_focus.at(j) == 0.0)
            {
                focus_new = focus_fractions.at(j) - focus_step;
                if (focus_new <= focus_max_sim_off.at(j))
                    focus_new = 0.5 * (focus_min_sim_on.at(j) + focus_max_sim_off.at(j));
            }
            if (focus_new > 0.0)
            {
                df += focus_fractions.at(j) - focus_new;
                focus_fractions.at(j) = focus_new;
                n += 1;
            }
            k += 1;
        }

        if (n == 0)  // No receivers turned down, stop here
            skip_to_next_iter = true;
        else
        {
            int n_increase = n_should_operate - k;
            for (size_t i = 0; i < n_increase; i++)
            {
                int j = low_to_high_order.at(n_should_operate - 1 - i);
                double dfi = fmin(df / (n_increase - i), 1.0 - focus_fractions.at(j));
                if (focus_min_sim_at_T.at(j) < 1.0)  // Only turn up receiver j to min value where it can achieve target outlet T (if known)
                    dfi = fmin(dfi, focus_min_sim_at_T.at(j) - focus_fractions.at(j));
                focus_fractions.at(j) += dfi;
                df -= dfi;
            }
            if (df > 0.001)  // Need to turn back up some of the receivers that turned down
            {
                for (size_t i = 0; i < k; i++)
                {
                    int j = low_to_high_order.at(k - 1 - i);
                    double dfi = fmin(df, focus_min_sim_at_T.at(j) - focus_fractions.at(j));
                    focus_fractions.at(j) += dfi;
                    df -= dfi;
                }
            }
        }
    }

    // All receivers are operating and at least one receiver is operating at the target exit temperature.
    // Try increasing the focus fraction of receivers operating below the target exit temperature
    else
    {
        double df = 0.0;
        double focus_new;

        // Turn up recievers that are under temperature, targeting the minimum focus fraction at which the receiver achieves the target
        for (size_t i = 0; i < nrec; i++)
        {
            if (is_expect_on.at(i) && !is_at_T.at(i))
            {
                focus_new = fmin(1.0, focus_fractions.at(i) + focus_step);
                if (focus_new >= focus_min_sim_at_T.at(i))
                    focus_new = 0.5 * (focus_min_sim_at_T.at(i) + focus_max_sim_under_T.at(i));
                df += focus_new - focus_fractions.at(i);
                focus_fractions.at(i) = focus_new;
            }
        }

        // Turn down recievers that are at temperature, targeting the minimum focus fraction at which the receiver achieves the target
        int n = 0;
        int n_turned_down = 0;
        for (size_t i = 0; i < nrec; i++)
        {
            if (is_expect_on.at(i) && is_at_T.at(i))
            {
                focus_new = 0.0;
                if (m_approx_min_focus_at_T.at(i) > 0.0 && focus_fractions.at(i) > m_approx_min_focus_at_T.at(i))
                    focus_new = focus_fractions.at(i) - fmin(df, focus_fractions.at(i) - m_approx_min_focus_at_T.at(i));
                else if (m_approx_min_focus_at_T.at(i) == 0.0)
                {
                    focus_new = focus_fractions.at(i) - df / (n_at_target_T - n);
                    if (focus_new <= focus_max_sim_under_T.at(i))
                        focus_new = 0.5 * (focus_min_sim_at_T.at(i) + focus_max_sim_under_T.at(i));
                }

                if (focus_new > 0.0) // True unless receiver i is already at min focus for target T
                {
                    df -= (focus_fractions.at(i) - focus_new);
                    focus_fractions.at(i) = focus_new;
                    n_turned_down += 1;
                }
                n += 1;
            }
        }

        if (n_turned_down == 0)  // No receivers were able to turn down without going under known minimum focus
            skip_to_next_iter = true;
        else if (df > 0.001)
        {
            for (size_t i = 0; i < nrec; i++)
            {
                if (is_expect_on.at(i) && !is_at_T.at(i))
                    focus_fractions.at(i) -= df / n_under_T;
            }
        }

    }
    return skip_to_next_iter;
}


void C_csp_falling_particle_collector_receiver::run_component_models(const C_csp_weatherreader::S_outputs& weather,
    const C_csp_solver_htf_1state& htf_state_in,
    const C_csp_collector_receiver::S_csp_cr_inputs& inputs,
    const C_csp_solver_sim_info& sim_info,
    bool is_fixed_states,
    std::vector<double>& heliostat_field_control_per_rec)
{
    for (size_t i = 0; i < mc_pt_receivers.size(); i++)
    {
        if (heliostat_field_control_per_rec.at(i) < 0.01) // Receiver i is not able to operate
        {
            mc_pt_heliostatfields.at(i)->off(weather, sim_info);
            mc_pt_receivers.at(i)->off(weather, htf_state_in, sim_info);
        }
        else
        {
            // First call heliostat field class, then use its outputs as inputs to receiver class
            mc_pt_heliostatfields.at(i)->call(weather, heliostat_field_control_per_rec.at(i), sim_info);

            // Get heliostat field outputs and set corresponding receiver inputs
            C_pt_receiver::S_inputs receiver_inputs;
            receiver_inputs.m_plant_defocus = mc_pt_heliostatfields.at(i)->ms_outputs.m_plant_defocus_out;  //[-]
            receiver_inputs.m_input_operation_mode = inputs.m_input_operation_mode;                         // TODO: This uses the reported state of the aggregated receiver for each individual receiver.  Should be fine when startup requirements are zero?
            receiver_inputs.m_flux_map_input = &mc_pt_heliostatfields.at(i)->ms_outputs.m_flux_map_out;
            receiver_inputs.m_clearsky_dni = mc_pt_heliostatfields.at(i)->ms_outputs.m_clearsky_dni;        //[W/m2]
            if (is_fixed_states && m_fixed_mode_mflow_method == 0)  // Set receiver fixed mass flow (this mass flow will be used ONLY if the receiver mass flow iterations can't achieve the target temperature).
                mc_pt_receivers.at(i)->set_fixed_mflow(heliostat_field_control_per_rec.at(i) * m_mdot_from_estimates.at(i));
            mc_pt_receivers.at(i)->call(weather, htf_state_in, receiver_inputs, sim_info);
        }
    }
    combine_results();
    return;
}



void C_csp_falling_particle_collector_receiver::call(const C_csp_weatherreader::S_outputs &weather,
	const C_csp_solver_htf_1state &htf_state_in,
	const C_csp_collector_receiver::S_csp_cr_inputs &inputs,
	C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
	//C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
	const C_csp_solver_sim_info &sim_info,
    bool is_fixed_states)
{
    size_t nrec = mc_pt_receivers.size();
    double heliostat_field_control = inputs.m_field_control;
    
    //-- Set allowable number of iterations to split defocus between fields
    int n_split_iter = 1;
    bool allow_outer_iter = false;
    if (nrec > 1 && heliostat_field_control < 0.99 && is_fixed_states)
    {
        n_split_iter = 10;
        allow_outer_iter = true;
    }

    //--- Set initial defocus for each field
    //    The focus fraction provided by the controller is defined relative to the initial solution with focus fraction = 1, which may have some receivers already off.
    //    Assume receivers that were off in estimates are also off in the intial solution with focus fraction 1, and apply the controller defocus to the remaining receivers
    std::vector<double> heliostat_field_control_per_rec = split_focus(heliostat_field_control, m_is_on_from_estimates);

    //--- Cases without iteration for defocus split between receivers
    if (n_split_iter == 1 && !allow_outer_iter)
    {
        run_component_models(weather, htf_state_in, inputs, sim_info, is_fixed_states, heliostat_field_control_per_rec);
    }

    //--- Cases including iteration for defocus split
    else
    {
        double focus_step = fmin(0.1 * heliostat_field_control, 0.05);
        double T_hot_des = mc_pt_receivers.at(0)->get_T_htf_hot_des() - 273.15;
        double bound_tol = 0.025;

        std::vector<double>focus_min_sim_at_T(nrec, 1.0);       // Minimum simulated focus fraction at which receiver achieved exit T
        std::vector<double>focus_max_sim_under_T(nrec, 0.0);    // Maximum simulated focus fraction at which receiver didn't achieve exit T
        std::vector<double>focus_min_sim_on(nrec, 1.0);         // Minimum simulated focus fraction at which receiver was on
        std::vector<double>focus_max_sim_off(nrec, 0.0);        // Maximum simulated focus fraction at which receiver was off

        bool is_current_soln_best = false;  // Is the current solution also the best solution?
        int n_on_best = -1;                 // Number of receivers "on" in current best solution
        double T_hot_best = 0.0;            // Combined mass-weighted average particle exit temperature in the current best solution
        std::vector<double>heliostat_field_control_per_rec_best(nrec, 0.0); // Focus fraction for each receiver in the current best solution

        std::vector<bool>is_expect_on = m_is_on_from_estimates;  // Expected state of each receiver prior to solution
        int n_should_operate = m_n_on_from_estimates;            // Number of receivers that we expect to be "on"
        bool stop_outer = false;

        while (n_should_operate >= 0 && !stop_outer)            // Iterations over number of receivers that we're trying to keep "on"
        {
            bool is_any_above_T = false;                                         // Has a solution been found with at least one receiver at the target T
            n_split_iter = n_should_operate > 1 ? n_split_iter : 1;              // Only need one iteration if only one receiver is operating
            std::vector<int> low_to_high_order = order_receivers(is_expect_on);  // Low-to-high performance order of receivers that are expected to be on 

            int q = 0;
            while (q < n_split_iter)  // Iterations over defocus split between receivers we're trying to keep "on"
            {

                //--- Run models for each heliostat field and receiver, collect states, and update limits
                run_component_models(weather, htf_state_in, inputs, sim_info, is_fixed_states, heliostat_field_control_per_rec);
                int n_on = 0;                           // Number of receivers on after solution
                int n_at_target_T = 0;                  // Number of receivers at target temperature
                std::vector<bool> is_on(nrec, false);   // Is receiver on after solution?
                std::vector<bool> is_at_T(nrec, false); // Is receiver at target temperature?
                for (size_t i = 0; i < nrec; i++)
                {
                    is_on.at(i) = mc_pt_receivers.at(i)->ms_outputs.m_Q_thermal > 0.001;                // Receiver i is operating
                    if (is_on.at(i))
                        n_on += 1;

                    is_at_T.at(i) = (T_hot_des - mc_pt_receivers.at(i)->outputs.m_T_salt_hot) < 1.0;    // Receiver i is at target temperature
                    if (is_at_T.at(i))
                        n_at_target_T += 1;
                }

                //--- Update known operational limits
                update_limits(heliostat_field_control_per_rec, is_on, is_at_T, bound_tol, focus_min_sim_on, focus_max_sim_off, focus_min_sim_at_T, focus_max_sim_under_T);

                //--- Stop all iterations here if the current solution is sufficient (all receivers are operating at the target outlet temperature), or if iteration isn't allowed
                if (n_at_target_T == n_should_operate || (!allow_outer_iter && n_split_iter == 1))
                {
                    is_current_soln_best = true;
                    stop_outer = true;
                    break;
                }

                //--- Check if this solution is better than any prior solutions
                is_current_soln_best = false;
                if (n_on > n_on_best)  // This solution has more receivers "on" than any previous solution
                    is_current_soln_best = true;
                else if (n_on == n_on_best && m_combined_outputs.T_salt_hot >= T_hot_best) // This solution has the same number of receivers "on", but a higher combined particle exit temperature
                    is_current_soln_best = true;
                if (is_current_soln_best)
                {
                    n_on_best = n_on;
                    T_hot_best = m_combined_outputs.T_salt_hot;
                    heliostat_field_control_per_rec_best = heliostat_field_control_per_rec;
                }


                //--- Create next split of focus fractions 
                if (q < n_split_iter - 1)
                {
                    bool is_skip_to_next = false;
                    int n_turned_off = n_should_operate - n_on;         // Number of receivers that were intended to be "on", but that turned off under current defocus conditions
                    int n_under_T = n_should_operate - n_at_target_T;   // Number of receivers that were intended to be "on", but that are operating under the target temperature at the current defocus conditions

                    if (is_any_above_T && n_turned_off == 0 && n_under_T == n_should_operate)   // We've previously found a solution with at least one receiver at the target temperature, unlikely to do any better with more iteration
                        is_skip_to_next = true;

                    if (n_turned_off > 0 && n_under_T < n_should_operate) // All receivers are on and at least one is at the target particle exit temperature
                        is_any_above_T = true;

                    if (!is_skip_to_next)
                        is_skip_to_next = update_focus_split(heliostat_field_control_per_rec, focus_step, low_to_high_order, is_expect_on, is_on, is_at_T, focus_min_sim_on, focus_max_sim_off, focus_min_sim_at_T, focus_max_sim_under_T);

                    if (is_skip_to_next)
                        q = n_split_iter - 1;
                }

                //--- If this is the last iteration, check to see if the best solution has at least one receiver off (that was expected to be on).  If so, shut down worst receiver and re-split the controller defocus signal among the remaining receivers
                stop_outer = true;
                if (q == n_split_iter - 1 && allow_outer_iter && n_on_best < n_should_operate)
                {
                    stop_outer = false;
                    int iworst = low_to_high_order.at(0);
                    is_expect_on.at(iworst) = false;
                    n_should_operate -= 1;
                    double avg_defocus = std::min(1.0, heliostat_field_control * m_n_on_from_estimates / n_should_operate);
                    heliostat_field_control_per_rec = split_focus(avg_defocus, is_expect_on);
                }
                q += 1;
            }
        }

        // Re-run receiver models if the best solution is not the current solution
        heliostat_field_control_per_rec = heliostat_field_control_per_rec_best;  // Heliostat field fractions might have been changed from best solution when we tried to update the defocus split, even if last call to receiver models was best solution
        if (!is_current_soln_best)
        {
            run_component_models(weather, htf_state_in, inputs, sim_info, is_fixed_states, heliostat_field_control_per_rec);
        }
    }

    set_outputs(cr_out_solver);
    return;
}

void C_csp_falling_particle_collector_receiver::off(const C_csp_weatherreader::S_outputs &weather,
	const C_csp_solver_htf_1state &htf_state_in,
	C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
	const C_csp_solver_sim_info &sim_info)
{
    size_t nrec = mc_pt_receivers.size();
    for (size_t i = 0; i < nrec; i++)
    {
        mc_pt_heliostatfields.at(i)->off(weather, sim_info);  //In OFF call, looking specifically for whether STOW parasitics apply
        mc_pt_receivers.at(i)->off(weather, htf_state_in, sim_info);
    }

    // Aggregate results
    combine_results();
    set_outputs(cr_out_solver);

	return;
}

void C_csp_falling_particle_collector_receiver::startup(const C_csp_weatherreader::S_outputs &weather,
	const C_csp_solver_htf_1state &htf_state_in,
	C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
	const C_csp_solver_sim_info &sim_info)
{
	// For now, define startup(...) shell that calls call() with operation mode defined.

	// Set heliostat field call() parameters and solve
	C_csp_collector_receiver::S_csp_cr_inputs inputs;
	inputs.m_input_operation_mode = C_csp_collector_receiver::STARTUP;
	inputs.m_field_control = 1.0;

	call(weather, htf_state_in, inputs, cr_out_solver, sim_info, m_fix_mode_from_estimates);

    // TODO: Need to call off modes here for individual receivers that can't operate?
}

void C_csp_falling_particle_collector_receiver::on(const C_csp_weatherreader::S_outputs &weather,
	const C_csp_solver_htf_1state &htf_state_in,
    double q_dot_elec_to_CR_heat /*MWt*/, double field_control,
	C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
	const C_csp_solver_sim_info &sim_info)
{
	// For now, define on(...) shell that calls call() with operation mode defined.
	// Should eventually develop an 'on' method for the MSPT

	// Define 'C_csp_cr_inputs' for call(...)
	C_csp_collector_receiver::S_csp_cr_inputs inputs;
	inputs.m_input_operation_mode = C_csp_collector_receiver::ON;
	inputs.m_field_control = field_control;

	call(weather, htf_state_in, inputs, cr_out_solver, sim_info, m_fix_mode_from_estimates);

    // Full collector-receiver "off" mode will only be called if ALL receivers are off.
    // Need to check here if any individual receiver is off and call the individual collector and receiver off modes to correctly capture field parasitics and to re-set receiver startup time requirements
    for (int i = 0; i < mc_pt_receivers.size(); i++)
    {
        if (mc_pt_receivers.at(i)->ms_outputs.m_Q_thermal < 1.e-6)
        {
            mc_pt_heliostatfields.at(i)->off(weather, sim_info); 
            mc_pt_receivers.at(i)->off(weather, htf_state_in, sim_info);
        }
    }
    combine_results();
    set_outputs(cr_out_solver);
    return;

}

void C_csp_falling_particle_collector_receiver::estimates(const C_csp_weatherreader::S_outputs &weather,
	const C_csp_solver_htf_1state &htf_state_in,
	C_csp_collector_receiver::S_csp_cr_est_out &est_out,
	const C_csp_solver_sim_info &sim_info)
{
	// For now, define estimates(...) shell that calls call() with operation mode defined.
	// Should eventually develop an estimate(...) method for the MSPT
	
	C_csp_collector_receiver::S_csp_cr_inputs inputs;
	inputs.m_input_operation_mode = C_csp_collector_receiver::STEADY_STATE;
	inputs.m_field_control = 1.0;

	C_csp_collector_receiver::S_csp_cr_out_solver cr_out_solver;

    // Reset any fixed state requirements to allow the receiver to choose it's own state during estimates
    size_t nrec = mc_pt_receivers.size();
    if (m_fix_mode_from_estimates)
    {
        for (size_t i = 0; i < nrec; i++)
        {
            mc_pt_receivers.at(i)->set_state_requirement(false); // Allow receiver to choose it's own state during estimates
            if (m_fixed_mode_mflow_method == 0)
                mc_pt_receivers.at(i)->set_fixed_mflow(std::numeric_limits<double>::quiet_NaN());
        }
    }
    // Set nominal values used in "call" when splitting the defocus signal to allow all receivers to try to operate
    m_n_on_from_estimates = nrec;  
    std::fill(m_is_on_from_estimates.begin(), m_is_on_from_estimates.end(), true); 

    // Run field/receiver models with no defocus
	call(weather, htf_state_in, inputs, cr_out_solver, sim_info, false);

	int mode = get_operating_state();

	if( mode == C_csp_collector_receiver::ON || mode == C_csp_collector_receiver::OFF_NO_SU_REQ)
	{
		est_out.m_q_dot_avail = cr_out_solver.m_q_thermal;			//[MWt]
		est_out.m_m_dot_avail = cr_out_solver.m_m_dot_salt_tot;		//[kg/hr]
		est_out.m_T_htf_hot = cr_out_solver.m_T_salt_hot;			//[C]
		est_out.m_q_startup_avail = 0.0;
	}
	else
	{
		est_out.m_q_startup_avail = cr_out_solver.m_q_thermal;		//[MWt]
		est_out.m_q_dot_avail = 0.0;
		est_out.m_m_dot_avail = 0.0;
		est_out.m_T_htf_hot = 0.0;
	}

    // Collect expected receiver performance before defocus (called from controller with receiver inlet T = cycle return temperature assuming design point hot temperature and current time step ambient conditions)
    m_n_on_from_estimates = 0;
    for (size_t i = 0; i < mc_pt_receivers.size(); i++)
    {
        m_qthermal_from_estimates.at(i) = mc_pt_receivers.at(i)->ms_outputs.m_Q_thermal;
        m_mdot_from_estimates.at(i) = mc_pt_receivers.at(i)->ms_outputs.m_m_dot_salt_tot/3600.;    // kg/s
        m_is_on_from_estimates.at(i) = m_qthermal_from_estimates.at(i) > 1e-3;
        if (m_is_on_from_estimates.at(i))
            m_n_on_from_estimates += 1;
        if (m_fix_mode_from_estimates)
            mc_pt_receivers.at(i)->set_state_requirement(true); // Require receiver state to be fixed to that in inputs.m_input_operation_mode in subsequent calls
        m_approx_min_focus.at(i) = 0.0;
        m_approx_min_focus_at_T.at(i) = 0.0;
    }
    return;
}

double C_csp_falling_particle_collector_receiver::calculate_optical_efficiency( const C_csp_weatherreader::S_outputs &weather, const C_csp_solver_sim_info &sim )
{
    /*
    Evaluate optical efficiency. This is a required function for the parent class, 
    but doesn't do much other than simply call the optical efficiency model in this case.
    */
    size_t nrec = mc_pt_receivers.size();
    double eta_field = 0.0;
    double q_dot_field_inc = 0.0;
    for (size_t i = 0; i < nrec; i++)
    {
        mc_pt_heliostatfields.at(i)->call(weather, 1., sim);
        q_dot_field_inc += mc_pt_heliostatfields.at(i)->ms_outputs.m_q_dot_field_inc;
        eta_field += mc_pt_heliostatfields.at(i)->ms_outputs.m_eta_field * mc_pt_heliostatfields.at(i)->ms_outputs.m_q_dot_field_inc;
    }
    if (q_dot_field_inc != 0.0)
        eta_field /= q_dot_field_inc;
    return eta_field;
}

double C_csp_falling_particle_collector_receiver::get_collector_area()
{
    double A_sf = 0.0;
    size_t nrec = mc_pt_receivers.size();
    for (size_t i = 0; i < nrec; i++)
    {
        A_sf += mc_pt_heliostatfields.at(i)->ms_params.m_A_sf;
    }
    return A_sf;
}

double C_csp_falling_particle_collector_receiver::calculate_thermal_efficiency_approx( const C_csp_weatherreader::S_outputs &weather, double q_inc, const C_csp_solver_sim_info& sim)
{
    // This is only called from the dispatch model, q_inc is the total incident power for all receivers.
    // For now separating into individual receivers based on the number of heliostats per field
    // TODO: Call receiver class and separate based on ratio of power delivered by each field instead?

    size_t nrec = mc_pt_receivers.size();
    double A_sf_tot = get_collector_area();
    double eta_therm = 0.0;
    double q_inc_rec = 0.0;
    for (size_t i = 0; i < nrec; i++)
    {
        q_inc_rec = q_inc * mc_pt_heliostatfields.at(i)->ms_params.m_A_sf / A_sf_tot;
        eta_therm += mc_pt_receivers.at(i)->estimate_thermal_efficiency(weather, q_inc_rec) * q_inc_rec;
    }
    if (q_inc != 0.0)
        eta_therm /= q_inc;  // Weighted-average of receiver efficiencies based on incident power
    return eta_therm;
}


void C_csp_falling_particle_collector_receiver::converged()
{
    bool is_field_tracking_final_per_rec, is_field_tracking_final;
    double startup_time_remain_final_per_rec, startup_energy_remain_final_per_rec, startup_time_remain_final, startup_energy_remain_final;
    C_csp_collector_receiver::E_csp_cr_modes op_mode_final_per_rec, op_mode_final;

    startup_time_remain_final_per_rec = startup_energy_remain_final_per_rec = std::numeric_limits<double>::quiet_NaN();
    startup_time_remain_final = startup_energy_remain_final = 1e10;

    std::fill(m_approx_min_focus.begin(), m_approx_min_focus.end(), 0.0);
    std::fill(m_approx_min_focus_at_T.begin(), m_approx_min_focus_at_T.end(), 0.0);

    size_t nrec = mc_pt_receivers.size();
    is_field_tracking_final = false;
    op_mode_final = E_csp_cr_modes::OFF;  
    for (size_t i = 0; i < nrec; i++)
    {
        mc_pt_heliostatfields.at(i)->converged();
        mc_pt_receivers.at(i)->converged();

        // Set reported heliostat converged value  // TODO: This could be refactored...
        mc_pt_heliostatfields.at(i)->get_converged(is_field_tracking_final_per_rec);
        if (is_field_tracking_final_per_rec)
            is_field_tracking_final = true;  // Report that the field was tracking if any of the receivers were operating

        // Get final receiver states
        mc_pt_receivers.at(i)->get_converged_values(op_mode_final_per_rec, startup_energy_remain_final_per_rec, startup_time_remain_final_per_rec);
        startup_time_remain_final = fmin(startup_time_remain_final, startup_time_remain_final_per_rec);         // Minimum startup time remaining out of all receivers
        startup_energy_remain_final = fmin(startup_energy_remain_final, startup_energy_remain_final_per_rec);   // Minimum startup energy remaining out of all receivers

        if (op_mode_final_per_rec == E_csp_cr_modes::ON || op_mode_final_per_rec == E_csp_cr_modes::STEADY_STATE)  // If any receiver is on, report as ON
            op_mode_final = op_mode_final_per_rec;
        else if (op_mode_final_per_rec == E_csp_cr_modes::STARTUP && op_mode_final != E_csp_cr_modes::ON)  // Report STARTUP if none of the receivers are on, but at least one is starting up
            op_mode_final = E_csp_cr_modes::STARTUP;
        else if (op_mode_final_per_rec == E_csp_cr_modes::OFF_NO_SU_REQ && op_mode_final != E_csp_cr_modes::ON)
            op_mode_final = E_csp_cr_modes::OFF_NO_SU_REQ;
    }

    mc_reported_outputs.value(E_IS_FIELD_TRACKING_FINAL, (int)is_field_tracking_final);			//[-]
    mc_reported_outputs.value(E_REC_OP_MODE_FINAL, (int)op_mode_final);
    mc_reported_outputs.value(E_REC_STARTUP_TIME_REMAIN_FINAL, startup_time_remain_final);
    mc_reported_outputs.value(E_REC_STARTUP_ENERGY_REMAIN_FINAL, startup_energy_remain_final);

	mc_reported_outputs.set_timestep_outputs();
}

void C_csp_falling_particle_collector_receiver::write_output_intervals(double report_time_start,
	const std::vector<double> & v_temp_ts_time_end, double report_time_end)
{
	mc_reported_outputs.send_to_reporting_ts_array(report_time_start,
		v_temp_ts_time_end, report_time_end);
}
