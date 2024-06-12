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

#ifndef __csp_solver_falling_particle_collector_receiver_
#define __csp_solver_falling_particle_collector_receiver_

#include "csp_solver_core.h"
#include "csp_solver_pt_sf_perf_interp.h"
#include "csp_solver_pt_receiver.h"


class C_csp_falling_particle_collector_receiver : public C_csp_collector_receiver
{

private:
    std::vector<C_pt_sf_perf_interp*> mc_pt_heliostatfields;
    std::vector<std::shared_ptr<C_pt_receiver>> mc_pt_receivers;

    struct combined_outputs
    {
        double q_dot_field_inc;         //[MWt]
        double q_dot_rec_inc;           //[MWt]
        double q_thermal;               //[MWt]
        double q_dot_transport_loss;    //[MWt]
        double q_dot_refl_loss;         //[MWt]
        double q_dot_thermal_loss;      //[MWt]

        double q_startup;               //[MWt-hr]
        double q_dot_startup;           //[MWt]
        double time_required_su;        //[s]

        double eta_field;               //[-]
        double eta_thermal;             //[-]

        double m_dot_salt_tot;          //[kg/hr]
        double T_salt_hot;              //[C]
        double T_salt_cold;             //[C]

        double sf_adjust;               //[-]
        double component_defocus;       //[-]

        double W_dot_tracking;          //[MWe]
        double W_dot_transport;         //[MWe]
        double W_dot_elec_in_tot;       //[MWe]

        combined_outputs()
        {
            clear();
        }

        void clear()
        {
            q_dot_field_inc = q_dot_rec_inc = q_thermal = q_dot_transport_loss = q_dot_refl_loss = q_dot_thermal_loss = 0.0;
            q_startup = q_dot_startup = time_required_su = 0.0;
            eta_field = eta_thermal = 0.0;
            m_dot_salt_tot = T_salt_hot = T_salt_cold = 0.0;
            sf_adjust = component_defocus = 0.0;
            W_dot_tracking = W_dot_transport = W_dot_elec_in_tot = 0.0;
        }
    };

    combined_outputs m_combined_outputs;

    // Receiver and field results from the most recent estimates call
    bool m_fix_mode_from_estimates;     // Require receiver state to match that from estimates call, even if receiver can't achieve target exit temperature?
    bool m_fixed_mode_mflow_method;     // Method for setting mass flow when m_is_mode_fixed_to_input_mode = True.   0 = fix mass flow rate, 1 = solve for mass flow rate at maximum exit temperature
    std::vector<double> m_sf_qinc_from_estimates;       // Incident solar power from estimates call (MWt)
    std::vector<double> m_rec_qthermal_from_estimates;  // Receiver Qthermal from estimates call (MWt)
    std::vector<double> m_eta_rec_from_estimates;       // Receiver efficiency from estimates call
    std::vector<double> m_mdot_from_estimates;          // REceiver mass flow from estimates call (kg/s)
    

    void combine_results();
    void set_outputs(C_csp_collector_receiver::S_csp_cr_out_solver& cr_out_solver);


public:
	
	enum
	{
		E_FIELD_Q_DOT_INC,		    //[MWt] Field incident thermal power
		E_FIELD_ETA_OPT,		    //[-] Optical efficiency
		E_FIELD_ADJUST,			    //[-] Field adjustment factor
        E_IS_FIELD_TRACKING_FINAL,  //[-] Is field tracking at end of timestep
		
        E_REC_DEFOCUS,              //[-] Receiver component defocus (to satisfy max mass flow rates or flux limits)
        E_Q_DOT_INC,			    //[MWt] Receiver incident thermal power
		E_ETA_THERMAL,			    //[-] Receiver thermal efficiency
		E_Q_DOT_THERMAL,		    //[MWt] Receiver thermal power to HTF less piping loss
		E_M_DOT_HTF,			    //[kg/hr] Receiver mass flow rate
		E_Q_DOT_STARTUP,		    //[MWt] Receiver startup thermal power consumed
		E_T_HTF_IN,				    //[C] Receiver HTF inlet temperature
		E_T_HTF_OUT,			    //[C] Receiver HTF outlet temperature
		E_Q_DOT_PIPE_LOSS,		    //[MWt] Tower piping losses
        E_Q_DOT_LOSS,               //[MWt] Receiver convection and radiation losses
        E_Q_DOT_REFL_LOSS,          //[MWt] Receiver reflection losses (0 for external)
        E_W_DOT_TRACKING,           //[MWe] Heliostat tracking power
        E_W_DOT_PUMP,               //[MWe] Pumping power

		E_CLEARSKY,				    //[W/m2] Clear-sky DNI 

        E_REC_OP_MODE_FINAL,        //[-] Final receiver operating mode (see E_csp_cr_modes)
        E_REC_STARTUP_TIME_REMAIN_FINAL,    //[hr] Final receiver startup time remaining
        E_REC_STARTUP_ENERGY_REMAIN_FINAL,   //[W-hr] Final receiver startup energy remaining

        // Outputs for individual receivers
        E_Q_DOT_INC_1,		        //[MWt] Receiver 1 incident thermal power
        E_Q_DOT_INC_2,		        //[MWt] Receiver 2 incident thermal power
        E_Q_DOT_INC_3,		        //[MWt] Receiver 3 incident thermal power
        E_Q_DOT_THERMAL_1,		    //[MWt] Receiver 1 thermal power to HTF less piping loss
        E_Q_DOT_THERMAL_2,		    //[MWt] Receiver 2 thermal power to HTF less piping loss
        E_Q_DOT_THERMAL_3,		    //[MWt] Receiver 3 thermal power to HTF less piping loss
        E_ETA_THERMAL_1,		    //[MWt] Receiver 1 thermal efficiency
        E_ETA_THERMAL_2,		    //[MWt] Receiver 2 thermal efficiency
        E_ETA_THERMAL_3,		    //[MWt] Receiver 3 thermal efficiency
        E_FIELD_ETA_OPT_1,		    //[-] Optical efficiency of field 1
        E_FIELD_ETA_OPT_2,		    //[-] Optical efficiency of field 2
        E_FIELD_ETA_OPT_3,		    //[-] Optical efficiency of field 3
	};
	
	C_csp_reported_outputs mc_reported_outputs;
	
    C_csp_falling_particle_collector_receiver(std::vector<C_pt_sf_perf_interp*> pt_heliostatfields, std::vector<std::shared_ptr<C_pt_receiver>> pt_receivers);

	~C_csp_falling_particle_collector_receiver();

	void init(const C_csp_collector_receiver::S_csp_cr_init_inputs init_inputs, 
			C_csp_collector_receiver::S_csp_cr_solved_params & solved_params);

	C_csp_collector_receiver::E_csp_cr_modes get_operating_state();

    double get_startup_time();
    double get_startup_energy(); //MWh
    double get_pumping_parasitic_coef();  //MWe/MWt
    double get_min_power_delivery();    //MWt
    double get_max_power_delivery(double T_htf_cold_in /*C*/);    //MWt
	double get_tracking_power();		//MWe
	double get_col_startup_power();		//MWe-hr

    void off(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		//C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
		const C_csp_solver_sim_info &sim_info);

	void startup(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		const C_csp_solver_sim_info &sim_info);

	void on(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
        double q_dot_elec_to_CR_heat /*MWt*/, double field_control,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		const C_csp_solver_sim_info &sim_info);

	void estimates(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_est_out &est_out,
		const C_csp_solver_sim_info &sim_info);

	void converged();

	void write_output_intervals(double report_time_start,
		const std::vector<double> & v_temp_ts_time_end, double report_time_end);

    double calculate_optical_efficiency( const C_csp_weatherreader::S_outputs &weather, const C_csp_solver_sim_info &sim );
  
    double calculate_thermal_efficiency_approx( const C_csp_weatherreader::S_outputs &weather, double q_incident /*MW*/ , const C_csp_solver_sim_info& sim);

    double get_collector_area();


	void call(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		const C_csp_collector_receiver::S_csp_cr_inputs &inputs,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		//C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
		const C_csp_solver_sim_info &sim_info,
        bool is_fixed_states);

};








#endif //__csp_solver_falling_particle_collector_receiver_
