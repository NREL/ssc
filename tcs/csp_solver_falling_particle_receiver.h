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

#ifndef __csp_solver_falling_particle_receiver_
#define __csp_solver_falling_particle_receiver_

#include "csp_solver_util.h"

#include "htf_props.h"
#include "csp_solver_core.h"
#include "csp_solver_pt_receiver.h"

class C_falling_particle_receiver : public C_pt_receiver
{

protected:

    struct s_steady_state_soln
    {
        C_csp_collector_receiver::E_csp_cr_modes mode;
        bool rec_is_off;

        double T_amb;				// Dry bulb temperature (K)
        double v_wind_10;			// Wind speed at 10m (m/s)
        double wind_dir;            // Wind direction
        double p_amb;				// Ambient pressure (Pa)
        double T_sky;               // Sky temperature (K)

        double flux_sum;            // Flux map summed (W/m2)

        double dni_applied_to_measured; //[-] Ratio of DNI used to measured DNI
        double plant_defocus;       // plant defocus
        double clearsky_to_input_dni;   //[-]

        double od_control;          // Defocus control
   
        double m_dot_tot;           // Total particle mass flow (kg/s)
        double T_particle_cold_in;  // Cold particle inlet temperature (K)
        double T_particle_hot;      // Particle outlet T including thermal loss from particle transport [K]
        double T_particle_hot_rec;  // Particle outlet T at receiver exit [K]

        double Q_inc;               // Total solar energy incident on the particle curtain (W)
        double Q_refl;			    // Total solar reflection loss (W)
        double Q_rad;			    // Total radiation loss (W)
        double Q_adv;			    // Total advection loss (W)
        double Q_transport;         // Total thermal loss from particle transport (W)
        double Q_thermal;           // Total thermal power delivered to the particles (including losses from particle transport) (W)
        double eta;                 // Receiver efficienty (energy to particles / solar energy incident on curtain)
        double hadv;                // Advective loss coefficient (including wind effects) (W/m2/K)

        //util::matrix_t<double> m_dot_per_zone;  // Particle mass flow per control zone (kg/s)
        util::matrix_t<double> T_p;			// Particle temperature (K) [ny,nx]
        util::matrix_t<double> T_back_wall; // Back wall temperature (K)
        double T_front_wall;                // Front wall temperature (K)
        util::matrix_t<double> phip;        // Curtain solids volume fraction
        util::matrix_t<double> vel;         // Curtain velocity (m/s)
        util::matrix_t<double> thc;         // Curtain thickness (m)
        util::matrix_t<double> tauc;        // Curtain transmittance
        util::matrix_t<double> rhoc;        // Curtain reflectance


        util::matrix_t<double> q_dot_inc;  // Curtain element incident solar energy (W/m2)
        //util::matrix_t<double> q_dot_conv; // Curtain element convection loss (W/m2)
        //util::matrix_t<double> q_dot_rad;  // Curtain element IR radiation loss (W/m2)
        //util::matrix_t<double> q_dot_loss; // Curtain element total convection and IR radiation loss (W/m2)
        //util::matrix_t<double> q_dot_htf;  // Curtain element energy to HTF (W/m2)

        util::matrix_t<double> K;
        util::matrix_t<double> Kinv;

        s_steady_state_soln()
        {
            clear();
        }

        void clear()
        {
            T_amb = v_wind_10 = p_amb = T_sky = std::numeric_limits<double>::quiet_NaN();
            dni_applied_to_measured = od_control = plant_defocus = clearsky_to_input_dni = std::numeric_limits<double>::quiet_NaN();
            m_dot_tot = T_particle_cold_in = T_particle_hot = T_particle_hot_rec = std::numeric_limits<double>::quiet_NaN();
            Q_inc = Q_refl = Q_rad = Q_adv = Q_transport = Q_thermal = eta = hadv = std::numeric_limits<double>::quiet_NaN();

            mode = C_csp_collector_receiver::E_csp_cr_modes::OFF;
            rec_is_off = true;
        }
    };

    //--- Input parameters defined in constructor

    // Model specs
    int m_model_type;           // 0 = Fixed efficiency, 1 = Sandia efficiency correlation for free-falling receiver, 2 = Sandia efficiency correlation for multi-stage receiver, 3 = Detailed receiver model
    double m_fixed_efficiency;  // User-defined fixed efficiency, only used if m_model_type == 0


    // Cavity and curtain geometry
    double m_ap_height;                 // Aperture height [m]
    double m_ap_width;                  // Aperture width [m]
    double m_ap_height_ratio;           // Aperture height / curtain height [-]
    double m_ap_width_ratio;            // Aperture width / curtain width [-]
    double m_ap_curtain_depth_ratio;    // (Distance between aperture and particle curtain) / aperture height [-]
    bool m_is_ap_at_bot;                // Is aperture positioned at the bottom of the cavity?  // TODO: Ask Bill if this option is in the optical model
    bool m_is_curtain_flat;             // [-]
    double m_curtain_rad;                 // Curtain radius of curvature (only used if m_is_curtain_flat = False) [-]


    // Particle and curtain properties
    double m_particle_dp;       // Particle diameter [m]
    double m_particle_abs;      // Particle solar absorptivity [-]
    double m_curtain_emis;      // Curtain emissivity [-]
    double m_dthdy;             // Rate of curtain thickness increase with respect to fall distance [-]
    double m_tauc_mult;         // User-provided multiplier to adjust curtain transmissivity [-]
    double m_phi0;              // Initial particle curtain volume fraction
    double m_initial_fall_height_ratio;       // Initial fall height of particle curtain before reaching top of cavity / cavity height [-]
    int m_hadv_model;           // Model used for advective loss: 0 = user-defined constant value, 1 = Sandia's correlation
    double m_hadv_user;         // User-provided constant advective loss coefficient (Only used if m_hadv_model = 0)

    // Cavity wall properties
    double m_cav_emis;          // Cavity wall emissivity [-]
    double m_cav_twall;         // Cavity wall thickness [-]
    double m_cav_kwall;         // Cavity wall thermal conductivity [-]
    double m_cav_hext;          // External loss coefficient [W/m2/K]   // TODO: Simplify twall, kwall, hext to an effective loss coefficient

    // Operating parameters
    double m_T_particle_hot_target; //[K], converted from C in constructor
    double m_csky_frac;         //[-]   
    double m_hl_ffact;		    //[-] // TODO: Separate factors for advection and radiation?

    // Flow control and discretization
    int m_n_x;              // Particle curtain (and back wall) discretization in width direction
    int m_n_y;              // Particle curtain (and back wall) discretization in height direction
    int m_n_zone_control;   // Number of particle flow control "zones" (must result in an integer number of discretized width positions included in each flow control zone)


    //--- Hardcoded parameters
    double m_tol_od;            //[-]
    double m_eta_therm_des_est; //[-]
    bool m_use_constant_piping_loss;
    bool m_include_back_wall_convection;    // Include convective heat trasnfer to back wall (assuming air velocity is the same as the local particle velocity)
    bool m_include_wall_axial_conduction;   // Include axial conduction in the back wall

    //--- Calculated parameters
    double m_curtain_height;    // Particle curtain height [m]
    double m_curtain_width;     // Particle curtain width [m]
    double m_cav_width;         // Cavity width [m]
    double m_ap_area;           // Aperture area [m2]
    double m_curtain_dist;      // Distance between aperture and curtain [m]
    double m_cav_front_area;    // Area of cavity wall in front of particle curtain [m2]
    double m_curtain_elem_area;  // Area of each discretized curtain element [m2]
    util::matrix_t<double> m_vf;  // View factor matrix


    // State variables
    double m_E_su;              //[W-hr] startup energy
    double m_E_su_prev;         //[W-hr] startup energy
    double m_t_su;              //[hr] startup time
    double m_t_su_prev;         //[hr] startup time requirement

    // Stored solutions
    s_steady_state_soln m_mflow_soln_prev;  // Steady state solution using actual DNI from the last call to the model
    s_steady_state_soln m_mflow_soln_csky_prev;  // Steady state solution using clear-sky DNI from the last call to the model

    // Model output arrays
    // Member variables so don't waste time constructing each call?
    //util::matrix_t<double> m_q_dot_inc;
    //util::matrix_t<double> m_T_s;
    //util::matrix_t<double> m_T_panel_out;
    //util::matrix_t<double> m_T_panel_in;
    //util::matrix_t<double> m_T_panel_ave;
    //util::matrix_t<double> m_q_dot_conv;
    //util::matrix_t<double> m_q_dot_rad;
    //util::matrix_t<double> m_q_dot_loss;
    //util::matrix_t<double> m_q_dot_abs;

private:

	// track number of calls per timestep, reset = -1 in converged() call
	int m_ncall;

    class C_MEQ__q_dot_des : public C_monotonic_equation
    {
    private:
        C_falling_particle_receiver* mpc_rec;
        util::matrix_t<double> m_flux_map_input;

        double m_min_to_max_flux_ratio;
        double m_step;      //[s]
        double m_plant_defocus;  //[-]
        C_csp_collector_receiver::E_csp_cr_modes m_input_operation_mode;

    public:

        C_MEQ__q_dot_des(C_falling_particle_receiver* pc_rec);

        virtual int operator()(double flux_max /*kW/m2*/, double *q_dot_des /*MWt*/) override;
    };

protected:

    void init_mspt_common();

    void design_point_steady_state(double& eta_thermal_des_calc /*-*/,
        double& W_dot_rec_pump_des_calc /*MWe*/,
        double& W_dot_rec_pump__tower_only /*MWe*/, double& W_dot_rec_pump__rec_only /*MWe*/,
        double& rec_pump_coef /*MWe/MWt*/, double& vel_htf_des /*m/s*/);

    bool use_previous_solution(const s_steady_state_soln& soln, const s_steady_state_soln& soln_prev);
    util::matrix_t<double> calculate_flux_profiles(double flux_sum /*W/m2*/, double dni_scale /*-*/, double plant_defocus /*-*/,
        double od_control /*-*/, const util::matrix_t<double>* flux_map_input);
    void calculate_steady_state_soln(s_steady_state_soln& soln, double tol, bool use_constant_piping_loss, int max_iter = 50);
    void solve_for_mass_flow(s_steady_state_soln& soln);
    void solve_for_mass_flow_and_defocus(s_steady_state_soln& soln, double m_dot_htf_max, const util::matrix_t<double>* flux_map_input);
    void solve_for_defocus_given_flow(s_steady_state_soln& soln, const util::matrix_t<double>* flux_map_input);

    void solve_mass_momentum(util::matrix_t<double>& mdot_per_elem, util::matrix_t<double>& phip, util::matrix_t<double>& vel, util::matrix_t<double>& th);

    void calculate_local_curtain_optical_properties(double th, double phip, double& rhoc, double& tauc);

    void calculate_curtain_optical_properties(util::matrix_t<double>& phip, util::matrix_t<double>& th, util::matrix_t<double>& rhoc, util::matrix_t<double>& tauc);

    void calculate_advection_coeff_sandia(double vel, double Tprop, double wspd, double wdir, double Pamb, double& hadv, double& fwind);

    double calculate_wall_convection_coeff(double vel, double len, double Tprop, double P_amb);

    int get_nelem();

    void calculate_view_factors();

    util::matrix_t<double> get_reflectivity_vector(util::matrix_t<double>& rhoc);

    void calculate_coeff_matrix(util::matrix_t<double>& rhoc, util::matrix_t<double>& tauc, util::matrix_t<double>& K, util::matrix_t<double>& Kinv);

    void calculate_radiative_exchange(util::matrix_t<double>& Esource, util::matrix_t<double>& Kinv, util::matrix_t<double>& rhoc, util::matrix_t<double>& tauc,
                                        util::matrix_t<double>& qnetc, util::matrix_t<double>& qnetw, double qnetwf, double qnetap);

    double calculate_passive_surface_T(double Tguess, double qnet_rad, double hconv, double Tconv, double Tamb, bool is_axial_conduction, double Tcond_prev, double Tcond_next);

    double calculate_mass_wtd_avg_exit(util::matrix_t<double>& m_flow_per_elem, util::matrix_t<double>& mat);

    void invert(util::matrix_t<double>& A, util::matrix_t<double>& Ainv);

    double vf_parallel_rect(double x1, double y1, double e1, double n1, double dx, double dy, double de, double dn, double z);

    util::matrix_t<double> sum_over_rows(util::matrix_t<double>& mat, bool exclude_last);

    util::matrix_t<double> sum_over_cols(util::matrix_t<double>& mat);

    double sum_over_rows_and_cols(util::matrix_t<double>& mat, bool exclude_last_row);


public:
	// Class to save messages for up stream classes
	C_csp_messages csp_messages;

	// Methods
    C_falling_particle_receiver(double h_tower /*m*/, double epsilon /*-*/,
        double T_htf_hot_des /*C*/, double T_htf_cold_des /*C*/,
        double f_rec_min /*-*/, double q_dot_rec_des /*MWt*/,
        double rec_su_delay /*hr*/, double rec_qf_delay /*-*/,
        double m_dot_htf_max_frac /*-*/, double eta_pump /*-*/,
        double od_tube /*mm*/, double th_tube /*mm*/,
        double piping_loss_coefficient /*Wt/m2-K*/, double pipe_length_add /*m*/, double pipe_length_mult /*-*/,
        int field_fl, util::matrix_t<double> field_fl_props,
        int tube_mat_code /*-*/,
        int night_recirc /*-*/,
        int model_type /*-*/, double fixed_efficiency /*-*/,
        double ap_height /*m*/, double ap_width /*m*/, double ap_height_ratio /*-*/, double ap_width_ratio /*-*/,
        double ap_curtain_depth_ratio /*-*/, bool is_ap_at_bot /*-*/,
        double particle_dp /*m*/, double particle_abs /*-*/, double curtain_emis /*-*/,
        double dthdy /*-*/, int hadv_model /*-*/, double hadv_user  /*-*/,
        double cav_emis /*-*/, double cav_twall /*m*/, double cav_kwall /*m*/, double cav_hext /*W/m2/K*/,
        double hl_ffact /*-*/, int n_x, int  n_y, int n_zone_control /*-*/,
        double T_hot_target /*C*/, double csky_frac /*-*/);

	~C_falling_particle_receiver(){};

	virtual void init() override;

	virtual void call(const C_csp_weatherreader::S_outputs &weather, 
		const C_csp_solver_htf_1state &htf_state_in, 
		const C_pt_receiver::S_inputs &inputs,
		const C_csp_solver_sim_info &sim_info) override;


	virtual void off(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		const C_csp_solver_sim_info &sim_info) override;

	virtual void converged() override;

    void calc_pump_performance(double rho_f, double mdot /*kg/s*/, double ffact, double& PresDrop_calc /*MPa*/, double& WdotPump_calc /*W*/);

    void calc_pump_performance(double rho_f, double mdot /*kg/s*/, double ffact, double& PresDrop_calc /*MPa*/, double& WdotPump_calc /*W*/, double& ratio_dP_tower_to_rec /*-*/);

    double get_pumping_parasitic_coef() override;

    double area_proj() override;

    void get_solved_design_common(double& m_dot_rec_total /*kg/s*/,
        double& T_htf_cold_des /*K*/, int& n_panels);

    void overwrite_startup_requirements_to_on();

    

};

#endif // __csp_solver_mspt_receiver_222_