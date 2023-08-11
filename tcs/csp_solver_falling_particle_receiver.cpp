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

#include "csp_solver_falling_particle_receiver.h"
#include "csp_solver_core.h"
#include "sam_csp_util.h"

#include "Ambient.h"
#include "definitions.h"


C_falling_particle_receiver::C_falling_particle_receiver(double h_tower /*m*/, double epsilon /*-*/,
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
    double T_hot_target /*C*/, double csky_frac /*-*/) : C_pt_receiver(h_tower, epsilon,
        T_htf_hot_des, T_htf_cold_des,
        f_rec_min, q_dot_rec_des,
        rec_su_delay, rec_qf_delay,
        m_dot_htf_max_frac, eta_pump,
        od_tube, th_tube,
        piping_loss_coefficient, pipe_length_add,
        pipe_length_mult,
        field_fl, field_fl_props,
        tube_mat_code,
        night_recirc)
{
    // Parameters not shared upstream with C_pt_receiver

    // Model specs
    m_model_type = model_type;
    m_fixed_efficiency = fixed_efficiency;

    // Cavity and curtain geometry
    m_ap_height = ap_height;
    m_ap_width = ap_width;
    m_ap_height_ratio = ap_height_ratio;
    m_ap_width_ratio = ap_width_ratio;
    m_ap_curtain_depth_ratio = ap_curtain_depth_ratio;
    m_is_ap_at_bot = is_ap_at_bot;
    m_is_curtain_flat = true;
    m_curtain_rad = std::numeric_limits<double>::quiet_NaN();


    // Particle and curtain properties
    m_particle_dp = particle_dp;
    m_particle_abs = particle_abs;
    m_curtain_emis = curtain_emis;
    m_dthdy = dthdy;
    m_tauc_mult = 1.0;
    m_phi0 = 0.6;
    m_hadv_model = hadv_model;
    m_hadv_user = hadv_user;


    // Cavity wall properties
    m_cav_emis = cav_emis;
    m_cav_twall = cav_twall;
    m_cav_kwall = cav_kwall;
    m_cav_hext = cav_hext;


    // Operating parameters
    m_T_particle_hot_target = T_hot_target + 273.15;   //[K] convert from C
    m_csky_frac = csky_frac;  
    m_hl_ffact = hl_ffact;

    // Model, flow control, and discretization
    m_n_x = n_x;
    m_n_y = n_y;
    m_n_zone_control = 1;


    // Hard-coded (for now?) parameters
    m_tol_od = 0.001;		    //[-] Tolerance for over-design iteration
    m_eta_therm_des_est = 0.9;  //[-] Estimated and used to calculate min incident flux
    m_use_constant_piping_loss = true;
    m_include_back_wall_convection = false;
    m_include_wall_axial_conduction = false;

    // Calculated parameters
    m_curtain_height = std::numeric_limits<double>::quiet_NaN();
    m_curtain_width = std::numeric_limits<double>::quiet_NaN();
    m_cav_width = std::numeric_limits<double>::quiet_NaN();
    m_ap_area = std::numeric_limits<double>::quiet_NaN();
    m_curtain_dist = std::numeric_limits<double>::quiet_NaN();
    m_cav_front_area = std::numeric_limits<double>::quiet_NaN();
    m_curtain_elem_area = std::numeric_limits<double>::quiet_NaN();

    // State variables
    m_E_su_prev = std::numeric_limits<double>::quiet_NaN();
    m_t_su_prev = std::numeric_limits<double>::quiet_NaN();
    m_E_su = std::numeric_limits<double>::quiet_NaN();
	m_t_su = std::numeric_limits<double>::quiet_NaN();

	m_ncall = -1;

}






//void C_falling_particle_receiver::get_solved_design_common(double& m_dot_rec_total /*kg/s*/,
//   double& T_htf_cold_des /*K*/, int& n_panels)
//{
//   m_dot_rec_total = m_m_dot_htf_des;      //[kg/s]
//   T_htf_cold_des = m_T_htf_cold_des;      //[K]
//    n_panels = m_n_panels;                  //[-]
//}

void C_falling_particle_receiver::init()
{

    C_pt_receiver::init();

    if (m_n_zone_control > 1)
    {
        throw(C_csp_exception("Flow control in more than one zone is not currently implemented", "Particle receiver initialization"));
    }

    m_curtain_height = m_ap_height / m_ap_height_ratio;  
    m_cav_width = m_ap_width / m_ap_width_ratio;
    m_curtain_width = m_cav_width;
    if (!m_is_curtain_flat)
    {
        double curtain_angle = 2 * asin(0.5 * m_cav_width / m_curtain_rad);
        m_curtain_width = m_curtain_rad * curtain_angle;
        if (curtain_angle > CSP::pi)
        {
            throw(C_csp_exception("Calculated particle curtain angle exceeds 180 degrees", "Particle receiver initialization"));
        }
        throw(C_csp_exception("Curved particle curtain is not currently implemented", "Particle receiver initialization"));
    }
    m_curtain_elem_area = (m_curtain_height / (m_n_y - 1)) * (m_curtain_width / m_n_x);   // Curtain element area [m2]
    m_curtain_dist = m_ap_curtain_depth_ratio * m_ap_height;
    m_cav_front_area = 2 * (m_curtain_height + m_cav_width) * m_curtain_dist + m_curtain_height * (m_cav_width - m_ap_width) + m_ap_width * (m_curtain_height - m_ap_height);  
    m_ap_area = m_ap_height * m_ap_width;

    m_E_su_prev = m_q_rec_des * m_rec_qf_delay;	//[W-hr] Startup energy
    m_t_su_prev = m_rec_su_delay;				//[hr] Startup time requirement

    double c_htf_des = field_htfProps.Cp((m_T_htf_hot_des + m_T_htf_cold_des) / 2.0) * 1000.0;		//[J/kg-K] Specific heat at design conditions
    m_m_dot_htf_des = m_q_rec_des / (c_htf_des * (m_T_htf_hot_des - m_T_htf_cold_des));					//[kg/s]
    m_m_dot_htf_max = m_m_dot_htf_max_frac * m_m_dot_htf_des;	//[kg/s]

    // If no startup requirements, then receiver is always ON
        // ... in the sense that the controller doesn't need to worry about startup
    if (m_E_su_prev == 0.0 && m_t_su_prev == 0.0) {
        m_mode_prev = C_csp_collector_receiver::OFF_NO_SU_REQ;					//[-] 0 = requires startup, 1 = starting up, 2 = running
    }
    else {
        m_mode_prev = C_csp_collector_receiver::OFF;					//[-] 0 = requires startup, 1 = starting up, 2 = running
    }



    // Calculate view factors
    if (m_model_type == 3)
        calculate_view_factors();
    

    // Initialize output arrays
    /*
    m_q_dot_inc.resize(m_n_panels);
    m_q_dot_inc.fill(0.0);

    m_T_s.resize(m_n_panels);
    m_T_s.fill(0.0);

    m_T_panel_out.resize(m_n_panels);
    m_T_panel_out.fill(0.0);

    m_T_panel_in.resize(m_n_panels);
    m_T_panel_in.fill(0.0);

    m_T_panel_ave.resize(m_n_panels);
    m_T_panel_ave.fill(0.0);

    m_q_dot_conv.resize(m_n_panels);
    m_q_dot_conv.fill(0.0);

    m_q_dot_rad.resize(m_n_panels);
    m_q_dot_rad.fill(0.0);

    m_q_dot_loss.resize(m_n_panels);
    m_q_dot_loss.fill(0.0);

    m_q_dot_abs.resize(m_n_panels);
    m_q_dot_abs.fill(0.0);
    */

    /*
    design_point_steady_state(m_eta_thermal_des_calc,
        m_W_dot_rec_pump_des_calc,
        m_W_dot_pumping_tower_share, m_W_dot_pumping_rec_share,
        m_rec_pump_coef, m_vel_htf_des);
    */

    m_ncall = -1;
    return;
}




void C_falling_particle_receiver::call(const C_csp_weatherreader::S_outputs& weather,
    const C_csp_solver_htf_1state& htf_state_in,
    const C_pt_receiver::S_inputs& inputs,
    const C_csp_solver_sim_info& sim_info)
{
    // Increase call-per-timestep counter
    // Converge() sets it to -1, so on first call this line will adjust it = 0
    m_ncall++;

    double step = sim_info.ms_ts.m_step;	//[s]
    double time = sim_info.ms_ts.m_time;	//[s]

    double plant_defocus = inputs.m_plant_defocus;          //[-]
    const util::matrix_t<double>* flux_map_input = inputs.m_flux_map_input;  // Flux distributions (ny,nx) [kW/m2] 
    C_csp_collector_receiver::E_csp_cr_modes input_operation_mode = inputs.m_input_operation_mode;
    double clearsky_dni = inputs.m_clearsky_dni;

    double T_particle_cold_in = htf_state_in.m_temp + 273.15;	//[K] Cold particle inlet temp, convert from C
    double P_amb = weather.m_pres * 100.0;	//[Pa] Ambient pressure, convert from mbar
    double T_dp = weather.m_tdew + 273.15;	//[K] Dew point temperature, convert from C
    double T_amb = weather.m_tdry + 273.15;	//[K] Dry bulb temperature, convert from C
    double I_bn = weather.m_beam;           //[W/m2]
    double v_wind_10 = weather.m_wspd;      //[m/s]
    double v_wind = log((m_h_tower + m_curtain_height / 2) / 0.003) / log(10.0 / 0.003) * v_wind_10;  // Estimated wind velocity at receiver location
    double wind_direc = weather.m_wdir;                // TODO: Check wind directions against Sandia's
    double hour = time / 3600.0;			//[hr] Hour of the year
    double T_sky = CSP::skytemp(T_amb, T_dp, hour);     //[K]

    double clearsky_adj = std::fmax(clearsky_dni, I_bn);  // Adjust clear-sky DNI to be the actual DNI, if the actual DNI is higher
    double clearsky_to_input_dni = clearsky_adj / I_bn;
    if (I_bn < 1.E-6) {
        clearsky_to_input_dni = 1.0;
    }

    bool rec_is_off = false;
    double od_control = std::numeric_limits<double>::quiet_NaN();
    s_steady_state_soln soln;

    //double T_particle_prop, cp_particle, q_dot_inc_pre_defocus;
    //T_particle_prop = cp_particle = q_dot_inc_pre_defocus = std::numeric_limits<double>::quiet_NaN();

    double m_dot_tot, T_particle_hot, eta, T_htf_prop, cp_htf, W_lift;
    m_dot_tot = T_particle_hot = eta = T_htf_prop = cp_htf = W_lift = std::numeric_limits<double>::quiet_NaN();
    double Q_inc, Q_refl, Q_adv, Q_rad, Q_transport, Q_thermal, Q_inc_pre_defocus;
    Q_inc = Q_refl = Q_adv = Q_rad = Q_transport = Q_thermal = Q_inc_pre_defocus = std::numeric_limits<double>::quiet_NaN();
    double q_thermal_steadystate, q_thermal_csky;
    q_thermal_steadystate = q_thermal_csky = std::numeric_limits<double>::quiet_NaN();


    // Set current timestep stored values to NaN so we know that code solved for them
    m_mode = C_csp_collector_receiver::OFF;
    m_E_su = std::numeric_limits<double>::quiet_NaN();
    m_t_su = std::numeric_limits<double>::quiet_NaN();

    if (input_operation_mode < C_csp_collector_receiver::OFF || input_operation_mode > C_csp_collector_receiver::STEADY_STATE) {
        error_msg = util::format("Input operation mode must be either [0,1,2], but value is %d", input_operation_mode);
        throw(C_csp_exception(error_msg, "MSPT receiver timestep performance call"));
    }

    // Check resolution of flux map input compared to resolution of the particle curtain discretization
    // TODO: Generalize to allow different resolution
    int n_flux_y = (int)flux_map_input->nrows();
    int n_flux_x = (int)flux_map_input->ncols();
    if (n_flux_y != m_n_y || n_flux_x != m_n_x) {
        error_msg = "The falling particle receiver model flux map input must match the specified discretization resolution of the particle curtain";
        throw(C_csp_exception(error_msg, "MSPT receiver timestep performance call"));
    }


    double flux_sum = 0.0;
    for (int i = 0; i < n_flux_y; i++) {
        for (int j = 0; j < n_flux_x; j++) {
            flux_sum += flux_map_input->at(i, j);
        }
    }


    bool rec_is_defocusing = false;

    // Do an initial check to make sure the solar position called is valid
    // If it's not, return the output equal to zeros. Also check to make sure
    // the solar flux is at a certain level, otherwise the correlations aren't valid
    if (input_operation_mode == C_csp_collector_receiver::OFF)
    {
        rec_is_off = true;
    }

    if (plant_defocus == 0.0 || flux_sum <= 1.E-6) // I_bn <= 1.E-6)
    {
        m_mode = C_csp_collector_receiver::OFF;
        rec_is_off = true;
    }

    //T_particle_prop = (m_T_htf_hot_des + T_particle_cold_in) / 2.0;		// Temperature for particle property evaluation [K].  TODO: allow variable properties?
    //cp_particle = field_htfProps.Cp(T_particle_prop) * 1000.0;			// Particle specific heat  [J/kg-K] 

    //if (field_eff < m_eta_field_iter_prev && m_od_control < 1.0)
    //{	// In a prior call-iteration this timestep, the component control was set < 1.0
    //    //     indicating that receiver requested defocus due to mass flow rate constraint
    //    // This call, the plant requests the field to defocus. Under the new plant defocus
    //    //     the corresponding required *component* defocus decreases, because less flux on receiver
    //    // So this line helps "correctly" allocate defocus from the component to the controller
    //    // But, this likely makes the defocus iteration trickier because the iterator
    //    //    won't see a response in output mass flow or heat until m_od_control is back to 1.0
    //    // Component defocus also depends on inlet temperature, which can make current method tricky
    //    //    because the mass_flow_and_defocus code will only adjust m_od_control down, not up
    //    //    and then following calls-iterations use the previous m_od_control as a baseline
    //    //    unless adjusted here.
    //	m_od_control = fmin(m_od_control + (1.0 - field_eff / m_eta_field_iter_prev), 1.0);
    //}
    // So maybe just try resetting m_od_control each call?
    //      This will also force correct allocation of defocus. Might be a bit slower because it's calling
    //           the mspt component defocus method more frequently, but seems more straight-forward
    //           and might make the upstream problem easier to solve or at least easier to understand
    od_control = 1.0;

    // Initialize steady state solutions with current weather, DNI, field efficiency, and inlet conditions
    s_steady_state_soln soln_actual, soln_clearsky;
    soln.T_amb = T_amb;             //[K]
    soln.v_wind_10 = v_wind_10;     //[m/s]
    soln.wind_dir = wind_direc;
    soln.p_amb = P_amb;             //[Pa]
    soln.T_sky = T_sky;             //[K]

    soln.flux_sum = flux_sum;       //[W/m2]
    soln.dni_applied_to_measured = 1.0;     //[-]
    soln.plant_defocus = plant_defocus;     //[-]
    soln.clearsky_to_input_dni = clearsky_to_input_dni;     // clearsky_adj / I_bn;   //[-]
    soln.T_particle_cold_in = T_particle_cold_in;   //[K]	
    soln.od_control = od_control;           //[-] Initial component defocus control (may be adjusted during the solution)
    soln.mode = input_operation_mode;
    soln.rec_is_off = rec_is_off;

    if ((std::isnan(clearsky_to_input_dni) || clearsky_to_input_dni < 0.9999) && m_csky_frac > 0.0001)
        throw(C_csp_exception("Clear-sky DNI to measured is NaN or less than 1 but is required >= 1 in the clear-sky receiver model"));

    // Get total incident flux before defocus is applied
    util::matrix_t<double> mt_q_dot_inc_pre_defocus = calculate_flux_profiles(flux_sum, 1.0, 1.0, 1.0, flux_map_input);  // W/m2
    Q_inc_pre_defocus = 0.0;
    for (int j = 0; j < m_n_y; j++) {
        for (int i = 0; i < m_n_x; i++) {
            Q_inc_pre_defocus += mt_q_dot_inc_pre_defocus.at(j,i) * m_curtain_elem_area;
        }
    }

    if (rec_is_off) {
        soln.q_dot_inc.resize_fill(m_n_y, m_n_x, 0.0);
    }
    else
    {
        //--- Solve for mass flow at actual and/or clear-sky DNI extremes
        if (m_csky_frac <= 0.9999 || clearsky_to_input_dni < 1.0001) // Solve for mass flow at actual DNI?
        {
            soln_actual = soln;  // Sets initial solution properties (inlet T, initial defocus control, etc.)
            //soln_actual.dni = I_bn;
            soln_actual.dni_applied_to_measured = 1.0;

            if (use_previous_solution(soln_actual, m_mflow_soln_prev))  // Same conditions were solved in the previous call to this method
                soln_actual = m_mflow_soln_prev;
            else
                solve_for_mass_flow_and_defocus(soln_actual, m_m_dot_htf_max, flux_map_input);

            m_mflow_soln_prev = soln_actual;
        }

        if (m_csky_frac >= 0.0001) // Solve for mass flow at clear-sky DNI?
        {
            if (clearsky_to_input_dni < 1.0001)
                soln_clearsky = soln_actual;
            else
            {
                soln_clearsky = soln;
                //soln_clearsky.dni = clearsky_adj;
                soln_clearsky.dni_applied_to_measured = soln.clearsky_to_input_dni;     // clearsky_adj / soln.dni;    //[-]

                if (use_previous_solution(soln_clearsky, m_mflow_soln_csky_prev))  // Same conditions were solved in the previous call to this method
                    soln_clearsky = m_mflow_soln_csky_prev;
                else
                    solve_for_mass_flow_and_defocus(soln_clearsky, m_m_dot_htf_max, flux_map_input);

                m_mflow_soln_csky_prev = soln_clearsky;
            }
        }

        //--- Set mass flow and calculate final solution
        if (clearsky_to_input_dni < 1.0001 || m_csky_frac < 0.0001)  // Flow control based on actual DNI
            soln = soln_actual;

        else if (soln_clearsky.rec_is_off)    // Receiver can't operate at this time point 
        {
            soln.rec_is_off = true;
            soln.q_dot_inc = soln_clearsky.q_dot_inc;
        }

        else if (m_csky_frac > 0.9999)   // Flow control based only on clear-sky DNI
        {
            soln.m_dot_tot = soln_clearsky.m_dot_tot;
            soln.rec_is_off = soln_clearsky.rec_is_off;
            soln.od_control = soln_clearsky.od_control;
            soln.q_dot_inc = calculate_flux_profiles(flux_sum, 1.0, plant_defocus, soln_clearsky.od_control, flux_map_input);  // Absorbed flux profiles at actual DNI and clear-sky defocus
            calculate_steady_state_soln(soln, 0.00025, m_use_constant_piping_loss);  // Solve energy balances at clear-sky mass flow rate and actual DNI conditions
        }

        else  // Receiver can operate and flow control based on a weighted average of clear-sky and actual DNI
        {

            if (soln_actual.rec_is_off)  // Receiver is off in actual DNI solution -> Set mass flow to the minimum value
            {
                soln_actual.m_dot_tot = m_f_rec_min * m_m_dot_htf_max;
                soln_actual.od_control = 1.0;
            }

            soln.rec_is_off = false;
            soln.m_dot_tot = (1.0 - m_csky_frac) * soln_actual.m_dot_tot + m_csky_frac * soln_clearsky.m_dot_tot;  // weighted average of clear-sky and actual DNI

            if (soln_clearsky.od_control >= 0.9999)  // No defocus in either clear-sky or actual DNI solutions
            {
                soln.od_control = soln_clearsky.od_control;
                soln.q_dot_inc = soln_actual.q_dot_inc;
                calculate_steady_state_soln(soln, 0.00025, m_use_constant_piping_loss); // Solve energy balances at this mass flow rate and actual DNI conditions
            }
            else
            {
                soln.od_control = (1.0 - m_csky_frac) * soln_actual.od_control + m_csky_frac * soln_clearsky.od_control;
                solve_for_defocus_given_flow(soln, flux_map_input);     // Solve for defocus to achieve as close as possible to target outlet T with this mass flow and actual DNI conditions
            }

        }
    }

    // Set variables for use in the rest of the solution
    rec_is_off = soln.rec_is_off;
    m_mode = soln.mode;
    od_control = soln.od_control;

    m_dot_tot = soln.m_dot_tot;
    T_particle_hot = soln.T_particle_hot;
    //T_salt_hot_rec = soln.T_salt_hot_rec;
    eta = soln.eta;

    //u_coolant = soln.u_salt;
    //f = soln.f;
    T_htf_prop = (T_particle_hot + T_particle_cold_in) / 2.0;
    cp_htf = field_htfProps.Cp(T_htf_prop) * 1000.0;
    //rho_coolant = field_htfProps.dens(T_coolant_prop, 1.0);

    Q_inc = soln.Q_inc;
    Q_refl = soln.Q_refl;
    Q_adv = soln.Q_adv;
    Q_rad = soln.Q_rad;
    Q_transport = soln.Q_transport;
    Q_thermal = soln.Q_thermal;

    // Calculate total absorbed solar energy and minimum absorbed per panel if not calculated in the steady state solutions
    if (soln.Q_inc != soln.Q_inc)
    {
        Q_inc = sum_over_rows_and_cols(soln.q_dot_inc, true)* m_curtain_elem_area;
    }


    q_thermal_steadystate = soln.Q_thermal;
    q_thermal_csky = 0.0;
    if (m_csky_frac > 0.0001)
        q_thermal_csky = soln_clearsky.Q_thermal;  // Steady state thermal power with clear-sky DNI




    double DELTAP, Pres_D, ratio_dP_tower_to_rec, W_dot_pump, q_thermal, q_startup;
    DELTAP = Pres_D = ratio_dP_tower_to_rec = W_dot_pump = q_thermal = q_startup = std::numeric_limits<double>::quiet_NaN();

    q_startup = 0.0;

    double time_required_su = step / 3600.0;

    if (!rec_is_off)
    {
        switch (input_operation_mode)
        {
        case C_csp_collector_receiver::STARTUP:
        {
            double qdot = m_dot_tot * cp_htf * (T_particle_hot - T_particle_cold_in);

            double time_require_su_energy = m_E_su_prev / qdot;	//[hr]
            double time_require_su_ramping = m_t_su_prev;

            double time_required_max = fmax(time_require_su_energy, time_require_su_ramping);	//[hr]

            double time_step_hrs = step / 3600.0;		//[hr]

            if (time_required_max > time_step_hrs)		// Can't completely startup receiver in maximum allowable timestep
            {											// Need to advance timestep and try again
                time_required_su = time_step_hrs;
                m_mode = C_csp_collector_receiver::STARTUP;
                q_startup = qdot * step / 3600.0;
            }
            else
            {
                time_required_su = time_required_max;		//[hr]
                m_mode = C_csp_collector_receiver::ON;

                double q_startup_energy_req = m_E_su_prev;	//[W-hr]
                double q_startup_ramping_req = qdot * m_t_su_prev;	//[W-hr]
                q_startup = fmax(q_startup_energy_req, q_startup_ramping_req);
            }

            m_E_su = fmax(0.0, m_E_su_prev - qdot * step / 3600.0);
            m_t_su = fmax(0.0, m_t_su_prev - step / 3600.0);
        }

        rec_is_off = true;

        break;

        case C_csp_collector_receiver::ON:

            m_E_su = m_E_su_prev;
            m_t_su = m_t_su_prev;
            m_mode = C_csp_collector_receiver::ON;
            q_startup = 0.0;

            q_thermal = m_dot_tot * cp_htf * (T_particle_hot - T_particle_cold_in);

            if (Q_inc < m_q_dot_inc_min)
            {
                // If output here is less than specified allowed minimum, then need to shut off receiver
                m_mode = C_csp_collector_receiver::OFF;

                // Include here outputs that are ONLY set to zero if receiver completely off, and not attempting to start-up
                //W_dot_pump = 0.0;
                // Pressure drops
                //DELTAP = 0.0; Pres_D = 0.0; u_coolant = 0.0; ratio_dP_tower_to_rec = 0.0;
            }

            break;

        case C_csp_collector_receiver::STEADY_STATE:

            m_mode = C_csp_collector_receiver::STEADY_STATE;

            break;

        }	// End switch() on input_operation_mode

        // Pressure drop calculations
        //calc_pump_performance(rho_coolant, m_dot_salt_tot, f, Pres_D, W_dot_pump, ratio_dP_tower_to_rec);

        q_thermal = m_dot_tot * cp_htf * (T_particle_hot - T_particle_cold_in);

        // After convergence, determine whether the mass flow rate falls below the lower limit
        if (Q_inc < m_q_dot_inc_min)
        {
            // GOTO 900
            // Steady State always reports q_thermal (even when much less than min) because model is letting receiver begin startup with this energy
            // Should be a way to communicate to controller that q_thermal is less than q_min without losing this functionality
            if (m_mode != C_csp_collector_receiver::STEADY_STATE || m_mode_prev == C_csp_collector_receiver::ON || m_mode_prev == C_csp_collector_receiver::OFF_NO_SU_REQ)
                rec_is_off = true;
        }
    }
    else
    {	// If receiver was off BEFORE startup deductions
        m_mode = C_csp_collector_receiver::OFF;

        // Include here outputs that are ONLY set to zero if receiver completely off, and not attempting to start-up
        //W_dot_pump = 0.0;
        // Pressure drops
        //DELTAP = 0.0; Pres_D = 0.0; u_coolant = 0.0, ratio_dP_tower_to_rec = 0.0;
    }

    if (rec_is_off)
    {
        m_dot_tot = 0.0;
        eta = 0.0;
        Q_refl = 0.0;
        Q_adv = 0.0;
        Q_rad = 0.0;
        Q_transport = 0.0;
        Q_thermal = 0.0;

        // Set the receiver outlet temperature equal to the inlet design temperature
        T_particle_hot = m_T_htf_cold_des;

        Q_inc_pre_defocus = 0.0;
        Q_inc = 0.0;
        q_thermal_csky = q_thermal_steadystate = 0.0;

        // Reset m_od_control (for reporting)
        od_control = 1.0;		//[-]
    }

    // Particle lift power
    W_lift = m_dot_tot * (m_h_tower + m_curtain_height) * 9.8067 / m_eta_pump;

    outputs.m_m_dot_salt_tot = m_dot_tot * 3600.0;		//[kg/hr] convert from kg/s
    outputs.m_eta_therm = eta;						    //[-] RECEIVER thermal efficiency (includes radiation and convective losses. reflection losses are contained in receiver flux model)
    outputs.m_W_dot_pump =  W_lift / 1.E6;				            //[MW] convert from W
    outputs.m_q_conv_sum = Q_adv / 1.E6;				//[MW] convert from W
    outputs.m_q_rad_sum = Q_rad / 1.E6;					//[MW] convert from W
    outputs.m_q_dot_refl_loss = Q_refl / 1.E6;
    outputs.m_Q_thermal = Q_thermal / 1.E6;				//[MW] convert from W
    outputs.m_T_salt_hot = T_particle_hot - 273.15;		//[C] convert from K
    outputs.m_component_defocus = od_control;				//[-]
    outputs.m_q_dot_rec_inc_pre_defocus = Q_inc_pre_defocus / 1.E6;    //[MWt]
    outputs.m_q_dot_rec_inc = Q_inc / 1.E6;			    //[MW] convert from W
    outputs.m_q_startup = q_startup / 1.E6;					//[MW-hr] convert from W-hr
    outputs.m_dP_receiver = 0.0;	                    //[bar] receiver pressure drop, convert from Pa
    outputs.m_dP_total = 0.0;						    //[bar] total pressure drop, convert from MPa
    outputs.m_ratio_dP_tower_to_rec = 0.0;              //[-] ratio of total pressure drop that is caused by tower height
    outputs.m_vel_htf = 0.0;							//[m/s]
    outputs.m_T_salt_cold = T_particle_cold_in - 273.15;		//[C] convert from K
    outputs.m_time_required_su = time_required_su * 3600.0;	//[s], convert from hr in code
    if (q_thermal > 0.0)
        outputs.m_q_dot_piping_loss = Q_transport / 1.E6;	//[MWt]
    else
        outputs.m_q_dot_piping_loss = 0.0;		//[MWt]
    outputs.m_q_heattrace = 0.0;

    outputs.m_Q_thermal_csky_ss = q_thermal_csky / 1.e6; //[MWt]
    outputs.m_Q_thermal_ss = q_thermal_steadystate / 1.e6; //[MWt]

    ms_outputs = outputs;
}

/*
C_falling_particle_receiver::C_MEQ__q_dot_des::C_MEQ__q_dot_des(C_falling_particle_receiver* pc_rec)
{
    mpc_rec = pc_rec;

    m_flux_map_input.resize_fill(1, mpc_rec->m_n_panels, std::numeric_limits<double>::quiet_NaN());

    // Member constants
    m_min_to_max_flux_ratio = 0.45; //[-]
    m_step = 3600.0;        //[s]
    m_plant_defocus = 1.0;   //[-]
    m_input_operation_mode = C_csp_collector_receiver::STEADY_STATE;
}
*/

/*
int C_falling_particle_receiver::C_MEQ__q_dot_des::operator()(double flux_max, double *q_dot_des MWt) //([kW/m2], [MWt])
{
    double min_panel_flux = m_min_to_max_flux_ratio * flux_max;
    double max_less_min = flux_max - min_panel_flux;

    for (int i = 0; i < mpc_rec->m_n_panels; i++) {
        m_flux_map_input(0, i) = max_less_min * sind(i / (double)(mpc_rec->m_n_panels - 1) * 180) + min_panel_flux;
    }

    mpc_rec->call(m_step,
        mpc_rec->m_P_amb_des, mpc_rec->m_T_amb_des, mpc_rec->m_T_sky_des,
        1.0,

        mpc_rec->m_v_wind_10_des,
        m_plant_defocus,
        &m_flux_map_input, m_input_operation_mode,
        mpc_rec->m_T_htf_cold_des);

    *q_dot_des = mpc_rec->ms_outputs.m_Q_thermal;   //[MWt]

    return 0;
}
*/


/*
void C_falling_particle_receiver::design_point_steady_state(double& eta_thermal_des_calc,
    double& W_dot_rec_pump_des_calc, //[MWe]
    double& W_dot_rec_pump__tower_only,  double& W_dot_rec_pump__rec_only,  // [MWe], [MWe]
    double& rec_pump_coef, double& vel_htf_des)  //[MWe/MWt], [m/s]
{

    C_MEQ__q_dot_des c_qot_des_eq(this);
    C_monotonic_eq_solver c_q_dot_des_solver(c_qot_des_eq);

    c_q_dot_des_solver.settings(1.E-3, 5, 0.1, 10000, true);
    double tol_solved = std::numeric_limits<double>::quiet_NaN();
    double max_flux_solved = std::numeric_limits<double>::quiet_NaN();
    int iter_solved = -1;
    int q_dot_des_code = 0;
    try{
        q_dot_des_code = c_q_dot_des_solver.solve(775.0, 700.0, m_q_rec_des*1.E-6, max_flux_solved, tol_solved, iter_solved);
    }
    catch (C_csp_exception) {

        eta_thermal_des_calc = std::numeric_limits<double>::quiet_NaN();
        W_dot_rec_pump_des_calc = std::numeric_limits<double>::quiet_NaN();
        rec_pump_coef = std::numeric_limits<double>::quiet_NaN();

        return;
    }

    eta_thermal_des_calc = ms_outputs.m_eta_therm;       //[-]
    W_dot_rec_pump_des_calc = ms_outputs.m_W_dot_pump;   //[MWe]
    W_dot_rec_pump__tower_only = W_dot_rec_pump_des_calc * ms_outputs.m_ratio_dP_tower_to_rec;  //[MWe]
    W_dot_rec_pump__rec_only = W_dot_rec_pump_des_calc - W_dot_rec_pump__tower_only;               //[MWe]

    // Should q term include piping losses?
    rec_pump_coef = W_dot_rec_pump_des_calc / ms_outputs.m_Q_thermal;    //[MWe/MWt]

    vel_htf_des = ms_outputs.m_vel_htf;       //[m/s]
    return;
}
*/



void C_falling_particle_receiver::off(const C_csp_weatherreader::S_outputs &weather,
	const C_csp_solver_htf_1state &htf_state_in,
	const C_csp_solver_sim_info &sim_info)
{
	// Don't currently need *any* of these inputs, but if we add recirculation or thermal capacitance it would be helpful to have in place
	m_mode = C_csp_collector_receiver::OFF;

	// Assuming no night recirculation, so... these should be zero
	outputs.m_m_dot_salt_tot = 0.0;		//[kg/hr] convert from kg/s
	outputs.m_eta_therm = 0.0;			//[-] RECEIVER thermal efficiency (includes radiation and convective losses. reflection losses are contained in receiver flux model)
	outputs.m_W_dot_pump = 0.0;			//[MW] convert from W
	outputs.m_q_conv_sum = 0.0;			//[MW] convert from W
	outputs.m_q_rad_sum = 0.0;			//[MW] convert from W
	outputs.m_Q_thermal = 0.0;			//[MW] convert from W
	outputs.m_T_salt_hot = 0.0;			//[C] convert from K
	outputs.m_component_defocus = 1.0;	//[-]
	outputs.m_q_dot_rec_inc = 0.0;		//[MW] convert from kW
	outputs.m_q_startup = 0.0;			//[MW-hr] convert from W-hr
	outputs.m_dP_receiver = 0.0;			//[bar] receiver pressure drop, convert from Pa
	outputs.m_dP_total = 0.0;			//[bar] total pressure drop, convert from MPa
	outputs.m_vel_htf = 0.0;				//[m/s]
	outputs.m_T_salt_cold = 0.0;			//[C] convert from K
	outputs.m_time_required_su = sim_info.ms_ts.m_step;	//[s], convert from hr in code
	outputs.m_q_dot_piping_loss = 0.0;	//[MWt]
    outputs.m_q_heattrace = 0.0;
	
	outputs.m_Q_thermal_csky_ss = 0.0; //[MWt]
	outputs.m_Q_thermal_ss = 0.0; //[MWt]

    ms_outputs = outputs;
	
	return;
}

void C_falling_particle_receiver::overwrite_startup_requirements_to_on()
{
    m_mode_prev = C_csp_collector_receiver::ON;
    m_E_su_prev = 0.0;
    m_t_su_prev = 0.0;
}

double C_falling_particle_receiver::get_pumping_parasitic_coef()
{
    return 0.0;
}

double C_falling_particle_receiver::area_proj()
{
    return m_ap_area; //[m^2] aperture area of the receiver
}


void C_falling_particle_receiver::converged()
{
	// Check HTF props?
	//!MJW 9.8.2010 :: Call the property range check subroutine with the inlet and outlet HTF temps to make sure they're in the valid range
	//call check_htf(Coolant,T_salt_hot)
	//call check_htf(Coolant,T_salt_cold)

	if( m_mode == C_csp_collector_receiver::STEADY_STATE )
	{
		throw(C_csp_exception("Receiver should only be run at STEADY STATE mode for estimating output. It must be run at a different mode before exiting a timestep",
			"MSPT receiver converged method"));
    }

    if (m_mode == C_csp_collector_receiver::OFF)
    {
        m_E_su = m_q_rec_des * m_rec_qf_delay;
        m_t_su = m_rec_su_delay;

        // If no startup requirements, then receiver is always ON
        // ... in the sense that the controller doesn't need to worry about startup
        if (m_E_su == 0.0 && m_t_su == 0.0) {
            m_mode = C_csp_collector_receiver::OFF_NO_SU_REQ;
        }
    }

    m_mode_prev = m_mode;
    m_E_su_prev = m_E_su;
    m_t_su_prev = m_t_su;

    m_ncall = -1;

    ms_outputs = outputs;
}


bool C_falling_particle_receiver::use_previous_solution(const s_steady_state_soln& soln, const s_steady_state_soln& soln_prev)
{
    // Are these conditions identical to those used in the last solution?
    if (!soln_prev.rec_is_off &&
        //soln.dni == soln_prev.dni &&
        soln.dni_applied_to_measured == soln_prev.dni_applied_to_measured &&
        soln.T_particle_cold_in == soln_prev.T_particle_cold_in &&
        soln.plant_defocus == soln_prev.plant_defocus &&
        soln.od_control == soln_prev.od_control &&
        soln.T_amb == soln_prev.T_amb &&
        soln.v_wind_10 == soln_prev.v_wind_10 &&
        soln.p_amb == soln_prev.p_amb &&
        soln.T_sky == soln_prev.T_sky &&
        soln.flux_sum == soln_prev.flux_sum)
    {
        return true;
    }
    else
        return false;
}


// Calculate flux profiles (interpolated to curtain elements at specified DNI)
// TODO: Update with interpolation to allow the curtain discretization resolution to differ from the provided flux profile resolution
util::matrix_t<double> C_falling_particle_receiver::calculate_flux_profiles(double flux_sum /*W/m2*/, double flux_scale /*-*/, double plant_defocus /*-*/,
    double od_control, const util::matrix_t<double>* flux_map_input)
{
    util::matrix_t<double> q_dot_inc, flux;
    int n_flux_y = (int)flux_map_input->nrows();
    int n_flux_x = (int)flux_map_input->ncols();
    flux.resize_fill(n_flux_y, n_flux_x, 0.0);
    q_dot_inc.resize_fill(m_n_y, m_n_x, 0.0);

    double total_defocus = plant_defocus * od_control;

    if (flux_sum > 1.0)
    {
        for (int j = 0; j < n_flux_y; j++)
        {
            for (int i = 0; i < n_flux_x; i++)
            {
                flux.at(j, i) = (*flux_map_input)(j,i) * total_defocus * flux_scale; //[kW/m^2];
                q_dot_inc.at(j, i) = flux.at(j, i) * 1000;  // Flux incident on curtain (assuming curtain discretization resolution and flux profile resolution match) [W/m^2]
            }
        }
    }
    else
    {
        flux.fill(0.0);
    }
    return q_dot_inc;
}

// Calculate steady state temperature and heat loss profiles for a given mass flow and incident flux
void C_falling_particle_receiver::calculate_steady_state_soln(s_steady_state_soln& soln, double tol, bool use_constant_piping_loss, int max_iter)
{
   
    double P_amb = soln.p_amb;
    double T_amb = soln.T_amb;
    double v_wind_10 = soln.v_wind_10;
    double wdir = soln.wind_dir;
    double T_sky = soln.T_sky;
    double T_cold_in = soln.T_particle_cold_in;

    double v_wind = log((m_h_tower + m_curtain_height / 2) / 0.003) / log(10.0 / 0.003) * v_wind_10;
    double dy = m_curtain_height / (m_n_y - 1);
    double dx = m_curtain_width / m_n_x;

    bool soln_exists = (soln.T_particle_hot == soln.T_particle_hot);  // Does a solution already exist?

    double Q_inc, Q_refl, Q_rad, Q_adv, Q_transport, Q_thermal;
    double Tp_out, T_particle_prop, cp, particle_density;

    Q_inc = sum_over_rows_and_cols(soln.q_dot_inc, true) * m_curtain_elem_area;  // Total solar power incident on curtain [W]
    Q_refl = Q_rad = Q_adv = Q_transport = 0.0;
    T_particle_prop = (m_T_htf_hot_des + T_cold_in) / 2.0; 
    particle_density = field_htfProps.dens(T_particle_prop, 1.0);
    cp = field_htfProps.Cp(T_particle_prop) * 1000.0;			        // Particle specific heat  [J/kg-K]

    if (m_model_type == 0) // Fixed user-defined receiver efficiency
    {
        Q_thermal = m_fixed_efficiency * Q_inc;  // Thermal power to particles [W]
        Tp_out = T_cold_in + Q_thermal / (soln.m_dot_tot * cp);

    }
    else if (m_model_type == 1 || m_model_type == 2) // Receiver efficiency correlations from Sandia (https://www.osti.gov/biblio/1890267, page 43)
    {
        // TODO: Check/warn about bounds where the correlation is valid?
        double q, theta, val, eta;
        double A, B, C, D, E, F, G, H;
        if (m_model_type == 1)  // Free-falling particle receiver
        {
            A = 0.8481;
            B = 0.2498;
            C = -1.0116;
            D = -7.9429e-5;
            E = -1.4575e-7;
            F = 5.5;
            G = 7.5;
            H = 5000.;
        }
        else if (m_model_type == 2)  // Multistage particle receiver
        {
            A = 0.9351;
            B = -0.0560;
            C = -0.5519;
            D = -6.4055e-5;
            E = -3.1344e-6;
            F = 5.0;
            G = 9.1;
            H = 5000.;
        }

        q = exp(-Q_inc * 1.e-6 / m_ap_area);
        val = 180 - fabs(180 - wdir);
        theta = pow(val, F) * exp(-val / G) / H;
        eta = A + B*q + C*pow(q, 2) + D*q*v_wind*theta + E*pow(v_wind, 2)*theta;
        eta = fmax(eta, 0.0);
        Q_thermal = eta*Q_inc;
        Tp_out = T_cold_in + Q_thermal / (soln.m_dot_tot * cp);
    }


    else if (m_model_type == 3)  // Detailed receiver model
    {
        //--- Solve mass and momentum equations for curtain velocity, thickness, void fraction
        util::matrix_t<double> mdot_per_elem(m_n_x);  // Mass flow (kg/s) per element
        soln.m_dot_tot = 0.0;
        for (int i = 0; i < m_n_x; i++)
        {
            mdot_per_elem.at(i) = soln.m_dot_tot / m_n_x;  // TODO: Update to allow for multiple mass flow control zones
            soln.m_dot_tot += mdot_per_elem.at(i);
        }
        solve_mass_momentum(mdot_per_elem, soln.phip, soln.vel, soln.thc);

        //--- Calculate curtain optical properties from solution for curtain void fraction and thickness
        calculate_curtain_optical_properties(soln.phip, soln.thc, soln.rhoc, soln.tauc);


        //--- Calculate coefficient matrix for radiative exchange
        // TODO: exclude this calculation if the mass flow rates haven't changed?
        calculate_coeff_matrix(soln.rhoc, soln.tauc, soln.K, soln.Kinv);



        //-- Solve radiative exchange equation for solar energy (independent of temperature)
        //   Radiative exchange is solved only for solar flux that's reflected/transmitted by the curtain on the first pass
        int nelem = get_nelem();
        int nyr = m_n_y- 1;
        int nx = m_n_x;
        double qnet_ap_sol, qnet_wf_sol;
        util::matrix_t<double> Esol, qnetc_sol, qnetw_sol;
        Esol.resize_fill(nelem, 0.0);
        for (int i = 0; i < m_n_x; i++)
        {
            for (int j = 0; j < m_n_y - 1; j++)
            {
                Esol.at(1 + j * nx + i) = soln.rhoc.at(j, i) * soln.q_dot_inc.at(j, i);            // Energy "source" at front curtain surface
                Esol.at(1 + nyr * nx + j * nx + i) = soln.tauc.at(j, i) * soln.q_dot_inc.at(j, i);    // Energy "source" at back curtain surface
            }
        }
        calculate_radiative_exchange(Esol, soln.Kinv, soln.rhoc, soln.tauc, qnetc_sol, qnetw_sol, qnet_wf_sol, qnet_ap_sol);
        for (int i = 0; i < m_n_x; i++)
        {
            for (int j = 0; j < m_n_y; j++)
            {
                qnetc_sol.at(j, i) += soln.q_dot_inc.at(j, i);  // Radiative exchange solution above was only for reflected/transmitted energy, add incoming solar here
            }
        }
        Q_refl = qnet_ap_sol * m_ap_area;   // Solar reflection loss [W]


        //--- Initialize solutions for particle and wall temperatures
        double Twf, Twfnew;
        util::matrix_t<double> Tp, Tw, Tpnew, Twnew;
        Tp.resize_fill(m_n_y, m_n_x, T_cold_in);        // Particle temperature [K]
        Tpnew.resize_fill(m_n_y, m_n_x, T_cold_in);
        Tw.resize_fill(m_n_y, m_n_x, T_cold_in);       // Back wall temperature [K]
        Twnew.resize_fill(m_n_y, m_n_x, T_cold_in);

        //--- Set initial guess for particle and wall temperatures
        if (soln_exists) // Use existing solution as the initial guess
        {
            Tp = soln.T_p;
            Tw = soln.T_back_wall;
            Twf = soln.T_front_wall;
        }
        else
        {
            double power_approx, Tout_approx, dT, qnet_approx;
            double eta_est = 0.85;
            cp = field_htfProps.Cp(T_particle_prop) * 1000.0;			        // Particle specific heat  [J/kg-K]
            util::matrix_t<double> flux_sum_over_heights = sum_over_rows(soln.q_dot_inc, true);
            for (int i = 0; i < m_n_x; i++)
            {
                power_approx = eta_est * flux_sum_over_heights.at(i) * m_curtain_elem_area;  // Approximate power to particles [W]
                Tout_approx = T_cold_in + power_approx / cp / mdot_per_elem.at(i);  // Approximate particle exit temperature [K]
                dT = (Tout_approx - T_cold_in) / (m_n_y - 1);
                for (int j = 0; j < m_n_y; j++)
                {
                    Tp.at(j, i) = soln.T_particle_cold_in + j * dT;
                    qnet_approx = m_curtain_emis * CSP::sigma * pow(Tp.at(j, i), 4) + m_cav_emis * soln.tauc.at(j, i) * soln.q_dot_inc.at(j, i); //Approximate net radiative heat transfer to wall (W/m2)
                    Tw.at(j, i) = pow(qnet_approx / (m_cav_emis * CSP::sigma), 0.25);
                }
            }
            Twf = T_cold_in;  // TODO: Better guess for front wall temperature
        }


        //-- Temperature solution iterations
        double Q_inc, Q_refl, Q_rad, Q_adv, Q_transport, Q_thermal;
        double Tpout, vel_out, Tfilm, hadv, fwind, qnet_ap, qnet_wf, hwconv, qadv, qtot, dh, cp;
        util::matrix_t<double> EIR, qnetc, qnetw;
        EIR.resize(nelem);
        cp = field_htfProps.Cp(T_particle_prop) * 1000.0;		    // Particle specific heat  [J/kg-K]
        vel_out = calculate_mass_wtd_avg_exit(mdot_per_elem, soln.vel);  // Mass-weighted average exit velocity
        for (int q = 0; q < max_iter; q++)
        {
            Q_rad = Q_adv = Q_transport = Q_thermal = 0.0;

            //-- Calculate advection loss coefficient
            if (m_hadv_model == 0)
            {
                hadv = m_hadv_user;
                fwind = 1.0;
            }
            else if (m_hadv_model == 1) // Use Sandia's correlations for advective loss
            {
                Tpout = calculate_mass_wtd_avg_exit(mdot_per_elem, Tp);
                Tfilm = 0.5 * (0.5 * (Tpout + T_cold_in) + T_amb);
                calculate_advection_coeff_sandia(vel_out,  Tfilm, v_wind, wdir, P_amb, hadv, fwind);
                hadv = fmax(hadv, 0.0);
            }
            else  // TODO: Need an error here... eventually expand to include a lookup table
            {
                hadv = 0.0;
                fwind = 1.0;
            }

            //-- Solve IR radiative exchange
            EIR.at(0) = CSP::sigma * pow(T_sky, 4);                   // Back cavity wall
            EIR.at(nelem - 1) = m_cav_emis * CSP::sigma * pow(Twf, 4);  // Front cavity wall
            for (int i = 0; i < m_n_x; i++)
            {
                for (int j = 0; j < m_n_y - 1; j++)
                {
                    EIR.at(1 + j * nx + i) = m_curtain_emis * CSP::sigma * pow(Tp.at(j, i), 4);             // Front curtain surface
                    EIR.at(1 + nx * nyr + j * nx + i) = m_curtain_emis * CSP::sigma * pow(Tp.at(j, i), 4);  // Back curtain surface
                    EIR.at(1 + 2 * nx * nyr + j * nx + i) = m_cav_emis * CSP::sigma * pow(Tw.at(j, i), 4);    // Back wall surface
                }
            }
            calculate_radiative_exchange(EIR, soln.Kinv, soln.rhoc, soln.tauc, qnetc, qnetw, qnet_wf, qnet_ap);
            Q_rad = qnet_ap * m_ap_area;    // IR radiation loss [W]

            //--- Combine solar and IR exchange
            qnet_ap += qnet_ap_sol;
            qnet_wf += qnet_wf_sol;
            for (int i = 0; i < m_n_x; i++)
            {
                for (int j = 0; j < m_n_y; j++)
                {
                    qnetc.at(j, i) += qnetc_sol.at(j, i);
                    qnetw.at(j, i) += qnetw_sol.at(j, i);
                }
            }


            //--- Calculate new wall and particle temperature solutions
            for (int i = 0; i < m_n_x; i++)
            {
                for (int j = 0; j < m_n_y; j++)
                {
                    hwconv = m_include_back_wall_convection ? calculate_wall_convection_coeff(soln.vel.at(j, i), (j + 0.5) * dy, 0.5 * (Tw.at(j, i) + Tp.at(j, i)), P_amb) : 0.0;

                    // Solve for particle temperature at next vertical node
                    qadv = hadv * fwind * (Tp.at(j, i) - T_amb);
                    if (j < m_n_y - 1)
                    {
                        qtot = qnetc.at(j, i) - qadv - hwconv * (Tp.at(j, i) - Tw.at(j, i));
                        dh = qtot * (dy / (soln.phip.at(j + 1, i) * soln.thc.at(j + 1, i) + soln.vel.at(j + 1, i) * particle_density));
                        Tpnew.at(j + 1, i) = Tpnew.at(j, i) + dh / cp;
                        Q_adv += qadv * m_curtain_elem_area;
                        Q_thermal += dh * mdot_per_elem.at(i) * m_curtain_elem_area;
                    }

                    // Solve for back wall temperature at current node
                    double Tcond_prev, Tcond_next;
                    if (m_include_wall_axial_conduction)
                    {
                        if (j == m_n_y - 1)
                            Tcond_next = Twnew.at(j - 1, i);  // Tw(j+1) = Tw(j-1) for heat flux = 0 boundary condition at last node
                        else if (i == 0)  // First iteration doesn't have a good value for Tw(j+1) already defined
                            Tcond_next = (j == 0) ? Tw.at(j, i) : Tw.at(j, i) + (Twnew.at(j, i) - Twnew.at(j - 1, i));  //As a first guess, assume wall temperature rise from (j to j+1) is the same as that from (j-1 to j)
                        else
                            Tcond_next = Tw.at(j + 1, i);
                        Tcond_prev = (j > 0) ? Twnew.at(j - 1, i) : Tcond_next;
                    }
                    Twnew.at(j, i) = calculate_passive_surface_T(Tw.at(j, i), qnetw.at(j, i), hwconv, Tp.at(j, i), T_amb, m_include_wall_axial_conduction, Tcond_prev, Tcond_next);

                }
            }
            Twfnew = calculate_passive_surface_T(Twf, qnet_wf, 0.0, 0.0, T_amb, false, 0.0, 0.0);

            //--- Calculate losses

            //--- Check convergence and update temperature solution
        }
    }



	if (soln.T_particle_hot < soln.T_particle_cold_in)
		soln.mode = C_csp_collector_receiver::OFF;


	// Save overall energy loss
    soln.Q_inc = Q_inc;
    soln.Q_thermal = Q_thermal;
	soln.Q_refl = Q_refl;
	soln.Q_rad = Q_rad;
	soln.Q_adv = Q_adv;
    soln.Q_transport = Q_transport;
    soln.eta = soln.Q_inc>0 ? soln.Q_thermal / soln.Q_inc : 0.0;   


	soln.rec_is_off = false;
	if (soln.mode == C_csp_collector_receiver::OFF)
		soln.rec_is_off = true;

	// Save final temperature profile solution
	if (!soln.rec_is_off)
	{
        soln.T_particle_hot = Tp_out;
	}

	return;

}

// Calculate mass flow rate needed to achieve target outlet temperature (m_T_salt_hot_target) given incident flux profiles
void C_falling_particle_receiver::solve_for_mass_flow(s_steady_state_soln &soln)
{

	bool soln_exists = (soln.m_dot_tot == soln.m_dot_tot);

    double T_particle_prop = (m_T_particle_hot_target + soln.T_particle_cold_in) / 2.0; // Temperature for particle property evaluation [K].  
    double cp = field_htfProps.Cp(T_particle_prop) * 1000.0;   //[J/kg-K] 

	double m_dot_guess;
	if (soln_exists)  // Use existing solution as initial guess
	{
		m_dot_guess = soln.m_dot_tot;
	}
	else  // Set initial guess for mass flow solution
	{
        double Qinc = sum_over_rows_and_cols(soln.q_dot_inc, true) * m_curtain_elem_area;  // Total solar power incident on curtain [W]
        double eta_guess = m_model_type == 0 ? m_fixed_efficiency : 0.85;
        m_dot_guess = eta_guess * Qinc / (cp * (m_T_particle_hot_target - soln.T_particle_cold_in));	//[kg/s] Particle mass flow rate
	}


	// Set solution tolerance
	double T_hot_guess = 9999.9;		//[K] Initial guess value for error calculation
	double err = -999.9;					//[-] Relative outlet temperature error
	double tol = std::numeric_limits<double>::quiet_NaN();
	tol = 0.00025;
	int qq_max = 50;
	int qq = 0;

	bool converged = false;
	while (!converged)
	{
		qq++;

		// if the problem fails to converge after 50 iterations, then the power is likely negligible and the zero set can be returned
		if (qq > qq_max)
		{
			soln.mode = C_csp_collector_receiver::OFF;  // Set the startup mode
			soln.rec_is_off = true;
			break;
		}

		soln.m_dot_tot = m_dot_guess;
		double tolT = tol;
		calculate_steady_state_soln(soln, tolT, m_use_constant_piping_loss, 50);   // Solve steady state thermal model		
		err = (soln.T_particle_hot - m_T_particle_hot_target) / m_T_particle_hot_target;
		
		if (soln.rec_is_off)  // SS solution was unsuccessful or resulted in an infeasible exit temperature -> remove outlet T for solution to start next iteration from the default intial guess
			soln.T_particle_hot = std::numeric_limits<double>::quiet_NaN();

		if (std::abs(err) > tol)
		{
			m_dot_guess = soln.Q_thermal / (cp * (m_T_particle_hot_target - soln.T_particle_cold_in));			//[kg/s]
			if (m_dot_guess < 1.E-5)
			{
				soln.mode = C_csp_collector_receiver::OFF;
				soln.rec_is_off = true;
				break;
			}
		}
		//else if (err > 0.0)  // Solution has converged but outlet T is above target.  CSP solver seems to perform better with slightly under-design temperature than with slightly over-design temperatures. 
		//	m_dot_salt_guess *= (soln.T_salt_hot - soln.T_salt_cold_in) / ((1.0 - 0.5*tol) * m_T_salt_hot_target - soln.T_salt_cold_in);
		else
			converged = true;
	}

	return;
}

// Calculate mass flow rate and defocus needed to achieve target outlet temperature given DNI
void C_falling_particle_receiver::solve_for_mass_flow_and_defocus(s_steady_state_soln &soln, double m_dot_htf_max, const util::matrix_t<double> *flux_map_input)
{

	bool rec_is_defocusing = true;
	double err_od = 999.0;

	while (rec_is_defocusing)
	{
		if (soln.rec_is_off)
			break;

		soln.q_dot_inc = calculate_flux_profiles(soln.flux_sum, soln.dni_applied_to_measured, soln.plant_defocus, soln.od_control, flux_map_input);  // Calculate flux profiles
		solve_for_mass_flow(soln);	// Iterative calculation of mass flow to produce target outlet temperature

		if (soln.rec_is_off)
			break;

		double m_dot_tot = soln.m_dot_tot;

		// Limit the HTF mass flow rate to the maximum, if needed
		rec_is_defocusing = false;
		if ((m_dot_tot > m_dot_htf_max))
		{
			double err_od = (m_dot_tot - m_dot_htf_max) / m_dot_htf_max;
			if (err_od < m_tol_od)
			{
				rec_is_defocusing = false;
			}
			else
			{
				soln.od_control = soln.od_control * pow((m_dot_htf_max / m_dot_tot), 0.8);	//[-] Adjust the over-design defocus control by modifying the current value
				rec_is_defocusing = true;
			}
		}

	}

	return;
}

// Calculate defocus needed to maintain outlet temperature under the target value given DNI and mass flow

void C_falling_particle_receiver::solve_for_defocus_given_flow(s_steady_state_soln &soln, const util::matrix_t<double> *flux_map_input)
{ 
	
	double Tprev, od, odprev, odlow, odhigh;
	double tolT = 0.00025;
	double urf = 0.8;

	Tprev = odprev = odlow = std::numeric_limits<double>::quiet_NaN();
	od = soln.od_control;
	odhigh = 1.0;

	od = odprev * (m_T_particle_hot_target - soln.T_particle_cold_in) / (Tprev - soln.T_particle_cold_in);

	int q = 0;
	while (q<50)
	{
		soln.od_control = od;
		if (odprev != odprev)
			soln.q_dot_inc = calculate_flux_profiles(soln.flux_sum, soln.dni_applied_to_measured, soln.plant_defocus, soln.od_control, flux_map_input);
		else
			soln.q_dot_inc = soln.q_dot_inc * soln.od_control / odprev; // Calculate flux profiles (note flux is directly proportional to defocus control)
		
		calculate_steady_state_soln(soln, tolT, m_use_constant_piping_loss);     // Solve steady state thermal model 

		if (soln.od_control > 0.9999 && soln.T_particle_hot < m_T_particle_hot_target)  // Impossible for solution to achieve temperature target
			break;
		else if ((std::abs(soln.T_particle_hot - m_T_particle_hot_target) / m_T_particle_hot_target) < tolT)
			break;
		else
		{
			
			if (soln.rec_is_off)
			{
				odlow = soln.od_control;
				od = odlow + 0.5*(odhigh - odlow);
			}
			else if (odprev != odprev)
			{
				od = odprev * (m_T_particle_hot_target - soln.T_particle_cold_in) / (Tprev - soln.T_particle_cold_in);
			}
			else
			{
				double delta_od = (soln.T_particle_hot - m_T_particle_hot_target) / ((soln.T_particle_hot - Tprev) / (soln.od_control - odprev));
				double od = soln.od_control - urf * delta_od;
				if (od < odlow || od > odhigh)
				{
					if (odlow == odlow)
						od = odlow + 0.5*(odhigh - odlow);
					else
						od = od * 0.95*odhigh;
				}
			}

			odprev = soln.od_control;
			Tprev = soln.T_particle_hot;
		}
		q++;
	}

	return;
}



// Solve mass and momentum balances for particle velocity, curtain thickness, particle volume fraction
// TODO: Generalize for non-uniform mass flow along the curtain width
void C_falling_particle_receiver::solve_mass_momentum(util::matrix_t<double>& mdot_per_elem, util::matrix_t<double>& phip, util::matrix_t<double>& vel, util::matrix_t<double>& th)
{
    int i, j, ny, nx;
    ny = m_n_y;
    nx = m_n_x;

    double g = 9.81;
    double dx = m_curtain_width / nx;
    double dy = m_curtain_height / (ny - 1);
    double initial_fall_height = m_initial_fall_height_ratio * m_curtain_height;
    double Tavg = 0.5 * (m_T_htf_cold_des + m_T_htf_hot_des);
    double rhop = field_htfProps.dens(Tavg, 1.0);


    phip.resize_fill(ny, nx, 0.0);
    vel.resize_fill(ny, nx, 0.0);
    th.resize_fill(ny, nx, 0.0);
    

    // Set mass flow per zone and initial condition at the top of the cavity
    for (i = 0; i < nx; i++)
    {
        phip.at(0, i) = m_phi0;
        vel.at(0, i) = pow(2 * g * initial_fall_height, 0.5);   // Initial particle velocity (m/s) assuming no drag
        th.at(0, i) = mdot_per_elem.at(i) / (phip.at(0, i) * vel.at(0, i) * dx * rhop);
    }

    for (j = 0; j < ny-1; j++)
    {
        for (i = 0; i < nx; i++)
        {
            th.at(j+1,i) = th.at(j,i) + m_dthdy * dy;
            vel.at(j+1,i) = vel.at(j,i) + dy * 9.8 / vel.at(j,i);
            phip.at(j+1,i) = (phip.at(j,i) * th.at(j,i) * vel.at(j,i)) / (th.at(j+1,i) * vel.at(j+1,i));
        }
    }
    return;
}



// Calculate curtain optical properties based on approximation in Gonzalez et al. Solar Energy 213 (2021) 211–224 (https://doi.org/10.1016/j.solener.2020.11.012)
void C_falling_particle_receiver::calculate_local_curtain_optical_properties(double th, double phip, double &rhoc, double &tauc)
{
    double dp = m_particle_dp;
    double length = pow((4. / 3.) * CSP::pi * pow(0.5 * dp, 3) / phip, (1. / 3.));  // Side length of virtual cube containing one particle
    double phis = CSP::pi * pow(0.5 * dp, 2) / pow(length, 2);  // Probability of beam hitting particle in cube (assumes beam strikes cube head-on)
    double Pb = 0.5 * (1.0 - m_particle_abs);   // Probability of backward reflection
    double Ps = 0.125 * (1.0 - m_particle_abs); // Probability of side reflection
    double Nl = th / length;                    // Number of layers in curtain
    double f = 1.0 / (1.0 - Pb - 2 * Ps) + (Pb + 2 * Ps) / pow(1 - Pb - 2 * Ps, 2);  
    double rhol1 = Pb * phis + 4 * f * pow(Ps * phis, 2) / phis;
    rhoc = rhol1 * (1.0 - pow(1.0-phis, 2*Nl)) / (1.0 - pow(1.0-phis, 2)); // Curtain reflectance

    double tc0 = pow(1-phis, Nl);  // Transmittance without hitting any layers
    double tcs = Nl*tc0*4*f*pow(Ps*phis, 2) / phis;
    double tcbf = pow(rhol1, 2) * tc0 * (pow(1-phis, 2*Nl) - Nl*pow(1-phis, 2) + Nl - 1) / pow(pow(phis, 2) - 2*phis, 2);
    tauc = tc0 + tcs + tcbf;  //Curtain transmittance
    tauc *= m_tauc_mult;  // Optionally adjust by a user-provided multiplier for transmissivity (default value of multiplier = 1.0)
    return;
}



// Solve mass and momentum balances for particle velocity, curtain thickness, particle volume fraction
// TODO: Generalize for non-uniform mass flow along the curtain width
void C_falling_particle_receiver::calculate_curtain_optical_properties(util::matrix_t<double>& phip, util::matrix_t<double>& th,
                                                                       util::matrix_t<double>& rhoc, util::matrix_t<double>& tauc)
{
    int ny = m_n_y;
    int nx = m_n_x;
    double tauc1, rhoc1;
    tauc.resize_fill(ny, nx, 0.0);
    rhoc.resize_fill(ny, nx, 0.0);
    for (int j = 0; j < ny; j++)
    {
        for (int i = 0; i < nx; i++)
        {
            calculate_local_curtain_optical_properties(th.at(j, i), phip.at(j, i), rhoc1, tauc1);
            rhoc.at(j, i) = rhoc1;
            tauc.at(j, i) = tauc1;
        }
    }
    return;
}



// Advective loss coefficients from the best-fit correlations to Sandia's CFD model based on Gonzalez et al. Solar Energy 255 (2023) 301–313 (https://doi.org/10.1016/j.solener.2023.03.046)
void C_falling_particle_receiver::calculate_advection_coeff_sandia(double vel, double Tprop, double wspd, double wdir, double Pamb, double &hadv, double &fwind)
{
    
    double A, B, C, D, E, F, G, H;
    A = -12331;
    B = 1.949;
    C = 0.7002;
    D = 0.2370;
    E = 0.0089;
    F = 178.7;
    G = 134.3;
    H = 27.49;

    double k_air = ambient_air.cond(Tprop);				//[W/m-K] The conductivity of the ambient air
    double mu_air = ambient_air.visc(Tprop);			//[kg/m-s] Dynamic viscosity of the ambient air
    double rho_air = ambient_air.dens(Tprop, Pamb);		//[kg/m^3] Density of the ambient air
    double Re = rho_air * vel * m_curtain_height / mu_air;
    double Nu = A + B * pow(Re, C);
    hadv = Nu * k_air / m_curtain_height;   // Advective loss coefficient without wind (W/m2/K)

    double wind_factor = 1.0;
    double phi_wind = exp(-pow((std::abs(wdir - F) - G) / H, 2));
    fwind = 1.0 + (D - E * m_ap_height) * wspd * phi_wind;

    return;
}


// Wall convection coefficient from flat-plate correlations
double C_falling_particle_receiver::calculate_wall_convection_coeff(double vel, double len, double Tprop, double P_amb)
{
    double k_air = ambient_air.cond(Tprop);				//[W/m-K] The conductivity of the ambient air
    double mu_air = ambient_air.visc(Tprop);			//[kg/m-s] Dynamic viscosity of the ambient air
    double rho_air = ambient_air.dens(Tprop, P_amb);		//[kg/m^3] Density of the ambient air
    double cp_air = ambient_air.Cp(Tprop) * 1000;	    //[J/kg-K] Specific heat of the ambient air

    double Pr = cp_air * mu_air / k_air;
    double Re = rho_air * vel * len / mu_air;
    double Nu = 0.0296 * pow(Re, 0.8) * pow(Pr, 1. / 3.);
    double h = Nu * k_air / len;
    return h;
}


int C_falling_particle_receiver::get_nelem()
{
    int ny = m_n_y - 1;  // Solution in y-direction is defined at node locations including inlet and outlet
    int nx = m_n_x;
    int nelem = 1 + 3 * nx * ny + 1;   // One element for the aperture, nx*ny elements on each side of the curtain and the back wall, one element for front wall
    return nelem;
}


void C_falling_particle_receiver::calculate_view_factors()
{
    int i, j, k, isym, ksym, ib, iw;

    int ny = m_n_y - 1;  // Solution in y-direction is defined at node locations including inlet and outlet
    int nx = m_n_x;
    int nelem = get_nelem();
    double dy = m_curtain_height / ny;
    double dx = m_curtain_width / nx;
    double ap_area = m_ap_height * m_ap_width;

    m_vf.resize_fill(nelem, nelem, 0.0);  // View factor matrix. Curtain height/width elements are ordered as (y0,x0), (y0,x1)... (y0,xn), (y1,x0)...

    double top_height;
    if (m_is_ap_at_bot)
        top_height = m_curtain_height - m_ap_height;  // Distance between top of cavity and top of aperture
    else
        top_height = 0.5 * (m_curtain_height - m_ap_height);
    double side_width = 0.5 * (m_cav_width - m_ap_width);


    // View factors from each curtain element to the aperture and front wall(note, this assumes the curtain is flat!)
    // TODO: Update for a curved curtain
    int nxhalf;
    if (nx % 2 == 0)
        nxhalf = (int)(nx / 2);
    else
        nxhalf = (int)((nx + 1) / 2);

    double vf_ap_sum = 0.0;
    double vf_fw_sum = 0.0;
    for (j = 0; j < ny; j++)
    {
        for (k = 0; k < nxhalf; k++)
        {
            i = 1 + j * nx + k;  // Front curtain element index in flattened array
            m_vf.at(i, 0) = vf_parallel_rect(k * dx, j * dy, side_width, top_height, dx, dy, m_ap_width, m_ap_height, m_curtain_dist);  // View factor from front curtain element to full aperture
            m_vf.at(0, i) = m_vf.at(i, 0) * (dx * dy / ap_area);                            // View factor from aperture to front curtain element
            m_vf.at(i, nelem - 1) = 1.0 - m_vf.at(i, 0);                                    // View factor from front curtain element to front wall. Each element can only see the aperture or the front wall 
            m_vf.at(nelem - 1, i) = m_vf.at(i, nelem - 1) * (dx * dy / m_cav_front_area);  // View factor from front wall to front curtain element

            ksym = nx - k - 1;
            isym = 1 + j * nx + ksym;   // Symmetric curtain element
            m_vf.at(isym, 0) = m_vf.at(i, 0);
            m_vf.at(0, isym) = m_vf.at(0, i);
            m_vf.at(isym, nelem - 1) = m_vf.at(i, nelem - 1);
            m_vf.at(nelem - 1, isym) = m_vf.at(nelem - 1, i);

            vf_ap_sum += m_vf.at(0, i) + m_vf.at(0, isym);                  // Sum of view factors from aperture to front curtain
            vf_fw_sum += m_vf.at(nelem - 1, i) + m_vf.at(nelem - 1, isym);  // Sum of view factors from front wall to front curtain
        }
    }

    // View factors from curtain back to back wall.  Currently assumes that curtain is close enough to the back wall that each curtain element can only see the back wall element immediately behind it.
    // TODO: Add option to calculate all curtain-to-back-wall view factors
    for (j = 0; j < ny; j++)
    {
        for (k = 0; k < nx; k++)
        {
            ib = 1 + (nx * ny) + j * nx + k;
            iw = 1 + 2 * (nx * ny) + j * nx + k;
            m_vf.at(ib, iw) = 1.0;
            m_vf.at(iw, ib) = 1.0;
        }
    }

    // View factors from aperture to front wall, and front wall to itself
    m_vf.at(0, nelem - 1) = 1.0 - vf_ap_sum;
    m_vf.at(nelem - 1, 0) = m_vf.at(0, nelem - 1) * (ap_area / m_cav_front_area);
    m_vf.at(nelem - 1, nelem - 1) = 1.0 - vf_fw_sum - m_vf.at(nelem - 1, 0);

    return;
}

// View factors between parallel rectangles (http://www.thermalradiation.net/sectionc/C-13.html)
// x1, x2=x1+dx, y1, y2=y1+dy are extents of surface 1; e1, e2=e1+de, n1, n2=n1+dn are extents of surface 2, z is the separation distance between rectangles 
double C_falling_particle_receiver::vf_parallel_rect(double x1, double y1, double e1, double n1, double dx, double dy, double de, double dn, double z)
{
    int i, j, k, m;
    double x, y, n, e, v1, v2, f;
    double vf = 0.0;
    for (i = 0; i < 2; i++)
    {
        for (j = 0; j < 2; j++)
        {
            for (k = 0; k < 2; k++)
            {
                for (m = 0; m < 2; m++)
                {
                    x = x1 + i * dx;
                    y = y1 + j * dy;
                    n = n1 + k * dn;
                    e = e1 + m * de;
                    v1 = pow(pow(x - e, 2) + pow(z, 2), 0.5);
                    v2 = pow(pow(y - n, 2) + pow(z, 2), 0.5);
                    f = (y - n) * v1 * atan((y - n) / v1) + (x - e) * v2 * atan((x - e) / v2) - 0.5 * pow(z, 2) * log(pow(x - e, 2) + pow(y - n, 2) + pow(z, 2));
                    vf += pow(-1.0, i + j + k + m) * (1. / 2. / CSP::pi) * f;
                }
            }
        }
    }
    vf /= (dx * dy);
    return vf;
}

// Set up array containing reflectivity of all surface elements
util::matrix_t<double> C_falling_particle_receiver::get_reflectivity_vector(util::matrix_t<double>& rhoc)
{
    int ny = m_n_y - 1;  // Soln in y-direction is defined at node locations including inlet and outlet
    int nx = m_n_x;
    int nelem = get_nelem();
    util::matrix_t<double> rho(nelem);
    int x, y;
    for (int i = 0; i < nelem; i++)
    {
        if (i == 0)
            rho.at(i) = 0.0;  // Aperture
        else if (i < 1 + 2 * (nx * ny))  // Front or back curtain
        {
            x = (i - 1) % nx;           // Width position on curtain
            y = (int)((i - 1) / nx);    // Height position on curtain
            rho.at(i) = rhoc.at(y, x);
        }
        else            // Back cavity wall or front cavity wall
            rho.at(i) = 1.0 - m_cav_emis;
    }
    return rho;
}

void C_falling_particle_receiver::calculate_coeff_matrix(util::matrix_t<double>& rhoc, util::matrix_t<double>& tauc, util::matrix_t<double>& K, util::matrix_t<double>& Kinv)
{

    // TODO: Generalize to include view factors between each curtain element and all back wall elements
    //       Simplify loops below if each curtain element can only see one back wall element (most of the elements in K are zero)
    int i, i1, i2, x, y, iback, ifront;
    int ny = m_n_y - 1;  // Solution in y-direction is defined at node locations including inlet and outlet
    int nx = m_n_x;
    int nelem = get_nelem();
    K.resize_fill(nelem, nelem, 0.0);  // Curtain height/width elements are ordered as (y0,x0), (y0,x1)... (y0,xn), (y1,x0)...
 

    // Create vector with reflectivity of all elements
    util::matrix_t<double> rho(nelem);
    rho = get_reflectivity_vector(rhoc);

    //--- Calculate coefficient matrix
    
    for (i1 = 0; i1 < nelem; i1++)
    {
        K.at(i1, i1) = 1.0;
        for (i2 = 0; i2 < nelem; i2++)
        {
            K.at(i1, i2) -= rho.at(i1) * m_vf.at(i1, i2);

            // Add modifications accounting for transmissivity of curtain
            if (i1 >= 1 && i1 < 1 + (nx * ny))  // Element on front of curtain
            {
                iback = i1 + nx * ny;       // Corresponding element on the back of the curtain
                x = (i1 - 1) % nx;          // Width position on curtain
                y = (int)((i1 - 1) / nx);     // Height position on curtain
                K.at(i1, i2) -= tauc.at(y, x) * m_vf.at(iback, i2);
            }

            else if (i1 >= 1 + (nx * ny) && i1 < 1 + (nx * ny))  // Element on back of curtain
            {
                ifront = i1 - nx * ny;              // Corresponding element on the front of the curtain
                x = (i1 - 1 - nx*ny) % nx;          // Width position on curtain
                y = (int)((i1 - 1 - nx*ny) / nx);  // Height position on curtain
                K.at(i1, i2) -= tauc.at(y, x) * m_vf.at(ifront, i2);
            }
        }
    }


    //--- Invert coefficient matrix
    invert(K, Kinv);

    return;
}

// Solve radiative exchange equations and calculate net radiative energy incoming to each element
void C_falling_particle_receiver::calculate_radiative_exchange(util::matrix_t<double>& Esource, util::matrix_t<double>& Kinv, util::matrix_t<double>& rhoc, util::matrix_t<double>& tauc,
                                                                util::matrix_t<double>& qnetc, util::matrix_t<double>& qnetw, double qnetwf, double qnetap)
{
    // Curtain height/width elements are ordered as (y0,x0), (y0,x1)... (y0,xn), (y1,x0)...
    int ny = m_n_y - 1;  // Solution in y-direction is defined at node locations including inlet and outlet
    int nx = m_n_x;
    int nelem = get_nelem();
    util::matrix_t<double> J, qin, qnet;
    J.resize_fill(nelem, 0.0);      // Radiosity [W/m2]
    qin.resize_fill(nelem, 0.0);    // Total incoming energy [W/m2]
    qnet.resize_fill(nelem, 0.0);   // Net incoming energy
    qnetc.resize_fill(m_n_y, nx, 0.0); // Net incoming energy to the curtain (both sides)
    qnetw.resize_fill(m_n_y, nx, 0.0); // Net incoming energy to the back wall


    // Radiosity (total outgoing energy)
    for (int i = 0; i < nelem; i++)
    {
        for (int j = 0; j < nelem; j++)
        {
            J.at(i) += Kinv.at(i, j) * Esource.at(j);
        }
    }

    // Total incoming energy and net incoming energy
    util::matrix_t<double> rho = get_reflectivity_vector(rhoc);
    int x, y;
    for (int i = 0; i < nelem; i++)
    {
        for (int j = 0; j < nelem; j++)
        {
            qin.at(i) += m_vf.at(i, j) * J.at(j);
        }
        qnet.at(i) = (1.0 - rho.at(i)) * qin.at(i) - Esource.at(i);

        // Modification to net incoming energy for curtain elements
        if (i > 0 && i < 1 + 2 * nx * ny)
        {
            int k = (i < 1 + nx * ny) ? 1 : 1 + nx * ny;
            x = (i - k) % nx;          // Width position on curtain
            y = (int)((i - k) / nx);  // Height position on curtain
        qnet.at(i) -= tauc.at(y, x) * qin.at(i);
        }
    }

    // Separate net incoming energy into separate outputs for curtain, back wall, front wall, aperture
    qnetap = qnet.at(0);   // Net energy incoming to aperture
    qnetwf += qnet.at(nelem - 1);   // Net energy incoming to front wall
    for (int i = 0; i < nx; i++)
    {
        for (int j = 0; j < ny; j++)
        {
            int kf = 1 + j * nx + i;            // Element on front of curtain
            int kb = 1 + nx * ny + j * nx + i;      // Element on back of curtain
            int kw = 1 + 2 * nx * ny + j * nx + i;    // Element on back wall
            qnetc.at(j, i) = qnet.at(kf) + qnet.at(kb);
            qnetw.at(j, i) = qnet.at(kw);

        }
        // Assume last vertical node is equal to previous node (this nodes is not used in the energy balances, so this is just to have something reasonable to complete wall temperature arrays)
        qnetc.at(m_n_y - 1, i) = qnetc.at(m_n_y - 2, i);
        qnetw.at(m_n_y - 1, i) = qnetc.at(m_n_y - 2, i);
    }


    return;
}


// Calculate passive surface temperature
double C_falling_particle_receiver::calculate_passive_surface_T(double Tguess, double qnet_rad, double hconv, double Tconv, double Tamb, bool is_axial_conduction, double Tcond_prev, double Tcond_next)
{
    // Twguess = current guess for wall temperature
    // qnet_rad = current estimate of net radiative heat flux into wall (incoming - outgoing including emission)
    // Tamb = ambient temperature
    // hconv, Tconv = heat transfer coefficient and temperature
    // is_axial_conduction = include axial condition?  If True, Tcond_prev and Tcond_next are current guess for solution at previous/next wall temperature nodes

    // Wall energy balance: d/dy(kdTw/dy) + qrad - hconv*(Tw-Tconv) - qext = 0  (with axial conduction) where qrad = net incoming radiative flux
    // Solution is based on current guess for cavity radiative exchange with wall emission recomputed:
    //      qrad = qrad* - (Ew - Ew*) where qrad* = current estimate of radiative flux at Tguess, Ew* = wall emission at Tguess, Ew = wall emission at new wall T
    //      New wall temperature solution is found by iterating on linearization temperature (Tw^4 = Tlin^4 * 4Tlin^3*(Tw-Tlin) for numerical stability


    double Rwall, dy, num, den, Tlin, Tw, Tdiff, tol, urf;
    int niter = 10;  // Max number of iterations
    tol = 0.1;   // Tolerance for wall temperature solution
    Rwall = 1.0 / m_cav_hext + m_cav_twall / m_cav_kwall;  // Cavity wall thermal resistance
    dy = m_curtain_height / (m_n_y - 1); urf = 1.0;

    Tlin = Tguess;
    for (int m = 0; m < niter; m++)
    {
        den = 4 * m_cav_emis * CSP::sigma * pow(Tlin, 3) + hconv + 1.0 / Rwall;
        num = qnet_rad + m_cav_emis * CSP::sigma * (3 * pow(Tlin, 4) + pow(Tguess, 4)) + hconv * Tconv + Tamb / Rwall;
        if (is_axial_conduction)
        {
            den += 2 * m_cav_kwall / pow(dy, 2);
            num += m_cav_kwall * (Tcond_next + Tcond_prev) / (pow(dy, 2));
        }
        Tw = num / den;
        Tdiff = std::abs(Tw- Tlin);
        if (Tdiff < tol)
            break;
        Tlin = (1.0 - urf) * Tlin + urf*Tw;
    }
    return Tw;
}

// Calculate mass-weighted average particle exit temperature
double C_falling_particle_receiver::calculate_mass_wtd_avg_exit(util::matrix_t<double>& m_flow_per_elem, util::matrix_t<double>& mat)
{
    int nx = (int)mat.ncols();
    int ny = (int)mat.nrows();
    double mtot = 0.0;
    double outlet = 0.0;
    for (int i = 0; i < nx; i++)
    {
        mtot += m_flow_per_elem.at(i);
        outlet += m_flow_per_elem.at(i) * mat.at(ny - 1, i);
    }
    outlet /= mtot;
    return outlet;
}




// Matrix inversion (Gauss-Jordan)
// TODO: Update this to use some pre-existing package?
void C_falling_particle_receiver::invert(util::matrix_t<double>& A, util::matrix_t<double>& Ainv)
{
    size_t i, j, k, nelem;
    double m;
    nelem = A.nrows();
    util::matrix_t<double> A1;
    A.copy(A1);
    Ainv.resize_fill(nelem, nelem, 0.0);
    for (i = 0; i < nelem; i++)
        Ainv.at(i, i) = 1.0;

    for (i = 0; i < nelem; i++)
    {
        // Make diagonal entry in row i equal to 1.0
        m = A.at(i, i);
        for (j = 0; j < nelem; j++) 
        {
            A.at(i, j) = A.at(i, j) / m;
            Ainv.at(i, j) = Ainv.at(i, j) / m;
        }

        // Use row i to eliminate entries in all other rows
        for (k = 0; k < nelem; k++)
        {
            if (k != i)
            {
                m = A1.at(k, i);
                for (j = 0; j < nelem; j++)
                {
                    A1.at(k, j) = A1.at(k, j) - m * A1.at(i, j);
                    Ainv.at(k, j) = Ainv.at(k, j) - m * Ainv.at(i, j);
                }
            }
        }
    }
    return;
}

util::matrix_t<double> C_falling_particle_receiver::sum_over_rows(util::matrix_t<double>& mat, bool exclude_last)
{
    int nr = mat.nrows();
    int nc = mat.ncols();
    int nrsum = nr;
    if (exclude_last)
        nrsum -= 1;

    util::matrix_t<double> msum;
    msum.resize_fill(nc, 0.0);
    for (int i = 0; i < nc; i++)
    {
        for (int j = 0; j < nrsum; j++)
            msum.at(i) += mat.at(j, i);
    }
    return msum;
}

util::matrix_t<double> C_falling_particle_receiver::sum_over_cols(util::matrix_t<double>& mat)
{
    int nr = mat.nrows();
    int nc = mat.ncols();
    util::matrix_t<double> msum;
    msum.resize_fill(nr, 0.0);
    for (int j = 0; j < nr; j++)
    {
        for (int i = 0; i < nc; i++)
            msum.at(j) += mat.at(j, i);
    }
    return msum;
}

double C_falling_particle_receiver::sum_over_rows_and_cols(util::matrix_t<double>& mat, bool exclude_last_row)
{
    int nr = mat.nrows();
    int nc = mat.ncols();
    int nrsum = nr;
    if (exclude_last_row)
        nrsum -= 1;

    double msum = 0.0;
    for (int j = 0; j < nrsum; j++) {
        for (int i = 0; i < nc; i++) {
            msum += mat.at(j, i);
        }
    }
    return msum;
}

