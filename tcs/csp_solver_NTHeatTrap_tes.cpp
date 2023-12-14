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


#include "csp_solver_NTHeatTrap_tes.h"

static C_csp_reported_outputs::S_output_info S_output_info[] =
{
    {C_csp_NTHeatTrap_tes::E_Q_DOT_LOSS, C_csp_reported_outputs::TS_WEIGHTED_AVE},		//[MWt] TES thermal losses
    {C_csp_NTHeatTrap_tes::E_W_DOT_HEATER, C_csp_reported_outputs::TS_WEIGHTED_AVE},		//[MWe] TES freeze protection power
    {C_csp_NTHeatTrap_tes::E_TES_T_HOT, C_csp_reported_outputs::TS_LAST},					//[C] TES final hot tank temperature
    {C_csp_NTHeatTrap_tes::E_TES_T_COLD, C_csp_reported_outputs::TS_LAST},				//[C] TES cold temperature at end of timestep      
    {C_csp_NTHeatTrap_tes::E_M_DOT_TANK_TO_TANK, C_csp_reported_outputs::TS_WEIGHTED_AVE},		//[MWt] TES thermal losses
    {C_csp_NTHeatTrap_tes::E_MASS_COLD_TANK, C_csp_reported_outputs::TS_LAST},			//[kg] Mass in cold tank at end of timestep		
    {C_csp_NTHeatTrap_tes::E_MASS_HOT_TANK, C_csp_reported_outputs::TS_LAST},				//[kg] Mass in hot tank at end of timestep
    {C_csp_NTHeatTrap_tes::E_HOT_TANK_HTF_PERC_FINAL, C_csp_reported_outputs::TS_LAST},	//[%] Final percent fill of available hot tank mass
    {C_csp_NTHeatTrap_tes::E_W_DOT_HTF_PUMP, C_csp_reported_outputs::TS_WEIGHTED_AVE},	//[MWe] 

    csp_info_invalid
};

C_csp_NTHeatTrap_tes::C_csp_NTHeatTrap_tes(
    int external_fl,                             // [-] external fluid identifier
    util::matrix_t<double> external_fl_props,    // [-] external fluid properties
    int tes_fl,                                  // [-] tes fluid identifier
    util::matrix_t<double> tes_fl_props,         // [-] tes fluid properties
    double q_dot_design,                         // [MWt] Design heat rate in and out of tes
    double frac_max_q_dot,                       // [-] the max design heat rate as a fraction of the nominal
    double Q_tes_des,			                 // [MWt-hr] design storage capacity
    double h_tank,			                     // [m] tank height
    double u_tank,			                     // [W/m^2-K]
    int tank_pairs,			                     // [-]
    double hot_tank_Thtr,		                 // [C] convert to K in init()
    double hot_tank_max_heat,	                 // [MW]
    double cold_tank_Thtr,	                     // [C] convert to K in init()
    double cold_tank_max_heat,                   // [MW]
    double dt_hot,			                     // [C] Temperature difference across heat exchanger - assume hot and cold deltaTs are equal
    double T_cold_des,	                         // [C] convert to K in init()
    double T_hot_des,	                         // [C] convert to K in init()
    double T_tank_hot_ini,	                     // [C] Initial temperature in hot storage tank
    double T_tank_cold_ini,	                     // [C] Initial temperature in cold storage cold
    double h_tank_min,		                     // [m] Minimum allowable HTF height in storage tank
    double f_V_hot_ini,                          // [%] Initial fraction of available volume that is hot
    double htf_pump_coef,		                 // [kW/kg/s] Pumping power to move 1 kg/s of HTF through sink
    bool tanks_in_parallel,                      // [-] Whether the tanks are in series or parallel with the external system. Series means external htf must go through storage tanks.
    double V_tes_des,                            // [m/s] Design-point velocity for sizing the diameters of the TES piping
    bool calc_design_pipe_vals,                  // [-] Should the HTF state be calculated at design conditions
    double tes_pump_coef,		                 // [kW/kg/s] Pumping power to move 1 kg/s of HTF through tes loop
    double eta_pump,                             // [-] Pump efficiency, for newer pumping calculations
    bool has_hot_tank_bypass,                    // [-] True if the bypass valve causes the source htf to bypass just the hot tank and enter the cold tank before flowing back to the external system.
    double T_tank_hot_inlet_min,                 // [C] Minimum source htf temperature that may enter the hot tank
    bool custom_tes_p_loss,                      // [-] True if the TES piping losses should be calculated using the TES pipe lengths and minor loss coeffs, false if using the pumping loss parameters
    bool custom_tes_pipe_sizes,                  // [-] True if the TES diameters and wall thicknesses parameters should be used instead of calculating them
    util::matrix_t<double> k_tes_loss_coeffs,    // [-] Combined minor loss coefficients of the fittings and valves in the collection (including bypass) and generation loops in the TES 
    util::matrix_t<double> tes_diams,            // [m] Imported inner diameters for the TES piping as read from the modified output files
    util::matrix_t<double> tes_wallthicks,       // [m] Imported wall thicknesses for the TES piping as read from the modified output files
    util::matrix_t<double> tes_lengths,          // [m] Imported lengths for the TES piping as read from the modified output files
    double pipe_rough,                           // [m] Pipe absolute roughness
    double dP_discharge                          // [bar] Pressure drop on the TES discharge side (e.g., within the steam generator)
    )
    :
        m_external_fl(external_fl), m_external_fl_props(external_fl_props), m_tes_fl(tes_fl), m_tes_fl_props(tes_fl_props),
        m_q_dot_design(q_dot_design), m_frac_max_q_dot(frac_max_q_dot), m_Q_tes_des(Q_tes_des), m_h_tank(h_tank),
        m_u_tank(u_tank), m_tank_pairs(tank_pairs), m_hot_tank_Thtr(hot_tank_Thtr), m_hot_tank_max_heat(hot_tank_max_heat),
        m_cold_tank_Thtr(cold_tank_Thtr), m_cold_tank_max_heat(cold_tank_max_heat), m_dt_hot(dt_hot), m_T_cold_des(T_cold_des),
        m_T_hot_des(T_hot_des), m_T_tank_hot_ini(T_tank_hot_ini), m_T_tank_cold_ini(T_tank_cold_ini),
        m_h_tank_min(h_tank_min), m_f_V_hot_ini(f_V_hot_ini), m_htf_pump_coef(htf_pump_coef), tanks_in_parallel(tanks_in_parallel),
        V_tes_des(V_tes_des), calc_design_pipe_vals(calc_design_pipe_vals), m_tes_pump_coef(tes_pump_coef),
        eta_pump(eta_pump), has_hot_tank_bypass(has_hot_tank_bypass), T_tank_hot_inlet_min(T_tank_hot_inlet_min),
        custom_tes_p_loss(custom_tes_p_loss), custom_tes_pipe_sizes(custom_tes_pipe_sizes), k_tes_loss_coeffs(k_tes_loss_coeffs),
        tes_diams(tes_diams), tes_wallthicks(tes_wallthicks), tes_lengths(tes_lengths),
        pipe_rough(pipe_rough), dP_discharge(dP_discharge)
{

    if (tes_lengths.ncells() < 11) {
        double lengths[11] = { 0., 90., 100., 120., 0., 30., 90., 80., 80., 120., 80. };
        this->tes_lengths.assign(lengths, 11);
    }

    m_vol_tank = m_V_tank_active = m_q_pb_design = m_ts_hours =
        m_V_tank_hot_ini = m_mass_total_active = m_d_tank = m_q_dot_loss_des =
        m_cp_external_avg = m_rho_store_avg = m_m_dot_tes_des_over_m_dot_external_des = std::numeric_limits<double>::quiet_NaN();

    mc_reported_outputs.construct(S_output_info);
}

C_csp_NTHeatTrap_tes::C_csp_NTHeatTrap_tes()
{
    m_vol_tank = m_V_tank_active = m_q_pb_design = m_Q_tes_des =
        m_V_tank_hot_ini = m_mass_total_active = m_d_tank = m_q_dot_loss_des =
        m_cp_external_avg = m_rho_store_avg = m_m_dot_tes_des_over_m_dot_external_des = std::numeric_limits<double>::quiet_NaN();

    mc_reported_outputs.construct(S_output_info);
}


void C_csp_NTHeatTrap_tes::init(const C_csp_tes::S_csp_tes_init_inputs init_inputs)
{
    if (!(m_Q_tes_des > 0.0))
    {
        m_is_tes = false;
        return;		// No storage!
    }

    m_is_tes = true;

    // Declare instance of fluid class for EXTERNAL fluid
    // Set fluid number and copy over fluid matrix if it makes sense
    if (m_external_fl != HTFProperties::User_defined && m_external_fl < HTFProperties::End_Library_Fluids)
    {
        if (!mc_external_htfProps.SetFluid(m_external_fl))
        {
            throw(C_csp_exception("External HTF code is not recognized", "Two Tank TES Initialization"));
        }
    }
    else if (m_external_fl == HTFProperties::User_defined)
    {
        int n_rows = (int)m_external_fl_props.nrows();
        int n_cols = (int)m_external_fl_props.ncols();
        if (n_rows > 2 && n_cols == 7)
        {
            if (!mc_external_htfProps.SetUserDefinedFluid(m_external_fl_props))
            {
                error_msg = util::format(mc_external_htfProps.UserFluidErrMessage(), n_rows, n_cols);
                throw(C_csp_exception(error_msg, "Two Tank TES Initialization"));
            }
        }
        else
        {
            error_msg = util::format("The user defined external HTF table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", n_rows, n_cols);
            throw(C_csp_exception(error_msg, "Two Tank TES Initialization"));
        }
    }
    else
    {
        throw(C_csp_exception("External HTF code is not recognized", "Two Tank TES Initialization"));
    }


    // Declare instance of fluid class for STORAGE fluid.
    // Set fluid number and copy over fluid matrix if it makes sense.
    if (m_tes_fl != HTFProperties::User_defined && m_tes_fl < HTFProperties::End_Library_Fluids)
    {
        if (!mc_store_htfProps.SetFluid(m_tes_fl))
        {
            throw(C_csp_exception("Storage HTF code is not recognized", "Two Tank TES Initialization"));
        }
    }
    else if (m_tes_fl == HTFProperties::User_defined)
    {
        int n_rows = (int)m_tes_fl_props.nrows();
        int n_cols = (int)m_tes_fl_props.ncols();
        if (n_rows > 2 && n_cols == 7)
        {
            if (!mc_store_htfProps.SetUserDefinedFluid(m_tes_fl_props))
            {
                error_msg = util::format(mc_store_htfProps.UserFluidErrMessage(), n_rows, n_cols);
                throw(C_csp_exception(error_msg, "Two Tank TES Initialization"));
            }
        }
        else
        {
            error_msg = util::format("The user defined storage HTF table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", n_rows, n_cols);
            throw(C_csp_exception(error_msg, "Two Tank TES Initialization"));
        }
    }
    else
    {
        throw(C_csp_exception("Storage HTF code is not recognized", "Two Tank TES Initialization"));
    }

    bool is_hx_calc = false;
    if (m_tes_fl != m_external_fl)
        is_hx_calc = true;
    else if (m_external_fl != HTFProperties::User_defined)
        is_hx_calc = false;
    else
    {
        is_hx_calc = !mc_external_htfProps.equals(&mc_store_htfProps);
    }
    if (is_hx_calc == true)
    {
        throw(C_csp_exception("NT Tank must have the same external and internal fluid", "NT TES Initialization"));
    }

    if (tanks_in_parallel) {
        m_is_cr_to_cold_tank_allowed = false;
    }
    else {
        m_is_cr_to_cold_tank_allowed = true;
    }

    // Calculate thermal power to PC at design
    m_q_pb_design = m_q_dot_design * 1.E6;	//[Wt]

    // Convert parameter units
    m_hot_tank_Thtr += 273.15;		//[K] convert from C
    m_cold_tank_Thtr += 273.15;		//[K] convert from C
    m_T_cold_des += 273.15;		    //[K] convert from C
    m_T_hot_des += 273.15;		    //[K] convert from C
    m_T_tank_hot_ini += 273.15;		//[K] convert from C
    m_T_tank_cold_ini += 273.15;		//[K] convert from C

    m_ts_hours = m_Q_tes_des / m_q_dot_design;

    double d_tank_temp = std::numeric_limits<double>::quiet_NaN();
    double q_dot_loss_temp = std::numeric_limits<double>::quiet_NaN();
    double T_tes_hot_des, T_tes_cold_des;

    T_tes_hot_des = m_T_hot_des;
    T_tes_cold_des = m_T_cold_des;

    two_tank_tes_sizing(mc_store_htfProps, m_Q_tes_des, T_tes_hot_des, T_tes_cold_des,
        m_h_tank_min, m_h_tank, m_tank_pairs, m_u_tank,
        m_V_tank_active, m_vol_tank, m_d_tank, m_q_dot_loss_des);

    // 5.13.15, twn: also be sure that hx is sized such that it can supply full load to sink
    double duty = m_q_pb_design * std::max(1.0, m_frac_max_q_dot);		//[W] Allow all energy from the source to go into storage at any time

    // Calculate initial storage values

    // Initial storage charge based on % mass 
    double T_tes_ave = 0.5 * (T_tes_hot_des + T_tes_cold_des);
    double cp_ave = mc_store_htfProps.Cp_ave(T_tes_cold_des, T_tes_hot_des);				//[kJ/kg-K] Specific heat at average temperature
    m_rho_store_avg = mc_store_htfProps.dens(T_tes_ave, 1.0);
    m_mass_total_active = m_Q_tes_des * 3600.0 / (cp_ave / 1000.0 * (T_tes_hot_des - T_tes_cold_des));  //[kg] Total HTF mass at design point inlet/outlet T
    double V_inactive = m_vol_tank - m_V_tank_active;

    double rho_hot_des = mc_store_htfProps.dens(T_tes_hot_des, 1.0);
    double rho_cold_des = mc_store_htfProps.dens(T_tes_cold_des, 1.0);
    double rho_hot = mc_store_htfProps.dens(m_T_tank_hot_ini, 1.0);
    double rho_cold = mc_store_htfProps.dens(m_T_tank_cold_ini, 1.0);
    double m_hot_ini = m_f_V_hot_ini * 0.01 * m_mass_total_active + V_inactive * rho_hot_des;  // Updating intiial storage charge calculation to avoid variation in total mass with specified initial T
    double m_cold_ini = (1.0 - m_f_V_hot_ini * 0.01) * m_mass_total_active + V_inactive * rho_cold_des;
    double V_hot_ini = m_hot_ini / rho_hot;
    double V_cold_ini = m_cold_ini / rho_cold;

    double T_hot_ini = m_T_tank_hot_ini;		//[K]
    double T_cold_ini = m_T_tank_cold_ini;	//[K]

    // Initialize cold and hot tanks
            // Hot tank
    mc_hot_tank.init(mc_store_htfProps, m_vol_tank, m_h_tank, m_h_tank_min,
        m_u_tank, m_tank_pairs, m_hot_tank_Thtr, m_hot_tank_max_heat,
        V_hot_ini, T_hot_ini, T_tes_hot_des);
    // Cold tank
    mc_cold_tank.init(mc_store_htfProps, m_vol_tank, m_h_tank, m_h_tank_min,
        m_u_tank, m_tank_pairs, m_cold_tank_Thtr, m_cold_tank_max_heat,
        V_cold_ini, T_cold_ini, T_tes_cold_des);


}

bool C_csp_NTHeatTrap_tes::does_tes_exist()
{
	return m_is_tes;
}

bool C_csp_NTHeatTrap_tes::is_cr_to_cold_allowed()
{
	return m_is_cr_to_cold_tank_allowed;
}

double C_csp_NTHeatTrap_tes::get_hot_temp()
{
	return mc_hot_tank.get_m_T_prev();	//[K]
}

double C_csp_NTHeatTrap_tes::get_cold_temp()
{
	return mc_cold_tank.get_m_T_prev();	//[K]
}

double C_csp_NTHeatTrap_tes::get_hot_tank_vol_frac()
{
	return mc_hot_tank.get_vol_frac();
}

double C_csp_NTHeatTrap_tes::get_initial_charge_energy()
{
    //MWh
    if (std::isnan(m_V_tank_hot_ini))
    {
        return m_q_pb_design * m_ts_hours * (m_f_V_hot_ini / 100.0) * 1.e-6;
    }
    else
    {
        //TODO: m_V_tank_hot_ini does not get initialized to user value...
        return m_q_pb_design * m_ts_hours * m_V_tank_hot_ini / m_vol_tank * 1.e-6;
    }
}

double C_csp_NTHeatTrap_tes::get_min_charge_energy()
{
    //MWh
    return 0.; //m_q_pb_design * m_ts_hours * m_h_tank_min / m_h_tank*1.e-6;
}

double C_csp_NTHeatTrap_tes::get_max_charge_energy()
{
    return m_q_pb_design * m_ts_hours / 1.e6;
}

double C_csp_NTHeatTrap_tes::get_degradation_rate()
{
    //calculates an approximate "average" tank heat loss rate based on some assumptions. Good for simple optimization performance projections.
    double d_tank = sqrt(m_vol_tank / ((double)m_tank_pairs * m_h_tank * 3.14159));
    double e_loss = m_u_tank * 3.14159 * m_tank_pairs * d_tank * (m_T_cold_des + m_T_hot_des - 576.3) * 1.e-6;  //MJ/s  -- assumes full area for loss, Tamb = 15C
    return e_loss / (m_q_pb_design * m_ts_hours * 3600.); //s^-1  -- fraction of heat loss per second based on full charge
}

void C_csp_NTHeatTrap_tes::reset_storage_to_initial_state()
{
    // Initial storage charge based on % mass
    double Q_tes_des = m_q_pb_design / 1.E6 * m_ts_hours;		//[MWt-hr] TES thermal capacity at design
    double cp_ave = mc_store_htfProps.Cp_ave(m_T_cold_des, m_T_hot_des);				//[kJ/kg-K] Specific heat at average temperature
    double mtot = Q_tes_des * 3600.0 / (cp_ave / 1000.0 * (m_T_hot_des - m_T_cold_des));  //[kg] Total HTF mass
    double rho_hot = mc_store_htfProps.dens(m_T_hot_des, 1.0);
    double rho_cold = mc_store_htfProps.dens(m_T_cold_des, 1.0);

    double V_inactive = m_vol_tank - m_V_tank_active;
    double V_hot_ini = m_f_V_hot_ini * 0.01 * mtot / rho_hot + V_inactive;			//[m^3]
    double V_cold_ini = (1.0 - m_f_V_hot_ini * 0.01) * mtot / rho_cold + V_inactive;	//[m^3]

    double T_hot_ini = m_T_tank_hot_ini;		//[K]
    double T_cold_ini = m_T_tank_cold_ini;	//[K]

    // Initialize cold and hot tanks
    // Hot tank
    mc_hot_tank.init(mc_store_htfProps, m_vol_tank, m_h_tank, m_h_tank_min,
        m_u_tank, m_tank_pairs, m_hot_tank_Thtr, m_hot_tank_max_heat,
        V_hot_ini, T_hot_ini, m_T_hot_des);
    // Cold tank
    mc_cold_tank.init(mc_store_htfProps, m_vol_tank, m_h_tank, m_h_tank_min,
        m_u_tank, m_tank_pairs, m_cold_tank_Thtr, m_cold_tank_max_heat,
        V_cold_ini, T_cold_ini, m_T_cold_des);
}

void C_csp_NTHeatTrap_tes::discharge_avail_est(double T_cold_K, double step_s,
    double& q_dot_dc_est /*MWt*/, double& m_dot_external_est /*kg/s*/, double& T_hot_external_est /*K*/)
{
    double f_storage = 0.0;		// for now, hardcode such that storage always completely discharges

    double m_dot_tank_disch_avail = mc_hot_tank.m_dot_available(f_storage, step_s);	//[kg/s]

    if (m_dot_tank_disch_avail == 0) {
        q_dot_dc_est = 0.;
        m_dot_external_est = 0.;
        T_hot_external_est = std::numeric_limits<double>::quiet_NaN();
        return;
    }

    double T_hot_ini = mc_hot_tank.get_m_T_prev();		//[K]

    double cp_T_avg = mc_store_htfProps.Cp_ave(T_cold_K, T_hot_ini);		//[kJ/kg-K] spec heat at average temperature during discharge from hot to cold
    q_dot_dc_est = m_dot_tank_disch_avail * cp_T_avg * (T_hot_ini - T_cold_K) * 1.E-3;	//[MW]
    m_dot_external_est = m_dot_tank_disch_avail;
    T_hot_external_est = T_hot_ini;
    
}

void C_csp_NTHeatTrap_tes::charge_avail_est(double T_hot_K, double step_s,
    double& q_dot_ch_est /*MWt*/, double& m_dot_external_est /*kg/s*/, double& T_cold_external_est /*K*/)
{
    double f_ch_storage = 0.0;	// for now, hardcode such that storage always completely charges

    double m_dot_tank_charge_avail = mc_cold_tank.m_dot_available(f_ch_storage, step_s);	//[kg/s]

    double T_cold_ini = mc_cold_tank.get_m_T_prev();	//[K]

    // for debugging
    double T_hot_ini = mc_hot_tank.get_m_T_prev();      //[K]
    double cp_T_tanks_avg = mc_store_htfProps.Cp_ave(T_cold_ini, T_hot_ini);   // [kJ/kg-K]
    double mass_avail_hot_tank = mc_hot_tank.m_dot_available(f_ch_storage, step_s) * step_s;   //[kg]
    double tes_charge_state = mass_avail_hot_tank * cp_T_tanks_avg * (T_hot_ini - T_cold_ini) * 1.e-3 / 3600.;  // [MWht]

    double cp_T_avg = mc_store_htfProps.Cp_ave(T_cold_ini, T_hot_K);	//[kJ/kg-K] spec heat at average temperature during charging from cold to hot
    q_dot_ch_est = m_dot_tank_charge_avail * cp_T_avg * (T_hot_K - T_cold_ini) * 1.E-3;	//[MW]
    m_dot_external_est = m_dot_tank_charge_avail;	//[kg/s]
    T_cold_external_est = T_cold_ini;				//[K]
    
}

int C_csp_NTHeatTrap_tes::solve_tes_off_design(double timestep /*s*/, double  T_amb /*K*/,
    double m_dot_cr_to_cv_hot /*kg/s*/, double m_dot_cv_hot_to_sink /*kg/s*/, double m_dot_cr_to_cv_cold /*kg/s*/,
    double T_cr_out_hot /*K*/, double T_sink_out_cold /*K*/,
    double& T_sink_htf_in_hot /*K*/, double& T_cr_in_cold /*K*/,
    C_csp_tes::S_csp_tes_outputs& s_outputs)		//, C_csp_solver_htf_state & s_tes_ch_htf, C_csp_solver_htf_state & s_tes_dc_htf)
{
    // Enthalpy balance on inlet to cold cv
    double T_htf_cold_cv_in = T_sink_out_cold;     //[K]
    double m_dot_total_to_cv_cold = m_dot_cv_hot_to_sink + m_dot_cr_to_cv_cold;    //[kg/s]
    if (m_dot_total_to_cv_cold > 0.0) {
        T_htf_cold_cv_in = (m_dot_cv_hot_to_sink * T_sink_out_cold + m_dot_cr_to_cv_cold * T_cr_out_hot) / (m_dot_total_to_cv_cold);
    }

    // Total mass flow leaving cold tank to cr
    // One of the RHS should always be 0...
    double m_dot_cv_cold_to_cr = m_dot_cr_to_cv_hot + m_dot_cr_to_cv_cold;

    s_outputs = S_csp_tes_outputs();

    double m_dot_cr_to_tes_hot, m_dot_cr_to_tes_cold, m_dot_tes_hot_out, m_dot_pc_to_tes_cold, m_dot_tes_cold_out, m_dot_tes_cold_in;
    m_dot_cr_to_tes_hot = m_dot_cr_to_tes_cold = m_dot_tes_hot_out = m_dot_pc_to_tes_cold = m_dot_tes_cold_out = m_dot_tes_cold_in = std::numeric_limits<double>::quiet_NaN();
    double m_dot_src_to_sink, m_dot_sink_to_src;
    m_dot_src_to_sink = m_dot_sink_to_src = std::numeric_limits<double>::quiet_NaN();

    if (tanks_in_parallel)
    {
        // Receiver bypass is possible in a parallel configuration,
        //  but need to determine if it actually makes sense and how to model it
        if (m_dot_cr_to_cv_cold != 0.0) {
            throw(C_csp_exception("Receiver output to cold tank not allowed in parallel TES configuration"));
        }
        m_dot_cr_to_tes_cold = 0.0;

        if (m_dot_cr_to_cv_hot >= m_dot_cv_hot_to_sink)
        {
            m_dot_cr_to_tes_hot = m_dot_cr_to_cv_hot - m_dot_cv_hot_to_sink;		//[kg/s]
            m_dot_tes_hot_out = 0.0;							//[kg/s]
            m_dot_pc_to_tes_cold = 0.0;							//[kg/s]
            m_dot_tes_cold_out = m_dot_cr_to_tes_hot;			//[kg/s]
            m_dot_src_to_sink = m_dot_cv_hot_to_sink;		//[kg/s]
            m_dot_sink_to_src = m_dot_cv_hot_to_sink;		//[kg/s]
        }
        else
        {
            m_dot_cr_to_tes_hot = 0.0;							//[kg/s]
            m_dot_tes_hot_out = m_dot_cv_hot_to_sink - m_dot_cr_to_cv_hot;		//[kg/s]
            m_dot_pc_to_tes_cold = m_dot_tes_hot_out;			//[kg/s]
            m_dot_tes_cold_out = 0.0;							//[kg/s]
            m_dot_src_to_sink = m_dot_cr_to_cv_hot;			//[kg/s]
            m_dot_sink_to_src = m_dot_cr_to_cv_hot;			//[kg/s]
        }
        m_dot_tes_cold_in = m_dot_pc_to_tes_cold;
    }
    else
    {   // Serial configuration


        m_dot_cr_to_tes_hot = m_dot_cr_to_cv_hot;		//[kg/s]
        m_dot_cr_to_tes_cold = m_dot_cr_to_cv_cold;     //[kg/s]
        m_dot_tes_hot_out = m_dot_cv_hot_to_sink;		//[kg/s]
        m_dot_pc_to_tes_cold = m_dot_cv_hot_to_sink;	//[kg/s]
        m_dot_tes_cold_out = m_dot_cr_to_cv_hot + m_dot_cr_to_cv_cold;		//[kg/s]
        m_dot_tes_cold_in = m_dot_total_to_cv_cold;     //[kg/s]
        m_dot_src_to_sink = 0.0;				//[kg/s]
        m_dot_sink_to_src = 0.0;				//[kg/s]
    }

    double q_dot_heater = std::numeric_limits<double>::quiet_NaN();			//[MWe]  Heating power required to keep tanks at a minimum temperature
    double m_dot_cold_tank_to_hot_tank = std::numeric_limits<double>::quiet_NaN();	//[kg/s] Hot tank mass flow rate, valid for direct and indirect systems
    double W_dot_rhtf_pump = std::numeric_limits<double>::quiet_NaN();		//[MWe]  Pumping power, just for tank-to-tank in indirect storage
    double q_dot_loss = std::numeric_limits<double>::quiet_NaN();			//[MWt]  Storage thermal losses
    double q_dot_dc_to_htf = std::numeric_limits<double>::quiet_NaN();		//[MWt]  Thermal power to the HTF from storage
    double q_dot_ch_from_htf = std::numeric_limits<double>::quiet_NaN();	//[MWt]  Thermal power from the HTF to storage
    double T_hot_ave = std::numeric_limits<double>::quiet_NaN();		    //[K]    Average hot tank temperature over timestep
    double T_cold_ave = std::numeric_limits<double>::quiet_NaN();			//[K]    Average cold tank temperature over timestep
    double T_hot_final = std::numeric_limits<double>::quiet_NaN();			//[K]    Hot tank temperature at end of timestep
    double T_cold_final = std::numeric_limits<double>::quiet_NaN();			//[K]    Cold tank temperature at end of timestep

    if (tanks_in_parallel)
    {

        if (m_dot_cr_to_cv_hot >= m_dot_cv_hot_to_sink) // Charging
        {
            T_sink_htf_in_hot = T_cr_out_hot;       //[K]
            double m_dot_tes_ch = m_dot_cr_to_cv_hot - m_dot_cv_hot_to_sink;    //[kg/s]
            double T_htf_tes_cold = std::numeric_limits<double>::quiet_NaN();   //[K]
            bool ch_solved = charge(timestep,
                T_amb,
                m_dot_tes_ch,
                T_cr_out_hot,
                T_htf_tes_cold,
                q_dot_heater, m_dot_cold_tank_to_hot_tank, W_dot_rhtf_pump,
                q_dot_loss, q_dot_dc_to_htf, q_dot_ch_from_htf,
                T_hot_ave, T_cold_ave, T_hot_final, T_cold_final);

            // Check if TES.charge method solved
            if (!ch_solved)
            {
                return -3;
            }

            // Enthalpy balance to calculate T_htf_cold to CR
            if (m_dot_cr_to_cv_hot == 0.0)
            {
                T_cr_in_cold = T_htf_tes_cold;       //[K]
            }
            else
            {
                T_cr_in_cold = (m_dot_tes_ch * T_htf_tes_cold + m_dot_cv_hot_to_sink * T_sink_out_cold) / m_dot_cr_to_cv_hot;       //[K]
            }
        }
        else    // Discharging
        {
            T_cr_in_cold = T_sink_out_cold;     //[K]
            double m_dot_tes_dc = m_dot_cv_hot_to_sink - m_dot_cr_to_cv_hot;    //[kg/s]
            double T_htf_tes_hot = std::numeric_limits<double>::quiet_NaN();
            bool is_tes_success = discharge(timestep,
                T_amb,
                m_dot_tes_dc,
                T_sink_out_cold,
                T_htf_tes_hot,
                q_dot_heater, m_dot_cold_tank_to_hot_tank, W_dot_rhtf_pump,
                q_dot_loss, q_dot_dc_to_htf, q_dot_ch_from_htf,
                T_hot_ave, T_cold_ave, T_hot_final, T_cold_final);

            m_dot_cold_tank_to_hot_tank *= -1.0;

            // Check if discharge method solved
            if (!is_tes_success)
            {
                return -4;
            }

            T_sink_htf_in_hot = (m_dot_tes_dc * T_htf_tes_hot + m_dot_cr_to_cv_hot * T_cr_out_hot) / m_dot_cv_hot_to_sink;   //[K]
        }
    }
    else // Serial tank operation
    {


        // Inputs are:
        // 1) Mass flow rate of HTF from source to hot tank
        // 2) Mass flow rate of HTF from source to cold tank
        // 3) Mass flow rate of HTF from hot tank to sink
        // 4) Temperature of HTF leaving source and entering hot tank
        // 5) Temperature of HTF leaving the sink and entering the cold tank

        double q_dot_ch_est, m_dot_tes_ch_max, T_cold_to_src_est;
        q_dot_ch_est = m_dot_tes_ch_max = T_cold_to_src_est = std::numeric_limits<double>::quiet_NaN();
        charge_avail_est(T_cr_out_hot, timestep, q_dot_ch_est, m_dot_tes_ch_max, T_cold_to_src_est);

        if (m_dot_cr_to_cv_hot > m_dot_cv_hot_to_sink && std::max(1.E-4, (m_dot_cr_to_cv_hot - m_dot_cv_hot_to_sink)) > 1.0001 * std::max(1.E-4, m_dot_tes_ch_max))
        {
            q_dot_heater = std::numeric_limits<double>::quiet_NaN();
            m_dot_cold_tank_to_hot_tank = std::numeric_limits<double>::quiet_NaN();
            W_dot_rhtf_pump = std::numeric_limits<double>::quiet_NaN();
            q_dot_loss = std::numeric_limits<double>::quiet_NaN();
            q_dot_dc_to_htf = std::numeric_limits<double>::quiet_NaN();
            q_dot_ch_from_htf = std::numeric_limits<double>::quiet_NaN();
            T_hot_ave = std::numeric_limits<double>::quiet_NaN();
            T_cold_ave = std::numeric_limits<double>::quiet_NaN();
            T_hot_final = std::numeric_limits<double>::quiet_NaN();
            T_cold_final = std::numeric_limits<double>::quiet_NaN();

            return -1;
        }

        double q_dot_dc_est, m_dot_tes_dc_max, T_hot_to_pc_est;
        q_dot_dc_est = m_dot_tes_dc_max = T_hot_to_pc_est = std::numeric_limits<double>::quiet_NaN();
        // Use temperature downstream of sink-out and cr-to-cold-tank mixer
        discharge_avail_est(T_htf_cold_cv_in, timestep, q_dot_dc_est, m_dot_tes_dc_max, T_hot_to_pc_est);

        // If mass flow into the cold tank *from the sink* is greater than mass flow going from cold tank to source to hot tank
        if (m_dot_cv_hot_to_sink > m_dot_cr_to_cv_hot && std::max(1.E-4, (m_dot_cv_hot_to_sink - m_dot_cr_to_cv_hot)) > 1.0001 * std::max(1.E-4, m_dot_tes_dc_max))
        {
            q_dot_heater = std::numeric_limits<double>::quiet_NaN();
            m_dot_cold_tank_to_hot_tank = std::numeric_limits<double>::quiet_NaN();
            W_dot_rhtf_pump = std::numeric_limits<double>::quiet_NaN();
            q_dot_loss = std::numeric_limits<double>::quiet_NaN();
            q_dot_dc_to_htf = std::numeric_limits<double>::quiet_NaN();
            q_dot_ch_from_htf = std::numeric_limits<double>::quiet_NaN();
            T_hot_ave = std::numeric_limits<double>::quiet_NaN();
            T_cold_ave = std::numeric_limits<double>::quiet_NaN();
            T_hot_final = std::numeric_limits<double>::quiet_NaN();
            T_cold_final = std::numeric_limits<double>::quiet_NaN();

            return -2;
        }

        // serial operation constrained to direct configuration, so HTF leaving TES must pass through another plant component
        m_dot_cold_tank_to_hot_tank = 0.0;	//[kg/s]

        double q_heater_hot, q_dot_loss_hot, q_heater_cold, q_dot_loss_cold;
        q_heater_hot = q_dot_loss_hot = q_heater_cold = q_dot_loss_cold = std::numeric_limits<double>::quiet_NaN();

        // Call energy balance on hot tank discharge to get average outlet temperature over timestep
        mc_hot_tank.energy_balance(timestep, m_dot_cr_to_cv_hot, m_dot_cv_hot_to_sink,
            T_cr_out_hot, T_amb,
            T_sink_htf_in_hot, q_heater_hot, q_dot_loss_hot);

        // Call energy balance on cold tank charge to track tank mass and temperature
        // Use mass flow and temperature downstream of sink-out and cr-to-cold-tank mixer
        mc_cold_tank.energy_balance(timestep, m_dot_total_to_cv_cold, m_dot_cv_cold_to_cr,
            T_htf_cold_cv_in, T_amb,
            T_cr_in_cold, q_heater_cold, q_dot_loss_cold);

        // Set output structure
        q_dot_heater = q_heater_cold + q_heater_hot;			//[MWt]

        W_dot_rhtf_pump = 0;                              //[MWe] Tank-to-tank pumping power

        q_dot_loss = q_dot_loss_cold + q_dot_loss_hot;	//[MWt]
        q_dot_ch_from_htf = 0.0;		                    //[MWt]
        T_hot_ave = T_sink_htf_in_hot;						//[K]
        T_cold_ave = T_cr_in_cold;						//[K]
        T_hot_final = mc_hot_tank.get_m_T_calc();			//[K]
        T_cold_final = mc_cold_tank.get_m_T_calc();		//[K]

        // Net TES discharge
        double cp_field = mc_external_htfProps.Cp_ave(T_cold_ave, T_cr_out_hot);
        double cp_cycle = mc_store_htfProps.Cp_ave(T_htf_cold_cv_in, T_hot_ave);
        double q_dot_tes_net_discharge = (cp_field * (m_dot_tes_cold_out * T_cold_ave - m_dot_cr_to_tes_hot * T_cr_out_hot)
            + cp_cycle * (m_dot_tes_hot_out * T_hot_ave - m_dot_total_to_cv_cold * T_htf_cold_cv_in)) / 1000.0;		//[MWt]

        if (m_dot_cv_hot_to_sink >= m_dot_cr_to_cv_hot)
        {
            q_dot_ch_from_htf = 0.0;

            q_dot_dc_to_htf = q_dot_tes_net_discharge;	//[MWt]
        }
        else
        {
            q_dot_dc_to_htf = 0.0;

            q_dot_ch_from_htf = -q_dot_tes_net_discharge;	//[MWt]
        }

    }

    // Solve pumping power here
    double W_dot_htf_pump = pumping_power(m_dot_cr_to_cv_hot, m_dot_cv_hot_to_sink, std::abs(m_dot_cold_tank_to_hot_tank),
        T_cr_in_cold, T_cr_out_hot, T_sink_htf_in_hot, T_sink_out_cold,
        false);     //[-] C_MEQ__m_dot_tes will not send cr_m_dot to TES if recirculating

    s_outputs.m_q_heater = q_dot_heater;
    s_outputs.m_W_dot_elec_in_tot = W_dot_htf_pump;             //[MWe]

    s_outputs.m_q_dot_dc_to_htf = q_dot_dc_to_htf;
    s_outputs.m_q_dot_ch_from_htf = q_dot_ch_from_htf;
    s_outputs.m_m_dot_cr_to_tes_hot = m_dot_cr_to_tes_hot;		//[kg/s]
    s_outputs.m_m_dot_cr_to_tes_cold = m_dot_cr_to_tes_cold;    //[kg/s]
    s_outputs.m_m_dot_tes_hot_out = m_dot_tes_hot_out;			//[kg/s]
    s_outputs.m_m_dot_pc_to_tes_cold = m_dot_pc_to_tes_cold;	//[kg/s]
    s_outputs.m_m_dot_tes_cold_out = m_dot_tes_cold_out;		//[kg/s]
    s_outputs.m_m_dot_tes_cold_in = m_dot_tes_cold_in;          //[kg/s]
    s_outputs.m_m_dot_src_to_sink = m_dot_src_to_sink;	        //[kg/s]
    s_outputs.m_m_dot_sink_to_src = m_dot_sink_to_src;	        //[kg/s]

    s_outputs.m_T_tes_cold_in = T_htf_cold_cv_in;       //[K]

    s_outputs.m_m_dot_cold_tank_to_hot_tank = m_dot_cold_tank_to_hot_tank;

    mc_reported_outputs.value(E_Q_DOT_LOSS, q_dot_loss);		//[MWt]
    mc_reported_outputs.value(E_W_DOT_HEATER, q_dot_heater);		//[MWt]
    mc_reported_outputs.value(E_TES_T_HOT, T_hot_final - 273.15);	//[C]
    mc_reported_outputs.value(E_TES_T_COLD, T_cold_final - 273.15);	//[C]
    mc_reported_outputs.value(E_M_DOT_TANK_TO_TANK, m_dot_cold_tank_to_hot_tank);	//[kg/s]
    mc_reported_outputs.value(E_MASS_COLD_TANK, mc_cold_tank.get_m_m_calc());		//[kg]
    mc_reported_outputs.value(E_MASS_HOT_TANK, mc_hot_tank.get_m_m_calc());			//[kg]
    mc_reported_outputs.value(E_W_DOT_HTF_PUMP, W_dot_htf_pump);    //[MWe]

    return 0;
}

void C_csp_NTHeatTrap_tes::converged()
{
}

void C_csp_NTHeatTrap_tes::write_output_intervals(double report_time_start,
    const std::vector<double>& v_temp_ts_time_end, double report_time_end)
{
}

void C_csp_NTHeatTrap_tes::assign(int index, double* p_reporting_ts_array, size_t n_reporting_ts_array)
{
}

double /*MWe*/ C_csp_NTHeatTrap_tes::pumping_power(double m_dot_sf /*kg/s*/, double m_dot_pb /*kg/s*/, double m_dot_tank /*kg/s*/,
    double T_sf_in /*K*/, double T_sf_out /*K*/, double T_pb_in /*K*/, double T_pb_out /*K*/, bool recirculating)
{
    return 0;
}


void C_csp_NTHeatTrap_tes::get_design_parameters(double& vol_one_temp_avail /*m3*/, double& vol_one_temp_total /*m3*/, double& d_tank /*m*/,
    double& q_dot_loss_des /*MWt*/, double& dens_store_htf_at_T_ave /*kg/m3*/, double& Q_tes /*MWt-hr*/)
{
    vol_one_temp_avail = m_V_tank_active;   //[m3]
    vol_one_temp_total = m_vol_tank;        //[m3]
    d_tank = m_d_tank;                      //[m]
    q_dot_loss_des = m_q_dot_loss_des;      //[MWt]
    dens_store_htf_at_T_ave = m_rho_store_avg;  //[kg/m3]
    Q_tes = m_Q_tes_des;                    //[MWt-hr]
}

bool C_csp_NTHeatTrap_tes::charge(double timestep /*s*/, double T_amb /*K*/, double m_dot_htf_in /*kg/s*/,
    double T_htf_hot_in /*K*/, double& T_htf_cold_out /*K*/,
    double& q_dot_heater /*MWe*/, double& m_dot_tank_to_tank /*kg/s*/, double& W_dot_rhtf_pump /*MWe*/,
    double& q_dot_loss /*MWt*/, double& q_dot_dc_to_htf /*MWt*/, double& q_dot_ch_from_htf /*MWt*/,
    double& T_hot_ave /*K*/, double& T_cold_ave /*K*/, double& T_hot_final /*K*/, double& T_cold_final /*K*/)
{
    // This method calculates the timestep-average cold charge return temperature of the TES system.
    // This is out of the external side of the heat exchanger (HX), opposite the tank (or 'TES') side,
    // or if no HX (direct storage), this is equal to the cold tank outlet temperature.

    // The method returns FALSE if the system input mass flow rate is greater than the allowable charge 

    // Inputs are:
    // 1) Mass flow rate of HTF into TES system (equal to that exiting the system)
    // 2) Temperature of HTF into TES system. If no heat exchanger, this temperature
    //	   is of the HTF directly entering the hot tank

    double q_dot_ch_est, m_dot_tes_ch_max, T_cold_to_src_est;
    q_dot_ch_est = m_dot_tes_ch_max = T_cold_to_src_est = std::numeric_limits<double>::quiet_NaN();
    charge_avail_est(T_htf_hot_in, timestep, q_dot_ch_est, m_dot_tes_ch_max, T_cold_to_src_est);

    if (m_dot_htf_in > 1.0001 * m_dot_tes_ch_max && m_dot_htf_in > 1.E-6)
    {
        q_dot_heater = std::numeric_limits<double>::quiet_NaN();
        m_dot_tank_to_tank = std::numeric_limits<double>::quiet_NaN();
        W_dot_rhtf_pump = std::numeric_limits<double>::quiet_NaN();
        q_dot_loss = std::numeric_limits<double>::quiet_NaN();
        q_dot_dc_to_htf = std::numeric_limits<double>::quiet_NaN();
        q_dot_ch_from_htf = std::numeric_limits<double>::quiet_NaN();
        T_hot_ave = std::numeric_limits<double>::quiet_NaN();
        T_cold_ave = std::numeric_limits<double>::quiet_NaN();
        T_hot_final = std::numeric_limits<double>::quiet_NaN();
        T_cold_final = std::numeric_limits<double>::quiet_NaN();

        return false;
    }

    double q_heater_cold, q_heater_hot, q_dot_loss_cold, q_dot_loss_hot, m_dot_src,
        T_src_hot_in, T_src_cold_out, m_dot_tank, T_hot_tank_in;
    q_heater_cold = q_heater_hot = q_dot_loss_cold = q_dot_loss_hot = T_cold_ave = T_hot_ave = m_dot_src =
        T_src_hot_in = T_src_cold_out = m_dot_tank = T_hot_tank_in = std::numeric_limits<double>::quiet_NaN();

    m_dot_src = m_dot_tank = m_dot_htf_in;
    T_src_hot_in = T_hot_tank_in = T_htf_hot_in;

    // Call energy balance on cold tank discharge to get average outlet temperature over timestep
    mc_cold_tank.energy_balance(timestep, 0.0, m_dot_tank, 0.0, T_amb, T_cold_ave, q_heater_cold, q_dot_loss_cold);

    // Call energy balance on hot tank charge to track tank mass and temperature
    mc_hot_tank.energy_balance(timestep, m_dot_tank, 0.0, T_hot_tank_in, T_amb, T_hot_ave, q_heater_hot, q_dot_loss_hot);

    q_dot_heater = q_heater_cold + q_heater_hot;			//[MWt]


    m_dot_tank_to_tank = 0.0;
    W_dot_rhtf_pump = 0;                          //[MWe] Just tank-to-tank pumping power
    T_htf_cold_out = T_cold_ave;

    q_dot_loss = q_dot_loss_cold + q_dot_loss_hot;	//[MWt]
    q_dot_dc_to_htf = 0.0;							//[MWt]
    T_hot_ave = T_hot_ave;							//[K]
    T_cold_ave = T_cold_ave;  						//[K]
    T_hot_final = mc_hot_tank.get_m_T_calc();			//[K]
    T_cold_final = mc_cold_tank.get_m_T_calc();		//[K]

    // Calculate thermal power to HTF
    double cp_htf_ave = mc_external_htfProps.Cp_ave(T_htf_cold_out, T_htf_hot_in);		//[kJ/kg-K]
    q_dot_ch_from_htf = m_dot_htf_in * cp_htf_ave * (T_htf_hot_in - T_htf_cold_out) / 1000.0;		//[MWt]

    return true;

}


bool C_csp_NTHeatTrap_tes::discharge(double timestep /*s*/, double T_amb /*K*/, double m_dot_htf_in /*kg/s*/,
    double T_htf_cold_in /*K*/, double& T_htf_hot_out /*K*/,
    double& q_dot_heater /*MWe*/, double& m_dot_tank_to_tank /*kg/s*/, double& W_dot_rhtf_pump /*MWe*/,
    double& q_dot_loss /*MWt*/, double& q_dot_dc_to_htf /*MWt*/, double& q_dot_ch_from_htf /*MWt*/,
    double& T_hot_ave /*K*/, double& T_cold_ave /*K*/, double& T_hot_final /*K*/, double& T_cold_final /*K*/)
{
    // This method calculates the timestep-average hot discharge temperature of the TES system. This is out of the external side of the heat exchanger (HX), opposite the tank (or 'TES') side,
    // or if no HX (direct storage), this is equal to the hot tank outlet temperature.

    // The method returns FALSE if the system output (same as input) mass flow rate is greater than that available

    // Inputs are:
    // 1) Mass flow rate of HTF into TES system (equal to that exiting the system)
    // 2) Temperature of HTF into TES system. If no heat exchanger, this temperature
    //	   is of the HTF directly entering the cold tank

    double q_dot_dc_est, m_dot_tes_dc_max, T_hot_to_pc_est;
    q_dot_dc_est = m_dot_tes_dc_max = T_hot_to_pc_est = std::numeric_limits<double>::quiet_NaN();
    discharge_avail_est(T_htf_cold_in, timestep, q_dot_dc_est, m_dot_tes_dc_max, T_hot_to_pc_est);

    if (m_dot_htf_in > 1.0001 * m_dot_tes_dc_max && m_dot_htf_in > 1.E-6)      // mass flow in = mass flow out
    {
        q_dot_heater = std::numeric_limits<double>::quiet_NaN();
        m_dot_tank_to_tank = std::numeric_limits<double>::quiet_NaN();
        W_dot_rhtf_pump = std::numeric_limits<double>::quiet_NaN();
        q_dot_loss = std::numeric_limits<double>::quiet_NaN();
        q_dot_dc_to_htf = std::numeric_limits<double>::quiet_NaN();
        q_dot_ch_from_htf = std::numeric_limits<double>::quiet_NaN();
        T_hot_ave = std::numeric_limits<double>::quiet_NaN();
        T_cold_ave = std::numeric_limits<double>::quiet_NaN();
        T_hot_final = std::numeric_limits<double>::quiet_NaN();
        T_cold_final = std::numeric_limits<double>::quiet_NaN();

        return false;
    }

    double q_heater_cold, q_heater_hot, q_dot_loss_cold, q_dot_loss_hot, m_dot_src,
        T_src_cold_in, T_src_hot_out, m_dot_tank, T_cold_tank_in;
    q_heater_cold = q_heater_hot = q_dot_loss_cold = q_dot_loss_hot = T_cold_ave = T_hot_ave =
        m_dot_src = T_src_cold_in = T_src_hot_out = m_dot_tank = T_cold_tank_in = std::numeric_limits<double>::quiet_NaN();

    m_dot_src = m_dot_tank = m_dot_htf_in;
    T_src_cold_in = T_cold_tank_in = T_htf_cold_in;

    // Call energy balance on hot tank discharge to get average outlet temperature over timestep
    mc_hot_tank.energy_balance(timestep, 0.0, m_dot_tank, 0.0, T_amb, T_hot_ave, q_heater_hot, q_dot_loss_hot);

    // Call energy balance on cold tank charge to track tank mass and temperature
    mc_cold_tank.energy_balance(timestep, m_dot_tank, 0.0, T_cold_tank_in, T_amb, T_cold_ave, q_heater_cold, q_dot_loss_cold);

    q_dot_heater = q_heater_cold + q_heater_hot;			//[MWt]

    m_dot_tank_to_tank = 0.0;
    W_dot_rhtf_pump = 0;                          //[MWe] Just tank-to-tank pumping power
    T_htf_hot_out = T_hot_ave;

    q_dot_loss = q_dot_loss_cold + q_dot_loss_hot;	//[MWt]
    q_dot_ch_from_htf = 0.0;		                    //[MWt]
    T_hot_ave = T_hot_ave;						    //[K]
    T_cold_ave = T_cold_ave;							//[K]
    T_hot_final = mc_hot_tank.get_m_T_calc();			//[K]
    T_cold_final = mc_cold_tank.get_m_T_calc();		//[K]

    // Calculate thermal power to HTF
    double cp_htf_ave = mc_external_htfProps.Cp_ave(T_htf_cold_in, T_htf_hot_out);		//[kJ/kg-K]
    q_dot_dc_to_htf = m_dot_htf_in * cp_htf_ave * (T_htf_hot_out - T_htf_cold_in) / 1000.0;		//[MWt]

    return true;
}
