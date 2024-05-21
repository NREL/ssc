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


#include "csp_solver_particlecline_tes.h"

static C_csp_reported_outputs::S_output_info S_output_info[] =
{
    {C_csp_particlecline_tes::E_Q_DOT_LOSS, C_csp_reported_outputs::TS_WEIGHTED_AVE},		//[MWt] TES thermal losses
    {C_csp_particlecline_tes::E_W_DOT_HEATER, C_csp_reported_outputs::TS_WEIGHTED_AVE},		//[MWe] TES freeze protection power
    {C_csp_particlecline_tes::E_TES_T_HOT, C_csp_reported_outputs::TS_LAST},					//[C] TES final hot tank temperature
    {C_csp_particlecline_tes::E_TES_T_COLD, C_csp_reported_outputs::TS_LAST},				//[C] TES cold temperature at end of timestep      
    {C_csp_particlecline_tes::E_M_DOT_TANK_TO_TANK, C_csp_reported_outputs::TS_WEIGHTED_AVE},		//[MWt] TES thermal losses
    {C_csp_particlecline_tes::E_MASS_COLD_TANK, C_csp_reported_outputs::TS_LAST},			//[kg] Mass in cold tank at end of timestep		
    {C_csp_particlecline_tes::E_MASS_HOT_TANK, C_csp_reported_outputs::TS_LAST},				//[kg] Mass in hot tank at end of timestep
    {C_csp_particlecline_tes::E_HOT_TANK_HTF_PERC_FINAL, C_csp_reported_outputs::TS_LAST},	//[%] Final percent fill of available hot tank mass
    {C_csp_particlecline_tes::E_W_DOT_HTF_PUMP, C_csp_reported_outputs::TS_WEIGHTED_AVE},	//[MWe]

    csp_info_invalid
};

C_csp_particlecline_tes::C_csp_particlecline_tes(
    int external_fl,                             // [-] external fluid identifier
    util::matrix_t<double> external_fl_props,    // [-] external fluid properties
    int tes_fl,                                  // [-] tes fluid identifier
    util::matrix_t<double> tes_fl_props,         // [-] tes fluid properties
    double q_dot_design,                         // [MWt] Design heat rate in and out of tes
    double frac_max_q_dot,                       // [-] the max design heat rate as a fraction of the nominal
    double Q_tes_des,			                 // [MWt-hr] design storage capacity
    bool is_h_fixed,                             // [] [true] Height is input, calculate diameter, [false] diameter input, calculate height
    double h_tank_in,			                 // [m] tank height input
    double d_tank_in,                            // [m] tank diameter input
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
    double V_tes_des                             // [m/s] Design-point velocity for sizing the diameters of the TES piping
    )
    :
    m_external_fl(external_fl), m_external_fl_props(external_fl_props), m_tes_fl(tes_fl), m_tes_fl_props(tes_fl_props),
    m_q_dot_design(q_dot_design), m_frac_max_q_dot(frac_max_q_dot), m_Q_tes_des(Q_tes_des),
    m_is_h_fixed(is_h_fixed), m_h_tank_in(h_tank_in), m_d_tank_in(d_tank_in),
    m_u_tank(u_tank), m_tank_pairs(tank_pairs), m_hot_tank_Thtr(hot_tank_Thtr), m_hot_tank_max_heat(hot_tank_max_heat),
    m_cold_tank_Thtr(cold_tank_Thtr), m_cold_tank_max_heat(cold_tank_max_heat), m_dt_hot(dt_hot), m_T_cold_des(T_cold_des),
    m_T_hot_des(T_hot_des), m_T_tank_hot_ini(T_tank_hot_ini), m_T_tank_cold_ini(T_tank_cold_ini),
    m_h_tank_min(h_tank_min), m_f_V_hot_ini(f_V_hot_ini), m_htf_pump_coef(htf_pump_coef), tanks_in_parallel(tanks_in_parallel),
    V_tes_des(V_tes_des)
{

    if (tes_lengths.ncells() < 11) {
        double lengths[11] = { 0., 90., 100., 120., 0., 30., 90., 80., 80., 120., 80. };
        this->tes_lengths.assign(lengths, 11);
    }

    m_vol_tank = m_V_tank_active = m_q_pb_design = m_ts_hours =
        m_V_tank_hot_ini = m_mass_total_active = m_h_tank_calc = m_d_tank_calc = m_q_dot_loss_des =
        m_cp_external_avg = m_rho_store_avg = m_m_dot_tes_des_over_m_dot_external_des = std::numeric_limits<double>::quiet_NaN();

    mc_reported_outputs.construct(S_output_info);
}


C_csp_particlecline_tes::C_csp_particlecline_tes()
{
    m_vol_tank = m_V_tank_active = m_q_pb_design = m_Q_tes_des =
        m_V_tank_hot_ini = m_mass_total_active = m_h_tank_calc = m_d_tank_calc = m_q_dot_loss_des =
        m_cp_external_avg = m_rho_store_avg = m_m_dot_tes_des_over_m_dot_external_des = std::numeric_limits<double>::quiet_NaN();

    m_q_dot_design = m_frac_max_q_dot = m_h_tank_in = m_d_tank_in = m_u_tank = m_cold_tank_Thtr = m_hot_tank_Thtr
        = m_hot_tank_max_heat = m_cold_tank_max_heat = m_cold_tank_max_heat = m_dt_hot = m_T_cold_des = m_T_hot_des
        = m_dP_src_des = m_T_tank_hot_ini = m_T_tank_cold_ini = m_h_tank_min = m_f_V_hot_ini = m_htf_pump_coef
        = m_tes_pump_coef = eta_pump = V_tes_des = P_in_des
        = pipe_vol_tot
        = std::numeric_limits<double>::quiet_NaN();


    m_ts_hours = std::numeric_limits<double>::quiet_NaN();

    m_external_fl_props = m_tes_fl_props = util::matrix_t<double>();

    m_is_h_fixed = m_is_cr_to_cold_tank_allowed = m_is_hx = m_is_tes = tanks_in_parallel = false;
    m_tank_pairs = 0;

    mc_reported_outputs.construct(S_output_info);
}


void C_csp_particlecline_tes::init(const C_csp_tes::S_csp_tes_init_inputs init_inputs)
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

    bool is_hx_calc = true;

    if (m_tes_fl != m_external_fl)
        is_hx_calc = true;
    else if (m_external_fl != HTFProperties::User_defined)
        is_hx_calc = false;
    else
    {
        is_hx_calc = !mc_external_htfProps.equals(&mc_store_htfProps);
    }

    m_is_hx = is_hx_calc;

    if (m_is_hx && !tanks_in_parallel)
    {
        mc_csp_messages.add_message(C_csp_messages::NOTICE, "The inputs specified serial TES operation, but the external and storage fluids are different."
            " The simulation modeled parallel TES operation.\n");
        tanks_in_parallel = true;
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

    double T_tes_hot_des, T_tes_cold_des;
    if (m_is_hx) {
        T_tes_hot_des = m_T_hot_des - m_dt_hot;
        T_tes_cold_des = m_T_cold_des + m_dt_hot;
    }
    else {
        T_tes_hot_des = m_T_hot_des;
        T_tes_cold_des = m_T_cold_des;
    }

    // SIZE
    if (m_is_h_fixed)
    {

    }
    else
    {

    }

    // 5.13.15, twn: also be sure that hx is sized such that it can supply full load to sink
    double duty = m_q_pb_design * std::max(1.0, m_frac_max_q_dot);		//[W] Allow all energy from the source to go into storage at any time

    // Initial storage charge based on % mass 
    double T_tes_ave = 0.5 * (T_tes_hot_des + T_tes_cold_des);
    double cp_ave = mc_store_htfProps.Cp_ave(T_tes_cold_des, T_tes_hot_des);				//[kJ/kg-K] Specific heat at average temperature
    m_rho_store_avg = mc_store_htfProps.dens(T_tes_ave, 1.0);
    m_mass_total_active = m_Q_tes_des * 3600.0 / (cp_ave / 1000.0 * (T_tes_hot_des - T_tes_cold_des));  //[kg] Total HTF mass at design point inlet/outlet T

}

bool C_csp_particlecline_tes::does_tes_exist()
{
	return m_is_tes;
}

bool C_csp_particlecline_tes::is_cr_to_cold_allowed()
{
	return m_is_cr_to_cold_tank_allowed;
}

double C_csp_particlecline_tes::get_hot_temp()
{
    return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_particlecline_tes::get_cold_temp()
{
	return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_particlecline_tes::get_hot_tank_vol_frac()
{
	return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_particlecline_tes::get_initial_charge_energy()
{
    return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_particlecline_tes::get_min_charge_energy()
{
    return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_particlecline_tes::get_max_charge_energy()
{
    return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_particlecline_tes::get_degradation_rate()
{
    return std::numeric_limits<double>::quiet_NaN();
}

void C_csp_particlecline_tes::reset_storage_to_initial_state()
{
    return;
}

void C_csp_particlecline_tes::discharge_avail_est(double T_cold_K, double step_s,
    double& q_dot_dc_est /*MWt*/, double& m_dot_external_est /*kg/s*/, double& T_hot_external_est /*K*/)
{
    return;
}

void C_csp_particlecline_tes::charge_avail_est(double T_hot_K, double step_s,
    double& q_dot_ch_est /*MWt*/, double& m_dot_external_est /*kg/s*/, double& T_cold_external_est /*K*/)
{
    return;
}

int C_csp_particlecline_tes::solve_tes_off_design(double timestep /*s*/, double  T_amb /*K*/,
    double m_dot_cr_to_cv_hot /*kg/s*/, double m_dot_cv_hot_to_sink /*kg/s*/, double m_dot_cr_to_cv_cold /*kg/s*/,
    double T_cr_out_hot /*K*/, double T_sink_out_cold /*K*/,
    double& T_sink_htf_in_hot /*K*/, double& T_cr_in_cold /*K*/,
    C_csp_tes::S_csp_tes_outputs& s_outputs)		//, C_csp_solver_htf_state & s_tes_ch_htf, C_csp_solver_htf_state & s_tes_dc_htf)
{
    return std::numeric_limits<double>::quiet_NaN();
}

void C_csp_particlecline_tes::converged()
{
    return;
}

void C_csp_particlecline_tes::write_output_intervals(double report_time_start,
    const std::vector<double>& v_temp_ts_time_end, double report_time_end)
{
    mc_reported_outputs.send_to_reporting_ts_array(report_time_start,
        v_temp_ts_time_end, report_time_end);
}

void C_csp_particlecline_tes::assign(int index, double* p_reporting_ts_array, size_t n_reporting_ts_array)
{
    mc_reported_outputs.assign(index, p_reporting_ts_array, n_reporting_ts_array);
}

double /*MWe*/ C_csp_particlecline_tes::pumping_power(double m_dot_sf /*kg/s*/, double m_dot_pb /*kg/s*/, double m_dot_tank /*kg/s*/,
    double T_sf_in /*K*/, double T_sf_out /*K*/, double T_pb_in /*K*/, double T_pb_out /*K*/, bool recirculating)
{
    return std::numeric_limits<double>::quiet_NaN();
}


void C_csp_particlecline_tes::get_design_parameters(double& vol_one_temp_avail /*m3*/, double& vol_one_temp_total /*m3*/,
    double& h_tank_calc /*m*/, double& d_tank_calc /*m*/,
    double& q_dot_loss_des /*MWt*/, double& dens_store_htf_at_T_ave /*kg/m3*/, double& Q_tes /*MWt-hr*/)
{
    return;
}

bool C_csp_particlecline_tes::charge(double timestep /*s*/, double T_amb /*K*/, double m_dot_htf_in /*kg/s*/,
    double T_htf_hot_in /*K*/, double& T_htf_cold_out /*K*/,
    double& q_dot_heater /*MWe*/, double& m_dot_tank_to_tank /*kg/s*/, double& W_dot_rhtf_pump /*MWe*/,
    double& q_dot_loss /*MWt*/, double& q_dot_dc_to_htf /*MWt*/, double& q_dot_ch_from_htf /*MWt*/,
    double& T_hot_ave /*K*/, double& T_cold_ave /*K*/, double& T_hot_final /*K*/, double& T_cold_final /*K*/,
    double& q_dot_out_cold /*MW*/, double& q_dot_out_hot /*MW*/, double& q_dot_error_cold, double& q_dot_error_hot,
    double& q_dot_error_total /*MW*/, double& q_dot_error_leak /*MW*/, double& q_dot_error_wall /*MW*/,
    double& q_dot_error_corrected /*MW*/)
{
    return std::numeric_limits<bool>::quiet_NaN();
}

bool C_csp_particlecline_tes::discharge(double timestep /*s*/, double T_amb /*K*/, double m_dot_htf_in /*kg/s*/,
    double T_htf_cold_in /*K*/, double& T_htf_hot_out /*K*/,
    double& q_dot_heater /*MWe*/, double& m_dot_tank_to_tank /*kg/s*/, double& W_dot_rhtf_pump /*MWe*/,
    double& q_dot_loss /*MWt*/, double& q_dot_dc_to_htf /*MWt*/, double& q_dot_ch_from_htf /*MWt*/,
    double& T_hot_ave /*K*/, double& T_cold_ave /*K*/, double& T_hot_final /*K*/, double& T_cold_final /*K*/,
    double& q_dot_out_cold /*MW*/, double& q_dot_out_hot /*MW*/, double& q_dot_error_cold, double& q_dot_error_hot,
    double& q_dot_error_total /*MW*/, double& q_dot_error_leak /*MW*/, double& q_dot_error_wall /*MW*/,
    double& q_dot_error_corrected /*MW*/)
{
    return std::numeric_limits<bool>::quiet_NaN();
}

int C_csp_particlecline_tes::pressure_drops(double m_dot_sf, double m_dot_pb,
    double T_sf_in, double T_sf_out, double T_pb_in, double T_pb_out, bool recirculating,
    double& P_drop_col, double& P_drop_gen)
{
    return std::numeric_limits<bool>::quiet_NaN();
}

double C_csp_particlecline_tes::get_min_storage_htf_temp()
{
    return std::numeric_limits<bool>::quiet_NaN();
}

double C_csp_particlecline_tes::get_max_storage_htf_temp()
{
    return std::numeric_limits<bool>::quiet_NaN();
}

double C_csp_particlecline_tes::get_storage_htf_density()
{
    return std::numeric_limits<bool>::quiet_NaN();
}

double C_csp_particlecline_tes::get_storage_htf_cp()
{
    return std::numeric_limits<bool>::quiet_NaN();
}
