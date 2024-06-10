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
    double T_tank_hot_ini_C,	                     // [C] Initial temperature in hot storage tank
    double T_tank_cold_ini_C,	                     // [C] Initial temperature in cold storage cold
    double f_V_hot_ini,                          // [%] Initial fraction of available volume that is hot
    int n_xstep,                                 // number spatial sub steps
    int n_subtimestep                            // number subtimesteps
)
    :
    m_f_V_hot_ini(f_V_hot_ini),
    m_n_xstep(n_xstep), m_n_subtimestep(n_subtimestep)
{
    // Convert Temperature Units
    m_T_tank_hot_ini = T_tank_hot_ini_C + 273.15;
    m_T_tank_cold_ini = T_tank_cold_ini_C + 273.15;

    // Temporary Sizing Info
    m_height = 8.4;     // [m]
    m_diameter = 8.4;   // [m]

    // Temporary Hard Code
    m_k_eff = 1.0;         // [W/m K] effective conductivity of magnetite
    m_void_frac = 0.4;
    m_dens_solid = 5175;   // [kg/m3] density of magnetite
    m_cp_solid = 874.2;    // [J/kg K] specific heat of magnetite

    mc_reported_outputs.construct(S_output_info);
}

C_csp_particlecline_tes::C_csp_particlecline_tes()
{
    mc_reported_outputs.construct(S_output_info);
}

void C_csp_particlecline_tes::set_T_grad_init(std::vector<double> T_grad_init)
{
    m_T_prev_vec = T_grad_init;
    m_use_T_grad_init = true;
}

void C_csp_particlecline_tes::init(const C_csp_tes::S_csp_tes_init_inputs init_inputs)
{
    // Define Cross Sectional Area
    m_Ac = M_PI * std::pow(0.5 * m_diameter, 2.0);

    // Define initial temperatures
    if (m_use_T_grad_init == false)
    {
        double dx = m_height / m_n_xstep;               // [m]
        m_T_prev_vec = std::vector<double>(m_n_xstep + 1);
        // Loop through space
        for (int i = 0; i <= m_n_xstep; i++)
        {
            double frac = (i * dx) / m_height;
            if (frac < m_f_V_hot_ini * 0.01)
                m_T_prev_vec[i] = m_T_tank_hot_ini;
            else
                m_T_prev_vec[i] = m_T_tank_cold_ini;
        }
    }
}

bool C_csp_particlecline_tes::does_tes_exist()
{
	return false;
}

bool C_csp_particlecline_tes::is_cr_to_cold_allowed()
{
	return false;
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
    else
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
    

    return std::numeric_limits<double>::quiet_NaN();
}

void C_csp_particlecline_tes::converged()
{
    m_T_prev_vec = m_T_calc_vec;
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
    double& T_hot_ave /*K*/, double& T_cold_ave /*K*/, double& T_hot_final /*K*/, double& T_cold_final /*K*/
    )
{
    // Inputs
    double dens_fluid = 1.204;  // [kg/m3] density of air at room temp
    double cp_fluid = 1005;     // [J/kg K] specific heat of air at room temp

    // Define timestep and spatial step
    double dt = timestep / m_n_subtimestep;  // [s] subtimestep
    double dx = m_height / m_n_xstep;               // [m]

    // Calculate Coefficients (assume constant for now)
    double cp_eff = m_void_frac * dens_fluid * cp_fluid
        + (1.0 - m_void_frac) * m_dens_solid * m_cp_solid;  // [J/m3 K]
    double u0 = m_dot_htf_in / m_Ac;          // [kg/s m3]
    double alpha = (dens_fluid * u0 * cp_fluid * dt) / (cp_eff * dx);
    double beta = (m_k_eff * dt) / (cp_eff * std::pow(dx, 2.0));

    // Initialize Temperature Vectors
    std::vector<double> T_calc_vec(m_n_xstep + 1);
    std::vector<double> T_prev_vec_subtime = m_T_prev_vec;
    std::vector<double> T_out_vec(m_n_xstep, 0.0);

    // Loop through subtimesteps
    for (int n = 0; n < m_n_subtimestep; n++)
    {
        // Loop through space
        for (int i = 0; i <= m_n_xstep; i++)
        {
            // Charge INLET
            if (i == 0)
            {
                T_calc_vec[i] = (T_prev_vec_subtime[0] + (alpha * T_htf_hot_in)
                    + (beta * (T_prev_vec_subtime[i + 1] - (2.0 * T_prev_vec_subtime[i]) + T_htf_hot_in)))
                    / (1.0 + alpha);
            }

            // Charge OUTLET
            else if (i == m_n_xstep)
            {
                T_calc_vec[i] = (T_prev_vec_subtime[i] + (alpha * T_calc_vec[i - 1])
                    + (beta * (T_prev_vec_subtime[i] - 2.0 * T_prev_vec_subtime[i - 1] + T_prev_vec_subtime[i - 2])))
                    / (1.0 + alpha);
            }
            else
            {
                T_calc_vec[i] = (T_prev_vec_subtime[i] + (alpha * T_calc_vec[i - 1])
                    + (beta * (T_prev_vec_subtime[i + 1] - 2.0 * T_prev_vec_subtime[i] + T_prev_vec_subtime[i - 1])))
                    / (1.0 + alpha);
            }
        }

        // Reset Prev Vec
        T_prev_vec_subtime = T_calc_vec;

        // Save outlet temp
        T_out_vec[n] = T_calc_vec[m_n_xstep];
    }

    m_T_calc_vec = T_calc_vec;

    // Calculate Time Average Outlet Temp
    double T_out_avg = std::accumulate(T_out_vec.begin(), T_out_vec.end(), 0.0) / T_out_vec.size();

    return true;
}

bool C_csp_particlecline_tes::discharge(double timestep /*s*/, double T_amb /*K*/, double m_dot_htf_in /*kg/s*/,
    double T_htf_cold_in /*K*/, double& T_htf_hot_out /*K*/,
    double& q_dot_heater /*MWe*/, double& m_dot_tank_to_tank /*kg/s*/, double& W_dot_rhtf_pump /*MWe*/,
    double& q_dot_loss /*MWt*/, double& q_dot_dc_to_htf /*MWt*/, double& q_dot_ch_from_htf /*MWt*/,
    double& T_hot_ave /*K*/, double& T_cold_ave /*K*/, double& T_hot_final /*K*/, double& T_cold_final /*K*/
    )
{
    // Inputs
    double dens_fluid = 1.204;  // [kg/m3] density of air at room temp
    double cp_fluid = 1005;     // [J/kg K] specific heat of air at room temp

    // Define timestep and spatial step
    double dt = timestep / m_n_subtimestep;  // [s] subtimestep
    double dx = m_height / m_n_xstep;               // [m]

    // Calculate Coefficients (assume constant for now)
    double cp_eff = m_void_frac * dens_fluid * cp_fluid
        + (1.0 - m_void_frac) * m_dens_solid * m_cp_solid;  // [J/m3 K]
    double u0 = m_dot_htf_in / m_Ac;          // [kg/s m3]
    double alpha = (dens_fluid * u0 * cp_fluid * dt) / (cp_eff * dx);
    double beta = (m_k_eff * dt) / (cp_eff * std::pow(dx, 2.0));

    // Initialize Temperature Vectors
    std::vector<double> T_calc_vec(m_n_xstep + 1);
    std::vector<double> T_prev_vec_subtime = m_T_prev_vec;
    std::vector<double> T_out_vec(m_n_xstep, 0.0);

    // Loop through subtimesteps
    for (int n = 0; n < m_n_subtimestep; n++)
    {
        // Loop through space
        for (int i = m_n_xstep; i >= 0; i--)
        {
            // Discharge OUTLET
            if (i == 0)
            {
                T_calc_vec[i] = (T_prev_vec_subtime[i] + (alpha * T_calc_vec[i + 1])
                    + (beta * (T_prev_vec_subtime[i] - 2.0 * T_prev_vec_subtime[i + 1] + T_prev_vec_subtime[i + 2])))
                    / (1.0 + alpha);
            }
            // Discharge INLET
            else if (i == m_n_xstep)
            {
                T_calc_vec[i] = (T_prev_vec_subtime[i] + (alpha * T_htf_cold_in)
                    + (beta * (T_prev_vec_subtime[i - 1] - (2.0 * T_prev_vec_subtime[i]) + T_htf_cold_in)))
                    / (1.0 + alpha);
            }
            // Middle space
            else
            {
                T_calc_vec[i] = (T_prev_vec_subtime[i] + (alpha * T_calc_vec[i + 1])
                    + (beta * (T_prev_vec_subtime[i - 1] - 2.0 * T_prev_vec_subtime[i] + T_prev_vec_subtime[i + 1])))
                    / (1.0 + alpha);
            }
        }

        // Reset Prev Vec
        T_prev_vec_subtime = T_calc_vec;

        // Save outlet temp
        T_out_vec[n] = T_calc_vec[0];
    }

    m_T_calc_vec = T_calc_vec;

    // Calculate Time Average Outlet Temp
    double T_out_avg = std::accumulate(T_out_vec.begin(), T_out_vec.end(), 0.0) / T_out_vec.size();

    return true;
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
