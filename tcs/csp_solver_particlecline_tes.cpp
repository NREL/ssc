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
    double T_cold_des_C,	                    // [C] convert to K in constructor()
    double T_hot_des_C,	                        // [C] convert to K in constructor()
    double T_tank_hot_ini_C,	                     // [C] Initial temperature in hot storage tank
    double T_tank_cold_ini_C,	                     // [C] Initial temperature in cold storage cold
    double f_V_hot_ini,                             // [%] Initial fraction of available volume that is hot
    int n_xstep,                                    // number spatial sub steps
    int n_subtimestep,                              // number subtimesteps
    double tes_pump_coef		                    // [kW/kg/s] Pumping power to move 1 kg/s of HTF through tes loop
)
    :
    m_external_fl(external_fl), m_external_fl_props(external_fl_props),
    m_f_V_hot_ini(f_V_hot_ini),
    m_n_xstep(n_xstep), m_n_subtimestep(n_subtimestep), m_tes_pump_coef(tes_pump_coef)
{
    // Convert Temperature Units
    m_T_cold_des = T_cold_des_C + 273.15;
    m_T_hot_des = T_hot_des_C + 273.15;
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

void C_csp_particlecline_tes::set_T_grad_init(std::vector<double> T_grad_init_C)
{
    for (double T_C : T_grad_init_C)
        m_T_prev_vec.push_back(T_C + 273.15);

    m_use_T_grad_init = true;
}

void C_csp_particlecline_tes::init(const C_csp_tes::S_csp_tes_init_inputs init_inputs)
{
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
    // Return temperature closest to charge inlet (hot)
    return m_T_prev_vec[0];
}

double C_csp_particlecline_tes::get_cold_temp()
{
    // Return temperature furthest from charge inlet (cold)
    return m_T_prev_vec[m_T_prev_vec.size() - 1];
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
    // Enthalpy balance on inlet to cold cv
    double T_htf_cold_cv_in = T_sink_out_cold;     //[K]
    double m_dot_total_to_cv_cold = m_dot_cv_hot_to_sink + m_dot_cr_to_cv_cold;    //[kg/s]
    if (m_dot_total_to_cv_cold > 0.0) {
        T_htf_cold_cv_in = (m_dot_cv_hot_to_sink * T_sink_out_cold + m_dot_cr_to_cv_cold * T_cr_out_hot) / (m_dot_total_to_cv_cold);
    }

    s_outputs = S_csp_tes_outputs();

    double m_dot_cr_to_tes_hot, m_dot_cr_to_tes_cold, m_dot_tes_hot_out, m_dot_pc_to_tes_cold, m_dot_tes_cold_out, m_dot_tes_cold_in;
    m_dot_cr_to_tes_hot = m_dot_cr_to_tes_cold = m_dot_tes_hot_out = m_dot_pc_to_tes_cold = m_dot_tes_cold_out = m_dot_tes_cold_in = std::numeric_limits<double>::quiet_NaN();
    double m_dot_src_to_sink, m_dot_sink_to_src;
    m_dot_src_to_sink = m_dot_sink_to_src = std::numeric_limits<double>::quiet_NaN();

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

    // Solve pumping power here
    double W_dot_htf_pump = std::numeric_limits<double>::quiet_NaN();

    s_outputs.m_q_heater = q_dot_heater;                        //[MWe] Heater
    s_outputs.m_W_dot_elec_in_tot = W_dot_rhtf_pump;            //[MWe] Use this pumping power?

    s_outputs.m_q_dot_dc_to_htf = q_dot_dc_to_htf;              //[MWt] Thermal power to the HTF from storage
    s_outputs.m_q_dot_ch_from_htf = q_dot_ch_from_htf;          //[MWt]  Thermal power from the HTF to storage
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
    //mc_reported_outputs.value(E_MASS_COLD_TANK, mc_cold_tank.get_m_m_calc());		//[kg]
    //mc_reported_outputs.value(E_MASS_HOT_TANK, mc_hot_tank.get_m_m_calc());			//[kg]
    mc_reported_outputs.value(E_W_DOT_HTF_PUMP, W_dot_htf_pump);    //[MWe]
    //mc_reported_outputs.value(E_VOL_TOT, vol_total);    //[m3]
    //mc_reported_outputs.value(E_MASS_TOT, mc_cold_tank.get_m_m_calc() + mc_hot_tank.get_m_m_calc());    //[m3]

    return 0;
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
    //double dens_fluid = 1.204;  // [kg/m3] density of air at room temp
    //double cp_fluid = 1005;     // [J/kg K] specific heat of air at room temp

    double dens_fluid_avg = mc_external_htfProps.dens((m_T_cold_des + m_T_hot_des) * 0.5, 1); //[kg/m3]
    double cp_fluid_avg = mc_external_htfProps.Cp_ave(m_T_cold_des, m_T_hot_des);

    // Define timestep and spatial step
    double dt = timestep / m_n_subtimestep;  // [s] subtimestep
    double dx = m_height / m_n_xstep;               // [m]

    // Calculate Coefficients (assume constant for now)
    double cp_eff = m_void_frac * dens_fluid_avg * cp_fluid_avg
        + (1.0 - m_void_frac) * m_dens_solid * m_cp_solid;  // [J/m3 K]
    double u0 = m_dot_htf_in / m_Ac;          // [kg/s m3]
    double alpha = (dens_fluid_avg * u0 * cp_fluid_avg * dt) / (cp_eff * dx);
    double beta = (m_k_eff * dt) / (cp_eff * std::pow(dx, 2.0));

    // Initialize Temperature Vectors
    std::vector<double> T_calc_vec(m_n_xstep + 1);          // [K] Temperature gradient at end of subtimestep
    std::vector<double> T_prev_vec_subtime = m_T_prev_vec;  // [K] Temperature gradient at beginning of subtimestep
    std::vector<double> T_out_vec(m_n_subtimestep, 0.0);    // [K] Temperature at Outlet
    std::vector<double> T_hot_vec(m_n_subtimestep, 0.0);    // [K] Temperature closest to inlet

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

        // Save temp closest to inlet
        T_hot_vec[n] = T_calc_vec[0];
    }

    m_T_calc_vec = T_calc_vec;

    // Calculate Time Average Outlet Temp
    double T_out_avg = std::accumulate(T_out_vec.begin(), T_out_vec.end(), 0.0) / T_out_vec.size(); //[K]

    // Calculate Time Average Closest to Inlet Temp
    double T_hot_avg = std::accumulate(T_hot_vec.begin(), T_hot_vec.end(), 0.0) / T_hot_vec.size(); //[K]

    // No Heater (for now)
    q_dot_heater = 0.0;     //[MWt]

    // No tank to tank mass (only one tank)
    m_dot_tank_to_tank = std::numeric_limits<double>::quiet_NaN();  //[kg/s]

    // Pumping Power
    W_dot_rhtf_pump = m_dot_htf_in * m_tes_pump_coef / 1.E3;    //[MWe] Pumping power through tank

    // Cold out = Average Outlet Temp
    T_htf_cold_out = T_out_avg; //[K]

    // Heat Loss (need to calculate)
    q_dot_loss = std::numeric_limits<double>::quiet_NaN();  //[MWt]

    // Heat transferred from storage to htf
    q_dot_dc_to_htf = 0.0;  //[MWt] <- should this be calculated?

    // Average Hot Temperature in Tank
    T_hot_ave = T_hot_avg;  //[K] Average temperature in tank location closest to inlet

    // Average Cold outlet temperature
    T_cold_ave = T_out_avg;    //[K] 

    // Final Hot Temperature
    T_hot_final = T_hot_vec[T_hot_vec.size() - 1];  // [K]

    // Final Cold Temperature
    T_cold_final = T_out_vec[T_out_vec.size() - 1]; // [K]

    // Charge power from htf to storage
    q_dot_ch_from_htf = 0.0;    // [MWt]
    for (double T_out : T_out_vec)
    {
        q_dot_ch_from_htf += m_dot_htf_in * cp_fluid_avg * (T_htf_hot_in - T_out) * 1.E-3 * (dt / timestep);  // [MWt]
    }

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
    //double dens_fluid = 1.204;  // [kg/m3] density of air at room temp
    //double cp_fluid = 1005;     // [J/kg K] specific heat of air at room temp

    double dens_fluid_avg = mc_external_htfProps.dens((m_T_cold_des + m_T_hot_des) * 0.5, 1); //[kg/m3]
    double cp_fluid_avg = mc_external_htfProps.Cp_ave(m_T_cold_des, m_T_hot_des);

    // Define timestep and spatial step
    double dt = timestep / m_n_subtimestep;  // [s] subtimestep
    double dx = m_height / m_n_xstep;               // [m]

    // Calculate Coefficients (assume constant for now)
    double cp_eff = m_void_frac * dens_fluid_avg * cp_fluid_avg
        + (1.0 - m_void_frac) * m_dens_solid * m_cp_solid;  // [J/m3 K]
    double u0 = m_dot_htf_in / m_Ac;          // [kg/s m3]
    double alpha = (dens_fluid_avg * u0 * cp_fluid_avg * dt) / (cp_eff * dx);
    double beta = (m_k_eff * dt) / (cp_eff * std::pow(dx, 2.0));

    // Initialize Temperature Vectors
    std::vector<double> T_calc_vec(m_n_xstep + 1);              // [K] Temperature gradient at end of subtimestep
    std::vector<double> T_prev_vec_subtime = m_T_prev_vec;      // [K] Temperature gradient at beginning of subtimestep
    std::vector<double> T_out_vec(m_n_subtimestep, 0.0);        // [K] Temperature at Outlet
    std::vector<double> T_cold_vec(m_n_subtimestep, 0.0);       // [K] Temperature closest to inlet (cold)

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

        // Save Temperature closest to inlet (cold temp)
        T_cold_vec[n] = T_calc_vec[T_calc_vec.size() - 1];
    }

    m_T_calc_vec = T_calc_vec;

    // Calculate Time Average Outlet Temp (hot)
    double T_out_avg = std::accumulate(T_out_vec.begin(), T_out_vec.end(), 0.0) / T_out_vec.size();

    // Calculate Time Average Cold Temp (closest to inlet)
    double T_cold_avg = std::accumulate(T_cold_vec.begin(), T_cold_vec.end(), 0.0) / T_cold_vec.size();

    // No Heater (for now)
    q_dot_heater = 0.0;     //[MWt]

    // No tank to tank mass (only one tank)
    m_dot_tank_to_tank = std::numeric_limits<double>::quiet_NaN();  //[kg/s]

    // Pumping Power
    W_dot_rhtf_pump = m_dot_htf_in * m_tes_pump_coef / 1.E3;    //[MWe] Pumping power through tank

    // Cold out = Average Outlet Temp
    T_htf_hot_out = T_out_avg; //[K]

    // Heat Loss (need to calculate)
    q_dot_loss = std::numeric_limits<double>::quiet_NaN();  //[MWt]

    // Heat transferred from storage to htf
    q_dot_dc_to_htf = 0.0;  //[MWt]
    for (double T_out : T_out_vec)
    {
        q_dot_dc_to_htf += m_dot_htf_in * cp_fluid_avg * (T_out - T_htf_cold_in) * 1.E-3 * (dt / timestep);  // [MWt]
    }

    // Average Hot Temperature in Tank
    T_hot_ave = T_out_avg;  //[K] Average temperature in tank location closest to inlet

    // Average Cold outlet temperature
    T_cold_ave = T_cold_avg;    //[K] 

    // Final Hot Temperature
    T_hot_final = T_out_vec[T_out_vec.size() - 1];  // [K]

    // Final Cold Temperature
    T_cold_final = T_cold_vec[T_cold_vec.size() - 1]; // [K]

    // Charge power from htf to storage
    q_dot_ch_from_htf = 0.0;    // [MWt]

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
