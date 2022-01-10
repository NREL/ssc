/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "csp_solver_cr_heat_pump.h"
#include "csp_solver_core.h"

C_csp_cr_heat_pump::C_csp_cr_heat_pump(double COP_heat_des /*-*/, double q_dot_hot_out_des /*MWt*/,
    double f_elec_consume_vs_W_dot_thermo /*-*/,
    double T_HT_HTF_hot /*C*/, double T_HT_HTF_cold /*C*/, double T_CT_HTF_cold /*C*/, double T_CT_HTF_hot /*C*/,
    double f_q_dot_min /*-*/, double f_q_dot_des_allowable_su /*-*/, double hrs_startup_at_max_rate /*hr*/,
    double heat_pump_HT_htf_pump_coef /*kW/kg/s*/, double heat_pump_CT_htf_pump_coef /*kW/kg/s*/,
    int HT_htf_code /*-*/, util::matrix_t<double> HT_ud_htf_props,
    int CT_htf_code /*-*/, util::matrix_t<double> CT_ud_htf_props)
{
    // Defined in constructor
    m_COP_heat_des = COP_heat_des;              //[-]
    m_q_dot_hot_out_des = q_dot_hot_out_des;    //[MWt]
    m_f_elec_consume_vs_W_dot_thermo_des = f_elec_consume_vs_W_dot_thermo;  //[-]
    m_T_HT_HTF_hot_des = T_HT_HTF_hot;              //[C]
    m_T_HT_HTF_cold_des = T_HT_HTF_cold;            //[C]
    m_T_CT_HTF_cold_des = T_CT_HTF_cold;            //[C]
    m_T_CT_HTF_hot_des = T_CT_HTF_hot;              //[C]

    m_f_q_dot_min = f_q_dot_min;                    //[-]

    m_f_q_dot_des_allowable_su = f_q_dot_des_allowable_su;  //[-]
    m_hrs_startup_at_max_rate = hrs_startup_at_max_rate;    //[hr]

    m_heat_pump_HT_htf_pump_coef = heat_pump_HT_htf_pump_coef;  //[kW/kg/s]
    m_heat_pump_CT_htf_pump_coef = heat_pump_CT_htf_pump_coef;  //[kW/kg/s]

    m_HT_htf_code = HT_htf_code;
    m_HT_ud_htf_props = HT_ud_htf_props;

    m_CT_htf_code = CT_htf_code;
    m_CT_ud_htf_props = CT_ud_htf_props;

    // Initialize calculated member variables to nan
    m_W_dot_in_thermo_des = std::numeric_limits<double>::quiet_NaN();
    m_q_dot_cold_in_des = std::numeric_limits<double>::quiet_NaN();
    m_W_dot_consume_elec_des = std::numeric_limits<double>::quiet_NaN();
    m_W_dot_in_net_des = std::numeric_limits<double>::quiet_NaN();
    m_COP_net_des = std::numeric_limits<double>::quiet_NaN();

    m_T_HT_HTF_avg_des = std::numeric_limits<double>::quiet_NaN();
    m_cp_HT_HTF_des = std::numeric_limits<double>::quiet_NaN();
    m_T_CT_HTF_avg_des = std::numeric_limits<double>::quiet_NaN();
    m_cp_CT_HTF_des = std::numeric_limits<double>::quiet_NaN();

    m_m_dot_HT_des = std::numeric_limits<double>::quiet_NaN();
    m_W_dot_HT_htf_pump_des = std::numeric_limits<double>::quiet_NaN();

    m_m_dot_CT_des = std::numeric_limits<double>::quiet_NaN();
    m_W_dot_CT_htf_pump_des = std::numeric_limits<double>::quiet_NaN();

    m_q_dot_min_des = std::numeric_limits<double>::quiet_NaN();

    m_q_dot_su_max = std::numeric_limits<double>::quiet_NaN();
    m_W_dot_su_max = std::numeric_limits<double>::quiet_NaN();
    m_E_su_des = std::numeric_limits<double>::quiet_NaN();
    m_E_W_dot_su_des = std::numeric_limits<double>::quiet_NaN();
    m_t_su_des = std::numeric_limits<double>::quiet_NaN();

    // Timestep state variables
    m_E_su_initial = std::numeric_limits<double>::quiet_NaN();
    m_E_su_calculated = std::numeric_limits<double>::quiet_NaN();
}

C_csp_cr_heat_pump::~C_csp_cr_heat_pump(){}

// ***********************
// Inherited methods
// ***********************
void C_csp_cr_heat_pump::init(const C_csp_collector_receiver::S_csp_cr_init_inputs init_inputs,
    C_csp_collector_receiver::S_csp_cr_solved_params& solved_params)
{
    // Not using init_inputs because the parameters are relevant to solar energy

    heat_pump_helpers::design_calcs(m_q_dot_hot_out_des, m_COP_heat_des,
        m_f_elec_consume_vs_W_dot_thermo_des,
        m_W_dot_in_thermo_des, m_q_dot_cold_in_des,
        m_W_dot_consume_elec_des, m_W_dot_in_net_des,
        m_COP_net_des);

    std::unique_ptr<HTFProperties> HT_htfProps(new HTFProperties());
    m_HT_htfProps = std::move(HT_htfProps);
    m_HT_htfProps->Initialize(m_HT_htf_code, m_HT_ud_htf_props);

    std::unique_ptr<HTFProperties> CT_htfProps(new HTFProperties());
    m_CT_htfProps = std::move(CT_htfProps);
    m_CT_htfProps->Initialize(m_CT_htf_code, m_CT_ud_htf_props);

    double eta_htf_pump = 0.85;     //[-] used to back out pressure drop

    m_T_HT_HTF_avg_des = 0.5 * (m_T_HT_HTF_cold_des + m_T_HT_HTF_hot_des);    //[C]
    m_cp_HT_HTF_des = m_HT_htfProps->Cp(m_T_HT_HTF_avg_des + 273.15);         //[kJ/kg-K]
    double rho_HT_htf_des = m_HT_htfProps->dens(m_T_HT_HTF_avg_des + 273.15,1.0);   //[kg/m3]  
    m_m_dot_HT_des = m_q_dot_hot_out_des * 1.E3 / (m_cp_HT_HTF_des * (m_T_HT_HTF_hot_des - m_T_HT_HTF_cold_des));    //[kg/s]
    m_W_dot_HT_htf_pump_des = m_heat_pump_HT_htf_pump_coef*m_m_dot_HT_des*1.E-3;        //[MWe]
    double HT_htf_deltaP = m_W_dot_HT_htf_pump_des*rho_HT_htf_des/m_m_dot_HT_des*eta_htf_pump;   //[MPa]

    m_T_CT_HTF_avg_des = 0.5 * (m_T_CT_HTF_cold_des + m_T_CT_HTF_hot_des);    //[C]
    m_cp_CT_HTF_des = m_CT_htfProps->Cp(m_T_CT_HTF_avg_des + 273.15);         //[kJ/kg-K]
    double rho_CT_htf_des = m_CT_htfProps->dens(m_T_CT_HTF_avg_des + 273.15,1.0);   //[kg/m3]
    m_m_dot_CT_des = m_q_dot_cold_in_des*1.E3/(m_cp_CT_HTF_des*(m_T_CT_HTF_hot_des-m_T_CT_HTF_cold_des));  //[kg/s]
    m_W_dot_CT_htf_pump_des = m_heat_pump_CT_htf_pump_coef*m_m_dot_CT_des*1.E-3;        //[MWe]
    double CT_htf_deltaP = m_W_dot_CT_htf_pump_des*rho_CT_htf_des/m_m_dot_CT_des*eta_htf_pump;  //[MWe]

    // Min operating
    m_q_dot_min_des = m_f_q_dot_min * m_q_dot_hot_out_des;  //[MWt]

    // Check startup parameters
    m_f_q_dot_des_allowable_su = std::max(0.0, m_f_q_dot_des_allowable_su); //[-]
    m_hrs_startup_at_max_rate = std::max(0.0, m_hrs_startup_at_max_rate);   //[hr]

    // Calculate design startup requirements
        // Base startup on q_dot_hot even though it's heat out of cycle and to TES
        // Probably the most critical HX in system. also analogous to q_dot_rec?
    m_q_dot_su_max = m_q_dot_hot_out_des * m_f_q_dot_des_allowable_su;  //[MWt]
    m_W_dot_su_max = m_q_dot_su_max / m_COP_heat_des;           //[-]
    m_E_su_des = m_q_dot_su_max * m_hrs_startup_at_max_rate;    //[MWt-hr]
    m_E_W_dot_su_des = m_W_dot_su_max * m_hrs_startup_at_max_rate;  //[MWe-hr]
    m_t_su_des = m_E_su_des / m_q_dot_su_max;   //[hr]

    solved_params.m_T_htf_cold_des = m_T_HT_HTF_cold_des + 273.15; //[K]
    solved_params.m_P_cold_des = std::numeric_limits<double>::quiet_NaN();  //[kPa]
    solved_params.m_x_cold_des = std::numeric_limits<double>::quiet_NaN();  //[-]
    solved_params.m_T_htf_hot_des = m_T_HT_HTF_hot_des + 273.15;   //[K]
    solved_params.m_q_dot_rec_des = m_q_dot_hot_out_des;         //[MWt]
    solved_params.m_A_aper_total = 0.0;                         //[m2]
    solved_params.m_dP_sf = HT_htf_deltaP*1.E1;                //[bar] convert from MPa

    // State variables
    m_E_su_initial = m_E_su_des;        //[MWt-hr]
    m_operating_mode_converged = C_csp_collector_receiver::OFF;					//

    return;
}

C_csp_collector_receiver::E_csp_cr_modes C_csp_cr_heat_pump::get_operating_state()
{
    return m_operating_mode_converged;  //[-]
}

double C_csp_cr_heat_pump::get_startup_time()   //[hr]
{
    return m_t_su_des;      //[hr]
}

double C_csp_cr_heat_pump::get_startup_energy() //[MWt-hr]
{
    return m_E_su_des;      //[MWt-hr]
}

double C_csp_cr_heat_pump::get_pumping_parasitic_coef()  //MWe/MWt
{
    throw(C_csp_exception("C_csp_cr_heat_pump::get_pumping_parasitic_coef() is not complete"));
}

double C_csp_cr_heat_pump::get_min_power_delivery()    //MWt
{
    return m_q_dot_min_des;     //[MWt]
}

double C_csp_cr_heat_pump::get_max_power_delivery(double T_cold_in)   //MWt
{
    return m_q_dot_hot_out_des; //[MWt]
}

double C_csp_cr_heat_pump::get_tracking_power()		//MWe
{
    throw(C_csp_exception("C_csp_cr_heat_pump::get_tracking_power() is not complete"));
}

double C_csp_cr_heat_pump::get_col_startup_power()		//MWe-hr
{
    throw(C_csp_exception("C_csp_cr_heat_pump::get_col_startup_power() is not complete"));
}

void C_csp_cr_heat_pump::off(const C_csp_weatherreader::S_outputs& weather,
    const C_csp_solver_htf_1state& htf_state_in,
    C_csp_collector_receiver::S_csp_cr_out_solver& cr_out_solver,
    const C_csp_solver_sim_info& sim_info)
{
    // Set outputs required by solver
    cr_out_solver.m_q_startup = 0.0;                //[MWt-hr]
    cr_out_solver.m_time_required_su = 0.0;         //[s]
    cr_out_solver.m_m_dot_salt_tot = 0.0;           //[kg/hr]
    cr_out_solver.m_q_thermal = 0.0;                //[MWt]
    cr_out_solver.m_T_salt_hot = m_T_HT_HTF_hot_des;//[C]
    cr_out_solver.m_component_defocus = 1.0;        //[-]
    cr_out_solver.m_is_recirculating = false;       //[-]

    cr_out_solver.m_q_dot_heater = 0.0;             //[MWt]

    cr_out_solver.m_W_dot_elec_in_tot = 0.0;        //[MWe]

    m_operating_mode = C_csp_collector_receiver::OFF;
    m_E_su_calculated = m_E_su_des;     //[MWt-hr]

    return;
}

void C_csp_cr_heat_pump::startup(const C_csp_weatherreader::S_outputs& weather,
    const C_csp_solver_htf_1state& htf_state_in,
    C_csp_collector_receiver::S_csp_cr_out_solver& cr_out_solver,
    const C_csp_solver_sim_info& sim_info)
{
    double step_hrs = sim_info.ms_ts.m_step / 3600.0;    //[hr]

    // Assume startup is always at max startup rate
    double q_dot_su = m_q_dot_su_max;   //[MWt]
    double time_remaining_su = m_E_su_initial / q_dot_su; //[hr]

    double time_required_su = std::numeric_limits<double>::quiet_NaN();

    if (time_remaining_su > step_hrs) {
        time_required_su = step_hrs;
        m_operating_mode = C_csp_collector_receiver::STARTUP;
    }
    else {
        time_required_su = time_remaining_su;
        m_operating_mode = C_csp_collector_receiver::ON;
    }

    double q_startup = q_dot_su * time_required_su;         //[MWt-hr]

    // Apply net COP to get electricity consumption at startup
    double W_dot_in_thermo = q_dot_su / m_COP_net_des;     //[MWe]
    // But don't calculate or estimate HT & CT pumping power
    //    because we don't really know mechanisms of startup
    double W_dot_htf_pumps = 0.0;       //[MWe]

    m_E_su_calculated = fmax(0.0, m_E_su_initial - q_startup);  //[MWt-hr]

    cr_out_solver.m_q_startup = q_startup;                  //[MWt-hr]
    cr_out_solver.m_time_required_su = time_required_su * 3600.0; //[s]
    cr_out_solver.m_m_dot_salt_tot = 0.0;                   //[kg/hr]
    cr_out_solver.m_q_thermal = 0.0;                        //[MWt]
    cr_out_solver.m_T_salt_hot = m_T_HT_HTF_hot_des;        //[C]
    cr_out_solver.m_component_defocus = 1.0;                //[-]
    cr_out_solver.m_is_recirculating = false;               //[-]

    cr_out_solver.m_q_dot_heater = 0.0;             //[MWt]

    cr_out_solver.m_W_dot_elec_in_tot = W_dot_in_thermo + W_dot_htf_pumps;        //[MWe]

    return;
}

void C_csp_cr_heat_pump::on(const C_csp_weatherreader::S_outputs& weather,
    const C_csp_solver_htf_1state& htf_state_in,
    double q_dot_elec_to_CR_heat /*MWt*/, double field_control,
    C_csp_collector_receiver::S_csp_cr_out_solver& cr_out_solver,
    const C_csp_solver_sim_info& sim_info)
{
    throw(C_csp_exception("C_csp_cr_heat_pump::on() is not complete"));
}

void C_csp_cr_heat_pump::estimates(const C_csp_weatherreader::S_outputs& weather,
    const C_csp_solver_htf_1state& htf_state_in,
    C_csp_collector_receiver::S_csp_cr_est_out& est_out,
    const C_csp_solver_sim_info& sim_info)
{
    // Assume:
    // 1) no dependence between available heater output and weather
    // 2) heater is always capable of design output
    // 3) no mass flow rate bounds (for now)
    // 4) heater is controlled to always return HTF at design hot temperature

    double m_dot_htf = m_q_dot_hot_out_des * 1.E3 / (m_cp_HT_HTF_des * (m_T_HT_HTF_hot_des - htf_state_in.m_temp));    //[kg/s]

    E_csp_cr_modes mode = get_operating_state();

    if (mode == C_csp_collector_receiver::ON || mode == C_csp_collector_receiver::OFF_NO_SU_REQ)
    {
        est_out.m_q_dot_avail = m_q_dot_hot_out_des;	//[MWt]
        est_out.m_m_dot_avail = m_dot_htf * 3600.0;		//[kg/hr]
        est_out.m_T_htf_hot = m_T_HT_HTF_hot_des;		//[C]
        est_out.m_q_startup_avail = 0.0;                //[MWt]
    }
    else
    {
        est_out.m_q_startup_avail = m_q_dot_hot_out_des;	//[MWt]
        est_out.m_q_dot_avail = 0.0;
        est_out.m_m_dot_avail = 0.0;
        est_out.m_T_htf_hot = 0.0;
    }

    return;
}

void C_csp_cr_heat_pump::converged()
{
    throw(C_csp_exception("C_csp_cr_heat_pump::converged() is not complete"));
}

void C_csp_cr_heat_pump::write_output_intervals(double report_time_start,
    const std::vector<double>& v_temp_ts_time_end, double report_time_end)
{
    throw(C_csp_exception("C_csp_cr_heat_pump::write_output_intervals() is not complete"));
}

double C_csp_cr_heat_pump::calculate_optical_efficiency(const C_csp_weatherreader::S_outputs& weather, const C_csp_solver_sim_info& sim)
{
    throw(C_csp_exception("C_csp_cr_heat_pump::calculate_optical_efficiency() is not complete"));
}

double C_csp_cr_heat_pump::calculate_thermal_efficiency_approx(const C_csp_weatherreader::S_outputs& weather, double q_incident /*MW*/)
{
    throw(C_csp_exception("C_csp_cr_heat_pump::calculate_thermal_efficiency() is not complete"));
}

double C_csp_cr_heat_pump::get_collector_area()
{
    // Collector area is not a relevant metric for a heat pump
    return std::numeric_limits<double>::quiet_NaN();
}

void C_csp_cr_heat_pump::get_design_parameters(double& W_dot_in /*MWe*/, double& q_dot_cold_in /*MWt*/,
    double& W_dot_elec_parasitic /*MWe*/, double& W_dot_in_net /*MWe*/,
    double& COP_net /*-*/,
    double& m_dot_HT_htf /*kg/s*/, double& cp_HT_htf /*kJ/kg-K*/, double& W_dot_HT_htf_pump /*MWe*/,
    double& m_dot_CT_htf /*kg/s*/, double& cp_CT_htf /*kJ/kg-K*/, double& W_dot_CT_htf_pump /*MWe*/,
    double& E_su /*MWt-hr*/)
{
    W_dot_in = m_W_dot_in_thermo_des;       //[MWe]
    q_dot_cold_in = m_q_dot_cold_in_des;    //[MWt]
    W_dot_elec_parasitic = m_W_dot_consume_elec_des;    //[MWe]
    W_dot_in_net = m_W_dot_in_net_des;      //[MWe]
    COP_net = m_COP_net_des;                //[-]

    m_dot_HT_htf = m_m_dot_HT_des;          //[kg/s]
    cp_HT_htf = m_cp_HT_HTF_des;            //[kJ/kg-K]
    W_dot_HT_htf_pump = m_W_dot_HT_htf_pump_des;    //[MWe]

    m_dot_CT_htf = m_m_dot_CT_des;          //[kg/s]
    cp_CT_htf = m_cp_CT_HTF_des;            //[kJ/kg-K]
    W_dot_CT_htf_pump = m_W_dot_CT_htf_pump_des;    //[MWe]

    E_su = m_E_su_des;                      //[MWt-hr]
}

// ***************************************************************
// ***************************************************************

void heat_pump_helpers::design_calcs(double q_dot_hot_out /*MWt*/, double COP_heat /*-*/,
    double f_elec_consume_vs_W_dot_thermo /*-*/,
    double& W_dot_in_thermo /*MWe*/, double& q_dot_cold_in /*MWt*/,
    double& W_dot_consume_elec /*MWe*/, double& W_dot_in_net /*MWe*/,
    double& COP_heat_net /*-*/)
{
    W_dot_in_thermo = q_dot_hot_out / COP_heat;             //[MWe]
    q_dot_cold_in = W_dot_in_thermo * (COP_heat - 1.0);     //[MWt]
    W_dot_consume_elec = f_elec_consume_vs_W_dot_thermo * W_dot_in_thermo;  //[MWe]
    W_dot_in_net = W_dot_in_thermo + W_dot_consume_elec;    //[MWe]
    COP_heat_net = q_dot_hot_out / W_dot_in_net;            //[-]
}
