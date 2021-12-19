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

#include "csp_solver_pc_ptes.h"
#include "csp_solver_core.h"

// ***********************
// Inherited methods
// ***********************
C_pc_ptes::C_pc_ptes(double W_dot_thermo /*MWe*/, double eta_therm_mech /*-*/,
    double f_elec_consume_vs_gen /*-*/, double fixed__q_dot_cold__to__q_dot_warm /*-*/,
    double T_HT_HTF_hot /*C*/, double T_HT_HTF_cold /*C*/, double T_CT_HTF_cold /*C*/, double T_CT_HTF_hot /*C*/,
    double cycle_max_frac /*-*/, double cycle_cutoff_frac /*-*/, double q_sby_frac /*-*/,
    double startup_time /*hr*/, double startup_frac /*-*/,
    double HT_htf_pump_coef /*kW/kg/s*/, double CT_htf_pump_coef /*kW/kg/s*/,
    int HT_htf_code /*-*/, util::matrix_t<double> HT_ud_htf_props,
    int CT_htf_code /*-*/, util::matrix_t<double> CT_ud_htf_props)
{
    m_W_dot_thermo_des = W_dot_thermo;      //[MWe]
    m_eta_therm_mech_des = eta_therm_mech;              //[-]
    m_f_elec_consume_vs_gen = f_elec_consume_vs_gen;    //[-]
    m_fixed__q_dot_cold__to__q_dot_warm = fixed__q_dot_cold__to__q_dot_warm;  //[-]
    m_T_HT_HTF_hot_des = T_HT_HTF_hot;              //[C]
    m_T_HT_HTF_cold_des = T_HT_HTF_cold;            //[C]
    m_T_CT_HTF_cold_des = T_CT_HTF_cold;            //[C]
    m_T_CT_HTF_hot_des = T_CT_HTF_hot;              //[C]
    m_cycle_max_frac_des = cycle_max_frac;          //[-]
    m_cycle_cutoff_frac_des = cycle_cutoff_frac;    //[-]
    m_q_sby_frac_des = q_sby_frac;                  //[-]
    m_startup_time_des = startup_time;              //[hr]
    m_startup_frac_des = startup_frac;              //[-]

    m_HT_htf_pump_coef_des = HT_htf_pump_coef;      //[kW/kg/s]
    m_CT_htf_pump_coef_des = CT_htf_pump_coef;      //[kW/kg/s]

    m_HT_htf_code = HT_htf_code;
    m_HT_ud_htf_props = HT_ud_htf_props;

    m_CT_htf_code = CT_htf_code;
    m_CT_ud_htf_props = CT_ud_htf_props;

    // Initialize calculated member variables to nan
    m_W_dot_net_des = std::numeric_limits<double>::quiet_NaN();
    m_q_dot_hot_in_des = std::numeric_limits<double>::quiet_NaN();
    m_q_dot_cold_out_thermo_des = std::numeric_limits<double>::quiet_NaN();
    m_W_dot_elec_parasitic = std::numeric_limits<double>::quiet_NaN();
    m_eta_overall_des = std::numeric_limits<double>::quiet_NaN();
    m_q_dot_cold_to_CTES = std::numeric_limits<double>::quiet_NaN();
    m_q_dot_cold_to_surroundings = std::numeric_limits<double>::quiet_NaN();

    m_T_HT_HTF_avg_des = std::numeric_limits<double>::quiet_NaN();
    m_cp_HT_HTF_des = std::numeric_limits<double>::quiet_NaN();
    m_T_CT_HTF_avg_des = std::numeric_limits<double>::quiet_NaN();
    m_cp_CT_HTF_des = std::numeric_limits<double>::quiet_NaN();

    m_m_dot_HT_des = std::numeric_limits<double>::quiet_NaN();
    m_m_dot_HT_min = std::numeric_limits<double>::quiet_NaN();
    m_m_dot_HT_max = std::numeric_limits<double>::quiet_NaN();
    m_W_dot_HT_htf_pump_des = std::numeric_limits<double>::quiet_NaN();

    m_m_dot_CT_des = std::numeric_limits<double>::quiet_NaN();
    m_W_dot_CT_htf_pump_des = std::numeric_limits<double>::quiet_NaN();


    m_E_su_des = std::numeric_limits<double>::quiet_NaN();
}

void C_pc_ptes::init(C_csp_power_cycle::S_solved_params& solved_params)
{
    pc_ptes_helpers::design_calcs__all(m_W_dot_thermo_des, m_f_elec_consume_vs_gen,
                                        m_eta_therm_mech_des, m_fixed__q_dot_cold__to__q_dot_warm,
                                        m_W_dot_net_des, m_W_dot_elec_parasitic,
                                        m_q_dot_hot_in_des, m_q_dot_cold_out_thermo_des, m_eta_overall_des,
                                        m_q_dot_cold_to_CTES, m_q_dot_cold_to_surroundings);

    std::unique_ptr<HTFProperties> HT_htfProps(new HTFProperties());
    m_HT_htfProps = std::move(HT_htfProps);
    m_HT_htfProps->Initialize(m_HT_htf_code, m_HT_ud_htf_props);

    std::unique_ptr<HTFProperties> CT_htfProps(new HTFProperties());
    m_CT_htfProps = std::move(CT_htfProps);
    m_CT_htfProps->Initialize(m_CT_htf_code, m_CT_ud_htf_props);

    double eta_htf_pump = 0.85;     //[-] used to back out pressure drop

    m_T_HT_HTF_avg_des = 0.5*(m_T_HT_HTF_cold_des + m_T_HT_HTF_hot_des);    //[C]
    m_cp_HT_HTF_des = m_HT_htfProps->Cp(m_T_HT_HTF_avg_des+273.15);         //[kJ/kg-K]
    m_m_dot_HT_des = m_q_dot_hot_in_des * 1.E3 / (m_cp_HT_HTF_des * (m_T_HT_HTF_hot_des - m_T_HT_HTF_cold_des));    //[kg/s]
    double rho_HT_htf_des = m_HT_htfProps->dens(m_T_HT_HTF_avg_des + 273.15, 1.0);   //[kg/m3]  
    m_W_dot_HT_htf_pump_des = m_HT_htf_pump_coef_des * m_m_dot_HT_des * 1.E-3;        //[MWe]
    double HT_htf_deltaP = m_W_dot_HT_htf_pump_des * rho_HT_htf_des / m_m_dot_HT_des * eta_htf_pump;   //[MPa]

    m_T_CT_HTF_avg_des = 0.5*(m_T_CT_HTF_cold_des + m_T_CT_HTF_hot_des);    //[C]
    m_cp_CT_HTF_des = m_CT_htfProps->Cp(m_T_CT_HTF_avg_des+273.15);         //[kJ/kg-K]
    m_m_dot_CT_des = m_q_dot_cold_to_CTES*1.E3/(m_cp_CT_HTF_des*(m_T_CT_HTF_hot_des-m_T_CT_HTF_cold_des));  //[kg/s]
    double rho_CT_htf_des = m_CT_htfProps->dens(m_T_CT_HTF_avg_des + 273.15, 1.0);   //[kg/m3]
    m_W_dot_CT_htf_pump_des = m_CT_htf_pump_coef_des * m_m_dot_CT_des * 1.E-3;        //[MWe]
    double CT_htf_deltaP = m_W_dot_CT_htf_pump_des * rho_CT_htf_des / m_m_dot_CT_des * eta_htf_pump;  //[MWe]

    // Apply min/max fractions
    m_m_dot_HT_min = m_cycle_cutoff_frac_des * m_m_dot_HT_des;    //[kg/s]
    m_m_dot_HT_max = m_cycle_max_frac_des * m_m_dot_HT_des;       //[kg/s]


    // Startup energy
    m_E_su_des = m_startup_frac_des*m_q_dot_hot_in_des;     //[MWt-hr]

    // Initialize state variables
    m_operating_mode_prev = OFF;        // Assume cycle is off when simulation begins
    m_startup_energy_remain_prev = m_E_su_des;          //[MWt-hr]
    m_startup_time_remain_prev = m_startup_time_des;    //[hr]
    if(m_startup_frac_des == 0.0 && m_startup_time_des == 0.0 && m_operating_mode_prev == OFF){
        m_operating_mode_prev = OFF_NO_SU_REQ;
    }

    // Set solved_params values
    solved_params.m_W_dot_des = m_W_dot_thermo_des;		//[MW] *thermo*, don't subtract electrical parasitics
    solved_params.m_eta_des = m_eta_therm_mech_des;		//[-] *thermo*, don't include electrical parasitics
    solved_params.m_q_dot_des = m_q_dot_hot_in_des;		//[MWt]
    solved_params.m_q_startup = m_E_su_des;	            //[MWt-hr]
    solved_params.m_max_frac = m_cycle_max_frac_des;		//[-]
    solved_params.m_cutoff_frac = m_cycle_cutoff_frac_des;	//[-]
    solved_params.m_sb_frac = m_q_sby_frac_des;				//[-]
    solved_params.m_T_htf_hot_ref = m_T_HT_HTF_hot_des;		//[C]
    solved_params.m_m_dot_design = m_m_dot_HT_des*3600.0;	//[kg/hr] *HT HTF*
    solved_params.m_m_dot_min = m_m_dot_HT_min*3600.;		//[kg/hr] *HT HTF*
    solved_params.m_m_dot_max = m_m_dot_HT_max*3600.;		//[kg/hr] *HT HTF*

    return;
}

C_csp_power_cycle::E_csp_power_cycle_modes C_pc_ptes::get_operating_state()
{
    throw(C_csp_exception("C_pc_tes::get_operating_state() is not complete"));
}

double C_pc_ptes::get_cold_startup_time()
{
    throw(C_csp_exception("C_pc_tes::get_cold_startup_time() is not complete"));
}

double C_pc_ptes::get_warm_startup_time()
{
    throw(C_csp_exception("C_pc_tes::get_warm_startup_time() is not complete"));
}

double C_pc_ptes::get_hot_startup_time()
{
    throw(C_csp_exception("C_pc_tes::get_hot_startup_time() is not complete"));
}

double C_pc_ptes::get_standby_energy_requirement()    //[MW]
{
    throw(C_csp_exception("C_pc_tes::get_standby_energy_requirement() is not complete"));
}

double C_pc_ptes::get_cold_startup_energy()    //[MWh]
{
    throw(C_csp_exception("C_pc_tes::get_cold_startup_energy() is not complete"));
}

double C_pc_ptes::get_warm_startup_energy()    //[MWh]
{
    throw(C_csp_exception("C_pc_tes::get_warm_startup_energy() is not complete"));
}

double C_pc_ptes::get_hot_startup_energy()    //[MWh]
{
    throw(C_csp_exception("C_pc_tes::get_hot_startup_energy() is not complete"));
}

double C_pc_ptes::get_max_thermal_power()     //MW
{
    throw(C_csp_exception("C_pc_tes::get_max_thermal_power() is not complete"));
}

double C_pc_ptes::get_min_thermal_power()     //MW
{
    throw(C_csp_exception("C_pc_tes::get_min_thermal_power() is not complete"));
}

void C_pc_ptes::get_max_power_output_operation_constraints(double T_amb /*C*/, double& m_dot_HTF_ND_max, double& W_dot_ND_max)	//[-] Normalized over design power
{
    throw(C_csp_exception("C_pc_tes::get_max_power_output_operation_constraints() is not complete"));
}

double C_pc_ptes::get_efficiency_at_TPH(double T_degC, double P_atm, double relhum_pct, double* w_dot_condenser)
{
    throw(C_csp_exception("C_pc_tes::get_efficiency_at_TPH() is not complete"));
}

double C_pc_ptes::get_efficiency_at_load(double load_frac, double* w_dot_condenser)
{
    throw(C_csp_exception("C_pc_tes::get_efficiency_at_load() is not complete"));
}

double C_pc_ptes::get_htf_pumping_parasitic_coef()		//[kWe/kWt]
{
    throw(C_csp_exception("C_pc_tes::get_htf_pumping_parasitic_coef() is not complete"));
}

// This can vary between timesteps for Type224, depending on remaining startup energy and time
double C_pc_ptes::get_max_q_pc_startup()		//[MWt]
{
    throw(C_csp_exception("C_pc_tes::get_max_q_pc_startup() is not complete"));
}

void C_pc_ptes::call(const C_csp_weatherreader::S_outputs& weather,
    C_csp_solver_htf_1state& htf_state_in,
    const C_csp_power_cycle::S_control_inputs& inputs,
    C_csp_power_cycle::S_csp_pc_out_solver& out_solver,
    //C_csp_power_cycle::S_csp_pc_out_report &out_report,
    const C_csp_solver_sim_info& sim_info)
{
    throw(C_csp_exception("C_pc_tes::call() is not complete"));
}

void C_pc_ptes::converged()
{
    throw(C_csp_exception("C_pc_tes::converged() is not complete"));
}

void C_pc_ptes::write_output_intervals(double report_time_start,
    const std::vector<double>& v_temp_ts_time_end, double report_time_end)
{
    throw(C_csp_exception("C_pc_tes::write_output_intervals() is not complete"));
}

void C_pc_ptes::assign(int index, double* p_reporting_ts_array, size_t n_reporting_ts_array)
{
    throw(C_csp_exception("C_pc_tes::assign() is not complete"));
}

void C_pc_ptes::get_design_parameters(double& W_dot_net /*MWe*/, double& q_dot_hot_in /*MWt*/,
    double& q_dot_cold_out_thermo /*MWt*/, double& W_dot_elec_parasitic /*MWe*/,
    double& eta_net /*-*/, double& q_dot_cold_to_CTES /*MWt*/, double& q_dot_cold_to_surr /*MWt*/,
    double& m_dot_HT_htf /*kg/s*/, double& cp_HT_htf /*kJ/kg-K*/, double& W_dot_HT_htf_pump /*MWe*/,
    double& m_dot_CT_htf /*kg/s*/, double& cp_CT_htf /*kJ/kg-K*/, double& W_dot_CT_htf_pump /*MWe*/)
{
    W_dot_net = m_W_dot_net_des;        //[MWe]
    q_dot_hot_in = m_q_dot_hot_in_des;  //[MWt]
    q_dot_cold_out_thermo = m_q_dot_cold_out_thermo_des;    //[MWt]
    W_dot_elec_parasitic = m_W_dot_elec_parasitic;          //[MWe]
    eta_net = m_eta_overall_des;        //[-]
    q_dot_cold_to_CTES = m_q_dot_cold_to_CTES;              //[MWt]
    q_dot_cold_to_surr = m_q_dot_cold_to_surroundings;      //[MWt]

    m_dot_HT_htf = m_m_dot_HT_des;      //[kg/s]
    cp_HT_htf = m_cp_HT_HTF_des;        //[kJ/kg-K]
    W_dot_HT_htf_pump = m_W_dot_HT_htf_pump_des;    //[MWe]
    m_dot_CT_htf = m_m_dot_CT_des;      //[kg/s]
    cp_CT_htf = m_cp_CT_HTF_des;        //[kJ/kg-K]
    W_dot_CT_htf_pump = m_W_dot_CT_htf_pump_des;    //[MWe]
}

void pc_ptes_helpers::design_calcs__all(double W_dot_thermo /*MWe*/, double f_elec_consume_vs_gen /*-*/,
    double eta_therm_mech /*-*/, double fixed__q_dot_cold__to__q_dot_warm /*-*/,
    double& W_dot_net /*MWe*/, double& W_dot_consume_elec /*MWe*/,
    double& q_dot_hot_in /*MWt*/, double& q_dot_cold_out_thermo /*MWt*/, double& eta_net /*-*/,
    double& q_dot_cold_to_CTES, double& q_dot_reject_to_surroundings /*MWt*/)
{
    design_calcs__no_ctes(W_dot_thermo, f_elec_consume_vs_gen,
        eta_therm_mech,
        W_dot_net, W_dot_consume_elec,
        q_dot_hot_in, q_dot_cold_out_thermo, eta_net);

    design_calcs__q_dot_ctes(q_dot_hot_in, fixed__q_dot_cold__to__q_dot_warm, q_dot_cold_out_thermo,
        q_dot_cold_to_CTES, q_dot_reject_to_surroundings);
}

void pc_ptes_helpers::design_calcs__no_ctes(double W_dot_thermo /*MWe*/, double f_elec_consume_vs_gen /*-*/,
    double eta_therm_mech /*-*/,
    double& W_dot_net /*MWe*/, double& W_dot_consume_elec /*MWe*/,
    double& q_dot_hot_in /*MWt*/, double& q_dot_cold_out_thermo /*MWt*/, double& eta_net /*-*/)
{
    W_dot_consume_elec = W_dot_thermo * f_elec_consume_vs_gen;  //[MWe]
    W_dot_net = W_dot_thermo - W_dot_consume_elec;      //[MWe]
    q_dot_hot_in = W_dot_thermo / eta_therm_mech;       //[MWt]
    q_dot_cold_out_thermo = W_dot_thermo * (1. / eta_therm_mech - 1.);   //[MWt]
    eta_net = W_dot_net / q_dot_hot_in;
}

void pc_ptes_helpers::design_calcs__q_dot_ctes(double q_dot_hot_in /*MWt*/, double fixed__q_dot_cold__to__q_dot_warm /*-*/,
    double q_dot_cold_out_thermo /*MWt*/,
    double& q_dot_cold_to_CTES, double& q_dot_reject_to_surroundings /*MWt*/)
{
    q_dot_cold_to_CTES = fixed__q_dot_cold__to__q_dot_warm * q_dot_hot_in;  //[MWt]
    q_dot_reject_to_surroundings = q_dot_cold_out_thermo - q_dot_cold_to_CTES;    //[MWt]
}
