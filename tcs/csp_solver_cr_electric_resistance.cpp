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

#include "csp_solver_cr_electric_resistance.h"
#include "csp_solver_core.h"

#include "htf_props.h"

C_csp_cr_electric_resistance::C_csp_cr_electric_resistance(double T_htf_cold_des /*C*/, double T_htf_hot_des /*C*/, double q_dot_heater_des /*MWt*/,
    int htf_code /*-*/, util::matrix_t<double> ud_htf_props)
{
    // Pass arguements to member data
    m_T_htf_cold_des = T_htf_cold_des;      //[C]
    m_T_htf_hot_des = T_htf_hot_des;        //[C]
    m_q_dot_heater_des = q_dot_heater_des;  //[MWt]

    m_htf_code = htf_code;              //[-] htf fluid code
    m_ud_htf_props = ud_htf_props;      //[-] user defined fluid properties

    // Initialize calculated member data
    m_m_dot_htf_des = std::numeric_limits<double>::quiet_NaN();
    m_dP_htf = std::numeric_limits<double>::quiet_NaN();
}

C_csp_cr_electric_resistance::~C_csp_cr_electric_resistance(){}

// Inherited methods
void C_csp_cr_electric_resistance::init(const C_csp_collector_receiver::S_csp_cr_init_inputs init_inputs,
    C_csp_collector_receiver::S_csp_cr_solved_params& solved_params)
{
    // Not using init_inputs because the parameters are relevant to solar energy

    // Declare instance of fluid class for FIELD fluid
    if (m_htf_code != HTFProperties::User_defined && m_htf_code < HTFProperties::End_Library_Fluids)
    {
        if (!mc_pc_htfProps.SetFluid(m_htf_code))
        {
            throw(C_csp_exception("C_csp_cr_electric_resistance::init HTF code is not recognized"));
        }
    }
    else if (m_htf_code == HTFProperties::User_defined)
    {
        // Check that 'm_field_fl_props' is allocated and correct dimensions
        int n_rows = (int)m_ud_htf_props.nrows();
        int n_cols = (int)m_ud_htf_props.ncols();
        if (n_rows > 2 && n_cols == 7)
        {
            if (!mc_pc_htfProps.SetUserDefinedFluid(m_ud_htf_props))
            {
                std::string error_msg = util::format(mc_pc_htfProps.UserFluidErrMessage(), n_rows, n_cols);
                throw(C_csp_exception(error_msg, "Heat Sink Initialization"));
            }
        }
        else
        {
            std::string error_msg = util::format("The user defined field HTF table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", n_rows, n_cols);
            throw(C_csp_exception(error_msg, "Heat Sink Initialization"));
        }
    }
    else
    {
        throw(C_csp_exception("Power cycle HTF code is not recognized", "Heat Sink Initialization"));
    }

    // For now, set pressure drop (and then pumping power) through heater and piping = 0
    // The pumping power is converted to heat, so will capture in the total electricity consumption
    m_dP_htf = 0.0;

    // Calculate the design point HTF mass flow rate
    double cp_htf_des = mc_pc_htfProps.Cp_ave(m_T_htf_cold_des + 273.15, m_T_htf_hot_des + 273.15, 5);	//[kJ/kg-K]
    m_m_dot_htf_des = m_q_dot_heater_des*1.E3 / (cp_htf_des*(m_T_htf_hot_des - m_T_htf_cold_des));	//[kg/s]

    solved_params.m_T_htf_cold_des = m_T_htf_cold_des + 273.15; //[K]
    solved_params.m_P_cold_des = std::numeric_limits<double>::quiet_NaN();  //[kPa]
    solved_params.m_x_cold_des = std::numeric_limits<double>::quiet_NaN();  //[-]
    solved_params.m_T_htf_hot_des = m_T_htf_hot_des + 273.15;   //[K]
    solved_params.m_q_dot_rec_des = m_q_dot_heater_des;         //[MWt]
    solved_params.m_A_aper_total = 0.0;                         //[m2]
    solved_params.m_dP_sf = m_dP_htf;                           //[bar]

}

int C_csp_cr_electric_resistance::get_operating_state()
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::get_operating_state(...) is not complete"));
    return -1;
}

double C_csp_cr_electric_resistance::get_startup_time()
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::get_startup_time(...) is not complete"));
    return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_cr_electric_resistance::get_startup_energy() //MWh
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::get_startup_energy(...) is not complete"));
    return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_cr_electric_resistance::get_pumping_parasitic_coef()  //MWe/MWt
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::get_pumping_parasitic_coef(...) is not complete"));
    return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_cr_electric_resistance::get_min_power_delivery()    //MWt
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::get_min_power_delivery(...) is not complete"));
    return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_cr_electric_resistance::get_tracking_power()	//MWe
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::get_tracking_power(...) is not complete"));
    return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_cr_electric_resistance::get_col_startup_power()		//MWe-hr
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::get_col_startup_power(...) is not complete"));
    return std::numeric_limits<double>::quiet_NaN();
}

void C_csp_cr_electric_resistance::off(const C_csp_weatherreader::S_outputs& weather,
    const C_csp_solver_htf_1state& htf_state_in,
    C_csp_collector_receiver::S_csp_cr_out_solver& cr_out_solver,
    const C_csp_solver_sim_info& sim_info)
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::off(...) is not complete"));
}

void C_csp_cr_electric_resistance::startup(const C_csp_weatherreader::S_outputs& weather,
    const C_csp_solver_htf_1state& htf_state_in,
    C_csp_collector_receiver::S_csp_cr_out_solver& cr_out_solver,
    const C_csp_solver_sim_info& sim_info)
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::startup(...) is not complete"));
}

void C_csp_cr_electric_resistance::on(const C_csp_weatherreader::S_outputs& weather,
    const C_csp_solver_htf_1state& htf_state_in,
    double field_control,
    C_csp_collector_receiver::S_csp_cr_out_solver& cr_out_solver,
    const C_csp_solver_sim_info& sim_info)
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::on(...) is not complete"));
}

void C_csp_cr_electric_resistance::estimates(const C_csp_weatherreader::S_outputs& weather,
    const C_csp_solver_htf_1state& htf_state_in,
    C_csp_collector_receiver::S_csp_cr_est_out& est_out,
    const C_csp_solver_sim_info& sim_info)
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::estimates(...) is not complete"));
}

void C_csp_cr_electric_resistance::converged()
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::converged(...) is not complete"));
}

void C_csp_cr_electric_resistance::write_output_intervals(double report_time_start,
    const std::vector<double>& v_temp_ts_time_end, double report_time_end)
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::write_output_intervals(...) is not complete"));
}

double C_csp_cr_electric_resistance::calculate_optical_efficiency(const C_csp_weatherreader::S_outputs& weather, const C_csp_solver_sim_info& sim)
{
    // Optical efficiency is not a relevant metric for electric resistance heater
    return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_cr_electric_resistance::calculate_thermal_efficiency_approx(const C_csp_weatherreader::S_outputs& weather, double q_incident /*MW*/)
{
    throw(C_csp_exception("C_csp_cr_electric_resistance::calculate_thermal_efficiency_approx(...) is not complete"));
    return std::numeric_limits<double>::quiet_NaN();
}

double C_csp_cr_electric_resistance::get_collector_area()
{
    // Used by dispatch model
    return 0.0;
}
