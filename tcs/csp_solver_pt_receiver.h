/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as “System Advisor Model” or “SAM”. Except
*  to comply with the foregoing, the terms “System Advisor Model”, “SAM”, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

#ifndef __csp_solver_pt_receiver_
#define __csp_solver_pt_receiver_

#include "csp_solver_util.h"

#include "htf_props.h"
#include "csp_solver_core.h"

class C_pt_receiver
{
public:
    virtual ~C_pt_receiver() = 0;

    C_csp_messages csp_messages;        // Class to save messages for upstream classes

    //int m_n_panels;					    //[-] number of panels in receiver
    //double m_d_rec;					    //[m] diameter of receiver
    //double m_h_rec;					    //[m] height of just the receiver
    double m_h_tower;				    //[m] height of the tower
    //double m_od_tube;				    //[mm] outer diameter of each receiver tube, converted to [m] in init()
    //double m_th_tube;				    //[mm] wall thickness of receiver tubes, converted to [m] in init()
    //double m_epsilon;				    //[-] emissivity of the receiver panels
    //double m_hl_ffact;				    //[-] heat loss fudge factor for external forced convection on receiver
    double m_T_htf_hot_des;			    //[C] hot outlet HTF temperature at design, converted to [K] in init()
    double m_T_htf_cold_des;		    //[C] cold inlet HTF temperature at design, converted to [K] in init()
    double m_f_rec_min;				    //[-] minimum receiver thermal output as fraction of design
    double m_q_rec_des;				    //[MW] design recever thermal output, converted to [W] in init()
    double m_rec_su_delay;			    //[hr] required startup time
    double m_rec_qf_delay;			    //[-] required startup energy as fraction of design thermal output
    double m_m_dot_htf_max_frac;	    //[-] maximum receiver HTF mass flow as fraction of design mass flow

    //int m_n_flux_x;                     //[-] number of receiver flux nodes in the circumferential direction
    //int m_n_flux_y;                     //[-] number of receiver flux nodes in the vertical direction

    double m_q_dot_inc_min;             //[Wt] minimum receiver thermal power

    double m_eta_pump;					//[-] HTF pump efficiency
    int m_night_recirc;					//[-] 1=receiver is circulating HTF at night, otherwise not


    struct S_inputs
    {
        double m_field_eff;					                //[-] = (irradiance on receiver) / (I_bn * area of all heliostats)
        int m_input_operation_mode;			                //[-] operating mode of collector receiver, corresponding to enum C_csp_collector_receiver::E_csp_cr_modes
        const util::matrix_t<double> *m_flux_map_input;		//[-] flux values for each receiver surface node, as fraction of an evenly distributed irradiance

        S_inputs()
        {
            m_field_eff = std::numeric_limits<double>::quiet_NaN();
            m_input_operation_mode = -1;
        }
    };

    struct S_outputs
    {
        double m_m_dot_salt_tot;		//[kg/hr] HTF mass flow through receiver
        double m_eta_therm;				//[-] receiver thermal efficiency
        double m_W_dot_pump;			//[MW] HTF pumping power
        double m_q_conv_sum;			//[MW] total receiver convection losses
        double m_q_rad_sum;				//[MW] total receiver radiation losses
        double m_Q_thermal;				//[MW] thermal power delivered to TES/PC: subtracts piping losses (q_dot_rec - q_dot_piping_losses)
        double m_T_salt_hot;			//[C] HTF outlet temperature
        double m_field_eff_adj;			//[-] heliostat field efficiency including component defocus
        double m_component_defocus;		//[-] defocus applied by receiver to stay within mass flow or other constraints
        double m_q_dot_rec_inc;			//[MWt] receiver incident thermal power (after reflection losses)
        double m_q_startup;				//[MWt-hr] thermal energy used to start receiver
        double m_dP_receiver;			//[bar] receiver pressure drop
        double m_dP_total;				//[bar] total pressure drop
        double m_vel_htf;				//[m/s] HTF flow velocity through receiver tubes
        double m_T_salt_cold;			//[C] HTF inlet temperature
        double m_m_dot_ss;				//[kg/hr] HTF mass flow during steady-state operation (e.g., not equal to m_m_dot_salt_tot during startup)
        double m_q_dot_ss;				//[MW] thermal power delivered to TES/PC during steady-state operation (e.g., not equal to m_Q_thermal during startup)
        double m_f_timestep;			//[-] fraction of nominal timestep the receiver is not starting up
        double m_time_required_su;		//[s] time it took receiver to startup
        double m_q_dot_piping_loss;		//[MWt] thermal power lost from piping to surroundings 

        S_outputs()
        {
            m_m_dot_salt_tot = m_eta_therm = m_W_dot_pump = m_q_conv_sum = m_q_rad_sum = m_Q_thermal =
            m_T_salt_hot = m_field_eff_adj = m_component_defocus = m_q_dot_rec_inc = m_q_startup =
            m_dP_receiver = m_dP_total = m_vel_htf = m_T_salt_cold = m_m_dot_ss = m_q_dot_ss = m_f_timestep =
            m_time_required_su = m_q_dot_piping_loss = std::numeric_limits<double>::quiet_NaN();
        }
    };

    S_outputs ms_outputs;

    void clear_outputs();

    virtual void init() = 0;

    virtual void call(const C_csp_weatherreader::S_outputs &weather,
        const C_csp_solver_htf_1state &htf_state_in,
        const C_pt_receiver::S_inputs &inputs,
        const C_csp_solver_sim_info &sim_info) = 0;

    virtual void off(const C_csp_weatherreader::S_outputs &weather,
        const C_csp_solver_htf_1state &htf_state_in,
        const C_csp_solver_sim_info &sim_info) = 0;

    virtual void converged() = 0;

    virtual void calc_pump_performance(double rho_f, double mdot, double ffact, double &PresDrop_calc,
        double &WdotPump_calc) = 0;

    HTFProperties *get_htf_property_object();

protected:
    C_pt_receiver() {};

private:
    HTFProperties field_htfProps;       // heat transfer fluid properties
    HTFProperties tube_material;		// receiver tube material
    HTFProperties ambient_air;			// ambient air properties

    double m_m_dot_htf_des;             //[kg/s] receiver HTF mass flow at design

    std::string error_msg;              // member string for exception messages

};

void C_pt_receiver::clear_outputs()
{
    ms_outputs.m_m_dot_salt_tot =
        ms_outputs.m_eta_therm =
        ms_outputs.m_W_dot_pump =
        ms_outputs.m_q_conv_sum =
        ms_outputs.m_q_rad_sum =
        ms_outputs.m_Q_thermal =
        ms_outputs.m_T_salt_hot =
        ms_outputs.m_field_eff_adj =
        ms_outputs.m_component_defocus =
        ms_outputs.m_q_dot_rec_inc =
        ms_outputs.m_q_startup =
        ms_outputs.m_dP_receiver =
        ms_outputs.m_dP_total =
        ms_outputs.m_vel_htf =
        ms_outputs.m_T_salt_cold =
        ms_outputs.m_m_dot_ss =
        ms_outputs.m_q_dot_ss =
        ms_outputs.m_f_timestep = 
        ms_outputs.m_time_required_su =
        ms_outputs.m_q_dot_piping_loss = std::numeric_limits<double>::quiet_NaN();
}

HTFProperties *C_pt_receiver::get_htf_property_object()
{
    return &field_htfProps;
}

#endif  // __csp_solver_pt_receiver_