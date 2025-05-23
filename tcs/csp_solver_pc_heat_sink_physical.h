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

#ifndef __csp_solver_pc_heat_sink_phsyical_
#define __csp_solver_pc_heat_sink_phsyical_

#include "csp_solver_core.h"
#include "csp_solver_util.h"
#include "heat_exchangers.h"

#include "lib_util.h"
#include "htf_props.h"

class C_pc_heat_sink_physical : public C_csp_power_cycle
{

public:

	enum
	{
		E_Q_DOT_HEAT_SINK,		//[MWt]
		E_W_DOT_PUMPING,		//[MWe]
		E_M_DOT_HTF,			//[kg/s]
		E_T_HTF_IN,				//[C]
		E_T_HTF_OUT,			//[C]

        E_M_DOT_EXT,            //[kg/s]
        E_X_OUT_EXT,            //[-]
        E_T_OUT_EXT,            //[C]
        E_HX_MIN_DT             //[C]
	};
	
	C_csp_reported_outputs mc_reported_outputs;    

private:

    C_HX_htf_to_steam m_hx;

	double m_max_frac;		//[-]
	double m_m_dot_htf_des;	//[kg/s]
    double m_m_dot_ext_des; //[kg/s] External fluid design mdot
    double m_m_dot_ext_min; //[kg/s] Min ext fluid mdot
    double m_m_dot_ext_max; //[kg/s] Max ext fluid mdot
    double m_h_ext_cold_des;    // [kJ/kg] Steam inlet enthalpy
    double m_h_ext_hot_des;     // [kJ/kg] Steam target outlet enthalpy
    double m_T_ext_hot_des; //[C]

    double m_hx_UA_des;     //[kW/K]

    bool m_did_init_pass;   //[-]

	HTFProperties mc_pc_htfProps;

    C_HX_counterflow_CRM::S_des_solved mc_hx_des_solved;

	void check_double_params_are_set();

public:

    double get_m_dot_ext_des() { return m_m_dot_ext_des; }
    double get_T_ext_out_des() { return m_T_ext_hot_des; }
    double get_hx_min_dT_des() { return mc_hx_des_solved.m_min_DT_design; }
    double get_hx_UA_des() { return m_hx_UA_des; }

	struct S_params
	{
        // Inputs
		double m_T_htf_cold_des;	//[C]
		double m_T_htf_hot_des;		//[C]
		double m_q_dot_des;			//[MWt]
		double m_htf_pump_coef;		//[kW/kg/s] Pumping power to move 1 kg/s of HTF through power cycle

        double m_max_frac;      //[-] max heat sink input (from TOD fracs)

		int m_pc_fl;				//[-] integer flag identifying Heat Transfer Fluid (HTF) in power block {1-27}
		util::matrix_t<double> m_pc_fl_props;

        double m_T_ext_cold_des;    //[C] External fluid inlet temperature (constant)
        double m_Q_ext_hot_des;     //[] External fluid target outlet quality
        //double m_T_ext_hot_des;     //[C] External fluid outlet temperature (hot target)
        double m_P_ext_cold_des;    //[kPa] External fluid inlet pressure (constant)
        double m_P_ext_hot_des;     //[kPa] External fluid outlet pressure
        double m_f_m_dot_ext_min;   //[kg/s] Minimum fraction external fluid mass flow rate of design
        double m_f_m_dot_ext_max;   //[kg/s] Maximum fraction external fluid mass flow rate of design

        int m_N_sub_hx;             //[] Number of HX nodes
        double m_od_tol;            //[] HX Off design tolerance

		S_params()
		{
			m_T_htf_cold_des = m_T_htf_hot_des = 
				m_q_dot_des = m_htf_pump_coef =
                m_max_frac = m_T_ext_cold_des = m_Q_ext_hot_des =
                m_P_ext_cold_des = m_P_ext_hot_des =
                m_f_m_dot_ext_min = m_f_m_dot_ext_max = m_od_tol =
                std::numeric_limits<double>::quiet_NaN();

            m_pc_fl = -1;
            m_N_sub_hx = -1;
		}
	};
	
	S_params ms_params;

	C_pc_heat_sink_physical();

	~C_pc_heat_sink_physical(){};

	virtual void init(C_csp_power_cycle::S_solved_params &solved_params);

	virtual C_csp_power_cycle::E_csp_power_cycle_modes get_operating_state();

	virtual double get_cold_startup_time();
	virtual double get_warm_startup_time();
	virtual double get_hot_startup_time();
	virtual double get_standby_energy_requirement();    //[MW]
	virtual double get_cold_startup_energy();    //[MWh]
	virtual double get_warm_startup_energy();    //[MWh]
	virtual double get_hot_startup_energy();    //[MWh]
	virtual double get_max_thermal_power();     //MW
	virtual double get_min_thermal_power();     //MW
	virtual void get_max_power_output_operation_constraints(double T_amb /*C*/, double & m_dot_HTF_ND_max, double & W_dot_ND_max);	//[-] Normalized over design power
	virtual double get_efficiency_at_TPH(double T_degC, double P_atm, double relhum_pct, double *w_dot_condenser=0);
	virtual double get_efficiency_at_load(double load_frac, double *w_dot_condenser=0);
	virtual double get_htf_pumping_parasitic_coef();		//[kWe/kWt]

	// This can vary between timesteps for Type224, depending on remaining startup energy and time
	virtual double get_max_q_pc_startup();		//[MWt]


	virtual void call(const C_csp_weatherreader::S_outputs &weather,
		C_csp_solver_htf_1state &htf_state_in,
		const C_csp_power_cycle::S_control_inputs &inputs,
		C_csp_power_cycle::S_csp_pc_out_solver &out_solver,
		//C_csp_power_cycle::S_csp_pc_out_report &out_report,
		const C_csp_solver_sim_info &sim_info);

	virtual void converged();

	virtual void write_output_intervals(double report_time_start,
		const std::vector<double> & v_temp_ts_time_end, double report_time_end);

	virtual void assign(int index, double *p_reporting_ts_array, size_t n_reporting_ts_array);
};


#endif // __csp_solver_pc_heat_sink_physical
