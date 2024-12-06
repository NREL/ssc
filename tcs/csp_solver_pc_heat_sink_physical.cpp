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

#include "csp_solver_pc_heat_sink_physical.h"
#include "csp_solver_core.h"

#include "htf_props.h"

static C_csp_reported_outputs::S_output_info S_output_info[]=
{
	{C_pc_heat_sink_physical::E_Q_DOT_HEAT_SINK, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_pc_heat_sink_physical::E_W_DOT_PUMPING, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_pc_heat_sink_physical::E_M_DOT_HTF, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_pc_heat_sink_physical::E_T_HTF_IN, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_pc_heat_sink_physical::E_T_HTF_OUT, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	
	csp_info_invalid
};

C_pc_heat_sink_physical::C_pc_heat_sink_physical()
{
	mc_reported_outputs.construct(S_output_info);

    m_max_frac = std::numeric_limits<double>::quiet_NaN();

	m_m_dot_htf_des = std::numeric_limits<double>::quiet_NaN();
}

void C_pc_heat_sink_physical::check_double_params_are_set()
{
	if( !check_double(ms_params.m_T_htf_cold_des) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling the C_pc_heat_sink init() method:", "m_W_dot_des"));
	}
	if( !check_double(ms_params.m_T_htf_hot_des) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling the C_pc_heat_sink init() method:", "m_W_dot_des"));
	}
	if( !check_double(ms_params.m_q_dot_des) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling the C_pc_heat_sink init() method:", "m_W_dot_des"));
	}
	if( !check_double(ms_params.m_htf_pump_coef) )
	{
		throw(C_csp_exception("The following parameter was not set prior to calling the C_pc_heat_sink init() method:", "m_W_dot_des"));
	}
}

void C_pc_heat_sink_physical::init(C_csp_power_cycle::S_solved_params &solved_params)
{
	check_double_params_are_set();

	// Declare instance of fluid class for FIELD fluid
	if( ms_params.m_pc_fl != HTFProperties::User_defined && ms_params.m_pc_fl < HTFProperties::End_Library_Fluids )
	{
		if( !mc_pc_htfProps.SetFluid(ms_params.m_pc_fl) )
		{
			throw(C_csp_exception("Power cycle HTF code is not recognized", "Rankine Indirect Power Cycle Initialization"));
		}
	}
	else if( ms_params.m_pc_fl == HTFProperties::User_defined )
	{
		// Check that 'm_field_fl_props' is allocated and correct dimensions
		int n_rows = (int)ms_params.m_pc_fl_props.nrows();
		int n_cols = (int)ms_params.m_pc_fl_props.ncols();
		if( n_rows > 2 && n_cols == 7 )
		{
			if( !mc_pc_htfProps.SetUserDefinedFluid(ms_params.m_pc_fl_props) )
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

    // Define Heat Exchanger 
    NS_HX_counterflow_eqs::E_UA_target_type target_type = NS_HX_counterflow_eqs::E_UA_target_type::E_constant_UA; // Required for two phase steam outlet
    this->m_hx.initialize(ms_params.m_pc_fl, ms_params.m_pc_fl_props, ms_params.m_N_sub_hx, target_type);

    // Get Steam Inlet Enthalpy (inputs are required to be subcooled)
    water_state water_props;
    int prop_error_code = water_TP(ms_params.m_T_ext_cold_des + 273.15, ms_params.m_P_ext_cold_des, &water_props);
    if (prop_error_code != 0)
    {
        throw(C_csp_exception("Inlet water properties failed",
            "Get Subcooled Enthalpy"));
    }
    m_h_ext_cold_des = water_props.enth;   //[kJ/kg] Inlet steam enthalpy

    // Get Steam Target Enthalpy (inputs are required to be in two phase)
    prop_error_code = water_PQ(ms_params.m_P_ext_cold_des, ms_params.m_Q_ext_hot_des, &water_props);
    if (prop_error_code != 0)
    {
        throw(C_csp_exception("Water properties in two phase region failed",
            "Get Target Enthalpy"));
    }
    m_h_ext_hot_des = water_props.enth;    //[kJ/kg] Target enthalpy

    // Design HX
    C_HX_counterflow_CRM::S_des_solved des_solved;
    this->m_hx.design_w_TP_PH(ms_params.m_T_htf_hot_des + 273.15, 1.0, ms_params.m_T_htf_cold_des + 273.15, 1.0,
        ms_params.m_P_ext_cold_des, m_h_ext_cold_des, ms_params.m_P_ext_hot_des, m_h_ext_hot_des,
        ms_params.m_q_dot_des * 1e3, des_solved);

    // Assign Design External mdot
    m_m_dot_ext_des = this->m_hx.ms_des_calc_UA_par.m_m_dot_cold_des;         //[kg/s]
    m_m_dot_ext_min = m_m_dot_ext_des * ms_params.m_f_m_dot_ext_min;    //[kg/s]
    m_m_dot_ext_max = m_m_dot_ext_des * ms_params.m_f_m_dot_ext_max;    //[kg/s]

	// Assign Design HTF mdot
    m_m_dot_htf_des = m_hx.ms_des_calc_UA_par.m_m_dot_hot_des;	//[kg/s]

	// Set 'solved_params' structure
	solved_params.m_W_dot_des = 0.0;		//[MWe] Assuming heat sink is not generating electricity FOR THIS MODEL
	solved_params.m_eta_des = 1.0;			//[-] Same
	solved_params.m_q_dot_des = ms_params.m_q_dot_des;	//[MWt]
	solved_params.m_q_startup = 0.0;		//[MWt-hr] Assuming heat sink does not require any startup energy
	
    this->m_max_frac = ms_params.m_max_frac;      //[-]
	solved_params.m_max_frac = m_max_frac;	//[-] For now (set in constructor), make this really large so heat sink can handle any collector-receiver output
	solved_params.m_max_frac = 1.0;			//[-]

	
	solved_params.m_cutoff_frac = 0.0;		//[-] Similarly, don't put a floor on the thermal power input
	solved_params.m_sb_frac = 0.0;			//[-] So, don't need standby
	solved_params.m_T_htf_hot_ref = ms_params.m_T_htf_hot_des;	//[C]
	solved_params.m_m_dot_design = m_m_dot_htf_des*3600.0;		//[kg/hr]
	solved_params.m_m_dot_min = solved_params.m_m_dot_design*solved_params.m_cutoff_frac;	//[kg/hr]
	solved_params.m_m_dot_max = solved_params.m_m_dot_design*solved_params.m_max_frac;		//[kg/hr]
}

C_csp_power_cycle::E_csp_power_cycle_modes C_pc_heat_sink_physical::get_operating_state()
{
	// Assume heat sink is always able to accept thermal power from solar field/TES
	return C_csp_power_cycle::ON;
}

double C_pc_heat_sink_physical::get_cold_startup_time()
{
	return 0.0;
}

double C_pc_heat_sink_physical::get_warm_startup_time()
{
	return 0.0;
}

double C_pc_heat_sink_physical::get_hot_startup_time()
{
	return 0.0;
}

double C_pc_heat_sink_physical::get_standby_energy_requirement()
{
	return 0.0;	//[MWt]
}

double C_pc_heat_sink_physical::get_cold_startup_energy()
{
	return 0.0;	//[MWh]
}

double C_pc_heat_sink_physical::get_warm_startup_energy()
{
	return 0.0;	//[MWh]
}

double C_pc_heat_sink_physical::get_hot_startup_energy()
{
	return 0.0;	//[MWh]
}

double C_pc_heat_sink_physical::get_max_thermal_power()
{
	return m_max_frac * ms_params.m_q_dot_des;	//[MWt]
}

double C_pc_heat_sink_physical::get_min_thermal_power()
{
	return 0.0;		//[MWt]
}

void C_pc_heat_sink_physical::get_max_power_output_operation_constraints(double T_amb /*C*/, double & m_dot_HTF_ND_max, double & W_dot_ND_max)
{
	m_dot_HTF_ND_max = m_max_frac;		//[-]
	W_dot_ND_max = m_dot_HTF_ND_max;	//[-]
	
	return ;	//[-]
}

double C_pc_heat_sink_physical::get_efficiency_at_TPH(double T_degC, double P_atm, double relhum_pct, double *w_dot_condenser)
{
    return 1.;
}

double C_pc_heat_sink_physical::get_efficiency_at_load(double load_frac, double *w_dot_condenser)
{
    return 1.;
}

double C_pc_heat_sink_physical::get_max_q_pc_startup()
{
	return 0.0;		//[MWt]
}

double C_pc_heat_sink_physical::get_htf_pumping_parasitic_coef()
{
	return ms_params.m_htf_pump_coef* (m_m_dot_htf_des) / (ms_params.m_q_dot_des*1000.0);	// kWe/kWt
}

void C_pc_heat_sink_physical::call(const C_csp_weatherreader::S_outputs &weather,
	C_csp_solver_htf_1state &htf_state_in,
	const C_csp_power_cycle::S_control_inputs &inputs,
	C_csp_power_cycle::S_csp_pc_out_solver &out_solver,
	const C_csp_solver_sim_info &sim_info)
{
    // Test SIMPLE heat sink
    if (false)
    {
        double T_htf_hot = htf_state_in.m_temp;		//[C]
        double m_dot_htf = inputs.m_m_dot / 3600.0;	//[kg/s]

        double cp_htf = mc_pc_htfProps.Cp_ave(ms_params.m_T_htf_cold_des + 273.15, T_htf_hot + 273.15);	//[kJ/kg-K]

        // For now, let's assume the Heat Sink can always return the HTF at the design cold temperature
        double q_dot_htf = m_dot_htf * cp_htf * (T_htf_hot - ms_params.m_T_htf_cold_des) / 1.E3;		//[MWt]

        out_solver.m_P_cycle = 0.0;		//[MWe] No electricity generation
        out_solver.m_T_htf_cold = ms_params.m_T_htf_cold_des;		//[C]
        out_solver.m_m_dot_htf = m_dot_htf * 3600.0;	//[kg/hr] Return inlet mass flow rate

        double W_dot_cooling_parasitic = 0.0;   //[MWe] No cooling load

        out_solver.m_time_required_su = 0.0;	//[s] No startup requirements, for now
        out_solver.m_q_dot_htf = q_dot_htf;		//[MWt] Thermal power form HTF

        double W_dot_htf_pump = ms_params.m_htf_pump_coef * m_dot_htf / 1.E3;   //[MWe]
        out_solver.m_W_dot_elec_parasitics_tot = W_dot_cooling_parasitic + W_dot_htf_pump;  //[MWe]

        out_solver.m_was_method_successful = true;

        mc_reported_outputs.value(E_Q_DOT_HEAT_SINK, q_dot_htf);	//[MWt]
        mc_reported_outputs.value(E_W_DOT_PUMPING, W_dot_htf_pump);	//[MWe]
        mc_reported_outputs.value(E_M_DOT_HTF, m_dot_htf);			//[kg/s]
        mc_reported_outputs.value(E_T_HTF_IN, T_htf_hot);			//[C]
        mc_reported_outputs.value(E_T_HTF_OUT, out_solver.m_T_htf_cold);	//[C]
        return;
    }





    // Process inputs
    double m_dot_htf = inputs.m_m_dot / 3600.0;	//[kg/s]
    int standby_control = inputs.m_standby_control;	//[-] 1: On, 2: Standby, 3: Off

    double q_dot, T_c_out, T_h_out_C, m_dot_c, tol_solved;

    // Handle no mass flow coming in
    if (inputs.m_m_dot < 1e-5 && standby_control == ON)
    {
        // Test SIMPLE heat sink
        if (true)
        {
            double T_htf_hot = htf_state_in.m_temp;		//[C]
            double m_dot_htf = inputs.m_m_dot / 3600.0;	//[kg/s]

            double cp_htf = mc_pc_htfProps.Cp_ave(ms_params.m_T_htf_cold_des + 273.15, T_htf_hot + 273.15);	//[kJ/kg-K]

            // For now, let's assume the Heat Sink can always return the HTF at the design cold temperature
            double q_dot_htf = m_dot_htf * cp_htf * (T_htf_hot - ms_params.m_T_htf_cold_des) / 1.E3;		//[MWt]

            out_solver.m_P_cycle = 0.0;		//[MWe] No electricity generation
            out_solver.m_T_htf_cold = ms_params.m_T_htf_cold_des;		//[C]
            out_solver.m_m_dot_htf = m_dot_htf * 3600.0;	//[kg/hr] Return inlet mass flow rate

            double W_dot_cooling_parasitic = 0.0;   //[MWe] No cooling load

            out_solver.m_time_required_su = 0.0;	//[s] No startup requirements, for now
            out_solver.m_q_dot_htf = q_dot_htf;		//[MWt] Thermal power form HTF

            double W_dot_htf_pump = ms_params.m_htf_pump_coef * m_dot_htf / 1.E3;   //[MWe]
            out_solver.m_W_dot_elec_parasitics_tot = W_dot_cooling_parasitic + W_dot_htf_pump;  //[MWe]

            out_solver.m_was_method_successful = true;

            mc_reported_outputs.value(E_Q_DOT_HEAT_SINK, q_dot_htf);	//[MWt]
            mc_reported_outputs.value(E_W_DOT_PUMPING, W_dot_htf_pump);	//[MWe]
            mc_reported_outputs.value(E_M_DOT_HTF, m_dot_htf);			//[kg/s]
            mc_reported_outputs.value(E_T_HTF_IN, T_htf_hot);			//[C]
            mc_reported_outputs.value(E_T_HTF_OUT, out_solver.m_T_htf_cold);	//[C]
            return;
        }

        out_solver.m_P_cycle = 0;		                    //[MWt] No steam generation
        out_solver.m_T_htf_cold = htf_state_in.m_temp;		//[C]
        out_solver.m_m_dot_htf = inputs.m_m_dot;	        //[kg/hr] Return inlet mass flow rate
        out_solver.m_time_required_su = 0;	                //[s] No startup requirements, for now
        out_solver.m_q_dot_htf = 0;		                    //[MWt] Thermal power from HTF
        out_solver.m_W_dot_elec_parasitics_tot = 0;         //[MWe]

        out_solver.m_was_method_successful = true;
        return;
    }

    switch (standby_control)
    {
        case STARTUP:
        case ON:
        case STANDBY:
        {
            try
            {
                // Get HTF inlet enthalpy
                double h_htf_hot = mc_pc_htfProps.enth(htf_state_in.m_temp + 273.15) * 1e-3;   //[kJ/kg]

                // Run Off design to find steam mdot to hit enthalpy target
                double h_ext_out_calc, h_htf_out_calc;
                int solve_code =  m_hx.off_design_target_cold_PH_out(m_h_ext_hot_des, m_m_dot_ext_min, m_m_dot_ext_max,
                    ms_params.m_P_ext_cold_des, m_h_ext_cold_des, ms_params.m_P_ext_hot_des,
                    1.0, h_htf_hot, 1.0, m_dot_htf, ms_params.m_od_tol,
                    q_dot, h_ext_out_calc, h_htf_out_calc, m_dot_c, tol_solved);

                if (solve_code == C_monotonic_eq_solver::CONVERGED)
                {
                    // Solved succesfully
                    
                    // Get HTF temperature from enthalpy
                    T_h_out_C = mc_pc_htfProps.temp(h_htf_out_calc*1e3); //[C]
                    
                    out_solver.m_P_cycle = 0.0;		            //[MWe] No electricity generation
                    out_solver.m_T_htf_cold = T_h_out_C;		//[C]
                    out_solver.m_m_dot_htf = m_dot_htf * 3600.0;//[kg/hr] Return inlet mass flow rate
                    out_solver.m_time_required_su = 0.0;	    //[s] No startup requirements, for now
                    out_solver.m_q_dot_htf = q_dot * 1e-3;		//[MWt] Thermal power form HTF

                    out_solver.m_was_method_successful = true;
                    break;
                }
                else
                {
                    // test why it failed
                    if (true)
                    {
                        std::vector<double> mdot_vec;
                        std::vector<double> h_vec;
                        int total_runs = 200;
                        for (int i = 0; i < total_runs; i++)
                        {
                            double frac = (double)i / (double)total_runs;
                            double mdot = m_m_dot_ext_min + (frac * (m_m_dot_ext_max - m_m_dot_ext_min));
                            try
                            {
                                m_hx.off_design_solution_fixed_dP_enth(m_h_ext_cold_des, ms_params.m_P_ext_cold_des, mdot, ms_params.m_P_ext_hot_des,
                                    h_htf_hot, 1.0, m_dot_htf, 1.0, ms_params.m_od_tol,
                                    q_dot, h_ext_out_calc, h_htf_out_calc);
                            }
                            catch (C_csp_exception exc)
                            {
                                h_ext_out_calc = 0;
                            }

                            h_vec.push_back(h_ext_out_calc);
                            mdot_vec.push_back(mdot);
                        }


                        int x = 0;
                    }



                    // Could not solve
                    q_dot = 0;
                    T_h_out_C = htf_state_in.m_temp;

                    out_solver.m_P_cycle = 0;		//[MWe] No electricity generation
                    out_solver.m_T_htf_cold = htf_state_in.m_temp;		//[C]
                    out_solver.m_m_dot_htf = inputs.m_m_dot;	//[kg/hr] Return inlet mass flow rate
                    out_solver.m_time_required_su = 0;	//[s] No startup requirements, for now
                    out_solver.m_q_dot_htf = 0;		//[MWt] Thermal power form HTF
                    out_solver.m_W_dot_elec_parasitics_tot = 0;  //[MWe]

                    out_solver.m_was_method_successful = true;
                    break;
                }   
            }
            catch (C_csp_exception exc)
            {
                double NaN = std::numeric_limits<double>::quiet_NaN();
                out_solver.m_P_cycle = NaN;		//[MWe] No electricity generation
                out_solver.m_T_htf_cold = NaN;		//[C]
                out_solver.m_m_dot_htf = NaN;	//[kg/hr] Return inlet mass flow rate
                out_solver.m_time_required_su = NaN;	//[s] No startup requirements, for now
                out_solver.m_q_dot_htf = NaN;		//[MWt] Thermal power form HTF
                out_solver.m_W_dot_elec_parasitics_tot = NaN;  //[MWe]

                out_solver.m_was_method_successful = false;
                return;
            }
            break;
        }
        case OFF:
        {
            // Test SIMPLE heat sink
            if (true)
            {
                double T_htf_hot = htf_state_in.m_temp;		//[C]
                double m_dot_htf = inputs.m_m_dot / 3600.0;	//[kg/s]

                double cp_htf = mc_pc_htfProps.Cp_ave(ms_params.m_T_htf_cold_des + 273.15, T_htf_hot + 273.15);	//[kJ/kg-K]

                // For now, let's assume the Heat Sink can always return the HTF at the design cold temperature
                double q_dot_htf = m_dot_htf * cp_htf * (T_htf_hot - ms_params.m_T_htf_cold_des) / 1.E3;		//[MWt]

                out_solver.m_P_cycle = 0.0;		//[MWe] No electricity generation
                out_solver.m_T_htf_cold = ms_params.m_T_htf_cold_des;		//[C]
                out_solver.m_m_dot_htf = m_dot_htf * 3600.0;	//[kg/hr] Return inlet mass flow rate

                double W_dot_cooling_parasitic = 0.0;   //[MWe] No cooling load

                out_solver.m_time_required_su = 0.0;	//[s] No startup requirements, for now
                out_solver.m_q_dot_htf = q_dot_htf;		//[MWt] Thermal power form HTF

                double W_dot_htf_pump = ms_params.m_htf_pump_coef * m_dot_htf / 1.E3;   //[MWe]
                out_solver.m_W_dot_elec_parasitics_tot = W_dot_cooling_parasitic + W_dot_htf_pump;  //[MWe]

                out_solver.m_was_method_successful = true;

                mc_reported_outputs.value(E_Q_DOT_HEAT_SINK, q_dot_htf);	//[MWt]
                mc_reported_outputs.value(E_W_DOT_PUMPING, W_dot_htf_pump);	//[MWe]
                mc_reported_outputs.value(E_M_DOT_HTF, m_dot_htf);			//[kg/s]
                mc_reported_outputs.value(E_T_HTF_IN, T_htf_hot);			//[C]
                mc_reported_outputs.value(E_T_HTF_OUT, out_solver.m_T_htf_cold);	//[C]
                return;
            }

            q_dot = 0;
            T_h_out_C = htf_state_in.m_temp;

            out_solver.m_P_cycle = 0;		//[MWe] No electricity generation
            out_solver.m_T_htf_cold = htf_state_in.m_temp;		//[C]
            out_solver.m_m_dot_htf = inputs.m_m_dot;	//[kg/hr] Return inlet mass flow rate
            out_solver.m_time_required_su = 0;	//[s] No startup requirements, for now
            out_solver.m_q_dot_htf = 0;		//[MWt] Thermal power form HTF
            out_solver.m_W_dot_elec_parasitics_tot = 0;  //[MWe]

            out_solver.m_was_method_successful = true;
            break;
        }
        default:
            int x = 0;
    }

    double W_dot_cooling_parasitic = 0.0;   //[MWe] No cooling load
    double W_dot_htf_pump = ms_params.m_htf_pump_coef * m_dot_htf / 1.E3;   //[MWe]
    out_solver.m_W_dot_elec_parasitics_tot = W_dot_cooling_parasitic + W_dot_htf_pump;  //[MWe]

	mc_reported_outputs.value(E_Q_DOT_HEAT_SINK, q_dot*1e-3);	//[MWt]
	mc_reported_outputs.value(E_W_DOT_PUMPING, W_dot_htf_pump);	//[MWe]
	mc_reported_outputs.value(E_M_DOT_HTF, m_dot_htf);			//[kg/s]
	mc_reported_outputs.value(E_T_HTF_IN, htf_state_in.m_temp);	//[C]
	mc_reported_outputs.value(E_T_HTF_OUT, T_h_out_C);	        //[C]

	return;
}

void C_pc_heat_sink_physical::converged()
{
	// Nothing, so far, in model is time dependent
	

	// But need to set final timestep outputs to reported outputs class
	mc_reported_outputs.set_timestep_outputs();

	return;
}

void C_pc_heat_sink_physical::write_output_intervals(double report_time_start,
	const std::vector<double> & v_temp_ts_time_end, double report_time_end)
{
	mc_reported_outputs.send_to_reporting_ts_array(report_time_start,
		v_temp_ts_time_end, report_time_end);
}

void C_pc_heat_sink_physical::assign(int index, double *p_reporting_ts_array, size_t n_reporting_ts_array)
{
	mc_reported_outputs.assign(index, p_reporting_ts_array, n_reporting_ts_array);

	return;
}
