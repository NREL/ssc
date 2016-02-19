#include "heat_exchangers.h"
#include "csp_solver_util.h"
#include "sam_csp_util.h"
#include <algorithm>
#include "numeric_solvers.h"

C_HX_counterflow::C_HX_counterflow()
{
	m_is_HX_initialized = false;
	m_is_HX_designed = false;
}

void C_HX_counterflow::initialize(const S_des_par & des_par_in)
{
	// Set member structure
	ms_des_par = des_par_in;

	// Set up HTFProperties for the hot fluid
	if( ms_des_par.m_hot_fl != CO2 )
	{
		if( ms_des_par.m_hot_fl != HTFProperties::User_defined && ms_des_par.m_hot_fl < HTFProperties::End_Library_Fluids )
		{
			if( !mc_hot_fl.SetFluid(ms_des_par.m_hot_fl, true) )
			{
				throw(C_csp_exception("Hot fluid code is not recognized", "Counter flow heat exchanger initialization"));
			}
		}
		else if( ms_des_par.m_hot_fl == HTFProperties::User_defined )
		{
			int n_rows = ms_des_par.mc_hot_fl_props.nrows();
			int n_cols = ms_des_par.mc_hot_fl_props.ncols();
			if( n_rows > 2 && n_cols == 7 )
			{
				if( !mc_hot_fl.SetUserDefinedFluid(ms_des_par.mc_hot_fl_props, true) )
				{
					std::string error_msg = util::format(mc_hot_fl.UserFluidErrMessage(), n_rows, n_cols);
					throw(C_csp_exception(error_msg, "Counter flow heat exchanger initialization"));
				}
			}
			else
			{
				std::string error_msg = util::format("The user defined hot fluid table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", n_rows, n_cols);
				throw(C_csp_exception(error_msg, "Counter flow heat exchanger initialization"));
			}
		}
		else
		{
			throw(C_csp_exception("Hot fluid code is not recognized", "Counter flow heat exchanger initialization"));
		}
	}

	// Set up HTFProperties for the cold fluid
	if( ms_des_par.m_cold_fl != CO2 )
	{
		if( ms_des_par.m_cold_fl != HTFProperties::User_defined && ms_des_par.m_cold_fl < HTFProperties::End_Library_Fluids )
		{
			if( !mc_cold_fl.SetFluid(ms_des_par.m_cold_fl, true) )
			{
				throw(C_csp_exception("Cold fluid code is not recognized", "Counter flow heat exchanger initialization"));
			}
		}
		else if( ms_des_par.m_cold_fl == HTFProperties::User_defined )
		{
			int n_rows = ms_des_par.mc_cold_fl_props.nrows();
			int n_cols = ms_des_par.mc_hot_fl_props.ncols();
			if( n_rows > 2 && n_cols == 7 )
			{
				if( !mc_cold_fl.SetUserDefinedFluid(ms_des_par.mc_cold_fl_props, true) )
				{
					std::string error_msg = util::format(mc_cold_fl.UserFluidErrMessage(), n_rows, n_cols);
					throw(C_csp_exception(error_msg, "Counter flow heat exchanger initialization"));
				}
			}
			else
			{
				std::string error_msg = util::format("The user defined cold fluid table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", n_rows, n_cols);
				throw(C_csp_exception(error_msg, "Counter flow heat exchanger initialization"));
			}
		}
		else
		{
			throw(C_csp_exception("Cold fluid code is not recognized", "Counter flow heat exchanger initialization"));
		}
	}

	// Class is initialized
	m_is_HX_initialized = true;

	return;
}

void C_HX_counterflow::calc_req_UA(double q_dot /*kWt*/, double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/,
	double T_c_in /*K*/, double T_h_in /*K*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/,
	double & UA /*kW/K*/, double & min_DT /*C*/, double & eff /*-*/, double & T_h_out /*K*/, double & T_c_out /*K*/, double & q_dot_calc /*kWt*/)
{
	// Check inputs
	if( q_dot < 0.0 )
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"Input heat transfer rate is less than 0.0. It must be >= 0.0", 4));
	}
	if( m_dot_c < 1.E-14 )
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"The cold mass flow rate must be a positive value"));
	}
	if( m_dot_h < 1.E-14 )
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"The hot mass flow rate must be a positive value"));
	}
	if( T_h_in < T_c_in )
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"Inlet hot temperature is colder than the cold inlet temperature", 5));
	}
	if( P_h_in < P_h_out )
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"Hot side outlet pressure is greater than hot side inlet pressure", 6));
	}
	if( P_c_in < P_c_out )
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"Cold side outlet pressure is greater than cold side inlet pressure", 7));
	}
	if( q_dot <= 1.E-14 )	// very low Q_dot; assume it is zero
	{
		// Set outputs, and S_des_solved members		
		UA = 0.0;						//[kW/K]
		q_dot_calc = 0.0;				//[kW]
		min_DT = T_h_in - T_c_in;		//[K]
		eff = 0.0;							//[-]
		T_h_out = T_h_in;	//[K]
		T_c_out = T_c_in;	//[K]

		return;
	}

	// Calculate inlet enthalpies from known state points
	double h_c_in = std::numeric_limits<double>::quiet_NaN();
	double h_h_in = std::numeric_limits<double>::quiet_NaN();
	double h_c_out = std::numeric_limits<double>::quiet_NaN();
	double h_h_out = std::numeric_limits<double>::quiet_NaN();
	int prop_error_code = 0;

	if( ms_des_par.m_cold_fl == CO2 )
	{
		prop_error_code = CO2_TP(T_c_in, P_c_in, &mc_co2_props);
		if( prop_error_code != 0 )
		{
			throw(C_csp_exception("C_HX_counterflow::design",
				"Cold side inlet enthalpy calculations failed", 8));
		}
		h_c_in = mc_co2_props.enth;		//[kJ/kg]
		h_c_out = h_c_in + q_dot / m_dot_c;	//[kJ/kg]
		prop_error_code = CO2_PH(P_c_out, h_c_out, &mc_co2_props);
		if( prop_error_code != 0 )
		{
			throw(C_csp_exception("C_HX_counterflow::design",
				"Cold side outlet temperature calculations failed", 8));
		}
		T_c_out = mc_co2_props.temp;	//[K]
	}
	else
	{
		h_c_in = mc_cold_fl.enth_lookup(T_c_in);		//[kJ/kg]
		h_c_out = h_c_in + q_dot / m_dot_c;				//[kJ/kg]
		T_c_out = mc_cold_fl.temp_lookup(h_c_out);		//[K]
	}

	if( ms_des_par.m_hot_fl == CO2 )
	{
		prop_error_code = CO2_TP(T_h_in, P_h_in, &mc_co2_props);
		if( prop_error_code != 0 )
		{
			throw(C_csp_exception("C_HX_counterflow::design",
				"Hot side inlet enthalpy calculations failed", 9));
		}
		h_h_in = mc_co2_props.enth;			//[kJ/kg]
		h_h_out = h_h_in - q_dot / m_dot_h;	//[kJ/kg]
		prop_error_code = CO2_PH(P_h_out, h_h_out, &mc_co2_props);
		if( prop_error_code != 0 )
		{
			throw(C_csp_exception("C_HX_counterflow::design",
				"Hot side outlet temperature calculations failed", 9));
		}
		T_h_out = mc_co2_props.temp;		//[K]
	}
	else
	{
		h_h_in = mc_hot_fl.enth_lookup(T_h_in);	//[kJ/kg]
		h_h_out = h_h_in - q_dot / m_dot_h;			//[kJ/kg]
		T_h_out = mc_hot_fl.temp_lookup(h_h_out);	//[K]
	}

	int N_nodes = ms_des_par.m_N_sub_hx + 1;
	double h_h_prev = 0.0;
	double T_h_prev = 0.0;
	double h_c_prev = 0.0;
	double T_c_prev = 0.0;
	UA = 0.0;
	min_DT = T_h_in;
	// Loop through the sub-heat exchangers
	for( int i = 0; i < N_nodes; i++ )
	{
		// Assume pressure varies linearly through heat exchanger
		double P_c = P_c_out + i*(P_c_in - P_c_out) / (N_nodes - 1);
		double P_h = P_h_in - i*(P_h_in - P_h_out) / (N_nodes - 1);

		// Calculate the entahlpy at the node
		double h_c = h_c_out + i*(h_c_in - h_c_out) / (N_nodes - 1);
		double h_h = h_h_in - i*(h_h_in - h_h_out) / (N_nodes - 1);

		// ****************************************************
		// Calculate the hot and cold temperatures at the node
		double T_h = std::numeric_limits<double>::quiet_NaN();
		if( ms_des_par.m_hot_fl == CO2 )
		{
			prop_error_code = CO2_PH(P_h, h_h, &mc_co2_props);
			if( prop_error_code != 0 )
			{
				throw(C_csp_exception("C_HX_counterflow::design",
					"Cold side inlet enthalpy calculations failed", 12));
			}
			T_h = mc_co2_props.temp;		//[K]
		}
		else
		{
			T_h = mc_hot_fl.temp_lookup(h_h);	//[K]
		}

		double T_c = std::numeric_limits<double>::quiet_NaN();
		if( ms_des_par.m_cold_fl == CO2 )
		{
			prop_error_code = CO2_PH(P_c, h_c, &mc_co2_props);
			if( prop_error_code != 0 )
			{
				throw(C_csp_exception("C_HX_counterflow::design",
					"Cold side inlet enthalpy calculations failed", 13));
			}
			T_c = mc_co2_props.temp;		//[K]
		}
		else
		{
			T_c = mc_cold_fl.temp_lookup(h_c);	//[K]
		}

		// ****************************************************
		// ****************************************************
		// ****************************************************

		// Check that 2nd law is not violated
		if( T_c >= T_h )
		{
			throw(C_csp_exception("C_HX_counterflow::design",
				"Cold temperature is hotter than hot temperature.", 11));
		}

		// Track the minimum temperature difference in the heat exchanger
		min_DT = fmin(min_DT, T_h - T_c);

		// Perform effectiveness-NTU and UA calculations 
		if( i > 0 )
		{
			double C_dot_h = m_dot_h*(h_h_prev - h_h) / (T_h_prev - T_h);			// [kW/K] hot stream capacitance rate
			double C_dot_c = m_dot_c*(h_c_prev - h_c) / (T_c_prev - T_c);			// [kW/K] cold stream capacitance rate
			double C_dot_min = fmin(C_dot_h, C_dot_c);				// [kW/K] Minimum capacitance stream
			double C_dot_max = fmax(C_dot_h, C_dot_c);				// [kW/K] Maximum capacitance stream
			double C_R = C_dot_min / C_dot_max;						// [-] Capacitance ratio of sub-heat exchanger
			double eff = (q_dot / (double)ms_des_par.m_N_sub_hx) / (C_dot_min*(T_h_prev - T_c));	// [-] Effectiveness of each sub-heat exchanger
			double NTU = 0.0;
			if( C_R != 1.0 )
				NTU = log((1.0 - eff*C_R) / (1.0 - eff)) / (1.0 - C_R);		// [-] NTU if C_R does not equal 1
			else
				NTU = eff / (1.0 - eff);
			UA += NTU*C_dot_min;								//[kW/K] Sum UAs for each hx section
		}
		h_h_prev = h_h;
		T_h_prev = T_h;
		h_c_prev = h_c;
		T_c_prev = T_c;

	}

	// Check for NaNs in UA
	if( UA != UA )
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"NaN found for total heat exchanger UA", 14));
	}

	q_dot_calc = q_dot;

	// **************************************************************
	// Calculate the HX effectiveness
	double q_dot_max = calc_max_q_dot(T_h_in, P_h_in, P_h_out, m_dot_h,
			T_c_in, P_c_in, P_c_out, m_dot_c);

	eff = q_dot / q_dot_max;

	return;
}

double C_HX_counterflow::calc_max_q_dot(double T_h_in, double P_h_in, double P_h_out, double m_dot_h,
	double T_c_in, double P_c_in, double P_c_out, double m_dot_c)
{
	double Q_dot_cold_max = std::numeric_limits<double>::quiet_NaN();
	int prop_error_code = 0;
	if( ms_des_par.m_cold_fl == CO2 )
	{
		prop_error_code = CO2_TP(T_c_in, P_c_in, &mc_co2_props);
		if( prop_error_code != 0 )
		{
			throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot",
				"Cold side inlet enthalpy calculations at effectiveness calc failed",12));
		}
		double h_c_in = mc_co2_props.enth;

		prop_error_code = CO2_TP(T_h_in, P_c_out, &mc_co2_props);
		if( prop_error_code != 0 )
		{
			throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot",
				"Cold side inlet enthalpy calculations at effectiveness calc failed", 12));
		}
		Q_dot_cold_max = m_dot_c*(mc_co2_props.enth - h_c_in);	//[kWt]
	}
	else
	{
		double h_c_in = mc_cold_fl.enth_lookup(T_c_in);
		double h_c_out_max = mc_cold_fl.enth_lookup(T_h_in);		//[kJ/kg]
		Q_dot_cold_max = m_dot_c*(h_c_out_max - h_c_in);	//[kWt]
	}

	double Q_dot_hot_max = std::numeric_limits<double>::quiet_NaN();
	if( ms_des_par.m_hot_fl == CO2 )
	{
		prop_error_code = CO2_TP(T_h_in, P_h_in, &mc_co2_props);
		if( prop_error_code != 0 )
		{
			throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot",
				"Hot side inlet enthalpy calculations at effectiveness calc failed", 12));
		}
		double h_h_in = mc_co2_props.enth;

		prop_error_code = CO2_TP(T_c_in, P_h_out, &mc_co2_props);
		if( prop_error_code != 0 )
		{
			throw(C_csp_exception("C_HX_counterflow::calc_max_q_dot",
				"Hot side inlet enthalpy calculations at effectiveness calc failed", 12));
		}
		Q_dot_hot_max = m_dot_h*(h_h_in - mc_co2_props.enth);	//[kWt]
	}
	else
	{
		double h_h_in = mc_hot_fl.enth_lookup(T_h_in);		//[kJ/kg]
		double h_h_out_min = mc_hot_fl.enth_lookup(T_c_in);	//[kJ/kg]
		Q_dot_hot_max = m_dot_h*(h_h_in - h_h_out_min);		//[kWt]
	}

	return std::min(Q_dot_hot_max, Q_dot_cold_max);
}

void C_HX_counterflow::design(double Q_dot /*kWt*/, double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/,
	double T_c_in /*K*/, double T_h_in /*K*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/,
	double & UA /*kW/K*/, double & min_DT /*C*/)
{
	/*Designs heat exchanger given its mass flow rates, inlet temperatures, and a heat transfer rate.
	Note: the heat transfer rate must be positive.*/

	// Set 'S_des_solved' members that are known
	ms_des_solved.m_T_h_in = T_h_in;			//[K]
	ms_des_solved.m_T_c_in = T_c_in;			//[K]
	ms_des_solved.m_m_dot_cold_des = m_dot_c;	//[kg/s]
	ms_des_solved.m_m_dot_hot_des = m_dot_h;	//[kg/s]
	ms_des_solved.m_DP_cold_des = P_c_in - P_c_out;	//[kPa]
	ms_des_solved.m_DP_hot_des = P_h_in - P_h_out;	//[kPa]

	// Trying to solve design point, so set boolean to false until method solves successfully
	m_is_HX_designed = false;

	// Check that design parameters are set
	if( !m_is_HX_initialized )
	{
		throw(C_csp_exception("C_HX_counterflow::design",
			"Design parameters are not initialized!"));
	}

	double UA_calc, min_DT_calc, eff_calc, T_h_out_calc, T_c_out_calc, q_dot_calc;
	UA_calc = min_DT_calc = eff_calc = T_h_out_calc = T_c_out_calc = q_dot_calc = std::numeric_limits<double>::quiet_NaN();
	
	calc_req_UA(Q_dot, m_dot_c, m_dot_h, T_c_in, T_h_in, P_c_in, P_c_out, P_h_in, P_h_out,
		UA_calc, min_DT_calc, eff_calc, T_h_out_calc, T_c_out_calc, q_dot_calc);

	ms_des_solved.m_UA_design_total = UA = UA_calc;
	ms_des_solved.m_min_DT_design = min_DT = min_DT_calc;
	ms_des_solved.m_eff_design = eff_calc;
	ms_des_solved.m_T_h_out = T_h_out_calc;
	ms_des_solved.m_T_c_out = T_c_out_calc;
	ms_des_solved.m_Q_dot_design = q_dot_calc;

	// Specify that method solved successfully
	m_is_HX_designed = true;

	return;
}

int C_HX_counterflow::C_mono_eq_UA_v_q::operator()(double q_dot /*kWt*/, double *UA_calc /*kW/K*/)
{
	// Using ms_od_par!!! Must be defined upstream!
	S_od_par *ms_od_par = &(mp_c_hx->ms_od_par);

	// Calculate off-design UA and pressure drop here

	// Storing results in ms_od_solved!
	// If catch error, then need to set to NaN
	S_od_solved *ms_od_solved = &(mp_c_hx->ms_od_solved);

	try
	{
	mp_c_hx->calc_req_UA(q_dot, ms_od_par->m_m_dot_c, ms_od_par->m_m_dot_h, ms_od_par->m_T_c_in, ms_od_par->m_T_h_in,
		ms_od_par->m_P_c_in, ms_od_solved->m_P_c_out, ms_od_par->m_P_h_in, ms_od_solved->m_P_h_out, ms_od_solved->m_UA_total, 
		ms_od_solved->m_min_DT, ms_od_solved->m_eff,
		ms_od_solved->m_T_h_out, ms_od_solved->m_T_c_out, ms_od_solved->m_q_dot);
	}
	catch( C_csp_exception )
	{
		// Reset solved OD parameters to NaN
		ms_od_solved->m_q_dot = ms_od_solved->m_T_c_out =
			ms_od_solved->m_T_h_out = ms_od_solved->m_UA_total =
			ms_od_solved->m_min_DT = ms_od_solved->m_eff = std::numeric_limits<double>::quiet_NaN();

		// reset 'UA_calc' to NaN
		*UA_calc = ms_od_solved->m_UA_total;		//[kW/K]

		return -1;
	}

	*UA_calc = ms_od_solved->m_UA_total;		//[kW/K]

	return 0;
}

void C_HX_counterflow::od_performance(double T_c_in /*K*/, double P_c_in /*kPa*/, double m_dot_c /*kg/s*/, 
	double T_h_in /*K*/, double P_h_in /*kPa*/, double m_dot_h /*kg/s*/,
	double & q_dot /*kWt*/, double & T_c_out /*K*/, double & T_h_out /*K*/)
{
	ms_od_par.m_T_c_in = T_c_in;		//[K]
	ms_od_par.m_P_c_in = P_c_in;		//[kPa]
	ms_od_par.m_m_dot_c = m_dot_c;		//[kg/s]
	ms_od_par.m_T_h_in = T_h_in;		//[K]
	ms_od_par.m_P_h_in = P_h_in;		//[kPa]
	ms_od_par.m_m_dot_h = m_dot_h;		//[kg/s]

	// Calculate off-design UA and DP

	// Set o.d. DP in ms_od_solver
	ms_od_solved.m_P_c_out = ms_od_par.m_P_c_in - ms_des_solved.m_DP_cold_des*od_delta_p_cold_frac(ms_od_par.m_m_dot_c);
	ms_od_solved.m_P_h_out = ms_od_par.m_P_h_in - ms_des_solved.m_DP_hot_des*od_delta_p_hot_frac(ms_od_par.m_m_dot_h);

	// Set o.d. UA as solver target
	double UA_target = ms_des_solved.m_UA_design_total*od_UA_frac(m_dot_c, m_dot_h);	//[kW/K]

	// Calculate maximum possible heat transfer, use to set upper bound
	double q_dot_upper = calc_max_q_dot(ms_od_par.m_T_h_in, ms_od_par.m_P_h_in, ms_od_solved.m_P_h_out, ms_od_par.m_m_dot_h,
							ms_od_par.m_T_c_in, ms_od_par.m_P_c_in, ms_od_solved.m_P_c_out, ms_od_par.m_m_dot_c);

	// Use design point effectiveness to generate 2 guess values
	double q_dot_guess_lower = ms_des_solved.m_eff_design*q_dot_upper;
	double q_dot_guess_upper = 0.85*q_dot_guess_lower;		
	
	// Complete solver settings
	double tol = 0.001;
	double q_dot_lower = 1.E-10;		//[kWt]

	C_mono_eq_UA_v_q od_hx_eq(this);
	C_monotonic_eq_solver od_hx_solver(od_hx_eq);

	// Set solver settings
	od_hx_solver.settings(tol, 100, q_dot_lower, q_dot_upper, true);

	// Solve
	bool is_converged, is_real_error;
	double x_solved, tol_solved;
	x_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_solved = -1;
	
	int od_hx_code = od_hx_solver.solve(q_dot_guess_lower, q_dot_guess_upper, UA_target,
		is_converged, is_real_error, x_solved, tol_solved, iter_solved);

	if( !(od_hx_code == C_monotonic_eq_solver::CONVERGED || od_hx_code == C_monotonic_eq_solver::SLOPE_POS_NO_POS_ERR) )
	{
		throw(C_csp_exception("Off-design heat exchanger method failed"));
	}

	q_dot = ms_od_solved.m_q_dot;
	T_c_out = ms_od_solved.m_T_c_out;
	T_h_out = ms_od_solved.m_T_h_out;

	return;
}

double C_HX_counterflow::od_delta_p_cold_frac(double m_dot_c /*kg/s*/)
{
	return pow(m_dot_c/ms_des_solved.m_m_dot_cold_des, 1.75);
}

double C_HX_counterflow::od_delta_p_hot_frac(double m_dot_h /*kg/s*/)
{
	return pow(m_dot_h/ms_des_solved.m_m_dot_hot_des, 1.75);
}

double C_HX_counterflow::od_UA_frac(double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/)
{
	double m_dot_ratio = 0.5*(m_dot_c/ms_des_solved.m_m_dot_cold_des + m_dot_h/ms_des_solved.m_m_dot_hot_des);
	return pow(m_dot_ratio, 0.8);
}

void C_HX_co2_to_htf::initialize(int hot_fl, util::matrix_t<double> hot_fl_props)
{
	// Hard-code some of the design parameters
	ms_des_par.m_N_sub_hx = 5;
	ms_des_par.m_cold_fl = CO2;

	// Read-in hot side HTF props
	ms_des_par.m_hot_fl = hot_fl;
	ms_des_par.mc_hot_fl_props = hot_fl_props;

	// Set up HTFProperties for the hot fluid
	if( ms_des_par.m_hot_fl != HTFProperties::User_defined && ms_des_par.m_hot_fl < HTFProperties::End_Library_Fluids )
	{
		if( !mc_hot_fl.SetFluid(ms_des_par.m_hot_fl, true) )
		{
			throw(C_csp_exception("Hot fluid code is not recognized", "C_HX_co2_to_htf::initialization"));
		}
	}
	else if( ms_des_par.m_hot_fl == HTFProperties::User_defined )
	{
		int n_rows = ms_des_par.mc_hot_fl_props.nrows();
		int n_cols = ms_des_par.mc_hot_fl_props.ncols();
		if( n_rows > 2 && n_cols == 7 )
		{
			if( !mc_hot_fl.SetUserDefinedFluid(ms_des_par.mc_hot_fl_props, true) )
			{
				std::string error_msg = util::format(mc_hot_fl.UserFluidErrMessage(), n_rows, n_cols);
				throw(C_csp_exception(error_msg, "C_HX_co2_to_htf::initialization"));
			}
		}
		else
		{
			std::string error_msg = util::format("The user defined hot fluid table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", n_rows, n_cols);
			throw(C_csp_exception(error_msg, "C_HX_co2_to_htf::initialization"));
		}
	}
	else
	{
		throw(C_csp_exception("Hot fluid code is not recognized", "C_HX_co2_to_htf::initialization"));
	}

	// Class is initialized
	m_is_HX_initialized = true;

}

void C_HX_co2_to_htf::initialize(int hot_fl)
{
	util::matrix_t<double> null_fluid_props;

	initialize(hot_fl, null_fluid_props);
}

bool N_compact_hx::get_compact_hx_geom(int enum_compact_hx_config, double & d_out, double & fin_pitch, double & D_h,
	double & fin_thk, double & sigma, double & alpha, double & A_fin_to_surf,
	double & s_h, double & s_v, double & fin_V_per_m)
{

	switch( enum_compact_hx_config )
	{
	case fc_tubes_s80_38T:
		d_out = 0.0102;		//[m] Outer tube diameter
		fin_pitch = 315;	//[1/m] Number of fins per meter of tube
		D_h = 0.003632;		//[m] Hydraulic diameter of air side
		fin_thk = 0.0003302;//[m] Fin thickness
		sigma = 0.534;		//[-] Ratio of free-flow to frontal area
		alpha = 587;		//[1/m] Ratio of gas-side heat transfer area to core volume
		A_fin_to_surf = 0.913;	//[-] Ratio of finned to total surface area on gas-side
		s_h = 0.022;		//[m] Distance between tubes in air flow direction
		s_v = 0.0254;		//[m] Distance between tubes perpendicular to air flow direction
		fin_V_per_m = (s_h*s_v - 0.25*CSP::pi*pow(d_out, 2))*fin_thk*fin_pitch;

		return true;

	case fc_tubes_sCF_88_10Jb:
		d_out = 0.02601;		//[m] Outer tube diameter
		fin_pitch = 346;	//[1/m] Number of fins per meter of tube
		D_h = 0.01321;		//[m] Hydraulic diameter of air side
		fin_thk = 0.000305;//[m] Fin thickness
		sigma = 0.642;		//[-] Ratio of free-flow to frontal area
		alpha = 191;		//[1/m] Ratio of gas-side heat transfer area to core volume
		A_fin_to_surf = 0.825;	//[-] Ratio of finned to total surface area on gas-side
		s_h = 0.0524;		//[m] Distance between tubes in air flow direction
		s_v = 0.07818;		//[m] Distance between tubes perpendicular to air flow direction
		fin_V_per_m = 0.25*CSP::pi*(pow(0.04412, 2) - pow(d_out, 2))*fin_thk*fin_pitch;

		return true;

	default:
		return false;
	}

};

bool N_compact_hx::get_compact_hx_f_j(int enum_compact_hx_config, double Re, double & f, double & j_H)
{
	double Re_mm = fmax(0.001, Re*1e-3);

	switch( enum_compact_hx_config )
	{
	case fc_tubes_s80_38T:
		f = 0.02949346*pow(Re_mm, -0.208110211);
		j_H = 0.0105331507*pow(Re_mm, -0.400092073);
		return true;

	case fc_tubes_sCF_88_10Jb:
		f = 0.0606753986*pow(Re_mm, -0.256298233);
		j_H = 0.0148711552*pow(Re_mm, -0.382144871);
		return true;

	default:
		return false;
	}

};

C_CO2_to_air_cooler::C_CO2_to_air_cooler()
{
	m_th = m_eta_fan = m_roughness =
		m_d_in = m_A_cs = m_relRough = m_Depth = m_W_par = m_N_par = m_N_tubes = m_L_tube = m_L_path = m_A_surf_total = m_UA_total = m_V_total =
		m_T_amb_des = m_P_amb_des =
		m_T_hot_in_des = m_P_hot_in_des = m_m_dot_total = m_W_dot_fan_des = m_delta_P_des = m_T_hot_out_des = m_m_dot_air_des = m_Q_dot_des = m_P_hot_out_des =
		m_d_out = m_fin_pitch = m_D_h = m_fin_thk = m_sigma = m_alpha = m_A_fin_to_surf = m_s_h = m_s_v = m_fin_V_per_m = numeric_limits<double>::quiet_NaN();

	m_N_loops = m_N_nodes = m_enum_compact_hx_config = -1;

	mc_air.SetFluid(mc_air.Air);
}

bool C_CO2_to_air_cooler::design_hx(double T_amb_K, double P_amb_Pa, double T_hot_in_K, double P_hot_in_kPa,
	double m_dot_hot_kg_s, double W_dot_fan_MW, double deltaP_kPa, double T_hot_out_K)
{
	// double T_amb_K, double P_amb_Pa, double T_hot_in_K, double P_hot_in_kPa, double m_dot_hot_kg_s
	// double W_dot_fan_MW, double deltaP_kPa, double T_hot_out_K

	//m_enum_compact_hx_config = fc_tubes_s80_38T;
	m_enum_compact_hx_config = N_compact_hx::fc_tubes_sCF_88_10Jb;

	// Get HX Geometry
	N_compact_hx::get_compact_hx_geom(m_enum_compact_hx_config, m_d_out, m_fin_pitch, m_D_h, m_fin_thk,
		m_sigma, m_alpha, m_A_fin_to_surf, m_s_h, m_s_v, m_fin_V_per_m);

	// Thickness should really be tied to HX config
	//m_th = 0.001;		//fc_tubes_s80-38T
	m_th = 0.0024;		//fc_tubes_sCF-88-10Jb

	// Get Remaining Design Info: hardcode for now, but eventually will be inputs
	// Air-Cooler Specs
	m_N_loops = 3;
	m_N_nodes = 5;
	m_eta_fan = 0.5;
	m_d_in = m_d_out - 2 * m_th;
	m_roughness = 4.5E-5;					//[m] absolute roughness of material
	m_A_cs = 0.25*CSP::pi*pow(m_d_in, 2);	//[m2] flow cross-section area
	m_relRough = m_roughness / m_d_in;		//[-] Relative Roughness

	/* Set These With Function Arguments
	// Design Ambient Conditions
	m_T_amb_des = 32.0 + 273.15;		//[K] Air Prop routine needs K
	m_P_amb_des = 101325.0;				//[Pa] Air prop routine needs Pa

	// Hot-side Inlet Conditions
	m_T_hot_in_des = 100.0 + 273.15;	//[K] Other htf props routines need K
	m_P_hot_in_des = 8000.0;			//[kPa] CO2 props need kPa
	m_m_dot_total = 938.9;				//[kg/s] Total sCO2 mass flow into air-cooler

	// Design Performance Targets
	m_W_dot_fan_des = 0.35;				//[MW]
	m_delta_P_des = 62.5;				//[kPa]
	m_T_hot_out_des = 48.0 + 273.15;	//[K]
	*/
	m_T_amb_des = T_amb_K;
	m_P_amb_des = P_amb_Pa;

	m_T_hot_in_des = T_hot_in_K;
	m_P_hot_in_des = P_hot_in_kPa;
	m_m_dot_total = m_dot_hot_kg_s;

	m_W_dot_fan_des = W_dot_fan_MW;
	m_delta_P_des = deltaP_kPa;
	m_T_hot_out_des = T_hot_out_K;

	m_P_hot_out_des = m_P_hot_in_des - m_delta_P_des;
	//double P_hot_ave = 0.5*(m_P_hot_out_des + m_P_hot_in_des);
	double P_hot_ave = m_P_hot_in_des;
	// Set up 'matrix_t's for temperature and pressure
	// Using index 1 for m_N_nodes, so 0 index remains undefined
	// Also, each node requires inlet&outlet temp, so in total, m_N_nodes + 2 required
	util::matrix_t<double>    T_co2(m_N_nodes + 2, m_N_loops + 1);
	util::matrix_t<double>    P_co2(m_N_nodes + 2, m_N_loops + 1);
	util::matrix_t<double>    T_air(m_N_nodes + 2, m_N_loops + 1);
	T_co2.fill(numeric_limits<double>::quiet_NaN());
	P_co2.fill(numeric_limits<double>::quiet_NaN());
	T_air.fill(numeric_limits<double>::quiet_NaN());

	// index that gives outlet temperatur and pressure: depends on whether odd or even loops
	m_final_outlet_index = ((m_N_loops + 2) % 2)*m_N_nodes + 1;

	// Assume air props don't change significantly in air cooler
	double mu_air = mc_air.visc(m_T_amb_des);
	double v_air = 1.0 / mc_air.dens(m_T_amb_des, m_P_amb_des);
	double cp_air = mc_air.Cp(m_T_amb_des)*1000.0;
	double k_air = mc_air.cond(m_T_amb_des);
	double Pr_air = (cp_air*mu_air / k_air);

	// Calculate the required heat rejection
	CO2_state co2_props;
	CO2_TP(m_T_hot_in_des, P_hot_ave, &co2_props);
	double h_in_des = co2_props.enth*1000.0;					//[J/kg]
	CO2_TP(m_T_hot_out_des, m_P_hot_in_des, &co2_props);
	double h_out_des = co2_props.enth*1000.0;					//[J/kg]
	double Q_dot_des = m_m_dot_total*(h_in_des - h_out_des);	//[W]
	double deltaT_hot = m_T_hot_in_des - m_T_hot_out_des;		//[K,C] Hot side temperature difference

	m_Depth = m_s_h * m_N_loops;	//[m] Dimension parallel to air flow

	// 1) Guess dimension perpendicular to air AND hot fluid flow
	// (basically the number of parallel flow paths)

	// ********************************************************************************
	// ** Set up guesses and control for bisection and false-position **
	// ** Try to get better guess by estimating length required to hit pressure drop **
	// ********************************************************************************
	double T_co2_deltaP_eval = 0.75*m_T_hot_in_des + 0.25*m_T_hot_out_des;
	CO2_TP(T_co2_deltaP_eval, m_P_hot_in_des, &co2_props);
	double visc_dyn_co2_g = CO2_visc(co2_props.dens, co2_props.temp)*1.E-6;

	// Just try hitting a "reasonable" Reynolds number?
	// This sets the mass flow rate in the tube, which then sets the number of required tubes
	//  to contain the defined mass flow rate
	//double Re_co2_g = m_dot_tube*m_d_in / (m_A_cs*visc_dyn_co2_g);
	double Re_g = 5.E6;		//[-] A "reasonable" Reynolds number
	double m_dot_tube_g1 = Re_g*m_A_cs*visc_dyn_co2_g / m_d_in;	//[kg/s] Mass flow rate to achieve Reynolds number

	double N_par_g = m_m_dot_total / m_dot_tube_g1;	//[-] Number of parallel flow paths required to contain all mass flow

	double W_par = N_par_g * m_s_v;		//[m] Dimension perpendicular to air AND hot fluid flow... parallel paths dimension

	//**********************************************************************************
	//**********************************************************************************
	//**********************************************************************************
	// Overwrite here to test against EES code
	// W_par = 218.1;
	// ***************************************
	//**********************************************************************************

	double tol = 0.001;		//[-] Relative tolerance for convergence
	double diff_T_hot_out = 2.0*tol;	//[-] Set diff > tol
	int iter_W_par = 0;

	bool is_lowbound_W_par = false;
	double x_lower_W_par = numeric_limits<double>::quiet_NaN();
	bool is_lowdiff_W_par = false;
	double y_lower_W_par = numeric_limits<double>::quiet_NaN();

	bool is_upbound_W_par = false;
	double x_upper_W_par = numeric_limits<double>::quiet_NaN();
	bool is_updiff_W_par = false;
	double y_upper_W_par = numeric_limits<double>::quiet_NaN();

	// Values defined in inner nests that are need at this level
	double L_tube = numeric_limits<double>::quiet_NaN();
	double h_conv_air = numeric_limits<double>::quiet_NaN();
	double V_total = numeric_limits<double>::quiet_NaN();
	double N_par = numeric_limits<double>::quiet_NaN();
	double N_tubes = numeric_limits<double>::quiet_NaN();
	double m_dot_air_total = std::numeric_limits<double>::quiet_NaN();
	double A_surf_node = std::numeric_limits<double>::quiet_NaN();

	while( fabs(diff_T_hot_out) > tol )
	{
		iter_W_par++;		// Increase iteration counter

		// Guess new W_parallel!
		if( iter_W_par > 1 )
		{
			if( diff_T_hot_out > 0.0 )	// Calculated inlet temperature is too high, decrease length
			{
				is_upbound_W_par = true;
				x_upper_W_par = W_par;
				y_upper_W_par = diff_T_hot_out;
				if( is_lowbound_W_par )
				{
					W_par = -y_upper_W_par*(x_lower_W_par - x_upper_W_par) / (y_lower_W_par - y_upper_W_par) + x_upper_W_par;
				}
				else
				{
					W_par *= 0.5;
				}
			}
			else						// Calculated inlet tempearture is too low, increase length
			{
				is_lowbound_W_par = true;
				x_lower_W_par = W_par;
				y_lower_W_par = diff_T_hot_out;
				if( is_upbound_W_par )
				{
					W_par = -y_upper_W_par*(x_lower_W_par - x_upper_W_par) / (y_lower_W_par - y_upper_W_par) + x_upper_W_par;
				}
				else
				{
					W_par *= 2.0;
				}
			}
		}

		// Divide by the distance between tubes to get number of parallel units
		// This is number of tube connected to the inlet headers
		// Can be any positive rational number so a continuous solution space is available
		N_par = W_par / m_s_v;
		N_tubes = N_par*m_N_loops;
		/// Can now calculate the mass flow rate per tube
		double m_dot_tube = m_m_dot_total / N_par;

		// 2) Guess the length of the hot side tube for one pass/loop
		// ********************************************************************************
		// ** Try to estimate length required to hit pressure drop **
		// ********************************************************************************
		CO2_TP(T_co2_deltaP_eval, P_hot_ave, &co2_props);
		double visc_dyn_co2_g = CO2_visc(co2_props.dens, co2_props.temp)*1.E-6;
		double Re_co2_g = m_dot_tube*m_d_in / (m_A_cs*visc_dyn_co2_g);

		double rho_co2_g = co2_props.dens;
		double visc_kin_co2_g = visc_dyn_co2_g / rho_co2_g;
		double cond_co2_g = CO2_cond(co2_props.dens, co2_props.temp);
		double specheat_co2_g = co2_props.cp*1000.0;
		double alpha_co2_g = cond_co2_g / (specheat_co2_g*rho_co2_g);
		double Pr_co2 = visc_kin_co2_g / alpha_co2_g;

		double Nusselt_co2_g = -999.9;
		double f_co2_g = -999.9;

		// Specifying the length over diameter = 1000 sets the problem as Fully Developed Flow
		CSP::PipeFlow(Re_co2_g, Pr_co2, 1000.0, m_relRough, Nusselt_co2_g, f_co2_g);

		double u_m = m_dot_tube / (rho_co2_g*m_A_cs);
		//m_delta_P_des*1000.0 = f_co2_g*L_node*rho_co2_g*pow(u_m,2)/(2.0*m_d_in)
		L_tube = m_delta_P_des*1000.0*(2.0*m_d_in) / (f_co2_g*rho_co2_g*pow(u_m, 2)) / m_N_loops;
		//**********************************************************************************
		//**********************************************************************************
		//**********************************************************************************
		// Overwrite here to test against EES code
		// L_tube = 7.04;
		// ***************************************
		//**********************************************************************************

		// Want to set increasingly tight tolerances to help convergence of outer loops
		double tol_L_tube = tol / 5.0;
		double diff_deltaP = 2.0*tol_L_tube;
		int iter_L_tube = 0;

		bool is_lowbound_L_tube = false;
		double x_lower_L_tube = numeric_limits<double>::quiet_NaN();
		bool is_lowdiff_L_tube = false;
		double y_lower_L_tube = numeric_limits<double>::quiet_NaN();

		bool is_upbound_L_tube = false;
		double x_upper_L_tube = numeric_limits<double>::quiet_NaN();
		bool is_updiff_L_tube = false;
		double y_upper_L_tube = numeric_limits<double>::quiet_NaN();

		while( fabs(diff_deltaP) > tol_L_tube )
		{
			iter_L_tube++;

			if( iter_L_tube > 1 )
			{
				if( diff_deltaP > 0.0 )	// Calculated pressure drop too high - decrease length
				{
					is_upbound_L_tube = true;
					x_upper_L_tube = L_tube;
					y_upper_L_tube = diff_deltaP;
					if( is_lowbound_L_tube )
					{
						L_tube = -y_upper_L_tube*(x_lower_L_tube - x_upper_L_tube) / (y_lower_L_tube - y_upper_L_tube) + x_upper_L_tube;
					}
					else
					{
						L_tube *= 0.5;
					}
				}
				else						// Calculated pressure drop too low - increase length
				{
					is_lowbound_L_tube = true;
					x_lower_L_tube = L_tube;
					y_lower_L_tube = diff_deltaP;
					if( is_upbound_L_tube )
					{
						L_tube = -y_upper_L_tube*(x_lower_L_tube - x_upper_L_tube) / (y_lower_L_tube - y_upper_L_tube) + x_upper_L_tube;
					}
					else
					{
						L_tube *= 2.0;
					}
				}
			}

			double L_total = L_tube*m_N_loops;		//[m] Total length of flow path including loops
			double L_node = L_tube / m_N_nodes;		//[m] Length of one node
			double V_node = L_node*m_s_v*m_s_h;		//[m^3] Volume of one node
			V_total = L_tube*m_Depth*W_par;	//[m^3] Total HX volume

			// 2.5) Iterative loop to find air mass flow rate resulting in target fan power
			//double m_dot_air_total = 3668.0;
			// Try reasonably overestimating air mass flow rate by energy balance assuming small increase in air temp
			// Q_dot_des = m_dot_air_total*cp_air*deltaT
			// *** After 1st iteration can do something smarter here ****
			m_dot_air_total = Q_dot_des / (5.0*cp_air);		// Assume 5K temp difference
			//**********************************************************************************
			// Overwrite here to test against EES code
			// m_dot_air_total = 6165;
			// ***************************************
			//**********************************************************************************

			bool is_lowbound_m_dot = false;
			double x_lower_m_dot = numeric_limits<double>::quiet_NaN();
			double y_lower_m_dot = numeric_limits<double>::quiet_NaN();

			bool is_upbound_m_dot = false;
			double x_upper_m_dot = numeric_limits<double>::quiet_NaN();
			double y_upper_m_dot = numeric_limits<double>::quiet_NaN();

			// Another loop in, so tighten convergence
			double tol_m_dot = tol_L_tube / 2.0;					//[-] Relative tolerance for convergence
			double diff_W_dot_fan = 2.0*tol_m_dot;			//[-] Set diff > tol
			int iter_m_dot = 0;

			// Variable solved in additional nests that are required at this level
			h_conv_air = numeric_limits<double>::quiet_NaN();

			while( fabs(diff_W_dot_fan) > tol_m_dot )
			{
				iter_m_dot++;

				if( iter_m_dot > 1 )
				{
					if( diff_W_dot_fan > 0.0 )	// Calculated fan power too high - decrease m_dot
					{
						is_upbound_m_dot = true;
						x_upper_m_dot = m_dot_air_total;
						y_upper_m_dot = diff_W_dot_fan;
						if( is_lowbound_m_dot )
						{
							if( fmax(y_upper_m_dot, y_lower_m_dot) < 0.75 )
								m_dot_air_total = -y_upper_m_dot*(x_lower_m_dot - x_upper_m_dot) / (y_lower_m_dot - y_upper_m_dot) + x_upper_m_dot;
							else
								m_dot_air_total = 0.5*(x_lower_m_dot + x_upper_m_dot);
							//-y_upper    *(x_lower       - x_upper     )/(y_lower      -     y_upper) +x_upper;
						}
						else
						{
							m_dot_air_total *= 0.1;
						}
					}
					else						// Calculated fan power too low - increase m_dot
					{
						is_lowbound_m_dot = true;
						x_lower_m_dot = m_dot_air_total;
						y_lower_m_dot = diff_W_dot_fan;
						if( is_upbound_m_dot )
						{
							if( fmax(y_upper_m_dot, y_lower_m_dot) < 0.75 )
								m_dot_air_total = -y_upper_m_dot*(x_lower_m_dot - x_upper_m_dot) / (y_lower_m_dot - y_upper_m_dot) + x_upper_m_dot;
							else
								m_dot_air_total = 0.5*(x_lower_m_dot + x_upper_m_dot);
						}
						else
						{
							m_dot_air_total *= 10.0;
						}
					}
				}

				double G_air = m_dot_air_total / (m_sigma*L_tube*W_par);
				double Re_air = G_air*m_D_h / mu_air;
				double f_air, j_H_air;
				f_air, j_H_air = numeric_limits<double>::quiet_NaN();

				if( !N_compact_hx::get_compact_hx_f_j(m_enum_compact_hx_config, Re_air, f_air, j_H_air) )
					return false;

				double deltaP_air = pow(G_air, 2.0)*v_air*0.5*f_air*m_alpha*V_total / (m_sigma*L_tube*W_par);
				h_conv_air = j_H_air*G_air*cp_air / pow(Pr_air, (2.0 / 3.0));	//[W/m^2-K]

				double V_dot_air_total = m_dot_air_total*v_air;
				double W_dot_fan = deltaP_air*V_dot_air_total / m_eta_fan / 1.E6;

				diff_W_dot_fan = (W_dot_fan - m_W_dot_fan_des) / m_W_dot_fan_des;

			}	// Iteration on air mass flow rate

			A_surf_node = V_node*m_alpha;		//[-] Air-side surface area of node
			double UA_node = A_surf_node*h_conv_air;	//[W/K] Conductance of node - assuming air convective heat transfer is governing resistance

			// Set known inlet conditions: iteration thru # of loops needs previous loop info
			T_co2(1, 0) = m_T_hot_out_des;
			P_co2(1, 0) = m_P_hot_in_des - m_delta_P_des;
			for( int i = 1; i < m_N_nodes + 2; i++ )
				T_air(i, 0) = m_T_amb_des;

			// Assuming constant air props, so can set those
			double m_dot_air_tube = m_dot_air_total / (N_par*m_N_nodes);
			double C_dot_air = cp_air*m_dot_air_tube;

			for( int j = 1; j < m_N_loops + 1; j++ )
			{
				// Set up constants and multipliers to switch direction of flow
				double mult_const = (j + 1) % 2;
				double constant = m_N_nodes + 2;
				double mult_index = 1.0 - 2.0*mult_const;
				double out_const = mult_index;

				// Set inlet temperatures & pressures of current row
				double mult_inlet = (j + 1) % 2;
				double const_inlet = m_N_nodes;
				double inlet = mult_inlet*const_inlet + 1;

				// Set loop inlet conditions
				T_co2(inlet, j) = T_co2(inlet, j - 1);
				P_co2(inlet, j) = P_co2(inlet, j - 1);

				double deltaT_prev = numeric_limits<double>::quiet_NaN();

				for( int i = 1; i < m_N_nodes + 1; i++ )
				{
					double in = mult_const*constant + mult_index*i;
					double out = in + out_const;
					double air_in = fmin(in, out);

					// Guess outlet temperature
					double T_out_guess = numeric_limits<double>::quiet_NaN();
					//if(deltaT_prev != deltaT_prev)
					//	T_out_guess = T_co2(in,j) + 0.5*deltaT_hot/(m_N_loops*m_N_nodes);
					//else
					//	T_out_guess = T_co2(in,j) + 0.8*deltaT_prev;						
					T_out_guess = T_co2(in, j) + 1.0;

					double tol_T_in = tol_m_dot / 5.0;		//[-] Relative tolerance for convergence
					double diff_T_in = 2.0*tol_T_in;		//[-] Set diff > tol
					int iter_T_in = 0;

					bool is_lowbound_T_out = false;
					double x_lower_T_out = numeric_limits<double>::quiet_NaN();
					double y_lower_T_out = numeric_limits<double>::quiet_NaN();

					bool is_upbound_T_out = false;
					double x_upper_T_out = numeric_limits<double>::quiet_NaN();
					double y_upper_T_out = numeric_limits<double>::quiet_NaN();

					// Values solved in inner nest required later in code
					double Q_dot_node = numeric_limits<double>::quiet_NaN();

					while( fabs(diff_T_in) > tol_T_in )
					{
						iter_T_in++;

						if( iter_T_in > 1 )
						{
							if( diff_T_in > 0.0 )		// Guessed temperature too high - reduce guess
							{
								is_upbound_T_out = true;
								x_upper_T_out = T_out_guess;
								y_upper_T_out = diff_T_in;
								if( is_lowbound_T_out )
								{
									T_out_guess = -y_upper_T_out*(x_lower_T_out - x_upper_T_out) / (y_lower_T_out - y_upper_T_out) + x_upper_T_out;
								}
								else
								{
									T_out_guess = 0.5*(T_out_guess + T_co2(in, j));
								}
							}
							else						// Calculated fan power too low - increase m_dot
							{
								is_lowbound_T_out = true;
								x_lower_T_out = T_out_guess;
								y_lower_T_out = diff_T_in;
								if( is_upbound_T_out )
								{
									T_out_guess = -y_upper_T_out*(x_lower_T_out - x_upper_T_out) / (y_lower_T_out - y_upper_T_out) + x_upper_T_out;
								}
								else
								{
									T_out_guess = T_out_guess + (T_out_guess - T_co2(in, j));
								}
							}

						}
						T_out_guess = fmin(700.0 + 273.15, T_out_guess);

						if( x_lower_T_out >= 700.0 + 273.15 )
						{
							break;
						}

						//if( x_lower_T_out > m_T_hot_in_des + 100.0 )
						//{
						//	reguess_length = true;
						//	T_co2(final_outlet_index, m_N_loops) = T_out_guess;
						//	P_co2(final_outlet_index, m_N_loops) = 1.05*m_P_hot_in_des;
						//	break;
						//}

						double T_out_ave = 0.5*(T_out_guess + T_co2(in, j));

						int co2_prop_error = CO2_TP(T_out_ave, P_hot_ave, &co2_props);
						double cp_co2_ave = co2_props.cp*1000.0;

						// Capacitance rates
						double C_dot_co2 = cp_co2_ave*m_dot_tube;
						double C_dot_min = fmin(C_dot_air, C_dot_co2);
						double C_dot_max = fmax(C_dot_air, C_dot_co2);
						double Q_dot_max = C_dot_min*(T_out_guess - T_air(air_in, j - 1));
						double NTU = UA_node / C_dot_min;
						double CR = C_dot_min / C_dot_max;
						// Unmixed cross-flow
						double epsilon = 1 - exp(pow(NTU, 0.22) / CR*(exp(-CR*pow(NTU, 0.78)) - 1));
						Q_dot_node = epsilon*Q_dot_max;

						double T_in_check = T_out_guess - Q_dot_node / C_dot_co2;

						//T_co2(out, j) = min(700.0 + 273.15, T_co2(in, j) + Q_dot_node / C_dot_co2 );
						m_L_tube = T_out_guess;
						deltaT_prev = T_co2(out, j) - T_co2(in, j);

						diff_T_in = (T_in_check - T_co2(in, j)) / T_co2(in, j);

					}	// **** End T_out (node) iteration ***********************

					T_co2(out, j) = fmin(700.0 + 273.15, T_out_guess);
					T_air(air_in, j) = T_air(air_in, j - 1) + Q_dot_node / C_dot_air;

					// Add pressure drop calcs (co2_props is up-to-date)
					// ** Could also move this to a function if also called to guess length
					double visc_dyn_co2 = CO2_visc(co2_props.dens, co2_props.temp)*1.E-6;
					double Re_co2 = m_dot_tube*m_d_in / (m_A_cs*visc_dyn_co2);

					double rho_co2 = co2_props.dens;
					double visc_kin_co2 = visc_dyn_co2 / rho_co2;
					double cond_co2 = CO2_cond(co2_props.dens, co2_props.temp);
					double specheat_co2 = co2_props.cp*1000.0;
					double alpha_co2 = cond_co2 / (specheat_co2*rho_co2);
					double Pr_co2 = visc_kin_co2 / alpha_co2;

					double Nusselt_co2 = -999.9;
					double f_co2 = -999.9;

					// Specifying the length over diameter = 1000 sets the problem as Fully Developed Flow
					// CSP::PipeFlow(Re_co2, Pr_co2, 1000.0, m_relRough, Nusselt_co2, f_co2);
					CSP::PipeFlow(Re_co2, Pr_co2, L_node / m_d_in, m_relRough, Nusselt_co2, f_co2);

					double u_m = m_dot_tube / (rho_co2*m_A_cs);
					P_co2(out, j) = P_co2(in, j) + f_co2*L_node*rho_co2*pow(u_m, 2) / (2.0*m_d_in) / 1000.0;

					double deltaP_node = P_co2(out, j) - P_co2(in, j);

					m_V_total = P_co2(out, j);

					P_co2(out, j) = fmin(25000.0, fmax(1000.0, P_co2(out, j)));

				}	// End iteration through nodes in flow path		

			}	// End iteration through loop in flow path

			double T_co2_hot_calc = T_co2(m_final_outlet_index, m_N_loops);

			double deltaP_co2_calc = P_co2(m_final_outlet_index, m_N_loops) - m_P_hot_out_des;

			diff_deltaP = (deltaP_co2_calc - m_delta_P_des) / m_delta_P_des;

		}	// Iteration on length of 1 tube length

		diff_T_hot_out = (T_co2(m_final_outlet_index, m_N_loops) - m_T_hot_in_des) / m_T_hot_in_des;

	};

	// Final reporting metrics
	m_W_par = W_par;			//[m] Dimension in loop/air flow direction
	m_N_par = N_par;			//[-] Number of parallel flow paths
	m_N_tubes = N_tubes;		//[-] Number of tubes
	m_L_tube = L_tube;			//[m] Tube length
	m_L_path = L_tube*m_N_loops;	//[m] Total flow path length
	m_A_surf_total = V_total*m_alpha;			//[-] Air-side surface area of node
	m_UA_total = m_A_surf_total*h_conv_air;		//[W/K] Total HX Conductance
	m_V_total = V_total;		//[m^3] Total HX "footprint" Volume
	m_m_dot_air_des = m_dot_air_total;
	m_Q_dot_des = Q_dot_des;	//[W]
	m_A_surf_node = A_surf_node;	//[m^2]

	double L_tube_total = m_L_tube*m_N_tubes;
	double tube_volume = 0.25*CSP::pi*(pow(m_d_out, 2) - pow(m_d_in, 2))*L_tube_total;
	m_material_V = tube_volume + m_fin_V_per_m*L_tube_total;

	return true;
};

void C_CO2_to_air_cooler::off_design_hx(double T_amb_K, double P_amb_Pa, double T_hot_in_K, double P_hot_in_kPa,
	double m_dot_hot_kg_s, double T_hot_out_K, double & W_dot_fan_MW, int & error_code)
{
	double T_amb = T_amb_K;
	double P_amb = P_amb_Pa;
	double T_hot_in = T_hot_in_K;
	double P_hot_in = P_hot_in_kPa;
	double m_dot_hot = m_dot_hot_kg_s;
	double T_hot_out = T_hot_out_K;

	// Assume air props don't change significantly in air cooler
	double mu_air = mc_air.visc(T_amb);
	double v_air = 1.0 / mc_air.dens(T_amb, P_amb);
	double cp_air = mc_air.Cp(T_amb)*1000.0;
	double k_air = mc_air.cond(T_amb);
	double Pr_air = (cp_air*mu_air / k_air);

	// Calculate the required heat rejection
	CO2_state co2_props;
	CO2_TP(T_hot_in, P_hot_in, &co2_props);			// Assumes no pressure drop...
	double h_in = co2_props.enth*1000.0;					//[J/kg]
	CO2_TP(T_hot_out, P_hot_in, &co2_props);
	double h_out = co2_props.enth*1000.0;					//[J/kg]
	double Q_dot = m_dot_hot*(h_in - h_out);				//[W]
	double deltaT_hot = T_hot_in - T_hot_out;				//[K,C] Hot side temperature difference

	// Set up matrices for HX
	util::matrix_t<double>    T_co2(m_N_nodes + 2, m_N_loops + 1);
	util::matrix_t<double>    P_co2(m_N_nodes + 2, m_N_loops + 1);
	util::matrix_t<double>    T_air(m_N_nodes + 2, m_N_loops + 1);
	T_co2.fill(numeric_limits<double>::quiet_NaN());
	P_co2.fill(numeric_limits<double>::quiet_NaN());
	T_air.fill(numeric_limits<double>::quiet_NaN());

	// Set known inlet conditions: iteration thru # of loops needs previous loop info
	T_co2(1, 0) = T_hot_out;
	P_co2(1, 0) = P_hot_in;			// Again, neglecting pressure drops
	for( int i = 1; i < m_N_nodes + 2; i++ )
		T_air(i, 0) = T_amb;

	double T_hot_tol = 0.001;		//[-] Relative tolerance for convergence
	double diff_T_hot_in = 2.0*T_hot_tol;

	int iter_T_hot = -1;

	double y_upper, y_lower, x_upper, x_lower;
	y_upper = y_lower = x_upper = x_lower = std::numeric_limits<double>::quiet_NaN();
	bool is_upper = false;
	bool is_lower = false;

	double m_dot_tube = m_dot_hot / m_N_par;

	// Guess air mass flow rate through heat exchanger
	double m_dot_air = (Q_dot / m_Q_dot_des)*m_m_dot_air_des;

	double W_dot_fan = std::numeric_limits<double>::quiet_NaN();

	// ********************************
	// Iteration on air mass flow rate
	// ********************************

	while( fabs(diff_T_hot_in) > T_hot_tol )
	{
		iter_T_hot++;

		if( iter_T_hot > 25 )
		{
			if( fabs(W_dot_fan - m_W_dot_fan_des) / m_W_dot_fan_des < 2.0 )		// value "close enough" to be "reasonable"
			{
				W_dot_fan_MW = W_dot_fan;
				error_code = 2;
				return;
			}
			else
			{
				W_dot_fan_MW = -999.9;
				error_code = 1;
				return;
			}
		}

		if( iter_T_hot > 0 )
		{
			if( diff_T_hot_in > 0.0 )			// diff_T_hot_in = (T_co2(m_final_outlet_index, m_N_loops) - T_hot_in) / T_hot_in; ---> mass flow rate too high
			{
				y_upper = diff_T_hot_in;
				x_upper = m_dot_air;
				is_upper = true;
				if( is_lower )
					m_dot_air = -y_upper*(x_lower - x_upper) / (y_lower - y_upper) + x_upper;
				else
					m_dot_air *= 0.5;
			}
			else						// UA_diff = (UA_calc - UA_PHX_od) / UA_PHX_od;  -> Q_dot_PHX too large	 
			{
				y_lower = diff_T_hot_in;
				x_lower = m_dot_air;
				is_lower = true;
				if( is_upper )
					m_dot_air = -y_upper*(x_lower - x_upper) / (y_lower - y_upper) + x_upper;
				else
					m_dot_air *= 1.5;
			}
		}

		// Calculate air-side heat transfer coefficient and UA
		double G_air = m_dot_air / (m_sigma*m_L_tube*m_W_par);
		double Re_air = G_air*m_D_h / mu_air;
		double f_air, j_H_air;
		f_air, j_H_air = numeric_limits<double>::quiet_NaN();

		if( !N_compact_hx::get_compact_hx_f_j(m_enum_compact_hx_config, Re_air, f_air, j_H_air) )
		{
			W_dot_fan_MW = -999.9;
			error_code = 1;
			return;
		}


		double deltaP_air = pow(G_air, 2.0)*v_air*0.5*f_air*m_alpha*m_V_total / (m_sigma*m_L_tube*m_W_par);
		double h_conv_air = j_H_air*G_air*cp_air / pow(Pr_air, (2.0 / 3.0));	//[W/m^2-K]

		double V_dot_air = m_dot_air*v_air;
		W_dot_fan = deltaP_air*V_dot_air / m_eta_fan / 1.E6;				//[MW]

		double UA_node = m_A_surf_node*h_conv_air;	//[W/K] Conductance of node - assuming air convective heat transfer is governing resistance

		for( int j = 1; j < m_N_loops + 1; j++ )
		{
			// Assuming constant air props, so can set those
			double m_dot_air_tube = m_dot_air / (m_N_par*m_N_nodes);
			double C_dot_air = cp_air*m_dot_air_tube;

			// Set up constants and multipliers to switch direction of flow
			double mult_const = (j + 1) % 2;
			double constant = m_N_nodes + 2;
			double mult_index = 1.0 - 2.0*mult_const;
			double out_const = mult_index;

			// Set inlet temperatures & pressures of current row
			double mult_inlet = (j + 1) % 2;
			double const_inlet = m_N_nodes;
			double inlet = mult_inlet*const_inlet + 1;

			// Set loop inlet conditions
			T_co2(inlet, j) = T_co2(inlet, j - 1);
			P_co2(inlet, j) = P_co2(inlet, j - 1);

			double deltaT_prev = numeric_limits<double>::quiet_NaN();

			for( int i = 1; i < m_N_nodes + 1; i++ )
			{
				double in = mult_const*constant + mult_index*i;
				double out = in + out_const;
				double air_in = fmin(in, out);

				// Guess outlet temperature
				double T_out_guess = T_co2(in, j) + 1.0;

				double tol_T_in = T_hot_tol / 50.0;		//[-] Relative tolerance for convergence
				double diff_T_in = 2.0*tol_T_in;		//[-] Set diff > tol
				int iter_T_in = 0;

				bool is_lowbound_T_out = false;
				double x_lower_T_out = numeric_limits<double>::quiet_NaN();
				double y_lower_T_out = numeric_limits<double>::quiet_NaN();

				bool is_upbound_T_out = false;
				double x_upper_T_out = numeric_limits<double>::quiet_NaN();
				double y_upper_T_out = numeric_limits<double>::quiet_NaN();

				// Values solved in inner nest required later in code
				double Q_dot_node = numeric_limits<double>::quiet_NaN();

				while( fabs(diff_T_in) > tol_T_in )
				{
					iter_T_in++;

					if( iter_T_in > 1 )
					{
						if( diff_T_in > 0.0 )		// Guessed temperature too high - reduce guess
						{
							is_upbound_T_out = true;
							x_upper_T_out = T_out_guess;
							y_upper_T_out = diff_T_in;
							if( is_lowbound_T_out )
							{
								T_out_guess = -y_upper_T_out*(x_lower_T_out - x_upper_T_out) / (y_lower_T_out - y_upper_T_out) + x_upper_T_out;
							}
							else
							{
								T_out_guess = 0.5*(T_out_guess + T_co2(in, j));
							}
						}
						else						// Calculated fan power too low - increase m_dot
						{
							is_lowbound_T_out = true;
							x_lower_T_out = T_out_guess;
							y_lower_T_out = diff_T_in;
							if( is_upbound_T_out )
							{
								T_out_guess = -y_upper_T_out*(x_lower_T_out - x_upper_T_out) / (y_lower_T_out - y_upper_T_out) + x_upper_T_out;
							}
							else
							{
								T_out_guess = T_out_guess + (T_out_guess - T_co2(in, j));
							}
						}

					}
					T_out_guess = fmin(700.0 + 273.15, T_out_guess);

					if( x_lower_T_out >= 700.0 + 273.15 )
					{
						break;
					}

					double T_out_ave = 0.5*(T_out_guess + T_co2(in, j));

					// Check this error?
					int co2_prop_error = CO2_TP(T_out_ave, P_hot_in, &co2_props);
					double cp_co2_ave = co2_props.cp*1000.0;

					// Capacitance rates
					double C_dot_co2 = cp_co2_ave*m_dot_tube;
					double C_dot_min = fmin(C_dot_air, C_dot_co2);
					double C_dot_max = fmax(C_dot_air, C_dot_co2);
					double Q_dot_max = C_dot_min*(T_out_guess - T_air(air_in, j - 1));
					double NTU = UA_node / C_dot_min;
					double CR = C_dot_min / C_dot_max;
					// Unmixed cross-flow
					double epsilon = 1 - exp(pow(NTU, 0.22) / CR*(exp(-CR*pow(NTU, 0.78)) - 1));
					Q_dot_node = epsilon*Q_dot_max;

					double T_in_check = T_out_guess - Q_dot_node / C_dot_co2;

					deltaT_prev = T_co2(out, j) - T_co2(in, j);

					double T_last = T_co2(in, j);

					diff_T_in = (T_in_check - T_co2(in, j)) / T_co2(in, j);

				}	// **** End T_out (node) iteration ***********************

				T_co2(out, j) = fmin(700.0 + 273.15, T_out_guess);
				T_air(air_in, j) = T_air(air_in, j - 1) + Q_dot_node / C_dot_air;

			}	// **** End one row of nodes iteration		
		}	// **** End number of passes iteration

		double T_last = T_co2(m_final_outlet_index, m_N_loops);

		diff_T_hot_in = (T_co2(m_final_outlet_index, m_N_loops) - T_hot_in) / T_hot_in;

	}	// End air mass flow rate iteration

	W_dot_fan_MW = W_dot_fan;
	error_code = 0;
	return;
}
