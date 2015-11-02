#include "ud_power_cycle.h"
#include "csp_solver_util.h"

void C_ud_power_cycle::init(const util::matrix_t<double> & T_htf_ind, double T_htf_ref /*C*/, double T_htf_low /*C*/, double T_htf_high /*C*/,
	const util::matrix_t<double> & T_amb_ind, double T_amb_ref /*C*/, double T_amb_low /*C*/, double T_amb_high /*C*/,
	const util::matrix_t<double> & m_dot_htf_ind, double m_dot_htf_ref /*-*/, double m_dot_htf_low /*-*/, double m_dot_htf_high /*-*/)
{

	// Set up Linear Interp class
	int error_index = -2;
	int column_index_array[1] = {0};
	if( !mc_T_htf_ind.Set_1D_Lookup_Table( T_htf_ind, column_index_array, 1, error_index) )
	{
		if(error_index == -1)
		{
			throw(C_csp_exception("Table representing Hot HTF Temperature parametric results must have"
							"at least 3 rows", "User defined power cycle initialization"));
		}
		else
		{
			throw(C_csp_exception("The Hot HTF Temperature must monotonically increase in the table",
							"User defined power cycle initialization"));
		}
	}

	if( !mc_T_amb_ind.Set_1D_Lookup_Table(T_amb_ind, column_index_array, 1, error_index) )
	{
		if( error_index == -1 )
		{
			throw(C_csp_exception("Table representing Ambient Temperature parametric results must have"
				"at least 3 rows", "User defined power cycle initialization"));
		}
		else
		{
			throw(C_csp_exception("The Ambient Temperature must monotonically increase in the table",
				"User defined power cycle initialization"));
		}
	}

	if( !mc_m_dot_htf_ind.Set_1D_Lookup_Table(m_dot_htf_ind, column_index_array, 1, error_index) )
	{
		if( error_index == -1 )
		{
			throw(C_csp_exception("Table representing HTF mass flow rate parametric results must have"
				"at least 3 rows", "User defined power cycle initialization"));
		}
		else
		{
			throw(C_csp_exception("The HTF mass flow rate must monotonically increase in the table",
				"User defined power cycle initialization"));
		}
	}

	// Set member data for reference and upper and lower bounds of independent variables
	m_T_htf_ref = T_htf_ref;
	m_T_htf_low = T_htf_low;
	m_T_htf_high = T_htf_high;

	m_T_amb_ref = T_amb_ref;
	m_T_amb_low = T_amb_low;
	m_T_amb_high = T_amb_high;

	m_m_dot_htf_ref = m_dot_htf_ref;
	m_m_dot_htf_low = m_dot_htf_low;
	m_m_dot_htf_high = m_dot_htf_high;

	// Check that the reference (design) value and upper and lower levels for each independent variable are contained within the x-range of the corresponding table
		// T_HTF
	if( !mc_T_htf_ind.check_x_value_x_col_0(m_T_htf_ref) )
	{
		m_error_msg = util::format("The user defined power cycle table containing parametric runs on the hot HTF temperature"
		" must contain the design HTF temperature %lg [C]. %s [C]", m_T_htf_ref, mc_T_htf_ind.get_error_msg());
		throw(C_csp_exception(m_error_msg, "User defined power cycle initialization"));
	}
	if( !mc_T_htf_ind.check_x_value_x_col_0(m_T_htf_low) )
	{
		m_error_msg = util::format("The user defined power cycle table containing parametric runs on the hot HTF temperature"
			" must contain the lower level HTF temperature %lg [C]. %s [C]", m_T_htf_low, mc_T_htf_ind.get_error_msg());
		throw(C_csp_exception(m_error_msg, "User defined power cycle initialization"));
	}
	if( !mc_T_htf_ind.check_x_value_x_col_0(m_T_htf_high) )
	{
		m_error_msg = util::format("The user defined power cycle table containing parametric runs on the hot HTF temperature"
			" must contain the upper level HTF temperature %lg [C]. %s [C]", m_T_htf_high, mc_T_htf_ind.get_error_msg());
		throw(C_csp_exception(m_error_msg, "User defined power cycle initialization"));
	}

		// T_amb
	if( !mc_T_amb_ind.check_x_value_x_col_0(m_T_amb_ref) )
	{
		m_error_msg = util::format("The user defined power cycle table containing parametric runs on the ambient temperature"
		" must contain the design ambient temperature %lg [C]. %s [C]", m_T_amb_ref, mc_T_amb_ind.get_error_msg());
		throw(C_csp_exception(m_error_msg, "User defined power cycle initialization"));
	}
	if( !mc_T_amb_ind.check_x_value_x_col_0(m_T_amb_low) )
	{
		m_error_msg = util::format("The user defined power cycle table containing parametric runs on the ambient temperature"
		" must contain the lower level ambient temperature %lg [C]. %s [C]", m_T_amb_low, mc_T_amb_ind.get_error_msg());
		throw(C_csp_exception(m_error_msg, "User defined power cycle initialization"));
	}
	if( !mc_T_amb_ind.check_x_value_x_col_0(m_T_amb_high) )
	{
		m_error_msg = util::format("The user defined power cycle table containing parametric runs on the ambient temperature"
		" must contain the upper level ambient temperature %lg [C]. %s [C]", m_T_amb_high, mc_T_amb_ind.get_error_msg());
		throw(C_csp_exception(m_error_msg, "User defined power cycle initialization"));
	}

		// m_dot_HTF
	if( !mc_m_dot_htf_ind.check_x_value_x_col_0(m_m_dot_htf_ref) )
	{
		m_error_msg = util::format("The user defined power cycle table containing parametric runs on the normalized HTF mass flow rate"
		" must contain the design normalized HTF mass flow rate %lg [-]. %s [-]", m_m_dot_htf_ref, mc_m_dot_htf_ind.get_error_msg());
		throw(C_csp_exception(m_error_msg, "User defined power cycle initialization"));
	}
	if( !mc_m_dot_htf_ind.check_x_value_x_col_0(m_m_dot_htf_low) )
	{
		m_error_msg = util::format("The user defined power cycle table containing parametric runs on the normalized HTF mass flow rate"
			" must contain the lower level normalized HTF mass flow rate %lg [-]. %s [-]", m_m_dot_htf_low, mc_m_dot_htf_ind.get_error_msg());
		throw(C_csp_exception(m_error_msg, "User defined power cycle initialization"));
	}
	if( !mc_m_dot_htf_ind.check_x_value_x_col_0(m_m_dot_htf_high) )
	{
		m_error_msg = util::format("The user defined power cycle table containing parametric runs on the normalized HTF mass flow rate"
			" must contain the upper level normalized HTF mass flow rate %lg [-]. %s [-]", m_m_dot_htf_high, mc_m_dot_htf_ind.get_error_msg());
		throw(C_csp_exception(m_error_msg, "User defined power cycle initialization"));
	}

	// ************************************************************************
	// ************************************************************************

	// Calculate main effects of each independent variable at its upper and lower levels
	m_ME_T_htf_low.resize(4);
	m_ME_T_htf_high.resize(4);

	m_ME_T_amb_low.resize(4);
	m_ME_T_amb_high.resize(4);

	m_ME_m_dot_htf_low.resize(4);
	m_ME_m_dot_htf_high.resize(4);

	for(int i = 0; i < 4; i++)
	{
		int i_col = i*3+2;

		m_ME_T_htf_low[i] = mc_T_htf_ind.interpolate_x_col_0(i_col, m_T_htf_low) - 1.0;
		m_ME_T_htf_high[i] = mc_T_htf_ind.interpolate_x_col_0(i_col, m_T_htf_high) - 1.0;

		m_ME_T_amb_low[i] = mc_T_amb_ind.interpolate_x_col_0(i_col, m_T_amb_low) - 1.0;
		m_ME_T_amb_high[i] = mc_T_amb_ind.interpolate_x_col_0(i_col, m_T_amb_high) - 1.0;

		m_ME_m_dot_htf_low[i] = mc_m_dot_htf_ind.interpolate_x_col_0(i_col, m_m_dot_htf_low) - 1.0;
		m_ME_m_dot_htf_high[i] = mc_m_dot_htf_ind.interpolate_x_col_0(i_col, m_m_dot_htf_high) - 1.0;
	}

	// Set up 2D tables to store calculated Interactions	
	int n_T_htf_runs = mc_T_htf_ind.get_number_of_rows();
	int n_T_amb_runs = mc_T_amb_ind.get_number_of_rows();
	int n_m_dot_htf_runs = mc_m_dot_htf_ind.get_number_of_rows();

	// 2 interaction effects (upper and lower) for each output
	util::matrix_t<double> T_htf_int_on_T_amb(n_T_amb_runs, 9);
	util::matrix_t<double> T_amb_int_on_m_dot_htf(n_m_dot_htf_runs, 9);
	util::matrix_t<double> m_dot_htf_int_on_T_htf(n_T_htf_runs, 9);
	
	// Calculate interaction effects
	for(int i = 0; i < 4; i++)
	{
		// T_HTF interaction on ambient temperature
		for(int j = 0; j < n_T_amb_runs; j++)
		{
			if( i == 0 )
			{
				T_htf_int_on_T_amb(j,0) = mc_T_amb_ind.get_x_value_x_col_0(j);
			}
				// lower level interaction effect
			double aa = mc_T_amb_ind.Get_Value(i*3+1,j);
			double bb = m_ME_T_htf_low[i];
			double cc = mc_T_amb_ind.Get_Value(i*3+2,j);
			T_htf_int_on_T_amb(j,i*2+1) = -(mc_T_amb_ind.Get_Value(i*3+1,j)-1.0-m_ME_T_htf_low[i]-(mc_T_amb_ind.Get_Value(i*3+2,j)-1.0));
				// upper level interaction
			aa = mc_T_amb_ind.Get_Value(i*3+3,j);
			bb = m_ME_T_htf_high[i];
			cc = mc_T_amb_ind.Get_Value(i*3+2,j);
			T_htf_int_on_T_amb(j,i*2+2) = -(mc_T_amb_ind.Get_Value(i*3+3,j)-1.0-m_ME_T_htf_high[i]-(mc_T_amb_ind.Get_Value(i*3+2,j)-1.0));
		}

		// Ambient temperature interaction on HTF mass flow rate
		for(int j = 0; j < n_m_dot_htf_runs; j++)
		{
			if( i == 0 )
			{
				T_amb_int_on_m_dot_htf(j,0) = mc_m_dot_htf_ind.get_x_value_x_col_0(j);
			}
				// lower level interaction effect
			double aa = mc_m_dot_htf_ind.Get_Value(i*3+1,j);
			double bb = m_ME_T_amb_low[i];
			double cc = mc_m_dot_htf_ind.Get_Value(i*3+2,j);
			T_amb_int_on_m_dot_htf(j,i*2+1) = -(mc_m_dot_htf_ind.Get_Value(i*3+1,j)-1.0-m_ME_T_amb_low[i]-(mc_m_dot_htf_ind.Get_Value(i*3+2,j)-1.0));
				// upper level interaction effect
			aa = mc_m_dot_htf_ind.Get_Value(i*3+3,j);
			bb = m_ME_T_amb_high[i];
			cc = mc_m_dot_htf_ind.Get_Value(i*3+2,j);
			T_amb_int_on_m_dot_htf(j,i*2+2) = -(mc_m_dot_htf_ind.Get_Value(i*3+3,j)-1.0-m_ME_T_amb_high[i]-(mc_m_dot_htf_ind.Get_Value(i*3+2,j)-1.0));
		}

		// HTF mass flow
		for(int j = 0; j < n_T_htf_runs; j++)
		{
			if( i == 0 )
			{
				m_dot_htf_int_on_T_htf(j,0) = mc_T_htf_ind.get_x_value_x_col_0(j);
			}
				// lower level interaction effect
			double aa = mc_T_htf_ind.Get_Value(i*3+1,j);
			double bb = m_ME_m_dot_htf_low[i];
			double cc = mc_T_htf_ind.Get_Value(i*3+2,j);
			m_dot_htf_int_on_T_htf(j,i*2+1) = -(mc_T_htf_ind.Get_Value(i*3+1,j)-1.0-m_ME_m_dot_htf_low[i]-(mc_T_htf_ind.Get_Value(i*3+2,j)-1.0));
				// upper level interaction effect
			aa = mc_T_htf_ind.Get_Value(i*3+3,j);
			bb = m_ME_m_dot_htf_high[i];
			cc = mc_T_htf_ind.Get_Value(i*3+2,j);
			m_dot_htf_int_on_T_htf(j,i*2+2) = -(mc_T_htf_ind.Get_Value(i*3+3,j)-1.0-m_ME_m_dot_htf_high[i]-(mc_T_htf_ind.Get_Value(i*3+2,j)-1.0));
		}
	}

	// Initialize Linear_Interp classes for interaction effects
	if( !mc_T_htf_on_T_amb.Set_1D_Lookup_Table(T_htf_int_on_T_amb, column_index_array, 1, error_index) )
	{
		throw(C_csp_exception("Initialization of interpolation table for the interaction effect of T_HTF levels"
		"on the ambient temperature failed", "User defined power cycle initialization"));
	}
	if( !mc_T_amb_on_m_dot_htf.Set_1D_Lookup_Table(T_amb_int_on_m_dot_htf, column_index_array, 1, error_index) )
	{
		throw(C_csp_exception("Initialization of interpolation table for the interaction effect of T_amb levels"
			"on HTF mass flow rate failed", "User defined power cycle initialization"));
	}
	if( !mc_m_dot_htf_on_T_htf.Set_1D_Lookup_Table(m_dot_htf_int_on_T_htf, column_index_array, 1, error_index) )
	{
		throw(C_csp_exception("Initialization of interpolation table for the interaction effect of m_dot_HTF levels"
			"on the HTF temperature failed", "User defined power cycle initialization"));
	}
	
}

double C_ud_power_cycle::get_W_dot_gross_ND(double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/)
{
	// This call needs to define which columns to search
	// Then use 'get_interpolated_ND_output' to get ND total effect
	
	return get_interpolated_ND_output(i_W_dot_gross, T_htf_hot, T_amb, m_dot_htf_ND);

	// Also, maybe want to check parameters against max/min, or if extrapolating, or something?
}

double C_ud_power_cycle::get_Q_dot_HTF_ND(double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/)
{
	// This call needs to define which columns to search
	// Then use 'get_interpolated_ND_output' to get ND total effect

	return get_interpolated_ND_output(i_Q_dot_HTF, T_htf_hot, T_amb, m_dot_htf_ND);

	// Also, maybe want to check parameters against max/min, or if extrapolating, or something?
}

double C_ud_power_cycle::get_W_dot_cooling_ND(double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/)
{
	// This call needs to define which columns to search
	// Then use 'get_interpolated_ND_output' to get ND total effect

	return get_interpolated_ND_output(i_W_dot_cooling, T_htf_hot, T_amb, m_dot_htf_ND);

	// Also, maybe want to check parameters against max/min, or if extrapolating, or something?
}

double C_ud_power_cycle::get_m_dot_water_ND(double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/)
{
	// This call needs to define which columns to search
	// Then use 'get_interpolated_ND_output' to get ND total effect

	return get_interpolated_ND_output(i_m_dot_water, T_htf_hot, T_amb, m_dot_htf_ND);

	// Also, maybe want to check parameters against max/min, or if extrapolating, or something?
}

double C_ud_power_cycle::get_interpolated_ND_output(int i_ME /*M.E. table index*/, 
							double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/)
{
	
	double ME_T_htf = mc_T_htf_ind.interpolate_x_col_0(i_ME*3+2, T_htf_hot) - 1.0;
	double ME_T_amb = mc_T_amb_ind.interpolate_x_col_0(i_ME*3+2, T_amb) - 1.0;
	double ME_m_dot_htf = mc_m_dot_htf_ind.interpolate_x_col_0(i_ME*3+2, m_dot_htf_ND) - 1.0;

	double INT_T_htf_on_T_amb = 0.0;
	if( T_htf_hot < m_T_htf_ref )
	{
		INT_T_htf_on_T_amb = mc_T_htf_on_T_amb.interpolate_x_col_0(i_ME*2+1,T_amb)*(T_htf_hot-m_T_htf_ref)/(m_T_htf_ref-m_T_htf_low);
	}
	if( T_htf_hot > m_T_htf_ref )
	{
		INT_T_htf_on_T_amb = mc_T_htf_on_T_amb.interpolate_x_col_0(i_ME*2+2,T_amb)*(T_htf_hot-m_T_htf_ref)/(m_T_htf_ref-m_T_htf_high);
	}

	double INT_T_amb_on_m_dot_htf = 0.0;
	if( T_amb < m_T_amb_ref )
	{
		INT_T_amb_on_m_dot_htf = mc_T_amb_on_m_dot_htf.interpolate_x_col_0(i_ME*2+1,m_dot_htf_ND)*(T_amb-m_T_amb_ref)/(m_T_amb_ref-m_T_amb_low);
	}
	if( T_amb > m_T_amb_ref )
	{
		INT_T_amb_on_m_dot_htf = mc_T_amb_on_m_dot_htf.interpolate_x_col_0(i_ME*2+2,m_dot_htf_ND)*(T_amb-m_T_amb_ref)/(m_T_amb_ref-m_T_amb_high);
	}

	double INT_m_dot_htf_on_T_htf = 0.0;
	if( m_dot_htf_ND < m_m_dot_htf_ref )
	{
		INT_m_dot_htf_on_T_htf = mc_m_dot_htf_on_T_htf.interpolate_x_col_0(i_ME*2+1,T_htf_hot)*(m_dot_htf_ND-m_m_dot_htf_ref)/(m_m_dot_htf_ref-m_m_dot_htf_low);
	}
	if( m_dot_htf_ND > m_m_dot_htf_ref )
	{
		INT_T_amb_on_m_dot_htf = mc_m_dot_htf_on_T_htf.interpolate_x_col_0(i_ME*2+2,T_htf_hot)*(m_dot_htf_ND-m_m_dot_htf_ref)/(m_m_dot_htf_ref-m_m_dot_htf_high);
	}

	return 1.0 + ME_T_htf + ME_T_amb + ME_m_dot_htf + INT_T_htf_on_T_amb + INT_T_amb_on_m_dot_htf + INT_m_dot_htf_on_T_htf;
}