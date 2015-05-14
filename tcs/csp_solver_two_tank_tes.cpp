#include "csp_solver_two_tank_tes.h"
#include "csp_solver_util.h"

C_csp_two_tank_tes::C_csp_two_tank_tes()
{
	m_V_tank_active = std::numeric_limits<double>::quiet_NaN();

	m_V_tank_hot_prev = m_T_tank_hot_prev = m_V_tank_cold_prev = m_T_tank_cold_prev =
		m_m_tank_hot_prev = m_m_tank_cold_prev = std::numeric_limits<double>::quiet_NaN();
}

void C_csp_two_tank_tes::init()
{
	// Declare instance of fluid class for FIELD fluid
	// Set fluid number and copy over fluid matrix if it makes sense
	if( ms_params.m_field_fl != HTFProperties::User_defined && ms_params.m_field_fl < HTFProperties::End_Library_Fluids )
	{
		if( !mc_field_htfProps.SetFluid(ms_params.m_field_fl) )
		{
			throw(C_csp_exception("Field HTF code is not recognized", "Two Tank TES Initialization"));
		}
	}
	else if( ms_params.m_field_fl == HTFProperties::User_defined )
	{
		int n_rows = ms_params.m_field_fl_props.nrows();
		int n_cols = ms_params.m_field_fl_props.ncols();
		if( n_rows > 2 && n_cols == 7 )
		{
			if( !mc_field_htfProps.SetUserDefinedFluid(ms_params.m_field_fl_props) )
			{
				error_msg = util::format(mc_field_htfProps.UserFluidErrMessage(), n_rows, n_cols);
				throw(C_csp_exception(error_msg, "Two Tank TES Initialization"));
			}
		}
		else
		{
			error_msg = util::format("The user defined field HTF table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", n_rows, n_cols);
			throw(C_csp_exception(error_msg, "Two Tank TES Initialization"));
		}
	}
	else
	{
		throw(C_csp_exception("Field HTF code is not recognized", "Two Tank TES Initialization"));
	}


	// Declare instance of fluid class for STORAGE fluid.
	// Set fluid number and copy over fluid matrix if it makes sense.
	if( ms_params.m_tes_fl != HTFProperties::User_defined && ms_params.m_tes_fl_props < HTFProperties::End_Library_Fluids )
	{
		if( !mc_store_htfProps.SetFluid(ms_params.m_tes_fl) )
		{
			throw(C_csp_exception("Storage HTF code is not recognized", "Two Tank TES Initialization"));
		}	
	}
	else if( ms_params.m_tes_fl == HTFProperties::User_defined )
	{
		int n_rows = ms_params.m_tes_fl_props.nrows();
		int n_cols = ms_params.m_tes_fl_props.ncols();
		if( n_rows > 2 && n_cols == 7 )
		{
			if( !mc_store_htfProps.SetUserDefinedFluid(ms_params.m_tes_fl_props) )
			{
				error_msg = util::format(mc_store_htfProps.UserFluidErrMessage(), n_rows, n_cols);
				throw(C_csp_exception(error_msg, "Two Tank TES Initialization"));
			}
		}
		else
		{
			error_msg = util::format("The user defined storage HTF table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", n_rows, n_cols);
			throw(C_csp_exception(error_msg, "Two Tank TES Initialization"));
		}
	}
	else
	{
		throw(C_csp_exception("Storage HTF code is not recognized", "Two Tank TES Initialization"));
	}

	bool is_hx_calc = true;

	if( ms_params.m_tes_fl != ms_params.m_field_fl )
		is_hx_calc = true;
	else if( ms_params.m_field_fl != HTFProperties::User_defined )
		is_hx_calc = false;
	else
	{
		is_hx_calc = !mc_field_htfProps.equals(&mc_store_htfProps);
	}

	if( ms_params.m_is_hx != is_hx_calc )
	{
		if( is_hx_calc )
			mc_csp_messages.add_message(C_csp_messages::NOTICE, "Input field and storage fluids are different, but the inputs did not specify a field-to-storage heat exchanger. The system was modeled assuming a heat exchanger.");
		else
			mc_csp_messages.add_message(C_csp_messages::NOTICE, "Input field and storage fluids are identical, but the inputs specified a field-to-storage heat exchanger. The system was modeled assuming no heat exchanger.");

		ms_params.m_is_hx = is_hx_calc;
	}

	// Convert parameter units
	ms_params.m_q_pb_design *= 1.E6;			//[W] convert from MW
	ms_params.m_hot_tank_Thtr += 273.15;		//[K] convert from C
	ms_params.m_cold_tank_Thtr += 273.15;		//[K] convert from C
	ms_params.m_T_field_in_des += 273.15;		//[K] convert from C
	ms_params.m_T_field_out_des += 273.15;		//[K] convert from C

	// 5.13.15, twn: also be sure that hx is sized such that it can supply full load to power cycle, in cases of low solar multiples
	double duty = ms_params.m_q_pb_design * fmax(1.0, ms_params.m_solarm);		//[W] Allow all energy from the field to go into storage at any time

	if( ms_params.m_ts_hours > 0.0 )
	{
		int hx_config = Storage_HX::Counter_flow;
		double dt_cold = ms_params.m_dt_hot;
		if( !mc_hx_storage.define_storage(mc_field_htfProps, mc_store_htfProps, !ms_params.m_is_hx, hx_config, duty, 
			ms_params.m_vol_tank, ms_params.m_h_tank, ms_params.m_u_tank, ms_params.m_tank_pairs, ms_params.m_hot_tank_Thtr, ms_params.m_cold_tank_Thtr, 
			ms_params.m_tank_max_heat, ms_params.m_dt_hot, dt_cold, ms_params.m_T_field_out_des, ms_params.m_T_field_in_des) )
		{
			throw(C_csp_exception("Heat exchanger sizing failed", "Two Tank TES Initialization"));
		}
	}

	// Do we need to define minimum and maximum thermal powers to/from storage?
	// The 'duty' definition should allow the tanks to accept whatever the field and/or power cycle can provide...

	// Set initial storage values
	m_V_tank_hot_prev = ms_params.m_V_tank_hot_ini;					//[m^3]
	m_V_tank_cold_prev = ms_params.m_vol_tank - m_V_tank_hot_prev;	//[m^3]

	m_T_tank_hot_prev = ms_params.m_T_tank_hot_ini + 273.15;		//[K]
	m_T_tank_cold_prev = ms_params.m_T_tank_cold_ini + 273.15;		//[K]

	m_m_tank_hot_prev = m_V_tank_hot_prev*mc_store_htfProps.dens(m_T_tank_hot_prev, 1.0);
	m_m_tank_cold_prev = m_V_tank_cold_prev*mc_store_htfProps.dens(m_T_tank_cold_prev, 1.0);

	m_V_tank_active = ms_params.m_vol_tank*(1.0 - 2.0*ms_params.m_h_tank_min / ms_params.m_h_tank);

}