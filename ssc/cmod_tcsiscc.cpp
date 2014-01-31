// Integrated Solar Combined Cycle
#include "core.h"
#include "tckernel.h"

static var_info _cm_vtab_tcsiscc[] = {
//   weather reader inputs
//   VARTYPE            DATATYPE          NAME                LABEL                                                                      UNITS           META            GROUP            REQUIRED_IF                CONSTRAINTS              UI_HINTS
    { SSC_INPUT,        SSC_STRING,      "file_name",         "local weather file path",                                                 "",             "",             "weather",        "*",                       "LOCAL_FILE",            "" },
																															           
	{ SSC_INPUT,        SSC_MATRIX,      "eta_map",           "Solar field efficiency map",                                              "",             "",             "solarfield",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "n_zen",             "Number of zenith positions in efficiency map",                            "",             "",             "solarfield",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "n_azi",             "Number of azimuth positions in efficiency map",                           "",             "",             "solarfield",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "n_hel",             "Number of heliostats in the field",                                       "",             "",             "solarfield",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "q_start",           "Electric work for starting up one heliostat",                            "kWe-hr",        "",             "solarfield",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "p_run",             "Electric power for tracking one heliostat",                              "kWe",           "",             "solarfield",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "v_wind_max",        "Maximum tolerable wind speed",                                           "m/s",           "",             "solarfield",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hel_stow_deploy",   "Heliostat field stow/deploy solar elevation angle",                      "deg",           "",             "solarfield",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "eta_pump",          "HTF pump efficiency",                                                    "",              "",             "solarfield",     "*",                       "",                      "" },	

	{ SSC_INPUT,        SSC_NUMBER,      "N_panels",          "Number of individual panels on the receiver",                            "",              "",             "receiver",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "D_rec",             "The overall outer diameter of the receiver",                             "m",             "",             "receiver",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "H_rec",             "The height of the receiver",                                             "m",             "",             "receiver",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "THT",               "The height of the tower (hel. pivot to rec equator)",                    "m",             "",             "receiver",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "d_tube_out",        "The outer diameter of an individual receiver tube",                      "mm",            "",             "receiver",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "th_tube",           "The wall thickness of a single receiver tube",                           "mm",            "",             "receiver",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "mat_tube",          "The material name of the receiver tubes",                                "",              "",             "receiver",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rec_htf",           "The name of the HTF used in the receiver",                               "",              "",             "receiver",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_MATRIX,      "field_fl_props",    "User defined field fluid property data",                                 "-",              "7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows", "receiver", "*", "",     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Flow_type",		  "A flag indicating which flow pattern is used",				           	"",			     "",             "receiver",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "epsilon",			  "The emissivity of the receiver surface coating",				           	"",		         "",             "receiver",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hl_ffact",	      "The heat loss factor (thermal loss fudge factor)",			           	"",			     "",             "receiver",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "T_htf_hot_des",	  "Hot HTF outlet temperature at design conditions",			           	"C",		     "",             "receiver",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "T_htf_cold_des",	  "Cold HTF inlet temperature at design conditions",			           	"C",		     "",             "receiver",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "f_rec_min",		  "Minimum receiver mass flow rate turn down fraction",			           	"",		         "",             "receiver",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Q_rec_des",		  "Design-point receiver thermal power output",					           	"MWt",	         "",             "receiver",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rec_su_delay",	  "Fixed startup delay time for the receiver",					           	"hr",	         "",             "receiver",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rec_qf_delay",	  "Energy-based receiver startup delay (fraction of rated thermal power)",	"",		         "",             "receiver",       "*",                       "",                      "" }, 
	{ SSC_INPUT,        SSC_NUMBER,      "m_dot_htf_max",	  "Maximum receiver mass flow rate",									    "kg/hr",         "",             "receiver",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "A_sf",			  "Solar Field Area",                                                       "m^2",           "",             "receiver",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,		 "cycle_config",      "Configuration of ISCC power cycle",                                      "-",             "",             "receiver",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_MATRIX,		 "fluxmap_angles",    "Matrix containing zenith and azimuth angles for flux maps",              "-",              "2 columns - azimuth angle, zenith angle. number of rows must equal number of flux maps provided",             "receiver",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_MATRIX,		 "fluxmap",           "Matrix containing flux map for various solar positions",                 "-",             "",             "receiver",       "*",                       "",                      "" },

	// sam_iscc_powerblock.cpp: not reading in parameters that have already been read in for type 222! need to set these below
	{ SSC_INPUT,        SSC_NUMBER,		 "plant_elevation",   "Plant Elevation",                                                        "m",             "",             "powerblock",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,		 "hot_side_delta_t",  "Hot side temperature HX temperature difference",                         "C",             "",             "powerblock",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,		 "pinch_point",       "Cold side HX pinch point",                                               "C",             "",             "powerblock",     "*",                       "",                      "" },

	// sam_iscc_parasitics
	{ SSC_INPUT,        SSC_NUMBER,      "W_htf_pc_pump",     "Required pumping power for HTF through power block",                     "kJ/kg",         "",             "parasitics",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Piping_loss",       "Thermal loss per meter of piping",                                       "Wt/m",          "",             "parasitics",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Piping_length",     "Total length of exposed piping",                                         "m",             "",             "parasitics",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "Q_sf_des",          "Design point solar field thermal output",                                "MW",            "",             "parasitics",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pb_fixed_par",      "Fixed parasitic load - runs at all times",                               "MWe/MWcap",     "",             "parasitics",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "bop_par",           "Balance of plant parasitic power fraction",                              "MWe/MWcap",     "",             "parasitics",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "bop_par_f",         "Balance of plant parasitic power fraction - mult frac",                  "none",          "",             "parasitics",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "bop_par_0",         "Balance of plant parasitic power fraction - const coeff",                "none",          "",             "parasitics",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "bop_par_1",         "Balance of plant parasitic power fraction - linear coeff",               "none",          "",             "parasitics",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "bop_par_2",         "Balance of plant parasitic power fraction - quadratic coeff",            "none",          "",             "parasitics",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "W_dot_fossil_des",  "Fossil-only cycle output at design",                                     "MWe",           "",             "parasitics",     "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "W_dot_solar_des",   "Solar contribution to cycle output at design"                            "MWe",           "",             "parasitics",     "*",                       "",                      "" },

	// OUTPUTS
	// weather
	{ SSC_OUTPUT,       SSC_ARRAY,       "tdry",              "Ambient dry bulb temperature",                                   "C",            "",             "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "wspd",              "Wind Speed",                                                     "m/s",          "",             "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "beam",              "Beam normal irradiance",                                         "W/m2",         "",             "Outputs",        "*",                       "LENGTH=8760",           "" },

	// sam_type221
	{ SSC_OUTPUT,       SSC_ARRAY,       "pparasi",           "Parasitic tracking/startup power",                                       "MWe",           "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "eta_field",         "Solar field efficiency (no defocusing)",                                 "",              "",             "Outputs",        "*",                      "",                       "" },
	
	// sam_type222 MS receiver
	{ SSC_OUTPUT,       SSC_ARRAY,       "m_dot_salt_tot",    "Receiver m_dot, derated for startup",                                    "kg/s",          "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "m_dot_ss",          "Receiver m_dot, steady state",                                           "kg/s",          "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "eta_therm",         "Receiver thermal efficiency",                                            "",              "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "W_dot_pump",        "Receiver pump power",                                                    "MWe",           "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "field_eff_adj",     "Solar field efficiency w/ defocusing",                                   "",              "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "q_startup",         "Startup power",                                                          "MWe",           "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "f_timestep",        "Fraction of timestep receiver is operating - not starting up",           "",              "",             "Outputs",        "*",                      "",                       "" },
	
	// sam_iscc_powerblock
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_htf_cold",        "HTF temp at outlet of HTF-steam HX",                                     "C",             "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_st_cold",         "Steam temp from NGCC to HX",                                             "C",             "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_st_hot",          "Steam temp from HX back to NGCC",                                        "C",             "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_dot_max",         "Max allowable thermal power to NGCC",                                    "MWt",           "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "fuel_use",          "Natural gas used during timestep",                                 "MMBTU",         "",             "Outputs",        "*",                      "",                       "" },

	// sam_iscc_parasitics
	{ SSC_OUTPUT,       SSC_ARRAY,       "W_dot_pc_hybrid",   "Net power cycle output including solar power",                           "MWe",           "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "W_dot_pc_fossil",   "Net power cycle output only considering fossil power",                   "MWe",           "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "W_dot_plant_hybrid","Net plant output including solar power & parasitics",                    "MWe",           "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "W_dot_plant_fossil","Net plant output only considering fossil power & parasitics",            "MWe",           "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "W_dot_plant_solar", "Net plant output attributable to solar",                                 "MWe",           "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "eta_solar_use",     "Solar use efficiency considering parasitics",                            "",              "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "eta_fuel",          "Electrical efficiency of fossil only operation",                         "%",             "",             "Outputs",        "*",                      "",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "solar_fraction",    "Solar fraction",                                                         "",              "",             "Outputs",        "*",                      "",                       "" },

	var_info_invalid };

class cm_tcsiscc : public tcKernel
{
public:

	cm_tcsiscc(tcstypeprovider *prov)
	:tcKernel(prov)
	{
		add_var_info( _cm_vtab_tcsiscc );
		//set_store_all_parameters(true); // default is 'false' = only store TCS parameters that match the SSC_OUTPUT variables above 
	}

	void exec( ) throw( general_error )
	{
		//Add weather file reader unit
		int weather = add_unit("weatherreader", "TCS weather reader");
		int hel_field = add_unit("sam_mw_pt_type221", "solarfield");
		int receiver = add_unit("sam_mw_pt_type222", "receiver");
		int iscc_pb = add_unit("sam_iscc_powerblock");
		int iscc_parasitics = add_unit("sam_iscc_parasitics");

		//int blah = as_integer("n_zen");

		//Set weatherreader parameters
		set_unit_value_ssc_string( weather, "file_name" );
		set_unit_value( weather, "track_mode", 1.0 );
		set_unit_value( weather, "tilt", 0.0 );
		set_unit_value( weather, "azimuth", 0.0 );

		// Set solarfield (type 221) parameters
		set_unit_value_ssc_matrix(hel_field, "eta_map");
		set_unit_value_ssc_double(hel_field, "n_zen"); // 8
		set_unit_value_ssc_double(hel_field, "n_azi"); // 13
		set_unit_value_ssc_double(hel_field, "n_hel"); // 8929
		set_unit_value_ssc_double(hel_field, "q_start");
		set_unit_value_ssc_double(hel_field, "p_run");
		set_unit_value_ssc_double(hel_field, "v_wind_max");
		set_unit_value_ssc_double(hel_field, "hel_stow_deploy");

		// Heliostat field (type 221) inputs
		bool bConnected = connect(weather, "wspd", hel_field, "vwind");
		bConnected = connect(weather, "solzen", hel_field, "theta");
		bConnected = connect(weather, "solazi", hel_field, "phi");
		set_unit_value(hel_field, "field_control", 1.0);

		// Receiver (type 222) parameters
		set_unit_value_ssc_double(receiver, "N_panels");
		set_unit_value_ssc_double(receiver, "D_rec");
		set_unit_value_ssc_double(receiver, "H_rec");
		set_unit_value_ssc_double(receiver, "THT");
		set_unit_value_ssc_double(receiver, "d_tube_out");
		set_unit_value_ssc_double(receiver, "th_tube");
		set_unit_value_ssc_double(receiver, "mat_tube");
		set_unit_value_ssc_double(receiver, "rec_htf");
		set_unit_value_ssc_matrix(receiver, "field_fl_props");
		set_unit_value_ssc_double(receiver, "Flow_type");
		set_unit_value_ssc_double(receiver, "epsilon");
		set_unit_value_ssc_double(receiver, "hl_ffact");
		set_unit_value_ssc_double(receiver, "T_htf_hot_des");
		set_unit_value_ssc_double(receiver, "T_htf_cold_des");
		set_unit_value_ssc_double(receiver, "f_rec_min");
		set_unit_value_ssc_double(receiver, "Q_rec_des");
		set_unit_value_ssc_double(receiver, "rec_su_delay");
		set_unit_value_ssc_double(receiver, "rec_qf_delay");
		set_unit_value_ssc_double(receiver, "m_dot_htf_max");
		set_unit_value_ssc_double(receiver, "A_sf");
		set_unit_value(receiver, "is_direct_iscc", 1.0);
		set_unit_value_ssc_double(receiver, "cycle_config");
		set_unit_value_ssc_matrix(receiver, "fluxmap_angles");
		set_unit_value_ssc_matrix(receiver, "fluxmap");

		// Connect Receiver (type 222) inputs
		bConnected = connect(weather, "solazi", receiver, "azimuth");
		bConnected = connect(weather, "solzen", receiver, "zenith");
		set_unit_value(receiver, "T_salt_hot_target", as_double("T_htf_hot_des"));	
		bConnected = connect(iscc_pb, "T_htf_cold", receiver, "T_salt_cold");
		bConnected = connect(weather, "wspd", receiver, "V_wind_10");
		bConnected = connect(weather, "pres", receiver, "P_amb");
		set_unit_value_ssc_double(receiver, "eta_pump");
		bConnected = connect(weather, "tdew", receiver, "T_dp");
		bConnected = connect(weather, "beam", receiver, "I_bn");
		bConnected = connect(hel_field, "eta_field", receiver, "field_eff");
		bConnected = connect(weather, "tdry", receiver, "T_db");
		set_unit_value(receiver, "night_recirc", 0.0);							// Hardcoded - night_recirc not set in or passed from UI
		set_unit_value_ssc_double(receiver, "hel_stow_deploy");

		// Set necessary receiver initial values
		set_unit_value(receiver, "T_salt_cold", as_double("T_htf_cold_des"));			

		// Set NGCC Parameters
		set_unit_value(iscc_pb, "HTF_code", as_double("rec_htf"));						
		set_unit_value_ssc_matrix(iscc_pb, "User_htf_props");
		set_unit_value(iscc_pb, "Q_sf_des", as_double("Q_rec_des"));
		set_unit_value_ssc_double(iscc_pb, "plant_elevation");
		set_unit_value(iscc_pb, "cycle_config", as_double("cycle_config"));
		set_unit_value_ssc_double(iscc_pb, "hot_side_delta_t");
		set_unit_value_ssc_double(iscc_pb, "pinch_point");

		// Connect NGCC Inputs
		bConnected = connect(weather, "tdry", iscc_pb, "T_amb");
		bConnected = connect(weather, "pres", iscc_pb, "P_amb");
		bConnected = connect(receiver, "m_dot_salt_tot", iscc_pb, "m_dot_ms_ss");
		bConnected = connect(receiver, "q_dot_ss", iscc_pb, "q_dot_rec_ss");
		bConnected = connect(receiver, "T_salt_cold", iscc_pb, "T_rec_in");
		bConnected = connect(receiver, "T_salt_hot", iscc_pb, "T_rec_out");
		//bConnected = connect(receiver, "f_timestep", iscc_pb, "f_timestep");		// 12/5/13, twn: input removed from cpp code

		// Set ISCC Parasitic Parameters
		set_unit_value_ssc_double(iscc_parasitics, "W_htf_pc_pump");
		set_unit_value_ssc_double(iscc_parasitics, "Piping_loss");
		set_unit_value_ssc_double(iscc_parasitics, "Piping_length");
		set_unit_value_ssc_double(iscc_parasitics, "Q_sf_des");
		set_unit_value_ssc_double(iscc_parasitics, "pb_fixed_par");
		set_unit_value_ssc_double(iscc_parasitics, "bop_par");
		set_unit_value_ssc_double(iscc_parasitics, "bop_par_f");
		set_unit_value_ssc_double(iscc_parasitics, "bop_par_0");
		set_unit_value_ssc_double(iscc_parasitics, "bop_par_1");
		set_unit_value_ssc_double(iscc_parasitics, "bop_par_2");
		set_unit_value_ssc_double(iscc_parasitics, "W_dot_fossil_des");
		set_unit_value_ssc_double(iscc_parasitics, "W_dot_solar_des");

		// Connect ISCC Parasitic Inputs
		bConnected = connect(hel_field, "pparasi", iscc_parasitics, "W_dot_tracking");
		bConnected = connect(receiver, "W_dot_pump", iscc_parasitics, "W_dot_rec_pump");
		bConnected = connect(receiver, "m_dot_ss", iscc_parasitics, "m_dot_htf_ss");
		bConnected = connect(iscc_pb, "W_dot_pc_hybrid", iscc_parasitics, "W_dot_pc_hybrid");
		bConnected = connect(iscc_pb, "W_dot_pc_fossil", iscc_parasitics, "W_dot_pc_fossil");
		bConnected = connect(receiver, "f_timestep", iscc_parasitics, "f_timestep");
		bConnected = connect(receiver, "q_dot_ss", iscc_parasitics, "q_solar_ss");
		bConnected = connect(iscc_pb, "q_dot_fuel", iscc_parasitics, "q_dot_fuel");

		// check if all connections worked
		if ( !bConnected )
			throw exec_error( "tcs_iscc", util::format("there was a problem connecting outputs of one unit to inputs of another for the simulation.") );

		// Run simulation
		size_t hours = 8760;
		if (0 > simulate(3600, hours*3600, 3600) )
			throw exec_error( "tcs_iscc", util::format("there was a problem simulating in tcs_iscc.") );

		// get the outputs
		if (!set_all_output_arrays() )
			throw exec_error( "tcs_iscc", util::format("there was a problem returning the results from the simulation.") );

	}

};

DEFINE_TCS_MODULE_ENTRY( tcsiscc, "Triple pressure NGCC integrated with MS power tower", 4 )
