#include "core.h"
#include "tckernel.h"

static var_info _cm_vtab_tcsgeneric_solar[] = {
//   weather reader inputs
//   VARTYPE            DATATYPE          NAME                LABEL                                                             UNITS           META            GROUP            REQUIRED_IF                CONSTRAINTS              UI_HINTS
    { SSC_INPUT,        SSC_STRING,      "file_name",         "local weather file path",                                        "",             "",             "Weather",       "*",                       "LOCAL_FILE",            "" },
    { SSC_INPUT,        SSC_NUMBER,      "track_mode",        "Tracking mode",                                                  "",             "",             "Weather",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tilt",              "Tilt angle of surface/axis",                                     "",             "",             "Weather",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "azimuth",           "Azimuth angle of surface/axis",                                  "",             "",             "Weather",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "latitude",         "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",             "",             "",             "Weather",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "longitude",        "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",             "",             "",             "Weather",       "*",                       "",                      "" },

//   Generic solar model (type 260) inputs
//   VARTYPE            DATATYPE          NAME                LABEL                                                             UNITS           META            GROUP            REQUIRED_IF                CONSTRAINTS              UI_HINTS
	{ SSC_INPUT,        SSC_MATRIX,      "OpticalTable",     "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "solarfield",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "timezone",         "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "solarfield",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "theta_stow",       "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "solarfield",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "theta_dep",        "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "solarfield",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "interp_arr",       "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "solarfield",    "*",                       "INTEGER",               "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rad_type",         "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "solarfield",    "*",                       "INTEGER",               "" },
	{ SSC_INPUT,        SSC_NUMBER,      "solarm",           "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "solarfield",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "T_sfdes",          "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "solarfield",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "irr_des",          "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "solarfield",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "eta_opt_soil",     "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "solarfield",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "eta_opt_gen",      "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "solarfield",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "f_sfhl_ref",       "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "solarfield",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_ARRAY,       "sfhlQ_coefs",      "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "solarfield",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_ARRAY,       "sfhlT_coefs",      "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "solarfield",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_ARRAY,       "sfhlV_coefs",      "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "solarfield",    "*",                       "",                      "" },

	{ SSC_INPUT,        SSC_NUMBER,      "qsf_des",          "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "powerblock",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "w_des",            "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "powerblock",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "eta_des",          "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "powerblock",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "f_wmax",           "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "powerblock",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "f_wmin",           "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "powerblock",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "f_startup",        "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "powerblock",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "eta_lhv",          "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "powerblock",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_ARRAY,       "etaQ_coefs",       "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "powerblock",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_ARRAY,       "etaT_coefs",       "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "powerblock",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "T_pcdes",          "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "powerblock",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "PC_T_corr",        "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "powerblock",    "*",                       "INTEGER",               "" },
	{ SSC_INPUT,        SSC_NUMBER,      "f_Wpar_fixed",     "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "powerblock",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "f_Wpar_prod",      "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "powerblock",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_ARRAY,       "Wpar_prodQ_coefs", "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "powerblock",    "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_ARRAY,       "Wpar_prodT_coefs", "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "powerblock",    "*",                       "",                      "" },

	{ SSC_INPUT,        SSC_NUMBER,      "hrs_tes",          "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "storage",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "f_charge",         "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "storage",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "f_disch",          "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "storage",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "f_etes_0",         "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "storage",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "f_teshl_ref",      "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "storage",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_ARRAY,       "teshlX_coefs",     "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "storage",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_ARRAY,       "teshlT_coefs",     "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "storage",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ntod",             "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "storage",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_ARRAY,       "tod_sched",        "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "storage",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_ARRAY,       "disws",            "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "storage",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_ARRAY,       "diswos",           "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "storage",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_ARRAY,       "qdisp",            "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "storage",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_ARRAY,       "fdisp",            "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "storage",       "*",                       "",                      "" },

	// initial values
	{ SSC_INPUT,        SSC_NUMBER,      "ibn",              "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "initial",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ibh",              "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "initial",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "itoth",            "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "initial",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tdb",              "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "initial",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "twb",              "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "initial",       "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "vwind",            "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",            "",             "",             "initial",       "*",                       "",                      "" },


// OUTPUTS
// The names of the output variables should match the parameter names for the TCS units in order to signal the TCS kernel to store the values by timestep

//   VARTYPE            DATATYPE          NAME                LABEL                                                             UNITS           META            GROUP            REQUIRED_IF                CONSTRAINTS              UI_HINTS
    { SSC_OUTPUT,       SSC_ARRAY,       "global",            "Global horizontal irradiance",                                   "W/m2",         "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "beam",              "Beam normal irradiance",                                         "W/m2",         "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "diff",              "Diffuse horizontal irradiance",                                  "W/m2",         "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "irr_used",          "Irradiation value used in simulation",                           "W/m2",         "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "tdry",              "Dry bulb temperature",                                           "C",            "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "twet",              "Wet bulb temperature",                                           "C",            "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "wspd",              "Wind velocity",                                                  "m/s",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "hour_of_day",       "Hour of the day",                                                "hour",         "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "day_of_year",       "Day of the year",                                                "day",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "solzen",            "Solar elevation angle",                                          "deg",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "solazi",            "Solar azimuth angle (-180..180, 0deg=South)",                    "deg",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "eta_opt_sf",        "Solar field optical efficiency",                                 "none",         "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "f_sfhl_qdni",       "Solar field load-based thermal loss correction",                 "none",         "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "f_sfhl_tamb",       "Solar field temp.-based thermal loss correction",                "none",         "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "f_sfhl_vwind",      "Solar field wind-based thermal loss correction",                 "none",         "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_hl_sf",           "Solar field thermal losses",                                     "MWt",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_sf",              "Solar field delivered thermal power",                            "MWt",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_inc",             "Qdni - Solar incident energy, before all losses",                "MWt",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_to_pb",           "Thermal energy to the power conversion system",                  "MWt",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_startup",         "Power conversion startup energy",                                "MWt",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_to_tes",          "Thermal energy into storage",                                    "MWt",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_from_tes",        "Thermal energy from storage",                                    "MWt",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "e_in_tes",          "Energy in storage",                                              "MWt-hr",       "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_hl_tes",          "Thermal losses from storage",                                    "MWt",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dump_tesfull",    "Dumped energy  exceeding storage charge level max",              "MWt",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dump_teschg",     "Dumped energy exceeding exceeding storage charge rate",          "MWt",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dump_umin",       "Dumped energy from falling below min. operation fraction",       "MWt",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_dump_tot",        "Total dumped energy",                                            "MWt",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_fossil",          "Thermal energy supplied from aux firing",                        "MWt",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "q_gas",             "Energy content of fuel required to supply Qfos",                 "MWt",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "f_effpc_qtpb",      "Load-based conversion efficiency correction",                    "none",         "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "f_effpc_tamb",      "Temp-based conversion efficiency correction",                    "none",         "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "eta_cycle",         "Adjusted power conversion efficiency",                           "none",         "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "w_gr_solar",        "Power produced from the solar component",                        "MWh",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "w_gr_fossil",       "Power produced from the fossil component",                       "MWh",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "w_gr",              "Total gross power production",                                   "MWh",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "w_par_fixed",       "Fixed parasitic losses",                                         "MWh",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "w_par_prod",        "Production-based parasitic losses",                              "MWh",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "w_par_tot",         "Total parasitic losses",                                         "MWh",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "w_par_online",      "Online parasitics",                                              "MWh",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "w_par_offline",     "Offline parasitics",                                             "MWh",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "enet",              "Net electric output",                                            "MWh",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },



	var_info_invalid };



class cm_tcsgeneric_solar : public tcKernel
{
public:

	cm_tcsgeneric_solar(tcstypeprovider *prov)
	:tcKernel(prov)
	{
		add_var_info( _cm_vtab_tcsgeneric_solar );
		//set_store_all_parameters(true); // default is 'false' = only store TCS parameters that match the SSC_OUTPUT variables above
	}

	void exec( ) throw( general_error )
	{
		//if ( 0 >= load_library("typelib") ) throw exec_error( "tcsgeneric_solar", util::format("could not load the tcs type library.") );

		bool debug_mode = (__DEBUG__ == 1);  // When compiled in VS debug mode, this will use the trnsys weather file; otherwise, it will attempt to open the file with name that was passed in
		//Add weather file reader unit
		int weather = 0;
		if(debug_mode) weather = add_unit("trnsys_weatherreader", "TRNSYS weather reader");
		else weather = add_unit("weatherreader", "TCS weather reader");
		//Add Physical Solar Field Model
		int	gss = add_unit( "sam_mw_gen_type260", "Generic solar model" );

		if(debug_mode)
		{
			set_unit_value( weather, "file_name", "C:/svn_NREL/main/ssc/tcs/typelib/TRNSYS_weather_outputs/tucson_trnsys_weather.out" );
			set_unit_value( weather, "i_hour", "TIME" );
			set_unit_value( weather, "i_month", "month" );
			set_unit_value( weather, "i_day", "day" );
			set_unit_value( weather, "i_global", "GlobalHorizontal" );
			set_unit_value( weather, "i_beam", "DNI" );
			set_unit_value( weather, "i_diff", "DiffuseHorizontal" );
			set_unit_value( weather, "i_tdry", "T_dry" );
			set_unit_value( weather, "i_twet", "T_wet" );
			set_unit_value( weather, "i_tdew", "T_dew" );
			set_unit_value( weather, "i_wspd", "WindSpeed" );
			set_unit_value( weather, "i_wdir", "WindDir" );
			set_unit_value( weather, "i_rhum", "RelHum" );
			set_unit_value( weather, "i_pres", "AtmPres" );
			set_unit_value( weather, "i_snow", "SnowCover" );
			set_unit_value( weather, "i_albedo", "GroundAlbedo" );
			set_unit_value( weather, "i_poa", "POA" );
			set_unit_value( weather, "i_solazi", "Azimuth" );
			set_unit_value( weather, "i_solzen", "Zenith" );
			set_unit_value( weather, "i_lat", "Latitude" );
			set_unit_value( weather, "i_lon", "Longitude" );
			set_unit_value( weather, "i_shift", "Shift" );
		}
		else
		{
			//Set weatherreader parameters
			set_unit_value_ssc_string( weather, "file_name" );
			set_unit_value_ssc_double( weather, "track_mode" );    //, 1 );
			set_unit_value_ssc_double( weather, "tilt" );          //, 0 );
			set_unit_value_ssc_double( weather, "azimuth" );       //, 0 );
		}

		//Set parameters
        set_unit_value_ssc_double(gss, "latitude" ); //, 35);
        set_unit_value_ssc_double(gss, "longitude" ); //, -117);
		set_unit_value_ssc_matrix(gss, "OpticalTable" ); //, opt_data);
        set_unit_value_ssc_double(gss, "timezone" ); //, -8);
        set_unit_value_ssc_double(gss, "theta_stow" ); //, 170);
        set_unit_value_ssc_double(gss, "theta_dep" ); //, 10);
        set_unit_value_ssc_double(gss, "interp_arr" ); //, 1);
        set_unit_value_ssc_double(gss, "rad_type" ); //, 1);
        set_unit_value_ssc_double(gss, "solarm" ); //, solarm);
        set_unit_value_ssc_double(gss, "T_sfdes" ); //, T_sfdes);
        set_unit_value_ssc_double(gss, "irr_des" ); //, irr_des);
        set_unit_value_ssc_double(gss, "eta_opt_soil" ); //, eta_opt_soil);
        set_unit_value_ssc_double(gss, "eta_opt_gen" ); //, eta_opt_gen);
        set_unit_value_ssc_double(gss, "f_sfhl_ref" ); //, f_sfhl_ref);
        set_unit_value_ssc_array(gss, "sfhlQ_coefs" ); //, [1,-0.1,0,0]);
        set_unit_value_ssc_array(gss, "sfhlT_coefs" ); //, [1,0.005,0,0]);
        set_unit_value_ssc_array(gss, "sfhlV_coefs" ); //, [1,0.01,0,0]);
        set_unit_value_ssc_double(gss, "qsf_des" ); //, q_sf);
        set_unit_value_ssc_double(gss, "w_des" ); //, w_gr_des);
        set_unit_value_ssc_double(gss, "eta_des" ); //, eta_cycle_des);
        set_unit_value_ssc_double(gss, "f_wmax" ); //, 1.05);
        set_unit_value_ssc_double(gss, "f_wmin" ); //, 0.25);
        set_unit_value_ssc_double(gss, "f_startup" ); //, 0.2);
        set_unit_value_ssc_double(gss, "eta_lhv" ); //, 0.9);
        set_unit_value_ssc_array(gss, "etaQ_coefs" ); //, [0.9,0.1,0,0,0]);
        set_unit_value_ssc_array(gss, "etaT_coefs" ); //, [1,-0.002,0,0,0]);
        set_unit_value_ssc_double(gss, "T_pcdes" ); //, 21);
        set_unit_value_ssc_double(gss, "PC_T_corr" ); //, 1);
        set_unit_value_ssc_double(gss, "f_Wpar_fixed" ); //, f_Wpar_fixed);
        set_unit_value_ssc_double(gss, "f_Wpar_prod" ); //, f_Wpar_prod);
        set_unit_value_ssc_array(gss, "Wpar_prodQ_coefs" ); //, [1,0,0,0]);
        set_unit_value_ssc_array(gss, "Wpar_prodT_coefs" ); //, [1,0,0,0]);
        set_unit_value_ssc_double(gss, "hrs_tes" ); //, hrs_tes);
        set_unit_value_ssc_double(gss, "f_charge" ); //, 0.98);
        set_unit_value_ssc_double(gss, "f_disch" ); //, 0.98);
        set_unit_value_ssc_double(gss, "f_etes_0" ); //, 0.1);
        set_unit_value_ssc_double(gss, "f_teshl_ref" ); //, 0.35);
        set_unit_value_ssc_array(gss, "teshlX_coefs" ); //, [1,0,0,0]);
        set_unit_value_ssc_array(gss, "teshlT_coefs" ); //, [1,0,0,0]);
        set_unit_value_ssc_double(gss, "ntod" ); //, 9);
        set_unit_value_ssc_array(gss, "disws" ); //, [0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1]);
        set_unit_value_ssc_array(gss, "diswos" ); //, [0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1]);
        set_unit_value_ssc_array(gss, "qdisp" ); //, [1,1,1,1,1,1,1,1,1]);
        set_unit_value_ssc_array(gss, "fdisp" ); //, [0,0,0,0,0,0,0,0,0]);


		//Set the initial values
        set_unit_value_ssc_double(gss, "ibn" ); //, 0.);	//Beam-normal (DNI) irradiation
        set_unit_value_ssc_double(gss, "ibh" ); //, 0.);	//	Beam-horizontal irradiation
        set_unit_value_ssc_double(gss, "itoth" ); //, 0.);	//	Total horizontal irradiation
        set_unit_value_ssc_double(gss, "tdb" ); //, 15.);	//	Ambient dry-bulb temperature
        set_unit_value_ssc_double(gss, "twb" ); //, 10.);	//	Ambient wet-bulb temperature
        set_unit_value_ssc_double(gss, "vwind" ); //, 1.);	//	Wind velocity

		// Connect the units
		bool bConnected = connect(weather, "beam", gss, "ibn");
		bConnected &= connect(weather, "global", gss, "itoth");
		//no ibh?
		bConnected &= connect(weather, "tdry", gss, "tdb");
		bConnected &= connect(weather, "twet", gss, "twb");
		bConnected &= connect(weather, "wspd", gss, "vwind");
		//location
		bConnected &= connect(weather, "lat", gss, "latitude");
		bConnected &= connect(weather, "lon", gss, "longitude");
		bConnected &= connect(weather, "tz", gss, "timezone");

		// Example for changing an input variable name in the SSC interface
		// set_unit_value( u3, "m_dot_htf", as_double("m_dot_htf_init") );


		// check if all connections worked
		if ( !bConnected )
			throw exec_error( "tcsgeneric_solar", util::format("there was a problem connecting outputs of one unit to inputs of another for the simulation.") );

		// Run simulation
		size_t hours = 8760;
		if (0 > simulate(3600, hours*3600, 3600) )
			throw exec_error( "tcsgeneric_solar", util::format("there was a problem simulating in tcsgeneric_solar.") );

		// get the outputs
		if (!set_all_output_arrays() )
			throw exec_error( "tcsgeneric_solar", util::format("there was a problem returning the results from the simulation.") );


		//set_output_array("i_SfTi",8760);
	}

};

DEFINE_TCS_MODULE_ENTRY( tcsgeneric_solar, "Generic CSP model using the generic solar TCS types.", 4 )

//static compute_module *_create_tcsgeneric_solar() {
//	extern tcstypeprovider TCSTP;
//	return new cm_tcsgeneric_solar( &TCSTP, I_TEST );
//}
//module_entry_info cm_entry_tcsgeneric_solar = { "tcsgeneric_solar", "rtrrt", 4, _create_tcsgeneric_solar };
