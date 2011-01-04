#include "cmod_trnbase.h"

static var_info _cm_vtab_trndish[] = {
/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP                  REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/	
	{ SSC_INPUT,        SSC_STRING,      "weather_file",            "Weather data file (TM2,TM2,EPW)",   "",       "",                      "Dish Stirling",       "*",                    "",                              "" },

	// solar field  
	{ SSC_INPUT,        SSC_NUMBER,      "num_coll_ns",             "Number of collectors, North-South", "",       "",                      "Dish Stirling",       "*",                    "POSITIVE,INTEGER",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "num_coll_ew",             "Number of collectors, East-West",   "",       "",                      "Dish Stirling",       "*",                    "POSITIVE,INTEGER",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sep_coll_ns",             "Collector separation, North-South", "m",      "",                      "Dish Stirling",       "*",                    "MIN=0",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sep_coll_ew",             "Collector separation, East-West",   "m",      "",                      "Dish Stirling",       "*",                    "MIN=0",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "wind_stow",               "Wind stow speed",                   "m/s",    "",                      "Dish Stirling",       "*",                    "MIN=0",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "slope_ns",                "Ground slope, North-South",         "deg",    "",                      "Dish Stirling",       "*",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "slope_ew",                "Ground slope, East-West",           "deg",    "",                      "Dish Stirling",       "*",                    "",                              "" },

	// collector
	{ SSC_INPUT,        SSC_NUMBER,      "slot_gap_width",          "Slot gap width",                    "m",      "",                      "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "slot_gap_height",         "Slot gap height",                   "m",      "",                      "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "mirror_reflectivity",     "Mirror reflectivity",               "frac",   "0..1",                  "Dish Stirling",       "*",                    "MIN=0,MAX=1",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "proj_mirror_area",        "Projected mirror area",             "m2",     "",                      "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "total_mirror_area",       "Total mirror area",                 "m2",     "",                      "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "insol_cut_in",            "Insolation cut in",                 "W/m2",   "",                      "Dish Stirling",       "*",                    "MIN=0",                              "" },

	// receiver
	{ SSC_INPUT,        SSC_NUMBER,      "aperture_diam",           "Receiver aperture diameter",        "m",      "",                      "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "absorber_absorp",         "Absorber absorptivity",             "frac",   "",                      "Dish Stirling",       "*",                    "MIN=0,MAX=1",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "absorber_area",           "Absorber area",                     "m2",     "",                      "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cavity_wall_absorp",      "Cavity wall absorptivity",          "frac",   "",                      "Dish Stirling",       "*",                    "MIN=0,MAX=1",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cavity_wall_area",        "Cavity wall area",                  "m2",     "",                      "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "insul_thickness",         "Insulation thickness",              "m",      "",                      "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "insul_conductivity",      "Insulation conductivity",           "W/mK",   "",                      "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cavity_diameter",         "Internal cavity diameter",          "m",      "",                      "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cavity_pressure",         "Internal cavity pressure",          "kPa",    "",                      "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cavity_depth",            "Internal cavity depth",             "m",      "",                      "Dish Stirling",       "*",                    "MIN=0",                              "" },

	// stirling engine
	{ SSC_INPUT,        SSC_NUMBER,      "heater_set_temp",         "Head heater set temp",              "K",      "",                      "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "heater_low_temp",         "Head heater low temp",              "K",      "",                      "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "engine_rpm",              "Engine speed",                      "rpm",    "",                      "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "displacement",            "Displacement volume",               "m3",     "",                      "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "beale_c0",                "Beale Coefficient 0",               "",       "",                      "Dish Stirling",       "*",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "beale_c1",                "Beale Coefficient 1",               "",       "",                      "Dish Stirling",       "*",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "beale_c2",                "Beale Coefficient 2",               "",       "",                      "Dish Stirling",       "*",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "beale_c3",                "Beale Coefficient 3",               "",       "",                      "Dish Stirling",       "*",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "beale_c4",                "Beale Coefficient 4",               "",       "",                      "Dish Stirling",       "*",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pressure_c0",             "Pressure Coefficient 0",            "",       "",                      "Dish Stirling",       "*",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pressure_c1",             "Pressure Coefficient 1",            "",       "",                      "Dish Stirling",       "*",                    "",                              "" },

	// parasitics
	{ SSC_INPUT,        SSC_NUMBER,      "pump_speed",              "Cooling system pump speed",         "rpm",    "",                      "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fan_speed_1",             "Cooling fan speed 1",               "rpm",    "",                      "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fan_speed_2",             "Cooling fan speed 2",               "rpm",    "",                      "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fan_speed_3",             "Cooling fan speed 3",               "rpm",    "",                      "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "temp_speed_2",            "Fluid temp at fan speed 2",         "'C",     "",                      "Dish Stirling",       "*",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "temp_speed_3",            "Fluid temp at fan speed 3",         "'C",     "",                      "Dish Stirling",       "*",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fluid_type",              "Fluid type",                        "",       "0=Water,1=V50%EG,2=V25%EG,3=V40%PG,4=V25%PG",  "Dish Stirling",  "*",  "MIN=0,MAX=4,INTEGER",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "control_power",           "Controller parasitic power",        "W",      "",                      "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cooler_eff",              "Cooler effectiveness",              "frac",   "",                      "Dish Stirling",       "*",                    "MIN=0,MAX=1",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "radiator_eff",            "Radiator effectiveness",            "frac",   "",                      "Dish Stirling",       "*",                    "MIN=0,MAX=1",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "b_cooler",                "Cooler 'b' parameter",              "",       "",                      "Dish Stirling",       "?=0.7",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "b_radiator",              "Radiator 'b' parameter",            "",       "",                      "Dish Stirling",       "?=0.7",                    "",                              "" },
	
	// reference condition parameters
	{ SSC_INPUT,        SSC_NUMBER,      "ref_delta_temp",          "Reference delta temp in receiver",  "K",       "",                     "Dish Stirling",       "*",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ref_aperture_diam",       "Reference aperture diameter",       "m",       "",                     "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ref_intercept_factor",    "Reference intercept factor",        "frac",    "",                     "Dish Stirling",       "*",                    "MIN=0,MAX=1",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ref_focal_length",        "Reference mirror focal length",     "m",       "",                     "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ref_pump_power",          "Reference pump power",              "W",       "",                     "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ref_pump_speed",          "Reference pump speed",              "rpm",     "",                     "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ref_fluid_type",          "Reference fluid type",              "",        "0=Water,1=V50%EG,2=V25%EG,3=V40%PG,4=V25%PG", "Dish Stirling", "*",   "MIN=0,MAX=4,INTEGER",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ref_fluid_temp",          "Reference fluid temp",              "K",       "",                     "Dish Stirling",       "*",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ref_fluid_flow_rate",     "Reference fluid flow rate",         "gal/min", "",                     "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ref_fan_power",           "Reference fan power",               "W",       "",                     "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ref_fan_speed",           "Reference fan speed",               "rpm",     "",                     "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ref_air_density",         "Reference air density",             "kg/m3",   "",                     "Dish Stirling",       "*",                    "MIN=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ref_air_flow_rate",       "Reference air flow rate",           "cfm",     "",                     "Dish Stirling",       "*",                    "MIN=0",                              "" },

	// outputs
	{ SSC_OUTPUT,       SSC_ARRAY,       "e_coll_in",               "Collector energy (in)",             "kWh",     "",                     "Dish Stirling",       "*",                    "",                              "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "e_coll_out",              "Collector energy (out)",            "kWh",     "",                     "Dish Stirling",       "*",                    "",                              "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "e_rec_in",                "Receiver energy (in)",              "kWh",     "",                     "Dish Stirling",       "*",                    "",                              "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "e_rec_out",               "Receiver energy (out)",             "kWh",     "",                     "Dish Stirling",       "*",                    "",                              "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "e_gross",                 "Gross electric generation",         "kWh",     "",                     "Dish Stirling",       "*",                    "",                              "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "e_net",                   "Net electric generation",           "kWh",     "",                     "Dish Stirling",       "*",                    "",                              "" },
	
var_info_invalid };
	
class cm_trndish : public cm_trnbase
{
private:

public:
	cm_trndish() : cm_trnbase()
	{
		add_var_info( _cm_vtab_trndish );
	}

	virtual void write_include_file( FILE *fp ) throw( general_error )
	{
		const char *weather_file = as_string("weather_file");

		std::string annual = work_dir() + util::path_separator() + "dish_annual.out";
		std::string monthly = work_dir() + util::path_separator() + "dish_monthly.out";
		std::string hourly = work_dir() + util::path_separator() + "dish_hourly.out";

		fprintf(fp, "ASSIGN \"%s\" 40\n", annual.c_str());
		fprintf(fp, "ASSIGN \"%s\" 25\n", monthly.c_str());
		fprintf(fp, "ASSIGN \"%s\" 36\n", hourly.c_str());
		fprintf(fp, "ASSIGN \"%s\" 35\n", data_file().c_str());
		fprintf(fp, "ASSIGN \"%s\" 13\n\n", weather_file);
		
		fprintf(fp, "CONSTANTS 18\n");
		fprintf(fp, "d_ap = %lg\n", as_double("aperture_diam"));
		fprintf(fp, "reflectivity = %lg\n", as_double("mirror_reflectivity"));
		fprintf(fp, "num_collect = %d\n", as_integer("num_coll_ns")*as_integer("num_coll_ew"));
		fprintf(fp, "NNS = %d\n", as_integer("num_coll_ns"));
		fprintf(fp, "NEW = %d\n", as_integer("num_coll_ew"));
		fprintf(fp, "NS_dish_sep = %lg\n", as_double("sep_coll_ns"));
		fprintf(fp, "EW_dish_sep = %lg\n", as_double("sep_coll_ew"));
		fprintf(fp, "wind_stow_speed = %lg\n", as_double("wind_stow"));
		fprintf(fp, "slope_NS = %lg\n", as_double("slope_ns"));
		fprintf(fp, "slope_EW = %lg\n", as_double("slope_ew"));
		fprintf(fp, "width_slot_gap = %lg\n", as_double("slot_gap_width"));
		fprintf(fp, "height_slot_gap = %lg\n", as_double("slot_gap_height"));
		fprintf(fp, "proj_area = %lg\n", as_double("proj_mirror_area"));
		fprintf(fp, "total_area = %lg\n", as_double("total_mirror_area"));
		fprintf(fp, "d_ap_TEST = %lg\n", as_double("ref_aperture_diam"));
		fprintf(fp, "test_intercept_f = %lg\n", as_double("ref_intercept_factor"));
		fprintf(fp, "test_focal_length = %lg\n", as_double("ref_focal_length"));
		fprintf(fp, "i_cut_in = %lg\n", as_double("insol_cut_in"));
		fprintf(fp, "\n");

		fprintf(fp, "CONSTANTS 2\n");
		fprintf(fp, "TRAKMODE = 4\n");
		fprintf(fp, "WFType = %d\n", weather_file_type(weather_file));
		fprintf(fp, "\n");

		fprintf(fp, "CONSTANTS 13\n");
		fprintf(fp, "rec_type = 1\n"); // always DIR
		fprintf(fp, "trans_cover = 1\n"); 
		fprintf(fp, "alpha_absorb = %lg\n", as_double("absorber_absorp"));
		fprintf(fp, "A_absorb = %lg\n", as_double("absorber_area"));
		fprintf(fp, "alpha_wall = %lg\n", as_double("cavity_wall_absorp"));
		fprintf(fp, "A_wall = %lg\n", as_double("cavity_wall_area"));
		fprintf(fp, "L_insul = %lg\n", as_double("insul_thickness"));
		fprintf(fp, "k_insul = %lg\n", as_double("insul_conductivity"));
		fprintf(fp, "d_cav = %lg\n", as_double("cavity_diameter"));
		fprintf(fp, "p_cav = %lg\n", as_double("cavity_pressure"));
		fprintf(fp, "l_cav = %lg\n", as_double("cavity_depth"));
		fprintf(fp, "delta_T_DIR = %lg\n", as_double("ref_delta_temp"));
		fprintf(fp, "delta_T_reflux = 40\n");
		fprintf(fp, "\n");

		fprintf(fp, "CONSTANTS 11\n");
		fprintf(fp, "T_heater_head_high = %lg\n", as_double("heater_set_temp"));
		fprintf(fp, "T_heater_head_low = %lg\n", as_double("heater_low_temp"));
		fprintf(fp, "Beale_const_coef = %lg\n", as_double("beale_c0"));
		fprintf(fp, "Beale_first_coef = %lg\n", as_double("beale_c1"));
		fprintf(fp, "Beale_square_coef = %lg\n", as_double("beale_c2"));
		fprintf(fp, "Beale_third_coef = %lg\n", as_double("beale_c3"));
		fprintf(fp, "Beale_fourth_coef = %lg\n", as_double("beale_c4"));
		fprintf(fp, "Pressure_coef = %lg\n", as_double("pressure_c0"));
		fprintf(fp, "Pressure_first = %lg\n", as_double("pressure_c1"));
		fprintf(fp, "engine_speed = %lg\n", as_double("engine_rpm"));
		fprintf(fp, "V_displaced = %lg\n", as_double("displacement"));
		fprintf(fp, "\n");

		fprintf(fp, "CONSTANTS 22\n");
		fprintf(fp, "SYS_AVAIL = 1.0\n");
		fprintf(fp, "PUMPSPEED = %lg\n", as_double("pump_speed"));
		fprintf(fp, "FAN_SPD1 = %lg\n", as_double("fan_speed_1"));
		fprintf(fp, "FAN_SPD2 = %lg\n", as_double("fan_speed_2"));
		fprintf(fp, "FAN_SPD3 = %lg\n", as_double("fan_speed_3"));
		fprintf(fp, "T_cool_speed2 = %lg\n", as_double("temp_speed_2"));
		fprintf(fp, "T_cool_speed3 = %lg\n", as_double("temp_speed_3"));
		fprintf(fp, "coolfluid = %d\n", 1+as_integer("fluid_type"));
		fprintf(fp, "P_controls = %lg\n", as_double("control_power"));
		fprintf(fp, "EPS_COOL = %lg\n", as_double("cooler_eff"));
		fprintf(fp, "EPS_RADTR = %lg\n", as_double("radiator_eff"));
		fprintf(fp, "b_radiator = %lg\n", as_double("b_radiator"));
		fprintf(fp, "b_cooler = %lg\n", as_double("b_cooler"));
		fprintf(fp, "TEST_P_pump = %lg\n", as_double("ref_pump_power"));
		fprintf(fp, "TEST_pump_speed = %lg\n", as_double("ref_pump_speed"));
		fprintf(fp, "TEST_coolfluid = %d\n", 1+as_integer("ref_fluid_type"));
		fprintf(fp, "TEST_T_fluid = %lg\n", as_double("ref_fluid_temp"));
		fprintf(fp, "TEST_V_dot_Fluid = %lg\n", as_double("ref_fluid_flow_rate"));
		fprintf(fp, "TEST_P_fan = %lg\n", as_double("ref_fan_power"));
		fprintf(fp, "TEST_fan_speed = %lg\n", as_double("ref_fan_speed"));
		fprintf(fp, "TEST_fan_rho_air = %lg\n", as_double("ref_air_density"));
		fprintf(fp, "TEST_fan_CFM = %lg\n", as_double("ref_air_flow_rate"));
		fprintf(fp, "\n");

	}

	virtual void process_outputs() throw( general_error )
	{
		std::string hourly = work_dir() + util::path_separator() + "dish_hourly.out";

		output d;
		if (!d.read( hourly.c_str() )) throw general_error("could not read hourly output file: " + hourly);
		
		update("Saving data...", 99.5);

		save_column(d, "Power_in_Coll_Field", "e_coll_in", 1000.0, 8760);
		save_column(d, "Power_out_Coll_Field", "e_coll_out", 1000.0, 8760);
		save_column(d, "Power_in_Receiver_Field", "e_rec_in", 1000.0, 8760);
		save_column(d, "Receiver_Power_Out_Field", "e_rec_out", 1000.0, 8760);
		save_column(d, "Gross_Field_Power", "e_gross", 1000.0, 8760);
		save_column(d, "Net_Field_Power", "e_net", 1000.0, 8760);

		//if (!util::remove_file( hourly.c_str() ))
			//log("failed to delete hourly file: " + hourly);
	}

	virtual const char *deck_name()
	{
		return "csp_dish";
	}
};

DEFINE_MODULE_ENTRY( trndish, "Dish stirling simulator (TRNSYS)", 5 )
