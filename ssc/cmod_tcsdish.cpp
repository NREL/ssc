#include "core.h"
#include "tckernel.h"

static var_info _cm_vtab_tcsdish[] = {
//   weather reader inputs
//   VARTYPE            DATATYPE          NAME                      LABEL                                                                             UNITS           META            GROUP            REQUIRED_IF                CONSTRAINTS              UI_HINTS
    { SSC_INPUT,        SSC_STRING,      "file_name",               "local weather file path",                                                        "",             "",             "Weather",       "*",                       "LOCAL_FILE",            "" },
    { SSC_INPUT,        SSC_NUMBER,      "track_mode",              "Tracking mode",                                                                  "",             "",             "Weather",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tilt",                    "Tilt angle of surface/axis",                                                     "",             "",             "Weather",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "azimuth",                 "Azimuth angle of surface/axis",                                                  "",             "",             "Weather",       "*",                       "",                      "" },


	// sam_pf_dish_collector_type295_variables
	//PARAMETERS
    { SSC_INPUT,        SSC_NUMBER,      "d_ap",                    "Dish aperture diameter",                                                         "m",            "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rho",                     "Mirror surface reflectivity",                                                    "-",            "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_ns",                    "Number of collectors North-South",                                               "-",            "",             "type295",       "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_ew",                    "Number of collectors East-West",                                                 "-",            "",             "type295",       "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "ns_dish_sep",             "Collector separation North-South",                                               "m",            "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "ew_dish_sep",             "Collector separation East-West",                                                 "m",            "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "slope_ns",                "North-South ground slope",                                                       "%",            "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "slope_ew",                "East-West ground slope",                                                         "%",            "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "w_slot_gap",              "Slot gap width",                                                                 "m",            "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "h_slot_gap",              "Slot gap height",                                                                "m",            "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "manufacturer",            "Dish manufacturer (fixed as 5 = other)",                                         "-",            "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "wind_stow_speed",         "Wind stow speed",                                                                "m/s",          "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "A_proj",                  "Projected mirror area",                                                          "m^2",          "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "I_cut_in",                "Insolation cut in value",                                                        "W/m^2",        "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "d_ap_test",               "Receiver aperture diameter during test",                                         "m",            "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "test_if",                 "Test intercept factor",                                                          "-",            "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "test_L_focal",            "Focal length of mirror system",                                                  "m",            "",             "type295",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "A_total",                 "Total Area",                                                                     "m^2",          "",             "type295",       "*",                       "",                      "" },
	// INPUTS
  //{ SSC_INPUT,        SSC_NUMBER,      "I_beam",                  "Direct normal radiation",                                                        "kJ/hr-m2",     "",             "type295",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_amb",                   "Dry bulb temperature",                                                           "C",            "",             "type295",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "wind_speed",              "Wind velocity",                                                                  "m/s",          "",             "type295",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "zenith",                  "Solar zenith angle",                                                             "deg",          "",             "type295",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "P_atm",                   "Atmospheric pressure",                                                           "Pa",           "",             "type295",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "azimuth",                 "Solar azimuth angle",                                                            "deg",          "",             "type295",       "*",                       "",                      "" },

	// sam_pf_dish_receiver_type296_variables
	//PARAMETERS
    { SSC_INPUT,        SSC_NUMBER,      "rec_type",                "Receiver type (always = 1)",                                                     "-",            "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "transmittance_cover",     "Transmittance cover (always = 1)",                                               "-",            "",             "type296",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "manufacturer",            "Manufacturer (always=5)",                                                        "-",            "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "alpha_absorber",          "Absorber absorptance",                                                           "-",            "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "A_absorber",              "Absorber surface area",                                                          "m^2",          "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "alpha_wall",              "Cavity absorptance",                                                             "-",            "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "A_wall",                  "Cavity surface area",                                                            "m^2",          "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "L_insulation",            "Insulation thickness",                                                           "m",            "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "k_insulation",            "Insulation thermal conductivity",                                                "W/m-K",        "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "d_cav",                   "Internal diameter of cavity perp to aperture",                                   "m",            "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cav",                   "Internal cavity pressure with aperture covered",                                 "kPa",          "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "L_cav",                   "Internal depth of cavity perp to aperture",                                      "m",            "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "DELTA_T_DIR",             "Delta temperature for DIR receiver",                                             "K",            "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "DELTA_T_REFLUX",          "Delta temp for REFLUX receiver (always = 40)",                                   "K",            "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_heater_head_high",      "Heater Head Set Temperature",                                                    "K",            "",             "type296",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_heater_head_low",       "Header Head Lowest Temperature",                                                 "K",            "",             "type296",       "*",                       "",                      "" },
	// INPUTS
  //{ SSC_INPUT,        SSC_NUMBER,      "Power_in_rec",            "Power entering the receiver from the collector",                                 "kW",           "",             "type296",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_amb",                   "Ambient temperature in Kelvin",                                                  "K",            "",             "type296",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "P_atm",                   "Atmospheric pressure",                                                           "Pa",           "",             "type296",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "wind_speed",              "Wind velocity",                                                                  "m/s",          "",             "type296",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "sun_angle",               "Solar altitude angle",                                                           "deg",          "",             "type296",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "n_collectors",            "Total number of collectors (Num N-S x Num E-W)",                                 "-",            "",             "type296",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "DNI",                     "Direct normal radiation",                                                        "W/m^2",        "",             "type296",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "I_cut_in",                "The cut-in DNI value used in the simulation",                                    "W/m^2",        "",             "type296",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "d_ap",                    "The aperture diameter used in the simulation",                                   "m",            "",             "type296",       "*",                       "",                      "" },

	// sam_pf_dish_engine_type297_variables
	//PARAMETERS
  //{ SSC_INPUT,        SSC_NUMBER,      "manufacturer",            "Manufacturer (fixed as 5)",                                                      "-",            "",             "type297",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_heater_head_high",      "Heater Head Set Temperature",                                                    "K",            "",             "type297",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_heater_head_low",       "Header Head Lowest Temperature",                                                 "K",            "",             "type297",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Beale_const_coef",        "Beale Constant Coefficient",                                                     "-",            "",             "type297",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Beale_first_coef",        "Beale first-order coefficient",                                                  "1/W",          "",             "type297",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Beale_square_coef",       "Beale second-order coefficient",                                                 "1/W^2",        "",             "type297",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Beale_third_coef",        "Beale third-order coefficient",                                                  "1/W^3",        "",             "type297",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Beale_fourth_coef",       "Beale fourth-order coefficient",                                                 "1/W^4",        "",             "type297",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Pressure_coef",           "Pressure constant coefficient",                                                  "MPa",          "",             "type297",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Pressure_first",          "Pressure first-order coefficient",                                               "MPa/W",        "",             "type297",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "engine_speed",            "Engine operating speed",                                                         "rpm",          "",             "type297",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_displaced",             "Displaced engine volume",                                                        "m3",           "",             "type297",       "*",                       "",                      "" },
	// INPUTS
  //{ SSC_INPUT,        SSC_NUMBER,      "P_SE",                    "Receiver output power",                                                          "kW",           "",             "type297",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_amb",                   "Ambient temperature in Kelvin",                                                  "K",            "",             "type297",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "N_cols",                  "Number of collectors",                                                           "-",            "",             "type297",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_compression_in",        "Receiver efficiency",                                                            "C",            "",             "type297",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_heater_head_operate",   "Receiver head operating temperature",                                            "K",            "",             "type297",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "P_in_collector",          "Power incident on the collector",                                                "kW",           "",             "type297",       "*",                       "",                      "" },

	//sam_pf_dish_parasitics_type298_variables
	//PARAMETERS
    { SSC_INPUT,        SSC_NUMBER,      "cooling_tower_on",        "Option to use a cooling tower (set to 0=off)",                                   "-",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tower_mode",              "Cooling tower type (natural or forced draft)",                                   "-",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "d_pipe_tower",            "Runner pipe diameter to the cooling tower (set to 0.4m)",                        "m",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tower_m_dot_water",       "Tower cooling water flow rate (set to 134,000 kg/hr)",                           "kg/s",         "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tower_m_dot_water_test",  "Test value for the cooling water flow rate (set to 134,000 kg/hr)",              "kg/s",         "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tower_pipe_material",     "Tower pipe material (1=plastic, 2=new cast iron, 3=riveted steel)",              "-",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_tower_pump",          "Tower pump efficiency (set to 0.6)",                                             "-",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fan_control_signal",      "Fan control signal (set to 1, not used in this model)",                          "-",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "epsilon_power_test",      "Test value for cooling tower effectiveness (set to 0.7)",                        "-",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "system_availability",     "System availability (set to 1.0)",                                               "-",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pump_speed",              "Reference Condition Pump Speed",                                                 "rpm",          "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fan_speed1",              "Cooling system fan speed 1",                                                     "rpm",          "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fan_speed2",              "Cooling system fan speed 2",                                                     "rpm",          "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fan_speed3",              "Cooling system fan speed 3",                                                     "rpm",          "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_cool_speed2",           "Cooling Fluid Temp. For Fan Speed 2 Cut-In",                                     "C",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_cool_speed3",           "Cooling Fluid Temp. For Fan Speed 3 Cut-In",                                     "C",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "epsilon_cooler_test",     "Cooler effectiveness",                                                           "-",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "epsilon_radiator_test",   "Radiator effectiveness",                                                         "-",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cooling_fluid",           "Reference Condition Cooling Fluid: 1=Water,2=V50%EG,3=V25%EG,4=V40%PG,5=V25%PG", "-",            "",             "type298",       "*",                       "INTEGER",               "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "manufacturer",            "Manufacturer (fixed as 5=other)",                                                "-",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_controls",              "Control System Parasitic Power, Avg.",                                           "W",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "test_P_pump",             "Reference Condition Pump Parasitic Power",                                       "W",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "test_pump_speed",         "Reference Condition Pump Speed",                                                 "rpm",          "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "test_cooling_fluid",      "Reference Condition Cooling Fluid",                                              "-",            "",             "type298",       "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "test_T_fluid",            "Reference Condition Cooling Fluid Temperature",                                  "K",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "test_V_dot_fluid",        "Reference Condition Cooling Fluid Volumetric Flow Rate",                         "gpm",          "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "test_P_fan",              "Reference Condition Cooling System Fan Power",                                   "W",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "test_fan_speed",          "Reference Condition Cooling System Fan Speed",                                   "rpm",          "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "test_fan_rho_air",        "Reference condition fan air density",                                            "kg/m^3",       "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "test_fan_cfm",            "Reference condition van volumentric flow rate",                                  "cfm",          "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "b_radiator",              "b_radiator parameter",                                                           "-",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "b_cooler",                "b_cooler parameter",                                                             "-",            "",             "type298",       "*",                       "",                      "" },
	// INPUTS
  //{ SSC_INPUT,        SSC_NUMBER,      "gross_power",             "Stirling engine gross output",                                                   "kW",           "",             "type298",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_amb",                   "Ambient temperature in Kelvin",                                                  "K",            "",             "type298",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "N_cols",                  "Number of collectors",                                                           "none",         "",             "type298",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "DNI",                     "Direct normal radiation (not interpolated)",                                     "W/m2",         "",             "type298",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_heater_head_low",       "Header Head Lowest Temperature",                                                 "K",            "",             "type298",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "V_swept",                 "Displaced engine volume",                                                        "cm3",          "",             "type298",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "frequency",               "Engine frequency (= RPM/60s)",                                                   "1/s",          "",             "type298",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "engine_pressure",         "Engine pressure",                                                                "Pa",           "",             "type298",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "I_cut_in",                "Cut in DNI value used in the simulation",                                        "W/m2",         "",             "type298",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "Q_reject",                "Stirling engine losses",                                                         "W",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Tower_water_outlet_temp", "Tower water outlet temperature (set to 20)",                                       "C",          "",             "type298",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "P_amb_Pa",                "Atmospheric pressure",                                                           "Pa",           "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "ns_dish_separation",      "North-South dish separation used in the simulation",                             "m",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "ew_dish_separation",      "East-West dish separation used in the simulation",                               "m",            "",             "type298",       "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_tower_fan",             "Tower fan power (set to 0)",                                                     "kJ/hr",        "",             "type298",       "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "power_in_collector",      "Power incident on the collector",                                                "kW",           "",             "type298",       "*",                       "",                      "" },



// OUTPUTS
// The names of the output variables should match the parameter names for the TCS units in order to signal the TCS kernel to store the values by timestep

//   VARTYPE            DATATYPE          NAME                LABEL                                                             UNITS           META            GROUP            REQUIRED_IF                CONSTRAINTS              UI_HINTS
    { SSC_OUTPUT,       SSC_ARRAY,       "net_power",         "Net system output power",                                        "kW",           "",             "Outputs",        "*",                      "LENGTH=8760",           "" },

	{ SSC_OUTPUT, SSC_ARRAY, "tdry", "Dry bulb temperature", "C", "", "Outputs", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "wspd", "Wind speed", "m/s", "", "Outputs", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "beam", "Beam normal irradiance", "W/m2", "", "Outputs", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "Power_in_collector", "Power incident on the collector", "kW", "", "Outputs", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "Power_out_col", "Total power from the collector dish", "kW", "", "Outputs", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "Power_in_rec", "Power entering the receiver from the collector", "kW", "", "Outputs", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "P_out_rec", "Receiver output power", "kW", "", "Outputs", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "P_out_SE", "Stirling engine gross output", "kW", "", "Outputs", "*", "LENGTH=8760", "" },
  //{ SSC_OUTPUT, SSC_ARRAY, "net_power", "Net system output power", "kW", "", "Outputs", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "eta_net", "Net system efficiency", "-", "", "Outputs", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "Phi_shade", "Dish-to-dish shading performance factor", "-", "", "Outputs", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "Collector_Losses", "Total collector losses (Incident - P_out)", "kW", "", "Outputs", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "eta_collector", "Collector efficiency", "-", "", "Outputs", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "Q_rec_losses", "Receiver thermal losses", "kW", "", "Outputs", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "eta_rec", "Receiver efficiency", "-", "", "Outputs", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "T_heater_head_operate", "Receiver head operating temperature", "K", "", "Outputs", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "P_SE_losses", "Stirling engine losses", "-", "", "Outputs", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "eta_SE", "Stirling engine efficiency", "-", "", "Outputs", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "engine_pressure", "Engine pressure", "Pa", "", "Outputs", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "P_parasitic", "Total parasitic power load", "W", "", "Outputs", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "T_compression", "Cold sink temperature / compression temperature", "K", "", "Outputs", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "T_tower_out", "Cooling fluid temperature into the cooler and out of the tower", "C", "", "Outputs", "*", "LENGTH=8760", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "T_tower_in", "Cooling fluid temperature out of the cooling and into the tower", "C", "", "Outputs", "*", "LENGTH=8760", "" },
	var_info_invalid };



class cm_tcsdish : public tcKernel
{
public:

	cm_tcsdish(tcstypeprovider *prov)
	:tcKernel(prov)
	{
		add_var_info( _cm_vtab_tcsdish );
		//set_store_all_parameters(true); // default is 'false' = only store TCS parameters that match the SSC_OUTPUT variables above
	}

	void exec( ) throw( general_error )
	{
		//if ( 0 >= load_library("typelib") ) throw exec_error( "tcsdish", util::format("could not load the tcs type library.") );

		bool debug_mode = true; //(__DEBUG__ == 1);  // When compiled in VS debug mode, this will use the trnsys weather file; otherwise, it will attempt to open the file with name that was passed in
		//Add weather file reader unit
		int weather = 0;
		if(debug_mode) weather = add_unit("trnsys_weatherreader", "TRNSYS weather reader");
		else weather = add_unit("weatherreader", "TCS weather reader");

		//Add units
		int collector = add_unit("sam_pf_dish_collector_type295");
		int receiver = add_unit("sam_pf_dish_receiver_type296");
		int engine = add_unit("sam_pf_dish_engine_type297");
		int parasitics = add_unit("sam_pf_dish_parasitics_type298");

		// set weather reader parameters/inputs
		if(debug_mode)
		{
			set_unit_value( weather, "file_name", "C:/svn_NREL/main/ssc/tcsdata/typelib/TRNSYS_weather_outputs/tucson_trnsys_weather.out" );
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

		// Set Collector Parameters
		set_unit_value_ssc_double( collector, "d_ap"); //  d_ap );
		set_unit_value_ssc_double( collector, "rho"); //  reflectivity );
		set_unit_value_ssc_double( collector, "n_ns"); //  NNS );
		set_unit_value_ssc_double( collector, "n_ew"); //  NEW );
		set_unit_value_ssc_double( collector, "ns_dish_sep"); //  NS_dish_sep );
		set_unit_value_ssc_double( collector, "ew_dish_sep"); //  EW_dish_sep );
		set_unit_value_ssc_double( collector, "slope_ns"); //  slope_NS );
		set_unit_value_ssc_double( collector, "slope_ew"); //  slope_EW );
		set_unit_value_ssc_double( collector, "w_slot_gap"); //  width_slot_gap );
		set_unit_value_ssc_double( collector, "h_slot_gap"); //  height_slot_gap );
		set_unit_value_ssc_double( collector, "manufacturer"); //  5 );
		set_unit_value_ssc_double( collector, "wind_stow_speed"); //  wind_stow_speed );
		set_unit_value_ssc_double( collector, "A_proj"); //  proj_area );
		set_unit_value_ssc_double( collector, "I_cut_in"); //  i_cut_in );
		set_unit_value_ssc_double( collector, "d_ap_test"); //  d_ap_TEST );
		set_unit_value_ssc_double( collector, "test_if"); //  test_intercept_f );
		set_unit_value_ssc_double( collector, "test_L_focal"); //  test_focal_length );
		set_unit_value_ssc_double( collector, "A_total"); //  total_area );

		// Connect Collector Parameters
		bool bConnected = connect( weather, "beam", collector, "I_beam" );
		bConnected &= connect( weather, "tdry", collector, "T_amb" );
		bConnected &= connect( weather, "wspd", collector, "wind_speed" );
		bConnected &= connect( weather, "solzen", collector, "zenith" );
		bConnected &= connect( weather, "pres", collector, "P_atm" );
		bConnected &= connect( weather, "solazi", collector, "azimuth" );

		// Set Receiver Parameters
		set_unit_value_ssc_double( receiver, "rec_type"); //  rec_type );
		set_unit_value_ssc_double( receiver, "transmittance_cover"); //  trans_cover );
		set_unit_value_ssc_double( receiver, "manufacturer"); //  5 );
		set_unit_value_ssc_double( receiver, "alpha_absorber"); //  alpha_absorb );
		set_unit_value_ssc_double( receiver, "A_absorber"); //  A_absorb );
		set_unit_value_ssc_double( receiver, "alpha_wall"); //  alpha_wall );
		set_unit_value_ssc_double( receiver, "A_wall"); //  A_wall );
		set_unit_value_ssc_double( receiver, "L_insulation"); //  L_insul );
		set_unit_value_ssc_double( receiver, "k_insulation"); //  k_insul );
		set_unit_value_ssc_double( receiver, "d_cav"); //  d_cav );
		set_unit_value_ssc_double( receiver, "P_cav"); //  p_cav );
		set_unit_value_ssc_double( receiver, "L_cav"); //  l_cav );
		set_unit_value_ssc_double( receiver, "DELTA_T_DIR"); //  delta_T_DIR );
		set_unit_value_ssc_double( receiver, "DELTA_T_REFLUX"); //  delta_T_reflux );
		set_unit_value_ssc_double( receiver, "T_heater_head_high"); //  T_heater_head_high );
		set_unit_value_ssc_double( receiver, "T_heater_head_low"); //  T_heater_head_low );

		// Connect Receiver Inputs
		bConnected &= connect( collector, "Power_in_rec", receiver, "Power_in_rec" );
		bConnected &= connect( weather, "tdry", receiver, "T_amb" );
		bConnected &= connect( weather, "pres", receiver, "P_atm" );
		bConnected &= connect( weather, "wspd", receiver, "wind_speed" );
		bConnected &= connect( weather, "solzen", receiver, "sun_angle" );
		bConnected &= connect( collector, "Number_of_collectors", receiver, "n_collectors" );
		bConnected &= connect( weather, "beam", receiver, "DNI" );
		bConnected &= connect( collector, "I_cut_in", receiver, "I_cut_in" );
		bConnected &= connect( collector, "d_ap_out", receiver, "d_ap" );

		// Set Engine Parameters
		set_unit_value_ssc_double( engine, "manufacturer"); //  5 );
		set_unit_value_ssc_double( engine, "T_heater_head_high"); //  T_heater_head_high );
		set_unit_value_ssc_double( engine, "T_heater_head_low"); //  T_heater_head_low );
		set_unit_value_ssc_double( engine, "Beale_const_coef"); //  Beale_const_coef );
		set_unit_value_ssc_double( engine, "Beale_first_coef"); //  Beale_first_coef );
		set_unit_value_ssc_double( engine, "Beale_square_coef"); //  Beale_square_coef );
		set_unit_value_ssc_double( engine, "Beale_third_coef"); //  Beale_third_coef );
		set_unit_value_ssc_double( engine, "Beale_fourth_coef"); //  Beale_fourth_coef );
		set_unit_value_ssc_double( engine, "Pressure_coef"); //  Pressure_coef );
		set_unit_value_ssc_double( engine, "Pressure_first"); //  Pressure_first );
		set_unit_value_ssc_double( engine, "engine_speed"); //  engine_speed );
		set_unit_value_ssc_double( engine, "V_displaced"); //  V_displaced );
		// initial values
		set_unit_value_ssc_double( engine, "T_compression", as_double("T_compression_in") ); //  273.15 );

		// Connect Engine Parameters
		bConnected &= connect( receiver, "P_out_rec", engine, "P_SE" );
		bConnected &= connect( weather, "tdry", engine, "T_amb" );
		bConnected &= connect( collector, "Number_of_collectors", engine, "N_cols" );
		bConnected &= connect( parasitics, "T_compression", engine, "T_compression" );
		bConnected &= connect( receiver, "T_heater_head_operate", engine, "T_heater_head_operate" );
		bConnected &= connect( collector, "Power_in_collector", engine, "P_in_collector" );

		// Set Parasitic Parameters
		set_unit_value_ssc_double( parasitics, "cooling_tower_on"); //  0 );
		set_unit_value_ssc_double( parasitics, "tower_mode"); //  1 );
		set_unit_value_ssc_double( parasitics, "d_pipe_tower"); //  0.4 );
		set_unit_value_ssc_double( parasitics, "tower_m_dot_water"); //  134000 );
		set_unit_value_ssc_double( parasitics, "tower_m_dot_water_test"); //  134000 );
		set_unit_value_ssc_double( parasitics, "tower_pipe_material"); //  1 );
		set_unit_value_ssc_double( parasitics, "eta_tower_pump"); //  0.6 );
		set_unit_value_ssc_double( parasitics, "fan_control_signal"); //  1 );
		set_unit_value_ssc_double( parasitics, "epsilon_power_test"); //  0.7 );
		set_unit_value_ssc_double( parasitics, "system_availability"); //  SYS_AVAIL );
		set_unit_value_ssc_double( parasitics, "pump_speed"); //  PUMPSPEED );
		set_unit_value_ssc_double( parasitics, "fan_speed1"); //  FAN_SPD1 );
		set_unit_value_ssc_double( parasitics, "fan_speed2"); //  FAN_SPD2 );
		set_unit_value_ssc_double( parasitics, "fan_speed3"); //  FAN_SPD3 );
		set_unit_value_ssc_double( parasitics, "T_cool_speed2"); //  T_cool_speed2 );
		set_unit_value_ssc_double( parasitics, "T_cool_speed3"); //  T_cool_speed3 );
		set_unit_value_ssc_double( parasitics, "epsilon_cooler_test"); //  EPS_COOL );
		set_unit_value_ssc_double( parasitics, "epsilon_radiator_test"); //  EPS_RADTR );
		set_unit_value_ssc_double( parasitics, "cooling_fluid"); //  coolfluid );
		set_unit_value_ssc_double( parasitics, "manufacturer"); //  5 );
		set_unit_value_ssc_double( parasitics, "P_controls"); //  P_controls );
		set_unit_value_ssc_double( parasitics, "test_P_pump"); //  TEST_P_pump );
		set_unit_value_ssc_double( parasitics, "test_pump_speed"); //  TEST_pump_speed );
		set_unit_value_ssc_double( parasitics, "test_cooling_fluid"); //  TEST_coolfluid );
		set_unit_value_ssc_double( parasitics, "test_T_fluid"); //  TEST_T_fluid );
		set_unit_value_ssc_double( parasitics, "test_V_dot_fluid"); //  TEST_V_dot_Fluid );
		set_unit_value_ssc_double( parasitics, "test_P_fan"); //  TEST_P_fan );
		set_unit_value_ssc_double( parasitics, "test_fan_speed"); //  TEST_fan_speed );
		set_unit_value_ssc_double( parasitics, "test_fan_rho_air"); //  TEST_fan_rho_air );
		set_unit_value_ssc_double( parasitics, "test_fan_cfm"); //  TEST_fan_CFM );
		set_unit_value_ssc_double( parasitics, "b_radiator"); //  b_radiator );
		set_unit_value_ssc_double( parasitics, "b_cooler"); //  b_cooler );
		// initial values
		set_unit_value_ssc_double( parasitics, "I_cut_in"); //  i_cut_in );
		set_unit_value_ssc_double( parasitics, "Tower_water_outlet_temp"); //  0.0 );
		set_unit_value_ssc_double( parasitics, "ns_dish_separation"); //  NS_dish_sep );
		set_unit_value_ssc_double( parasitics, "ew_dish_separation"); //  NS_dish_sep );
		set_unit_value_ssc_double( parasitics, "P_tower_fan"); //  0.0 );

		// Connect Parasitics Inputs
		bConnected &= connect( engine, "P_out_SE", parasitics, "gross_power" );
		bConnected &= connect( weather, "tdry", parasitics, "T_amb" );
		bConnected &= connect( collector, "Number_of_collectors", parasitics, "N_cols" );
		bConnected &= connect( weather, "beam", parasitics, "DNI" );
		bConnected &= connect( engine, "T_heater_head_low", parasitics, "T_heater_head_low" );
		bConnected &= connect( engine, "V_displaced", parasitics, "V_swept" );
		bConnected &= connect( engine, "frequency", parasitics, "frequency" );
		bConnected &= connect( engine, "engine_pressure", parasitics, "engine_pressure" );
		bConnected &= connect( engine, "P_SE_losses", parasitics, "Q_reject" );
		bConnected &= connect( weather, "pres", parasitics, "P_amb_Pa" );
		bConnected &= connect( collector, "Power_in_collector", parasitics, "power_in_collector" );

		// Example for changing an input variable name in the SSC interface
		// set_unit_value( u3, "m_dot_htf", as_double("m_dot_htf_init") );


		// check if all connections worked
		if ( !bConnected )
			throw exec_error( "tcsdish", util::format("there was a problem connecting outputs of one unit to inputs of another for the simulation.") );

		// Run simulation
		size_t hours = 8760;
		if (0 > simulate(3600, hours*3600, 3600) )
			throw exec_error( "tcsdish", util::format("there was a problem simulating in tcsdish.") );

		// get the outputs
		if (!set_all_output_arrays() )
			throw exec_error( "tcsdish", util::format("there was a problem returning the results from the simulation.") );


		//set_output_array("i_SfTi",8760);
	}

};

DEFINE_TCS_MODULE_ENTRY( tcsdish, "Dish Stirling model using the TCS types.", 4 )

//static compute_module *_create_tcsdish() {
//	extern tcstypeprovider TCSTP;
//	return new cm_tcsdish( &TCSTP, I_TEST );
//}
//module_entry_info cm_entry_tcsdish = { "tcsdish", "rtrrt", 4, _create_tcsdish };
