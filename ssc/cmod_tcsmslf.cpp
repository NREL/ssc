#include "core.h"
#include "tckernel.h"

static var_info _cm_vtab_tcsmslf[] = {
/*	EXAMPLE LINES FOR INPUTS
    { SSC_INPUT,        SSC_NUMBER,      "XXXXXXXXXXXXXX",    "Label",                                                                               "",              "",            "sca",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "INTINTINTINT",      "Label",                                                                               "",              "",            "parasitic",      "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_ARRAY,       "XXXXXXXXXXX",       "Number indicating the receiver type",                                                 "",              "",            "hce",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "XXXXXXXXXXX",       "Label",                                                                               "",              "",            "tes",            "*",                       "",                      "" },
*/

//    VARTYPE           DATATYPE          NAME                 LABEL                                                                                 UNITS            META            GROUP            REQUIRED_IF                 CONSTRAINTS             UI_HINTS
    { SSC_INPUT,        SSC_STRING,      "file_name",         "local weather file path",                                                             "",              "",            "Weather",        "*",                       "LOCAL_FILE",            "" },
    { SSC_INPUT,        SSC_NUMBER,      "track_mode",        "Tracking mode",                                                                       "",              "",            "Weather",        "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tilt",              "Tilt angle of surface/axis",                                                          "",              "",            "Weather",        "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "azimuth",           "Azimuth angle of surface/axis",                                                       "",              "",            "Weather",        "*",                       "",                      "" },

    // TOU
    { SSC_INPUT,        SSC_MATRIX,      "weekday_schedule",  "12x24 Time of Use Values for week days",                                              "",             "",             "tou_translator", "*",                       "",                      "" }, 
    { SSC_INPUT,        SSC_MATRIX,      "weekend_schedule",  "12x24 Time of Use Values for week end days",                                          "",             "",             "tou_translator", "*",                       "",                      "" }, 

	// Type 262 (solar field collector) parameters
	{ SSC_INPUT, 	SSC_NUMBER,	"nMod", 			"Number of collector modules in a loop", 	"none", 	"", 	"", 	"?=16" },
	{ SSC_INPUT, 	SSC_NUMBER,	"nRecVar", 			"Number of receiver variantions", 	"none", 	"", 	"", 	"?=4" },
	{ SSC_INPUT, 	SSC_NUMBER,	"nLoops", 			"Number of loops in the field", 	"none", 	"", 	"", 	"105" },
	{ SSC_INPUT, 	SSC_NUMBER,	"eta_pump", 		"HTF pump efficiency", 	"none", 	"", 	"", 	"0.85" },
	{ SSC_INPUT, 	SSC_NUMBER,	"HDR_rough", 		"Header pipe roughness", 	"m", 	"", 	"", 	"4.57E-05" },
	{ SSC_INPUT, 	SSC_NUMBER,	"theta_stow", 		"stow angle", 	"deg", 	"", 	"", 	"170" },
	{ SSC_INPUT, 	SSC_NUMBER,	"theta_dep", 		"deploy angle", 	"deg", 	"", 	"", 	"10" },
	{ SSC_INPUT, 	SSC_NUMBER,	"FieldConfig", 		"Number of subfield headers", 	"none", 	"", 	"", 	"2" },
	{ SSC_INPUT, 	SSC_NUMBER,	"T_startup", 		"The required temperature of the system before the power block can be switched on", 	"C", 	"", 	"", 	"150" },
	{ SSC_INPUT, 	SSC_NUMBER,	"pb_rated_cap",		"Rated plant capacity", 	"MWe", 	"", 	"", 	"111" },
	{ SSC_INPUT, 	SSC_NUMBER,	"m_dot_htfmin",		"Minimum loop HTF flow rate", 	"kg/s", 	"", 	"", 	"1" },
	{ SSC_INPUT, 	SSC_NUMBER,	"m_dot_htfmax",		"Maximum loop HTF flow rate", 	"kg/s", 	"", 	"", 	"12" },
	{ SSC_INPUT, 	SSC_NUMBER, "T_loop_in_des",	"Design loop inlet temperature", 	"C", 	"", 	"", 	"293" },
	{ SSC_INPUT, 	SSC_NUMBER,	"T_loop_out", 		"Target loop outlet temperature", 	"C", 	"", 	"", 	"500" },
	{ SSC_INPUT, 	SSC_NUMBER,	"Fluid", 			"Field HTF fluid number", 	"none", 	"", 	"", 	"17" },
	{ SSC_INPUT, 	SSC_NUMBER,	"T_field_ini", 		"Initial field temperature", 	"C", 	"", 	"", 	"300" },
	{ SSC_INPUT, 	SSC_MATRIX,	"HTF_data", 		"Fluid property data", 	"none", 	"7 columns (T,Cp,dens,visc,kvisc,cond,h), 	at least 3 rows", 	"", 	"" },
	{ SSC_INPUT, 	SSC_NUMBER,	"T_fp", 			"Freeze protection temperature (heat trace activation temperature)", 	"C", 	"", 	"", 	"260" },
	{ SSC_INPUT, 	SSC_NUMBER,	"I_bn_des", 		"Solar irradiation at design", 	"W/m2", 	"", 	"", 	"950" },
	{ SSC_INPUT, 	SSC_NUMBER,	"V_hdr_max", 		"Maximum HTF velocity in the header at design", 	"m/s", 	"", 	"", 	"3" },
	{ SSC_INPUT, 	SSC_NUMBER,	"V_hdr_min", 		"Minimum HTF velocity in the header at design", 	"m/s", 	"", 	"", 	"2" },
	{ SSC_INPUT, 	SSC_NUMBER,	"Pipe_hl_coef",		"Loss coefficient from the header, 	runner pipe, 	and non-HCE piping", 	"W/m2-K", 	"", 	"", 	"0.45" },
	{ SSC_INPUT, 	SSC_NUMBER,	"SCA_drives_elec", 	"Tracking power, 	in Watts per SCA drive", 	"W/module", 	"", 	"", 	"125" },
	{ SSC_INPUT, 	SSC_NUMBER,	"fthrok", 			"Flag to allow partial defocusing of the collectors", 	"none", 	"", 	"", 	"1" },
	{ SSC_INPUT, 	SSC_NUMBER,	"fthrctrl", 		"Defocusing strategy", 	"none", 	"", 	"", 	"2" },
	{ SSC_INPUT, 	SSC_NUMBER,	"ColAz", 			"Collector azimuth angle", 	"deg", 	"", 	"", 	"0" },
	{ SSC_INPUT, 	SSC_NUMBER,	"solar_mult", 		"Solar multiple", 	"none", 	"", 	"", 	"2" },
	{ SSC_INPUT, 	SSC_NUMBER,	"mc_bal_hot", 		"The heat capacity of the balance of plant on the hot side", 	"kWht/K-MWt", 	"", 	"", 	"0.2" },
	{ SSC_INPUT, 	SSC_NUMBER,	"mc_bal_cold", 		"The heat capacity of the balance of plant on the cold side", 	"kWht/K-MWt", 	"", 	"", 	"0.2" },
	{ SSC_INPUT, 	SSC_NUMBER,	"mc_bal_sca", 		"Non-HTF heat capacity associated with each SCA - per meter basis", 	"Wht/K-m", 	"", 	"", 	"4.5" },

	{ SSC_INPUT, 	SSC_NUMBER,	"opt_model", 		"The optical model (1=Solar position ; 2=Collector incidence table ; 3 = IAM polys)", 	"none", 	"", 	"", 	"1" },
	{ SSC_INPUT, 	SSC_NUMBER,	"A_aperture", 		"Reflective aperture area of the collector", 	"m2", 	"", 	"", 	"470.3" },
	{ SSC_INPUT, 	SSC_NUMBER,	"reflectivity", 	"Solar-weighted mirror reflectivity value ", 	"none", 	"", 	"", 	"0.935" },
	{ SSC_INPUT, 	SSC_NUMBER, "TrackingError", 	"Tracking error derate", 	"none", 	"", 	"", 	"0.994" },
	{ SSC_INPUT, 	SSC_NUMBER,	"GeomEffects", 		"Geometry effects derate", 	"none", 	"", 	"", 	"0.98" },
	{ SSC_INPUT, 	SSC_NUMBER,	"Dirt_mirror", 		"User-defined dirt on mirror derate", 	"none", 	"", 	"", 	"0.95" },
	{ SSC_INPUT, 	SSC_NUMBER,	"Error", 			"User-defined general optical error derate ", 	"none", 	"", 	"", 	"0.99" },
	{ SSC_INPUT, 	SSC_NUMBER,	"L_mod", 			"The length of the collector module", 	"m", 	"", 	"", 	"44.8" },
	{ SSC_INPUT, 	SSC_ARRAY, 	"IAM_T_coefs", 		"Incidence angle modifier coefficients - transversal plane", 	"none", 	"", 	"", 	"0.9896,0.044,-0.0721,-0.2327,0." },
	{ SSC_INPUT, 	SSC_ARRAY, 	"IAM_L_coefs", 		"Incidence angle modifier coefficients - longitudinal plane", 	"none", 	"", 	"", 	"1.0031,-0.2259,0.5368,-1.6434,0.7222" },
	{ SSC_INPUT, 	SSC_MATRIX,	"OpticalTable", 	"Values of the optical efficiency table", 	"none", 	"", 	"", 	"" },

	{ SSC_INPUT, 	SSC_NUMBER,	"rec_model", 		"Receiver model type (1=Polynomial ; 2=Evac tube)", 	"none", 	"", 	"", 	"2" },
	{ SSC_INPUT, 	SSC_ARRAY, 	"HCE_FieldFrac", 	"The fraction of the field occupied by this HCE type ", 	"none", 	"", 	"", 	"0.985,0.01,0.005,0" },
	{ SSC_INPUT, 	SSC_ARRAY, 	"D_abs_in", 		"The inner absorber tube diameter", 	"m", 	"", 	"", 	"0.066,0.066,0.066,0.066" },
	{ SSC_INPUT, 	SSC_ARRAY, 	"D_abs_out", 		"The outer absorber tube diameter", 	"m", 	"", 	"", 	"0.07,0.07,0.07,0.07" },
	{ SSC_INPUT, 	SSC_ARRAY, 	"D_glass_in", 		"The inner glass envelope diameter ", 	"m", 	"", 	"", 	"0.115,0.115,0.115,0.115" },
	{ SSC_INPUT, 	SSC_ARRAY, 	"D_glass_out", 		"The outer glass envelope diameter ", 	"m", 	"", 	"", 	"0.12,0.12,0.12,0.12" },
	{ SSC_INPUT, 	SSC_ARRAY, 	"D_plug", 			"The diameter of the absorber flow plug (optional) ", 	"m", 	"", 	"", 	"0,0,0,0" },
	{ SSC_INPUT, 	SSC_ARRAY, 	"Flow_type", 		"The flow type through the absorber", 	"none", 	"", 	"", 	"1,1,1,1" },
	{ SSC_INPUT, 	SSC_ARRAY, 	"Rough", 			"Roughness of the internal surface ", 	"m", 	"", 	"", 	"4.50E-05,4.50E-05,4.50E-05,4.50E-05" },
	{ SSC_INPUT, 	SSC_ARRAY, 	"alpha_env", 		"Envelope absorptance ", 	"none", 	"", 	"", 	"0.02,0.02,0,0" },
	{ SSC_INPUT, 	SSC_MATRIX, "epsilon_abs_1", 	"Absorber emittance - HCE variation 1", 	"none", 	"", 	"", 	"[100,150,200,250,300,350,400,450,500][0.064,0.0665,0.07,0.0745,0.08,0.0865,0.094,0.1025,0.112]" },
	{ SSC_INPUT, 	SSC_MATRIX, "epsilon_abs_2", 	"Absorber emittance - HCE variation 2", 	"none", 	"", 	"", 	"0,.65" },
	{ SSC_INPUT, 	SSC_MATRIX, "epsilon_abs_3", 	"Absorber emittance - HCE variation 3", 	"none", 	"", 	"", 	"0,.65" },
	{ SSC_INPUT, 	SSC_MATRIX, "epsilon_abs_4", 	"Absorber emittance - HCE variation 4", 	"none", 	"", 	"", 	"0,0" },
	{ SSC_INPUT, 	SSC_ARRAY, 	"alpha_abs", 		"Absorber absorptance ", 	"none", 	"", 	"", 	"0.96,0.96,0.8,0" },
	{ SSC_INPUT, 	SSC_ARRAY, 	"Tau_envelope", 	"Envelope transmittance", 	"none", 	"", 	"", 	"0.963,0.963,1,0" },
	{ SSC_INPUT, 	SSC_ARRAY, 	"epsilon_glass", 	"Glass envelope emissivity", 	"none", 	"", 	"", 	"0.86,0.86,1,0" },
	{ SSC_INPUT, 	SSC_ARRAY, 	"GlazingIntactIn", 	"The glazing intact flag {1=true, 	else=false}", 	"none", 	"", 	"", 	"1,1,0,1" },
	{ SSC_INPUT, 	SSC_ARRAY, 	"P_a", 				"Annulus gas pressure", 	"torr", 	"", 	"", 	"0.0001,750,750,0" },
	{ SSC_INPUT, 	SSC_ARRAY, 	"AnnulusGas", 		"Annulus gas type (1=air, 	26=Ar, 	27=H2)", 	"none", 	"", 	"", 	"27,1,1,27" },
	{ SSC_INPUT, 	SSC_ARRAY, 	"AbsorberMaterial", "Absorber material type", 	"none", 	"", 	"", 	"1,1,1,1" },
	{ SSC_INPUT, 	SSC_ARRAY, 	"Shadowing", 		"Receiver bellows shadowing loss factor", 	"none", 	"", 	"", 	"0.96,0.96,0.96,0.963" },
	{ SSC_INPUT, 	SSC_ARRAY, 	"dirt_env", 		"Loss due to dirt on the receiver envelope", 	"none", 	"", 	"", 	"0.98,0.98,1,0.98" },
	{ SSC_INPUT, 	SSC_ARRAY, 	"Design_loss", 		"Receiver heat loss at design", 	"W/m", 	"", 	"", 	"150,1100,1500,0" },
	{ SSC_INPUT, 	SSC_NUMBER, "L_mod_spacing", 	"Piping distance between sequential modules in a loop", 	"m", 	"", 	"", 	"1" },
	{ SSC_INPUT, 	SSC_NUMBER,	"L_crossover", 		"Length of crossover piping in a loop", 	"m", 	"", 	"", 	"15" },
	{ SSC_INPUT, 	SSC_ARRAY, 	"HL_T_coefs", 		"HTF temperature-dependent heat loss coefficients", 	"W/m-K", 	"", 	"", 	"0.,0.672,0.002556,0.,0." },
	{ SSC_INPUT, 	SSC_ARRAY, 	"HL_w_coefs", 		"Wind-speed-dependent heat loss coefficients", 	"W/m-(m/s)", 	"", 	"", 	"1.,0.,0.,0.,0." },
	{ SSC_INPUT, 	SSC_NUMBER,	"DP_nominal", 		"Pressure drop across a single collector assembly at design", 	"bar", 	"", 	"", 	"2.5" },
	{ SSC_INPUT, 	SSC_ARRAY, 	"DP_coefs", 		"Pressure drop mass flow based part-load curve", 	"none", 	"", 	"", 	"0.,1.,0.,0." },
	{ SSC_INPUT, 	SSC_NUMBER,	"rec_htf_vol", 		"Volume of HTF in a single collector unit per unit aperture area", 	"L/m2-ap", 	"", 	"", 	"1" },
	{ SSC_INPUT, 	SSC_NUMBER,	"T_amb_sf_des", 	"Ambient design-point temperature for the solar field", 	"C", 	"", 	"", 	"25" },
	{ SSC_INPUT, 	SSC_NUMBER,	"V_wind_des", 		"Design-point wind velocity", 	"m/s", 	"", 	"", 	"3.5" },

	// Type 262 inputs
	{ SSC_INPUT,	SSC_NUMBER,	"I_b",				"Direct normal incident solar irradiation",	"kJ/m2-hr",	"",	"",	"" },
	{ SSC_INPUT, 	SSC_NUMBER,	"T_db", 			"Dry bulb air temperature", 	"C", 	"", 	"", 	"" },
	{ SSC_INPUT, 	SSC_NUMBER,	"V_wind", 			"Ambient windspeed ", 	"m/s", 	"", 	"", 	"" },
	{ SSC_INPUT, 	SSC_NUMBER,	"P_amb", 			"Ambient pressure", 	"atm", 	"", 	"", 	"" },
	{ SSC_INPUT, 	SSC_NUMBER,	"T_dp", 			"The dewpoint temperature", 	"C", 	"", 	"", 	"" },
	{ SSC_INPUT, 	SSC_NUMBER,	"T_cold_in", 		"HTF return temperature", 	"C", 	"", 	"", 	"" },
	{ SSC_INPUT, 	SSC_NUMBER,	"m_dot_in", 		"HTF mass flow rate at the inlet ", 	"kg/hr", 	"", 	"", 	"" },
	{ SSC_INPUT, 	SSC_NUMBER,	"defocus", 			"Defocus control ", 	"none", 	"", 	"", 	"" },
	{ SSC_INPUT, 	SSC_NUMBER,	"SolarAz", 			"Solar azimuth angle ", 	"deg", 	"", 	"", 	"" },
	{ SSC_INPUT, 	SSC_NUMBER,	"SolarZen", 		"Solar zenith angle", 	"deg", 	"", 	"", 	"" },
	{ SSC_INPUT, 	SSC_NUMBER,	"latitude", 		"Site latitude read from weather file", 	"deg", 	"", 	"", 	"" },
	{ SSC_INPUT, 	SSC_NUMBER,	"longitude", 		"Site longitude read from weather file", 	"deg", 	"", 	"", 	"" },
	{ SSC_INPUT, 	SSC_NUMBER,	"timezone", 		"Time zone", 	"hr", 	"", 	"", 	"" },

	// Type 262 outputs
	{ SSC_OUTPUT, 	SSC_NUMBER, "T_sys_h", 			"Solar field HTF outlet temperature", 	"C", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "m_dot_avail", 		"HTF mass flow rate from the field", 	"kg/hr", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "q_avail", 			"Thermal power produced by the field", 	"MWt", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "DP_tot", 			"Total HTF pressure drop", 	"bar", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "W_dot_pump", 		"Required solar field pumping power", 	"MWe", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "E_fp_tot", 		"Freeze protection energy", 	"MW", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "T_sys_c", 			"Collector inlet temperature", 	"C", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "eta_optical", 		"Collector total optical efficiency", 	"none", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "EqOptEff", 		"Total solar field optical efficiency - including receiver optical losses", 	"none", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "sf_def", 			"The fraction of the solar field that's on focus", 	"none", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "m_dot_htf_tot", 	"The actual flow rate through the field..", 	"kg/hr", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "E_bal_startup", 	"Startup energy consumed", 	"MWt", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "q_inc_sf_tot", 	"Total power incident on the field", 	"MWt", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "q_abs_tot", 		"Total absorbed energy", 	"MWt", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "q_loss_tot", 		"Total receiver thermal and optical losses", 	"MWt", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "m_dot_htf", 		"Flow rate in a single loop", 	"kg/s", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "q_loss_spec_tot", 	"Field-average receiver thermal losses (convection and radiation)", 	"W/m", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "track_par_tot", 	"Parasitic electric power consumed by the tracking drives", 	"MWe", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "Pipe_hl", 			"Pipe heat loss in the hot header and the hot runner", 	"MWt", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "q_dump", 			"Dumped thermal energy", 	"MWt", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "phi_t", 			"Solar incidence angle in the collector transversal plane", 	"deg", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "theta_L", 			"Solar incidence angle in the collector longitudinal plane", 	"deg", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "t_loop_outlet", 	"HTF temperature immediately subsequent to the loop outlet", 	"C", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "c_htf_ave", 		"Average solar field specific heat", 	"J/kg-K", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "q_field_delivered","Total solar field thermal power delivered", 	"MWt", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "eta_thermal", 		"Solar field thermal efficiency (power out/ANI)", 	"none", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "E_loop_accum", 	"Accumulated internal energy change rate in the loops ONLY", 	"MWht", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "E_hdr_accum", 		"Accumulated internal energy change rate in the headers/SGS", 	"MWht", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "E_tot_accum", 		"Total accumulated internal energy change rate", 	"MWht", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_NUMBER, "E_field", 			"Accumulated internal energy in the entire solar field", 	"MWht", 	"", 	"", 	"" },
	{ SSC_OUTPUT, 	SSC_STRING, "piping_summary", 	"String containing description of field piping design", 	"none", 	"", 	"", 	"" },


	// Type 234 (powerblock) parameters
    { SSC_INPUT,        SSC_NUMBER,      "P_ref",             "Reference output electric power at design condition",                                 "MW",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_ref",           "Reference conversion efficiency at design condition",                                 "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_hot_ref",         "Reference HTF inlet temperature at design",                                           "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_cold_ref",        "Reference HTF outlet temperature at design",                                          "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dT_cw_ref",         "Reference condenser cooling water inlet/outlet T diff",                               "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_amb_des",         "Reference ambient temperature at design point",                                       "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_sby_frac",        "Fraction of thermal power required for standby mode",                                 "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_boil_des",        "Boiler operating pressure @ design",                                                  "bar",           "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_rh",             "Flag indicating whether reheat is used 0:no, 1:yes",                                  "none",          "",            "powerblock",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_rh_ref",          "Reheater operating pressure at design",                                               "bar",           "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_rh_hot_ref",      "Reheater design outlet temperature",                                                  "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rh_frac_ref",       "Reheater flow fraction at design",                                                    "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "CT",                "Flag for using dry cooling or wet cooling system",                                    "none",          "",            "powerblock",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_time",      "Time needed for power block startup",                                                 "hr",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_frac",      "Fraction of design thermal power needed for startup",                                 "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tech_type",         "Flag indicating which coef. set to use. (1=tower,2=trough,3=user)",                   "none",          "",            "powerblock",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_approach",        "Cooling tower approach temperature",                                                  "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_ITD_des",         "ITD at design for dry system",                                                        "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_ratio",      "Condenser pressure ratio",                                                            "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_bd_frac",        "Power block blowdown steam fraction ",                                                "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_min",        "Minimum condenser pressure",                                                          "inHg",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_pl_inc",          "Number of part-load increments for the heat rejection system",                        "none",          "",            "powerblock",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_ARRAY,       "F_wc",              "Fraction indicating wet cooling use for hybrid system",                               "none",          "",            "powerblock",     "*",                       "",                      "" },
	// Type 234 (powerblock) inputs
    { SSC_INPUT,        SSC_NUMBER,      "mode",              "Cycle part load control, from plant controller",                                      "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_hot",             "Hot HTF inlet temperature, from storage tank",                                        "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_st",          "HTF mass flow rate",                                                                  "kg/hr",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_wb",              "Ambient wet bulb temperature",                                                        "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "demand_var",        "Control signal indicating operational mode",                                          "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "standby_control",   "Control signal indicating standby mode",                                              "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_db_pwb",          "Ambient dry bulb temperature",                                                        "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_amb_pwb",         "Ambient pressure",                                                                    "atm",           "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "TOU",               "Current Time-of-use period",                                                          "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "relhum",            "Relative humidity of the ambient air",                                                "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "f_recSU",           "Fraction powerblock can run due to receiver startup",                                 "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dp_b",              "Pressure drop in boiler",                                                             "Pa",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dp_sh",             "Pressure drop in superheater",                                                        "Pa",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dp_rh",             "Pressure drop in reheater",                                                           "Pa",            "",            "powerblock",     "*",                       "",                      "" },

    // OUTPUTS
	// The names of the output variables should match the parameter names for the TCS units in order to signal to the TCS kernel to store the values by timestep

    // VARTYPE          DATATYPE          NAME                 LABEL                                                                                 UNITS           META            GROUP            REQUIRED_IF                 CONSTRAINTS             UI_HINTS
	// Type 261 (net energy calculator) outputs
    { SSC_OUTPUT,       SSC_ARRAY,       "W_net",              "Net electricity generation (or usage) by the plant",                                 "MWh",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },
    { SSC_OUTPUT,       SSC_ARRAY,       "W_cycle_gross",      "Electrical source - Power cycle gross output",                                       "MWh",          "",            "Outputs",        "*",                       "LENGTH=8760",           "" },

    var_info_invalid };

class cm_tcsmslf : public tcKernel
{
public:

	cm_tcsmslf(tcstypeprovider *prov)
	:tcKernel(prov)
	{
		add_var_info( _cm_vtab_tcsmslf );
		//set_store_all_parameters(true); // default is 'false' = only store TCS parameters that match the SSC_OUTPUT variables above
	}

	void exec( ) throw( general_error )
	{
		bool debug_mode = true; //(__DEBUG__ == 1);  // When compiled in VS debug mode, this will use the trnsys weather file; otherwise, it will attempt to open the file with name that was passed in
		//Add weather file reader unit
		int weather = 0;
		if(debug_mode) weather = add_unit("trnsys_weatherreader", "TRNSYS weather reader");
		else weather = add_unit("weatherreader", "TCS weather reader");

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
			//Set weather parameters
			set_unit_value_ssc_string( weather, "file_name" );
			set_unit_value_ssc_double( weather, "track_mode" );
			set_unit_value_ssc_double( weather, "tilt" );
			set_unit_value_ssc_double( weather, "azimuth" );
		}

		//Add the solar field collector unit
		int solarfield = add_unit("sam_mw_lf_type261_steam", "type 261 solarfield");
		//Add direct powerblock unit
		int powerblock = add_unit("sam_mw_type234", "type 234 powerblock");
		//Add unit to that summarizes energy output
		int E_net_calcs = add_unit("sam_mw_lf_type261_Wnet","type 261 enet calculator");



		// Now set solar field collector unit parameters
		set_unit_value_ssc_double( solarfield, "tes_hours"); // TSHOURS );
		set_unit_value_ssc_double( solarfield, "q_max_aux"); // q_max_aux );
		set_unit_value_ssc_double( solarfield, "LHV_eff"); // LHV_eff );
		set_unit_value_ssc_double( solarfield, "T_set_aux"); // T_set_aux );
		set_unit_value_ssc_double( solarfield, "T_field_in_des"); // T_field_in_des );
		set_unit_value_ssc_double( solarfield, "T_field_out_des"); // T_field_out_des );
		set_unit_value_ssc_double( solarfield, "x_b_des"); // x_b_des );
		set_unit_value_ssc_double( solarfield, "P_turb_des"); // P_turb_des );
		set_unit_value_ssc_double( solarfield, "fP_hdr_c"); // fP_hdr_c );
		set_unit_value_ssc_double( solarfield, "fP_sf_boil"); // fP_sf_boil );
		set_unit_value_ssc_double( solarfield, "fP_boil_to_sh"); // fP_boil_to_SH );
		set_unit_value_ssc_double( solarfield, "fP_sf_sh"); // fP_sf_sh );
		set_unit_value_ssc_double( solarfield, "fP_hdr_h"); // fP_hdr_h );
		set_unit_value_ssc_double( solarfield, "q_pb_des"); // Q_ref ); // = P_ref/eta_ref;
		set_unit_value_ssc_double( solarfield, "W_pb_des"); // W_pb_des );
		set_unit_value_ssc_double( solarfield, "cycle_max_fraction"); // cycle_max_fraction );
		set_unit_value_ssc_double( solarfield, "cycle_cutoff_frac"); // cycle_cutoff_frac );
		set_unit_value_ssc_double( solarfield, "t_sby"); // t_sby );
		set_unit_value_ssc_double( solarfield, "q_sby_frac"); // q_sby_frac );
		set_unit_value_ssc_double( solarfield, "solarm"); // solarm );
		set_unit_value_ssc_double( solarfield, "PB_pump_coef"); // PB_pump_coef );
		set_unit_value_ssc_double( solarfield, "PB_fixed_par"); // PB_fixed_par );
		set_unit_value_ssc_array( solarfield, "bop_array"); // [BOP_parVal, BOP_parPF, BOP_par0, BOP_par1, BOP_par2] );
		set_unit_value_ssc_array( solarfield, "aux_array"); // [Aux_parVal, Aux_parPF, Aux_par0, Aux_par1, Aux_par2] );
		set_unit_value_ssc_double( solarfield, "T_startup"); // T_startup );
		set_unit_value_ssc_double( solarfield, "fossil_mode"); // fossil_mode );
		set_unit_value_ssc_double( solarfield, "I_bn_des"); // I_bn_des );
		set_unit_value_ssc_double( solarfield, "is_sh"); // is_sh );
		set_unit_value_ssc_double( solarfield, "is_oncethru"); // is_oncethru );
		set_unit_value_ssc_double( solarfield, "is_multgeom"); // is_multgeom );
		set_unit_value_ssc_double( solarfield, "nModBoil"); // nModBoil );
		set_unit_value_ssc_double( solarfield, "nModSH"); // nModSH );
		set_unit_value_ssc_double( solarfield, "nLoops"); // nLoops );
		set_unit_value_ssc_double( solarfield, "eta_pump"); // eta_pump );
		set_unit_value_ssc_double( solarfield, "latitude"); // latitude );
		set_unit_value_ssc_double( solarfield, "theta_stow"); // theta_stow );
		set_unit_value_ssc_double( solarfield, "theta_dep"); // theta_dep );
		set_unit_value_ssc_double( solarfield, "m_dot_min"); // m_dot_min );
		set_unit_value_ssc_double( solarfield, "T_field_ini"); // T_field_ini );
		set_unit_value_ssc_double( solarfield, "T_fp"); // T_fp );
		set_unit_value_ssc_double( solarfield, "Pipe_hl_coef"); // Pipe_hl_coef );
		set_unit_value_ssc_double( solarfield, "SCA_drives_elec"); // SCA_drives_elec );
		set_unit_value_ssc_double( solarfield, "ColAz"); // ColAz );
		set_unit_value_ssc_double( solarfield, "e_startup"); // e_startup );
		set_unit_value_ssc_double( solarfield, "T_amb_des_sf"); // T_amb_des_sf );
		set_unit_value_ssc_double( solarfield, "V_wind_max"); // V_wind_max );
		set_unit_value_ssc_array( solarfield, "ffrac"); // [FFRAC_1,FFRAC_2,FFRAC_3,FFRAC_4,FFRAC_5,FFRAC_6,FFRAC_7,FFRAC_8,FFRAC_9] );
		// Set all matrix parameters
		set_unit_value_ssc_matrix(solarfield,"A_aperture"); //A_aper);
		set_unit_value_ssc_matrix(solarfield,"L_col"); //L_col);
		set_unit_value_ssc_matrix(solarfield,"OptCharType"); //OptCharType);
		set_unit_value_ssc_matrix(solarfield,"IAM_T"); //IAM_T);
		set_unit_value_ssc_matrix(solarfield,"IAM_L"); //IAM_L);
		set_unit_value_ssc_matrix(solarfield,"TrackingError"); //TrackingError);
		set_unit_value_ssc_matrix(solarfield,"GeomEffects"); //GeomEffects);
		set_unit_value_ssc_matrix(solarfield,"rho_mirror_clean"); //rho_mirror_clean);
		set_unit_value_ssc_matrix(solarfield,"dirt_mirror"); //dirt_mirror);
		set_unit_value_ssc_matrix(solarfield,"error"); //error);
		set_unit_value_ssc_matrix(solarfield,"HLCharType"); //HLCharType);
		set_unit_value_ssc_matrix(solarfield,"HL_dT"); //HL_dT);
		set_unit_value_ssc_matrix(solarfield,"HL_W"); //HL_W);
		set_unit_value_ssc_matrix(solarfield,"D_2"); //D_2);
		set_unit_value_ssc_matrix(solarfield,"D_3"); //D_3);
		set_unit_value_ssc_matrix(solarfield,"D_4"); //D_4);
		set_unit_value_ssc_matrix(solarfield,"D_5"); //D_5);
		set_unit_value_ssc_matrix(solarfield,"D_p"); //D_p);
		set_unit_value_ssc_matrix(solarfield,"Rough"); //Rough);
		set_unit_value_ssc_matrix(solarfield,"Flow_type"); //Flow_type);
		set_unit_value_ssc_matrix(solarfield,"AbsorberMaterial"); //AbsorberMaterial);
		set_unit_value_ssc_matrix(solarfield,"HCE_FieldFrac"); //HCE_FieldFrac);
		set_unit_value_ssc_matrix(solarfield,"alpha_abs"); //alpha_abs);
		set_unit_value_ssc_matrix(solarfield,"b_eps_HCE1"); //b_eps_HCE1);
		set_unit_value_ssc_matrix(solarfield,"b_eps_HCE2"); //b_eps_HCE2);
		set_unit_value_ssc_matrix(solarfield,"b_eps_HCE3"); //b_eps_HCE3);
		set_unit_value_ssc_matrix(solarfield,"b_eps_HCE4"); //b_eps_HCE4);
		if( as_integer("is_multgeom") != 0 )
		{
			set_unit_value_ssc_matrix(solarfield,"sh_eps_HCE1"); //s_eps_HCE1);
			set_unit_value_ssc_matrix(solarfield,"sh_eps_HCE2"); //s_eps_HCE2);
			set_unit_value_ssc_matrix(solarfield,"sh_eps_HCE3"); //s_eps_HCE3);
			set_unit_value_ssc_matrix(solarfield,"sh_eps_HCE4"); //s_eps_HCE4);
		}
		set_unit_value_ssc_matrix(solarfield,"alpha_env"); //alpha_env);
		set_unit_value_ssc_matrix(solarfield,"EPSILON_4"); //EPSILON_4);
		set_unit_value_ssc_matrix(solarfield,"Tau_envelope"); //Tau_envelope);
		set_unit_value_ssc_matrix(solarfield,"GlazingIntactIn"); //GlazingIntactIn);
		set_unit_value_ssc_matrix(solarfield,"AnnulusGas"); //AnnulusGas);
		set_unit_value_ssc_matrix(solarfield,"P_a"); //P_a);
		set_unit_value_ssc_matrix(solarfield,"Design_loss"); //Design_loss);
		set_unit_value_ssc_matrix(solarfield,"Shadowing"); //Shadowing);
		set_unit_value_ssc_matrix(solarfield,"Dirt_HCE"); //Dirt_HCE);
		set_unit_value_ssc_matrix(solarfield, "b_OpticalTable"); // opt_data);
		set_unit_value_ssc_matrix(solarfield, "sh_OpticalTable"); // opt_data);

		// Type 261 (solar field collector) inputs
		set_unit_value_ssc_double(solarfield, "dnifc"); // , 0.0);				//[W/m2] - not used
		set_unit_value_ssc_double(solarfield, "I_bn"); // 0.0);			    //[W/m2] - initial value
		set_unit_value_ssc_double(solarfield, "T_db"); // 15.0);			//[C] - initial value
		set_unit_value_ssc_double(solarfield, "T_dp"); // 10.0);			//[C] - connect to dew point
		set_unit_value_ssc_double(solarfield, "P_amb"); // 930.50);			//[mbar] - initial value
		set_unit_value_ssc_double(solarfield, "V_wind"); // 0.0);			//[m/s] - initial value
		set_unit_value_ssc_double(solarfield, "m_dot_htf_ref"); // 0.0);	//[kg/hr] - initial value
		set_unit_value_ssc_double(solarfield, "m_pb_demand"); // 0.0);			//[kg/hr] - not used
		set_unit_value_ssc_double(solarfield, "shift"); // 0.0);			//[deg] - initial value
		set_unit_value_ssc_double(solarfield, "SolarAz"); // 0.0);			//[deg] - initial value
		set_unit_value_ssc_double(solarfield, "SolarZen"); // 0.0);			//[deg] - initial value
		set_unit_value_ssc_double(solarfield, "T_pb_out"); // 290.0);			//[C] - initial value
		set_unit_value_ssc_double(solarfield, "TOUPeriod"); // 4);				//[-] - don't have TOU reader yet - all are same in default LF model though

		// connect solar field
		bool bConnected = connect(weather, "beam", solarfield, "I_bn");		//[W/m2] - connect to weather reader
		bConnected = connect(weather, "tdry", solarfield, "T_db");		//[C] - connect to weather reader
		bConnected = connect(weather, "tdew", solarfield, "T_dp");		//[C] - connect to weather reader
		bConnected = connect(weather, "pres", solarfield, "P_amb");		//[mbar] - connect to weather reader
		bConnected = connect(weather, "wspd", solarfield, "V_wind");		//[m/s] - connect to weather reader
		bConnected = connect(powerblock, "m_dot_ref", solarfield, "m_dot_htf_ref");	//[kg/hr] connect to powerblock
		bConnected = connect(weather, "shift", solarfield, "shift");		//[deg] - connect to weather reader
		bConnected = connect(weather, "solazi", solarfield, "SolarAz");	//[deg] - connect to weather reader
		bConnected = connect(weather, "solzen", solarfield, "SolarZen"); //[deg] - connect to weather reader
		bConnected = connect(powerblock, "T_cold", solarfield, "T_pb_out");	//[C] - connect to powerblock


		// Set Parameters for Direct Powerblock (type 234)
		set_unit_value_ssc_double(powerblock, "P_ref"); // P_ref);
		set_unit_value_ssc_double(powerblock, "eta_ref"); // eta_ref);
		set_unit_value_ssc_double(powerblock, "T_hot_ref"); // T_hot_ref);
		set_unit_value_ssc_double(powerblock, "T_cold_ref"); // T_cold_ref);
		set_unit_value_ssc_double(powerblock, "dT_cw_ref"); // dT_cw_ref);
		set_unit_value_ssc_double(powerblock, "T_amb_des"); // T_amb_des);
		set_unit_value_ssc_double(powerblock, "q_sby_frac"); // q_sby_frac);
		set_unit_value_ssc_double(powerblock, "P_boil_des"); // P_boil_ref);
		set_unit_value_ssc_double(powerblock, "is_rh"); // is_rh);
		set_unit_value_ssc_double(powerblock, "P_rh_ref"); // P_rh_ref);
		set_unit_value_ssc_double(powerblock, "T_rh_hot_ref"); // T_rh_hot_ref);
		set_unit_value_ssc_double(powerblock, "rh_frac_ref"); // rh_frac_ref);
		set_unit_value_ssc_double(powerblock, "CT"); // CT);
		set_unit_value_ssc_double(powerblock, "startup_time"); // startup_time);
		set_unit_value_ssc_double(powerblock, "startup_frac"); // startup_frac);
		set_unit_value_ssc_double(powerblock, "tech_type"); // tech_type);
		set_unit_value_ssc_double(powerblock, "T_approach"); // T_approach);
		set_unit_value_ssc_double(powerblock, "T_ITD_des"); // T_ITD_des);
		set_unit_value_ssc_double(powerblock, "P_cond_ratio"); // P_cond_ratio);
		set_unit_value_ssc_double(powerblock, "pb_bd_frac"); // pb_bd_frac);
		set_unit_value_ssc_double(powerblock, "P_cond_min"); // P_cond_min);
		set_unit_value_ssc_double(powerblock, "n_pl_inc"); // n_pl_inc);
		set_unit_value_ssc_array(powerblock, "F_wc"); // [F_wc_1, F_wc_2, F_wc_3, F_wc_4, F_wc_5, F_wc_6, F_wc_7, F_wc_8, F_wc_9]);


		// Set Inputs for Powerblock (type 234)
		set_unit_value_ssc_double(powerblock, "mode"); // 1);							//[-] initial value
		set_unit_value_ssc_double(powerblock, "T_hot"); // T_hot_ref);					//[C] initial value
		set_unit_value_ssc_double(powerblock, "m_dot_st"); // 0);						//[kg/hr] initial value
		set_unit_value_ssc_double(powerblock, "T_wb"); // 12.8);						//[C] Initial value
		set_unit_value_ssc_double(powerblock, "demand_var"); // P_ref);					//[kg/hr]
		set_unit_value_ssc_double(powerblock, "standby_control"); // 0);
		set_unit_value_ssc_double(powerblock, "T_db", as_double("T_db_pwb") ); // 12.8);
		set_unit_value_ssc_double(powerblock, "P_amb", as_double("P_amb_pwb") ); // 960);
		set_unit_value_ssc_double(powerblock, "TOU"); // 4);								//[-] No TOU reader yet
		set_unit_value_ssc_double(powerblock, "relhum"); // 0.25);						//[-] Initial value
		set_unit_value_ssc_double(powerblock, "f_recSU"); // 1);							//[-] Set to 1 for LF
		set_unit_value_ssc_double(powerblock, "dp_sh"); // 5.0);
		set_unit_value_ssc_double(powerblock,"dp_rh"); // 0.0);								//[Pa] no rh in LF

		// connect the powerblock
		bConnected = connect(solarfield, "cycle_pl_control", powerblock, "mode");	//[-] connect to LF solarfield
		bConnected = connect(solarfield, "T_field_out", powerblock, "T_hot");		//[C] connect to LF solarfield
		bConnected = connect(solarfield, "m_dot_to_pb", powerblock, "m_dot_st");		//[kg/hr] connect to LF solarfield
		bConnected = connect(weather, "twet", powerblock, "T_wb");					//[C] connect to weather reader
		bConnected = connect(solarfield, "m_dot_to_pb", powerblock, "demand_var");	//[kg/hr] 
		bConnected = connect(solarfield, "standby_control", powerblock, "standby_control");
		bConnected = connect(weather, "tdry", powerblock, "T_db");
		bConnected = connect(weather, "pres", powerblock, "P_amb");
		bConnected = connect(weather, "rhum", powerblock, "relhum");					//[-] connect to weather reader
		bConnected = connect(solarfield, "dP_sf_sh", powerblock, "dp_sh");			//[Pa] Pressure drop in sh

		// connect the net energy output calculator
		bConnected = connect(powerblock, "P_cycle", E_net_calcs, "W_cycle_gross");
		bConnected = connect(solarfield, "W_dot_par_tot", E_net_calcs, "W_par_sf_tot");
		bConnected = connect(powerblock, "W_cool_par", E_net_calcs, "W_par_cooling");



		// check if all connections worked
		if ( !bConnected )
			throw exec_error( "tcsmslf", util::format("there was a problem connecting outputs of one unit to inputs of another for the simulation.") );

		// Run simulation
		size_t hours = 8760;
		if (0 > simulate(3600, hours*3600, 3600) )
			throw exec_error( "tcsmslf", util::format("there was a problem simulating in the TCS linear fresnel model.") );

		// get the outputs
		if (!set_all_output_arrays() )
			throw exec_error( "tcsmslf", util::format("there was a problem returning the results from the simulation.") );


		//set_output_array("i_SfTi",8760);
	}

};

DEFINE_TCS_MODULE_ENTRY( tcsmslf, "CSP model using the molten salt linear fresnel TCS types.", 4 )
