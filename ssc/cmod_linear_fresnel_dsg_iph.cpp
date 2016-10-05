// Steam Linear Fresnel - direct steam generation 
#include "core.h"
#include "tckernel.h"

// for new solver class.
#include "common.h"
#include "lib_weatherfile.h"
#include "csp_solver_lf_dsg_collector_receiver.h"

#include "csp_solver_pc_steam_heat_sink.h"
#include "csp_solver_tou_block_schedules.h"
#include "csp_solver_two_tank_tes.h"

static bool ssc_linear_fresnel_dsg_iph_sim_progress(void *data, double percent, C_csp_messages *csp_msg, float time_sec);

static var_info _cm_vtab_linear_fresnel_dsg_iph[] = {
/*	EXAMPLE LINES FOR INPUTS
    { SSC_INPUT,        SSC_NUMBER,      "XXXXXXXXXXXXXX",    "Label",                                                                               "",              "",            "sca",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "INTINTINTINT",      "Label",                                                                               "",              "",            "parasitic",      "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_ARRAY,       "XXXXXXXXXXX",       "Number indicating the receiver type",                                                 "",              "",            "hce",            "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "XXXXXXXXXXX",       "Label",                                                                               "",              "",            "tes",            "*",                       "",                      "" },
*/

//    VARTYPE           DATATYPE          NAME                 LABEL                                                                                   UNITS            META            GROUP            REQUIRED_IF                 CONSTRAINTS             UI_HINTS
    { SSC_INPUT,        SSC_STRING,      "file_name",         "local weather file path",                                                             "",              "",            "Weather",        "*",                       "LOCAL_FILE",            "" },
    { SSC_INPUT,        SSC_NUMBER,      "track_mode",        "Tracking mode",                                                                       "",              "",            "Weather",        "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "tilt",              "Tilt angle of surface/axis",                                                          "",              "",            "Weather",        "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "azimuth",           "Azimuth angle of surface/axis",                                                       "",              "",            "Weather",        "*",                       "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "system_capacity",   "Nameplate capacity",                                                                 "kW",             "",            "linear fresnelr", "*", "", "" },

    // TOU
    { SSC_INPUT,        SSC_MATRIX,      "weekday_schedule",  "12x24 Time of Use Values for week days",                                              "",             "",             "tou_translator", "*",                       "",                      "" }, 
    { SSC_INPUT,        SSC_MATRIX,      "weekend_schedule",  "12x24 Time of Use Values for week end days",                                          "",             "",             "tou_translator", "*",                       "",                      "" }, 

	// Type 261 (solar field collector) parameters
    { SSC_INPUT,        SSC_NUMBER,      "tes_hours",         "Equivalent full-load thermal storage hours",                                          "hr",            "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_max_aux",         "Maximum heat rate of the auxiliary heater",                                           "MW",            "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "LHV_eff",           "Fuel LHV efficiency (0..1)",                                                          "none",          "",            "solarfield",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_set_aux",         "Aux heater outlet temperature set point",                                             "C",             "",            "solarfield",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_field_in_des",    "Field design inlet temperature",                                                      "C",             "",            "solarfield",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_field_out_des",   "Field loop outlet design temperature",                                                "C",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "x_b_des",           "Design point boiler outlet steam quality",                                            "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_turb_des",        "Design-point turbine inlet pressure",                                                 "bar",           "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fP_hdr_c",          "Average design-point cold header pressure drop fraction",                             "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fP_sf_boil",        "Design-point pressure drop across the solar field boiler fraction",                   "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fP_boil_to_sh",     "Design-point pressure drop between the boiler and superheater frac",                  "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fP_sf_sh",          "Design-point pressure drop across the solar field superheater frac",                  "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fP_hdr_h",          "Average design-point hot header pressure drop fraction",                              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_pb_des",          "Design heat input to the power block",                                                "MW",            "",            "solarfield",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "W_pb_des",          "Rated plant capacity",                                                                "MW",            "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cycle_max_fraction","Maximum turbine over design operation fraction",                                      "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "cycle_cutoff_frac", "Minimum turbine operation fraction before shutdown",                                  "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "t_sby",             "Low resource standby period",                                                         "hr",            "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_sby_frac",        "Fraction of thermal power required for standby",                                      "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "solarm",            "Solar multiple",                                                                      "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "PB_pump_coef",      "Pumping power required to move 1kg of HTF through power block flow",                  "kW/kg",         "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "PB_fixed_par",      "fraction of rated gross power consumed at all hours of the year",                     "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "bop_array",         "BOP_parVal, BOP_parPF, BOP_par0, BOP_par1, BOP_par2",                                 "-",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_ARRAY,       "aux_array",         "Aux_parVal, Aux_parPF, Aux_par0, Aux_par1, Aux_par2",                                 "-",             "",            "solarfield",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_startup",         "Startup temperature (same as field startup)",                                         "C",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "fossil_mode",       "Operation mode for the fossil backup {1=Normal,2=supp,3=toppin}",                     "none",          "",            "solarfield",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "I_bn_des",          "Design point irradiation value",                                                      "W/m2",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_sh",             "Does the solar field include a superheating section",                                 "none",          "",            "solarfield",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_oncethru",       "Flag indicating whether flow is once through with superheat",                         "none",          "",            "solarfield",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "is_multgeom",       "Does the superheater have a different geometry from the boiler {1=yes}",              "none",          "",            "solarfield",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "nModBoil",          "Number of modules in the boiler section",                                             "none",          "",            "solarfield",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "nModSH",            "Number of modules in the superheater section",                                        "none",          "",            "solarfield",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "nLoops",            "Number of loops",                                                                     "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_pump",          "Feedwater pump efficiency",                                                           "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "latitude",          "Site latitude resource page",                                                         "deg",           "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "theta_stow",        "stow angle",                                                                          "deg",           "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "theta_dep",         "deploy angle",                                                                        "deg",           "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_min",         "Minimum loop flow rate",                                                              "kg/s",          "",            "solarfield",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_field_ini",       "Initial field temperature",                                                           "C",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_fp",              "Freeze protection temperature (heat trace activation temperature)",                   "C",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "Pipe_hl_coef",      "Loss coefficient from the header.. runner pipe.. and non-HCE pipin",                  "W/m2-K",        "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "SCA_drives_elec",   "Tracking power.. in Watts per SCA drive",                                             "W/SCA",         "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "ColAz",             "Collector azimuth angle",                                                             "deg",           "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "e_startup",         "Thermal inertia contribution per sq meter of solar field",                            "kJ/K-m2",       "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_amb_des_sf",      "Design-point ambient temperature",                                                    "C",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_wind_max",        "Maximum allowable wind velocity before safety stow",                                  "m/s",           "",            "solarfield",     "*",                       "",                      "" },
    
	{ SSC_INPUT,        SSC_NUMBER,      "csp.lf.sf.water_per_wash",  "Water usage per wash",                "L/m2_aper",    "",    "heliostat", "*", "", "" },
	{ SSC_INPUT,        SSC_NUMBER,      "csp.lf.sf.washes_per_year", "Mirror washing frequency",            "",             "",    "heliostat", "*", "", "" },
	
	{ SSC_INPUT,        SSC_ARRAY,       "ffrac",             "Fossil dispatch logic - TOU periods",                                                 "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "A_aperture",        "(boiler, SH) Reflective aperture area of the collector module",                       "m^2",           "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "L_col",             "(boiler, SH) Active length of the superheater section collector module",              "m",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "OptCharType",       "(boiler, SH) The optical characterization method",                                    "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "IAM_T",             "(boiler, SH) Transverse Incident angle modifiers (0,1,2,3,4 order terms)",            "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "IAM_L",             "(boiler, SH) Longitudinal Incident angle modifiers (0,1,2,3,4 order terms)",          "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "TrackingError",     "(boiler, SH) User-defined tracking error derate",                                     "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "GeomEffects",       "(boiler, SH) User-defined geometry effects derate",                                   "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "rho_mirror_clean",  "(boiler, SH) User-defined clean mirror reflectivity",                                 "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "dirt_mirror",       "(boiler, SH) User-defined dirt on mirror derate",                                     "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "error",             "(boiler, SH) User-defined general optical error derate",                              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "HLCharType",        "(boiler, SH) Flag indicating the heat loss model type {1=poly.; 2=Forristall}",       "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "HL_dT",             "(boiler, SH) Heat loss coefficient - HTF temperature (0,1,2,3,4 order terms)",        "W/m-K^order",   "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "HL_W",              "(boiler, SH) Heat loss coef adj wind velocity (0,1,2,3,4 order terms)",               "1/(m/s)^order", "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_2",               "(boiler, SH) The inner absorber tube diameter",                                       "m",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_3",               "(boiler, SH) The outer absorber tube diameter",                                       "m",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_4",               "(boiler, SH) The inner glass envelope diameter",                                      "m",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_5",               "(boiler, SH) The outer glass envelope diameter",                                      "m",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "D_p",               "(boiler, SH) The diameter of the absorber flow plug (optional)",                      "m",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Rough",             "(boiler, SH) Roughness of the internal surface",                                      "m",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Flow_type",         "(boiler, SH) The flow type through the absorber",                                     "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "AbsorberMaterial",  "(boiler, SH) Absorber material type",                                                 "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "HCE_FieldFrac",     "(boiler, SH) The fraction of the field occupied by this HCE type (4: # field fracs)", "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "alpha_abs",         "(boiler, SH) Absorber absorptance (4: # field fracs)",                                "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "b_eps_HCE1",        "(temperature) Absorber emittance (eps)",                                              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "b_eps_HCE2",        "(temperature) Absorber emittance (eps)",                                              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "b_eps_HCE3",        "(temperature) Absorber emittance (eps)",                                              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "b_eps_HCE4",        "(temperature) Absorber emittance (eps)",                                              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sh_eps_HCE1",       "(temperature) Absorber emittance (eps)",                                              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sh_eps_HCE2",       "(temperature) Absorber emittance (eps)",                                              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sh_eps_HCE3",       "(temperature) Absorber emittance (eps)",                                              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sh_eps_HCE4",       "(temperature) Absorber emittance (eps)",                                              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "alpha_env",         "(boiler, SH) Envelope absorptance (4: # field fracs)",                                "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "EPSILON_4",         "(boiler, SH) Inner glass envelope emissivities (Pyrex) (4: # field fracs)",           "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Tau_envelope",      "(boiler, SH) Envelope transmittance (4: # field fracs)",                              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "GlazingIntactIn",   "(boiler, SH) The glazing intact flag {true=0; false=1} (4: # field fracs)",           "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "AnnulusGas",        "(boiler, SH) Annulus gas type {1=air; 26=Ar; 27=H2} (4: # field fracs)",              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "P_a",               "(boiler, SH) Annulus gas pressure (4: # field fracs)",                                "torr",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Design_loss",       "(boiler, SH) Receiver heat loss at design (4: # field fracs)",                        "W/m",           "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Shadowing",         "(boiler, SH) Receiver bellows shadowing loss factor (4: # field fracs)",              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "Dirt_HCE",          "(boiler, SH) Loss due to dirt on the receiver envelope (4: # field fracs)",           "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "b_OpticalTable",    "Values of the optical efficiency table",                                              "none",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_MATRIX,      "sh_OpticalTable",   "Values of the optical efficiency table",                                              "none",          "",            "solarfield",     "*",                       "",                      "" },

	// Type 261 (solar field collector) initial values
    { SSC_INPUT,        SSC_NUMBER,      "dnifc",             "Forecast DNI",                                                                        "W/m2",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "I_bn",              "Beam normal radiation (input kJ/m2-hr)",                                              "W/m2",          "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_db",              "Dry bulb air temperature",                                                            "C",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_dp",              "The dewpoint temperature",                                                            "C",             "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_amb",             "Ambient pressure",                                                                    "atm",           "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "V_wind",            "Ambient windspeed",                                                                   "m/s",           "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_htf_ref",     "Reference HTF flow rate at design conditions",                                        "kg/hr",         "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_pb_demand",       "Demand htf flow from the power block",                                                "kg/hr",         "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "shift",             "Shift in longitude from local standard meridian",                                     "deg",           "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "SolarAz_init",      "Solar azimuth angle",                                                                 "deg",           "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "SolarZen",          "Solar zenith angle",                                                                  "deg",           "",            "solarfield",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_pb_out_init",     "Fluid temperature from the power block",                                              "C",             "",            "solarfield",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "TOUPeriod",         "Time of use period",                                                                  "none",          "",            "solarfield",     "*",                       "",                      "" },

	// Type 234 (powerblock) parameters
  //{ SSC_INPUT,        SSC_NUMBER,      "P_ref",             "Reference output electric power at design condition",                                 "MW",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "eta_ref",           "Reference conversion efficiency at design condition",                                 "none",          "",            "powerblock",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "T_hot_ref",         "Reference HTF inlet temperature at design",                                           "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_cold_ref",        "Reference HTF outlet temperature at design",                                          "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dT_cw_ref",         "Reference condenser cooling water inlet/outlet T diff",                               "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_amb_des",         "Reference ambient temperature at design point",                                       "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "q_sby_frac",        "Fraction of thermal power required for standby mode",                                 "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_boil_des",        "Boiler operating pressure @ design",                                                  "bar",           "",            "powerblock",     "*",                       "",                      "" },
//    { SSC_INPUT,        SSC_NUMBER,      "is_rh",             "Flag indicating whether reheat is used 0:no, 1:yes",                                  "none",          "",            "powerblock",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_rh_ref",          "Reheater operating pressure at design",                                               "bar",           "",            "powerblock",     "*",                       "",                      "" },
//    { SSC_INPUT,        SSC_NUMBER,      "T_rh_hot_ref",      "Reheater design outlet temperature",                                                  "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "rh_frac_ref",       "Reheater flow fraction at design",                                                    "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "CT",                "Flag for using dry cooling or wet cooling system",                                    "none",          "",            "powerblock",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_time",      "Time needed for power block startup",                                                 "hr",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "startup_frac",      "Fraction of design thermal power needed for startup",                                 "none",          "",            "powerblock",     "*",                       "",                      "" },
 //   { SSC_INPUT,        SSC_NUMBER,      "tech_type",         "Flag indicating which coef. set to use. (1=tower,2=trough,3=user)",                   "none",          "",            "powerblock",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_approach",        "Cooling tower approach temperature",                                                  "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_ITD_des",         "ITD at design for dry system",                                                        "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_ratio",      "Condenser pressure ratio",                                                            "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "pb_bd_frac",        "Power block blowdown steam fraction ",                                                "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_cond_min",        "Minimum condenser pressure",                                                          "inHg",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "n_pl_inc",          "Number of part-load increments for the heat rejection system",                        "none",          "",            "powerblock",     "*",                       "INTEGER",               "" },
    { SSC_INPUT,        SSC_ARRAY,       "F_wc",              "Fraction indicating wet cooling use for hybrid system",                               "none",          "",            "powerblock",     "*",                       "",                      "" },
	// Type 234 (powerblock) inputs
    { SSC_INPUT,        SSC_NUMBER,      "pc_mode",           "Cycle part load control, from plant controller",                                      "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_hot",             "Hot HTF inlet temperature, from storage tank",                                        "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "m_dot_st",          "HTF mass flow rate",                                                                  "kg/hr",         "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_wb",              "Ambient wet bulb temperature",                                                        "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "demand_var",        "Control signal indicating operational mode",                                          "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "standby_control",   "Control signal indicating standby mode",                                              "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "T_db_pwb",          "Ambient dry bulb temperature",                                                        "C",             "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "P_amb_pwb",         "Ambient pressure",                                                                    "atm",           "",            "powerblock",     "*",                       "",                      "" },
  //{ SSC_INPUT,        SSC_NUMBER,      "TOU",               "Current Time-of-use period",                                                          "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "relhum",            "Relative humidity of the ambient air",                                                "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "f_recSU",           "Fraction powerblock can run due to receiver startup",                                 "none",          "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dp_b",              "Pressure drop in boiler",                                                             "Pa",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dp_sh",             "Pressure drop in superheater",                                                        "Pa",            "",            "powerblock",     "*",                       "",                      "" },
    { SSC_INPUT,        SSC_NUMBER,      "dp_rh",             "Pressure drop in reheater",                                                           "Pa",            "",            "powerblock",     "*",                       "",                      "" },

    // OUTPUTS
	// The names of the output variables should match the parameter names for the TCS units in order to signal to the TCS kernel to store the values by timestep

    // VARTYPE          DATATYPE          NAME                 LABEL                                                                                 UNITS           META            GROUP            REQUIRED_IF                 CONSTRAINTS             UI_HINTS
	// Type 261 (net energy calculator) outputs
	{ SSC_OUTPUT, SSC_ARRAY, "Q_thermal", "Thermal power to HTF", "MWt", "", "CR", "", "", "" },

	var_info_invalid };

class cm_linear_fresnel_dsg_iph : public tcKernel
{
public:

	cm_linear_fresnel_dsg_iph(tcstypeprovider *prov)
	:tcKernel(prov)
	{
		add_var_info(_cm_vtab_linear_fresnel_dsg_iph);
		add_var_info(vtab_adjustment_factors);
		add_var_info(vtab_technology_outputs);
	}

	void exec( ) throw( general_error )
	{
		// Weather reader
		C_csp_weatherreader weather_reader;
		weather_reader.m_filename = as_string("file_name");
		weather_reader.m_trackmode = 0;
		weather_reader.m_tilt = 0.0;
		weather_reader.m_azimuth = 0.0;
		weather_reader.init();

		// Set up ssc output arrays
		// Set steps per hour
		double nhourssim = 8760.0;				//[hr] Number of hours to simulate
		C_csp_solver::S_sim_setup sim_setup;
		sim_setup.m_sim_time_start = 0.0;				//[s] starting first hour of year
		sim_setup.m_sim_time_end = nhourssim*3600.0;	//[s] full year simulation

		int steps_per_hour = 1;			//[-]

		int n_wf_records = weather_reader.get_n_records();
		steps_per_hour = n_wf_records / 8760;	//[-]

		int n_steps_fixed = steps_per_hour*8760.0;	//[-]
		sim_setup.m_report_step = 3600.0 / (double)steps_per_hour;	//[s]
		//***************************************************************************
		//***************************************************************************

		C_csp_lf_dsg_collector_receiver c_lf_dsg;
		
		// Now set solar field collector unit parameters
		c_lf_dsg.m_q_max_aux = 0.0;			//[kWt] No aux for IPH
		c_lf_dsg.m_LHV_eff = 1.0;			//[-] No aux for IPH
		c_lf_dsg.m_T_set_aux = as_double("T_hot") + 273.15;				//[K], convert from [C]
		c_lf_dsg.m_T_field_in_des = as_double("T_cold_ref") +273.15;	//[K], convert from [C]
		c_lf_dsg.m_T_field_out_des = as_double("T_hot") + 273.15;		//[K], convert from [C]
		c_lf_dsg.m_x_b_des = as_double("x_b_des");				//[-]
		c_lf_dsg.m_P_turb_des = as_double("P_turb_des");		//[bar]
		c_lf_dsg.m_fP_hdr_c = as_double("fP_hdr_c");			//[-]
		c_lf_dsg.m_fP_sf_boil = as_double("fP_sf_boil");		//[-]
		c_lf_dsg.m_fP_boil_to_sh = as_double("fP_boil_to_sh");	//[-]
		c_lf_dsg.m_fP_sf_sh = as_double("fP_sf_sh");			//[-]
		c_lf_dsg.m_fP_hdr_h = as_double("fP_hdr_h");			//[-]
		c_lf_dsg.m_q_pb_des = as_double("q_pb_des")*1000.0;		//[kWt]   Q_ref ); // = P_ref/eta_ref;
		c_lf_dsg.m_W_pb_des = 0.0;								//[kWe]
		
		// These parameters describe the mass flow rate limits of specifically the solar field
		c_lf_dsg.m_m_dot_max_frac = 1.2;
		c_lf_dsg.m_m_dot_min_frac = 0.2;

		// These parameters describe the limits on what the power cycle / heat sink can accept
		// So, this *could* be left to the controller. 
		// In the process heat model, we assume the heat sink can accept everything the solar field produces
		// So we set these values to extremes.
		// In the Power Generation model, these values are used to scale pressure, which makes things more complicated
		// And is why we can't use min/max calcs to combine these with the m_m_dot_min/max_frac values above
		c_lf_dsg.m_cycle_max_fraction = 100.0;					//[-]
		c_lf_dsg.m_cycle_cutoff_frac = 0.0;						//[-] Scales the design pressure to find the lowest expected pressure in system
		
		
		c_lf_dsg.m_t_sby_des = 0.0;								//[hr] Used to calculated q_dot_aux, so hardcode = 0
		c_lf_dsg.m_q_sby_frac = 0.0;							//[-] Used to calculated q_dot_aux, so hardcode = 0
		c_lf_dsg.m_PB_pump_coef = 0.0;							//[kW/kg] Parameter not used -> need to remove from LFDSG CR
		c_lf_dsg.m_PB_fixed_par = 0.0;							//[-] Calculates fixed parasitics in CR class. Set to 0 and calculate this parasitic somewhere else
		c_lf_dsg.m_fossil_mode = 4;								//[-] in mode 4 the fossil mode sets the off-design pressure to the design pressure
		c_lf_dsg.m_I_bn_des = as_double("I_bn_des");			//[W/m2]
		c_lf_dsg.m_is_oncethru = true;							//[-] Once through because assuming boiler only, for now
		c_lf_dsg.m_is_sh_target = false;						//[-] Targeting 2-phase outlet
		c_lf_dsg.m_is_multgeom = false;							//[-] Only one geometry because assuming boiler only, for now
		c_lf_dsg.m_nModBoil = as_integer("nModBoil");			//[-] Number of modules in a loop
		c_lf_dsg.m_nModSH = 0;									//[-] No superheat, for now
		c_lf_dsg.m_nLoops = as_integer("nLoops");				//[-]
		c_lf_dsg.m_eta_pump = as_double("eta_pump");			//[-] 
		c_lf_dsg.m_latitude = as_double("latitude")*0.0174533;	//[rad], convert from [deg]
		c_lf_dsg.m_theta_stow = as_double("theta_stow")*0.0174533;	//[rad], convert from [deg]
		c_lf_dsg.m_theta_dep = as_double("theta_dep")*0.0174533;	//[rad], convert from [deg]
		c_lf_dsg.m_T_field_ini = as_double("T_cold_ref") +275.15;	//[K], convert from [C]
		c_lf_dsg.m_T_fp = as_double("T_fp") + 273.15;			//[K], convert from [C]
		c_lf_dsg.m_Pipe_hl_coef = as_double("Pipe_hl_coef");	//[W/m2-K]
		c_lf_dsg.m_SCA_drives_elec = as_double("SCA_drives_elec");	//[W/SCA]
		c_lf_dsg.m_ColAz = as_double("ColAz")*0.0174533;		//[rad], convert from [deg]
		c_lf_dsg.m_e_startup = as_double("e_startup");			//[kJ/K-m2], Thermal inertia contribution per sq meter of solar field
		c_lf_dsg.m_T_amb_des_sf = as_double("T_amb_des_sf") +273.15;	//[K] Ambient temperature at design
		c_lf_dsg.m_V_wind_max = as_double("V_wind_max");		//[m/s] Maximum wind speed before stow		
		
		// Not applying BOP plant parasitics for IPH, at least not through the SF model
		c_lf_dsg.m_bop_array.resize(5);
		for (int i = 0; i < 5; i++)
			c_lf_dsg.m_bop_array[i] = 0.0;

		// No aux heating in IPH model
		c_lf_dsg.m_aux_array.resize(5);
		for (int i = 0; i < 5; i++)
			c_lf_dsg.m_aux_array[i] = 0.0;

		// No fossil fill / aux heating in IPH model
		c_lf_dsg.m_ffrac.resize(5);
		for (int i = 0; i < 5; i++)
			c_lf_dsg.m_ffrac[i] = 0.0;

		c_lf_dsg.m_A_aperture = as_matrix("A_aperture");	//[m2]
		c_lf_dsg.m_L_col = as_matrix("L_col");				//[m]
		c_lf_dsg.m_OptCharType = as_matrix("OptCharType");	//[-]
		c_lf_dsg.m_IAM_T = as_matrix("IAM_T");		//[-]
		c_lf_dsg.m_IAM_L = as_matrix("IAM_L");		//[-]
		c_lf_dsg.m_TrackingError = as_matrix("TrackingError");	//[-]
		c_lf_dsg.m_GeomEffects = as_matrix("GeomEffects");		//[-]              "W/m-K^order", 
		c_lf_dsg.m_rho_mirror_clean = as_matrix("rho_mirror_clean");	//[-]	   "1/(m/s)^order"
		c_lf_dsg.m_dirt_mirror = as_matrix("dirt_mirror");				//[-]
		c_lf_dsg.m_error = as_matrix("error");				//[-]
		c_lf_dsg.m_HLCharType = as_matrix("HLCharType");	//[-]
		c_lf_dsg.m_HL_dT = as_matrix("HL_dT");				//[W/m-K^order]
		c_lf_dsg.m_HL_W = as_matrix("HL_W");				//[1/(m/s)^order]
		c_lf_dsg.m_D_2 = as_matrix("D_2");			//[m]
		c_lf_dsg.m_D_3 = as_matrix("D_3");			//[m]
		c_lf_dsg.m_D_4 = as_matrix("D_4");			//[m]
		c_lf_dsg.m_D_5 = as_matrix("D_5");			//[m]
		c_lf_dsg.m_D_p = as_matrix("D_p");			//[m]
		c_lf_dsg.m_Rough = as_matrix("Rough");		//[m]
		c_lf_dsg.m_Flow_type = as_matrix("Flow_type");	//[-]
		c_lf_dsg.m_AbsorberMaterial_in = as_matrix("AbsorberMaterial");	//[-]
		c_lf_dsg.m_HCE_FieldFrac = as_matrix("HCE_FieldFrac");	//[-]
		c_lf_dsg.m_alpha_abs = as_matrix("alpha_abs");	//[-]
		c_lf_dsg.m_b_eps_HCE1 = as_matrix("b_eps_HCE1");	//[-]
		c_lf_dsg.m_b_eps_HCE2 = as_matrix("b_eps_HCE2");	//[-]
		c_lf_dsg.m_b_eps_HCE3 = as_matrix("b_eps_HCE3");	//[-]
		c_lf_dsg.m_b_eps_HCE4 = as_matrix("b_eps_HCE4");	//[-]
		if(c_lf_dsg.m_is_multgeom)
		{
			c_lf_dsg.m_sh_eps_HCE1 = as_matrix("sh_eps_HCE1");	//[-]
			c_lf_dsg.m_sh_eps_HCE2 = as_matrix("sh_eps_HCE2");	//[-]
			c_lf_dsg.m_sh_eps_HCE3 = as_matrix("sh_eps_HCE3");	//[-]
			c_lf_dsg.m_sh_eps_HCE4 = as_matrix("sh_eps_HCE4");	//[-]
		}
		c_lf_dsg.m_alpha_env = as_matrix("alpha_env"); //[-] Envelope absorptance
		c_lf_dsg.m_EPSILON_4 = as_matrix("EPSILON_4"); //[-] Inner glass envelope emissivities
		c_lf_dsg.m_Tau_envelope = as_matrix("Tau_envelope"); //[-] Envelope transmittance
		c_lf_dsg.m_GlazingIntactIn = (bool) as_matrix("GlazingIntactIn"); //[-] Is the glazing intact?
		c_lf_dsg.m_AnnulusGas_in = as_matrix("AnnulusGas"); //[-]
		c_lf_dsg.m_P_a = as_matrix("P_a");					//[torr] Annulus gas pressure 
		c_lf_dsg.m_Design_loss = as_matrix("Design_loss");	//[W/m] Receiver heat loss at design
		c_lf_dsg.m_Shadowing = as_matrix("Shadowing");		//[-] Receiver bellows shadowing loss factor
		c_lf_dsg.m_Dirt_HCE = as_matrix("Dirt_HCE");		//[-] Loss due to dirt on the receiver envelope
		c_lf_dsg.m_b_OpticalTable = as_matrix("b_OpticalTable");	//[-] Boiler Optical Table
		c_lf_dsg.m_sh_OpticalTable = as_matrix("sh_OpticalTable");	//[-] Superheater Optical Table

		// ********************************
		// ********************************
		// Now add the Heat Sink as a power cycle class
		// ********************************
		// ********************************
		// Heat Sink
		C_pc_steam_heat_sink steam_heat_sink;
		steam_heat_sink.ms_params.m_x_hot_des = as_double("x_b_des");		//[-] Inlet quality = field outlet
		steam_heat_sink.ms_params.m_T_hot_des = as_double("T_hot");			//[C] Inlet temperature = field outlet
		steam_heat_sink.ms_params.m_P_hot_des = as_double("P_turb_des")*100.0;	//[kPa], convert from [bar], Inlet pressure = field outlet = design
		steam_heat_sink.ms_params.m_T_cold_des = as_double("T_cold_ref");	//[C] Outlet temperature = FIELD design inlet temperature
		steam_heat_sink.ms_params.m_dP_frac_des = 0.0;						//[-] Fractional pressure drop through heat sink at design
		steam_heat_sink.ms_params.m_q_dot_des = as_double("q_pb_des");		//[MWt] Design thermal power to heat sink
		steam_heat_sink.ms_params.m_m_dot_max_frac = c_lf_dsg.m_cycle_max_fraction;	//[-]
		steam_heat_sink.ms_params.m_pump_eta_isen = 1.0;					//[-]


		// ********************************
		// ********************************
		// Now add the TOU class
		// ********************************
		// ********************************
		C_csp_tou_block_schedules tou;
		tou.setup_block_uniform_tod();
		tou.mc_dispatch_params.m_dispatch_optimize = false;

		// System parameters
		C_csp_solver::S_csp_system_params system;
		system.m_pb_fixed_par = as_double("pb_fixed_par");
		system.m_bop_par = 0.0;
		system.m_bop_par_f = 0.0;
		system.m_bop_par_0 = 0.0;
		system.m_bop_par_1 = 0.0;
		system.m_bop_par_2 = 0.0;

		// ********************************
		// ********************************
		// Now add the storage class
		// ********************************
		// ********************************
		C_csp_two_tank_tes storage;
		C_csp_two_tank_tes::S_params *tes = &storage.ms_params;		

		// Instantiate Solver
		C_csp_solver csp_solver(weather_reader, c_lf_dsg, steam_heat_sink, storage, tou, system);

		int out_type = -1;
		std::string out_msg = "";
		try
		{
			// Initialize Solver
			csp_solver.init(); 
		}
		catch( C_csp_exception &csp_exception )
		{
			// Report warning before exiting with error
			while( csp_solver.mc_csp_messages.get_message(&out_type, &out_msg) )
			{
				log(out_msg, out_type);
			}

			log(csp_exception.m_error_message, SSC_ERROR, -1.0);

			return;
		}

		ssc_number_t *p_gen = allocate("gen", n_steps_fixed);

		try
		{
			// Simulate !
			csp_solver.Ssimulate(sim_setup,
				ssc_linear_fresnel_dsg_iph_sim_progress, 
				(void*)this);
		}
		catch( C_csp_exception &csp_exception )
		{
			// Report warning before exiting with error
			while( csp_solver.mc_csp_messages.get_message(&out_type, &out_msg) )
			{
				log(out_msg);
			}

			log(csp_exception.m_error_message, SSC_WARNING);

			return;
		}

	}

};

static bool ssc_linear_fresnel_dsg_iph_sim_progress(void *data, double percent, C_csp_messages *csp_msg, float time_sec)
{
	cm_linear_fresnel_dsg_iph *cm = static_cast<cm_linear_fresnel_dsg_iph*> (data);
	if( !cm )
		false;

	if( csp_msg != 0 )
	{
		int out_type;
		string message;
		while( csp_msg->get_message(&out_type, &message) )
		{
			cm->log(message, out_type == C_csp_messages::WARNING ? SSC_WARNING : SSC_NOTICE, time_sec);
		}
	}
	bool ret = cm->update("", percent);

	return ret;
}

DEFINE_TCS_MODULE_ENTRY(linear_fresnel_dsg_iph, "CSP model using the linear fresnel TCS types.", 4)
