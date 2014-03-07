#include "core.h"

static var_info _cm_vtab_windbos[] = {
/*   VARTYPE           DATATYPE         NAME                              LABEL                                                      UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
																	      								                             
	// Inputs														      								                             
	{ SSC_INPUT,        SSC_NUMBER,      "machine_rating",                "Machine Rating(kWs)",                                     "kW",     "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rotor_diameter",                "Rotor Diameter(meters)",                                  "m",      "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hub_height",                    "Hub Height(meters)",                                      "m",      "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "number_of_turbines",            "Number of Turbines",                                      "",       "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "interconnect_voltage",          "Interconnect Voltage(kV)",                                "kV",     "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "distance_to_interconnect",      "Distance to Interconnect(mi)",                            "miles",  "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "site_terrain",                  "Site Terrain",                                            "",       "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "turbine_layout",                "Turbine Layout",                                          "",       "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "soil_condition",                "Soil Condition",                                          "",       "",                      "wind_bos",      "*",                       "",                              "" },
		
	// Calculated Values - Entering in Data will Override Program
	{ SSC_INPUT,        SSC_NUMBER,      "constructio_time",              "Construction Time(months)",                               "months", "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "om_building_size",              "O&M Building Size(ft2)",                                  "ft^2",   "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "quantity_test_met_towers",      "Quantity of Temporary Meteorological Towers for Testing", "",       "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "quantity_permanent_met_towers", "Quantity of Permanent Meteorological Towers for Testing", "",       "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "weather_delay_days",            "Wind / Weather delay days",                               "",       "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "crane_breakdowns",              "Crane breakdowns",                                        "",       "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "access_road_entrances",         "Access road entrances",                                   "",       "",                      "wind_bos",      "*",                       "",                              "" },

	// inputs from cost model outputs
	{ SSC_INPUT,        SSC_NUMBER,      "turbine_capital_cost",          "Turbine Capital Cost",                                    "$/kW",   "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tower_top_mass",                "Tower Top Mass",                                          "Tonnes", "",                      "wind_bos",      "*",                       "",                              "" },

	// advanced user BOS inputs
	{ SSC_INPUT,        SSC_NUMBER,      "delivery_assist_required",      "Delivery Assist Required",                                "y/n",    "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pad_mount_transformer_required","Pad mount Transformer required",                          "y/n",    "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "new_switchyard_required",       "New Switchyard Required",                                 "y/n",    "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rock_trenching_required",       "Rock trenching required",                                 "%",      "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "mv_thermal_backfill",           "MV thermal backfill",                                     "mi",     "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "mv_overhead_collector",         "MV overhead collector",                                   "mi",     "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "performance_bond",              "Performance bond",                                        "%",      "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "contingency",                   "Contingency",                                             "%",      "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "warranty_management",           "Warranty management",                                     "%",      "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sales_and_use_tax",             "Sales and Use Tax",                                       "%",      "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "overhead",                      "Overhead",                                                "%",      "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "profit_margin",                 "Profit Margin",                                           "%",      "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "development_fee",               "Development Fee",                                         "$M",     "",                      "wind_bos",      "*",                       "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "turbine_transportation",        "Turbine Transportation",                                  "mi",     "",                      "wind_bos",      "*",                       "",                              "" },

//	{ SSC_OUTPUT,       SSC_ARRAY,       "e_net",                         "AC Generation",                                           "kWh",    "",                      "wind_bos",      "*",                       "LENGTH=8760",                   "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "project_total_budgeted_cost",   "Project Total Budgeted Cost",                             "$s",     "",                      "wind_bos",      "*",                       "",                              "" },

var_info_invalid };

class cm_windbos : public compute_module
{
public:
	cm_windbos()
	{
		add_var_info(_cm_vtab_windbos);
	}

	void exec( ) throw( general_error )
	{
		// get values
		ssc_number_t value = as_number("machine_rating");

		// run model (do math)
		ssc_number_t output = value + 17;

		// assign outputs
		assign( "project_total_budgeted_cost", var_data(output * 100) );
	}
};

DEFINE_MODULE_ENTRY( windbos, "Wind Balance of System cost model", 1 )
