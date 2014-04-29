#include "core.h"
#include "lib_windfile.h"
#include "lib_windwatts.h"

static var_info _cm_vtab_generic_system[] = {
//	  VARTYPE           DATATYPE         NAME                           LABEL                                 UNITS           META     GROUP                REQUIRED_IF        CONSTRAINTS           UI_HINTS
	{ SSC_INPUT,        SSC_NUMBER,      "spec_mode",                  "Spec mode: 0=constant CF,1=profile",  "",             "",      "generic_system",      "*",               "",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "derate",                     "Derate",                              "%",            "",      "generic_system",      "*",               "",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "nameplate_capacity",         "Nameplace Capcity",                   "kW",           "",      "generic_system",      "*",               "",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "capacity_factor",            "Capacity Factor",                     "%",            "",      "generic_system",      "*",               "",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "heat_rate",                  "Heat Rate",                           "MMBTUs/MWhe",  "",      "generic_system",      "*",               "",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "conv_eff",                   "Conversion Efficiency",               "%",            "",      "generic_system",      "*",               "",                    "" },
	{ SSC_INPUT,        SSC_ARRAY,       "energy_output_array",        "Array of Energy Output Profile",      "kWh",          "",      "generic_system",      "*",               "",                    "" }, 
																														      														   
//    OUTPUTS ----------------------------------------------------------------------------								      														   
//	  VARTYPE           DATATYPE         NAME                          LABEL                                   UNITS           META     GROUP                 REQUIRED_IF        CONSTRAINTS           UI_HINTS
	{ SSC_OUTPUT,       SSC_ARRAY,       "hourly_energy",              "Hourly Energy",                        "kW",           "",      "generic_system",      "*",               "LENGTH=8760",         "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_energy",             "Monthly Energy",                       "kWh",          "",      "generic_system",      "*",               "LENGTH=12",           "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "annual_energy",              "Annual Energy",                        "kWh",          "",      "generic_system",      "*",               "",                    "" },

	{ SSC_OUTPUT,       SSC_NUMBER,      "fuel_usage",                 "Annual Fuel Usage",                    "kWht",         "",      "generic_system",      "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "water_usage",                "Annual Water Usage",                   "",             "",      "generic_system",      "*",               "",                    "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "heat_rate_output",           "Heat Rate Conversion Factor",          "MMBTUs/MWhe",  "",      "generic_system",      "*",               "",                    "" },

var_info_invalid };

class cm_generic_system : public compute_module
{
private:
public:
	
	cm_generic_system()
	{
		add_var_info( _cm_vtab_generic_system );
	}

	void exec( ) throw( general_error )
	{
		int spec_mode = as_integer("spec_mode");
		ssc_number_t *enet = allocate("hourly_energy", 8760);

		double derate = (1 - (double)as_number("derate") / 100);
		double annual_output = 0;

		if (spec_mode == 0)
		{
			double output = (double)as_number("nameplate_capacity")
				* (double)as_number("capacity_factor") / 100
				* derate;

			annual_output = 8760 * output;

			for (int i = 0; i<8760; i++)
				enet[i] = output;
		}
		else
		{
			size_t count = 0;
			ssc_number_t *data = as_array("energy_output_array", &count);

			if (!data)
				throw exec_error("windpower", util::format("energy_output_array variable had no values."));

			int nmult = count / 8760;
			if (nmult * 8760 != count)
				throw exec_error("windpower", util::format("energy_output_array not a multiple of 8760: len=%d.", count));

			int c = 0;
			int i = 0;
			while (c<8760 && i<count)
			{
				double integ = 0;
				for (int j = 0; j<nmult; j++)
				{
					integ += data[i];
					i++;
				}

				enet[c] = integ*derate;
				annual_output += enet[c];
				c++;
			}
		}

		accumulate_monthly("hourly_energy", "monthly_energy");
		accumulate_annual("hourly_energy", "annual_energy");

		// if conversion efficiency is zero then set fuel usage to zero per email from Paul 5/17/12
		double fuel_usage = 0.0;
		if (as_double("conv_eff") != 0.0)
			fuel_usage = annual_output * 100.0 / as_double("conv_eff");
		assign("fuel_usage", fuel_usage);

		assign("water_usage", 0.0);
		assign("heat_rate_output", as_double("heat_rate") *  as_double("conv_eff") / 100.0);
	} // exec
};

DEFINE_MODULE_ENTRY( generic_system, "Utility scale wind farm model (adapted from TRNSYS code by P.Quinlan and openWind software by AWS Truepower)", 2 );

