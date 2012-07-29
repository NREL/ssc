#include "lib_financial.h"
using namespace libfin;
#include "core.h"
#include <sstream>

#ifndef WIN32
#include <float.h>
#endif


static var_info _cm_vtab_annualoutput[] = {


/*   VARTYPE           DATATYPE         NAME                           LABEL                                    UNITS     META                                      GROUP                REQUIRED_IF                 CONSTRAINTS                     UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,     "analysis_years",              "Analyis period",                        "years",  "",                                       "AnnualOutput",      "?=30",                   "INTEGER,MIN=0,MAX=50",           "" },
	{ SSC_INPUT,        SSC_NUMBER,     "energy_net_annual",		   "Annual energy produced by system",      "kWh",    "",                                       "AnnualOutput",      "*",					    "",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,      "energy_availability",		   "Annual energy availability",	        "%",      "",                                       "AnnualOutput",      "*",						"",                              "" },
	{ SSC_INPUT,        SSC_ARRAY,      "energy_degradation",		   "Annual energy degradation",	            "%",      "",                                       "AnnualOutput",      "*",						"",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,     "system_use_lifetime_output",  "Lifetime hourly system outputs",        "0/1",    "0=hourly first year,1=hourly lifetime",  "AnnualOutput",      "*",						"INTEGER,MIN=0",                 "" },
	{ SSC_INPUT,        SSC_ARRAY,		"energy_net_hourly",	       "Hourly energy produced by the system",  "kW",     "",                                       "AnnualOutput",      "*",						"",                              "" },


/* output */
	{ SSC_OUTPUT,        SSC_ARRAY,     "cf_energy_net",               "Net energy",                            "kWh",     "",                                      "AnnualOutput",      "*",                      "",                               "" },


var_info_invalid };

extern var_info
	vtab_standard_financial[],
	vtab_oandm[],
	vtab_tax_credits[],
	vtab_payment_incentives[];

enum {
	CF_energy_net,

	CF_Availability,
	CF_Degradation,

	CF_max };



class cm_annualoutput : public compute_module
{
private:
	util::matrix_t<double> cf;

public:
	cm_annualoutput()
	{
		add_var_info( _cm_vtab_annualoutput );
	}

	void exec( ) throw( general_error )
	{

		// cash flow initialization
		int nyears = as_integer("analysis_years");
		cf.resize_fill( CF_max, nyears+1, 0.0 );


		int i=0;
		double first_year_energy = as_double("energy_net_annual");
		size_t count_avail = 0;
		ssc_number_t *avail = 0;
		avail = as_array("energy_availability", &count_avail);
		size_t count_degrad = 0;
		ssc_number_t *degrad = 0;
		degrad = as_array("energy_degradation", &count_degrad);

		// degradation starts in year 2 for single value degradation - no degradation in year 1 - degradation =1.0
		if (count_degrad == 1)
		{
			if (as_integer("system_use_lifetime_output"))
			{
				if (nyears>=1) cf.at(CF_Degradation,1) = 1.0;
				for (i=2;i<=nyears;i++) cf.at(CF_Degradation,i) = 1.0 - degrad[0]/100.0;
			}
			else
				for (i=1;i<=nyears;i++) cf.at(CF_Degradation,i) = pow((1.0 - degrad[0]/100.0),i-1);
		}
		else if (count_degrad > 0)
		{
			for (i=0;i<nyears && i<(int)count_degrad;i++) cf.at(CF_Degradation,i+1) = (1.0 - degrad[i]/100.0);
		}

		if (count_avail == 1)
		{
			for (i=1;i<=nyears;i++) cf.at(CF_Availability,i)  = avail[0]/100.0;
		}
		else if (count_avail > 0)
		{
			for (i=0;i<nyears && i<(int)count_avail;i++) cf.at(CF_Availability,i+1) = avail[i]/100.0;
		}

		// dispatch
		if (as_integer("system_use_lifetime_output"))
		{
			compute_lifetime_output(nyears);
		}
		else
		{
			for (i=1;i<=nyears;i++)
				cf.at(CF_energy_net,i) = first_year_energy * cf.at(CF_Degradation,i) * cf.at(CF_Availability,i);
		}

		save_cf( CF_energy_net, nyears,"cf_energy_net" );

	}



	bool compute_lifetime_output(int nyears)
	{
	//Calculate energy dispatched in each dispatch period 
		ssc_number_t *hourly_enet; // hourly energy output


		int h;
		size_t count;

	// hourly energy
		hourly_enet = as_array("energy_net_hourly", &count );
		if ( (int)count != (8760*nyears))
		{
			std::stringstream outm;
			outm <<  "Bad hourly dispatch output length (" << count << "), should be (analysis period-1) * 8760 value (" << 8760*nyears << ")";
			log( outm.str() );
			return false;
		}


		for (int y=1;y<=nyears;y++)
		{
			for (h=0;h<8760;h++)
			{
				cf.at(CF_energy_net,y) += hourly_enet[(y-1)*8760+h] * cf.at(CF_Availability,y) * cf.at(CF_Degradation,y);
			}
		}


	
		return true;
	}

	void save_cf(int cf_line, int nyears, const std::string &name)
	{
		ssc_number_t *arrp = allocate( name, nyears+1 );
		for (int i=0;i<=nyears;i++)
			arrp[i] = (ssc_number_t)cf.at(cf_line, i);
	}


};




DEFINE_MODULE_ENTRY( annualoutput, "Annual Output_", 1 );


