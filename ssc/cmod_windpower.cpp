#include "core.h"
#include "lib_windfile.h"
#include "lib_windwatts.h"

static var_info _cm_vtab_windpower[] = {
	{ SSC_INPUT,        SSC_STRING,      "file_name",                  "local SWRF file path",		       "",       "",                      "Weather",      "*",               "LOCAL_FILE",            "" },
		
	{ SSC_INPUT,        SSC_NUMBER,      "shear",                      "Shear exponent",                   "",       "",                      "WindPower",      "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "turbul",                     "Turbulence coefficient",           "frac",   "",                      "WindPower",      "*",             "",                      "" },
	{ SSC_INPUT,        SSC_ARRAY,       "pc_wind",                    "Power curve wind speed array",     "0/1/2",  "",                      "WindPower",      "*",             "",                      "" }, 
	{ SSC_INPUT,        SSC_ARRAY,       "pc_power",                   "Power curve turbine output array", "deg",    "",                      "WindPower",      "*",             "LENGTH_EQUAL=pc_wind",  "" },
	{ SSC_INPUT,        SSC_ARRAY,       "wt_x",                       "Turbine X coordinates",            "m",      "",                      "WindPower",      "*",             "",                      "" },
	{ SSC_INPUT,        SSC_ARRAY,       "wt_y",                       "Turbine Y coordinates",            "m",      "",                      "WindPower",      "*",             "LENGTH_EQUAL=wt_x",     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hub_ht",                     "Hub height",                       "m",      "",                      "WindPower",      "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rotor_di",                   "Rotor diameter",                   "m",      "",                      "WindPower",      "*",             "",                      "" },
	//{ SSC_INPUT,      SSC_NUMBER,      "ctl_mode",                   "Control mode",                     "0/1/2",  "",                      "WindPower",      "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cutin",                      "Cut-in wind speed",                "m/s",    "",                      "WindPower",      "*",             "",                      "" },
	//{ SSC_INPUT,        SSC_NUMBER,      "lossc",                      "Constant losses",                  "kW",     "",                      "WindPower",      "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "lossp",                      "Percentage losses",                "%",      "",                      "WindPower",      "*",             "",                      "" },
	//{ SSC_INPUT,        SSC_NUMBER,      "meas_ht",                    "Height of resource measurement",   "m",      "",                      "WindPower",      "*",             "INTEGER",               "" },


	{ SSC_INPUT,        SSC_NUMBER,      "model_choice",               "Hourly or Weibull model",		   "0/1",    "",                      "WindPower",      "*",             "INTEGER",               "" },
	{ SSC_INPUT,        SSC_NUMBER,      "weibullK",                   "Weibull K factor for wind resource","",      "",                      "WindPower",      "*",             "",		              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "max_cp",                     "Max cp",						   "",       "",                      "WindPower",      "*",             "",		              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "resource_class",             "Wind Resource Class",			   "",       "",                      "WindPower",      "*",             "",		              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "elevation",                  "Elevation",						   "m",      "",                      "WindPower",      "*",             "",		              "" },
	{ SSC_INPUT,        SSC_ARRAY,       "hub_efficiency",             "Array of hub efficiencies",		   "%",      "",                      "WindPower",      "*",             "LENGTH_EQUAL=pc_wind",  "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "farmpwr",                    "Net electric generation",          "kWhac",  "",                      "WindPower",      "*",             "LENGTH=8760",     "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "winddir",                    "Wind direction",                   "deg",    "",                      "WindPower",      "*",             "LENGTH=8760",     "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "windspd",                    "Wind speed",                       "m/s",    "",                      "WindPower",      "*",             "LENGTH=8760",     "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "temp",                       "Air temperature",                  "'C",     "",                      "WindPower",      "*",             "LENGTH=8760",     "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "pres",                       "Pressure",                         "atm",    "",                      "WindPower",      "*",             "LENGTH=8760",     "" },
	{ SSC_OUTPUT,       SSC_MATRIX,      "wtpwr",                      "Power at each WT",                 "kWhac",  "",                      "WindPower",      "*",             "ROWS=8760",       "" },
	{ SSC_OUTPUT,       SSC_MATRIX,      "wteff",                      "Eff at each WT",                   "kWhac",  "",                      "WindPower",      "*",             "ROWS=8760",       "" },
	{ SSC_OUTPUT,       SSC_MATRIX,      "wtvel",                      "Wind speed at each WT",            "kWhac",  "",                      "WindPower",      "*",             "ROWS=8760",       "" },



var_info_invalid };

class cm_windpower : public compute_module
{
private:
public:
	
	cm_windpower()
	{
		add_var_info( _cm_vtab_windpower );
	}

	void exec( ) throw( general_error )
	{
		// might be used in either model
		double shear = as_double("shear");
		double turbul = as_double("turbul");
		double hub_ht = as_double("hub_ht");
		double rotor_di = as_double("rotor_di");
		//double meas_ht = as_double("meas_ht");
		double cutin = as_double("cutin");
		double lossc = 0 ; // as_double("lossc");
		double lossp = as_double("lossp");

		size_t pc_len = 0;
		ssc_number_t *pc_w = as_array( "pc_wind", &pc_len );
		ssc_number_t *pc_p = as_array( "pc_power", NULL );

		size_t nwt = 0;
		ssc_number_t *wt_x = as_array( "wt_x", &nwt );
		ssc_number_t *wt_y = as_array( "wt_y", NULL );

		// have to be allocated to return without errors
		ssc_number_t *farmpwr = allocate( "farmpwr", 8760 );
		ssc_number_t *wspd = allocate("windspd", 8760);
		ssc_number_t *wdir = allocate("winddir", 8760);
		ssc_number_t *air_temp = allocate("temp", 8760);
		ssc_number_t *air_pres = allocate("pres", 8760);
		util::matrix_t<ssc_number_t> &mat_wtpwr = allocate_matrix( "wtpwr", 8760, nwt );
		util::matrix_t<ssc_number_t> &mat_wteff = allocate_matrix( "wteff", 8760, nwt );
		util::matrix_t<ssc_number_t> &mat_wtvel = allocate_matrix( "wtvel", 8760, nwt );

		std::vector<double> Dn(nwt), Cs(nwt), 
			Power(nwt), Thrust(nwt), Eff(nwt), 
			Wind(nwt), Turb(nwt), 
			
			X(nwt), Y(nwt),
			dpcW(pc_len), dpcP(pc_len);

		size_t i,j;

		for (i=0;i<pc_len;i++)
		{
			dpcW[i] = (double)pc_w[i];
			dpcP[i] = (double)pc_p[i];
		}

		// now choose which model to run
		int iModelType = as_integer("model_choice"); // 0=hourly farm model (8760 array outputs), 1=weibull statistical model (single outputs)
		if (iModelType == 1) // doing a Weibull estimate, not an hourly simulation
		{
			double weibull_k = as_double("weibullK");
			double max_cp = as_double("max_cp");
			double resource_class = as_double("resource_class");
			double elevation = as_double("elevation");
			
			ssc_number_t *hub_efficiency = as_array( "hub_efficiency", NULL );
			std::vector<double> dp_hub_eff(pc_len);
			for (i=0;i<pc_len;i++)
				dp_hub_eff[i] = (double)hub_efficiency[i];

			double turbine_kw = turbine_output_using_weibull(rotor_di, weibull_k, shear, max_cp, hub_ht, resource_class, elevation, (int)pc_len, &dpcW[0], &dpcP[0], &dp_hub_eff[0]);
			turbine_kw = turbine_kw * (1 - (lossp/100.0)) - lossc;

			ssc_number_t farm_kw = (ssc_number_t) turbine_kw * nwt;

			for (i=0;i<8760;i++)
				farmpwr[i] = farm_kw/8760.0f;

			return;
		}


		const char *file = as_string("file_name");
		windfile wf(file);		
		if (!wf.ok()) 
			throw exec_error("windpower", "failed to read local weather file: " + std::string(file) + " " + wf.error());
		
		/* ctl_mode hardwired to '2'.  apparently not implemented 
		  correctly for modes 0 and 1, so no point exposing it.
		  apd 03jan11 */

		int ctl_mode = 2; // as_integer("ctl_mode");

		for (i=0;i<nwt;i++)
		{
			X[i] = (double)wt_x[i];
			Y[i] = (double)wt_y[i];
		}


		util::matrix_t<ssc_number_t> &mat_dn = allocate_matrix("dn", 8760, nwt );
		util::matrix_t<ssc_number_t> &mat_cs = allocate_matrix("cs", 8760, nwt );
		size_t nstep = 8760;

		for (i=0;i<8760;i++)
		{
			
			if ( i % (nstep/20) == 0)
				update( "calculating", 100.0f * ((float)i) / ((float)nstep), (float)i );

			double wind, dir, temp, pres, closest_speed_meas_ht, closest_dir_meas_ht;
			if (!wf.read( hub_ht, &wind, &dir, &temp, &pres, &closest_speed_meas_ht, &closest_dir_meas_ht))
				throw exec_error( "windpower", util::format("error reading wind resource file at %d: ", i) + wf.error() );

			if ( fabs(closest_speed_meas_ht - hub_ht) > 35.0 )
				throw exec_error( "windpower", util::format("the closest wind speed measurement height (%lg m) found in the resource file is more than 35 m from the hub height specified (%lg m)", closest_speed_meas_ht, hub_ht ));

			if ( fabs(closest_dir_meas_ht - closest_speed_meas_ht) > 10.0 )
				throw exec_error( "windpower", util::format("the closest wind speed and direction measurement heights were more than 10m apart in the file: %lg m", closest_speed_meas_ht ));

			double farmp = 0;

			if ( (int)nwt != wind_power( 
						/* inputs */
						wind, /* m/s */
						dir, /* degrees */
						shear,
						turbul,
						pres,  /* Atm */
						temp, /* deg C */
						(int)nwt,
						&X[0],
						&Y[0],
						(int)pc_len,
						&dpcW[0],
						&dpcP[0],
						closest_speed_meas_ht,
						hub_ht,
						rotor_di,
						ctl_mode,
						cutin,
						0,
						0,
						lossc,
						lossp/100.0,

						/* outputs */
						&farmp,
						&Dn[0],
						&Cs[0],
						&Power[0],
						&Thrust[0],
						&Eff[0],
						&Wind[0],
						&Turb[0] ) ) 
				throw exec_error( "windpower", util::format("error in wind calculation time %d", i) );


			farmpwr[i] = (ssc_number_t) farmp;
			wspd[i] = (ssc_number_t) wind;
			wdir[i] = (ssc_number_t) dir;
			air_temp[i] = (ssc_number_t) temp;
			air_pres[i] = (ssc_number_t) pres;

		
			for (j=0;j<nwt;j++)
			{
				mat_dn.at(i,j) = (ssc_number_t)Dn[j];
				mat_cs.at(i,j) = (ssc_number_t)Cs[j];
				mat_wtpwr.at(i,j) = (ssc_number_t) Power[j];
				mat_wteff.at(i,j) = (ssc_number_t) Eff[j];
				mat_wtvel.at(i,j) = (ssc_number_t) Wind[j];
			}
		}
	}

};

DEFINE_MODULE_ENTRY( windpower, "Utility scale wind farm model (ported from original TRNSYS P.Quinlan)", 2 );

