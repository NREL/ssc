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
	//{ SSC_INPUT,        SSC_NUMBER,      "elevation",                  "Elevation",						   "m",      "",                      "WindPower",      "*",             "",		              "" },
	{ SSC_INPUT,        SSC_ARRAY,       "hub_efficiency",             "Array of hub efficiencies",		   "%",      "",                      "WindPower",      "*",             "LENGTH_EQUAL=pc_wind",  "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "farmpwr",                    "Net electric generation",          "kWhac",  "",                      "WindPower",      "*",             "LENGTH=8760",     "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "winddir",                    "Wind direction",                   "deg",    "",                      "WindPower",      "*",             "LENGTH=8760",     "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "windspd",                    "Wind speed",                       "m/s",    "",                      "WindPower",      "*",             "LENGTH=8760",     "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "temp",                       "Air temperature",                  "'C",     "",                      "WindPower",      "*",             "LENGTH=8760",     "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "pres",                       "Pressure",                         "atm",    "",                      "WindPower",      "*",             "LENGTH=8760",     "" },
//	{ SSC_OUTPUT,       SSC_MATRIX,      "wtpwr",                      "Power at each WT",                 "kWhac",  "",                      "WindPower",      "*",             "ROWS=8760",       "" },
//	{ SSC_OUTPUT,       SSC_MATRIX,      "wteff",                      "Eff at each WT",                   "kWhac",  "",                      "WindPower",      "*",             "ROWS=8760",       "" },
//	{ SSC_OUTPUT,       SSC_MATRIX,      "wtvel",                      "Wind speed at each WT",            "kWhac",  "",                      "WindPower",      "*",             "ROWS=8760",       "" },



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
		wind_power_calculator wpc;

		wpc.m_dShearExponent =  as_double("shear");
		wpc.m_dTurbulenceIntensity = as_double("turbul");
		wpc.m_dHubHeight = as_double("hub_ht");
		wpc.m_dRotorDiameter = as_double("rotor_di");
		//double meas_ht = as_double("meas_ht");
		wpc.m_dCutInSpeed = as_double("cutin");
		wpc.m_dLossesAbsolute = 0 ; // as_double("lossc");
		wpc.m_dLossesPercent = as_double("lossp")/100.0;

		ssc_number_t *pc_w = as_array( "pc_wind", &wpc.m_iLengthOfTurbinePowerCurveArray );
		ssc_number_t *pc_p = as_array( "pc_power", NULL );

		ssc_number_t *wt_x = as_array( "wt_x", &wpc.m_iNumberOfTurbinesInFarm );
		ssc_number_t *wt_y = as_array( "wt_y", NULL );
		if (wpc.m_iNumberOfTurbinesInFarm < 1)
			throw exec_error( "windpower", util::format("the number of wind turbines was zero.") );
		if (wpc.m_iNumberOfTurbinesInFarm > wpc.GetMaxTurbines())
			throw exec_error( "windpower", util::format("the wind model is only configured to handle up to %d turbines.", wpc.GetMaxTurbines()) );

		size_t nstep = 8760;

		// have to be allocated to return without errors
		ssc_number_t *farmpwr = allocate( "farmpwr", nstep );
		ssc_number_t *wspd = allocate("windspd", nstep);
		ssc_number_t *wdir = allocate("winddir", nstep);
		ssc_number_t *air_temp = allocate("temp", nstep);
		ssc_number_t *air_pres = allocate("pres", nstep);


		std::vector<double> Dn(wpc.m_iNumberOfTurbinesInFarm), Cs(wpc.m_iNumberOfTurbinesInFarm), 
			Power(wpc.m_iNumberOfTurbinesInFarm), Thrust(wpc.m_iNumberOfTurbinesInFarm), Eff(wpc.m_iNumberOfTurbinesInFarm), 
			Wind(wpc.m_iNumberOfTurbinesInFarm), Turb(wpc.m_iNumberOfTurbinesInFarm), 
			
			X(wpc.m_iNumberOfTurbinesInFarm), Y(wpc.m_iNumberOfTurbinesInFarm),
			dpcW(wpc.m_iLengthOfTurbinePowerCurveArray), dpcP(wpc.m_iLengthOfTurbinePowerCurveArray);

		size_t i,j;

		for (i=0;i<wpc.m_iLengthOfTurbinePowerCurveArray;i++)
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
			//double elevation = as_double("elevation");
			
			ssc_number_t *hub_efficiency = as_array( "hub_efficiency", NULL );
			std::vector<double> dp_hub_eff(wpc.m_iLengthOfTurbinePowerCurveArray);
			for (i=0;i<wpc.m_iLengthOfTurbinePowerCurveArray;i++)
				dp_hub_eff[i] = (double)hub_efficiency[i];

			//double turbine_kw = turbine_output_using_weibull(rotor_di, weibull_k, shear, max_cp, hub_ht, resource_class, elevation, (int)wpc.m_iLengthOfTurbinePowerCurveArray, &dpcW[0], &dpcP[0], &dp_hub_eff[0]);
			double turbine_kw = wpc.turbine_output_using_weibull(weibull_k, max_cp, resource_class, &dpcW[0], &dpcP[0], &dp_hub_eff[0]);
			turbine_kw = turbine_kw * (1 - wpc.m_dLossesPercent) - wpc.m_dLossesAbsolute;

			ssc_number_t farm_kw = (ssc_number_t) turbine_kw * wpc.m_iNumberOfTurbinesInFarm;

			for (i=0;i<nstep;i++)
				farmpwr[i] = farm_kw/ (ssc_number_t) nstep;

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

		for (i=0;i<wpc.m_iNumberOfTurbinesInFarm;i++)
		{
			X[i] = (double)wt_x[i];
			Y[i] = (double)wt_y[i];
		}


		// these won't be useful until matrix variables can be passed back as outputs
		//util::matrix_t<ssc_number_t> &mat_wtpwr = allocate_matrix( "wtpwr", nstep, wpc.m_iNumberOfTurbinesInFarm );
		//util::matrix_t<ssc_number_t> &mat_wteff = allocate_matrix( "wteff", nstep, wpc.m_iNumberOfTurbinesInFarm );
		//util::matrix_t<ssc_number_t> &mat_wtvel = allocate_matrix( "wtvel", nstep, wpc.m_iNumberOfTurbinesInFarm );
		//util::matrix_t<ssc_number_t> &mat_dn = allocate_matrix("dn", nstep, wpc.m_iNumberOfTurbinesInFarm );
		//util::matrix_t<ssc_number_t> &mat_cs = allocate_matrix("cs", nstep, wpc.m_iNumberOfTurbinesInFarm );

		for (i=0;i<nstep;i++)
		{
			
			if ( i % (nstep/20) == 0)
				update( "calculating", 100.0f * ((float)i) / ((float)nstep), (float)i );

			// if wf.read is set to interpolate (last input), and it's able to do so, then it will set wpc.m_dMeasurementHeight equal to hub_ht
			// direction will not be interpolated, pressure and temperature will be if possible
			double wind, dir, temp, pres, closest_dir_meas_ht;
			if (!wf.read( wpc.m_dHubHeight, &wind, &dir, &temp, &pres, &wpc.m_dMeasurementHeight, &closest_dir_meas_ht, true))
				throw exec_error( "windpower", util::format("error reading wind resource file at %d: ", i) + wf.error() );

			if ( fabs(wpc.m_dMeasurementHeight - wpc.m_dHubHeight) > 35.0 )
				throw exec_error( "windpower", util::format("the closest wind speed measurement height (%lg m) found is more than 35 m from the hub height specified (%lg m)", wpc.m_dMeasurementHeight, wpc.m_dHubHeight ));

			if ( fabs(closest_dir_meas_ht - wpc.m_dMeasurementHeight) > 10.0 )
				if (i>0) // if this isn't the first hour, then it's probably because of interpolation
				{
					// probably interpolated wind speed, but could not interpolate wind direction because the directions were too far apart.
					// first, verify:
					if ( (wpc.m_dMeasurementHeight == wpc.m_dHubHeight) && (closest_dir_meas_ht != wpc.m_dHubHeight) )
						// now, alert the user of this discrepancy
						throw exec_error( "windpower", util::format("on hour %d, SAM interpolated the wind speed to an %lgm measurement height, but could not interpolate the wind direction from the two closest measurements because the directions encountered were too disparate", i+1, wpc.m_dMeasurementHeight ));
					else
						throw exec_error( "windpower", util::format("SAM encountered an error at hour %d: hub height = %lg, closest wind speed meas height = %lg, closest wind direction meas height = %lg ", i+1, wpc.m_dHubHeight, wpc.m_dMeasurementHeight, closest_dir_meas_ht ));
				}
				else
					throw exec_error( "windpower", util::format("the closest wind speed measurement height (%lg m) and direction measurement height (%lg m) were more than 10m apart", wpc.m_dMeasurementHeight, closest_dir_meas_ht ));

			double farmp = 0;

			if ( (int)wpc.m_iNumberOfTurbinesInFarm != wpc.wind_power( 
						/* inputs */
						wind, /* m/s */
						dir, /* degrees */
						pres,  /* Atm */
						temp, /* deg C */
						&X[0],
						&Y[0],
						&dpcW[0],
						&dpcP[0],

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

		
			//for (j=0;j<nwt;j++)
			//{
			//	mat_dn.at(i,j) = (ssc_number_t)Dn[j];
			//	mat_cs.at(i,j) = (ssc_number_t)Cs[j];
			//	mat_wtpwr.at(i,j) = (ssc_number_t) Power[j];
			//	mat_wteff.at(i,j) = (ssc_number_t) Eff[j];
			//	mat_wtvel.at(i,j) = (ssc_number_t) Wind[j];
			//}
		}
	}

};

DEFINE_MODULE_ENTRY( windpower, "Utility scale wind farm model (ported from original TRNSYS P.Quinlan)", 2 );

