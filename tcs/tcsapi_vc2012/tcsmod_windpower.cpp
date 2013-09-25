#include "core.h"

static var_info _cm_vtab_windpower[] = {
	{ TCS_INPUT,        TCS_STRING,      "file_name",                  "local SWRF file path",		       "",       "",                      "Weather",      "*",               "LOCAL_FILE",            "" },
		
	{ TCS_INPUT,        TCS_NUMBER,      "shear",                      "Shear exponent",                   "",       "",                      "WindPower",      "*",             "",                      "" },
	{ TCS_INPUT,        TCS_NUMBER,      "turbul",                     "Turbulence coefficient",           "frac",   "",                      "WindPower",      "*",             "",                      "" },
	{ TCS_INPUT,        TCS_ARRAY,       "pc_wind",                    "Power curve wind speed array",     "0/1/2",  "",                      "WindPower",      "*",             "",                      "" }, 
	{ TCS_INPUT,        TCS_ARRAY,       "pc_power",                   "Power curve turbine output array", "deg",    "",                      "WindPower",      "*",             "LENGTH_EQUAL=pc_wind",  "" },
	//{ TCS_INPUT,        TCS_ARRAY,       "pc_rpm",	                   "Turbine RPM curve",                "rpm",    "",                      "WindPower",      "*",             "LENGTH_EQUAL=pc_wind",  "" },
	{ TCS_INPUT,        TCS_ARRAY,       "wt_x",                       "Turbine X coordinates",            "m",      "",                      "WindPower",      "*",             "",                      "" },
	{ TCS_INPUT,        TCS_ARRAY,       "wt_y",                       "Turbine Y coordinates",            "m",      "",                      "WindPower",      "*",             "LENGTH_EQUAL=wt_x",     "" },
	{ TCS_INPUT,        TCS_NUMBER,      "hub_ht",                     "Hub height",                       "m",      "",                      "WindPower",      "*",             "",                      "" },
	{ TCS_INPUT,        TCS_NUMBER,      "rotor_di",                   "Rotor diameter",                   "m",      "",                      "WindPower",      "*",             "",                      "" },
	//{ TCS_INPUT,      TCS_NUMBER,      "ctl_mode",                   "Control mode",                     "0/1/2",  "",                      "WindPower",      "*",             "INTEGER",               "" },
	{ TCS_INPUT,        TCS_NUMBER,      "cutin",                      "Cut-in wind speed",                "m/s",    "",                      "WindPower",      "*",             "",                      "" },
	{ TCS_INPUT,        TCS_NUMBER,      "wake_model",                 "Wake Model",                       "0/1/2",  "",                      "WindPower",      "*",             "INTEGER",               "" },
	{ TCS_INPUT,        TCS_NUMBER,      "lossp",                      "Percentage losses",                "%",      "",                      "WindPower",      "*",             "",                      "" },
	//{ TCS_INPUT,        TCS_NUMBER,      "meas_ht",                    "Height of resource measurement",   "m",      "",                      "WindPower",      "*",             "INTEGER",               "" },


	{ TCS_INPUT,        TCS_NUMBER,      "model_choice",               "Hourly or Weibull model",		   "0/1",    "",                      "WindPower",      "*",             "INTEGER",               "" },
	{ TCS_INPUT,        TCS_NUMBER,      "weibullK",                   "Weibull K factor for wind resource","",      "",                      "WindPower",      "*",             "",		              "" },
	{ TCS_INPUT,        TCS_NUMBER,      "max_cp",                     "Max cp",						   "",       "",                      "WindPower",      "*",             "",		              "" },
	{ TCS_INPUT,        TCS_NUMBER,      "resource_class",             "Wind Resource Class",			   "",       "",                      "WindPower",      "*",             "",		              "" },
	//{ TCS_INPUT,        TCS_NUMBER,      "elevation",                  "Elevation",						   "m",      "",                      "WindPower",      "*",             "",		              "" },
	//{ TCS_INPUT,        TCS_ARRAY,       "hub_efficiency",             "Array of hub efficiencies",		   "%",      "",                      "WindPower",      "*",             "LENGTH_EQUAL=pc_wind",  "" },

	{ TCS_OUTPUT,       TCS_ARRAY,       "turbine_output",             "Turbine output",                   "kW",     "",                      "WindPower",      "*",             "LENGTH_EQUAL=pc_wind",  "" },
	{ TCS_OUTPUT,       TCS_ARRAY,       "farmpwr",                    "Net electric generation",          "kWhac",  "",                      "WindPower",      "*",             "LENGTH=8760",     "" },
	{ TCS_OUTPUT,       TCS_ARRAY,       "winddir",                    "Wind direction",                   "deg",    "",                      "WindPower",      "*",             "LENGTH=8760",     "" },
	{ TCS_OUTPUT,       TCS_ARRAY,       "windspd",                    "Wind speed",                       "m/s",    "",                      "WindPower",      "*",             "LENGTH=8760",     "" },
	{ TCS_OUTPUT,       TCS_ARRAY,       "temp",                       "Air temperature",                  "'C",     "",                      "WindPower",      "*",             "LENGTH=8760",     "" },
	{ TCS_OUTPUT,       TCS_ARRAY,       "pres",                       "Pressure",                         "atm",    "",                      "WindPower",      "*",             "LENGTH=8760",     "" },
//	{ TCS_OUTPUT,       TCS_MATRIX,      "wtpwr",                      "Power at each WT",                 "kWhac",  "",                      "WindPower",      "*",             "ROWS=8760",       "" },
//	{ TCS_OUTPUT,       TCS_MATRIX,      "wteff",                      "Eff at each WT",                   "kWhac",  "",                      "WindPower",      "*",             "ROWS=8760",       "" },
//	{ TCS_OUTPUT,       TCS_MATRIX,      "wtvel",                      "Wind speed at each WT",            "kWhac",  "",                      "WindPower",      "*",             "ROWS=8760",       "" },
var_info_invalid };

double gammaln(double x);

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
		if ( 0 >= m_kern.load_library("typelib") )
			throw exec_error( "windpower", util::format("could not load the tcs type library.") );


		//kern.add_unit("type","name");

		size_t pc_array_count = 0;
		tcs_number_t *pc_w = as_array( "pc_wind", &pc_array_count );
		tcs_number_t *pc_p = as_array( "pc_power", NULL );
		std::vector<double> turbine_outkW(pc_array_count); 

		double weibull_k = as_double("weibullK");
		double max_cp = as_double("max_cp");
		double resource_class = as_double("resource_class");

		double m_dShearExponent =  as_double("shear");
		double m_dHubHeight = as_double("hub_ht");


					double hub_ht_windspeed = pow((m_dHubHeight/50.0),m_dShearExponent) * resource_class;
					double denom = exp(gammaln(1+(1/hub_ht_windspeed)));

					double lambda = hub_ht_windspeed/denom;
					//double air_density = physics::Pa_PER_Atm * pow( (1-((0.0065*elevation)/288.0)), (physics::GRAVITY_MS2/(0.0065*287.15)) ) / (287.15*(288.0-0.0065*elevation));

					// 'RUN' MODEL ****************************************************************************************
					double total_energy_turbine=0;//, total_energy_generic=0;
					std::vector<double> weibull_cummulative(pc_array_count, 0);
					std::vector<double> weibull_bin(pc_array_count, 0);
					//std::vector<double> weibull_probability(m_iLengthOfTurbinePowerCurveArray, 0);
					//std::vector<double> energy_turbine(m_iLengthOfTurbinePowerCurveArray, 0);	// energy from turbine chosen from library

					// weibull_k = 2.10; // used for testing: this is off in the 5th significant digit when passed into SSC from samwx
					weibull_cummulative[0] = 0.0;
					weibull_bin[0] = 0.0;
					turbine_outkW[0] = 0.0;
					for (size_t i=1; i<pc_array_count; i++)
					{
						// calculate Weibull likelihood of the wind blowing in the range from windspeed[i-1] to windspeed[i]
						weibull_cummulative[i] = 1.0 - exp(-pow(pc_w[i]/lambda,weibull_k));
						weibull_bin[i] = weibull_cummulative[i] - weibull_cummulative[i-1];
						// THIS IS NOT FOR THE BIN wind speed[i to i-1]: weibull_probability[i] = ( (weibull_k / pow(lambda,weibull_k)) * pow(wind_speed[i],(weibull_k - 1)) * exp(-pow(wind_speed[i]/lambda,weibull_k)) );

						// calculate annual energy from turbine at this wind speed = (hours per year at this wind speed) X (turbine output at wind speed)
						turbine_outkW[i] = (8760.0 * weibull_bin[i]) * pc_p[i];

						// keep track of cummulative output
						total_energy_turbine += turbine_outkW[i];
					}

		double turbine_kw = total_energy_turbine;

		tcs_number_t farm_kw = (tcs_number_t) turbine_kw * pc_array_count;

		size_t nstep = 8760;
		tcs_number_t *turbine_output = allocate( "turbine_output", pc_array_count );
		tcs_number_t *farmpwr = allocate( "farmpwr", nstep );

		for (int i=0;i<nstep;i++)
			farmpwr[i] = farm_kw/ (tcs_number_t) nstep;

		for (int i=0; i<pc_array_count; i++)
			turbine_output[i] = (tcs_number_t) turbine_outkW[i];

		// have to be allocated to return without errors
		tcs_number_t *wspd = allocate("windspd", nstep);
		tcs_number_t *wdir = allocate("winddir", nstep);
		tcs_number_t *air_temp = allocate("temp", nstep);
		tcs_number_t *air_pres = allocate("pres", nstep);

		return;
	} // exec
};

double gammaln(double x)
{
    // Based on VBA code in Xnumbers.xla v 5.6
	// by Foxes Team, 2007
    // E -mail: leovlp@libero.it
    // Web:    http://digilander.libero.it/foxes
	// 10.11.2006

	double z, w, s, p, mantissa, expo;
	std::vector<double> cf(15);
	const double DOUBLEPI = 2 * 3.1415;
    const double G_ = 607.0/128.0; //= 4.7421875
    
    z = x - 1;
    cf[0] = 0.999999999999997;
    cf[1] = 57.1562356658629;
    cf[2] = -59.5979603554755;
    cf[3] = 14.1360979747417;
    cf[4] = -0.49191381609762;
    cf[5] = 3.39946499848119E-05;
    cf[6] = 4.65236289270486E-05;
    cf[7] = -9.83744753048796E-05;
    cf[8] = 1.58088703224912E-04;
    cf[9] = -2.10264441724105E-04;
    cf[10] = 2.17439618115213E-04;
    cf[11] = -1.64318106536764E-04;
    cf[12] = 8.44182239838528E-05;
    cf[13] = -2.61908384015814E-05;
    cf[14] = 3.68991826595316E-06;
    
    w = exp(G_)/sqrt(DOUBLEPI);
    s = cf[0];

	for(int i=1; i<15; i++){
        s += cf[i] / (z + i);
	}
    s = s / w;
    p = log((z + G_ + 0.5) / exp(1.0)) * (z + 0.5) / log(10.0);
    
	//split in mantissa and exponent to avoid overflow
    expo = floor(p);
    p = p - floor(p);
    mantissa = pow(10, p) * s;
    
	//rescaling
    p = floor(log(mantissa) / log(10.0));  // 'int' replaced with '' since VBA 'int' rounds negative numbers down
    mantissa = mantissa * pow(10.0, -p);
    expo = expo + p;

	return log(mantissa) + expo * log(10.0);
}


DEFINE_MODULE_ENTRY( windpower, "Test wind module for tcs interop", 217 );

