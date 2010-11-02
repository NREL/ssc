#include "core.h"
#include "lib_wfhrly.h"
#include "lib_windwatts.h"

static var_info _cm_vtab_windwatts[] = {
	{ SSC_INPUT,        SSC_STRING,      "file_name",                  "local weather file path",          "",       "",                      "Weather",      "*",               "LOCAL_FILE",            "" },
		
	{ SSC_INPUT,        SSC_NUMBER,      "shear",                      "Shear exponent",                   "",       "",                      "WindWatts",      "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "turbul",                     "Turbulence coefficient",           "frac",   "",                      "WindWatts",      "*",             "",                      "" },
	{ SSC_INPUT,        SSC_ARRAY,       "pc_wind",                    "Power curve wind speed array",     "0/1/2",  "",                      "WindWatts",      "*",             "",                      "" }, 
	{ SSC_INPUT,        SSC_ARRAY,       "pc_power",                   "Power curve turbine output array", "deg",    "",                      "WindWatts",      "*",             "LENGTH_EQUAL=pc_wind",  "" },
	{ SSC_INPUT,        SSC_ARRAY,       "wt_x",                       "Turbine X coordinates",            "m",      "",                      "WindWatts",      "*",             "",                      "" },
	{ SSC_INPUT,        SSC_ARRAY,       "wt_y",                       "Turbine Y coordinates",            "m",      "",                      "WindWatts",      "*",             "LENGTH_EQUAL=wt_x",     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hub_ht",                     "Hub height",                       "m",      "",                      "WindWatts",      "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "rotor_di",                   "Rotor diameter",                   "m",      "",                      "WindWatts",      "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ctl_mode",                   "Control mode",                     "0/1/2",  "",                      "WindWatts",      "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cutin",                      "Cut-in wind speed",                "m/s",    "",                      "WindWatts",      "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "lossc",                      "Constant losses",                  "kW",     "",                      "WindWatts",      "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "lossp",                      "Percentage losses",                "0-100",  "",                      "WindWatts",      "*",             "",                      "" },
	
	{ SSC_OUTPUT,       SSC_ARRAY,       "farmpwr",                    "AC wind farm power",               "kWhdc",  "",                      "WindWatts",      "*",             "LENGTH=8760",     "" },
	{ SSC_OUTPUT,       SSC_MATRIX,      "wtpwr",                      "Power at each WT",                 "kWhac",  "",                      "WindWatts",      "*",             "ROWS=8760",       "" },
	{ SSC_OUTPUT,       SSC_MATRIX,      "wteff",                      "Eff at each WT",                   "kWhac",  "",                      "WindWatts",      "*",             "ROWS=8760",       "" },
	{ SSC_OUTPUT,       SSC_MATRIX,      "wtvel",                      "Wind speed at each WT",            "kWhac",  "",                      "WindWatts",      "*",             "ROWS=8760",       "" },



var_info_invalid };

class cm_windwatts : public compute_module
{
private:
public:
	
	class weather_reader
	{
	public:
		weather_reader() : wf(0) {  }
		~weather_reader() { if (wf) wf_close(wf); }
		wf_obj_t wf;
	};

	cm_windwatts()
	{
		add_var_info( _cm_vtab_windwatts );
	}

	bool exec( ) throw( general_error )
	{
		const char *file = as_string("file_name");
		
		wf_header hdr;
		wf_data dat;
		weather_reader reader;
		reader.wf= wf_open( file, &hdr );

		if (!reader.wf) throw exec_error("windwatts", "failed to read local weather file: " + std::string(file));

		size_t pc_len = 0;
		ssc_number_t *pc_w = as_array( "pc_wind", &pc_len );
		ssc_number_t *pc_p = as_array( "pc_power", NULL );

		size_t nwt = 0;
		ssc_number_t *wt_x = as_array( "wt_x", &nwt );
		ssc_number_t *wt_y = as_array( "wt_y", NULL );

		double shear = as_double("shear");
		double turbul = as_double("turbul");
		double hub_ht = as_double("hub_ht");
		double rotor_di = as_double("rotor_di");
		int ctl_mode = as_integer("ctl_mode");
		double cutin = as_double("cutin");
		double lossc = as_double("lossc");
		double lossp = as_double("lossp");

		std::vector<double> Dn(nwt), Cs(nwt), 
			Power(nwt), Thrust(nwt), Eff(nwt), 
			Wind(nwt), Turb(nwt), 
			
			X(nwt), Y(nwt),
			dpcW(pc_len), dpcP(pc_len);

		size_t i,j;

		for (i=0;i<nwt;i++)
		{
			X[i] = (double)wt_x[i];
			Y[i] = (double)wt_y[i];
		}

		for (i=0;i<pc_len;i++)
		{
			dpcW[i] = (double)pc_w[i];
			dpcP[i] = (double)pc_p[i];
		}

		ssc_number_t *farmpwr = allocate( "farmpwr", 8760 );
		ssc_number_t *wspd = allocate("wspd", 8760);
		ssc_number_t *wdir = allocate("wdir", 8760);
		util::matrix_t<ssc_number_t> &mat_wtpwr = allocate_matrix( "wtpwr", 8760, nwt );
		util::matrix_t<ssc_number_t> &mat_wteff = allocate_matrix( "wteff", 8760, nwt );
		util::matrix_t<ssc_number_t> &mat_wtvel = allocate_matrix( "wtvel", 8760, nwt );
		util::matrix_t<ssc_number_t> &mat_dn = allocate_matrix("dn", 8760, nwt );
		util::matrix_t<ssc_number_t> &mat_cs = allocate_matrix("cs", 8760, nwt );

		double last_wind, last_theta, wind, theta;
		wf_read_data( reader.wf, &dat );  // read the first line
		wind = last_wind = dat.wspd;
		theta = last_theta = dat.wdir;

		for (i=0;i<8760;i++)
		{
			
			double farmp = 0;

			if ( (int)nwt != wind_power( 
						/* inputs */
						wind,
						theta,
						shear,
						turbul,
						dat.pres*0.000986923267,  /* convert mbar to Atm */
						dat.tdry,
						nwt,
						X.data(),
						Y.data(),
						pc_len,
						dpcW.data(),
						dpcP.data(),
						10.0, /* 10 meter data measure height in TMY2 */
						hub_ht,
						rotor_di,
						ctl_mode,
						cutin,
						0,
						0,
						lossc,
						lossp,

						/* outputs */
						&farmp,
						Dn.data(),
						Cs.data(),
						Power.data(),
						Thrust.data(),
						Eff.data(),
						Wind.data(),
						Turb.data() ) ) 
				throw exec_error( "windwatts", util::format("error in wind calculation time %d", i) );


			farmpwr[i] = (ssc_number_t) farmp;
			wspd[i] = (ssc_number_t) wind;
			wdir[i] = (ssc_number_t) theta;

			for (j=0;j<nwt;j++)
			{
				mat_dn.at(i,j) = (ssc_number_t)Dn[j];
				mat_cs.at(i,j) = (ssc_number_t)Cs[j];
				mat_wtpwr.at(i,j) = (ssc_number_t) Power[j];
				mat_wteff.at(i,j) = (ssc_number_t) Eff[j];
				mat_wtvel.at(i,j) = (ssc_number_t) Wind[j];
			}

			
			if (i < 8759)
			{
				if (!wf_read_data( reader.wf, &dat ))
					throw exec_error("easywatts", "could not read data line " + util::to_string((int)i+1) + " of 8760");

				wind = (last_wind+dat.wspd)/2.0;
				theta = (last_theta+dat.wdir)/2.0;
				last_wind = dat.wspd;
				last_theta = dat.wdir;
			}

		}

		return true;
	}
};

DEFINE_MODULE_ENTRY( windwatts, "Wind farm model (ported from original TRNSYS P.Quinlan)", 1 );

