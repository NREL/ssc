#include <math.h>

#include "core.h"
#include "lib_weatherfile.h"
#include "lib_irradproc.h"
#include "lib_util.h"

#ifndef M_PI
#define M_PI 3.141592653589793238462643
#endif

static var_info _cm_vtab_swhsolopt[] = {
/*   VARTYPE           DATATYPE         NAME                      LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_STRING,      "file_name",             "local weather file path",          "",       "",                      "Weather",      "*",                         "LOCAL_FILE",                          "" },

	{ SSC_INPUT,        SSC_ARRAY,       "scaled_draw",           "Hot water draw",                   "kg/hr",  "",                      "SWHsolopt",      "*",                       "LENGTH=8760",               "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "tilt",                  "Collector tilt",                   "deg",    "",                      "SWHsolopt",      "*",                       "MIN=0,MAX=90",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "azimuth",               "Collector azimuth",                "deg",    "90=E,180=S",            "SWHsolopt",      "*",                       "MIN=0,MAX=360",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "albedo",                "Ground reflectance factor",        "0..1",   "",                      "SWHsolopt",      "*",                       "FACTOR",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "irrad_mode",            "Irradiance input mode",            "0/1",    "Beam+Diff,Global+Beam", "SWHsolopt",      "?=0",                     "INTEGER,MIN=0,MAX=1",               "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sky_model",             "Tilted surface irradiance model",  "0/1/2",  "Isotropic,HDKR,Perez",  "SWHsolopt",      "?=2",                     "INTEGER,MIN=0,MAX=2",               "" },

	{ SSC_INPUT,        SSC_NUMBER,      "mdot",                  "Total system mass flow rate",      "kg/s",   "",                      "SWHsolopt",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ncoll",                 "Number of collectors",             "",       "",                      "SWHsolopt",      "*",                       "POSITIVE,INTEGER",                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fluid",			      "Working fluid in system",          "",       "Water,Glycol",          "SWHsolopt",      "*",                       "INTEGER,MIN=0,MAX=1",               "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "area_coll",             "Single collector area",            "m2",     "",                      "SWHsolopt",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "FRta",                  "FRta",                             "",       "",                      "SWHsolopt",      "*",                       "",                                  "" }, 
	{ SSC_INPUT,        SSC_NUMBER,      "FRUL",                  "FRUL",                             "",       "",                      "SWHsolopt",      "*",                       "",                                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "b0",                    "Incidence angle modifier",         "",       "",                      "SWHsolopt",      "*",                       "",                                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "test_fluid",            "Fluid used in collector test",     "",       "Water,Glycol",          "SWHsolopt",      "*",                       "INTEGER,MIN=0,MAX=1",               "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "pipe_length",           "Length of piping in system",       "m",      "",                      "SWHsolopt",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pipe_diam",             "Pipe diameter",                    "m",      "",                      "SWHsolopt",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pipe_k",                "Pipe insulation conductivity",     "W/m2.C", "",                      "SWHsolopt",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pipe_insul",            "Pipe insulation thickness",        "m",      "",                      "SWHsolopt",      "*",                       "POSITIVE",                          "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "tank_h2d_ratio",        "Solar tank height to diameter ratio", "",    "",                      "SWHsolopt",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "U_tank",                "Solar tank heat loss coefficient",  "W/m2K", "",                      "SWHsolopt",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "V_tank",                "Solar tank volume",                 "m3",    "",                      "SWHsolopt",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hx_eff",                "Heat exchanger effectiveness",      "0..1",  "",                      "SWHsolopt",      "*",                       "POSITIVE",                          "" },

	{ SSC_INPUT,        SSC_NUMBER,      "T_room",                "Ambient temperature in mech room", "C",      "",                      "SWHsolopt",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "T_tank_max",            "Max temperature in storage tank",  "C",      "",                      "SWHsolopt",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "T_set",                 "Set temperature",                  "C",      "",                      "SWHsolopt",      "*",                       "POSITIVE",                          "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "pump_power",            "Pump power",                       "W",      "",                      "SWHsolopt",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pump_eff",              "Pumping efficiency",               "%",      "",                      "SWHsolopt",      "*",                       "PERCENT",                           "" },
	
	{ SSC_OUTPUT,       SSC_ARRAY,       "beam",                  "Beam irradiance",                  "W/m2",  "",                      "SWHsolopt",      "*",                            "LENGTH=8760",               "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "diffuse",               "Diffuse irradiance",               "W/m2",  "",                      "SWHsolopt",      "*",                            "LENGTH=8760",               "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_dry",                 "Dry bulb temperature",             "C",     "",                      "SWHsolopt",      "*",                            "LENGTH=8760",               "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_mains",               "Mains temperature",                "C",     "",                      "SWHsolopt",      "*",                            "LENGTH=8760",               "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "poa",                   "Plane of array irradiance",        "W/m2",  "",                      "SWHsolopt",      "*",                            "LENGTH=8760",               "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "incident",              "Incident irradiance",              "W/m2",  "",                      "SWHsolopt",      "*",                            "LENGTH=8760",               "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_useful",              "Q useful",                         "Wh",    "",                      "SWHsolopt",      "*",                            "LENGTH=8760",               "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_deliv",               "Q delivered",                      "Wh",    "",                      "SWHsolopt",      "*",                            "LENGTH=8760",               "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_loss",                "Q loss",                           "Wh",    "",                      "SWHsolopt",      "*",                            "LENGTH=8760",               "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_tank",                "T tank",                           "C",     "",                      "SWHsolopt",      "*",                            "LENGTH=8760",               "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_deliv",               "T delivered",                      "C",     "",                      "SWHsolopt",      "*",                            "LENGTH=8760",               "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_pump",                "Pumping power",                    "Wh",    "",                      "SWHsolopt",      "*",                            "LENGTH=8760",               "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_aux",                 "Q auxiliary",                      "Wh",    "",                      "SWHsolopt",      "*",                            "LENGTH=8760",               "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_auxonly",             "Q auxiliary only",                 "Wh",    "",                      "SWHsolopt",      "*",                            "LENGTH=8760",               "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_saved",               "Q saved",                          "Wh",    "",                      "SWHsolopt",      "*",                            "LENGTH=8760",               "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "mode",                  "Operation mode",                   "Wh",    "1,2,3,4",               "SWHsolopt",      "*",                            "LENGTH=8760",               "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_hot",                 "T hot",                            "C",     "",                      "SWHsolopt",      "*",                            "LENGTH=8760",               "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_cold",                "T cold",                           "C",     "",                      "SWHsolopt",      "*",                            "LENGTH=8760",               "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "V_hot",                 "V hot",                            "m3",    "",                      "SWHsolopt",      "*",                            "LENGTH=8760",               "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "V_cold",                "V cold",                           "m3",    "",                      "SWHsolopt",      "*",                            "LENGTH=8760",               "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "draw",                  "Hot water draw",                   "kg/hr",  "",                      "SWHsolopt",      "*",                       "LENGTH=8760",               "" },
	
	var_info_invalid };

class cm_swhsolopt : public compute_module
{
public:
	
	cm_swhsolopt()
	{
		add_var_info( _cm_vtab_swhsolopt );
	}

	void exec( ) throw( general_error )
	{
		const char *file = as_string("file_name");

		weatherfile wf(file);
		if (!wf.ok()) throw exec_error("swhsolopt", "failed to read local weather file: " + std::string(file));
				
		/* **********************************************************************
		   Read user specified system parameters from compute engine
		   ********************************************************************** */
		
	
		double albedo = as_double("albedo"); // ground reflectance fraction
		double tilt = as_double("tilt"); // collector tilt in degrees
		double azimuth = as_double("azimuth"); // collector azimuth in degrees  (180=south, 90=east)
		int irrad_mode = as_integer("irrad_mode"); // 0=beam&diffuse, 1=total&beam
		int sky_model = as_integer("sky_model"); // 0=isotropic, 1=hdkr, 2=perez
	
		size_t len;
		ssc_number_t *draw = as_array("scaled_draw", &len);
		if ( len != 8760 ) throw exec_error("swhsolopt", "draw profile must have 8760 values");

		double mdot = as_double("mdot"); // total system mass flow rate (kg/s)
		double area = as_double("area_coll") * as_integer("ncoll"); // total solar collector area (m2)

		int ifluid = as_integer("fluid"); // 0=water, 1=glycol
		double fluid_cp = (ifluid==0) ? 4816 : 3705;  // working fluid specific heat in J/kgK
		
		int itest = as_integer("test_fluid"); // 0=water, 1=glycol
		double test_cp = (itest==0) ? 4816 : 3705;  // test fluid specific heat in J/kgK

		double FRta = as_double("FRta"); // FR(ta)_n (D&B pp 291) (dimensionless) collector heat removal factor * effective transmittance-absorption product (intercept on efficiency curve); indication of how energy is absorbed.
		double FRUL = as_double("FRUL"); // FRUL (D&B pp 291) (W/m2.C)  collector heat removal factor * collector heat loss coefficient (slope of efficiency curve); indication of how energy is lost.
		double b0 = as_double("b0"); // incidence angle modifier coefficient (D&B pp 297) (unitless)

		double pipe_diam = as_double("pipe_diam"); // pipe diameter in system (m)
		double pipe_k = as_double("pipe_k"); // pipe insulation conductivity (W/m2.C)
		double pipe_insul = as_double("pipe_insul");  // pipe insulation thickness (m)
		double pipe_length = as_double("pipe_length"); // length of piping in system (m)

		double tank_h2d_ratio = as_double("tank_h2d_ratio"); // ratio of tank height to diameter (dimensionless)
		double U_tank = as_double("U_tank"); // W/m2.C storage tank heat loss coefficient (U-value)
		double V_tank = as_double("V_tank"); // solar tank volume (m3)
		double Eff_hx = as_double("hx_eff"); // heat exchanger effectiveness (0..1)

		double T_room = as_double("T_room"); // ambient temperature in mechanical room or location of storage tank, hx, etc
		double T_tank_max = as_double("T_tank_max"); // max temp of water in storage tank
		double T_set = as_double("T_set"); // hot water set point temperature
	
		double pump_watts = as_double("pump_power"); // pump size in Watts
		double pump_eff = as_double("pump_eff"); // pumping efficiency
		
		double tank_radius = pow( V_tank / (2*M_PI*tank_h2d_ratio), 0.33333333 );
		double tank_height = tank_radius * 2 * tank_h2d_ratio;
		double tank_area =  2*M_PI*tank_radius*tank_radius + 2*M_PI*tank_radius*tank_height; // 2*pi*R^2 + 2*pi*r*h
		double UA_tank = tank_area * U_tank;
		
		double pipe_od = pipe_diam + pipe_insul*2;
		// pipe U calculation (W/m2.C), http://en.wikipedia.org/wiki/Heat_transfer_coefficient (of pipe wall, h_wall = 2*k/(i*ln(o/i)) )
		double U_pipe = 2 * pipe_k / ( pipe_od * ::log( pipe_od / pipe_diam ) ); //  **TODO** CHECK whether should be pipe_diam*log(pipe_od/pipe_diam) in denominator
		double UA_pipe = U_pipe * M_PI * pipe_od * pipe_length; // W/'C
		
		/* **********************************************************************
		   Initialize data storage, read weather file, set draw profile 
		   ********************************************************************** */	
		int i;
		double dT = 3600; // Time step, seconds

		
		ssc_number_t *Beam = allocate("beam", 8760);
		ssc_number_t *Diffuse = allocate("diffuse", 8760);
		ssc_number_t *T_dry = allocate("T_dry", 8760);
		ssc_number_t *T_mains = allocate("T_mains", 8760);
		ssc_number_t *out_Draw = allocate("draw", 8760);
		ssc_number_t *PlaneOfArray = allocate("poa", 8760 );
		ssc_number_t *Incident = allocate("incident", 8760);
		ssc_number_t *G_Tcrit = allocate("G_Tcrit", 8760);
		
		ssc_number_t *out_FRta_use = allocate("FRta_use", 8760);
		ssc_number_t *out_FRUL_use = allocate("FRUL_use", 8760);

		ssc_number_t *out_Q_useful = allocate("Q_useful", 8760);
		ssc_number_t *out_Q_deliv = allocate("Q_deliv", 8760);
		ssc_number_t *out_Q_loss = allocate("Q_loss", 8760);
		ssc_number_t *out_T_tank = allocate("T_tank", 8760);
		ssc_number_t *out_T_deliv = allocate("T_deliv", 8760);
		ssc_number_t *out_P_pump = allocate("P_pump", 8760);
		ssc_number_t *out_Q_aux = allocate("Q_aux", 8760);
		ssc_number_t *out_Q_auxonly = allocate("Q_auxonly", 8760);
		ssc_number_t *out_Q_saved = allocate("Q_saved", 8760);

		ssc_number_t *out_V_hot = allocate("V_hot", 8760);
		ssc_number_t *out_V_cold = allocate("V_cold", 8760);
		ssc_number_t *out_T_hot = allocate("T_hot", 8760);
		ssc_number_t *out_T_cold = allocate("T_cold", 8760);

		ssc_number_t *Mode = allocate("mode", 8760);
	
		/*
		Draw[0] = 2.4f;
		Draw[1] = 1.2f;
		Draw[2] = 0.8f;
		Draw[3] = 1.0f;
		Draw[4] = 1.9f;
		Draw[5] = 6.1f;
		Draw[6] = 14.0f;
		Draw[7] = 16.2f;
		Draw[8] = 15.7f;
		Draw[9] = 13.9f;
		Draw[10] = 11.9f;
		Draw[11] = 10.1f;
		Draw[12] = 8.6f;
		Draw[13] = 7.7f;
		Draw[14] = 6.9f;
		Draw[15] = 7.0f;
		Draw[16] = 8.0f;
		Draw[17] = 10.2f;
		Draw[18] = 12.2f;
		Draw[19] = 12.6f;
		Draw[20] = 11.4f;
		Draw[21] = 9.9f;
		Draw[22] = 7.6f;
		Draw[23] = 5.1f;
		*/
	
		double temp_sum = 0;
		double monthly_avg_temp[12];
		for (i=0;i<12;i++)
			monthly_avg_temp[i] = 0;
	
		for ( i=0; i < 8760; i++ )
		{
			wf.read();
			
			Beam[i] = (ssc_number_t)wf.dn;
			Diffuse[i] = (ssc_number_t)wf.df;
			T_dry[i] = (ssc_number_t)wf.tdry;		
			T_mains[i] = 0;
			PlaneOfArray[i] = 0;
			Incident[i] = 0;
			   
		/* **********************************************************************
		   Process radiation (Isotropic model), calculate Incident[i] through cover
		   ********************************************************************** */	
			irrad tt;
			if (irrad_mode == 0) tt.set_beam_diffuse( wf.dn, wf.df );
			else tt.set_global_beam( wf.gh, wf.dn );
			tt.set_location( wf.lat, wf.lon, wf.tz );
			tt.set_time( wf.year, wf.month, wf.day, wf.hour, wf.minute, 1 );
			tt.set_sky_model( sky_model /* isotropic=0, hdkr=1, perez=2 */, albedo );
			tt.set_surface( 0, tilt, azimuth, 0, 0, 0 );
			tt.calc();

			double poa[3];
			tt.get_poa( &poa[0], &poa[1], &poa[2], 0, 0, 0 );
			PlaneOfArray[i] = (ssc_number_t)( poa[0] + poa[1] + poa[2] ); // total PoA on surface

			double aoi = 0;
			tt.get_angles( &aoi, 0, 0, 0, 0 ); // note: angles returned in degrees

			// -------------------------------------
			// calculate transmittance through cover
					
			// incidence angle modifier (IAM) for beam (D&B eqn 6.17.10 pp 297)
			double Kta_b = 1 - b0*( 1/cos(aoi*M_PI/180) - 1 );
			if (Kta_b < 0) Kta_b = 0;
			if (Kta_b > 1) Kta_b = 0;
		
			// effective incidence angle for sky diffuse radiation (D&B eqn 5.4.2 pp 215)
			double cos_theta_eff_diffuse = cos( 59.7*M_PI/180 - 0.1388*tilt*M_PI/180 + 0.001497*tilt*M_PI/180*tilt*M_PI/180 );
			// incidence angle modifier (IAM) for diffuse (D&B eqn 6.17.10 pp 297)
			double Kta_d = 1 - b0*( 1/cos_theta_eff_diffuse - 1 );
			if (Kta_d < 0) Kta_d = 0;
		
			// effective incidence angle modifier for ground reflected radiation (D&B eqn 5.4.1 pp 215)
			double cos_theta_eff_ground = cos( 90*M_PI/180 - 0.5788*tilt*M_PI/180 + 0.002693*tilt*M_PI/180*tilt*M_PI/180 );
			// incidence angle modifier (IAM) for ground reflected radiation (D&B eqn 6.17.10 pp 297)
			double Kta_g = 1 - b0*( 1/cos_theta_eff_ground - 1);
			if (Kta_g < 0) Kta_g = 0;
			

			Incident[i] = (ssc_number_t)( Kta_b*poa[0]
				+ Kta_d*poa[1]
				+ Kta_g*poa[2] );
		
			temp_sum += T_dry[i];
			monthly_avg_temp[ util::month_of(i) - 1 ] += T_dry[i];
		}
	
		// Algorithm for calculating mains water temperature from paper 
		// ASES 2007 (J.Burch & C.Christensen)
		// "Towards Development of an Algorithm for Mains Water Temperature"
		// Verified against code in TRNSYS Type 15 Weather Reader and SolOpt
		double min_monthly_avg = 1e99;
		double max_monthly_avg = -1e99;
		for (i=0;i<12;i++)
		{
			monthly_avg_temp[i] = monthly_avg_temp[i]/( util::nday[i]*24 );
			if (monthly_avg_temp[i] < min_monthly_avg) min_monthly_avg = monthly_avg_temp[i];
			if (monthly_avg_temp[i] > max_monthly_avg) max_monthly_avg = monthly_avg_temp[i];
		}
	
		double avg_temp_high_f = 32 + 1.8 * max_monthly_avg;
		double avg_temp_low_f = 32 + 1.8 * min_monthly_avg;
		double annual_avg_temp = temp_sum / 8760 * 1.8 + 32;
	
		double mains_ratio = 0.4 + 0.01*(annual_avg_temp-44);
		double lag = 35 - (annual_avg_temp - 44);
		
		/* **********************************************************************
		   Calculate hourly mains water temperature
		   ********************************************************************** */
		for ( i=0; i < 8760; i++ )
		{
			// calculate hour of day  ( goes 1..24 )
			// and julian day  ( goes 1..365 )
		
			// (Julian day is used in the Julian date (JD) system of time measurement for scientific use by 
			// the astronomy community, presenting the interval of time in days and fractions of a day since 
			// January 1, 4713 BC Greenwich noon - WIKIPEDIA)
			int hour = 0;
			int julian_day = (int)(((double) (i+1) )/24.0);		
			if ( (double)julian_day == (((double) (i+1) )/24) )
				hour = 24;
			else
			{
				hour =  (i+1) - (julian_day*24);
				julian_day++;
			}
				
			T_mains[i] = (ssc_number_t)(  annual_avg_temp + 5 + mains_ratio * ( (avg_temp_high_f - avg_temp_low_f)/2 )
					* sin( M_PI/180*(0.986*(julian_day-15-lag)-90)) );
			T_mains[i] = (ssc_number_t)( (T_mains[i]-32)/1.8 ); // convert to 'C
		}
	
	
		/* **********************************************************************
		   Calcuate additional SHW system parameters 
		   ********************************************************************** */

		/* set initial conditions on some simulation variables */
		double V_hot = 0.8 * V_tank;
		double V_cold = V_tank-V_hot;
		double T_hot = T_mains[1] + 40; // initial hot temp 40'C above ambient
		double T_cold = T_mains[1];
		double T_hx_in = 45; // initial inlet temp of heat exchanger 'C
		double T_hx_out = 40; // initial outlet temp of heat exchanger 'C
		
		double Q_tankloss = 0;
		double Q_useful_prev = 0.0;
		double T_tank_prev = V_hot/V_tank*T_hot + V_cold/V_tank*T_cold; // weighted average tank temperature (initial)
		double T_deliv_prev = 0.0;

		/* **********************************************************************
		   Calculate SHW performance: Q_useful, Q_deliv, T_deliv, T_tank, Q_pump, Q_aux, Q_auxonly, Q_saved
		   ********************************************************************** */	
	
		double Cp_water = 4816; // Cp_water@15'C (J/kg.K)  assume constant fluid properties

		double Cp_tank = Cp_water; // assume tank is water. Cp_water@15'C (J/kg.K)
		double rho_tank = 1000; // density of water, kg/m3

		for ( i=0; i < 8760; i++ )
		{
			// at beginning of this timestep, temp is the same as end of last timestep
			double T_tank = T_tank_prev;
			double Q_useful = Q_useful_prev;
			double T_deliv = T_deliv_prev;
		
			double mdotCp_coll = mdot * fluid_cp; // mass flow rate (kg/s) * Cp_fluid (J/kg.K)
			double mdotCp_test = mdot * test_cp; // mass flow rate (kg/s) * Cp_test
		
			// update HX inlet temp using useful energy absorbed in last time step Qu=epsilon*mCp(Ti-Ts)
			T_hx_in = T_tank + Q_useful / ( Eff_hx * mdotCp_coll );
		
			// update HX outlet temp by knowing amount of energy transferred to tank Qu=mCp(Ti-To)
			T_hx_out = T_hx_in - Q_useful / mdotCp_coll;
		
			/* Flow rate corrections to FRta, FRUL (D&B pp 307) */
			double FprimeUL = -mdotCp_test / area * ::log( 1 - FRUL*area/mdotCp_test ); // D&B eqn 6.20.4 **TODO** CHECK |use vs |test
			double r = ( mdotCp_coll/area*(1-exp(-area*FprimeUL/mdotCp_coll)) ) / FRUL; // D&B eqn 6.20.3
			double FRta_use = r*FRta;
			double FRUL_use = r*FRUL;
				
			/* Pipe loss adjustment (D&B pp 430) */
			FRta_use = FRta_use / ( 1+UA_pipe/mdotCp_coll ); // D&B eqn 10.3.9
			FRUL_use = FRUL_use * ( (1-UA_pipe/mdotCp_coll + 2*UA_pipe/(area*FRUL_use) ) / (1 + UA_pipe/mdotCp_coll) ); // D&B eqn 10.3.10
				
			/* Heat exchanger adjustment (D&B pp 427) */
			double FR_ratio = 1/( 1 + (area*FRUL_use/mdotCp_coll)*(mdotCp_coll/(Eff_hx*mdotCp_coll)-1)); // D&B eqn 10.2.3
			FRta_use *= FR_ratio;
			FRUL_use *= FR_ratio;

			out_FRta_use[i] = (ssc_number_t)FRta_use;
			out_FRUL_use[i] = (ssc_number_t)FRUL_use;
								
			double mdot_mix = draw[i];		
			double T_tank_last_iter = 0.0;

			int niter = 0;
			do
			{
				if (T_deliv > T_set)
				{
					// limit flow rate to mixing valve by effective ratio of T_set/T_deliv
					mdot_mix =  draw[i] * (Cp_water*T_set - Cp_water*T_mains[i]) 
											/(Cp_tank *T_deliv - Cp_water*T_mains[i]);
				}
				
				double T_dry_prev = T_dry[i];
				if ( i > 0 ) T_dry_prev = T_dry[i-1];

				/* calculate critical radiation for operation */
				double Gcrit  = FRUL_use*( T_tank - T_dry[i] ) / FRta_use; // D&B eqn 6.8.2
				G_Tcrit[i] = (ssc_number_t)Gcrit;
				if ( Incident[i] > Gcrit )
					Q_useful = area*( FRta_use*Incident[i] - FRUL_use*(T_tank_prev - T_dry_prev) ); // D&B eqn 6.8.1 
				else
					Q_useful = 0.0; // absorbed radiation does not exceed thermal losses, etc => no operation
			
				T_tank_last_iter = T_tank;
			
				/* During solar collection, tank is assumed mixed.
				   During no solar collection hours, tank is assumed startifed (modeled with 2 variable volume nodes) */
				if (Q_useful > 0)
				{
				/* MIXED TANK -- solar collection */
			
					T_tank_last_iter = T_tank;
				
					if (Q_useful_prev == 0.0)
					{
						// this hour has solar collection, after previous hour with no solar collection
						T_tank = T_tank_prev;
						Mode[i] = 1;
					}
					else
					{
						// this hour has solar collection, after previous hour with solar collection
						T_tank = T_tank_prev * 1/(1+ mdot_mix/(rho_tank*V_tank))
							+ ( Q_useful*dT - Q_tankloss*dT - mdot_mix*Cp_tank*273.15 + mdot_mix*Cp_water*(T_mains[i]+273.15) )
							  / ( rho_tank * V_tank * Cp_tank * ( 1 + mdot_mix / (rho_tank*V_tank) ) );
					
						if (T_tank > T_tank_max) T_tank = T_tank_max;
						Q_tankloss = UA_tank * (T_tank - T_room);
						Mode[i] = 2;
					}
				
					T_deliv = T_tank;
				}
				else
				{
				/* STRATIFIED TANK -- no solar collection */
					if (Q_useful_prev > 0.0)
					{
						// after previous hour with collection
						V_hot = V_tank - mdot_mix/rho_tank;
						if (V_hot < 0) V_hot = 0;
						T_hot = T_tank_prev - UA_tank * V_hot/V_tank * (T_hot-T_room)*dT / (rho_tank * Cp_tank * V_tank);
						V_cold = V_tank-V_hot;
						T_cold = T_mains[i] - UA_tank * V_cold/V_tank * (T_mains[i]-T_room)*dT / (rho_tank * Cp_tank * V_tank);

						if (V_hot > 0)
							T_deliv = T_hot;
						else
							T_deliv = T_cold;

						Mode[i] = 3;
					}
					else
					{
						// after previous hour with no solar collections
						V_hot = V_hot - mdot_mix / rho_tank;
						if (V_hot < 0) V_hot = 0;
						T_hot = T_hot - UA_tank * V_hot / V_tank * (T_hot - T_room) * dT / (rho_tank * Cp_tank * V_tank);
						V_cold = V_tank-V_hot;
						// note: T_cold calculation is approximate, doesn't account for incoming cold water temperature
						T_cold = T_cold - UA_tank * V_cold / V_tank * (T_cold - T_room) * dT / (rho_tank * Cp_tank * V_tank);

						if (V_hot > 0)
							T_deliv = T_hot;
						else
							T_deliv = T_cold;

						Mode[i] = 4;
					}
				
					T_tank = V_hot / V_tank * T_hot + V_cold / V_tank * T_cold;
					Q_tankloss = UA_tank * V_hot / V_tank * (T_hot - T_room) + UA_tank * V_cold / V_tank * (T_cold - T_room);
				
					break; // no iteration when tank is stratified
				}
			} while ( fabs(T_tank_last_iter - T_tank) / T_tank >= 0.001 || ++niter >= 10 );
		
		
			// if the delivered water temperature from SHW is greater then set temp
			// mix with cold mains to lower temp to setp temp
			double Q_deliv = 0.0;
			if (T_deliv > T_set)
				Q_deliv = mdot_mix / dT * (Cp_tank*T_deliv - Cp_water*T_mains[i]);
			else
				Q_deliv = draw[i] / dT * (Cp_tank*T_deliv - Cp_water*T_mains[i]);
			
			double Q_loss = Q_tankloss;
		
			// calculate pumping losses (pump size is user entered) -
			double P_pump = (Q_useful > 0) ? pump_watts*pump_eff : 0.0;
		
			// amount of auxiliary energy needed to bring delivered water to set temperature
			double Q_aux = draw[i] / dT * Cp_water * (T_set - T_deliv);
			if (Q_aux < 0) Q_aux = 0.0;
		
			// amount of energy needed to bring T_mains to set temperature (without SHW)
			double Q_auxonly = draw[i] / dT * Cp_water * (T_set - T_mains[i]);
			if (Q_auxonly < 0) Q_auxonly = 0.0;
		
			// Energy saved by SHW system is difference between auxonly system and shw+aux system
			double Q_saved = Q_auxonly - Q_aux;

			// save some values for next iteration
			Q_useful_prev = Q_useful;
			T_tank_prev = T_tank;
			T_deliv_prev = T_deliv;

			// save output variables
			out_Q_useful[i] = (ssc_number_t) Q_useful;
			out_Q_deliv[i] = (ssc_number_t) Q_deliv;
			out_Q_loss[i] = (ssc_number_t) Q_loss;
			out_T_tank[i] = (ssc_number_t) T_tank;
			out_T_deliv[i] = (ssc_number_t) T_deliv;
			out_P_pump[i] = (ssc_number_t) P_pump;
			out_Q_aux[i] = (ssc_number_t) Q_aux;
			out_Q_auxonly[i] = (ssc_number_t) Q_auxonly;
			out_Q_saved[i] = (ssc_number_t) Q_saved;
			out_T_hot[i] = (ssc_number_t) T_hot;
			out_T_cold[i] = (ssc_number_t) T_cold;
			out_V_hot[i] = (ssc_number_t) V_hot;
			out_V_cold[i] = (ssc_number_t) V_cold;
			out_Draw[i] = draw[i]; // pass to outputs for visualization
		}

		// finished with calculations.
	}

};

DEFINE_MODULE_ENTRY( swhsolopt, "Solar Water Heating using SolOpt model with modifications.", 1 )
