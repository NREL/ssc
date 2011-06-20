#include <math.h>

#include "core.h"
#include "lib_wfhrly.h"
#include "lib_util.h"

#ifndef M_PI
#define M_PI 3.141592653589793238462643
#endif

static var_info _cm_vtab_swhsolopt[] = {
/*   VARTYPE           DATATYPE         NAME                      LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_STRING,      "file_name",             "local weather file path",          "",       "",                      "Weather",      "*",                       "LOCAL_FILE",                          "" },
		
	{ SSC_INPUT,        SSC_NUMBER,      "mdot_single_coll",      "Mass flow rate in each collector", "kg/s",   "",                      "SWHsolopt",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "area_coll",             "Total solar collector area",       "m2",     "",                      "SWHsolopt",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "area_single_coll",      "Single collector area",            "m2",     "",                      "SWHsolopt",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "FRta",                  "FRta",                             "",       "",                      "SWHsolopt",      "*",                       "",                                  "" }, 
	{ SSC_INPUT,        SSC_NUMBER,      "FRUL",                  "FRUL",                             "",       "",                      "SWHsolopt",      "*",                       "",                                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "b0",                    "Incidence angle modifier",         "",       "",                      "SWHsolopt",      "*",                       "",                                  "" },

	{ SSC_INPUT,        SSC_NUMBER,      "pipe_k",                "Pipe insulation conductivity",     "W/m2.C", "",                      "SWHsolopt",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pipe_insul",            "Pipe insulation thickness",        "m",      "",                      "SWHsolopt",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pipe_length",           "Length of piping in system",       "m",      "",                      "SWHsolopt",      "*",                       "POSITIVE",                          "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "tank_h2d_ratio",        "Tank height to diameter ratio",    "",       "",                      "SWHsolopt",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "U_tank",                "Tank heat loss coefficient",       "W/m2.C", "",                      "SWHsolopt",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "U_hx",                  "Heat exchanger transfer coeff",    "W/m2.C", "",                      "SWHsolopt",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hx_area_percent",       "% of tank area comprising HX",     "%",      "",                      "SWHsolopt",      "*",                       "PERCENT",                           "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "albedo",                "Ground reflectance factor",        "0..1",   "",                      "SWHsolopt",      "*",                       "FACTOR",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tilt",                  "Collector tilt",                   "deg",    "",                      "SWHsolopt",      "*",                       "MIN=0,MAX=90",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "azimuth",               "Collector azimuth",                "deg",    "",                      "SWHsolopt",      "*",                       "MIN=-180,MAX=180",                  "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "T_room",                "Ambient temperature in mech room", "C",      "",                      "SWHsolopt",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "T_tank_max",            "Max temperature in storage tank",  "C",      "",                      "SWHsolopt",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "T_set",                 "Set temperature",                  "C",      "",                      "SWHsolopt",      "*",                       "POSITIVE",                          "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "pump_watts",            "Pump power",                       "W",      "",                      "SWHsolopt",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pump_eff",              "Pumping efficiency",               "%",      "",                      "SWHsolopt",      "*",                       "PERCENT",                           "" },
	
	
	{ SSC_OUTPUT,       SSC_ARRAY,       "beam",                  "Beam irradiance",                  "W/m2",  "",                      "SWHsolopt",      "*",                            "LENGTH=8760",               "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "diffuse",               "Diffuse irradiance",               "W/m2",  "",                      "SWHsolopt",      "*",                            "LENGTH=8760",               "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_dry",                 "Dry bulb temperature",             "C",     "",                      "SWHsolopt",      "*",                            "LENGTH=8760",               "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_mains",               "Mains temperature",                "C",     "",                      "SWHsolopt",      "*",                            "LENGTH=8760",               "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "draw",                  "Hot water draw",                   "kg/hr", "",                      "SWHsolopt",      "*",                            "LENGTH=8760",               "" },
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

	var_info_invalid };

class cm_swhsolopt : public compute_module
{
public:

	class weather_reader
	{
	public:
		weather_reader() : wf(0) {  }
		~weather_reader() { if (wf) wf_close(wf); }
		wf_obj_t wf;
	};

	cm_swhsolopt()
	{
		add_var_info( _cm_vtab_swhsolopt );
	}

	void exec( ) throw( general_error )
	{
		const char *file = as_string("file_name");

		wf_header hdr;
		wf_data dat;
		weather_reader reader;
		reader.wf = wf_open( file, &hdr );

		if (!reader.wf) throw exec_error("swhsolopt", "failed to read local weather file: " + std::string(file));
				
		/* **********************************************************************
		   Read user specified system parameters from compute engine
		   ********************************************************************** */	
	
		double mdot_single_coll = as_double("mdot_single_coll"); // mass flow rate in each collector panel (kg/s)
		double area_coll = as_double("area_coll"); // total solar collector area in system (m2)
		double area_single_coll = as_double("area_single_coll"); // single collector area (m2)
		double FRta = as_double("FRta"); // FR(ta)_n (D&B pp 291) (dimensionless) collector heat removal factor * effective transmittance-absorption product (intercept on efficiency curve); indication of how energy is absorbed.
		double FRUL = as_double("FRUL"); // FRUL (D&B pp 291) (W/m2.C)  collector heat removal factor * collector heat loss coefficient (slope of efficiency curve); indication of how energy is lost.
		double b0 = as_double("b0"); // incidence angle modifier coefficient (D&B pp 297) (unitless)
	
		double pipe_k = as_double("pipe_k"); // pipe insulation conductivity (W/m2.C)
		double pipe_insul = as_double("pipe_insul");  // pipe insulation thickness (m)
		double pipe_length = as_double("pipe_length"); // length of piping in system (m)
		double tank_h2d_ratio = as_double("tank_h2d_ratio"); // ratio of tank height to diameter (dimensionless)

		double U_tank = as_double("U_tank"); // W/m2.C storage tank heat loss coefficient (U-value)
		double U_hx = as_double("U_hx"); // W/m2.C heat exchanger overall heat transfer coefficient
		double hx_area_percent = as_double("hx_area_percent"); // percentage of tank area that comprises the heat exchanger
	
		double albedo = as_double("albedo"); // ground reflectance fraction
		double tilt = as_double("tilt"); // collector tilt in degrees (aka beta in D&B pp 13)
		double azimuth = as_double("azimuth"); // collector azimuth in degrees  (-180 .. 180, 0=south) (in D&B pp 13)

		double T_room = as_double("T_room"); // ambient temperature in mechanical room or location of storage tank, hx, etc
		double T_tank_max = as_double("T_tank_max"); // max temp of water in storage tank
		double T_set = as_double("T_set"); // hot water set point temperature
	
		double pump_watts = as_double("pump_watts"); // pump size in Watts
		double pump_eff = as_double("pump_eff"); // pumping efficiency
	
	
		/* **********************************************************************
		   Initialize data storage, read weather file, set draw profile 
		   ********************************************************************** */	
		int i;
		double dT = 3600; // Time step, seconds

		
		ssc_number_t *Beam = allocate("beam", 8760);
		ssc_number_t *Diffuse = allocate("diffuse", 8760);
		ssc_number_t *T_dry = allocate("T_dry", 8760);
		ssc_number_t *T_mains = allocate("T_mains", 8760);
		ssc_number_t *Draw = allocate("draw", 8760);
		ssc_number_t *Incident = allocate("incident", 8760);

		ssc_number_t *arr_Q_useful = allocate("Q_useful", 8760);
		ssc_number_t *arr_Q_deliv = allocate("Q_deliv", 8760);
		ssc_number_t *arr_Q_loss = allocate("Q_loss", 8760);
		ssc_number_t *arr_T_tank = allocate("T_tank", 8760);
		ssc_number_t *arr_T_deliv = allocate("T_deliv", 8760);
		ssc_number_t *arr_P_pump = allocate("P_pump", 8760);
		ssc_number_t *arr_Q_aux = allocate("Q_aux", 8760);
		ssc_number_t *arr_Q_auxonly = allocate("Q_auxonly", 8760);
		ssc_number_t *arr_Q_saved = allocate("Q_saved", 8760);

		ssc_number_t *Mode = allocate("mode", 8760);
	
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
	
		for ( i=0; i < 8760; i++ )
		{	
			Beam[i] = Diffuse[i] = T_dry[i] = T_mains[i] = Incident[i] = 0.0f;			
			if (i >= 24) Draw[i] = Draw[i-24];
		}
	
		double latitude = hdr.lat; // degrees ('phi' in D&B pp 13)
		double temp_sum = 0;
		double monthly_avg_temp[12];
		for (i=0;i<12;i++)
			monthly_avg_temp[i] = 0;
	
		for ( i=0; i < 8760; i++ )
		{
			wf_read_data( reader.wf, &dat );		
			Beam[i] = (ssc_number_t)dat.dn;
			Diffuse[i] = (ssc_number_t)dat.df;
			T_dry[i] = (ssc_number_t)dat.tdry;	
		
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
		   Process radiation (Isotropic model), calculate Incident[i], T_mains[i]
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
		
			// declination (D&B eqn 1.6.1a pp 14)
			double declination = 23.45*sin(M_PI/180 * 360*(284+julian_day)/365);
		
			// hour angle (15 deg per hour, morning negative, afternoon positive) (D&B pp 13)
			double hour_angle = 15 * (12-hour);
		
			// cos(angle of incidence) (D&B eqn 1.6.2 pp 14) 
			double delta = declination * M_PI/180.0;   // delta: declination (deg)
			double phi = latitude * M_PI/180.0;        // phi: latitude (deg)
			double beta = tilt * M_PI/180.0;           // beta: tilt or slope (deg)
			double gamma = azimuth * M_PI/180.0;       // gamma: azimuth (deg)
			double omega = hour_angle * M_PI/180.0;    // omega: hour angle (deg)
				
			double cos_theta = sin(delta)*sin(phi)*cos(beta) - sin(delta)*cos(phi)*sin(beta)*cos(gamma)
				+ cos(delta)*cos(phi)*cos(beta)*cos(omega) + cos(delta)*sin(phi)*sin(beta)*cos(gamma)*cos(omega)
				+ cos(delta)*sin(beta)*sin(gamma)*sin(omega);
		
			// zenith (angle between vertical line from ground to sun) (D&B eqn 1.6.5 pp 15)
			// beam to ground incidence angle
			double cos_theta_z = cos(phi)*cos(delta)*cos(omega) + sin(phi)*sin(delta);
			if (cos_theta_z < 0) cos_theta_z = 0;
		
			// incidence angle modifier (IAM) for beam (D&B eqn 6.17.10 pp 297)
			double Kta_b = 1 - b0*( 1/cos_theta - 1 );
			if (Kta_b < 0) Kta_b = 0;
			if (Kta_b > 1) Kta_b = 0;
		
			// calculate transmittance through cover
		
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
				
			// Isotropic diffuse equation for determining absorbed solar radiation 'S' per unit area
			// D&B eqn 5.9.1 pp 221: S = I_b*R_b*Kta_b + ....
			//                         = I_b*cos(theta)/cos(theta_z)*Kta_b + ....
			// assumes radiation I_b is on horizontal plane, whereas we have it (dn) normal to sun
			// so dn*cos(theta)/I_b = R_b; I_b = dn*cos(theta)/R_b,  where R_b=cos_theta/cos_theta_z (D&B eqn 1.8.1 pp 24)
			double I_d = Diffuse[i]; // diffuse radiation per unit area, horizontal surface
			double I_g = Beam[i] * cos_theta_z + Diffuse[i]; // ground reflected
		
			double S_b = (Beam[i]*cos_theta) * Kta_b;
			double S_d = I_d * Kta_d * ( (1+cos(M_PI/180*tilt))/2 );
			double S_g = I_g * Kta_g * albedo * ( (1-cos(M_PI/180*tilt))/2 );
		
			Incident[i] = (ssc_number_t)( S_b + S_d + S_g );
		
		
			T_mains[i] = (ssc_number_t)(  annual_avg_temp + 5 + mains_ratio * ( (avg_temp_high_f - avg_temp_low_f)/2 )
					* sin( M_PI/180*(0.986*(julian_day-15-lag)-90)) );
			T_mains[i] = (ssc_number_t)( (T_mains[i]-32)/1.8 ); // convert to 'C
		}
	
	
		/* **********************************************************************
		   Calcuate additional SHW system parameters 
		   ********************************************************************** */	
	   
		int coll_fluid = GLYCOL34; // choice of collector fluid for determining properties (could be user specified later?)

		// circulation flow rate kg/s, multipled up by number of collectors in system (a little simplistic)
		double mdot_coll = mdot_single_coll * area_coll / area_single_coll; 
	
	
		// storage tank size calculation:  uses rule-of-thumb of 1.5 gallons per sqft collector area
		// area(m2) * 10.763 ft2/m2 * 1.5 gal/ft2 * 0.0037 m3/gal
		double V_tank = area_coll * 10.764 * 1.5 * 0.003785412;
		double tank_radius = pow( V_tank / (2*M_PI*tank_h2d_ratio), 0.33333333 );
		double tank_height = tank_radius * 2 * tank_h2d_ratio;
		double tank_area =  2*M_PI*tank_radius*tank_radius + 2*M_PI*tank_radius*tank_height; // 2*pi*R^2 + 2*pi*r*h
		double UA_tank = tank_area * U_tank;
		double UA_hx = U_hx * hx_area_percent/100 * tank_area; // W/'C
		
		// pipe sizing
		// linearized from table in "Solar Hot Water Systems" by Tom Lane
		// problem: for large collector areas, gives unreasonable pipe diameters?
		double pipe_diam = area_coll * 10.764 * 0.00005 + 0.0169; // pipe diam in meters
		double pipe_od = pipe_diam + pipe_insul*2;
		// pipe U calculation (W/m2.C), http://en.wikipedia.org/wiki/Heat_transfer_coefficient (of pipe wall, h_wall = 2*k/(i*ln(o/i)) )
		double U_pipe = 2 * pipe_k / ( pipe_od * ::log( pipe_od / pipe_diam ) ); //  **TODO** CHECK whether should be pipe_diam*log(pipe_od/pipe_diam) in denominator
		double UA_pipe = U_pipe * M_PI * pipe_od * pipe_length; // W/'C
	
		double Eff_hx = 0.5;
	
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
		
		for ( i=0; i < 8760; i++ )
		{
			// at beginning of this timestep, temp is the same as end of last timestep
			double T_tank = T_tank_prev;
			double Q_useful = Q_useful_prev;
			double T_deliv = T_deliv_prev;
		
			double Cp_coll = Cp(coll_fluid, (T_hx_in + T_hx_out)/2); // specific heat of collector fluid, at average Hx temp (J/kg.K)
			double mdotCp_coll = mdot_coll * Cp_coll; // mass flow rate (kg/s) * Cp (J/kg.K)
			double mdotCp_test = mdot_coll * 4186; // mass flow rate (kg/s) * Cp_water@15'C (J/kg.K)
			double Cp_tank = Cp(WATER, T_tank);
			double rho_tank = Density(WATER, T_tank);
		
			// update HX inlet temp using useful energy absorbed in last time step Qu=epsilon*mCp(Ti-Ts)
			T_hx_in = T_tank + Q_useful / ( Eff_hx * mdotCp_coll );
		
			// update HX outlet temp by knowing amount of energy transferred to tank Qu=mCp(Ti-To)
			T_hx_out = T_hx_in - Q_useful / mdotCp_coll;
		
			/* Flow rate corrections to FRta, FRUL (D&B pp 307) */
			double FprimeUL = -mdotCp_test / area_coll * ::log( 1 - FRUL*area_coll/mdotCp_test ); // D&B eqn 6.20.4 **TODO** CHECK |use vs |test
			double r = ( mdotCp_coll/area_coll*(1-exp(-area_coll*FprimeUL/mdotCp_coll)) ) / FRUL; // D&B eqn 6.20.3
			double FRta_corr = r*FRta;
			double FRUL_corr = r*FRUL;
				
			/* Pipe loss adjustment (D&B pp 430) */
			FRta_corr = FRta_corr / ( 1+UA_pipe/mdotCp_coll ); // D&B eqn 10.3.9
			FRUL_corr = FRUL_corr * ( (1-UA_pipe/mdotCp_coll + 2*UA_pipe/(area_coll*FRUL_corr) ) / (1 + UA_pipe/mdotCp_coll) ); // D&B eqn 10.3.10
				
			/* Heat exchanger adjustment (D&B pp 427) */
			double FR_ratio = 1/( 1 + (area_coll*FRUL_corr/mdotCp_coll)*(mdotCp_coll/(Eff_hx*mdotCp_coll)-1)); // D&B eqn 10.2.3
			FRta_corr *= FR_ratio;
			FRUL_corr *= FR_ratio;
								
			double mdot_mix = Draw[i];		
			double T_tank_last_iter = 0.0;

			int niter = 0;
			do
			{
				if (T_deliv > T_set)
				{
					// limit flow rate to mixing valve by effective ratio of T_set/T_deliv
					mdot_mix =  Draw[i] * (Cp(WATER, T_set)*T_set - Cp(WATER, T_mains[i])*T_mains[i]) 
											/(Cp_tank *T_deliv - Cp(WATER, T_mains[i])*T_mains[i]);
				}
				
				double T_dry_prev = T_dry[i];
				if ( i > 0 ) T_dry_prev = T_dry[i-1];

				/* calculate critical radiation for operation */
				double G_Tcrit = FRUL_corr*( T_tank - T_dry[i] ) / FRta_corr; // D&B eqn 6.8.2			
				if ( Incident[i] > G_Tcrit )
					Q_useful = area_coll*( FRta_corr*Incident[i] - FRUL_corr*(T_tank_prev - T_dry_prev) ); // D&B eqn 6.8.1 
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
							+ ( Q_useful*dT - Q_tankloss*dT - mdot_mix*Cp_tank*273.15 + mdot_mix*Cp(WATER,T_mains[i])*(T_mains[i]+273.15) )
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
						T_hot = T_tank_prev - UA_tank * V_hot/V_tank * (T_hot-T_room)*dT / (rho_tank * Cp(WATER,T_hot)*V_tank);
						V_cold = V_tank-V_hot;
						T_cold = T_mains[i] - UA_tank * V_cold/V_tank * (T_mains[i]-T_room)*dT / (rho_tank*Cp(WATER,T_cold)*V_tank);

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
						T_hot = T_hot - UA_tank * V_hot / V_tank * (T_hot - T_room) * dT / (rho_tank * Cp(WATER,T_hot) * V_tank);
						V_cold = V_tank-V_hot;
						// note: T_cold calculation is approximate, doesn't account for incoming cold water temperature
						T_cold = T_cold - UA_tank * V_cold / V_tank * (T_cold - T_room) * dT / (rho_tank * Cp(WATER,T_cold) * V_tank);

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
				Q_deliv = mdot_mix / dT * (Cp_tank * T_deliv - Cp(WATER, T_mains[i])*T_mains[i]);
			else
				Q_deliv = Draw[i] / dT * (Cp_tank * T_deliv - Cp(WATER, T_mains[i])*T_mains[i]);
			
			double Q_loss = Q_tankloss;
		
			// calculate pumping losses (pump size is user entered) -
			double P_pump = (Q_useful > 0) ? pump_watts*pump_eff : 0.0;
		
			// amount of auxiliary energy needed to bring delivered water to set temperature
			double Q_aux = Draw[i] / dT * Cp(WATER,T_set) * (T_set - T_deliv);
			if (Q_aux < 0) Q_aux = 0.0;
		
			// amount of energy needed to bring T_mains to set temperature (without SHW)
			double Q_auxonly = Draw[i] / dT * Cp(WATER,T_set) * (T_set - T_mains[i]);
			if (Q_auxonly < 0) Q_auxonly = 0.0;
		
			// Energy saved by SHW system is difference between auxonly system and shw+aux system
			double Q_saved = Q_auxonly - Q_aux;

			// save some values for next iteration
			Q_useful_prev = Q_useful;
			T_tank_prev = T_tank;
			T_deliv_prev = T_deliv;

			// save output variables
			arr_Q_useful[i] = (ssc_number_t) Q_useful;
			arr_Q_deliv[i] = (ssc_number_t) Q_deliv;
			arr_Q_loss[i] = (ssc_number_t) Q_loss;
			arr_T_tank[i] = (ssc_number_t) T_tank;
			arr_T_deliv[i] = (ssc_number_t) T_deliv;
			arr_P_pump[i] = (ssc_number_t) P_pump;
			arr_Q_aux[i] = (ssc_number_t) Q_aux;
			arr_Q_auxonly[i] = (ssc_number_t) Q_auxonly;
			arr_Q_saved[i] = (ssc_number_t) Q_saved;
		}

		// finished with calculations.
	}



	/* ************************************************
	     Some helper functions 
	   ************************************************ */

	enum { WATER, GLYCOL34 };

	/* returns the specific heat of the fluid in J/(kg*K)
	   at the given Tcerature ('C) */
	double Cp( int fluid, double Tc )
	{
		switch(fluid)
		{
		case WATER:
			if (Tc < 2)Tc = 2;
			if (Tc > 82) Tc = 82;
			return -2.93583442453E-09  * Tc*Tc*Tc*Tc*Tc*Tc
				+ 9.020835231785E-07   * Tc*Tc*Tc*Tc*Tc 
				- 1.01686339942834E-04 * Tc*Tc*Tc*Tc 
				+ 4.92261451632708E-03 * Tc*Tc*Tc 
				- 6.77793511936617E-02 * Tc*Tc 
				- 1.64165585623681     * Tc 
				+ 4218.93560683432;
			
		case GLYCOL34:
			if ( Tc < -20 ) Tc = -20;
			if ( Tc > 100 ) Tc = 100;
			return -8.6805555018E-10   * Tc*Tc*Tc*Tc*Tc*Tc
				+ 2.0833333096854E-07  * Tc*Tc*Tc*Tc*Tc
				- 1.3888888805727E-05  * Tc*Tc*Tc*Tc
				- 1.1448064718E-10     * Tc*Tc*Tc
				+ 1.81944492754427E-02 * Tc*Tc
				+ 2.71666684783669     * Tc 
				+ 3559.99998966649;

		default:
			return 0.0;
		}
	}

	/* returns the density of the fluid in kg/m3
	   at the given temperature ('C) */
	double Density( int fluid, double Tc )
	{
		switch(fluid)
		{
		case WATER:
			if (Tc < 2) Tc = 2;
			if (Tc > 82) Tc = 82;
			return -2.0143060378702E-07 * Tc*Tc*Tc*Tc
				+ 5.49479743699455E-05  * Tc*Tc*Tc
				- 8.28854009657304E-03  * Tc*Tc
				+ 6.55541751787336E-02  * Tc 
				+ 999.815720234197;
	
		case GLYCOL34:
			return 1.9531250185E-10    * Tc*Tc*Tc*Tc*Tc*Tc
				- 5.338541702801E-08   * Tc*Tc*Tc*Tc*Tc
				+ 4.81770844047019E-06 * Tc*Tc*Tc*Tc
				- 1.22395859071145E-04 * Tc*Tc*Tc
				- 4.45833196479117E-03 * Tc*Tc
				- 0.292499950902793    * Tc 
				+ 1062.99999725054;
		default:
			return 0.0;
		}
	}
};

DEFINE_MODULE_ENTRY( swhsolopt, "Solar Water Heating using SolOpt model with modifications.", 1 )
