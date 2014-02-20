#include <math.h>
#include "core.h"
#include "lib_weatherfile.h"
#include "lib_irradproc.h"
#include "lib_util.h"
#include <sstream>

/* -------------------------------------
2 Mode Model
Time Marching Method: Implicit Euler
Conduction Between Nodes: Off
-------------------------------------- */

#ifndef M_PI
#define M_PI 3.141592653589793238462643
#endif

static var_info _cm_vtab_swh[] = {
	/*   VARTYPE           DATATYPE         NAME                      LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT, SSC_NUMBER, "energy_availability", "First year energy availability", "%", "", "AnnualOutput", "*", "", "" },
	{ SSC_INPUT, SSC_MATRIX, "energy_curtailment", "First year energy curtailment", "", "(0..1)", "AnnualOutput", "*", "", "" },

	
	
	{ SSC_INPUT,        SSC_STRING,      "solar_resource_file",             "local weather file path",          "",       "",                      "Weather",      "*",                         "LOCAL_FILE",                  "" },

	{ SSC_INPUT,        SSC_ARRAY,       "scaled_draw",           "Hot water draw",                   "kg/hr",  "",                      "SWH",      "*",                       "LENGTH=8760",						 "" },


	{ SSC_INPUT,        SSC_NUMBER,      "tilt",                  "Collector tilt",                   "deg",    "",                      "SWH",      "*",                       "MIN=0,MAX=90",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "azimuth",               "Collector azimuth",                "deg",    "90=E,180=S",            "SWH",      "*",                       "MIN=0,MAX=360",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "albedo",                "Ground reflectance factor",        "0..1",   "",                      "SWH",      "*",                       "FACTOR",                            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "irrad_mode",            "Irradiance input mode",            "0/1",    "Beam+Diff,Global+Beam", "SWH",      "?=0",                     "INTEGER,MIN=0,MAX=1",               "" },
	{ SSC_INPUT,        SSC_NUMBER,      "sky_model",             "Tilted surface irradiance model",  "0/1/2",  "Isotropic,HDKR,Perez",  "SWH",      "?=2",                     "INTEGER,MIN=0,MAX=2",               "" },

	{ SSC_INPUT,        SSC_NUMBER,      "mdot",                  "Total system mass flow rate",      "kg/s",   "",                      "SWH",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ncoll",                 "Number of collectors",             "",       "",                      "SWH",      "*",                       "POSITIVE,INTEGER",                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "fluid",			      "Working fluid in system",          "",       "Water,Glycol",          "SWH",      "*",                       "INTEGER,MIN=0,MAX=1",               "" },

	{ SSC_INPUT,        SSC_NUMBER,      "area_coll",             "Single collector area",            "m2",     "",                      "SWH",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "FRta",                  "FRta",                             "",       "",                      "SWH",      "*",                       "",                                  "" }, 
	{ SSC_INPUT,        SSC_NUMBER,      "FRUL",                  "FRUL",                             "",       "",                      "SWH",      "*",                       "",                                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "iam",                   "Incidence angle modifier",         "",       "",                      "SWH",      "*",                       "",                                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "test_fluid",            "Fluid used in collector test",     "",       "Water,Glycol",          "SWH",      "*",                       "INTEGER,MIN=0,MAX=1",               "" },
	{ SSC_INPUT,        SSC_NUMBER,      "test_flow",             "Flow rate used in collector test", "kg/s",   "",                      "SWH",      "*",                       "POSITIVE",                          "" },

	{ SSC_INPUT,        SSC_NUMBER,      "pipe_length",           "Length of piping in system",       "m",      "",                      "SWH",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pipe_diam",             "Pipe diameter",                    "m",      "",                      "SWH",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pipe_k",                "Pipe insulation conductivity",     "W/m2.C", "",                      "SWH",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pipe_insul",            "Pipe insulation thickness",        "m",      "",                      "SWH",      "*",                       "POSITIVE",                          "" },

	{ SSC_INPUT,        SSC_NUMBER,      "tank_h2d_ratio",        "Solar tank height to diameter ratio", "",    "",                      "SWH",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "U_tank",                "Solar tank heat loss coefficient",  "W/m2K", "",                      "SWH",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "V_tank",                "Solar tank volume",                 "m3",    "",                      "SWH",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hx_eff",                "Heat exchanger effectiveness",      "0..1",  "",                      "SWH",      "*",                       "POSITIVE",                          "" },

	{ SSC_INPUT,        SSC_NUMBER,      "T_room",                "Ambient temperature in mech room", "C",      "",                      "SWH",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "T_tank_max",            "Max temperature in storage tank",  "C",      "",                      "SWH",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "T_set",                 "Set temperature",                  "C",      "",                      "SWH",      "*",                       "POSITIVE",                          "" },

	{ SSC_INPUT,        SSC_NUMBER,      "pump_power",            "Pump power",                       "W",      "",                      "SWH",      "*",                       "POSITIVE",                          "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pump_eff",              "Pumping efficiency",               "%",      "",                      "SWH",      "*",                       "PERCENT",                           "" },

	{ SSC_INPUT,        SSC_NUMBER,      "use_custom_mains",      "Use custom mains",                   "%",      "",                      "SWH",      "*",                       "INTEGER,MIN=0,MAX=1",            "" },
	{ SSC_INPUT,        SSC_ARRAY,       "custom_mains",          "Custom mains",						"C",      "",                      "SWH",      "*",                       "LENGTH=8760",                     "" },

	{ SSC_INPUT,        SSC_NUMBER,      "use_custom_set",		  "Use custom set points",              "%",      "",                      "SWH",      "*",                       "INTEGER,MIN=0,MAX=1",            "" },
	{ SSC_INPUT,        SSC_ARRAY,       "custom_set",            "Custom set points",					"C",      "",                      "SWH",      "*",                       "LENGTH=8760",                     "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "beam",                  "Irradiance - Beam",                  "W/m2",  "",                      "SWH",      "*",                        "LENGTH=8760",                     "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "diffuse",               "Irradiance - Diffuse",               "W/m2",  "",                      "SWH",      "*",                        "LENGTH=8760",                     "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "I_incident",            "Irradiance - Incident",              "W/m2",  "",                      "SWH",      "*",                        "LENGTH=8760",                     "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "I_transmitted",         "Irradiance - Transmitted",           "W/m2",  "",                      "SWH",      "*",                        "LENGTH=8760",                     "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_aux",                 "Q auxiliary",                      "kWh",    "",                      "SWH",      "*",                        "LENGTH=8760",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_auxonly",             "Q auxiliary only",                 "kWh",    "",                      "SWH",      "*",                        "LENGTH=8760",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_deliv",               "Q delivered",                      "kWh",    "",                      "SWH",      "*",                        "LENGTH=8760",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_loss",                "Q loss",                           "kWh",    "",                      "SWH",      "*",                        "LENGTH=8760",                      "" },
//	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_saved",               "Q saved",                          "kWh",    "",                      "SWH",      "*",                        "LENGTH=8760",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "hourly_energy",		  "System energy",                          "kWh",    "",                      "SWH",      "*",                        "LENGTH=8760",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_transmitted",         "Q transmitted",                    "kWh",    "",                      "SWH",      "*",                        "LENGTH=8760",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "Q_useful",              "Q useful",                         "kWh",    "",                      "SWH",      "*",                        "LENGTH=8760",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "P_pump",                "P pump",                          "kWh",    "",                      "SWH",      "*",                        "LENGTH=8760",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_dry",                 "T ambient",						  "C",		"",                      "SWH",      "*",                        "LENGTH=8760",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_cold",                "T cold",                           "C",     "",                      "SWH",      "*",                        "LENGTH=8760",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_deliv",               "T delivered",                      "C",      "",                      "SWH",      "*",                        "LENGTH=8760",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_hot",                 "T hot",                            "C",     "",                      "SWH",      "*",                        "LENGTH=8760",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_mains",               "T mains",						  "C",      "",                      "SWH",      "*",                        "LENGTH=8760",                      "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "T_tank",                "T tank",                           "C",      "",                      "SWH",      "*",                        "LENGTH=8760",                      "" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "V_hot",                 "V hot",                            "m3",    "",                      "SWH",      "*",                        "LENGTH=8760",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "V_cold",                "V cold",                           "m3",    "",                      "SWH",      "*",                        "LENGTH=8760",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "draw",                  "Hot water draw",                   "kg/hr",  "",                      "SWH",      "*",                       "LENGTH=8760",                       "" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "mode",                  "Operation mode",                   "",      "1,2,3,4",               "SWH",      "*",                         "LENGTH=8760",                      "" },

	var_info_invalid };

class cm_swh : public compute_module
{
public:

	cm_swh()
	{
		add_var_info(_cm_vtab_swh);
	}

	void exec() throw(general_error)
	{
		const char *file = as_string("solar_resource_file");

		weatherfile wf(file);
		if (!wf.ok()) throw exec_error("swh", "failed to read local weather file: " + std::string(file));

		/* **********************************************************************
		Read user specified system parameters from compute engine
		********************************************************************** */

		/* constant fluid properties */
		double Cp_water = 4182.; // Cp_water@40'C (J/kg.K)
		double rho_water = 1000.; // 992.2; // density of water, kg/m3 @ 40'C
		double Cp_glycol = 3400; // 3705; // Cp_glycol

		double hour2sec = 1. / 3600.; // multiply hour quantity to get second quantity
		double W2kW = 1. / 1000.; // multiply Wh quantity to get kWh quantity;

		/* sky model properties */
		double albedo = as_double("albedo"); // ground reflectance fraction
		double tilt = as_double("tilt"); // collector tilt in degrees
		double azimuth = as_double("azimuth"); // collector azimuth in degrees  (180=south, 90=east)
		int irrad_mode = as_integer("irrad_mode"); // 0=beam&diffuse, 1=total&beam
		int sky_model = as_integer("sky_model"); // 0=isotropic, 1=hdkr, 2=perez

		/* extract arrays */
		size_t len;
		ssc_number_t *draw = as_array("scaled_draw", &len);
		if (len != 8760) throw exec_error("swh", "draw profile must have 8760 values");
		ssc_number_t *custom_mains = as_array("custom_mains", &len);
		if (len != 8760) throw exec_error("swh", "custom mains profile must have 8760 values");
		ssc_number_t *custom_set = as_array("custom_set", &len);
		if (len != 8760) throw exec_error("swh", "custom set temperature profile must have 8760 values");

		/* working fluid settings */
		int ifluid = as_integer("fluid"); // 0=water, 1=glycol
		double fluid_cp = (ifluid == 0) ? Cp_water : Cp_glycol;  // working fluid specific heat in J/kgK

		int itest = as_integer("test_fluid"); // 0=water, 1=glycol
		double test_cp = (itest == 0) ? Cp_water : Cp_glycol;  // test fluid specific heat in J/kgK
		double test_flow = as_double("test_flow"); // collector test flow rate (kg/s)

		/* collector properties */
		double mdot = as_double("mdot"); // total system mass flow rate (kg/s)
		double area = as_double("area_coll") * as_integer("ncoll"); // total solar collector area (m2)

		double FRta = as_double("FRta"); // FR(ta)_n (D&B pp 291) (dimensionless) collector heat removal factor * effective transmittance-absorption product (intercept on efficiency curve); indication of how energy is absorbed.
		double FRUL = as_double("FRUL"); // FRUL (D&B pp 291) (W/m2.C)  collector heat removal factor * collector heat loss coefficient (slope of efficiency curve); indication of how energy is lost.
		double iam = as_double("iam"); // incidence angle modifier coefficient (D&B pp 297) (unitless)

		/* pipe properties */
		double pipe_diam = as_double("pipe_diam"); // pipe diameter in system (m)
		double pipe_k = as_double("pipe_k"); // pipe insulation conductivity (W/m2.C)
		double pipe_insul = as_double("pipe_insul");  // pipe insulation thickness (m)
		double pipe_length = as_double("pipe_length"); // length of piping in system (m)

		/* tank properties */
		double tank_h2d_ratio = as_double("tank_h2d_ratio"); // ratio of tank height to diameter (dimensionless)
		double U_tank = as_double("U_tank"); // W/m2.C storage tank heat loss coefficient (U-value)
		double V_tank = as_double("V_tank"); // solar tank volume (m3)
		double tank_radius = pow(V_tank / (2 * M_PI*tank_h2d_ratio), 0.33333333);
		double tank_height = tank_radius * 2 * tank_h2d_ratio;
		double tank_area = 2 * M_PI*tank_radius*tank_radius + 2 * M_PI*tank_radius*tank_height; // 2*pi*R^2 + 2*pi*r*h
		double UA_tank = tank_area * U_tank;
		double tank_cross_section = M_PI*tank_radius*tank_radius;

		/* pipe, and heat exchange properties */
		double Eff_hx = as_double("hx_eff"); // heat exchanger effectiveness (0..1)
		double pump_watts = as_double("pump_power"); // pump size in Watts
		double pump_eff = as_double("pump_eff"); // pumping efficiency

		// pipe U calculation (W/m2.C), http://en.wikipedia.org/wiki/Heat_transfer_coefficient (of pipe wall, h_wall = 2*k/(i*ln(o/i)) )
		double pipe_od = pipe_diam + pipe_insul * 2;
		double U_pipe = 2 * pipe_k / (pipe_od * ::log(pipe_od / pipe_diam)); //  **TODO** CHECK whether should be pipe_diam*log(pipe_od/pipe_diam) in denominator
		double UA_pipe = U_pipe * M_PI * pipe_od * pipe_length; // W/'C

		/* temperature properties */
		double T_room = as_double("T_room"); // ambient temperature in mechanical room or location of storage tank, hx, etc
		double T_tank_max = as_double("T_tank_max"); // max temp of water in storage tank
		double T_set = as_double("T_set"); // hot water set point temperature


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
		ssc_number_t *I_incident = allocate("I_incident", 8760);
		ssc_number_t *I_transmitted = allocate("I_transmitted", 8760);

		ssc_number_t *out_Q_transmitted = allocate("Q_transmitted", 8760);
		ssc_number_t *out_Q_useful = allocate("Q_useful", 8760);
		ssc_number_t *out_Q_deliv = allocate("Q_deliv", 8760);
		ssc_number_t *out_Q_loss = allocate("Q_loss", 8760);
		ssc_number_t *out_T_tank = allocate("T_tank", 8760);
		ssc_number_t *out_T_deliv = allocate("T_deliv", 8760);
		ssc_number_t *out_P_pump = allocate("P_pump", 8760);
		ssc_number_t *out_Q_aux = allocate("Q_aux", 8760);
		ssc_number_t *out_Q_auxonly = allocate("Q_auxonly", 8760);
		//		ssc_number_t *out_Q_saved = allocate("Q_saved", 8760);
		ssc_number_t *out_hourly_energy = allocate("hourly_energy", 8760);

		ssc_number_t *out_V_hot = allocate("V_hot", 8760);
		ssc_number_t *out_V_cold = allocate("V_cold", 8760);
		ssc_number_t *out_T_hot = allocate("T_hot", 8760);
		ssc_number_t *out_T_cold = allocate("T_cold", 8760);

		ssc_number_t *Mode = allocate("mode", 8760);


		double temp_sum = 0.;
		double monthly_avg_temp[12];
		for (i = 0; i < 12; i++) monthly_avg_temp[i] = 0.;

		for (i = 0; i < 8760; i++)
		{
			wf.read();

			Beam[i] = (ssc_number_t)wf.dn;
			Diffuse[i] = (ssc_number_t)wf.df;
			T_dry[i] = (ssc_number_t)wf.tdry;
			T_mains[i] = 0.;
			I_incident[i] = 0;
			I_transmitted[i] = 0;

			// accumulate for averaging
			temp_sum += T_dry[i];
			monthly_avg_temp[util::month_of(i) - 1] += T_dry[i];

			/* **********************************************************************
			Process radiation (Isotropic model), calculate Incident[i] through cover
			********************************************************************** */
			irrad tt;
			if (irrad_mode == 0) tt.set_beam_diffuse(wf.dn, wf.df);
			else tt.set_global_beam(wf.gh, wf.dn);
			tt.set_location(wf.lat, wf.lon, wf.tz);
			tt.set_time(wf.year, wf.month, wf.day, wf.hour, wf.minute, 1);
			tt.set_sky_model(sky_model /* isotropic=0, hdkr=1, perez=2 */, albedo);
			tt.set_surface(0, tilt, azimuth, 0, 0, 0);
			tt.calc();

			double poa[3];
			tt.get_poa(&poa[0], &poa[1], &poa[2], 0, 0, 0);
			I_incident[i] = (ssc_number_t)(poa[0] + poa[1] + poa[2]); // total PoA on surface

			double aoi = 0;
			tt.get_angles(&aoi, 0, 0, 0, 0); // note: angles returned in degrees

			// -------------------------------------
			// calculate transmittance through cover
			double Kta_d = 0.0;
			double Kta_b = 0.0;
			double Kta_g = 0.0;

			// incidence angle modifier (IAM) for beam (D&B eqn 6.17.10 pp 297)
			if (aoi <= 60.0) Kta_b = 1 - iam*(1 / cos(aoi*M_PI / 180) - 1);
			else if (aoi > 60.0 && aoi <= 90.0)  Kta_b = (1 - iam)*(aoi - 90.0)*M_PI / 180;
			if (Kta_b < 0) Kta_b = 0;


			// effective incidence angle for sky diffuse radiation (D&B eqn 5.4.2 pp 215)
			double theta_eff_diffuse = 59.7*M_PI / 180 - 0.1388*tilt*M_PI / 180 + 0.001497*tilt*M_PI / 180 * tilt*M_PI / 180;
			double cos_theta_eff_diffuse = cos(theta_eff_diffuse);

			// incidence angle modifier (IAM) for diffuse (D&B eqn 6.17.10 pp 297)
			if (theta_eff_diffuse <= M_PI / 3.) Kta_d = 1 - iam*(1 / cos_theta_eff_diffuse - 1);
			else if (theta_eff_diffuse > M_PI / 3. && theta_eff_diffuse <= M_PI / .2) Kta_d = (1 - iam)*(theta_eff_diffuse - M_PI / 2.);
			if (Kta_d < 0) Kta_d = 0;


			// effective incidence angle modifier for ground reflected radiation (D&B eqn 5.4.1 pp 215)
			double theta_eff_ground = 90 * M_PI / 180 - 0.5788*tilt*M_PI / 180 + 0.002693*tilt*M_PI / 180 * tilt*M_PI / 180;
			double cos_theta_eff_ground = cos(theta_eff_ground);

			// incidence angle modifier (IAM) for ground reflected radiation (D&B eqn 6.17.10 pp 297)
			if (theta_eff_ground <= M_PI / 3) Kta_g = 1 - iam*(1 / cos_theta_eff_ground - 1);
			else if (theta_eff_ground > M_PI / 3 && theta_eff_ground <= M_PI / 2) Kta_g = (1 - iam)*(theta_eff_ground - M_PI / 2.);
			if (Kta_g < 0) Kta_g = 0;


			I_transmitted[i] = (ssc_number_t)(Kta_b*poa[0] + Kta_d*poa[1] + Kta_g*poa[2]);


		}



		// Compute Mains Temperature based on user input
		int use_custom_mains = as_integer("use_custom_mains");
		if (use_custom_mains)
		{
			for (i = 0; i < 8760; i++)
			{
				T_mains[i] = (ssc_number_t)(custom_mains[i]);
			}
		}
		else
		{

			// Algorithm for calculating mains water temperature from paper 
			// ASES 2007 (J.Burch & C.Christensen)
			// "Towards Development of an Algorithm for Mains Water Temperature"
			// Verified against code in TRNSYS Type 15 Weather Reader
			double min_monthly_avg = 1e99;
			double max_monthly_avg = -1e99;
			for (i = 0; i < 12; i++)
			{
				monthly_avg_temp[i] = monthly_avg_temp[i] / (util::nday[i] * 24);
				if (monthly_avg_temp[i] < min_monthly_avg) min_monthly_avg = monthly_avg_temp[i];
				if (monthly_avg_temp[i] > max_monthly_avg) max_monthly_avg = monthly_avg_temp[i];
			}

			double avg_temp_high_f = 32. + 1.8 * max_monthly_avg; //F
			double avg_temp_low_f = 32. + 1.8 * min_monthly_avg; // F
			double annual_avg_temp = (temp_sum / 8760.) * 1.8 + 32.; // F

			double mains_ratio = 0.4 + 0.01*(annual_avg_temp - 44.); // F
			double lag = 35. - (annual_avg_temp - 44.); // F

			/* **********************************************************************
			Calculate hourly mains water temperature
			********************************************************************** */
			for (i = 0; i < 8760; i++)
			{
				// calculate hour of day  ( goes 1..24 )
				// and julian day  ( goes 1..365 )

				// (Julian day is used in the Julian date (JD) system of time measurement for scientific use by 
				// the astronomy community, presenting the interval of time in days and fractions of a day since 
				// January 1, 4713 BC Greenwich noon - WIKIPEDIA)
				int hour = 0;
				int julian_day = (int)(((double)(i + 1)) / 24);
				if ((double)julian_day == (((double)(i + 1)) / 24.0))
					hour = 24;
				else
				{
					hour = (i + 1) - (julian_day * 24);
					julian_day++;
				}

				T_mains[i] = (ssc_number_t)(annual_avg_temp + 6. + mains_ratio * ((avg_temp_high_f - avg_temp_low_f) / 2.)
					* sin(M_PI / 180 * (0.986*(julian_day - 15 - lag) - 90.)));
				T_mains[i] = (ssc_number_t)((T_mains[i] - 32) / 1.8); // convert to 'C
			}
		}

		/* **********************************************************************
			Determine set temperatures based on user input
			********************************************************************** */
		int use_custom_set = as_integer("use_custom_set");
		double T_set_array[8760];
		if (use_custom_set == 0)
		{
			for (i = 0; i < 8760; i++) T_set_array[i] = T_set;
		}
		else
		{
			for (i = 0; i < 8760; i++) T_set_array[i] = custom_set[i];
		}

		/* **********************************************************************
		Calculate additional SWH system parameters
		********************************************************************** */

		/* set initial conditions on some simulation variables */
		double T_hot_prev_hour = T_mains[0] + 40.; // initial hot temp 40'C above ambient
		double T_cold_prev_hour = T_mains[0];
		double Q_tankloss = 0;
		double Q_useful_prev_hour = 0.0;
		double V_hot_prev_hour = 0.8 * V_tank;
		double V_cold_prev_hour = V_tank - V_hot_prev_hour;
		double T_tank_prev_hour = V_hot_prev_hour / V_tank*T_hot_prev_hour + V_cold_prev_hour / V_tank*T_cold_prev_hour; // weighted average tank temperature (initial)
		double T_deliv_prev_hour = 0.0;

		/* *********************************************************************************************
		Calculate SHW performance: Q_useful, Q_deliv, T_deliv, T_tank, Q_pump, Q_aux, Q_auxonly, energy_net (Q_saved)
		*********************************************************************************************** */
		for (i = 0; i < 8760; i++)
		{
			// at beginning of this timestep, temp values are the same as end of last timestep
			double T_tank = T_tank_prev_hour;
			double Q_useful = Q_useful_prev_hour;
			double T_deliv = T_deliv_prev_hour;
			double V_hot = V_hot_prev_hour;
			double V_cold = V_tank - V_hot;
			double T_hot = T_hot_prev_hour;
			double T_cold = T_cold_prev_hour;


			double mdotCp_use = mdot * fluid_cp; // mass flow rate (kg/s) * Cp_fluid (J/kg.K)
			double mdotCp_test = test_flow * test_cp; // test flow (kg/s) * Cp_test

			/* Flow rate corrections to FRta, FRUL (D&B pp 307) */
			double FprimeUL = -mdotCp_test / area * ::log(1 - FRUL*area / mdotCp_test); // D&B eqn 6.20.4
			double r = (mdotCp_use / area*(1 - exp(-area*FprimeUL / mdotCp_use))) / FRUL; // D&B eqn 6.20.3
			double FRta_use = r*FRta;
			double FRUL_use = r*FRUL;

			/* Pipe loss adjustment (D&B pp 430) */
			FRta_use = FRta_use / (1 + UA_pipe / mdotCp_use); // D&B eqn 10.3.9
			FRUL_use = FRUL_use * ((1 - UA_pipe / mdotCp_use + 2 * UA_pipe / (area*FRUL_use)) / (1 + UA_pipe / mdotCp_use)); // D&B eqn 10.3.10

			/* Heat exchanger adjustment (D&B pp 427) */
			double FR_ratio = 1 / (1 + (area*FRUL_use / mdotCp_use)*(mdotCp_use / (Eff_hx*mdotCp_use) - 1)); // D&B eqn 10.2.3
			FRta_use *= FR_ratio;
			FRUL_use *= FR_ratio;


			double mdot_mix = draw[i] * hour2sec; // (kg/s)
			double T_dry_prev = T_dry[i];
			if (i > 0) T_dry_prev = T_dry[i - 1];


			if (Q_useful_prev_hour <= 0)
				//	assume that in the 1st hour system operates for only 50% of the hour and there is negligible temperature rise across the collector
				Q_useful = area*(FRta_use*I_transmitted[i] - 0.5 * (FRUL_use*(T_tank - T_dry[i])));
			else
				Q_useful = area*(FRta_use*I_transmitted[i] - FRUL_use*(T_tank - T_dry[i])); // D&B eqn 6.8.1 					
			if (Q_useful > 0)
			{
				Mode[i] = 1;
				/* MIXED TANK -- during charging */

				// tank mass
				double m_tank = rho_water*V_tank;

				// Implicit Euler calculation
				T_tank = ((T_tank_prev_hour*m_tank*Cp_water) + dT*((Q_useful)+(UA_tank*T_room)
					+ (mdot_mix*Cp_water*T_mains[i]))) / ((m_tank*Cp_water) + dT*(UA_tank + mdot_mix*Cp_water));


				if (T_tank > T_tank_max) T_tank = T_tank_max;
				Q_tankloss = UA_tank * (T_tank - T_room);
				T_deliv = T_tank;
			}
			else
			{
				/* STRATIFIED TANK -- during discharging */

				// If previous hour had solar collection 
				// (i.e. previous hour was mixed tank, and we don't yet have hot & cold node temperatures), 
				// use the previous tank temperature
				// and mains temperature for the reference hot node and cold node
				// temperatures in the stratified tank

				double hotLoss = 0.0;
				double coldLoss = 0.0;
				double A_cold = 0.0;
				double A_hot = 0.0;

				if (Q_useful_prev_hour > 0)
				{
					// previous hour had solar collection, so assume whole tank is hot at previous step
					V_hot = V_tank - mdot_mix*dT / rho_water;
					double m_hot = V_hot*rho_water;
					if (V_hot < 0) V_hot = 0;

					T_hot = ((T_tank_prev_hour * Cp_water * m_hot) + (dT*UA_tank * T_room)) / ((m_hot*Cp_water) + (dT*UA_tank)); // IMPLICIT NON-STEADY (Euler)
					V_cold = V_tank - V_hot;
					T_cold = T_mains[i];
					Q_tankloss = UA_tank*(T_hot - T_room);
				}
				else
				{
					// previous hour did not have solar collection
					V_hot = V_hot_prev_hour - mdot_mix*dT / rho_water;
					double m_hot = V_hot_prev_hour*rho_water;
					double h_hot = V_hot_prev_hour / tank_cross_section;
					A_hot = tank_cross_section + 2 * M_PI*tank_radius*h_hot;

					// ensure volumes are positive
					if (V_hot < 0) V_hot = 0;
					if (V_hot == 0) T_hot = T_hot_prev_hour;
					else T_hot = ((T_hot_prev_hour * Cp_water * m_hot) + (dT*U_tank*A_hot * T_room)) / ((m_hot*Cp_water) + (dT*U_tank*A_hot)); // IMPLICIT NON-STEADY (Euler)

					V_cold = V_tank - V_hot;
					hotLoss = U_tank * A_hot * (T_hot - T_room);

					// check whether there is a cold volume
					if (V_cold_prev_hour == 0 || V_cold == 0) T_cold = T_cold_prev_hour;
					else
					{
						double m_cold = rho_water*V_cold;
						double h_cold = V_cold / tank_cross_section;
						A_cold = tank_cross_section + 2 * M_PI*tank_radius*h_cold;
						T_cold = ((T_cold_prev_hour*m_cold*Cp_water) + (dT*U_tank*A_cold*T_room) + (dT*mdot_mix*Cp_water*T_mains[i]))
							/ ((m_cold*Cp_water) + (dT*A_cold*U_tank) + (mdot_mix*dT*Cp_water)); // IMPLICIT NON-STEADY
					}
					coldLoss = U_tank*A_cold*(T_cold - T_room);
					Q_tankloss = hotLoss + coldLoss;

				}
				// delivered water hot if there is hot water to provide
				if (V_hot > 0)
					T_deliv = T_hot;
				else
					T_deliv = T_cold;

				Mode[i] = 2;

				T_tank = V_hot / V_tank * T_hot + V_cold / V_tank * T_cold;
			}

			// calculate pumping losses (pump size is user entered) -
			double P_pump = (Q_useful > 0) ? pump_watts*pump_eff : 0.0;

			// compute energy delivered
			double Q_deliv = mdot_mix* Cp_water *(T_deliv - T_mains[i]) - P_pump;

			// amount of auxiliary energy needed to bring delivered water to set temperature
			double Q_aux = draw[i] / dT * Cp_water * (T_set_array[i] - T_deliv);


			if (Q_aux < 0) Q_aux = 0.0;

			// amount of energy needed to bring T_mains to set temperature (without SHW)
			double Q_auxonly = draw[i] / dT * Cp_water * (T_set_array[i] - T_mains[i]);

			if (Q_auxonly < 0) Q_auxonly = 0.0;

			// Energy saved by SHW system is difference between aux only system and shw+aux system - the pump losses
			double Q_saved = Q_auxonly - Q_aux - P_pump;

			// save some values for next hour
			Q_useful_prev_hour = Q_useful;
			T_tank_prev_hour = T_tank;
			V_hot_prev_hour = V_hot;
			V_cold_prev_hour = V_tank - V_hot;
			T_deliv_prev_hour = T_deliv;
			T_hot_prev_hour = T_hot;
			T_cold_prev_hour = T_cold;

			// Zero out Q_useful if <0
			if (Q_useful < 0) Q_useful = 0.0;


			// save output variables - convert Q values to kWh 
			out_Q_transmitted[i] = (ssc_number_t)(I_transmitted[i] * area);
			out_Q_useful[i] = (ssc_number_t)(Q_useful * W2kW);
			out_Q_deliv[i] = (ssc_number_t)(Q_deliv * W2kW); //this is currently being output from a financial model as "Hourly Energy Delivered", they are equivalent
			out_Q_loss[i] = (ssc_number_t)(Q_tankloss * W2kW);
			out_T_tank[i] = (ssc_number_t)T_tank;
			out_T_deliv[i] = (ssc_number_t)T_deliv;
			out_P_pump[i] = (ssc_number_t)(P_pump * W2kW);
			out_Q_aux[i] = (ssc_number_t)(Q_aux * W2kW);
			out_Q_auxonly[i] = (ssc_number_t)(Q_auxonly* W2kW);
			//			out_Q_saved[i] = (ssc_number_t) (Q_saved* W2kW);
			out_hourly_energy[i] = (ssc_number_t)(Q_saved* W2kW);
			out_T_hot[i] = (ssc_number_t)T_hot;
			out_T_cold[i] = (ssc_number_t)T_cold;
			out_V_hot[i] = (ssc_number_t)V_hot;
			out_V_cold[i] = (ssc_number_t)V_cold;
			out_Draw[i] = draw[i]; // pass to outputs for visualization
		}

		// finished with calculations.

		// availability and curtailment - standard application to hourly_energy
		ssc_number_t avail = as_number("energy_availability") / 100;
		size_t nrows, ncols;
		ssc_number_t *diurnal_curtailment = as_matrix("energy_curtailment", &nrows, &ncols);
		if ((nrows != 12) || (ncols != 24))
		{
			std::ostringstream stream_error;
			stream_error << "month x hour curtailment factors must have 12 rows and 24 columns, input has " << nrows << " rows and " << ncols << " columns.";
			std::string const str_error = stream_error.str();
			throw exec_error("annualoutput", str_error);
		}
		i = 0;
		for (int m = 0; m < 12; m++)
			for (int d = 0; d < util::nday[m]; d++)
				for (int h = 0; h < 24; h++)
					if (i < 8760)
					{
						// first year availability applied
						out_hourly_energy[i] *= diurnal_curtailment[m*ncols + h] * avail;
						i++;
					}

	}

};

DEFINE_MODULE_ENTRY( swh, "Solar water heating model using variable tank node volume model.", 4 )
