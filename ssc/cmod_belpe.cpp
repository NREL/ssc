
#include <limits>
#include <cmath>

#include "core.h"
#include "lib_weatherfile.h"
#include "lib_irradproc.h"

#ifndef M_PI
#define M_PI 3.14159265358979323
#endif

#ifndef DTOR
#define DTOR 0.0174532925
#endif

static var_info _cm_vtab_belpe[] = 
{	
/*   VARTYPE			DATATYPE        NAME                LABEL								UNITS		META			GROUP                     REQUIRED_IF	CONSTRAINTS		UI_HINTS*/
	{ SSC_INPUT,        SSC_STRING,		"weather_file",		"Weather Data file",				"n/a",		"",				"Load Profile Estimator", "*",			"LOCAL_FILE",	"" },
//	{ SSC_INPUT,        SSC_NUMBER,		"tstep",            "time step",						"hrs",      "",				"Load Profile Estimator", "*",			"",				"Time Step" },
	{ SSC_INPUT,        SSC_NUMBER,		"floor_area",		"Building floor area",				"m2",		"",				"Load Profile Estimator", "*",			"",				"Floor area" },
	{ SSC_INPUT,        SSC_NUMBER,		"Stories",			"Number of stories",				"#",		"",				"Load Profile Estimator", "*",			"",				"Stories" },

	{ SSC_INPUT,		SSC_NUMBER,		"YrBuilt",			"Year Built",						"yr",		"",				"Load Profile Estimator", "*",			"",				"" },
	{ SSC_INPUT,		SSC_NUMBER,		"Retrofits",		"Energy Retrofitted",				"0/1",		"0=No, 1=Yes",	"Load Profile Estimator", "*",			"",				"" },//energy retrofits
	{ SSC_INPUT,		SSC_NUMBER,		"Occupants",		"Occupants",						"#",		"",				"Load Profile Estimator", "*",			"",				"" },
	{ SSC_INPUT,		SSC_ARRAY,		"Occ_Schedule",		"Hourly occupant schedule",			"%/hr",		"",				"Load Profile Estimator", "*",			"",				"" },
	{ SSC_INPUT,		SSC_NUMBER,		"THeat",			"heating setpoint",					"degF",		"",				"Load Profile Estimator", "*",			"",				"" },
	{ SSC_INPUT,        SSC_NUMBER,		"TCool",			"Cooling SetPoint",					"degF",		"",				"Load Profile Estimator", "*",			"",				"" },
	{ SSC_INPUT,		SSC_NUMBER,		"THeatSB",			"heating setpoint SetBack",			"degf",		"",				"Load Profile Estimator", "*",			"",				"" },
	{ SSC_INPUT,		SSC_NUMBER,		"TCoolSB",			"cooling setpoint SetBack",			"degF",		"",				"Load Profile Estimator", "*",			"",				"" },
	{ SSC_INPUT,		SSC_ARRAY,		"T_Sched",			"Temperature schedule",				"degF",		"",				"Load Profile Estimator", "*",			"LENGTH=24",	"" },
	
	{ SSC_INPUT,		SSC_NUMBER,		"en_heat",			"Enable electric heat",				"0/1",		"",				"Load Profile Estimator", "*",			"BOOLEAN",		"" },
	{ SSC_INPUT,		SSC_NUMBER,		"en_cool",			"Enable electric cool",				"0/1",		"",				"Load Profile Estimator", "*",			"BOOLEAN",		"" },
	{ SSC_INPUT,		SSC_NUMBER,		"en_fridge",		"Enable electric fridge",			"0/1",		"",				"Load Profile Estimator", "*",			"BOOLEAN",		"" },
	{ SSC_INPUT,		SSC_NUMBER,		"en_range",			"Enable electric range",			"0/1",		"",				"Load Profile Estimator", "*",			"BOOLEAN",		"" },
	{ SSC_INPUT,		SSC_NUMBER,		"en_dish",			"Enable electric dishwasher",		"0/1",		"",				"Load Profile Estimator", "*",			"BOOLEAN",		"" },
	{ SSC_INPUT,		SSC_NUMBER,		"en_wash",			"Enable electric washer",			"0/1",		"",				"Load Profile Estimator", "*",			"BOOLEAN",		"" },
	{ SSC_INPUT,		SSC_NUMBER,		"en_dry",			"Enable electric dryer",			"0/1",		"",				"Load Profile Estimator", "*",			"BOOLEAN",		"" },
	{ SSC_INPUT,		SSC_NUMBER,		"en_mels",			"Enable electric mels",				"0/1",		"",				"Load Profile Estimator", "*",			"BOOLEAN",		"" },

	//ADD HVAC AND NONHVAC LOADS AS OUTPUTS ETC
	{ SSC_OUTPUT,       SSC_ARRAY,		"HVAC_load",		"Electric Load due to HVAC",		"Wh",       "",		"Load Profile Estimator", "*",			"LENGTH=8760",	"" },
	{ SSC_OUTPUT,       SSC_ARRAY,		"non_HVAC_load",	"Electric Load due to Non-HVAC",	"Wh",       "",		"Load Profile Estimator", "*",			"LENGTH=8760",	"" },
	{ SSC_OUTPUT,       SSC_ARRAY,		"load",				"Electric Load",					"Wh",       "",		"Load Profile Estimator", "*",			"LENGTH=8760",	"" },


var_info_invalid };

// Computes a building load given various building parameters as inputs and a weather file. Algorithm uses global (GHI), direct (DNI), and diffuse (DHI) irradiance components as well as temp and wind speed.
class cm_belpe : public compute_module
{
private:
public:
	cm_belpe()
	{
		add_var_info(_cm_vtab_belpe); 
	}

	//////////////////////////////SUPPORTING FUNCTIONS///////////////////////////////////////////

	//sums a vector a of length n
	double sum(double *a, int n)
	{
		double acc = 0;
		for (int i = 0; i<n; i++) acc += a[i];
		return acc;
	}

	//sums a vector a of length n from starting point m
	double sumsub(double *a, int m, int n)
	{
		double acc = 0;
		for (int i = m; i<=n; i++) acc += a[i];
		return acc;
	}

	//averages an 8760 array ("hourly") into a 12 array ("monthly") of monthly averages
	void monthly_averages( ssc_number_t *hourly, ssc_number_t *monthly )
	{
		int c = 0;
		for (int i=0;i<12;i++) // each month
		{
			monthly[i] = 0;
			for (int d=0;d<util::nday[i];d++) // for each day in each month
				for (int h=0;h<24;h++) // for each hour in each day
					monthly[i] += hourly[c++];
			monthly[i] /= util::nday[i]; //divide the monthly sum by the number of days in the month for an average
		}			
	}

	//////////////////////////////MAIN FUNCTION///////////////////////////////////////////
	void exec() throw(general_error)
	{		
		//8760 arrays of month, day, and hour neeeded for lots of calcs, initialize those here
		int month[8761], day[8761], hour[8761];
		int i = 0;
		for (int m = 0; m < 12; m++)
		{
			for (int d = 0; d < util::nday[m]; d++)
			{
				for (int h = 0; h <= 23; h++)
				{
					month[i] = m;
					day[i] = d;
					hour[i] = h;
					i++;
				}
			}
		}
		//add hour 8761 for the euler forward later
		month[8760] = 0; day[8760] = 0; hour[8760] = 0;

		// read weather file inputs 		
		const char *file = as_string("weather_file");
		weatherfile wf(file);
		if (!wf.ok()) throw exec_error("belpe", "failed to read local weather file: " + std::string(file));

		ssc_number_t *T_ambF = allocate("T_ambF", 8761);
		ssc_number_t *VwindMPH = allocate("VwindMPH", 8761);
		ssc_number_t *GHI = allocate("GHI", 8761);
		std::vector<double> RadWallN(8761), RadWallS(8761), RadWallE(8761), RadWallW(8761);

		for (size_t i = 0; i < 8760; i++)
		{
			if (!wf.read()) throw exec_error(" belpe", "error reading record in weather file");

			//calculate irradiances on four walls of building, needed later
			irrad irr;
			irr.set_location(wf.lat, wf.lon, wf.tz);
			irr.set_time(wf.year, wf.month, wf.day, wf.hour, wf.minute, wf.step / 3600);
			irr.set_global_beam(wf.gh, wf.dn);	//CHANGE THIS WHEN OPTION TO USE GLOBAL AND DIFFUSE IS INTRODUCED
			irr.set_sky_model(2, 0.2); //using Perez model and default albedo
			//variables to store irradiance info
			double beam, sky, gnd;
			//North wall
			irr.set_surface(0, 90, 0, 0, 0, 0);
			irr.get_poa(&beam, &sky, &gnd, 0, 0, 0);
			RadWallN[i] = beam + sky + gnd;
			//East wall
			irr.set_surface(0, 90, 90, 0, 0, 0);
			irr.get_poa(&beam, &sky, &gnd, 0, 0, 0);
			RadWallE[i] = beam + sky + gnd;
			//South wall
			irr.set_surface(0, 90, 180, 0, 0, 0);
			irr.get_poa(&beam, &sky, &gnd, 0, 0, 0);
			RadWallS[i] = beam + sky + gnd;
			//West wall
			irr.set_surface(0, 90, 270, 0, 0, 0);
			irr.get_poa(&beam, &sky, &gnd, 0, 0, 0);
			RadWallW[i] = beam + sky + gnd;
			
			//read and store other weather variables needed in calculations
			T_ambF[i] = (ssc_number_t)(wf.tdry*1.8 + 32);
			VwindMPH[i] = (ssc_number_t)(wf.wspd * 2.237);
			GHI[i] = (ssc_number_t)wf.gh;
		}
		//add 8761 hour for the euler forward later
		RadWallN[8760] = RadWallN[0]; 
		RadWallS[8760] = RadWallS[0]; 
		RadWallE[8760] = RadWallE[0]; 
		RadWallW[8760] = RadWallW[0];
		T_ambF[8760] = T_ambF[0];
		VwindMPH[8760] = VwindMPH[0];
		GHI[8760] = GHI[0];

		// calculate average annual temperature
		double T_annual_avg = 0;
		for (size_t i = 0; i < 8760; i++)
			T_annual_avg += T_ambF[i];
		T_annual_avg /= 8760;
		double TGnd = (T_annual_avg - 32) / 1.8; //Deg C because used just for the avg exterior envelope T.

		//radiation pre-processing
		double alphaho_wall = 0.15 / 5.6783;  //From ASHRAE for light colors(dark is x2).Converted to SI units
		std::vector<double> T_solair_walls(8761), T_solair_roof(8761), T_solair(8761), T_solairF(8761);
		for (int i = 0; i < 8761; i++)
		{
			T_solair_walls[i] = 4 * ((T_ambF[i] - 32) / 1.8 + alphaho_wall * (RadWallN[i] + RadWallS[i] + RadWallE[i] + RadWallW[i]) / 4); //N, S, E, W averaged
			T_solair_roof[i] = (T_ambF[i] - 32) / 1.8 + alphaho_wall * GHI[i] - 7;
			T_solair[i] = (T_solair_walls[i] + T_solair_roof[i] + TGnd) / 6; //This is in degrees C. Should GND be separated ?
			T_solairF[i] = T_solair[i] *1.8 + 32;
		}

		//Timestep is hourly for now.
		//double tstep = as_double("tstep");
		double dT = 1;

		// read building parameter inputs
		double A_Floor = as_double("floor_area");
		double Stories = as_double("Stories");
		double YrBuilt = as_double("YrBuilt");
		double Occupants = as_double("Occupants");
		bool EnergyRetrofits = as_boolean("Retrofits"); // 1 = yes, 0 = no. Governs building construction for older bldgs.

		size_t len_Occ_Schedule = 0;
		ssc_number_t *Occ_Schedule = as_array("Occ_Schedule", &len_Occ_Schedule);

		if (len_Occ_Schedule != 24)
			throw exec_error("belpe", "occupancy schedule needs to have 24 values");

		double THeat = as_double("THeat");
		double TCool = as_double("TCool");
		double THeatSB = as_double("THeatSB");
		double TCoolSB = as_double("TCoolSB");
		size_t len_T_Sched = 0;
		ssc_number_t* T_Sched = as_array("T_Sched", &len_T_Sched);

		if (len_T_Sched != 24) throw exec_error("belpe", "temperature schedule must have 24 values");

		ssc_number_t en_heat = as_number("en_heat"); // boolean, so will be 0 or 1
		ssc_number_t en_cool = as_number("en_cool"); // boolean, so will be 0 or 1
		ssc_number_t en_fridge = as_number("en_fridge"); // boolean, so will be 0 or 1
		ssc_number_t en_range = as_number("en_range"); // boolean, so will be 0 or 1
		ssc_number_t en_dish = as_number("en_dish"); // boolean, so will be 0 or 1
		ssc_number_t en_wash = as_number("en_wash"); // boolean, so will be 0 or 1
		ssc_number_t en_dry = as_number("en_dry"); // boolean, so will be 0 or 1
		ssc_number_t en_mels = as_number("en_mels"); // boolean, so will be 0 or 1

		//Possible other input options include color, construction, WWR, bldg L&W, wall
		//height per floor

		// allocate output array
		ssc_number_t *hvac_load = allocate("HVAC_load", 8760);
		ssc_number_t *non_hvac_load = allocate("non_HVAC_load", 8760);
		ssc_number_t *load = allocate("load", 8760);

		// If calibrating to util bills need user to be able to enter vacation.
		//These are bldg AM.defaults -- could also be default in the tool.
		const int N_vacation = 14;
		double VacationMonths[N_vacation] = { 5, 5, 5, 8, 8, 8, 8, 8, 8, 8, 12, 12, 12, 12 };
		double VacationDays[N_vacation] = { 26, 27, 28, 12, 13, 14, 15, 16, 17, 18, 22, 23, 24, 25 };

		//Default Values -- user does NOT change these!
		double H_ceiling = 8; //ft
		double WWR = 0.15;   //Recommended 15 - 18 % ....but analyze, maybe user needs this option
		double NL = 0; // normalized leakage

		//Get ACH using Normalized Leakage area as per Persily, 2006.  As well as
		//LBL leakage model.
		if (A_Floor <= 1600)
		{
			if (YrBuilt < 1940)
				NL = 1.29;
			else if (YrBuilt < 1970)
				NL = 1.03;
			else if (YrBuilt < 1990)
				NL = 0.65;
			else
				NL = 0.31;
		}
		else
		{
			if (YrBuilt < 1940)
				NL = 0.58;
			else if (YrBuilt < 1970)
				NL = 0.49;
			else if (YrBuilt < 1990)
				NL = 0.36;
			else
				NL = 0.24;
		}

		double ELA = NL*A_Floor*0.0929 / 1000 / pow(Stories, 0.3); //This ? estimated leakage area" is in m^2
		ELA = 1550 * ELA;  //Now it's inches
		double Cs, Cw;
		if (1 == Stories)
		{
			Cs = 0.015;
			Cw = 0.0065;
		}
		else if (Stories == 2)
		{
			Cs = 0.0299;
			Cw = 0.0086;
		}
		else
		{
			Cs = 0.045;
			Cw = 0.0101;
		}

		//This is the end of getting the air changes, for this part of the code

		//Default is stick frame construction. Renv started as defaults from Building America, based solely on age. But then was scaled to fit BeOpt for the older case (R increase from 4 to 5)
		// Windows are NOT separated out for conduction calcs but I put them here just in case (UWin)
		double Renv, SHGC;
		if (YrBuilt > 1990 || EnergyRetrofits == 1)
		{
			Renv = 16; //These are in IP Units hr*ft ^ 2 * degF / BTU
			//Uwin = 0.4;
			SHGC = 0.25;
		}
		else if (YrBuilt > 1980)
		{
			Renv = 12;
			//Uwin = 0.4;
			SHGC = 0.25;
		}
		else
		{
			Renv = 5;
			//Uwin = 1;
			SHGC = 0.53; //This basically matches BEOPT 0.76.WHY ? ? ? ? Possibly Bldg AM 0.7 factor for internal shading.MJB suggests shading or diffuse.Says 0.5, 025 fine!
		}

		double Cenv = 2; //BTU / ft^2degF    Note that this is stick frame - more like 10 for masonry, or 18 for heavy masonry(comm)
		double hsurf = 0.68; //Same units as Renv hr*ft ^ 2 * degF / Btu
		double Cmass = 1.6; //BTU / ft^2degF --doesn't change much!

		// C*1.8 + 32 = F

		//Those are sort of all the default values(above)

		ssc_number_t TambFAvg[12];
		monthly_averages(T_ambF, TambFAvg);

		// Is it heating or cooling season ? ? This methodology taken entirely from Building America guidelines
		double HtEn[13] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
		double ClEn[13] = { 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0 }; //Because Jul and Aug always have cooling on!BLDG AM
		for (int m = 0; m < 12; m++)
		{
			if (TambFAvg[m] <= 66)
				HtEn[m] = 1;
			else
				ClEn[m] = 1;
		}
		ClEn[12] = ClEn[0];
		HtEn[12] = HtEn[0];

		std::vector<double> HtEnNew(13);
		std::vector<double> ClEnNew(13);

		for (int i = 0; i < 13; i++)
		{
			HtEnNew[i] = HtEn[i];
			ClEnNew[i] = ClEn[i];
		}

		for (int m = 0; m < 12; m++)
		{
			if (ClEn[m] == 0 && ClEn[m + 1] == 1)
			{
				HtEnNew[m + 1] = 1;
				ClEnNew[m] = 1;
			}
			else if (HtEn[m] == 0 && HtEn[m + 1] == 1)
				ClEnNew[m + 1] = 1;
		}


		for (int i = 0; i < 13; i++)
		{
			HtEn[i] = HtEnNew[i];
			ClEn[i] = ClEnNew[i];
		}

		//Determines how much of the solar gain distributed to internal mass and how
		//much to the walls
		double SolMassFrac = 0.2;
		double SolEnvFrac = 1 - SolMassFrac;

		//BUILDING DIMENSIONMS
		//A_wins = A_Floor*WFR; %Assumed distributed evenly on walls
		double A_Wins = sqrt(A_Floor / Stories) * 4 * H_ceiling*Stories*WWR;
		double A_Walls = sqrt(A_Floor / Stories) * 4 * (H_ceiling + 2)*Stories - A_Wins;   //It's a cube with a bit of a plenum
		double Aenv = A_Walls + 2 * A_Floor; //This one includes floor
		double V_bldg = A_Floor * H_ceiling * Stories; //Exclude the plenum from conditioned volume
		double AIntWall = A_Floor / 2; //Interior partition walls - typical default
		double AIntMass = 0.4*A_Floor; //Bldg AM default for internal mass
		double AIntTot = A_Wins + Aenv + AIntWall + AIntMass;
		double Cair = 0.075*0.245*V_bldg * 10; //BTU / degF  Note adjust factor of 10 --MJB

		//INTERNAL LOADS	
		double PerPersonLoad = 220 / 3412.142; //BTU / hr / person to kWh / person(BLDG America for single zone) --sensible only
		//Divide into radiative and convective(heat transfer to mass vs.transfer to air)

		double PPL_rad[24], PPL_conv[24];
		for (int i = 0; i < 24; i++)
		{
			PPL_rad[i] = 0.6*PerPersonLoad*ceil(Occupants*Occ_Schedule[i]);
			PPL_conv[i] = 0.4*PerPersonLoad*ceil(Occupants*Occ_Schedule[i]);
		}

		//These are all from building america -- annual kWh loads
		double NBR = round((Occupants - 0.87) / 0.59); //number of bedrooms
		double Load_light = 2000;
		double Load_fridge = 600 * en_fridge;
		double Load_range = (250 + 83 * NBR)*en_range;
		double Load_dw = (87.6 + 29.2*NBR)*en_dish;
		double Load_wash = (38.8 + 12.9*NBR)*en_wash;
		double Load_dry = (538.2 + 179.4*NBR)*en_dry;
		double Load_mels = 1595 + 248 * NBR + .426*A_Floor*en_mels;

		//Now the HOURLY plug loads, weekday and weekend!These painstakingly
		//scaled from BeOpt and Bldg America.The loads are separated out because
		//user could have some and not others in house
		double FridgeFrac[24] = { 4, 3.9, 3.8, 3.7, 3.6, 3.6, 3.7, 4, 4.1, 4.2, 4, 4, 4.2, 4.2, 4.2, 4.2, 4.5, 4.8, 5, 4.8, 4.6, 4.5, 4.4, 4.2 };
		double DWFrac[24] = { 17, 14, 13, 12, 12, 15, 20, 30, 60, 64, 57, 50, 40, 48, 38, 35, 38, 50, 88, 110, 90, 68, 46, 33 };
		double RangeFrac[24] = { 8, 8, 5, 5, 8, 10, 26, 44, 48, 50, 44, 50, 57, 48, 45, 55, 85, 150, 120, 60, 40, 25, 15, 10 };
		double WasherFrac[24] = { 10, 8, 5, 5, 8, 10, 20, 50, 70, 85, 85, 75, 68, 60, 52, 50, 50, 50, 50, 50, 50, 47, 30, 17 };
		double DryerFrac[24] = { 10, 8, 5, 5, 8, 10, 10, 20, 50, 70, 85, 85, 75, 68, 60, 52, 50, 50, 50, 50, 50, 47, 30, 17 };

		double FridgeHrFrac = Load_fridge / (sum(FridgeFrac, 24)*(1 + 0.1 * 2 / 7));
		double DWHrFrac = Load_dw / (sum(DWFrac, 24)*(1 + 0.1 * 2 / 7));
		double RangeHrFrac = Load_range / (sum(RangeFrac, 24)*(1 + 0.1 * 2 / 7));
		double WasherHrFrac = Load_wash / (sum(WasherFrac, 24)*(1 + 0.1 * 2 / 7));
		double DryerHrFrac = Load_dry / (sum(DryerFrac, 24)*(1 + 0.1 * 2 / 7));

		std::vector<double> FridgeHourly(24);
		std::vector<double> FridgeHourlyWkend(24);
		std::vector<double> DWHourly(24);
		std::vector<double> DWHourlyWkend(24);
		std::vector<double> RangeHourly(24);
		std::vector<double> RangeHourlyWkend(24);
		std::vector<double> WasherHourly(24);
		std::vector<double> WasherHourlyWkend(24);
		std::vector<double> DryerHourly(24);
		std::vector<double> DryerHourlyWkend(24);

		for (int i = 0; i < 24; i++)
		{
			FridgeHourly[i] = FridgeFrac[i] * FridgeHrFrac;
			FridgeHourlyWkend[i] = FridgeHourly[i] * 1.1;
			DWHourly[i] = DWFrac[i] * DWHrFrac;
			DWHourlyWkend[i] = DWHourly[i] * 1.1;
			RangeHourly[i] = RangeFrac[i] * RangeHrFrac;
			RangeHourlyWkend[i] = RangeHourly[i] * 1.1;
			WasherHourly[i] = WasherFrac[i] * WasherHrFrac;
			WasherHourlyWkend[i] = WasherHourly[i] * 1.1;
			DryerHourly[i] = DryerFrac[i] * DryerHrFrac;
			DryerHourlyWkend[i] = DryerHourly[i] * 1.1;

		}

		//Weekday and weekend loads, summed and assumed half radiative / half
		//convective
		std::vector<double> TotalPlugHourlyWkday(24), SensibleEquipRadorConvWkday(24), SensibleEquipRadorConvWkend(24), TotalPlugHourlyWkend(24);
		for (int i = 0; i < 24; i++)
		{
			TotalPlugHourlyWkday[i] = (FridgeHourly[i] + DWHourly[i] + RangeHourly[i] + WasherHourly[i] + DryerHourly[i]) / 365;
			SensibleEquipRadorConvWkday[i] = 0.5*(FridgeHourly[i] + DWHourly[i] * 0.6 + RangeHourly[i] * 0.4 + WasherHourly[i] * 0.8 + DryerHourly[i] * 0.15) / 365;
			SensibleEquipRadorConvWkend[i] = SensibleEquipRadorConvWkday[i] * 1.1;   //These are just 50 / 50 rad and conv, but the multipliers are BLDG AM sensible load fractions
			TotalPlugHourlyWkend[i] = (FridgeHourlyWkend[i] + DWHourlyWkend[i] + RangeHourlyWkend[i] + WasherHourlyWkend[i] + DryerHourlyWkend[i]) / 365;
		}

		//Vacation!affects ONLY THE LARGE APPLIANCES AND OCCUPANCY as per Bldg AM
		//But why would DW, etc run if nobody home ? I set to just fridge for
		//appliances.
		std::vector<double> TotalPlugHourlyVacay(24), SensibleEquipRadorConvVacay(24);
		for (int i = 0; i < 24; i++)
		{
			TotalPlugHourlyVacay[i] = FridgeHourly[i] / 365;
			SensibleEquipRadorConvVacay[i] = 0.5*FridgeHourly[i] / 365;
		}

		//Now the Hourly MELS; these use the January BeOpt and then vary
		// (imprecisely)by month based on other months' BeOpts.   They do NOT vary
		//weekday vs.weekend, as per BeOpt.
		double MELSFrac[24] = { 0.441138, 0.406172, 0.401462, 0.395811, 0.380859, 0.425, 0.491056, 0.521783, 0.441138, 0.375444, 0.384274, 0.384391, 0.377916, 0.390984, 0.4130, 0.435957, 0.515661, 0.626446, 0.680131, 0.702029, 0.726164, 0.709211, 0.613731, 0.533321 };
		double MELSMonthly[12] = { 1, 1, .88, .88, .88, .77, .77, .77, .77, .85, .85, 1 };
		std::vector<double> MELSHourlyJan(24);
		double MELSHrFrac;
		double MELSMonthSum[12];
		double MELSFracSum = sum(MELSFrac, 24);

		for (int i = 0; i < 12; i++)
		{
			MELSMonthSum[i] = MELSFracSum * MELSMonthly[i] * util::nday[i];
		}
		MELSHrFrac = Load_mels / sum(MELSMonthSum, 12);

		for (int i = 0; i < 24; i++)
		{
			MELSHourlyJan[i] = MELSHrFrac*MELSFrac[i];
		}
		//And then the lighting.These again use Jan.BeOpt as a basis, but the
		//fractions vary by hour AND by month.This may be climate dependent but
		//that is ignored for now.The matrix will be rows are months and colums are the time
		//periods, ie 9 - 6, 7 - 3, 4 - 8.
		double LightFrac[24] = { 0.1758680, 0.1055210, 0.070347, 0.070347, 0.070347, 0.076992, 0.165661, 0.345977, 0.330648, 0.169731, 0.13829, 0.137022, 0.137272, 0.140247, 0.158163, 0.230715, 0.418541, 0.710948, 0.931195, 0.88306, 0.790511, 0.675746, 0.509894, 0.354185 };
		double L96 = sumsub(LightFrac, 0, 5) + sumsub(LightFrac,20,23);
		double L73 = sumsub(LightFrac, 6, 14);
		double L48 = sumsub(LightFrac,15,19);
		double Lightz[12][3] = { { L96, L73, L48 }, { L96, L73, L48 }, { L96, L73, L48 }, { L96, L73, L48 }, { L96, L73, L48 }, { L96, L73, L48 }, { L96, L73, L48 }, { L96, L73, L48 }, { L96, L73, L48 }, { L96, L73, L48 }, { L96, L73, L48 }, { L96, L73, L48 } };
		//LightMonHr = [1	1	1
		// 1	0.77	0.76
		// 1	0.52	0.55
		// 1	0.51	0.28
		// 1	0.41	0.21
		// 1	0.39	0.19
		// 1	0.41	0.19
		// 1	0.47	0.23
		// 1	0.61	0.36
		// 1	0.82	0.57
		// 1	0.84	1.02
		// 1	1.04	1.14
		//];
		double LightMonHr[12][3] = {
			{ 1, 1.05, 1.05 },
			{ 1, 0.9, 1 },
			{1, 0.6, 0.75},
			{1, 0.61, 0.3},
			{1, 0.45, 0.10},
			{1, 0.42, 0.10},
			{1, 0.43, 0.10},
			{1, 0.51, 0.14},
			{1, 0.62, 0.38},
			{1, 0.82, 0.52},
			{1, 0.84, 1.02},
			{1, 1.04, 1.14}
		};

		double LightUse[12][3];
		for (int i = 0; i < 12; i++)
		{
			for (int j = 0; j < 3; j++)
			{
				LightUse[i][j] = Lightz[i][j] * LightMonHr[i][j];
			}
		}
		
		std::vector<double> MonthlyDailyLightUse(12);

		for (int i = 0; i < 12; i++)
		{
			MonthlyDailyLightUse[i] = sum(LightUse[i],3);
		}

		double AnnualLightUseHrs = 0;
		for (int i = 0; i < 12; i++)
		{
			AnnualLightUseHrs += MonthlyDailyLightUse[i] * util::nday[i];
		}

		double  LightHrFrac = Load_light / AnnualLightUseHrs;
		std::vector<double> LightHourlyJan(24);
		for (int i = 0; i < 24; i++)
		{
			LightHourlyJan[i] = LightHrFrac*LightFrac[i];
		}

		//NEED UNITS!!!!!!
		//END INTERNAL ELEC LOADS
			
		//THIS IS TO FIGURE OUT THE HVAC FAN USAGE WITH GAS HEATER
		//Capacity of gas heater....really should be more climate - dependent
		double n_heat, GasHeat_capacity;
		if (YrBuilt > 1980 || EnergyRetrofits == 1)
		{
		      n_heat = 0.78; //HVAC sys efficiency for gas forced air
			  GasHeat_capacity = 35 * A_Floor / 1000; //kBTU / h
		}
		else
		{
			n_heat = 0.65;
			GasHeat_capacity = 40 * A_Floor / 1000;
		};
			
		//Aux elec for gas heater(fans, etc)
		double AuxHeat;
		if (en_heat == 0) // assume gas heater at this point
		//This is Bldg AM number :
			AuxHeat = 9.2*GasHeat_capacity; //kWh annual, should divide over running hours in the end
		else 
			AuxHeat = 0;
		//END HEATING FANS
			
		//COOLING SEER
		double SEER;
		if (YrBuilt >= 2005 || EnergyRetrofits == 1)
		// Note that I don't separate out fans/pumps, though BeOPT does!?  But my
		//predictions are *still* high.
			SEER = 13;
		else 
			SEER = 10;
		//END COOLING SEER
		
		int D = 1; //somehow I am one day off BEOPT so compensating here(this is days of the week)
		// TMY DEFAULT IS MONDAY!!!!!!!
		//Sol - Air -- This part is ALL SI -- get effective envelope temperatures for the heat transfer.
		std::vector<double> Vacay(8761), Hset(8761), Cset(8761);
		std::vector<double> Tmass(8761), Tair(8761), Tsurf(8761);
		double Heaton[8761];
		//All the initial loads - divided into radiatinve & convective
		
		std::vector<double> EquipElecHrLoad(8761), EquipRadHrLoad(8761), EquipConvHrLoad(8761), MELSElecHrLoad(8761), MELSRadHrLoad(8761);
		std::vector<double> MELSConvHrLoad(8761), LightElecHrLoad(8761), LightRadHrLoad(8761), LightConvHrLoad(8761), PPLRadHrLoad(8761), PPLConvHrLoad(8761);
		std::vector<double> TAnew(8761), TSnew(8761), TMnew(8761);
		std::vector<double> QInt_Rad(8761), QInt_Conv(8761), Q_SolWin(8761);
		std::vector<double> CFM(8761), UAInf(8761), QInf(8761), QG(8761);
		std::vector<double> QN(8761), QHV2(8761), Tdiff(8761);
		
		
		std::vector<double> HourlyNonHVACLoad(8761);

		//MAIN 8760 LOOP STARTS HERE*************************************************************************************************************

		for (int i = 0; i < 8760; i++)
		{
			Vacay[i] = 0; //Initialize vacation to zero
			int Hr = hour[i];
			int NextHr = hour[i + 1];
			
			//The day of the week (to figure out weekends)
			if (Hr == 1)
				D = D + 1;
			if (D > 7)
				D = 1;
			int Mon = month[i];
			int Dy = day[i];
			int NextMon = month[i + 1];
			int NextDay = day[i + 1];

			//Are we on vacation ?
			for (int v = 0; v < N_vacation; v++)
			{
				if (Mon == (VacationMonths[v]-1) && Dy == (VacationDays[v]-1)) //need to subtract 1 from VacationMonths and VacationDays because Mon and Dy are 0 subscripted (Jan is Mon[0])
					Vacay[i] = 1;

				if (NextMon == (VacationMonths[v]-1) && NextDay == (VacationDays[v]-1))
					Vacay[i + 1] = 1;
			}

			//First the setpoints for heating / cooling
			if (Vacay[i] == 0 && T_Sched[Hr] == 1)
			{
				Hset[i] = THeat;
				Cset[i] = TCool;
			}
			else  //setback if on vacation or if temperature schedule says so
			{
				Hset[i] = THeatSB;
				Cset[i] = TCoolSB;
			};

			if (i == 0) // First hour has to be preset.It's January so I assume it's heating.
			{
				Tmass[i] = Hset[i];
				Tair[i] = Hset[i];
				Tsurf[i] = Hset[i];
				Heaton[i] = HtEn[1];
				//All the initial loads - divided into radiatinve & convective
				EquipElecHrLoad[i] = TotalPlugHourlyWkend[Hr];
				EquipRadHrLoad[i] = SensibleEquipRadorConvWkend[Hr];
				EquipConvHrLoad[i] = SensibleEquipRadorConvWkend[Hr];
				MELSElecHrLoad[i] = MELSHourlyJan[Hr];
				MELSRadHrLoad[i] = MELSElecHrLoad[i] * 0.5*0.734;
				MELSConvHrLoad[i] = MELSElecHrLoad[i] * 0.5*0.734;
				LightElecHrLoad[i] = LightHourlyJan[Hr];
				LightRadHrLoad[i] = LightElecHrLoad[i] * 0.7;
				LightConvHrLoad[i] = LightElecHrLoad[i] * 0.3;
				PPLRadHrLoad[i] = PPL_rad[Hr];
				PPLConvHrLoad[i] = PPL_conv[Hr];
			}

			if (Vacay[i] == 1) // Less loads if on vacation
			{
				EquipElecHrLoad[i] = TotalPlugHourlyVacay[Hr];
				EquipRadHrLoad[i] = SensibleEquipRadorConvVacay[Hr];
				EquipConvHrLoad[i] = EquipRadHrLoad[i];
				PPLRadHrLoad[i] = 0;
				PPLConvHrLoad[i] = 0;
			}

			if (i == 8760) // assign vals for hour "8761" which is just part of the euler forward calculation
			{
				EquipElecHrLoad[i + 1] = EquipElecHrLoad[1];
				EquipRadHrLoad[i + 1] = SensibleEquipRadorConvWkend[1];
				EquipConvHrLoad[i + 1] = SensibleEquipRadorConvWkend[1];
				MELSElecHrLoad[i + 1] = MELSHourlyJan[1];
				MELSRadHrLoad[i + 1] = MELSElecHrLoad[1] * 0.5*0.734;
				MELSConvHrLoad[i + 1] = MELSElecHrLoad[1] * 0.5*0.734;
				LightElecHrLoad[i + 1] = LightHourlyJan[1];
				LightRadHrLoad[i + 1] = LightElecHrLoad[1] * 0.7;
				LightConvHrLoad[i + 1] = LightElecHrLoad[1] * 0.3;
				PPLRadHrLoad[i + 1] = PPL_rad[1];
				PPLConvHrLoad[i + 1] = PPL_conv[1];
			}

			for (int v = 0; v < N_vacation; v++)
			{
				if (VacationMonths[v] == 1 && VacationDays[v] == 1)
					EquipElecHrLoad[i + 1] = TotalPlugHourlyVacay[1];
				EquipRadHrLoad[i + 1] = SensibleEquipRadorConvVacay[1];
				PPLRadHrLoad[i + 1] = 0;
				PPLConvHrLoad[i + 1] = 0;
			}

			if (i > 1) // These are the new values for each temperature, which were determined previous timestep
			{
				Tair[i] = TAnew[i - 1];
				Tsurf[i] = TSnew[i - 1];
				Tmass[i] = TMnew[i - 1];
			}

			//Interior gains
			//Equipment load -- depends on whether weekday or weekend
			if (Vacay[i + 1] == 1) // Vacation
			{
				EquipElecHrLoad[i + 1] = TotalPlugHourlyVacay[NextHr];
				EquipRadHrLoad[i + 1] = SensibleEquipRadorConvVacay[NextHr];
				EquipConvHrLoad[i + 1] = SensibleEquipRadorConvVacay[NextHr];
			}
			else if ((D == 2 && Hr < 24) || (D == 7 && Hr == 24) || D == 1) // weekend!(hour i + 1)
			{
				EquipElecHrLoad[i + 1] = TotalPlugHourlyWkend[NextHr];
				EquipRadHrLoad[i + 1] = SensibleEquipRadorConvWkend[NextHr];   //These are just 50 / 50 rad and conv, but the multipliers are BLDG AM sensible load fractions
				EquipConvHrLoad[i + 1] = SensibleEquipRadorConvWkend[NextHr];
			}
			else //weekday(hour i + 1)
			{
				EquipElecHrLoad[i + 1] = TotalPlugHourlyWkday[NextHr];
				EquipRadHrLoad[i + 1] = SensibleEquipRadorConvWkday[NextHr];   //These are just 50 / 50 rad and conv, but the multipliers are BLDG AM sensible load fractions
				EquipConvHrLoad[i + 1] = SensibleEquipRadorConvWkday[NextHr];
			}

			//Next the MELS
			MELSElecHrLoad[i + 1] = MELSHourlyJan[NextHr] * MELSMonthly[NextMon];
			MELSRadHrLoad[i + 1] = MELSElecHrLoad[i + 1] * 0.734*0.5;
			MELSConvHrLoad[i + 1] = MELSElecHrLoad[i + 1] * 0.734*0.5;

			//And the lighting
			int ind = 0;
			if (NextHr > 20 || NextHr<7)
				ind = 0;
			else if (NextHr>6 && NextHr < 16)
				ind = 1;
			else ind = 2;

			LightElecHrLoad[i + 1] = LightHourlyJan[NextHr] * LightMonHr[NextMon][ind];
			LightRadHrLoad[i + 1] = LightElecHrLoad[i + 1] * 0.7;
			LightConvHrLoad[i + 1] = LightElecHrLoad[i + 1] * 0.3;

			//And finally the people!
			if (Vacay[i + 1] == 1)
			{
				PPLRadHrLoad[i + 1] = 0;
				PPLConvHrLoad[i + 1] = 0;
			}
			else
			{
				PPLRadHrLoad[i + 1] = PPL_rad[NextHr];
				PPLConvHrLoad[i + 1] = PPL_conv[NextHr];
			}

			//Convert internal gains to BTU / hr(radiative and convective)
			QInt_Rad[i + 1] = 3412.142*(PPLRadHrLoad[i + 1] + LightRadHrLoad[i + 1] + MELSRadHrLoad[i + 1] + EquipRadHrLoad[i + 1]);
			QInt_Conv[i + 1] = 3412.142*(PPLConvHrLoad[i + 1] + LightConvHrLoad[i + 1] + MELSConvHrLoad[i + 1] + EquipConvHrLoad[i + 1]);

			//Solar Gains - distributed to envelope evenly(all walls, ceil, floor)
			// and to internal mass, with fractions of each denoted above.
			Q_SolWin[i + 1] = SHGC*(RadWallE[i + 1] + RadWallW[i + 1] + RadWallN[i + 1] + RadWallS[i + 1]) / 4 * A_Wins / 10.764; //This is now Watts
			Q_SolWin[i + 1] = Q_SolWin[i + 1] * 3.412;  //And now it's BTU/hr

			//Get the infiltration(cheating - using this hr's Tair; is assumed similar to prev hour's)
			CFM[i + 1] = ELA * sqrt(Cs*abs(T_ambF[i + 1] - Tair[i]) + Cw*pow(VwindMPH[i + 1], 2)) * 0.67;
			UAInf[i + 1] = CFM[i + 1] * 60 * 0.018;
			QInf[i] = UAInf[i] * (T_ambF[i] - Tair[i]); //This would be BTU / hr -- all convective
			QG[i] = QInt_Conv[i] + QInf[i]; //Same!

			//Calculate the new air temperature in the space(euler forward)
			double bar = 1 + dT / Cenv / Renv + dT / hsurf / Cenv;
			double bardub = 1 + dT / Cmass / hsurf;
			double TAnewBot = 1 + UAInf[i + 1] * dT / Cair + dT / Cair*AIntMass / hsurf - pow(dT, 2) * AIntMass / Cair / Cmass / hsurf / hsurf / bardub + Aenv*dT / Cair / hsurf - dT*Aenv / Cair / Cenv / hsurf / hsurf / bar;
			double TAnewTop = Tair[i] + dT / Cair*(QInt_Conv[i + 1] + UAInf[i + 1] * T_ambF[i + 1]) + dT*AIntMass*(Tmass[i] + SolMassFrac*(Q_SolWin[i + 1] / AIntMass + QInt_Rad[i + 1] / AIntMass)) / Cair / hsurf / bardub + Aenv*dT / Cair / hsurf / bar*(Tsurf[i] + dT*T_solairF[i + 1] / Cenv / Renv + SolEnvFrac*(Q_SolWin[i + 1] / Aenv + QInt_Rad[i + 1] / Aenv));
			TAnew[i] = TAnewTop / TAnewBot;

			//Plug loads (non-HVAC)
			double HourlyNonHVACLoad = (LightElecHrLoad[i] + MELSElecHrLoad[i] + EquipElecHrLoad[i]); //kWh
			non_hvac_load[i] = (ssc_number_t)HourlyNonHVACLoad * 1000; //Wh

			//Now for the HVAC controls
			if (Cset[i] <= TAnew[i]) // Cooling temperature requirement met
			{
				if (ClEn[Mon] == 0) // This is if not in cooling season!
				{
					Heaton[i] = 0;
					QN[i + 1] = 0;
					QHV2[i + 1] = 0;
				}
				else    //Actually Cooling
				{
					Heaton[i] = 0;
					Tdiff[i] = TAnew[i] - Cset[i];
					TAnew[i] = Cset[i];
					QN[i + 1] = Cair / dT / 1 * (TAnew[i] * TAnewBot - TAnewTop);  //BTU
					QHV2[i + 1] = QN[i] / SEER*en_cool;
				}
			}
			else if (HtEn[Mon] == 0) // Heating temperature met, but not in season
			{
				Heaton[i] = 0;
				QN[i + 1] = 0;
				QHV2[i + 1] = 0;
			}
			else //really heating
			{
				Heaton[i] = 1;
				Tdiff[i] = Hset[i] - TAnew[i];
				TAnew[i] = Hset[i];
				QN[i + 1] = Cair / dT*(TAnew[i] * TAnewBot - TAnewTop);  //BTU
				QHV2[i + 1] = (QN[i] * 0.2931)*en_heat; //Wh
			}
			TMnew[i] = (Tmass[i] + dT / Cmass*(TAnew[i] / hsurf + SolMassFrac*(Q_SolWin[i] + QInt_Rad[i]) / AIntMass)) / bardub;
			TSnew[i] = (Tsurf[i] + dT / Cenv*(SolEnvFrac*(Q_SolWin[i] / Aenv + QInt_Rad[i] / Aenv) + T_solairF[i + 1] / Renv + TAnew[i] / hsurf)) / bar;

			//HVAC Loads completed
			hvac_load[i] = QHV2[i]; //Wh

			//Total load for the hour
			load[i] = hvac_load[i] + non_hvac_load[i]; //Wh

		}

		//Must modify the HVAC load for gas heat

		//Aux heating fans(if gas heat)
		double HrsHeat = sum(Heaton, 8760);
		double AuxHeatPerHr;
		if (HrsHeat != 0)
		{
			AuxHeatPerHr = AuxHeat / HrsHeat / 1000; //This is Wh
			for (i = 0; i < 8760; i++)
			{
				if (Heaton[i])
				{
					hvac_load[i] += AuxHeatPerHr; //Wh
					load[i] = hvac_load[i] + non_hvac_load[i]; //Wh
				}				
			}
		}
	}
};

DEFINE_MODULE_ENTRY( belpe, "Estimates an electric load profile given basic building characteristics and a weather file", 1 )