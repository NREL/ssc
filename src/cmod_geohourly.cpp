#include "core.h"
#include "lib_wfreader.h"
#include "lib_powerblock.h"
#include "lib_physics.h"
#include "lib_geohourly_interface.h"


static var_info _cm_vtab_geohourly[] = {
/*   VARTYPE           DATATYPE         NAME							LABEL								UNITS			META				GROUP          REQUIRED_IF			CONSTRAINTS             UI_HINTS*/

	// climate and resource inputs
	{ SSC_INPUT,        SSC_STRING,     "file_name",					"local weather file path",			"",				"",					"Weather",			"*",			"LOCAL_FILE",			"" },
	{ SSC_INPUT,        SSC_NUMBER,     "resource_potential",			"Resource Potential",				"MW",			"",					"GeoHourly",		"*",            "",						"" },
	{ SSC_INPUT,        SSC_NUMBER,     "resource_type",				"Type of Resource",					"",				"",					"GeoHourly",		"*",            "INTEGER",				"" },
	{ SSC_INPUT,        SSC_NUMBER,     "resource_temp",				"Resource Temperature",				"C",			"",                 "GeoHourly",		"*",            "",						"" },
	{ SSC_INPUT,        SSC_NUMBER,     "resource_depth",				"Resource Depth",					"m",			"",                 "GeoHourly",		"*",            "",						"" },

	// Other inputs
	{ SSC_INPUT,		SSC_NUMBER,		"analysis_period",				"Analysis Lifetime",				"years",		"",					"GeoHourly",		"*",			"INTEGER",				"" },
	{ SSC_INPUT,		SSC_NUMBER,		"model_choice",					"Which model to run (0,1,2)",		"",				"",					"GeoHourly",		"*",			"INTEGER",				"" },

	// geothermal plant and equipment
	{ SSC_INPUT,		SSC_NUMBER,		"nameplate",					"Desired plant output",				"kW",			"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"analysis_type",				"Analysis Type",					"",				"",					"GeoHourly",		"*",			"INTEGER",				"" },
	{ SSC_INPUT,		SSC_NUMBER,		"num_wells",					"Number of Wells",					"",				"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"num_wells_getem",				"Number of Wells GETEM calc'd",		"",				"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"conversion_type",				"Conversion Type",					"",				"",					"GeoHourly",		"*",			"INTEGER",				"" },
	{ SSC_INPUT,		SSC_NUMBER,		"plant_efficiency_input",		"Plant efficiency",					"",				"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"conversion_subtype",			"Conversion Subtype",				"",				"",					"GeoHourly",		"*",			"INTEGER",				"" },
	{ SSC_INPUT,		SSC_NUMBER,		"decline_type",					"Temp decline Type",				"",				"",					"GeoHourly",		"*",			"INTEGER",				"" },
	{ SSC_INPUT,		SSC_NUMBER,		"temp_decline_rate",			"Temperature decline rate",			"%/yr",			"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"temp_decline_max",				"Maximum temperature decline",		"C",			"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"wet_bulb_temp",				"Wet Bulb Temperature",				"C",			"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"ambient_pressure",				"Ambient pressure",					"psi",			"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"well_flow_rate",				"Production flow rate per well",	"kg/s",			"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"pump_efficiency",				"Pump efficiency",					"%",			"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"delta_pressure_equip",	"Delta pressure across surface equipment",	"psi",			"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"excess_pressure_pump",			"Excess pressure @ pump suction",	"psi",			"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"well_diameter",				"Production well diameter",			"in",			"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"casing_size",					"Production pump casing size",		"in",			"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"inj_well_diam",				"Injection well diameter",			"in",			"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"design_temp",					"Power block design temperature",	"C",			"",					"GeoHourly",		"*",			"",						"" },

	// detailed geothermal inputs
	{ SSC_INPUT,		SSC_NUMBER,		"rock_thermal_conductivity",		"Rock thermal conductivity",	"J/m-day-C",	"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"rock_specific_heat",				"Rock specific heat",			"J/kg-C",		"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"rock_density",						"Rock density",					"kg/m^3",		"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"reservoir_pressure_change_type",	"Reservoir pressure change type","",			"",					"GeoHourly",		"*",			"INTEGER",				"" },
	{ SSC_INPUT,		SSC_NUMBER,		"reservoir_pressure_change",		"Pressure change",				"psi-h/1000lb",	"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"reservoir_width",					"Reservoir width",				"m",			"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"reservoir_height",					"Reservoir height",				"m",			"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"reservoir_permeability",			"Reservoir Permeability",		"darcys",		"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"inj_prod_well_distance","Distance from injection to production wells",	"m",		"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"subsurface_water_loss",			"Subsurface water loss",		"%",			"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"fracture_aperature",				"Fracture aperature",			"m",			"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"fracture_width",					"Fracture width",				"m",			"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_INPUT,		SSC_NUMBER,		"num_fractures",					"Number of fractures",			"",				"",					"GeoHourly",		"*",			"INTEGER",				"" },
	{ SSC_INPUT,		SSC_NUMBER,		"fracture_angle",					"Fracture angle",				"deg",			"",					"GeoHourly",		"*",			"",						"" },

	// power block inputs (these could change on an hourly basis, but don't here)


	// power block parameters needed but not on power block SAM input page
	{ SSC_INPUT,        SSC_NUMBER,      "tech_type",						"Technology type ID",			"(1-4)",		"",					"GeoHourly",     "*",             "INTEGER",				"" },
	{ SSC_INPUT,        SSC_NUMBER,      "T_htf_cold_ref",					"Outlet design temp",			"C",			"",					"GeoHourly",     "*",             "",						"" },
	{ SSC_INPUT,        SSC_NUMBER,      "T_htf_hot_ref",					"Inlet design temp",			"C",			"",                 "GeoHourly",     "*",             "",						"" },
	{ SSC_INPUT,        SSC_NUMBER,      "HTF",								"Heat trans fluid type ID",		"(1-27)",		"",                 "GeoHourly",     "*",             "INTEGER",				"" },

	// power block input parameters
	//{ SSC_INPUT,        SSC_NUMBER,      "P_ref",							"Design Output",					"MW",		"",                      "GeoHourly",     "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "P_boil",							"Design Boiler Pressure",			"bar",		"",                      "GeoHourly",     "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "eta_ref",							"Desgin conversion efficiency",		"%",		"",                      "GeoHourly",     "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "q_sby_frac",						"% thermal power for standby mode",	"%",		"",                      "GeoHourly",     "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "startup_frac",					"% thermal power for startup",		"%",		"",                      "GeoHourly",     "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "startup_time",					"Hours to start power block",		"hours",	"",                      "GeoHourly",     "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "pb_bd_frac",						"Blowdown steam fraction",			"%",		"",                      "GeoHourly",     "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "T_amb_des",						"Design ambient temperature",		"C",		"",                      "GeoHourly",     "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "CT",								"Condenser type (Wet, Dry,Hybrid)",	"(1-3)",	"",                      "GeoHourly",     "*",             "INTEGER",               "" },
	{ SSC_INPUT,        SSC_NUMBER,      "dT_cw_ref",						"Design condenser cooling water inlet/outlet T diff",	"C","",			 "GeoHourly",     "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "T_approach",						"Approach Temperature",				"C",		"",                      "GeoHourly",     "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "T_ITD_des",						"Design ITD for dry system",		"C",		"",                      "GeoHourly",     "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "P_cond_ratio",					"Condenser pressure ratio",         "",			"",                      "GeoHourly",     "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "P_cond_min",						"Minimum condenser pressure",		"in Hg",	"",                      "GeoHourly",     "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hr_pl_nlev",						"# part-load increments",			"(0-9)",	"",                      "GeoHourly",     "*",             "INTEGER",               "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hc_ctl1",							"HC Control 1",						"",			"",                      "GeoHourly",     "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hc_ctl2",							"HC Control 2",						"",			"",                      "GeoHourly",     "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hc_ctl3",							"HC Control 3",						"",			"",                      "GeoHourly",     "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hc_ctl4",							"HC Control 4",						"",			"",                      "GeoHourly",     "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hc_ctl5",							"HC Control 5",						"",			"",                      "GeoHourly",     "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hc_ctl6",							"HC Control 6",						"",			"",                      "GeoHourly",     "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hc_ctl7",							"HC Control 7",						"",			"",                      "GeoHourly",     "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hc_ctl8",							"HC Control 8",						"",			"",                      "GeoHourly",     "*",             "",                      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "hc_ctl9",							"HC Control 9",						"",			"",                      "GeoHourly",     "*",             "",                      "" },

	// OUTPUTS
	// User can specify whether the analysis should be done hourly or monthly.  With monthly analysis, there are only monthly results.
	// With hourly analysis, there are still monthly results, but there are hourly (over the whole lifetime of the project) results as well.
	{ SSC_OUTPUT,       SSC_NUMBER,		"pump_work",							"Pump work",									"MW",		"",				"GeoHourly",     "*",             "",						"" },

	{ SSC_OUTPUT,       SSC_ARRAY,       "annual_replacements",					"Resource replacement? (1=yes)",				"kWhac",	"",				"GeoHourly",     "*",             "",						"" },

	{ SSC_OUTPUT,       SSC_ARRAY,		 "monthly_resource_temperature",		"Monthly avg resource temperature",				"C",		"",             "GeoHourly",     "*",             "",						"" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_power",						"Monthly power",								"kW",		"",				"GeoHourly",     "*",             "",						"" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "monthly_energy",						"Monthly energy",								"kWh",		"",				"GeoHourly",     "*",             "",						"" },

	{ SSC_OUTPUT,       SSC_ARRAY,		 "timestep_resource_temperature",		"Resource temperature in each time step",		"C",		"",				"GeoHourly",     "*",             "",						"" },
	{ SSC_OUTPUT,       SSC_ARRAY,       "timestep_power",						"Power in each time step",						"kW",		"",				"GeoHourly",     "*",             "",						"" },
	{ SSC_OUTPUT,       SSC_ARRAY,	     "timestep_test_values",				"Test output values in each time step",			"",			"",             "GeoHourly",     "*",             "",						"" },

	{ SSC_OUTPUT,       SSC_ARRAY,	     "timestep_pressure",					"Atmospheric pressure in each time step",		"atm",		"",             "GeoHourly",     "*",             "",						"" },
	{ SSC_OUTPUT,       SSC_ARRAY,	     "timestep_dry_bulb",					"Dry bulb temperature in each time step",		"C",		"",             "GeoHourly",     "*",             "",						"" },
	{ SSC_OUTPUT,       SSC_ARRAY,	     "timestep_wet_bulb",					"Wet bulb temperature in each time step",		"C",		"",             "GeoHourly",     "*",             "",						"" },


var_info_invalid };

class cm_geothermalhourly : public compute_module
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

	cm_geothermalhourly()
	{
		add_var_info( _cm_vtab_geohourly );
	}

	void exec( ) throw( general_error )
	{
		// Set power block parameters (parameters don't change hourly)
		SPowerBlockParameters pbp;
		// power block parameters NOT on the SAM power block input page
		pbp.tech_type = as_integer("tech_type");			// flag for which coef set to use (1=tower,2=trough,3=Sliding pressure power cycle formulation, 4=geothermal)
		pbp.T_htf_cold_ref = as_double("T_htf_cold_ref");	// design outlet fluid temp
		pbp.T_htf_hot_ref = as_double("T_htf_hot_ref");		// design inlet fluid temp
		pbp.HTF = as_integer("HTF");						// heat transfer fluid type

		// power block parameters on the SAM power block input page
		pbp.P_ref = as_double("nameplate")/1000; // P_ref wants MW, 'nameplate' in kW
		pbp.P_boil = as_double("P_boil");
		pbp.eta_ref = as_double("eta_ref");
		pbp.q_sby_frac = as_double("q_sby_frac");
		pbp.startup_frac = as_double("startup_frac");
		pbp.startup_time = as_double("startup_time");
		pbp.pb_bd_frac = as_double("pb_bd_frac");
		pbp.T_amb_des = as_double("T_amb_des");
		pbp.CT = as_integer("CT");
		pbp.dT_cw_ref = as_double("dT_cw_ref");
		pbp.T_approach = as_double("T_approach");
		pbp.T_ITD_des = as_double("T_ITD_des");
		pbp.P_cond_ratio = as_double("P_cond_ratio");
		pbp.P_cond_min = as_double("P_cond_min");
		pbp.n_pl_inc = as_integer("hr_pl_nlev");
		pbp.F_wc[0] = as_double("hc_ctl1");
		pbp.F_wc[1] = as_double("hc_ctl2");
		pbp.F_wc[2] = as_double("hc_ctl3");
		pbp.F_wc[3] = as_double("hc_ctl4");
		pbp.F_wc[4] = as_double("hc_ctl5");
		pbp.F_wc[5] = as_double("hc_ctl6");
		pbp.F_wc[6] = as_double("hc_ctl7");
		pbp.F_wc[7] = as_double("hc_ctl8");
		pbp.F_wc[8] = as_double("hc_ctl9");

		// Set power block input values that won't change hourly in geothermal model
		SPowerBlockInputs pbInputs;
		pbInputs.mode = 2;
		if ( as_integer("analysis_type") == 0) // used number of wells as calculated by GETEM
			pbInputs.m_dot_htf = as_double("well_flow_rate")* 3600.0 * as_double("num_wells_getem"); // (kg/sec) * (sec/hour) * (# wells) = total flow (kg/hour)
		else // use number of wells input by user
			pbInputs.m_dot_htf = as_double("well_flow_rate")* 3600.0 * as_double("num_wells"); // (kg/sec) * (sec/hour) * (# wells) = total flow (kg/hour)
		pbInputs.demand_var = pbInputs.m_dot_htf;
		pbInputs.standby_control = 1;
		pbInputs.rel_humidity = 0.7;
		pbInputs.f_restart = 1.0;
		pbInputs.TOU = 4; // HOW TO CALCULATE THE TIME OF USE PERIOD FROM HOUR OF YEAR???????????????????


		// Geothermal inputs **********************************************
		CGeothermalInterface oGeo;
		if ( as_integer("analysis_type") == 0)
			oGeo.SetDesiredPlantSalesKW( as_double("nameplate") );		// automatically sets calculation basis to 'desired plant output'
		else
			oGeo.SetDesiredNumberOfWells( as_double("num_wells") );	// automatically sets calculation basis to 'desired number of wells'
		oGeo.SetConversionType( 1+as_integer("conversion_type") );
		oGeo.SetPlantEfficiency( as_double("plant_efficiency_input")/100 ); 
		oGeo.SetFlashSubType( 1+as_integer("conversion_subtype") );

		// power block parameters and initial inputs
		oGeo.SetWeatherFileName(as_string("file_name"));
		oGeo.SetPowerBlockParameters(pbp);
		oGeo.SetPowerBlockInputs(pbInputs);
	
		// temperature decline
		oGeo.SetTemperatureDeclineMethod( 1+as_integer("decline_type") );
		oGeo.SetTemperatureDeclineRate( as_double("temp_decline_rate")/100 );
		oGeo.SetMaxTempDeclineC( as_double("temp_decline_max") );

		// flash inputs	
		oGeo.SetWetBulbTemperatureC( as_double("wet_bulb_temp") );
		oGeo.SetPressureAmbientPSI( as_double("ambient_pressure" ) );

		//pumping parameters
		oGeo.SetProductionFlowRateKgPerS( as_double("well_flow_rate") );
		oGeo.SetPumpEfficiency( as_double("pump_efficiency")/100 );
		oGeo.SetPressureChangeAcrossSurfaceEquipment( as_double("delta_pressure_equip") );
		oGeo.SetExcessPressurePSI( as_double("excess_pressure_pump") );
		oGeo.SetProductionWellDiameter( as_double("well_diameter") );
		oGeo.SetPumpCasingDiameter( as_double("casing_size") );
		oGeo.SetInjectionWellDiameter( as_double("inj_well_diam") );

		//resource characterization
		oGeo.SetPotentialResourceMW( as_double("resource_potential" ) );
		oGeo.SetResourceType( 1+as_integer("resource_type") );
		oGeo.SetResourceDepthMeters( as_double("resource_depth") );
		oGeo.SetResourceTemperatureCelcius( as_double("resource_temp") );
		oGeo.SetPlantDesignTemperatureCelcius( as_double("design_temp") );

		//reservoir properties
		oGeo.SetRockThermalConductivity( as_double("rock_thermal_conductivity") );
		oGeo.SetRockSpecificHeat(  as_double("rock_specific_heat") );
		oGeo.SetRockDensity(  as_double("rock_density") );
		oGeo.SetPressureCalculationMethod( 1+as_integer("reservoir_pressure_change_type"));
		switch(as_integer("reservoir_pressure_change_type"))
		{
			case 0: // pressure change entered by user
				oGeo.SetReservoirPressureChange( as_double("reservoir_pressure_change") );
				break;

			case 1: // use fracture flow (EGS only)
				break;

			case 2: // permeability * area
				oGeo.SetReservoirWidthM( as_double("reservoir_width") );
				oGeo.SetReservoirHeightM( as_double("reservoir_height") );
				oGeo.SetReservoirPermeability( as_double("reservoir_permeability") );
				oGeo.SetWellDistance2( as_double("inj_prod_well_distance") );
				break;
		}
		oGeo.SetSubsurfaceWaterLossRate( as_double("subsurface_water_loss")/100 );
		oGeo.SetSubsurfaceFractureAperature( as_double("fracture_aperature") );
		oGeo.SetNumberOfFractures( as_double("num_fractures") );
		oGeo.SetSubsurfaceFractureWidth( as_double("fracture_width") );
		oGeo.SetSubsurfaceFractureAngle( as_double("fracture_angle") );


		// calculate output array sizes
		oGeo.SetModelChoice(as_integer("model_choice")); // with model choice set, model then 'knows' whether this is hourly or monthly analysis
		size_t iyears = as_integer("analysis_period");
		if ( iyears == 0)
			throw exec_error("geothermalhourly", "Invalid analysis period specified in the geothermal hourly model.");

		// set geothermal inputs RE how analysis is done and for how long
		oGeo.SetProjectLifeYears( iyears );

		// allocate lifetime annual arrays (one element per year, over lifetime of project)
		ssc_number_t *annual_replacements = allocate( "annual_replacements", iyears);

		// allocate lifetime monthly arrays (one element per month, over lifetime of project)
		size_t monthly_array_size = 12 * iyears;
		ssc_number_t *monthly_resource_temp = allocate( "monthly_resource_temperature", monthly_array_size);
		ssc_number_t *monthly_power = allocate( "monthly_power", monthly_array_size);
		ssc_number_t *monthly_energy = allocate( "monthly_energy", monthly_array_size);

		// allocate lifetime timestep arrays (one element per timestep, over lifetime of project)
		// if this is a monthly analysis, these are redundant with monthly arrays that track same outputs
		size_t timestep_array_size = oGeo.GetTimeStepsInAnalysis();
		ssc_number_t *timestep_resource_temp = allocate( "timestep_resource_temperature", timestep_array_size);
		ssc_number_t *timestep_power = allocate( "timestep_power", timestep_array_size);
		ssc_number_t *timestep_test_values = allocate( "timestep_test_values", timestep_array_size);

		ssc_number_t *timestep_pressure = allocate( "timestep_pressure", timestep_array_size);
		ssc_number_t *timestep_dry_bulb = allocate( "timestep_dry_bulb", timestep_array_size);
		ssc_number_t *timestep_wet_bulb = allocate( "timestep_wet_bulb", timestep_array_size);

		// set pointer to annual array
		oGeo.SetPointerToReplacementArray(annual_replacements);

		// set pointers to monthly arrays
		oGeo.SetPointerToMonthlyTemperatureArray(monthly_resource_temp);
		oGeo.SetPointerToMonthlyOutputArray(monthly_power);
		oGeo.SetPointerToMonthlyPowerArray(monthly_energy);
		
		// set pointers for timestep arrays (for monthly analysis, these are redundant)
		oGeo.SetPointerToTimeStepTemperatureArray(timestep_resource_temp);
		oGeo.SetPointerToTimeStepOutputArray(timestep_power);
		oGeo.SetPointerToTimeStepTestArray(timestep_test_values);

		oGeo.SetPointerToTimeStepPressureArray(timestep_pressure);
		oGeo.SetPointerToTimeStepDryBulbArray(timestep_dry_bulb);
		oGeo.SetPointerToTimeStepWetBulbArray(timestep_wet_bulb);

		
		//update( "calculating", (float)0.0, (float)0.0 );
		//update("Running model...", 10.0);

		// run simulation
		if (oGeo.RunGeoHourly() != 0)
			throw exec_error("geothermalhourly", "error from geothermal hourly model: " + oGeo.GetErrorMsg() + ".");

		assign("pump_work", var_data((ssc_number_t) oGeo.ShowPumpWorkMW()) );

	}
};

DEFINE_MODULE_ENTRY( geothermalhourly, "Hourly geothermal model using general power block code from TRNSYS Type 224 code by M.Wagner, and some GETEM model code.", 2 );

