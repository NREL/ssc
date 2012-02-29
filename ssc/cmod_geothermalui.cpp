#include "core.h"
#include "lib_weatherfile.h"
#include "lib_physics.h"
#include "lib_geothermal.h"


//temporary for diagnostics
#include "lib_geohourly_interface.h"


static var_info _cm_vtab_geothermalui[] = {
/*   VARTYPE           DATATYPE         NAME							LABEL								UNITS			META				GROUP          REQUIRED_IF			CONSTRAINTS             UI_HINTS*/

	// climate and resource inputs
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
	{ SSC_INPUT,		SSC_NUMBER,		"specify_pump_work",			"Did user specify pump work?",		"0 or 1",		"",					"GeoHourly",		"*",			"INTEGER",				"" },
	{ SSC_INPUT,		SSC_NUMBER,		"specified_pump_work_amount",	"Pump work specified by user",		"MW",			"",					"GeoHourly",		"*",			"",						"" },

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

	// temp input for testing
	{ SSC_INPUT,        SSC_NUMBER,      "hr_pl_nlev",						"# part-load increments",			"(0-9)",	"",                      "GeoHourly",     "*",             "INTEGER",               "" },


	// OUTPUTS
	// User can specify whether the analysis should be done hourly or monthly.  With monthly analysis, there are only monthly results.
	// With hourly analysis, there are still monthly results, but there are hourly (over the whole lifetime of the project) results as well.
	{ SSC_OUTPUT,		SSC_NUMBER,		"num_wells_getem",					"Number of wells calc'd by GETEM",	"",			"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"plant_brine_eff",					"Plant Brine Efficiency",			"",			"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"gross_output",						"Gross output calc'd by GETEM",		"",			"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"pump_depth_ft",					"Pump depth calculated by GETEM",	"ft",		"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_OUTPUT,       SSC_NUMBER,		"pump_work",						"Pump work calc'd by GETEM",		"MW",		"",					"GeoHourly",	    "*",            "",						"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"pump_hp",							"Pump hp calculated by GETEM",		"hp",		"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"reservoir_pressure",				"Reservoir pres calc'd by GETEM",	"",			"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"reservoir_avg_temp",				"Avg reservoir temp calc'd by GETEM","C",		"",					"GeoHourly",		"*",			"",						"" },
	{ SSC_OUTPUT,		SSC_NUMBER,		"bottom_hole_pressure",				"Bottom hole pres calc'd by GETEM",	"",			"",					"GeoHourly",		"*",			"",						"" },

var_info_invalid };


static void my_update_function( float percent, void *data )
{
	if (data) ((compute_module*)data)->update("working...", percent);
}

class cm_geothermalui : public compute_module
{
private:
public:
	
	cm_geothermalui()
	{
		add_var_info( _cm_vtab_geothermalui );
	}

	void exec( ) throw( general_error )
	{


//-----------------------------------------------------------------------------------------------------------------------------------------------
if (as_integer("hr_pl_nlev") == 8)
{

		// set the geothermal model inputs -------------------------------------
		SGeothermal_Inputs geo_inputs;
		geo_inputs.md_RatioInjectionToProduction = 0.5; // THIS SHOULD BE AN INPUT. ALTHOUGH IT'S FROM THE COST PAGE, IT'S USED IN NON-COST EQUATION
		geo_inputs.md_DesiredSalesCapacityKW = as_double("nameplate");
		geo_inputs.md_NumberOfWells = as_double("num_wells");
		if ( as_integer("analysis_type") == 0)
			geo_inputs.me_cb = POWER_SALES;
		else
			geo_inputs.me_cb = NUMBER_OF_WELLS;

		if ( as_integer("conversion_type") == 0)
			geo_inputs.me_ct = BINARY;
		else if ( as_integer("conversion_type") == 1)
			geo_inputs.me_ct = FLASH;

		switch ( as_integer("conversion_subtype") )
		{
			case 0:	geo_inputs.me_ft = SINGLE_FLASH_NO_TEMP_CONSTRAINT; break;
			case 1:	geo_inputs.me_ft = SINGLE_FLASH_WITH_TEMP_CONSTRAINT; break;
			case 2:	geo_inputs.me_ft = DUAL_FLASH_NO_TEMP_CONSTRAINT; break;
			case 3:	geo_inputs.me_ft = DUAL_FLASH_WITH_TEMP_CONSTRAINT; break;			
		}
		geo_inputs.md_PlantEfficiency = as_double("plant_efficiency_input")/100;


		// temperature decline
		if ( as_integer("decline_type") == 0 )
			geo_inputs.me_tdm = ENTER_RATE;
		else if ( as_integer("decline_type") == 1 )
			geo_inputs.me_tdm = CALCULATE_RATE;
		geo_inputs.md_TemperatureDeclineRate = as_double("temp_decline_rate")/100;
		geo_inputs.md_MaxTempDeclineC = as_double("temp_decline_max");

		// flash inputs
		geo_inputs.md_TemperatureWetBulbC = as_double("wet_bulb_temp");
		geo_inputs.md_PressureAmbientPSI = as_double("ambient_pressure" );

		//pumping parameters
		geo_inputs.md_ProductionFlowRateKgPerS = as_double("well_flow_rate");
		geo_inputs.md_GFPumpEfficiency = as_double("pump_efficiency")/100;
		geo_inputs.md_PressureChangeAcrossSurfaceEquipmentPSI = as_double("delta_pressure_equip");
		geo_inputs.md_ExcessPressureBar = physics::PsiToBar( as_double("excess_pressure_pump") );
		geo_inputs.md_DiameterProductionWellInches = as_double("well_diameter");
		geo_inputs.md_DiameterPumpCasingInches = as_double("casing_size");
		geo_inputs.md_DiameterInjectionWellInches = as_double("inj_well_diam");
		geo_inputs.mb_CalculatePumpWork = ( 1 != as_integer("specify_pump_work") );
		geo_inputs.md_UserSpecifiedPumpWorkKW = as_double("specified_pump_work_amount") * 1000; // entered in MW

		//resource characterization
		if ( as_integer("resource_type") == 0 )
			geo_inputs.me_rt =  HYDROTHERMAL;
		else if ( as_integer("resource_type") == 1 )
			geo_inputs.me_rt = EGS;
		geo_inputs.md_ResourceDepthM = as_double("resource_depth");
		geo_inputs.md_TemperatureResourceC = as_double("resource_temp");
		geo_inputs.me_dc = TEMPERATURE;
		geo_inputs.md_TemperaturePlantDesignC = as_double("design_temp");

		//reservoir properties
		geo_inputs.md_TemperatureEGSAmbientC = 15.0;
		geo_inputs.md_EGSThermalConductivity = as_double("rock_thermal_conductivity");
		geo_inputs.md_EGSSpecificHeatConstant = as_double("rock_specific_heat");
		geo_inputs.md_EGSRockDensity = as_double("rock_density");
		switch(as_integer("reservoir_pressure_change_type"))
		{
			case 0: geo_inputs.me_pc = ENTER_PC; break;				// pressure change entered by user
			case 1: geo_inputs.me_pc = SIMPLE_FRACTURE; break;		// use fracture flow (EGS only)
			case 2: geo_inputs.me_pc = K_AREA; break;				// permeability * area
		}
		geo_inputs.md_ReservoirDeltaPressure = as_double("reservoir_pressure_change");
		geo_inputs.md_ReservoirWidthM = as_double("reservoir_width");
		geo_inputs.md_ReservoirHeightM = as_double("reservoir_height");
		geo_inputs.md_ReservoirPermeability = as_double("reservoir_permeability");
		geo_inputs.md_DistanceBetweenProductionInjectionWellsM = as_double("inj_prod_well_distance");
		geo_inputs.md_WaterLossPercent = as_double("subsurface_water_loss")/100;
		geo_inputs.md_EGSFractureAperature = as_double("fracture_aperature");
		geo_inputs.md_EGSNumberOfFractures = as_double("num_fractures");
		geo_inputs.md_EGSFractureWidthM = as_double("fracture_width");
		geo_inputs.md_EGSFractureAngle = as_double("fracture_angle");

		// calculate output array sizes
		geo_inputs.mi_ModelChoice = as_integer("model_choice");		 // 0=GETEM, 1=Power Block monthly, 2=Power Block hourly
		// set geothermal inputs RE how analysis is done and for how long
		geo_inputs.mi_ProjectLifeYears = as_integer("analysis_period");
		if ( geo_inputs.mi_ProjectLifeYears == 0)
			throw general_error("invalid analysis period specified in the geothermal hourly model");

		// Create output object
		SGeothermal_Outputs geo_outputs;

		// allocate lifetime annual arrays (one element per year, over lifetime of project)
		geo_outputs.maf_ReplacementsByYear = allocate( "annual_replacements", geo_inputs.mi_ProjectLifeYears);
		//ssc_number_t *annual_replacements = allocate( "annual_replacements", geo_inputs.mi_ProjectLifeYears);

		// allocate lifetime monthly arrays (one element per month, over lifetime of project)
		geo_outputs.maf_monthly_resource_temp = allocate( "monthly_resource_temperature", 12 * geo_inputs.mi_ProjectLifeYears);
		geo_outputs.maf_monthly_power = allocate( "monthly_power", 12 * geo_inputs.mi_ProjectLifeYears);
		geo_outputs.maf_monthly_energy = allocate( "monthly_energy", 12 * geo_inputs.mi_ProjectLifeYears);

		// allocate lifetime timestep arrays (one element per timestep, over lifetime of project)
		// if this is a monthly analysis, these are redundant with monthly arrays that track same outputs
		geo_inputs.mi_MakeupCalculationsPerYear = (geo_inputs.mi_ModelChoice == 2) ? 8760 : 12; 
		geo_inputs.mi_TotalMakeupCalculations = geo_inputs.mi_ProjectLifeYears * geo_inputs.mi_MakeupCalculationsPerYear; 

		geo_outputs.maf_timestep_resource_temp = allocate( "timestep_resource_temperature", geo_inputs.mi_TotalMakeupCalculations);
		geo_outputs.maf_timestep_power = allocate( "timestep_power", geo_inputs.mi_TotalMakeupCalculations);
		geo_outputs.maf_timestep_test_values = allocate( "timestep_test_values", geo_inputs.mi_TotalMakeupCalculations);

		geo_outputs.maf_timestep_pressure = allocate( "timestep_pressure", geo_inputs.mi_TotalMakeupCalculations);
		geo_outputs.maf_timestep_dry_bulb = allocate( "timestep_dry_bulb", geo_inputs.mi_TotalMakeupCalculations);
		geo_outputs.maf_timestep_wet_bulb = allocate( "timestep_wet_bulb", geo_inputs.mi_TotalMakeupCalculations);

		//update( "calculating", (float)0.0, (float)0.0 );
		//update("Running model...", 10.0);

		// run simulation
		std::string err_msg;
		if (FillOutputsForInterface( err_msg, geo_inputs, geo_outputs ) != 0)
			throw exec_error("geothermal", "error from geothermal hourly model: " + err_msg + ".");

		assign("num_wells_getem", var_data((ssc_number_t) geo_outputs.md_NumberOfWells ) );
		assign("plant_brine_eff", var_data((ssc_number_t) geo_outputs.md_PlantBrineEffectiveness ) );
		assign("gross_output", var_data((ssc_number_t) geo_outputs.md_GrossPlantOutputMW ) );

		assign("pump_depth_ft", var_data((ssc_number_t) geo_outputs.md_PumpDepthFt ) );
		assign("pump_work", var_data((ssc_number_t) geo_outputs.md_PumpWorkKW/1000 ) ); // kW must be converted to MW
		assign("pump_hp", var_data((ssc_number_t) geo_outputs.md_PumpHorsePower ) );

		assign("reservoir_pressure", var_data((ssc_number_t) geo_outputs.md_PressureChangeAcrossReservoir ) );
		assign("reservoir_avg_temp", var_data((ssc_number_t) physics::FarenheitToCelcius(geo_outputs.md_AverageReservoirTemperatureF) ) );
		assign("bottom_hole_pressure", var_data((ssc_number_t) geo_outputs.md_BottomHolePressure ) );
}//-----------------------------------------------------------------------------------------------------------------------------------------------
else
{//-----------------------------------------------------------------------------------------------------------------------------------------------
		// Geothermal inputs **********************************************
		CGeothermalInterface oGeo;
		if ( as_integer("analysis_type") == 0)
			oGeo.SetDesiredPlantSalesKW( as_double("nameplate") );		// automatically sets calculation basis to 'desired plant output'
		else
			oGeo.SetDesiredNumberOfWells( as_double("num_wells") );	// automatically sets calculation basis to 'desired number of wells'
		oGeo.SetConversionType( 1+as_integer("conversion_type") );
		oGeo.SetPlantEfficiency( as_double("plant_efficiency_input")/100 ); 
		oGeo.SetFlashSubType( 1+as_integer("conversion_subtype") );

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
		oGeo.SetCalculatePumpWork( 1 != as_integer("specify_pump_work") );
		oGeo.SetUserSpecifiedPumpWorkMW( as_double("specified_pump_work_amount") );

		//resource characterization
		oGeo.SetResourceType( 1+as_integer("resource_type") );
		oGeo.SetResourceDepthMeters( as_double("resource_depth") );
		double resource_temp = as_double("resource_temp");
		oGeo.SetResourceTemperatureCelcius( resource_temp );

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
			throw general_error("invalid analysis period specified in the geothermal hourly model");

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

		//samsim_set_d( (long)this,"geotherm.number_of_wells_used", getem.ShowNumberOfWells());				// (correct)
		//samsim_set_d( (long)this,"geotherm.plant_efficiency_used", getem.ShowPlantBrineEffectiveness());	// in watt-hr/lb (NEW)
		//samsim_set_d( (long)this,"geotherm.gross_output", getem.ShowGrossOutput());							// in MW (correct)
		//samsim_set_d( (long)this,"geotherm.pump_depth", getem.ShowPumpDepthFeet());							// in Meters (correct)
		//samsim_set_d( (long)this,"geotherm.pump_work", getem.ShowPumpWork());								// in MW (correct)
		//samsim_set_d( (long)this,"geotherm.pump_size_hp", getem.ShowPumpHorsePower());						// in Horse Power (correct)
		//samsim_set_d( (long)this,"geotherm.delta_pressure_reservoir", getem.ShowPressureChange());			// in PSI (I changed this)
		//samsim_set_d( (long)this,"geotherm.avg_reservoir_temp", getem.ShowAverageReservoirTemperature());	// in Celcius (correct)
		//samsim_set_d( (long)this,"geotherm.bottom_hole_pressure", getem.ShowBottomHolePressure());			// in PSI (correct)
		
		assign("num_wells_getem", var_data((ssc_number_t) oGeo.ShowNumberOfWells() ) );
		assign("plant_brine_eff", var_data((ssc_number_t) oGeo.ShowPlantBrineEffectiveness() ) );
		assign("gross_output", var_data((ssc_number_t) oGeo.ShowGrossOutput() ) );
		assign("pump_depth_ft", var_data((ssc_number_t) oGeo.ShowPumpDepthFeet() ) );
		assign("pump_work", var_data((ssc_number_t) oGeo.ShowPumpWorkMW()) );
		assign("pump_hp", var_data((ssc_number_t) oGeo.ShowPumpHorsePower() ) );
		assign("reservoir_pressure", var_data((ssc_number_t) oGeo.ShowPressureChange() ) );
		assign("reservoir_avg_temp", var_data((ssc_number_t) oGeo.ShowAverageReservoirTemperature() ) );
		assign("bottom_hole_pressure", var_data((ssc_number_t) oGeo.ShowBottomHolePressure() ) );

}//-----------------------------------------------------------------------------------------------------------------------------------------------



	}
};

DEFINE_MODULE_ENTRY( geothermalui, "Geothermal user interface calculations using GETEM model code.", 3 );

