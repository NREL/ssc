#include <memory>
#include <vector>

#include "lib_pv_io_manager.h" 

PVIOManager::PVIOManager(compute_module*  cm, std::string cmName)
{
	std::unique_ptr<Irradiance_IO> ptr(new Irradiance_IO(cm, cmName));
	m_IrradianceIO = std::move(ptr);

	std::unique_ptr<Simulation_IO> ptr2(new Simulation_IO(cm, *m_IrradianceIO));
	m_SimulationIO = std::move(ptr2);

	std::unique_ptr<ShadeDB8_mpp> shadeDatabase(new ShadeDB8_mpp());
	m_shadeDatabase = std::move(shadeDatabase);
	m_shadeDatabase->init();

	std::unique_ptr<Inverter_IO> ptrInv(new Inverter_IO(cm, cmName));
	m_InverterIO = std::move(ptrInv);

	// Gather subarrays which are enabled
	nSubarrays = 1;
	std::unique_ptr<Subarray_IO> subarray1(new Subarray_IO(cm, cmName, 1));
	m_SubarraysIO.push_back(std::move(subarray1));

	for (size_t subarray = 2; subarray <= 4; subarray++)
	{
		// can eventually create a module for each Subarray to allow flexibility.
		std::unique_ptr<Subarray_IO> ptr3(new Subarray_IO(cm, cmName, subarray));
		if (ptr3->enable) {
			m_SubarraysIO.push_back(std::move(ptr3));
			nSubarrays++;
		}
	}

	// Aggregate Subarray outputs in different structure
	std::unique_ptr<PVSystem_IO> pvSystem(new PVSystem_IO(cm, cmName, m_SimulationIO.get(), m_IrradianceIO.get(), getSubarrays(), m_InverterIO.get()));
	m_PVSystemIO = std::move(pvSystem);

	m_computeModule = cm;
	m_computeModuleName = cmName;
}

Irradiance_IO * PVIOManager::getIrradianceIO()  { return m_IrradianceIO.get(); }
compute_module * PVIOManager::getComputeModule()  { return m_computeModule; }
Subarray_IO * PVIOManager::getSubarrayIO(size_t subarrayNumber)  { return m_SubarraysIO[subarrayNumber].get(); }

std::vector<Subarray_IO *> PVIOManager::getSubarrays() 
{
	std::vector<Subarray_IO*> subarrays;
	for (size_t subarray = 0; subarray < m_SubarraysIO.size(); subarray++) {
		subarrays.push_back(m_SubarraysIO[subarray].get());
	}
	return subarrays;
}

PVSystem_IO * PVIOManager::getPVSystemIO()  { return m_PVSystemIO.get(); }

Simulation_IO * PVIOManager::getSimulationIO()  { return m_SimulationIO.get(); }


Simulation_IO::Simulation_IO(compute_module* cm, Irradiance_IO & IrradianceIO)
{
	numberOfWeatherFileRecords = IrradianceIO.numberOfWeatherFileRecords;
	stepsPerHour = IrradianceIO.stepsPerHour;
	dtHour = IrradianceIO.dtHour;

	if (cm->is_assigned("system_use_lifetime_output")) useLifetimeOutput = cm->as_integer("system_use_lifetime_output");
	numberOfYears = 1;
	if (useLifetimeOutput) {
		numberOfYears = cm->as_integer("analysis_period");
	}
	numberOfSteps = numberOfYears * numberOfWeatherFileRecords;
}

Irradiance_IO::Irradiance_IO(compute_module* cm, std::string cmName)
{
	numberOfSubarrays = 4;
	radiationMode = cm->as_integer("irrad_mode");
	skyModel = cm->as_integer("sky_model");

	if (cm->is_assigned("solar_resource_file")) {
		weatherDataProvider = std::unique_ptr<weather_data_provider>(new weatherfile(cm->as_string("solar_resource_file")));
		weatherfile *weatherFile = dynamic_cast<weatherfile*>(weatherDataProvider.get());
		if (!weatherFile->ok()) throw compute_module::exec_error(cmName, weatherFile->message());
		if (weatherFile->has_message()) cm->log(weatherFile->message(), SSC_WARNING);
	}
	else if (cm->is_assigned("solar_resource_data")) {
		weatherDataProvider = std::unique_ptr<weather_data_provider>(new weatherdata(cm->lookup("solar_resource_data")));
		if (weatherDataProvider->has_message()) cm->log(weatherDataProvider->message(), SSC_WARNING);
	}
	else {
		throw compute_module::exec_error(cmName, "No weather data supplied");
	}

	// assumes instantaneous values, unless hourly file with no minute column specified
	tsShiftHours = 0.0;
	instantaneous = true;
	if (weatherDataProvider->has_data_column(weather_data_provider::MINUTE))
	{
		// if we have an file with a minute column, then
		// the starting time offset equals the time 
		// of the first record (for correct plotting)
		// this holds true even for hourly data with a minute column
		weather_record rec;
		if (weatherDataProvider->read(&rec))
			tsShiftHours = rec.minute / 60.0;

		weatherDataProvider->rewind();
	}
	else if (weatherDataProvider->nrecords() == 8760)
	{
		// hourly file with no minute data column.  assume
		// integrated/averaged values and use mid point convention for interpreting results
		instantaneous = false;
		tsShiftHours = 0.5;
	}
	else
		throw compute_module::exec_error(cmName, "subhourly weather files must specify the minute for each record");

	weatherDataProvider->header(&weatherHeader);

	//total number of records in the weather file (i.e. 8760 * timestep)
	numberOfWeatherFileRecords = weatherDataProvider->nrecords();
	stepsPerHour = numberOfWeatherFileRecords / 8760;
	dtHour = 1.0 / stepsPerHour;

	if ( numberOfWeatherFileRecords % 8760 != 0 )
		throw compute_module::exec_error(cmName, util::format("invalid number of data records (%zu): must be an integer multiple of 8760", numberOfWeatherFileRecords));
	if (stepsPerHour < 1 || stepsPerHour > 60)
		throw compute_module::exec_error(cmName, util::format("%d timesteps per hour found. Weather data should be single year.", stepsPerHour));

	useWeatherFileAlbedo = cm->as_boolean("use_wf_albedo");
	userSpecifiedMonthlyAlbedo = cm->as_vector_double("albedo");
	
	checkWeatherFile(cm, cmName);
	AllocateOutputs(cm);
}

void Irradiance_IO::checkWeatherFile(compute_module * cm, std::string cmName)
{
	for (size_t idx = 0; idx < numberOfWeatherFileRecords; idx++)
	{
		if (!weatherDataProvider->read(&weatherRecord))
			throw compute_module::exec_error(cmName, "could not read data line " + util::to_string((int)(idx + 1)) + " in weather file");

		// Check for missing data
		if ((weatherRecord.gh != weatherRecord.gh) && (radiationMode == DN_GH || radiationMode == GH_DF)) {
			cm->log(util::format("missing global irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], exiting",
				weatherRecord.gh, weatherRecord.year, weatherRecord.month, weatherRecord.day, weatherRecord.hour), SSC_ERROR, (float)idx);
			return;
		}
		if ((weatherRecord.dn != weatherRecord.dn) && (radiationMode == DN_DF || radiationMode == DN_GH)) {
			cm->log(util::format("missing beam irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], exiting",
				weatherRecord.dn, weatherRecord.year, weatherRecord.month, weatherRecord.day, weatherRecord.hour), SSC_ERROR, (float)idx);
			return;
		}
		if ((weatherRecord.df != weatherRecord.df) && (radiationMode == DN_DF || radiationMode == GH_DF)) {
			cm->log(util::format("missing diffuse irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], exiting",
				weatherRecord.df, weatherRecord.year, weatherRecord.month, weatherRecord.day, weatherRecord.hour), SSC_ERROR, (float)idx);
			return;
		}
		if ((weatherRecord.poa != weatherRecord.poa) && (radiationMode == POA_R || radiationMode == POA_P)) {
			cm->log(util::format("missing POA irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], exiting",
				weatherRecord.poa, weatherRecord.year, weatherRecord.month, weatherRecord.day, weatherRecord.hour), SSC_ERROR, (float)idx);
			return;
		}
		if (weatherRecord.tdry != weatherRecord.tdry) {
			cm->log(util::format("missing temperature %lg W/m2 at time [y:%d m:%d d:%d h:%d], exiting",
				weatherRecord.tdry, weatherRecord.year, weatherRecord.month, weatherRecord.day, weatherRecord.hour), SSC_ERROR, (float)idx);
			return;
		}
		if (weatherRecord.wspd != weatherRecord.wspd) {
			cm->log(util::format("missing wind speed %lg W/m2 at time [y:%d m:%d d:%d h:%d], exiting",
				weatherRecord.wspd, weatherRecord.year, weatherRecord.month, weatherRecord.day, weatherRecord.hour), SSC_ERROR, (float)idx);
			return;
		}

		// Check for bad data
		if ((weatherRecord.gh < 0 || weatherRecord.gh > irradiationMax) && (radiationMode == DN_GH || radiationMode == GH_DF))
		{
			cm->log(util::format("out of range global irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], set to zero",
				weatherRecord.gh, weatherRecord.year, weatherRecord.month, weatherRecord.day, weatherRecord.hour), SSC_WARNING, (float)idx);
			weatherRecord.gh = 0;
		}
		if ((weatherRecord.dn < 0 || weatherRecord.dn > irradiationMax) && (radiationMode == DN_DF || radiationMode == DN_GH))
		{
			cm->log(util::format("out of range beam irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], set to zero",
				weatherRecord.dn, weatherRecord.year, weatherRecord.month, weatherRecord.day, weatherRecord.hour), SSC_WARNING, (float)idx);
			weatherRecord.dn = 0;
		}
		if ((weatherRecord.df < 0 || weatherRecord.df > irradiationMax) && (radiationMode == DN_DF || radiationMode == GH_DF))
		{
			cm->log(util::format("out of range diffuse irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], set to zero",
				weatherRecord.df, weatherRecord.year, weatherRecord.month, weatherRecord.day, weatherRecord.hour), SSC_WARNING, (float)idx);
			weatherRecord.df = 0;
		}
		if ((weatherRecord.poa < 0 || weatherRecord.poa > irradiationMax) && (radiationMode == POA_R || radiationMode == POA_P))
		{
			cm->log(util::format("out of range POA irradiance %lg W/m2 at time [y:%d m:%d d:%d h:%d], set to zero",
				weatherRecord.poa, weatherRecord.year, weatherRecord.month, weatherRecord.day, weatherRecord.hour), SSC_WARNING, (float)idx);
			weatherRecord.poa = 0;
		}
		int month_idx = weatherRecord.month - 1;
		bool albedoError = false;
		if (useWeatherFileAlbedo && (!std::isfinite(weatherRecord.alb) || weatherRecord.alb < 0 || weatherRecord.alb > 1)) {
			albedoError = true;
		}
		else if ((month_idx >= 0 && month_idx < 12) && (userSpecifiedMonthlyAlbedo[month_idx] < 0 || userSpecifiedMonthlyAlbedo[month_idx] > 1)) {
			albedoError = true;
		}
		if (albedoError) {
			throw compute_module::exec_error(cmName,
				util::format("Error retrieving albedo value: Invalid month in weather file or invalid albedo value in weather file"));
		}
	}
	weatherDataProvider->rewind();
}

void Irradiance_IO::AllocateOutputs(compute_module* cm)
{
	p_weatherFileGHI = cm->allocate("gh", numberOfWeatherFileRecords);
	p_weatherFileDNI = cm->allocate("dn", numberOfWeatherFileRecords);
	p_weatherFileDHI = cm->allocate("df", numberOfWeatherFileRecords);
	p_sunPositionTime = cm->allocate("sunpos_hour", numberOfWeatherFileRecords);
	p_weatherFileWindSpeed = cm->allocate("wspd", numberOfWeatherFileRecords);
	p_weatherFileAmbientTemp = cm->allocate("tdry", numberOfWeatherFileRecords);
	p_weatherFileAlbedo = cm->allocate("alb", numberOfWeatherFileRecords);
	p_weatherFileSnowDepth = cm->allocate("snowdepth", numberOfWeatherFileRecords);

	// If using input POA, must have POA for every subarray or assume POA applies to each subarray
	for (size_t subarray = 0; subarray != numberOfSubarrays; subarray++) {
		std::string wfpoa = "wfpoa" + util::to_string(static_cast<int>(subarray + 1));
		p_weatherFilePOA.push_back(cm->allocate(wfpoa, numberOfWeatherFileRecords));
	}

	//set up the calculated components of irradiance such that they aren't reported if they aren't assigned
	//three possible calculated irradiance: gh, df, dn
	if (radiationMode == DN_DF) p_IrradianceCalculated[0] = cm->allocate("gh_calc", numberOfWeatherFileRecords); //don't calculate global for POA models
	if (radiationMode == DN_GH || radiationMode == POA_R || radiationMode == POA_P) p_IrradianceCalculated[1] = cm->allocate("df_calc", numberOfWeatherFileRecords);
	if (radiationMode == GH_DF || radiationMode == POA_R || radiationMode == POA_P) p_IrradianceCalculated[2] = cm->allocate("dn_calc", numberOfWeatherFileRecords);

	//output arrays for solar position calculations- same for all four subarrays
	p_sunZenithAngle = cm->allocate("sol_zen", numberOfWeatherFileRecords);
	p_sunAltitudeAngle = cm->allocate("sol_alt", numberOfWeatherFileRecords);
	p_sunAzimuthAngle = cm->allocate("sol_azi", numberOfWeatherFileRecords);
	p_absoluteAirmass = cm->allocate("airmass", numberOfWeatherFileRecords);
	p_sunUpOverHorizon = cm->allocate("sunup", numberOfWeatherFileRecords);
}

void Irradiance_IO::AssignOutputs(compute_module* cm)
{
	cm->assign("ts_shift_hours", var_data((ssc_number_t)tsShiftHours));
}

Subarray_IO::Subarray_IO(compute_module* cm, std::string cmName, size_t subarrayNumber)
{
	prefix = "subarray" + util::to_string(static_cast<int>(subarrayNumber)) + "_";

	enable = true;
	if (subarrayNumber > 1)
		enable = cm->as_boolean(prefix + "enable");

	if (enable)
	{
		nStrings = cm->as_integer(prefix + "nstrings");
		nModulesPerString = cm->as_integer("modules_per_string");
		tiltDegrees = fabs(cm->as_double(prefix + "tilt"));
		azimuthDegrees = cm->as_double(prefix + "azimuth");
		trackMode = cm->as_integer(prefix + "track_mode");
		trackerRotationLimitDegrees = cm->as_double(prefix + "rotlim");
		tiltEqualLatitude = cm->as_boolean(prefix + "tilt_eq_lat");
		groundCoverageRatio = cm->as_double(prefix + "gcr");
		monthlyTiltDegrees = cm->as_vector_double(prefix + "monthly_tilt");
		backtrackingEnabled = cm->as_boolean(prefix + "backtrack");
		moduleAspectRatio = cm->as_double("module_aspect_ratio");
		usePOAFromWeatherFile = false;

		// losses
		rearIrradianceLossPercent = cm->as_double(prefix + "rear_irradiance_loss") / 100;
		dcOptimizerLossPercent = cm->as_double("dcoptimizer_loss")/100;
		mismatchLossPercent = cm->as_double(prefix + "mismatch_loss") / 100;
		diodesLossPercent = cm->as_double(prefix + "diodeconn_loss") / 100;
		dcWiringLossPercent = cm->as_double(prefix + "dcwiring_loss") / 100;
		trackingLossPercent = cm->as_double(prefix + "tracking_loss") / 100;
		nameplateLossPercent = cm->as_double(prefix + "nameplate_loss") / 100;

		dcLossTotalPercent = 1 - (
			(1 - dcOptimizerLossPercent) * 
			(1 - mismatchLossPercent) * 
			(1 - diodesLossPercent) * 
			(1 - dcWiringLossPercent) * 
			(1 - trackingLossPercent) * 
			(1 - nameplateLossPercent));

		if (groundCoverageRatio < 0.01)
			throw compute_module::exec_error(cmName, "array ground coverage ratio must obey 0.01 < gcr");


		monthlySoiling = cm->as_vector_double(prefix + "soiling");
		if (monthlySoiling.size() != 12) throw compute_module::exec_error(cmName, "soiling loss array must have 12 values: subarray " + util::to_string((int)(subarrayNumber)));

		//convert from % to derate
		for (size_t m = 0; m < monthlySoiling.size(); m++)
			monthlySoiling[m] = 1.0 - monthlySoiling[m] / 100.0;

		// Shading database
		enableSelfShadingOutputs = false;
		if (!shadeCalculator.setup(cm, prefix)) {
			throw compute_module::exec_error(cmName, prefix + "_shading: " + shadeCalculator.get_error());
		}
		
		shadeMode = cm->as_integer(prefix + "shade_mode");
		
		selfShadingInputs.mod_orient = cm->as_integer(prefix + "mod_orient");
		selfShadingInputs.nmody = cm->as_integer(prefix + "nmody");
		selfShadingInputs.nmodx = cm->as_integer(prefix + "nmodx");
		selfShadingInputs.nstrx = selfShadingInputs.nmodx / nModulesPerString;
		poa.nonlinearDCShadingDerate = 1;

		if (trackMode == FIXED_TILT || trackMode == SEASONAL_TILT || (trackMode == SINGLE_AXIS && !backtrackingEnabled))
		{
			if (shadeMode != NO_SHADING)
			{
				// Calculate the number of rows given the module dimensions of each row.
				selfShadingInputs.nrows = (int)floor((nStrings * nModulesPerString) / (selfShadingInputs.nmodx * selfShadingInputs.nmody));

				//if nrows comes out to be zero, this will cause a divide by zero error. Give an error in this case.
				if (selfShadingInputs.nrows == 0 && nStrings != 0)
					throw compute_module::exec_error(cmName, "Self shading: Number of rows calculated for subarray " + util::to_string(int(subarrayNumber)) + " was zero. Please check your inputs.");

				// Otherwise, if self-shading configuration does not have equal number of modules as specified on system design page for that subarray,
				// compute dc derate using the self-shading configuration and apply it to the whole subarray. Give warning.
				if ((size_t)(selfShadingInputs.nmodx * selfShadingInputs.nmody * selfShadingInputs.nrows) != (size_t)(nStrings * nModulesPerString))
					cm->log(util::format("The product of number of modules along side and bottom for subarray %d is not equal to the number of modules in the subarray. Check your inputs for self shading.",
						int(subarrayNumber)), SSC_WARNING);

				// assume aspect ratio of 1.7 (see variable "aspect_ratio" below to change this assumption)
				selfShadingInputs.str_orient = 1;	//assume horizontal wiring
				selfShadingInputs.mask_angle_calc_method = 0; //assume worst case mask angle calc method
				selfShadingInputs.ndiode = 3;	//assume 3 diodes- maybe update this assumption based on number of cells in the module?
			}
		}

		// Snow model
		enableShowModel = cm->as_boolean("en_snow_model");
		if (enableShowModel)
		{
			if (trackMode == SEASONAL_TILT)
				throw compute_module::exec_error(cmName, "Time-series tilt input may not be used with the snow model at this time: subarray " + util::to_string((int)(subarrayNumber)));
			if (snowModel.setup(selfShadingInputs.nmody, (float)tiltDegrees)) {
				if (!snowModel.good) {
					cm->log(snowModel.msg, SSC_ERROR);
				}
			}
		}

		if (nStrings < 0) {
			throw compute_module::exec_error(cmName, "invalid string allocation between subarrays.  all subarrays must have zero or positive number of strings.");
		}

		// Initialize module model
		std::unique_ptr<Module_IO> tmp(new Module_IO(cm, cmName, 1- dcLossTotalPercent));
		Module = std::move(tmp);

	}
}
void Subarray_IO::AssignOutputs(compute_module* cm)
{
	//assign output dc loss
	double tmp = dcLossTotalPercent * 100;
	cm->assign(prefix + "dcloss", var_data((ssc_number_t)tmp));

	Module->AssignOutputs(cm);
}


PVSystem_IO::PVSystem_IO(compute_module* cm, std::string cmName, Simulation_IO * SimulationIO, Irradiance_IO * IrradianceIO, std::vector<Subarray_IO*> SubarraysAll, Inverter_IO * InverterIO)
{
	Irradiance = IrradianceIO;
	Simulation = SimulationIO;
	Subarrays = SubarraysAll;
	Inverter = InverterIO;

	numberOfSubarrays = Subarrays.size();
	stringsInParallel = 0;
	for (size_t s = 0; s < numberOfSubarrays; s++) {
		stringsInParallel += static_cast<int>(Subarrays[s]->nStrings);
	}
	AllocateOutputs(cm);

	modulesPerString = cm->as_integer("modules_per_string");
	numberOfInverters = cm->as_integer("inverter_count");
	ratedACOutput = Inverter->ratedACOutput * numberOfInverters;
	acDerate = 1 - cm->as_double("acwiring_loss") / 100;	
	acLossPercent = (1 - acDerate) * 100;
	transmissionDerate = 1 - cm->as_double("transmission_loss") / 100;
	transmissionLossPercent = (1 - transmissionDerate) * 100;

	enableDCLifetimeLosses = cm->as_boolean("en_dc_lifetime_losses");
	enableACLifetimeLosses = cm->as_boolean("en_ac_lifetime_losses");

	// The shared inverter of the PV array and a tightly-coupled DC connected battery
	std::unique_ptr<SharedInverter> tmpSharedInverter(new SharedInverter(Inverter->inverterType, numberOfInverters, &Inverter->sandiaInverter, &Inverter->partloadInverter));
	m_sharedInverter = std::move(tmpSharedInverter);
	
	// Register shared inverter with inverter_IO
	InverterIO->setupSharedInverter(cm, m_sharedInverter.get());

	// PV Degradation
	if (Simulation->useLifetimeOutput)
	{
		std::vector<double> dc_degrad = cm->as_vector_double("dc_degradation");

		// degradation assumed to start at year 2
		p_dcDegradationFactor[0] = 1.0;
		p_dcDegradationFactor[1] = 1.0;

		if (dc_degrad.size() == 1)
		{
			for (size_t i = 1; i < Simulation->numberOfYears ; i++)
				p_dcDegradationFactor[i + 1] = (ssc_number_t)pow((1.0 - dc_degrad[0] / 100.0), i);
		}
		else if (dc_degrad.size() > 0)
		{
			for (size_t i = 1; i < Simulation->numberOfYears && i < dc_degrad.size(); i++)
				p_dcDegradationFactor[i + 1] = (ssc_number_t)(1.0 - dc_degrad[i] / 100.0);
		}

		//read in optional DC and AC lifetime daily losses, error check length of arrays
		if (enableDCLifetimeLosses)
		{

			std::vector<double> dc_lifetime_losses = cm->as_vector_double("dc_lifetime_losses");
			if (dc_lifetime_losses.size() != Simulation->numberOfYears * 365)
				throw compute_module::exec_error(cmName, "Length of the lifetime daily DC losses array must be equal to the analysis period * 365");
		}
		if (enableACLifetimeLosses)
		{
			std::vector<double> ac_lifetime_losses = cm->as_vector_double("ac_lifetime_losses");
			if (ac_lifetime_losses.size() != Simulation->numberOfYears * 365)
				throw compute_module::exec_error(cmName, "Length of the lifetime daily AC losses array must be equal to the analysis period * 365");
		}
	}
	// Transformer losses
	transformerLoadLossFraction = cm->as_number("transformer_load_loss") * (ssc_number_t)(util::percent_to_fraction);  
	transformerNoLoadLossFraction = cm->as_number("transformer_no_load_loss") *(ssc_number_t)(util::percent_to_fraction);

	// MPPT
	voltageMpptLow1Module = cm->as_double("mppt_low_inverter") / modulesPerString;
	voltageMpptHi1Module = cm->as_double("mppt_hi_inverter") / modulesPerString;
	clipMpptWindow = false;

	if (voltageMpptLow1Module > 0 && voltageMpptHi1Module > voltageMpptLow1Module)
	{
		int moduleType = Subarrays[0]->Module->moduleType;
		if (moduleType == 1     // cec with database
			|| moduleType == 2   // cec with user specs
			|| moduleType == 4) // iec61853 single diode
		{
			clipMpptWindow = true;
		}
		else
		{
			cm->log("The simple efficiency and Sandia module models do not allow limiting module voltage to the MPPT tracking range of the inverter.", SSC_NOTICE);
		}
	}
	else
	{
		cm->log("Inverter MPPT voltage tracking window not defined - modules always operate at MPPT.", SSC_NOTICE);
	}
}

void PVSystem_IO::AllocateOutputs(compute_module* cm)
{
	size_t numberOfWeatherFileRecords = Irradiance->numberOfWeatherFileRecords;
	size_t numberOfLifetimeRecords = Simulation->numberOfSteps;
	size_t numberOfYears = Simulation->numberOfYears;

	for (size_t subarray = 0; subarray < Subarrays.size(); subarray++)
	{
		if (Subarrays[subarray]->enable)
		{
			std::string prefix = Subarrays[subarray]->prefix;
			p_angleOfIncidence.push_back(cm->allocate(prefix + "aoi", numberOfWeatherFileRecords));
			p_angleOfIncidenceModifier.push_back(cm->allocate(prefix + "aoi_modifier", numberOfWeatherFileRecords));
			p_surfaceTilt.push_back(cm->allocate(prefix + "surf_tilt", numberOfWeatherFileRecords));
			p_surfaceAzimuth.push_back(cm->allocate(prefix + "surf_azi", numberOfWeatherFileRecords));
			p_axisRotation.push_back(cm->allocate(prefix + "axisrot", numberOfWeatherFileRecords));
			p_idealRotation.push_back(cm->allocate(prefix + "idealrot", numberOfWeatherFileRecords));
			p_poaNominalFront.push_back(cm->allocate(prefix + "poa_nom", numberOfWeatherFileRecords));
			p_poaShadedFront.push_back(cm->allocate(prefix + "poa_shaded", numberOfWeatherFileRecords));
			p_poaShadedSoiledFront.push_back(cm->allocate(prefix + "poa_shaded_soiled", numberOfWeatherFileRecords));
			p_poaBeamFront.push_back(cm->allocate(prefix + "poa_eff_beam", numberOfWeatherFileRecords));
			p_poaDiffuseFront.push_back(cm->allocate(prefix + "poa_eff_diff", numberOfWeatherFileRecords));
			p_poaTotal.push_back(cm->allocate(prefix + "poa_eff", numberOfWeatherFileRecords));
			p_poaRear.push_back(cm->allocate(prefix + "poa_rear", numberOfWeatherFileRecords));
			p_poaFront.push_back(cm->allocate(prefix + "poa_front", numberOfWeatherFileRecords));
			p_derateSoiling.push_back(cm->allocate(prefix + "soiling_derate", numberOfWeatherFileRecords));
			p_beamShadingFactor.push_back(cm->allocate(prefix + "beam_shading_factor", numberOfWeatherFileRecords));
			p_temperatureCell.push_back(cm->allocate(prefix + "celltemp", numberOfWeatherFileRecords));
			p_moduleEfficiency.push_back(cm->allocate(prefix + "modeff", numberOfWeatherFileRecords));
			p_dcVoltage.push_back(cm->allocate(prefix + "dc_voltage", numberOfWeatherFileRecords));
			p_voltageOpenCircuit.push_back(cm->allocate(prefix + "voc", numberOfWeatherFileRecords));
			p_currentShortCircuit.push_back(cm->allocate(prefix + "isc", numberOfWeatherFileRecords));
			p_dcPowerGross.push_back(cm->allocate(prefix + "dc_gross", numberOfWeatherFileRecords));
			p_derateLinear.push_back(cm->allocate(prefix + "linear_derate", numberOfWeatherFileRecords));
			p_derateSelfShading.push_back(cm->allocate(prefix + "ss_derate", numberOfWeatherFileRecords));
			p_derateSelfShadingDiffuse.push_back(cm->allocate(prefix + "ss_diffuse_derate", numberOfWeatherFileRecords));
			p_derateSelfShadingReflected.push_back(cm->allocate(prefix + "ss_reflected_derate", numberOfWeatherFileRecords));

			if (Subarrays[subarray]->enableShowModel) {
				p_snowLoss.push_back(cm->allocate(prefix + "snow_loss", numberOfWeatherFileRecords));
				p_snowCoverage.push_back(cm->allocate(prefix + "snow_coverage", numberOfWeatherFileRecords));
			}

			if (Subarrays[subarray]->enableSelfShadingOutputs)
			{
				// ShadeDB validation
				p_shadeDB_GPOA.push_back(cm->allocate("shadedb_" + prefix + "gpoa", numberOfWeatherFileRecords));
				p_shadeDB_DPOA.push_back(cm->allocate("shadedb_" + prefix + "dpoa", numberOfWeatherFileRecords));
				p_shadeDB_temperatureCell.push_back(cm->allocate("shadedb_" + prefix + "pv_cell_temp", numberOfWeatherFileRecords));
				p_shadeDB_modulesPerString.push_back(cm->allocate("shadedb_" + prefix + "mods_per_str", numberOfWeatherFileRecords));
				p_shadeDB_voltageMaxPowerSTC.push_back(cm->allocate("shadedb_" + prefix + "str_vmp_stc", numberOfWeatherFileRecords));
				p_shadeDB_voltageMPPTLow.push_back(cm->allocate("shadedb_" + prefix + "mppt_lo", numberOfWeatherFileRecords));
				p_shadeDB_voltageMPPTHigh.push_back(cm->allocate("shadedb_" + prefix + "mppt_hi", numberOfWeatherFileRecords));
			}
			p_shadeDBShadeFraction.push_back(cm->allocate("shadedb_" + prefix + "shade_frac", numberOfWeatherFileRecords));
		}
	}
	p_transformerNoLoadLoss = cm->allocate("xfmr_nll_ts", numberOfWeatherFileRecords);
	p_transformerLoadLoss = cm->allocate("xfmr_ll_ts", numberOfWeatherFileRecords);
	p_transformerLoss = cm->allocate("xfmr_loss_ts", numberOfWeatherFileRecords);

	p_poaFrontNominalTotal = cm->allocate("poa_nom", numberOfWeatherFileRecords);
	p_poaFrontBeamNominalTotal = cm->allocate("poa_beam_nom", numberOfWeatherFileRecords);
	p_poaFrontBeamTotal = cm->allocate("poa_beam_eff", numberOfWeatherFileRecords);
	p_poaFrontShadedTotal = cm->allocate("poa_shaded", numberOfWeatherFileRecords);
	p_poaFrontShadedSoiledTotal = cm->allocate("poa_shaded_soiled", numberOfWeatherFileRecords);
	p_poaFrontTotal = cm->allocate("poa_front", numberOfWeatherFileRecords);
	p_poaRearTotal = cm->allocate("poa_rear", numberOfWeatherFileRecords);
	p_poaTotalAllSubarrays = cm->allocate("poa_eff", numberOfWeatherFileRecords);

	p_snowLossTotal = cm->allocate("dc_snow_loss", numberOfWeatherFileRecords);

	p_inverterDCVoltage = cm->allocate("inverter_dc_voltage", numberOfLifetimeRecords);
	p_inverterEfficiency = cm->allocate("inv_eff", numberOfWeatherFileRecords);
	p_inverterClipLoss = cm->allocate("inv_cliploss", numberOfWeatherFileRecords);
	p_inverterMPPTLoss = cm->allocate("dc_invmppt_loss", numberOfWeatherFileRecords);

	p_inverterPowerConsumptionLoss = cm->allocate("inv_psoloss", numberOfWeatherFileRecords);
	p_inverterNightTimeLoss = cm->allocate("inv_pntloss", numberOfWeatherFileRecords);
	p_inverterThermalLoss = cm->allocate("inv_tdcloss", numberOfWeatherFileRecords);

	p_acWiringLoss = cm->allocate("ac_wiring_loss", numberOfWeatherFileRecords);
	p_transmissionLoss = cm->allocate("ac_transmission_loss", numberOfWeatherFileRecords);
	p_systemDCPower = cm->allocate("dc_net", numberOfLifetimeRecords);
	p_systemACPower = cm->allocate("gen", numberOfLifetimeRecords);

	if (Simulation->useLifetimeOutput)
	{
		p_dcDegradationFactor = cm->allocate("dc_degrade_factor", numberOfYears + 1);
	}

}
void PVSystem_IO::AssignOutputs(compute_module* cm)
{
	cm->assign("ac_loss", var_data((ssc_number_t)acLossPercent));
}

Module_IO::Module_IO(compute_module* cm, std::string cmName, double dcLoss)
{
	moduleType = cm->as_integer("module_model");

	enableMismatchVoltageCalc = cm->as_boolean("enable_mismatch_vmax_calc");
	if (enableMismatchVoltageCalc && 
		moduleType != MODULE_CEC_DATABASE && 
		moduleType != MODULE_CEC_USER_INPUT && 
		moduleType != MODULE_IEC61853) {
		throw compute_module::exec_error(cmName, "String level subarray mismatch can only be calculated using a single-diode based module model.");
	}

	simpleEfficiencyForceNoPOA = false;
	mountingSpecificCellTemperatureForceNoPOA = false;
	selfShadingFillFactor = 0;
	isConcentratingPV = false;
	isBifacial = false;

	if (moduleType == MODULE_SIMPLE_EFFICIENCY)
	{
		simpleEfficiencyModel.VmpNominal = cm->as_double("spe_vmp");
		simpleEfficiencyModel.VocNominal = cm->as_double("spe_voc");
		simpleEfficiencyModel.Area = cm->as_double("spe_area");
		referenceArea = simpleEfficiencyModel.Area;
		isBifacial = cm->as_boolean("spe_is_bifacial");
		bifaciality = cm->as_double("spe_bifaciality");
		bifacialTransmissionFactor = cm->as_double("spe_bifacial_transmission_factor");
		groundClearanceHeight = cm->as_double("spe_bifacial_ground_clearance_height");
		for (int i = 0; i<5; i++)
		{
			simpleEfficiencyModel.Rad[i] = cm->as_double(util::format("spe_rad%d", i));
			simpleEfficiencyModel.Eff[i] = 0.01*cm->as_double(util::format("spe_eff%d", i));
			if (i > 0 && simpleEfficiencyModel.Rad[i] <= simpleEfficiencyModel.Rad[i - 1])
				throw compute_module::exec_error(cmName, "simpleEfficiencyModel model radiation levels must increase monotonically");
		}

		simpleEfficiencyModel.Gamma = cm->as_double("spe_temp_coeff");
		simpleEfficiencyModel.Reference = cm->as_integer("spe_reference");

		switch (cm->as_integer("spe_module_structure"))
		{
		case 0: //glass/cell/polymer sheet - open rack
			sandiaCellTemp.a = -3.56;
			sandiaCellTemp.b = -0.0750;
			sandiaCellTemp.DT0 = 3;
			break;
		case 1: //glass/cell/glass - open rack
			sandiaCellTemp.a = -3.47;
			sandiaCellTemp.b = -0.0594;
			sandiaCellTemp.DT0 = 3;
			break;
		case 2: //polymer/thin film/steel - open rack
			sandiaCellTemp.a = -3.58;
			sandiaCellTemp.b = -0.113;
			sandiaCellTemp.DT0 = 3;
			break;
		case 3: //Insulated back (building-integrated PV)
			sandiaCellTemp.a = -2.81;
			sandiaCellTemp.b = -0.0455;
			sandiaCellTemp.DT0 = 0;
			break;
		case 4: //close roof mount
			sandiaCellTemp.a = -2.98;
			sandiaCellTemp.b = -0.0471;
			sandiaCellTemp.DT0 = 1;
			break;
		case 5: //user defined
			sandiaCellTemp.a = cm->as_double("spe_a");
			sandiaCellTemp.b = cm->as_double("spe_b");
			sandiaCellTemp.DT0 = cm->as_double("spe_dT");
			break;
		default:
			throw compute_module::exec_error(cmName, "invalid simpleEfficiencyModel module structure and mounting");
		}

		simpleEfficiencyModel.fd = cm->as_double("spe_fd");
		sandiaCellTemp.fd = simpleEfficiencyModel.fd;

		if (simpleEfficiencyModel.fd < 1.0)
			simpleEfficiencyForceNoPOA = true;

		cellTempModel = &sandiaCellTemp;
		moduleModel = &simpleEfficiencyModel;
		moduleWattsSTC = simpleEfficiencyModel.WattsStc();
		voltageMaxPower = simpleEfficiencyModel.VmpNominal;
	}
	else if (moduleType == MODULE_CEC_DATABASE)
	{
		isBifacial = cm->as_boolean("cec_is_bifacial");
		bifaciality = cm->as_double("cec_bifaciality");
		bifacialTransmissionFactor = cm->as_double("cec_bifacial_transmission_factor");
		groundClearanceHeight = cm->as_double("cec_bifacial_ground_clearance_height");
		cecModel.Area = cm->as_double("cec_area");
		referenceArea = cecModel.Area;
		cecModel.Vmp = cm->as_double("cec_v_mp_ref");
		cecModel.Imp = cm->as_double("cec_i_mp_ref");
		cecModel.Voc = cm->as_double("cec_v_oc_ref");
		cecModel.Isc = cm->as_double("cec_i_sc_ref");
		cecModel.alpha_isc = cm->as_double("cec_alpha_sc");
		cecModel.beta_voc = cm->as_double("cec_beta_oc");
		cecModel.a = cm->as_double("cec_a_ref");
		cecModel.Il = cm->as_double("cec_i_l_ref");
		cecModel.Io = cm->as_double("cec_i_o_ref");
		cecModel.Rs = cm->as_double("cec_r_s");
		cecModel.Rsh = cm->as_double("cec_r_sh_ref");
		cecModel.Adj = cm->as_double("cec_adjust");

		selfShadingFillFactor = cecModel.Vmp * cecModel.Imp / cecModel.Voc / cecModel.Isc;
		voltageMaxPower = cecModel.Vmp;

		if (cm->as_integer("cec_temp_corr_mode") == 0)
		{
			nominalOperatingCellTemp.Tnoct = cm->as_double("cec_t_noct");
			int standoff = cm->as_integer("cec_standoff");
			nominalOperatingCellTemp.standoff_tnoct_adj = 0;
			switch (standoff)
			{
				case 2: nominalOperatingCellTemp.standoff_tnoct_adj = 2; break; // between 2.5 and 3.5 inches
				case 3: nominalOperatingCellTemp.standoff_tnoct_adj = 6; break; // between 1.5 and 2.5 inches
				case 4: nominalOperatingCellTemp.standoff_tnoct_adj = 11; break; // between 0.5 and 1.5 inches
				case 5: nominalOperatingCellTemp.standoff_tnoct_adj = 18; break; // less than 0.5 inches											
																// note: all others, standoff_tnoct_adj = 0;
			}

			int height = cm->as_integer("cec_height");
			nominalOperatingCellTemp.ffv_wind = 0.51;
			if (height == 1)
				nominalOperatingCellTemp.ffv_wind = 0.61;

			cellTempModel = &nominalOperatingCellTemp;
		}
		else
		{
			/*	int MC; // Mounting configuration (1=rack,2=flush,3=integrated,4=gap)
			int HTD; // Heat transfer dimension (1=Module,2=Array)
			int MSO; // Mounting structure orientation (1=does not impede flow beneath, 2=vertical supports, 3=horizontal supports)
			int Nrows, Ncols; // number of modules in rows and columns, when using array heat transfer dimensions
			double Length; // module length, along horizontal dimension, (m)
			double Width; // module width, along vertical dimension, (m)
			double Wgap;  // gap width spacing (m)
			double TbackInteg; */

			mountingSpecificCellTemp.DcDerate = dcLoss;  
			mountingSpecificCellTemp.MC = cm->as_integer("cec_mounting_config") + 1;
			mountingSpecificCellTemp.HTD = cm->as_integer("cec_heat_transfer") + 1;
			mountingSpecificCellTemp.MSO = cm->as_integer("cec_mounting_orientation") + 1;
			mountingSpecificCellTemp.Wgap = cm->as_double("cec_gap_spacing");
			mountingSpecificCellTemp.Length = cm->as_double("cec_module_length");
			mountingSpecificCellTemp.Width = cm->as_double("cec_module_width");
			mountingSpecificCellTemp.Nrows = cm->as_integer("cec_array_rows");
			mountingSpecificCellTemp.Ncols = cm->as_integer("cec_array_cols");
			mountingSpecificCellTemp.TbackInteg = cm->as_double("cec_backside_temp");

			cellTempModel = &mountingSpecificCellTemp;
			mountingSpecificCellTemperatureForceNoPOA = true;
		}

		moduleModel = &cecModel;
		moduleWattsSTC = cecModel.Vmp * cecModel.Imp;
	}
	else if (moduleType == MODULE_CEC_USER_INPUT)
	{
		isBifacial = cm->as_boolean("6par_is_bifacial");
		bifaciality = cm->as_double("6par_bifaciality");
		bifacialTransmissionFactor = cm->as_double("6par_bifacial_transmission_factor");
		groundClearanceHeight = cm->as_double("6par_bifacial_ground_clearance_height");

		int tech_id = module6par::monoSi;
		int type = cm->as_integer("6par_celltech"); // "monoSi,multiSi,CdTe,CIS,CIGS,Amorphous"
		switch (type)
		{
		case 0: tech_id = module6par::monoSi; break;
		case 1: tech_id = module6par::multiSi; break;
		case 2: tech_id = module6par::CdTe; break;
		case 3: tech_id = module6par::CIS; break;
		case 4: tech_id = module6par::CIGS; break;
		case 5: tech_id = module6par::Amorphous; break;
		}

		double Vmp = cm->as_double("6par_vmp");
		double Imp = cm->as_double("6par_imp");
		double Voc = cm->as_double("6par_voc");
		double Isc = cm->as_double("6par_isc");
		double alpha = cm->as_double("6par_aisc");
		double beta = cm->as_double("6par_bvoc");
		double gamma = cm->as_double("6par_gpmp");
		int nser = cm->as_integer("6par_nser");

		module6par m(tech_id, Vmp, Imp, Voc, Isc, beta, alpha, gamma, nser, 298.15);
		int err = m.solve_with_sanity_and_heuristics<double>(300, 1e-7);

		if (err != 0)
			throw compute_module::exec_error(cmName, "CEC 6 parameter model:  Could not solve for normalized coefficients.  Please check your inputs.");

		cecModel.Area = cm->as_double("6par_area");
		referenceArea = cecModel.Area;
		cecModel.Vmp = Vmp;
		cecModel.Imp = Imp;
		cecModel.Voc = Voc;
		cecModel.Isc = Isc;
		cecModel.alpha_isc = alpha;
		cecModel.beta_voc = beta;
		cecModel.a = m.a;
		cecModel.Il = m.Il;
		cecModel.Io = m.Io;
		cecModel.Rs = m.Rs;
		cecModel.Rsh = m.Rsh;
		cecModel.Adj = m.Adj;

		selfShadingFillFactor = cecModel.Vmp * cecModel.Imp / cecModel.Voc / cecModel.Isc;
		voltageMaxPower = cecModel.Vmp;

		setupNOCTModel(cm, "6par");
		cellTempModel = &nominalOperatingCellTemp;
		moduleModel = &cecModel;
		moduleWattsSTC = cecModel.Vmp * cecModel.Imp;
	}
	else if (moduleType == MODULE_SANDIA)
	{
		sandiaModel.A0 = cm->as_double("snl_a0");
		sandiaModel.A1 = cm->as_double("snl_a1");
		sandiaModel.A2 = cm->as_double("snl_a2");
		sandiaModel.A3 = cm->as_double("snl_a3");
		sandiaModel.A4 = cm->as_double("snl_a4");
		sandiaModel.aImp = cm->as_double("snl_aimp");
		sandiaModel.aIsc = cm->as_double("snl_aisc");
		sandiaModel.Area = cm->as_double("snl_area");
		referenceArea = sandiaModel.Area;
		sandiaModel.B0 = cm->as_double("snl_b0");
		sandiaModel.B1 = cm->as_double("snl_b1");
		sandiaModel.B2 = cm->as_double("snl_b2");
		sandiaModel.B3 = cm->as_double("snl_b3");
		sandiaModel.B4 = cm->as_double("snl_b4");
		sandiaModel.B5 = cm->as_double("snl_b5");
		sandiaModel.BVmp0 = cm->as_double("snl_bvmpo");
		sandiaModel.BVoc0 = cm->as_double("snl_bvoco");
		sandiaModel.C0 = cm->as_double("snl_c0");
		sandiaModel.C1 = cm->as_double("snl_c1");
		sandiaModel.C2 = cm->as_double("snl_c2");
		sandiaModel.C3 = cm->as_double("snl_c3");
		sandiaModel.C4 = cm->as_double("snl_c4");
		sandiaModel.C5 = cm->as_double("snl_c5");
		sandiaModel.C6 = cm->as_double("snl_c6");
		sandiaModel.C7 = cm->as_double("snl_c7");
		sandiaModel.fd = cm->as_double("snl_fd");
		sandiaModel.Imp0 = cm->as_double("snl_impo");
		sandiaModel.Isc0 = cm->as_double("snl_isco");
		sandiaModel.Ix0 = cm->as_double("snl_ixo");
		sandiaModel.Ixx0 = cm->as_double("snl_ixxo");
		sandiaModel.mBVmp = cm->as_double("snl_mbvmp");
		sandiaModel.mBVoc = cm->as_double("snl_mbvoc");
		sandiaModel.DiodeFactor = cm->as_double("snl_n");
		sandiaModel.NcellSer = cm->as_integer("snl_series_cells");
		sandiaModel.Vmp0 = cm->as_double("snl_vmpo");
		sandiaModel.Voc0 = cm->as_double("snl_voco");

		selfShadingFillFactor = sandiaModel.Vmp0 * sandiaModel.Imp0 / sandiaModel.Voc0 / sandiaModel.Isc0;
		voltageMaxPower = sandiaModel.Vmp0;

		if (sandiaModel.fd == 0) {
			isConcentratingPV = true;
		}

		// by default, use database values
		double A = cm->as_double("snl_a");
		double B = cm->as_double("snl_b");
		double DT = cm->as_double("snl_dtc");

		switch (cm->as_integer("snl_module_structure"))
		{
		case 1: //glass/cell/polymer sheet - open rack
			A = -3.56;
			B = -0.0750;
			DT = 3;
			break;
		case 2: //glass/cell/glass - open rack
			A = -3.47;
			B = -0.0594;
			DT = 3;
			break;
		case 3: //polymer/thin film/steel - open rack
			A = -3.58;
			B = -0.113;
			DT = 3;
			break;
		case 4: //Insulated back (building-integrated PV)
			A = -2.81;
			B = -0.0455;
			DT = 0;
			break;
		case 5: //close roof mount
			A = -2.98;
			B = -0.0471;
			DT = 1;
			break;
		case 6: //user defined
			A = cm->as_double("snl_ref_a");
			B = cm->as_double("snl_ref_b");
			DT = cm->as_double("snl_ref_dT");
			break;

		default:
			break;
		}

		sandiaCellTemp.a = A;
		sandiaCellTemp.b = B;
		sandiaCellTemp.DT0 = DT;
		sandiaCellTemp.fd = sandiaModel.fd;

		cellTempModel = &sandiaCellTemp;
		moduleModel = &sandiaModel;
		moduleWattsSTC = sandiaModel.Vmp0 * sandiaModel.Imp0;
	}
	else if (moduleType == MODULE_IEC61853 )
	{
		// IEC 61853 model
		elevenParamSingleDiodeModel.NcellSer = cm->as_integer("sd11par_nser");
		elevenParamSingleDiodeModel.Area = cm->as_double("sd11par_area");
		elevenParamSingleDiodeModel.AMA[0] = cm->as_double("sd11par_AMa0");
		elevenParamSingleDiodeModel.AMA[1] = cm->as_double("sd11par_AMa1");
		elevenParamSingleDiodeModel.AMA[2] = cm->as_double("sd11par_AMa2");
		elevenParamSingleDiodeModel.AMA[3] = cm->as_double("sd11par_AMa3");
		elevenParamSingleDiodeModel.AMA[4] = cm->as_double("sd11par_AMa4");
		elevenParamSingleDiodeModel.GlassAR = cm->as_boolean("sd11par_glass");

		setupNOCTModel(cm, "sd11par");

		elevenParamSingleDiodeModel.Vmp0 = cm->as_double("sd11par_Vmp0");
		elevenParamSingleDiodeModel.Imp0 = cm->as_double("sd11par_Imp0");
		elevenParamSingleDiodeModel.Voc0 = cm->as_double("sd11par_Voc0");
		elevenParamSingleDiodeModel.Isc0 = cm->as_double("sd11par_Isc0");
		elevenParamSingleDiodeModel.alphaIsc = cm->as_double("sd11par_alphaIsc");
		elevenParamSingleDiodeModel.n = cm->as_double("sd11par_n");
		elevenParamSingleDiodeModel.Il = cm->as_double("sd11par_Il");
		elevenParamSingleDiodeModel.Io = cm->as_double("sd11par_Io");
		elevenParamSingleDiodeModel.Egref = cm->as_double("sd11par_Egref");
		elevenParamSingleDiodeModel.D1 = cm->as_double("sd11par_d1");
		elevenParamSingleDiodeModel.D2 = cm->as_double("sd11par_d2");
		elevenParamSingleDiodeModel.D3 = cm->as_double("sd11par_d3");
		elevenParamSingleDiodeModel.C1 = cm->as_double("sd11par_c1");
		elevenParamSingleDiodeModel.C2 = cm->as_double("sd11par_c2");
		elevenParamSingleDiodeModel.C3 = cm->as_double("sd11par_c3");

		cellTempModel = &nominalOperatingCellTemp;
		moduleModel = &elevenParamSingleDiodeModel;
		moduleWattsSTC = elevenParamSingleDiodeModel.Vmp0 * elevenParamSingleDiodeModel.Imp0;
		referenceArea = elevenParamSingleDiodeModel.Area;
		selfShadingFillFactor = elevenParamSingleDiodeModel.Vmp0 * elevenParamSingleDiodeModel.Imp0 / elevenParamSingleDiodeModel.Voc0 / elevenParamSingleDiodeModel.Isc0;
		voltageMaxPower = elevenParamSingleDiodeModel.Vmp0;
	}
	else
		throw compute_module::exec_error(cmName, "invalid pv module model type");
}
void Module_IO::setupNOCTModel(compute_module* cm, const std::string &prefix)
{
	nominalOperatingCellTemp.Tnoct = cm->as_double(prefix + "_tnoct");
	nominalOperatingCellTemp.ffv_wind = 0.51; // less than 22ft high (1 story)
	if (cm->as_integer(prefix + "_mounting") == 1) nominalOperatingCellTemp.ffv_wind = 0.61;  // greater than 22ft high (2 story)

	int standoff = cm->as_integer(prefix + "_standoff"); // bipv,3.5in,2.5-3.5in,1.5-2.5in,0.5-1.5in,ground/rack
	nominalOperatingCellTemp.standoff_tnoct_adj = 0;
	switch (standoff)
	{
	case 2: nominalOperatingCellTemp.standoff_tnoct_adj = 2; break; // between 2.5 and 3.5 inches
	case 3: nominalOperatingCellTemp.standoff_tnoct_adj = 6; break; // between 1.5 and 2.5 inches
	case 4: nominalOperatingCellTemp.standoff_tnoct_adj = 11; break; // between 0.5 and 1.5 inches
	case 5: nominalOperatingCellTemp.standoff_tnoct_adj = 18; break; // less than 0.5 inches
	}
}

void Module_IO::AssignOutputs(compute_module* cm)
{
	cm->assign("6par_a", var_data((ssc_number_t)cecModel.a));
	cm->assign("6par_Io", var_data((ssc_number_t)cecModel.Io));
	cm->assign("6par_Il", var_data((ssc_number_t)cecModel.Il));
	cm->assign("6par_Rs", var_data((ssc_number_t)cecModel.Rs));
	cm->assign("6par_Rsh", var_data((ssc_number_t)cecModel.Rsh));
	cm->assign("6par_Adj", var_data((ssc_number_t)cecModel.Adj));
}

Inverter_IO::Inverter_IO(compute_module *cm, std::string cmName)
{
	inverterType =  cm->as_integer("inverter_model");
	
	if (inverterType == 0) // cec database
	{
		sandiaInverter.Paco = cm->as_double("inv_snl_paco");
		sandiaInverter.Pdco = cm->as_double("inv_snl_pdco");
		sandiaInverter.Vdco = cm->as_double("inv_snl_vdco");
		sandiaInverter.Pso = cm->as_double("inv_snl_pso");
		sandiaInverter.Pntare = cm->as_double("inv_snl_pnt");
		sandiaInverter.C0 = cm->as_double("inv_snl_c0");
		sandiaInverter.C1 = cm->as_double("inv_snl_c1");
		sandiaInverter.C2 = cm->as_double("inv_snl_c2");
		sandiaInverter.C3 = cm->as_double("inv_snl_c3");
		ratedACOutput = sandiaInverter.Paco;
	}
	else if (inverterType == 1) // datasheet data
	{
		double eff_ds = cm->as_double("inv_ds_eff") / 100.0;
		sandiaInverter.Paco = cm->as_double("inv_ds_paco");
		if (eff_ds != 0)
			sandiaInverter.Pdco = sandiaInverter.Paco / eff_ds;
		else
			sandiaInverter.Pdco = 0;
		sandiaInverter.Vdco = cm->as_double("inv_ds_vdco");
		sandiaInverter.Pso = cm->as_double("inv_ds_pso");
		sandiaInverter.Pntare = cm->as_double("inv_ds_pnt");
		sandiaInverter.C0 = 0;
		sandiaInverter.C1 = 0;
		sandiaInverter.C2 = 0;
		sandiaInverter.C3 = 0;
		ratedACOutput = sandiaInverter.Paco;
	}
	else if (inverterType == 2) // partload curve
	{
		partloadInverter.Vdco = cm->as_double("inv_pd_vdco");
		partloadInverter.Paco = cm->as_double("inv_pd_paco");
		partloadInverter.Pdco = cm->as_double("inv_pd_pdco");
		partloadInverter.Pntare = cm->as_double("inv_pd_pnt");

		std::vector<double> pl_pd = cm->as_vector_double("inv_pd_partload");
		std::vector<double> eff_pd = cm->as_vector_double("inv_pd_efficiency");

		partloadInverter.Partload = pl_pd;
		partloadInverter.Efficiency = eff_pd;
		ratedACOutput = partloadInverter.Paco;
	}
	else if (inverterType == 3) // coefficient generator
	{
		sandiaInverter.Paco = cm->as_double("inv_cec_cg_paco");
		sandiaInverter.Pdco = cm->as_double("inv_cec_cg_pdco");
		sandiaInverter.Vdco = cm->as_double("inv_cec_cg_vdco");
		sandiaInverter.Pso = cm->as_double("inv_cec_cg_psco");
		sandiaInverter.Pntare = cm->as_double("inv_cec_cg_pnt");
		sandiaInverter.C0 = cm->as_double("inv_cec_cg_c0");
		sandiaInverter.C1 = cm->as_double("inv_cec_cg_c1");
		sandiaInverter.C2 = cm->as_double("inv_cec_cg_c2");
		sandiaInverter.C3 = cm->as_double("inv_cec_cg_c3");
		ratedACOutput = sandiaInverter.Paco;
	}
	else
	{
		throw compute_module::exec_error("pvsamv1", "invalid inverter model type");
	}
}

void Inverter_IO::setupSharedInverter(compute_module* cm, SharedInverter * a_sharedInverter)
{
	sharedInverter = a_sharedInverter;

	// Inverter thermal derate curves
	util::matrix_t<double> inv_tdc;
	if (inverterType == 0) // cec database
	{
		inv_tdc = cm->as_matrix("inv_tdc_cec_db");
	}
	else if (inverterType == 1) // datasheet data
	{
		inv_tdc = cm->as_matrix("inv_tdc_ds");
	}
	else if (inverterType == 2) // partload curve
	{
		inv_tdc = cm->as_matrix("inv_tdc_plc");
	}
	else if (inverterType == 3) // coefficient generator
	{
		inv_tdc = cm->as_matrix("inv_tdc_cec_cg");
	}

	// Parse into std::vector form
	std::vector<std::vector<double>> thermalDerateCurves;
	for (size_t r = 0; r < inv_tdc.nrows(); r++) {
		std::vector<double> row;
		for (size_t c = 0; c < inv_tdc.row(r).ncells(); c++) {
			row.push_back(inv_tdc.at(r, c));
		}
		thermalDerateCurves.push_back(row);
	}
	int err = sharedInverter->setTempDerateCurves(thermalDerateCurves);
	if ( err > 1) {
		//throw compute_module::exec_error("pvsamv1", "Inverter temperature derate curve " + util::to_string((int)( -err - 1)) + " is invalid.");
	}
}
