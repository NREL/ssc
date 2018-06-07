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

	// Gather subarrays which are enabled
	nSubarrays = 1;
	std::unique_ptr<Subarray_IO> subarray1(new Subarray_IO(cm, cmName, 1));
	m_SubarraysIO.push_back(std::move(subarray1));

	for (size_t subarray = 2; subarray <= 4; subarray++)
	{
		std::unique_ptr<Subarray_IO> ptr3(new Subarray_IO(cm, cmName, subarray));
		if (ptr3->enable) {
			m_SubarraysIO.push_back(std::move(ptr3));
			nSubarrays++;
		}
	}

	// Aggregate Subarray outputs in different structure
	std::unique_ptr<PVSystem_IO> pvSystem(new PVSystem_IO(cm, m_SimulationIO.get(), m_IrradianceIO.get(), getSubarrays()));
	m_PVSystemIO = std::move(pvSystem);


	m_computeModule = cm;
	m_computeModuleName = cmName;
}

Irradiance_IO * PVIOManager::getIrradianceIO() const { return m_IrradianceIO.get(); }
compute_module * PVIOManager::getComputeModule() const { return m_computeModule; }
Subarray_IO * PVIOManager::getSubarrayIO(size_t subarrayNumber) const { return m_SubarraysIO[subarrayNumber].get(); }

std::vector<Subarray_IO *> PVIOManager::getSubarrays() const
{
	std::vector<Subarray_IO*> subarrays;
	for (size_t subarray = 0; subarray < m_SubarraysIO.size(); subarray++) {
		subarrays.push_back(m_SubarraysIO[subarray].get());
	}
	return subarrays;
}

PVSystem_IO * PVIOManager::getPVSystemIO() const { return m_PVSystemIO.get(); }

Simulation_IO * PVIOManager::getSimulationIO() const { return m_SimulationIO.get(); }


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
	}
	else if (cm->is_assigned("solar_resource_data")) {
		weatherDataProvider = std::unique_ptr<weather_data_provider>(new weatherdata(cm->lookup("solar_resource_data")));
	}
	else {
		throw compute_module::exec_error(cmName, "No weather data supplied");
	}

	// Check weather file
	if (weatherDataProvider->has_message()) cm->log(weatherDataProvider->message(), SSC_WARNING);
	weatherfile *weatherFile = dynamic_cast<weatherfile*>(weatherDataProvider.get());
	if (!weatherFile->ok()) throw compute_module::exec_error(cmName, weatherFile->message());
	if (weatherFile->has_message()) cm->log(weatherFile->message(), SSC_WARNING);

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

	if (stepsPerHour < 1 || stepsPerHour > 60 || stepsPerHour * 8760 != numberOfWeatherFileRecords)
		throw compute_module::exec_error(cmName, util::format("invalid number of data records (%zu): must be an integer multiple of 8760", numberOfWeatherFileRecords));

	useWeatherFileAlbedo = cm->as_boolean("use_wf_albedo");
	userSpecifiedMonthlyAlbedo = cm->as_vector_double("albedo");

	AllocateOutputs(cm);
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
		dcLoss = (1 - cm->as_double(prefix + "mismatch_loss") / 100) *
			(1 - cm->as_double(prefix + "diodeconn_loss") / 100) *
			(1 - cm->as_double(prefix + "dcwiring_loss") / 100) *
			(1 - cm->as_double(prefix + "tracking_loss") / 100) *
			(1 - cm->as_double(prefix + "nameplate_loss") / 100) *
			(1 - cm->as_double("dcoptimizer_loss") / 100);

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
		selfShadingInputs.mod_orient = selfShadingInputs.nmodx / nModulesPerString;
		poa.nonlinearDCShadingDerate = 1;

		if (trackMode == FIXED_TILT || trackMode == SEASONAL_TILT || (trackMode == SINGLE_AXIS && backtrackingEnabled))
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
				if ((selfShadingInputs.nmodx * selfShadingInputs.nmody * selfShadingInputs.nrows) != (nStrings * nModulesPerString))
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

	}
}

PVSystem_IO::PVSystem_IO(compute_module* cm, Simulation_IO * SimulationIO, Irradiance_IO * IrradianceIO, std::vector<Subarray_IO*> SubarraysAll)
{
	Irradiance = IrradianceIO;
	Simulation = SimulationIO;
	Subarrays = SubarraysAll;
	numberOfSubarrays = Subarrays.size();

	AllocateOutputs(cm);
}

void PVSystem_IO::AllocateOutputs(compute_module* cm)
{
	size_t numberOfWeatherFileRecords = Irradiance->numberOfWeatherFileRecords;
	size_t numberOfLifetimeRecords = Simulation->numberOfSteps;

	for (size_t subarray = 0; subarray < Subarrays.size(); subarray++)
	{
		if (Subarrays[subarray]->enable)
		{
			std::string prefix = Subarrays[subarray]->prefix;
			p_angleOfIncidence.push_back(cm->allocate(prefix + "aoi", numberOfWeatherFileRecords));
			p_surfaceTilt.push_back(cm->allocate(prefix + "surf_tilt", numberOfWeatherFileRecords));
			p_surfaceAzimuth.push_back(cm->allocate(prefix + "surf_azi", numberOfWeatherFileRecords));
			p_axisRotation.push_back(cm->allocate(prefix + "axisrot", numberOfWeatherFileRecords));
			p_idealRotation.push_back(cm->allocate(prefix + "idealrot", numberOfWeatherFileRecords));
			p_poaNominal.push_back(cm->allocate(prefix + "poa_nom", numberOfWeatherFileRecords));
			p_poaShaded.push_back(cm->allocate(prefix + "poa_shaded", numberOfWeatherFileRecords));
			p_poaBeamFront.push_back(cm->allocate(prefix + "poa_eff_beam", numberOfWeatherFileRecords));
			p_poaDiffuseFront.push_back(cm->allocate(prefix + "poa_eff_diff", numberOfWeatherFileRecords));
			p_poaTotal.push_back(cm->allocate(prefix + "poa_eff", numberOfWeatherFileRecords));
			p_poaRear.push_back(cm->allocate(prefix + "poa_rear", numberOfWeatherFileRecords));
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

	p_poaNominalTotal = cm->allocate("poa_nom", numberOfWeatherFileRecords);
	p_poaBeamFrontNominalTotal = cm->allocate("poa_beam_nom", numberOfWeatherFileRecords);
	p_poaShadedTotal = cm->allocate("poa_shaded", numberOfWeatherFileRecords);
	p_poaTotalAllSubarrays = cm->allocate("poa_eff", numberOfWeatherFileRecords);
	p_poaBeamFrontTotal = cm->allocate("poa_beam_eff", numberOfWeatherFileRecords);

	p_snowLossTotal = cm->allocate("dc_snow_loss", numberOfWeatherFileRecords);

	p_inverterDCVoltage = cm->allocate("inverter_dc_voltage", numberOfLifetimeRecords);
	p_inverterEfficiency = cm->allocate("inv_eff", numberOfWeatherFileRecords);
	p_inverterClipLoss = cm->allocate("inv_cliploss", numberOfWeatherFileRecords);
	p_inverterMPPTLoss = cm->allocate("dc_invmppt_loss", numberOfWeatherFileRecords);

	p_inverterPowerConsumptionLoss = cm->allocate("inv_psoloss", numberOfWeatherFileRecords);
	p_inverterNightTimeLoss = cm->allocate("inv_pntloss", numberOfWeatherFileRecords);
	p_acWiringLoss = cm->allocate("ac_wiring_loss", numberOfWeatherFileRecords);

	p_systemDCPower = cm->allocate("dc_net", numberOfLifetimeRecords);
	p_systemACPower = cm->allocate("gen", numberOfLifetimeRecords);
}


void Subarray_IO::AssignOutputs(compute_module* cm)
{
	//assign output dc loss
	double tmp = (1 - dcLoss) * 100;
	cm->assign(prefix + "dcloss", var_data((ssc_number_t)tmp));
}


