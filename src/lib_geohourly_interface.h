#ifndef __lib_geohourly_interface
#define __lib_geohourly_interface

#include <math.h>
#include <assert.h>

#include <vector>
#include <string>

#include "lib_geohourly.h"
#include "lib_powerblock.h"

class CGeothermalInterface
{
private:
	CGeoHourlyOutputs oGeoOutputs;

public:
	CGeothermalInterface( );
	~CGeothermalInterface( );

private:
	std::string m_strErrMsg;

public:
	//========================== Model Choice (GETEM, Power Block, ??) =======================================
	void SetModelChoice(int choice) { oGeoOutputs.SetModelChoice(choice); }
	int GetModelChoice( ) { return oGeoOutputs.GetModelChoice(); }
	unsigned int GetTimeStepsInAnalysis( ) { return oGeoOutputs.analysisTimeSteps(); }


	//========================== plant configuration ========================================================

	// energy calculation basis
	void SetDesiredPlantSalesKW(double kw) { oGeoOutputs.cb = POWER_SALES; oGeoOutputs.SetDesiredSalesCapacityKW(kw); }						// KW, default 15000 (15MW), desired sales from plant (plant capacity is larger do account for pumping and other losses)
	void SetDesiredNumberOfWells(double numberOfWells) { oGeoOutputs.cb = NUMBER_OF_WELLS; oGeoOutputs.SetNumberOfWells(numberOfWells); }		// number of wells, default is that GETEM will calculate this based on desired output of plant
	int GetCalculationBasis( ) { return (int)oGeoOutputs.cb; }																		// 1 = power output (# wells is calculated), 2 = number of wells (power output is calculated)
	double GetPlantOutput( ) { return oGeoOutputs.PlantOutputKW(); }
	double GetPlantGrossOutput( ) { return oGeoOutputs.PlantSizeKW(); }
	double GetPlantNetOutput( ) { return oGeoOutputs.PowerSalesKW(); }

	// conversion plant type
	void SetConversionType(int plantType){oGeoOutputs.cst = (conversionTypes)plantType; }								// 1=Binary, 2=Flash
	int GetConversionType( ){ return (int)oGeoOutputs.cst; }

	// Power block inputs
	void SetWeatherFileName( const char * fileName) { oGeoOutputs.SetWeatherFileName(fileName); }
	void SetPowerBlockParameters(const SPowerBlockParameters& pbp) {oGeoOutputs.SetPowerBlockParameters(pbp); }
	SPowerBlockParameters GetPowerBlockParameters( ) { return oGeoOutputs.GetPowerBlockParameters(); }
	void SetPowerBlockInputs(const SPowerBlockInputs& pbi) {oGeoOutputs.SetPowerBlockInputs(pbi); }
	void SetTOUPeriodArray(int * tou) { oGeoOutputs.SetTOUPeriodArray(tou); }

	// plant brine effectiveness (this value is calculated for flash plants)
	void SetPlantEfficiency(double percent) { oGeoOutputs.SetPlantEfficiency(percent); }
	double GetPlantEfficiency( ) { return oGeoOutputs.GetPlantEfficiency(); }
	double GetPBE( ) { return oGeoOutputs.GetPlantBrineEffectiveness(); }												// plant brine effectiveness (plant efficiency)

	// set the flash plant sub-type
	void SetFlashSubType(int ft){oGeoOutputs.ft = (flashTypes)ft; }														// 1=single flash, no constraint, 2=single flash, with constraint, 
	int GetFlashSubType( ){ return (int)oGeoOutputs.ft; }																// 3=dual flash, no constraint, 4=dual flash, with constraint

		// to show user intermediate values
		double ShowNumberOfWells( ) { return oGeoOutputs.GetNumberOfProductionWells(); }
		double ShowPlantBrineEffectiveness( ) { return oGeoOutputs.GetPlantBrineEffectiveness(); }					// same as above, for calculated value in interface
		double ShowMaxPlantBrineEffectiveness( ) { return oGeoOutputs.GetMaxBinaryBrineEffectiveness(); }				// max theoretical plant brine effectiveness including exit temp constraint
		double ShowGrossOutput( ) { return oGeoOutputs.GetGrossPlantOutputMW(); } // mega watts
		double ShowNetOutput( ) { return oGeoOutputs.GetNetPlantOutputMW(); } // mega watts


	//========================== resource characterization ==================================================
	void SetPotentialResourceMW(double megaWatts) { oGeoOutputs.mdPotentialResourceMW = megaWatts; }						// MW, default = 200 MW, determines how many times reservoir can be replaced
	double GetPotentialResourceMW( ) { return oGeoOutputs.mdPotentialResourceMW; }

	double GetAmbientTemperatureC( ) { return oGeoOutputs.GetAmbientTemperatureC(); }									// degrees C, default 10 (not sure why there's two ambient temperatures)

	void SetResourceType(int resourceType) {if(resourceType == 1) oGeoOutputs.rt = HYDROTHERMAL; else oGeoOutputs.rt = EGS; }	// 1=Hydrothermal (default), !1=EGS 
	int GetResourceType( ){ return (int)oGeoOutputs.rt; }

	void SetResourceTemperatureCelcius(double degreesCelcius){oGeoOutputs.SetResourceTemperatureC(degreesCelcius); }		// degrees C, default 200
	double GetResourceTemperatureCelcius( ){ return oGeoOutputs.GetResourceTemperatureC(); }
	double ShowResourceTemperatureCelcius( ){ return oGeoOutputs.GetResourceTemperatureC(); }

	void SetResourceDepthMeters(double meters){oGeoOutputs.SetResourceDepthM(meters); }									// meters, default 2000
	double GetResourceDepthMeters( ){ return oGeoOutputs.GetResourceDepthM(); }
	double ShowResourceDepthMeters( ){ return oGeoOutputs.GetResourceDepthM(); }

	void SetTemperatureGradient(double tempGradient){oGeoOutputs.SetTemperatureGradient(tempGradient);}					// Degrees C per kM, default 35
	double GetTemperatureGradient( ){ return oGeoOutputs.GetTemperatureGradient(); }
	double ShowTemperatureGradient( ){ return oGeoOutputs.GetTemperatureGradient(); }

	void SetEGSAmbientTemperatureC(double degreesCelcius) { oGeoOutputs.SetAmbientEGSTemperatureC(degreesCelcius); }		// degrees C, default 15
	double GetEGSAmbientTemperatureC( ) { return oGeoOutputs.GetAmbientEGSTemperatureC(); }

	void SetPlantDesignTemperatureCelcius(double degreesCelcius){oGeoOutputs.SetTemperaturePlantDesignC(degreesCelcius); }	// degrees C, default 225, only used for EGS
	double GetPlantDesignTemperatureCelcius( ){ return oGeoOutputs.GetTemperaturePlantDesignC(); }						//  (Hydrothermal analysis only uses resource temp)
	double GetPlantDesignTemperatureMinCelcius( ) { return oGeoOutputs.GetTemperaturePlantDesignMinC(); }


	//========================== temperature decline inputs =================================================
	void SetTemperatureDeclineMethod(int tdm){ oGeoOutputs.tdm = (tempDeclineMethod)tdm; }							// 1=user enter rate, 2=GETEM calculate it (choice 2 is valid 
	int GetTemperatureDeclineMethod( ){ return oGeoOutputs.tdm; }													// for EGS resourceTypes only)

	void SetTemperatureDeclineRate(double percentPerYear) { oGeoOutputs.mdTemperatureDeclineRate = percentPerYear; }	// '% per year, 3% is default
	double GetTemperatureDeclineRate( ) { return oGeoOutputs.mdTemperatureDeclineRate; }

	void SetMaxTempDeclineC(double degC) { oGeoOutputs.mdMaxTempDeclineC = degC; }									// degrees C, default = 30
	double GetMaxTempDeclineC( ) { return oGeoOutputs.mdMaxTempDeclineC; }

	//========================== flash technology inputs ====================================================
	//In an hourly model, these inputs should come from the weather file, changing hourly
	void SetWetBulbTemperatureC(double degreesCelcius) { oGeoOutputs.mdTemperatureWetBulbC = degreesCelcius; }							// degrees celcius - used in Flash brine effectiveness
	double GetWetBulbTemperatureC( ) { return oGeoOutputs.mdTemperatureWetBulbC; }													// calcs and to calculate ambient temperature for flash

	void SetPressureAmbientPSI(double psi) { oGeoOutputs.mdPressureAmbientPSI = psi; }													// psi, default=14.7, mostly for use in calculating flash brine effectiveness, but also pump work
	double GetPressureAmbientPSI( ) { return oGeoOutputs.mdPressureAmbientPSI; }


	//========================== pumping parameters =========================================================
	void SetProductionFlowRateKgPerS(double flowRateKgPerSecond) { oGeoOutputs.SetProductionFlowRateKgPerS(flowRateKgPerSecond); }		// Kg per second per well, 70 is default
	double GetProductionFlowRateKgPerS( ) { return oGeoOutputs.GetProductionFlowRateKgPerS(); }

	void SetPumpEfficiency(double pumpEfficiencyPercent) { oGeoOutputs.SetGFPumpEfficiency(pumpEfficiencyPercent); }						// default=0.6 or 60%
	double GetPumpEfficiency( ) { return oGeoOutputs.GetGFPumpEfficiency(); }

	void SetPressureChangeAcrossSurfaceEquipment(double psi) { oGeoOutputs.mdPressureChangeAcrossSurfaceEquipmentPSI = psi; }			// default 25 psi
	double GetPressureChangeAcrossSurfaceEquipment( ) { return oGeoOutputs.mdPressureChangeAcrossSurfaceEquipmentPSI; }

	void SetExcessPressureBar(double bar) { oGeoOutputs.mdExcessPressureBar = bar; }														// default 3.5 bar, [2B.Resource&Well Input].D205
	double GetExcessPressureBar( ) { return oGeoOutputs.mdExcessPressureBar; }

	void SetExcessPressurePSI(double psi) { oGeoOutputs.mdExcessPressureBar = PsiToBar(psi); }											// same as above, in PSI
	double GetExcessPressurePSI( ) { return BarToPsi(oGeoOutputs.mdExcessPressureBar); }

	void SetProductionWellDiameter(double inches) { oGeoOutputs.SetDiameterProductionWellInches(inches); }								// default 10 inches
	double GetProductionWellDiameter( ) { return oGeoOutputs.GetDiameterProductionWellInches(); }

	void SetPumpCasingDiameter(double inches) { oGeoOutputs.SetDiameterPumpCasingInches(inches); }										// default 9.925 inches
	double GetPumpCasingDiameter( ) { return oGeoOutputs.GetDiameterPumpCasingInches(); }

	void SetInjectionWellDiameter(double inches) { oGeoOutputs.SetDiameterInjectionWellInches(inches); }									// default 10 inches
	double SetInjectionWellDiameter( ) { return oGeoOutputs.GetDiameterInjectionWellInches(); }

		// to show user intermediate values
		double ShowPumpDepthFeet( ) { return oGeoOutputs.GetPumpDepthFt(); } // ft
		double ShowPumpWorkMW( ) { return oGeoOutputs.GetPumpWorkKW()/1000; } // mega watts

	void SetCalculatePumpWork(bool bCalcPumpWork){ oGeoOutputs.mbCalculatePumpWork = bCalcPumpWork; }									// true (default) = getem calculates pump work
	bool GetCalculatePumpWork( ){ return oGeoOutputs.mbCalculatePumpWork; }

	void SetUserSpecifiedPumpWorkMW(double dUserSpecifiedPumpWorkMW) { oGeoOutputs.mdUserSpecifiedPumpWorkKW = dUserSpecifiedPumpWorkMW*1000; }	// 
	double GetUserSpecifiedPumpWorkMW( ) { return oGeoOutputs.mdUserSpecifiedPumpWorkKW/1000; }



	//========================== reservoir parameters =======================================================
	// rock properties
	void SetRockThermalConductivity(double conductivity) { oGeoOutputs.SetEGSThermalConductivity(conductivity); }		// default 259,200 Joules per m-day-C, [2B.Resource&Well Input].D240
	double GetRockThermalConductivity( ) { return oGeoOutputs.GetEGSThermalConductivity(); }

	void SetRockSpecificHeat(double specificHeat) { oGeoOutputs.SetEGSSpecificHeatConstant(specificHeat); }				// default 950 Joules per kg-C, [2B.Resource&Well Input].D241
	double GetRockSpecificHeat( ) { return oGeoOutputs.GetEGSSpecificHeatConstant(); }

	void SetRockDensity(double density) { oGeoOutputs.SetEGSRockDensity(density); }										// default 2600 kg per cubic meter, [2B.Resource&Well Input].D242
	double GetRockDensity( ) { return oGeoOutputs.GetEGSRockDensity(); }

	// how to calculate the change in pressure across the reservoir
	void SetPressureCalculationMethod(int pcMethod){ oGeoOutputs.pc = (reservoirPressureChangeCalculation)pcMethod; }		// 1=user enter pressure change, 2=SAM calculates it using simple fracture flow (for EGS resourceTypes only
	int GetPressureCalculationMethod( ){ return (int)oGeoOutputs.pc; }													// 3=SAM calculates it using k*A (permeability x area)

	void SetReservoirPressureChange(double psiHPer1000lb) { oGeoOutputs.mdReservoirDeltaPressure = psiHPer1000lb; }			// default 0.35 psi-h per 1000lb, [2B.Resource&Well Input].D171
	double GetReservoirPressureChange( ) { return oGeoOutputs.mdReservoirDeltaPressure ; }

	// reservoir characteristics for pump power calculations
	void SetReservoirWidthM(double meters) { oGeoOutputs.SetReservoirWidthM(meters); }										// default = 500 meters, [2B.Resource&Well Input].F181
	double GetReservoirWidthM( ) { return oGeoOutputs.GetReservoirWidthM(); }

	void SetReservoirHeightM(double meters) { oGeoOutputs.SetReservoirHeightM(meters); }										// default = 100 meters, [2B.Resource&Well Input].F180
	double GetReservoirHeightM( ) { return oGeoOutputs.GetReservoirHeightM(); }

	void SetReservoirPermeability(double darcyUnits) { oGeoOutputs.SetReservoirPermeability(darcyUnits); }					// default = 0.05 darcy units, [2B.Resource&Well Input].D179
	double GetReservoirPermeability( ) { return oGeoOutputs.GetReservoirPermeability(); }

	// there are two inputs for this in GETEM - why?
	// used to calculate pressure change across reservoir
	void SetWellDistance2(double meters) { oGeoOutputs.mdDistanceBetweenProductionInjectionWellsM = meters; }	// default 1500 m [2B.Resource&Well Input].F185
	double GetWellDistance2( ) { return oGeoOutputs.mdDistanceBetweenProductionInjectionWellsM; }

	// this well distance is used for EGS calculations, to get the 'effective length' of the fractures
	void SetWellDistance(double meters) { oGeoOutputs.mdEGSDistanceBetweenProductionInjectionWellsM = meters; }	// default 1000 m [2B.Resource&Well Input].F76
	double GetWellDistance( ) { return oGeoOutputs.mdEGSDistanceBetweenProductionInjectionWellsM; }

	// subsurface rock and fracture parameters for EGS
	void SetSubsurfaceWaterLossRate(double percent) { oGeoOutputs.SetWaterLossPercent(percent); }						// default 2%
	double GetSubsurfaceWaterLossRate( ) { return oGeoOutputs.GetWaterLossPercent(); }

	void SetSubsurfaceFractureAperature(double meters) { oGeoOutputs.SetEGSFractureAperature(meters); }					// default 0.0004 m
	double GetSubsurfaceFractureAperature( ) { return oGeoOutputs.GetEGSFractureAperature(); }

	void SetNumberOfFractures(double numberOfFractures) { oGeoOutputs.SetEGSNumberOfFractures(numberOfFractures); }		// default 6
	double GetNumberOfFractures( ) { return oGeoOutputs.GetEGSNumberOfFractures(); }

	void SetSubsurfaceFractureWidth(double meters) { oGeoOutputs.SetEGSFractureWidthM(meters); }							// default 175 m
	double GetSubsurfaceFractureWidth( ) { return oGeoOutputs.GetEGSFractureWidthM(); }

	void SetSubsurfaceFractureAngle(double degrees) { oGeoOutputs.mdEGSFractureAngle = degrees; }						// default 15 degrees
	double GetSubsurfaceFractureAngle( ) { return oGeoOutputs.mdEGSFractureAngle; }

		// to show user intermediate values
		double ShowPressureChange( ) { return oGeoOutputs.GetPressureChangeAcrossReservoir(); } // psi
		double ShowAverageReservoirTemperature( ) { return FarenheitToCelcius(oGeoOutputs.GetAverageReservoirTemperatureUsedF()); }	// degrees C
		double ShowBottomHolePressure( ) { return oGeoOutputs.GetBottomHolePressure(); } // psi


	//========================== Number of wells to be drilled ==============================================
	void SetConfirmationWellSuccessRate(double percent) { oGeoOutputs.mdConfirmationWellSuccessRate = percent; }				// default = 50%
	double GetConfirmationWellSuccessRate( ) { return oGeoOutputs.mdConfirmationWellSuccessRate; }

	void SetRatioInjectionToProduction(double ratio) { oGeoOutputs.mdRatioInjectionToProduction = ratio; }					// default = 0.50, half as many injection wells
	double GetRatioInjectionToProduction( ) { return oGeoOutputs.mdRatioInjectionToProduction; }

		// to show user intermediate values
		double ShowConfirmationWellsUsedAsProduction( ) { return oGeoOutputs.numberConfirmationWellsUsedForProduction(); }
		double ShowProductionWellsDrilled( ) { return oGeoOutputs.numberProductionWellsDrilled(); }
		double ShowInjectionWellsDrilled( ) { return oGeoOutputs.numberInjectionWellsDrilled(); }




	//========================== drilling and associated costs ==============================================
	// exloration
	void SetExplorationWellCostMultiplier(double newValue) { oGeoOutputs.mdExplorationWellCostMultiplier = newValue; }					// default = 0.5, [2A.Scenario Input].D57
	double GetExplorationWellCostMultiplier( ) { return oGeoOutputs.mdExplorationWellCostMultiplier; }

	void SetExplorationWells(double newValue) { oGeoOutputs.mdExplorationWells = newValue; }												// default = 2, [2A.Scenario Input].D50
	double GetExplorationWells( ) { return oGeoOutputs.mdExplorationWells; }

	void SetNonWellExplorationCosts(double dollars) { oGeoOutputs.mdNonWellExplorationCosts = dollars; }									// default = $750,000; [2A.Scenario Input].D60
	double GetNonWellExplorationCosts( ) { return oGeoOutputs.mdNonWellExplorationCosts; }

		double ShowExplorationCostPerWell( ) { return oGeoOutputs.costPerExplorationWell(); }
		double ShowExplorationDrillingCost( ) { return oGeoOutputs.costExplorationVariable(); }
		double ShowExplorationCost( ) { return oGeoOutputs.capitalCostsExploration(); }


	// confirmation
	void SetConfirmationWellCostMultiplier(double newValue) { oGeoOutputs.mdConfirmationWellCostMultiplier = newValue; }					// default = 1.2, [2A.Scenario Input].D58
	double GetConfirmationWellCostMultiplier( ) { return oGeoOutputs.mdConfirmationWellCostMultiplier; }

	void SetConfirmationWells(double newValue) { oGeoOutputs.mdConfirmationWells = newValue; }											// default = 2, [2A.Scenario Input].D55
	double GetConfirmationWells( ) { return oGeoOutputs.mdConfirmationWells; }

	void SetNonWellConfirmationCosts(double dollars) { oGeoOutputs.mdNonWellConfirmationCosts = dollars; }								// default = $250,000; [2A.Scenario Input].D61
	double GetNonWellConfirmationCosts( ) { return oGeoOutputs.mdNonWellConfirmationCosts; }

		double ShowConfirmationCostPerWell( ) { return oGeoOutputs.costPerConfirmationWell(); }
		double ShowConfirmationDrillingCost( ) { return oGeoOutputs.costConfirmationVariable(); }
		double ShowConfirmationCost( ) { return oGeoOutputs.capitalCostsConfirmation(); }


	// production
	void SetCostCurveForProductionWell(int cc){oGeoOutputs.SetProductionWellCostCurve((wellCostCurveChoices)cc); }						// default=med, 1=Low, 2=Med, 3=High
	int GetCostCurveForProductionWell( ){ return (int)oGeoOutputs.GetProductionWellCostCurve(); }
		double ShowProductionCostPerWell( ) { return oGeoOutputs.costPerProductionWell(); }
		double ShowProductionDrillingCost( ) { return oGeoOutputs.costProductionWells(); }


	// injection
	void SetCostCurveForInjectionWell(int cc){oGeoOutputs.SetInjectionWellCostCurve((wellCostCurveChoices)cc); }							// default=med, 1=Low, 2=Med, 3=High
	int GetCostCurveForInjectionWell( ){ return (int)oGeoOutputs.GetInjectionWellCostCurve(); }
		double ShowInjectionCostPerWell( ) { return oGeoOutputs.costPerInjectionWell(); }
		double ShowInjectionDrillingCost( ) { return oGeoOutputs.costInjectionWells(); }			


	// production and injection (total well field)
	void SetWellFieldNonDrillingCosts(double dollars) { oGeoOutputs.mdWellFieldNonDrillingCosts = dollars; }								// default = $250,000
	double GetWellFieldNonDrillingCosts( ) { return oGeoOutputs.mdWellFieldNonDrillingCosts; }
		double ShowTotalWellsDrilled( ) { return oGeoOutputs.totalWellsDrilled(); }
		double ShowWellFieldDrillingCost( ) { return oGeoOutputs.costWellFieldDrilling(); }
		double ShowTotalWellFieldCost( ) { return oGeoOutputs.capitalCostsMainWells(); }


	// other surface equipment
	void SetSurfaceEquipmentCostPerWell(double dollarsPerWell) { oGeoOutputs.mdSurfaceEquipmentCostPerWell = dollarsPerWell; }			// default = $125,000/well
	double GetSurfaceEquipmentCostPerWell( ) { return oGeoOutputs.mdSurfaceEquipmentCostPerWell; }
		double ShowSurfaceEquipmentInstallationCosts( ) { return oGeoOutputs.costSurfaceEquipment(); }

	// well stimulation
	void SetWellStimulationCostPerWell(double dollarsPerWell) { oGeoOutputs.mdWellStimulationCostPerWell = dollarsPerWell; }				// default = $1,000,000/well
	double GetWellStimulationCostPerWell( ) { return oGeoOutputs.mdWellStimulationCostPerWell; }
		double ShowStimulationCosts( ) { return oGeoOutputs.costStimulation(); }


	//========================== plant capital costs ========================================================
	void SetCapitalCostPerKW(double capCostDollarsPerKW) { oGeoOutputs.SetPlantCapitalCostPerKW(capCostDollarsPerKW); }					// default = $1800/kW, forces the model to use this cost/kW, eventually model will calculate this based on resource temp, depth, etc.
	double GetCapitalCostPerKW( ) { return oGeoOutputs.GetPlantCapitalCostPerKW(); }
		double ShowPlantCapitalCost( ) { return oGeoOutputs.capitalCostsPlant(); }


	//========================== pump cost inputs ===========================================================
	//void SetProductionPumpCostCasing(double dollarsPerFoot) { oGeoOutputs.mdProductionPumpCostCasing = dollarsPerFoot; }				// default = $45/ft				([7A.GF Pumps].G144)
	//double GetProductionPumpCostCasing( ) { return oGeoOutputs.mdProductionPumpCostCasing; }

	//void SetProductionPumpCostInstall(double dollarsPerFoot) { oGeoOutputs.mdProductionPumpCostInstall = dollarsPerFoot; }				// default = $5/ft				([7A.GF Pumps].G145)
	//double GetProductionPumpCostInstall( ) { return oGeoOutputs.mdProductionPumpCostInstall; }

	//void SetProductionPumpCostOther(double dollars) { oGeoOutputs.mdProductionPumpCostOther = dollars; }								// default = $10,000
	//double GetProductionPumpCostOther( ) { return oGeoOutputs.mdProductionPumpCostOther; }

	void SetPumpCostPerFoot(double dollarsPerFoot) { oGeoOutputs.mdPumpCostPerFoot = dollarsPerFoot; }									// default = $50/ft, combination of casing cost and installation cost
	double GetPumpCostPerFoot( ) { return oGeoOutputs.mdPumpCostPerFoot; }

	void SetPumpCostPerHP(double dollars) { oGeoOutputs.mdPumpCostPerHP = dollars; }														// default = $12,000 per horsepower
	double GetPumpCostPerHP( ) { return oGeoOutputs.mdPumpCostPerHP; }
		//double ShowCasingCostPerPump( ) { return oGeoOutputs.pumpCasingCost(); }
		double ShowInstallationCostPerPump( ) { return oGeoOutputs.pumpInstallationCost(); }
		double ShowPumpHorsePower( ) { return oGeoOutputs.GetPumpSizeHP(); }
		double ShowPumpOnlyCost( ) { return oGeoOutputs.pumpOnlyCost(); }
		double ShowTotalCostPerPump( ) { return oGeoOutputs.productionPumpCost(); }
		double ShowTotalPumpCost( ) { return oGeoOutputs.costDownHolePumps(); }



	//========================== recapitalization costs =====================================================
		double ShowCalculatedRecapitalizationCost( ) { return oGeoOutputs.capitalCostFieldReplacement(); }



	//========================== total direct costs =========================================================
	void SetContingencyRate(double contingencyPercent) { oGeoOutputs.mdContingencyFactor = contingencyPercent; }							//  default = 5%, added to project costs
	double GetContingencyRate( ) { return oGeoOutputs.mdContingencyFactor; }
		double ShowTotalCapitalCost( ) { return oGeoOutputs.capitalCostsPlantAndField(); }
		double ShowContingencyCost( ) { return oGeoOutputs.contingencyCosts(); }
		double ShowTotalDirectCost( ) { return oGeoOutputs.capitalCostsUpFront(); }



	//========================== other inputs not yet used ==================================================
	void SetFinalYearsWithNoReplacement(double yrs) { oGeoOutputs.mdFinalYearsWithNoReplacement = yrs; }				// default = 5 years
	double GetFinalYearsWithNoReplacement( ) { return oGeoOutputs.mdFinalYearsWithNoReplacement; }


	//========================== related to project finance (not useful in SAM) =============================
	// royalty
	void SetRoyaltyRate(double royaltyPercent) { oGeoOutputs.mdRoyaltyPercent = royaltyPercent; }										//  default = 10%, added to project costs
	double GetRoyaltyRate( ) { return oGeoOutputs.mdRoyaltyPercent; }

 	// O & M costs
	void SetPlantOMCost(double centsPerKWh) { oGeoOutputs.SetPlantOMCost(centsPerKWh); }													// default = 2 cents/kW
	double GetPlantOMCost( ) { return oGeoOutputs.GetOMCostsPlant(); }

	void SetFieldOMCost(double centsPerKWh) { oGeoOutputs.SetFieldOMCost(centsPerKWh); }													// default = 1 cents/kW
	double GetFieldOMCost( ) { return oGeoOutputs.GetOMCostsField(); }

	double GetOtherOMCost( ) { return oGeoOutputs.GetOMCostsOther(); }

	void SetProjectLifeYears(int yrs) { oGeoOutputs.miProjectLifeYears = yrs; }										// default = 30 years
	int GetProjectLifeYears( ) { return oGeoOutputs.miProjectLifeYears; }

	void SetAnnualDiscountRate(double discountRate) { oGeoOutputs.mdAnnualDiscountRate = discountRate; }				// %, 0.1 = 10% - annual discount rate
	double GetAnnualDiscountRate( ) { return oGeoOutputs.mdAnnualDiscountRate; }

	void SetUtilizationFactor(double utilizationFactor) { oGeoOutputs.mdUtilizationFactor = utilizationFactor; }		// %, 0.95 = 95% (adjusts capacity factor)
	double GetUtilizationFactor( ) { return oGeoOutputs.mdUtilizationFactor; }

	// result arrays
	void SetPointerToReplacementArray(float * p) {oGeoOutputs.SetPointerToReplacementArray(p);}

	void SetPointerToMonthlyTemperatureArray(float * p) { oGeoOutputs.SetPointerToMonthlyTemperatureArray(p); }
	void SetPointerToMonthlyOutputArray(float * p) { oGeoOutputs.SetPointerToMonthlyOutputArray(p); }
	void SetPointerToMonthlyPowerArray(float * p) { oGeoOutputs.SetPointerToMonthlyPowerArray(p); }

	void SetPointerToTimeStepTemperatureArray(float * p) {oGeoOutputs.SetPointerToTimeStepTemperatureArray(p);}
	void SetPointerToTimeStepOutputArray(float * p) {oGeoOutputs.SetPointerToTimeStepOutputArray(p);}
	void SetPointerToTimeStepTestArray(float * p) {oGeoOutputs.SetPointerToTimeStepTestArray(p);}

	void SetPointerToTimeStepPressureArray(float * p) {oGeoOutputs.SetPointerToTimeStepPressureArray(p);}
	void SetPointerToTimeStepDryBulbArray(float * p)  {oGeoOutputs.SetPointerToTimeStepDryBulbArray(p);}
	void SetPointerToTimeStepWetBulbArray(float * p)  {oGeoOutputs.SetPointerToTimeStepWetBulbArray(p);}

	//========================== status and run =============================================================
	bool AbleToDetermineAlgorithm( ){return (oGeoOutputs.determineMakeupAlgorithm() == NO_MAKEUP_ALGORITHM) ? false : true; }
	bool IsReadyToRun( );
	bool ErrorOccured( );
	std::string GetErrorMsg( );
	int RunGeoHourly(void (*update_function)(float,void*),void*user_data); // 0=clean run, !=0 means errors
	void Initialize( ) { oGeoOutputs.init(); }


	//========================== outputs ====================================================================
	// energy related and other outputs
	int GetMakeupAlgorithmUsed( ) { return (int)oGeoOutputs.GetMakeupAlgorithm(); } // 1=binary, 2=flash, 3=EGS
	double GetPumpWorkWH( ) { return oGeoOutputs.GetPumpWorkWattHrPerLb(); }
	//double GetTemperatureCAtPeriodi(int i) { return oGeoOutputs.calculatedTemperatureC(i); }
	//double GetPowerAtPeriodi_GETEM(unsigned int i) { return oGeoOutputs.netPower(i); }
	//double GetPowerAtPeriodi_Type224(unsigned int i) { return oGeoOutputs.type224Power(i); }
	//double GetDiscountedNetPowerAtPeriodi(int i) { return oGeoOutputs.discountedNetPower(i); }
	//double GetDiscountedDesignPowerAtPeriodi(int i) { return oGeoOutputs.discountedDesignPower(i); }
	double GetRelativeRevenue( ) { return oGeoOutputs.relativeRevenue(); }
	double GetEnergyProductionKWh( ) { return oGeoOutputs.energyProductionKWh(); }

	// array outputs
	//unsigned int GetOutputArraySize( ) { return oGeoOutputs.analysisTimeSteps(); }
	//std::vector<double> GetPlantOutputs( )     { return oGeoOutputs.GetPointerToOutputArray(); }		 // plant output over time
	//std::vector<int> GetReplacements( )		  { return oGeoOutputs.GetPointerToReplacementArray(); }  // zero for no replacement, 1 if reservoir replaced
	//std::vector<double> GetTemperatures( )	  { return oGeoOutputs.GetPointerToTemperatureCArray(); } // temperatures over time in degrees C
	//std::vector<double> GetType224Outputs( )	  { return oGeoOutputs.GetPointerToType224OutputArray(); }// values from TRNSYS type224 power block output

	//float * GetTestValues( ) { return oGeoOutputs.GetPointerToTestValueArray(); }	 // values for debugging code

	// cost of energy outputs for pie chart
	double GetCentsPerKWhExploration( ) { return 100 * oGeoOutputs.dollarsPerKWhExploration(); }
	double GetCentsPerKWhConfirmation( ) { return 100 * oGeoOutputs.dollarsPerKWhConfirmation(); }
	double GetCentsPerKWhWellFieldCapital( )  { return 100 * oGeoOutputs.dollarsPerKWhWellFieldCapital(); }
	double GetCentsPerKWhWellFieldMakeup( )   { return 100 * oGeoOutputs.dollarsPerKWhWellFieldMakeup(); }
	double GetCentsPerKWhWellFieldOM( )  { return 100 * oGeoOutputs.dollarsPerKWhWellFieldOM(); }
	double GetCentsPerKWhFieldOtherCaptial( ) { return 100 * oGeoOutputs.dollarsPerKWhFieldOtherCaptial(); }
	double GetCentsPerKWhFieldOtherOM( ) { return 100 * oGeoOutputs.dollarsPerKWhFieldOtherOM(); }
	double GetCentsPerKWhPlantCapital( )  { return 100 * oGeoOutputs.dollarsPerKWhPlantCapital(); }
	double GetCentsPerKWhPlantOM( )  { return 100 * oGeoOutputs.dollarsPerKWhPlantOM(); }
	double GetCentsPerKWhRoyalty( )  { return 100 * oGeoOutputs.dollarsPerKWhRoyalty(); }
	double GetCentsPerKWhContingency( ) { return 100 * oGeoOutputs.dollarsPerKWhContingency(); }

	// other financial outputs
	double GetCentsPerKWHOM( ) { return 100 * oGeoOutputs.dollarsPerKWhOMTotal(); }
	double GetCentsPerKWHCaptial( ) { return 100 * oGeoOutputs.dollarsPerKWhCapitalTotal(); }
	double GetCentsPerKWHTotal( ) { return 100 * oGeoOutputs.dollarsPerKWhTotal(); }

	// capital costs... on "4A.Binary System" or "5A.Flash-Steam System " sheet
	double GetExplorationCosts( )  { return oGeoOutputs.capitalCostsExploration(); }				// E60  these two are 
	double GetConfirmationCosts( ) { return oGeoOutputs.capitalCostsConfirmation(); }				// E61	often combined
	double GetMainWellCosts( )     { return oGeoOutputs.capitalCostsMainWells(); }				// E62  
	double GetOtherFieldCosts( )   { return oGeoOutputs.capitalCostsOtherField(); }				// E63
	double GetPlantCapitalCosts( ) { return oGeoOutputs.capitalCostsPlant(); }					// E68
	double GetContingencyCosts( )  { return oGeoOutputs.contingencyCosts(); }						// E69
	double GetFieldReplacementCost( ) { return oGeoOutputs.capitalCostFieldReplacement(); }		// I80 "makeup costs"
};


#endif // __lib_geohourly_interface
