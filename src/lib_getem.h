#ifndef __lib_getem_h
#define __lib_getem_h

#include <math.h>
#include <assert.h>

#include <vector>
#include <string>

#include "lib_getemint.h"

class CGETEMInterface
{
private:
	CGETEMOutputs oGetem;

public:
	CGETEMInterface(void);
	~CGETEMInterface(void);

	//========================== plant configuration ========================================================

	// energy calculation basis
	void SetDesiredPlantSalesKW(double kw) { oGetem.cb = POWER_SALES; oGetem.SetDesiredSalesCapacityKW(kw); }						// KW, default 15000 (15MW), desired sales from plant (plant capacity is larger do account for pumping and other losses)
	void SetDesiredNumberOfWells(double numberOfWells) { oGetem.cb = NUMBER_OF_WELLS; oGetem.SetNumberOfWells(numberOfWells); }		// number of wells, default is that GETEM will calculate this based on desired output of plant
	int GetCalculationBasis(void) { return (int)oGetem.cb; }																		// 1 = power output (# wells is calculated), 2 = number of wells (power output is calculated)
	double GetPlantOutput(void) { return oGetem.PlantOutputKW(); }
	double GetPlantGrossOutput(void) { return oGetem.PlantSizeKW(); }
	double GetPlantNetOutput(void) { return oGetem.PowerSalesKW(); }

	// conversion plant type
	void SetConversionType(int plantType){oGetem.cst = (conversionTypes)plantType; }								// 1=Binary, 2=Flash
	int GetConversionType(void){ return (int)oGetem.cst; }

	// plant brine effectiveness (this value is calculated for flash plants)
	void SetPlantEfficiency(double percent) { oGetem.SetPlantEfficiency(percent); }
	double GetPlantEfficiency(void) { return oGetem.GetPlantEfficiency(); }
	double GetPBE(void) { return oGetem.GetPlantBrineEffectiveness(); }												// plant brine effectiveness (plant efficiency)

	// set the flash plant sub-type
	void SetFlashSubType(int ft){oGetem.ft = (flashTypes)ft; }														// 1=single flash, no constraint, 2=single flash, with constraint, 
	int GetFlashSubType(void){ return (int)oGetem.ft; }																// 3=dual flash, no constraint, 4=dual flash, with constraint

		// to show user intermediate values
		double ShowNumberOfWells(void) { return oGetem.GetNumberOfProductionWells(); }
		double ShowPlantBrineEffectiveness(void) { return oGetem.GetPlantBrineEffectiveness(); }					// same as above, for calculated value in interface
		double ShowMaxPlantBrineEffectiveness(void) { return oGetem.GetMaxBinaryBrineEffectiveness(); }				// max theoretical plant brine effectiveness including exit temp constraint
		double ShowGrossOutput(void) { return oGetem.GetGrossPlantOutputMW(); } // mega watts
		double ShowNetOutput(void) { return oGetem.GetNetPlantOutputMW(); } // mega watts


	//========================== resource characterization ==================================================
	void SetPotentialResourceMW(double megaWatts) { oGetem.mdPotentialResourceMW = megaWatts; }						// MW, default = 200 MW, determines how many times reservoir can be replaced
	double GetPotentialResourceMW(void) { return oGetem.mdPotentialResourceMW; }

	double GetAmbientTemperatureC(void) { return oGetem.GetAmbientTemperatureC(); }									// degrees C, default 10 (not sure why there's two ambient temperatures)

	void SetResourceType(int resourceType) {if(resourceType == 1) oGetem.rt = HYDROTHERMAL; else oGetem.rt = EGS; }	// 1=Hydrothermal (default), !1=EGS 
	int GetResourceType(void){ return (int)oGetem.rt; }

	void SetResourceTemperatureCelcius(double degreesCelcius){oGetem.SetResourceTemperatureC(degreesCelcius); }		// degrees C, default 200
	double GetResourceTemperatureCelcius(void){ return oGetem.GetResourceTemperatureC(); }
	double ShowResourceTemperatureCelcius(void){ return oGetem.GetResourceTemperatureC(); }

	void SetResourceDepthMeters(double meters){oGetem.SetResourceDepthM(meters); }									// meters, default 2000
	double GetResourceDepthMeters(void){ return oGetem.GetResourceDepthM(); }
	double ShowResourceDepthMeters(void){ return oGetem.GetResourceDepthM(); }

	void SetTemperatureGradient(double tempGradient){oGetem.SetTemperatureGradient(tempGradient);}					// Degrees C per kM, default 35
	double GetTemperatureGradient(void){ return oGetem.GetTemperatureGradient(); }
	double ShowTemperatureGradient(void){ return oGetem.GetTemperatureGradient(); }

	void SetEGSAmbientTemperatureC(double degreesCelcius) { oGetem.SetAmbientEGSTemperatureC(degreesCelcius); }		// degrees C, default 15
	double GetEGSAmbientTemperatureC(void) { return oGetem.GetAmbientEGSTemperatureC(); }

	void SetPlantDesignTemperatureCelcius(double degreesCelcius){oGetem.SetTemperaturePlantDesignC(degreesCelcius); }	// degrees C, default 225, only used for EGS
	double GetPlantDesignTemperatureCelcius(void){ return oGetem.GetTemperaturePlantDesignC(); }						//  (Hydrothermal analysis only uses resource temp)
	double GetPlantDesignTemperatureMinCelcius(void) { return oGetem.GetTemperaturePlantDesignMinC(); }


	//========================== temperature decline inputs =================================================
	void SetTemperatureDeclineMethod(int tdm){ oGetem.tdm = (tempDeclineMethod)tdm; }							// 1=user enter rate, 2=GETEM calculate it (choice 2 is valid 
	int GetTemperatureDeclineMethod(void){ return oGetem.tdm; }													// for EGS resourceTypes only)

	void SetTemperatureDeclineRate(double percentPerYear) { oGetem.mdTemperatureDeclineRate = percentPerYear; }	// '% per year, 3% is default
	double GetTemperatureDeclineRate(void) { return oGetem.mdTemperatureDeclineRate; }

	void SetMaxTempDeclineC(double degC) { oGetem.mdMaxTempDeclineC = degC; }																	// degrees C, default = 30
	double GetMaxTempDeclineC(void) { return oGetem.mdMaxTempDeclineC; }


	//========================== flash technology inputs ====================================================
	void SetWetBulbTemperatureC(double degreesCelcius) { oGetem.mdTemperatureWetBulbC = degreesCelcius; }							// degrees celcius - used in Flash brine effectiveness
	double GetWetBulbTemperatureC(void) { return oGetem.mdTemperatureWetBulbC; }													// calcs and to calculate ambient temperature for flash

	void SetPressureAmbientPSI(double psi) { oGetem.mdPressureAmbientPSI = psi; }													// psi, default=14.7, mostly for use in calculating flash brine effectiveness, but also pump work
	double GetPressureAmbientPSI(void) { return oGetem.mdPressureAmbientPSI; }


	//========================== pumping parameters =========================================================
	void SetProductionFlowRateKgPerS(double flowRateKgPerSecond) { oGetem.SetProductionFlowRateKgPerS(flowRateKgPerSecond); }		// Kg per second per well, 70 is default
	double GetProductionFlowRateKgPerS(void) { return oGetem.GetProductionFlowRateKgPerS(); }

	void SetPumpEfficiency(double pumpEfficiencyPercent) { oGetem.SetGFPumpEfficiency(pumpEfficiencyPercent); }						// default=0.6 or 60%
	double GetPumpEfficiency(void) { return oGetem.GetGFPumpEfficiency(); }

	void SetPressureChangeAcrossSurfaceEquipment(double psi) { oGetem.mdPressureChangeAcrossSurfaceEquipmentPSI = psi; }			// default 25 psi
	double GetPressureChangeAcrossSurfaceEquipment(void) { return oGetem.mdPressureChangeAcrossSurfaceEquipmentPSI; }

	void SetExcessPressureBar(double bar) { oGetem.mdExcessPressureBar = bar; }														// default 3.5 bar, [2B.Resource&Well Input].D205
	double GetExcessPressureBar(void) { return oGetem.mdExcessPressureBar; }

	void SetExcessPressurePSI(double psi) { oGetem.mdExcessPressureBar = PsiToBar(psi); }											// same as above, in PSI
	double GetExcessPressurePSI(void) { return BarToPsi(oGetem.mdExcessPressureBar); }

	void SetProductionWellDiameter(double inches) { oGetem.SetDiameterProductionWellInches(inches); }								// default 10 inches
	double GetProductionWellDiameter(void) { return oGetem.GetDiameterProductionWellInches(); }

	void SetPumpCasingDiameter(double inches) { oGetem.SetDiameterPumpCasingInches(inches); }										// default 9.925 inches
	double GetPumpCasingDiameter(void) { return oGetem.GetDiameterPumpCasingInches(); }

	void SetInjectionWellDiameter(double inches) { oGetem.SetDiameterInjectionWellInches(inches); }									// default 10 inches
	double SetInjectionWellDiameter(void) { return oGetem.GetDiameterInjectionWellInches(); }

		// to show user intermediate values
		double ShowPumpDepthFeet(void) { return oGetem.GetPumpDepthFt(); } // ft
		double ShowPumpWork(void) { return oGetem.GetPumpWorkKW()/1000; } // mega watts




	//========================== reservoir parameters =======================================================
	// rock properties
	void SetRockThermalConductivity(double conductivity) { oGetem.SetEGSThermalConductivity(conductivity); }		// default 259,200 Joules per m-day-C, [2B.Resource&Well Input].D240
	double GetRockThermalConductivity(void) { return oGetem.GetEGSThermalConductivity(); }

	void SetRockSpecificHeat(double specificHeat) { oGetem.SetEGSSpecificHeatConstant(specificHeat); }				// default 950 Joules per kg-C, [2B.Resource&Well Input].D241
	double GetRockSpecificHeat(void) { return oGetem.GetEGSSpecificHeatConstant(); }

	void SetRockDensity(double density) { oGetem.SetEGSRockDensity(density); }										// default 2600 kg per cubic meter, [2B.Resource&Well Input].D242
	double GetRockDensity(void) { return oGetem.GetEGSRockDensity(); }

	// how to calculate the change in pressure across the reservoir
	void SetPressureCalculationMethod(int pcMethod){ oGetem.pc = (reservoirPressureChangeCalculation)pcMethod; }		// 1=user enter pressure change, 2=SAM calculates it using simple fracture flow (for EGS resourceTypes only
	int GetPressureCalculationMethod(void){ return (int)oGetem.pc; }													// 3=SAM calculates it using k*A (permeability x area)

	void SetReservoirPressureChange(double psiHPer1000lb) { oGetem.mdReservoirDeltaPressure = psiHPer1000lb; }			// default 0.35 psi-h per 1000lb, [2B.Resource&Well Input].D171
	double GetReservoirPressureChange(void) { return oGetem.mdReservoirDeltaPressure ; }

	// reservoir characteristics for pump power calculations
	void SetReservoirWidthM(double meters) { oGetem.SetReservoirWidthM(meters); }										// default = 500 meters, [2B.Resource&Well Input].F181
	double GetReservoirWidthM(void) { return oGetem.GetReservoirWidthM(); }

	void SetReservoirHeightM(double meters) { oGetem.SetReservoirHeightM(meters); }										// default = 100 meters, [2B.Resource&Well Input].F180
	double GetReservoirHeightM(void) { return oGetem.GetReservoirHeightM(); }

	void SetReservoirPermeability(double darcyUnits) { oGetem.SetReservoirPermeability(darcyUnits); }					// default = 0.05 darcy units, [2B.Resource&Well Input].D179
	double GetReservoirPermeability(void) { return oGetem.GetReservoirPermeability(); }

	// there are two inputs for this in GETEM - why?
	// used to calculate pressure change across reservoir
	void SetWellDistance2(double meters) { oGetem.mdDistanceBetweenProductionInjectionWellsM = meters; }	// default 1500 m [2B.Resource&Well Input].F185
	double GetWellDistance2(void) { return oGetem.mdDistanceBetweenProductionInjectionWellsM; }

	// this well distance is used for EGS calculations, to get the 'effective length' of the fractures
	void SetWellDistance(double meters) { oGetem.mdEGSDistanceBetweenProductionInjectionWellsM = meters; }	// default 1000 m [2B.Resource&Well Input].F76
	double GetWellDistance(void) { return oGetem.mdEGSDistanceBetweenProductionInjectionWellsM; }

	// subsurface rock and fracture parameters for EGS
	void SetSubsurfaceWaterLossRate(double percent) { oGetem.SetWaterLossPercent(percent); }						// default 2%
	double GetSubsurfaceWaterLossRate(void) { return oGetem.GetWaterLossPercent(); }

	void SetSubsurfaceFractureAperature(double meters) { oGetem.SetEGSFractureAperature(meters); }					// default 0.0004 m
	double GetSubsurfaceFractureAperature(void) { return oGetem.GetEGSFractureAperature(); }

	void SetNumberOfFractures(double numberOfFractures) { oGetem.SetEGSNumberOfFractures(numberOfFractures); }		// default 6
	double GetNumberOfFractures(void) { return oGetem.GetEGSNumberOfFractures(); }

	void SetSubsurfaceFractureWidth(double meters) { oGetem.SetEGSFractureWidthM(meters); }							// default 175 m
	double GetSubsurfaceFractureWidth(void) { return oGetem.GetEGSFractureWidthM(); }

	void SetSubsurfaceFractureAngle(double degrees) { oGetem.mdEGSFractureAngle = degrees; }						// default 15 degrees
	double GetSubsurfaceFractureAngle(void) { return oGetem.mdEGSFractureAngle; }

		// to show user intermediate values
		double ShowPressureChange(void) { return oGetem.GetPressureChangeAcrossReservoir(); } // psi
		double ShowAverageReservoirTemperature(void) { return FarenheitToCelcius(oGetem.GetAverageReservoirTemperatureUsedF()); }	// degrees C
		double ShowBottomHolePressure(void) { return oGetem.GetBottomHolePressure(); } // psi


	//========================== Number of wells to be drilled ==============================================
	void SetConfirmationWellSuccessRate(double percent) { oGetem.mdConfirmationWellSuccessRate = percent; }				// default = 50%
	double GetConfirmationWellSuccessRate(void) { return oGetem.mdConfirmationWellSuccessRate; }

	void SetRatioInjectionToProduction(double ratio) { oGetem.mdRatioInjectionToProduction = ratio; }					// default = 0.50, half as many injection wells
	double GetRatioInjectionToProduction(void) { return oGetem.mdRatioInjectionToProduction; }

		// to show user intermediate values
		double ShowConfirmationWellsUsedAsProduction(void) { return oGetem.numberConfirmationWellsUsedForProduction(); }
		double ShowProductionWellsDrilled(void) { return oGetem.numberProductionWellsDrilled(); }
		double ShowInjectionWellsDrilled(void) { return oGetem.numberInjectionWellsDrilled(); }




	//========================== drilling and associated costs ==============================================
	// exloration
	void SetExplorationWellCostMultiplier(double newValue) { oGetem.mdExplorationWellCostMultiplier = newValue; }					// default = 0.5, [2A.Scenario Input].D57
	double GetExplorationWellCostMultiplier(void) { return oGetem.mdExplorationWellCostMultiplier; }

	void SetExplorationWells(double newValue) { oGetem.mdExplorationWells = newValue; }												// default = 2, [2A.Scenario Input].D50
	double GetExplorationWells(void) { return oGetem.mdExplorationWells; }

	void SetNonWellExplorationCosts(double dollars) { oGetem.mdNonWellExplorationCosts = dollars; }									// default = $750,000; [2A.Scenario Input].D60
	double GetNonWellExplorationCosts(void) { return oGetem.mdNonWellExplorationCosts; }

		double ShowExplorationCostPerWell(void) { return oGetem.costPerExplorationWell(); }
		double ShowExplorationDrillingCost(void) { return oGetem.costExplorationVariable(); }
		double ShowExplorationCost(void) { return oGetem.capitalCostsExploration(); }


	// confirmation
	void SetConfirmationWellCostMultiplier(double newValue) { oGetem.mdConfirmationWellCostMultiplier = newValue; }					// default = 1.2, [2A.Scenario Input].D58
	double GetConfirmationWellCostMultiplier(void) { return oGetem.mdConfirmationWellCostMultiplier; }

	void SetConfirmationWells(double newValue) { oGetem.mdConfirmationWells = newValue; }											// default = 2, [2A.Scenario Input].D55
	double GetConfirmationWells(void) { return oGetem.mdConfirmationWells; }

	void SetNonWellConfirmationCosts(double dollars) { oGetem.mdNonWellConfirmationCosts = dollars; }								// default = $250,000; [2A.Scenario Input].D61
	double GetNonWellConfirmationCosts(void) { return oGetem.mdNonWellConfirmationCosts; }

		double ShowConfirmationCostPerWell(void) { return oGetem.costPerConfirmationWell(); }
		double ShowConfirmationDrillingCost(void) { return oGetem.costConfirmationVariable(); }
		double ShowConfirmationCost(void) { return oGetem.capitalCostsConfirmation(); }


	// production
	void SetCostCurveForProductionWell(int cc){oGetem.SetProductionWellCostCurve((wellCostCurveChoices)cc); }						// default=med, 1=Low, 2=Med, 3=High
	int GetCostCurveForProductionWell(void){ return (int)oGetem.GetProductionWellCostCurve(); }
		double ShowProductionCostPerWell(void) { return oGetem.costPerProductionWell(); }
		double ShowProductionDrillingCost(void) { return oGetem.costProductionWells(); }


	// injection
	void SetCostCurveForInjectionWell(int cc){oGetem.SetInjectionWellCostCurve((wellCostCurveChoices)cc); }							// default=med, 1=Low, 2=Med, 3=High
	int GetCostCurveForInjectionWell(void){ return (int)oGetem.GetInjectionWellCostCurve(); }
		double ShowInjectionCostPerWell(void) { return oGetem.costPerInjectionWell(); }
		double ShowInjectionDrillingCost(void) { return oGetem.costInjectionWells(); }			


	// production and injection (total well field)
	void SetWellFieldNonDrillingCosts(double dollars) { oGetem.mdWellFieldNonDrillingCosts = dollars; }								// default = $250,000
	double GetWellFieldNonDrillingCosts(void) { return oGetem.mdWellFieldNonDrillingCosts; }
		double ShowTotalWellsDrilled(void) { return oGetem.totalWellsDrilled(); }
		double ShowWellFieldDrillingCost(void) { return oGetem.costWellFieldDrilling(); }
		double ShowTotalWellFieldCost(void) { return oGetem.capitalCostsMainWells(); }


	// other surface equipment
	void SetSurfaceEquipmentCostPerWell(double dollarsPerWell) { oGetem.mdSurfaceEquipmentCostPerWell = dollarsPerWell; }			// default = $125,000/well
	double GetSurfaceEquipmentCostPerWell(void) { return oGetem.mdSurfaceEquipmentCostPerWell; }
		double ShowSurfaceEquipmentInstallationCosts(void) { return oGetem.costSurfaceEquipment(); }

	// well stimulation
	void SetWellStimulationCostPerWell(double dollarsPerWell) { oGetem.mdWellStimulationCostPerWell = dollarsPerWell; }				// default = $1,000,000/well
	double GetWellStimulationCostPerWell(void) { return oGetem.mdWellStimulationCostPerWell; }
		double ShowStimulationCosts(void) { return oGetem.costStimulation(); }


	//========================== plant capital costs ========================================================
	void SetCapitalCostPerKW(double capCostDollarsPerKW) { oGetem.SetPlantCapitalCostPerKW(capCostDollarsPerKW); }					// default = $1800/kW, forces the model to use this cost/kW, eventually model will calculate this based on resource temp, depth, etc.
	double GetCapitalCostPerKW(void) { return oGetem.GetPlantCapitalCostPerKW(); }
		double ShowPlantCapitalCost(void) { return oGetem.capitalCostsPlant(); }


	//========================== pump cost inputs ===========================================================
	//void SetProductionPumpCostCasing(double dollarsPerFoot) { oGetem.mdProductionPumpCostCasing = dollarsPerFoot; }				// default = $45/ft				([7A.GF Pumps].G144)
	//double GetProductionPumpCostCasing(void) { return oGetem.mdProductionPumpCostCasing; }

	//void SetProductionPumpCostInstall(double dollarsPerFoot) { oGetem.mdProductionPumpCostInstall = dollarsPerFoot; }				// default = $5/ft				([7A.GF Pumps].G145)
	//double GetProductionPumpCostInstall(void) { return oGetem.mdProductionPumpCostInstall; }

	//void SetProductionPumpCostOther(double dollars) { oGetem.mdProductionPumpCostOther = dollars; }								// default = $10,000
	//double GetProductionPumpCostOther(void) { return oGetem.mdProductionPumpCostOther; }

	void SetPumpCostPerFoot(double dollarsPerFoot) { oGetem.mdPumpCostPerFoot = dollarsPerFoot; }									// default = $50/ft, combination of casing cost and installation cost
	double GetPumpCostPerFoot(void) { return oGetem.mdPumpCostPerFoot; }

	void SetPumpCostPerHP(double dollars) { oGetem.mdPumpCostPerHP = dollars; }														// default = $12,000 per horsepower
	double GetPumpCostPerHP(void) { return oGetem.mdPumpCostPerHP; }
		//double ShowCasingCostPerPump(void) { return oGetem.pumpCasingCost(); }
		double ShowInstallationCostPerPump(void) { return oGetem.pumpInstallationCost(); }
		double ShowPumpHorsePower(void) { return oGetem.GetPumpSizeHP(); }
		double ShowPumpOnlyCost(void) { return oGetem.pumpOnlyCost(); }
		double ShowTotalCostPerPump(void) { return oGetem.productionPumpCost(); }
		double ShowTotalPumpCost(void) { return oGetem.costDownHolePumps(); }



	//========================== recapitalization costs =====================================================
		double ShowCalculatedRecapitalizationCost(void) { return oGetem.capitalCostFieldReplacement(); }



	//========================== total direct costs =========================================================
	void SetContingencyRate(double contingencyPercent) { oGetem.mdContingencyFactor = contingencyPercent; }							//  default = 5%, added to project costs
	double GetContingencyRate(void) { return oGetem.mdContingencyFactor; }
		double ShowTotalCapitalCost(void) { return oGetem.capitalCostsPlantAndField(); }
		double ShowContingencyCost(void) { return oGetem.contingencyCosts(); }
		double ShowTotalDirectCost(void) { return oGetem.capitalCostsUpFront(); }



	//========================== other inputs not yet used ==================================================
	void SetFinalYearsWithNoReplacement(double yrs) { oGetem.mdFinalYearsWithNoReplacement = yrs; }				// default = 5 years
	double GetFinalYearsWithNoReplacement(void) { return oGetem.mdFinalYearsWithNoReplacement; }

	void SetMakeupAnalysesPerYear(int periods) { oGetem.SetMakeupAnalysesPerYear(periods); }					// periods per year, default is 12 = one per month
	int GetMakeupAnalysesPerYear(void) { return oGetem.GetMakeupAnalysesPerYear(); }



	//========================== related to project finance (not useful in SAM) =============================
	// royalty
	void SetRoyaltyRate(double royaltyPercent) { oGetem.mdRoyaltyPercent = royaltyPercent; }										//  default = 10%, added to project costs
	double GetRoyaltyRate(void) { return oGetem.mdRoyaltyPercent; }

 	// O & M costs
	void SetPlantOMCost(double centsPerKWh) { oGetem.SetPlantOMCost(centsPerKWh); }													// default = 2 cents/kW
	double GetPlantOMCost(void) { return oGetem.GetOMCostsPlant(); }

	void SetFieldOMCost(double centsPerKWh) { oGetem.SetFieldOMCost(centsPerKWh); }													// default = 1 cents/kW
	double GetFieldOMCost(void) { return oGetem.GetOMCostsField(); }

	double GetOtherOMCost(void) { return oGetem.GetOMCostsOther(); }

	void SetProjectLifeYears(int yrs) { oGetem.miProjectLifeYears = yrs; }										// default = 30 years
	int GetProjectLifeYears(void) { return oGetem.miProjectLifeYears; }

	void SetAnnualDiscountRate(double discountRate) { oGetem.mdAnnualDiscountRate = discountRate; }				// %, 0.1 = 10% - annual discount rate
	double GetAnnualDiscountRate(void) { return oGetem.mdAnnualDiscountRate; }

	void SetUtilizationFactor(double utilizationFactor) { oGetem.mdUtilizationFactor = utilizationFactor; }		// %, 0.95 = 95% (adjusts capacity factor)
	double GetUtilizationFactor(void) { return oGetem.mdUtilizationFactor; }




	//========================== status and run =============================================================
	bool AbleToDetermineAlgorithm(void){return (oGetem.determineMakeupAlgorithm() == NO_MAKEUP_ALGORITHM) ? false : true; }
	bool IsReadyToRun(void);
	bool ErrorOccured(void);
	std::string GetErrorMsg(void);
	int RunGETEM(void); // 0=clean run, !=0 means errors
	void Initialize(void) { oGetem.init(); }



	//========================== outputs ====================================================================
	// energy related and other outputs
	int GetMakeupAlgorithmUsed(void) { return (int)oGetem.GetMakeupAlgorithm(); } // 1=binary, 2=flash, 3=EGS
	double GetPumpWorkWH(void) { return oGetem.GetPumpWorkWattHrPerLb(); }
	//double GetNetPowerAtPeriodi(int i) { return oGetem.discountedNetPower(i); }
	//double GetDesignPowerAtPeriodi(int i) { return oGetem.PlantOutputKW(i); }
	double GetDiscountedNetPowerAtPeriodi(int i) { return oGetem.discountedNetPower(i); }
	double GetDiscountedDesignPowerAtPeriodi(int i) { return oGetem.discountedDesignPower(i); }
	double GetRelativeRevenue(void) { return oGetem.relativeRevenue(); }
	double GetEnergyProductionKWh(void) { return oGetem.energyProductionKWh(); }

	// array outputs
	int GetPlantOutputArraySize(void) { return oGetem.analysisTimeSteps(); }
	std::vector<double> GetPlantOutputs(void)     { return oGetem.GetPointerToOutputArray(); }		 // plant output over time
	std::vector<int> GetReplacements(void)		  { return oGetem.GetPointerToReplacementArray(); }  // zero for no replacement, 1 if reservoir replaced
	std::vector<double> GetTemperatures(void)	  { return oGetem.GetPointerToTemperatureCArray(); } // temperatures over time in degrees C
	std::vector<double> GetTestValues(void)		  { return oGetem.GetPointerToTestValueArray(); }	 // values for debugging code

	// cost of energy outputs for pie chart
	double GetCentsPerKWhExploration(void) { return 100 * oGetem.dollarsPerKWhExploration(); }
	double GetCentsPerKWhConfirmation(void) { return 100 * oGetem.dollarsPerKWhConfirmation(); }
	double GetCentsPerKWhWellFieldCapital(void)  { return 100 * oGetem.dollarsPerKWhWellFieldCapital(); }
	double GetCentsPerKWhWellFieldMakeup(void)   { return 100 * oGetem.dollarsPerKWhWellFieldMakeup(); }
	double GetCentsPerKWhWellFieldOM(void)  { return 100 * oGetem.dollarsPerKWhWellFieldOM(); }
	double GetCentsPerKWhFieldOtherCaptial(void) { return 100 * oGetem.dollarsPerKWhFieldOtherCaptial(); }
	double GetCentsPerKWhFieldOtherOM(void) { return 100 * oGetem.dollarsPerKWhFieldOtherOM(); }
	double GetCentsPerKWhPlantCapital(void)  { return 100 * oGetem.dollarsPerKWhPlantCapital(); }
	double GetCentsPerKWhPlantOM(void)  { return 100 * oGetem.dollarsPerKWhPlantOM(); }
	double GetCentsPerKWhRoyalty(void)  { return 100 * oGetem.dollarsPerKWhRoyalty(); }
	double GetCentsPerKWhContingency(void) { return 100 * oGetem.dollarsPerKWhContingency(); }

	// other financial outputs
	double GetCentsPerKWHOM(void) { return 100 * oGetem.dollarsPerKWhOMTotal(); }
	double GetCentsPerKWHCaptial(void) { return 100 * oGetem.dollarsPerKWhCapitalTotal(); }
	double GetCentsPerKWHTotal(void) { return 100 * oGetem.dollarsPerKWhTotal(); }

	// capital costs... on "4A.Binary System" or "5A.Flash-Steam System " sheet
	double GetExplorationCosts(void)  { return oGetem.capitalCostsExploration(); }				// E60  these two are 
	double GetConfirmationCosts(void) { return oGetem.capitalCostsConfirmation(); }				// E61	often combined
	double GetMainWellCosts(void)     { return oGetem.capitalCostsMainWells(); }				// E62  
	double GetOtherFieldCosts(void)   { return oGetem.capitalCostsOtherField(); }				// E63
	double GetPlantCapitalCosts(void) { return oGetem.capitalCostsPlant(); }					// E68
	double GetContingencyCosts(void)  { return oGetem.contingencyCosts(); }						// E69
	double GetFieldReplacementCost(void) { return oGetem.capitalCostFieldReplacement(); }		// I80 "makeup costs"
};


#endif // __Geothermal__
