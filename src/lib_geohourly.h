#ifndef __geothermalMemberClassDefinitions__
#define __geothermalMemberClassDefinitions__

#include <math.h>
#include <vector>
#include "lib_weatherfile.h"
#include "lib_physics.h"
#include "lib_powerblock.h"


const double MAX_TEMP_RATIO = 1.134324;  // max valid value for (resource temp)/(plant design temp) where both are measured in Kelvin
const bool IMITATE_GETEM = false;
const double GETEM_FT_IN_METER = (IMITATE_GETEM) ? 3.28083 : physics::FT_PER_METER; // feet per meter - largest source of discrepancy
const double GETEM_PSI_PER_BAR = (IMITATE_GETEM) ? 14.50377 : physics::PSI_PER_BAR; // psi per bar
const double GETEM_PSI_PER_INHG = (IMITATE_GETEM) ? 0.49115 : physics::PSI_PER_INHG; // psi per inch of mercury
const double GETEM_KGM3_PER_LBF3 = (IMITATE_GETEM) ? (35.3146/2.20462) : physics::KGM3_PER_LBF3; // lbs/ft^3 per kg/m^3 
const double GETEM_LB_PER_KG = (IMITATE_GETEM) ? 2.20462 : physics::LB_PER_KG; // pounds per kilogram
const double GETEM_KW_PER_HP = (IMITATE_GETEM) ? 0.7457 : physics::KW_PER_HP; // kilowatts per unit of horsepower
const double GRAVITY_MS2 = (IMITATE_GETEM) ? 9.807 : physics::GRAVITY_MS2; // meters per second^2; this varies between 9.78 and 9.82 depending on latitude
const double DAYS_PER_YEAR = (IMITATE_GETEM) ? 365 : 365.25;

const double DEFAULT_AMBIENT_TEMPC_BINARY = 10;  // degrees C
const double AMBIENT_TEMPC_FOR_GRADIENT = 10;  // degrees C, embedded in [2B.Resource&Well Input].D14



enum calculationBasis { NO_CALCULATION_BASIS, POWER_SALES, NUMBER_OF_WELLS };
enum resourceTypes { NO_RESOURCE_TYPE, HYDROTHERMAL, EGS };
enum conversionTypes { NO_CONVERSION_TYPE, BINARY, FLASH }; //}
enum flashTypes { NO_FLASH_SUBTYPE, SINGLE_FLASH_NO_TEMP_CONSTRAINT, SINGLE_FLASH_WITH_TEMP_CONSTRAINT, DUAL_FLASH_NO_TEMP_CONSTRAINT, DUAL_FLASH_WITH_TEMP_CONSTRAINT };
enum tempDeclineMethod {NO_TEMPERATURE_DECLINE_METHOD, ENTER_RATE, CALCULATE_RATE };
enum makeupAlgorithmType { NO_MAKEUP_ALGORITHM, MA_BINARY, MA_FLASH, MA_EGS }; //}
enum condenserTypes { NO_CONDENSER_TYPE, SURFACE, DIRECT_CONTACT };
enum ncgRemovalTypes { NO_NCG_TYPE, JET, VAC_PUMP, HYBRID };
enum wellCostCurveChoices { NO_COST_CURVE, LOW, MED, HIGH };
enum depthCalculationForEGS { NOT_CHOSEN, DEPTH, TEMPERATURE };
enum reservoirPressureChangeCalculation { NO_PC_CHOICE, ENTER_PC, SIMPLE_FRACTURE, K_AREA };


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// GETEMPhysics ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
inline double FarenheitToCelcius(const double &dTemp) {return physics::FarenheitToCelcius(dTemp); };
inline double CelciusToFarenheit(const double &dTemp) {return physics::CelciusToFarenheit(dTemp); };

inline double KelvinToCelcius(const double &dTemp) {return physics::KelvinToCelcius(dTemp); };
inline double CelciusToKelvin(const double &dTemp) {return physics::CelciusToKelvin(dTemp); };

inline double FarenheitToKelvin(const double &dTemp){return physics::FarenheitToKelvin(dTemp); };
inline double KelvinToFarenheit(const double &dTemp){return physics::KelvinToFarenheit(dTemp); };

inline double areaCircle(const double &radius) { return physics::areaCircle(radius); }

inline double MetersToFeet(const double &m) {return m * GETEM_FT_IN_METER; }
inline double FeetToMeters(const double &ft) {return ft / GETEM_FT_IN_METER; }
inline double M2ToFeet2(const double &mSquared) { return (IMITATE_GETEM) ? mSquared * 10.76391 : mSquared * pow(GETEM_FT_IN_METER,2); }

inline double BarToPsi(const double &bar) { return bar * GETEM_PSI_PER_BAR; }
inline double PsiToBar(const double &psi){ return psi / GETEM_PSI_PER_BAR; }

inline double InHgToPsi(const double &inHg) { return inHg * GETEM_PSI_PER_INHG; }
inline double PsiToInHg(const double &psi){ return psi / GETEM_PSI_PER_INHG; }

inline double KgPerM3ToLbPerCf(const double &kgPerM3) { return kgPerM3 / GETEM_KGM3_PER_LBF3; }
inline double LbPerCfToKgPerM3(const double &lbPerCf) { return lbPerCf * GETEM_KGM3_PER_LBF3; }
inline double LbPerCfToKgPerM3_B(const double &lbPerCf) { return (IMITATE_GETEM) ? lbPerCf * 16.01846 : lbPerCf * GETEM_KGM3_PER_LBF3; }

inline double KgToLb(const double &kg) { return kg * GETEM_LB_PER_KG; }
inline double LbToKg(const double &lb) { return lb / GETEM_LB_PER_KG; }

inline double HPtoKW(const double &hp) { return hp * GETEM_KW_PER_HP; }
inline double KWtoHP(const double &kw) { return kw / GETEM_KW_PER_HP; }

inline double toWattHr(const double &btu) { return (btu/3.413); }
inline double PSItoFT(const double &psi) { return psi * 144 / 62.4; }  // convert PSI to pump 'head' in feet.  assumes water density ~ 62.4 lb/ft^3
inline double PSItoFTB(const double &psi) { return (IMITATE_GETEM) ? psi*144/62 : PSItoFT(psi); }  // convert PSI to pump 'head' in feet.  assumes water density ~ 62 lb/ft^3 if imitating GETEM

double pumpSizeInHP(const double &flow_LbPerHr, const double &head_Ft, const double &eff, std::string sErr);
double pumpWorkInWattHr(const double &flow_LbPerHr, const double &head_Ft, const double &eff, std::string sErr);

// other functions handy throughout GETEM C++
double evaluatePolynomial(const double &val, const double &c0, const double &c1, const double &c2, const double &c3, const double &c4, const double &c5, const double &c6);

double discountValue(const double &dVal, const double &dDRate, const double &dTimePeriods, std::string sErr);
double my_erfc(const double &x);

void setNonZeroValue(double &dVal, const double &newVal, std::string varName, std::string sErr);
void setPositiveValue(double &dVal, const double &newVal, std::string varName, std::string sErr);
void setPositiveValue(int &dVal, const int &newVal, std::string varName, std::string sErr);
void setZeroTo1(double &dVal, const double &newVal, std::string varName, std::string sErr);

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Declaration and Implementation of CGeothermalConstants //////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class CGeothermalConstants
{
public:
	CGeothermalConstants(void) {}
	virtual ~CGeothermalConstants(void) {}

	void init(const double &c1, const double &c2, const double &c3, const double &c4, const double &c5, const double &c6, const double &c7) { md1=c1; md2=c2; md3=c3; md4=c4; md5=c5; md6=c6; md7=c7; }
	double evaluatePolynomial(double val) { return ::evaluatePolynomial(val, md1, md2, md3, md4, md5, md6, md7); }

private:
	double md1,md2,md3,md4,md5,md6,md7;

};

 

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Declaration of CGETEMGlobals ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class CGETEMGlobals
{
public:
	CGETEMGlobals(void);
	virtual ~CGETEMGlobals(void){}
	static CGETEMGlobals GG;

	CGeothermalConstants oAmbientEnthalpyConstants;
	CGeothermalConstants oAmbientEntropyConstants;

	CGeothermalConstants oBinaryEnthalpyConstants;
	CGeothermalConstants oBinaryEntropyConstants;

	CGeothermalConstants oFlashEnthalpyConstants;
	CGeothermalConstants oFlashEntropyConstants;

	CGeothermalConstants oSVC; // Specific Volume Constants

	CGeothermalConstants oPC;	// Pressure Constants
	CGeothermalConstants oPressureAmbientConstants;	// Pressure Constants near ambient temperatures

	CGeothermalConstants oDensityConstants;	// Density Constants (in pump power calculations)

	CGeothermalConstants oFlashTempConstants;

	CGeothermalConstants oFlashConstants1;
	CGeothermalConstants oFlashConstants2;

	CGeothermalConstants oPSatConstants;

	CGeothermalConstants oEGSDensity;
	CGeothermalConstants oEGSSpecificHeat;

	CGeothermalConstants oMinimumTemperatureQuartz;
	CGeothermalConstants oMinimumTemperatureChalcedony;

	double GetSiPrecipitationTemperatureF(double geoFluidTempF);
	double EGSWaterDensity(double tempC);
	double EGSSpecificHeat(double tempC);

	double GetDHa(double pressurePSI);
	double GetDHb(double pressurePSI);

	double GetFlashEnthalpyF(double temperatureF);
	double GetFlashEnthalpyG(double temperatureF);
	double GetFlashTemperature(double pressurePSI);

	double additionalCWPumpHeadSurface(void) { return 10 * 144 / 62.4; }
	int injectionPumpingCycles(void) { return 6; }
	int mGeothermalFluidForFlash(void) { return 1000; } // D67 in "5C.Flash-Steam Plant Perf"

private:
	CGeothermalConstants oDHaUnder150;
	CGeothermalConstants oDHbUnder150;
	CGeothermalConstants oDHa150To1500;
	CGeothermalConstants oDHb150To1500;
	CGeothermalConstants oDHaOver1500;
	CGeothermalConstants oDHbOver1500;

	CGeothermalConstants oFlashEnthalpyFUnder125;
	CGeothermalConstants oFlashEnthalpyF125To325;
	CGeothermalConstants oFlashEnthalpyF325To675;
	CGeothermalConstants oFlashEnthalpyFOver675;

	CGeothermalConstants oFlashEnthalpyGUnder125;
	CGeothermalConstants oFlashEnthalpyG125To325;
	CGeothermalConstants oFlashEnthalpyG325To675;
	CGeothermalConstants oFlashEnthalpyGOver675;

	CGeothermalConstants oFlashTemperatureUnder2;
	CGeothermalConstants oFlashTemperature2To20;
	CGeothermalConstants oFlashTemperature20To200;
	CGeothermalConstants oFlashTemperature200To1000;
	CGeothermalConstants oFlashTemperatureOver1000;
};



/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Declaration of CGeothermalFluid /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class CGeothermalFluid
{
public:
	CGeothermalFluid(void);
	virtual ~CGeothermalFluid(void){}
	
	void init(const CGeothermalConstants &enthalpyConstants, const CGeothermalConstants &entropyConstants);
	double enthalpy(double tempF){return moEnthalpyConstants.evaluatePolynomial(tempF); }
	double entropy(double tempF) {return moEntropyConstants.evaluatePolynomial(tempF); }

private:
	CGeothermalConstants moEnthalpyConstants;
	CGeothermalConstants moEntropyConstants;
};


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// GETEMEquations //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
double calcEGSTemperatureConstant(double tempC, double maxSecondLawEff);
double calcEGSAverageWaterTemperatureC(double temp1C, double temp2C, double maxEff);


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Declaration of CGeoFluidContainer ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class CGeoFluidContainer
{
public:
	CGeoFluidContainer(void);
	virtual ~CGeoFluidContainer(void){}

	double GetAEForBinaryWattHr(double tempF, double ambientTempF){return toWattHr(GetAEForBinaryBTU(tempF, ambientTempF)); }
	double GetAEForFlashWattHr(double tempF, double ambientTempF){return toWattHr(GetAEForFlashBTU(tempF, ambientTempF)); }

	double GetAEForBinaryWattHrUsingC(double tempC, double ambientTempC) {return GetAEForBinaryWattHr(CelciusToFarenheit(tempC), CelciusToFarenheit(ambientTempC)); }
	double GetAEForFlashWattHrUsingC(double tempC, double ambientTempC) {return GetAEForFlashWattHr(CelciusToFarenheit(tempC), CelciusToFarenheit(ambientTempC)); }

private:
	CGETEMGlobals m_oGG;
	double GetAEForBinaryBTU(double tempF, double ambientTempF);
	double GetAEForFlashBTU(double tempHighF, double tempLowF);

	CGeothermalFluid moAmbientGeothermalFluid;
	CGeothermalFluid moBinaryGeothermalFluid;
	CGeothermalFluid moFlashGeothermalFluid;
};

 
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Declaration of CGeoHourlyBaseInputs //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class CGeoHourlyBaseInputs
{
public:
	CGeoHourlyBaseInputs(void);
	virtual ~CGeoHourlyBaseInputs(void){}

	resourceTypes rt;
	conversionTypes cst;
	flashTypes ft;
	tempDeclineMethod tdm;
	calculationBasis cb;
	depthCalculationForEGS dc;
	reservoirPressureChangeCalculation pc;

	CGeoFluidContainer GetGeoFluidContainer(void){ return moGFC;}
	int miProjectLifeYears;			// years
	//unsigned int analysisTimeSteps(void) { return (miProjectLifeYears * GetMakeupAnalysesPerYear()) + 1; } // analysis is done for period zero also, so add 1
	unsigned int analysisTimeSteps(void) { return (miProjectLifeYears * GetMakeupAnalysesPerYear()); } // analysis is done for period zero also, so add 1

	void SetModelChoice(int choice) { miModelChoice = ( (choice>=0) && (choice<3) ) ? choice : -1; }
	int GetModelChoice(void) { return miModelChoice; }

	double mdUtilizationFactor; //not explained well, but used to Get annual capacity factor
    double mdTemperatureDeclineRate; // % per year
	double mdTemperatureWetBulbC;    // degrees celcius - used in Flash calcs brine effectiveness calcs an flash injection temperature calcs
    double mdMaxTempDeclineC;
    double mdFinalYearsWithNoReplacement;
    double mdPotentialResourceMW;
	double mdPressureAmbientPSI;
	double mdAnnualDiscountRate;

	// pumping parameters
	double mdPressureChangeAcrossSurfaceEquipmentPSI; // 25 psi [2B.Resource&Well Input].D146
	double mdPumpCostPerHP; // $12,000
	bool mbCalculatePumpWork; // true
	double mdUserSpecifiedPumpWorkKW; // zero


	void SetResourceTemperatureC(double degreesCelcius) { dc = TEMPERATURE; setPositiveValue(mdTemperatureResourceC, degreesCelcius, "Resource Temperature", m_strErrMsg); }
	double GetResourceTemperatureC(void);
	double GetResourceTempF(void) { return CelciusToFarenheit(this->GetResourceTemperatureC()); }
	
	void SetResourceDepthM(double meters) { dc = DEPTH; setPositiveValue(mdResourceDepthM, meters, "Resource Depth", m_strErrMsg); }
	double GetResourceDepthM(void);
	double GetResourceDepthFt(void){ return MetersToFeet(GetResourceDepthM()); }

	void SetTemperatureGradient(double degreesCelciusPerKM) { setPositiveValue(mdEGSResourceTemperatureGradient, degreesCelciusPerKM, "Temperature Gradient", m_strErrMsg); }
	double GetTemperatureGradient(void);

	double GetAmbientTemperatureC(conversionTypes ct = NO_CONVERSION_TYPE);
	double GetAmbientTemperatureF(void) { return CelciusToFarenheit(GetAmbientTemperatureC()); }

	void SetAmbientEGSTemperatureC(double degreesCelcius){ setPositiveValue(mdTemperatureEGSAmbientC, degreesCelcius,"EGS Ambient Temperature", m_strErrMsg); }  // not sure why the ambient temperature for EGS is different from other ambient temperature
	double GetAmbientEGSTemperatureC(void){ return mdTemperatureEGSAmbientC; }

	void SetTemperaturePlantDesignC(double plantDesignTempC) { setPositiveValue(mdTemperaturePlantDesignC, plantDesignTempC, "Plant Design Temperature", m_strErrMsg); }
	double GetTemperaturePlantDesignC(void) { return (rt == EGS) ? mdTemperaturePlantDesignC : GetResourceTemperatureC(); }
	double GetTemperaturePlantDesignF(void) { return CelciusToFarenheit(GetTemperaturePlantDesignC()); }
	double GetTemperatureRatio(void) { return CelciusToKelvin(GetResourceTemperatureC()) / CelciusToKelvin(GetTemperaturePlantDesignC()); } // max valid value is MAX_TEMP_RATIO
	double GetTemperaturePlantDesignMinC(void) { return KelvinToCelcius(CelciusToKelvin(GetResourceTemperatureC())/MAX_TEMP_RATIO); }
	double GetTemperatureGFExitF(void) { return (GetTemperaturePlantDesignC() < 180) ? exitTempLowF() : exitTempHighF(); }  // degrees farenheit - exit temperature for geothermal fluid
	double GetTemperatureGFExitC(void) { return FarenheitToCelcius(GetTemperatureGFExitF()); }

	double flowRatePerWell(void) { return (60 * 60 * KgToLb(mdProductionFlowRateKgPerS)); } // lbs per hour, one well
	double flowRateTotal(void) { return (flowRatePerWell() * GetNumberOfWells()); }// lbs per hour, all wells

	double NumberOfReservoirs(void) { return floor(mdPotentialResourceMW * 1000 / PlantOutputKW()); } // KW = (watt-hr/lb)*(lbs/hr) / 1000
	double DesignCapacityKW(void) { return PlantOutputKW() - GetPumpWorkKW(); } // Max Net KW, after pumping losses (temperature at beginning)//should be the same as powerSales, but GETEM calculates it with assumed Binary constants
	double PowerSalesKW(void) { return PlantSizeKW() - GetPumpWorkKW(); } // Max Net KW, after pumping losses (temperature at beginning) //should be same as designCapacity
	double PlantSizeKW(void) { return flowRateTotal() * GetPlantBrineEffectiveness() / 1000.0;} //	Gross KW, before pump losses (temperature at beginning) = (lbs/hr) * (watt-hr / lb) / 1000
	double PlantOutputKW() { return (IMITATE_GETEM) ? flowRateTotal() * secondLawEfficiencyGETEM() * availableEnergyGETEM() / 1000.0 : PlantSizeKW(); }	//this should be the same as PlantSizeKW, but GETEM calculates it differently in different places [(lbs/hr) * % * (watt-hr / lb)]
	double PlantNetOutputKW(void) { return PlantSizeKW(); }  // used in cost calculation
	// FIX THE REDUNDANCIES IN THE ABOVE CRAP, FIX 'PLANTNETOUTPUT', SINCE IT'S GROSS OUTPUT


	double GetPumpWorkKW(void) { return (mbCalculatePumpWork) ? GetPumpWorkWattHrPerLb() * flowRateTotal() / 1000.0 : mdUserSpecifiedPumpWorkKW; }	// shortcut to function in CPumpPowerCalculator
	double grossCapacityPerWell(void) { return this->flowRatePerWell() * (GetPlantBrineEffectiveness()) / 1000.0; }		// before pumping losses
	double netCapacityPerWell(void)	  { return this->flowRatePerWell() * netBrineEffectiveness() / 1000.0; }			// after pumping losses
	double netBrineEffectiveness(void) { return GetPlantBrineEffectiveness() - GetPumpWorkWattHrPerLb(); }

	void SetNumberOfWells(double numWells) { setPositiveValue(mdNumberOfWells, numWells, "Number Of Wells", m_strErrMsg); }
	double GetNumberOfWells(void) { if (netCapacityPerWell()!=0) return (cb == NUMBER_OF_WELLS) ? mdNumberOfWells : mdDesiredSalesCapacityKW / netCapacityPerWell(); else return 0;}
	void SetDesiredSalesCapacityKW(double kw) { setPositiveValue(mdDesiredSalesCapacityKW, kw, "Desired Sales Capacity", m_strErrMsg); }
	double GetSalesCapacityKW(void) { return (cb == POWER_SALES) ? mdDesiredSalesCapacityKW : mdNumberOfWells * netCapacityPerWell(); }



	// Old Available Energy (AE) functions
	double availableEnergyGETEM(void) { return GetAE(); }
	double availableEnergyBinary(void){ return moGFC.GetAEForBinaryWattHrUsingC(GetTemperaturePlantDesignC(), GetAmbientTemperatureC()); }	// watt-hr/lb - Calculate available energy using binary constants and plant design temp (short cut)
	double availableEnergyFlash(void) { return moGFC.GetAEForFlashWattHrUsingC(GetTemperaturePlantDesignC(), GetAmbientTemperatureC()); }	// watt-hr/lb - Calculate available energy using flash constants and plant design temp (short cut)
	// needs to use the EGS ambient temperature
	double availableEnergyEGS()   { return moGFC.GetAEForFlashWattHrUsingC(mdTemperaturePlantDesignC, mdTemperatureEGSAmbientC); }		// watt-hr/lb - not sure why the flash constants are used to calc EGS available energy
	
	// New Available Energy (AE) functions
	double GetAE(void)					  { return GetAEAtTemp(GetTemperaturePlantDesignC()) ; }
	double GetAEBinary(void)			  { return GetAEBinaryAtTemp(GetTemperaturePlantDesignC()); }
	double GetAEFlash(void)				  { return GetAEFlashAtTemp(GetTemperaturePlantDesignC()); }
	double GetAEAtTemp(double tempC)	  { return (cst == BINARY) ? GetAEBinaryAtTemp(tempC) : GetAEFlashAtTemp(tempC) ; }
	double GetAEBinaryAtTemp(double tempC){ return moGFC.GetAEForBinaryWattHrUsingC(tempC, GetAmbientTemperatureC()); }	// watt-hr/lb - Calculate available energy using binary constants and plant design temp (short cut)
	double GetAEFlashAtTemp (double tempC){ return moGFC.GetAEForFlashWattHrUsingC(tempC, GetAmbientTemperatureC()); }	// watt-hr/lb - Calculate available energy using flash constants and plant design temp (short cut)
	double GetAEAtExit(void)			  { return GetAEAtTemp(GetTemperatureGFExitC()); }		// watt-hr/lb - Calculate available energy using binary constants and plant design temp (short cut)
	double GetAEMaxPossible(void)		  { return (IMITATE_GETEM) ? GetAEBinary() -  GetAEBinaryAtTemp(GetTemperatureGFExitC()) : GetAE() - GetAEAtExit(); }					// watt-hr/lb - [10B.GeoFluid].H54 "maximum possible available energy accounting for the available energy lost due to a silica constraint on outlet temperature"


	// new for C++ GETEM
	void SetPlantEfficiency(double percent) { setPositiveValue(mdPlantEfficiency, percent, "Plant Efficiency", m_strErrMsg); }
	double GetPlantEfficiency(void)		{ return mdPlantEfficiency; }
	
	makeupAlgorithmType determineMakeupAlgorithm(void);
	makeupAlgorithmType GetMakeupAlgorithm(void){ return mat; }

	double InectionPumpHeadUsed(void) { return 0; } // [2B.Resource&Well Input].D162
	double injectionTemperatureC(void); // calculate injection temperature in degrees C

	// Non cost inputs, but used mainly to develop capital costs
	double mdExplorationWells;
	double mdConfirmationWells;
	double mdConfirmationWellSuccessRate;
	double mdRatioInjectionToProduction; 
	double mdRatioInjectionDepthToProductionDepth;
	double mdNumberOfSpareProductionWells;

	// financial properties to Set
	double mdFixedChargeRate;					//I don't think this can be completely independent of discount rate as it is in GETEM
    double mdRoyaltyPercent;					//percent of annual, field related costs to add in order to approximate royalty fees
    double mdContingencyFactor;					//percent adder to total capital costs (field and plant)

	double mdNonWellExplorationCosts;			// $s					([2A.Scenario Input].D60)
	double mdNonWellConfirmationCosts;			// $s					([2A.Scenario Input].D61)
	double mdExplorationWellCostMultiplier;		//						([2A.Scenario Input].D57)
	double mdConfirmationWellCostMultiplier;	//						([2A.Scenario Input].D58)

	double mdWellFieldNonDrillingCosts;			// $s					([2B.Resource&Well Input].D69)
	double mdSurfaceEquipmentCostPerWell;		// $s per well			([2B.Resource&Well Input].D68)
	double mdWellStimulationCostPerWell;		// $s per well			([2B.Resource&Well Input].D111)

	//double mdProductionPumpCostOther;			// $s					([7A.GF Pumps].G143)
	//double mdProductionPumpCostCasing;		// $/ft					([7A.GF Pumps].G144)
	//double mdProductionPumpCostInstall;		// $/ft					([7A.GF Pumps].G145)
	double mdPumpCostPerFoot;					// $/ft, combination of the two above

	void SetPlantCapitalCostPerKW(double capCostPerKW) { mbCalculatePlantCost = false; mdPlantCapitalCostPerKWUserInput = capCostPerKW; }
	void SetPlantOMCost(double centsPerKWh) { mbCalculatePlantOM = false; mdOMCostsPlantUserInput = centsPerKWh; }
	void SetFieldOMCost(double centsPerKWh) { mbCalculateFieldOM = false; mdOMCostsFieldUserInput = centsPerKWh; }


	// GF pumping
	/* Base inputs has to be an abstract class because it needs this function to be implemented by the CGeoHourlyAnalysis class
	This is a way around a circular class reference dilemma:
	CPumpPowerCalculator needs values from CGeoHourlyBaseInputs (several), and visa versa (CGeoHourlyBaseInputs needs values from CPumpPowerCalculator - pump work).
	It's not quite this simple, because it's not actually CGeoHourlyBaseInputs that needs the values, it's CGeoHourlyAnalysis that needs
	CGeoHourlyBaseInputs to have the values, but the end result is the same.  This virtual function is part of the solution.
	Similar situation for the plant brine effectiveness (plant efficiency)
	*/
	virtual double GetPumpWorkWattHrPerLb(void)=0;
	virtual double GetPlantBrineEffectiveness(void)=0;
	virtual double GetFractionOfInletGFInjected(void)=0;
	
	void SetFractionOfInletGFInjected(double frac) { setPositiveValue(mdFractionOfInletGFInjected, frac, "Fraction of Inlet Geothermal Fluid Injected", m_strErrMsg); } // G135 on [7A.GF Pumps]




	// reservoir characteristics (used for GF pumping calculations in CPumpPowerCalculator)
	double mdExcessPressureBar;				// default 3.5 bar, [2B.Resource&Well Input].D205
	double mdReservoirDeltaPressure;		// psi-h per 1000lb
	double mdDistanceBetweenProductionInjectionWellsM;			// default 1500 meters, used to calculate the pressure change across the reservoir

	// added for EGS
	double mdEGSDistanceBetweenProductionInjectionWellsM;		// default 1000 meters, used to calculate the EGS 'effective length' or 'fracture length'
	double mdTemperatureEGSInjectionC;							// degrees C
	double mdEGSFractureAngle;									// degrees from horizontal
	double mdEGSTimeInput;										// years, not really explained - user is supposed to vary input until a calculated value equals plant design temp [7C.EGS Subsrfce HX].D42 (fTimeStar)



	double EGSTimeStar(void) { return calcEGSTimeStar(EGSAverageWaterTemperatureC2()); }
	double calcEGSReservoirConstant(double avgWaterTempC, double timeDays);

	// Why does GETEM calculate the average water temperature for EGS two different ways?  Is one better?  Method 2 is certainly simpler.
	double EGSAverageWaterTemperatureC1(void) { return calcEGSAverageWaterTemperatureC(GetResourceTemperatureC(), mdTemperaturePlantDesignC, GetPlantBrineEffectiveness() / availableEnergyEGS() ); }// degrees C (used in EGS makeup, and on [7C.EGS Subsrfce HX]
	//double GetemEGSTemperatureConstant(void)  { return calcEGSTemperatureConstant( (GetPlantBrineEffectiveness() / availableEnergyEGS()), mdTemperaturePlantDesignC); }
	//double EGSAverageWaterTemperatureC1(void) { return KelvinToCelcius(CelciusToKelvin(GetResourceTemperatureC()) * GetemEGSTemperatureConstant()); }	
	double EGSAverageWaterTemperatureC2(void) { return (injectionTemperatureC() + GetResourceTemperatureC())/2; } // degrees C (used in [6Bb.Makeup-EGS HX ].X35 to calc time*
	double EGSAverageReservoirTemperatureF(void);	//[7C.EGS Subsrfce HX].D52, [7B.Reservoir Hydraulics].D24


	// Values that are direct results of EGS inputs (no inputs needed)
	double EGSFractureLength(void) { return mdEGSDistanceBetweenProductionInjectionWellsM / cos(mdEGSFractureAngle * physics::PI / 180); } //fEffectiveLength, meters used in pump power calcs
	double EGSFractureLengthUserAdjusted(void) { return EGSFractureLength() * mdFractureLengthAdjustment; }

	void SetEGSFractureWidthM(double meters) { setPositiveValue(mdEGSFractureWidthM, meters, "Fracture Width", m_strErrMsg); }
	double GetEGSFractureWidthM (void) { return mdEGSFractureWidthM; }

	void SetEGSFractureAperature(double meters) { setPositiveValue(mdEGSFractureAperature, meters, "Fracture Aperature", m_strErrMsg); }
	double GetEGSFractureAperature (void) { return mdEGSFractureAperature; }

	void SetEGSThermalConductivity(double tc) { setPositiveValue(mdEGSThermalConductivity, tc, "Thermal Conductivity", m_strErrMsg); }		// J/m-day-C
	double GetEGSThermalConductivity (void) { return (IsHourly()) ? mdEGSThermalConductivity/24 : mdEGSThermalConductivity; }					// convert to J/m-hr-C for hourly analysis

	void SetEGSSpecificHeatConstant(double specificHeat) { setPositiveValue(mdEGSSpecificHeatConstant, specificHeat, "Specific Heat Constant", m_strErrMsg); }
	double GetEGSSpecificHeatConstant (void) { return mdEGSSpecificHeatConstant; }

	void SetEGSRockDensity(double density) { setPositiveValue(mdEGSRockDensity, density, "Rock Density", m_strErrMsg); }
	double GetEGSRockDensity (void) { return mdEGSRockDensity; }

	void SetEGSNumberOfFractures(double number) { setPositiveValue(mdEGSNumberOfFractures, number, "Number of Fractures", m_strErrMsg); }
	double GetEGSNumberOfFractures (void) { return mdEGSNumberOfFractures; }

	void SetProductionFlowRateKgPerS(double KGperS) { setPositiveValue(mdProductionFlowRateKgPerS, KGperS, "Production Flow Rate", m_strErrMsg); }
	double GetProductionFlowRateKgPerS (void) { return mdProductionFlowRateKgPerS; }

	void SetGFPumpEfficiency(double percent) { setPositiveValue(mdGFPumpEfficiency, percent, "Pump Efficiency", m_strErrMsg); }
	double GetGFPumpEfficiency (void) { return mdGFPumpEfficiency; }

	void SetReservoirPermeability(double darcys) { setPositiveValue(mdReservoirPermeability, darcys, "Reservoir Permeability", m_strErrMsg); }
	double GetReservoirPermeability (void) { return mdReservoirPermeability; }

	void SetReservoirHeightM(double meters) { setPositiveValue(mdReservoirHeightM, meters, "Reservoir Height", m_strErrMsg); }
	double GetReservoirHeightM (void) { return mdReservoirHeightM; }

	void SetReservoirWidthM(double meters) { setPositiveValue(mdReservoirWidthM, meters, "Reservoir Width", m_strErrMsg); }
	double GetReservoirWidthM (void) { return mdReservoirWidthM; }

	void SetWaterLossPercent(double percent) { setZeroTo1(mdWaterLossPercent, percent, "Percent Water Loss", m_strErrMsg); }
	double GetWaterLossPercent (void) { return mdWaterLossPercent; }

	void SetDiameterPumpCasingInches(double inches) { setPositiveValue(mdDiameterPumpCasingInches, inches, "Diameter of the Pump Casing", m_strErrMsg); }
	double GetDiameterPumpCasingInches (void) { return mdDiameterPumpCasingInches; }

	void SetDiameterProductionWellInches(double inches) { setPositiveValue(mdDiameterProductionWellInches, inches, "Diameter of the Production Well Inches", m_strErrMsg); }
	double GetDiameterProductionWellInches (void) { return mdDiameterProductionWellInches; }

	void SetDiameterInjectionWellInches(double inches) { setPositiveValue(mdDiameterInjectionWellInches, inches, "Diameter of the Injection Well", m_strErrMsg); }
	double GetDiameterInjectionWellInches (void) { return mdDiameterInjectionWellInches; }

	// to make this 'thread-safe', global variables have to be removed.
	// these public member vars take the place of the globals
	std::string m_strErrMsg;
	CGETEMGlobals m_oGG; // create the global object that provides access to all the constants

	// Added June 2011 for geothermal hourly model
	void SetPowerBlockParameters(const SPowerBlockParameters& pbp) { m_pbp = pbp; }
	SPowerBlockParameters GetPowerBlockParameters(void) { return m_pbp; }
	void SetWeatherFileName( const char * fn) { mcFileName = fn;}
	void SetPowerBlockInputs(const SPowerBlockInputs& pbi) { m_pbi = pbi; }
	void SetTOUPeriodArray(int * tou) { m_tou = tou; }
	int GetTOUForHour(int iHour) { return m_tou[iHour]; }

protected:
	// CAN'T BE SET TO ZERO
	double mdEGSFractureWidthM;									// meters
	double mdEGSFractureAperature;								// meters
	double mdEGSThermalConductivity;							// J/m-day-C
	double mdEGSSpecificHeatConstant;							// J/kg-C
	double mdEGSRockDensity;									// kg/m^3
	double mdEGSNumberOfFractures;
	double mdProductionFlowRateKgPerS;		// 70 kilograms per second in one well (fFlowRate in VBA)
	double mdGFPumpEfficiency;				// 0.6
	double mdReservoirPermeability;			// default = 0.05 darcy units, [2B.Resource&Well Input].D179
	double mdReservoirHeightM;				// default = 100 meters, [2B.Resource&Well Input].F180
	double mdReservoirWidthM;				// default = 500 meters, [2B.Resource&Well Input].F181
	double mdWaterLossPercent;
	int miModelChoice;						// 0=GETEM, 1=Power Block monthly, 2=Power Block hourly
	double mdDiameterPumpCasingInches;		// 9.625
	double mdDiameterProductionWellInches;	// 10;
	double mdDiameterInjectionWellInches;	// 10;
	double mdNumberOfWells;					// entered or calculated, depending on 'cb' (moved to 'protected' June 2011 for hourly modeling)


	double mdPlantEfficiency; // not in GETEM - essentially the ratio of plant brine effectiveness to max possible brine effectiveness

	bool mbCalculatePlantCost;					// yes or no
	double mdPlantCapitalCostPerKWUserInput;	// $s/kW				([2E.Conversion Systems] D68 for Binary or D200 for Flash)
	bool mbCalculatePlantOM;
	double mdOMCostsPlantUserInput;				//O&M costs in cents/kWh, related to plant ([2A.Scenario Input].D145)
	bool mbCalculateFieldOM;
	double mdOMCostsFieldUserInput;				//O&M costs in cents/kWh, related to field ([2A.Scenario Input].D146)

	// used in CGeoHourlyAnalysis
	double mdPumpDepthFt;
	double mdPumpSizeHP;
	double mdPumpSizeHPInjection;

	bool inputErrors(void);

	// Added June 2011 for geothermal hourly model
	const char * mcFileName;
	SPowerBlockParameters m_pbp;
	SPowerBlockInputs m_pbi;
	int * m_tou;
	bool IsHourly(void) { return (GetMakeupAnalysesPerYear() == 8760) ? true : false; }
	int GetMakeupAnalysesPerYear(void) { return (miModelChoice == 2) ? 8760 : 12; }
	bool ReturnGETEMResults(void) { return (miModelChoice == 0) ? true : false; }

private:
	double secondLawEfficiencyGETEM(void);

	// EGS values that are direct results of EGS inputs (no inputs needed)
	double EGSFractureSurfaceArea(void) { return mdEGSFractureWidthM * EGSFractureLength(); } //fFractureSurfaceArea, m^2
	double EGSFractureCrossSectionArea(void) { return mdEGSFractureWidthM * mdEGSFractureAperature; } //fCrossSectionalArea, m^2
	double EGSAlpha(void) { return GetEGSThermalConductivity() / (mdEGSSpecificHeatConstant * mdEGSRockDensity); } // fAlpha (m^2 per day) or (m^2 per hr)

	// These EGS function are used in EGS makeup calculations and in pumping calculations
	double flowTimePeriod(void) { return (IsHourly()) ? 60*60 /* hourly analysis uses hourly flow*/ : 60*60*24 /*monthly analysis uses daily flow*/; }
	double EGSFlowPerFracture(double tempC) { return ((mdProductionFlowRateKgPerS / m_oGG.EGSWaterDensity(tempC))/mdEGSNumberOfFractures)*flowTimePeriod(); } // m^3 per day or per hour
	double EGSVelocity(double tempC) { return EGSFlowPerFracture(tempC) / EGSFractureCrossSectionArea(); }		// m^3 per day / m^2 = m/day (or hour)
	double EGSLengthOverVelocity(double tempC) { return EGSFractureLength() / EGSVelocity(tempC); }				// m / m per day = days (or hours)

	double calcEGSTimeStar(double tempC) { return (pow(GetEGSThermalConductivity() * EGSFractureSurfaceArea()/(27 * m_oGG.EGSWaterDensity(tempC) * m_oGG.EGSSpecificHeat(tempC) * EGSFlowPerFracture(tempC)),2) / EGSAlpha()) + EGSLengthOverVelocity(tempC); }

	// private member variables
	double mdResourceDepthM;
    double mdTemperaturePlantDesignC;
	double mdEGSResourceTemperatureGradient;
    double mdTemperatureResourceC;
	double mdTemperatureEGSAmbientC; // Note in GETEM spreadsheet says that this is only used in calculating resource temp or depth.  However, if EGS calculations are based on depth, then resource temp is based on this number, so all power calcs are based on it as well

	double mdDesiredSalesCapacityKW;		// entered or calculated, linked to 'cb', like above

	double mdFractionOfInletGFInjected;		// set from flash brine effectiveness for flash, or 1 for binary
	double mdFractureLengthAdjustment;		// used for one instance of where the EGS fracture length is used.  All others use the original fracture length

	CGeoFluidContainer moGFC;
	makeupAlgorithmType mat;  // only the 'determineMakeupAlgorithm()' function can change this

	double exitTempLowF(void){ return (0.8229 * GetTemperaturePlantDesignF()) - 127.71; }
	double exitTempHighF(void) { return (0.00035129 * pow(GetTemperaturePlantDesignF(),2)) + (0.69792956 * GetTemperaturePlantDesignF()) - 159.598; }
};


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Declaration of CPumpPowerCalculator /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class CPumpPowerCalculator
{
public:
	CPumpPowerCalculator(void) { mpGBI = NULL; }
	void init(CGeoHourlyBaseInputs* gbi);		// pass in a pointer to the CGeoHourlyBaseInputs object
	virtual ~CPumpPowerCalculator(void){}
	
	
	double GetTotalPumpPower(std::string sErr); // watt-hr/lb
	double DiameterPumpCasingFt(void) { return mpGBI->GetDiameterPumpCasingInches()/12; }
	double DiameterProductionWellFt(void) { return mpGBI->GetDiameterProductionWellInches()/12; }
	double DiameterInjectionWellFt(void) { return mpGBI->GetDiameterInjectionWellInches()/12; }

	double mdAdditionalPressure;					// manually enter additional psi for injection pumps
	double reservoirAreaSqM(void) { return mpGBI->GetReservoirHeightM() * mpGBI->GetReservoirWidthM(); }
	double reservoirAreaSqFt(void) { return MetersToFeet(mpGBI->GetReservoirHeightM()) * MetersToFeet(mpGBI->GetReservoirWidthM()); }
	double mdPumpSetDepthFt;						// if user wants to override calculation, use this input
	double mdCT;									// these are both inputs that are shaded out in GETEM
	double mdCP;									//	"		"			"			"			"
	
	// Only used for pumping energy and cost calculations.  For EGS this can be calculated based on desired temp, and a temp gradient
					
	bool mbProductionWellsPumped;
	bool mbAdditionalPressureRequired;
	bool mbCalculatePumpDepth;
	double GetCalculatedPumpDepthInFeet(void);
	double pumpHeadFt(void);
	double GetCalculatedPumpSizeHP(void) { return pumpSizeInHP(mpGBI->flowRatePerWell(), pumpHeadFt(), mpGBI->GetGFPumpEfficiency(), mpGBI->m_strErrMsg); }


	// has to be public so we can show the value to users
	double GetReservoirTemperatureF(void) { return (mpGBI->rt == EGS) ? mpGBI->EGSAverageReservoirTemperatureF() : mpGBI->GetResourceTempF(); }	// G54 on [7B.Reservoir Hydraulics]
	double GetPressureChangeAcrossReservoir();		// [7B.Reservoir Hydraulics].G70
	double GetBottomHolePressure(void);				// [7B.Reservoir Hydraulics].G75


private:
	CGeoHourlyBaseInputs* mpGBI;

	// Convert foot-lbs per hour to watt-hr/lb and include pump efficiency
	double productionPumpPower(void);	// watt-hr per lb of flow
	double frictionFactor(double Re) { return pow((0.79 * log(Re) - 1.640),-2);}

	double pressureWellHeadPSI(void);				// [7A.GF Pumps].G61
	double pressureInjectionWellBottomHolePSI();	// [7B.Reservoir Hydraulics].G72, [7A.GF Pumps].G50
	double pressureHydrostaticPSI(void);			// [7B.Reservoir Hydraulics].G17

	double mdBottomHolePressure;
	bool mbBottomHolePressureCalculated;

	// used so that pressure change is only calculated once
	double mdPressureChangeAcrossReservoir;
	bool mbPressureChangeCalculated;

	// production wells
	double productionTempF(void) { return CelciusToFarenheit(mpGBI->GetTemperaturePlantDesignC()); }
	double productionDensity(void) { return 1/mpGBI->m_oGG.oSVC.evaluatePolynomial(productionTempF()); }
	double productionFlowRate(void) { return (mpGBI->flowRatePerWell()/productionDensity())/3600; } // lbs per hr / lbs per cf = cf/hr
	double productionViscosity(void) { return 0.115631 * pow(productionTempF(),-1.199532); } // seems like this is resource temp in spreadsheet!

	// resource depth
	double GetResourceDepthM(void)       { return mpGBI->GetResourceDepthM(); } 
	double GetResourceDepthFt(void)      { return mpGBI->GetResourceDepthFt(); }
	double GetProductionWellDepthFt(void){ return mpGBI->GetResourceDepthFt(); }
	double GetInjectionWellDepthFt(void) { return mpGBI->GetResourceDepthFt(); }

	// Calculate injection pump items
	double getInjectionTempForResource(void) { return (mpGBI->rt == EGS) ? mpGBI->mdTemperatureEGSInjectionC : mpGBI->injectionTemperatureC(); }		// D15 - degrees C
	double injectionTempF(void) { return CelciusToFarenheit(getInjectionTempForResource()); }															// G15 - degrees F
	double injectionDensity(void) { return (1 / mpGBI->m_oGG.oSVC.evaluatePolynomial(injectionTempF())); }											// G19,G44, G128 - lb/ft^3
	double pZero(void) { return mpGBI->m_oGG.oPC.evaluatePolynomial(injectionTempF()); }															// G16 - psi
	double waterLoss(void) { return (1/(1 - mpGBI->GetWaterLossPercent())); }																				// G130 - lb/hr

	double injectionPumpPower(void)  { return (mbAdditionalPressureRequired) ? GetInjectionPumpPower() : 0; }
	double GetInjectionPumpPower(void)  { return pumpWorkInWattHr(waterLoss(), injectionPumpHeadFt(), mpGBI->GetGFPumpEfficiency(), mpGBI->m_strErrMsg) * mpGBI->GetFractionOfInletGFInjected(); } // ft-lbs/hr
	double injectionPumpHeadFt(void) { return injectionPressure() * 144 / injectionDensity(); }															// G129
	double injectionPressure(void) { return (mdAdditionalPressure >= 0) ? mdAdditionalPressure : calcInjectionPressure(); }
	double calcInjectionPressure(void);																													// G40

};


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Declaration of CFlashBrineEffectiveness /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class CFlashBrineEffectiveness
{
public:
	CFlashBrineEffectiveness(void);
	void init(CGeoHourlyBaseInputs* gbi);
	virtual ~CFlashBrineEffectiveness(void){}

	double brineEffectiveness(void); // these are the only public functions
	double waterLossFractionOfGF(void);

	condenserTypes mCondenserType;
	ncgRemovalTypes mNCGRemovalType;
	double mdNCGLevelPPM;
	double mdDeltaPressureHPFlashPSI;
	double mdDeltaPressureLPFlashPSI;
	double mdEfficiencyTurbine;
	double mdEfficiencyGenerator;
	double mdBaseCWPumpHeadFt;
	double mdDeltaTemperatureCWF;
	double mdTemperaturePinchPtCondenserF;
	double mdTemperaturePinchPtCoolingTowerF;
	int miNumberOfCoolingStages;
	double mdMoleWeightNCG;
	double mdMoleWeightH2O;
	double mdEfficiencyPump;
	double mdEfficencyVacuumPump;
	double mdPressureCondenserNCGPartialInHG;


private:
	CGeoHourlyBaseInputs* mpGBI;

	bool TempConstraint(void) { return ((mpGBI->ft == DUAL_FLASH_WITH_TEMP_CONSTRAINT) || (mpGBI->ft == SINGLE_FLASH_WITH_TEMP_CONSTRAINT)) ; }
	int FlashCount(void) { return (mpGBI->ft >= DUAL_FLASH_NO_TEMP_CONSTRAINT) ? 2 : 1; }
	double TemperatureWetBulbF(void) { return CelciusToFarenheit(mpGBI->mdTemperatureWetBulbC); }

	
	bool mbFlashPressuresCalculated;
	bool mbBrineEffectivenessCalculated;
	double mdFlashBrineEffectiveness;

	double mdPressureHPFlashPSI; // D29, D64
	double mdPressureLPFlashPSI; // D30, D65

	double temperatureCondF(void) { return TemperatureWetBulbF() + mdDeltaTemperatureCWF + mdTemperaturePinchPtCondenserF + mdTemperaturePinchPtCoolingTowerF; }	// D71 - deg F
	double pressureSaturation(void) { return mpGBI->m_oGG.oPSatConstants.evaluatePolynomial(temperatureCondF()); }												// D72 - psi
	double pressureCondenser(void) { return pressureSaturation() + InHgToPsi(mdPressureCondenserNCGPartialInHG); }													// D74 - psi


//////////////////////////////////////// Flash Pressures ///////////////////////////////////////////////////
	double tempFlashLimitF(void) { return CelciusToFarenheit(mpGBI->m_oGG.oFlashTempConstants.evaluatePolynomial(mpGBI->GetResourceTemperatureC())); }				// D26 - deg F
	double pressureFlashAmorphousSilica(void) { return mpGBI->m_oGG.oPC.evaluatePolynomial(tempFlashLimitF()); }												// D27 - psi
	double pressureSingleNoConstraint() { return (0.0207 * temperatureCondF() - 0.8416) * exp(0.0334*pow(temperatureCondF(),-0.1732) * mpGBI->GetTemperaturePlantDesignC()); } // Q64
	double pressureSingleWithConstraint() { return (pressureSingleNoConstraint() < pressureFlashAmorphousSilica()) ? pressureFlashAmorphousSilica() : pressureSingleNoConstraint(); } // S64
	double pressureSingleToTest(void) { return (TempConstraint()) ? pressureSingleWithConstraint() : pressureSingleNoConstraint(); }							// Q64 or S64
	double pressureSingle(void) { return (pressureSingleToTest() < mpGBI->mdPressureAmbientPSI) ? mpGBI->mdPressureAmbientPSI : pressureSingleToTest(); }							// O64

	double pressureDualHighNoConstraint() { return mpGBI->m_oGG.oFlashConstants1.evaluatePolynomial(temperatureCondF()) * exp(mpGBI->m_oGG.oFlashConstants2.evaluatePolynomial(temperatureCondF()) * mpGBI->GetTemperaturePlantDesignC()); } // R64
	double pressureDualHighWithConstraint();																														// T64
	double pressureDualHigh(void) { return (TempConstraint()) ? pressureDualHighWithConstraint() : pressureDualHighNoConstraint(); }							// P64
	double pressureDualLowUnconstrained() { return (0.12632*exp(0.01918 * temperatureCondF())) * exp((0.0146 * exp(-0.00205*temperatureCondF()) * mpGBI->GetTemperaturePlantDesignC())); } // R65
	double pressureDualLowConstrained() { return (pressureDualLowUnconstrained() < pressureFlashAmorphousSilica()) ? pressureFlashAmorphousSilica() : pressureDualLowUnconstrained(); } // T65
	double pressureDualLowToTest(void) { return (TempConstraint()) ? pressureDualLowConstrained() : pressureDualLowUnconstrained(); }							// R65 or T65
	double pressureDualLow(void) { return  (pressureDualLowToTest() < mpGBI->mdPressureAmbientPSI) ? mpGBI->mdPressureAmbientPSI : pressureDualLowToTest(); }						// P65
	void calculateFlashPressures(void);


//////////////////////////////////////// Turbine Output ///////////////////////////////////////////////////
	double calculateDH(double pressureIn);
	double calculateX(double enthalpyIn, double temperatureF);

	double enthalpyPlantDesignTemp(void) { return mpGBI->m_oGG.GetFlashEnthalpyF(CelciusToFarenheit(mpGBI->GetTemperaturePlantDesignC())); }					// D69
	double enthalpyChangeTurbine(double dEnthalpyDeltaInitial, double dEnthalpyTurbineG);																			// I65-I80, I87-I102

	// Turbine 1 - high pressure
	double turbine1dHInitial(void) { return calculateDH(mdPressureHPFlashPSI - mdDeltaPressureHPFlashPSI); }														// I65
	double turbine1TemperatureF(void) { return mpGBI->m_oGG.GetFlashTemperature(mdPressureHPFlashPSI); }														// D80
	double turbine1EnthalpyF(void) { return  mpGBI->m_oGG.GetFlashEnthalpyF(turbine1TemperatureF()); }															// D81
	double turbine1EnthalpyG(void) { return  mpGBI->m_oGG.GetFlashEnthalpyG(turbine1TemperatureF()); }															// D82
	double turbine1DH(void) { return enthalpyChangeTurbine(turbine1dHInitial(), turbine1EnthalpyG()); }																// I80 - btu/lb
	double turbine1HEx(void) { return turbine1EnthalpyG() - turbine1DH(); }																							// I81 - btu/lb
	double turbine1X(void) { return calculateX(enthalpyPlantDesignTemp(), turbine1TemperatureF()); }																// D83 - %
	double turbine1Steam(void) { return mpGBI->m_oGG.mGeothermalFluidForFlash() * turbine1X(); }																										// D85 - lb/hr
	double turbine1NetSteam(void) { return turbine1Steam() - mForNCGRemoval(); }																					// I82 lb/hr
	double turbine1OutputKWh(void) { return turbine1DH() * turbine1NetSteam() / 3413; }																				// I83 - kW/hr = (btu/lb) * (lb/hr) / (btu/kW)

	// Turbine 2 - low pressure
	double turbine2dHInitial(void) { return calculateDH(mdPressureLPFlashPSI - mdDeltaPressureLPFlashPSI); }														// I87
	double turbine2TemperatureF(void) { return mpGBI->m_oGG.GetFlashTemperature(mdPressureLPFlashPSI); }														// D88
	double turbine2EnthalpyF(void) { return mpGBI->m_oGG.GetFlashEnthalpyF(turbine2TemperatureF()); }															// D89
	double turbine2EnthalpyG(void) { return mpGBI->m_oGG.GetFlashEnthalpyG(turbine2TemperatureF());}															// D90
	double turbine2DH(void) { return enthalpyChangeTurbine(turbine2dHInitial(), turbine2EnthalpyG()); }																// I102 - btu/lb
	double turbine2HEx(void) { return turbine2EnthalpyG() - turbine2DH(); }																							// I103 - btu/lb
	double turbine2X(void) { return calculateX(turbine1EnthalpyF(), turbine2TemperatureF()); }																		// D91 %
	double turbine2Steam(void) { return (FlashCount() == 2) ? mpGBI->m_oGG.mGeothermalFluidForFlash() * turbine2X() * (1-turbine1X()) : 0; }																						// I104, D93 - lb/hr
	double turbine2OutputKWh(void) { return turbine2DH() * turbine2Steam() / 3413; }																				// I105 - kW/hr


//////////////////////////////////////// NCG Removal ///////////////////////////////////////////////////////////
	double pTotal(void) { return (IMITATE_GETEM) ? pressureSaturation() + (mdPressureCondenserNCGPartialInHG * 0.49) : pressureCondenser(); } // calculated separately in spreadsheet, but mathematically equivalent to pressureCondenser					   D150,D74 - psi
	double pRatio(void) { return exp(log(mpGBI->mdPressureAmbientPSI / (pTotal()))/miNumberOfCoolingStages); }																// D151
	double ncgFlowLbsPerHour(void) { return mpGBI->m_oGG.mGeothermalFluidForFlash() * this->mdNCGLevelPPM / 1000000; }										// D152 - lbs/hour
	double ncgFlowMolesPerHour(void) { return ncgFlowLbsPerHour() / mdMoleWeightNCG; }																				// D162... - moles/hr
	double pSuction(int stage) { return pTotal() * pow(pRatio(),stage-1); }																							// D165, D214
	double pInter(int stage,  std::string sErr);																																		// D156, D205, D253 - psi
	double prJet(int stage){ return pInter(stage, mpGBI->m_strErrMsg) / pInter(stage - 1, mpGBI->m_strErrMsg); }																							// D157, D206, D254
	double h2oMolesPerHour(int st) { return ncgFlowMolesPerHour() / ((pSuction(st)/pressureSaturation()) - 1); }													// D163, D212, D260 - moles/hr
	double totalVentFlow(int st) { return ncgFlowLbsPerHour() + (h2oMolesPerHour(st) * mdMoleWeightH2O); }															// D161, D210, D258
	double moleWeightVent(int st) { return totalVentFlow(st) /(ncgFlowMolesPerHour() + h2oMolesPerHour(st)); }														// D164, D213, D261
	double suctionSteamRatio(int st) { return pSuction(st) / mdPressureHPFlashPSI; }																				// D167, D216, D264
	double AR(int stage) { return ((3.5879 * pow(prJet(stage),-2.1168)) + 0.1) * pow(suctionSteamRatio(stage),(-1.155 * pow(prJet(stage),-0.0453))); }				// D168, D217, D265
	double ERd(int stage) { return (1.0035 * AR(stage) + 8.9374)* pow(suctionSteamRatio(stage),(2.9594* pow(AR(stage),-0.8458) + 0.99)); }							// D169, D218, D266
	double ER(int st) { return ERd(st) * pow((((460 + mpGBI->m_oGG.GetFlashTemperature(mdPressureHPFlashPSI)) * moleWeightVent(st))/((temperatureCondF() + 460) * mdMoleWeightH2O)),0.5); } // D170, D219, D267
	double steamFlow(int st) { return (st >= 3 && (mNCGRemovalType != JET || miNumberOfCoolingStages < 3)) ? 0 : totalVentFlow(st) / ER(st); }																						// D171, D220, D268 - lb/hr
	double mForNCGRemoval(void);																																	// D302

	
//////////////////////////////////////// CW Pump Power KW //////////////////////////////////////////////////////
	double pumpWorkKW(double flowLbPerHr, double pumpHeadFt) { return HPtoKW((flowLbPerHr * pumpHeadFt)/(60 * 33000 * mdEfficiencyPump)); }
	double overAllHEx(void);																																		// I107

	// Main Pump Power
	double deltaPressureCondenserFt() { return (mCondenserType == SURFACE) ? mpGBI->m_oGG.additionalCWPumpHeadSurface() : PSItoFT(mpGBI->mdPressureAmbientPSI + 1 - (pressureCondenser())); } // O102
	double cwPumpHead(void) { return mdBaseCWPumpHeadFt + deltaPressureCondenserFt(); }																				// D110 - ft
	double overAllSteam(void) { return (this->FlashCount() == 2) ? turbine1NetSteam() + turbine2Steam() : turbine1NetSteam(); }										// D96
	double qCondenser(void) { return overAllSteam() * (overAllHEx() - mpGBI->m_oGG.GetFlashEnthalpyF(temperatureCondF())); }									// D99
	double cwFlow(void) { return qCondenser() / this->mdDeltaTemperatureCWF; }																						// D114
	double mainCWPumpPowerKW(void) { return pumpWorkKW(cwFlow(), cwPumpHead()); }																					// part of I116
	

	// CW Pump Work
	double h2oVentFlow(int stage) { return h2oMolesPerHour(stage) * mdMoleWeightH2O; }																				// D160 - lb/hr
	double moleRatio(int st) { return (pInter(st, mpGBI->m_strErrMsg) / pressureSaturation()) - 1; }																					// D184,
	double flowSteamMolesPerHr(int st) { return ncgFlowMolesPerHour() / moleRatio(st); }																			// D186,
	double flowSteamLbPerHr(int st) { return flowSteamMolesPerHr(st) * mdMoleWeightH2O; }																			// D187,  - lb/hr
	double condensedSteamLbPerHour(int stage) { return steamFlow(stage) + h2oVentFlow(stage) - flowSteamLbPerHr(stage); }											// D188 = D171+D160-D187 = stage1CondensedSteam (lb/hr)
	double pumpWorkFromSteamFlow(double flow);																														// D189-D198, 
	double cwPumpWorkKWByStage(int st) { return pumpWorkFromSteamFlow(condensedSteamLbPerHour(st)); }																// D199 - kW
	double cwPumpWorkKW(void) { return cwPumpWorkKWByStage(1) + cwPumpWorkKWByStage(2) + cwPumpWorkKWByStage(3); }													// D305 - kW, part of I116
	double cwPumpingKW() { return mainCWPumpPowerKW() + cwPumpWorkKW(); }


//////////////////////////////////////// Condensate Pump Power KW //////////////////////////////////////////////

	double condensatePumpHead(void) { return PSItoFTB(mpGBI->mdPressureAmbientPSI + 1 - pressureCondenser()) + mdBaseCWPumpHeadFt; }										// D121
	double condensatePumpPowerKW(void) { return pumpWorkKW(overAllSteam(), condensatePumpHead()); }																	// D125->kw, part of I117

	double condensatePumpHeadByStage(int st) { return PSItoFTB(mpGBI->mdPressureAmbientPSI + 1 - pInter(st, mpGBI->m_strErrMsg)); }																// D201, D249, D297
	double condensatePumpWorkByStage(int st) { return pumpWorkKW(condensedSteamLbPerHour(st), condensatePumpHeadByStage(st)); }										// D203, ... kW
	double totalCondensatePumpWorkKW(void) { return condensatePumpWorkByStage(1) + condensatePumpWorkByStage(2) + condensatePumpWorkByStage(3);	}					// D306 - kW

	double condensatePumpingKW(void) { return condensatePumpPowerKW() + totalCondensatePumpWorkKW(); }																// I117 - kW


//////////////////////////////////////// Fan Power KW //////////////////////////////////////////////////////////
	double qRejectByStage(int stage) { return condensedSteamLbPerHour(stage) * (mpGBI->m_oGG.GetFlashEnthalpyG(temperatureCondF()) - mpGBI->m_oGG.GetFlashEnthalpyF(temperatureCondF())); } // D190
	double qRejectTotal(void) { return qRejectByStage(1) + qRejectByStage(2) + qRejectByStage(3); }																	// D303
	double qRejectedTower(void) { return qCondenser() + qRejectTotal(); }																							// D101

	double fanPowerCoeffA(void) { return -2.0814 * log(mdDeltaTemperatureCWF) + 10.6013; }																			// O95
	double fanPowerCoeffB(void) { return -0.0188 * pow(mdDeltaTemperatureCWF,0.0232); }																				// P95
	double fanPower(void) { return fanPowerCoeffA() * exp(this->TemperatureWetBulbF() * fanPowerCoeffB()); }														// D103 - hp per MMBtu/hr
	double fanPowerKW(void) { return HPtoKW(fanPower() * qRejectedTower() / 1000000); }																				// D105, I118


//////////////////////////////////////// Vacuum Pump Power KW //////////////////////////////////////////////////
	double deltaPressureByStage(int st) { return pInter(st, mpGBI->m_strErrMsg) - pSuction(st); }																						// D173, D222, D270 - psi
	double densityForVacuumPump(int st) { return pSuction(st) * moleWeightVent(st) /((temperatureCondF() + 460)*10.7316); }											// D166, D215, D263 - lb/ft^3
	double vaccumPumpHead(int st) { return deltaPressureByStage(st) * 144 / densityForVacuumPump(st); }																// D175, D224, D272 - ft
	double vacuumPumpWorkByStage(int st) { return (mNCGRemovalType == VAC_PUMP || (st == 3 && mNCGRemovalType == HYBRID)) ? pumpWorkKW(totalVentFlow(st),  vaccumPumpHead(st)) : 0; } // D178, D227, D275 - kW
	double vacuumPumpingKW(void) { return vacuumPumpWorkByStage(1) + vacuumPumpWorkByStage(2) + vacuumPumpWorkByStage(3); }											// D307, I119


//////////////////////////////////////// Condenser Injection Pump Power KW /////////////////////////////////////
	double injectionDeltaP(void) { return (FlashCount() == 1) ? mdPressureHPFlashPSI - mpGBI->mdPressureAmbientPSI : mdPressureLPFlashPSI - mpGBI->mdPressureAmbientPSI; }		// D127 - psi (condensate injection delta pressure)
	double injectionPumpHead(void) {return PSItoFT(injectionDeltaP()); }																							// D128 - ft
	double injCoeffA(void) { return -0.0001769 * log(mdDeltaTemperatureCWF) + 0.0011083; }																			// R95
	double injCoeffB(void) { return  0.0657628 * log(mdDeltaTemperatureCWF) - 0.4091309; }																			// S95
	double injCoeffC(void) { return -6.7041142 * log(mdDeltaTemperatureCWF) + 44.3438937; }																			// T95
	double injCoeffD(void) { return -0.0325112 * pow(mdDeltaTemperatureCWF,2) + (6.831236 * mdDeltaTemperatureCWF) - 64.6250943; }									// U95

	double evaporativeWaterLoss(void) { return ( (injCoeffA() * pow(TemperatureWetBulbF(),3)) + (injCoeffB() * pow(TemperatureWetBulbF(),2)) + (injCoeffC() * TemperatureWetBulbF()) + injCoeffD()) * qRejectedTower() / 1000000; } // D129 - lb/hr (evaporative water loss)
	double drift(void) { return 0.0005 * cwFlow(); }																												// D130
	double blowDown(void) { return evaporativeWaterLoss() /(mpGBI->m_oGG.injectionPumpingCycles() - 1) - drift(); }																	// D132
	double waterLoss(void) { return evaporativeWaterLoss() + drift() + blowDown(); }																				// D133
	double steamCondensate(void) { return (turbine1Steam() + turbine2Steam()) - waterLoss(); }																		// D135
	double steamCondensateInjected(void) { return (steamCondensate() < 0) ? 0 : steamCondensate(); }																// D136 - lb/hr
	double condenserInjectionPumpingKW() {	return pumpWorkKW(steamCondensateInjected(), injectionPumpHead()); }													// D138, I120 - kW

};
  




/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Declaration of CMakeupAlgorithm /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class CMakeupAlgorithm
{
public:
	
	CMakeupAlgorithm(void); // { miReservoirReplacements = 0; mdWorkingTemperatureC=0; moSecondLawConstants.init(130.8952, -426.5406, 462.9957, -166.3503, 0, 0, 0); } // by default, load Binary(EGS) secondLawConstants - let flash over write them
	virtual ~CMakeupAlgorithm(void){}
	virtual makeupAlgorithmType GetType(void)=0;	// this is an abstract class, it should never be created, only derived objects

	bool SetScenarioParameters( CGeoHourlyBaseInputs* gbi);
	virtual void calculateNewTemperature(void) { mdWorkingTemperatureC = mdWorkingTemperatureC * (1 - (mpGBI->mdTemperatureDeclineRate / 12)); } // For EGS temperature calculations, this virtual function is over-ridden by the EGS makeup algorithm class.
	bool wantToReplaceReservoir(void) { return ( mdWorkingTemperatureC < (mpGBI->GetResourceTemperatureC() - mpGBI->mdMaxTempDeclineC) ) ? true : false; }
	virtual bool canReplaceReservoir(double dTimePassedInYears) { return ( (miReservoirReplacements < mpGBI->NumberOfReservoirs() ) && (dTimePassedInYears + mpGBI->mdFinalYearsWithNoReplacement <= mpGBI->miProjectLifeYears) ) ? true : false; }
	virtual void replaceReservoir(void) { miReservoirReplacements++; mdWorkingTemperatureC = mpGBI->GetResourceTemperatureC(); }
	double plantGrossPower(void) {return (plantBrineEfficiency() * mpGBI->flowRateTotal() / 1000.0); }
	double plantNetPower(void) { return plantGrossPower() - mpGBI->GetPumpWorkKW(); } // kW, as a function of the temperature over time
	double plantNetPowerkW(void) { double pnp = plantNetPower(); return (pnp>0) ? pnp : 0; } // kW
	double plantNetPowerMW(void) { return plantNetPowerkW() / 1000.0; } // MW
	double GetWorkingTemperatureC(void) { return mdWorkingTemperatureC; }
	double TestValue(void) { return plantNetPower(); }
	std::string GetLastErrorMessage(void) { return m_strMAError; }

	// Added June 2011 for geothermal hourly model
	bool OpenWeatherFile(const char * fn);
	void SetPowerBlockFlowRateKgPerSec(double dFlowRateKgPerSec) { m_pbInputs.m_dot_htf = dFlowRateKgPerSec*3600.0; /* m_dot_htf should be in kg per hour */ }
	void SetPowerBlockInputs(const SPowerBlockInputs& pbi) { m_pbInputs = pbi; }
	bool ReadWeatherForTimeStep(const bool bHourly, unsigned int timeStep);
	void SetType224Inputs(void);
	double GetType224OutputkW(void);
	float WeatherPressure(void) { return (float)m_pbInputs.P_amb; }
	float WeatherDryBulb(void)  { return (float)m_pbInputs.T_db; }
	float WeatherWetBulb(void)  { return (float)m_pbInputs.T_wb; }

protected:
	double mdWorkingTemperatureC;
	int miReservoirReplacements;	// how many times the reservoir has been 'replaced' (holes redrilled)
	CGeoHourlyBaseInputs* mpGBI;		// use scenario parameters
	CGeothermalConstants moSecondLawConstants; //Used to calculate second law (of thermodynamics) efficiencies
	std::string m_strMAError;

	virtual double temperatureRatio(void) { return CelciusToKelvin(mdWorkingTemperatureC) / CelciusToKelvin(mpGBI->GetTemperaturePlantDesignC()); }
	virtual double plantBrineEfficiency() { return secondLawEfficiency() * mpGBI->GetAEBinaryAtTemp(mdWorkingTemperatureC); } // plant Brine Efficiency as a function of temperature
	double secondLawEfficiency() { return maxSecondLawEfficiency() * fractionOfMaxEfficiency(); } // separate step just to help with debugging (compare to spreadsheet) 

	double maxSecondLawEfficiency() { return  mpGBI->GetPlantBrineEffectiveness() /  getemAEForSecondLaw(); }
	double getemAEForSecondLaw(void) { return (IMITATE_GETEM) ? mpGBI->GetAEBinary() : mpGBI->GetAE() ; }  // GETEM uses the correct ambient temperature, but it always uses Binary constants, even if flash is chosen as the conversion technology
	
	// the available energy, in GETEM, is actually based on plant design temp, although resource temp is being used to calculate the output
	// this only matters for EGS resources, where resource temp and plant design temp are different
	// this leads to Plant brine effectiveness higher than input values
	// which leads to actual plant output(after pumping losses) > design output (before pump losses) ??
	// which leads to relative revenue > 1 ??
	
	virtual double fractionOfMaxEfficiency() { return(temperatureRatio() > 0.98) ? moSecondLawConstants.evaluatePolynomial(temperatureRatio()) : 1.0177 * pow(temperatureRatio(), 2.6237); }

	// Added June 2011 for geothermal hourly model
	SPowerBlockInputs m_pbInputs;
	CPowerBlock_Type224 m_pb;
	weatherfile m_wf;
	bool m_bWeatherFileOpen;
	long m_lReadCount;  // resource file reads through the year, 1 to 8760
	long m_lHourCount;	// hour of analysis (zero to yearsX8760); used to tell the Power Block how many seconds have passed.

private:
	bool ReadNextLineInWeatherFile(void);

};



/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Declaration and Implementation of CBinaryMakeup /////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class CBinaryMakeup : public CMakeupAlgorithm
{
public:

	CBinaryMakeup(void){ } //moSecondLawConstants.init(130.8952, -426.5406, 462.9957, -166.3503, 0, 0, 0); }// ("6Ab. Makeup-Annl%").Range("R24:R27")
	~CBinaryMakeup(void){}
	makeupAlgorithmType GetType(void) { return MA_BINARY; }; // this is the only function binary has to override

};



/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Declaration of CFlashMakeup /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class CFlashMakeup : public CMakeupAlgorithm
{
public:
	CFlashMakeup(void) { mbInitialized = false;}
	virtual ~CFlashMakeup(void){}

	makeupAlgorithmType GetType(void) { return MA_FLASH; }


private:
	double plantBrineEfficiency() { return secondLawEfficiency() * mpGBI->GetAEFlashAtTemp(mdWorkingTemperatureC); } // plant Brine Efficiency as a function of temperature
	double fractionOfMaxEfficiency();
	void initializeSecondLawConstants();
	bool mbInitialized;
};

 
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Declaration of CEGSMakeup /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class CEGSMakeup : public CMakeupAlgorithm
{
public:
	CEGSMakeup(void) { mdTimeOfLastReservoirReplacement=0; mdYearsAtNextTimeStep=0; mdCurrentEfficiency=0; }
	virtual ~CEGSMakeup(void){}

	makeupAlgorithmType GetType(void) { return MA_EGS; }

	// These functions over-ride the CMakeupAlgorithm functions  to update some values necessary for EGS calculations
	void calculateNewTemperature();
	bool canReplaceReservoir(double dTimePassedInYears);  // dTimePassedInYears -> mdYearsAtNextTimeStep
	void replaceReservoir();

private:
	double LastProducitonTemperatureF(void) { return CelciusToFarenheit(mdLastProductionTemperatureC); }  // shortcut for production temp in F
	double temperatureRatio(void) { return CelciusToKelvin(mdLastProductionTemperatureC) / CelciusToKelvin(mpGBI->GetTemperaturePlantDesignC()); }
	double plantBrineEfficiency(void); // over-ridden to update mdCurrentEfficiency
	double newEGSProductionTemperatureC() { return mpGBI->GetResourceTemperatureC() + ((newInjectionTemperatureC() - mpGBI->GetResourceTemperatureC()) * functionOfRockProperties()); }
	double newInjectionTemperatureC();
	double averageReservoirTempC(void) { return calcEGSAverageWaterTemperatureC(mdLastProductionTemperatureC, mdLastProductionTemperatureC, maxSecondLawEfficiency()); }
	double daysSinceLastReDrill() {	return (mdYearsAtNextTimeStep - mdTimeOfLastReservoirReplacement) * DAYS_PER_YEAR; } 
	double functionOfRockProperties() { return mpGBI->calcEGSReservoirConstant(averageReservoirTempC(), daysSinceLastReDrill()); }

	double mdLastProductionTemperatureC; // store the last temperature before calculating new one
	double mdCurrentEfficiency;
	double mdYearsAtNextTimeStep;
	double mdTimeOfLastReservoirReplacement;
};


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Declaration of CGeoHourlyAnalysis /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class CGeoHourlyAnalysis : public CGeoHourlyBaseInputs
{
public:
	CGeoHourlyAnalysis(void);
	virtual ~CGeoHourlyAnalysis(void);
	void init(void);  // initialize the objects used in this one

	double relativeRevenue(void) { return (analyzeIfNecessary()) ? mdLifeTimeDiscountedNetPower / mdLifeTimeDiscountedDesignPower : 0; }  // levelized average capacity factor, before being adjusted by the 'utilization factor'
	double lifeTimeDiscountedNetPower(void) { return (analyzeIfNecessary()) ? mdLifeTimeDiscountedNetPower : 0; }
	double lifeTimeDiscountedDesignPower(void) { return (analyzeIfNecessary()) ? mdLifeTimeDiscountedDesignPower: 0; }
	double energyProductionKWh(void) { return PowerSalesKW() * DAYS_PER_YEAR * 24 * relativeRevenue() * mdUtilizationFactor; }										// D81

	double levelizedFieldReplacementCostFactor(void) { return (analyzeIfNecessary()) ? mdPresentCostFactorFieldReplacements / mdSumOfPresentWorthFactors : 0; }
	double presentValueCostFactor(void) { return (analyzeIfNecessary()) ? mdPresentCostFactorFieldReplacements : 0; }
	double sumOfPresentWorthFactors(void) { return (analyzeIfNecessary()) ? mdSumOfPresentWorthFactors : 0; }

	bool readyToAnalyze(void);
	bool analyzeIfNecessary(void) {if (mbAnalysisRequired) return analyze(0, 0); else return true; }
	bool analyze( void (*update_function)(float, void*), void *user_data );

	void SetPumpDepthFt(double feet) { mdPumpDepthFt = feet; }
	double GetPumpDepthFt(void) { return (mdPumpDepthFt) ? mdPumpDepthFt : moPPC.GetCalculatedPumpDepthInFeet(); }
	void SetPumpSizeHP(double hp) { mdPumpSizeHP = hp; }
	double GetPumpSizeHP(void){ return (mdPumpSizeHP) ? mdPumpSizeHP : moPPC.GetCalculatedPumpSizeHP(); }
	void SetPumpSizeHPInjection(double hp) { mdPumpSizeHPInjection = hp; }
	double GetPumpSizeHPInjection(void) { return mdPumpSizeHPInjection; }

	// virtual functions in CGeoHourlyBaseInputs
	double GetPumpWorkWattHrPerLb(void) { return moPPC.GetTotalPumpPower(m_strErrMsg); } // small errors in pump work introduce biases throughout the results
	//double GetPlantBrineEffectiveness(void) { return (this->cst == FLASH) ? moFBE.brineEffectiveness() : 11.7414224664536; }
	double GetPlantBrineEffectiveness(void) { return (this->cst == FLASH) ? moFBE.brineEffectiveness() : GetMaxBinaryBrineEffectiveness() * mdPlantEfficiency; }
	double GetFractionOfInletGFInjected(void);  // used in CPumpPowerCalculator
	bool TimeToUpdateInterface(float dPercentDone, float iNotificationIntervalInPercent);


	// GETEM's "optimizer" seems to pick the max possible brine effectiveness for the default binary plant, so use this as a proxy for now
	double GetMaxBinaryBrineEffectiveness(void) { return GetAEMaxPossible() * ((GetTemperaturePlantDesignC() < 150) ? 0.14425 * exp(0.008806 * GetTemperaturePlantDesignC()) : 0.57); }


	// for use in the interface to show 'calculated' values
	double GetNumberOfProductionWells(void) { return this->GetNumberOfWells(); }
	double GetGrossPlantOutputMW(void) { return this->PlantOutputKW()/1000; }
	double GetNetPlantOutputMW(void) { return this->PowerSalesKW()/1000; }
	double GetPressureChangeAcrossReservoir(void) { return moPPC.GetPressureChangeAcrossReservoir(); }
	double GetAverageReservoirTemperatureUsedF(void) { return moPPC.GetReservoirTemperatureF(); }
	double GetBottomHolePressure(void) { return moPPC.GetBottomHolePressure(); }

	// Added June 2011 to use create an hourly model using TRNSYS Type 224 code
	void SetPointerToReplacementArray(float * p) { m_afReplacementsByYear = p; }

	void SetPointerToMonthlyTemperatureArray(float * p) { m_afMonthlyAvgTempC = p; }
	void SetPointerToMonthlyOutputArray(float * p) { m_afPowerByMonth = p; }
	void SetPointerToMonthlyPowerArray(float * p) { m_afEnergyByMonth = p; }

	void SetPointerToTimeStepTemperatureArray(float * p) { m_afTemperatureC = p; }
	void SetPointerToTimeStepOutputArray(float * p) { m_afPowerByTimeStep = p; }
	void SetPointerToTimeStepTestArray(float * p) { m_afTestValues = p; }

	void SetPointerToTimeStepPressureArray(float * p) {m_afPressure = p;}
	void SetPointerToTimeStepDryBulbArray(float * p)  {m_afDryBulb = p;}
	void SetPointerToTimeStepWetBulbArray(float * p)  {m_afWetBulb = p;}

private:
	CFlashBrineEffectiveness moFBE;
	CPumpPowerCalculator moPPC;

	CMakeupAlgorithm* moMA;
	bool mbAnalysisRequired;

	double mdLifeTimeDiscountedDesignPower;
	double mdLifeTimeDiscountedNetPower;
	double mdPresentCostFactorFieldReplacements;
	double mdSumOfPresentWorthFactors;

	// Changed in June 2011 to create an hourly model - these arrays will be hourly or monthly, depending on how analysis is being done
	// Tracked annually
	float * m_afReplacementsByYear; // array of ones and zero's over time, ones representing years where reservoirs are replaced

	// Tracked monthly, over lifetime of project
	float * m_afMonthlyAvgTempC;
	float * m_afPowerByMonth;
	float * m_afEnergyByMonth;

	// Could be monthly or hourly, depending on how analysis is being done
	float * m_afTemperatureC; // array of temperatures over time
	float * m_afPowerByTimeStep;
	float * m_afTestValues;   // array of values for debugging

	float * m_afPressure;
	float * m_afDryBulb;
	float * m_afWetBulb;
};



 
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Declaration of CGeoHourlyOutputs ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class CGeoHourlyOutputs : public CGeoHourlyAnalysis // which is derived from CGeoHourlyBaseInputs
{

public:
	CGeoHourlyOutputs(void);
	virtual ~CGeoHourlyOutputs(void){}

	// costs per kWh
	double dollarsPerKWhTotal(void) { return dollarsPerKWhCapitalTotal() + dollarsPerKWhOMTotal() + royaltyDollarsPerKWhr(); }						// $/kWh
	double dollarsPerKWhCapitalTotal(void) { return dollarsPerKWhCapitalUpFront() + dollarsPerKWhCaptialFuture(); }									// $/kWh
	double dollarsPerKWhCapitalUpFront(void) { return costPerKWh(capitalCostsUpFront()); }															// $/kWh
	double dollarsPerKWhCaptialFuture(void)  { return dollarsPerKWhWellFieldMakeup(); }																// $/kWh
	double dollarsPerKWhOMTotal(void) { return OMCostsTotal(); }																					// $/kWh    
	double royaltyDollarsPerKWhr(void) { return royaltyDollarsPerKWhrMinusContingencies() + royaltyOnContingencies(); }

	// for pie chart
	double dollarsPerKWhExploration(void) { return costPerKWh(capitalCostsExploration()); }
	double dollarsPerKWhConfirmation(void) { return costPerKWh(capitalCostsConfirmation()); }
	double dollarsPerKWhWellFieldCapital(void) { return costPerKWh(capitalCostsMainWells()); }
	double dollarsPerKWhWellFieldMakeup(void)  { return levelizedTotalMakeupCostDollars() / energyProductionKWh(); }								// $/kWh
	double dollarsPerKWhWellFieldOM(void)  { return GetOMCostsField(); }
	double dollarsPerKWhFieldOtherCaptial(void) { return costPerKWh(capitalCostsOtherField()); }
	double dollarsPerKWhFieldOtherOM(void) { return GetOMCostsOther(); }
	double dollarsPerKWhPlantCapital(void) { return costPerKWh(capitalCostsPlant()); }
	double dollarsPerKWhPlantOM(void) { return GetOMCostsPlant(); }
	double dollarsPerKWhRoyalty(void) { return royaltyDollarsPerKWhrMinusContingencies(); }
	double dollarsPerKWhContingency(void) { return costPerKWh(contingencyCosts()) + royaltyOnContingencies(); }

	// 'make up' capital expenses - future captial expenses due to field 'replacement' (re-drilling wells to tap a new part of resource)
	double levelizedTotalMakeupCostDollars(void) { return capitalCostFieldReplacement() * levelizedFieldReplacementCostFactor(); }					// $/year											// $s



	///////////// CAPITAL COSTS ///////////////////////////////////////////////////////////////////
	double capitalDollarsPerKW(void)      { return capitalCostsUpFront() / PowerSalesKW(); }																	// $/kW

	double capitalCostsUpFront(void)	{ return capitalCostsPlantAndField() + contingencyCosts(); }
	double capitalCostsPlantAndField(void)	{ return capitalCostsPlant() + capitalCostsField(); }
	double capitalCostsField(void)	{ return capitalCostsExploration() + capitalCostsConfirmation() + capitalCostsMainWells() + capitalCostsOtherField(); }		// not totaled on sheet
	double capitalCostsPlant(void)  { return PlantNetOutputKW() * GetPlantCapitalCostPerKW(); }																	// E68
	double contingencyCosts(void)	{ return capitalCostsPlantAndField() * mdContingencyFactor; }																// E69

	// field costs... on "4A.Binary System" or "5A.Flash-Steam System " sheet
	double capitalCostsExploration(void)  { return mdNonWellExplorationCosts + costExplorationVariable(); }														// E60
	double capitalCostsConfirmation(void) { return mdNonWellConfirmationCosts + costConfirmationVariable(); }													// E61
	double capitalCostsMainWells(void)    { return mdWellFieldNonDrillingCosts + costWellFieldDrilling(); }														// E62
	double capitalCostsOtherField(void)   { return costSurfaceEquipment() + costDownHolePumps() + costInjectionPumps() + costStimulation(); }					// E63
	
	// plant capital costs
	double GetPlantCapitalCostPerKW(void) { return (mbCalculatePlantCost) ? calculatedPlantCostDollarsPerKW() : mdPlantCapitalCostPerKWUserInput; }
	double calculatedPlantCostDollarsPerKW(void) { return calculatedPlantCostDollars() / PlantSizeKW(); }
	double calculatedPlantCostDollars(void) { return plantDirectCosts() + plantIndirectCosts(); }
	double plantDirectCosts(void) { return 10;  } // unfinished
	double plantIndirectCosts(void) { return 100; } // unfinished

	// exploration and confirmation - on "4A.Binary System" or "5A.Flash-Steam System " sheet
	double costPerExplorationWell(void)   { return costPerProductionWell() * mdExplorationWellCostMultiplier; }
	double costExplorationVariable(void)  { return costPerExplorationWell() * mdExplorationWells; }																// L30, D60 - not in 1000s or 1000000s
	double costPerConfirmationWell(void)  { return costPerProductionWell() * mdConfirmationWellCostMultiplier; }
	double costConfirmationVariable(void) { return costPerConfirmationWell() * mdConfirmationWells; }															// L31, D61

	// main wells - production and injection wells - on "4A.Binary System" or "5A.Flash-Steam System " sheet
	double costWellFieldDrilling(void)        { return costProductionWells() + costSpareProductionWells() + costInjectionWells(); }								// D62
	double costPerProductionWell(void)		  { return wellCostEstimate(GetResourceDepthFt(), pwccc); } 
	double costProductionWells(void)		  { return costPerProductionWell() * numberProductionWellsDrilled(); }												// L36
	double numberConfirmationWellsUsedForProduction(void) { return (mdConfirmationWells * mdConfirmationWellSuccessRate); }										// J32
	double numberProductionWellsDrilled(void) { return GetNumberOfWells() - numberConfirmationWellsUsedForProduction(); }										// J35
	double costSpareProductionWells(void)     { return costPerProductionWell() * mdNumberOfSpareProductionWells; }												// L37

	double numberInjectionWellsDrilled(void)  { return GetNumberOfWells() * mdRatioInjectionToProduction; }														// J38
	double costPerInjectionWell(void)		  { return wellCostEstimate(GetResourceDepthFt() * mdRatioInjectionDepthToProductionDepth, iwccc); }
	double costInjectionWells(void)           { return costPerInjectionWell() * numberInjectionWellsDrilled(); }												// L38


	// other field costs
	double totalWellsDrilled(void)    { return numberProductionWellsDrilled() + numberInjectionWellsDrilled(); }												// J49
	double costSurfaceEquipment(void) { return totalWellsDrilled() * mdSurfaceEquipmentCostPerWell; }															// C64
	double costDownHolePumps(void)    { return GetNumberOfWells() * productionPumpCost(); }																		// C65
	double costInjectionPumps(void)   { return mdPumpCostPerHP * ppiIndexPump() * pow(GetPumpSizeHPInjection(),0.5) * 1.4; }									// C66
	double costStimulation(void)      { return totalWellsDrilled() * mdWellStimulationCostPerWell; }															// C67

	// pump costs
	double productionPumpCost(void) { return pumpOnlyCost() + pumpInstallationCost(); } // + pumpCasingCost(); }
	double pumpOnlyCost(void) { return mdPumpCostPerHP * ppiIndexPump() * pow(GetPumpSizeHP(),0.5); }
	double pumpInstallationCost(void) { return mdPumpCostPerFoot * GetPumpDepthFt(); }
	//double pumpInstallationCost(void) { return (mdProductionPumpCostInstall * GetPumpDepthFt()) + mdProductionPumpCostOther; }
	//double pumpCasingCost(void) { return mdProductionPumpCostCasing * GetPumpDepthFt(); }
	

	// "makeup costs", I80 on 4A.Binary System and 5A.Flash-Steam System - cost to replace a reservoir (confirmation wells, production wells, spare production wells, injection wells, downhole pumps, field piping, etc.)
	double capitalCostFieldReplacement(void) { return costWellFieldDrilling() + costConfirmationVariable() + costDownHolePumps() + costSurfaceEquipment(); }	// I80


	/////////// O & M COSTS ///////////////////////////////////////////////////////////////////////
	double annualCostsOM(void){ return dollarsPerKWhOMTotal() * energyProductionKWh(); }																		// $ per year = ($/kWh) * (kWh/yr)
	double OMCostsTotal(void) { return GetOMCostsField() + GetOMCostsPlant() + GetOMCostsOther(); } // $s per kWh
	double GetOMCostsField(void) { return (mbCalculateFieldOM) ? 0 : mdOMCostsFieldUserInput/100; } // $s per kWh
	double GetOMCostsPlant(void) { return (mbCalculatePlantOM) ? 0 : mdOMCostsPlantUserInput/100; } // $s per kWh
	double GetOMCostsOther(void) { return 0; }	// $s/kWh - O&M costs related to non-well field costs - NOT an input, it's calculated by GETEM if other O&M costs are calculated (not yet an option in SAM)

	
	/////////// Other COSTS ///////////////////////////////////////////////////////////////////////
	double royaltyCostFunctionGETEM(double inCost) { return inCost * mdRoyaltyPercent / energyProductionKWh(); }  // used in GETEM equaitons with wierd units
	double royaltyCostFunctionNew(double inCostPerKWh) { return inCostPerKWh * 1.01 * 0.0175; }
	double royaltyOnContingencies(void) { return (IMITATE_GETEM) ? royaltyCostFunctionGETEM(1000 * annualize(contingencyCosts()) / PowerSalesKW()) : royaltyCostFunctionNew(costPerKWh(contingencyCosts())); }
	double royaltyDollarsPerKWhrMinusContingencies(void);


	double ppiIndexWell(void) { return 2.34325522482584; } // should eveuntually return the PPI from "tables" sheet in GETEM
	double ppiIndexPump(void) { return 1.28009127210496; } // should eveuntually return the PPI from "tables" sheet in GETEM

	void SetProductionWellCostCurve(wellCostCurveChoices cc) { pwccc = cc; }
	wellCostCurveChoices GetProductionWellCostCurve(void) { return pwccc; }

	void SetInjectionWellCostCurve(wellCostCurveChoices cc) { iwccc = cc; }
	wellCostCurveChoices GetInjectionWellCostCurve(void) { return iwccc; }

private:
	// Private Members
	// sources for capital costs
	wellCostCurveChoices pwccc;								// production well cost curve choice - NO_COST_CURVE, LOW, MED, HIGH
	wellCostCurveChoices iwccc;								// injection well cost curve choice - NO_COST_CURVE, LOW, MED, HIGH
	double wellCostEstimate(double depth, wellCostCurveChoices costCurve);

	double annualize(double inCost) { return inCost * mdFixedChargeRate; }
	double costPerKWh(double inCost) { return annualize(inCost) / energyProductionKWh(); }
};
 



#endif // __geothermalMemberClassDefinitions__