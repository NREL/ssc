// define classes
#include "lib_geothermal.h"

namespace geothermal
{

const double MAX_TEMP_RATIO = 1.134324;  // max valid value for (resource temp)/(plant design temp) where both are measured in Kelvin
const double DEFAULT_AMBIENT_TEMPC_BINARY = 10.0;			// degrees C
const double AMBIENT_TEMPC_FOR_GRADIENT = 10.0;				// degrees C, embedded in [2B.Resource&Well Input].D14
const double WET_BULB_TEMPERATURE_FOR_FLASH_CALCS = 15.0;	// degrees C, used in Flash calcs brine effectiveness calcs an flash injection temperature calcs
const bool ADDITIONAL_PRESSURE_REQUIRED = false;
const double PUMP_EFFICIENCY = 0.6;
const double EGS_THERMAL_CONDUCTIVITY = 3*3600*24;				// J/m-day-C
const double PRESSURE_CHANGE_ACROSS_SURFACE_EQUIPMENT_PSI = 25;	// 25 psi [2B.Resource&Well Input].D146, H146
const double TEMPERATURE_EGS_INJECTIONC = 76.1;					// degrees C, [7C.EGS Subsrfce HX].D11 [should be a function of plant design temperature]
const double TEMPERATURE_EGS_AMBIENT_C = 15.0;					// Note in GETEM spreadsheet says that this is only used in calculating resource temp or depth.  However, if EGS calculations are based on depth, then resource temp is based on this number, so all power calcs are based on it as well
const double CONST_CT = 0.0009;									// these are both inputs that are shaded out in GETEM
const double CONST_CP = 0.000000000464;							//	"		"			"			"			"
const double EXCESS_PRESSURE_BAR = 3.5;							// default 3.5 bar, [2B.Resource&Well Input].D205
const double PRESSURE_AMBIENT_PSI = 14.7; // default
const double RESERVOIR_DELTA_PRESSURE = 0.35;					// default = .35 psi-h per 1000 lb [2B.Resource&Well Input].D171
const double WATER_LOSS_PERCENT = 0.02;							// 2%
const double EGS_TIME_INPUT = 3.076;							// years, not really explained - user is supposed to vary input until a calculated value equals plant design temp [7C.EGS Subsrfce HX].D42 (fTimeStar)
const double FRACTURE_LENGTH_ADJUSTMENT = 2;					// used for one instance of where the EGS fracture length is used.  All others use the original fracture length
const double DELTA_PRESSURE_HP_FLASH_PSI = 2.2;
const double DELTA_TEMPERATURE_CWF = 30.0;						// degrees F
const double TEMPERATURE_PINCH_PT_CONDENSER_F = 10.0;
const double TEMPERATURE_PINCH_PT_COOLING_TOWER_F = 15;
const double PRESSURE_CONDENSER_NCG_PARTIAL_INHG = 0.5;			// inches of Mercury
const double GEOTHERMAL_FLUID_FOR_FLASH = 1000;					// D67 in "5C.Flash-Steam Plant Perf" [was an integer, not sure why]
const double EFFICIENCY_TURBINE = 0.825;
const ncgRemovalTypes NCG_REMOVAL_TYPE = JET;
const int NUMBER_OF_COOLING_STAGES = 3;							// 1,2, or 3







/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// GETEM Physics and general equations
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
const bool IMITATE_GETEM = false;
const double GETEM_FT_IN_METER = (IMITATE_GETEM) ? 3.28083 : physics::FT_PER_METER; // feet per meter - largest source of discrepancy
const double GETEM_PSI_PER_BAR = (IMITATE_GETEM) ? 14.50377 : physics::PSI_PER_BAR; // psi per bar
const double GETEM_PSI_PER_INHG = (IMITATE_GETEM) ? 0.49115 : physics::PSI_PER_INHG; // psi per inch of mercury
const double GETEM_KGM3_PER_LBF3 = (IMITATE_GETEM) ? (35.3146/2.20462) : physics::KGM3_PER_LBF3; // lbs/ft^3 per kg/m^3 
const double GETEM_LB_PER_KG = (IMITATE_GETEM) ? 2.20462 : physics::LB_PER_KG; // pounds per kilogram
const double GETEM_KW_PER_HP = (IMITATE_GETEM) ? 0.7457 : physics::KW_PER_HP; // kilowatts per unit of horsepower
const double GRAVITY_MS2 = (IMITATE_GETEM) ? 9.807 : physics::GRAVITY_MS2; // meters per second^2; this varies between 9.78 and 9.82 depending on latitude
const double DAYS_PER_YEAR = (IMITATE_GETEM) ? 365 : 365.25;

double MetersToFeet(const double &m) {return m * GETEM_FT_IN_METER; }
double FeetToMeters(const double &ft) {return ft / GETEM_FT_IN_METER; }
double M2ToFeet2(const double &mSquared) { return (IMITATE_GETEM) ? mSquared * 10.76391 : mSquared * pow(GETEM_FT_IN_METER,2); }

double InHgToPsi(const double &inHg) { return inHg * GETEM_PSI_PER_INHG; }
double PsiToInHg(const double &psi){ return psi / GETEM_PSI_PER_INHG; }
double BarToPsi(const double &bar) { return bar * GETEM_PSI_PER_BAR; }

double KgPerM3ToLbPerCf(const double &kgPerM3) { return kgPerM3 / GETEM_KGM3_PER_LBF3; }
double LbPerCfToKgPerM3(const double &lbPerCf) { return lbPerCf * GETEM_KGM3_PER_LBF3; }
double LbPerCfToKgPerM3_B(const double &lbPerCf) { return (IMITATE_GETEM) ? lbPerCf * 16.01846 : lbPerCf * GETEM_KGM3_PER_LBF3; }

double KgToLb(const double &kg) { return kg * GETEM_LB_PER_KG; }
double LbToKg(const double &lb) { return lb / GETEM_LB_PER_KG; }

double HPtoKW(const double &hp) { return hp * GETEM_KW_PER_HP; }
double KWtoHP(const double &kw) { return kw / GETEM_KW_PER_HP; }

double PSItoFTB(const double &psi) { return (IMITATE_GETEM) ? psi*144/62 : physics::PSItoFT(psi); }  // convert PSI to pump 'head' in feet.  assumes water density ~ 62 lb/ft^3 if imitating GETEM

double pumpSizeInHP(const double &flow_LbPerHr, const double &head_Ft, const double &eff, std::string sErr)
{
	if (eff <= 0) {
		sErr = ("Pump efficiency <= 0 in 'pumpSizeInHP'.");
		return 0;
	}
	return (flow_LbPerHr * head_Ft)/(60 * 33000 * eff);
}

double pumpWorkInWattHr(const double &flow_LbPerHr, const double &head_Ft, const double &eff, std::string sErr)
{
	return HPtoKW(1000 * pumpSizeInHP(flow_LbPerHr, head_Ft, eff, sErr));
}

double calcEGSTemperatureConstant(double tempC, double maxSecondLawEff) 
{	// not explained.  a constant used to calculate the 'average water temp' for EGS resource
	// it's used in [7C.EGS Subsrfce HX].D127, and to do the makeup calculations in [6Bb.Makeup-EGS HX]
	double c1 = (-0.0006 * tempC) - 0.0681;
	double c2 = (-0.0004 * tempC) + 1.0166;
	double c3 = (maxSecondLawEff * c1) + c2;
	double c4 = (-0.0002 * tempC) + 0.9117;
	double c5 = (-0.001 * tempC) + 0.55;
	return (tempC < 150) ? c3 : ((maxSecondLawEff < c5) ? c3 : c4);
}

double calcEGSAverageWaterTemperatureC(double temp1C, double temp2C, double maxEff)
{
	return physics::KelvinToCelcius(physics::CelciusToKelvin(temp1C) * calcEGSTemperatureConstant(temp2C, maxEff));
}


double gauss_error_function(const double &x)
{
    // Based VBA code in Xnumbers.xla v 5.6
	// by Foxes Team, 2007
    // E -mail: leovlp@libero.it
    // Web:    http://digilander.libero.it/foxes
	// 10.11.2006


	//if (is) wxMessageBox(wxString::Format("Reached max loops in ERFC calculation (u <= 2). x = %2.10f\n", x));

    
    // returns the integral of Gauss' standard error function and complimentary error function
	// first 6 digits match MS Excel erfc function, but they are different after that
	int i;
    double u, a0, a1, a2, b0, B1, b2, g, t, p, s, f1, f2=0, d;
	double y, yc; // y = err function, yc = complimentary error function
    const int maxloop = 2000;
    const double tiny = 10e-15;
    u = fabs(x);   //10.11.06 fix bug for x<<0. Thanks to Michael Hautus
    if (u <= 2)
	{
        t = 2 * u * u; p = 1; s = 1;
        for(i = 3; i <= maxloop; i = i + 2)
		{
            p = p * t / i;
            s = s + p;
            if (p < tiny) break;
		}
        //if (i >= maxloop - 1) wxMessageBox(("Reached max loops in ERFC calculation (u<=2)"));
		//if (i >= maxloop - 1)
		//	wxMessageBox(wxString::Format("Reached max loops in ERFC calculation (u <= 2). x = %2.10f\n", x));
        y = 2 * s * u * exp(-u * u) / sqrt(physics::PI);
        if (x < 0) y = -y;
        yc = 1 - y;
	}
	else
	{
        a0 = 1; b0 = 0; a1 = 0; B1 = 1; f1 = 0;
        for (i = 1; i<= maxloop; i++)
		{
            g = 2 - fmod(i, 2.0);
            a2 = g * u * a1 + i * a0;
            b2 = g * u * B1 + i * b0;
            f2 = a2 / b2;
            d = fabs(f2 - f1);
            if (d < tiny) break;
            a0 = a1 / b2;
            b0 = B1 / b2;
            a1 = a2 / b2;
            B1 = 1;
            f1 = f2;
		}
        //if (i >= maxloop - 1) wxMessageBox(("Reached max loops in ERFC calculation (u > 2)"));
//		if (i >= maxloop - 1)
//			wxMessageBox(wxString::Format("Reached max loops in ERFC calculation (u > 2). x = %2.10f\n", x));
        yc = 2 * exp(-u * u) / (2 * u + f2) / sqrt(physics::PI);
        y = 1 - yc;
		if (x < 0) { y = -y; yc = 2 - yc; }
	}
	return yc;  // y = err function, yc = complimentary error function
}

	void setNonZeroValue(double &dVal, const double &newVal, std::string varName, std::string sErr)
	{
		if(newVal == 0) { sErr = ("Input " + varName + " cannot be zero."); return; }
		dVal = newVal;
		return;
	}

	void setPositiveValue(double &dVal, const double &newVal, std::string varName, std::string sErr)
	{
		if(newVal <= 0) { sErr = ("Input " + varName + " cannot less than or equal to zero."); return; }
		dVal = newVal;
		return;
	}

	void setPositiveValue(int &iVal, const int &newVal, std::string varName, std::string sErr)
	{
		if(newVal <= 0) { sErr = ("Input " + varName + " cannot less than or equal to zero."); return; }
		iVal = newVal;
		return;
	}

	void setZeroTo1(double &dVal, const double &newVal, std::string varName, std::string sErr)
	{
		if( (newVal < 0) || (newVal >= 1)) { sErr = ("Input " + varName + " must be >= 0 and less than 100 percent."); return; }
		dVal = newVal;
		return;
	}

	double evaluatePolynomial(const double &x, const double &c0, const double &c1, const double &c2, const double &c3, const double &c4, const double &c5, const double &c6)
	{
		return (c0 + (c1 * x) + (c2 * pow(x,2)) + (c3 * pow(x,3)) + (c4 * pow(x,4)) + (c5 * pow(x,5)) + (c6 * pow(x,6)));
	}

	// Convert foot-lbs per hour to watt-hr/lb and include pump efficiency
	double FrictionFactor(double Re) { return pow((0.79 * log(Re) - 1.640),-2);}

	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Create CPolynomial class and objects to use throughout code
	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	class CPolynomial
	{
	public:
		CPolynomial(void) {md1=0.0; md2=0.0; md3=0.0; md4=0.0; md5=0.0; md6=0.0; md7=0.0;}
		CPolynomial(const double &c1) {md1=c1; md2=0.0; md3=0.0; md4=0.0; md5=0.0; md6=0.0; md7=0.0;}
		CPolynomial(const double &c1, const double &c2) {md1=c1; md2=c2; md3=0.0; md4=0.0; md5=0.0; md6=0.0; md7=0.0;}
		CPolynomial(const double &c1, const double &c2, const double &c3) {md1=c1; md2=c2; md3=c3; md4=0.0; md5=0.0; md6=0.0; md7=0.0;}
		CPolynomial(const double &c1, const double &c2, const double &c3, const double &c4) {md1=c1; md2=c2; md3=c3; md4=c4; md5=0.0; md6=0.0; md7=0.0;}
		CPolynomial(const double &c1, const double &c2, const double &c3, const double &c4, const double &c5) {md1=c1; md2=c2; md3=c3; md4=c4; md5=c5; md6=0.0; md7=0.0;}
		CPolynomial(const double &c1, const double &c2, const double &c3, const double &c4, const double &c5, const double &c6) {md1=c1; md2=c2; md3=c3; md4=c4; md5=c5; md6=c6; md7=0.0;}
		CPolynomial(const double &c1, const double &c2, const double &c3, const double &c4, const double &c5, const double &c6, const double &c7) {md1=c1; md2=c2; md3=c3; md4=c4; md5=c5; md6=c6; md7=c7;}
		virtual ~CPolynomial(void) {}

		//void init(const double &c1, const double &c2, const double &c3, const double &c4, const double &c5, const double &c6, const double &c7) { md1=c1; md2=c2; md3=c3; md4=c4; md5=c5; md6=c6; md7=c7; }
		double evaluate(double val) { return evaluatePolynomial(val, md1, md2, md3, md4, md5, md6, md7); }

	private:
		double md1,md2,md3,md4,md5,md6,md7;

	};

	// Enthalpy and Entropy Constants
	CPolynomial oAmbientEnthalpyConstants(-31.76958886, 0.997066497, 0.00001087);
	CPolynomial oAmbientEntropyConstants(-0.067875028480951, 0.002201824618666, -0.000002665154152, 0.000000004390426, -0.000000000004355);
	CPolynomial oBinaryEnthalpyConstants(-24.113934502, 0.83827719984, 0.0013462856545, -5.9760546933E-6, 1.4924845946E-8, -1.8805783302E-11, 1.0122595469E-14);
	CPolynomial oBinaryEntropyConstants(-0.060089552413, 0.0020324314656, -1.2026247967E-6, -1.8419111147E-09, 8.8430105661E-12, -1.2945213491E-14, 7.3991541798E-18);
	CPolynomial oFlashEnthalpyConstants(-32.232886, 1.0112508, -0.00013079803, 0.00000050269721, -0.00000000050170088, 1.5041709E-13, 7.0459062E-16);
	CPolynomial oFlashEntropyConstants(-0.067756238, 0.0021979159, -0.0000026352004, 0.0000000045293969, -6.5394475E-12, 6.2185729E-15, -2.2525163E-18);

	// specific volume calculation constants
	CPolynomial oSVC(0.017070951786, -0.000023968043944, 0.00000022418007508, -9.1528222658E-10, 2.1775771856E-12, -2.6995711458E-15, 1.4068205291E-18);
	
	// pressure calculation constants
	CPolynomial oPC(8.0894106754, -0.19788525656, 0.0019695373372, -0.0000091909636468, 0.000000024121846658, -2.5517506351E-12);
	CPolynomial oPressureAmbientConstants(0.320593729630411, -0.0156410175570826, 0.0003545452343917, -0.0000027120923771, 0.0000000136666056);
	CPolynomial oDensityConstants(62.329, 0.0072343, -0.00012456, 0.00000020215, -0.00000000017845);
	CPolynomial oFlashTempConstants(113.186, -2.48032, 0.0209139, -0.0000557641, 0.0000000542893);

	// used in calculating flash brine effectiveness
	CPolynomial oFlashConstants1(-1.306483, 0.2198881, -0.003125628, 0.0000173028, -0.00000003258986);
	CPolynomial oFlashConstants2(0.01897203, -0.0002054368, 0.000002824477, -0.00000001427949, 0.00000000002405238);
	CPolynomial oPSatConstants(0.0588213, -0.0018299913, 0.00010459209, -0.00000084085735, 0.0000000086940123);

	// EGS
	CPolynomial oEGSDensity(0.001003773308, -0.00000043857183, 0.00000001365689, -0.00000000006419, 0.00000000000013);
	CPolynomial oEGSSpecificHeat(4.301651536642, - 0.011554722573, 0.00020328187235, - 0.0000011433197, 0.00000000217642);

	// Min geothermal fluid outlet temperatures to prevent Si precipitation
	// If fluid temp >= 356 degrees F (180 C), use quartz curve
	CPolynomial oMinimumTemperatureQuartz(-159.597976, 0.69792956, 0.00035129);
	// If fluid temp < 356 degrees F (180 C), use chalcedony curve
	CPolynomial oMinimumTemperatureChalcedony(-127.71, 0.8229);

	// The constants for the following 19 objects were only used within the CGETEMGlobals class
	CPolynomial oDHaUnder150(60.251233, -0.28682223, 0.0049745244, -0.000050841601, 0.00000026431087, -0.00000000054076309);
	CPolynomial oDHa150To1500(53.67656, -0.02861559, 0.0000469389, -0.000000047788062, 0.000000000024733176, -5.0493347E-15);
	CPolynomial oDHaOver1500(123.86562, -0.18362579, 0.00016780015, -0.000000077555328, 0.000000000017815452, -1.6323827E-15);
	CPolynomial oDHbUnder150(-2.1991099, 1.4133748, -0.019163136, 0.0001766481, -0.00000087079731, 0.0000000017257066);
	CPolynomial oDHb150To1500(33.304544, 0.27192791, -0.00045591346, 0.000000443209, -0.00000000022501399, 4.5323448E-14);
	CPolynomial oDHbOver1500(740.43412, -1.5040745, 0.0014334909, -0.00000067364263, 0.00000000015600207, -1.4371477E-14);

	// Getting enthalpy from temperature
	CPolynomial oFlashEnthalpyFUnder125(-32.479184, 1.0234315, -0.00034115062, 0.0000020320904, -0.000000004480902);
	CPolynomial oFlashEnthalpyF125To325(-31.760088, 0.9998551, -0.000027703224, 0.000000073480055, 0.00000000025563678);
	CPolynomial oFlashEnthalpyF325To675(-1137.0718729, 13.426933583, -0.055373746094, 0.00012227602697, -0.00000013378773724, 5.8634263518E-11);
	CPolynomial oFlashEnthalpyFOver675(-5658291651.7, 41194401.715, -119960.00955, 174.6587566, -0.12714518982, 0.000037021613128);

	CPolynomial oFlashEnthalpyGUnder125(1061.0996074, 0.44148580795, -0.000030268712038, -0.00000015844186585, -7.2150559138E-10);
	CPolynomial oFlashEnthalpyG125To325(1061.9537518, 0.42367961566, 0.000099006018886, -0.00000051596852593, -0.0000000005035389718);
	CPolynomial oFlashEnthalpyG325To675(-3413.791688, 60.38391862, -0.33157805684, 0.00096963380389, -0.0000015842735401, 0.0000000013698021251, -4.9118123157E-13);
	CPolynomial oFlashEnthalpyGOver675(7355226428.1, -53551582.984, 155953.29919, -227.07686319, 0.16531315908, -0.000048138033984);

	// Getting temperature from pressure
	CPolynomial oFlashTemperatureUnder2(14.788238833, 255.85632577, -403.56297354, 400.57269432, -222.30982965, 63.304761377, -7.1864066799);
	CPolynomial oFlashTemperature2To20(78.871966537, 31.491049082, -4.8016701723, 0.49468791547, -0.029734376328, 0.00094358038872, -0.000012178121702);
	CPolynomial oFlashTemperature20To200(161.40853789, 4.3688747745, -0.062604066919, 0.00061292292067, -0.0000034988475881, 0.00000001053096688, -1.2878309875E-11);
	CPolynomial oFlashTemperature200To1000(256.29706201, 0.93056131917, -0.0020724712921, 0.0000034048164769, -0.0000000034275245432, 1.8867165569E-12, -4.3371351471E-16);
	CPolynomial oFlashTemperatureOver1000(342.90613285, 0.33345911089, -0.00020256473758, 0.000000094407417758, -2.7823504188E-11, 4.589696886E-15, -3.2288675486E-19);


	double EGSWaterDensity(double tempC) { return 1 / oEGSDensity.evaluate(tempC); }			// kg/m^3
	double EGSSpecificHeat(double tempC) { return oEGSSpecificHeat.evaluate(tempC) * 1000; }	// J/kg-C


	double GetDHa(double pressurePSI)
	{
		if (pressurePSI > 1500)
			return oDHaOver1500.evaluate(pressurePSI);
		else if (pressurePSI > 150)
			return oDHa150To1500.evaluate(pressurePSI);
		else
			return oDHaUnder150.evaluate(pressurePSI);
	}

	double GetDHb(double pressurePSI)
	{
		if (pressurePSI > 1500)
			return oDHbOver1500.evaluate(pressurePSI);
		else if (pressurePSI > 150)
			return oDHb150To1500.evaluate(pressurePSI);
		else
			return oDHbUnder150.evaluate(pressurePSI);
	}

	double GetFlashEnthalpyF(double temperatureF)
	{
		if (temperatureF > 675)
			return  oFlashEnthalpyFOver675.evaluate(temperatureF);
		else if (temperatureF > 325)
			return  oFlashEnthalpyF325To675.evaluate(temperatureF);
		else if (temperatureF > 125)
			return  oFlashEnthalpyF125To325.evaluate(temperatureF);
		else 
			return  oFlashEnthalpyFUnder125.evaluate(temperatureF);
	}

	double GetFlashEnthalpyG(double temperatureF)
	{
		if (temperatureF > 675)
			return  oFlashEnthalpyGOver675.evaluate(temperatureF);
		else if (temperatureF > 325)
			return  oFlashEnthalpyG325To675.evaluate(temperatureF);
		else if (temperatureF > 125)
			return  oFlashEnthalpyG125To325.evaluate(temperatureF);
		else 
			return  oFlashEnthalpyGUnder125.evaluate(temperatureF);
	}

	double GetFlashTemperature(double pressurePSI)
	{
		if (pressurePSI > 1000)
			return  oFlashTemperatureOver1000.evaluate(pressurePSI);
		else if (pressurePSI > 200)
			return  oFlashTemperature200To1000.evaluate(pressurePSI);
		else if (pressurePSI > 20)
			return  oFlashTemperature20To200.evaluate(pressurePSI);
		else if (pressurePSI > 2)
			return  oFlashTemperature2To20.evaluate(pressurePSI);
		else 
			return  oFlashTemperatureUnder2.evaluate(pressurePSI);
	}


	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Declaration of CGeoFluidContainer2 
	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	class CGeoFluidContainer2
	{
	public:
		double GetAEForBinaryWattHr(double tempF, double ambientTempF){return physics::toWattHr(GetAEForBinaryBTU(tempF, ambientTempF)); }
		double GetAEForFlashWattHr(double tempF, double ambientTempF){return physics::toWattHr(GetAEForFlashBTU(tempF, ambientTempF)); }

		double GetAEForBinaryWattHrUsingC(double tempC, double ambientTempC) {return GetAEForBinaryWattHr(physics::CelciusToFarenheit(tempC), physics::CelciusToFarenheit(ambientTempC)); }
		double GetAEForFlashWattHrUsingC(double tempC, double ambientTempC) {return GetAEForFlashWattHr(physics::CelciusToFarenheit(tempC), physics::CelciusToFarenheit(ambientTempC)); }

	private:
		double GetAEForBinaryBTU(double tempHighF, double tempLowF)
		{
			return (oBinaryEnthalpyConstants.evaluate(tempHighF) - oAmbientEnthalpyConstants.evaluate(tempLowF)) - ((tempLowF + 460) * (oBinaryEntropyConstants.evaluate(tempHighF) - oAmbientEntropyConstants.evaluate(tempLowF)));
		}

		double GetAEForFlashBTU(double tempHighF, double tempLowF)
		{
			return (oFlashEnthalpyConstants.evaluate(tempHighF) - oAmbientEnthalpyConstants.evaluate(tempLowF)) - ((tempLowF + 460) * (oFlashEntropyConstants.evaluate(tempHighF) - oAmbientEntropyConstants.evaluate(tempLowF)));
		}

	};
	CGeoFluidContainer2 oGFC;



};// namespace geotheraml

/*
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Declaration of CGETEMGlobals2
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class CGETEMGlobals2
{
public:
	CGETEMGlobals2(void);
	virtual ~CGETEMGlobals2(void){}
	static CGETEMGlobals2 GG;


	double GetSiPrecipitationTemperatureF(double geoFluidTempF);

	double GetDHa(double pressurePSI);
	double GetDHb(double pressurePSI);


	double additionalCWPumpHeadSurface(void) { return 10 * 144 / 62.4; }
	int injectionPumpingCycles(void) { return 6; }

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

	CGeoFluidContainer2 GetGeoFluidContainer(void){ return moGFC;}
	int miProjectLifeYears;			// years
	//unsigned int analysisTimeSteps(void) { return (miProjectLifeYears * GetMakeupAnalysesPerYear()) + 1; } // analysis is done for period zero also, so add 1
	unsigned int analysisTimeSteps(void) { return (miProjectLifeYears * GetMakeupAnalysesPerYear()); } // analysis is done for period zero also, so add 1

	void SetModelChoice(int choice) { miModelChoice = ( (choice>=0) && (choice<3) ) ? choice : -1; }
	int GetModelChoice(void) { return miModelChoice; }

	double mdUtilizationFactor; //not explained well, but used to Get annual capacity factor
    double mdTemperatureDeclineRate; // % per year
	//--//double mdTemperatureWetBulbC;    // degrees celcius - used in Flash calcs brine effectiveness calcs an flash injection temperature calcs
    double mdMaxTempDeclineC;
    double mdFinalYearsWithNoReplacement;
    double mdPotentialResourceMW;
	double mdAnnualDiscountRate;

	// pumping parameters
	double mdPumpCostPerHP; // $12,000
	bool mbCalculatePumpWork; // true
	double mdUserSpecifiedPumpWorkKW; // zero


	

	void SetTemperatureGradient(double degreesCelciusPerKM) { setPositiveValue(mdEGSResourceTemperatureGradient, degreesCelciusPerKM, "Temperature Gradient", m_strErrMsg); }

	void SetTemperaturePlantDesignC(double plantDesignTempC) { setPositiveValue(mdTemperaturePlantDesignC, plantDesignTempC, "Plant Design Temperature", m_strErrMsg); }
	double GetTemperatureRatio(void) { return CelciusToKelvin(GetResourceTemperatureC()) / CelciusToKelvin(GetTemperaturePlantDesignC()); } // max valid value is MAX_TEMP_RATIO
	double GetTemperaturePlantDesignMinC(void) { return KelvinToCelcius(CelciusToKelvin(GetResourceTemperatureC())/MAX_TEMP_RATIO); }


	double NumberOfReservoirs(void) { return floor(mdPotentialResourceMW * 1000 / PlantOutputKW()); } // KW = (watt-hr/lb)*(lbs/hr) / 1000
	double DesignCapacityKW(void) { return PlantOutputKW() - GetPumpWorkKW(); } // Max Net KW, after pumping losses (temperature at beginning)//should be the same as powerSales, but GETEM calculates it with assumed Binary constants
	double PowerSalesKW(void) { return PlantSizeKW() - GetPumpWorkKW(); } // Max Net KW, after pumping losses (temperature at beginning) //should be same as designCapacity
	double PlantSizeKW(void) { return flowRateTotal() * GetPlantBrineEffectiveness() / 1000.0;} //	Gross KW, before pump losses (temperature at beginning) = (lbs/hr) * (watt-hr / lb) / 1000
	double PlantOutputKW() { return (IMITATE_GETEM) ? flowRateTotal() * secondLawEfficiencyGETEM() * availableEnergyGETEM() / 1000.0 : PlantSizeKW(); }	//this should be the same as PlantSizeKW, but GETEM calculates it differently in different places [(lbs/hr) * % * (watt-hr / lb)]
	double PlantNetOutputKW(void) { return PlantSizeKW(); }  // used in cost calculation
	// FIX THE REDUNDANCIES IN THE ABOVE CRAP, FIX 'PLANTNETOUTPUT', SINCE IT'S GROSS OUTPUT


	double grossCapacityPerWell(void) { return this->flowRatePerWell() * (GetPlantBrineEffectiveness()) / 1000.0; }		// before pumping losses

	void SetNumberOfWells(double numWells) { setPositiveValue(mdNumberOfWells, numWells, "Number Of Wells", m_strErrMsg); }
	void SetDesiredSalesCapacityKW(double kw) { setPositiveValue(mdDesiredSalesCapacityKW, kw, "Desired Sales Capacity", m_strErrMsg); }



	// Old Available Energy (AE) functions
	double availableEnergyGETEM(void) { return GetAE(); }
	double availableEnergyFlash(void) { return moGFC.GetAEForFlashWattHrUsingC(GetTemperaturePlantDesignC(), GetAmbientTemperatureC()); }	// watt-hr/lb - Calculate available energy using flash constants and plant design temp (short cut)
	// needs to use the EGS ambient temperature
	
	// New Available Energy (AE) functions


	// new for C++ GETEM
	void SetPlantEfficiency(double percent) { setPositiveValue(mdPlantEfficiency, percent, "Plant Efficiency", m_strErrMsg); }
	double GetPlantEfficiency(void)		{ return mdPlantEfficiency; }
	
	makeupAlgorithmType determineMakeupAlgorithm(void);
	makeupAlgorithmType GetMakeupAlgorithm(void){ return mat; }

	double injectionTemperatureC(void); // calculate injection temperature in degrees C

	// Non cost inputs, but used mainly to develop capital costs
	double mdRatioInjectionToProduction; 


	// GF pumping
	// Base inputs has to be an abstract class because it needs this function to be implemented by the CGeoHourlyAnalysis class
	//This is a way around a circular class reference dilemma:
	//CPumpPowerCalculator needs values from CGeoHourlyBaseInputs (several), and visa versa (CGeoHourlyBaseInputs needs values from CPumpPowerCalculator - pump work).
	//It's not quite this simple, because it's not actually CGeoHourlyBaseInputs that needs the values, it's CGeoHourlyAnalysis that needs
	//CGeoHourlyBaseInputs to have the values, but the end result is the same.  This virtual function is part of the solution.
	//Similar situation for the plant brine effectiveness (plant efficiency)
	
	//--//virtual double GetPumpWorkWattHrPerLb(void)=0;
	virtual double GetFractionOfInletGFInjected(void)=0;
	
	void SetFractionOfInletGFInjected(double frac) { setPositiveValue(mdFractionOfInletGFInjected, frac, "Fraction of Inlet Geothermal Fluid Injected", m_strErrMsg); } // G135 on [7A.GF Pumps]




	// reservoir characteristics (used for GF pumping calculations in CPumpPowerCalculator)
	double mdDistanceBetweenProductionInjectionWellsM;			// default 1500 meters, used to calculate the pressure change across the reservoir

	// added for EGS
	double mdEGSDistanceBetweenProductionInjectionWellsM;		// default 1000 meters, used to calculate the EGS 'effective length' or 'fracture length'
	double mdEGSFractureAngle;									// degrees from horizontal




	// Why does GETEM calculate the average water temperature for EGS two different ways?  Is one better?  Method 2 is certainly simpler.
	//double GetemEGSTemperatureConstant(void)  { return calcEGSTemperatureConstant( (GetPlantBrineEffectiveness() / availableEnergyEGS()), mdTemperaturePlantDesignC); }


	// Values that are direct results of EGS inputs (no inputs needed)

	void SetEGSSpecificHeatConstant(double specificHeat) { setPositiveValue(mdEGSSpecificHeatConstant, specificHeat, "Specific Heat Constant", m_strErrMsg); }
	double GetEGSSpecificHeatConstant (void) { return mdEGSSpecificHeatConstant; }

	void SetEGSRockDensity(double density) { setPositiveValue(mdEGSRockDensity, density, "Rock Density", m_strErrMsg); }
	double GetEGSRockDensity (void) { return mdEGSRockDensity; }

	double GetReservoirHeightM (void) { return mdReservoirHeightM; }

	double GetReservoirWidthM (void) { return mdReservoirWidthM; }

	void SetDiameterPumpCasingInches(double inches) { setPositiveValue(mdDiameterPumpCasingInches, inches, "Diameter of the Pump Casing", m_strErrMsg); }

	// to make this 'thread-safe', global variables have to be removed.
	// these public member vars take the place of the globals
	std::string m_strErrMsg;
	CGETEMGlobals2 m_oGG; // create the global object that provides access to all the constants

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
	double mdEGSSpecificHeatConstant;							// J/kg-C
	double mdEGSRockDensity;									// kg/m^3
	double mdEGSNumberOfFractures;
	//--//double mdGFPumpEfficiency;				// 0.6
	double mdReservoirPermeability;			// default = 0.05 darcy units, [2B.Resource&Well Input].D179
	double mdReservoirHeightM;				// default = 100 meters, [2B.Resource&Well Input].F180
	double mdReservoirWidthM;				// default = 500 meters, [2B.Resource&Well Input].F181
	int miModelChoice;						// 0=GETEM, 1=Power Block monthly, 2=Power Block hourly
	double mdDiameterPumpCasingInches;		// 9.625
	double mdDiameterProductionWellInches;	// 10;
	double mdDiameterInjectionWellInches;	// 10;
	double mdNumberOfWells;					// entered or calculated, depending on 'cb' (moved to 'protected' June 2011 for hourly modeling)


	double mdPlantEfficiency; // not in GETEM - essentially the ratio of plant brine effectiveness to max possible brine effectiveness

	// used in CGeoHourlyAnalysis
	double mdPumpDepthFt;
	double mdPumpSizeHP;
	double mdPumpSizeHPInjection;

	//--//bool inputErrors(void);

	// Added June 2011 for geothermal hourly model
	const char * mcFileName;
	SPowerBlockParameters m_pbp;
	SPowerBlockInputs m_pbi;
	int * m_tou;
	//--//bool IsHourly(void) { return (GetMakeupAnalysesPerYear() == 8760) ? true : false; }
	int GetMakeupAnalysesPerYear(void) { return (miModelChoice == 2) ? 8760 : 12; }
	bool ReturnGETEMResults(void) { return (miModelChoice == 0) ? true : false; }

private:
	double secondLawEfficiencyGETEM(void);

	// EGS values that are direct results of EGS inputs (no inputs needed)

	// These EGS function are used in EGS makeup calculations and in pumping calculations

	// private member variables
	double mdResourceDepthM;
    double mdTemperaturePlantDesignC;
	double mdEGSResourceTemperatureGradient;
    double mdTemperatureResourceC;

	double mdDesiredSalesCapacityKW;		// entered or calculated, linked to 'cb', like above

	double mdFractionOfInletGFInjected;		// set from flash brine effectiveness for flash, or 1 for binary

	CGeoFluidContainer2 moGFC;
	makeupAlgorithmType mat;  // only the 'determineMakeupAlgorithm()' function can change this

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
	
	

	double mdAdditionalPressure;					// manually enter additional psi for injection pumps
	double reservoirAreaSqM(void) { return mpGBI->GetReservoirHeightM() * mpGBI->GetReservoirWidthM(); }
	
	// Only used for pumping energy and cost calculations.  For EGS this can be calculated based on desired temp, and a temp gradient
					
	bool mbProductionWellsPumped;
	double GetCalculatedPumpDepthInFeet(void);
	double GetCalculatedPumpSizeHP(void) { return pumpSizeInHP(mpGBI->flowRatePerWell(), pumpHeadFt(), geothermal::PUMP_EFFICIENCY, mpGBI->m_strErrMsg); }


	// has to be public so we can show the value to users
	double GetBottomHolePressure(void);				// [7B.Reservoir Hydraulics].G75


private:
	CGeoHourlyBaseInputs* mpGBI;



	double mdBottomHolePressure;
	bool mbBottomHolePressureCalculated;

	// used so that pressure change is only calculated once
	bool mbPressureChangeCalculated;


	// Calculate injection pump items
	double waterLoss(void) { return (1/(1 - WATER_LOSS_PERCENT)); }																				// G130 - lb/hr

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

	double waterLossFractionOfGF(void);

	condenserTypes mCondenserType;
	double mdNCGLevelPPM;
	double mdDeltaPressureLPFlashPSI;
	double mdEfficiencyGenerator;
	double mdBaseCWPumpHeadFt;
	double mdMoleWeightNCG;
	double mdMoleWeightH2O;
	double mdEfficiencyPump;
	double mdEfficencyVacuumPump;


private:
	CGeoHourlyBaseInputs* mpGBI;

	bool TempConstraint(void) { return ((mpGBI->ft == DUAL_FLASH_WITH_TEMP_CONSTRAINT) || (mpGBI->ft == SINGLE_FLASH_WITH_TEMP_CONSTRAINT)) ; }
	int FlashCount(void) { return (mpGBI->ft >= DUAL_FLASH_NO_TEMP_CONSTRAINT) ? 2 : 1; }

	
	double mdFlashBrineEffectiveness;

	double mdPressureHPFlashPSI; // D29, D64
	double mdPressureLPFlashPSI; // D30, D65



//////////////////////////////////////// Flash Pressures ///////////////////////////////////////////////////
	double tempFlashLimitF(void) { return CelciusToFarenheit(mpGBI->m_oGG.oFlashTempConstants.evaluate(mpGBI->GetResourceTemperatureC())); }				// D26 - deg F
	double pressureFlashAmorphousSilica(void) { return mpGBI->m_oGG.oPC.evaluate(tempFlashLimitF()); }												// D27 - psi
	double pressureSingleNoConstraint() { return (0.0207 * temperatureCondF() - 0.8416) * exp(0.0334*pow(temperatureCondF(),-0.1732) * mpGBI->GetTemperaturePlantDesignC()); } // Q64
	double pressureSingleWithConstraint() { return (pressureSingleNoConstraint() < pressureFlashAmorphousSilica()) ? pressureFlashAmorphousSilica() : pressureSingleNoConstraint(); } // S64
	double pressureSingleToTest(void) { return (TempConstraint()) ? pressureSingleWithConstraint() : pressureSingleNoConstraint(); }							// Q64 or S64
	double pressureSingle(void) { return (pressureSingleToTest() < mpGBI->PRESSURE_AMBIENT_PSI) ? mpGBI->PRESSURE_AMBIENT_PSI : pressureSingleToTest(); }							// O64

	double pressureDualHighNoConstraint() { return mpGBI->m_oGG.oFlashConstants1.evaluate(temperatureCondF()) * exp(mpGBI->m_oGG.oFlashConstants2.evaluate(temperatureCondF()) * mpGBI->GetTemperaturePlantDesignC()); } // R64
	double pressureDualHighWithConstraint();																														// T64
	double pressureDualHigh(void) { return (TempConstraint()) ? pressureDualHighWithConstraint() : pressureDualHighNoConstraint(); }							// P64
	double pressureDualLowUnconstrained() { return (0.12632*exp(0.01918 * temperatureCondF())) * exp((0.0146 * exp(-0.00205*temperatureCondF()) * mpGBI->GetTemperaturePlantDesignC())); } // R65
	double pressureDualLowConstrained() { return (pressureDualLowUnconstrained() < pressureFlashAmorphousSilica()) ? pressureFlashAmorphousSilica() : pressureDualLowUnconstrained(); } // T65
	double pressureDualLowToTest(void) { return (TempConstraint()) ? pressureDualLowConstrained() : pressureDualLowUnconstrained(); }							// R65 or T65
	double pressureDualLow(void) { return  (pressureDualLowToTest() < mpGBI->PRESSURE_AMBIENT_PSI) ? mpGBI->PRESSURE_AMBIENT_PSI : pressureDualLowToTest(); }						// P65


//////////////////////////////////////// Turbine Output ///////////////////////////////////////////////////



	// Turbine 2 - low pressure
	double turbine2dHInitial(void) { return calculateDH(mdPressureLPFlashPSI - mdDeltaPressureLPFlashPSI); }														// I87
	double turbine2TemperatureF(void) { return mpGBI->m_oGG.GetFlashTemperature(mdPressureLPFlashPSI); }														// D88
	double turbine2EnthalpyF(void) { return mpGBI->m_oGG.GetFlashEnthalpyF(turbine2TemperatureF()); }															// D89
	double turbine2EnthalpyG(void) { return mpGBI->m_oGG.GetFlashEnthalpyG(turbine2TemperatureF());}															// D90
	double turbine2DH(void) { return enthalpyChangeTurbine(turbine2dHInitial(), turbine2EnthalpyG()); }																// I102 - btu/lb
	double turbine2HEx(void) { return turbine2EnthalpyG() - turbine2DH(); }																							// I103 - btu/lb
	double turbine2X(void) { return calculateX(turbine1EnthalpyF(), turbine2TemperatureF()); }																		// D91 %
	double turbine2Steam(void) { return (FlashCount() == 2) ? mpGBI->m_oGG.GEOTHERMAL_FLUID_FOR_FLASH() * turbine2X() * (1-turbine1X()) : 0; }																						// I104, D93 - lb/hr
	double turbine2OutputKWh(void) { return turbine2DH() * turbine2Steam() / 3413; }																				// I105 - kW/hr


//////////////////////////////////////// NCG Removal ///////////////////////////////////////////////////////////
	double pTotal(void) { return (IMITATE_GETEM) ? pressureSaturation() + (PRESSURE_CONDENSER_NCG_PARTIAL_INHG * 0.49) : pressureCondenser(); } // calculated separately in spreadsheet, but mathematically equivalent to pressureCondenser					   D150,D74 - psi
	double pRatio(void) { return exp(log(mpGBI->PRESSURE_AMBIENT_PSI / (pTotal()))/NUMBER_OF_COOLING_STAGES); }																// D151
	double ncgFlowLbsPerHour(void) { return mpGBI->m_oGG.GEOTHERMAL_FLUID_FOR_FLASH() * this->mdNCGLevelPPM / 1000000; }										// D152 - lbs/hour
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
	double steamFlow(int st) { return (st >= 3 && (NCG_REMOVAL_TYPE != JET || NUMBER_OF_COOLING_STAGES < 3)) ? 0 : totalVentFlow(st) / ER(st); }																						// D171, D220, D268 - lb/hr

	
//////////////////////////////////////// CW Pump Power KW //////////////////////////////////////////////////////
	double pumpWorkKW(double flowLbPerHr, double pumpHeadFt) { return HPtoKW((flowLbPerHr * pumpHeadFt)/(60 * 33000 * mdEfficiencyPump)); }
	double overAllHEx(void);																																		// I107

	// Main Pump Power
	double deltaPressureCondenserFt() { return (mCondenserType == SURFACE) ? mpGBI->m_oGG.additionalCWPumpHeadSurface() : PSItoFT(mpGBI->PRESSURE_AMBIENT_PSI + 1 - (pressureCondenser())); } // O102
	double cwPumpHead(void) { return mdBaseCWPumpHeadFt + deltaPressureCondenserFt(); }																				// D110 - ft
	double overAllSteam(void) { return (this->FlashCount() == 2) ? turbine1NetSteam() + turbine2Steam() : turbine1NetSteam(); }										// D96
	double qCondenser(void) { return overAllSteam() * (overAllHEx() - mpGBI->m_oGG.GetFlashEnthalpyF(temperatureCondF())); }									// D99
	double cwFlow(void) { return qCondenser() / this->DELTA_TEMPERATURE_CWF; }																						// D114
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

	double condensatePumpHead(void) { return PSItoFTB(mpGBI->PRESSURE_AMBIENT_PSI + 1 - pressureCondenser()) + mdBaseCWPumpHeadFt; }										// D121
	double condensatePumpPowerKW(void) { return pumpWorkKW(overAllSteam(), condensatePumpHead()); }																	// D125->kw, part of I117

	double condensatePumpHeadByStage(int st) { return PSItoFTB(mpGBI->PRESSURE_AMBIENT_PSI + 1 - pInter(st, mpGBI->m_strErrMsg)); }																// D201, D249, D297
	double condensatePumpWorkByStage(int st) { return pumpWorkKW(condensedSteamLbPerHour(st), condensatePumpHeadByStage(st)); }										// D203, ... kW
	double totalCondensatePumpWorkKW(void) { return condensatePumpWorkByStage(1) + condensatePumpWorkByStage(2) + condensatePumpWorkByStage(3);	}					// D306 - kW

	double condensatePumpingKW(void) { return condensatePumpPowerKW() + totalCondensatePumpWorkKW(); }																// I117 - kW


//////////////////////////////////////// Fan Power KW //////////////////////////////////////////////////////////
	double qRejectByStage(int stage) { return condensedSteamLbPerHour(stage) * (mpGBI->m_oGG.GetFlashEnthalpyG(temperatureCondF()) - mpGBI->m_oGG.GetFlashEnthalpyF(temperatureCondF())); } // D190
	double qRejectTotal(void) { return qRejectByStage(1) + qRejectByStage(2) + qRejectByStage(3); }																	// D303
	double qRejectedTower(void) { return qCondenser() + qRejectTotal(); }																							// D101

	double fanPowerCoeffA(void) { return -2.0814 * log(DELTA_TEMPERATURE_CWF) + 10.6013; }																			// O95
	double fanPowerCoeffB(void) { return -0.0188 * pow(DELTA_TEMPERATURE_CWF,0.0232); }																				// P95
	double fanPower(void) { return fanPowerCoeffA() * exp(this->TemperatureWetBulbF() * fanPowerCoeffB()); }														// D103 - hp per MMBtu/hr
	double fanPowerKW(void) { return HPtoKW(fanPower() * qRejectedTower() / 1000000); }																				// D105, I118


//////////////////////////////////////// Vacuum Pump Power KW //////////////////////////////////////////////////
	double deltaPressureByStage(int st) { return pInter(st, mpGBI->m_strErrMsg) - pSuction(st); }																						// D173, D222, D270 - psi
	double densityForVacuumPump(int st) { return pSuction(st) * moleWeightVent(st) /((temperatureCondF() + 460)*10.7316); }											// D166, D215, D263 - lb/ft^3
	double vaccumPumpHead(int st) { return deltaPressureByStage(st) * 144 / densityForVacuumPump(st); }																// D175, D224, D272 - ft
	double vacuumPumpWorkByStage(int st) { return (NCG_REMOVAL_TYPE == VAC_PUMP || (st == 3 && NCG_REMOVAL_TYPE == HYBRID)) ? pumpWorkKW(totalVentFlow(st),  vaccumPumpHead(st)) : 0; } // D178, D227, D275 - kW
	double vacuumPumpingKW(void) { return vacuumPumpWorkByStage(1) + vacuumPumpWorkByStage(2) + vacuumPumpWorkByStage(3); }											// D307, I119


//////////////////////////////////////// Condenser Injection Pump Power KW /////////////////////////////////////
	double injectionDeltaP(void) { return (FlashCount() == 1) ? mdPressureHPFlashPSI - mpGBI->PRESSURE_AMBIENT_PSI : mdPressureLPFlashPSI - mpGBI->PRESSURE_AMBIENT_PSI; }		// D127 - psi (condensate injection delta pressure)
	double injectionPumpHead(void) {return PSItoFT(injectionDeltaP()); }																							// D128 - ft
	double injCoeffA(void) { return -0.0001769 * log(DELTA_TEMPERATURE_CWF) + 0.0011083; }																			// R95
	double injCoeffB(void) { return  0.0657628 * log(DELTA_TEMPERATURE_CWF) - 0.4091309; }																			// S95
	double injCoeffC(void) { return -6.7041142 * log(DELTA_TEMPERATURE_CWF) + 44.3438937; }																			// T95
	double injCoeffD(void) { return -0.0325112 * pow(DELTA_TEMPERATURE_CWF,2) + (6.831236 * DELTA_TEMPERATURE_CWF) - 64.6250943; }									// U95

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
	double plantGrossPower(void) {return (plantBrineEfficiency() * mpGBI->flowRateTotal() / 1000.0); }
	double plantNetPower(void) { return plantGrossPower() - mpGBI->GetPumpWorkKW(); } // kW, as a function of the temperature over time
	double plantNetPowerkW(void) { double pnp = plantNetPower(); return (pnp>0) ? pnp : 0; } // kW
	double plantNetPowerMW(void) { return plantNetPowerkW() / 1000.0; } // MW
	double GetWorkingTemperatureC(void) { return mdWorkingTemperatureC; }
	double TestValue(void) { return plantNetPower(); }
	std::string GetLastErrorMessage(void) { return m_strMAError; }

	// Added June 2011 for geothermal hourly model
	//--//bool OpenWeatherFile(const char * fn);
	void SetPowerBlockFlowRateKgPerSec(double dFlowRateKgPerSec) { m_pbInputs.m_dot_htf = dFlowRateKgPerSec*3600.0; [[ m_dot_htf should be in kg per hour ]] }
	void SetPowerBlockInputs(const SPowerBlockInputs& pbi) { m_pbInputs = pbi; }
	//--//bool ReadWeatherForTimeStep(const bool bHourly, unsigned int timeStep);
	void SetType224Inputs(void);
	double GetType224OutputkW(void);
	float WeatherPressure(void) { return (float)m_pbInputs.P_amb; }
	float WeatherDryBulb(void)  { return (float)m_pbInputs.T_db; }
	float WeatherWetBulb(void)  { return (float)m_pbInputs.T_wb; }

protected:
	//--//double mdWorkingTemperatureC;
	//--//int miReservoirReplacements;	// how many times the reservoir has been 'replaced' (holes redrilled)
	CGeoHourlyBaseInputs* mpGBI;		// use scenario parameters
	CGeothermalConstants2 moSecondLawConstants; //Used to calculate second law (of thermodynamics) efficiencies
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
	
	virtual double fractionOfMaxEfficiency() { return(temperatureRatio() > 0.98) ? moSecondLawConstants.evaluate(temperatureRatio()) : 1.0177 * pow(temperatureRatio(), 2.6237); }

	// Added June 2011 for geothermal hourly model
	SPowerBlockInputs m_pbInputs;
	CPowerBlock_Type224 m_pb;
	//--//weatherfile m_wf;
	//--//bool m_bWeatherFileOpen;
	//--//long m_lReadCount;  // resource file reads through the year, 1 to 8760
	//--//long m_lHourCount;	// hour of analysis (zero to yearsX8760); used to tell the Power Block how many seconds have passed.

private:
	//--//bool ReadNextLineInWeatherFile(void);

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
	CEGSMakeup(void) { mdCurrentEfficiency=0; }
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
	double functionOfRockProperties() { return mpGBI->EGSReservoirConstant(averageReservoirTempC(), daysSinceLastReDrill()); }

	//--//double mdLastProductionTemperatureC; // store the last temperature before calculating new one
	double mdCurrentEfficiency;
	//--//double mdYearsAtNextTimeStep;
	//--//double mdTimeOfLastReservoirReplacement;
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

	//--//bool readyToAnalyze(void);
	bool analyzeIfNecessary(void) {if (mbAnalysisRequired) return analyze(0, 0); else return true; }
	//--//bool analyze( void (*update_function)(float, void*), void *user_data );

	void SetPumpDepthFt(double feet) { mdPumpDepthFt = feet; }
	double GetPumpDepthFt(void) { return (mdPumpDepthFt) ? mdPumpDepthFt : moPPC.GetCalculatedPumpDepthInFeet(); }
	void SetPumpSizeHP(double hp) { mdPumpSizeHP = hp; }
	double GetPumpSizeHP(void){ return (mdPumpSizeHP) ? mdPumpSizeHP : moPPC.GetCalculatedPumpSizeHP(); }
	void SetPumpSizeHPInjection(double hp) { mdPumpSizeHPInjection = hp; }
	double GetPumpSizeHPInjection(void) { return mdPumpSizeHPInjection; }

	// virtual functions in CGeoHourlyBaseInputs
	double GetPumpWorkWattHrPerLb(void) { return moPPC.GetTotalPumpPower(m_strErrMsg); } // small errors in pump work introduce biases throughout the results
	//double GetPlantBrineEffectiveness(void) { return (this->cst == FLASH) ? moFBE.brineEffectiveness() : 11.7414224664536; }
	double GetFractionOfInletGFInjected(void);  // used in CPumpPowerCalculator
	//--//bool TimeToUpdateInterface(float dPercentDone, float iNotificationIntervalInPercent);




	// for use in the interface to show 'calculated' values
	double GetNumberOfProductionWells(void) { return this->GetNumberOfWells(); }
	double GetGrossPlantOutputMW(void) { return this->PlantOutputKW()/1000; }
	double GetNetPlantOutputMW(void) { return this->PowerSalesKW()/1000; }


private:
	//--//float fLastIntervalDone;

	CFlashBrineEffectiveness moFBE;
	CPumpPowerCalculator moPPC;

	double mdLifeTimeDiscountedDesignPower;
	double mdLifeTimeDiscountedNetPower;
	double mdPresentCostFactorFieldReplacements;
	double mdSumOfPresentWorthFactors;

};
*/


//******************************************************************************************************************************************************************************
//******************************************************************************************************************************************************************************
// Declaration of CGeothermalAnalyzer 
//******************************************************************************************************************************************************************************
//******************************************************************************************************************************************************************************
class CGeothermalAnalyzer
{
public:
	CGeothermalAnalyzer(const SPowerBlockParameters& pbp, SPowerBlockInputs& pbi, const SGeothermal_Inputs& gti, SGeothermal_Outputs& gto);
	~CGeothermalAnalyzer();

	bool analyze( void (*update_function)(float, void*), void *user_data );
	std::string error() { return ms_ErrorString; }


private:
	// objects
	SPowerBlockParameters mo_pb_p;
	SPowerBlockInputs mo_pb_in;
	SGeothermal_Inputs mo_geo_in;
	SGeothermal_Outputs mo_geo_out;

	// variables
	std::string ms_ErrorString;
	float mf_LastIntervalDone;
	weatherfile m_wf;
	bool mb_WeatherFileOpen;
	long ml_ReadCount;  // resource file reads through the year, 1 to 8760
	long ml_HourCount;	// hour of analysis (zero to yearsX8760); used to tell the Power Block how many seconds have passed.
	makeupAlgorithmType me_makeup; // { NO_MAKEUP_ALGORITHM, MA_BINARY, MA_FLASH, MA_EGS }; //}
	int mi_ReservoirReplacements;	// how many times the reservoir has been 'replaced' (holes redrilled)
	double md_WorkingTemperatureC;
	double md_LastProductionTemperatureC; // store the last temperature before calculating new one
	double md_YearsAtNextTimeStep; // for EGS calcs
	double md_TimeOfLastReservoirReplacement; // for EGS calcs
	double md_PressureChangeAcrossReservoir; // could be used to store value in between calls to the function that calculates it, to save cpu cycles
	bool mb_BrineEffectivenessCalculated;
	bool mb_FlashPressuresCalculated;


	// functions
	bool IsHourly();
	double GetPumpWorkKW(void);
	void ReplaceReservoir(void);
	double GetTemperatureGradient(void);	// degrees C per km
	double GetResourceTemperatureC(void);	// degrees C
	double GetTemperaturePlantDesignC(void);
	double GetResourceDepthM(void);			// meters
	double GetAmbientTemperatureC(conversionTypes ct = NO_CONVERSION_TYPE);
	double InjectionTemperatureC(void); // calculate injection temperature in degrees C

	double GetAEAtTemp(double tempC);
	double GetAEBinaryAtTemp(double tempC);
	double GetAEFlashAtTemp (double tempC);
	double GetAE(void);
	double GetAEBinary(void);
	double GetAEFlash(void);


	double EGSTimeStar(double tempC);
	double EGSAverageWaterTemperatureC2(void);
	double EGSThermalConductivity (void);
	double EGSFractureLength(void);
	double EGSFractureSurfaceArea(void);
	double EGSFlowPerFracture(double tempC);
	double EGSAlpha(void);
	double EGSLengthOverVelocity(double tempC);
	double availableEnergyEGS();
	double EGSReservoirConstant(double avgWaterTempC, double timeDays);


	double GetPumpWorkWattHrPerLb(void);
	double GetCalculatedPumpDepthInFeet(void);
	double pumpHeadFt(void);
	double pressureInjectionWellBottomHolePSI();	// [7B.Reservoir Hydraulics].G72, [7A.GF Pumps].G50
	double pressureWellHeadPSI(void);				// [7A.GF Pumps].G61
	double GetPressureChangeAcrossReservoir();		// [7B.Reservoir Hydraulics].G70
	double pressureHydrostaticPSI(void);			// [7B.Reservoir Hydraulics].G17
	double pZero(void);
	double injectionTempF(void);
	double injectionDensity(void);



	// production wells
	double productionTempF(void);
	double productionDensity(void);
	double productionFlowRate(void);// lbs per hr / lbs per cf = cf/hr
	double productionViscosity(void);
	double flowRatePerWell(void);		// take Kg/second input and translate to lbs/hour
	double flowRateTotal(void);			// flow rate per well * number of wells
	double GetNumberOfWells(void);
	double GetPlantBrineEffectiveness(void);

	// turbine output
	double calculateX(double enthalpyIn, double temperatureF);
	double enthalpyChangeTurbine(double dEnthalpyDeltaInitial, double dEnthalpyTurbineG); // I65-I80, I87-I102

	// Flash Turbine 1 - high pressure
	double turbine1dHInitial(void); // I65
	double turbine1TemperatureF(void); // D80
	double turbine1EnthalpyF(void); // D81
	double turbine1EnthalpyG(void); // D82
	double turbine1DH(void); // I80 - btu/lb
	double turbine1HEx(void); // I81 - btu/lb
	double turbine1X(void); // D83 - %
	double turbine1Steam(void); // D85 - lb/hr
	double turbine1NetSteam(void); // I82 lb/hr
	double turbine1OutputKWh(void); // I83 - kW/hr = (btu/lb) * (lb/hr) / (btu/kW)

	double calculateDH(double pressureIn);
	double TemperatureWetBulbF(void);
	double temperatureCondF(void); // D71 - deg F
	double pressureSaturation(void); // D72 - psi
	double pressureCondenser(void); // D74 - psi


	double FlashBrineEffectiveness(void);
	void calculateFlashPressures(void);


	bool OpenWeatherFile(const char * fn);
	bool ReadWeatherForTimeStep(const bool bHourly, unsigned int timeStep);
	bool ReadNextLineInWeatherFile(void);

	bool determineMakeupAlgorithm();
	bool inputErrors(void);
	bool readyToAnalyze(void);

	bool TimeToUpdateInterface(float fPercentDone, float fNotificationIntervalInPercent);

};

/*

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Implementation of CGETEMGlobals2 /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
CGETEMGlobals2::CGETEMGlobals2(void)
{
	// private - values returned through public functions implemented below
}

double CGETEMGlobals2::GetSiPrecipitationTemperatureF(double geoFluidTempF)
{
	return (geoFluidTempF >= 356) ? oMinimumTemperatureQuartz.evaluate(geoFluidTempF) : oMinimumTemperatureChalcedony.evaluate(geoFluidTempF);
}





 




/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Implementation of CGeoHourlyBaseInputs
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
CGeoHourlyBaseInputs::CGeoHourlyBaseInputs(void)
{
	tdm = ENTER_RATE;
	rt = HYDROTHERMAL;
	cst = BINARY;
	ft = NO_FLASH_SUBTYPE;
	mat = NO_MAKEUP_ALGORITHM;
	cb = POWER_SALES;
	dc = DEPTH;
	pc = K_AREA;

	// new to GETEM in this implementation (wasn't in spreadsheet version)
	mdPlantEfficiency = 0.95; // ratio of brine effectiveness to max brine effectiveness

	// public properties
    mdTemperaturePlantDesignC = 200;
    mdTemperatureResourceC = 200;
    mdTemperatureDeclineRate = 0.03;
    mdTemperatureWetBulbC = 15;
	mdResourceDepthM = 2000;
	
	// EGS related properties
	mdEGSResourceTemperatureGradient = 35;			// degrees C per kM
	mdEGSSpecificHeatConstant = 950;				// J/kg-C
	mdEGSRockDensity = 2600;						// kg/m^3
	mdEGSFractureWidthM = 175; // meters
	mdEGSFractureAperature = 0.0004; // meters
	mdEGSDistanceBetweenProductionInjectionWellsM = 1000; // meters, used to calculate the 'effective length' for EGS
	mdEGSNumberOfFractures = 6;
	mdEGSFractureAngle = 15; // degrees from horizontal


	// pumping parameters
	mdDiameterPumpCasingInches = 9.625;				// default = 9.926, [2B.Resource&Well Input].H140
	mdDiameterProductionWellInches = 10;			// default = 10, [2B.Resource&Well Input].H138
	mdDiameterInjectionWellInches = 10;				// default = 10, [2B.Resource&Well Input].H142
	mdPumpCostPerHP = 12000;						// default = $12,000 per HP, built into the equation in [7A.GF Pumps].G151
	mbCalculatePumpWork = true;
	mdUserSpecifiedPumpWorkKW = 0;

	mdReservoirPermeability = 0.05;			// default = 0.05 darcy units, [2B.Resource&Well Input].D179
	mdReservoirHeightM = 100;				// default = 100 meters, [2B.Resource&Well Input].F180
	mdReservoirWidthM = 500;				// default = 500 meters, [2B.Resource&Well Input].F181
	mdDistanceBetweenProductionInjectionWellsM = 1500; // meters [2B.Resource&Well Input].F185, used to calculate the pressure change across the reservoir

	// public well properties - potential inputs for wells
	mdRatioInjectionToProduction = 0.5;  // half as many injection wells.

    miProjectLifeYears = 30;
	miModelChoice = -1;
	mdMaxTempDeclineC = 30;
    mdAnnualDiscountRate = 0.1;
    mdFinalYearsWithNoReplacement = 5;
    mdPotentialResourceMW = 200;
    mdUtilizationFactor = (IMITATE_GETEM) ? 0.95 : 1; //not explained well, but used to adjust annual capacity factor

	// other public

	mdPumpSizeHP = 0;
	mdPumpDepthFt = 0;
	mdPumpSizeHPInjection = 0;
	mdNumberOfWells = 0;  // default is that this will be calculated based on desired Total Power kW, brine effectiveness, and flow
	mdFractionOfInletGFInjected = 1; // 100% is default for binary

	mdDesiredSalesCapacityKW = 15000;

	// Added June 2011 for geothermal hourly model
	m_pbp.P_ref = 0;
	mcFileName = NULL;
}





double CGeoHourlyBaseInputs::secondLawEfficiencyGETEM() // This assumes the use of Binary constants and is only necessary for GETEM method of calculating PlantOutputKW - keep private
{
	double ae = GetAEBinary();
	if (ae == 0) { m_strErrMsg = ("ae = zero in CGeoHourlyBaseInputs::secondLawEfficiencyGETEM"); return 0;}
	return GetPlantBrineEffectiveness() / ae;
}


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Implementation of CPumpPowerCalculator //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void CPumpPowerCalculator::init(CGeoHourlyBaseInputs* gbi)
{
	mpGBI = gbi;

    mdAdditionalPressure = 0;


	mbBottomHolePressureCalculated = false;
	mbPressureChangeCalculated = false;
	mbProductionWellsPumped = true;
}



// PRIVATE FUNCTIONS -----------------------------------------------------------------







double CPumpPowerCalculator::calcInjectionPressure()
{	// on [7A.GF Pumps] unless otherwise noted
	double excessPressureAvailble = pressureInjectionWellBottomHolePSI() - pressureHydrostaticPSI();
	double injectionPressurePSI = 150 - excessPressureAvailble;
	return (injectionPressurePSI < 0) ? 0 : injectionPressurePSI; // If it's less than zero, use zero.
}






/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Implementation of CFlashBrineEffectiveness //////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Public Functions -------------------------------------------------------------------------------
CFlashBrineEffectiveness::CFlashBrineEffectiveness(void)
{
	mCondenserType = SURFACE;
	mdNCGLevelPPM = 100;
	mdPressureHPFlashPSI = 0;
	mdPressureLPFlashPSI = 0;
	mdDeltaPressureLPFlashPSI = 1.0;
	mdEfficiencyGenerator = 0.98;
	mdBaseCWPumpHeadFt = 60.0;
	mdMoleWeightNCG = 44.0;
	mdMoleWeightH2O = 18.0;
	mdEfficiencyPump = 0.7;
	mdEfficencyVacuumPump = 0.7;
}

void CFlashBrineEffectiveness::init(CGeoHourlyBaseInputs* gbi)
{
	mpGBI = gbi;
	mb_FlashPressuresCalculated = false;
	mb_BrineEffectivenessCalculated = false;
}



double CFlashBrineEffectiveness::waterLossFractionOfGF(void)
{
	this->calculateFlashPressures();
	return waterLoss() / mpGBI->m_oGG.GEOTHERMAL_FLUID_FOR_FLASH();
}


// Private Functions ------------------------------------------------------------------------------
//////////////////////////////////////// Flash Pressures //////////////////////////////////////////
double CFlashBrineEffectiveness::pressureDualHighWithConstraint()
{
	double a = (temperatureCondF() > 125) ? 1.59 + (0.0015547 * exp(0.0354727*temperatureCondF())) : 1.59 + (0.098693 * exp(0.0025283*temperatureCondF()));
	double b = (temperatureCondF() > 125) ? 0.01916 - (0.000005307 * exp(0.031279921*temperatureCondF())) : 0.01916 - (0.000167123 * exp(0.00400728*temperatureCondF()));
	return a * exp(b * mpGBI->GetTemperaturePlantDesignC());
}


//////////////////////////////////////// Turbine Output ///////////////////////////////////////////




//////////////////////////////////////// NCG Removal //////////////////////////////////////////////
double CFlashBrineEffectiveness::pInter(int stage, std::string sErr)
{	// D156, D205, D253 - psi
	switch (stage)
	{
		case 0: return pTotal();
		case 1: return pTotal() * pRatio();
		case 2: return (NUMBER_OF_COOLING_STAGES > 2) ? pTotal() * pRatio() * pRatio()  : mpGBI->PRESSURE_AMBIENT_PSI;
		case 3: return mpGBI->PRESSURE_AMBIENT_PSI;
		default: { sErr = ("Invalid stage in CFlashBrineEffectiveness::pInter"); return 0; }
	}
}






//////////////////////////////////////// CW Pump Power KW /////////////////////////////////////////
double CFlashBrineEffectiveness::overAllHEx() //I107
{ return (this->FlashCount() == 2) ? ((turbine2HEx() * turbine2Steam()) + (turbine1HEx() * turbine1NetSteam()))/(turbine1NetSteam()+turbine2Steam()) : turbine1HEx(); }



double CFlashBrineEffectiveness::pumpWorkFromSteamFlow(double flow)
{
	double enthalpyCondF = mpGBI->m_oGG.GetFlashEnthalpyF(temperatureCondF());
	double enthalpyCondG = mpGBI->m_oGG.GetFlashEnthalpyG(temperatureCondF());
	
	double qReject = flow * (enthalpyCondG - enthalpyCondF);
	double cwFlow = qReject / DELTA_TEMPERATURE_CWF;
	double pumpHead = mdBaseCWPumpHeadFt + mpGBI->m_oGG.additionalCWPumpHeadSurface();
	return pumpWorkKW(cwFlow, pumpHead);
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Implementation of CMakeupAlgorithm //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
CMakeupAlgorithm::CMakeupAlgorithm()
{
	moSecondLawConstants.init(130.8952, -426.5406, 462.9957, -166.3503, 0, 0, 0);  // by default, load Binary(EGS) secondLawConstants - let flash over write them
}


bool CMakeupAlgorithm::SetScenarioParameters( CGeoHourlyBaseInputs* gbi)
{
	mpGBI = gbi;

	if ( m_pb.InitializeForParameters(mpGBI->GetPowerBlockParameters() ) )
		return true;
	else
		m_strMAError = "There was an error initializing the power block with the input parameters: " + m_pb.GetLastError();

	return false;
}

void CMakeupAlgorithm::SetType224Inputs(void)
{
	// set inputs that change for each timestep
	m_pbInputs.T_htf_hot = mdWorkingTemperatureC;
	m_pbInputs.T_wb = m_wf.twet;
	m_pbInputs.T_db = m_wf.tdry;
	m_pbInputs.P_amb = physics::mBarToAtm(m_wf.pres);
	m_pbInputs.TOU = mpGBI->GetTOUForHour(m_lReadCount-1);
}


double CMakeupAlgorithm::GetType224OutputkW(void)
{
	// run power block model
	if (!m_pb.Execute((m_lHourCount-1)*3600, m_pbInputs))
		m_strMAError = "There was an error running the power block model: " + m_pb.GetLastError();
	
	// return outputs
	return m_pb.GetOutputkW();
}


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Implementation of CFlashMakeup //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
double CFlashMakeup::fractionOfMaxEfficiency() 
{
    double tr = temperatureRatio();
	initializeSecondLawConstants();
	return (1.1 - (0.1 * pow(tr, moSecondLawConstants.evaluate(CelciusToKelvin(mpGBI->GetResourceTemperatureC())) )));
}

void CFlashMakeup::initializeSecondLawConstants()
{
	if (mbInitialized) return;
	switch (mpGBI->ft) //{ NO_FLASH_SUBTYPE, SINGLE_FLASH_NO_TEMP_CONSTRAINT, SINGLE_FLASH_WITH_TEMP_CONSTRAINT, DUAL_FLASH_NO_TEMP_CONSTRAINT, DUAL_FLASH_WITH_TEMP_CONSTRAINT }
	{
		case SINGLE_FLASH_NO_TEMP_CONSTRAINT:
		case SINGLE_FLASH_WITH_TEMP_CONSTRAINT:
			// ("6Ef.Flash Makeup").Range("R20:V20")
			moSecondLawConstants.init(-3637.06, 25.7411, -0.0684072, 0.0000808782, -0.0000000359423, 0, 0);
			break;

		case DUAL_FLASH_NO_TEMP_CONSTRAINT:
			// ("6Ef.Flash Makeup").Range("R22:V22")
			moSecondLawConstants.init(-2762.4048, 18.637876, -0.047198813, 0.000053163057, -0.000000022497296, 0, 0);
			break;

		case DUAL_FLASH_WITH_TEMP_CONSTRAINT:
			// ("6Ef.Flash Makeup").Range("R21:V21")
			moSecondLawConstants.init(-4424.6599, 31.149268, -0.082103498, 0.000096016499, -0.00000004211223, 0, 0);
			break;

		default: m_strMAError = ("Invalid flash technology in CFlashMakeup::initializeSecondLawConstants"); return;
	}
	mbInitialized = true;
}


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Implementation of CEGSMakeup ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void CEGSMakeup::calculateNewTemperature()
{	// This function over-rides CMakeupAlgorithm::calculateNewTemperature(double dTempCalculationsPerYear) to calculate the temperature drop for the EGS resource.
	// The EGS temperature drop depends on the amount of fluid being produced (makes intuitive sense).
	mdLastProductionTemperatureC = mdWorkingTemperatureC;
	mdWorkingTemperatureC = newEGSProductionTemperatureC();
}


bool CEGSMakeup::canReplaceReservoir(double dTimePassedInYears)
{
	mdYearsAtNextTimeStep = dTimePassedInYears;
	return CMakeupAlgorithm::canReplaceReservoir(dTimePassedInYears);
}

double CEGSMakeup::plantBrineEfficiency(void)
{
    mdCurrentEfficiency = maxSecondLawEfficiency() * fractionOfMaxEfficiency();
    return mdCurrentEfficiency * mpGBI->GetAEBinaryAtTemp(mdWorkingTemperatureC); // oSP.GeothermalFuildContainer.availableEnergyWattHr(Binary, toF(fWorkingTemperatureC))
}


double CEGSMakeup::newInjectionTemperatureC()
{
    double tempBrineEfficiencyC = KelvinToCelcius( exp((-0.42 * log(mdLastProductionTemperatureC) + 1.4745) * mdCurrentEfficiency) * CelciusToKelvin(mdLastProductionTemperatureC));
	double tempSILimitC = FarenheitToCelcius(mpGBI->m_oGG.GetSiPrecipitationTemperatureF(LastProducitonTemperatureF()));
	return (tempBrineEfficiencyC >= tempSILimitC) ? tempBrineEfficiencyC : tempSILimitC;
}
*/



CGeothermalAnalyzer::CGeothermalAnalyzer(const SPowerBlockParameters& pbp, SPowerBlockInputs& pbi, const SGeothermal_Inputs& gti, SGeothermal_Outputs& gto)
//******************************************************************************************************************************************************************************
//******************************************************************************************************************************************************************************
// Implementation of CGeoHourlyAnalysis
//******************************************************************************************************************************************************************************
//******************************************************************************************************************************************************************************
{
	mo_pb_p = pbp;
	mo_pb_in = pbi;
	mo_geo_in = gti;
	mo_geo_out = gto;

	me_makeup = NO_MAKEUP_ALGORITHM;
	ms_ErrorString = "";
	mb_WeatherFileOpen = false;
	ml_ReadCount = 0;
	ml_HourCount = 0;
	mi_ReservoirReplacements = 0; 
	md_WorkingTemperatureC=0; 
	md_YearsAtNextTimeStep=0;
	md_TimeOfLastReservoirReplacement=0;
	mf_LastIntervalDone = 0.0f;
	mb_BrineEffectivenessCalculated = false;
	mb_FlashPressuresCalculated = false;


	// make sure the pump work calculator and flash brine effectiveness calculator have a pointer to the base inputs
	//moFBE.init(this); 
	//moPPC.init(this);
}

CGeothermalAnalyzer::~CGeothermalAnalyzer(void)
{
	// delete anything?
}

bool CGeothermalAnalyzer::IsHourly() { return (mo_geo_in.mi_MakeupCalculationsPerYear == 8760) ? true : false; }

double CGeothermalAnalyzer::GetPumpWorkKW(void)
{
	return (mo_geo_in.mb_CalculatePumpWork) ? GetPumpWorkWattHrPerLb() * flowRateTotal() / 1000.0 : mo_geo_in.md_UserSpecifiedPumpWorkKW;
}

void CGeothermalAnalyzer::ReplaceReservoir(void)
{
	mi_ReservoirReplacements++; 
	md_WorkingTemperatureC = GetResourceTemperatureC(); 

	if(me_makeup = MA_EGS)
	{
		md_LastProductionTemperatureC = md_WorkingTemperatureC; 
		if (md_YearsAtNextTimeStep > 0) md_TimeOfLastReservoirReplacement = md_YearsAtNextTimeStep - (EGSTimeStar(EGSAverageWaterTemperatureC2()) / geothermal::DAYS_PER_YEAR);
	}
}

double CGeothermalAnalyzer::GetTemperatureGradient(void) // degrees C per km
{	// Conversation with Chad on August 30th 2010, 10am MT - just use the average gradient, even if it's changing at that point according to the depth/temp graph.
	if (mo_geo_in.me_rt == HYDROTHERMAL) { return ((mo_geo_in.md_TemperatureResourceC - GetAmbientTemperatureC(BINARY))/mo_geo_in.md_ResourceDepthM)*1000; }
	return ((mo_geo_in.md_TemperatureResourceC - mo_geo_in.md_TemperatureEGSAmbientC)/mo_geo_in.md_ResourceDepthM)*1000;
}

double CGeothermalAnalyzer::GetResourceTemperatureC(void) // degrees C
{
	if ( (mo_geo_in.me_rt == EGS) && (mo_geo_in.me_dc == DEPTH) ) return ((mo_geo_in.md_ResourceDepthM/1000) * GetTemperatureGradient()) + mo_geo_in.md_TemperatureEGSAmbientC;
	return mo_geo_in.md_TemperatureResourceC;
}

double CGeothermalAnalyzer::GetTemperaturePlantDesignC(void) { return (mo_geo_in.me_rt == EGS) ? mo_geo_in.md_TemperaturePlantDesignC : GetResourceTemperatureC(); }

double CGeothermalAnalyzer::GetResourceDepthM(void) // meters
{
	if ( (mo_geo_in.me_rt == EGS) && (mo_geo_in.me_dc == TEMPERATURE) ) return 1000 * (mo_geo_in.md_TemperatureResourceC - mo_geo_in.md_TemperatureEGSAmbientC) / GetTemperatureGradient();
	return mo_geo_in.md_ResourceDepthM;
}

double CGeothermalAnalyzer::GetAmbientTemperatureC(conversionTypes ct)
{
	if (ct == NO_CONVERSION_TYPE) ct = mo_geo_in.me_ct;
	return (ct == BINARY) ? geothermal::DEFAULT_AMBIENT_TEMPC_BINARY : (1.3842 * geothermal::WET_BULB_TEMPERATURE_FOR_FLASH_CALCS) + 5.1772 ;
}

double CGeothermalAnalyzer::InjectionTemperatureC() // calculate injection temperature in degrees C
{	// Plant design temp AND resource temp have to be Set correctly!!!
	// These are the calculations done at the bottom of [10B.GeoFluid] with the result in D89
	
	// this is used in pump work calculations, and in EGS energy produciton calculations
	if ((GetTemperaturePlantDesignC() != GetResourceTemperatureC()) && ( (me_makeup == MA_BINARY) || (me_makeup == MA_FLASH) ) )
	{
		ms_ErrorString = ("Resource temperature was not equal to plant design temp in non-EGS analysis in CGeoHourlyBaseInputs::injectionTemperatureC");
		return 0;
	}

	double a = (-0.000655 * GetTemperaturePlantDesignC()) + 1.01964;
	double b = (-0.00244 * GetTemperaturePlantDesignC()) - 0.0567;
	double dPlantBrineEffectiveness = (geothermal::IMITATE_GETEM) ? 10.35 : GetPlantBrineEffectiveness();
	double eff = dPlantBrineEffectiveness / GetAEBinary(); //available energy based on resource temp
	double tr = a * exp(b*eff);
	double t1 = physics::KelvinToCelcius(physics::CelciusToKelvin(GetTemperaturePlantDesignC()) * tr);
	double t2 = GetAmbientTemperatureC() + 27;

	double x = geothermal::evaluatePolynomial(GetTemperaturePlantDesignC(), 4.205944351495, 0.3672417729236, -0.0036294799613, 0.0000706584462, -0.0000001334837, 0, 0);
	double x1 = geothermal::evaluatePolynomial(x, -0.294394, 0.307616, -0.000119669, -0.00000000425191, 0.0000000000249634, 0, 0);

	double t3 = physics::FarenheitToCelcius(physics::CelciusToFarenheit(x1)+1);
	double y = (t1>t2) ? t1 : t2;

	return ( (t3>y) ? t3 : y );
}

double CGeothermalAnalyzer::GetAEAtTemp(double tempC) { return (mo_geo_in.me_ct == BINARY) ? GetAEBinaryAtTemp(tempC) : GetAEFlashAtTemp(tempC) ; }
double CGeothermalAnalyzer::GetAEBinaryAtTemp(double tempC){ return geothermal::oGFC.GetAEForBinaryWattHrUsingC(tempC, GetAmbientTemperatureC()); }	// watt-hr/lb - Calculate available energy using binary constants and plant design temp (short cut)
double CGeothermalAnalyzer::GetAEFlashAtTemp (double tempC){ return geothermal::oGFC.GetAEForFlashWattHrUsingC(tempC, GetAmbientTemperatureC()); }	// watt-hr/lb - Calculate available energy using flash constants and plant design temp (short cut)
double CGeothermalAnalyzer::GetAE(void) { return GetAEAtTemp(GetTemperaturePlantDesignC()) ; }
double CGeothermalAnalyzer::GetAEBinary(void) { return GetAEBinaryAtTemp(GetTemperaturePlantDesignC()); }// watt-hr/lb - Calculate available energy using binary constants and plant design temp (short cut)
double CGeothermalAnalyzer::GetAEFlash(void) { return GetAEFlashAtTemp(GetTemperaturePlantDesignC()); }

double CGeothermalAnalyzer::EGSTimeStar(double tempC)
{
	double dEGSFractureSurfaceArea = mo_geo_in.md_EGSFractureWidthM * EGSFractureLength();
	double d2 = 27 * geothermal::EGSWaterDensity(tempC) * geothermal::EGSSpecificHeat(tempC) * EGSFlowPerFracture(tempC);
	return ( pow(EGSThermalConductivity() * dEGSFractureSurfaceArea/(d2),2) / EGSAlpha() ) + EGSLengthOverVelocity(tempC);
}

double CGeothermalAnalyzer::EGSAverageWaterTemperatureC2()
{	// degrees C (used in [6Bb.Makeup-EGS HX ].X35 to calc time*
	return (InjectionTemperatureC() + GetResourceTemperatureC())/2;
}

double CGeothermalAnalyzer::EGSThermalConductivity()
{	// convert to J/m-hr-C for hourly analysis
	return (IsHourly()) ? geothermal::EGS_THERMAL_CONDUCTIVITY/24 : geothermal::EGS_THERMAL_CONDUCTIVITY;
}

double CGeothermalAnalyzer::EGSFractureLength()
{	//fEffectiveLength, meters used in pump power calcs
	// return 1000 / cos(mo_geo_in.md_EGSFractureAngle * physics::PI / 180); // Distance was hardwired to 1000m in GETEM
	return mo_geo_in.md_DistanceBetweenProductionInjectionWellsM / cos(mo_geo_in.md_EGSFractureAngle * physics::PI / 180);
}

double CGeothermalAnalyzer::EGSFractureSurfaceArea()
{	//fFractureSurfaceArea, m^2
	return mo_geo_in.md_EGSFractureWidthM * EGSFractureLength();
}

double CGeothermalAnalyzer::EGSFlowPerFracture(double tempC)
{	// m^3 per day or per hour
	double dFlowInTimePeriod = (IsHourly()) ? 60*60  : 60*60*24 ; // hourly analysis uses hourly flow, monthly analysis uses daily flow
	return ((mo_geo_in.md_ProductionFlowRateKgPerS / geothermal::EGSWaterDensity(tempC)) / mo_geo_in.md_EGSNumberOfFractures) * dFlowInTimePeriod;
}

double CGeothermalAnalyzer::EGSAlpha(void)
{	// fAlpha (m^2 per day) or (m^2 per hr)
	return EGSThermalConductivity() / (mo_geo_in.md_EGSSpecificHeatConstant * mo_geo_in.md_EGSRockDensity);
}

double CGeothermalAnalyzer::EGSLengthOverVelocity(double tempC)
{
	double dEGSFractureCrossSectionArea = mo_geo_in.md_EGSFractureWidthM * mo_geo_in.md_EGSFractureAperature; // Cross Sectional Area, m^2
	double dEGSVelocity = EGSFlowPerFracture(tempC) / dEGSFractureCrossSectionArea;		// m^3 per day / m^2 = m/day (or hour)
	return EGSFractureLength() / dEGSVelocity; 	// (m / m per day) = days (or hours)
}

double CGeothermalAnalyzer::availableEnergyEGS()
{	// watt-hr/lb - not sure why the flash constants are used to calc EGS available energy
	return geothermal::oGFC.GetAEForFlashWattHrUsingC(mo_geo_in.md_TemperaturePlantDesignC, geothermal::TEMPERATURE_EGS_AMBIENT_C);
}

double CGeothermalAnalyzer::EGSReservoirConstant(double avgWaterTempC, double timePeriods ) // timePeriods could be days or hours
{	// all this is from [7C.EGS Subsrfce HX], also from the calculations over time on 6Bb.Makeup-EGS HX, AF62-AF422
	// this is best done in CGeoHourlyBaseInputs because it requires many CGeoHourlyBaseInputs properties, but it is used in several classes
	double lv = EGSLengthOverVelocity(avgWaterTempC);	// days (or hours)
	if (timePeriods <= lv) return 0;

	double cp = geothermal::EGSSpecificHeat(avgWaterTempC);	// J/kg-C
	double rho = geothermal::EGSWaterDensity(avgWaterTempC);	// kg/m^3
	double flow = EGSFlowPerFracture(avgWaterTempC);	// m^3 per day (or per hour)

	//double EGSFractureSurfaceArea(void) { return mdEGSFractureWidthM * EGSFractureLength(); } //fFractureSurfaceArea, m^2

	double x = (EGSThermalConductivity() * EGSFractureSurfaceArea()) / (cp * rho * flow * sqrt(EGSAlpha()*(timePeriods - lv) ) );
	return geothermal::gauss_error_function(x);
}



double CGeothermalAnalyzer::GetPumpWorkWattHrPerLb(void)
{	// Enter 1 for flow to Get power per lb of flow
	double productionPumpPower = geothermal::pumpWorkInWattHr(1, pumpHeadFt(), geothermal::PUMP_EFFICIENCY, ms_ErrorString);
	if (!ms_ErrorString.empty()) return 0;

	double injectionPumpPower = 0;
	if (geothermal::ADDITIONAL_PRESSURE_REQUIRED)
		injectionPumpPower = 0;// geothermal::pumpWorkInWattHr(waterLoss(), injectionPumpHeadFt(), geothermal::PUMP_EFFICIENCY, ms_ErrorString) * mpGBI->GetFractionOfInletGFInjected(); // ft-lbs/hr

	double retVal = productionPumpPower + injectionPumpPower; // watt-hr per lb of flow
	if (retVal < 0)
	{
		ms_ErrorString = ("CGeothermalAnalyzer::GetPumpWorkWattHrPerLb calculated a value < 0"); 
		return 0; 
	}
	return retVal;
}

double CGeothermalAnalyzer::GetPressureChangeAcrossReservoir()
{	// MOVE THIS? It's only used in GetCalculatedPumpDepthInFeet

	// [7B.Reservoir Hydraulics].G70
    if (mo_geo_in.me_pc == ENTER_PC) return geothermal::RESERVOIR_DELTA_PRESSURE * flowRatePerWell() / 1000.0;
	//if (mbPressureChangeCalculated) return md_PressureChangeAcrossReservoir;

	// if user didn't input the pressure change, we have to calculate it.  start with these
	double dEGSAverageWaterTemperatureC1 = geothermal::calcEGSAverageWaterTemperatureC(GetResourceTemperatureC(), mo_geo_in.md_TemperaturePlantDesignC, GetPlantBrineEffectiveness() / availableEnergyEGS() ); // degrees C (used in EGS makeup, and on [7C.EGS Subsrfce HX]
	//double EGSAverageWaterTemperatureC1 = KelvinToCelcius(CelciusToKelvin(GetResourceTemperatureC()) * GetemEGSTemperatureConstant()); // other equation used in GETEM	

	// all this is from [7C.EGS Subsrfce HX]
	double waterTempC = (geothermal::IMITATE_GETEM) ? dEGSAverageWaterTemperatureC1 : EGSAverageWaterTemperatureC2(); // degrees C
	double days = geothermal::EGS_TIME_INPUT * geothermal::DAYS_PER_YEAR;
	double erfcEGS = EGSReservoirConstant(waterTempC, days);
	double tempEGSProductionC = GetResourceTemperatureC() + (geothermal::TEMPERATURE_EGS_INJECTIONC - GetResourceTemperatureC()) * erfcEGS;
	double dEGSAverageReservoirTemperatureF = physics::CelciusToFarenheit((geothermal::TEMPERATURE_EGS_INJECTIONC + tempEGSProductionC)/2);  //[7C.EGS Subsrfce HX].D52, [7B.Reservoir Hydraulics].D24

	double dResourceTempF = physics::CelciusToFarenheit(GetResourceTemperatureC());
	double dReservoirTemperatureF = (mo_geo_in.me_rt == EGS) ? dEGSAverageReservoirTemperatureF : dResourceTempF;	// G54 on [7B.Reservoir Hydraulics]

	double density = geothermal::oDensityConstants.evaluate(dReservoirTemperatureF); // lbs per ft^3
	double volumetricFlow =(flowRatePerWell() / density)/3600; // ft^3 per second
	double viscosity = 0.115631 * pow(dReservoirTemperatureF, -1.199532); // lb per ft-second

	if ((mo_geo_in.me_rt == EGS) && (mo_geo_in.me_pc == SIMPLE_FRACTURE)  )
	{	// only a valid option for EGS resources
		// calculate the pressure change across the reservoir using simple fracture flow
		double dEGSFractureLengthUserAdjusted = EGSFractureLength() * geothermal::FRACTURE_LENGTH_ADJUSTMENT;
		double effectiveLengthFt = geothermal::MetersToFeet(dEGSFractureLengthUserAdjusted);
		double fractureFlowArea = geothermal::MetersToFeet(mo_geo_in.md_EGSFractureAperature) * geothermal::MetersToFeet(mo_geo_in.md_EGSFractureWidthM);  // ft^2
		double hydraulicDiameter = (2 * fractureFlowArea) / (geothermal::MetersToFeet(mo_geo_in.md_EGSFractureAperature) + geothermal::MetersToFeet(mo_geo_in.md_EGSFractureWidthM));  // ft
		double flowPerFracture = volumetricFlow / mo_geo_in.md_EGSNumberOfFractures; // ft^3 per second
		double velocity = flowPerFracture/fractureFlowArea; // ft per second
		double Re = density * velocity * hydraulicDiameter / viscosity;
		double frictionFactor = 64/Re;
		double headLoss = frictionFactor * (effectiveLengthFt/hydraulicDiameter) * pow(velocity,2)/(2 * physics::GRAVITY_FTS2); // ft
		md_PressureChangeAcrossReservoir = headLoss * density / 144; // psi
	}
	else {
		// calculate the change in pressure across reservoir using K*A (from [7B.Reservoir Hydraulics].G70)
		double dReservoirAreaSqFt = geothermal::MetersToFeet(mo_geo_in.md_ReservoirHeightM) * geothermal::MetersToFeet(mo_geo_in.md_ReservoirWidthM);
		double G53 = geothermal::M2ToFeet2(mo_geo_in.md_ReservoirPermeability * dReservoirAreaSqFt *  0.000000000000986923); //ft^4
		double G61 = volumetricFlow * viscosity * geothermal::MetersToFeet(mo_geo_in.md_DistanceBetweenProductionInjectionWellsM) / G53; // lbs per second^2-ft
		md_PressureChangeAcrossReservoir = G61/physics::GRAVITY_FTS2/144; // change in pressure (psi)
	}
	//mbPressureChangeCalculated = true;
	return md_PressureChangeAcrossReservoir;
}

double CGeothermalAnalyzer::GetCalculatedPumpDepthInFeet(void)
{	// Calculate the pumpSetDepth

	double dBottomHolePressure; // [7B.Reservoir Hydraulics].G75
	double dInectionPumpHeadUsed = 0; // [2B.Resource&Well Input].D162

	if (mo_geo_in.me_rt == EGS)
		dBottomHolePressure = (pressureInjectionWellBottomHolePSI() + dInectionPumpHeadUsed) - GetPressureChangeAcrossReservoir();
	else
		dBottomHolePressure = pressureHydrostaticPSI() - GetPressureChangeAcrossReservoir();

	double pressureDiff = dBottomHolePressure  - pressureWellHeadPSI();
	double dDiameterProductionWellFt = mo_geo_in.md_DiameterProductionWellInches/12;

	double areaWell = physics::areaCircle(dDiameterProductionWellFt/2); // ft^2
	double velocityWell = productionFlowRate()/areaWell;
	double ReWell = dDiameterProductionWellFt * velocityWell * productionDensity()/productionViscosity();
	double frictionHeadLossWell = (geothermal::FrictionFactor(ReWell)/dDiameterProductionWellFt)* pow(velocityWell,2)/(2 * physics::GRAVITY_FTS2);

	double pumpSetting = ((pressureDiff*144)/productionDensity())*(1-frictionHeadLossWell);   // [7A.GF Pumps].D89
	return (geothermal::MetersToFeet(GetResourceDepthM()) - pumpSetting < 0) ? 0 : geothermal::MetersToFeet(GetResourceDepthM()) - pumpSetting; // feet - [7A.GF Pumps].D90
}

double CGeothermalAnalyzer::pumpHeadFt() // ft
{	// calculate the friction head loss of the casing
	double dDiameterPumpCasingFt = mo_geo_in.md_DiameterPumpCasingInches/12;
	double areaCasing = physics::areaCircle(dDiameterPumpCasingFt/2); // ft^2
	double velocityCasing = productionFlowRate()/areaCasing;

	double dReCasing = dDiameterPumpCasingFt * velocityCasing * productionDensity()/productionViscosity();
	double frictionHeadLossCasing = (geothermal::FrictionFactor(dReCasing) * GetCalculatedPumpDepthInFeet() / dDiameterPumpCasingFt)* pow(velocityCasing,2)/(2 * physics::GRAVITY_FTS2); //feet

	// Add (friction head loss) and (pump Set depth) to Get total pump head.
	return frictionHeadLossCasing;// + GetCalculatedPumpDepthInFeet();
}

double CGeothermalAnalyzer::pressureInjectionWellBottomHolePSI() // [7B.Reservoir Hydraulics].G72, [7A.GF Pumps].G50
{
	double injectionWellSurfacePressurePSI = (mo_geo_in.me_ct == FLASH) ? 0 : (pressureWellHeadPSI() - geothermal::PRESSURE_CHANGE_ACROSS_SURFACE_EQUIPMENT_PSI); // [2B.Resource&Well Input].D149
	// this used to incorrectly convert PSI to bar - does this still work for Binary?????????????????????
	double pMax = (pZero() > injectionWellSurfacePressurePSI) ? pZero() : injectionWellSurfacePressurePSI; //G18
	double depthFt = geothermal::MetersToFeet(GetResourceDepthM()); //G22
	double G23 = pMax + injectionDensity() * depthFt/144; // psi

	double flowRate = mo_geo_in.md_ProductionFlowRateKgPerS / mo_geo_in.md_RatioInjectionToProduction /(1 - geothermal::WATER_LOSS_PERCENT); // kg per second
	flowRate = geothermal::KgToLb(flowRate) / injectionDensity();  // cf per second
	double dDiameterInjectionWellFt = mo_geo_in.md_DiameterInjectionWellInches/12;
	double areaInjectionWell = physics::areaCircle(dDiameterInjectionWellFt/2); // ft^2
	double velocityInjectionWell = flowRate/areaInjectionWell;

	double viscosity = 0.0925 * pow(injectionTempF(),-1.159);
	double ReInjectionWell = dDiameterInjectionWellFt * velocityInjectionWell * injectionDensity()/viscosity;

	double frictionHeadLossInjectionWell = (geothermal::FrictionFactor(ReInjectionWell) * depthFt / dDiameterInjectionWellFt)* pow(velocityInjectionWell,2)/(2 * physics::GRAVITY_FTS2); //feet
	double G36 = frictionHeadLossInjectionWell * injectionDensity() / 144; // conversion to psi

	return G23 - G36; // pressureBHInjection, psi
}

double CGeothermalAnalyzer::pressureWellHeadPSI()
{
	double tempF = physics::CelciusToFarenheit(GetTemperaturePlantDesignC());
	double pressureSaturation = geothermal::oPC.evaluate(tempF); // valid above boiling, I guess.
	double pressureExcessPSI = geothermal::BarToPsi(geothermal::EXCESS_PRESSURE_BAR); // bar to psi
	return (GetTemperaturePlantDesignC() > 100) ? pressureSaturation + pressureExcessPSI : geothermal::PRESSURE_AMBIENT_PSI + pressureExcessPSI;
}

double CGeothermalAnalyzer::pressureHydrostaticPSI()
{	// calculate the hydrostatic pressure (at the bottom of the well)
	double tempAmbientF = (geothermal::IMITATE_GETEM) ? physics::CelciusToFarenheit(geothermal::TEMPERATURE_EGS_AMBIENT_C) : physics::CelciusToFarenheit(GetAmbientTemperatureC());
	double pressureAmbientBar = physics::PsiToBar(geothermal::oPressureAmbientConstants.evaluate(tempAmbientF));

	double tempF = (geothermal::IMITATE_GETEM) ? physics::CelciusToFarenheit(geothermal::TEMPERATURE_EGS_AMBIENT_C) : physics::CelciusToFarenheit(GetAmbientTemperatureC());
	double densityAmbient = geothermal::LbPerCfToKgPerM3_B(geothermal::oDensityConstants.evaluate(tempF));

	double tempAmbientC = (geothermal::IMITATE_GETEM) ? 10 : GetAmbientTemperatureC(); // GETEM assumes 10 deg C ambient temperature here. Above, the assumption is 15 deg C ambient.
	double tempGradient = (mo_geo_in.me_rt == EGS) ? GetTemperatureGradient()/1000 : (GetResourceTemperatureC() - tempAmbientC ) / GetResourceDepthM();  

	// hydrostatic pressure at production well depth (GetResourceDepthFt) in bar
	double d1 = densityAmbient * geothermal::GRAVITY_MS2 * geothermal::CONST_CP;
	double d2 = (exp(d1 * (GetResourceDepthM() - (0.5 * geothermal::CONST_CT * tempGradient * pow(GetResourceDepthM(),2))))-1);
	double pressureHydrostaticBar = pressureAmbientBar + (1/geothermal::CONST_CP) * (d2)/100000;
	
	return geothermal::BarToPsi(pressureHydrostaticBar);
}

double CGeothermalAnalyzer::pZero(void) { return geothermal::oPC.evaluate(injectionTempF()); }	// G16 - psi

double CGeothermalAnalyzer::injectionTempF(void)
{
	double dInjectionTempForResource = (mo_geo_in.me_rt == EGS) ? geothermal::TEMPERATURE_EGS_INJECTIONC : InjectionTemperatureC();	// D15 - degrees C
	return physics::CelciusToFarenheit(dInjectionTempForResource);	// G15 - degrees F
}

double CGeothermalAnalyzer::injectionDensity(void) { return (1 / geothermal::oSVC.evaluate(injectionTempF())); }											// G19,G44, G128 - lb/ft^3



// wells
double CGeothermalAnalyzer::productionTempF(void) { return physics::CelciusToFarenheit(GetTemperaturePlantDesignC()); }
double CGeothermalAnalyzer::productionDensity(void) { return geothermal::oSVC.evaluate(productionTempF()); }
double CGeothermalAnalyzer::productionFlowRate(void) { return (flowRatePerWell()/productionDensity())/3600; } // lbs per hr / lbs per cf = cf/hr
double CGeothermalAnalyzer::productionViscosity(void) { return 0.115631 * pow(productionTempF(),-1.199532); } // seems like this is resource temp in spreadsheet!
double CGeothermalAnalyzer::flowRatePerWell(void) { return (60 * 60 * geothermal::KgToLb(mo_geo_in.md_ProductionFlowRateKgPerS)); } // lbs per hour, one well
double CGeothermalAnalyzer::flowRateTotal(void) { return (flowRatePerWell() * GetNumberOfWells()); }								// lbs per hour, all wells

double CGeothermalAnalyzer::GetNumberOfWells(void)
{
	if (mo_geo_in.me_cb == NUMBER_OF_WELLS)
		return mo_geo_in.md_NumberOfWells;
	else
	{
		double netBrineEffectiveness = GetPlantBrineEffectiveness() - GetPumpWorkWattHrPerLb();
		double netCapacityPerWell = flowRatePerWell() * netBrineEffectiveness / 1000.0;			// after pumping losses
		if (netCapacityPerWell == 0)
		{
			ms_ErrorString = "The well capacity was calculated to be zero.  Could not continue analysis.";
			return 0;
		}
		return mo_geo_in.md_DesiredSalesCapacityKW / netCapacityPerWell;
	}
}

double CGeothermalAnalyzer::GetPlantBrineEffectiveness(void)
{
	double dTemperaturePlantDesignF = physics::CelciusToFarenheit(GetTemperaturePlantDesignC());
	double exitTempLowF = (0.8229 * dTemperaturePlantDesignF ) - 127.71;
	double exitTempHighF = (0.00035129 * pow(dTemperaturePlantDesignF,2)) + (0.69792956 * dTemperaturePlantDesignF) - 159.598;
	double dTemperatureGFExitF = (GetTemperaturePlantDesignC() < 180) ? exitTempLowF : exitTempHighF;  // degrees farenheit - exit temperature for geothermal fluid
	double dTemperatureGFExitC = physics::FarenheitToCelcius(dTemperatureGFExitF);
	double dAE_At_Exit = GetAEAtTemp(dTemperatureGFExitC); // watt-hr/lb - Calculate available energy using binary constants and plant design temp (short cut)
	// GETEM's "optimizer" seems to pick the max possible brine effectiveness for the default binary plant, so use this as a proxy for now
	double dAEMaxPossible = (geothermal::IMITATE_GETEM) ? GetAEBinary() -  GetAEBinaryAtTemp(dTemperatureGFExitC) : GetAE() - dAE_At_Exit; // watt-hr/lb - [10B.GeoFluid].H54 "maximum possible available energy accounting for the available energy lost due to a silica constraint on outlet temperature"
	double dMaxBinaryBrineEffectiveness = dAEMaxPossible * ((GetTemperaturePlantDesignC() < 150) ? 0.14425 * exp(0.008806 * GetTemperaturePlantDesignC()) : 0.57);
	return (mo_geo_in.me_ct == FLASH) ? FlashBrineEffectiveness() : dMaxBinaryBrineEffectiveness * mo_geo_in.md_PlantEfficiency;
}

double CGeothermalAnalyzer::calculateX(double enthalpyIn, double temperatureF)
{
	double enthalpyF = geothermal::GetFlashEnthalpyF(temperatureF);
	double enthalpyG = geothermal::GetFlashEnthalpyG(temperatureF);
	return (enthalpyIn - enthalpyF)/(enthalpyG-enthalpyF);
}


double CGeothermalAnalyzer::enthalpyChangeTurbine(double dEnthalpyDeltaInitial, double dEnthalpyTurbineG)
{	// I65-I80, I87-I102
	double xPrime, effTurb, dEnthapyDelta, hEx;

	dEnthapyDelta = dEnthalpyDeltaInitial;
	for (int i=0;i<4;i++) {
		hEx = dEnthalpyTurbineG - dEnthapyDelta;
		xPrime = calculateX(hEx, temperatureCondF());
		xPrime = (xPrime > 0.95) ? 0 : 0.95 - xPrime;
		effTurb = geothermal::EFFICIENCY_TURBINE - (0.5 * xPrime);
		dEnthapyDelta = dEnthalpyDeltaInitial * effTurb;
	}
	return dEnthapyDelta;
}


// Turbine 1 - high pressure
double CGeothermalAnalyzer::turbine1dHInitial() { return calculateDH(mdPressureHPFlashPSI - geothermal::DELTA_PRESSURE_HP_FLASH_PSI); } // I65
double CGeothermalAnalyzer::turbine1TemperatureF() { return geothermal::GetFlashTemperature(mdPressureHPFlashPSI); } // D80
double CGeothermalAnalyzer::turbine1EnthalpyF() { return  geothermal::GetFlashEnthalpyF(turbine1TemperatureF()); }	// D81
double CGeothermalAnalyzer::turbine1EnthalpyG() { return  geothermal::GetFlashEnthalpyG(turbine1TemperatureF()); }	// D82
double CGeothermalAnalyzer::turbine1DH() { return enthalpyChangeTurbine(turbine1dHInitial(), turbine1EnthalpyG()); } // I80 - btu/lb
double CGeothermalAnalyzer::turbine1HEx() { return turbine1EnthalpyG() - turbine1DH(); } // I81 - btu/lb
double CGeothermalAnalyzer::turbine1X()
{	// D83 - %
	double enthalpyPlantDesignTemp = geothermal::GetFlashEnthalpyF(physics::CelciusToFarenheit(GetTemperaturePlantDesignC()));// D69
	return calculateX(enthalpyPlantDesignTemp, turbine1TemperatureF()); 
}																
double CGeothermalAnalyzer::turbine1Steam() { return geothermal::GEOTHERMAL_FLUID_FOR_FLASH * turbine1X(); }																										// D85 - lb/hr
double CGeothermalAnalyzer::turbine1NetSteam()
{	// I82 lb/hr
	double dForNCGRemoval = 0.0;
	if(geothermal::NCG_REMOVAL_TYPE != VAC_PUMP)
	{
		double dSteamFlow = steamFlow(1); 
		if (geothermal::NUMBER_OF_COOLING_STAGES > 1) { dSteamFlow += steamFlow(2); }
		if (geothermal::NUMBER_OF_COOLING_STAGES > 2) { dSteamFlow += steamFlow(3); }
		dForNCGRemoval = dSteamFlow;
	}
	return turbine1Steam() - dForNCGRemoval;
}
double CGeothermalAnalyzer::turbine1OutputKWh() { return turbine1DH() * turbine1NetSteam() / 3413; }																				// I83 - kW/hr = (btu/lb) * (lb/hr) / (btu/kW)

double CGeothermalAnalyzer::calculateDH(double pressureIn)
{
	double a = geothermal::GetDHa(pressureIn);
	double b = geothermal::GetDHb(pressureIn);
	double x = pressureIn /(pressureCondenser());
	return a * log(x) + b;
}

double CGeothermalAnalyzer::TemperatureWetBulbF(void) { return physics::CelciusToFarenheit( mo_geo_in.md_TemperatureWetBulbC); }
double CGeothermalAnalyzer::temperatureCondF(void)
{	// D71 - deg F
	return TemperatureWetBulbF() + geothermal::DELTA_TEMPERATURE_CWF + geothermal::TEMPERATURE_PINCH_PT_CONDENSER_F + geothermal::TEMPERATURE_PINCH_PT_COOLING_TOWER_F;
}
double CGeothermalAnalyzer::pressureSaturation(void) { return geothermal::oPSatConstants.evaluate(temperatureCondF()); }// D72 - psi
double CGeothermalAnalyzer::pressureCondenser(void) { return pressureSaturation() + geothermal::InHgToPsi(geothermal::PRESSURE_CONDENSER_NCG_PARTIAL_INHG); }// D74 - psi


double CGeothermalAnalyzer::FlashBrineEffectiveness(void)
{
	if (!mb_BrineEffectivenessCalculated) {
		calculateFlashPressures();



		double dGrossOutput = turbine1OutputKWh();
		if (FlashCount() == 2) dGrossOutput += turbine2OutputKWh();
		double dGrossPower = dGrossOutput * mdEfficiencyGenerator;

		double dParasiticPower = cwPumpingKW() + condensatePumpingKW() + fanPowerKW() + vacuumPumpingKW() + condenserInjectionPumpingKW();
		mdFlashBrineEffectiveness = dGrossPower - dParasiticPower;
		mb_BrineEffectivenessCalculated = true;
	}
	return mdFlashBrineEffectiveness;
}

void CGeothermalAnalyzer::calculateFlashPressures(void)
{	// This function Sets some values that will be used throughout the calculations of the flash brine effectiveness
	// These cannot be Set during initialization since some of the public properties may have been changed.  These
	// need to be calculated right when the brine effectiveness is calculated.

	if (mb_FlashPressuresCalculated) return;
	
	// if single flash - add flash pressure to delta pressure and quit
	if (FlashCount() == 1)
	{
		mdPressureHPFlashPSI = pressureSingle() + geothermal::DELTA_PRESSURE_HP_FLASH_PSI;
		return;
	}

	// dual flash, have to calculate both
	// high pressure flash
//i think this might be using the wrong temperature - resource instead of plant design - for EGS
	mdPressureHPFlashPSI = pressureDualHigh() + geothermal::DELTA_PRESSURE_HP_FLASH_PSI;


	// low pressure flash
	mdPressureLPFlashPSI = pressureDualLow() + mdDeltaPressureLPFlashPSI;
	mb_FlashPressuresCalculated = true;
}



bool CGeothermalAnalyzer::OpenWeatherFile(const char * fn)
{
	mb_WeatherFileOpen = false;
	ml_ReadCount = 0;
	if (!m_wf.open(fn))
		ms_ErrorString = "Could not open the weather file: " + std::string(fn);
	else
		mb_WeatherFileOpen = true;

	return mb_WeatherFileOpen;
}

// Read one line in weather file for hourly analysis, or calculate the average values for a month for monthly analysis
bool CGeothermalAnalyzer::ReadWeatherForTimeStep(bool bHourly, unsigned int timeStep)
{	
	// if this is an hourly analysis, just ignore the time step and get the data from the next line in the weather file
	if (bHourly) return ReadNextLineInWeatherFile();

	// Not an hourly analysis, so calculate the monthly weather info
	int month = (timeStep % 12) + 1;
	double hours = util::hours_in_month(month);
	if (hours==0)
	{
		ms_ErrorString = "util::hours_in_month returned zero for month =  " + util::to_string(month) + ".";
		return false;
	}

	double pressure=0, wetbulb=0, drybulb=0, rel_humidity=0;
	for (int i = 0; i<hours; i++)
	{
		ReadNextLineInWeatherFile();
		pressure += m_wf.pres;
		wetbulb += m_wf.twet;
		drybulb += m_wf.tdry;
		rel_humidity += m_wf.rhum;
	}
	m_wf.pres = pressure / hours;
	m_wf.twet = wetbulb / hours;
	m_wf.tdry = drybulb / hours;
	m_wf.rhum = rel_humidity / hours;
	return true;
}
// Private function, called from ReadWeatherForTimeStep(timeStep)
// Read the next line from weather file; rewind file if passed the end; assumes 8760 hour weather file;
bool CGeothermalAnalyzer::ReadNextLineInWeatherFile(void)
{
	if (ml_ReadCount >= 8760)
	{
		m_wf.rewind();
		ml_ReadCount = 0;
	}

	if (!m_wf.read())
	{
		ms_ErrorString = "Could not read  line " + util::to_string((int)ml_ReadCount+1) + " in the weather file.";
		return false;
	}
	ml_ReadCount++;
	ml_HourCount++;

	return true;
}


bool CGeothermalAnalyzer::determineMakeupAlgorithm()
{   // This is the logic to determine which makeup algorithm GETEM uses: Binary, Flash, or EGS
    // Just because the user chooses "EGS" from the drop-down box on the "2A.Scenario Input" sheet,
    // does NOT mean that the model will use the results from the EGS makeup sheet.
	me_makeup = NO_MAKEUP_ALGORITHM;
    
	if ((mo_geo_in.me_rt != HYDROTHERMAL) && (mo_geo_in.me_rt != EGS)) ms_ErrorString = "Reource type not recognized in CGeoHourlyBaseInputs::determineMakeupAlgorithm.";
	if ((mo_geo_in.me_ct != BINARY) && (mo_geo_in.me_ct != FLASH))   ms_ErrorString = "Conversion system not recognized in CGeoHourlyBaseInputs::determineMakeupAlgorithm.";
	if (ms_ErrorString != "") return false;

    if (mo_geo_in.me_tdm == ENTER_RATE)
	{ // if user has chosen to enter the temperature decline rate, then the makeup is calculated either with the binary or flash method.
        if (mo_geo_in.me_ct == BINARY)
            me_makeup = MA_BINARY;
		else
		{
            if ((mo_geo_in.me_ft > NO_FLASH_SUBTYPE) && (mo_geo_in.me_ft <= DUAL_FLASH_WITH_TEMP_CONSTRAINT))
                me_makeup = MA_FLASH;
            else
                ms_ErrorString = ("Conversion system Set to 'flash', but the type of flash system was not recognized in CGeoHourlyBaseInputs::determineMakeupAlgorithm");
		}
	}
    else if (mo_geo_in.me_tdm == CALCULATE_RATE)
	{	// this temperature decline can only be calculated for Binary conversion systems with EGS resources
        if (mo_geo_in.me_rt == EGS)
		{
            if (mo_geo_in.me_ct == BINARY)
                me_makeup = MA_EGS;
            else
                ms_ErrorString = ("Fluid temperature decline rate cannot be calculated for EGS resources using a flash plant");
		}
		else
            ms_ErrorString = ("Fluid temperature decline rate cannot be calculated for hydrothermal resources");
	}
	else
		ms_ErrorString = ("Error: Fluid temperature decline method not recognized in CGeoHourlyBaseInputs::determineMakeupAlgorithm.");

    return ( me_makeup != NO_MAKEUP_ALGORITHM );
}


bool CGeothermalAnalyzer::inputErrors(void)
{
	if (!ms_ErrorString.empty()) return true;
	if (mo_geo_in.mi_ProjectLifeYears == 0) { ms_ErrorString = ("Project life was zero."); return true; }
	if (mo_geo_in.mi_ModelChoice < 0) { ms_ErrorString = ("The model choice was not set."); return true; }

	//if (GetTemperaturePlantDesignC() > GetResourceTemperatureC()) { ms_ErrorString = ("Plant design temperature cannot be greater than the resource temperature."); return true; }

	//if (mdPotentialResourceMW < PlantSizeKW()/1000) { ms_ErrorString = ("Resource potential must be greater than the gross plant output."); return true; }

	//if ( (this->rt != EGS) && (this->pc == SIMPLE_FRACTURE) ) { ms_ErrorString = ("Reservoir pressure change based on simple fracture flow can only be calculated for EGS resources."); return true; }

	//if ( (this->rt != EGS) && (this->tdm == CALCULATE_RATE) ) { ms_ErrorString = ("Temperature decline can only be calculated for EGS resources."); return true; }

	//if ((tdm == ENTER_RATE) && (mdTemperatureDeclineRate < 0))
	//	{ ms_ErrorString = ("Fluid temperature decline method chosen was 'enter rate', but the rate is < 0"); return true; }

	//if ( (GetTemperatureRatio() > MAX_TEMP_RATIO) && ReturnGETEMResults() )
	//	{ ms_ErrorString = ("Plant design temperature is too low for resource temperature.  GETEM equations will return invalid results."); return true; }

	//if ( this->netBrineEffectiveness() == 0 ) // this will cause a division by zero error
	//	{ ms_ErrorString = ("Inputs led to a divide by zero error.  Pump work = Plant output, so the net efficiency is zero."); return true; }

	//if ( this->netBrineEffectiveness() < 0 )
	//	{ ms_ErrorString = ("Inputs lead to required pump energy being greater than the plant output."); return true; }


	//if (GetAEBinary() == 0)
	//	{ ms_ErrorString = ("Inputs lead to available energy = zero, which will cause a division by zero error."); return true;}

	//if (m_pbp.P_ref == 0)
	//	{ ms_ErrorString = ("The power block parameters were not initialized."); return true;}

	if (!ms_ErrorString.empty()) return true;
	return false;
}


bool CGeothermalAnalyzer::readyToAnalyze()
{
	if ( inputErrors() ) return false;
	//if (moMA) delete moMA; moMA=NULL;

	if (!determineMakeupAlgorithm()) return false; // determineMakeupAlgorithm Set and returns this->mat

	if (!OpenWeatherFile(mo_geo_in.mc_WeatherFileName) ) return false;

	if ( !mo_geo_out.maf_ReplacementsByYear || !mo_geo_out.maf_monthly_resource_temp || !mo_geo_out.maf_monthly_power || !mo_geo_out.maf_monthly_energy || !mo_geo_out.maf_timestep_resource_temp ||
		 !mo_geo_out.maf_timestep_power || !mo_geo_out.maf_timestep_test_values || !mo_geo_out.maf_timestep_pressure || !mo_geo_out.maf_timestep_dry_bulb || !mo_geo_out.maf_timestep_wet_bulb )

	{
		ms_ErrorString = "One of the output arrays was not initialized in the geothermal hourly model.";
		return false;
	}

	return true;
}

bool CGeothermalAnalyzer::analyze( void (*update_function)(float, void*), void *user_data )
{
	if (!readyToAnalyze()) return false;   // open weather file m_wf

	// We're ready to analyze: moMA is now a valid MakeupAlgorithm with an open weather file and the power block parameters have been passed in
//	if ( !moMA->SetScenarioParameters(this) )
	//{
	//	m_strErrMsg = moMA->GetLastErrorMessage();
	//	return false;
	//}

	// ReSet all calculated values to zero
    double dElapsedTimeInYears = 0.0;
	float fPercentDone = 0.0;
	bool bCanReplaceReservoir = false;
    
    // Initialize
    ReplaceReservoir();

	// Go through time step (hours or months) one by one
    //for (unsigned int iElapsedTimeSteps = 0;  iElapsedTimeSteps < analysisTimeSteps();  iElapsedTimeSteps++)
    bool bReDrill = false;
	unsigned int iElapsedMonths = 0, iElapsedTimeSteps = 0, iEvaluationsInMonth = 0;
	float fMonthlyPowerTotal, fJunk;
	for (unsigned int year = 0;  year < mo_geo_in.mi_ProjectLifeYears;  year++)
	{
		mo_geo_out.maf_ReplacementsByYear[year] = 0;
		for (unsigned int month=1; month<13; month++)
		{
			fPercentDone = (float)iElapsedMonths/(float)(12*mo_geo_in.mi_ProjectLifeYears)*100.0f;

			if ( (update_function != 0) && (TimeToUpdateInterface( fPercentDone, 5.0f )) )
				(*update_function)( fPercentDone, user_data );

			fMonthlyPowerTotal = 0;
			for (unsigned int hour=0; hour < (unsigned int)util::hours_in_month(month); hour++)
			{
				if ( IsHourly() || (hour==0) )
				{
					// Error check
					if (iElapsedTimeSteps >= mo_geo_in.mi_TotalMakeupCalculations )
					{
						ms_ErrorString = "Time step exceded the array size in CGeoHourlyAnalysis::analyze().";
						return false;
					}

					// Read weather file info (function is smart enough to average for month if tis is a monthly analysis)
					// The call to ReadWeatherForTimeStep increments the hour counter (over whole life), and file read counter [0 to 8760(=# lines in weather file]
					if(!ReadWeatherForTimeStep(IsHourly(), iElapsedTimeSteps)) return false;

					// Put weather data into power block inputs
//					moMA->SetType224Inputs();

					// record current temperature (temperature changes monthly, but this is an hourly record of it)
					mo_geo_out.maf_timestep_resource_temp[iElapsedTimeSteps] = 1.1f;//(float)moMA->GetWorkingTemperatureC(); // NOTE: If EGS temp drop is being calculated, then plantNetPowerkW must be called.  No production = no temp change
					mo_geo_out.maf_timestep_pressure[iElapsedTimeSteps] = 1.2f;//moMA->WeatherPressure();
					mo_geo_out.maf_timestep_dry_bulb[iElapsedTimeSteps] = 1.3f;//moMA->WeatherDryBulb();
					mo_geo_out.maf_timestep_wet_bulb[iElapsedTimeSteps] = 1.4f;//moMA->WeatherWetBulb();

					// record outputs based on current inputs
					if ( mo_geo_in.mi_ModelChoice == 0 ) // model choice 0 = GETEM
						mo_geo_out.maf_timestep_power[iElapsedTimeSteps] =1.5f;// (float)moMA->plantNetPowerkW(); // = Gross power - pump work
					else
					{
						mo_geo_out.maf_timestep_power[iElapsedTimeSteps] =1.6f;// (float)moMA->GetType224OutputkW() - (float)GetPumpWorkKW();
						//fJunk = (float)moMA->plantNetPowerkW(); // kinda works, but not quite the same
					}

					mo_geo_out.maf_timestep_test_values[iElapsedTimeSteps] = (float)(year + 1)*1000 + (month);//+(hour); // puts number formatted "year,month,hour_of_month" number into test value

					fMonthlyPowerTotal += mo_geo_out.maf_timestep_power[iElapsedTimeSteps];
		
					//dElapsedTimeInYears = year + util::percent_of_year(month,hour);
					if (!ms_ErrorString.empty()) { return false; }
					iElapsedTimeSteps++;
					dElapsedTimeInYears = iElapsedTimeSteps * (1.0/mo_geo_in.mi_MakeupCalculationsPerYear);  //moved to be after iElapsedTimeSteps++;
				}
			}//hours

			mo_geo_out.maf_monthly_resource_temp[iElapsedMonths] =1.7f;// (float)moMA->GetWorkingTemperatureC();	// resource temperature for this month
			iEvaluationsInMonth = (IsHourly()) ? util::hours_in_month(month) : 1;
			mo_geo_out.maf_monthly_power[iElapsedMonths] = fMonthlyPowerTotal/iEvaluationsInMonth;		// avg monthly power
			mo_geo_out.maf_monthly_energy[iElapsedMonths] = fMonthlyPowerTotal*util::hours_in_month(month)/iEvaluationsInMonth;		// energy output in month (kWh)

			// Is it possible and do we want to replace the reservoir in the next time step?
			// moMA->canReplaceReservoir HAS to run, even if "wantToReplaceReservoir" returns false, because it sets a value in moMA.
			// So, first run it and get the value, then use if statement.  (C++ in VC++ will not evaluate further parts of the expression
			// if the first one is false, since it knows the statement will be false.  Other compilers could evaluate from the right hand
			// side.  In order to make sure this works consistently - separate the expression.)
			bCanReplaceReservoir = false;// moMA->canReplaceReservoir(dElapsedTimeInYears + (1.0/mo_geo_in.mi_MakeupCalculationsPerYear));
			if (/*(moMA->wantToReplaceReservoir()) &&*/ (bCanReplaceReservoir))
			{
				//moMA->replaceReservoir(); // this will 'reset' temperature back to original resource temp
				mo_geo_out.maf_ReplacementsByYear[year] = mo_geo_out.maf_ReplacementsByYear[year] + 1;
			}
			//else
				//moMA->calculateNewTemperature(); // once per month -> reduce temperature from last temp

			iElapsedMonths++;
		}//months
	}//years

	if (!ms_ErrorString.empty() ) return false;
	//mbAnalysisRequired = false;
	return true;
}

/*
double CGeoHourlyAnalysis::GetFractionOfInletGFInjected(void)
{
	if (this->rt == EGS)
		return (1 + WATER_LOSS_PERCENT);
	else
		if (this->cst == BINARY)
			return 1;
		else
			return (1 - moFBE.waterLossFractionOfGF());

}
*/

bool CGeothermalAnalyzer::TimeToUpdateInterface(float fPercentDone, float fNotificationIntervalInPercent)
{	// Needs to be called with fPercentDone = zero at beginning of each run to reset the static var
	
	if (fPercentDone==0)
	{
		mf_LastIntervalDone = 0;
		return true;
	}

	if (fPercentDone >= (mf_LastIntervalDone+fNotificationIntervalInPercent) )
	{
		mf_LastIntervalDone += fNotificationIntervalInPercent;
		return true;			
	}

	return false;
}





/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// RunGeothermalAnalysis
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int RunGeothermalAnalysis(void (*update_function)(float,void*),void*user_data, std::string &err_msg, 
				 const SPowerBlockParameters &pbp, SPowerBlockInputs &pbInputs, 
				 const SGeothermal_Inputs &geo_inputs, SGeothermal_Outputs &geo_outputs)
{
	// return value 0 = clean run; 1 = error with message; 2 = unknown error, no message
	CGeothermalAnalyzer geo_analyzer(pbp, pbInputs, geo_inputs, geo_outputs);
	if ( geo_analyzer.analyze(update_function,user_data) )  // 
		return 0;
	else
		if ( geo_analyzer.error() != "")
		{
			err_msg = geo_analyzer.error();
			return 1; // error that was flagged
		}
		else		
			{ err_msg = "Unknown error during run"; return 2; }
}
