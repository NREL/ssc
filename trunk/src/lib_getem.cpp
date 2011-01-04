// define classes, global constants, etc.
#include "lib_getem.h"


/* apd 3jan2010 

   These globals (errMsg and GG) need to be removed and placed inside
   one of the classes, especially errMsg.  Otherwise GETEM will not be 
   thread-safe.  Once this is completed, just remove this comment also.

   Thanks
*/

std::string errMsg;
CGETEMGlobals CGETEMGlobals::GG; // create the global object that provides access to all the constants


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Implementation of CGETEMInterface ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
CGETEMInterface::CGETEMInterface(void) {errMsg = "";}
CGETEMInterface::~CGETEMInterface(void){}


bool CGETEMInterface::IsReadyToRun(void) { return oGetem.readyToAnalyze(); }
bool CGETEMInterface::ErrorOccured(void) { return (errMsg == "") ? false : true; }
std::string CGETEMInterface::GetErrorMsg(void) { return errMsg; }

int CGETEMInterface::RunGETEM(void)
{
	// do analysis and store results in an 'outputs' object
	if ( oGetem.readyToAnalyze() && oGetem.analyze() )  // 
		return 0;
	else
		if (errMsg != "")
			return 1; // error that was flagged
		else		
			{ errMsg = "Unknown error during run"; return 2; }
}


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// GETEMPhysics and GETEMEquations /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
double FarenheitToCelcius(const double &dTemp){return ((5.0/9.0) * (dTemp - 32.0)); };
double CelciusToFarenheit(const double &dTemp){return (1.8 * dTemp) + 32.0; };

double KelvinToCelcius(const double &dTemp){return (dTemp-273.0); };
double CelciusToKelvin(const double &dTemp){return (dTemp+273.0); };

double FarenheitToKelvin(const double &dTemp){return (CelciusToKelvin(FarenheitToCelcius(dTemp))); };
double KelvinToFarenheit(const double &dTemp){return (CelciusToFarenheit(KelvinToCelcius(dTemp))); };

double areaCircle(const double &radius) { return PI * pow(radius,2); }


double MetersToFeet(const double &m) {return m * GETEM_FT_IN_METER; }
double FeetToMeters(const double &ft) {return ft / GETEM_FT_IN_METER; }
double M2ToFeet2(const double &mSquared) { return (IMITATE_GETEM) ? mSquared * 10.76391 : mSquared * pow(GETEM_FT_IN_METER,2); }

double BarToPsi(const double &bar) { return bar * GETEM_PSI_PER_BAR; }
double PsiToBar(const double &psi){ return psi / GETEM_PSI_PER_BAR; }

double InHgToPsi(const double &inHg) { return inHg * GETEM_PSI_PER_INHG; }
double PsiToInHg(const double &psi){ return psi / GETEM_PSI_PER_INHG; }

double KgPerM3ToLbPerCf(const double &kgPerM3) { return kgPerM3 / GETEM_KGM3_PER_LBF3; }
double LbPerCfToKgPerM3(const double &lbPerCf) { return lbPerCf * GETEM_KGM3_PER_LBF3; }
double LbPerCfToKgPerM3_B(const double &lbPerCf) { return (IMITATE_GETEM) ? lbPerCf * 16.01846 : lbPerCf * GETEM_KGM3_PER_LBF3; }

double KgToLb(const double &kg) { return kg * GETEM_LB_PER_KG; }
double LbToKg(const double &lb) { return lb / GETEM_LB_PER_KG; }

double HPtoKW(const double &hp) { return hp * GETEM_KW_PER_HP; }
double KWtoHP(const double &kw) { return kw / GETEM_KW_PER_HP; }


double toWattHr(const double &btu) { return (btu/3.413); }
double PSItoFT(const double &psi) { return psi * 144 / 62.4; }  // convert PSI to pump 'head' in feet.  assumes water density ~ 62.4 lb/ft^3
double PSItoFTB(const double &psi) { return (IMITATE_GETEM) ? psi*144/62 : PSItoFT(psi); }  // convert PSI to pump 'head' in feet.  assumes water density ~ 62 lb/ft^3 if imitating GETEM

double pumpSizeInHP(const double &flow_LbPerHr, const double &head_Ft, const double &eff)
{
	if (eff <= 0) {
		errMsg = ("Pump efficiency <= 0 in 'pumpSizeInHP'.");
		return 0;
	}
	return (flow_LbPerHr * head_Ft)/(60 * 33000 * eff);
}
double pumpWorkInWattHr(const double &flow_LbPerHr, const double &head_Ft, const double &eff) { return HPtoKW(1000 * pumpSizeInHP(flow_LbPerHr, head_Ft, eff)); }


double evaluatePolynomial(const double &x, const double &c0, const double &c1, const double &c2, const double &c3, const double &c4, const double &c5, const double &c6)
{	return (c0 + (c1 * x) + (c2 * pow(x,2)) + (c3 * pow(x,3)) + (c4 * pow(x,4)) + (c5 * pow(x,5)) + (c6 * pow(x,6))); }


double discountValue(const double &dVal, const double &dDRate, const double &dTimePeriods)
{
	if (dDRate == -1) {
		errMsg = ("Division by zerio error in 'discountValue'.");
		return 0;
	}	
	return dVal / pow((1 + dDRate),dTimePeriods);
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
{ return KelvinToCelcius(CelciusToKelvin(temp1C) * calcEGSTemperatureConstant(temp2C, maxEff)); }


double my_erfc(const double &x)
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
    const double pi_ = 2*acos(0.0);
    const double tiny = 10e-15;
    u = abs(x);   //10.11.06 fix bug for x<<0. Thanks to Michael Hautus
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
        y = 2 * s * u * exp(-u * u) / sqrt(pi_);
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
            d = abs(f2 - f1);
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
        yc = 2 * exp(-u * u) / (2 * u + f2) / sqrt(pi_);
        y = 1 - yc;
		if (x < 0) { y = -y; yc = 2 - yc; }
	}
	return yc;  // y = err function, yc = complimentary error function
}

void setNonZeroValue(double &dVal, const double &newVal, std::string varName)
{
	if(newVal == 0) { errMsg = ("Input " + varName + " cannot be zero."); return; }
	dVal = newVal;
	return;
}

void setPositiveValue(double &dVal, const double &newVal, std::string varName)
{
	if(newVal <= 0) { errMsg = ("Input " + varName + " cannot less than or equal to zero."); return; }
	dVal = newVal;
	return;
}

void setPositiveValue(int &dVal, const int &newVal, std::string varName)
{
	if(newVal <= 0) { errMsg = ("Input " + varName + " cannot less than or equal to zero."); return; }
	dVal = newVal;
	return;
}

void setZeroTo1(double &dVal, const double &newVal, std::string varName)
{
	if( (newVal < 0) || (newVal >= 1)) { errMsg = ("Input " + varName + " must be >= 0 and less than 100 percent."); return; }
	dVal = newVal;
	return;
}


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Implementation of CGETEMGlobals /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
CGETEMGlobals::CGETEMGlobals(void)
{
	oAmbientEnthalpyConstants.init(-31.76958886, 0.997066497, 0.00001087, 0, 0, 0, 0);
	oAmbientEntropyConstants.init(-0.067875028480951, 0.002201824618666, -0.000002665154152, 0.000000004390426, -0.000000000004355, 0, 0);

	oBinaryEnthalpyConstants.init(-24.113934502, 0.83827719984, 0.0013462856545, -5.9760546933E-6, 1.4924845946E-8, -1.8805783302E-11, 1.0122595469E-14);
	oBinaryEntropyConstants.init(-0.060089552413, 0.0020324314656, -1.2026247967E-6, -1.8419111147E-09, 8.8430105661E-12, -1.2945213491E-14, 7.3991541798E-18);

	oFlashEnthalpyConstants.init(-32.232886, 1.0112508, -0.00013079803, 0.00000050269721, -0.00000000050170088, 1.5041709E-13, 7.0459062E-16);
	oFlashEntropyConstants.init(-0.067756238, 0.0021979159, -0.0000026352004, 0.0000000045293969, -6.5394475E-12, 6.2185729E-15, -2.2525163E-18);

	// specific volume calculation constants
	oSVC.init(0.017070951786, -0.000023968043944, 0.00000022418007508, -9.1528222658E-10, 2.1775771856E-12, -2.6995711458E-15, 1.4068205291E-18);
	
	// pressure calculation constants
	oPC.init(8.0894106754, -0.19788525656, 0.0019695373372, -0.0000091909636468, 0.000000024121846658, -2.5517506351E-12,0);
	oPressureAmbientConstants.init(0.320593729630411, -0.0156410175570826, 0.0003545452343917, -0.0000027120923771, 0.0000000136666056, 0, 0);

	oDensityConstants.init(62.329, 0.0072343, -0.00012456, 0.00000020215, -0.00000000017845, 0, 0);

	oFlashTempConstants.init(113.186, -2.48032, 0.0209139, -0.0000557641, 0.0000000542893, 0, 0);

	// used in calculating flash brine effectiveness
	oFlashConstants1.init(-1.306483, 0.2198881, -0.003125628, 0.0000173028, -0.00000003258986, 0, 0);
	oFlashConstants2.init(0.01897203, -0.0002054368, 0.000002824477, -0.00000001427949, 0.00000000002405238, 0, 0);

	oPSatConstants.init(0.0588213, -0.0018299913, 0.00010459209, -0.00000084085735, 0.0000000086940123, 0, 0);


	// private - values returned through public functions implemented below
	oDHaUnder150.init(60.251233, -0.28682223, 0.0049745244, -0.000050841601, 0.00000026431087, -0.00000000054076309, 0);
	oDHa150To1500.init(53.67656, -0.02861559, 0.0000469389, -0.000000047788062, 0.000000000024733176, -5.0493347E-15, 0);
	oDHaOver1500.init(123.86562, -0.18362579, 0.00016780015, -0.000000077555328, 0.000000000017815452, -1.6323827E-15, 0);

	oDHbUnder150.init(-2.1991099, 1.4133748, -0.019163136, 0.0001766481, -0.00000087079731, 0.0000000017257066, 0);
	oDHb150To1500.init(33.304544, 0.27192791, -0.00045591346, 0.000000443209, -0.00000000022501399, 4.5323448E-14, 0);
	oDHbOver1500.init(740.43412, -1.5040745, 0.0014334909, -0.00000067364263, 0.00000000015600207, -1.4371477E-14, 0);


	// Getting enthalpy from temperature
	oFlashEnthalpyFUnder125.init(-32.479184, 1.0234315, -0.00034115062, 0.0000020320904, -0.000000004480902, 0, 0);
	oFlashEnthalpyF125To325.init(-31.760088, 0.9998551, -0.000027703224, 0.000000073480055, 0.00000000025563678, 0, 0);
	oFlashEnthalpyF325To675.init(-1137.0718729, 13.426933583, -0.055373746094, 0.00012227602697, -0.00000013378773724, 5.8634263518E-11, 0);
	oFlashEnthalpyFOver675.init(-5658291651.7, 41194401.715, -119960.00955, 174.6587566, -0.12714518982, 0.000037021613128, 0);

	oFlashEnthalpyGUnder125.init(1061.0996074, 0.44148580795, -0.000030268712038, -0.00000015844186585, -7.2150559138E-10, 0, 0);
	oFlashEnthalpyG125To325.init(1061.9537518, 0.42367961566, 0.000099006018886, -0.00000051596852593, -0.0000000005035389718, 0, 0);
	oFlashEnthalpyG325To675.init(-3413.791688, 60.38391862, -0.33157805684, 0.00096963380389, -0.0000015842735401, 0.0000000013698021251, -4.9118123157E-13);
	oFlashEnthalpyGOver675.init(7355226428.1, -53551582.984, 155953.29919, -227.07686319, 0.16531315908, -0.000048138033984, 0);

	// Getting temperature from pressure
	oFlashTemperatureUnder2.init(14.788238833, 255.85632577, -403.56297354, 400.57269432, -222.30982965, 63.304761377, -7.1864066799);
	oFlashTemperature2To20.init(78.871966537, 31.491049082, -4.8016701723, 0.49468791547, -0.029734376328, 0.00094358038872, -0.000012178121702);
	oFlashTemperature20To200.init(161.40853789, 4.3688747745, -0.062604066919, 0.00061292292067, -0.0000034988475881, 0.00000001053096688, -1.2878309875E-11);
	oFlashTemperature200To1000.init(256.29706201, 0.93056131917, -0.0020724712921, 0.0000034048164769, -0.0000000034275245432, 1.8867165569E-12, -4.3371351471E-16);
	oFlashTemperatureOver1000.init(342.90613285, 0.33345911089, -0.00020256473758, 0.000000094407417758, -2.7823504188E-11, 4.589696886E-15, -3.2288675486E-19);

	// EGS
	oEGSDensity.init(0.001003773308, -0.00000043857183, 0.00000001365689, -0.00000000006419, 0.00000000000013, 0, 0);
	oEGSSpecificHeat.init(4.301651536642, - 0.011554722573, 0.00020328187235, - 0.0000011433197, 0.00000000217642, 0, 0);

	// Min geothermal fluid outlet temperatures to prevent Si precipitation
	// If fluid temp >= 356 degrees F (180 C), use quartz curve
	oMinimumTemperatureQuartz.init(-159.597976, 0.69792956, 0.00035129, 0, 0, 0, 0);
	// If fluid temp < 356 degrees F (180 C), use chalcedony curve
	oMinimumTemperatureChalcedony.init(-127.71, 0.8229, 0, 0, 0, 0, 0);
}

double CGETEMGlobals::GetSiPrecipitationTemperatureF(double geoFluidTempF)
{ return (geoFluidTempF >= 356) ? oMinimumTemperatureQuartz.evaluatePolynomial(geoFluidTempF) : oMinimumTemperatureChalcedony.evaluatePolynomial(geoFluidTempF); }

double CGETEMGlobals::EGSWaterDensity(double tempC) { return 1 / oEGSDensity.evaluatePolynomial(tempC); }			// kg/m^3
double CGETEMGlobals::EGSSpecificHeat(double tempC) { return oEGSSpecificHeat.evaluatePolynomial(tempC) * 1000; }	// J/kg-C


double CGETEMGlobals::GetDHa(double pressurePSI)
{
	if (pressurePSI > 1500)
		return CGETEMGlobals::GG.oDHaOver1500.evaluatePolynomial(pressurePSI);
	else if (pressurePSI > 150)
		return CGETEMGlobals::GG.oDHa150To1500.evaluatePolynomial(pressurePSI);
	else
		return CGETEMGlobals::GG.oDHaUnder150.evaluatePolynomial(pressurePSI);
}

double CGETEMGlobals::GetDHb(double pressurePSI)
{
	if (pressurePSI > 1500)
		return CGETEMGlobals::GG.oDHbOver1500.evaluatePolynomial(pressurePSI);
	else if (pressurePSI > 150)
		return CGETEMGlobals::GG.oDHb150To1500.evaluatePolynomial(pressurePSI);
	else
		return CGETEMGlobals::GG.oDHbUnder150.evaluatePolynomial(pressurePSI);
}

double CGETEMGlobals::GetFlashEnthalpyF(double temperatureF)
{
	if (temperatureF > 675)
		return  CGETEMGlobals::GG.oFlashEnthalpyFOver675.evaluatePolynomial(temperatureF);
	else if (temperatureF > 325)
		return  CGETEMGlobals::GG.oFlashEnthalpyF325To675.evaluatePolynomial(temperatureF);
	else if (temperatureF > 125)
		return  CGETEMGlobals::GG.oFlashEnthalpyF125To325.evaluatePolynomial(temperatureF);
	else 
		return  CGETEMGlobals::GG.oFlashEnthalpyFUnder125.evaluatePolynomial(temperatureF);
}

double CGETEMGlobals::GetFlashEnthalpyG(double temperatureF)
{
	if (temperatureF > 675)
		return  CGETEMGlobals::GG.oFlashEnthalpyGOver675.evaluatePolynomial(temperatureF);
	else if (temperatureF > 325)
		return  CGETEMGlobals::GG.oFlashEnthalpyG325To675.evaluatePolynomial(temperatureF);
	else if (temperatureF > 125)
		return  CGETEMGlobals::GG.oFlashEnthalpyG125To325.evaluatePolynomial(temperatureF);
	else 
		return  CGETEMGlobals::GG.oFlashEnthalpyGUnder125.evaluatePolynomial(temperatureF);
}

double CGETEMGlobals::GetFlashTemperature(double pressurePSI)
{
	if (pressurePSI > 1000)
		return  CGETEMGlobals::GG.oFlashTemperatureOver1000.evaluatePolynomial(pressurePSI);
	else if (pressurePSI > 200)
		return  CGETEMGlobals::GG.oFlashTemperature200To1000.evaluatePolynomial(pressurePSI);
	else if (pressurePSI > 20)
		return  CGETEMGlobals::GG.oFlashTemperature20To200.evaluatePolynomial(pressurePSI);
	else if (pressurePSI > 2)
		return  CGETEMGlobals::GG.oFlashTemperature2To20.evaluatePolynomial(pressurePSI);
	else 
		return  CGETEMGlobals::GG.oFlashTemperatureUnder2.evaluatePolynomial(pressurePSI);
}


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Implementation of CGeothermalFluid //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
CGeothermalFluid::CGeothermalFluid(void)
{	// constructor: initialize member variables and objects
	moEnthalpyConstants.init(0,0,0,0,0,0,0);
	moEntropyConstants.init(0,0,0,0,0,0,0);
}

void CGeothermalFluid::init(const CGeothermalConstants &enthalpyConstants, const CGeothermalConstants &entropyConstants)
{	// constructor: initialize member variables and objects
	moEnthalpyConstants = enthalpyConstants;
	moEntropyConstants = entropyConstants;
}
 
 
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Implementation of CGeoFluidContainer ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
CGeoFluidContainer::CGeoFluidContainer(void)
{
	moAmbientGeothermalFluid.init(CGETEMGlobals::GG.oAmbientEnthalpyConstants, CGETEMGlobals::GG.oAmbientEntropyConstants);
	moBinaryGeothermalFluid.init(CGETEMGlobals::GG.oBinaryEnthalpyConstants, CGETEMGlobals::GG.oBinaryEntropyConstants);
	moFlashGeothermalFluid.init(CGETEMGlobals::GG.oFlashEnthalpyConstants, CGETEMGlobals::GG.oFlashEntropyConstants);
}



double CGeoFluidContainer::GetAEForBinaryBTU(double tempHighF, double tempLowF)
{
	return (moBinaryGeothermalFluid.enthalpy(tempHighF) - moAmbientGeothermalFluid.enthalpy(tempLowF)) - ((tempLowF + 460) * (moBinaryGeothermalFluid.entropy(tempHighF) - moAmbientGeothermalFluid.entropy(tempLowF)));
}

double CGeoFluidContainer::GetAEForFlashBTU(double tempHighF, double tempLowF)
{
	return (moFlashGeothermalFluid.enthalpy(tempHighF) - moAmbientGeothermalFluid.enthalpy(tempLowF)) - ((tempLowF + 460) * (moFlashGeothermalFluid.entropy(tempHighF) - moAmbientGeothermalFluid.entropy(tempLowF)));
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Implementation of CGETEMBaseInputs //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
CGETEMBaseInputs::CGETEMBaseInputs(void)
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
	mdPressureAmbientPSI = 14.7; // default
	mdResourceDepthM = 2000;
	
	// EGS related properties
	// GETEM note says that this ambient temp is only used in calculating resource temp or depth.  However, if EGS resource temperature calculations are based on depth (user choice),
	// then resource temp is based on this number, so all power calcs are based on it as well.  This is not used consistently in GETEM
	mdTemperatureEGSAmbientC = 15;
	mdEGSResourceTemperatureGradient = 35;			// degrees C per kM
	mdTemperatureEGSInjectionC = 76.1;				// degrees C, [7C.EGS Subsrfce HX].D11 [should be a function of plant design temperature]
	mdEGSThermalConductivity = 3*3600*24;			// J/m-day-C
	mdEGSSpecificHeatConstant = 950;				// J/kg-C
	mdEGSRockDensity = 2600;						// kg/m^3
	mdEGSFractureWidthM = 175; // meters
	mdEGSFractureAperature = 0.0004; // meters
	mdEGSDistanceBetweenProductionInjectionWellsM = 1000; // meters, used to calculate the 'effective length' for EGS
	mdFractureLengthAdjustment = 2;
	mdEGSNumberOfFractures = 6;
	mdEGSFractureAngle = 15; // degrees from horizontal
	mdEGSTimeInput = 3.076; // years, but it's not well explained [7C.EGS Subsrfce HX].D42


	// pumping parameters
	mdProductionFlowRateKgPerS = 70;				// default = 70 kg/s, [2B.Resource&Well Input].H140
	mdGFPumpEfficiency = 0.6;						// default = 60%, [2A.Scenario Input].D107
	mdDiameterPumpCasingInches = 9.625;				// default = 9.926, [2B.Resource&Well Input].H140
	mdDiameterProductionWellInches = 10;			// default = 10, [2B.Resource&Well Input].H138
	mdDiameterInjectionWellInches = 10;				// default = 10, [2B.Resource&Well Input].H142
	mdPressureChangeAcrossSurfaceEquipmentPSI = 25; // default = 25 psi, [2B.Resource&Well Input].H146
	mdPumpCostPerHP = 12000;						// default = $12,000 per HP, built into the equation in [7A.GF Pumps].G151

	mdReservoirPermeability = 0.05;			// default = 0.05 darcy units, [2B.Resource&Well Input].D179
	mdReservoirHeightM = 100;				// default = 100 meters, [2B.Resource&Well Input].F180
	mdReservoirWidthM = 500;				// default = 500 meters, [2B.Resource&Well Input].F181
	mdExcessPressureBar = 3.5;				// default 3.5 bar, [2B.Resource&Well Input].D205
	mdReservoirDeltaPressure = 0.35;		// default = .35 psi-h per 1000 lb [2B.Resource&Well Input].D171
	mdDistanceBetweenProductionInjectionWellsM = 1500; // meters [2B.Resource&Well Input].F185, used to calculate the pressure change across the reservoir

	// public well properties - potential inputs for wells
	mdExplorationWells = 2;
	mdConfirmationWellSuccessRate = 0.5;
	mdConfirmationWells = 2;
	mdRatioInjectionToProduction = 0.5;  // half as many injection wells.
	mdRatioInjectionDepthToProductionDepth = 1;
	mdNumberOfSpareProductionWells = 0;

    miProjectLifeYears = 30;
	miMakeupAnalysesPerYear = 12;
    mdMaxTempDeclineC = 30;
    mdAnnualDiscountRate = 0.1;
    mdFinalYearsWithNoReplacement = 5;
    mdPotentialResourceMW = 200;
    mdUtilizationFactor = 0.95; //not explained well, but used to adjust annual capacity factor

	// cost related
    mdFixedChargeRate = 0.128; //I don//t think this can be completely independent of discount rate as it is in GETEM
    mdContingencyFactor = 0.05; //percent adder to total capital costs (field and plant)
    mdRoyaltyPercent = 0.1; //percent of annual, field related costs to add in order to approximate royalty fees

	mbCalculatePlantOM = false;
	mdOMCostsPlantUserInput = 2.0;		//O&M costs in cents/kWh, related to plant

	mbCalculateFieldOM = false;
	mdOMCostsFieldUserInput = 1.0;		//O&M costs in cents/kWh, related to field

	mbCalculatePlantCost = false;
	mdPlantCapitalCostPerKWUserInput = 1800;	// $s/kW				([2E.Conversion Systems] D68 for Binary or D200 for Flash)


	mdNonWellExplorationCosts =      750000;	// $s					([2A.Scenario Input].D60)
	mdNonWellConfirmationCosts =     250000;	// $s					([2A.Scenario Input].D61)
	mdExplorationWellCostMultiplier = 0.5;		//						([2A.Scenario Input].D57)
	mdConfirmationWellCostMultiplier = 1.2;		//						([2A.Scenario Input].D58)


	mdWellFieldNonDrillingCosts =    250000;	// $s					([2B.Resource&Well Input].D69)
	mdSurfaceEquipmentCostPerWell =  125000;	// $s per well			([2B.Resource&Well Input].D68)
	mdWellStimulationCostPerWell =  1000000;	// $s per well			([2B.Resource&Well Input].D111)


	//mdProductionPumpCostOther = 10000;		// $s					([7A.GF Pumps].G143)
	//mdProductionPumpCostCasing = 45;			// $/ft					([7A.GF Pumps].G144)
	//mdProductionPumpCostInstall = 5;			// $/ft					([7A.GF Pumps].G145)
	mdPumpCostPerFoot = 50;						// $/ft, combination of the two above

	// other public
	mdWaterLossPercent = 0.02;

	mdPumpSizeHP = 0;
	mdPumpDepthFt = 0;
	mdPumpSizeHPInjection = 0;
	mdNumberOfWells = 0;  // default is that this will be calculated based on desired Total Power kW, brine effectiveness, and flow
	mdFractionOfInletGFInjected = 1; // 100% is default for binary

	mdDesiredSalesCapacityKW = 15000;
}


double CGETEMBaseInputs::GetAmbientTemperatureC(conversionTypes ct)
{
	if (ct == NO_CONVERSION_TYPE) ct = cst;
	return (ct == BINARY) ? DEFAULT_AMBIENT_TEMPC_BINARY : (1.3842 * mdTemperatureWetBulbC) + 5.1772 ;

}


double CGETEMBaseInputs::GetTemperatureGradient(void) // degrees C per km
{	// Conversation with Chad on August 30th, 10am MT - just use the average gradient, even if it's changing at that point according to the depth/temp graph.
	if (this->rt == HYDROTHERMAL) { return ((mdTemperatureResourceC - GetAmbientTemperatureC(BINARY))/mdResourceDepthM)*1000; }
	return ((mdTemperatureResourceC - mdTemperatureEGSAmbientC)/mdResourceDepthM)*1000;
	//return mdEGSResourceTemperatureGradient; 
}


double CGETEMBaseInputs::GetResourceTemperatureC(void) // degrees C
{
	if ( (this->rt == EGS) && (dc == DEPTH) ) return ((mdResourceDepthM/1000) * GetTemperatureGradient()) + mdTemperatureEGSAmbientC;
	return mdTemperatureResourceC;
}


double CGETEMBaseInputs::GetResourceDepthM(void) // meters
{
	if ( (this->rt == EGS) && (dc == TEMPERATURE) ) return 1000 * (mdTemperatureResourceC - mdTemperatureEGSAmbientC) / GetTemperatureGradient();
	return mdResourceDepthM;
}


makeupAlgorithmType CGETEMBaseInputs::determineMakeupAlgorithm()
{   // This is the logic to determine which makeup algorithm GETEM uses: Binary, Flash, or EGS
    // Just because the user chooses "EGS" from the drop-down box on the "2A.Scenario Input" sheet,
    // does NOT mean that the model will use the results from the EGS makeup sheet.
    
	if ((rt != HYDROTHERMAL) && (rt != EGS)) errMsg = "Reource type not recognized in CGETEMBaseInputs::determineMakeupAlgorithm.";
	if ((cst != BINARY) && (cst != FLASH))   errMsg = "Conversion system not recognized in CGETEMBaseInputs::determineMakeupAlgorithm.";
	if (errMsg != "") return mat;

    if (tdm == ENTER_RATE)
	{ // if user has chosen to enter the temperature decline rate, then the makeup is calculated either with the binary or flash method.
        if (cst == BINARY)
            mat = MA_BINARY;
		else
		{
            if ((ft > NO_FLASH_SUBTYPE) && (ft <= DUAL_FLASH_WITH_TEMP_CONSTRAINT))
                mat = MA_FLASH;
            else
                errMsg = ("Conversion system Set to 'flash', but the type of flash system was not recognized in CGETEMBaseInputs::determineMakeupAlgorithm");
		}
	}
    else if (tdm == CALCULATE_RATE)
	{	// this temperature decline can only be calculated for Binary conversion systems with EGS resources
        if (rt == EGS)
		{
            if (cst == BINARY)
                mat = MA_EGS;
            else
                errMsg = ("Fluid temperature decline rate cannot be calculated for EGS resources using a flash plant");
		}
		else
            errMsg = ("Fluid temperature decline rate cannot be calculated for hydrothermal resources");
	}
	else
		errMsg = ("Error: Fluid temperature decline method not recognized in CGETEMBaseInputs::determineMakeupAlgorithm.");

    return mat;
}


double CGETEMBaseInputs::injectionTemperatureC() // calculate injection temperature in degrees C
{	// Plant design temp AND resource temp have to be Set correctly!!!
	// These are the calculations done at the bottom of [10B.GeoFluid] with the result in D89
	
	// this is used in pump work calculations, and in EGS energy produciton calculations
	if ((this->GetTemperaturePlantDesignC() != this->GetResourceTemperatureC()) && (this->mat != MA_EGS)) { errMsg = ("Resource temperature != plant design temp in non-EGS analysis in CGETEMBaseInputs::injectionTemperatureC"); return 0; }
	//if(mat == MA_EGS) {	errMsg = ("Not ready for EGS in CGETEMBaseInputs::injectionTemperatureC"); return 0; }

	double a = (-0.000655 * GetTemperaturePlantDesignC()) + 1.01964;
	double b = (-0.00244 * GetTemperaturePlantDesignC()) - 0.0567;
	double dPlantBrineEffectiveness = (IMITATE_GETEM) ? 10.35 : GetPlantBrineEffectiveness();
	double eff = dPlantBrineEffectiveness / availableEnergyBinary(); //available energy based on resource temp
	double tr = a * exp(b*eff);
	double t1 = KelvinToCelcius(CelciusToKelvin(GetTemperaturePlantDesignC()) * tr);
	double t2 = GetAmbientTemperatureC() + 27;

	CGeothermalConstants ogc;
	ogc.init(4.205944351495, 0.3672417729236, -0.0036294799613, 0.0000706584462, -0.0000001334837, 0, 0);
	double x = ogc.evaluatePolynomial(GetTemperaturePlantDesignC());

	ogc.init(-0.294394, 0.307616, -0.000119669, -0.00000000425191, 0.0000000000249634, 0, 0);
	double x1 = ogc.evaluatePolynomial(x);

	double t3 = FarenheitToCelcius(CelciusToFarenheit(x1)+1);
	double y = (t1>t2) ? t1 : t2;

	return ( (t3>y) ? t3 : y );
}


double CGETEMBaseInputs::calcEGSReservoirConstant(double avgWaterTempC, double timeDays)
{	// all this is from [7C.EGS Subsrfce HX]
	// this is best done in CGETEMBaseInputs because it requires many CGETEMBaseInputs properties, but it is used in several classes
	double cp = CGETEMGlobals::GG.EGSSpecificHeat(avgWaterTempC); // J/kg-C
	double rho = CGETEMGlobals::GG.EGSWaterDensity(avgWaterTempC); // kg/m^3
	double flow = EGSFlowPerFracture(avgWaterTempC); // m^3 per day
	double lv = EGSLengthOverVelocity(avgWaterTempC); // days
	double x = (mdEGSThermalConductivity * EGSFractureSurfaceArea()) / (cp * rho * flow * sqrt(EGSAlpha()*(timeDays - lv) ) );
	return my_erfc(x);
}


double CGETEMBaseInputs::EGSAverageReservoirTemperatureF(void) //[7C.EGS Subsrfce HX].D52, [7B.Reservoir Hydraulics].D24
{	// all this is from [7C.EGS Subsrfce HX]
	double waterTempC = (IMITATE_GETEM) ? EGSAverageWaterTemperatureC1() : EGSAverageWaterTemperatureC2(); // degrees C
	double days = mdEGSTimeInput * DAYS_PER_YEAR;
	double erfcEGS = calcEGSReservoirConstant(waterTempC, days);

	double tempEGSProductionC = GetResourceTemperatureC() + (mdTemperatureEGSInjectionC - GetResourceTemperatureC()) * erfcEGS;
	return CelciusToFarenheit((mdTemperatureEGSInjectionC + tempEGSProductionC)/2);
}

double CGETEMBaseInputs::secondLawEfficiencyGETEM() // This assumes the use of Binary constants and is only necessary for GETEM method of calculating PlantOutputKW - keep private
{
	double ae = availableEnergyBinary();
	if (ae == 0) { errMsg = ("ae = zero in CGETEMBaseInputs::secondLawEfficiencyGETEM"); return 0;}
	return GetPlantBrineEffectiveness() / ae;
}


bool CGETEMBaseInputs::inputErrors(void)
{
	if (errMsg != "") return true;
	if (!miProjectLifeYears) { errMsg = ("Project life is zero in CGETEMMakeupAnalysis::readyToAnalyze."); return true; }
	if (!miMakeupAnalysesPerYear) { errMsg = ("Time step is zero in CGETEMMakeupAnalysis::readyToAnalyze."); return true; }

	if (GetTemperaturePlantDesignC() > GetResourceTemperatureC()) { errMsg = ("Plant design temperature cannot be greater than the resource temperature."); return true; }

	if (mdPotentialResourceMW < PlantSizeKW()/1000) { errMsg = ("Resource potential must be greater than the gross plant output."); return true; }

	if ( (this->rt != EGS) && (this->pc == SIMPLE_FRACTURE) ) { errMsg = ("Reservoir pressure change based on simple fracture flow can only be calculated for EGS resources."); return true; }

	if ( (this->rt != EGS) && (this->tdm == CALCULATE_RATE) ) { errMsg = ("Temperature decline can only be calculated for EGS resources."); return true; }

	if ((tdm == ENTER_RATE) && (mdTemperatureDeclineRate < 0))
		{ errMsg = ("Fluid temperature decline method chosen was 'enter rate', but the rate is < 0 in CGETEMMakeupAnalysis::readyToAnalyze"); return true; }

	if (GetTemperatureRatio() > MAX_TEMP_RATIO)
		{ errMsg = ("Plant design temperature is too low for resource temperature.  GETEM equations will return invalid results."); return true; }

	if ( this->netBrineEffectiveness() == 0 ) // this will cause a division by zero error
		{ errMsg = ("Inputs led to a divide by zero error.  Pump work = Plant output, so the net efficiency is zero."); return true; }

	if ( this->netBrineEffectiveness() < 0 )
		{ errMsg = ("Inputs lead to required pump energy being greater than the plant output."); return true; }


	if (availableEnergyBinary() == 0)
		{ errMsg = ("Inputs lead to available energy = zero, which will cause a division by zero error."); return true;}

	return false;
}



/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Implementation of CPumpPowerCalculator //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void CPumpPowerCalculator::init(CGETEMBaseInputs* gbi)
{
	mpGBI = gbi;

    mdAdditionalPressure = 0;
	mdPumpSetDepthFt = 0;

	mdCT = 0.0009; // these are both inputs that are shaded out in GETEM
	mdCP = 0.000000000464;

	mbBottomHolePressureCalculated = false;
	mbPressureChangeCalculated = false;
	mbProductionWellsPumped = true;
	mbAdditionalPressureRequired = false;
	mbCalculatePumpDepth = true;
}


double CPumpPowerCalculator::GetTotalPumpPower(void) // watt-hr/lb
{
	double retVal = productionPumpPower() + injectionPumpPower();
	if (retVal < 0)
		{ errMsg = ("CPumpPowerCalculator::GetTotalPumpPower calculated a value < 0"); return 0; }
	return retVal;
}

// PRIVATE FUNCTIONS -----------------------------------------------------------------
double CPumpPowerCalculator::productionPumpPower(void) // ft-lbs/hr
{
	if (!mbProductionWellsPumped) return 0;
	
	// Enter 1 for flow to Get power per lb of flow
	return pumpWorkInWattHr(1, pumpHeadFt(), mpGBI->GetGFPumpEfficiency());
}


double CPumpPowerCalculator::GetCalculatedPumpDepthInFeet(void)
{	// Get the pump Set depth in ft
		
	if (!mbCalculatePumpDepth) return mdPumpSetDepthFt;

	// calculate the pumpSetDepth
	double pressureDiff = GetBottomHolePressure()  - pressureWellHeadPSI();

	double areaWell = areaCircle(DiameterProductionWellFt()/2); // ft^2
	double velocityWell = productionFlowRate()/areaWell;
	double ReWell = DiameterProductionWellFt() * velocityWell * productionDensity()/productionViscosity();
	double frictionHeadLossWell = (frictionFactor(ReWell)/DiameterProductionWellFt())* pow(velocityWell,2)/(2 * GRAVITY_FTS2);

	double pumpSetting = ((pressureDiff*144)/productionDensity())*(1-frictionHeadLossWell);   // [7A.GF Pumps].D89
	return (GetResourceDepthFt() - pumpSetting < 0) ? 0 : GetResourceDepthFt() - pumpSetting; // feet - [7A.GF Pumps].D90
}


double CPumpPowerCalculator::pressureWellHeadPSI(void)
{
	double tempF = CelciusToFarenheit(mpGBI->GetTemperaturePlantDesignC());
	double pressureSaturation = CGETEMGlobals::GG.oPC.evaluatePolynomial(tempF); // valid above boiling, I guess.
	double pressureExcessPSI = BarToPsi(mpGBI->mdExcessPressureBar); // bar to psi
	double pressureAmbientPSI = (IMITATE_GETEM) ? 14.7 : mpGBI->mdPressureAmbientPSI;
	return (mpGBI->GetTemperaturePlantDesignC() > 100) ? pressureSaturation + pressureExcessPSI : pressureAmbientPSI + pressureExcessPSI;
}


double CPumpPowerCalculator::GetBottomHolePressure()
{	// [7B.Reservoir Hydraulics].G75
	if (!mbBottomHolePressureCalculated) {
		mbBottomHolePressureCalculated = true;
		if (mpGBI->rt == EGS)
			mdBottomHolePressure = (pressureInjectionWellBottomHolePSI() + mpGBI->InectionPumpHeadUsed()) - GetPressureChangeAcrossReservoir();
		else
			mdBottomHolePressure = pressureHydrostaticPSI() - GetPressureChangeAcrossReservoir();
	}
	return mdBottomHolePressure;
}

double CPumpPowerCalculator::GetPressureChangeAcrossReservoir()
{	// [7B.Reservoir Hydraulics].G70
    if (mpGBI->pc == ENTER_PC) return mpGBI->mdReservoirDeltaPressure * mpGBI->flowRatePerWell() / 1000.0;
	if (mbPressureChangeCalculated) return mdPressureChangeAcrossReservoir;

	// if user didn't input the pressure change, we have to calculate it.  start with these
	double density = CGETEMGlobals::GG.oDensityConstants.evaluatePolynomial(GetReservoirTemperatureF()); // lbs per ft^3
	double volumetricFlow =(mpGBI->flowRatePerWell() / density)/3600; // ft^3 per second
	double viscosity = 0.115631 * pow(GetReservoirTemperatureF(), -1.199532); // lb per ft-second

	if ((mpGBI->rt == EGS) && (mpGBI->pc == SIMPLE_FRACTURE)  )
	{	// only a valid option for EGS resources
		// calculate the pressure change across the reservoir using simple fracture flow
		double effectiveLengthFt = MetersToFeet(mpGBI->EGSFractureLengthUserAdjusted());  // Only instance where EGSFractureLengthUserAdjusted is used.
		double fractureFlowArea = MetersToFeet(mpGBI->GetEGSFractureAperature()) * MetersToFeet(mpGBI->GetEGSFractureWidthM());  // ft^2
		double hydraulicDiameter = (2 * fractureFlowArea) / (MetersToFeet(mpGBI->GetEGSFractureAperature()) + MetersToFeet(mpGBI->GetEGSFractureWidthM()));  // ft
		double flowPerFracture = volumetricFlow/mpGBI->GetEGSNumberOfFractures(); // ft^3 per second
		double velocity = flowPerFracture/fractureFlowArea; // ft per second
		double Re = density * velocity * hydraulicDiameter / viscosity;
		double frictionFactor = 64/Re;
		double headLoss = frictionFactor * (effectiveLengthFt/hydraulicDiameter) * pow(velocity,2)/(2 * GRAVITY_FTS2); // ft
		mdPressureChangeAcrossReservoir = headLoss * density / 144; // psi
	}
	else {
		// calculate the change in pressure across reservoir using K*A (from [7B.Reservoir Hydraulics].G70)
		double G53 = M2ToFeet2(mpGBI->GetReservoirPermeability() * reservoirAreaSqFt() *  0.000000000000986923); //ft^4
		double G61 = volumetricFlow * viscosity * MetersToFeet(mpGBI->mdDistanceBetweenProductionInjectionWellsM) / G53; // lbs per second^2-ft
		mdPressureChangeAcrossReservoir = G61/GRAVITY_FTS2/144; // change in pressure (psi)
	}
	mbPressureChangeCalculated = true;
	return mdPressureChangeAcrossReservoir;
}


double CPumpPowerCalculator::pressureInjectionWellBottomHolePSI() // [7B.Reservoir Hydraulics].G72, [7A.GF Pumps].G50
{
	double injectionWellSurfacePressurePSI = (mpGBI->cst == FLASH) ? 0 : (pressureWellHeadPSI() - mpGBI->mdPressureChangeAcrossSurfaceEquipmentPSI); // [2B.Resource&Well Input].D149
	// this used to incorrectly convert PSI to bar - does this still work for Binary?????????????????????
	double pMax = (pZero() > injectionWellSurfacePressurePSI) ? pZero() : injectionWellSurfacePressurePSI; //G18
	double depthFt = (IMITATE_GETEM) ? this->GetProductionWellDepthFt() : this->GetInjectionWellDepthFt(); //G22
	double G23 = pMax + injectionDensity() * depthFt/144; // psi

	double flowRate = mpGBI->GetProductionFlowRateKgPerS() / mpGBI->mdRatioInjectionToProduction /(1 - mpGBI->GetWaterLossPercent()); // kg per second
	flowRate = KgToLb(flowRate) / injectionDensity();  // cf per second
	double areaInjectionWell = areaCircle(DiameterInjectionWellFt()/2); // ft^2
	double velocityInjectionWell = flowRate/areaInjectionWell;

	double viscosity = 0.0925 * pow(injectionTempF(),-1.159);
	double ReInjectionWell = DiameterInjectionWellFt() * velocityInjectionWell * injectionDensity()/viscosity;

	double frictionHeadLossInjectionWell = (frictionFactor(ReInjectionWell) * GetInjectionWellDepthFt() / DiameterInjectionWellFt())* pow(velocityInjectionWell,2)/(2 * GRAVITY_FTS2); //feet
	double G36 = frictionHeadLossInjectionWell * injectionDensity() / 144; // conversion to psi

	return G23 - G36; // pressureBHInjection, psi
}



double CPumpPowerCalculator::pressureHydrostaticPSI(void)
{	// calculate the hydrostatic pressure (at the bottom of the well)
	double tempAmbientF = (IMITATE_GETEM) ? CelciusToFarenheit(mpGBI->GetAmbientEGSTemperatureC()) : mpGBI->GetAmbientTemperatureF();
	double pressureAmbientBar = PsiToBar(CGETEMGlobals::GG.oPressureAmbientConstants.evaluatePolynomial(tempAmbientF));

	double tempF = (IMITATE_GETEM) ? CelciusToFarenheit(mpGBI->GetAmbientEGSTemperatureC()) : mpGBI->GetAmbientTemperatureF();
	double densityAmbient = LbPerCfToKgPerM3_B(CGETEMGlobals::GG.oDensityConstants.evaluatePolynomial(tempF));

	double tempAmbientC = (IMITATE_GETEM) ? 10 : mpGBI->GetAmbientTemperatureC(); // GETEM assumes 10 deg C ambient temperature here. Above, the assumption is 15 deg C ambient.
	double tempGradient = (mpGBI->rt == EGS) ? mpGBI->GetTemperatureGradient()/1000 : (mpGBI->GetResourceTemperatureC() - tempAmbientC ) / GetResourceDepthM();  

	// hydrostatic pressure at production well depth (GetResourceDepthFt) in bar
	double pressureHydrostaticBar = pressureAmbientBar + (1/mdCP) * (exp(densityAmbient * GRAVITY_MS2 * mdCP * (GetResourceDepthM() - (0.5 * mdCT * tempGradient * pow(GetResourceDepthM(),2))))-1)/100000;
	
	return BarToPsi(pressureHydrostaticBar);
}

double CPumpPowerCalculator::pumpHeadFt(void) // ft
{	// calculate the friction head loss of the casing
	double areaCasing = areaCircle(DiameterPumpCasingFt()/2); // ft^2
	double velocityCasing = productionFlowRate()/areaCasing;
	double ReCasing = DiameterPumpCasingFt() * velocityCasing * productionDensity()/productionViscosity();
	double frictionHeadLossCasing = (frictionFactor(ReCasing) * GetCalculatedPumpDepthInFeet() / DiameterPumpCasingFt())* pow(velocityCasing,2)/(2 * GRAVITY_FTS2); //feet

	// Add (friction head loss) and (pump Set depth) to Get total pump head.
	return frictionHeadLossCasing + GetCalculatedPumpDepthInFeet();
}


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
	mNCGRemovalType = JET;
	mdNCGLevelPPM = 100;
	mdPressureHPFlashPSI = 0;
	mdPressureLPFlashPSI = 0;
	mdDeltaPressureHPFlashPSI = 2.2;
	mdDeltaPressureLPFlashPSI = 1.0;
	mdEfficiencyTurbine = 0.825;
	mdEfficiencyGenerator = 0.98;
	mdBaseCWPumpHeadFt = 60.0;
	mdDeltaTemperatureCWF = 30.0;												// degrees F
	mdTemperaturePinchPtCondenserF = 10.0;
	mdTemperaturePinchPtCoolingTowerF = 15;
	miNumberOfCoolingStages = 3;												// 1,2, or 3
	mdMoleWeightNCG = 44.0;
	mdMoleWeightH2O = 18.0;
	mdEfficiencyPump = 0.7;
	mdEfficencyVacuumPump = 0.7;
	mdPressureCondenserNCGPartialInHG = 0.5;									// inches of Mercury
	mbFlashPressuresCalculated = false;
	mbBrineEffectivenessCalculated = false;
}

void CFlashBrineEffectiveness::init(CGETEMBaseInputs* gbi)
{
	mpGBI = gbi;
	mbFlashPressuresCalculated = false;
	mbBrineEffectivenessCalculated = false;
}


double CFlashBrineEffectiveness::brineEffectiveness(void)
{
	if (!mbBrineEffectivenessCalculated) {
		this->calculateFlashPressures();

		double dGrossOutput = turbine1OutputKWh();
		if (FlashCount() == 2) dGrossOutput += turbine2OutputKWh();
		double dGrossPower = dGrossOutput * mdEfficiencyGenerator;

		double dParasiticPower = cwPumpingKW() + condensatePumpingKW() + fanPowerKW() + vacuumPumpingKW() + condenserInjectionPumpingKW();
		mdFlashBrineEffectiveness = dGrossPower - dParasiticPower;
		mbBrineEffectivenessCalculated = true;
	}
	return mdFlashBrineEffectiveness;
}


double CFlashBrineEffectiveness::waterLossFractionOfGF(void)
{
	this->calculateFlashPressures();
	return waterLoss() / CGETEMGlobals::GG.mGeothermalFluidForFlash();
}


// Private Functions ------------------------------------------------------------------------------
//////////////////////////////////////// Flash Pressures //////////////////////////////////////////
double CFlashBrineEffectiveness::pressureDualHighWithConstraint()
{
	double a = (temperatureCondF() > 125) ? 1.59 + (0.0015547 * exp(0.0354727*temperatureCondF())) : 1.59 + (0.098693 * exp(0.0025283*temperatureCondF()));
	double b = (temperatureCondF() > 125) ? 0.01916 - (0.000005307 * exp(0.031279921*temperatureCondF())) : 0.01916 - (0.000167123 * exp(0.00400728*temperatureCondF()));
	return a * exp(b * mpGBI->GetTemperaturePlantDesignC());
}

void CFlashBrineEffectiveness::calculateFlashPressures(void)
{	// This function Sets some values that will be used throughout the calculations of the flash brine effectiveness
	// These cannot be Set during initialization since some of the public properties may have been changed.  These
	// need to be calculated right when the brine effectiveness is calculated.

	if (mbFlashPressuresCalculated) return;
	
	// if single flash - add flash pressure to delta pressure and quit
	if (FlashCount() == 1) { mdPressureHPFlashPSI = pressureSingle() + mdDeltaPressureHPFlashPSI; return; }

	// dual flash, have to calculate both
	// high pressure flash
//i think this might be using the wrong temperature - resource instead of plant design - for EGS
	mdPressureHPFlashPSI = pressureDualHigh() + mdDeltaPressureHPFlashPSI;


	// low pressure flash
	mdPressureLPFlashPSI = pressureDualLow() + mdDeltaPressureLPFlashPSI;
	mbFlashPressuresCalculated = true;
}


//////////////////////////////////////// Turbine Output ///////////////////////////////////////////
double CFlashBrineEffectiveness::calculateDH(double pressureIn)
{
	double a = CGETEMGlobals::GG.GetDHa(pressureIn);
	double b = CGETEMGlobals::GG.GetDHb(pressureIn);
	double x = pressureIn /(pressureCondenser());
	return a * log(x) + b;
}

double CFlashBrineEffectiveness::calculateX(double enthalpyIn, double temperatureF)
{
	double enthalpyF = CGETEMGlobals::GG.GetFlashEnthalpyF(temperatureF);
	double enthalpyG = CGETEMGlobals::GG.GetFlashEnthalpyG(temperatureF);
	return (enthalpyIn - enthalpyF)/(enthalpyG-enthalpyF);
}

double CFlashBrineEffectiveness::enthalpyChangeTurbine(double dEnthalpyDeltaInitial, double dEnthalpyTurbineG)
{	// I65-I80, I87-I102
	double xPrime, effTurb, dEnthapyDelta, hEx;

	dEnthapyDelta = dEnthalpyDeltaInitial;
	for (int i=0;i<4;i++) {
		hEx = dEnthalpyTurbineG - dEnthapyDelta;
		xPrime = calculateX(hEx, temperatureCondF());
		xPrime = (xPrime > 0.95) ? 0 : 0.95 - xPrime;
		effTurb = mdEfficiencyTurbine - (0.5 * xPrime);
		dEnthapyDelta = dEnthalpyDeltaInitial * effTurb;
	}
	return dEnthapyDelta;
}



//////////////////////////////////////// NCG Removal //////////////////////////////////////////////
double CFlashBrineEffectiveness::pInter(int stage)
{	// D156, D205, D253 - psi
	switch (stage)
	{
		case 0: return pTotal();
		case 1: return pTotal() * pRatio();
		case 2: return (miNumberOfCoolingStages > 2) ? pTotal() * pRatio() * pRatio()  : mpGBI->mdPressureAmbientPSI;
		case 3: return mpGBI->mdPressureAmbientPSI;
		default: { errMsg = ("Invalid stage in CFlashBrineEffectiveness::pInter"); return 0; }
	}
}



double CFlashBrineEffectiveness::mForNCGRemoval()
{
	if(this->mNCGRemovalType == VAC_PUMP) return 0;

	double dSteamFlow = steamFlow(1); 
	if (miNumberOfCoolingStages > 1) { dSteamFlow += steamFlow(2); }
	if (miNumberOfCoolingStages > 2) { dSteamFlow += steamFlow(3); }
	return dSteamFlow;
}



//////////////////////////////////////// CW Pump Power KW /////////////////////////////////////////
double CFlashBrineEffectiveness::overAllHEx() //I107
{ return (this->FlashCount() == 2) ? ((turbine2HEx() * turbine2Steam()) + (turbine1HEx() * turbine1NetSteam()))/(turbine1NetSteam()+turbine2Steam()) : turbine1HEx(); }



double CFlashBrineEffectiveness::pumpWorkFromSteamFlow(double flow)
{
	double enthalpyCondF = CGETEMGlobals::GG.GetFlashEnthalpyF(temperatureCondF());
	double enthalpyCondG = CGETEMGlobals::GG.GetFlashEnthalpyG(temperatureCondF());
	
	double qReject = flow * (enthalpyCondG - enthalpyCondF);
	double cwFlow = qReject / mdDeltaTemperatureCWF;
	double pumpHead = mdBaseCWPumpHeadFt + CGETEMGlobals::GG.additionalCWPumpHeadSurface();
	return pumpWorkKW(cwFlow, pumpHead);
}


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Implementation of CFlashMakeup //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
double CFlashMakeup::fractionOfMaxEfficiency() 
{
    double tr = temperatureRatio();
	initializeSecondLawConstants();
	return (1.1 - (0.1 * pow(tr, moSecondLawConstants.evaluatePolynomial(CelciusToKelvin(mpGBI->GetResourceTemperatureC())) )));
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

		default: errMsg = ("Invalid flash technology in CFlashMakeup::initializeSecondLawConstants"); return;
	}
	mbInitialized = true;
}


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Implementation of CEGSMakeup //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
bool CEGSMakeup::canReplaceReservoir(double dTimePassedInYears)
{
	mdYearsAtNextTimeStep = dTimePassedInYears;
	return ( (miReservoirReplacements < mpGBI->NumberOfReservoirs() ) && (dTimePassedInYears + mpGBI->mdFinalYearsWithNoReplacement <= mpGBI->miProjectLifeYears) ) ? true : false;
}

void CEGSMakeup::replaceReservoir(void)
{
	miReservoirReplacements++; 
	mdWorkingTemperatureC = mpGBI->GetResourceTemperatureC(); 
	mdLastProductionTemperatureC = mdWorkingTemperatureC; 
	if (mdYearsAtNextTimeStep > 0) mdTimeOfLastReservoirReplacement = mdYearsAtNextTimeStep - (mpGBI->EGSTimeStar() / DAYS_PER_YEAR);
}


double CEGSMakeup::plantBrineEfficiency(void)
{
    mdCurrentEfficiency = maxSecondLawEfficiency() * fractionOfMaxEfficiency();
    return mdCurrentEfficiency * mpGBI->GetAEBinaryAtTemp(mdWorkingTemperatureC); // oSP.GeothermalFuildContainer.availableEnergyWattHr(Binary, toF(fWorkingTemperatureC))
}


double CEGSMakeup::newInjectionTemperatureC(void)
{
    double tempBrineEfficiencyC = KelvinToCelcius( exp((-0.42 * log(mdLastProductionTemperatureC) + 1.4745) * mdCurrentEfficiency) * CelciusToKelvin(mdLastProductionTemperatureC));
	double tempSILimitC = FarenheitToCelcius(CGETEMGlobals::GG.GetSiPrecipitationTemperatureF(LastProducitonTemperatureF()));
	return (tempBrineEfficiencyC >= tempSILimitC) ? tempBrineEfficiencyC : tempSILimitC;
}


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Implementation of CGETEMMakeupAnalysis //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
CGETEMMakeupAnalysis::CGETEMMakeupAnalysis(void)
{
	init();
}

CGETEMMakeupAnalysis::~CGETEMMakeupAnalysis(void)
{
	if (moMA) delete moMA;
	//if (madNetPower) delete []madNetPower;
	//if (maiReplacements) delete []maiReplacements;
	//if (madTemperatureC) delete []madTemperatureC;
}

void CGETEMMakeupAnalysis::init(void)
{
	moMA = NULL;
	errMsg = "";
	//madNetPower = NULL;
	//maiReplacements = NULL;
	//madTemperatureC = NULL;
	mbAnalysisRequired = true;
	mbRunSetup = false;

	// make sure the pump work calculator and flash brine effectiveness calculator have a pointer to the base inputs
	moFBE.init(this); 
	moPPC.init(this);
}

bool CGETEMMakeupAnalysis::readyToAnalyze()
{
	if ( inputErrors() ) return false;
	if (moMA) delete moMA; moMA=NULL;

	switch (determineMakeupAlgorithm())  // determineMakeupAlgorithm Set and returns this->mat
	{
		case MA_BINARY:
			moMA = new CBinaryMakeup;
			break;
			
		case MA_FLASH:
			moMA = new CFlashMakeup;
			break;

		case MA_EGS:
			moMA = new CEGSMakeup;
			break;

		default:
			errMsg = ("Could not determine makeup algorithm from scenario parameters in CGETEMMakeupAnalysis::readyToAnalyze.");
			return false;
	}

	return true;
}

bool CGETEMMakeupAnalysis::analyze(void)
{
	mbAnalysisRequired = true;
	if (!readyToAnalyze()) { this->mbRunSetup = false; return false; }  //moMA is reset each time readyToAnalyze is called. don't 'setup' run until after this!!!

	// Since we're ready to analyze, setup run parameters that require more intensive calculations
	SetupRun();

	// ReSet all calculated values to zero
    mdLifeTimeDiscountedDesignPower = 0;
    mdLifeTimeDiscountedNetPower = 0;
	mdPresentCostFactorFieldReplacements = 0;
	mdSumOfPresentWorthFactors = 0;

    double dElapsedTimeInYears = 0.0;
	bool bCanReplaceReservoir = false;
    
    // Initialize reservoir replacement counters, temperatures, etc.
    moMA->replaceReservoir();

    for (int iElapsedTimeSteps = 0;  iElapsedTimeSteps < analysisTimeSteps();  iElapsedTimeSteps++)
	{
		maiReplacements[iElapsedTimeSteps] = 0;
		madTemperatureC[iElapsedTimeSteps] = moMA->GetWorkingTemperatureC();
		dElapsedTimeInYears = iElapsedTimeSteps * timeStepInYears();
		madNetPower[iElapsedTimeSteps] = moMA->plantNetPower();
		mdLifeTimeDiscountedNetPower += discountValue(madNetPower[iElapsedTimeSteps], mdAnnualDiscountRate, dElapsedTimeInYears); 
        mdLifeTimeDiscountedDesignPower += discountValue(DesignCapacityKW() / 1000, mdAnnualDiscountRate, dElapsedTimeInYears);
        mdSumOfPresentWorthFactors += discountValue(timeStepInYears(), mdAnnualDiscountRate, dElapsedTimeInYears);
		//madTestValues[iElapsedTimeSteps] = moMA->TestValue();

        // Is is possible and do we want to replace the reservoir in the next time step?
		// moMA->canReplaceReservoir HAS to run, even if "wantToReplaceReservoir" returns false, because it sets a value in moMA.
		// So, first run it and get the value, then use if statement.  (C++ in VC++ will not evaluate further parts of the expression
		// if the first one is false, since it knows the statement will be false.  Other compilers could evaluate from the right hand
		// side.  In order to make sure this works consistently - separate the expression.)
		bCanReplaceReservoir = moMA->canReplaceReservoir(dElapsedTimeInYears + timeStepInYears());
        if ( (moMA->wantToReplaceReservoir()) && (bCanReplaceReservoir) )
		{
            // Yes, so keep track of reservoir temperatures, replacements, etc.
            moMA->replaceReservoir();
			maiReplacements[iElapsedTimeSteps] = 1;
            
            // Calculate costs
            mdPresentCostFactorFieldReplacements += discountValue(1, mdAnnualDiscountRate, dElapsedTimeInYears); // discounted number of field replacements (mult by field cost = discounted cost of future field replacements)
		}
        else
            moMA->calculateNewTemperature(); // reduce temperature from last temp
	}
	if(mdLifeTimeDiscountedDesignPower == 0) { errMsg = ("LifeTime Discounted Design Power was zero in CGETEMMakeupAnalysis::analyze"); return false; }
	if(mdSumOfPresentWorthFactors == 0) { errMsg = ("Sum of Present Worth Factors was zero in CGETEMMakeupAnalysis::analyze"); return false; }
	mbAnalysisRequired = false;
	return true;
}



void CGETEMMakeupAnalysis::SetupRun(void) // private, only called in readyToAnalyze
{
	if (this->mbRunSetup) return;

	// make sure this is done after pump work, flash brine effectiveness, etc. and after moMA is set for use in CGETEMMakeupAnalysis
	moMA->SetScenarioParameters(this);

	// create space for dynamic arrays
	madNetPower.resize(analysisTimeSteps());
	//if (madNetPower) delete []madNetPower;
	//madNetPower = new double[analysisTimeSteps()];

	maiReplacements.resize(analysisTimeSteps());
	//if (maiReplacements) delete []maiReplacements;
	//maiReplacements = new int[analysisTimeSteps()];

	madTemperatureC.resize(analysisTimeSteps());
	//if (madTemperatureC) delete []madTemperatureC;
	//madTemperatureC = new double[analysisTimeSteps()];
	
	madTestValues.resize(analysisTimeSteps());

	// if we got here, the values have been set up, arrays have been allocated
	this->mbRunSetup = true;
}

double CGETEMMakeupAnalysis::GetFractionOfInletGFInjected(void)
{
	if (this->rt == EGS)
		return (1 + mdWaterLossPercent);
	else
		if (this->cst == BINARY)
			return 1;
		else
			return (1 - moFBE.waterLossFractionOfGF());

}


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////// Implementation of CGETEMOutputs ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
CGETEMOutputs::CGETEMOutputs(void)
{
	// costs curves
	pwccc = MED;
	iwccc = MED;
}


double CGETEMOutputs::wellCostEstimate(double depthFt, wellCostCurveChoices costCurve)
{
	double factor;
	switch(costCurve)
	{
		case LOW:  factor = 0.0001188; break;
		case HIGH: factor = 0.0001893; break;
		default:   factor = 0.0001490; break;
	}
	return ppiIndexWell() * 580 * exp(factor * depthFt) * 1000;
}



double CGETEMOutputs::royaltyDollarsPerKWhrMinusContingencies(void)
{	// dollars/kWh - NOT including royalties on contingencies
	// royalties in GETEM are supposed to = 10% (input) of "field-related annual costs to approximate the BLM calculation"

	if (IMITATE_GETEM)
	{
		// these seem reasonable
		double annualExploreConfirmCosts = annualize(capitalCostsConfirmation() + capitalCostsExploration() );		// $s/yr
		double annualMainWellCapitalCosts = annualize(capitalCostsMainWells());										
		double annualMainWellOMCosts = (GetOMCostsField()) * energyProductionKWh();
		double annualOtherFieldCapitalCosts = annualize(capitalCostsOtherField());
		double annualOtherFieldOMCosts = (GetOMCostsOther()) * energyProductionKWh();
		double annualFieldReplacementCosts =  levelizedTotalMakeupCostDollars();

		// units seem wrong on these in GETEM
		double royaltyExploreConfirm = royaltyCostFunctionGETEM(1000 * annualExploreConfirmCosts / PowerSalesKW());						// $s/yr per MW per kWh ??

		double royaltyGETEMMixedUnits1 = (1000 * annualMainWellCapitalCosts / PowerSalesKW())  + annualMainWellOMCosts;				// ($s/yr per MW) + ($/yr) ??
		double royaltyMainWell = royaltyCostFunctionGETEM(royaltyGETEMMixedUnits1);														// mixed units

		double royaltyGETEMMixedUnits2 = (1000 * annualOtherFieldCapitalCosts / PowerSalesKW())  + annualOtherFieldOMCosts;			// ($s/yr per MW) + ($/yr) ??
		double royaltyOtherField = royaltyCostFunctionGETEM(royaltyGETEMMixedUnits2);
		double royaltyFieldReplacements = royaltyCostFunctionGETEM(1000 * annualFieldReplacementCosts / PowerSalesKW()); 

		return royaltyExploreConfirm + royaltyMainWell + royaltyOtherField + royaltyFieldReplacements;
	}
	else
		return royaltyCostFunctionNew(( costPerKWh(capitalCostsPlant() + capitalCostsField()) + dollarsPerKWhCaptialFuture() ) + dollarsPerKWhOMTotal());
        // From Chad Augustine, Oct 2009: royalties = 1.75% of gross proceeds for years 1-10, 3.5% after that.
        // As a simplification, assume that all electricity produced is sold at an average price, and that the price
        // negotiated is 10% over the cost of generating it.
}
