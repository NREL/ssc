/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as “System Advisor Model” or “SAM”. Except
*  to comply with the foregoing, the terms “System Advisor Model”, “SAM”, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/
#ifndef __lib_ondinv_h
#define __lib_ondinv_h

#include <string>
#include <vector>
//#include "mlm_spline.h" // spline interpolator for efficiency curves
#include "bspline.h"
using namespace std;
using namespace SPLINTER;

class ond_inverter
{
public:
	ond_inverter();

	double PNomConv; // [W]
	double PMaxOUT; // [W]
	double VOutConv; // [W]
	double VMppMin; // [V]
	double VMPPMax; // [V]
	double VAbsMax; // [V]
	double PSeuil; // [W]
	string ModeOper; // [-]
	string CompPMax; // [-]
	string CompVMax; // [-]
	string ModeAffEnum; // [-]
	double PNomDC; // [W]
	double PMaxDC; // [W]
	double IMaxDC; // [A]
	double INomDC; // [A]
	double INomAC; // [A]
	double IMaxAC; // [A]
	double TPNom; // [°C]
	double TPMax; // [°C]
	double TPLim1; // [°C]
	double TPLimAbs; // [°C]
	double PLim1; // [kW]
	double PLimAbs; // [kW]
	double VNomEff[3]; // [V]
	int NbInputs; // [-]
	int NbMPPT; // [-]
	double Aux_Loss; // [W]
	double Night_Loss; // [W]
	double lossRDc; // [V/A]
	double lossRAc; // [A]
	int effCurve_elements; // [-]
	double effCurve_Pdc[3][100]; // [W]
	double effCurve_Pac[3][100]; // [W]
	double effCurve_eta[3][100]; // [-]
	int doAllowOverpower; // [-] // ADDED TO CONSIDER MAX POWER USAGE [2018-06-23, TR]
	int doUseTemperatureLimit; // [-] // ADDED TO CONSIDER TEMPERATURE LIMIT USAGE [2018-06-23, TR]

	bool acpower(	
		/* inputs */
		double Pdc,			/* Input power to inverter (Wdc) */
		double Vdc,			/* Voltage input to inverter (Vdc) */
		double Tamb,		/* Ambient temperature (°C) */

		/* outputs */
		double *Pac,		/* AC output power (Wac) */
		double *Ppar,		/* AC parasitic power consumption (Wac) */
		double *Plr,		/* Part load ratio (Pdc_in/Pdc_rated, 0..1) */
		double *Eff,		/* Conversion efficiency (0..1) */
		double *Pcliploss,	/* Power loss due to clipping loss (Wac) */
		double *Psoloss,	/* Power loss due to operating power consumption (Wdc) */
		double *Pntloss,	/* Power loss due to night time tare loss (Wac) */
		double *dcloss,		/* DC power loss (Wdc) */
		double *acloss		/* AC power loss (Wac) */
	);
	double calcEfficiency(
		double Pdc,
		int index_eta
	);
	double tempDerateAC(
		double arrayT[],
		double arrayPAC[],
		double T
	);
	virtual void initializeManual();

private:
	bool ondIsInitialized;

	int noOfEfficiencyCurves;
//	tk::spline effSpline[2][3];
//	BSpline m_bspline3[2][3];
	BSpline m_bspline3[3];
	double x_max[3];
	double x_lim[3];
	double Pdc_threshold;
	double a[3];
	double b[3];

	double PNomDC_eff;
	double PMaxDC_eff;
	double INomDC_eff;
	double IMaxDC_eff;
	double T_array[6];
	double PAC_array[6];



};

#endif
