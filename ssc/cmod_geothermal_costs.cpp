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

#include "core.h"
#include <stdio.h>
#include <math.h>
#include "lib_geothermal.h"



static var_info _cm_vtab_geothermal_costs[] = {
/*   VARTYPE           DATATYPE         NAME                              LABEL                                                       UNITS     META                      GROUP                   REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
																	      								                             
	// Inputs		
	{ SSC_INPUT,       SSC_NUMBER,      "gross_output",                   "Gross output from GETEM",							       "kW",        "",				    "GeoHourly",				"*",					  "",								"" },
	{ SSC_INPUT,       SSC_NUMBER,		 "design_temp",                    "Power block design temperature",					      "C",       "",					 "GeoHourly",			   "*",                        "",						         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "eff_secondlaw",				   "Second Law Efficiency",									  "%",        "",				     "GeoHourly",			   "*",                        "",							     "" },

	// Outputs	
	
	{ SSC_OUTPUT,       SSC_NUMBER,      "corrected_equip_cost",			"Total Equipment Cost",									  "$s",		 "",                      "GeoHourly",			   "*",                         "",                              "" },
var_info_invalid };



class cm_geothermal_costs : public compute_module
{
private:

	//Inputs
	std::vector< double> hx_ppi{ 0.89055793991,0.91950405341,0.93872198379,0.95674773486,0.96361468765,0.97229375298,0.98316642823,1.00000000000,0.99842632332,1.06628516929,1.22651406772,1.33285646161,1.37768240343,1.43814973772,1.41473533619,1.42336671435,1.46399618503,1.51297091082,1.53476394850,1.55479256080,1.60551437118,1.00000000000};		//HX Cost Index Normalized to 2001, 2002, 2007, 2010 and 2012; Beginning Year = 1995; Final Year = 2016;
	std::vector<double> steel_ppi{ 1.12931979250,	1.10309052385,	1.10871630014,	1.07408489808,	0.99985387594,	1.02228391905,	0.96156937240,	1.00000000000,	1.06517133046,	1.42390589611,	1.50047490319,	1.63534740995,	1.76276758968,	2.16044421714,	1.61320961496,	1.95981588369,	2.22020895740,	2.11039672682,	1.98494922189,	2.03492365018,	1.73196862318,	1.00000000000};	//Steel Cost Index Normalized to 2001, 2002, 2007, 2010 and 2012; Beginning Year = 1995; Final Year = 2016;
	std::vector<double> process_equip_ppi{ 0.88401892893,	0.90747040252,	0.92618126403,	0.94251808176,	0.95685145806,	0.96782339552,	0.98539534756,	1.00000000000,	1.01482944280,	1.07777492841,	1.15529549518,	1.22276690058,	1.30481820223,	1.38289340612,	1.40355926043,	1.41145093486,	1.45554814437,	1.50964978430,	1.53375704823,	1.63903161719,	1.65644438516,	1.00000000000}; //Process Equipment Cost Index Normalized to 2001, 2002, 2007, 2010 and 2012; Beginning Year = 1995; Final Year = 2016;
	std::vector<double> engineering_ppi{ 0.77985529049,	0.81069560857,	0.85901568880,	0.88856651623,	0.91331757289,	0.95404398596,	0.97585786947,	1.00000000000,	1.04810516513,	1.08163192206,	1.10233541085,	1.13582634859,	1.20943477326,	1.27498388137,	1.32975141486,	1.39193351959,	1.36234687299,	1.36474675836,	1.38835160112,	1.43348377391,	1.48482894711,	1.00000000000}; // Engineering Cost Index Normalized to 2001, 2002, 2007, 2010 and 2012; Beginning Year = 1995; Final Year = 2016;
	std::vector<double> pump_ppi{ 0.853374524715,	0.872338403042,	0.899382129278,	0.924049429658,	0.936264258555,	0.950332699620,	0.975903041825,	1.000000000000,	1.010646387833,	1.039876425856,	1.093203422053,	1.141682509506,	1.212975285171,	1.277851711027,	1.314115969582,	1.324192015209,	1.324572243346,	1.348716730038,	1.339163498099,	1.366539923954,	1.391168337366,	1.000000000000}; //Pump Cost Index Normalized to 2001, 2002, 2007, 2010 and 2012; Beginning Year = 1995; Final Year = 2016;
	std::vector<double> turbine_ppi{ 0.882749597424,	0.895883655395,	0.917874396135,	0.934581320451,	0.960396537842,	0.969303542673,	0.980072463768,	1.000000000000,	1.013285024155,	1.018820450886,	1.017562399356,	1.050221417069,	1.106682769726,	1.245118760064,	1.350392512077,	1.340328099839,	1.359752415459,	1.349889291465,	1.376811594203,	1.411835748792,	1.395147123408,	1.000000000000}; //Turbine-Generator Cost Index Normalized to 2001, 2002, 2007, 2010 and 2012; Beginning Year = 1995; Final Year = 2016;
	double user_adjust = 1;
	double size_ratio ;
	//double scaling_factor ;	//for the GF HX
	double ref_plant_size = 10000;	//kW
	double hx_cost_adjust = 1;
	double hx_cost;
	double sf_hx;
	double hx_gf_c1;
	double hx_gf_c2;
	double hx_gf_c;	//ref gf hx cost
	double sales_tax = 0.05; //Always 5% ? 
	double freight = 0.05;	//Also always 5% ?
	double total_material_cost_multiplier = 1.7;
	double steel = 0.22;
	double current_cost_ref_hx;

	double sf_condenser;
	double condenser_cost;
	double wf_pump_cost;
	double turbine_cost;

	double corrected_equip_cost;
	
	//Defining variables used in Direct Construction Cost Multiplier:
	double dc_cost_multiplier;	//Direct Construction Cost Multiplier  [Defined]
	double corrected_total_material_mult;	
	double corrected_construct_malts;
	double plant_size_adjustment;
	double direct_installation_multiplier;
	double escalation_equip_cost;	//Escalation in Equipment Cost
	double const_matls_rentals = 0.25;
	double multiplier_input_year;
	double corrected_labor;
	double labor_cost_multiplier = 0.27;
	double labor_fringe_benefits = 0.45;
		
	//Coefficients for GF HX cost calculation:
	double sf_0 = 1.01216;
	double sf_1 = -0.000760473;
	double sf_2 = 2.0145e-6;
	double sf_3 = 0;
	double hx_c10 = 5.95;
	double hx_c11 = 2163827753;
	double hx_c12 = -3.810541361;
	double hx_c20 = -22.09917;
	double hx_c21 = 0.4275955;
	double hx_c22 = -0.002356472;
	double hx_c23 = 4.244622e-06;

	//Coefficients for Air Cooled Condenser (ACC) Cost Calculations:
	double acc_c0 = 47;
	double acc_c1 = 11568490;
	double acc_c2 = -2.350919;
	double acc_c10 = 15.52712;
	double acc_c11 = 0.005950211;
	double acc_c12 = -0.001200635;
	double acc_c13 = 0.000005657483;
	double acc_c20  = 3.582461;
	double acc_c21 = -0.05107826;
	double acc_c22 = 0.000277465;
	double acc_c23 = -2.549391e-07;
	double acc_0;
	double acc_1;
	double acc_2;
	double accc_0;
	double accc_1;
	double accc_2;
	double acc_c;	//ref acc cost 
	double current_cost_ref_acc;

	//Coefficients for WF Pump Cost Calculation:
	double sf_wf;
	double wf_sf_c0 = -0.777900000001;
	double wf_sf_c1 = 0.029802;
	double wf_sf_c2 = -0.00019008;
	double wf_sf_c3 = 3.872e-07;
	double wf_c10 = 32.0607143;
	double wf_c11 = -0.2537857;
	double wf_c12 = 0.0006714;
	double wf_c20 = -0.3329714;
	double wf_c21 = 0.0559291;
	double wf_c22 = -0.0001977;
	double pcc_1;
	double pcc_2;
	double pcc_c;	//ref wf pump cost
	double current_cost_ref_pcc;

	//Coefficients for Turbine Cost Calculation: 
	double turbine_sf_c0 = 0.6642;
	double turbine_sf_c1 = 0.0003589091;
	double turbine_sf_c2 = -2.0218e-06;
	double sf_turbine;
	double turbine_c0 = 0.79664761905;
	double turbine_c1 = -0.00977366138;
	double turbine_c2 = 0.00004244825;
	double turbine_c3 = -5.321e-08;
	double turbine_c10 = -24.62889285714;
	double turbine_c11 = 0.49768131746; 
	double turbine_c12 = -0.00296254476;
	double turbine_c13 = 5.52551e-06;
	double ppc_0;
	double ppc_1;
	double turbine_c;	//Reference Turbine Cost
	double generator_c; //Reference Generator Cost
	double max_turbine_size = 15000*0.7457; //From GETEM
	double parasitic;
	double tg_size;
	double tg_sets;
	double ref_turbine_cost;
	double tg_cost;
	double current_cost_ref_tg;

	double plant_equip_cost;

public:
	//double hx_cost();

	cm_geothermal_costs() {

		add_var_info(_cm_vtab_geothermal_costs);
	}
	
	

	void exec() 
	{
		// for inputs, to get use as_integer, as_double, etc, e.g.
		//double unit_plant = as_double("nameplate");		//Gross plant size
		double design_temp = as_double("design_temp");	
		double eff = as_double("eff_secondlaw");	// w-h/lb
		double unit_plant = as_double("gross_output");

		//GF HX Equipment Cost Calculations:				
		size_ratio = unit_plant / ref_plant_size;
		sf_hx = (sf_3 * pow(design_temp,3)) + (sf_2 * pow(design_temp, 2)) + (sf_1 * design_temp) + sf_0 ;
		hx_gf_c1 = hx_c10 + (hx_c11 * pow(design_temp, hx_c12));
		hx_gf_c2 = hx_c20 + (hx_c21 * design_temp) + (hx_c22 * pow(design_temp, 2)) + (hx_c23 * pow(design_temp, 3));
		hx_gf_c = hx_gf_c1 * exp(hx_gf_c2 * eff);
		current_cost_ref_hx = hx_gf_c * hx_ppi[20] ;

		hx_cost = user_adjust * pow(size_ratio, sf_hx)*((ref_plant_size*hx_gf_c*hx_ppi[20]) / unit_plant);


		//Air Cooled Condenser Cost Calculations:
		sf_condenser = 1;
		accc_0 = (acc_c1 * pow(design_temp, acc_c2)) + acc_c0;
		accc_1 = exp((acc_c13*pow(design_temp, 3)) + (acc_c12*pow(design_temp, 2)) + (acc_c11*design_temp) + acc_c10);
		accc_2 = exp((acc_c23*pow(design_temp, 3)) + (acc_c22*pow(design_temp, 2)) + (acc_c21*design_temp) + acc_c20);
		acc_c = accc_1 * pow(eff, accc_2) + accc_0;
		//acc_c = 183.65;
		current_cost_ref_acc = acc_c * hx_ppi[20];
		condenser_cost = user_adjust * pow(size_ratio, sf_condenser)*((ref_plant_size * acc_c * hx_ppi[20])/ unit_plant);
		


		//WF Pumps Cost Calculation:
		sf_wf = (wf_sf_c3*pow(design_temp,3)) + (wf_sf_c2*pow(design_temp, 2)) + (wf_sf_c1*design_temp) + wf_sf_c0;
		pcc_1 = (wf_c12 * pow(design_temp, 2)) + (wf_c11 * design_temp) + wf_c10;
		pcc_2 = (wf_c22 * pow(design_temp, 2)) + (wf_c21 * design_temp) + wf_c20;
		pcc_c = pcc_1 * exp(pcc_2*eff);
		current_cost_ref_pcc = pcc_c * pump_ppi[20] ;
		wf_pump_cost = user_adjust * pow(size_ratio, sf_wf)*((ref_plant_size * pcc_c * pump_ppi[20])/ unit_plant);


		//Turbine Cost Calculation:
		if (unit_plant < ref_plant_size)
			sf_turbine = (turbine_sf_c2*pow(design_temp, 2)) + (turbine_sf_c1*design_temp) + turbine_sf_c0;
		else
			sf_turbine = 1;

		ppc_0 = turbine_c0 + (turbine_c1*design_temp) + (turbine_c2*pow(design_temp, 2)) + (turbine_c3*pow(design_temp, 3));
		ppc_1 = turbine_c10 + (turbine_c11*design_temp) + (turbine_c12*pow(design_temp, 2)) + (turbine_c13*pow(design_temp, 3));
		parasitic = (ppc_0 * exp(ppc_1 * eff))*ref_plant_size;
		tg_size = ref_plant_size + parasitic;
		tg_sets = tg_size / max_turbine_size ;

		if (tg_sets > 1)
			turbine_c = 7400 * pow(max_turbine_size, 0.6);		//Total $ of reference turbine
		else
			turbine_c = 7400 * pow(tg_size, 0.6);				//Total $ of reference turbine

		if (tg_sets < 1)
			ref_turbine_cost = turbine_c / ref_plant_size;		//Cost of turbine per kW ($/kW)
		else
			ref_turbine_cost = (turbine_c * tg_sets) / ref_plant_size;

		generator_c = ((1800 * pow(tg_size, 0.67)))/ref_plant_size;
		tg_cost = ref_turbine_cost + generator_c;
		current_cost_ref_tg = tg_cost * turbine_ppi[20];
		turbine_cost = user_adjust * pow(size_ratio, sf_turbine)*((ref_plant_size * tg_cost * turbine_ppi[20])/ unit_plant);
		

		//Calculating Direct Construction Cost Multiplier
		escalation_equip_cost =(current_cost_ref_acc + current_cost_ref_hx + current_cost_ref_pcc + current_cost_ref_tg)/(hx_gf_c + acc_c + pcc_c + tg_cost) ;
		corrected_labor = ((labor_cost_multiplier*engineering_ppi[20])/escalation_equip_cost)*(1+ labor_fringe_benefits );
		corrected_construct_malts = (const_matls_rentals * process_equip_ppi[20]) / escalation_equip_cost;
		corrected_total_material_mult = ((steel*steel_ppi[20]) + ((total_material_cost_multiplier - 1 - steel)*process_equip_ppi[20]))*(1 / escalation_equip_cost) + 1;
		multiplier_input_year = corrected_total_material_mult + corrected_labor + corrected_construct_malts ;
		plant_size_adjustment = 1.02875*pow((unit_plant / 1000), -0.01226);
		direct_installation_multiplier = plant_size_adjustment * multiplier_input_year;
		dc_cost_multiplier = (sales_tax + freight)*((corrected_total_material_mult + corrected_construct_malts)*plant_size_adjustment) + direct_installation_multiplier;

		

		//Total Plant Cost: 
		plant_equip_cost = hx_cost + condenser_cost + wf_pump_cost + turbine_cost;
		corrected_equip_cost = dc_cost_multiplier * plant_equip_cost;


		// for outputs, to assign, use:
		//assign("dc_cost_multiplier", var_data(static_cast<ssc_number_t>(dc_cost_multiplier)));
		assign("corrected_equip_cost", var_data(static_cast<ssc_number_t>(corrected_equip_cost)));

	};

};

DEFINE_MODULE_ENTRY(geothermal_costs, "Geothermal monthly and hourly models using general power block code from TRNSYS Type 224 code by M.Wagner, and some GETEM model code.", 3);
