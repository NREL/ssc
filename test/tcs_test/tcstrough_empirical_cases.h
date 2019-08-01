#ifndef _TCSTROUGH_EMPIRICAL_CASES_H
#define _TCSTROUGH_EMPIRICAL_CASES_H

#include <map>
#include "../input_cases/code_generator_utilities.h"
#include "tcstrough_empirical_common_data.h"

/**
* Data for high-level integration tests that verifies whether results for a parabolic trough (empirical)
* plant in Tucson, AZ matches expected results.
* Data generated from code-generator (Shift+F5) within SAM UI.
* Test uses SSCAPI interfaces (similiar to SDK usage) to pass and receive data to tcstrough_empirical
*/

// Parabolic trough (empirical) default configuration
int tcstrough_empirical_tucson_default(ssc_data_t &data)
{
	tcstrough_empirical_default(data);

	int status = run_module(data, "tcstrough_empirical");

	//single_owner_default(data);
	//status += run_module(data, "singleowner");

	return status;
}

// Parabolic trough (empirical) with alternative Solar Field HTF type
// Solar Field HTF type: Hitec Solar Salt
// Rest default configurations 
int tcstrough_empirical_tucson_solar_field_HTF(ssc_data_t &data)
{
	tcstrough_empirical_default(data);

	ssc_data_set_number(data, "HTFFluid", 18);
	ssc_data_set_number(data, "PTSmax", 676.471);
	ssc_data_set_number(data, "PFSmax", 342.699);

	int status = run_module(data, "tcstrough_empirical");

	return status;
}

// Parabolic trough (empirical) with alternative Solar Collector Assembly (SCA)
// Solar collector assembly (SCA): EuroTrough ET150
// Rest default configurations 
int tcstrough_empirical_tucson_SCA(ssc_data_t &data)
{
	tcstrough_empirical_default(data);

	ssc_data_set_number(data, "Solar_Field_Area", 856740);
	ssc_data_set_number(data, "Ave_Focal_Length", 2.1);
	ssc_data_set_number(data, "ScaLen", 150);
	ssc_data_set_number(data, "SCA_aper", 5.75);
	ssc_data_set_number(data, "TrkTwstErr", 0.99);
	ssc_data_set_number(data, "MirCln", 0.97);
	ssc_number_t p_HCEdust[4] = { 0.98, 0.98, 0.98, 0.98 };
	ssc_data_set_array(data, "HCEdust", p_HCEdust, 4);
	ssc_number_t p_RefMirrAper[4] = { 5.75, 5.75, 5.75, 5.75 };
	ssc_data_set_array(data, "RefMirrAper", p_RefMirrAper, 4);
	ssc_data_set_number(data, "SfPar", 0.228);
	ssc_data_set_number(data, "ChtfPar", 9.013);
	ssc_data_set_number(data, "AntiFrPar", 0.9013);

	int status = run_module(data, "tcstrough_empirical");

	return status;
}

// Parabolic trough (empirical) with alternative Heat Collection Element (HCE)
// Heat Collection Element (HCE): Luz Cermet Vacuum, Luz Cermet Hydrogren, and Luz Cermet Broken Glass
// Rest default configurations 
int tcstrough_empirical_tucson_HCE(ssc_data_t &data)
{
	tcstrough_empirical_default(data);

	ssc_data_set_number(data, "Solar_Field_Area", 1015848);
	ssc_number_t p_HCEBelShad[4] = { 0.971, 0.971, 0.971, 0 };
	ssc_data_set_array(data, "HCEBelShad", p_HCEBelShad, 4);
	ssc_number_t p_HCEEnvTrans[4] = { 0.935, 0.935, 1, 0 };
	ssc_data_set_array(data, "HCEEnvTrans", p_HCEEnvTrans, 4);
	ssc_number_t p_HCEabs[4] = { 0.925, 0.925, 0.8, 0 };
	ssc_data_set_array(data, "HCEabs", p_HCEabs, 4);
	ssc_number_t p_PerfFac[4] = { 1.25, 1.25, 1.25, 0 };
	ssc_data_set_array(data, "PerfFac", p_PerfFac, 4);
	ssc_number_t p_HCE_A0[4] = { 2.424, 7.0233, 100.05, 0 };
	ssc_data_set_array(data, "HCE_A0", p_HCE_A0, 4);
	ssc_number_t p_HCE_A1[4] = { 0.214, 1.275, -0.7351, 0 };
	ssc_data_set_array(data, "HCE_A1", p_HCE_A1, 4);
	ssc_number_t p_HCE_A2[4] = { -0.00047461, 0.0015105, -0.008635, 0 };
	ssc_data_set_array(data, "HCE_A2", p_HCE_A2, 4);
	ssc_number_t p_HCE_A3[4] = { 6.88e-06, 5.05e-06, 2.67e-05, 0 };
	ssc_data_set_array(data, "HCE_A3", p_HCE_A3, 4);
	ssc_number_t p_HCE_A4[4] = { 9.62e-08, 7.03e-08, 6.65e-07, 0 };
	ssc_data_set_array(data, "HCE_A4", p_HCE_A4, 4);
	ssc_number_t p_HCE_A5[4] = { -2.2423, -4.284, -99.043, 0 };
	ssc_data_set_array(data, "HCE_A5", p_HCE_A5, 4);
	ssc_number_t p_HCE_A6[4] = { 0.032325, 0.39685, 5.1672, 0 };
	ssc_data_set_array(data, "HCE_A6", p_HCE_A6, 4);
	ssc_data_set_number(data, "SfPar", 0.270);
	ssc_data_set_number(data, "ChtfPar", 10.687);
	ssc_data_set_number(data, "AntiFrPar", 1.069);


	int status = run_module(data, "tcstrough_empirical");

	return status;
}

// Parabolic trough (empirical) with alternative Power Cycle
// Power Cycle: APS Ormat 1MWe 300C
// Rest default configurations 
int tcstrough_empirical_tucson_power_cycle(ssc_data_t &data)
{
	tcstrough_empirical_default(data);

	ssc_data_set_number(data, "Solar_Field_Area", 1599020);
	ssc_data_set_number(data, "TurbEffG", 0.2071);
	ssc_data_set_number(data, "TurSUE", 0.05);
	ssc_data_set_number(data, "T2EPLF0", -0.1594);
	ssc_data_set_number(data, "T2EPLF1", 0.9262);
	ssc_data_set_number(data, "T2EPLF2", 1.1349);
	ssc_data_set_number(data, "T2EPLF3", -1.3606);
	ssc_data_set_number(data, "T2EPLF4", 0.4588);
	ssc_data_set_number(data, "E2TPLF0", 0.1492);
	ssc_data_set_number(data, "E2TPLF1", 0.8522);
	ssc_data_set_number(data, "E2TPLF2", -0.3247);
	ssc_data_set_number(data, "E2TPLF3", 0.44863);
	ssc_data_set_number(data, "E2TPLF4", -0.1256);
	ssc_data_set_number(data, "TempCorrF", 0);
	ssc_data_set_number(data, "TempCorr0", 1);
	ssc_data_set_number(data, "TempCorr1", 0);
	ssc_data_set_number(data, "TempCorr2", 0);
	ssc_data_set_number(data, "TempCorr3", 0);
	ssc_data_set_number(data, "TempCorr4", 0);
	ssc_data_set_number(data, "PTSmax", 535.973);
	ssc_data_set_number(data, "PFSmax", 543.0467);
	ssc_data_set_number(data, "SfPar", 0.42534);
	ssc_data_set_number(data, "ChtfPar", 16.8217);
	ssc_data_set_number(data, "AntiFrPar", 1.6822);

	int status = run_module(data, "tcstrough_empirical");

	return status;
}

// Parabolic trough (empirical) with alternative Thermal Stoage Fluid Type
// Thermal Storage Fluid Type: Therminol VP-1
// Rest default configurations 
int tcstrough_empirical_tucson_thermal_storage(ssc_data_t &data)
{
	tcstrough_empirical_default(data);

	ssc_data_set_number(data, "PTSmax", 676.4706);
	ssc_data_set_number(data, "PFSmax", 342.699);

	int status = run_module(data, "tcstrough_empirical");

	return status;
}

// Parabolic trough (empirical) with alternative Parasitic Electric Energy Use
// Parasitic Electric Energy Use: 500C Molten Salt HTF
// Rest default configurations 
int tcstrough_empirical_tucson_parasitic(ssc_data_t &data)
{
	tcstrough_empirical_default(data);

	ssc_data_set_number(data, "SfPar", 0.117);
	ssc_data_set_number(data, "SfParPF", 0.5);
	ssc_data_set_number(data, "ChtfPar", 3.231);
	ssc_data_set_number(data, "ChtfParPF", 0.35);
	ssc_data_set_number(data, "AntiFrPar", 0.323);
	ssc_data_set_number(data, "HhtfPar", 0.777);
	ssc_data_set_number(data, "HhtfParPF", 0.35);
	
	int status = run_module(data, "tcstrough_empirical");

	return status;
}

// Parabolic trough (empirical) with alternative location
// Location: Phoenix, AZ
// Rest default configurations 
int tcstrough_empirical_phoenix(ssc_data_t &data)
{
	tcstrough_empirical_default(data);

	char solar_resource_path_phoenix[512];
	int n = sprintf(solar_resource_path_phoenix, "%s/test/input_cases/trough_empirical_data/phoenix_az_33.450495_-111.983688_psmv3_60_tmy.csv", std::getenv("SSCDIR"));
	ssc_data_set_string(data, "file_name", solar_resource_path_phoenix);

	int status = run_module(data, "tcstrough_empirical");

	return status;
}

#endif
