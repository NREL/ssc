#ifndef MHK_TIDAL_INPUTS_H_
#define MHK_TIDAL_INPUTS_H_

#include <stdio.h>
#include <test/input_cases/code_generator_utilities.h>

void tidal_inputs(ssc_data_t &data) {

	ssc_number_t tidal_resource[68] = {					0.0,		 0.0090,
														0.1,		 0.0310,
														0.2,		 0.0420,
														0.3,		 0.0440,
														0.4,		 0.0480,
														0.5,		 0.0490,
														0.6,		 0.0530,
														0.7,		 0.0510,
														0.8,		 0.0520,
														0.9,		 0.0560,
														1.0,		 0.0500,
														1.1,		 0.0520,
														1.2,		 0.0500,
														1.3,		 0.0480,
														1.4,		 0.0470,
														1.5,		 0.0430,
														1.6,		 0.0420,
														1.7,		 0.0400,
														1.8,		 0.0340,
														1.9,		 0.0310,
														2.0,		 0.0260,
														2.1,		 0.0230,
														2.2,		 0.0200,
														2.3,		 0.0160,
														2.4,		 0.0130,
														2.5,		 0.0110,
														2.6,		 0.0070,
														2.7,		 0.0050,
														2.8,		 0.0040,
														2.9,		 0.0020,
														3.0,		 0.0010,
														3.1,		 0.00,
														3.2,		 0.00,
														3.3,		 0.00};
	ssc_data_set_matrix(data, "tidal_resource", tidal_resource, 34, 2);

	ssc_number_t tidal_power_curve[68] = {				0.0,		0.0,
														0.1,		0.0,
														0.2,		0.0,
														0.3,		0.0,
														0.4,		0.0,
														0.5,		0.0,
														0.6,		10.4,
														0.7,		20.8,
														0.8,		40.0,
														0.9,		59.1,
														1.0,		89.2,
														1.1,		119.3,
														1.2,		160.9,
														1.3,		202.5,
														1.4,		259.3,
														1.5,		316.1,
														1.6,		392.7,
														1.7,		469.2,
														1.8,		570.3,
														1.9,		671.4,
														2.0,		802.9,
														2.1,		934.4,
														2.2,		1024.7,
														2.3,		1115.0,
														2.4,		1115.0,
														2.5,		1115.0,
														2.6,		1115.0,
														2.7,		1115.0,
														2.8,		1115.0,
														2.9,		1115.0,
														3.0,		1115.0,
														3.1,		1115.0,
														3.2,		1085.4,
														3.3,		1055.7};

	ssc_data_set_matrix(data, "tidal_power_curve", tidal_power_curve, 34, 2);
	
	ssc_data_set_number(data, "annual_energy_loss", 0);	//in %
	ssc_data_set_number(data, "calculate_capacity", 1);	//Calculate rated capacity outside UI?
	ssc_data_set_number(data, "device_rated_capacity", 0);
}


#endif // !MHK_TIDAL_INPUTS_H_