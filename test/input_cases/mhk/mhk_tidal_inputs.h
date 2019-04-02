#ifndef MHK_TIDAL_INPUTS_H_
#define MHK_TIDAL_INPUTS_H_

#include <stdio.h>
#include <test/input_cases/code_generator_utilities.h>

void tidal_inputs(ssc_data_t &data) {

	ssc_number_t tidal_resource_definition[68] = {		0.0,		 0.90,
														0.1,		 3.10,
														0.2,		 4.20,
														0.3,		 4.40,
														0.4,		 4.80,
														0.5,		 4.90,
														0.6,		 5.30,
														0.7,		 5.10,
														0.8,		 5.20,
														0.9,		 5.60,
														1.0,		 5.00,
														1.1,		 5.20,
														1.2,		 5.00,
														1.3,		 4.80,
														1.4,		 4.70,
														1.5,		 4.30,
														1.6,		 4.20,
														1.7,		 4.00,
														1.8,		 3.40,
														1.9,		 3.10,
														2.0,		 2.60,
														2.1,		 2.30,
														2.2,		 2.00,
														2.3,		 1.60,
														2.4,		 1.30,
														2.5,		 1.10,
														2.6,		 0.70,
														2.7,		 0.50,
														2.8,		 0.40,
														2.9,		 0.20,
														3.0,		 0.10,
														3.1,		 0.00,
														3.2,		 0.00,
														3.3,		 0.00};
	ssc_data_set_matrix(data, "tidal_resource_definition", tidal_resource_definition, 34, 2);

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
	ssc_data_set_number(data, "rated_capacity", 0);
}


#endif // !MHK_TIDAL_INPUTS_H_