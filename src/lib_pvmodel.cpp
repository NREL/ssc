#include "lib_pvmodel.h"

int module_power(
	// INPUTS
	cell_temp_function f_celltemp,	
	void *module_spec_celltemp,
	
	module_power_function f_modpwr,
	void *module_spec_modpwr,
	
	pv_input_t *input,
	
	int mode, double opvoltage,
	
	double *power, double *voltage, double *current, 
	double *eff, double *voc, double *isc, double *celltemp )
{
	int code = (*f_celltemp)( module_spec_celltemp, input,  
		f_modpwr, module_spec_modpwr, 
		celltemp );
		
	if (code == 0)
		code = (*f_modpwr)( module_spec_modpwr, mode, opvoltage,
			*celltemp, input,
			power, voltage, current,
			eff, voc, isc );
	
	return code;
}

