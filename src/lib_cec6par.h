#ifndef cec6par_h
#define cec6par_h

#include "lib_pvmodel.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
   Implementation of CEC 5 (6) parameter model as presented by
   DeSoto, Klein, and Beckman, Solar Energy Journal 2005   
   http://minds.wisconsin.edu/handle/1793/7602
*/

typedef struct {
	double Area;
	double Vmp, Imp;
	double Voc, Isc;
	double alpha_isc, beta_voc;
	double T_noct; // 'C
	
	// six reference condition parameters (calculated by coeffgen)
	double a, Il, Io;
	double Rs, Rsh, Adjust;	
}  cec6par_module_t;

/*
	returns 0 on success.
   -1 indicates temp model failed to converge
*/
int cec6par_cell_temp_function (
	// INPUTS
	void *module_spec, // pointer to cec6par_module_t
	pv_input_t *input,
	
	module_power_function f_modpwr,
	void *module_spec_modpwr,
	
	// OUTPUTS
	double *celltemp );

 int cec6par_module_power_function (
	// INPUTS
	void *module_spec, // pointer to cec6par_module_t
	int mode, 
	double opvoltage, 
	double celltemp,	
	pv_input_t *input,
	
	// OUTPUTS
	double *power, double *voltage, double *current, 
	double *eff, double *voc, double *isc );

#ifdef __cplusplus
}
#endif


#endif

