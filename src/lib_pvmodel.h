#ifndef __pv_h
#define __pv_h

#ifdef __cplusplus
extern "C" {
#endif

enum { MAXPOWERPOINT, OPERATINGVOLTAGE };

typedef struct {
	double ibeam;
	double idiff;
	double ignd;
	double tamb;
	double wspd;
	double zenith;
	double incang;
	double elev;
	double tilt;
} pv_input_t;
	

typedef int (*module_power_function) (
	// INPUTS
	void *module_spec,
	int mode, 
	double opvoltage, 
	double celltemp,	
	pv_input_t *input,
	
	// OUTPUTS
	double *power, double *voltage, double *current, 
	double *eff, double *voc, double *isc );
	
typedef int (*cell_temp_function) (
	// INPUTS
	void *module_spec,
	pv_input_t *input,
	
	module_power_function f_modpwr,
	void *module_spec_modpwr,
	
	// OUTPUTS
	double *celltemp ); // degrees C

int module_power(
	// INPUTS
	cell_temp_function f_celltemp,	
	void *module_spec_celltemp,
	
	module_power_function f_modpwr,
	void *module_spec_modpwr,
	
	pv_input_t *input,
	
	int mode, double opvoltage,
	
	double *power, double *voltage, double *current, 
	double *eff, double *voc, double *isc, double *celltemp );
	
	

#ifdef __cplusplus
}
#endif


#endif

