#ifndef __csp_solver_core_
#define __csp_solver_core_

#include "tcstype.h"
#include "core.h"

class C_csp_component : compute_module
{
private:
	const tcsvarinfo *params;
	std::vector<tcsvalue> param_values;

	void set_csp_component_value(const char *name, double val);
	void set_csp_component_value(const char *name, double *p_array, size_t &l_array);
	void set_csp_component_value(const char *name, double *p_array, size_t &nr, size_t &nc);
	void set_csp_component_value(const char *name, const char *s);

	void set_csp_component_value(int idx, double val);
	void set_csp_component_value(int idx, double *p_array, size_t &l_array);
	void set_csp_component_value(int idx, double *p_array, size_t &nr, size_t &nc);
	void set_csp_component_value(int idx, const char *s);

	int find_var(const char *name);

	void tcsvalue_free(int idx);
	void tcsvalue_set_number(int idx, double val);
	void tcsvalue_set_array(int idx, double *p_array, size_t &l_array);
	void tcsvalue_set_matrix(int idx, double *p_array, size_t &nr, size_t &nc);
	void tcsvalue_set_string(int idx, const char *s);

	// Have to have this to access 'compute_module' public methods??
	void exec() throw(general_error)
	{
		
	}
	// *************************************************************

protected:
	void set_params_and_size_vector(const tcsvarinfo *params_in);
	double value(size_t idx);
	double *value(size_t idx, int &len);
	double *value(size_t idx, int &nr, int &nc);
	std::string value_str(size_t idx);


	

	

public:
	virtual ~C_csp_component(){};

	C_csp_component(){};

	void set_csp_component_value_ssc_double(const char *name, double val);
	void set_csp_component_value_ssc_array(const char *name, ssc_number_t *p_array, size_t &l_array);
	void set_csp_component_value_ssc_matrix(const char *name, ssc_number_t *p_array, size_t &nr, size_t &nc);
	void set_csp_component_value_ssc_string(const char *name, const char *s);

	virtual void init() {return;}

	// converged   
};


class C_csp_solar_field : public C_csp_component
{
private:
	double test_var;


public:
	C_csp_solar_field()
	{
		test_var = 1.23;
	};

	~C_csp_solar_field(){};

	// throw exception here
	virtual void init() {return;}

	// solve_field

	// optical_efficiency

	// internal_energy || time_to_startup

};


class C_csp_mspt_221_222 : public C_csp_solar_field
{
private:
	double place_holder;

public:
	C_csp_mspt_221_222();

	virtual ~C_csp_mspt_221_222(){};

	virtual void init();
};


#endif // !__csp_solver_core_
