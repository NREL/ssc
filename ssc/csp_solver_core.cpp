#include "csp_solver_core.h"

void C_csp_component::set_params_and_size_vector(const tcsvarinfo *params_in)
{
	params = params_in;

	// Find the size of the parameter ('tcsvarinfo') table
	int idx = 0;
	while( params[idx].var_type != TCS_INVALID && params[idx].name != 0 )
	{
		idx++;
	}

	// idx is the size of the table, including the final 'TCS_INVALID' row

	if( idx == 0 )
	{
		throw exec_error("Loading parameter tables:", "Parameter table not found");
	}

	param_values.resize(idx);
}

void C_csp_component::set_csp_component_value_ssc_double(const char *name, double val)
{
	set_csp_component_value(name, val);
}

void C_csp_component::set_csp_component_value_ssc_array(const char *name, ssc_number_t *p_array, size_t l_array)
{
	double *pt = new double[l_array];
	for( size_t i = 0; i < l_array; i++ )
		pt[i] = (double) p_array[i];

	set_csp_component_value(name, pt, l_array);

	delete [] pt;
}

void C_csp_component::set_csp_component_value_ssc_matrix(const char *name, ssc_number_t *p_array, size_t nr, size_t nc )
{
	double *pt = new double[nr*nc];
	for( size_t i = 0; i < nr*nc; i++ )
		pt[i] = (double)p_array[i];

	set_csp_component_value(name, pt, nr, nc);

	delete[] pt;
}

void C_csp_component::set_csp_component_value_ssc_string(const char *name, const char *s )
{
	set_csp_component_value(name, s);
}

void C_csp_component::set_csp_component_value(const char *name, double val)
{
	set_csp_component_value(find_var(name), val);
}

void C_csp_component::set_csp_component_value(const char *name, double *p_array, size_t l_array)
{
	set_csp_component_value(find_var(name), p_array, l_array);
}

void C_csp_component::set_csp_component_value(const char *name, double *p_array, size_t nr, size_t nc)
{
	set_csp_component_value(find_var(name), p_array, nr, nc);
}

void C_csp_component::set_csp_component_value(const char *name, const char *s)
{
	set_csp_component_value(find_var(name), s);
}

void C_csp_component::set_csp_component_value(int idx, double val)
{
	if( idx < (int)param_values.size() )
		tcsvalue_set_number(idx, val);
	else
		throw exec_error("Setting parameter values", "Exceeded values vector size");
}

void C_csp_component::set_csp_component_value(int idx, double *p_array, size_t l_array)
{
	if( idx < (int)param_values.size() )
		tcsvalue_set_array(idx, p_array, l_array);
	else
		throw exec_error("Setting parameter values", "Exceeded values vector size");
}

void C_csp_component::set_csp_component_value(int idx, double *p_array, size_t nr, size_t nc)
{
	if( idx < (int)param_values.size() )
		tcsvalue_set_matrix(idx, p_array, nr, nc);
	else
		throw exec_error("Setting parameter values", "Exceeded values vector size");
}

void C_csp_component::set_csp_component_value(int idx, const char *s)
{
	if( idx < (int)param_values.size() )
		tcsvalue_set_string(idx, s);
	else
		throw exec_error("Setting parameter values", "Exceeded values vector size");
}

void C_csp_component::tcsvalue_set_number(int idx, double val)
{
	tcsvalue_free(idx);
	param_values[idx].type = TCS_NUMBER;
	param_values[idx].data.value = val;
}

void C_csp_component::tcsvalue_set_array(int idx, double *p_array, size_t l_array)
{
	tcsvalue_free(idx);
	if( !p_array || l_array < 1 )
		return;

	param_values[idx].type = TCS_ARRAY;
	param_values[idx].data.array.values = new double[ l_array ];
	param_values[idx].data.array.length = l_array;

	for( int i = 0; i < l_array; i++ )
		param_values[idx].data.array.values[i] = p_array[i];
}

void C_csp_component::tcsvalue_set_matrix(int idx, double *p_array, size_t nr, size_t nc)
{
	tcsvalue_free(idx);
	if( !p_array || nr*nc < 1 )
		return;

	param_values[idx].type = TCS_MATRIX;
	param_values[idx].data.matrix.values = new double[nr*nc];
	param_values[idx].data.matrix.nrows = nr;
	param_values[idx].data.matrix.ncols = nc;

	for( int i = 0; i < nr*nc; i++ )
		param_values[idx].data.matrix.values[i] = p_array[i];
}

void C_csp_component::tcsvalue_set_string(int idx, const char *s)
{ 
	tcsvalue_free(idx);

	param_values[idx].type = TCS_STRING;

	if( !s )
	{
		(&param_values[idx])->data.cstr = new char[1];
		param_values[idx].data.cstr = 0;
	}

	(&param_values[idx])->data.cstr = new char[ strlen(s) + 1 ];
	strcpy((&param_values[idx])->data.cstr, s);	
}

int C_csp_component::find_var(const char *name)
{
	int idx = 0;

	while( params[idx].var_type != TCS_INVALID && params[idx].name != 0 )
	{
		if( strcmp(params[idx].name, name) == 0 )
			return idx;
		idx++;
	}

	throw exec_error("Setting parameter values", "SSC INPUT: '" + (std::string)name + "' does not match any System Input Name!\n");

	return -1;
}

void C_csp_component::tcsvalue_free(int idx)
{
	switch( param_values[idx].type )
	{
	case TCS_ARRAY:
		delete[] param_values[idx].data.array.values;
		break;
	case TCS_MATRIX:
		delete[] param_values[idx].data.matrix.values;
		break;
	case TCS_STRING:
		delete[] param_values[idx].data.cstr;
		break;
	}

	param_values[idx].type = TCS_INVALID;
}

double C_csp_component::value(size_t idx)
{
	if( param_values[idx].type != TCS_NUMBER )
		return std::numeric_limits<double>::quiet_NaN();

	return param_values[idx].data.value;
}

double *C_csp_component::value(size_t idx, int *len)
{
	if( param_values[idx].type != TCS_ARRAY || &len == NULL )
	{
		*len = 0;
		return 0;
	}

	*len = param_values[idx].data.array.length;
	return param_values[idx].data.array.values;
}

double *C_csp_component::value(size_t idx, int *nr, int *nc)
{
	if( param_values[idx].type != TCS_MATRIX || &nr == NULL || &nc == NULL )
	{
		*nr = 0;
		*nc = 0;
		return 0;
	}

	*nr = param_values[idx].data.matrix.nrows;
	*nc = param_values[idx].data.matrix.ncols;
	return param_values[idx].data.matrix.values;
}

std::string C_csp_component::value_str(size_t idx)
{
	if( param_values[idx].type != TCS_STRING )
		return std::string();

	return std::string(param_values[idx].data.cstr);
}
