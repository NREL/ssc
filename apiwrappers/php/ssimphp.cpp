#include "stdafx.h"/* declaration of functions to be exported */
#include "../samsim/src/samsimapi.h"

/*
SAMSIM PHP API

string ssimphp_version()
long ssimphp_get_model_count()
string ssimphp_get_model_name(long idx)
long ssimphp_create_context(string name)
long ssimphp_free_context(long cxt)
long ssimphp_switch_context(long cxt, string name)
long ssimphp_get_input_count(long cxt)
string ssimphp_get_input_name(long cxt, int idx)
long ssimphp_get_input_type(long cxt, int idx)
string ssimphp_get_input_desc(long cxt, int idx)
long ssimphp_get_output_count(long cxt)
string ssimphp_get_output_name(long cxt, int idx)
long ssimphp_get_output_type(long cxt, int idx)
string ssimphp_get_output_desc(long cxt, int idx)
long ssimphp_set_i(long cxt, string name, long value)
long ssimphp_set_ia(long cxt, string name, array data, long count)
long ssimphp_set_d(long cxt, string name, double value)
long ssimphp_set_da(long cxt, string name, array data, long count)
long ssimphp_set_s(long cxt, string name, string value)
long ssimphp_get_i(long cxt, string name)
array ssimphp_get_ia(long cxt, string name)
double ssimphp_get_d(long cxt, string name)
array ssimphp_get_da(long cxt, string name)
string ssimphp_get_s(long cxt, string name)
long ssimphp_precheck(long cxt)
long ssimphp_run(long cxt)
long ssimphp_message_count(long cxt)
string ssimphp_get_message(long cxt, int idx)

*/

PHP_FUNCTION(ssimphp_version);
PHP_FUNCTION(ssimphp_get_model_count);
PHP_FUNCTION(ssimphp_get_model_name);
PHP_FUNCTION(ssimphp_create_context);
PHP_FUNCTION(ssimphp_free_context);
PHP_FUNCTION(ssimphp_switch_context);
PHP_FUNCTION(ssimphp_get_input_count);
PHP_FUNCTION(ssimphp_get_input_name);
PHP_FUNCTION(ssimphp_get_input_type);
PHP_FUNCTION(ssimphp_get_input_desc);
PHP_FUNCTION(ssimphp_get_output_count);
PHP_FUNCTION(ssimphp_get_output_name);
PHP_FUNCTION(ssimphp_get_output_type);
PHP_FUNCTION(ssimphp_get_output_desc);
PHP_FUNCTION(ssimphp_set_i);
PHP_FUNCTION(ssimphp_set_ia);
PHP_FUNCTION(ssimphp_set_d);
PHP_FUNCTION(ssimphp_set_da);
PHP_FUNCTION(ssimphp_set_s);
PHP_FUNCTION(ssimphp_get_i);
PHP_FUNCTION(ssimphp_get_ia);
PHP_FUNCTION(ssimphp_get_d);
PHP_FUNCTION(ssimphp_get_da);
PHP_FUNCTION(ssimphp_get_s);
PHP_FUNCTION(ssimphp_precheck);
PHP_FUNCTION(ssimphp_run);
PHP_FUNCTION(ssimphp_message_count);
PHP_FUNCTION(ssimphp_get_message);
PHP_FUNCTION(ssimphp_load_library);
PHP_FUNCTION(ssimphp_query_entries);
PHP_FUNCTION(ssimphp_get_entry);
PHP_FUNCTION(ssimphp_apply_entry);



/* compiled function list so Zend knows what's in this module */
zend_function_entry ssimphp_functions[] = {
	PHP_FE(ssimphp_version, NULL)
	PHP_FE(ssimphp_get_model_count, NULL)
	PHP_FE(ssimphp_get_model_name, NULL)
	PHP_FE(ssimphp_create_context, NULL)
	PHP_FE(ssimphp_free_context, NULL)
	PHP_FE(ssimphp_switch_context,NULL)
	PHP_FE(ssimphp_get_input_count,NULL)
	PHP_FE(ssimphp_get_input_name,NULL)
	PHP_FE(ssimphp_get_input_type,NULL)
	PHP_FE(ssimphp_get_input_desc,NULL)
	PHP_FE(ssimphp_get_output_count,NULL)
	PHP_FE(ssimphp_get_output_name,	NULL)
	PHP_FE(ssimphp_get_output_type,	NULL)
	PHP_FE(ssimphp_get_output_desc,	NULL)
	PHP_FE(ssimphp_set_i,	NULL)
	PHP_FE(ssimphp_set_ia,	NULL)
	PHP_FE(ssimphp_set_d,	NULL)
	PHP_FE(ssimphp_set_da,	NULL)
	PHP_FE(ssimphp_set_s,	NULL)
	PHP_FE(ssimphp_get_i,	NULL)
	PHP_FE(ssimphp_get_ia,	NULL)
	PHP_FE(ssimphp_get_d,	NULL)
	PHP_FE(ssimphp_get_da,	NULL)
	PHP_FE(ssimphp_get_s,	NULL)
	PHP_FE(ssimphp_precheck,	NULL)
	PHP_FE(ssimphp_run,	NULL)
	PHP_FE(ssimphp_message_count,	NULL)
	PHP_FE(ssimphp_get_message,	NULL)
	PHP_FE(ssimphp_load_library,	NULL)
	PHP_FE(ssimphp_query_entries,	NULL)
	PHP_FE(ssimphp_get_entry,	NULL)
	PHP_FE(ssimphp_apply_entry,	NULL)
    {NULL, NULL, NULL}
};    

/* compiled module information */
zend_module_entry ssimphp_module_entry = {
    STANDARD_MODULE_HEADER,
    "Solar Advisor Model Module (ssimphp)",
    ssimphp_functions,
    NULL, NULL, NULL, NULL, NULL,
    "0.1.1", 
	STANDARD_MODULE_PROPERTIES
};    

/* implement standard "stub" routine to introduce ourselves to Zend */
ZEND_GET_MODULE(ssimphp)

/* ssimphp_version function */
/* This method takes 1 parameter, a long value, returns
   the value multiplied by 2 */
PHP_FUNCTION(ssimphp_version){
	char verbuf[8];
	int v0, v1, v2;
	samsim_get_ver(&v0, &v1, &v2);
	sprintf(verbuf,"%d.%d.%d", v0, v1, v2);
    RETURN_STRING(verbuf, true);
}

PHP_FUNCTION(ssimphp_get_model_count) 
{
	RETURN_LONG( samsim_get_model_count() );
}

PHP_FUNCTION(ssimphp_get_model_name)
{
	int argc = ZEND_NUM_ARGS();
	long idx;

	if (zend_parse_parameters(argc TSRMLS_CC, "l", &idx) == FAILURE) 
		return;

	char *ret = estrdup( samsim_get_model_name(idx) );
	RETURN_STRING( ret, true);
}

PHP_FUNCTION(ssimphp_create_context)
{
	char *name = NULL;
	int argc = ZEND_NUM_ARGS();
	int name_len;

	if (zend_parse_parameters(argc TSRMLS_CC, "s", &name, &name_len) == FAILURE) 
		RETURN_STRING("No model name specified.", true);

	RETURN_LONG( samsim_create_context( name ) );
}

PHP_FUNCTION(ssimphp_free_context)
{
	int argc = ZEND_NUM_ARGS();
	long cxt;

	if (zend_parse_parameters(argc TSRMLS_CC, "l", &cxt) == FAILURE) 
		RETURN_STRING("No context.", true);

	RETURN_LONG( samsim_free_context( cxt ) );
}

PHP_FUNCTION(ssimphp_switch_context)
{
	char *name = NULL;
	int argc = ZEND_NUM_ARGS();
	int name_len;
	long cxt;

	if (zend_parse_parameters(argc TSRMLS_CC, "ls", &cxt, &name, &name_len) == FAILURE) 
		return;

	RETURN_LONG( samsim_switch_context(cxt, name) );
}

PHP_FUNCTION(ssimphp_get_input_count)
{
	int argc = ZEND_NUM_ARGS();
	long cxt;

	if (zend_parse_parameters(argc TSRMLS_CC, "l", &cxt) == FAILURE) 
		return;

	RETURN_LONG( samsim_get_input_count(cxt) );
}

PHP_FUNCTION(ssimphp_get_input_name)
{
	int argc = ZEND_NUM_ARGS();
	long cxt;
	long idx;

	if (zend_parse_parameters(argc TSRMLS_CC, "ll", &cxt, &idx) == FAILURE) 
		return;

	int type;
	const char *name = samsim_get_input(cxt, idx, &type);
	RETURN_STRING( estrdup(name?name:"BAD CXT/IDX"), false);
}

PHP_FUNCTION(ssimphp_get_input_type)
{
	int argc = ZEND_NUM_ARGS();
	long cxt;
	long idx;

	if (zend_parse_parameters(argc TSRMLS_CC, "ll", &cxt, &idx) == FAILURE) 
		return;

	int type;
	samsim_get_input(cxt, idx, &type);
	RETURN_LONG( (long) type );
}

PHP_FUNCTION(ssimphp_get_input_desc)
{
	int argc = ZEND_NUM_ARGS();
	long cxt;
	long idx;

	if (zend_parse_parameters(argc TSRMLS_CC, "ll", &cxt, &idx) == FAILURE) 
		return;

	const char *s = samsim_get_input_desc(cxt, idx);
	RETURN_STRING( estrdup(s?s:"BAD CXT/IDX"), false);
}

PHP_FUNCTION(ssimphp_get_output_count)
{
	int argc = ZEND_NUM_ARGS();
	long cxt;

	if (zend_parse_parameters(argc TSRMLS_CC, "l", &cxt) == FAILURE) 
		return;

	RETURN_LONG( samsim_get_output_count( cxt ) );
}

PHP_FUNCTION(ssimphp_get_output_name)
{
	int argc = ZEND_NUM_ARGS();
	long cxt;
	long idx;

	if (zend_parse_parameters(argc TSRMLS_CC, "ll", &cxt, &idx) == FAILURE) 
		return;

	int type;
	const char *name = samsim_get_output(cxt, idx, &type);
	RETURN_STRING( estrdup( name?name:"BAD CXT/IDX"),false);
}

PHP_FUNCTION(ssimphp_get_output_type)
{
	int argc = ZEND_NUM_ARGS();
	long cxt;
	long idx;

	if (zend_parse_parameters(argc TSRMLS_CC, "ll", &cxt, &idx) == FAILURE) 
		return;

	int type;
	samsim_get_output(cxt,idx,&type);
	RETURN_LONG( (long) type);
}

PHP_FUNCTION(ssimphp_get_output_desc)
{
	int argc = ZEND_NUM_ARGS();
	long cxt;
	long idx;

	if (zend_parse_parameters(argc TSRMLS_CC, "ll", &cxt, &idx) == FAILURE) 
		return;

	const char *s = samsim_get_output_desc(cxt, idx);
	RETURN_STRING( estrdup(s?s:"BAD CXT/IDX"), false);
}

PHP_FUNCTION(ssimphp_set_i)
{
	char *name = NULL;
	int argc = ZEND_NUM_ARGS();
	int name_len;
	long cxt;
	long value;

	if (zend_parse_parameters(argc TSRMLS_CC, "lsl", &cxt, &name, &name_len, &value) == FAILURE) 
		return;

	RETURN_LONG(samsim_set_i(cxt,name,(int)value) );
}

PHP_FUNCTION(ssimphp_set_ia)
{
	char *name = NULL;
	int argc = ZEND_NUM_ARGS();
	int name_len;
	long cxt;
	long count;
	zval *data = NULL;

	if (zend_parse_parameters(argc TSRMLS_CC, "lsal", &cxt, &name, &name_len, &data, &count) == FAILURE) 
		return;

	int *vals = (int*)emalloc( sizeof(int)*count );
	for (int i=0;i<count;i++)
		vals[i] = Z_LVAL( data[i] );

	int ret = samsim_set_ia(cxt, name, vals, count);

	efree(vals);

	RETURN_LONG(ret);
}
PHP_FUNCTION(ssimphp_set_d)
{
	char *name = NULL;
	int argc = ZEND_NUM_ARGS();
	int name_len;
	long cxt;
	double value;

	if (zend_parse_parameters(argc TSRMLS_CC, "lsd", &cxt, &name, &name_len, &value) == FAILURE) 
		return;

	RETURN_LONG( samsim_set_d(cxt,name,value) );
}

PHP_FUNCTION(ssimphp_set_da)
{
	char *name = NULL;
	int argc = ZEND_NUM_ARGS();
	int name_len;
	long cxt;
	long count;
	zval *data = NULL;

	if (zend_parse_parameters(argc TSRMLS_CC, "lsal", &cxt, &name, &name_len, &data, &count) == FAILURE) 
		return;

	double *vals = (double*) emalloc( sizeof(double)*count );
	for (int i=0;i<count;i++)
		vals[i] = Z_DVAL( data[i] );

	int ret = samsim_set_da(cxt, name, vals, count);

	efree( vals );

	RETURN_LONG( ret );
}

PHP_FUNCTION(ssimphp_set_s)
{
	char *name = NULL;
	char *value = NULL;
	int argc = ZEND_NUM_ARGS();
	int name_len;
	int value_len;
	long cxt;

	if (zend_parse_parameters(argc TSRMLS_CC, "lss", &cxt, &name, &name_len, &value, &value_len) == FAILURE) 
		return;

	RETURN_LONG(samsim_set_s(cxt,name,value));
}


PHP_FUNCTION(ssimphp_get_i)
{
	char *name = NULL;
	int argc = ZEND_NUM_ARGS();
	int name_len;
	long cxt;

	if (zend_parse_parameters(argc TSRMLS_CC, "ls", &cxt, &name, &name_len) == FAILURE) 
		return;

	int val = 0;
	samsim_get_i(cxt, name, &val);
	RETURN_LONG( (long)val );
}

PHP_FUNCTION(ssimphp_get_ia)
{
	char *name = NULL;
	int argc = ZEND_NUM_ARGS();
	int name_len;
	long cxt;

	if (zend_parse_parameters(argc TSRMLS_CC, "ls", &cxt, &name, &name_len) == FAILURE) 
		return;

	array_init( return_value );

	int count = 0;
	int *vals = samsim_get_ia(cxt, name, &count);
	if (vals && count > 0)
	{
		for (int i=0;i<count;i++)
			add_index_long(return_value, i, vals[i]);
	}
}
PHP_FUNCTION(ssimphp_get_d)
{
	char *name = NULL;
	int argc = ZEND_NUM_ARGS();
	int name_len;
	long cxt;

	if (zend_parse_parameters(argc TSRMLS_CC, "ls", &cxt, &name, &name_len) == FAILURE) 
		return;

	double val = 0;
	samsim_get_d(cxt, name, &val);
	RETURN_DOUBLE( val );
}

PHP_FUNCTION(ssimphp_get_da)
{
	char *name = NULL;
	int argc = ZEND_NUM_ARGS();
	int name_len;
	long cxt;

	if (zend_parse_parameters(argc TSRMLS_CC, "ls", &cxt, &name, &name_len) == FAILURE) 
		return;

	array_init( return_value );
	int count = 0;
	double *vals = samsim_get_da(cxt, name, &count);
	if (vals && count > 0)
	{
		for (int i=0;i<count;i++)
			add_index_double( return_value, i, vals[i] );
	}
}

PHP_FUNCTION(ssimphp_get_s)
{
	char *name = NULL;
	int argc = ZEND_NUM_ARGS();
	int name_len;
	long cxt;

	if (zend_parse_parameters(argc TSRMLS_CC, "ls", &cxt, &name, &name_len) == FAILURE) 
		return;

	const char *s = samsim_get_s(cxt, name);
	RETURN_STRING( estrdup(s?s:"BAD CXT/NAME"), false);
}

PHP_FUNCTION(ssimphp_precheck)
{
	int argc = ZEND_NUM_ARGS();
	long cxt;

	if (zend_parse_parameters(argc TSRMLS_CC, "l", &cxt) == FAILURE) 
		return;

	RETURN_LONG( samsim_precheck( cxt ) );
}

PHP_FUNCTION(ssimphp_run)
{
	int argc = ZEND_NUM_ARGS();
	long cxt;

	if (zend_parse_parameters(argc TSRMLS_CC, "l", &cxt) == FAILURE) 
		return;

	RETURN_LONG( samsim_run( cxt ) );
}

PHP_FUNCTION(ssimphp_message_count)
{
	int argc = ZEND_NUM_ARGS();
	long cxt;

	if (zend_parse_parameters(argc TSRMLS_CC, "l", &cxt) == FAILURE) 
		return;

	RETURN_LONG( samsim_message_count( cxt ) );
}

PHP_FUNCTION(ssimphp_get_message)
{
	int argc = ZEND_NUM_ARGS();
	long cxt;
	long idx;

	if (zend_parse_parameters(argc TSRMLS_CC, "ll", &cxt, &idx) == FAILURE) 
		return;

	const char *s = samsim_get_message(cxt, idx);
	RETURN_STRING( estrdup(s?s:"BAD CXT/IDX"), false);
}

SAMSIM_API int samsim_load_library(int cxt, const char *file_name, const char *desc_prefix);
SAMSIM_API int samsim_query_entries(int cxt, const char *type);
SAMSIM_API const char *samsim_get_entry(int cxt, int idx);
SAMSIM_API int samsim_apply_entry(int cxt, const char *name);

PHP_FUNCTION(ssimphp_load_library)
{
	char *file_name = NULL;
	char *desc_prefix = NULL;
	int argc = ZEND_NUM_ARGS();
	int file_name_len;
	int desc_prefix_len;
	long cxt;

	if (zend_parse_parameters(argc TSRMLS_CC, "lss", &cxt, &file_name, &file_name_len, &desc_prefix, &desc_prefix_len) == FAILURE) 
		return;

	RETURN_LONG( samsim_load_library( cxt, file_name, desc_prefix) );
}

PHP_FUNCTION(ssimphp_query_entries)
{
	char *type = NULL;
	int argc = ZEND_NUM_ARGS();
	int type_len;
	long cxt;

	if (zend_parse_parameters(argc TSRMLS_CC, "ls", &cxt, &type, &type_len) == FAILURE) 
		return;

	RETURN_LONG( samsim_query_entries(cxt, type) );
}

PHP_FUNCTION(ssimphp_get_entry)
{
	int argc = ZEND_NUM_ARGS();
	long cxt;
	long idx;

	if (zend_parse_parameters(argc TSRMLS_CC, "ll", &cxt, &idx) == FAILURE) 
		return;

	const char *s = samsim_get_entry(cxt, idx);
	RETURN_STRING( estrdup(s?s:"BAD CXT/IDX"), false);
}

PHP_FUNCTION(ssimphp_apply_entry)
{
	char *name = NULL;
	int argc = ZEND_NUM_ARGS();
	int name_len;
	long cxt;

	if (zend_parse_parameters(argc TSRMLS_CC, "ls", &cxt, &name, &name_len) == FAILURE) 
		return;

	RETURN_LONG( samsim_apply_entry(cxt, name ));
}
