#include <wx/wx.h>
#include <wx/dynlib.h>

#include "dllinvoke.h"

static wxString m_path;
static wxString m_lastLoadTime;
static wxDynamicLibrary m_dll;


bool sscdll_load( const char *path )
{
	sscdll_unload();
	if (m_dll.Load( path ))
	{
		m_path = path;
		m_lastLoadTime = wxNow();
		return true;
	}
	else return false;
}

void sscdll_unload()
{
	if (m_dll.IsLoaded()) m_dll.Unload();
}

bool sscdll_isloaded()
{
	return m_dll.IsLoaded();
}

const char *sscdll_status()
{
static char buf[1024];

	if (!sscdll_isloaded())
	{
		strcpy(buf,"No ssc32.dll loaded.");
	}
	else
	{
		sprintf(buf, "%s ( %s ) Version %d", 
			(const char*)m_path.c_str(),
			(const char*)m_lastLoadTime.c_str(),
			ssc_version() );
	}
	return buf;
}

/*  dynamic linked implementations */


int ssc_version()
{
	static const char *f_name = __FUNCTION__;
	static int (*f)() = NULL;	
	if (!sscdll_isloaded()) { f=NULL; return 0; }

	if (!f) {
		f = (int(*)()) m_dll.GetSymbol( f_name );
		if (!f) return 0;
	}

	return (*f)(); 
}

ssc_data_t ssc_data_create()
{
	static const char *f_name = __FUNCTION__;
	static ssc_data_t (*f)() = NULL;
	
	if (!sscdll_isloaded()) { f=NULL; return NULL; }

	if (!f) {
		f = (ssc_data_t(*)())m_dll.GetSymbol(f_name);
		if (!f) return NULL;
	}

	return (*f)();	
}

void ssc_data_free( ssc_data_t p_data )
{
	static const char *f_name = __FUNCTION__;
	static void (*f)(ssc_data_t) = NULL;
	
	if (!sscdll_isloaded()) { f=NULL; return; }
	
	if (!f) {
		f = (void(*)(ssc_data_t))m_dll.GetSymbol(f_name);
		if (!f) return;
	}

	(*f)( p_data );
}

void ssc_data_unassign( ssc_data_t p_data, const char *name )
{
	static const char *f_name = __FUNCTION__;
	static void (*f)(ssc_data_t,const char*) = NULL;

	if (!sscdll_isloaded()){f=NULL; return;}

	if (!f) {
		f = (void(*)(ssc_data_t,const char*))m_dll.GetSymbol(f_name);
		if (!f) return;
	}

	(*f)( p_data, name );
}

int ssc_data_query( ssc_data_t p_data, const char *name )
{
	static const char *f_name = __FUNCTION__;
	static int (*f)(ssc_data_t, const char *) = NULL;

	if (!sscdll_isloaded()) { f=NULL; return SSC_INVALID; }

	if (!f) {
		f = (int(*)(ssc_data_t, const char *))m_dll.GetSymbol(f_name);
		if (!f) return SSC_INVALID;
	}

	return (*f)( p_data, name );
}

void ssc_data_set_string( ssc_data_t p_data, const char *name, const char *value )
{
	static const char *f_name = __FUNCTION__;
	static void (*f)(ssc_data_t, const char*, const char*) = NULL;

	if (!sscdll_isloaded()) { f=NULL; return; }

	if (!f) {
		f = (void(*)(ssc_data_t, const char*, const char*))m_dll.GetSymbol(f_name);
		if (!f) return;
	}

	(*f)(p_data, name, value);
}

void ssc_data_set_number( ssc_data_t p_data, const char *name, ssc_number_t value )
{
	static const char *f_name = __FUNCTION__;
	static void (*f)(ssc_data_t, const char*, ssc_number_t) = NULL;

	if (!sscdll_isloaded()) { f=NULL; return; }

	if (!f) {
		f = (void(*)(ssc_data_t, const char*, ssc_number_t))m_dll.GetSymbol(f_name);
		if (!f) return;
	}

	(*f)(p_data, name, value);
}

void ssc_data_set_array( ssc_data_t p_data, const char *name, ssc_number_t *pvalues, int length )
{
	static const char *f_name = __FUNCTION__;
	static void (*f)(ssc_data_t, const char*, ssc_number_t*, int) = NULL;

	if (!sscdll_isloaded()) { f=NULL; return; }
	
	if (!f) {
		f = (void(*)(ssc_data_t, const char*, ssc_number_t*, int))m_dll.GetSymbol(f_name);
		if (!f) return;
	}

	(*f)(p_data, name, pvalues, length);
}

void ssc_data_set_matrix( ssc_data_t p_data, const char *name, ssc_number_t *pvalues, int nrows, int ncols )
{
	static const char *f_name = __FUNCTION__;
	static void (*f)(ssc_data_t, const char*, ssc_number_t*, int, int) = NULL;

	if (!sscdll_isloaded()) { f=NULL; return; }

	if (!f) {
		f = (void(*)(ssc_data_t, const char*, ssc_number_t*, int, int))m_dll.GetSymbol(f_name);
		if (!f) return;
	}

	(*f)(p_data, name, pvalues, nrows, ncols);
}

const char *ssc_data_get_string( ssc_data_t p_data, const char *name )
{
	static const char *f_name = __FUNCTION__;
	static const char* (*f)( ssc_data_t, const char* ) = NULL;
	
	if (!sscdll_isloaded()) { f=NULL; return NULL; }

	if (!f) {
		f = (const char*(*)(ssc_data_t, const char*))m_dll.GetSymbol(f_name);
		if (!f) return NULL;
	}

	return (*f)(p_data, name);
}

ssc_bool_t ssc_data_get_number( ssc_data_t p_data, const char *name, ssc_number_t *value )
{
	static const char *f_name = __FUNCTION__;
	static ssc_bool_t (*f)(ssc_data_t, const char*, ssc_number_t*) = NULL;
	
	if (!sscdll_isloaded()) { f=NULL; return 0; }

	if (!f) {
		f = (ssc_bool_t(*)(ssc_data_t, const char*, ssc_number_t*))m_dll.GetSymbol(f_name);
		if (!f) return 0;
	}

	return (*f)(p_data, name, value);
}
/*
const ssc_number_t *ssc_data_get_array( ssc_data_t p_data, const char *name, int *length )
const ssc_number_t *ssc_data_get_matrix( ssc_data_t p_data, const char *name, int *nrows, int *ncols )
ssc_entry_t ssc_module_entry( int index )
const char *ssc_entry_name( ssc_entry_t p_entry )
const char *ssc_entry_description( ssc_entry_t p_entry )
int ssc_entry_version( ssc_entry_t p_entry )
ssc_module_t ssc_module_create( const char *name )
void ssc_module_free( ssc_module_t p_mod )
const ssc_info_t ssc_module_var_info( ssc_module_t p_mod, int index )
int ssc_info_var_type( ssc_info_t p_inf )
int ssc_info_data_type( ssc_info_t p_inf )
const char *ssc_info_name( ssc_info_t p_inf )
const char *ssc_info_label( ssc_info_t p_inf )
const char *ssc_info_units( ssc_info_t p_inf )
const char *ssc_info_meta( ssc_info_t p_inf )
const char *ssc_info_group( ssc_info_t p_inf )
const char *ssc_info_uihint( ssc_info_t p_inf )
ssc_bool_t ssc_module_exec_simple( const char *name, ssc_data_t p_data )
const char *ssc_module_exec_simple_nothread( const char *name, ssc_data_t p_data )
ssc_bool_t ssc_module_exec( ssc_module_t p_mod, ssc_data_t p_data )
ssc_bool_t ssc_module_exec_with_handler( 
	ssc_module_t p_mod, 
	ssc_data_t p_data, 
	ssc_bool_t (*pf_handler)( ssc_module_t, ssc_handler_t, int action, float f0, float f1, const char *s0, const char *s1, void *user_data ),
	void *pf_user_data )

void ssc_module_extproc_output( ssc_handler_t p_mod, const char *output_line )
ssc_param_t ssc_module_parameter( ssc_module_t p_mod, int index )
const char *ssc_param_name( ssc_param_t p_param )
const char *ssc_param_description( ssc_param_t p_param )
const char *ssc_param_default_value( ssc_param_t p_param )
int ssc_param_type( ssc_param_t p_param )
void ssc_module_parameter_string( ssc_module_t p_mod, const char *name, const char *value )
void ssc_module_parameter_float( ssc_module_t p_mod, const char *name, float value )
const char *ssc_module_log( ssc_module_t p_mod, int index, int *item_type, float *time )

*/