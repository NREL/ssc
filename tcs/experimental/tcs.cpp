
#include <string>
#include <numeric>
#include <cstring>
#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <cstdarg>
#include <limits>
#include <iostream>
#include <algorithm>

#include "tcs.h"

#if defined(__WINDOWS__)||defined(WIN32)||defined(_WIN32)||defined(__MINGW___)||defined(_MSC_VER)
#include <Windows.h>
void *dll_open(const char *name) { return (void*) ::LoadLibraryA( name ); }
void dll_close( void *handle ) { ::FreeLibrary( (HMODULE)handle ); }
void *dll_sym( void *handle, const char *name ) { return (void*) ::GetProcAddress( (HMODULE)handle, name ); }
#else
#include <dlfcn.h>
void *dll_open(const char *name) { return dlopen( name, RTLD_NOW ); }
void dll_close( void *handle ) { dlclose( handle ); }
void *dll_sym( void *handle, const char *name ) { return dlsym( handle, name ); }
#endif


tcsparam::tcsparam( double val )
{
	m_type = NUMBER;
	m_val = val;
	m_p = 0;
}

tcsparam::tcsparam( double *p, int len )
{
	if (len > 0)
	{
		m_len = len;
		m_p = new double[m_len];
		for (int i=0;i<m_len;i++)
			m_p[i] = p[i];
			
		m_type = ARRAY;
	}
	else
		m_type = INVALID;
}

tcsparam::tcsparam( double *p, int nrows, int ncols )
{
	if (nrows > 0 && ncols > 0)
	{
		m_nrows = nrows;
		m_ncols = ncols;
		m_p = new double[nrows*ncols];
		for ( int i=0;i<nrows*ncols;i++)
			m_p[i] = p[i];
		
		m_type = MATRIX;
	}
	else
		m_type = INVALID;
}

tcsparam::tcsparam( const std::string &s )
{
	m_type = STRING;
	m_str = s;
	m_p = 0;
}

tcsparam::~tcsparam()
{
	if ( m_p != 0 )
		delete [] m_p;
}

int tcsparam::type( )
{
	return m_type;
}

double tcsparam::val( )
{
	if (m_type == NUMBER) return m_val;
	else return std::numeric_limits<double>::quiet_NaN();
}

std::string tcsparam::str( )
{
	if (m_type == STRING) return m_str;
	else return std::string();
}

double *tcsparam::arr( int *len )
{
	if (m_type == ARRAY )
	{
		*len = m_len;
		return m_p;
	}
	else return 0;
}
double *tcsparam::mat( int *nr, int *nc )
{
	if (m_type == MATRIX)
	{
		*nr = m_nrows;
		*nc = m_ncols;
		return m_p;
	}
	else return 0;
}

static const char *__get_version( struct _tcscontext *t )
{
	tcsunit *unit = (tcsunit*)t->unit;
	return unit->kernel()->version();
}

static double __get_current_time ( struct _tcscontext *t )
{
	tcsunit *unit = (tcsunit*)t->unit;
	return unit->kernel()->time();
}

static double __get_start_time ( struct _tcscontext *t )
{
	tcsunit *unit = (tcsunit*)t->unit;
	return unit->kernel()->start_time();
}

static double __get_end_time ( struct _tcscontext *t )
{
	tcsunit *unit = (tcsunit*)t->unit;
	return unit->kernel()->end_time();
}

static double __get_step ( struct _tcscontext *t )
{
	tcsunit *unit = (tcsunit*)t->unit;
	return unit->kernel()->step();
}

static void __warning ( struct _tcscontext *t, const char *source, int id, const char *message )
{
	tcsunit *unit = (tcsunit*)t->unit;
	unit->kernel()->warning( source, id, message );
}

static void __halt ( struct _tcscontext *t, const char *source, int id, const char *message )
{
	tcsunit *unit = (tcsunit*)t->unit;
	unit->kernel()->halt( source, id, message );
}

static int __param_number ( struct _tcscontext *t, const char *name, double *val )
{
	tcsunit *unit = (tcsunit*)t->unit;
	return unit->param( name, val );
}

static double *__param_array ( struct _tcscontext *t, const char *name, int *count )
{
	tcsunit *unit = (tcsunit*)t->unit;
	return unit->param( name, count );
}

static double *__param_matrix ( struct _tcscontext *t, const char *name, int *nrows, int *ncols )
{
	tcsunit *unit = (tcsunit*)t->unit;
	return unit->param( name, nrows, ncols );
}

static const char *__param_string ( struct _tcscontext *t, const char *name )
{
	tcsunit *unit = (tcsunit*)t->unit;
	return unit->param( name );
}

tcsunit::tcsunit( tcskernel *kern )
{
	m_kernel = kern;
	m_context = new tcscontext;
	m_context->unit = (void*)this;
	m_context->get_version = __get_version;
	m_context->get_current_time = __get_current_time;
	m_context->get_start_time = __get_start_time;
	m_context->get_end_time = __get_end_time;
	m_context->get_step = __get_step;
	m_context->warning = __warning;
	m_context->halt = __halt;
	m_context->param_number = __param_number;
	m_context->param_array = __param_array;
	m_context->param_matrix = __param_matrix;
	m_context->param_string = __param_string;
	m_pInstance = 0;
	m_nInputs = -1;
	m_nOutputs = -1;
	m_ncall = 0;
	m_dyntype = 0;
}

tcsunit::~tcsunit()
{
	unload();
	
	delete m_context;
	
	for ( tcsparamhash::iterator it = m_params.begin();
		it != m_params.end();
		++it )
		delete (*it).second;
		
	m_params.clear();
}

	
bool tcsunit::load( )
{
	unload();
	
	if (m_type.empty()) return false;
	
	m_dyntype = m_kernel->library( m_type );
	if (!m_dyntype)
	{
		m_kernel->warning( m_type.c_str(), -1, "failed to locate dynamic type" );
		return false;
	}
	
	m_pInstance = m_dyntype->create( m_context, m_kernel->get_unique_id(), m_type.c_str() );
	if ( m_pInstance == 0 )
	{
		m_kernel->warning( m_type.c_str(), -1, "failed to create object instance" );
		unload();
		return false;
	}
	
	m_ncall = 0;
	m_dyntype->info( m_pInstance, &m_nInputs, &m_nOutputs );
	
	return true;
}

void tcsunit::unload()
{
	if ( m_pInstance != 0 && m_dyntype != 0 )
		m_dyntype->free( m_pInstance );
	
	m_pInstance = 0;	
	m_dyntype = 0;	
	m_nInputs = m_nOutputs = -1;
}

int tcsunit::call( double *in, int n_in, double *out, int n_out )
{
	if ( m_pInstance != 0 && m_dyntype != 0 )
		return m_dyntype->invoke( m_pInstance, m_ncall++, in, n_in, out, n_out );
	else
		return -1;
}


bool tcsunit::is_loaded()
{
	return m_pInstance != 0;
}

void tcsunit::set_type( const std::string &type )
{
	m_type = type;
}

void tcsunit::set_name( const std::string &name )
{
	m_name = name;
}

std::string tcsunit::type()
{
	return m_type;
}

std::string tcsunit::name()
{
	return m_name;
}

int tcsunit::num_inputs()
{
	return m_nInputs;
}

int tcsunit::num_outputs()
{
	return m_nOutputs;
}

void tcsunit::param( const char *name, tcsparam *val )
{
	tcsparamhash::iterator it = m_params.find( name );
	if ( it != m_params.end() )
	{
		delete (*it).second;
		(*it).second = val;
	}
	else
		m_params[ std::string(name) ] = val;
}

int tcsunit::param( const char *name, double *val )
{
	tcsparamhash::iterator it = m_params.find( name );
	if ( it != m_params.end() )
	{
		tcsparam *par = (*it).second;
		if (par->type() == tcsparam::NUMBER)
		{
			*val = par->val();
			return 0;
		}
	}
	return -1;
}

double *tcsunit::param( const char *name, int *count )
{
	tcsparamhash::iterator it = m_params.find( name );
	if ( it != m_params.end() )
	{
		tcsparam *par = (*it).second;
		if (par->type() == tcsparam::ARRAY)
			return par->arr( count );
	}
	return 0;
}

double *tcsunit::param( const char *name, int *nrows, int *ncols )
{
	tcsparamhash::iterator it = m_params.find( name );
	if ( it != m_params.end() )
	{
		tcsparam *par = (*it).second;
		if (par->type() == tcsparam::MATRIX)
			return par->mat( nrows, ncols );
	}
	return 0;
}

const char *tcsunit::param( const char *name )
{
	tcsparamhash::iterator it = m_params.find( name );
	if ( it != m_params.end() )
	{
		tcsparam *par = (*it).second;
		if (par->type() == tcsparam::STRING)
			return par->str().c_str();
	}
	return 0;
}

tcskernel *tcsunit::kernel()
{
	return m_kernel;
}







void *tcsdyntype::create( tcscontext *cxt, int id, const char *type )
{
	return (*tcs_create)( cxt, id, type );
}

void tcsdyntype::free( void *obj )
{
	return (*tcs_free)( obj );
}

int tcsdyntype::invoke( void *inst, int mode, double *in, int n_in, double *out, int n_out )
{
	return (*tcs_invoke)( inst, mode, in, n_in, out, n_out );
}

int tcsdyntype::info( void *inst, int *ni, int *no )
{
	return (*tcs_info)( inst, ni, no );
}

tcsdyntype::tcsdyntype( )
{
	m_library = 0;
}

bool tcsdyntype::reload( const std::vector<std::string> &paths, const std::string &type)
{
	unload();
	
	std::string ext;
#if defined(_WIN32)
	ext = ".dll";
#elif defined(__APPLE__)
	ext = ".dylib";
#else
	ext = ".so";
#endif
	for ( std::vector<std::string>::const_iterator it = paths.begin();
		it != paths.end();
		++it )
	{
		std::string path = *it + "/" + type + ext;
		
		if (void *plib = dll_open( path.c_str() ))
		{
			printf("loaded dynamic library (0x%X): %s\n", (unsigned int) plib, path.c_str() );
			
			tcs_create = ( void* (*)( tcscontext *, int, const char* ) ) dll_sym( plib, "tcs_create" );
			tcs_free = ( void (*)( void *) ) dll_sym( plib, "tcs_free" );
			tcs_invoke = ( int (*) (void *, int, double *, int, double *, int ) ) dll_sym( plib, "tcs_invoke" );
			tcs_info = ( int (*) (void *, int *, int * ) ) dll_sym( plib, "tcs_info" );
			
			if ( tcs_create != 0 
				&& tcs_free != 0
				&& tcs_invoke != 0
				&& tcs_info != 0 )
			{
				m_library = plib;
				m_path = path;
				m_type = type;
				
				return true;
			}
		}
	}
	
	return false;
}

tcsdyntype::~tcsdyntype()
{
	unload();
}

void tcsdyntype::unload()
{
	if (m_library)
	{
		printf("unloaded dynamic library (0x%X): %s\n", (unsigned int) m_library, m_path.c_str() );
		dll_close( m_library );
	}
}

std::string tcsdyntype::type()
{
	return m_type;
}

std::string tcsdyntype::path()
{
	return m_path;
}






tcskernel::tcskernel()
{
	m_uniqueId = 0;
	m_startTime = 1800; // start at 1/2 hour past midnight
	m_endTime = 31534200; // end at 1/2 hour before midnight 
	m_step = 3600; // hourly time step
	m_time = m_startTime;	
}

tcskernel::~tcskernel()
{
	clear_units();
	
	for ( std::vector<tcsdyntype*>::iterator it = m_libraries.begin();
		it != m_libraries.end();
		++it )
		delete (*it);
	
	m_libraries.clear();	
}

int tcskernel::get_unique_id()
{
	return m_uniqueId++;
}

const char *tcskernel::version()
{
	return "tcskernel version 1";
}

double tcskernel::time()
{
	return m_time;
}

double tcskernel::step()
{
	return m_step;
}

double tcskernel::start_time()
{
	return m_startTime;
}

double tcskernel::end_time()
{
	return m_endTime;
}

void tcskernel::log( const std::string &text )
{
	std::cout << text << std::endl;
}


static std::string to_string( int x )
{
	char buf[128];
	sprintf(buf, "%d", x);
	return std::string(buf);
}

static std::string to_string( double x )
{
	char buf[512];
	sprintf(buf, "%lf", x);
	return std::string(buf);
}

void tcskernel::warning( const char *src, int id, const char *msg )
{
	log( "time " + to_string(m_time) + " warning {" + std::string(src) + ", " + to_string(id) + "}:\n\t" + std::string(msg) + "\n");
}

void tcskernel::halt( const char *src, int id, const char *msg )
{
	log( "halt by component " + std::string(src) + " " + to_string(id) + " at time " + to_string(m_time)
		+ ": " + std::string(msg) );
}

void tcskernel::add_search_path( const std::string &path )
{
	std::vector<std::string>::iterator it = std::find( m_searchPathList.begin(), m_searchPathList.end(), path );
	if ( it == m_searchPathList.end() )
		m_searchPathList.push_back( path );
}

void tcskernel::clear_search_paths()
{
	m_searchPathList.clear();
}

tcsdyntype *tcskernel::library( const std::string &type )
{
	for ( std::vector<tcsdyntype*>::iterator it = m_libraries.begin();
		it != m_libraries.end();
		++it )
	{
		if ( (*it)->type() == type )
			return *it;		
	}
	
	tcsdyntype *d = new tcsdyntype( );
	if (! d->reload( m_searchPathList, type) )
	{
		delete d;
		return 0;
	}
	
	m_libraries.push_back( d );
	return d;
}

bool tcskernel::reload_libraries()
{
	bool ok = true;
	for ( std::vector<tcsdyntype*>::iterator it = m_libraries.begin();
		it != m_libraries.end();
		++it )
		if ( !(*it)->reload( m_searchPathList, (*it)->type()) )
			ok = false;

	return ok;
}

tcsunit *tcskernel::add_unit( const std::string &type, const std::string &name )
{
	tcsunit *u = new tcsunit( this );
	u->set_type( type );
	u->set_name( name );
	m_units.push_back( u );
	return u;
}

std::vector<tcsunit*> tcskernel::units()
{
	return m_units;
}

void tcskernel::clear_units()
{
	for ( std::vector<tcsunit*>::iterator it = m_units.begin();
		it != m_units.end();
		++it )
	{
		delete (*it);
	}
	
	m_units.clear();
}

void tcskernel::unload_units()
{
	for ( std::vector<tcsunit*>::iterator it = m_units.begin();
		it != m_units.end();
		++it )
		(*it)->unload();	
}
	
bool tcskernel::load_units()
{
	bool ok = true;
	for (std::vector<tcsunit*>::iterator it = m_units.begin();
		it != m_units.end();
		++it )
		if (! (*it)->load() )
			ok = false;
	
	return ok;
}

void tcskernel::connect( tcsunit *source, int output, tcsunit *target, int input )
{
}

void tcskernel::connect( double val, tcsunit *target, int input )
{
}

int main()
{
	tcskernel k;
	k.add_search_path(".");
	k.add_search_path("./types");	
	
	tcsunit *u1 = k.add_unit( "watertank", "Storage Tank 1" );
	u1->param( "nodes", new tcsparam( 3 ) );
	
	tcsunit *u2 = k.add_unit( "watertank", "Storage Tank 2" );
	u2->param( "nodes", new tcsparam( 3 ) );
	
	tcsunit *u3 = k.add_unit( "weather", "Weather File Data Reader" );
	u3->param( "file_name", new tcsparam( "c:/CO Boulder.tm2" ) );
	
	
	if ( !k.load_units() )
	{
		printf("failed to load all units!\n");
		return -1;
	}
	
	printf("nInputs: %d nOutputs: %d\n", u1->num_inputs(), u1->num_outputs() );
	printf("nInputs: %d nOutputs: %d\n", u2->num_inputs(), u2->num_outputs() );
	printf("nInputs: %d nOutputs: %d\n", u3->num_inputs(), u3->num_outputs() );
	
	
	printf("press enter to reload...");
	fflush(stdout);
	char c = fgetc(stdin);
	
	k.unload_units();
	
	bool ok = k.reload_libraries();
	printf("reload all libraries? %d\n", ok ? 1 : 0);
	
	k.load_units();
	
	printf("press enter to quit...");
	fflush(stdout);
	c = fgetc(stdin);
	
	return 0;
}

