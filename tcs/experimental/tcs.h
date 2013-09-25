#ifndef __tcs_h
#define __tcs_h

/* tcs: transient computation simulator 
   (c) nrel, 2011,  author: aron dobos */
   
#include <string>
#include <vector>


#ifdef _MSC_VER
#include <unordered_map>
using std::tr1::unordered_map;
#else
#include <tr1/unordered_map>
using std::tr1::unordered_map;
#endif

#include "tcstype.h"

class tcskernel;

class tcsparam
{
public:
	enum { INVALID, NUMBER, ARRAY, MATRIX, STRING };
	
	tcsparam( double val );
	tcsparam( double *p, int len );
	tcsparam( double *p, int nrows, int ncols );
	tcsparam( const std::string &s );	
	~tcsparam();
	
	int type( );
	double val( );
	std::string str( );
	double *arr( int *len );
	double *mat( int *nr, int *nc );
	
private:
	int m_type;
	std::string m_str;
	double *m_p;
	double m_val;
	int m_len, m_nrows, m_ncols;
};

typedef unordered_map< std::string, tcsparam* > tcsparamhash;

class tcsdyntype;

class tcsunit
{
public:
	tcsunit( tcskernel *kern );
	~tcsunit();
	
	bool load( );
	void unload( );
	bool is_loaded( );
	int call( double *in, int n_in, double *out, int n_out );
	
	void set_type( const std::string &type );
	void set_name( const std::string &name );
	std::string name();
	std::string type();
	int num_inputs();
	int num_outputs();
	
	tcskernel *kernel();	
	
	void param( const char *name, tcsparam *val );	
	int param( const char *name, double *val );
	double *param( const char *name, int *count );
	double *param( const char *name, int *nrows, int *ncols );
	const char *param( const char *name );
	
private:
	std::string m_type;
	std::string m_name;
	tcskernel *m_kernel;
	tcscontext *m_context;
	void *m_pInstance;
	int m_nInputs, m_nOutputs;
	int m_ncall;
	tcsparamhash m_params;
	
	tcsdyntype *m_dyntype;
};

class tcsdyntype
{
public:	
	tcsdyntype();
	~tcsdyntype();

	bool reload( const std::vector<std::string> &paths, const std::string &type );
	
	void *create( tcscontext *cxt, int id, const char *type );
	void free( void *obj );
	int invoke( void *inst, int mode, double *in, int n_in, double *out, int n_out );
	int info( void *inst, int *ni, int *no );

	std::string type();
	std::string path();
	
private:
	void* (*tcs_create) ( tcscontext *cxt, int id, const char *type );
	void  (*tcs_free)   ( void *inst );
	int   (*tcs_invoke) ( void *inst, int mode, double *in, int n_in, double *out, int n_out );
	int   (*tcs_info)   ( void *inst, int *ni, int *no );

	void *m_library;
	std::string m_type;
	std::string m_path;	
	
	void unload();
};

class tcskernel
{
public:
	tcskernel();
	~tcskernel();
	
	virtual void log( const std::string & text );
	
	int get_unique_id();
	const char *version();
	double time();
	double step();
	double start_time();
	double end_time();
	
	void add_search_path( const std::string &path );
	void clear_search_paths();
	
	tcsdyntype *library( const std::string &type );	
	bool reload_libraries();
	
	void warning( const char *src, int id, const char *msg );
	void halt( const char *src, int id, const char *msg );
	
	tcsunit *add_unit( const std::string &type, const std::string &name );	
	std::vector<tcsunit*> units();
	void clear_units();
	bool load_units();
	void unload_units();
	
	void connect( tcsunit *source, int output, tcsunit *target, int input );
	void connect( double val, tcsunit *target, int input );	
				
private:

	int m_uniqueId;
	double m_startTime;
	double m_endTime;
	double m_step;
	double m_time;

	std::vector<tcsdyntype*> m_libraries;	
	std::vector<std::string> m_searchPathList;
	std::vector<tcsunit*> m_units;
		
};

#endif
