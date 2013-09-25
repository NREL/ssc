#ifndef __tcstype_h
#define __tcstype_h


#if defined(__TCSTYPEINTERFACE__)
#define TCSEXPORT __declspec(dllexport)
#else
#define TCSEXPORT
#endif



extern "C" {
	struct _tcscontext {
		void *unit;
		const char * (*get_version)( struct _tcscontext *t );
		double (*get_current_time) ( struct _tcscontext *t );
		double (*get_start_time) ( struct _tcscontext *t );
		double (*get_end_time) ( struct _tcscontext *t );
		double (*get_step) ( struct _tcscontext *t );
		void (*warning) ( struct _tcscontext *t, const char *source, int id, const char *message );
		void (*halt) ( struct _tcscontext *t, const char *source, int id, const char *message );
		int (*param_number) ( struct _tcscontext *t, const char *name, double *val );
		double * (*param_array) ( struct _tcscontext *t, const char *name, int *count );
		double * (*param_matrix) ( struct _tcscontext *t, const char *name, int *nrows, int *ncols );
		const char * (*param_string) ( struct _tcscontext *t, const char *name );
	};
	typedef struct _tcscontext tcscontext;
}


#ifdef __TCSTYPEINTERFACE__

#include <string>
#include <numeric>
#include <cstring>
#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <cstdarg>
#include <limits>

class tcstypeinterface
{
public:
	tcstypeinterface( tcscontext *cxt, int id, const char *name );
	virtual ~tcstypeinterface();
	int __invoke( int mode, double *in, int n_in, double *out, int n_out );	
	void __config( int *nin, int *nout );
	virtual int init() = 0; // initialization and check (first) call
	virtual int call() = 0; // iterative call
protected:
	void configure( int ni, int no );
	double time( );
	double step( );
	double parameter( const char *name );
	double *parameter_array( const char *name, int *count );
	double *parameter_matrix( const char *name, int *nrows, int *ncols );
	const char *parameter_string( const char *name ); 
	double in( int idx );
	void out( int idx, double val );
	void warning( const char *msg, ... );
	void halt( const char *msg );
	
private:
	tcscontext *m_context;
	int m_typeId;
	std::string m_typeName;
	int m_nInputs, m_nOutputs;
	struct callinfo 
	{
		double *in, *out;
		int n_in, n_out;
	};
	callinfo m_ci;
};

tcstypeinterface::tcstypeinterface( tcscontext *cxt, int id, const char *name )
	: m_context(cxt), m_typeId(id), m_typeName(name), m_nInputs(-1), m_nOutputs(-1)
{
	m_ci.in = m_ci.out = 0;
	m_ci.n_in = m_ci.n_out = 0;
}

tcstypeinterface::~tcstypeinterface()
{
 /* nothing to do */
}

int tcstypeinterface::__invoke( int mode, double *in, int n_in, double *out, int n_out )
{
	m_ci.in = in; 
	m_ci.n_in = n_in;
	m_ci.out = out;
	m_ci.n_out = n_out;
	return mode == 0 ? init() : call();
}

void tcstypeinterface::__config( int *ni, int *no )
{
	*ni = m_nInputs;
	*no = m_nOutputs;
}

void tcstypeinterface::configure( int ni, int no )
{
	m_nInputs = ni;
	m_nOutputs = no;
}

double tcstypeinterface::time( )
{
	return m_context->get_current_time( m_context );
}

double tcstypeinterface::step( )
{
	return m_context->get_step( m_context );
}

double tcstypeinterface::parameter( const char *name )
{
	double val = 0;
	if ( m_context->param_number( m_context, name, &val ) != 0 )
	{
		warning( "invalid access to numeric parameter '%s'", name );
		return std::numeric_limits<double>::quiet_NaN();
	}
	else
		return val;
}

double *tcstypeinterface::parameter_array( const char *name, int *count )
{
	*count = 0;
	double *vec = m_context->param_array( m_context, name, count );
	if ( vec == 0 || count == 0 )
		warning("invalid access to array parameter '%s'", name);
	else
		return vec;
}

double *tcstypeinterface::parameter_matrix( const char *name, int *nrows, int *ncols )
{
	*nrows = *ncols = 0;
	double *mat = m_context->param_matrix( m_context, name, nrows, ncols );
	if ( mat == 0 || nrows == 0 || ncols == 0 )
		warning("invalid access to matrix parameter '%s'", name);
	else
		return mat;
}

const char *tcstypeinterface::parameter_string( const char *name )
{
	const char *s = m_context->param_string( m_context, name );
	if ( s == 0 )
		warning("invalid access to string parameter '%s'", name );
	else
		return s;
}

double tcstypeinterface::in( int idx )
{ 
	if (idx >= 0 && idx < m_ci.n_in)
		return m_ci.in[idx];
	else
	{
		warning( "invalid access to input [%d], %d max", idx, m_ci.n_in );
		return std::numeric_limits<double>::quiet_NaN();
	}
}

void tcstypeinterface::out( int idx, double val )
{
	if (idx >= 0 && idx < m_ci.n_out )
		m_ci.out[idx] = val;
	else
		warning( "invalid access to output [%d], %d max", idx, m_ci.n_out );
}

void tcstypeinterface::warning( const char *msg, ... )
{
	char buf[512];
	va_list ap;
	va_start(ap, msg);
#ifdef _MSC_VER	
	_vsnprintf(buf, 509, msg, ap);
#else
	vsnprintf(buf, 509, msg, ap);
#endif
	va_end(ap);
	m_context->warning( m_context, m_typeName.c_str(), m_typeId, buf );
}

void tcstypeinterface::halt( const char *msg )
{
	m_context->halt( m_context, m_typeName.c_str(), m_typeId, msg );
}


// export DLL functions for dynamic linkage to tcskernel

#define TCS_IMPLEMENT_TYPE( kls ) \
	extern "C" { \
	TCSEXPORT void *tcs_create( tcscontext *cxt, int id, const char *type ) \
	{ \
		kls *x = ( new kls(cxt, id, type) ); \
		if (!x) return 0; \
		int ni, no; \
		x->__config(&ni, &no); \
		if (ni < 0 || no < 0) \
		{ \
			cxt->warning( cxt, type, id, "failed to create: must call 'configure' in type constructor to register input and output vector lengths" ); \
			delete x; \
			return 0; \
		} \
		return (void*) x; \
	}  }
	
extern "C" {
TCSEXPORT void tcs_free( void *inst )
{
	tcstypeinterface *t = (tcstypeinterface*)inst;
	if ( t != 0 ) delete t;
}

TCSEXPORT int tcs_invoke( void *inst, int mode, double *in, int n_in, double *out, int n_out )
{
	tcstypeinterface *t = (tcstypeinterface*)inst;
	if ( t != 0 ) return t->__invoke( mode, in, n_in, out, n_out );
	else return -1;
}

TCSEXPORT int tcs_info( void *inst, int *ni, int *no )
{
	tcstypeinterface *t = (tcstypeinterface*)inst;
	if ( t != 0 )
	{
		t->__config(ni, no);
		return 0;
	}
	else return -1;
}

} // extern "C"

#endif /* __TCSTYPEINTERFACE__ */

#endif /* __tcstype_h */
