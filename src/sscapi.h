/*********************************************************************
 SSC: System Simulation Core
 A general purpose system simulation framework.
 Cross-platform (Windows/Unix) and is 64-bit compatible.
 (c) 2010 National Renewable Energy Laboratory
 Authors: Aron Dobos, Steve Janzou
*********************************************************************/

#ifndef __ssc_api_h
#define __ssc_api_h

#if defined(__WINDOWS__)&&defined(__DLL__)
#define SSCEXPORT __declspec(dllexport)
#else
#define SSCEXPORT
#endif

#ifdef __cplusplus
extern "C" {
#endif
	
/* Note:  Opaque pointer types will be 4-byte pointer 
	on 32-bit architectures, 8-byte pointer on 64-bit
	architectures.  Ensure that you are using the correct
	library for your operating platform: ssc32.dll or ssc64.dll 	*/

/* ssc_version: Returns the library version.  Version numbers start at 1. */
SSCEXPORT int ssc_version();


/* ssc_data_t: An opaque reference to a structure that holds a collection of variables.
  This structure can contain any number of variables referenced by name, and can hold
  strings, numbers, arrays, and matrices.  Matrices are stored in row-major order, where
  the array size is nrows*ncols, and the array index is calculated by r*ncols+c. 
  
  An ssc_data_t object holds all input and output variables for a simulation. It does not
  distinguish between input, output, and inout variables - that is handled at the model
  context level.

  For convenience, a ssc_data_t can be written to a file on disk, and later retrieved.
  */

typedef void* ssc_data_t;

/* Here the numeric type used internally in SSC is defined.  Do not change this
   without recompiling the library. All numeric values are stored in this format. */
typedef float ssc_number_t;
typedef int ssc_bool_t;

/* possible data types for variables in an ssc_data_t */
#define SSC_INVALID 0
#define SSC_STRING 1
#define SSC_NUMBER 2
#define SSC_ARRAY 3
#define SSC_MATRIX 4

/* create, release, query, read, and write data set objects */
SSCEXPORT ssc_data_t ssc_data_create();
SSCEXPORT void ssc_data_free( ssc_data_t p_data );
SSCEXPORT void ssc_data_unassign( ssc_data_t p_data, const char *name );
SSCEXPORT int ssc_data_query( ssc_data_t p_data, const char *name ); // returns SSC_INVALID if not assigned
/*SSCEXPORT ssc_bool_t ssc_data_write_disk( ssc_data_t p_data, const char *file );*/
/*SSCEXPORT ssc_bool_t ssc_data_read_disk( ssc_data_t p_data, const char *file );*/

/* assign values */
SSCEXPORT void ssc_data_set_string( ssc_data_t p_data, const char *name, const char *value );
SSCEXPORT void ssc_data_set_number( ssc_data_t p_data, const char *name, ssc_number_t value );
SSCEXPORT void ssc_data_set_array( ssc_data_t p_data, const char *name, ssc_number_t *pvalues, int length );
SSCEXPORT void ssc_data_set_matrix( ssc_data_t p_data, const char *name, ssc_number_t *pvalues, int nrows, int ncols );
/* retrieve values */
SSCEXPORT const char *ssc_data_get_string( ssc_data_t p_data, const char *name );
SSCEXPORT ssc_bool_t ssc_data_get_number( ssc_data_t p_data, const char *name, ssc_number_t *value );
SSCEXPORT const ssc_number_t *ssc_data_get_array( ssc_data_t p_data, const char *name, int *length );
SSCEXPORT const ssc_number_t *ssc_data_get_matrix( ssc_data_t p_data, const char *name, int *nrows, int *ncols );

/* ssc_module_entry: Returns information of all computation modules built into ssc. Example:
	int i=0;
	ssc_entry_t p_entry;
	while( p_entry = ssc_module_list(i++) )
	{
		printf("Compute Module '%s': \n", ssc_entry_name(p_entry), ssc_entry_description(p_entry));
	}
*/
typedef void* ssc_entry_t;
SSCEXPORT ssc_entry_t ssc_module_entry( int index );
SSCEXPORT const char *ssc_entry_name( ssc_entry_t p_entry );
SSCEXPORT const char *ssc_entry_description( ssc_entry_t p_entry );
SSCEXPORT int ssc_entry_version( ssc_entry_t p_entry );

/* ssc_module_t: An opaque reference to a computation module.
  A computation module performs a transformation on a ssc_data_t. It usually
  is used to calculate output variables given a set of input variables, but
  it can also be used to change the values of variables defined as INOUT. Modules
  types have unique names, and store information about what input variables are 
  required, what outputs can be expected, along with specific data type,
  unit, label, and meta information about each variable.
*/
typedef void* ssc_module_t; /* an opaque reference to a computation/transformation module */
typedef void* ssc_info_t; /* an opaque reference to variable information */

/* functions to create and release computation module objects */
SSCEXPORT ssc_module_t ssc_module_create( const char *name ); /* returns NULL if invalid name given and the module could not be created */
SSCEXPORT void ssc_module_free( ssc_module_t p_mod );

/* functions for obtaining information about variables */

/* variable type definitions */
#define SSC_INPUT 1
#define SSC_OUTPUT 2
#define SSC_INOUT 3

/* ssc_module_var_info: Returns references to variable info objects. Example for a previously created 'p_mod' object:
	int i=0;
	const ssc_info_t p_inf = NULL;
	while ( p_inf = ssc_module_var_info( p_mod, i++ ) )
	{
		int var_type = ssc_info_var_type( p_inf );   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
		int data_type = ssc_info_data_type( p_inf ); // SSC_STRING, SSC_NUMBER, SSC_ARRAY, SSC_MATRIX
		
		const char *name = ssc_info_name( p_inf );
		const char *label = ssc_info_label( p_inf );
		const char *units = ssc_info_units( p_inf );
		const char *meta = ssc_info_meta( p_inf );
		const char *group = ssc_info_group( p_inf );
	}

  Returns NULL for invalid index.

  Note that the ssc_info_* functions that return strings may return NULL if the computation module
  has not specified a value, i.e. no units or no grouping name.
*/

SSCEXPORT const ssc_info_t ssc_module_var_info( ssc_module_t p_mod, int index );

SSCEXPORT int ssc_info_var_type( ssc_info_t p_inf );
SSCEXPORT int ssc_info_data_type( ssc_info_t p_inf );
SSCEXPORT const char *ssc_info_name( ssc_info_t p_inf );
SSCEXPORT const char *ssc_info_label( ssc_info_t p_inf );
SSCEXPORT const char *ssc_info_units( ssc_info_t p_inf );
SSCEXPORT const char *ssc_info_meta( ssc_info_t p_inf );
SSCEXPORT const char *ssc_info_group( ssc_info_t p_inf );
SSCEXPORT const char *ssc_info_uihint( ssc_info_t p_inf );

/* ssc_module_exec_simple: The simplest way to run a computation module over a data set.
   Simply specify the name of the module, and a data set.  If the whole process succeeded,
   the function returns 1, otherwise 0.  No error messages are available. 
   This function can be thread-safe, depending on the computation module used.  If the
   computation module requires the execution of external binary executables, it is not
   thread-safe. However, simpler implementations that do all calculations internally are
   probably thread-safe.  Unfortunately there is no standard way to report the thread-safety
   of a particular computation module. This function can only work for computation modules
   that do not require any additional configuration parameters to run. */
SSCEXPORT ssc_bool_t ssc_module_exec_simple( const char *name, ssc_data_t p_data );

/* ssc_module_exec_simple_nothread: Another very simple way to run a computation module over a data set.
   The function returns NULL on success.  If something went wrong, the first error message is returned.
   Because the returned string references a common internal data container, this function 
   is never thread-safe.  This function can only work for computation modules
   that do not require any additional configuration parameters to run. */
SSCEXPORT const char *ssc_module_exec_simple_nothread( const char *name, ssc_data_t p_data );

/* action/notification types that can be sent to a handler function */
#define SSC_LOG 0 /* log a message. f0: (int)message type, f1: time, s0: message text, s1: unused */
#define SSC_UPDATE 1 /* notify simulation progress update. f0: percent done, f1: time, s0: current action text, s1: unused */
#define SSC_EXECUTE 2 /* request to run an external executable. f0: unused, f1: unused, s0: command line, s1: working directory */

/* on an external executable request, the handler function should:
   1. save the current working directory
   2. switch to the desired working directory (passed in arg2)
   3. run the specified command synchronously, redirecting the standard output stream of the child process
   4. for each line of output from the child process, call ssc_module_extproc_output( .. ) passing
      the computation module reference and the process output text
   5. when the process is done executing, restore the previously used working directory
   6. return 1 if everything went fine, or 0 if there was an error executing the child process

 ssc_module_exec: Runs a configured computation module over the specified data set.
   Returns 1 or 0.  Detailed notices, warnings, and errors can be retrieved
   either via a request_handler callback function, or using the ssc_module_message function. */

SSCEXPORT ssc_bool_t ssc_module_exec( ssc_module_t p_mod, ssc_data_t p_data ); /* uses default internal built-in handler */

typedef void* ssc_handler_t;

SSCEXPORT ssc_bool_t ssc_module_exec_with_handler( 
	ssc_module_t p_mod, 
	ssc_data_t p_data, 
	ssc_bool_t (*pf_handler)( ssc_module_t, ssc_handler_t, int action, float f0, float f1, const char *s0, const char *s1, void *user_data ),
	void *pf_user_data );


SSCEXPORT void ssc_module_extproc_output( ssc_handler_t p_mod, const char *output_line );

/* list all simulation parameters required for a computation module */
typedef void* ssc_param_t;
SSCEXPORT ssc_param_t ssc_module_parameter( ssc_module_t p_mod, int index );
SSCEXPORT const char *ssc_param_name( ssc_param_t p_param );
SSCEXPORT const char *ssc_param_description( ssc_param_t p_param );
SSCEXPORT const char *ssc_param_default_value( ssc_param_t p_param );
SSCEXPORT int ssc_param_type( ssc_param_t p_param );

/* Set computation module configuration parameters, return 1 or 0 
   these parameters can include simulation time steps, start and end times,
   working directories, local external executable names, supplemental file paths, etc */
SSCEXPORT void ssc_module_parameter_string( ssc_module_t p_mod, const char *name, const char *value );
SSCEXPORT void ssc_module_parameter_float( ssc_module_t p_mod, const char *name, float value );

#define SSC_NOTICE 1
#define SSC_WARNING 2
#define SSC_ERROR 3

/* Retrive notices, warnings, and error messages from the simulation */
SSCEXPORT const char *ssc_module_log( ssc_module_t p_mod, int index, int *item_type, float *time );


#ifdef __cplusplus
} /* extern "C" */
#endif

#endif

