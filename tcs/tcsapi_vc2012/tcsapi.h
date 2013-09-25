/**
   \file tcsaip.h

   \brief TCS: Transient Component Simulator Interop API

   An input/output framework meant to allow SAM access to the TCS kernel, based on the SSC Interop.
   Cross-platform (Windows/MacOSX/Unix) and is 32 and 64-bit compatible.

   Be sure to use the correct library for your operating platform: tcs32
   or tcs64. Opaque pointer types will be 4-byte pointer on 32-bit architectures,
   and 8-byte pointer on 64-bit architectures.

   Shared libraries have the .dll file extension on Windows,
   .dylib on MacOSX, and .so on Linux/Unix.

  \copyright 2013 National Renewable Energy Laboratory
  \authors Aron Dobos, Steven Janzou, Tom Ferguson
  */

#include "tcstype.h"

#ifndef __tcs_api_h
#define __tcs_api_h

// done in tcstype.h
//#if defined(__WINDOWS__)&&defined(__DLL__)
//#define TCSEXPORT __declspec(dllexport)
//#else
//#define TCSEXPORT
//#endif

#ifndef __TCSLINKAGECPP__

#ifdef __cplusplus
extern "C" {
#endif

#endif // __TCSLINKAGECPP__
	
/** Returns the library version number as an integer.  Version numbers start at 1. */
TCSEXPORT int tcsapi_version();

/** Returns information about the build configuration of this particular TCS library binary as a text string that lists the compiler, platform, build date/time and other information. */
TCSEXPORT const char *tcs_build_info();

/** An opaque reference to a structure that holds a collection of variables.  This structure can contain any number of variables referenced by name, and can hold strings, numbers, arrays, and matrices.  Matrices are stored in row-major order, where the array size is nrows*ncols, and the array index is calculated by r*ncols+c. An tcs_data_t object holds all input and output variables for a simulation. It does not distinguish between input, output, and input variables - that is handled at the model context level. */
typedef void* tcs_data_t;

/** The numeric type used in the TCS API. All numeric values are stored in this format. TCS uses 32-bit floating point numbers at the library interface to minimize memory usage.  Calculations inside compute modules generally are performed with double-precision 64-bit floating point internally. */
typedef float tcs_number_t;

/** The boolean type used internally in TCS. Zero values represent false; non-zero represents true. */
typedef int tcs_bool_t;

/** @name Data types:
  * Possible data types for variables in an tcs_data_t:
*/
/**@{*/ 
//#define TCS_INVALID 0
//#define TCS_STRING 1
//#define TCS_NUMBER 2
//#define TCS_ARRAY 3
//#define TCS_MATRIX 4
//#define TCS_TABLE 5
/**@}*/ 

/** Creates a new data object in memory.  A data object stores a table of named values, where each value can be of any TCS datatype. */
TCSEXPORT tcs_data_t tcs_data_create();

/** Frees the memory associated with a data object, where p_data is the data container to free. */
TCSEXPORT void tcs_data_free( tcs_data_t p_data );

/** Clears all of the variables in a data object. */
TCSEXPORT void tcs_data_clear( tcs_data_t p_data );

/** Unassigns the variable with the specified name. */
TCSEXPORT void tcs_data_unassign( tcs_data_t p_data, const char *name );

/** Querys the data object for the data type of the variable with the specified name. Returns the data object's data type, or TCS_INVALID if that variable was not found. */
TCSEXPORT int tcs_data_query( tcs_data_t p_data, const char *name );

/** Returns the name of the first variable in the table, or 0 (NULL) if the data object is empty. */
TCSEXPORT const char *tcs_data_first( tcs_data_t p_data );

/** Returns the name of the next variable in the table, or 0 (NULL) if there are no more variables in the table.  tcs_data_first must be called first. Example that iterates over all variables in a data object:

   \verbatim
	const char *key = tcs_data_first( my_data );
	while (key != 0)
	{
		int type = tcs_data_query( my_data, key );
		key = tcs_data_next( my_data );
	}
    \endverbatim

 */
TCSEXPORT const char *tcs_data_next( tcs_data_t p_data );

/** @name Assigning variable values.
The following functions do not take ownership of the data pointeres for arrays, matrices, and tables. A deep copy is made into the internal TCS engine. You must remember to free the table that you create to pass into 
tcs_data_set_table( ) for example.
*/
/**@{*/
/** Assigns value of type @a TCS_STRING */
TCSEXPORT void tcs_data_set_string( tcs_data_t p_data, const char *name, const char *value );

/** Assigns value of type @a TCS_NUMBER */
TCSEXPORT void tcs_data_set_number( tcs_data_t p_data, const char *name, tcs_number_t value );

/** Assigns value of type @a TCS_ARRAY */
TCSEXPORT void tcs_data_set_array( tcs_data_t p_data, const char *name, tcs_number_t *pvalues, int length );

/** Assigns value of type @a TCS_MATRIX . Matrices are specified as a continuous array, in row-major order.  Example: the matrix [[5,2,3],[9,1,4]] is stored as [5,2,3,9,1,4]. */
TCSEXPORT void tcs_data_set_matrix( tcs_data_t p_data, const char *name, tcs_number_t *pvalues, int nrows, int ncols );

/** Assigns value of type @a TCS_TABLE. */
TCSEXPORT void tcs_data_set_table( tcs_data_t p_data, const char *name, tcs_data_t table );
/**@}*/ 

/** @name Retrieving variable values.
The following functions return internal references to memory, and the returned string, array, matrix, and tables should not be freed by the user.
*/
/**@{*/
/** Returns the value of a @a TCS_STRING variable with the given name. */
TCSEXPORT const char *tcs_data_get_string( tcs_data_t p_data, const char *name );

/** Returns the value of a @a TCS_NUMBER variable with the given name. */
TCSEXPORT tcs_bool_t tcs_data_get_number( tcs_data_t p_data, const char *name, tcs_number_t *value );

/** Returns the value of a @a TCS_ARRAY variable with the given name. */
TCSEXPORT tcs_number_t *tcs_data_get_array( tcs_data_t p_data, const char *name, int *length );

/** Returns the value of a @a TCS_MATRIX variable with the given name. Matrices are specified as a continuous array, in row-major order.  Example: the matrix [[5,2,3],[9,1,4]] is stored as [5,2,3,9,1,4]. */
TCSEXPORT tcs_number_t *tcs_data_get_matrix( tcs_data_t p_data, const char *name, int *nrows, int *ncols );

/** Returns the value of a @a TCS_TABLE variable with the given name. */
TCSEXPORT tcs_data_t tcs_data_get_table( tcs_data_t p_data, const char *name );
/**@}*/ 

/** The opaque data structure that stores information about a compute module. */
typedef void* tcs_entry_t;

/** Returns compute module information for the i-th module in the TCS library. Returns 0 (NULL) for an invalid index. Example:

	\verbatim
	int i=0;
	tcs_entry_t p_entry;
	while( p_entry = tcs_module_entry(i++) )
	{
		printf("Compute Module '%s': \n", 
		     	tcs_entry_name(p_entry), 
			    tcs_entry_description(p_entry) );
	}
	\endverbatim
*/
TCSEXPORT tcs_entry_t tcs_module_entry( int index );

/** Returns the name of a compute module.  This is the name that is used to create a new compute module. */
TCSEXPORT const char *tcs_entry_name( tcs_entry_t p_entry );

/** Returns a short text description of a compute module. */
TCSEXPORT const char *tcs_entry_description( tcs_entry_t p_entry );

/** Returns version information about a compute module. */
TCSEXPORT int tcs_entry_version( tcs_entry_t p_entry );

/** An opaque reference to a computation module. A computation module performs a transformation on a tcs_data_t. It usually is used to calculate output variables given a set of input variables, but it can also be used to change the values of variables defined as INOUT. Modules types have unique names, and store information about what input variables are required, what outputs can be expected, along with specific data type, unit, label, and meta information about each variable. */
typedef void* tcs_module_t;

/** An opaque reference to variable information.  A compute module defines its input/output variables. */
typedef void* tcs_info_t;

/** Creates an instance of a compute module with the given name. Returns 0 (NULL) if invalid name given and the module could not be created */
TCSEXPORT tcs_module_t tcs_module_create( const char *name );

/** Releases an instance of a compute module created with tcs_module_create */
TCSEXPORT void tcs_module_free( tcs_module_t p_mod );

/** @name Variable types:*/
/**@{*/ 	
//#define TCS_INPUT 1
//#define TCS_OUTPUT 2
//#define TCS_INOUT 3
/**@}*/

/** Returns references to variable info objects.  Returns NULL for invalid index. Note that the tcs_info_* functions that return strings may return NULL if the computation module has not specified a value, i.e. no units or no grouping name. Example for a previously created 'p_mod' object:
  
	\verbatim 
	int i=0;
	const tcs_info_t p_inf = NULL;
	while ( p_inf = tcs_module_var_info( p_mod, i++ ) )
	{
		int var_type = tcs_info_var_type( p_inf );   // TCS_INPUT, TCS_OUTPUT, TCS_PARAM
		int data_type = tcs_info_data_type( p_inf ); // TCS_STRING, TCS_NUMBER, TCS_ARRAY, TCS_MATRIX
		
		const char *name = tcs_info_name( p_inf );
		const char *label = tcs_info_label( p_inf );
		const char *units = tcs_info_units( p_inf );
		const char *meta = tcs_info_meta( p_inf );
		const char *group = tcs_info_group( p_inf );
	}
	\endverbatim
*/
TCSEXPORT const tcs_info_t tcs_module_var_info( tcs_module_t p_mod, int index );

/** Returns variable type information: TCS_INPUT, TCS_OUTPUT, or TCS_PARAM */
TCSEXPORT int tcs_info_var_type( tcs_info_t p_inf );

/** Returns the data type of a variable: TCS_STRING, TCS_NUMBER, TCS_ARRAY, TCS_MATRIX, TCS_TABLE */
TCSEXPORT int tcs_info_data_type( tcs_info_t p_inf );

/** Returns the name of a variable */
TCSEXPORT const char *tcs_info_name( tcs_info_t p_inf );

/** Returns the short label description of the variable */
TCSEXPORT const char *tcs_info_label( tcs_info_t p_inf );

/** Returns the units of the values for the variable */
TCSEXPORT const char *tcs_info_units( tcs_info_t p_inf );

/** Returns any extra information about a variable */
TCSEXPORT const char *tcs_info_meta( tcs_info_t p_inf );

/** Returns any grouping information.  Variables can be assigned to groups for presentation to the user, for example */
TCSEXPORT const char *tcs_info_group( tcs_info_t p_inf );

/** Returns information about whether a variable is required to be assigned for 
a compute module to run.  It may alternatively be given a default value, specified as '?=<value>'. */
TCSEXPORT const char *tcs_info_required( tcs_info_t p_inf );

/** Returns constraints on the values accepted.  For example, MIN, MAX, BOOLEAN, INTEGER, POSITIVE are possible constraints. */
TCSEXPORT const char *tcs_info_constraints( tcs_info_t p_inf );

/** Returns additional information for use in a target application about how to show the variable to the user.  Not used currently. */
TCSEXPORT const char *tcs_info_uihint( tcs_info_t p_inf );

/** The simplest way to run a computation module over a data set. Simply specify the name of the module, and a data set.  If the whole process succeeded, the function returns 1, otherwise 0.  No error messages are available. This function can be thread-safe, depending on the computation module used. If the computation module requires the execution of external binary executables, it is not thread-safe. However, simpler implementations that do all calculations internally are probably thread-safe.  Unfortunately there is no standard way to report the thread-safety of a particular computation module. */
TCSEXPORT tcs_bool_t tcs_module_exec_simple( const char *name, tcs_data_t p_data );

/** Another very simple way to run a computation module over a data set. The function returns NULL on success.  If something went wrong, the first error message is returned. Because the returned string references a common internal data container, this function is never thread-safe.  */
TCSEXPORT const char *tcs_module_exec_simple_nothread( const char *name, tcs_data_t p_data );

/** @name Action/notification types that can be sent to a handler function: 
  *	TCS_LOG: Log a message in the handler. f0: (int)message type, f1: time, s0: message text, s1: unused. 
  *	TCS_UPDATE: Notify simulation progress update. f0: percent done, f1: time, s0: current action text, s1: unused.
*/
/**@{*/ 
#define TCS_LOG 0
#define TCS_UPDATE 1
/**@}*/

/** Runs an instantiated computation module over the specified data set. Returns Boolean: 1 or 0. Detailed notices, warnings, and errors can be retrieved using the tcs_module_log function. */
TCSEXPORT tcs_bool_t tcs_module_exec( tcs_module_t p_mod, tcs_data_t p_data ); /* uses default internal built-in handler */

/** An opaque pointer for transferring external executable output back to TCS */ 
typedef void* tcs_handler_t;

/** A full-featured way to run a compute module with a callback function to handle custom logging, progress updates, and cancelation requests. Returns Boolean: 1 or 0 indicating success or failure. */
TCSEXPORT tcs_bool_t tcs_module_exec_with_handler( 
	tcs_module_t p_mod, 
	tcs_data_t p_data, 
	tcs_bool_t (*pf_handler)( tcs_module_t, tcs_handler_t, int action, float f0, float f1, const char *s0, const char *s1, void *user_data ),
	void *pf_user_data );

/** @name Message types:*/
/**@{*/ 	
#define TCS_NOTICE 1
#define TCS_WARNING 2
#define TCS_ERROR 3
/**@}*/

/** Retrive notices, warnings, and error messages from the simulation. Returns a NULL-terminated ASCII C string with the message text, or NULL if the index passed in was invalid. */
TCSEXPORT const char *tcs_module_log( tcs_module_t p_mod, int index, int *item_type, float *time );

/** DO NOT CALL THIS FUNCTION: immediately causes a segmentation fault within the library. This is only useful for testing crash handling from an external application that is dynamically linked to the TCS library */
TCSEXPORT void __tcs_segfault();

#ifndef __TCSLINKAGECPP__

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif // __TCSLINKAGECPP__

#endif
