/**
\file vbapi.h

\brief SSC: SAM Simulation Core _stdcall wrapper for VBA

Be sure to use the correct library for your operating platform: ssc32
or ssc64. Opaque pointer types will be 4-byte pointer on 32-bit architectures,
and 8-byte pointer on 64-bit architectures.

Shared libraries have the .dll file extension on Windows,

\copyright 2012 National Renewable Energy Laboratory
\authors Aron Dobos, Steven Janzou

   C DLL Interface for Visual Basic (_stdcall)
*/

#ifndef __vbapi_h
#define __vbapi_h

#include "sscapi.h"

#ifdef __cplusplus
extern "C" {
#endif

#define VBCALL_CONVENTION _stdcall

SSCEXPORT long VBCALL_CONVENTION sscvb_version();
SSCEXPORT long VBCALL_CONVENTION sscvb_build_info(const char *build_info);

SSCEXPORT void *VBCALL_CONVENTION sscvb_data_create();
SSCEXPORT long VBCALL_CONVENTION sscvb_data_free(long p_data);
SSCEXPORT long VBCALL_CONVENTION sscvb_data_clear(long p_data);
SSCEXPORT long VBCALL_CONVENTION sscvb_data_unassign(long p_data, const char *name);
SSCEXPORT long VBCALL_CONVENTION sscvb_data_query(long p_data, const char *name);
SSCEXPORT long VBCALL_CONVENTION sscvb_data_first(long p_data, const char *data_first);
SSCEXPORT long VBCALL_CONVENTION sscvb_data_next(long p_data, const char *data_next);

SSCEXPORT long VBCALL_CONVENTION sscvb_data_set_string(long p_data, const char *name, const char *value);
SSCEXPORT long VBCALL_CONVENTION sscvb_data_set_number(void *p_data, const char *name, double value);
SSCEXPORT long VBCALL_CONVENTION sscvb_data_set_array(long p_data, const char *name, double *pvalues, long length);
SSCEXPORT long VBCALL_CONVENTION sscvb_data_set_matrix(long p_data, const char *name, double *pvalues, long nrows, long ncols);
SSCEXPORT long VBCALL_CONVENTION sscvb_data_set_table(long p_data, const char *name, long table);

SSCEXPORT long VBCALL_CONVENTION sscvb_data_get_string(long p_data, const char *name, const char *value);
SSCEXPORT long VBCALL_CONVENTION sscvb_data_get_number(long p_data, const char *name, double *value);
SSCEXPORT long VBCALL_CONVENTION sscvb_data_get_array(long p_data, const char *name, double *value, long length);
SSCEXPORT long VBCALL_CONVENTION sscvb_data_get_matrix(long p_data, const char *name, double *value, long *nrows, long *ncols);
// TODO test this
SSCEXPORT long VBCALL_CONVENTION sscvb_data_get_table(long p_data, const char *name, long table);

SSCEXPORT long VBCALL_CONVENTION sscvb_module_entry( long index);
SSCEXPORT long VBCALL_CONVENTION sscvb_entry_name(long p_entry, const char *name);
SSCEXPORT long VBCALL_CONVENTION sscvb_entry_description(long p_entry, const char *description);
SSCEXPORT long VBCALL_CONVENTION sscvb_entry_version(long p_entry, long index);

SSCEXPORT long VBCALL_CONVENTION sscvb_module_create(const char *name);
SSCEXPORT long VBCALL_CONVENTION sscvb_module_free(long p_mod);

SSCEXPORT long VBCALL_CONVENTION sscvb_module_var_info(long p_mod, long index);
SSCEXPORT long VBCALL_CONVENTION sscvb_info_var_type(long p_inf);
SSCEXPORT long VBCALL_CONVENTION sscvb_info_data_type(long p_inf);
SSCEXPORT long VBCALL_CONVENTION sscvb_info_name(long p_inf, const char *name);
SSCEXPORT long VBCALL_CONVENTION sscvb_info_label(long p_inf, const char *label);
SSCEXPORT long VBCALL_CONVENTION sscvb_info_units(long p_inf, const char *units);
SSCEXPORT long VBCALL_CONVENTION sscvb_info_meta(long p_inf, const char *meta);
SSCEXPORT long VBCALL_CONVENTION sscvb_info_group(long p_inf, const char *group);
SSCEXPORT long VBCALL_CONVENTION sscvb_info_required(long p_inf, const char *required);
SSCEXPORT long VBCALL_CONVENTION sscvb_info_constraints(long p_inf, const char *constraints);
SSCEXPORT long VBCALL_CONVENTION sscvb_info_uihint(long p_inf, const char *uihint);

SSCEXPORT long VBCALL_CONVENTION sscvb_module_exec_set_print(long print);

SSCEXPORT long VBCALL_CONVENTION sscvb_module_exec_simple(const char *name, long p_data);
SSCEXPORT long VBCALL_CONVENTION sscvb_module_exec_simple_nothread(const char *name, long p_data, const char *msg);
SSCEXPORT long VBCALL_CONVENTION sscvb_module_exec(long p_mod, long p_data);

// TODO test this
SSCEXPORT long VBCALL_CONVENTION sscvb_module_exec_with_handler(long p_mod, long p_data, long pf_handler, long pf_user_data);

SSCEXPORT long VBCALL_CONVENTION sscvb_module_log(long p_mod, long index, long *item_type, float *time, const char *msg);
SSCEXPORT long VBCALL_CONVENTION __sscvb_segfault();


#ifdef __cplusplus
} /* extern "C" */
#endif


#endif
