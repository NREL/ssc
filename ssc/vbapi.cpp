#include <stdlib.h>
#include <string.h>
#include "vbapi.h"

// test without marshalling

SSCEXPORT long VBCALL_CONVENTION sscvb_version()
{
	return (long)ssc_version();
}

SSCEXPORT long VBCALL_CONVENTION sscvb_build_info(const char *build_info)
{
	build_info = ssc_build_info();
	return (long)strlen(build_info);
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_create()
{
	return (long)ssc_data_create();
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_free(long p_data)
{
	if ((void*)p_data)
	{
		ssc_data_free((void*)p_data);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_clear(long p_data)
{
	if ((void*)p_data)
	{
		ssc_data_clear((void*)p_data);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_unassign(long p_data, const char *name)
{
	if ((void*)p_data)
	{
		ssc_data_unassign((void*)p_data, name);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_query(long p_data, const char *name)
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_first(long p_data, const char *data_first)
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_next(long p_data, const char *data_next)
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_set_string(long p_data, const char *name, const char *value)
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_set_number(long p_data, const char *name, ssc_number_t value)
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_set_array(long p_data, const char *name, ssc_number_t *pvalues, long length)
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_set_matrix(long p_data, const char *name, ssc_number_t *pvalues, long nrows, long ncols)
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_set_table(long p_data, const char *name, long table)
{
	return 0;
}


SSCEXPORT long VBCALL_CONVENTION sscvb_data_get_string(long p_data, const char *name, const char *value)
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_get_number(long p_data, const char *name, ssc_number_t *value)
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_get_array(long p_data, const char *name, ssc_number_t *value, long length)
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_get_matrix(long p_data, const char *name, ssc_number_t *value, long nrows, long ncols)
{
	return 0;
}

// TODO test this
SSCEXPORT long VBCALL_CONVENTION sscvb_data_get_table(long p_data, const char *name, long value)
{
	return 0;
}


SSCEXPORT long VBCALL_CONVENTION sscvb_module_entry(long p_entry, long index)
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_entry_name(long p_entry, const char *name)
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_entry_description(long p_entry, const char *description)
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_entry_version(long p_entry, long index)
{
	return 0;
}


SSCEXPORT long VBCALL_CONVENTION sscvb_module_create(const char *name)
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_module_free(long p_mod)
{
	return 0;
}


SSCEXPORT long VBCALL_CONVENTION sscvb_module_var_info(long p_mod, long index, long p_inf)
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_var_type(long p_inf, long var_type)
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_data_type(long p_inf, long data_type)
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_name(long p_inf, const char *name)
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_label(long p_inf, const char *label)
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_units(long p_inf, const char *units)
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_meta(long p_inf, const char *meta)
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_group(long p_inf, const char *group)
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_required(long p_inf, const char *required)
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_constraints(long p_inf, const char *constraints)
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_uihint(long p_inf, const char *uihint)
{
	return 0;
}


SSCEXPORT long VBCALL_CONVENTION sscvb_module_exec_set_print(long print)
{
	return 0;
}


SSCEXPORT long VBCALL_CONVENTION sscvb_module_exec_simple(const char *name, long p_data, long success)
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_module_exec_simple_nothread(const char *name, long p_data, const char *msg)\
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_module_exec(long p_mod, long p_data, long success)
{
	return 0;
}


// TODO test this
SSCEXPORT long VBCALL_CONVENTION sscvb_module_exec_with_handler(long p_mod, long p_data, long pf_handler, long pf_user_data, long success)
{
	return 0;
}


SSCEXPORT long VBCALL_CONVENTION sscvb_module_log(long p_mod, long index, long *item_type, float *time, const char *msg)
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION __sscvb_segfault()
{
	return 0;
}

