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
	if ((void*)p_data)
	{
		return (long)ssc_data_query((void*)p_data, name);
	}
	else
		return 0;
	
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_first(long p_data, const char *data_first)
{
	if ((void*)p_data)
	{
		data_first = ssc_data_first((void*)p_data);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_next(long p_data, const char *data_next)
{
	if ((void*)p_data)
	{
		data_next = ssc_data_next((void*)p_data);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_set_string(long p_data, const char *name, const char *value)
{
	if ((void*)p_data)
	{
		ssc_data_set_string((void*)p_data, name, value);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_set_number(long p_data, const char *name, double value)
{
	if ((void*)p_data)
	{
		ssc_data_set_number((void*)p_data, name, value);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_set_array(long p_data, const char *name, double *pvalues, long length)
{
	if ((void*)p_data)
	{

//		ssc_data_set_array((void*)p_data, name, pvalues, length);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_set_matrix(long p_data, const char *name, double *pvalues, long nrows, long ncols)
{
	if ((void*)p_data)
	{
//		ssc_data_set_matrix((void*)p_data, name, pvalues, nrows, ncols);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_set_table(long p_data, const char *name, long table)
{
	if ((void*)p_data && (void*)table)
	{
		ssc_data_set_table((void*)p_data, name, (void*)table);
		return 1;
	}
	else
		return 0;
}


SSCEXPORT long VBCALL_CONVENTION sscvb_data_get_string(long p_data, const char *name, const char *value)
{
	if ((void*)p_data)
	{
		value = ssc_data_get_string((void*)p_data, name);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_get_number(long p_data, const char *name, double *value)
{
	if ((void*)p_data)
	{
		ssc_number_t val;
		ssc_data_get_number((void*)p_data, name, &val);
		*value = (double)val;
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_get_array(long p_data, const char *name, double *value, long length)
{
	if ((void*)p_data)
	{
		int len;
//		value = ssc_data_get_array((void*)p_data, name, &len);
		return (long)len;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_data_get_matrix(long p_data, const char *name, double *value, long *nrows, long *ncols)
{
	if ((void*)p_data)
	{
		int nrow, ncol;
//		value = ssc_data_get_matrix((void*)p_data, name, &nrow, &ncol);
		*nrows = (long)nrow;
		*ncols = (long)ncol;
		return 1;
	}
	else
		return 0;
}

// TODO test this
SSCEXPORT long VBCALL_CONVENTION sscvb_data_get_table(long p_data, const char *name, long table)
{
	if ((void*)p_data && (void*)table)
	{
		return (long)ssc_data_get_table((void*)p_data, name);
	}
	else
		return 0;
}


SSCEXPORT long VBCALL_CONVENTION sscvb_module_entry(long index)
{
	return (long)ssc_module_entry((int)index);
}

SSCEXPORT long VBCALL_CONVENTION sscvb_entry_name(long p_entry, const char *name)
{
	if ((void*)p_entry)
	{
		name = ssc_entry_name((void*)p_entry);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_entry_description(long p_entry, const char *description)
{
	if ((void*)p_entry)
	{
		description = ssc_entry_description((void*)p_entry);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_entry_version(long p_entry, long index)
{
	if ((void*)p_entry)
	{
		return (long)ssc_entry_version((void*)p_entry);
	}
	else
		return 0;
}


SSCEXPORT long VBCALL_CONVENTION sscvb_module_create(const char *name)
{
	return (long)ssc_module_create(name);
}

SSCEXPORT long VBCALL_CONVENTION sscvb_module_free(long p_mod)
{
	if ((void *)p_mod)
	{
		ssc_module_free((void *)p_mod);
		return 1;
	}
	else
		return 0;
}


SSCEXPORT long VBCALL_CONVENTION sscvb_module_var_info(long p_mod, long index)
{
	if ((void *)p_mod)
		return (long)ssc_module_var_info((void *)p_mod, (int)index);
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_var_type(long p_inf)
{
	if ((void *)p_inf)
		return (long)ssc_info_var_type((void *)p_inf);
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_data_type(long p_inf)
{
	if ((void *)p_inf)
		return (long)ssc_info_data_type((void *)p_inf);
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_name(long p_inf, const char *name)
{
	if ((void *)p_inf)
	{
		name = ssc_info_name((void *)p_inf);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_label(long p_inf, const char *label)
{
	if ((void *)p_inf)
	{
		label = ssc_info_label((void *)p_inf);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_units(long p_inf, const char *units)
{
	if ((void *)p_inf)
	{
		units = ssc_info_units((void *)p_inf);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_meta(long p_inf, const char *meta)
{
	if ((void *)p_inf)
	{
		meta = ssc_info_meta((void *)p_inf);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_group(long p_inf, const char *group)
{
	if ((void *)p_inf)
	{
		group = ssc_info_group((void *)p_inf);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_required(long p_inf, const char *required)
{
	if ((void *)p_inf)
	{
		required = ssc_info_required((void *)p_inf);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_constraints(long p_inf, const char *constraints)
{
	if ((void *)p_inf)
	{
		constraints = ssc_info_constraints((void *)p_inf);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_info_uihint(long p_inf, const char *uihint)
{
	if ((void *)p_inf)
	{
		uihint = ssc_info_uihint((void *)p_inf);
		return 1;
	}
	else
		return 0;
}


SSCEXPORT long VBCALL_CONVENTION sscvb_module_exec_set_print(long print)
{
	ssc_module_exec_set_print((int)print);
	return 1;
}


SSCEXPORT long VBCALL_CONVENTION sscvb_module_exec_simple(const char *name, long p_data, long success)
{
	if ((void *)p_data)
		return (long)ssc_module_exec_simple(name, (void *)p_data);
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_module_exec_simple_nothread(const char *name, long p_data, const char *msg)
{
	if ((void *)p_data)
	{
		msg = ssc_module_exec_simple_nothread(name, (void *)p_data);
		return 1;
	}
	else
		return 0;
}

SSCEXPORT long VBCALL_CONVENTION sscvb_module_exec(long p_mod, long p_data)
{
	if ((void *)p_mod && (void *)p_data)
		return (long)ssc_module_exec((void *)p_mod, (void *)p_data);
	else
		return 0;
}


// TODO test this
SSCEXPORT long VBCALL_CONVENTION sscvb_module_exec_with_handler(long p_mod, long p_data, long pf_handler, long pf_user_data)
{
	if ((void *)p_mod && (void *)p_data)
		return (long)ssc_module_exec_with_handler((void *)p_mod, (void *)p_data, (ssc_bool_t(*)(ssc_module_t, ssc_handler_t, int action, float f0, float f1, const char *s0, const char *s1, void *user_data))pf_handler, (void *)pf_user_data);
	else
		return 0;
}


SSCEXPORT long VBCALL_CONVENTION sscvb_module_log(long p_mod, long index, long *item_type, float *time, const char *msg)
{
	return 0;
}

SSCEXPORT long VBCALL_CONVENTION __sscvb_segfault()
{
	__ssc_segfault();
	return 1;
}

