#include <cstring>

#include "core.h"
#include "sscapi.h"

SSCEXPORT int ssc_version()
{
	return 14; /* update this version number as needed */
}

/* to add new computation modules, 
	specify an extern module entry,
	and add it to 'module_table'
*/

extern module_entry_info 
/* extern declarations of modules for linking */
	cm_entry_utilityrate,
	cm_entry_cashloan,
	cm_entry_pvwatts,
	cm_entry_stdhrlywf,
	cm_entry_timeseq,
	cm_entry_easywatts,
	cm_entry_windwatts,
	cm_entry_levpartflip;

/* official module table */
static module_entry_info *module_table[] = {
	&cm_entry_utilityrate,
	&cm_entry_cashloan,
	&cm_entry_stdhrlywf,
	&cm_entry_pvwatts,
	&cm_entry_timeseq,
	&cm_entry_easywatts,
	&cm_entry_windwatts,
	&cm_entry_levpartflip,
	NULL };

SSCEXPORT ssc_module_t ssc_module_create( const char *name )
{
	std::string lname = util::lower_case( name );

	int i=0;
	while ( module_table[i] != NULL
		 && module_table[i]->f_create != NULL )
	{
		if ( lname == util::lower_case( module_table[i]->name ) )
			return (*(module_table[i]->f_create))();
		i++;
	}

	return NULL;
}

SSCEXPORT void ssc_module_free( ssc_module_t p_mod )
{
	compute_module *cm = static_cast<compute_module*>(p_mod);
	if (cm) delete cm;
}

/*************************** data object manipulation ***************************/

SSCEXPORT ssc_data_t ssc_data_create()
{
	return static_cast<ssc_data_t>( new var_table );
}

SSCEXPORT void ssc_data_free( ssc_data_t p_data )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (vt) delete vt;
}

SSCEXPORT void ssc_data_clear( ssc_data_t p_data )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (vt) vt->clear();
}

SSCEXPORT void ssc_data_unassign( ssc_data_t p_data, const char *name )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return;
	vt->unassign( name );
}

SSCEXPORT int ssc_data_query( ssc_data_t p_data, const char *name )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return SSC_INVALID;
	var_data *dat = vt->lookup(name);
	if (!dat) return SSC_INVALID;
	else return dat->type;
}

SSCEXPORT const char *ssc_data_first( ssc_data_t p_data ) // returns the name of the first data item, NULL if empty
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return NULL;
	return vt->first();
}

SSCEXPORT const char *ssc_data_next( ssc_data_t p_data ) // returns the next name in the data set object, NULL, if none left.
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return NULL;
	return vt->next();
}

SSCEXPORT void ssc_data_set_string( ssc_data_t p_data, const char *name, const char *value )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return;
	vt->assign( name, var_data( std::string(value) ) );
}

SSCEXPORT void ssc_data_set_number( ssc_data_t p_data, const char *name, ssc_number_t value )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return;
	vt->assign( name, var_data( value ) );
}

SSCEXPORT void ssc_data_set_array( ssc_data_t p_data, const char *name, ssc_number_t *pvalues, int length )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return;
	vt->assign( name, var_data( pvalues, length ) );
}

SSCEXPORT void ssc_data_set_matrix( ssc_data_t p_data, const char *name, ssc_number_t *pvalues, int nrows, int ncols )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return;
	vt->assign( name, var_data(pvalues, nrows, ncols) );
}

SSCEXPORT const char *ssc_data_get_string( ssc_data_t p_data, const char *name )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return NULL;
	var_data *dat = vt->lookup(name);
	if (!dat || dat->type != SSC_STRING) return NULL;
	return dat->str.c_str();	
}

SSCEXPORT ssc_bool_t ssc_data_get_number( ssc_data_t p_data, const char *name, ssc_number_t *value )
{
	if (!value) return 0;
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return 0;
	var_data *dat = vt->lookup(name);
	if (!dat || dat->type != SSC_NUMBER) return 0;
	*value = dat->num;
	return 1;	
}

SSCEXPORT const ssc_number_t *ssc_data_get_array(ssc_data_t p_data,  const char *name, int *length )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return NULL;
	var_data *dat = vt->lookup(name);
	if (!dat || dat->type != SSC_ARRAY) return NULL;
	if (length) *length = (int) dat->num.length();
	return dat->num.data();
}

SSCEXPORT const ssc_number_t *ssc_data_get_matrix( ssc_data_t p_data, const char *name, int *nrows, int *ncols )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return NULL;
	var_data *dat = vt->lookup(name);
	if (!dat || dat->type != SSC_MATRIX) return NULL;
	if (nrows) *nrows = (int) dat->num.nrows();
	if (ncols) *ncols = (int) dat->num.ncols();
	return dat->num.data();
}

SSCEXPORT ssc_entry_t ssc_module_entry( int index )
{
	int max=0;
	while( module_table[max++] != NULL );

	if (index >= 0 && index < max) return static_cast<ssc_entry_t>(module_table[index]);
	else return NULL;
}

SSCEXPORT const char *ssc_entry_name( ssc_entry_t p_entry )
{
	module_entry_info *p = static_cast<module_entry_info*>(p_entry);
	return p ? p->name : NULL;
}

SSCEXPORT const char *ssc_entry_description( ssc_entry_t p_entry )
{
	module_entry_info *p = static_cast<module_entry_info*>(p_entry);
	return p ? p->description : NULL;
}

SSCEXPORT int ssc_entry_version( ssc_entry_t p_entry )
{
	module_entry_info *p = static_cast<module_entry_info*>(p_entry);
	return p ? p->version : 0;
}


SSCEXPORT const ssc_info_t ssc_module_var_info( ssc_module_t p_mod, int index )
{
	compute_module *cm = static_cast<compute_module*>(p_mod);
	if (!cm) return NULL;
	return static_cast<ssc_info_t>( cm->info( index ) );
}

SSCEXPORT int ssc_info_var_type( ssc_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi ? vi->var_type : SSC_INVALID;
}

SSCEXPORT int ssc_info_data_type( ssc_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi ? vi->data_type : SSC_INVALID;
}

SSCEXPORT const char *ssc_info_name( ssc_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi ? vi->name : NULL;
}

SSCEXPORT const char *ssc_info_label( ssc_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi ? vi->label : NULL;
}

SSCEXPORT const char *ssc_info_units( ssc_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi ? vi->units : NULL;
}

SSCEXPORT const char *ssc_info_meta( ssc_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi ? vi->meta : NULL;
}

SSCEXPORT const char *ssc_info_group( ssc_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi ? vi->group : NULL;
}

SSCEXPORT const char *ssc_info_uihint( ssc_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi ? vi->ui_hint : NULL;
}

class default_sync_proc : public util::sync_piped_process
{
private:
	ssc_handler_t m_handler;
public:
	default_sync_proc( ssc_handler_t ph ) : m_handler(ph) {  }

	virtual void on_stdout(const std::string &line_text)
	{
		ssc_module_extproc_output( m_handler, line_text.c_str() );
	}
};

static ssc_bool_t default_internal_handler( ssc_module_t p_mod, ssc_handler_t p_handler,
	int action_type, float f0, float f1, 
	const char *s0, const char *s1,
	void *p_data )
{
	if (action_type == SSC_LOG)
	{
		// print log message to console
		std::cout << "Log ";
		switch( (int)f0 )
		{
		case SSC_NOTICE: std::cout << "Notice: " << s0 << " time " << f1 << std::endl; break;
		case SSC_WARNING: std::cout << "Warning: " << s0 << " time " << f1 << std::endl; break;
		case SSC_ERROR: std::cout << "Error: " << s0 << " time " << f1 << std::endl; break;
		default: std::cout << "Log notice uninterpretable: " << f0 << " time " << f1 << std::endl; break;
		}
		return 1;
	}
	else if (action_type == SSC_UPDATE)
	{
		// print status update to console
		std::cout << "Progress " << f0 << "%:" << s1 << " time " << f1 << std::endl;
		return 1; // return 0 to abort simulation as needed.
	}
	else if (action_type == SSC_EXECUTE)
	{
		// run the executable, pipe the output, and return output to p_mod
		// **TODO**
		default_sync_proc exe( p_handler );
		return exe.spawn( s0, s1 ) == 0;
	}
	else
		return 0;
}

SSCEXPORT ssc_bool_t ssc_module_exec_simple( const char *name, ssc_data_t p_data )
{
	ssc_module_t p_mod = ssc_module_create( name );
	if ( !p_mod ) return 0;
	
	ssc_bool_t result = ssc_module_exec( p_mod, p_data );

	ssc_module_free( p_mod );
	return result;
}

SSCEXPORT const char *ssc_module_exec_simple_nothread( const char *name, ssc_data_t p_data )
{
static char p_internal_buf[256];

	ssc_module_t p_mod = ssc_module_create( name );
	if (!p_mod) return 0;

	ssc_bool_t result = ssc_module_exec( p_mod, p_data );

	// copy over first error if there was one to internal buffer
	if (!result)
	{
		strcpy(p_internal_buf, "general error detected");

		const char *text;
		int type;
		int i=0;
		while( (text = ssc_module_log( p_mod, i, &type, NULL )) )
		{
			if (type == SSC_ERROR)
			{
				strncpy( p_internal_buf, text, 255 );
				break;
			}
			i++;
		}
	}

	ssc_module_free( p_mod );
	return result ? NULL : p_internal_buf;
}


SSCEXPORT ssc_bool_t ssc_module_exec( ssc_module_t p_mod, ssc_data_t p_data )
{
	return ssc_module_exec_with_handler( p_mod, p_data, default_internal_handler, NULL );
}

class default_exec_handler : public handler_interface
{
private:
	ssc_bool_t (*m_hfunc)( ssc_module_t, ssc_handler_t, int, float, float, const char *, const char *, void * );
	void *m_hdata;

public:
	default_exec_handler(
		compute_module *cm,
		ssc_bool_t (*f)( ssc_module_t, ssc_handler_t, int, float, float, const char *, const char *, void * ),
		void *d )
		: handler_interface(cm)
	{
		m_hfunc = f;
		m_hdata = d;
	}


	virtual void on_log( const std::string &text, int type, float time )
	{
		if (!m_hfunc) return;
		(*m_hfunc)( static_cast<ssc_module_t>( module() ), 
					static_cast<ssc_handler_t>( static_cast<handler_interface*>(this) ), 
					SSC_LOG, (float)type, time, text.c_str(), NULL, m_hdata );
	}

	virtual bool on_update( const std::string &text, float percent, float time )
	{
		if (!m_hfunc) return true;
		
		return (*m_hfunc)( static_cast<ssc_module_t>( module() ),
					static_cast<ssc_handler_t>( static_cast<handler_interface*>(this) ), 
					SSC_UPDATE, percent, time, text.c_str(), NULL, m_hdata ) ? 1 : 0;
	}

	virtual bool on_exec( const std::string &command, const std::string &workdir )
	{
		if (!m_hfunc) return false;

		return  (*m_hfunc)( static_cast<ssc_module_t>( module() ), 
							static_cast<ssc_handler_t>( static_cast<handler_interface*>(this) ),
							SSC_EXECUTE, 
							0, 0, 
							command.c_str(), workdir.c_str(), 
							m_hdata ) ? true : false;

	}

};

SSCEXPORT ssc_bool_t ssc_module_exec_with_handler( 
	ssc_module_t p_mod, 
	ssc_data_t p_data, 
	ssc_bool_t (*pf_handler)( ssc_module_t, ssc_handler_t, int, float, float, const char*, const char *, void * ),
	void *pf_user_data )
{
	compute_module *cm = static_cast<compute_module*>(p_mod);
	if (!cm) return 0;

	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt)
	{
		cm->log("invalid data object provided", SSC_ERROR);
		return 0;
	}

	if (pf_handler == NULL)
	{
		pf_handler = default_internal_handler;
		pf_user_data = NULL;
	}

	default_exec_handler h( cm, pf_handler, pf_user_data );
	return cm->compute( &h, vt ) ? true : false;
}


SSCEXPORT void ssc_module_extproc_output( ssc_handler_t p_handler, const char *output_line )
{
	handler_interface *hi = static_cast<handler_interface*>( p_handler );
	if (hi)	hi->on_stdout( output_line );
}

SSCEXPORT ssc_param_t ssc_module_parameter( ssc_module_t p_mod, int index )
{
	compute_module *cm = static_cast<compute_module*>(p_mod);
	if (!cm) return NULL;
	
	param_info *p = cm->get_param_info( index );
	return static_cast<ssc_param_t>(p);
}


SSCEXPORT const char *ssc_param_name( ssc_param_t p_param )
{
	param_info *p = static_cast<param_info*>(p_param);
	return p ? p->name : NULL;
}

SSCEXPORT const char *ssc_param_description( ssc_param_t p_param )
{
	param_info *p = static_cast<param_info*>(p_param);
	return p ? p->description : NULL;
}

SSCEXPORT const char *ssc_param_default_value( ssc_param_t p_param )
{
	param_info *p = static_cast<param_info*>(p_param);
	return p ? p->default_value : NULL;
}

SSCEXPORT int ssc_param_type( ssc_param_t p_param )
{
	param_info *p = static_cast<param_info*>(p_param);
	return p ? p->type : NULL;
}


SSCEXPORT void ssc_module_parameter_string( ssc_module_t p_mod, const char *name, const char *value )
{
	compute_module *cm = static_cast<compute_module*>(p_mod);
	if (!cm) return;
	cm->set_param( name, value );
}

SSCEXPORT void ssc_module_parameter_number( ssc_module_t p_mod, const char *name, ssc_number_t value )
{
	compute_module *cm = static_cast<compute_module*>(p_mod);
	if (!cm) return;
	cm->set_param( name, value );
}

SSCEXPORT const char *ssc_module_log( ssc_module_t p_mod, int index, int *item_type, float *time )
{
	compute_module *cm = static_cast<compute_module*>(p_mod);
	if (!p_mod) return NULL;

	compute_module::log_item *l = cm->log(index);
	if (!l) return NULL;

	if (item_type) *item_type = l->type;
	if (time) *time = l->time;

	return l->text.c_str();
}

SSCEXPORT void __ssc_segfault()
{
	std::string *pstr = NULL;
	std::string mystr = *pstr;
}
