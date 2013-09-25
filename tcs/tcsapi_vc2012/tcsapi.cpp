#include <cstring>

#include "core.h"
#include "tcsapi.h"

TCSEXPORT int tcsapi_version()
{
	return 1;
}

TCSEXPORT const char *tcs_build_info()
{
	static const char *_bi = __PLATFORM__ " " __ARCH__ " " __COMPILER__ " " __DATE__ " " __TIME__;
	return _bi;
}

/* to add new computation modules, 
	specify an extern module entry,
	and add it to 'module_table'
*/

int a1,a2,a3;
int * aIntPt[] = { &a1, &a2 };

/* extern declarations of modules for linking */
extern module_entry_info cm_entry_test_mod,
	cm_entry_windpower;

/* official module table */
static module_entry_info *module_table[] = {
	&cm_entry_windpower,
	&cm_entry_test_mod,
	0 };

TCSEXPORT tcs_module_t tcs_module_create( const char *name )
{
	std::string lname = util::lower_case( name );

	int i=0;
	while ( module_table[i] != 0
		 && module_table[i]->f_create != 0 )
	{
		if ( lname == util::lower_case( module_table[i]->name ) )
			return (*(module_table[i]->f_create))(); // calls the cm_xxxxxxx function (which is at the top of each cmod_xxx.cpp file)
		i++;
	}

	return 0;
}

TCSEXPORT void tcs_module_free( tcs_module_t p_mod )
{
	compute_module *cm = static_cast<compute_module*>(p_mod);
	if (cm) delete cm;
}

/*************************** data object manipulation ***************************/

TCSEXPORT tcs_data_t tcs_data_create()
{
	return static_cast<tcs_data_t>( new var_table );
}

TCSEXPORT void tcs_data_free( tcs_data_t p_data )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (vt) delete vt;
}

TCSEXPORT void tcs_data_clear( tcs_data_t p_data )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (vt) vt->clear();
}

TCSEXPORT void tcs_data_unassign( tcs_data_t p_data, const char *name )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return;
	vt->unassign( name );
}

TCSEXPORT int tcs_data_query( tcs_data_t p_data, const char *name )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return TCS_INVALID;
	var_data *dat = vt->lookup(name);
	if (!dat) return TCS_INVALID;
	else return dat->type;
}

TCSEXPORT const char *tcs_data_first( tcs_data_t p_data ) // returns the name of the first data item, 0 if empty
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return 0;
	return vt->first();
}

TCSEXPORT const char *tcs_data_next( tcs_data_t p_data ) // returns the next name in the data set object, 0, if none left.
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return 0;
	return vt->next();
}

TCSEXPORT void tcs_data_set_string( tcs_data_t p_data, const char *name, const char *value )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return;
	vt->assign( name, var_data( std::string(value) ) );
}

TCSEXPORT void tcs_data_set_number( tcs_data_t p_data, const char *name, tcs_number_t value )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return;
	vt->assign( name, var_data( value ) );
}

TCSEXPORT void tcs_data_set_array( tcs_data_t p_data, const char *name, tcs_number_t *pvalues, int length )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return;
	vt->assign( name, var_data( pvalues, length ) );
}

TCSEXPORT void tcs_data_set_matrix( tcs_data_t p_data, const char *name, tcs_number_t *pvalues, int nrows, int ncols )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return;
	vt->assign( name, var_data(pvalues, nrows, ncols) );
}

//TCSEXPORT void tcs_data_set_table( tcs_data_t p_data, const char *name, tcs_data_t table )
//{
//	var_table *vt = static_cast<var_table*>(p_data);
//	var_table *value = static_cast<var_table*>(table);
//	if (!vt || !value) return;
//	var_data *dat = vt->assign( name, var_data() );
//	dat->type = TCS_TABLE;
//	dat->table = *value;  // invokes operator= for deep copy
//}

TCSEXPORT const char *tcs_data_get_string( tcs_data_t p_data, const char *name )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return 0;
	var_data *dat = vt->lookup(name);
	if (!dat || dat->type != TCS_STRING) return 0;
	return dat->str.c_str();	
}

TCSEXPORT tcs_bool_t tcs_data_get_number( tcs_data_t p_data, const char *name, tcs_number_t *value )
{
	if (!value) return 0;
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return 0;
	var_data *dat = vt->lookup(name);
	if (!dat || dat->type != TCS_NUMBER) return 0;
	*value = dat->num;
	return 1;	
}

TCSEXPORT tcs_number_t *tcs_data_get_array(tcs_data_t p_data,  const char *name, int *length )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return 0;
	var_data *dat = vt->lookup(name);
	if (!dat || dat->type != TCS_ARRAY) return 0;
	if (length) *length = (int) dat->num.length();
	return dat->num.data();
}

TCSEXPORT tcs_number_t *tcs_data_get_matrix( tcs_data_t p_data, const char *name, int *nrows, int *ncols )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return 0;
	var_data *dat = vt->lookup(name);
	if (!dat || dat->type != TCS_MATRIX) return 0;
	if (nrows) *nrows = (int) dat->num.nrows();
	if (ncols) *ncols = (int) dat->num.ncols();
	return dat->num.data();
}

//TCSEXPORT tcs_data_t tcs_data_get_table( tcs_data_t p_data, const char *name )
//{
//	var_table *vt = static_cast<var_table*>(p_data);
//	if (!vt) return 0;
//	var_data *dat = vt->lookup(name);
//	if (!dat || dat->type != TCS_TABLE) return 0;
//	return static_cast<tcs_data_t>( &(dat->table) );
//}

TCSEXPORT tcs_entry_t tcs_module_entry( int index )
{
	int max=0;
	while( module_table[max++] != 0 );

	if (index >= 0 && index < max) return static_cast<tcs_entry_t>(module_table[index]);
	else return 0;
}

TCSEXPORT const char *tcs_entry_name( tcs_entry_t p_entry )
{
	module_entry_info *p = static_cast<module_entry_info*>(p_entry);
	return p ? p->name : 0;
}

TCSEXPORT const char *tcs_entry_description( tcs_entry_t p_entry )
{
	module_entry_info *p = static_cast<module_entry_info*>(p_entry);
	return p ? p->description : 0;
}

TCSEXPORT int tcs_entry_version( tcs_entry_t p_entry )
{
	module_entry_info *p = static_cast<module_entry_info*>(p_entry);
	return p ? p->version : 0;
}


TCSEXPORT const tcs_info_t tcs_module_var_info( tcs_module_t p_mod, int index )
{
	compute_module *cm = static_cast<compute_module*>(p_mod);
	if (!cm) return 0;
	return static_cast<tcs_info_t>( cm->info( index ) );
}

TCSEXPORT int tcs_info_var_type( tcs_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi ? vi->var_type : TCS_INVALID;
}

TCSEXPORT int tcs_info_data_type( tcs_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi ? vi->data_type : TCS_INVALID;
}

TCSEXPORT const char *tcs_info_name( tcs_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi ? vi->name : 0;
}

TCSEXPORT const char *tcs_info_label( tcs_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi ? vi->label : 0;
}

TCSEXPORT const char *tcs_info_units( tcs_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi ? vi->units : 0;
}

TCSEXPORT const char *tcs_info_meta( tcs_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi ? vi->meta : 0;
}

TCSEXPORT const char *tcs_info_required( tcs_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi? vi->required_if : 0;
}

TCSEXPORT const char *tcs_info_group( tcs_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi ? vi->group : 0;
}

TCSEXPORT const char *tcs_info_constraints( tcs_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi ? vi->constraints : 0;
}

TCSEXPORT const char *tcs_info_uihint( tcs_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi ? vi->ui_hint : 0;
}

/*
class default_sync_proc : public util::sync_piped_process
{
private:
	tcs_handler_t m_handler;
public:
	default_sync_proc( tcs_handler_t ph ) : m_handler(ph) {  }

	virtual void on_stdout(const std::string &line_text)
	{
		tcs_module_extproc_output( m_handler, line_text.c_str() );
	}
};
*/

static tcs_bool_t default_internal_handler( tcs_module_t p_mod, tcs_handler_t p_handler,
	int action_type, float f0, float f1, 
	const char *s0, const char *s1,
	void *p_data )
{
	if (action_type == TCS_LOG)
	{
		// print log message to console
		std::cout << "Log ";
		switch( (int)f0 )
		{
		case TCS_NOTICE: std::cout << "Notice: " << s0 << " time " << f1 << std::endl; break;
		case TCS_WARNING: std::cout << "Warning: " << s0 << " time " << f1 << std::endl; break;
		case TCS_ERROR: std::cout << "Error: " << s0 << " time " << f1 << std::endl; break;
		default: std::cout << "Log notice uninterpretable: " << f0 << " time " << f1 << std::endl; break;
		}
		return 1;
	}
	else if (action_type == TCS_UPDATE)
	{
		// print status update to console
		std::cout << "Progress " << f0 << "%:" << s1 << " time " << f1 << std::endl;
		return 1; // return 0 to abort simulation as needed.
	}
/*
	else if (action_type == TCS_EXECUTE)
	{
		// run the executable, pipe the output, and return output to p_mod
		// **TODO**
		default_sync_proc exe( p_handler );
		return exe.spawn( s0, s1 ) == 0;
	}
*/
	else
		return 0;
}

TCSEXPORT tcs_bool_t tcs_module_exec_simple( const char *name, tcs_data_t p_data )
{
	tcs_module_t p_mod = tcs_module_create( name );
	if ( !p_mod ) return 0;
	
	tcs_bool_t result = tcs_module_exec( p_mod, p_data );

	tcs_module_free( p_mod );
	return result;
}

TCSEXPORT const char *tcs_module_exec_simple_nothread( const char *name, tcs_data_t p_data )
{
static char p_internal_buf[256];

	tcs_module_t p_mod = tcs_module_create( name );
	if (!p_mod) return 0;

	tcs_bool_t result = tcs_module_exec( p_mod, p_data );

	// copy over first error if there was one to internal buffer
	if (!result)
	{
		strcpy(p_internal_buf, "general error detected");

		const char *text;
		int type;
		int i=0;
		while( (text = tcs_module_log( p_mod, i, &type, 0 )) )
		{
			if (type == TCS_ERROR)
			{
				strncpy( p_internal_buf, text, 255 );
				break;
			}
			i++;
		}
	}

	tcs_module_free( p_mod );
	return result ? 0 : p_internal_buf;
}


TCSEXPORT tcs_bool_t tcs_module_exec( tcs_module_t p_mod, tcs_data_t p_data )
{
	return tcs_module_exec_with_handler( p_mod, p_data, default_internal_handler, 0 );
}

class default_exec_handler : public handler_interface
{
private:
	tcs_bool_t (*m_hfunc)( tcs_module_t, tcs_handler_t, int, float, float, const char *, const char *, void * );
	void *m_hdata;

public:
	default_exec_handler(
		compute_module *cm,
		tcs_bool_t (*f)( tcs_module_t, tcs_handler_t, int, float, float, const char *, const char *, void * ),
		void *d )
		: handler_interface(cm)
	{
		m_hfunc = f;
		m_hdata = d;
	}


	virtual void on_log( const std::string &text, int type, float time )
	{
		if (!m_hfunc) return;
		(*m_hfunc)( static_cast<tcs_module_t>( module() ), 
					static_cast<tcs_handler_t>( static_cast<handler_interface*>(this) ), 
					TCS_LOG, (float)type, time, text.c_str(), 0, m_hdata );
	}

	virtual bool on_update( const std::string &text, float percent, float time )
	{
		if (!m_hfunc) return true;
		
		return (*m_hfunc)( static_cast<tcs_module_t>( module() ),
					static_cast<tcs_handler_t>( static_cast<handler_interface*>(this) ), 
					TCS_UPDATE, percent, time, text.c_str(), 0, m_hdata ) ? 1 : 0;
	}

/*
	virtual bool on_exec( const std::string &command, const std::string &workdir )
	{
		if (!m_hfunc) return false;

		return  (*m_hfunc)( static_cast<tcs_module_t>( module() ), 
							static_cast<tcs_handler_t>( static_cast<handler_interface*>(this) ),
							TCS_EXECUTE, 
							0, 0, 
							command.c_str(), workdir.c_str(), 
							m_hdata ) ? true : false;

	}
*/
};

TCSEXPORT tcs_bool_t tcs_module_exec_with_handler( 
	tcs_module_t p_mod, 
	tcs_data_t p_data, 
	tcs_bool_t (*pf_handler)( tcs_module_t, tcs_handler_t, int, float, float, const char*, const char *, void * ),
	void *pf_user_data )
{
	compute_module *cm = static_cast<compute_module*>(p_mod);
	if (!cm) return 0;

	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt)
	{
		cm->log("invalid data object provided", TCS_ERROR);
		return 0;
	}

	if (pf_handler == 0)
	{
		pf_handler = default_internal_handler;
		pf_user_data = 0;
	}

	default_exec_handler h( cm, pf_handler, pf_user_data );
	return cm->compute( &h, vt ) ? 1 : 0;
}


TCSEXPORT void tcs_module_extproc_output( tcs_handler_t p_handler, const char *output_line )
{
	handler_interface *hi = static_cast<handler_interface*>( p_handler );
	if (hi)	hi->on_stdout( output_line );
}

TCSEXPORT const char *tcs_module_log( tcs_module_t p_mod, int index, int *item_type, float *time )
{
	compute_module *cm = static_cast<compute_module*>(p_mod);
	if (!p_mod) return 0;

	compute_module::log_item *l = cm->log(index);
	if (!l) return 0;

	if (item_type) *item_type = l->type;
	if (time) *time = l->time;

	return l->text.c_str();
}

TCSEXPORT void __tcs_segfault()
{
	std::string *pstr = 0;
	std::string mystr = *pstr;
}
