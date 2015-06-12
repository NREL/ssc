#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <php.h>

#include "../../ssc/sscapi.h"

#define PHP_SSC_VERSION "1.0"
#define PHP_SSC_EXTNAME "sscphp"

typedef struct {
	ssc_module_t p_mod;
	ssc_data_t p_dat;
	ssc_data_t p_datref;
	ssc_info_t p_inf;
	ssc_entry_t p_ent;
} spobj;
static int spobj_id;
#define spobj_name "sscphpobj"

static void _free_spobj( zend_rsrc_list_entry *rsrc TSRMLS_DC ) {
	spobj *p = (spobj*)rsrc->ptr;

	if ( p->p_mod ) ssc_module_free( p->p_mod );
	if ( p->p_dat ) ssc_data_free( p->p_dat );

	// no need to free p_inf or p_ent p_datref
	// these are just internal ssc references

	efree(p);
};

static spobj *create_obj( ssc_module_t *m, ssc_data_t *d ) {
	spobj *p = emalloc(sizeof(spobj));
	p->p_mod = m;
	p->p_dat = d;
	p->p_datref = 0;
	p->p_inf = 0;
	p->p_ent = 0;
	return p;
}


PHP_MINIT_FUNCTION(sscphp) {
	spobj_id = zend_register_list_destructors_ex( _free_spobj, 
			NULL, spobj_name, module_number );

	return SUCCESS;
}

PHP_MSHUTDOWN_FUNCTION(sscphp) {
	return SUCCESS;
}

PHP_MINFO_FUNCTION(sscphp) {
	char buf[16];
	sprintf(buf, "%d", ssc_version());
	php_info_print_table_start();
	php_info_print_table_header(2, "NREL SAM Simulation Core (SSC) support", "enabled");
	php_info_print_table_row(2, "Version", buf );
	php_info_print_table_row(2, "Build", ssc_build_info() );
	php_info_print_table_end();
}


PHP_FUNCTION( sscphp_version ) {
	RETURN_LONG( ssc_version() );
}

PHP_FUNCTION( sscphp_build_info ) {
	RETURN_STRING( ssc_build_info(), 1 );
}

PHP_FUNCTION( sscphp_data_create ) {
	spobj *p = create_obj( 0, ssc_data_create() );
	ZEND_REGISTER_RESOURCE( return_value, p, spobj_id );
}

PHP_FUNCTION( sscphp_data_free ) {
	spobj *p;
	zval *res;

	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, "r", &res) == FAILURE )
		return;

	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );

	if ( !p ) return;

	if ( p->p_dat )
	{
		ssc_data_free( p->p_dat );
		p->p_dat = 0;
	}

	RETURN_TRUE;
}

PHP_FUNCTION( sscphp_data_clear ) {
	spobj *p;
	zval *res;

	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, "r", &res) == FAILURE )
		return;

	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );
	if ( !p ) return;

	if ( p->p_dat )
		ssc_data_clear( p->p_dat );

	RETURN_TRUE;
}

PHP_FUNCTION( sscphp_data_unassign )
{
	spobj *p;
	zval *res;
	char *name;
	int name_len;

	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, "rs", &res, &name, &name_len ) == FAILURE )
		return;

	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );
	if ( !p ) return;

	if ( p->p_dat )
		ssc_data_unassign( p->p_dat, name );
}

PHP_FUNCTION( sscphp_data_query )
{
	spobj *p;
	zval *res;
	char *name;
	int name_len;

	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, "rs", &res, &name, &name_len ) == FAILURE )
		return;

	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );
	if ( !p ) return;

	if ( p->p_dat )
		RETURN_LONG( ssc_data_query( p->p_dat, name ) );
}

PHP_FUNCTION( sscphp_data_first )
{
	spobj *p;
	zval *res;

	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, "r", &res) == FAILURE )
		return;

	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );
	if ( !p ) return;

	if ( p->p_dat )
	{
		const char *str = ssc_data_first(p->p_dat);
		if ( str )
			RETURN_STRING( str, 1 );
	}
}

PHP_FUNCTION( sscphp_data_next )
{
	spobj *p;
	zval *res;

	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, "r", &res) == FAILURE )
		return;

	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );
	if ( !p ) return;

	if ( p->p_dat )
	{
		const char *str = ssc_data_next( p->p_dat );
		if ( str )
			RETURN_STRING( str, 1 );
	}
}

PHP_FUNCTION( sscphp_data_set_string )
{
	spobj *p;
	zval *res;
	char *name;
	int name_len;
	char *value;
	int value_len;

	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, "rss", &res, &name, &name_len, &value, &value_len ) == FAILURE )
		return;

	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );
	if (!p ) return;

	if( p->p_dat )
		ssc_data_set_string( p->p_dat, name, value );
}

PHP_FUNCTION( sscphp_data_set_number )
{
	spobj *p;
	zval *res;
	char *name;
	int name_len;
	double value;

	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, "rsd", &res, &name, &name_len, &value ) == FAILURE )
		return;

	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );
	if (!p ) return;

	if( p->p_dat )
		ssc_data_set_number( p->p_dat, name, (ssc_number_t)value );
}

static zend_function_entry sscphp_functions[] = {
	PHP_FE( sscphp_version, NULL )
		PHP_FE( sscphp_build_info, NULL )
		PHP_FE( sscphp_data_create, NULL )
		PHP_FE( sscphp_data_free, NULL )
		PHP_FE( sscphp_data_clear, NULL )
		PHP_FE( sscphp_data_query, NULL )
		PHP_FE( sscphp_data_first, NULL )
		PHP_FE( sscphp_data_next, NULL )
		PHP_FE( sscphp_data_set_string, NULL )
		PHP_FE( sscphp_data_set_number, NULL )
		{ NULL, NULL, NULL }
};

zend_module_entry sscphp_module_entry = {
	STANDARD_MODULE_HEADER,
	PHP_SSC_EXTNAME,
	sscphp_functions,
	PHP_MINIT(sscphp),
	PHP_MSHUTDOWN(sscphp),
	NULL,
	NULL,
	PHP_MINFO(sscphp),
	PHP_SSC_VERSION,
	STANDARD_MODULE_PROPERTIES
};

// install module
ZEND_GET_MODULE(sscphp)

