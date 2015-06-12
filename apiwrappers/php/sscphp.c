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
	int is_dat_const_ref;
	ssc_info_t p_inf;
	ssc_entry_t p_ent;
} spobj;
static int spobj_id;
#define spobj_name "sscphpobj"

static void _free_spobj( zend_rsrc_list_entry *rsrc TSRMLS_DC ) {
	spobj *p = (spobj*)rsrc->ptr;

	if ( p->p_mod ) ssc_module_free( p->p_mod );
	if ( p->p_dat && !p->is_dat_const_ref ) ssc_data_free( p->p_dat );

	// no need to free p_inf or p_ent 
	// these are just internal ssc references

	efree(p);
};

static spobj *create_ref( ssc_module_t *m, ssc_data_t *d ) {
	spobj *p = emalloc(sizeof(spobj));
	p->p_mod = m;
	p->p_dat = d;
	p->is_dat_const_ref = 0;
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
	spobj *p = create_ref( 0, ssc_data_create() );
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

PHP_FUNCTION( sscphp_data_set_array )
{
	spobj *p;
	zval *res;
	char *name;
	int name_len;
	long i, len;
	zval *arr;
	zval **data;
	HashTable *hash;
	HashPosition pointer;

	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, "rsa", &res, &name, &name_len, &arr ) == FAILURE )
		return;
	
	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );
	if (!p || !p->p_dat) return;

	hash = Z_ARRVAL_P(arr);
	len = zend_hash_num_elements( hash );
	if (len < 1 ) return;

	ssc_number_t *vec = (ssc_number_t*) malloc( sizeof(ssc_number_t)*len );
	if ( !vec ) return;


	i=0;
	for( zend_hash_internal_pointer_reset_ex( hash, &pointer );
			zend_hash_get_current_data_ex(hash, (void**) &data, &pointer ) == SUCCESS;
			zend_hash_move_forward_ex(hash, &pointer ) )
	{

		if ( i >= len ) break;

		vec[i] = 0.0f;

		if ( Z_TYPE_PP(data) == IS_DOUBLE )
			vec[i] = (ssc_number_t)Z_DVAL_PP(data);
		else if ( Z_TYPE_PP(data) == IS_LONG )
			vec[i] = (ssc_number_t)Z_LVAL_PP(data);

		i++;
	}

	ssc_data_set_array( p->p_dat, name, vec, len );

	free( vec );
}

PHP_FUNCTION( sscphp_data_set_matrix )
{
	spobj *p;
	zval *res;
	char *name;
	int name_len;
	long i, j, nr, nc, nlen, index;
	zval *mat;
	zval **data;
	HashTable *hash, *row;
	HashPosition pointer1, pointer2;
	ssc_number_t *vec;

	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, "rsa", &res, &name, &name_len, &mat ) == FAILURE )
		return;
	
	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );
	if (!p || !p->p_dat) return;

	hash = Z_ARRVAL_P(mat);
	nr = zend_hash_num_elements( hash );
	if ( nr < 1 ) return;

	zend_hash_internal_pointer_reset_ex( hash, &pointer1 );
  	if ( zend_hash_get_current_data_ex(hash, (void**) &data, &pointer1 ) != SUCCESS 
	 || Z_TYPE_PP(data) != IS_ARRAY )
	 	return;

	row = Z_ARRVAL_PP(data);
	nc = zend_hash_num_elements( row );
	if ( nc < 1 ) return;

	nlen = nr * nc;

	vec = (ssc_number_t*)malloc( sizeof(ssc_number_t)*nlen );
	if ( !vec ) return;

	i = 0;
	for( zend_hash_internal_pointer_reset_ex( hash, &pointer1 );
			zend_hash_get_current_data_ex(hash, (void**) &data, &pointer1 ) == SUCCESS;
			zend_hash_move_forward_ex(hash, &pointer1) )
	{

		if ( i >= nr ) break;

		if ( Z_TYPE_PP(data) != IS_ARRAY ) continue;

		row = Z_ARRVAL_PP(data);

		j = 0;
		for( zend_hash_internal_pointer_reset_ex( row, &pointer2 );
			zend_hash_get_current_data_ex(row, (void**)&data, &pointer2 ) == SUCCESS;
			zend_hash_move_forward_ex(row, &pointer2 ) )
		{
			if ( j >= nc ) break;

			index = i*nc+j;

			if ( Z_TYPE_PP(data) == IS_DOUBLE )
				vec[ index ] = (ssc_number_t)Z_DVAL_PP(data);
			else if ( Z_TYPE_PP(data) == IS_LONG )
				vec[ index ] = (ssc_number_t)Z_LVAL_PP(data);

			j++;
		}

		i++;
	}


	ssc_data_set_matrix( p->p_dat, name, vec, nr, nc );
	free( vec );
}

PHP_FUNCTION( sscphp_data_set_table )
{
	spobj *p;
	zval *res;
	char *name;
	int name_len;
	zval *tab;
	spobj *t;

	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, "rsr", &res, &name, &name_len, &tab ) == FAILURE )
		return;

	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );
	if (!p || !p->p_dat) return;

	ZEND_FETCH_RESOURCE( t, spobj*, &tab, -1, spobj_name, spobj_id );
	if (!t || !t->p_dat) return;

	ssc_data_set_table( p->p_dat, name, t->p_dat );
}

PHP_FUNCTION( sscphp_data_get_string )
{
	spobj *p;
	zval *res;
	char *name;
	int name_len;
	const char *value;

	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, "rs", &res, &name, &name_len ) == FAILURE )
		return;

	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );
	if (!p || !p->p_dat) return;

	value = ssc_data_get_string( p->p_dat, name );
	RETURN_STRING( value ? value : "" , 1 )
}

PHP_FUNCTION( sscphp_data_get_number )
{
	spobj *p;
	zval *res;
	char *name;
	int name_len;
	ssc_number_t value;

	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, "rs", &res, &name, &name_len ) == FAILURE )
		return;

	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );
	if (!p || !p->p_dat) return;


	if ( ssc_data_get_number( p->p_dat, name, &value ) )
		RETURN_DOUBLE( (double)value );
}

PHP_FUNCTION( sscphp_data_get_array )
{
	spobj *p;
	zval *res;
	char *name;
	int name_len;
	ssc_number_t *arr;
	int i, len;

	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, "rs", &res, &name, &name_len ) == FAILURE )
		return;

	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );
	if (!p || !p->p_dat) return;

	arr = ssc_data_get_array( p->p_dat, name, &len );
	if ( !arr || len < 1 ) return;

	array_init( return_value );
	for( i=0;i<len;i++)
		add_index_double( return_value, i, (double)arr[i] );
}

PHP_FUNCTION( sscphp_data_get_matrix )
{
	spobj *p;
	zval *res;
	char *name;
	int name_len;
	ssc_number_t *mat;
	int i,j, nr, nc;

	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, "rs", &res, &name, &name_len ) == FAILURE )
		return;

	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );
	if ( !p || !p->p_dat ) return;

	mat = ssc_data_get_matrix( p->p_dat, name, &nr, &nc );
	if ( !mat || nr < 1 || nc < 1 ) return;

	array_init( return_value );
	for( i=0;i<nr;i++ )
	{
		zval *row;
		MAKE_STD_ZVAL( row );
		array_init( row );
		for( j=0;j<nc;j++ )
		{
			int index = i*nc+j;
			add_index_double( row, j, (double)mat[index] );
		}
		add_index_zval( return_value, i, row );
	}
}

PHP_FUNCTION( sscphp_data_get_table )
{
	spobj *p;
	zval *res;
	char *name;
	int name_len;
	ssc_data_t table;

	if ( zend_parse_parameters( ZEND_NUM_ARGS() TSRMLS_CC, "rs", &res, &name, &name_len ) == FAILURE )
		return;

	ZEND_FETCH_RESOURCE( p, spobj*, &res, -1, spobj_name, spobj_id );
	if ( !p || !p->p_dat ) return;

	table = ssc_data_get_table( p->p_dat, name );
	if ( !table ) return;
	
	spobj *t = create_ref( 0, 0);
	t->p_dat = table;
	t->is_dat_const_ref = 1; // ssc_data_get_table returns an internal reference
	ZEND_REGISTER_RESOURCE( return_value, t, spobj_id );
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
		PHP_FE( sscphp_data_set_array, NULL )
		PHP_FE( sscphp_data_set_matrix, NULL )
		PHP_FE( sscphp_data_set_table, NULL )
		PHP_FE( sscphp_data_get_string, NULL )
		PHP_FE( sscphp_data_get_number, NULL )
		PHP_FE( sscphp_data_get_array, NULL )
		PHP_FE( sscphp_data_get_matrix, NULL )
		PHP_FE( sscphp_data_get_table, NULL )
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

