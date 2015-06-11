#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <php.h>

#include "../../ssc/sscapi.h"

#define PHP_SSC_VERSION "1.0"
#define PHP_SSC_EXTNAME "sscphp"
#define phpext_ssc_ptr &sscphp_module_entry


PHP_FUNCTION( sscphp_version ) {
		RETURN_LONG( ssc_version() );
}

PHP_FUNCTION( sscphp_test ) {
		php_printf("test output from sscphp!");
}

static zend_function_entry sscphp_functions[] = {
	PHP_FE( sscphp_version, NULL )
	PHP_FE( sscphp_test, NULL )
	{ NULL, NULL, NULL }
};

zend_module_entry sscphp_module_entry = {
		STANDARD_MODULE_HEADER,
		PHP_SSC_EXTNAME,
		sscphp_functions,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		PHP_SSC_VERSION,
		STANDARD_MODULE_PROPERTIES
};

// install module
ZEND_GET_MODULE(sscphp)

