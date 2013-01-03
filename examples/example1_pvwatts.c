#include <stdio.h>
#include "sscapi.h"

int main(int argc, char *argv[])
{
	if ( argc < 2 )
	{
		printf("usage: pvwatts.exe <weather-file>\n");
		return -1;
	}
 
	// run PVWatts simulation for the specified weather file
	ssc_data_t data = ssc_data_create();
	if ( data == NULL )
	{
		printf("error: out of memory.\n");
		return -1;
	}
	
	ssc_data_set_string( data, "file_name", argv[1] ); // set the weather file name
	ssc_data_set_number( data, "system_size", 1.0f );  // system size of 1 kW DC
	ssc_data_set_number( data, "derate", 0.77f );      // system derate
	ssc_data_set_number( data, "track_mode", 0 );      // fixed tilt system
	ssc_data_set_number( data, "tilt", 20 );           // 20 degree tilt
	ssc_data_set_number( data, "azimuth", 180 );       // south facing (180 degrees)
	
	ssc_module_t module = ssc_module_create( "pvwattsv1" );
	if ( NULL == module )
	{
		printf("error: could not create 'pvwattsv1' module.\n");
		ssc_data_free( data );
		return -1;
	}

	if ( ssc_module_exec( module, data ) == 0 )
	{
		printf("error during simulation.\n");
		ssc_module_free( module );
		ssc_data_free( data );
		return -1;
	}
	
	double ac_total = 0;
	int len = 0;
	ssc_number_t *ac = ssc_data_get_array( data, "ac", &len );
	if ( ac != NULL )
	{
		int i;
		for ( i=0; i<len; i++ )
			ac_total += ac[i];
		printf("ac: %lg kWh\n", ac_total*0.001 );
	}
	else
	{
		printf("variable 'ac' not found.\n");
	}
	
	ssc_module_free( module );
	ssc_data_free( data );
	
	return 0;
}