#include <stdio.h>

#include "tcskernel.h"

int main(int argc, char *argv[])
{
	printf("hello, world\n");

	tcskernel k;
	k.add_search_path( "." );
	k.add_search_path( "./types" );

#ifdef _DEBUG
	k.add_search_path( "./Debug" );
#else
	k.add_search_path( "./Release" );
#endif

	k.load_library( "typelib" );

	int u_weather = k.add_unit("weatherreader", "Standard weather file input processor");
	int u_coll = k.add_unit("solarcollector", "Collector");

	printf("itype=%d\n", u_weather);
	printf("itype=%d\n", u_coll);
	
	k.connect( u_weather, "beam", u_coll, "incident" );

	fgetc(stdin);

	k.unload_libraries();

	printf("press enter to quit...\n");
	fgetc(stdin);
	return 0;
}