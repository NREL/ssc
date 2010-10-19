#include "core.h"

#include <cstdio>

#include "sscapi.h"

int main(int argc, char* argv[])
{

	std::cout << "System Simulator Core (SSC) Version " << ssc_version() << std::endl;
	std::cout << "Platform: " << __PLATFORM__ << std::endl;
	std::cout << "Arch: " << __ARCH__ << std::endl;
	std::cout << "Compiler: " << __COMPILER__ << std::endl;
	std::cout << "Build: " << __BUILD__ << std::endl << std::endl;
	std::cout << "sizeof(va_list): " << sizeof(va_list) << std::endl;
	std::cout << "sizeof(void*): " << sizeof(void*) << std::endl;
	std::cout << "sizeof(long long): " << sizeof(long long) << std::endl;
	std::cout << "sizeof(double): " << sizeof(double) << std::endl;
	std::cout << "sizeof(float): " << sizeof(float) << std::endl;
	std::cout << "sizeof(int): " << sizeof(int) << std::endl;
	std::cout << "sizeof(std::string): " << sizeof(std::string) << std::endl;
	std::cout << "sizeof(std::wstring): " << sizeof(std::wstring) << std::endl;
	std::cout << "sizeof(std::vector<int>): " << sizeof(std::vector<int>) << std::endl;

	
	void util_test(); // forward
	util_test();

	std::string last_path = util::get_cwd();
	std::cout << "working dir: " << last_path << std::endl;
	util::set_cwd("c:/Program Files");
	std::cout << "working dir: " << util::get_cwd() << std::endl;
	util::set_cwd("c:/Users/adobos/Documents/NREL");
	std::cout << "working dir: " << util::get_cwd() << std::endl;
	util::set_cwd("i:/cmlredist");
	std::cout << "working dir: " << util::get_cwd() << std::endl;
	util::set_cwd( last_path );
	std::cout << "working dir: " << util::get_cwd() << std::endl;

	void test_stdhrlywf(); // forward
	test_stdhrlywf();

	std::cout << std::endl << "Press a key to end..." << std::flush;
	getc(stdin);
	return 0;
}

void util_test()
{
	std::cout 
		<< " sizeof(matrix_t_t<float>)=" << sizeof(util::matrix_t<float>) << std::endl
		<< " sizeof(std::vector<float>)=" << sizeof(std::vector<float>) << std::endl
		<< " sizeof(std::string)=" << sizeof(std::string) << std::endl;
		
	std::vector<std::string> list = util::split("my dog& john| was| the best&creat,|,for,me", "&|", true, true);
	std::cout << "list size: " << list.size() << std::endl;
	for (int i=0;i<(int)list.size();i++)
		std::cout << ">>" << list[i] << "<<" << std::endl;
		
	std::cout << util::join(list, ";") << std::endl;
	

	int d_val = 23;
	double lg_val = -2241223115.0300;
	double lf_val = 1.023;
	double fp2_val = -1254251250.001231251;
	double m_val = 45675123.4451;
		
	std::cout << util::format("My value: %d %% '%s' $%m commafmt='%.2,' F%lf G%lg $  USD\n",
		d_val,
		"STR=",
		m_val,
		lg_val,
		lf_val,
		fp2_val
		);
	
	
	util::matrix_t<float> val;
	val = 12.45f;
		
	std::cout << val << " " << val.length() << std::endl;
	std::cout << "single membytes: " << val.membytes() << std::endl;
	
	val.resize(10);
	for (int i=0;i<10;i++)
		val[i] = i+3.24f;
	
	return;
	
	std::cout << val << " " << val.length() << std::endl;
	for (size_t i=0;i<val.length();i++)
		std::cout << "  > " << val[i] << std::endl;
		
	std::cout << "array membytes: " << val.membytes() << std::endl;
		
	val.resize_fill(3,4,10.9f);
	for (size_t r=0;r<val.nrows();r++)
	{
		val.at(r,0) = r-19.42f;
		for (size_t c=0;c<val.ncols();c++)
		{
			if (c%2==1) val.at(r,c) *= 2.5f+r;
			std::cout << " >> [" << r << "," << c << "]: " << val.at(r,c) << std::endl;
		}
	}
	std::cout << "matrix_t membytes: " << val.membytes() << std::endl;
	
	val.resize(8760);
	std::cout << "float 8760 matrix_t membytes: " << val.membytes() << std::endl;
	util::matrix_t<double> big(8760);
	std::cout << "double 8760 matrix_t membytes: " << big.membytes() << std::endl;
}

void test_stdhrlywf()
{
	ssc_data_t data = ssc_data_create();
	ssc_data_set_string( data, "file_name", "c:/SAM/2010.10.8/exelib/climate_files/CA Daggett.tm2" );
	ssc_data_set_number( data, "header_only", 1);

	ssc_module_t reader = ssc_module_create("stdhrlywf");
	if (!ssc_module_exec( reader, data ))
	{
		int i=0,type;
		float time;
		while (const char *err=ssc_module_log(reader, i++, &type, &time))
		{
			std::cout << err << std::endl;
		}
	}
	else
	{
		ssc_number_t lat, lon, tz, elev;
		ssc_data_get_number(data, "lat", &lat);
		ssc_data_get_number(data, "lon", &lon);
		ssc_data_get_number(data, "tz", &tz);
		ssc_data_get_number(data, "elev", &elev);
		std::cout << "loc: '" << ssc_data_get_string(data, "loc_text") << "' lat: " << lat << " lon: " << lon << " tz: " << tz << " elev: " << elev << std::endl;

		int length = 0;
		const ssc_number_t *dn = ssc_data_get_array(data, "dn", &length);
		if (!dn) std::cout << "no dn specified." <<std::endl;
		else
		{
			for (int i=0;i<24 && i<length;i++)
				std::cout << "\t dn," << i << ": " << dn[i] << std::endl;
		}
	}
	
	ssc_module_free( reader );
	ssc_data_free( data );
}

/*

void test_sim()
{
	// create data context
	ssc_data_t p_data = ssc_data_create();

	ssc_data_set_number( p_data, "pvwatts.dcrate", 5.3 );
	ssc_data_set_string( p_data, "pvwatts.name", "hello" );
	ssc_data_set_array( p_data, "pvwatts.monthly_soiling", soil_array, 12 );
	ssc_data_set_matrix( p_data, "pvwatts.azel_shade", azel_matrix, 24, 12 );

	
	itype = ssc_data_get_type( p_data, "pvwatts.azel_shade" );

		
	// create various processing objects

	// a simple one doesn't need timesteps, local folders, or external binaries to run
	// so just process the 'p_data' with the specified model, returning a single result
	// indicator
	
	i_result = ssc_exec_simple( "hourly_weather_reader", p_data );
	i_result = ssc_exec_simple( "financial_simple_cashflow_electric", p_data );
	i_result = ssc_exec_simple( "geothermal_ui_calcs", p_data );
	i_result = ssc_exec_simple( "pvwatts", p_data );

	const char *result_msg = ssc_simple_nothread( "pvwatts", p_data );

	int i=0;
	while ( const char *p_name = ssc_list(i++) )
	{
		// available model names
	}

	// create a complex processing object, i.e. for ptgen or trnsys
	ssc_context_t p_cxt = ssc_create( "trnsys_pv" );

	int i=0;
	while( ssc_info_t v_info = ssc_get_info(p_proc, i++) )
	{
		int var_type = ssc_get_var_type(v_info);
		int dat_type = ssc_get_data_type(v_info);
		const char *s_name = ssc_get_name(v_info);
		const char *s_units = ssc_get_units(v_info);
		const char *s_meta = ssc_get_meta(v_info);
		const char *s_label = ssc_get_label(v_info);
		const char *s_group = ssc_get_group(v_info);
	}

	// setup simulation start/end/step times, also local folders
	ib_ok = ssc_set_time( p_cxt, tstart, tend, tstep );
	ib_ok = ssc_set_dirs( p_cxt, sworkdir, sinstalldir );

	// configure external handler
	// handles log messages, simulation progress updates, external binary exec requests

		int (*handler_func) ( ssc_context_t p_cxt, 
			int request_type, float arg0, 
			const char *arg1, const char *arg2, 
			void *data_ptr );

	ib_ok = ssc_set_handler( p_cxt, my_func1_pointer, (void*)my_data_arg1 );

	ib_ok = ssc_exec( p_model, p_data );

	if (!ib_ok)
	{
		int i=0;
		while( const char *pmsg = ssc_process_get_message( i++, &itype ) )
		{
			printf("MSG(%d): %s\n", itype, pmsg);
		}
	}
	
	
	
	
	ssc_number_t e_net;
	ib_ok = ssc_data_get_number( p_data, "system.annual.e_net", &e_net );
	pchar = ssc_data_get_string( p_data, "system.log_file" );
	parr = ssc_data_get_array( p_data, "system.monthly.e_net", &count );
	pmat = ssc_data_get_matrix( p_data, "system.shade_effect", &rows, &cols);
	
	ssc_data_unassign( p_data, "system.output" );
	
	ssc_data_write_disk( p_data, "c:/test.dat", SIM_DATA_ALL );


	ssc_data_free( p_data );
}
*/
