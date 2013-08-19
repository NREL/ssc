#include "core.h"

#include "lib_weatherfile.h"

static var_info _cm_vtab_wfcsvconv[] = 
{	
/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP                     REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_STRING,      "input_file",               "Input weather file name",         "",       "tmy2,tmy3,intl,epw,smw",      "Weather File Converter", "*",                       "",                     "" },
	{ SSC_INPUT,        SSC_STRING,      "output_file",              "Output file name",                "",       "",                       "Weather File Converter", "?",                       "",                     "" },
	{ SSC_INPUT,        SSC_STRING,      "output_folder",            "Output folder",                   "",       "",                       "Weather File Converter", "?",                       "",                     "" },
	{ SSC_INPUT,        SSC_STRING,      "output_filename_format",   "Output file name format",         "",       "recognizes $city $state $country $type $loc",         "Weather File Converter", "?",                       "",                     "" },

var_info_invalid };

class cm_wfcsvconv : public compute_module
{
private:
public:
	cm_wfcsvconv()
	{
		add_var_info( _cm_vtab_wfcsvconv );
	}

	void exec( ) throw( general_error )
	{
		std::string input = as_string("input_file");

		if ( is_assigned("output_file") )
		{
			std::string output = as_string("output_file");
			if (!wfcsv::convert( input, output ))
				throw exec_error( "wfcsvconv", "could not convert " + input + " to " + output );
		}
		else
		{
			weatherfile hdr( input );
			if ( !hdr.ok() ) throw exec_error("wfcsvconv", "could not read input file: " + input );

			std::string state = hdr.state;

			std::string city = util::lower_case( hdr.city );
			util::replace( city, "_", " " );
			util::replace( city, "\"", "" );
			util::replace( city, "/", " " );
			util::replace( city, "\\", " " );

			for ( size_t i=0;i<city.length();i++ )
			{
				if ( i==0 || city[i-1] == ' ' )
					city[i] = toupper( city[i] );
			}

			std::string country = hdr.country;
			if ( !country.empty() ) country += " ";
			std::string loc = hdr.loc_id;
			std::string type = "?";


			switch( hdr.type() )
			{
			case weatherfile::TMY2: type = "TMY2"; break;
			case weatherfile::TMY3: type = "TMY3"; break;
			case weatherfile::EPW: type = "EPW"; break;
			case weatherfile::SMW: type = "SMW"; break;
			}
			hdr.close();

			std::string ofmt = "$country $state $city ($type)";
			if ( is_assigned("output_file_format") )
				ofmt = as_string("output_filename_format");

			std::string folder = util::path_only(input);
			if ( is_assigned( "output_folder") )
				folder = as_string( "output_folder" );
			std::string output = folder + "/" + ofmt;

			util::replace( output, "$city", city );
			util::replace( output, "$state", state );
			util::replace( output, "$country ", country );
			util::replace( output, "$loc", loc );
			util::replace( output, "$type", type );

			if ( util::ext_only( output ) != "csv" )
				output += ".csv";
			
			if (!wfcsv::convert( input, output ))
				throw exec_error( "wfcsvconv", "could not convert " + input + " to " + output );
		}
	}
};

DEFINE_MODULE_ENTRY( wfcsvconv, "Converter for TMY2, TMY3, INTL, EPW, SMW weather files to standard CSV format", 1 )


	
static var_info _cm_vtab_wfcsvread[] = 
{	
/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP                     REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_STRING,      "file_name",               "Input weather file name",         "",       "wfcsv format",      "Weather File Converter", "*",                       "",                     "" },
	
var_info_invalid };

class cm_wfcsvread : public compute_module
{
private:
public:
	cm_wfcsvread()
	{
		add_var_info( _cm_vtab_wfcsvread );
	}

	void exec( ) throw( general_error )
	{
		std::string file = as_string("file_name");
		wfcsv in( file );
		if ( !in.ok() ) throw exec_error( "wfcsvread", util::format("error code %d when reading file ", in.error()) + file );

		assign( "interpolate", var_data( in.interpolate() ? 1 : 0 ) );
		if ( !in.location().empty() ) assign( "location", var_data(in.location()) );
		if ( !in.city().empty() ) assign( "city", var_data(in.city()) );
		if ( !in.state().empty() ) assign( "state", var_data(in.state()) );
		if ( !in.country().empty() ) assign( "country", var_data(in.country()) );
		if ( !in.source().empty() ) assign( "source", var_data(in.source()) );
		if ( !in.description().empty() ) assign( "description", var_data(in.description()) );
		if ( !in.url().empty() ) assign( "url", var_data(in.url()) );

		assign( "lat", var_data(in.lat()) );
		assign( "lon", var_data(in.lon()) );
		assign( "tz", var_data(in.tz()) );
		assign( "elev", var_data(in.elev()) );

		if ( in.year() > 0 )
			assign( "year", var_data(in.year()) );

		std::vector<int> cols = in.get_columns();
		for( size_t i=0;i<cols.size();i++ )
		{
			size_t n = in.num_records();
			ssc_number_t *vec = allocate( in.get_canonical_name( cols[i] ), n );
			for ( size_t j=0;j<n;j++ )
				vec[j] = in.value( cols[i], j );
		}

	}
};

DEFINE_MODULE_ENTRY( wfcsvread, "Reads standard CSV weather files", 1 )
