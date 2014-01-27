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
			if (!weatherfile::convert_to_wfcsv( input, output ))
				throw exec_error( "wfcsvconv", "could not convert " + input + " to " + output );
		}
		else
		{
			weatherfile hdr( input, true );
			if ( !hdr.ok() ) throw exec_error("wfcsvconv", "could not read input file: " + input );

			std::string state = hdr.state;
			std::string city = weatherfile::normalize_city( hdr.city );
			
			std::string country = hdr.country;

			std::string loc = hdr.location;
			std::string type = "?";


			switch( hdr.type() )
			{
			case weatherfile::TMY2: type = "TMY2"; 
				if ( country.empty() ) country = "USA";
				break;
			case weatherfile::TMY3: type = "TMY3"; 
				if ( country.empty() ) country = "USA";
				break;
			case weatherfile::EPW: type = "EPW"; break;
			case weatherfile::SMW: type = "SMW"; break;
			}

			
			if ( !country.empty() ) country += " ";

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
			
			if (!weatherfile::convert_to_wfcsv( input, output ))
				throw exec_error( "wfcsvconv", "could not convert " + input + " to " + output );
		}
	}
};

DEFINE_MODULE_ENTRY( wfcsvconv, "Converter for TMY2, TMY3, INTL, EPW, SMW weather files to standard CSV format", 1 )


	
static var_info _cm_vtab_wfcsvread[] = 
{	
/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP                     REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_STRING,      "file_name",               "Input weather file name",         "",       "wfcsv format",      "Weather File Reader", "*",                       "",                     "" },
	{ SSC_INPUT,        SSC_NUMBER,      "scan_header_only",        "Scan all data or just header",    "",       "",                  "Weather File Reader", "?=0",                     "",                     "" },
	
var_info_invalid };

