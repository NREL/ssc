#include "core.h"

static var_info _cm_vtab_cashloan[] = {

/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,      "year",                       "Year (defaults to 1990)",        "",       "",                      "Weather",      "?=1990",                  "INTEGER,MIN=1950",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "lat",                        "Latitude",                       "deg",    "",                      "Weather",      "*",                       "MIN=-90,MAX=90",                           "" },
	{ SSC_INPUT,        SSC_NUMBER,      "lon",                        "Longitude",                      "deg",    "",                      "Weather",      "*",                       "MIN=-180,MAX=180",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tz",                         "Time zone rel. GMT",             "hrs",    "",                      "Weather",      "*",                       "",                                         "" },

var_info_invalid };



class cm_cashloan : public compute_module
{
private:
public:
	cm_cashloan()
	{
		add_var_info( _cm_vtab_cashloan );
	}

	bool exec( ) throw( general_error )
	{
		return false;
	}
};

DEFINE_MODULE_ENTRY( cashloan, "Residential/Commerical Cash or Loan Finance model.", 1 );


