#include "cmod_trnbase.h"

static var_info _cm_vtab_trntower[] = {
/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP                  REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/	

	{ SSC_INPUT,        SSC_STRING,      "weather_file",            "Weather data file (TM2,TM2,EPW)",   "",       "",                      "Dish Stirling",       "*",                    "",                              "" },

	// solar field

	// receiver

	// power block

	// storage

	// parasitics

	// flux maps and optical efficiency data

var_info_invalid };

class cm_trntower : public cm_trnbase
{
private:
public:
	cm_trntower() : cm_trnbase()
	{
		add_var_info( _cm_vtab_trntower );
	}

	virtual void write_include_file( FILE *fp ) throw( general_error )
	{
	}

	virtual void process_outputs() throw( general_error )
	{
		update("Saving data...", 99.5);
	}

	virtual const char *deck_name()
	{
		return "csp_tower";
	}
};

DEFINE_MODULE_ENTRY( trntower, "Power tower (central receiver) simulator (TRNSYS)", 1 )