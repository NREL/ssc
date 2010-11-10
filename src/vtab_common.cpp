#include "core.h"

var_info vtab_utility_rate[] = {

/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,      "year",                       "Year (defaults to 1990)",        "",       "",                      "Weather",      "?=1990",                  "INTEGER,MIN=1950",                         "" },

var_info_invalid };