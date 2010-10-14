#include "cm_pvwatts.h"

static var_info vtab_pvwatts[] = {

/* VARTYPE           DATATYPE         NAME                         LABEL                            UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS */
{ SSC_INPUT,        SSC_NUMBER,      "pvwatts.nameplate",         "Nameplate capacity",             "kW",     "",                      "PVWatts",      "*",                       "MIN=0.5,MAX=100000" },
{ SSC_INPUT,        SSC_NUMBER,      "pvwatts.derate",            "System derate value",            "frac",   "",                      "PVWatts",      "*",                       "MIN=0,MAX=1" },
{ SSC_INPUT,        SSC_NUMBER,      "pvwatts.tilt",              "Tilt angle",                     "deg",    "E=90,S=180,W=270",      "PVWatts",      "*",                       "MIN=0,MAX=360" },
{ SSC_INPUT,        SSC_NUMBER,      "pvwatts.azimuth",           "Azimuth angle",                  "deg",    "H=0,V=90",              "PVWatts",      "*",                       "MIN=0,MAX=90" },
{ SSC_INPUT,        SSC_NUMBER,      "pvwatts.tilt_eq_lat",       "Tilt=latitude override",         "0/1",    "",                      "PVWatts",      "?",                       "BOOLEAN" },

{ SSC_OUTPUT,       SSC_ARRAY,       "system.subhourly.e_net",    "Net energy output",              "kWh",    "",                      "PVWatts",      "?",                       "LENGTH_MULTIPLE_OF=8760" },
{ SSC_OUTPUT,       SSC_ARRAY,       "system.hourly.e_net",       "Net energy output",              "kWh",    "",                      "PVWatts",      "*",                       "LENGTH=8760" },
{ SSC_OUTPUT,       SSC_ARRAY,       "system.monthly.e_net",      "Net energy output",              "kWh",    "",                      "PVWatts",      "*",                       "LENGTH=12" },
{ SSC_OUTPUT,       SSC_ARRAY,       "system.annual.e_net",       "Net energy output",              "kWh",    "",                      "PVWatts",      "*",                       "LENGTH=1" },

var_info_invalid };

