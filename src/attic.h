/*DATATYPE                   DIRECTION               NAME                         LABEL                             UNITS     META_INFO                CONTEXT         REQUIRED_IF                 CONSTRAINTS */

static var_info vtab_weather[] = {
{ var_data::STRING,        var_info::INPUT,      "wf.file_name",              "Standard hourly weather file",   "",       "TMY2,TMY3,EPW",         "Weather",      "?",                        "ALLOW_MULTIPLE,LOCAL_FILE,TMYEPW"},

{ var_data::NUMBER,        var_info::INPUT,      "wd.start",                  "Weather data start time",        "hr",     "hours from Jan 1st",    "Weather",      "ne:wf.file_name",          "MIN=0" },
{ var_data::NUMBER,        var_info::INPUT,      "wd.end",                    "Weather data end time",          "hr",     "hours from Jan 1st",    "Weather",      "ne:wf.file_name",          "MIN=1" },
{ var_data::NUMBER,        var_info::INPUT,      "wd.step",                   "Weather data time step",         "hr",     "",                      "Weather",      "ne:wf.file_name",          "VALID_TIMESTEP" },                                        
{ var_data::NUMBER,        var_info::INPUT,      "wd.lat",                    "Latitude",                       "",       "",                      "Weather",      "ne:wf.file_name",          "" },                                                      
{ var_data::NUMBER,        var_info::INPUT,      "wd.lon",                    "Longitude",                      "",       "",                      "Weather",      "ne:wf.file_name",          "" },                                                      
{ var_data::NUMBER,        var_info::INPUT,      "wd.tz",                     "Time zone",                      "",       "",                      "Weather",      "ne:wf.file_name",          "" },                                                      
{ var_data::NUMBER,        var_info::INPUT,      "wd.elev",                   "Elevation",                      "",       "",                      "Weather",      "ne:wf.file_name",          "" },                                        
{ var_data::ARRAY,  var_info::INPUT,      "wd.gh",                     "Global horizontal",              "W/m2",   "",                      "Weather",      "ne:wf.file_name",          "LENGTH_TIMESTEP(weather.start|weather.end|weather.step)" },
{ var_data::ARRAY,  var_info::INPUT,      "wd.dn",                     "Direct normal radiation",        "W/m2",   "",                      "Weather",      "ne:wf.file_name",          "LENGTH_TIMESTEP(weather.start|weather.end|weather.step)" },
{ var_data::ARRAY,  var_info::INPUT,      "wd.df",                     "Diffuse radiation",              "W/m2",   "",                      "Weather",      "ne:wf.file_name",          "LENGTH_TIMESTEP(weather.start|weather.end|weather.step)" },
{ var_data::ARRAY,  var_info::INPUT,      "wd.tdry",                   "Dry-bulb temp",                  "C",      "",                      "Weather",      "ne:wf.file_name",          "LENGTH_TIMESTEP(weather.start|weather.end|weather.step)" },
{ var_data::ARRAY,  var_info::INPUT,      "wd.twet",                   "Wet-bulb temp",                  "C",      "",                      "Weather",      "ne:wf.file_name",          "LENGTH_TIMESTEP(weather.start|weather.end|weather.step)" },
{ var_data::ARRAY,  var_info::INPUT,      "wd.wspd",                   "Wind speed",                     "m/s",    "",                      "Weather",      "ne:wf.file_name",          "LENGTH_TIMESTEP(weather.start|weather.end|weather.step)" },
{ var_data::ARRAY,  var_info::INPUT,      "wd.wdir",                   "Wind direction",                 "deg",    "N=0,E=90,S=180,W=27",   "Weather",      "ne:wf.file_name",          "LENGTH_TIMESTEP(weather.start|weather.end|weather.step)" },
{ var_data::ARRAY,  var_info::INPUT,      "wd.rhum",                   "Relative humidity",              "%",      "",                      "Weather",      "ne:wf.file_name",          "LENGTH_TIMESTEP(weather.start|weather.end|weather.step)" },
{ var_data::ARRAY,  var_info::INPUT,      "wd.pres",                   "Pressure",                       "mbar",   "",                      "Weather",      "ne:wf.file_name",          "LENGTH_TIMESTEP(weather.start|weather.end|weather.step)" },
{ var_data::INVALID,       0,                       NULL,                       NULL,                             NULL,     NULL,                 NULL,           NULL,           NULL } };


static var_info vtab_pvwatts[] = {
/*DATATYPE                   DIRECTION               NAME                         LABEL                             UNITS     META_INFO                CONTEXT         REQUIRED_IF                 CONSTRAINTS */
{ var_data::NUMBER,        var_info::INPUT,      "pvwatts.nameplate",         "Nameplate capacity",             "kW",     "",                      "PVWatts",      "*",                        "ALLOW_MULTIPLE,MIN=0.5,MAX=100000" },
{ var_data::NUMBER,        var_info::INPUT,      "pvwatts.derate",            "System derate value",            "frac",   "",                      "PVWatts",      "*",                        "ALLOW_MULTIPLE,MIN=0,MAX=1" },
{ var_data::NUMBER,        var_info::INPUT,      "pvwatts.tilt",              "Tilt angle",                     "deg",    "E=90,S=180,W=270",      "PVWatts",      "*",                        "ALLOW_MULTIPLE,MIN=0,MAX=360" },
{ var_data::NUMBER,        var_info::INPUT,      "pvwatts.azimuth",           "Azimuth angle",                  "deg",    "H=0,V=90",              "PVWatts",      "*",                        "ALLOW_MULTIPLE,MIN=0,MAX=90" },
{ var_data::NUMBER,        var_info::INPUT,      "pvwatts.tilt_eq_lat",       "Tilt=latitude override",         "0/1",    "",                      "PVWatts",      "?",                        "ALLOW_MULTIPLE,BOOLEAN" },

{ var_data::ARRAY,  var_info::OUTPUT,     "system.subhourly.e_net",    "Net energy output",              "kWh",    "",                      "PVWatts",      NULL,                       "LENGTH_MULTIPLE_OF=8760" },
{ var_data::ARRAY,  var_info::OUTPUT,     "system.hourly.e_net",       "Net energy output",              "kWh",    "",                      "PVWatts",      NULL,                       "LENGTH=8760" },
{ var_data::ARRAY,  var_info::OUTPUT,     "system.monthly.e_net",      "Net energy output",              "kWh",    "",                      "PVWatts",      NULL,                       "LENGTH=12" },
{ var_data::ARRAY,  var_info::OUTPUT,     "system.annual.e_net",       "Net energy output",              "kWh",    "",                      "PVWatts",      NULL,                       "LENGTH=1" },

{ var_data::INVALID,       0,                       NULL,                       NULL,                             NULL,     NULL,                 NULL,           NULL,           NULL } };

