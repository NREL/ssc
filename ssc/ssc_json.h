#ifndef SYSTEM_ADVISOR_MODEL_SSC_JSON_H
#define SYSTEM_ADVISOR_MODEL_SSC_JSON_H

#include <string>

#include <vartab.h>


std::string var_table_to_json(var_table* vt);

var_table* json_to_var_table(const std::string &json);


#endif //SYSTEM_ADVISOR_MODEL_SSC_JSON_H
