#ifndef SYSTEM_ADVISOR_MODEL_LIB_STORAGE_VOLTAGE_H
#define SYSTEM_ADVISOR_MODEL_LIB_STORAGE_VOLTAGE_H

#include "lib_util.h"

#include <vector>
#include <map>
#include <string>
#include <stdio.h>
#include <algorithm>

// Forward declarations to reduce imports



// Messages
class message
{
public:
    message(){};
    virtual ~message(){};


    void add(std::string message);
    size_t total_message_count();
    size_t message_count(int index);
    std::string get_message(int index);
    std::string construct_log_count_string(int index);

protected:
    std::vector<std::string> messages;
    std::vector<int> count;
};



#endif //SYSTEM_ADVISOR_MODEL_LIB_STORAGE_VOLTAGE_H
