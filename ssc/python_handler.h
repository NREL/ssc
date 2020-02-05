#ifndef SYSTEM_ADVISOR_MODEL_PYTHON_HANDLER_H
#define SYSTEM_ADVISOR_MODEL_PYTHON_HANDLER_H

#include <future>
#include <chrono>

#include <iostream>
#include <string>

#include "vartab.h"

extern std::string* python_path;

bool ssc_set_python_path(const std::string& path);

class python_handler {
private:

    static int call_python(std::string module, var_table* data) {
        printf("%s %p\n", module.c_str(), (void*)data);
        std::string cmd = *python_path + "/" + "python -c 'print(\"i am python\")'";
        system(cmd.c_str());
        return 0;

    }
public:

    static bool call_python_module(std::string module, var_table* data){
        // check module exists in config

        auto future = std::async(std::launch::async, call_python, module, data);

        std::chrono::system_clock::time_point two_seconds_passed
                = std::chrono::system_clock::now() + std::chrono::seconds(60);

        if(std::future_status::ready == future.wait_until(two_seconds_passed)){
            std::cout << "f_completes: " << future.get() << "\n"; }
        else{
            std::cout << "f_completes did not complete!\n"; }

    }
};


#endif //SYSTEM_ADVISOR_MODEL_PYTHON_HANDLER_H
