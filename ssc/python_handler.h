#ifndef SYSTEM_ADVISOR_MODEL_PYTHON_HANDLER_H
#define SYSTEM_ADVISOR_MODEL_PYTHON_HANDLER_H

#include <future>
#include <chrono>
#include <stdio.h>
#include <iostream>
#include <string>

#include "vartab.h"

#ifdef _MSC_VER
#define popen _popen
#define pclose _pclose
#endif

extern std::string* python_path;

bool ssc_set_python_path(const std::string& path);

class python_handler {
private:
public:

    static bool call_python_module(std::string module, var_table* data){
        // check module exists in config

        std::promise<std::string> python_result;
        std::future<std::string> f_completes = python_result.get_future();
        std::thread([&](std::promise<std::string> python_result)
                    {
                        printf("thread %s %p\n", module.c_str(), (void*)data);
                        std::string cmd = *python_path + "/" + "python -c "
                                                       "'from landbosse.landbosse_api import run; run.run_landbosse()'";
                        FILE *file_pipe = popen(cmd.c_str(), "r");
                        if (!file_pipe)
                            python_result.set_value_at_thread_exit("python handler error. Could not call python.");

                        std::string mod_response;
                        char buffer[1024];
                        while (fgets(buffer, sizeof(buffer), file_pipe)){
                            mod_response += buffer;
                        }
                        pclose(file_pipe);
                        if (mod_response.empty())
                            python_result.set_value_at_thread_exit("python handler error. No valid return value from module.");
                        else
                            python_result.set_value_at_thread_exit(mod_response);
                    },
                    std::move(python_result)
        ).detach();

        std::chrono::system_clock::time_point two_seconds_passed
                = std::chrono::system_clock::now() + std::chrono::seconds(60);

        if(std::future_status::ready == f_completes.wait_until(two_seconds_passed)){
            std::cout << "f_completes: " << f_completes.get() << "\n"; }
        else{
            std::cout << "f_completes did not complete!\n"; }

    }
};


#endif //SYSTEM_ADVISOR_MODEL_PYTHON_HANDLER_H
