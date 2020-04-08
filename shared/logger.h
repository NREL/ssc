#ifndef SAM_SIMULATION_CORE_LOGGER_H
#define SAM_SIMULATION_CORE_LOGGER_H

#include <ostream>
#include <iostream>

class logger {
private:
    std::ostream& _out_stream;

    //Constructor: User provides custom output stream, or uses default (std::cout).
public:
    explicit logger(std::ostream& stream = std::cout): _out_stream(stream) {}

    //Implicit conversion to std::ostream
    explicit operator std::ostream& () {
        return _out_stream;
    }

    template<typename T>
    logger& operator<< (const T& data)
    {
        _out_stream << data;
    }
};

#endif //SAM_SIMULATION_CORE_LOGGER_H
