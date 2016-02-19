#include "csp_solver_util.h"

C_csp_messages::C_csp_messages()
{
	//m_message_list.resize(0);
    m_message_list.clear();
}

void C_csp_messages::add_message(int type, std::string msg)
{
	// Want first message last...
    m_message_list.insert( m_message_list.begin(), S_message_def(type, msg) );

}

void C_csp_messages::add_notice(std::string msg)
{
	add_message(C_csp_messages::NOTICE, msg);
}

bool C_csp_messages::get_message(int *type, std::string *msg)
{
    if(m_message_list.size() == 0)
        return false;

    S_message_def temp = m_message_list.back();
    m_message_list.pop_back();

    *msg = temp.msg;
    *type = temp.m_type;

    return true;
}

bool C_csp_messages::get_message(std::string *msg)
{
    int itemp;

    return get_message(&itemp, msg);
}

const char* C_csp_exception::what()
{
	return "CSP exception";
}

C_csp_exception::C_csp_exception(const char *cmsg)
{
	m_error_message = cmsg;
	m_code_location = "unknown";
	m_error_code = -1;
}
C_csp_exception::C_csp_exception(const std::string &error_message, const std::string &code_location)
{
	m_error_message = error_message;
	m_code_location = code_location;
	m_error_code = -1;
}
C_csp_exception::C_csp_exception(const std::string &error_message, const std::string &code_location, int error_code)
{
	m_error_message = error_message;
	m_code_location = code_location;
	m_error_code = error_code;
}