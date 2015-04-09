#include "csp_solver_util.h"

C_csp_messages::C_csp_messages()
{
	m_message_list.resize(0);
}

void C_csp_messages::add_message(int type, std::string msg)
{
	// Want first message last...
	int size = m_message_list.size();

	std::vector<S_message_def> message_list_copy;

	message_list_copy = m_message_list;
	
	mS_placeholder.m_type = type;
	mS_placeholder.msg = msg;

	m_message_list[0] = mS_placeholder;

	for( int i = 1; i < size; i++ )
		m_message_list[i] = message_list_copy[size - i];

	mS_placeholder.m_type = message_list_copy[size - 1].m_type;
	mS_placeholder.msg = message_list_copy[size - 1].msg;

	m_message_list.push_back(mS_placeholder);
}

bool C_csp_messages::get_message(int *type, std::string *msg)
{
	int size = m_message_list.size();
	if( size > 0 )
	{
		mS_placeholder = m_message_list[size - 1];
		*type = mS_placeholder.m_type;
		*msg = mS_placeholder.msg;

		m_message_list.resize(size - 1);

		return true;
	}
	else
	{
		return false;
	}
}

bool C_csp_messages::get_message(std::string *msg)
{
	int size = m_message_list.size();
	if( size > 0 )
	{
		mS_placeholder = m_message_list[size - 1];
		*msg = mS_placeholder.msg;

		m_message_list.resize(size - 1);

		return true;
	}
	else
	{
		return false;
	}
}

const char* C_csp_exception::what()
{
	return "CSP exceptoin";
}

C_csp_exception::C_csp_exception(const std::string &error_message, const std::string &code_location)
{
	m_error_message = error_message;
	m_code_location = code_location;
}