#ifndef __csp_solver_util_
#define __csp_solver_util_

#include <string>
#include <vector>

#include <exception>

class C_csp_messages
{

public:
	enum 
	{
		NOTICE,
		WARNING
	};

	struct S_message_def
	{
		int m_type;
		std::string msg;

		S_message_def()
		{
			m_type = -1;
			msg[0] = NULL;
		}
	};

	std::vector<S_message_def> m_message_list;	

private:
	S_message_def mS_placeholder;

public:
	C_csp_messages()
	{
		m_message_list.resize(0);
	}

	void add_message(int type, std::string msg)
	{
		mS_placeholder.m_type = type;
		mS_placeholder.msg = msg;

		m_message_list.push_back(mS_placeholder);
	}

	bool get_message(int *type, std::string *msg)
	{
		int size = m_message_list.size();
		if(size > 0)
		{
			mS_placeholder = m_message_list[size-1];
			*type = mS_placeholder.m_type;
			*msg = mS_placeholder.msg;

			m_message_list.resize(size-1);

			return true;
		}
		else
		{
			return false;
		}
	}

	bool get_message(std::string *msg)
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

};

class C_csp_exception : public exception
{
public:
	std::string m_error_message;
	std::string m_code_location;
	
	//virtual const char* what()
	//{
	//	return "CSP exception";
	//}

	C_csp_exception(const std::string &error_message, const std::string &code_location)
	{
		m_error_message = error_message;
		m_code_location = code_location;
	}

};









#endif