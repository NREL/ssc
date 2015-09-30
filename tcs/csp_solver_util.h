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

        S_message_def(int type, std::string msgin)
        {
            m_type = type;
            msg = msgin;
        };

	};

	std::vector<S_message_def> m_message_list;	

public:
	C_csp_messages();

	void add_message(int type, std::string msg);

	bool get_message(int *type, std::string *msg);

	bool get_message(std::string *msg);

};

class C_csp_exception : public std::exception
{
public:
	std::string m_error_message;
	std::string m_code_location;
	
	// Useful in case exception goes uncatched
	virtual const char* what();

	C_csp_exception(const std::string &error_message, const std::string &code_location);

};




#endif