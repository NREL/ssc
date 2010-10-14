#include <cstdio>
#include <cstdarg>
#include <cstring>
#include <cstdlib>
#include "util.h"

#ifdef __VISUALC__
/* taken from wxMSW-2.9.1/include/wx/defs.h - appropriate for Win32/Win64 */
#define va_copy(d, s) ((d)=(s))
#endif

std::vector< std::string > util::split( const std::string &str, const std::string &delim, bool ret_empty, bool ret_delim )
{
	std::vector< std::string > list;

	char cur_delim[2] = {0,0};
	std::string::size_type m_pos = 0;
	std::string token;
	
	while (m_pos < str.length())
	{
		std::string::size_type pos = str.find_first_of(delim, m_pos);
		if (pos == std::string::npos)
		{
			cur_delim[0] = 0;
			token.assign(str, m_pos, std::string::npos);
			m_pos = str.length();
		}
		else
		{
			cur_delim[0] = str[pos];
			std::string::size_type len = pos - m_pos;			
			token.assign(str, m_pos, len);
			m_pos = pos + 1;
		}
		
		if (token.empty() && !ret_empty)
			continue;

		list.push_back( token );
		
		if ( ret_delim && cur_delim[0] != 0 && m_pos < str.length() )
			list.push_back( std::string( cur_delim ) );
	}
	
	return list;
}

std::string util::join( const std::vector< std::string > &list, const std::string &delim )
{
	std::string str;
	for (std::vector<std::string>::size_type i=0;i<list.size();i++)
	{
		str += list[i];
		if (i < list.size()-1)
			str += delim;
	}
	return str;		
}


bool util::to_integer(const std::string &str, int *x)
{
	const char *startp = str.c_str();
	char *endp = NULL;
	*x = ::strtol( startp, &endp, 10 );	
	return !*endp && (endp!=startp);
}

bool util::to_float(const std::string &str, float *x)
{
	double val;
	bool ok = to_double(str, &val);
	*x = (float) val;
	return ok;
}

bool util::to_double(const std::string &str, double *x)
{
	const char *startp = str.c_str();
	char *endp = NULL;
	*x = ::strtod( startp, &endp );	
	return !*endp && (endp!=startp);
}

std::string util::to_string( int x, const char *fmt )
{
	char buf[64];
	sprintf(buf, fmt, x);
	return std::string(buf);
}

std::string util::to_string( double x, const char *fmt )
{
	char buf[256];
	sprintf(buf, fmt, x);
	return std::string(buf);
}

std::string util::lower_case( const std::string &in )
{
	std::string ret(in);
	for (std::string::size_type i=0;i<ret.length();i++)
		ret[i] = tolower(ret[i]);
	return ret;
}

std::string util::upper_case( const std::string &in )
{
	std::string ret(in);
	for (std::string::size_type i=0;i<ret.length();i++)
		ret[i] = toupper(ret[i]);
	return ret;
}
	


std::string util::format(const char *fmt, ...)
{
	if (!fmt || *fmt == 0) return "";

	va_list arglist;
	va_start( arglist, fmt );

	size_t ret = 0;

	int size = 512;
	char *buffer = new char[size];
	if (!buffer)
		return "";

	do
	{
		va_list argptr_copy;
		va_copy( argptr_copy, arglist );
		ret = util::format_vn(buffer,size-1,fmt,argptr_copy);
		va_end( argptr_copy );

		if (ret == 0)
		{
			delete [] buffer;
			size *= 2;
			buffer = new char[size];
			if (!buffer)
				return "";
		}
		
	}
	while (ret < 0);
	
	va_end(arglist);

	std::string s(buffer);
	if (buffer)
		delete [] buffer;
		
	return s;
}

size_t util::format_vn(char *buffer, int maxlen, const char *fmt, va_list arglist)
{
	char *p = (char*)fmt, *bp = buffer, *tp;
	char *bpmax = buffer+maxlen-1;
	int i;
	
	char arg_char;
	char *arg_str;
	int arg_int;
	unsigned int arg_uint;
	double arg_double;
	
#define TEMPLEN 256
	char temp[TEMPLEN];
	char tempfmt[TEMPLEN];
	char *decpt;
	size_t ndigit;
	int with_precision;
	char *with_comma;
	char prev;
	
	if (!p)
	{
		*bp = 0;
		return 0;
	}
	
	while( *p && bp<bpmax )
	{
		if (*p != '%')	*bp++ = *p++;
		else
		{
			p++;			
			switch (*p)
			{
			case 'd':
			case 'D':
			/* handle simple signed integer format */
				p++;
				arg_int = va_arg(arglist, int);
				sprintf(temp, "%d", arg_int);
				tp = temp;
				while (*tp && bp<bpmax)
					*bp++ = *tp++;					
				break;					
			
			case 'u':
			case 'U':
			/* handle simple unsigned integer format */
				p++;
				arg_uint = va_arg(arglist, unsigned int);
				sprintf(temp, "%u", arg_uint);
				tp = temp;
				while (*tp && bp<bpmax)
					*bp++ = *tp++;	
				break;
				
			case 'x':
			case 'X':
			/* handle hexadecimal unsigned integer format */
				p++;
				arg_uint = va_arg(arglist, unsigned int);
				sprintf(temp, "%x", arg_uint);
				tp = temp;
				while (*tp && bp<bpmax)
					*bp++ = *tp++;	
				break;
			
			case 'c':
			case 'C':
			/* handle simple char format */
				arg_char = (char)va_arg(arglist, int);
				if ( bp+1<bpmax ) *bp++ = arg_char;
				p++;
				break;
			
			case 's':
			case 'S':
			/* handle simple string format */
				p++;
				arg_str = va_arg(arglist, char*);
				tp = arg_str;
				while (*tp && bp<bpmax)
					*bp++ = *tp++;
				break;
			
			case '%':
				if (bp+1<bpmax)	*bp++ = *p++;
				break;
								
			
			case 'l':
			case 'L':
			case 'f':
			case 'F':
			case 'g':
			case 'G':
			case '.':
				with_precision = 0;
				with_comma = 0;
				tp = tempfmt;			
				*tp++ = '%';
				if (*p == '.')
				{ /* accumulate the precision */	
					with_precision = 1;
					*tp++ = *p++;
					if (*p == '0') with_precision = 2;
					while ( *p && isdigit(*p) )
						*tp++ = *p++;
				}
				*tp++ = 'l';
				if (*p == 'l' || *p == 'L')	p++;// skip lL
				if (*p == ',') // comma separated
				{
					*tp++ = 'f'; p++;
					with_comma = (char*)1;
				}
				else // fFgG
					*tp++ = *p++;

				*tp = '\0'; // end format string
				arg_double = va_arg(arglist, double);
				
				sprintf(temp, tempfmt, (double)arg_double);
				
				i=0;
				if (with_comma)
				{
					decpt = strchr(temp, '.');
					if (!decpt) ndigit = strlen(temp);
					else ndigit = (int)(decpt-temp);
					i=0-ndigit%3;
				}

				if ((!with_precision || with_comma!=NULL) && 
					!strchr(tempfmt,'g') &&
					!strchr(tempfmt,'G') &&
					(!(with_precision == 2)) )
				{
					tp = temp+strlen(temp)-1;
					while (tp > temp && *tp == '0')
						*tp-- = 0;
					if (*tp == '.')
						*tp-- = 0;					
				}
				
				tp = temp; decpt = 0; prev = 0;
				while (*tp && bp<bpmax)
				{
					if (*tp == '.') decpt = (char*)1;
					if (with_comma != NULL && isdigit(prev) && i%3==0 && !decpt && bp<bpmax) *bp++ = ',';
					prev = *tp;
					if (bp<bpmax) *bp++ = *tp++;
					i++;
				}
				
				break;			
			
			/* handle comma or money format (double precision) */
			case 'm':
			case 'M':
			case ',':
				arg_double = va_arg(arglist, double);
				if (*p == ',')
				{
					sprintf(temp, "%lf", arg_double);
					if (strchr(temp,'e')!=NULL) sprintf(temp, "%d", (int)arg_double);
				}
				else sprintf(temp, "%.2lf",  arg_double);
				
				decpt = strchr(temp, '.');
				if (!decpt) ndigit = strlen(temp);
				else ndigit = (int)(decpt-temp);	
							
				if (*p == ',')
				{
					tp = temp+strlen(temp)-1;
					while (tp > temp && *tp == '0') *tp-- = 0;
						
					if (*tp == '.') *tp-- = 0;
				}					
				
				i=0-(ndigit%3); tp = temp; decpt = 0; prev = 0;
				while (*tp)
				{
					if (*tp == '.')	decpt = (char*)1;
					if ( isdigit(prev) && i%3==0 && !decpt && bp<bpmax) *bp++ = ',';	
					prev = *tp;
					if (bp<bpmax) *bp++ = *tp;
					tp++; i++;
				}
				p++;
				break;
			}
			
		}
		
	}

	*bp = 0;

#undef TEMPLEN

	if (bp==bpmax) return 0;
	else return (bp-buffer);
}

