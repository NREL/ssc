#ifndef __lib_vartab_h
#define __lib_vartab_h

#include <string>
#include "sscapi.h"

#ifdef _MSC_VER
#include <unordered_map>
using std::tr1::unordered_map;
#else
#include <tr1/unordered_map>
using std::tr1::unordered_map;
#endif


class var_data
{
public:
	
	explicit var_data() : type(SSC_INVALID) { num=0.0; }
	explicit var_data( const var_data &cp ) : type(cp.type), num(cp.num), str(cp.str) {  }
	explicit var_data( const std::string &s ) : type(SSC_STRING), str(s) {  }
	explicit var_data( ssc_number_t n ) : type(SSC_NUMBER) { num = n; }
	explicit var_data( ssc_number_t *pvalues, int length ) { num.assign( pvalues, (size_t)length ); }
	explicit var_data( ssc_number_t *pvalues, int nr, int nc) { num.assign( pvalues, (size_t)nr, (size_t)nc ); }

	const char *type_name();
	static std::string type_name(int type);

	std::string to_string();
	static std::string to_string( const var_data &value );
	static bool parse( unsigned char type, const std::string &buf, var_data &value );

	void copy( const var_data &rhs ) { type=rhs.type; num=rhs.num; str=rhs.str; }
	
	unsigned char type;
	util::matrix_t<ssc_number_t> num;
	std::string str;
};

typedef unordered_map< std::string, var_data* > var_hash;

class var_table
{
public:
	explicit var_table();
	virtual ~var_table();

	void clear();
	var_data *assign( const std::string &name, const var_data &value );
	void unassign( const std::string &name );
	var_data *lookup( const std::string &name );
	const char *first();
	const char *next();

private:
	var_hash m_hash;
	var_hash::iterator m_iterator;
};

#endif
