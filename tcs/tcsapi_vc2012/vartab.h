#ifndef __lib_vartab_h
#define __lib_vartab_h

#include <string>
#include "tcsapi.h"

#ifdef _MSC_VER
#include <unordered_map>
using std::tr1::unordered_map;
#else
#include <tr1/unordered_map>
using std::tr1::unordered_map;
#endif

class var_data;

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
	unsigned int size() { return (unsigned int)m_hash.size(); }
	var_table &operator=( const var_table &rhs );

private:
	var_hash m_hash;
	var_hash::iterator m_iterator;
};


class var_data
{
public:
	
	var_data() : type(TCS_INVALID) { num=0.0; }
	var_data( const var_data &cp ) : type(cp.type), num(cp.num), str(cp.str) {  }
	var_data( const std::string &s ) : type(TCS_STRING), str(s) {  }
	var_data( tcs_number_t n ) : type(TCS_NUMBER) { num = n; }
	var_data( const tcs_number_t *pvalues, int length ) : type(TCS_ARRAY) { num.assign( pvalues, (size_t)length ); }
	var_data( const tcs_number_t *pvalues, int nr, int nc) : type(TCS_MATRIX) { num.assign( pvalues, (size_t)nr, (size_t)nc ); }

	const char *type_name();
	static std::string type_name(int type);

	std::string to_string();
	static std::string to_string( const var_data &value );
	static bool parse( unsigned char type, const std::string &buf, var_data &value );

	var_data &operator=(const var_data &rhs) { copy(rhs); return *this; }
	void copy( const var_data &rhs ) { type=rhs.type; num=rhs.num; str=rhs.str; table = rhs.table; }
	
	unsigned char type;
	util::matrix_t<tcs_number_t> num;
	std::string str;
	var_table table;
};

#endif
