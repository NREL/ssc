#include "lib_util.h"
#include "vartab.h"

static const char *var_data_types[] = 
{	"<invalid>", // TCS_INVALID
	"<string>",  // TCS_STRING
	"<number>",  // TCS_NUMBER
	"<array>",   // TCS_ARRAY
	"<matrix>",  // TCS_MATRIX
	"<table>",   // TCS_TABLE
	NULL };

const char *var_data::type_name()
{
	if (type < 6) return var_data_types[ (int)type ];
	else return NULL;
}

std::string var_data::type_name(int type)
{
	if (type >= 0 && type < 5) return var_data_types[ (int)type ];
	else return "";
}


std::string var_data::to_string()
{
	return var_data::to_string( *this );
}

std::string var_data::to_string( const var_data &value )
{
	switch( value.type )
	{
	case TCS_STRING:
		return value.str;
	case TCS_NUMBER:
		return util::to_string( value.num.value() );
	case TCS_ARRAY:
		{
			std::string s;
			for (size_t i=0;i<value.num.length();i++)
			{
				s += util::to_string( (double) value.num[i] );
				if ( i < value.num.length()-1 )	s += ',';
			}
			return s;
		}
	case TCS_MATRIX:
		{
			std::string s;
			for (size_t r=0;r<value.num.nrows();r++)
			{
				s += "[";
				for (size_t c=0; c<value.num.ncols();c++)
				{
					s += util::to_string( (double) value.num.at(r,c) );
					if ( c < value.num.ncols()-1 ) s += ' ';
				}
				s += "]";
			}
			return s;
		}
	}

	return "<invalid>";
}

bool var_data::parse( unsigned char type, const std::string &buf, var_data &value )
{
	switch(type)
	{
	case TCS_STRING:
		{
			value.type = TCS_STRING;
			value.str = buf;
			return true;
		}
	case TCS_NUMBER:
		{
			double x;
			if (util::to_double(buf, &x))
			{
				value.type = TCS_NUMBER;
				value.num = (tcs_number_t)x;
				return true;
			}
			else
				return false;

		}
	case TCS_ARRAY:
		{
			std::vector<std::string> tokens = util::split(buf," ,\t[]\n");
			value.type = TCS_ARRAY;
			value.num.resize_fill( tokens.size(), 0.0 );
			for (size_t i=0; i<tokens.size(); i++)
			{
				double x;
				if (util::to_double( tokens[i], &x ))
					value.num[i] = (tcs_number_t) x;
				else
					return false;
			}
			return true;
		}
	case TCS_MATRIX:
		{
			std::vector<std::string> rows = util::split(buf,"[]\n");
			if (rows.size() < 1) return false;
			std::vector<std::string> cur_row = util::split(rows[0], " ,\t");
			if (cur_row.size() < 1) return false;

			value.type = TCS_MATRIX;
			value.num.resize_fill( rows.size(), cur_row.size(), 0.0 );

			for( size_t c=0; c < cur_row.size(); c++)
			{
				double x;
				if (util::to_double(cur_row[c], &x)) value.num.at(0,c) = (tcs_number_t)x;
			}

			for (size_t r=1; r < rows.size(); r++)
			{
				cur_row = util::split(rows[r], " ,\t");
				for (size_t c=0; c<cur_row.size() && c<value.num.ncols(); c++)
				{
					double x;
					if (util::to_double(cur_row[c], &x))
						value.num.at(r,c) = (tcs_number_t)x;
				}
			}
			return true;
		}
	}

	return false;
}

var_table::var_table() : m_iterator(m_hash.begin())
{
	/* nothing to do here */
}

var_table::~var_table()
{
	clear();
}

var_table &var_table::operator=( const var_table &rhs )
{
	clear();

	for ( var_hash::const_iterator it = rhs.m_hash.begin();
		it != rhs.m_hash.end();
		++it )
		assign( (*it).first, *((*it).second) );

	return *this;
}

void var_table::clear()
{
	for ( var_hash::iterator it = m_hash.begin(); it !=m_hash.end(); ++it )
		delete it->second; // delete the var_data object
	m_hash.clear();
}

var_data *var_table::assign( const std::string &name, const var_data &val )
{
	var_data *v = lookup(name);
	if (!v)
	{
		v = new var_data;
		m_hash[ util::lower_case(name) ] = v;
	}
	
	v->copy(val);
	return v;
}

void var_table::unassign( const std::string &name )
{
	var_hash::iterator it = m_hash.find( util::lower_case(name) );
	if (it != m_hash.end())
	{
		delete (*it).second; // delete the associated data
		m_hash.erase( it );
	}
}

var_data *var_table::lookup( const std::string &name )
{
	var_hash::iterator it = m_hash.find( util::lower_case(name) );
	if ( it != m_hash.end() )
		return (*it).second;
	else
		return NULL;
}

const char *var_table::first( )
{
	m_iterator = m_hash.begin();
	if (m_iterator != m_hash.end())
		return m_iterator->first.c_str();
	else
		return NULL;
}

const char *var_table::next()
{
	if (m_iterator == m_hash.end()) return NULL;

	++m_iterator;

	if (m_iterator != m_hash.end())	return m_iterator->first.c_str();

	return NULL;
}

