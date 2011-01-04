#ifndef __lib_trnsys_h
#define __lib_trnsys_h

#include <cstdio>
#include <cstdlib>

#include "core.h"

/* common trnsys-based model helper functions */

class cm_trnbase : public compute_module
{
public:
	virtual void exec() throw(general_error);

protected:

	class output;

	cm_trnbase();
	virtual ~cm_trnbase() {  /* nothing to do here */ }

	virtual bool pre_trnsys_call() { return true; }
	virtual bool write_include_file( FILE *fp ) = 0;
	virtual bool write_deck_end( FILE *fp ) { return true; }
	virtual bool process_outputs() = 0;
	virtual const char *deck_name() = 0;

	bool save_column( output &data, const std::string &col_name, const std::string &var_name, ssc_number_t scale);

	bool accumulate_annual( output &data, const std::string &var, double &sum);
	bool accumulate_monthly( output &data, const std::string &var, double sums[12]);

	bool write_tou_file( const char *weekday_var,
		const char *weekend_var,
		const char *file );

	bool write_htf_file( const char *htf_type_var,
		const char *htf_user_var,
		const char *file );



	struct trndata_t
	{
		std::string name;
		std::string units;
		std::vector<double> data;
	};

	class output
	{
	private:
		std::vector<trndata_t*> m_cols;
		std::string m_fileName;

	public:
		output() {  }
		~output() { clear(); }
		std::string file_name() { return m_fileName; }

		bool read(const char *fn, size_t expected_data_len = 8760);
		void clear();

		/* returns -1 if data columns do not all 
		  have the same length or there are no columns */
		int data_length(); 
		int num_vars() { return m_cols.size(); }
		std::vector<std::string> variables();
		trndata_t *lookup( const std::string &var );		
	};

	class file_obj
	{
	public:
		file_obj() : p(0) {  }
		file_obj(const char *file, const char *mode) { p = fopen(file, mode); }
		~file_obj() { close(); }
		bool ok() { return 0!=p; }
		operator FILE*() const { return p; }
		void close() { if (p) ::fclose(p); p=0; }
	private:
		FILE *p;
	};

};

#endif

