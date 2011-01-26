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

	virtual void pre_trnsys_call() throw( general_error ) { }
	virtual void write_include_file( FILE *fp ) throw( general_error )= 0;
	virtual void write_deck_end( FILE *fp ) throw( general_error ) { }
	virtual void process_outputs() throw( general_error ) = 0;
	virtual const char *deck_name() throw( general_error )= 0;

	std::string work_dir();
	std::string data_file();

	virtual bool on_extproc_output( const std::string &text );

	void save_data( output &data, 
		const char *col_name, 
		const char *var_name, 
		ssc_number_t scale = 1.0,
		int check_num_values = -1) throw( general_error );

	bool accumulate_annual( output &data, const std::string &var, double &sum);
	bool accumulate_monthly( output &data, const std::string &var, double sums[12]);

	bool write_tou_file( const char *weekday_var,
		const char *weekend_var,
		const char *file );

	bool write_htf_file( const char *custom_var,
		const char *file );
	
	int weather_file_type(const char *wf);

	struct trndata_t
	{
		std::string name;
		std::string units;
		std::vector<ssc_number_t> data;
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


};

#endif

