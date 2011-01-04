
#include "cmod_trnbase.h"


static var_info _cm_vtab_trnbase[] = {
/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/	
	{ SSC_INPUT,        SSC_NUMBER,      "trn_udv1",                "TRNSYS User Defined Variable 1",   "",       "",                      "TRNSYS",       "?=0.0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "trn_udv2",                "TRNSYS User Defined Variable 2",   "",       "",                      "TRNSYS",       "?=0.0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "trn_udv3",                "TRNSYS User Defined Variable 3",   "",       "",                      "TRNSYS",       "?=0.0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "trn_udv4",                "TRNSYS User Defined Variable 4",   "",       "",                      "TRNSYS",       "?=0.0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "trn_udv5",                "TRNSYS User Defined Variable 5",   "",       "",                      "TRNSYS",       "?=0.0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "trn_udv7",                "TRNSYS User Defined Variable 6",   "",       "",                      "TRNSYS",       "?=0.0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "trn_udv7",                "TRNSYS User Defined Variable 7",   "",       "",                      "TRNSYS",       "?=0.0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "trn_udv8",                "TRNSYS User Defined Variable 8",   "",       "",                      "TRNSYS",       "?=0.0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "trn_udv9",                "TRNSYS User Defined Variable 9",   "",       "",                      "TRNSYS",       "?=0.0",                    "",                              "" },
	
	{ SSC_OUTPUT,       SSC_STRING,      "deck_file",               "TRNSYS Input Deck File",           "",       "",                      "TRNSYS",       "*",                        "",                              "" },
	{ SSC_OUTPUT,       SSC_STRING,      "log_file",                "TRNSYS Log File",                  "",       "",                      "TRNSYS",       "*",                        "",                              "" },

var_info_invalid };

	
static param_info _cm_params_trnbase[] = {
	/* TYPE,      NAME,       DEFAULT_VALUE,    DESCRIPTION */
	{ SSC_STRING, "trnsys_exe",    "",            "Path to TRNSYS executable" },
	{ SSC_STRING, "working_dir",   "",            "Local working directory" },
	{ SSC_STRING, "hourly_file",   "",            "(sub)Hourly output file" },
	{ SSC_NUMBER, "trnsys_start",  "0",           "Simulation start time (hr) (default=0)" },
	{ SSC_NUMBER, "trnsys_end",    "8760",        "Simulation end time (hr) (default=8760)" },
	{ SSC_NUMBER, "trnsys_step",   "1",           "Simulation time step (hr) (default=1)" },
	{ SSC_INVALID, NULL }  };

cm_trnbase::cm_trnbase()
{
	add_var_info( _cm_vtab_trnbase );
	set_param_info( _cm_params_trnbase );
}

bool cm_trnbase::accumulate_annual( output &data, const std::string &var, double &sum)
{
	sum = 0;

	trndata_t *d = data.lookup( var );
	if (!d) return false;

	for (size_t i=0;i<d->data.size();i++)
		sum += d->data[i];

	return true;
}

bool cm_trnbase::accumulate_monthly( output &data, const std::string &var, double sums[12])
{
	size_t i;
	for (i=0;i<12;i++) sums[i] = 0.0;

	trndata_t *pd = data.lookup( var );
	if (!pd || pd->data.size() != 8760) return false;

	i=0;
	for (size_t m=0;m<12;m++) // over months
		for (size_t d=0;d<(size_t)util::nday[m];d++) // over days
			for (size_t h=0;h<24;h++) // over hours
				sums[m] += pd->data[i++];  // 'i' will run 0 to 8759

	return true;
}

bool cm_trnbase::write_tou_file( const char *weekday_var, const char *weekend_var, const char *file )
{
	int tod[8760];
	const char *weekday = as_string(weekday_var);
	const char *weekend = as_string(weekend_var);

	if ( !weekday
		|| !weekend
		|| !util::translate_schedule( tod, weekday, weekend, 0, 8 )) return false;

	FILE *fp = fopen( file, "w" );
	if (!fp ) return false;

	fprintf(fp, "Period\n");
	for (size_t i=0;i<8760;i++)
		fprintf(fp, "%d\n", tod[i]);
	fclose(fp);
	return true;
}

void cm_trnbase::exec() throw(general_error)
{
	std::string work_dir = param_string("working_dir");
	std::string exe = param_string("trnsys_exe");
	std::string deck = deck_name();
	
	if (!util::dir_exists( work_dir.c_str() ))
		util::mkdir( work_dir.c_str(), true );

	if (!util::dir_exists( work_dir.c_str() ))
		throw general_error("could not create working directory for simulation: " + work_dir);
	
	std::string list_file = work_dir + util::path_separator() + deck + ".lst";
	std::string log_file = work_dir + util::path_separator() + deck + ".log";
	std::string trd_file = work_dir + util::path_separator() + deck + ".trd";

	file_obj f( trd_file.c_str(), "w" );
	if (!f.ok())
		throw general_error("could not create write TRNSYS input file: " + trd_file);
	
	fprintf(f, "VERSION 16\n");
	fprintf(f, "*****************************************************************\n");
	fprintf(f, "*** TRNSYS input file generated by SSC for %s", deck.c_str());
	fprintf(f, "*** Copyright (c) 2011 National Renewable Energy Laboratory\n");
	fprintf(f, "*****************************************************************\n\n");

	if (!write_include_file(f)) throw general_error("error writing include file section\n");

	fprintf(f, "\n\nCONSTANTS 10\n");
	for (int i=0;i<=9;i++)
		fprintf(f, "\tUDV%d=%lg\n", i, 
			as_double( util::format("trn_udv%d\n", i) ) );

	fprintf(f, "\n*** end include section ***\n\n");

	std::string deck_src = util::path_only(exe) + util::path_separator() + deck + ".trdsrc";
	file_obj src( deck_src.c_str(), "r" );
	if (!src.ok()) throw general_error("error reading deck source file: " + deck_src);

	int c;
	while ( EOF != (c=fgetc(src)) )
		fputc( c, f );

	src.close();
	
	if (!write_deck_end( f )) throw general_error("error writing deck common end portion");

	f.close();

	if (!pre_trnsys_call()) throw general_error("error with pre-trnsys call");
	
	// main call to external process marshaller:  run trn.exe
	if (!extproc( exe, work_dir )) throw general_error("failed to run trnsys");

	assign("deck_file", var_data(trd_file));
	assign("log_file", var_data(log_file));

	if (!process_outputs()) throw general_error("failed to read TRNSYS output data");
}



bool cm_trnbase::output::read(const char *fn, size_t expected_data_len)
{
	FILE *fp = fopen(fn, "r");
	if (!fp) return false;

	clear();

	size_t line_buf_len = 2048;

	std::string names, units;
	util::read_line(fp, names, line_buf_len );
	util::read_line(fp, units, line_buf_len );

	std::vector<std::string> name_list = util::split( names, " \t" );
	std::vector<std::string> unit_list = util::split( units, " \t" );

	if (name_list.size() != unit_list.size())
	{
		fclose( fp );
		return false;
	}

	size_t ncols = name_list.size();
	m_cols.resize( ncols );

	for (size_t i=0;i<ncols;i++)
	{
		m_cols[i] = new trndata_t;
		m_cols[i]->name = name_list[i];
		m_cols[i]->units = unit_list[i];
		m_cols[i]->data.reserve( expected_data_len );
	}

	
	size_t line = 0, ncol, ndbuf;
	char dblbuf[256], *p, *bp;
	
	line_buf_len = ncols * 100; // should be sufficient room for a whole line
	char *buf = new char[ line_buf_len ];
	
	while ( 1 )
	{
		if (fgets(buf,line_buf_len-1,fp) == 0)
			break;

		p = buf;
		ncol = 0;
		while (*p && ncol < ncols)
		{
			bp = dblbuf;
			ndbuf = 0;
			while(*p && (*p==' '||*p=='\t')) p++; // skip white space
			while(*p && (*p!=' '&&*p!='\t') && ++ndbuf < 127) *bp++ = *p++; // read in number
			*bp = '\0'; // terminate string
			m_cols[ncol++]->data.push_back( atof( dblbuf ) ); // convert number and save
		}
		line++;
	}

	delete [] buf;

	fclose(fp);

	m_fileName = std::string(fn);
	return true;
}

void cm_trnbase::output::clear()
{
	for (std::vector<trndata_t*>::iterator it = m_cols.begin();
		it != m_cols.end();
		++it)
		delete *it;

	m_cols.clear();
}

int cm_trnbase::output::data_length()
{
	int len = -1;

	for (std::vector<trndata_t*>::iterator it = m_cols.begin();
		it != m_cols.end();
		++it)
	{
		if (len == -1) len = (*it)->data.size();
		else if ( (*it)->data.size() != (size_t)len )
			return -1;
	}

	return len;
}

std::vector<std::string> cm_trnbase::output::variables()
{
	std::vector<std::string> list;
	for (std::vector<trndata_t*>::iterator it = m_cols.begin();
		it != m_cols.end();
		++it)
	{
		list.push_back( (*it)->name );
	}
	return list;
}

cm_trnbase::trndata_t *cm_trnbase::output::lookup( const std::string &var )
{
	for (size_t i=0;i<m_cols.size();i++)
	{
		if (util::lower_case(m_cols[i]->name) == util::lower_case(var))
			return m_cols[i];
	}

	return 0;
}
