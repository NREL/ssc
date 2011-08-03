
#include "cmod_trnbase.h"


static var_info _cm_vtab_trnbase[] = {
/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/	
	{ SSC_INPUT,        SSC_NUMBER,      "trn_udv1",                "TRNSYS User Defined Variable 1",   "",       "",                      "TRNSYS",       "?=0.0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "trn_udv2",                "TRNSYS User Defined Variable 2",   "",       "",                      "TRNSYS",       "?=0.0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "trn_udv3",                "TRNSYS User Defined Variable 3",   "",       "",                      "TRNSYS",       "?=0.0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "trn_udv4",                "TRNSYS User Defined Variable 4",   "",       "",                      "TRNSYS",       "?=0.0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "trn_udv5",                "TRNSYS User Defined Variable 5",   "",       "",                      "TRNSYS",       "?=0.0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "trn_udv6",                "TRNSYS User Defined Variable 6",   "",       "",                      "TRNSYS",       "?=0.0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "trn_udv7",                "TRNSYS User Defined Variable 7",   "",       "",                      "TRNSYS",       "?=0.0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "trn_udv8",                "TRNSYS User Defined Variable 8",   "",       "",                      "TRNSYS",       "?=0.0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "trn_udv9",                "TRNSYS User Defined Variable 9",   "",       "",                      "TRNSYS",       "?=0.0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "trn_udv10",                "TRNSYS User Defined Variable 10",   "",       "",                      "TRNSYS",       "?=0.0",                    "",                              "" },
	
	{ SSC_OUTPUT,       SSC_STRING,      "deck_file",               "TRNSYS Input Deck File",           "",       "",                      "TRNSYS",       "*",                        "",                              "" },
	{ SSC_OUTPUT,       SSC_STRING,      "log_file",                "TRNSYS Log File",                  "",       "",                      "TRNSYS",       "*",                        "",                              "" },

var_info_invalid };

	
static param_info _cm_params_trnbase[] = {
	/* TYPE,      NAME,       DEFAULT_VALUE,    DESCRIPTION */
	{ SSC_STRING, "trnsys_exe",     "",            "Path to TRNSYS executable" },
	{ SSC_STRING, "working_dir",    "",            "Local working directory" },
	{ SSC_STRING, "data_file",      "",            "(sub)Hourly output data file" },
	{ SSC_NUMBER, "trnsys_tmstart", "0",           "Simulation start time (hr) (default=0)" },
	{ SSC_NUMBER, "trnsys_tmstop",  "8760",        "Simulation end time (hr) (default=8760)" },
	{ SSC_NUMBER, "trnsys_tmstep",  "1",           "Simulation time step (hr) (default=1)" },
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

bool cm_trnbase::write_htf_file( const char *custom_var, const char *file )
{
	size_t nrows = 0, ncols = 0;
	ssc_number_t *tab = as_matrix(custom_var, &nrows, &ncols);

	if (ncols != 7 || nrows < 2)
	{
		log("user specified htf table must have exactly seven columns and at least two rows");
		return false;
	}

	FILE *fp = fopen(file, "w");
	if (!fp) return false;

	fprintf(fp, "&\n");
	for (size_t r=0;r<nrows;r++)
		for (size_t c=0;c<ncols;c++)
			fprintf(fp,"%lg%c", RCINDEX(tab,ncols,r,c), (c<ncols-1)?' ':'\n');

	fclose(fp);
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
		fprintf(fp, "%d\n", tod[i]+1); // translate to 1-9 for TRNSYS
//		fprintf(fp, "%d\n", tod[i]);
	fclose(fp);
	return true;
}

int cm_trnbase::weather_file_type(const char *wf)
{
	std::string ext = util::lower_case( util::ext_only( wf ) );
	
	if (ext == "tm2") return 2;
	if (ext == "tm3") return 7;
	if (ext == "epw") return 3;
	if (ext == "swf") return 8;

	return 0;
}

std::string cm_trnbase::work_dir()
{
	return param_string("working_dir");
}

std::string cm_trnbase::data_file()
{
	return param_string("data_file");
}

void cm_trnbase::exec() throw(general_error)
{
	std::string wkdir = work_dir();
	std::string exe = param_string("trnsys_exe");
	std::string deck = deck_name();
	deck = "decks/" + deck; // for consistency with exelib structure
	
	if (!util::dir_exists( wkdir.c_str() ))
		util::mkdir( wkdir.c_str(), true );

	if (!util::dir_exists( wkdir.c_str() ))
		throw general_error("could not create working directory for simulation: " + wkdir);
	
	std::string list_file = wkdir + util::path_separator() + deck + ".lst";
	std::string log_file = wkdir + util::path_separator() + deck + ".log";
	std::string trd_file = wkdir + util::path_separator() + deck + ".trd";

	util::stdfile f( trd_file.c_str(), "w" );
	if (!f.ok())
		throw general_error("could not create write TRNSYS input file: " + trd_file);
	
	fprintf(f, "VERSION 16\n");
	fprintf(f, "*****************************************************************\n");
	fprintf(f, "*** TRNSYS input file generated by SSC for %s\n", deck.c_str());
	fprintf(f, "*** Copyright (c) 2011 National Renewable Energy Laboratory\n");
	fprintf(f, "*****************************************************************\n\n");

	write_include_file(f);

	fprintf(f, "\n\nCONSTANTS 10\n");
	for (int i=1;i<=10;i++)
		fprintf(f, "\tUDV%d=%lg\n", i, 
			as_double( util::format("trn_udv%d", i) ) );

	fprintf(f, "\n*** end include section ***\n\n");

	fprintf(f, "EQUATIONS 3\n");
	fprintf(f, "\tSTART=%lg\n", (double)param_number("trnsys_tmstart"));
	fprintf(f, "\tSTOP=%lg\n",  (double)param_number("trnsys_tmstop"));
	fprintf(f, "\tSTEP=%lg\n\n",(double)param_number("trnsys_tmstep"));

	
	std::string deck_src = util::path_only(exe) + util::path_separator() + deck + ".trdsrc";
	util::stdfile src( deck_src.c_str(), "r" );
	if (!src.ok()) throw general_error("error reading deck source file: " + deck_src);

	int c;
	while ( EOF != (c=fgetc(src)) )
		fputc( c, f );

	src.close();
	
	write_deck_end( f );

	f.close();

	pre_trnsys_call( );
	
	// main call to external process marshaller:  run trn.exe
	if (!extproc( "\"" + exe + "\" \"" + trd_file + "\"", util::path_only(exe) )) throw general_error("failed to run trnsys");

	assign("deck_file", var_data(trd_file));
	assign("log_file", var_data(log_file));

	update("Reading data...", 99);
	process_outputs( );
	update("Finished.", 100);
}

bool cm_trnbase::on_extproc_output( const std::string &text )
{
	double percent = atof( text.c_str() );
	
	if (percent == 0.0)	update( "Processing...", 0.0f, 0.0f );
	else update( "Simulating...", (float)percent, 0.0f );
	
	return true;
}

void cm_trnbase::save_data( output &data, 
		const char *col_name, 
		const char *var_name, 
		ssc_number_t scale,
		int check_num_values) throw( general_error )
{
	trndata_t *pd = data.lookup( col_name );
	if (!pd) throw general_error("could not retrieve output data: " + std::string(col_name));

	size_t n = pd->data.size();

	if ( check_num_values >= 0 
		&& check_num_values != (int)n )
		throw general_error( util::format("inconsistent number of data values in output column %s: %d (%d expected)",
			col_name, (int)n, check_num_values ) );

	if ( n > 1 )
	{
		ssc_number_t *vec = allocate( var_name, n );
		for (size_t i=0;i<n;i++) vec[i] = pd->data[i]*scale;
	}
	else
	{
		assign( var_name, var_data( (ssc_number_t) pd->data[0] ) );
	}
}


/* trnsys output file reader */

bool cm_trnbase::output::read(const char *fn, size_t expected_data_len)
{
	FILE *fp = fopen(fn, "r");
	if (!fp) return false;

	clear();

	size_t line_buf_len = 2048;

	std::string names, units;
	util::read_line(fp, names, (int)line_buf_len );
	util::read_line(fp, units, (int)line_buf_len );

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
	
	line_buf_len = ncols * 128; // should be sufficient room for a whole line (128 chars per line)
	char *buf = new char[ line_buf_len ];
	
	while ( 1 )
	{
		if (fgets(buf,(int)line_buf_len-1,fp) == 0)
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
			m_cols[ncol++]->data.push_back( (ssc_number_t)atof( dblbuf ) ); // convert number and save
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
		if (len == -1) len = (int)((*it)->data.size());
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
