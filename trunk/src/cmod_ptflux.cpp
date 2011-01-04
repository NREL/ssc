#include "core.h"

static var_info _cm_vtab_ptflux[] = {

/*   VARTYPE           DATATYPE         NAME                         LABEL                              UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,        SSC_NUMBER,      "latitude",                 "Latitude",                          "deg",    "",                      "PTFlux",      "*",                         "",                         "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "solar_multiple",           "Solar multiple",                    "",       "",                      "PTFlux",      "*",                         "POSITIVE",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "span_angle",               "Field span angle",                  "deg",    "",                      "PTFlux",      "*",                         "MIN=0,MAX=360",            "" },

	{ SSC_INPUT,        SSC_MATRIX,      "heliostat_field",          "Heliostat field matrix",            "",       "",                      "PTFlux",      "*",                         "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "round_heliostats",         "Use round heliostats",              "0/1",    "",                      "PTFlux",      "*",                         "BOOLEAN",                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "heliostat_width",          "Heliostat width",                   "m",      "",                      "PTFlux",      "*",                         "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "heliostat_height",         "Heliostat height",                  "m",      "",                      "PTFlux",      "*",                         "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "field_availability",       "Heliostat field availability",      "%",      "",                      "PTFlux",      "*",                         "MIN=0,MAX=100",            "" },
	{ SSC_INPUT,        SSC_NUMBER,      "ratio_reflect_to_profile", "Ratio reflected area to profile",   "frac",   "",                      "PTFlux",      "*",                         "MIN=0,MAX=1",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "mirror_reflectivity",      "Mirror reflectivity",               "frac",   "",                      "PTFlux",      "*",                         "MIN=0,MAX=1",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "max_hel_tower_dist_ratio", "Max helio-to-tower distance ratio", "frac", "",                        "PTFlux",      "*",                         "POSITIVE",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "min_hel_tower_dist_ratio", "Min helio-to-tower distance ratio", "frac", "",                        "PTFlux",      "*",                         "POSITIVE",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "image_error",              "Image error",                       "rad",    "",                      "PTFlux",      "*",                         "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "coating_absorptivity",     "Coating absorptivity",              "frac",   "",                      "PTFlux",      "*",                         "MIN=0,MAX=1",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "max_receiver_flux",        "Max receiver flux",                 "kWt/m2", "",                      "PTFlux",      "*",                         "",                         "" },
	
	{ SSC_INPUT,        SSC_NUMBER,      "receiver_type",            "Receiver type",                     "",       "0=external,1=cavity",   "PTFlux",      "*",                         "MIN=0,MAX=1,INTEGER",      "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cavity_aperture_width",    "Cavity aperture width",             "m",      "",                      "PTFlux",      "receiver_type=1",           "",                         "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cavity_aperture_hw_ratio", "Cavity height-to-width ratio",      "frac",   "",                      "PTFlux",      "receiver_type=1",           "MIN=0,MAX=1",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cavity_lip_height_ratio",  "Cavity lip-to-height ratio",        "frac",   "",                      "PTFlux",      "receiver_type=1",           "MIN=0,MAX=1",              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "exter_height",             "External receiver height",          "m",      "",                      "PTFlux",      "receiver_type=0",           "POSITIVE",                 "" },
	{ SSC_INPUT,        SSC_NUMBER,      "exter_diameter",           "External receiver diameter",        "m",      "",                      "PTFlux",      "receiver_type=0",           "POSITIVE"                  "" },
	{ SSC_INPUT,        SSC_NUMBER,      "tower_height",             "Tower height",                      "m",      "",                      "PTFlux",      "*",                         "MIN=0",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "design_gross_capacity",    "Plant gross capacity",              "MW",     "",                      "PTFlux",      "*",                         "MIN=0",                    "" },
	{ SSC_INPUT,        SSC_NUMBER,      "cycle_eff",                "Cycle efficiency",                  "frac",   "",                      "PTFlux",      "*",                         "MIN=0",                    "" },


	// outputs
	{ SSC_OUTPUT,        SSC_MATRIX,     "optieff_matrix",           "Optical efficiency matrix",         "",    "",                         "PTFlux",      "*",                         "",                         "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "optieff_azimuth",          "Efficiency array azimuth angles"    "",    "",                         "PTFlux",      "*",                         "",                         "" },
	{ SSC_OUTPUT,        SSC_ARRAY,      "optieff_zenith",           "Efficiency array zenith angles"     "",    "",                         "PTFlux",      "*",                         "",                         "" },
	

var_info_invalid };

static param_info _cm_params_ptflux[] = {
	/* TYPE,      NAME,       DEFAULT_VALUE,    DESCRIPTION */
	{ SSC_STRING, "ptgen_exe",     "",            "Path to PTGEN executable" },
	{ SSC_STRING, "working_dir",   "",            "Local working directory" },
	{ SSC_INVALID, NULL }  };


class cm_ptflux : public compute_module
{
private:
	bool m_success;

public:
	cm_ptflux()
	{
		m_success = false;
		add_var_info( _cm_vtab_ptflux );
		set_param_info( _cm_params_ptflux );
	}

	void exec( ) throw( general_error )
	{
		std::string ptexe = param_string("ptgen_exe");
		std::string wkdir = param_string("working_dir");

		if (!util::dir_exists( wkdir.c_str() ))
			util::mkdir( wkdir.c_str(), true );

		if (!util::dir_exists( wkdir.c_str() ))
			throw general_error("could not create working directory for simulation: " + wkdir);

		std::string incfile = wkdir + util::path_separator() + "SAM_Tower_DELSOL.inc";
		util::stdfile fp(incfile.c_str(), "w");
		if (!fp.ok()) throw general_error("could not write to delsol input file: " + incfile);

		fprintf(fp, "! PTGEN/DELSOL3 INPUT FILE\n");
		fprintf(fp, "&input_vars\n\n");
		fprintf(fp, "! ****** HELIOSTAT SOLAR FIELD ******\n");
		fprintf(fp, "h_helio = %lg\n", as_double("heliostat_height"));
		fprintf(fp, "w_helio = %lg\n", as_double("heliostat_width"));
		fprintf(fp, "Hel_dens = %lg\n", as_double("ratio_reflect_to_profile"));
		fprintf(fp, "mirror_reflectivity = %lg\n", as_double("mirror_reflectivity")*as_double("field_availability"));
		fprintf(fp, "helio_is_round = %d\n", as_boolean("round_heliostats") ? 1 : 0);
		fprintf(fp, "solar_mult = %lg\n", as_double("solar_multiple"));
		fprintf(fp, "RADMAX = %lg\n", as_double("max_hel_tower_dist_ratio"));
		fprintf(fp, "RADMIN = %lg\n", as_double("min_hel_tower_dist_ratio"));
		fprintf(fp, "Image_error = %lg\n", as_double("image_error"));
		fprintf(fp, "field_angle = %lg\n", as_double("span_angle"));
		fprintf(fp, "\n");
	
		int rec_type = ((int)as_integer("receiver_type"))==1 ? 2 : 0;
		fprintf(fp, "! ****** RECEIVER & TOWER ******\n");
		fprintf(fp, "rec_type = %d\n", rec_type);
		fprintf(fp, "h_lip_spec = %lg\n", rec_type == 2 ? as_double("cavity_lip_height_ratio") : 0.0);
		fprintf(fp, "Rec_coating_abs = %lg\n", as_double("coating_absorptivity"));
		fprintf(fp, "max_flux = %lg\n", as_double("max_receiver_flux"));
		fprintf(fp, "Rec_min_d = 0.0\n");
		fprintf(fp, "Rec_max_d = 0.0\n");
		fprintf(fp, "Rec_nopt = 0\n");
		fprintf(fp, "h_tower_min = 0.0\n");
		fprintf(fp, "h_tower_max = 0.0\n");
		fprintf(fp, "h_tower_nopt = 0\n");
		fprintf(fp, "htw_rec_min = 0.0\n");
		fprintf(fp, "htw_rec_max = 0.0\n");
		fprintf(fp, "htw_rec_nopt = 0\n");
		fprintf(fp, "\n");

		fprintf(fp, "! ****** POWER BLOCK ******\n");
		fprintf(fp, "P_cycle_design = %lg\n", as_double("design_gross_capacity") );
		fprintf(fp, "Eff_cycle_design = %lg\n", as_double("cycle_eff") );
		fprintf(fp, "\n");

		fprintf(fp, "! ****** COSTS ******\n");
		fprintf(fp, "cost_heliostat = 0.0\n" );
		fprintf(fp, "cost_land = 0.0\n" );
		fprintf(fp, "cost_fixed = 0.0\n");
		fprintf(fp, "cost_tower_fixed = 0.0\n");
		fprintf(fp, "cost_tower_exp = 0.0\n");
		fprintf(fp, "cost_receiver_ref = 0.0\n");
		fprintf(fp, "cost_receiver_exp = 0.0\n");
		fprintf(fp, "cost_receiver_area = 0.0\n");
		fprintf(fp, "Fin_cont = 0.0\n");
		fprintf(fp, "Fin_rhom = 1.5\n");
		fprintf(fp, "Fin_rnhom = 0.0\n");
		fprintf(fp, "\n");

		fprintf(fp, "! ****** FINANCIALS ******\n");
		fprintf(fp, "Fin_spts = 0\n");
		fprintf(fp, "Fin_ext = 16\n");
		fprintf(fp, "Fin_esc = 0\n");
		fprintf(fp, "Fin_rinf = 2.5\n" ); // inflation rate
		fprintf(fp, "Fin_nytcon = 0\n");
		fprintf(fp, "Fin_afdc = 0\n");
		fprintf(fp, "Fin_disrt = 8.0\n" ); // discount rate
		fprintf(fp, "Fin_pti = 0.0\n" ); // property tax
		fprintf(fp, "Fin_tc = 10\n");
		fprintf(fp, "Fin_tr = 35.0\n"); // federal tax
		fprintf(fp, "Fin_fdebt = 40.0\n" ); // debt fraction
		fprintf(fp, "Fin_rdebt = 8.0\n"); // loan rate
		fprintf(fp, "Fin_roe = 15.0\n" ); // min_irr
		fprintf(fp, "Fin_ndep = 0\n");
		fprintf(fp, "Fin_nyop = 30\n" ); // analysis_period
		fprintf(fp, "Is_macrs = 1\n");
		fprintf(fp, "plant_lattitude = %lg\n", as_double("latitude") );
		fprintf(fp, "\n");
	
		fprintf(fp, "! ****** OTHER ******\n");
		fprintf(fp, "udayin = -100\n");
		fprintf(fp, "utimein = -100\n");

		size_t nrows, ncols;
		ssc_number_t *uf = as_matrix("heliostat_field", &nrows, &ncols);
		bool zonal = (ncols > 2);

		fprintf(fp, "iuserf = %d\n", zonal?2:3);
		fprintf(fp, "opt_only = 0\n");
		fprintf(fp, "nazm = %d\n", (int) ncols );
		fprintf(fp, "nrad = %d\n", (int) nrows );

		bool is_cavity = (as_integer("receiver_type")==1);
		double h_rec_spec = (is_cavity ? as_double("cavity_aperture_hw_ratio") : as_double("exter_height"));
		double rec_d_spec = (is_cavity ? as_double("cavity_aperture_width") : as_double("exter_diameter"));

		fprintf(fp, "h_rec_spec = %lg\n", h_rec_spec);
		fprintf(fp, "h_tow_spec = %lg\n", as_double("tower_height"));
		fprintf(fp, "Rec_d_spec = %lg\n", rec_d_spec);
		fprintf(fp, "/\n");

		std::string uffile = wkdir + util::path_separator() + "user_field.dat";
		if (!fp.open(uffile.c_str(), "w")) throw general_error("could not write to delsol field file: " + uffile);

		size_t i=0;
		for (size_t r=0;r<nrows;r++)
			for (size_t c=0;c<ncols;c++)
				fprintf(fp, "%lg%c", (double)uf[i++], (c < ncols-1) ? ' ' : '\n');
		
		fp.close();

		// run ptgen
		m_success = false;
		if (!extproc( "\"" + ptexe + "\"", wkdir )) throw general_error("failed to start flux map generation: " + ptexe);

		if (!m_success) throw general_error("ptgen failed to signal successful completion of flux map generation");
		
		// now read output

		// parse efficiency array file
		if (!fp.open( wkdir + util::path_separator() + "eff_array.dat", "r" ))
			throw general_error("could not parse efficiency array file: eff_array.dat");

		std::string line0, line1, buf;
		util::read_line(fp, line0);
		util::read_line(fp, line1);
		
		var_data azivec, zenvec;
		if (!var_data::parse( SSC_ARRAY, line0, azivec )) throw general_error("could not parse azimuth angle vector");
		if (!var_data::parse( SSC_ARRAY, line1, zenvec )) throw general_error("could not parse zenith angle vector");

		int nazi = azivec.num.length();
		int nzen = zenvec.num.length();
		if ( nazi <= 0 || nzen <= 0) throw general_error("failed to determine efficiency array size (nzen, nazi)");
		
		util::matrix_t<ssc_number_t> opt;
		opt.resize_fill( nzen, nazi, 0.0 );
		for (int c=0;c<nazi;c++)
		{
			for (int r=0;r<nzen;r++)
			{
				util::read_line(fp,buf);
				opt.at(r,c) = (ssc_number_t)atof(buf.c_str());
			}
		}

		assign( "optieff_matrix", var_data( opt.data(), opt.nrows(), opt.ncols() ) );
		assign( "optieff_azimuth", azivec );
		assign( "optieff_zenith", zenvec );
		
		fp.close();

	}

	int chcount( const std::string &s, char c )
	{
		int n = 0;
		for (std::string::size_type i=0;i<s.length();i++)
			if (s[i] == c) n++;

		return n;
	}
		
	virtual bool on_extproc_output( const std::string &text )
	{
		double percent = atof( text.c_str() );
	
		if (percent == 0.0)	update( "Processing...", 0.0f, 0.0f );
		else update( "Generating flux maps...", (float)percent, 0.0f );

		if (text.find( "Flux map generation successful" ) != std::string::npos) m_success = true;
	
		return true;//false;
	}
};


DEFINE_MODULE_ENTRY( ptflux, "Power tower flux map calculator (PTGEN)", 1 )
