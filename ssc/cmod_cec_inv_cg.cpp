#include "core.h"
#include <algorithm>
#include <sstream>


  
static var_info vtab_cec_inv_cg[] = {

/*   VARTYPE           DATATYPE         NAME                         LABEL                                           UNITS     META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT, SSC_NUMBER, "cec_inv_cg_power_units", "Sample data units for power output", "0=W,1=kW", "", "", "?=0", "INTEGER,MIN=0,MAX=1", "" },
	// each sample has 18x3 entries:
	// 6 output power percentages 10%, 20%, 30%, 50%, 75%, 100% of rated power
	// 3 voltages Vmin, Vnom, Vmax that the 6 output powers measured at
	// for a total of 18 (=6x3) rows
	// 3 measured values for each row - Output Power, Input Voltage and Efficiency
	{ SSC_INPUT, SSC_MATRIX, "cec_inv_cg_test_samples", "Sample data", "", "", "", "", "", "" },
	
	/* from pvsamv1
		{ SSC_INPUT,        SSC_NUMBER,      "mppt_low_inverter",                           "Minimum inverter MPPT voltage window",                    "Vdc",     "",                     "pvsamv1",       "",                    "?=0",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "mppt_hi_inverter",                            "Maximum inverter MPPT voltage window",                    "Vdc",     "",                     "pvsamv1",       "",                    "?=0",                              "" },

	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_c0",                                  "Curvature between ac-power and dc-power at ref",          "1/W",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_c1",                                  "Coefficient of Pdco variation with dc input voltage",     "1/V",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_c2",                                  "Coefficient of Pso variation with dc input voltage",      "1/V",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_c3",                                  "Coefficient of Co variation with dc input voltage",       "1/V",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_paco",                                "AC maximum power rating",                                 "Wac",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_pdco",                                "DC input power at which ac-power rating is achieved",     "Wdc",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_pnt",                                 "AC power consumed by inverter at night",                  "Wac",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_pso",                                 "DC power required to enable the inversion process",       "Wdc",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_vdco",                                "DC input voltage for the rated ac-power rating",          "Vdc",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	{ SSC_INPUT,        SSC_NUMBER,      "inv_snl_vdcmax",                              "Maximum dc input operating voltage",                      "Vdc",     "",                     "pvsamv1",       "inverter_model=0",                    "",                              "" },
	*/

	// intermediate outputs for testing and validation
	{ SSC_OUTPUT, SSC_MATRIX, "cec_inv_cg_J87_L91", "Excel cells J87:L91", "", "", "", "*", "", "" },


	// outputs Pdco, Vdco, Pso, c0, c1, c2, c3
	{ SSC_OUTPUT, SSC_NUMBER, "cec_inv_cg_Pdco", "CEC generated Pdco", "Wac", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "cec_inv_cg_Vdco", "CEC generated Vdco", "Vdc", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "cec_inv_cg_Pso", "CEC generated Pso", "Wdc", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "cec_inv_cg_c0", "CEC generated c0", "1/W", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "cec_inv_cg_c1", "CEC generated c1", "1/V", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "cec_inv_cg_c2", "CEC generated c2", "1/V", "", "", "*", "", "" },
	{ SSC_OUTPUT, SSC_NUMBER, "cec_inv_cg_c3", "CEC generated c3", "1/V", "", "", "*", "", "" },

	var_info_invalid };



class cm_cec_inv_cg : public compute_module
{
private:

public:
	cm_cec_inv_cg()
	{
		add_var_info( vtab_cec_inv_cg);
	}

	void exec( ) throw( general_error )
	{
		size_t count, i, j, nrows, ncols; 
		// 6 columns period, tier, max usage, max usage units, buy, sell
		ssc_number_t *cec_inv_cg_test_samples_in = as_matrix("cec_inv_cg_test_samples", &nrows, &ncols);
		if (nrows != 18)
		{
			std::ostringstream ss;
			ss << "The samples table must have 18 rows. Number of rows in samples table provided is " << nrows << " rows.";
			throw exec_error("cec_inv_cg", ss.str());
		}
		if ((ncols % 3) != 0)
		{
			std::ostringstream ss;
			ss << "The samples table must have number of columns divisible by 3. Number of columns in samples table provided is " << ncols << " columns.";
			throw exec_error("cec_inv_cg", ss.str());
		}
		size_t num_samples = ncols / 3;
		size_t columns_per_sample = 3;
		util::matrix_t<float> cec_inv_cg_test_samples(nrows, ncols);
		cec_inv_cg_test_samples.assign(cec_inv_cg_test_samples_in, nrows, ncols);

		util::matrix_t<ssc_number_t> &cec_inv_cg_J87_L91 = allocate_matrix("cec_inv_cg_J87_L91", 5, 3);
		ssc_number_t vdc = 0;
		for (size_t k = 0; k < cec_inv_cg_J87_L91.ncols(); k++)
			cec_inv_cg_J87_L91.at(0, k) = 0.0;

		for (j = 0; j < num_samples; j++)
		{
			for (i = 0; i < cec_inv_cg_test_samples.nrows(); i++)
			{
				vdc = cec_inv_cg_test_samples.at(i, j*columns_per_sample + 1);
				if (i < 6) // Vmin
					cec_inv_cg_J87_L91.at(0, 0) += vdc;
				else if (i < 12) // Vnom
					cec_inv_cg_J87_L91.at(0, 1) += vdc;
				else // Vmax
					cec_inv_cg_J87_L91.at(0, 2) += vdc;
			}
		}
		
		for (size_t k = 0; k < cec_inv_cg_J87_L91.ncols(); k++)
			cec_inv_cg_J87_L91.at(0, k) /= (6 * num_samples);

		// Vdc-Vnom
		for (size_t k = 0; k < cec_inv_cg_J87_L91.ncols(); k++)
			cec_inv_cg_J87_L91.at(1, k) = cec_inv_cg_J87_L91.at(0, k) - cec_inv_cg_J87_L91.at(0, 1);
		
		// vdco is the average of Vnom of all samples column 2 and rows 7 through 12
		assign("cec_inv_cg_Vdco", (var_data)cec_inv_cg_J87_L91.at(0, 1));



		assign("cec_inv_cg_Pdco", (var_data)0);
		assign("cec_inv_cg_Pso", (var_data)0);
		assign("cec_inv_cg_c0", (var_data)0);
		assign("cec_inv_cg_c1", (var_data)0);
		assign("cec_inv_cg_c2", (var_data)0);
		assign("cec_inv_cg_c3", (var_data)0);
	}


};

DEFINE_MODULE_ENTRY( cec_inv_cg, "CEC Inverter Coefficient Generator", 1 );


