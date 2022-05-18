#include "core.h"

#include "ud_power_cycle_jdm.h"

#include <iostream>
#include <fstream>

static var_info _cm_vtab_test_ud_power_cycle_jdm[] = {

	/*   VARTYPE   DATATYPE         NAME               LABEL                                          UNITS     META  GROUP REQUIRED_IF CONSTRAINTS         UI_HINTS*/
	{SSC_INPUT, SSC_NUMBER, "q_pb_design", "Design point power block thermal power", "MWt", "", "", "", "", ""},
    {SSC_INPUT, SSC_NUMBER, "n_T_htf", "Number of levels for HTF", "-", "", "", "*", "", ""},
    
	{SSC_OUTPUT, SSC_NUMBER, "W_dot_fossil", "Electric output with no solar contribution", "MWe", "", "", "", "", ""},
    {SSC_OUTPUT, SSC_NUMBER, "W_dot_ND_calc", "Non-dimensional Work Output", "-", "", "", "", "", ""},

	var_info_invalid};

class cm_test_ud_power_cycle_jdm : public compute_module
{
public:

	cm_test_ud_power_cycle_jdm()
	{
		add_var_info(_cm_vtab_test_ud_power_cycle_jdm);
	}

	void exec() override
	{
        // Create a log file
        std::ofstream logfile;
        logfile.open("dev_log.txt");

        logfile << "Start program.\n";

        // Setup independent variable combinations
            // HTF inlet temperature
        double T_htf_des = 560.0;   //[C]
        double T_htf_low = 545.0;   //[C]
        double T_htf_high = 575.0;  //[C]
        size_t n_T_htf = as_integer("n_T_htf");// = 7;
        double dT_T_htf = (T_htf_high - T_htf_low) / (double)(n_T_htf - 1);
        std::vector<double> T_htf_levels = std::vector<double>{ T_htf_low,T_htf_des,T_htf_high };
        size_t n_levels_Thtf = T_htf_levels.size();

            // HTF mass flow rate
        double m_dot_htf_ND_des = 1.0;     //[-] By definition, ND design mass flow is 1.0
        double m_dot_htf_ND_low = 0.5;     //[-]
        double m_dot_htf_ND_high = 1.5;    //[-]
        double m_dot_htf_ND_add = 0.75; //[-]
        size_t n_m_dot_htf_ND = 10;
        double dT_m_dot_htf_ND = (m_dot_htf_ND_high - m_dot_htf_ND_low) / (double)(n_m_dot_htf_ND - 1);
        std::vector<double> m_dot_htf_ND_levels = std::vector<double>{ m_dot_htf_ND_low, 0.75,m_dot_htf_ND_des ,1.25,m_dot_htf_ND_high };
        //std::vector<double> m_dot_htf_ND_levels = std::vector<double>{ m_dot_htf_ND_low, m_dot_htf_ND_des, m_dot_htf_ND_high };
        size_t n_levels_mdot = m_dot_htf_ND_levels.size();

            // Ambient temperature
        double T_amb_des = 25.0;    //[C]
        double T_amb_low = 0.0;     //[C]
        double T_amb_high = 50.0;   //[C]
        size_t n_T_amb = 10;
        double dT_T_amb = (T_amb_high - T_amb_low) / (double)(n_T_amb - 1);
        std::vector<double> T_amb_levels = std::vector<double>{ T_amb_low,T_amb_des,T_amb_high };
        size_t n_levels_Tamb = T_amb_levels.size();

        logfile << dT_T_htf << "," << dT_m_dot_htf_ND << "," << dT_T_amb << "\n";

        logfile << "Set up initial data.\n";

        size_t n_levels = 3;    // changing levels would require generalizing interpolation routines
        size_t n_total =  n_levels_mdot * n_T_htf + n_levels_Tamb * n_m_dot_htf_ND + n_levels_Thtf * n_T_amb;
        logfile << n_levels_Thtf << "," << n_T_htf << "," << n_levels_mdot << "," << n_m_dot_htf_ND << "," << n_levels_Tamb << "," << n_T_amb << "\n";
        util::matrix_t<double> udpc_data_full(n_total, C_ud_power_cycle_jdm::E_COL_M_H2O + 1, std::numeric_limits<double>::quiet_NaN());

        logfile << "Create udpc matrix.\n";

        size_t k = 0;
        for (size_t i = 0; i < n_levels_mdot; i++) {
            for (size_t j = 0; j < n_T_htf; j++) {
                udpc_data_full(k,C_ud_power_cycle_jdm::E_COL_T_HTF) = T_htf_low + j*dT_T_htf;
                udpc_data_full(k,C_ud_power_cycle_jdm::E_COL_M_DOT) = m_dot_htf_ND_levels[i];
                udpc_data_full(k,C_ud_power_cycle_jdm::E_COL_T_AMB) = T_amb_des;

                k++;
            }
        }

        //logfile << "Write first set of data into udpc matrix.\n";
        for (size_t i = 0; i < n_levels_Tamb; i++) {
            for (size_t j = 0; j < n_m_dot_htf_ND; j++) {
                udpc_data_full(k, C_ud_power_cycle_jdm::E_COL_T_HTF) = T_htf_des;
                udpc_data_full(k, C_ud_power_cycle_jdm::E_COL_M_DOT) = m_dot_htf_ND_low + j*dT_m_dot_htf_ND;
                udpc_data_full(k, C_ud_power_cycle_jdm::E_COL_T_AMB) = T_amb_levels[i];
                k++;
            }
        }

        for (size_t i = 0; i < n_levels_Thtf; i++) {
            for (size_t j = 0; j < n_T_amb; j++) {
                udpc_data_full(k, C_ud_power_cycle_jdm::E_COL_T_HTF) = T_htf_levels[i];
                udpc_data_full(k, C_ud_power_cycle_jdm::E_COL_M_DOT) = m_dot_htf_ND_des;
                udpc_data_full(k, C_ud_power_cycle_jdm::E_COL_T_AMB) = T_amb_low + j*dT_T_amb;
                k++;
            }
        }

        logfile << "Enter variable ranges into matrix.\n";

        // Check that index counter matches expected number of inputs
        if (k != n_total) {
            logfile << "udpc setup index counter final value does not match expected: " << k  << "," << n_total;
            throw(C_csp_exception("udpc setup index counter final value does not match expected"));
        }

        // Add extra data to test filter
        /*
        bool is_test_extra_data = true;
        if (is_test_extra_data) {
            size_t n_extra = 1;
            udpc_data_full.resize_preserve(n_total + n_extra, C_ud_power_cycle_jdm::E_COL_M_H2O + 1, std::numeric_limits<double>::quiet_NaN());
            udpc_data_full(n_total,C_ud_power_cycle_jdm::E_COL_T_HTF) = T_htf_low + 0.5*dT_T_htf;
            udpc_data_full(n_total,C_ud_power_cycle_jdm::E_COL_M_DOT) = m_dot_htf_ND_low + 0.5*dT_m_dot_htf_ND;
            udpc_data_full(n_total,C_ud_power_cycle_jdm::E_COL_T_AMB) = T_amb_low + 0.5*dT_T_amb;
        }
        */


        // Use example endo-reversible cycle model to calculate cycle performance
        double adjust = 0.0;
        C_endo_rev_cycle c_cycle(T_htf_des + adjust, T_amb_des + adjust);

        logfile << "Nrows: " << udpc_data_full.nrows() << "  ," << n_total; 

        for (size_t i = 0; i < udpc_data_full.nrows(); i++) {
            logfile << "Iteration #  " << i << "\n";
            c_cycle.performance(udpc_data_full(i, C_ud_power_cycle_jdm::E_COL_T_HTF),
                udpc_data_full(i, C_ud_power_cycle_jdm::E_COL_M_DOT),
                udpc_data_full(i, C_ud_power_cycle_jdm::E_COL_T_AMB),
                udpc_data_full(i, C_ud_power_cycle_jdm::E_COL_W_CYL),
                udpc_data_full(i, C_ud_power_cycle_jdm::E_COL_Q_CYL),
                udpc_data_full(i, C_ud_power_cycle_jdm::E_COL_W_COOL),
                udpc_data_full(i, C_ud_power_cycle_jdm::E_COL_M_H2O));
        }

        logfile << "Calculate performance for each variable set using endo-reversible class.\n";

        // Write out data for testing what script is doing
        std::ofstream outfile1;
        outfile1.open("test_output.csv");

        outfile1 << "Row number, T_htf, mdot, T_amb, Power\n";
        // Write out a list of the coordinates. Not a clever way to do this.
        for (size_t i = 0; i < udpc_data_full.nrows(); i++) {
            
            outfile1 << udpc_data_full(i, C_ud_power_cycle_jdm::E_COL_T_HTF) << "," << udpc_data_full(i, C_ud_power_cycle_jdm::E_COL_M_DOT) << "," << udpc_data_full(i, C_ud_power_cycle_jdm::E_COL_T_AMB) << "," << udpc_data_full(i,C_ud_power_cycle_jdm::E_COL_W_CYL) << "\n";
        }
        outfile1 << "\n";
        outfile1.close();
        logfile << "Write data out into a file.\n";

        
        // Initialize UDPC model with cycle performance data table
        C_ud_power_cycle_jdm c_udpc;
        int n_T_htf_udpc_calc, n_T_amb_udpc_calc, n_m_dot_udpc_calc;
        double T_htf_ref_udpc_calc;
        double T_amb_ref_udpc_calc;
        double m_dot_htf_ref_udpc_calc;
        std::vector<double> Y_at_T_htf_ref, Y_at_T_amb_ref, Y_at_m_dot_htf_ND_ref, Y_avg_at_refs;

        // Define n_variable here 
        n_T_htf_udpc_calc = n_T_htf;
        n_T_amb_udpc_calc = n_T_amb;
        n_m_dot_udpc_calc = n_m_dot_htf_ND;

        // Define design, low and high here, but -- again -- remove later
        T_htf_ref_udpc_calc = T_htf_des;
        m_dot_htf_ref_udpc_calc = m_dot_htf_ND_des;
        T_amb_ref_udpc_calc = T_amb_des;
        
        // Have added T_htf_levels,m_dot_htf_ND_levels,T_amb_levels to function call for now
        c_udpc.init(udpc_data_full,
            n_T_htf_udpc_calc, n_T_amb_udpc_calc, n_m_dot_udpc_calc,
            T_htf_ref_udpc_calc, T_amb_ref_udpc_calc, m_dot_htf_ref_udpc_calc, 
            Y_at_T_htf_ref, Y_at_T_amb_ref, Y_at_m_dot_htf_ND_ref, Y_avg_at_refs,
            T_htf_levels,m_dot_htf_ND_levels,T_amb_levels);

        

        
        // Sample UPDC model
            // at design point
        double W_dot_ND_calc = c_udpc.get_W_dot_gross_ND(T_htf_des-10, T_amb_des+5, 0.8);
        int Nsamp = 100;
        double mdotS = 0;
        double Wact, Q_cyl, W_cool, H2O, errS;
        // Create a results file
        std::ofstream resfile;
        resfile.open("test_results_new.txt");
        resfile << "Mdot     ,      W (actual)       ,       W (regression)       ,       Error (%)\n";

        for (size_t i = 0; i < Nsamp; i++) {
            mdotS = 0.25 + i * (1.75 - 0.25) / double(Nsamp - 1);
            //mdotS = 0.25;
            W_dot_ND_calc = c_udpc.get_W_dot_gross_ND(T_htf_des - 10, T_amb_des - 5, mdotS);
            
            // Results from original model
            c_cycle.performance(T_htf_des - 10, mdotS, T_amb_des - 5, Wact, Q_cyl, W_cool, H2O);
            errS = 100 * (Wact - W_dot_ND_calc) / Wact;
            resfile << mdotS << "," << Wact << "," << W_dot_ND_calc << "," << errS << "\n";
        }
        resfile.close();
        
        assign("W_dot_ND_calc", W_dot_ND_calc);     //[kWe]
        /*
        // Now let's be horrible and sample the original model and the regression model over a large number of points
        size_t const Nsamp = 21; // number of samples PER VARIABLE.

        // array to contain sample data points
        double sample_points[Nsamp][Nsamp][Nsamp][3];
        double ThtfS, TambS, mdotS;

        for (size_t i = 0; i < Nsamp; i++) {
            ThtfS = T_htf_low + i* (T_htf_high - T_htf_low) / (double)(Nsamp - 1);
            for (size_t j = 0; j < Nsamp; j++) {
                mdotS = m_dot_htf_ND_low + j * (m_dot_htf_ND_high - m_dot_htf_ND_low) / (double)(Nsamp - 1);
                for (size_t k = 0; k < Nsamp; k++) {
                    TambS = T_amb_low + k * (T_amb_high - T_amb_low) / (double)(Nsamp - 1);

                    // Assign points
                    sample_points[i][j][k][0] = ThtfS;
                    sample_points[i][j][k][1] = mdotS;
                    sample_points[i][j][k][2] = TambS;
                }
            }
        }

        // Now calculate the work out at each of these points using the original model and the regression model
        double endo_work[Nsamp][Nsamp][Nsamp];
        double reg_work[Nsamp][Nsamp][Nsamp];
        double err_work[Nsamp][Nsamp][Nsamp]; // Also calculate the error
        double Q_cyl, W_cool, H2O;
        for (size_t i = 0; i < Nsamp; i++) {
            for (size_t j = 0; j < Nsamp; j++) {
                for (size_t k = 0; k < Nsamp; k++) {
                    // Results from original model
                    c_cycle.performance(sample_points[i][j][k][0],
                        sample_points[i][j][k][1],
                        sample_points[i][j][k][2],
                        endo_work[i][j][k],
                        Q_cyl, W_cool, H2O);// not really interested in these as they're constant. Can I skip?

                    // Results from regression model
                    reg_work[i][j][k] = c_udpc.get_W_dot_gross_ND(sample_points[i][j][k][0],
                        sample_points[i][j][k][2],
                        sample_points[i][j][k][1]);

                    err_work[i][j][k] = 100 * (endo_work[i][j][k] - reg_work[i][j][k]) / endo_work[i][j][k];
                }
            }
        }

        // Write out data
        std::ofstream outfile1, outfile2, outfile3, outfile4;
        outfile1.open("coords.csv");
        outfile2.open("actual_data.csv");
        outfile3.open("reg_data.csv");
        outfile4.open("error_data.csv");

        // Write out a list of the coordinates. Not a clever way to do this.
        for (int i = 0; i < Nsamp; i++) {
            ThtfS = T_htf_low + i * (T_htf_high - T_htf_low) / (double)(Nsamp - 1);
            mdotS = m_dot_htf_ND_low + i * (m_dot_htf_ND_high - m_dot_htf_ND_low) / (double)(Nsamp - 1);
            TambS = T_amb_low + i * (T_amb_high - T_amb_low) / (double)(Nsamp - 1);
            outfile1 << ThtfS << "," << mdotS << "," << TambS << "\n";
        }

        for (int i = 0; i < Nsamp; i++) {
            for (int j = 0; j < Nsamp; j++) {
                for (int k = 0; k < Nsamp; k++) {

                    outfile2 << endo_work[j][k][i] << ",";
                    outfile3 << reg_work[j][k][i] << ",";
                    outfile4 << err_work[j][k][i] << ",";

                }
                outfile2 << "\n";
                outfile3 << "\n";
                outfile4 << "\n";
            }
            //outfile2 << "\n";
            //outfile3 << "\n";
            //outfile4 << "\n";
        }


        outfile1.close();
        outfile2.close();
        outfile3.close();
        outfile4.close();

        double abce = 1.23;

        */
        logfile.close();
		/*C_ud_power_cycle c_pc;

		c_pc.init(a_table, a_ref, a_low, a_high,
				b_table, b_ref, b_low, b_high,
				c_table, c_ref, c_low, c_high);

		int n_test = N_runs*N_runs*N_runs;
		
		std::vector<double> Y_actual(n_test);
		std::vector<double> Y_reg(n_test);
		std::vector<double> E_reg_less_act(n_test);

		double max_err = -1.0;

		for(int i = 0; i < N_runs; i++)
		{
			for(int j = 0; j < N_runs; j++)
			{
				for(int k = 0; k < N_runs; k++)
				{
					int index = i*N_runs*N_runs + j*N_runs + k;

					Y_actual[index] = three_var_eqn(a_table(i,0),b_table(j,0),c_table(k,0));

					Y_reg[index] = c_pc.get_W_dot_gross_ND(a_table(i,0),b_table(j,0),c_table(k,0))*Y_ref;

					E_reg_less_act[index] = (Y_reg[index] - Y_actual[index])/fmax(Y_actual[index],0.0001);

					if(fabs(E_reg_less_act[index]) > max_err)
					{
						max_err = fabs(E_reg_less_act[index]);
					}
				}
			}
		}*/
	}

    class C_endo_rev_cycle
    {
    public:

        double m_T_htf_hot_des;     //[C]
        double m_T_amb_des;         //[C]        
        double m_eta_endo_des;

        C_endo_rev_cycle(double T_htf_hot_des /*C*/, double T_amb_des /*-*/)
        {
            m_T_htf_hot_des = T_htf_hot_des;
            m_T_amb_des = T_amb_des;

            m_eta_endo_des = eta_endo(m_T_htf_hot_des, m_T_amb_des);
        }

        double eta_endo(double T_htf_hot /*C*/, double T_amb /*C*/)
        {
            double T_htf_hot_K = T_htf_hot + 273.15;
            double T_amb_K = T_amb + 273.15;

            return 1.0 - sqrt(T_amb_K / T_htf_hot_K);
        }

        void performance(double T_htf_hot /*C*/, double m_dot_htf_ND /*-*/, double T_amb /*C*/,
            double& W_dot_gross_ND, double& Q_dot_ND, double& W_dot_cooling_ND, double& m_dot_water_ND)
        {
            // Set constant cooling and water
            W_dot_cooling_ND = 1.0;
            m_dot_water_ND = 1.0;

            // Assume heat rate proportional to mass flow
            // And no ambient temperature dependence
            Q_dot_ND = m_dot_htf_ND;

            // Calculate new endo-reversible efficiency and adjust for part-load
            double eta_temp = eta_endo(T_htf_hot, T_amb);
            double eta_pl = pow(1 - abs(1-Q_dot_ND), 0.2);
            double eta = eta_temp * eta_pl;

            // calculate power by scaling by ratio of calculated and design endo-reversible efficiencies
            // eta / eta_des = (W_dot_gross_ND / Q_dot_ND) / (1.0 / 1.0)
            W_dot_gross_ND = eta / m_eta_endo_des * Q_dot_ND;
        }
    };

	double three_var_eqn(double a, double b, double c)
	{
		return a*pow(b,1.24) - a/pow(c,0.55) + b/(2*c+1.0)*a;
		//return 1.0;
	}

};

DEFINE_MODULE_ENTRY(test_ud_power_cycle_jdm, "Test user-defined power cylce model with JDM mods", 0)
