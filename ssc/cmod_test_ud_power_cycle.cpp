#include "core.h"

#include "ud_power_cycle.h"

static var_info _cm_vtab_test_ud_power_cycle[] = {

	/*   VARTYPE   DATATYPE         NAME               LABEL                                          UNITS     META  GROUP REQUIRED_IF CONSTRAINTS         UI_HINTS*/
	{SSC_INPUT, SSC_NUMBER, "q_pb_design", "Design point power block thermal power", "MWt", "", "", "", "", ""},

	{SSC_OUTPUT, SSC_NUMBER, "W_dot_fossil", "Electric output with no solar contribution", "MWe", "", "", "", "", ""},

	var_info_invalid};

class cm_test_ud_power_cycle : public compute_module
{
public:

	cm_test_ud_power_cycle()
	{
		add_var_info(_cm_vtab_test_ud_power_cycle);
	}

	void exec() override
	{
        // Setup independent variable combinations
            // HTF inlet temperature
        double T_htf_des = 565.0;   //[C]
        double T_htf_low = 545.0;   //[C]
        double T_htf_high = 575.0;  //[C]
        size_t n_T_htf = 7;
        double dT_T_htf = (T_htf_high - T_htf_low) / (double)(n_T_htf - 1);
        std::vector<double> T_htf_levels = std::vector<double>{ T_htf_low, T_htf_des, T_htf_high };

            // HTF mass flow rate
        double m_dot_htf_ND_des = 1.0;     //[-] By definition, ND design mass flow is 1.0
        double m_dot_htf_ND_low = 0.5;     //[-]
        double m_dot_htf_ND_high = 1.1;    //[-]
        size_t n_m_dot_htf_ND = 13;
        double dT_m_dot_htf_ND = (m_dot_htf_ND_high - m_dot_htf_ND_low) / (double)(n_m_dot_htf_ND - 1);
        std::vector<double> m_dot_htf_ND_levels = std::vector<double>{ m_dot_htf_ND_low, m_dot_htf_ND_des, m_dot_htf_ND_high };

            // Ambient temperature
        double T_amb_des = 35.0;    //[C]
        double T_amb_low = 0.0;     //[C]
        double T_amb_high = 45.0;   //[C]
        size_t n_T_amb = 10;
        double dT_T_amb = (T_amb_high - T_amb_low) / (double)(n_T_amb - 1);
        std::vector<double> T_amb_levels = std::vector<double>{ T_amb_low, T_amb_des, T_amb_high };

        size_t n_levels = 3;    // changing levels would require generalizing interpolation routines
        size_t n_total = n_levels * (n_T_htf + n_m_dot_htf_ND + n_T_amb);
        util::matrix_t<double> udpc_data_full(n_total, C_ud_power_cycle::E_COL_M_H2O + 1, std::numeric_limits<double>::quiet_NaN());

        size_t k = 0;
        for (size_t i = 0; i < n_levels; i++) {
            for (size_t j = 0; j < n_T_htf; j++) {
                udpc_data_full(k,C_ud_power_cycle::E_COL_T_HTF) = T_htf_low + j*dT_T_htf;
                udpc_data_full(k,C_ud_power_cycle::E_COL_M_DOT) = m_dot_htf_ND_levels[i];
                udpc_data_full(k,C_ud_power_cycle::E_COL_T_AMB) = T_amb_des;
                k++;
            }
        }

        for (size_t i = 0; i < n_levels; i++) {
            for (size_t j = 0; j < n_m_dot_htf_ND; j++) {
                udpc_data_full(k, C_ud_power_cycle::E_COL_T_HTF) = T_htf_des;
                udpc_data_full(k, C_ud_power_cycle::E_COL_M_DOT) = m_dot_htf_ND_low + j*dT_m_dot_htf_ND;
                udpc_data_full(k, C_ud_power_cycle::E_COL_T_AMB) = T_amb_levels[i];
                k++;
            }
        }

        for (size_t i = 0; i < n_levels; i++) {
            for (size_t j = 0; j < n_T_amb; j++) {
                udpc_data_full(k, C_ud_power_cycle::E_COL_T_HTF) = T_htf_levels[i];
                udpc_data_full(k, C_ud_power_cycle::E_COL_M_DOT) = m_dot_htf_ND_des;
                udpc_data_full(k, C_ud_power_cycle::E_COL_T_AMB) = T_amb_low + j*dT_T_amb;
                k++;
            }
        }

        // Check that index counter matches expected number of inputs
        if (k != n_total) {
            throw(C_csp_exception("udpc setup index counter final value does not match expected"));
        }

        // Add extra data to test filter
        bool is_test_extra_data = true;
        if (is_test_extra_data) {
            size_t n_extra = 1;
            udpc_data_full.resize_preserve(n_total + n_extra, C_ud_power_cycle::E_COL_M_H2O + 1, std::numeric_limits<double>::quiet_NaN());
            udpc_data_full(n_total,C_ud_power_cycle::E_COL_T_HTF) = T_htf_low + 0.5*dT_T_htf;
            udpc_data_full(n_total,C_ud_power_cycle::E_COL_M_DOT) = m_dot_htf_ND_low + 0.5*dT_m_dot_htf_ND;
            udpc_data_full(n_total,C_ud_power_cycle::E_COL_T_AMB) = T_amb_low + 0.5*dT_T_amb;
        }

        // Use example endo-reversible cycle model to calculate cycle performance
        double adjust = 10.0;
        C_endo_rev_cycle c_cycle(T_htf_des + adjust, T_amb_des + adjust);

        for (size_t i = 0; i < udpc_data_full.nrows(); i++) {
            c_cycle.performance(udpc_data_full(i, C_ud_power_cycle::E_COL_T_HTF),
                udpc_data_full(i, C_ud_power_cycle::E_COL_M_DOT),
                udpc_data_full(i, C_ud_power_cycle::E_COL_T_AMB),
                udpc_data_full(i, C_ud_power_cycle::E_COL_W_CYL),
                udpc_data_full(i, C_ud_power_cycle::E_COL_Q_CYL),
                udpc_data_full(i, C_ud_power_cycle::E_COL_W_COOL),
                udpc_data_full(i, C_ud_power_cycle::E_COL_M_H2O));
        }

        // Initialize UDPC model with cycle performance data table
        C_ud_power_cycle c_udpc;
        int n_T_htf_udpc_calc, n_T_amb_udpc_calc, n_m_dot_udpc_calc;
        double T_htf_ref_udpc_calc, T_htf_low_udpc_calc, T_htf_high_udpc_calc;
        double T_amb_ref_udpc_calc, T_amb_low_udpc_calc, T_amb_high_udpc_calc;
        double m_dot_htf_ref_udpc_calc, m_dot_htf_low_udpc_calc, m_dot_htf_high_udpc_calc;
        std::vector<double> Y_at_T_htf_ref, Y_at_T_amb_ref, Y_at_m_dot_htf_ND_ref, Y_avg_at_refs;
        c_udpc.init(udpc_data_full,
            n_T_htf_udpc_calc, n_T_amb_udpc_calc, n_m_dot_udpc_calc,
            T_htf_ref_udpc_calc, T_htf_low_udpc_calc, T_htf_high_udpc_calc,
            T_amb_ref_udpc_calc, T_amb_low_udpc_calc, T_amb_high_udpc_calc,
            m_dot_htf_ref_udpc_calc, m_dot_htf_low_udpc_calc, m_dot_htf_high_udpc_calc,
            Y_at_T_htf_ref, Y_at_T_amb_ref, Y_at_m_dot_htf_ND_ref, Y_avg_at_refs);


        // Sample UPDC model
            // at design point
        double W_dot_ND_calc = c_udpc.get_W_dot_gross_ND(T_htf_des, T_amb_des, 1.0);
        W_dot_ND_calc = c_udpc.get_W_dot_gross_ND(T_htf_des + adjust, T_amb_des + adjust, 1.0);

        double abce = 1.23;

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

DEFINE_MODULE_ENTRY(test_ud_power_cycle, "Test user-defined power cylce model", 0)
