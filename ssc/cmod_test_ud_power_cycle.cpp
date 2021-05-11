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
        bool is_test_extra_data;
        if (is_test_extra_data) {
            size_t n_extra = 1;
            udpc_data_full.resize_preserve(n_total + n_extra, C_ud_power_cycle::E_COL_M_H2O + 1, std::numeric_limits<double>::quiet_NaN());
            udpc_data_full(n_total,C_ud_power_cycle::E_COL_T_HTF) = T_htf_low + 0.5*dT_T_htf;
            udpc_data_full(n_total,C_ud_power_cycle::E_COL_M_DOT) = m_dot_htf_ND_levels[1];
            udpc_data_full(n_total,C_ud_power_cycle::E_COL_T_AMB) = T_amb_des;
        }

        // If try to pre-process and split table before defining dependent variables, what happens?
        util::matrix_t<double> T_htf_ind, m_dot_htf_ND_ind, T_amb_ind;
        N_udpc_common::split_ind_tbl(udpc_data_full, T_htf_ind, m_dot_htf_ND_ind, T_amb_ind);

        double a_ref = 12.0;
		double b_ref = 13.0;
		double c_ref = 14.0;
		double Y_ref = three_var_eqn(a_ref, b_ref, c_ref);

		double a_low = 10.0;
		double a_high = 14.0;

		double b_low = 10.0;
		double b_high = 16.0;

		double c_low = 10.0;
		double c_high = 18.0;

		int N_runs = 20;

		util::matrix_t<double> a_table(N_runs, 13, 1.0);
		util::matrix_t<double> b_table(N_runs, 13, 1.0);
		util::matrix_t<double> c_table(N_runs, 13, 1.0);

		for(int i = 0; i < N_runs; i++)
		{
			a_table(i,0) = a_low + (a_high-a_low)/(double)(N_runs-1)*i;
			a_table(i,1) = three_var_eqn(a_table(i,0),b_ref,c_low)/Y_ref;
			a_table(i,2) = three_var_eqn(a_table(i,0),b_ref,c_ref)/Y_ref;
			a_table(i,3) = three_var_eqn(a_table(i,0),b_ref,c_high)/Y_ref;

			b_table(i,0) = b_low + (b_high-b_low)/(double)(N_runs-1)*i;
			b_table(i,1) = three_var_eqn(a_low,b_table(i,0),c_ref)/Y_ref;
			b_table(i,2) = three_var_eqn(a_ref,b_table(i,0),c_ref)/Y_ref;
			b_table(i,3) = three_var_eqn(a_high,b_table(i,0),c_ref)/Y_ref;

			c_table(i,0) = c_low + (c_high-c_low)/(double)(N_runs-1)*i;
			c_table(i,1) = three_var_eqn(a_ref,b_low,c_table(i,0))/Y_ref;
			c_table(i,2) = three_var_eqn(a_ref,b_ref,c_table(i,0))/Y_ref;
			c_table(i,3) = three_var_eqn(a_ref,b_high,c_table(i,0))/Y_ref;
		}

		C_ud_power_cycle c_pc;

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
		}
	}

	double three_var_eqn(double a, double b, double c)
	{
		return a*pow(b,1.24) - a/pow(c,0.55) + b/(2*c+1.0)*a;
		//return 1.0;
	}

};

DEFINE_MODULE_ENTRY(test_ud_power_cycle, "Test user-defined power cylce model", 0)
