/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef __UD_POWER_CYCLE_JDM_
#define __UD_POWER_CYCLE_JDM_

#include <limits>
#include "interpolation_routines.h"
#include "csp_solver_util.h"

class C_ud_power_cycle_jdm
{
public:

    enum E_output_order
    {
        i_W_dot_gross = 0,
        i_Q_dot_HTF,
        i_W_dot_cooling,
        i_m_dot_water
    };

    enum
    {
        E_COL_T_HTF,
        E_COL_M_DOT,
        E_COL_T_AMB,
        E_COL_W_CYL,
        E_COL_Q_CYL,
        E_COL_W_COOL,
        E_COL_M_H2O
    };

private:
	
	// Each Linear_Interp Table in C_user_defined_pc shares the following column structure:

	//    Independent |    Gross Power Output   |   HTF Thermal Power	|   Cooling Parasitics  |	 Water Use 
	// 0)  Variable   |  1) -   2) 0     3) +   |  4) -   5) 0    6) +  |  7) -    8) 0    9) + | 10) -  11) 0   12) + 

	// Lookup table with dependent variables corresponding to parametric on independent variable T_htf_hot [C] (first column)
	Linear_Interp mc_T_htf_ind;		// At m_dot_htf levels

	// Lookup table with dependent variables corresponding to parametric on independent variable T_amb [C] (first column)
	Linear_Interp mc_T_amb_ind;		// At T_htf levels

	// Lookup table with dependent variables corresponding to parametric on independent variable m_dot_htf [ND] (first column)
	Linear_Interp mc_m_dot_htf_ind;	// At T_amb levels

	// Initialization will create three new interpolation tables for interaction effects
	
	//    Independent |   Gross Power Output   |   HTF Thermal Power  |   Cooling Parasitics   |	 Water Use        |
	// 0)  Variable   |  1) lower   2) upper   |  3) lower  4) upper  |  5) lower   6) upper   |  7) lower  8) upper  |

	
	Linear_Interp mc_T_htf_on_T_amb;		// rows = rows of T_amb
	Linear_Interp mc_T_amb_on_m_dot_htf;	// rows = rows of m_dot_htf
	Linear_Interp mc_m_dot_htf_on_T_htf;	// rows = rows of T_htf

	// member string for exception messages
	std::string m_error_msg;

	double get_interpolated_ND_output(int i_ME /*M.E. table index*/, double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/);

	double m_T_htf_ref;		//[C] Reference (design) HTF inlet temperature
	double m_T_htf_low;		//[C] Low level HTF inlet temperature (in T_amb parametric)
	double m_T_htf_high;	//[C] High level HTF inlet temperature (in T_amb parametric)

	double m_m_dot_htf_ref;		//[-] Reference (design) HTF mass flow rate
	double m_m_dot_htf_low;		//[-] Low level HTF mass flow rate (in T_HTF parametric)
	double m_m_dot_htf_high;	//[-] High level HTF mass flow rate (in T_HTF parametric)

	double m_T_amb_ref;		//[C] Reference (design) ambient temperature
	double m_T_amb_low;		//[C] Low level ambient temperature (in m_dot_htf parametric)
	double m_T_amb_high;	//[C] High level ambient temperature (in m_dot_htf parametric)

    int T_htf_ref_ind; // Index of the reference value within the Thtf levels
    int T_amb_ref_ind; // Index of the reference value within the Tamb levels
    int mdot_ref_ind; // Index of the reference value within the mdot levels

    // Vector that holds the value of each level
    std::vector<double> T_htf_levels;
    std::vector<double> m_dot_htf_ND_levels;
    std::vector<double> T_amb_levels;

    // Number of levels for each thingy
    size_t nlevels_Thtf;
    size_t nlevels_mdot;
    size_t nlevels_Tamb;

	// Can also save main effects of each independent variable at its upper and lower levels
    std::vector<double> m_Y_at_ref;     //[-]

    util::matrix_t<double> m_ME_T_htf;      //[-] Matrix that will contain main effects at all levels (low, high, and in-between)
	util::matrix_t<double> m_ME_T_amb;      //[-]
    util::matrix_t<double> m_ME_mdot_htf;       //[-]

public:

	C_ud_power_cycle_jdm(){};

	~C_ud_power_cycle_jdm(){};

    void init(const util::matrix_t<double>& udpc_table,
        int& n_T_htf_pars, int& n_T_amb_pars, int& n_m_dot_pars,
        double& T_htf_ref_calc /*C*/, 
        double& T_amb_ref_calc /*C*/, 
        double& m_dot_htf_ND_ref_calc, 
        std::vector<double>& Y_at_T_htf_ref, std::vector<double>& Y_at_T_amb_ref,
        std::vector<double>& Y_at_m_dot_htf_ND_ref, std::vector<double>& Y_avg_at_refs,
        std::vector<double>& T_htf_levels, std::vector<double>& m_dot_htf_ND_levels, std::vector<double>& T_amb_levels);

	double get_W_dot_gross_ND( double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/);

	double get_Q_dot_HTF_ND(double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/);

	double get_W_dot_cooling_ND(double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/);

	double get_m_dot_water_ND(double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/);	
};

class C_od_pc_function_jdm
{

public:

	struct S_f_inputs
	{	// Power cycle function inputs

		// From CSP System
		double m_T_htf_hot;		//[C] Hot HTF temperature from the receiver or storage
		double m_m_dot_htf_ND;	//[-] HTF mass flow rate

		// Ambient Conditions
		double m_T_amb;			//[C] Ambient temperature

		S_f_inputs()
		{
			m_T_htf_hot = m_m_dot_htf_ND = m_T_amb = std::numeric_limits<double>::quiet_NaN();
		}	
	};

	struct S_f_outputs
	{	// Power cycle function outputs

		double m_W_dot_gross_ND;	//[-] Off-design power / Design-point power
		double m_Q_dot_in_ND;		//[-] Off-design thermal power / Design-point thermal power
		double m_W_dot_cooling_ND;	//[-] Off-design cooling power / Design-point cooling power
		double m_m_dot_water_ND;	//[-] Off-design mass flow rate / Design-point mass flow rate

		S_f_outputs()
		{
			m_W_dot_gross_ND = m_Q_dot_in_ND = m_W_dot_cooling_ND = m_m_dot_water_ND = std::numeric_limits<double>::quiet_NaN();
		}
	};

	C_od_pc_function_jdm()
	{
	}
	~C_od_pc_function_jdm()
	{
	}

	virtual int operator()(S_f_inputs inputs, S_f_outputs & outputs) = 0;
};

class C_ud_pc_table_generator_jdm
{

private:
	C_od_pc_function_jdm &mf_pc_eq;
	std::string m_log_msg;
	std::string m_progress_msg;	

	void send_callback(bool is_od_model_error, int run_number, int n_runs_total,
		double T_htf_hot, double m_dot_htf_ND, double T_amb,
		double W_dot_gross_ND, double Q_dot_in_ND,
		double W_dot_cooling_ND, double m_dot_water_ND);

public:

	C_csp_messages mc_messages;

	C_ud_pc_table_generator_jdm(C_od_pc_function_jdm & f_pc_eq);

	~C_ud_pc_table_generator_jdm(){}

	int generate_tables(double T_htf_ref /*C*/, double T_htf_low /*C*/, double T_htf_high /*C*/, int n_T_htf /*-*/,
		double T_amb_ref /*C*/, double T_amb_low /*C*/, double T_amb_high /*C*/, int n_T_amb /*-*/,
		double m_dot_htf_ND_ref /*-*/, double m_dot_htf_ND_low /*-*/, double m_dot_htf_ND_high /*-*/, int n_m_dot_htf_ND,
		util::matrix_t<double> & T_htf_ind, util::matrix_t<double> & T_amb_ind, util::matrix_t<double> & m_dot_htf_ind);

	// Callback funtion
	bool(*mf_callback)(std::string &log_msg, std::string &progress_msg, void *data, double progress, int out_type);
	void *mp_mf_active;

};

namespace N_udpc_common_jdm
{
    // Not used - jdm
    void get_var_setup(const std::vector<double>& vec_unique, const std::vector<double>& var_vec,
        double& var_des, double& var_low, double& var_high);
    // Not used - jdm
    bool is_level_in_par(const std::vector<std::vector<double>> test_combs,
        const std::vector<std::vector<double>> full_table);
    // Not used - jdm
    int split_ind_tbl(const util::matrix_t<double>& combined, util::matrix_t<double>& T_htf_ind,
        util::matrix_t<double>& m_dot_ind, util::matrix_t<double>& T_amb_ind);
    // Not used - jdm
    int split_ind_tbl(const util::matrix_t<double>& combined, util::matrix_t<double>& T_htf_ind,
        util::matrix_t<double>& m_dot_ind, util::matrix_t<double>& T_amb_ind,
        int& n_T_htf_pars, int& n_T_amb_pars, int& n_m_dot_pars,
        double& m_dot_low, double& m_dot_des, double& m_dot_high,
        double& T_htf_low, double& T_htf_des, double& T_htf_high,
        double& T_amb_low, double& T_amb_des, double& T_amb_high);
    // Not used - jdm
    int combine_ind_tbl(util::matrix_t<double>& combined, const util::matrix_t<double>& T_htf_ind,
        const util::matrix_t<double>& m_dot_ind, const util::matrix_t<double>& T_amb_ind,
        double m_dot_low, double m_dot_des, double m_dot_high,
        double T_htf_low, double T_htf_des, double T_htf_high,
        double T_amb_low, double T_amb_des, double T_amb_high);
    // Added by jdm
    int find_reference_location(std::vector<double>& levels, double& ref, int& ind);
    int fill_data_tables(int& n_pars, size_t& nlevels, size_t& n_table_rows,
        double& m_low, double& dT, std::vector<double>& levels,
        double& ref_calc, const util::matrix_t<double>& udpc_table, util::matrix_t<double>& ind_table, size_t mode);
};

#endif
