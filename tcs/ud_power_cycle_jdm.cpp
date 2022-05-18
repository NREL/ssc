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

#include "ud_power_cycle_jdm.h"
#include "csp_solver_util.h"

#include "sam_csp_util.h"
#include <algorithm>
#include <set>
#include <fstream>
#include <unordered_set>
#include <map>

void C_ud_power_cycle_jdm::init(const util::matrix_t<double>& udpc_table,
    int& n_T_htf_pars, int& n_T_amb_pars, int& n_m_dot_pars,
    double& T_htf_ref_calc /*C*/, 
    double& T_amb_ref_calc /*C*/, 
    double& m_dot_htf_ND_ref_calc,
    std::vector<double>& Y_at_T_htf_ref, std::vector<double>& Y_at_T_amb_ref,
    std::vector<double>& Y_at_m_dot_htf_ND_ref, std::vector<double>& Y_avg_at_refs,
    std::vector<double>& T_htf_levels_in, std::vector<double>& m_dot_htf_ND_levels_in, std::vector<double>& T_amb_levels_in)
{
    // Create a log file
    std::ofstream logfile,template_data_file;
    logfile.open("power_cycle_class_log.txt");
    template_data_file.open("template_data_file.txt");
    logfile << "Enter class.\n";
    
    util::matrix_t<double> T_htf_ind_table, m_dot_htf_ND_ind_table, T_amb_ind_table;

    logfile << "Create matrices.\n";


    // We are provided with the following information:
    //  -   The values of each level for each variable, e.g. T_htf_levels_in
    //  -   The design point for each variable
    //  -   The number of points for each variable, e.g. n_T_htf_pars

    // Assign the input levels information to the levels vector contained in this class
    T_htf_levels = T_htf_levels_in;
    m_dot_htf_ND_levels = m_dot_htf_ND_levels_in;
    T_amb_levels = T_amb_levels_in;

    // Find the number of levels for each thingy
    nlevels_Thtf = T_htf_levels.size();
    nlevels_mdot = m_dot_htf_ND_levels.size();
    nlevels_Tamb = T_amb_levels.size();

    // Set the design point, and min and max
    // Set member data for reference and upper and lower bounds of independent variables
    m_T_htf_ref = T_htf_ref_calc;
    m_T_htf_low = *std::min_element(T_htf_levels.begin(), T_htf_levels.end());
    m_T_htf_high = *std::max_element(T_htf_levels.begin(), T_htf_levels.end());

    m_T_amb_ref = T_amb_ref_calc;
    m_T_amb_low = *std::min_element(T_amb_levels.begin(), T_amb_levels.end());
    m_T_amb_high = *std::max_element(T_amb_levels.begin(), T_amb_levels.end());;

    m_m_dot_htf_ref = m_dot_htf_ND_ref_calc;
    m_m_dot_htf_low = *std::min_element(m_dot_htf_ND_levels.begin(), m_dot_htf_ND_levels.end());
    m_m_dot_htf_high = *std::max_element(m_dot_htf_ND_levels.begin(), m_dot_htf_ND_levels.end());

    // Number of rows the input data should have
    size_t n_table_rows = nlevels_mdot * n_T_htf_pars + nlevels_Tamb * n_m_dot_pars + nlevels_Thtf * n_T_amb_pars;

    // Do some basic tests to make sure the input data is ok
    // Check minimum number of rows
    int n_par_min = 4;
    int n_min_runs = n_par_min * (nlevels_mdot + nlevels_Tamb + nlevels_Thtf);
    if (udpc_table.nrows() < n_min_runs)
    {
        throw(C_csp_exception("Not enough UDPC table rows", "UDPC Table Importation"));
    }

    if (udpc_table.nrows() != n_table_rows) {
        throw(C_csp_exception("The supplied input data does not have the correct number of data inputs.",
            "UDPC Table Importation"));
    }

    // Build tables for each independent variable
    const int ncols_Thtf = 1 + 4 * nlevels_mdot;
    const int ncols_mdot = 1 + 4 * nlevels_Tamb;
    const int ncols_Tamb = 1 + 4 * nlevels_Thtf;

    // HTF temperature table w/ HTF mass flow rate levels
    T_htf_ind_table.resize_fill(n_T_htf_pars, ncols_Thtf, 0.0);
    m_dot_htf_ND_ind_table.resize_fill(n_m_dot_pars, ncols_mdot, 0.0);
    T_amb_ind_table.resize_fill(n_T_amb_pars, ncols_Tamb, 0.0);

    
    double dT_T_htf = (m_T_htf_high - m_T_htf_low) / (double)(n_T_htf_pars - 1);
    double dT_mdot = (m_m_dot_htf_high - m_m_dot_htf_low) / (double)(n_m_dot_pars - 1);
    double dT_T_amb = (m_T_amb_high - m_T_amb_low) / (double)(n_T_amb_pars - 1);

    logfile << dT_T_htf << "," << dT_mdot << "," << dT_T_amb << "\n";

    logfile << "Finish creating various variables.\n";

    // This part creates a matrix that simply lists all the variable combinations that the user
    // should provide. This data will be written out to SAM (somehow) and the user must provide
    // data at these data points.
    util::matrix_t<double> data_template(n_table_rows, 3, std::numeric_limits<double>::quiet_NaN());
    //logfile << "template info: " << data_template.nrows() << "," << data_template.ncols() << "\n";
    size_t q = 0;
    for (size_t i = 0; i < nlevels_mdot; i++) {
        for (size_t j = 0; j < n_T_htf_pars; j++) {
            data_template(q, 0) = m_T_htf_low + j * dT_T_htf;
            data_template(q, 1) = m_dot_htf_ND_levels[i];
            data_template(q, 2) = m_T_amb_ref;
            q++;
        }
    }
    for (size_t i = 0; i < nlevels_Tamb; i++) {
        for (size_t j = 0; j < n_m_dot_pars; j++) {
            data_template(q, 0) = m_T_htf_ref;
            data_template(q, 1) = m_m_dot_htf_low + j * dT_mdot;
            data_template(q, 2) = T_amb_levels[i];
            q++;
        }
    }
    for (size_t i = 0; i < nlevels_Thtf; i++) {
        for (size_t j = 0; j < n_T_amb_pars; j++) {
            data_template(q, 0) = T_htf_levels[i];
            data_template(q, 1) = m_m_dot_htf_ref;
            data_template(q, 2) = m_T_amb_low + j * dT_T_amb;
            q++;
        }
    }

    for (size_t i = 0; i < data_template.nrows(); i++) {
        template_data_file << data_template(i, 0) << "," << data_template(i, 1) << "," << data_template(i, 2) << "\n";
    }

    // Search through the provided data and pick out the right bit to make the next set of tables that are used
    size_t mode;
    // For each T_htf (i) and mdot level (j) combination...
    N_udpc_common_jdm::fill_data_tables(n_T_htf_pars, nlevels_mdot, n_table_rows, m_T_htf_low,
            dT_T_htf, m_dot_htf_ND_levels, T_amb_ref_calc, udpc_table, T_htf_ind_table, mode = 0);
    logfile << "Added Thtf mdot variations to matrix.\n\n";

    // For each m_dot_htf_ND (i) and T_amb level (j) combination...
    N_udpc_common_jdm::fill_data_tables(n_m_dot_pars, nlevels_Tamb, n_table_rows, m_m_dot_htf_low,
        dT_mdot, T_amb_levels, T_htf_ref_calc, udpc_table, m_dot_htf_ND_ind_table, mode=1);
    logfile << "Added mdot Tamb variations to matrix.\n\n";

    // For each T_amb (i) and T_HTF level (j) combination...
    N_udpc_common_jdm::fill_data_tables(n_T_amb_pars, nlevels_Thtf, n_table_rows, m_T_amb_low,
        dT_T_amb, T_htf_levels, m_dot_htf_ND_ref_calc, udpc_table, T_amb_ind_table, mode=2);
    logfile << "Added Tamb Thtf variations to matrix.\n\n";

    
    // Set up Linear Interp class
    // This shouldn't require modification
    int error_index = -2;
    int column_index_array[1] = { 0 };
    if (!mc_T_htf_ind.Set_1D_Lookup_Table(T_htf_ind_table, column_index_array, 1, error_index))
    {
        if (error_index == -1)
        {
            throw(C_csp_exception("Table representing Hot HTF Temperature parametric results must have"
                "at least 3 rows", "User defined power cycle initialization"));
        }
        else
        {
            throw(C_csp_exception("The Hot HTF Temperature must monotonically increase in the table",
                "User defined power cycle initialization"));
        }
    }
    
    if (!mc_T_amb_ind.Set_1D_Lookup_Table(T_amb_ind_table, column_index_array, 1, error_index))
    {
        if (error_index == -1)
        {
            throw(C_csp_exception("Table representing Ambient Temperature parametric results must have"
                "at least 3 rows", "User defined power cycle initialization"));
        }
        else
        {
            throw(C_csp_exception("The Ambient Temperature must monotonically increase in the table",
                "User defined power cycle initialization"));
        }
    }

    if (!mc_m_dot_htf_ind.Set_1D_Lookup_Table(m_dot_htf_ND_ind_table, column_index_array, 1, error_index))
    {
        if (error_index == -1)
        {
            throw(C_csp_exception("Table representing HTF mass flow rate parametric results must have"
                "at least 3 rows", "User defined power cycle initialization"));
        }
        else
        {
            throw(C_csp_exception("The HTF mass flow rate must monotonically increase in the table",
                "User defined power cycle initialization"));
        }
    }

    
    // Find the index of the reference value within the levels vector. Need to know the position of the ref value.
    N_udpc_common_jdm::find_reference_location(T_htf_levels, m_T_htf_ref, T_htf_ref_ind);
    N_udpc_common_jdm::find_reference_location(T_amb_levels, m_T_amb_ref, T_amb_ref_ind);
    N_udpc_common_jdm::find_reference_location(m_dot_htf_ND_levels, m_m_dot_htf_ref, mdot_ref_ind);

    // Check that the reference (design) value and upper and lower levels for each independent variable are contained within the x-range of the corresponding table
    // This shouldn't require modification
    // T_HTF
    if (!mc_T_htf_ind.check_x_value_x_col_0(m_T_htf_ref))
    {
        m_error_msg = util::format("The user defined power cycle table containing parametric runs on the hot HTF temperature"
            " must contain the design HTF temperature %lg [C]. %s [C]", m_T_htf_ref, mc_T_htf_ind.get_error_msg().c_str());
        throw(C_csp_exception(m_error_msg, "User defined power cycle initialization"));
    }
    if (!mc_T_htf_ind.check_x_value_x_col_0(m_T_htf_low))
    {
        m_error_msg = util::format("The user defined power cycle table containing parametric runs on the hot HTF temperature"
            " must contain the lower level HTF temperature %lg [C]. %s [C]", m_T_htf_low, mc_T_htf_ind.get_error_msg().c_str());
        throw(C_csp_exception(m_error_msg, "User defined power cycle initialization"));
    }
    if (!mc_T_htf_ind.check_x_value_x_col_0(m_T_htf_high))
    {
        m_error_msg = util::format("The user defined power cycle table containing parametric runs on the hot HTF temperature"
            " must contain the upper level HTF temperature %lg [C]. %s [C]", m_T_htf_high, mc_T_htf_ind.get_error_msg().c_str());
        throw(C_csp_exception(m_error_msg, "User defined power cycle initialization"));
    }

    // T_amb
    if (!mc_T_amb_ind.check_x_value_x_col_0(m_T_amb_ref))
    {
        m_error_msg = util::format("The user defined power cycle table containing parametric runs on the ambient temperature"
            " must contain the design ambient temperature %lg [C]. %s [C]", m_T_amb_ref, mc_T_amb_ind.get_error_msg().c_str());
        throw(C_csp_exception(m_error_msg, "User defined power cycle initialization"));
    }
    if (!mc_T_amb_ind.check_x_value_x_col_0(m_T_amb_low))
    {
        m_error_msg = util::format("The user defined power cycle table containing parametric runs on the ambient temperature"
            " must contain the lower level ambient temperature %lg [C]. %s [C]", m_T_amb_low, mc_T_amb_ind.get_error_msg().c_str());
        throw(C_csp_exception(m_error_msg, "User defined power cycle initialization"));
    }
    if (!mc_T_amb_ind.check_x_value_x_col_0(m_T_amb_high))
    {
        m_error_msg = util::format("The user defined power cycle table containing parametric runs on the ambient temperature"
            " must contain the upper level ambient temperature %lg [C]. %s [C]", m_T_amb_high, mc_T_amb_ind.get_error_msg().c_str());
        throw(C_csp_exception(m_error_msg, "User defined power cycle initialization"));
    }

    // m_dot_HTF
    if (!mc_m_dot_htf_ind.check_x_value_x_col_0(m_m_dot_htf_ref))
    {
        m_error_msg = util::format("The user defined power cycle table containing parametric runs on the normalized HTF mass flow rate"
            " must contain the design normalized HTF mass flow rate %lg [-]. %s [-]", m_m_dot_htf_ref, mc_m_dot_htf_ind.get_error_msg().c_str());
        throw(C_csp_exception(m_error_msg, "User defined power cycle initialization"));
    }
    if (!mc_m_dot_htf_ind.check_x_value_x_col_0(m_m_dot_htf_low))
    {
        m_error_msg = util::format("The user defined power cycle table containing parametric runs on the normalized HTF mass flow rate"
            " must contain the lower level normalized HTF mass flow rate %lg [-]. %s [-]", m_m_dot_htf_low, mc_m_dot_htf_ind.get_error_msg().c_str());
        throw(C_csp_exception(m_error_msg, "User defined power cycle initialization"));
    }
    if (!mc_m_dot_htf_ind.check_x_value_x_col_0(m_m_dot_htf_high))
    {
        m_error_msg = util::format("The user defined power cycle table containing parametric runs on the normalized HTF mass flow rate"
            " must contain the upper level normalized HTF mass flow rate %lg [-]. %s [-]", m_m_dot_htf_high, mc_m_dot_htf_ind.get_error_msg().c_str());
        throw(C_csp_exception(m_error_msg, "User defined power cycle initialization"));
    }

    // ************************************************************************
    // ************************************************************************

    // Calculate main effects of each independent variable at its upper and lower levels
    // Have to rethink how this is all done
    // Vectors have four elements to reflect the four output variables
    //    Independent |    Gross Power Output   |   HTF Thermal Power	|   Cooling Parasitics  |	 Water Use 
    // 0)  Variable   |  1) -   2) 0     3) +   |  4) -   5) 0    6) +  |  7) -    8) 0    9) + | 10) -  11) 0   12) + 
    Y_at_T_htf_ref.resize(4);
    Y_at_T_amb_ref.resize(4);
    Y_at_m_dot_htf_ND_ref.resize(4);
    m_Y_at_ref.resize(4);

    m_ME_T_htf.resize_fill(nlevels_Thtf-1, 4, 0.0);
    m_ME_T_amb.resize_fill(nlevels_Tamb-1, 4, 0.0);
    m_ME_mdot_htf.resize_fill(nlevels_mdot-1, 4, 0.0);


    for (int i = 0; i < 4; i++)
    {
        // The column number depends on the number of levels and the location of the reference value within those levels. Orig: int i_col = i * 3 + 2;
        // Depending on the the number of levels, the columns that are accessed are different
        int i_col_mdot = i * nlevels_mdot + mdot_ref_ind + 1;
        int i_col_Thtf = i * nlevels_Thtf + T_htf_ref_ind + 1;
        int i_col_Tamb = i * nlevels_Tamb + T_amb_ref_ind + 1;


        Y_at_T_htf_ref[i] = mc_T_htf_ind.interpolate_x_col_0(i_col_mdot, m_T_htf_ref);
        Y_at_T_amb_ref[i] = mc_T_amb_ind.interpolate_x_col_0(i_col_Thtf, m_T_amb_ref);
        Y_at_m_dot_htf_ND_ref[i] = mc_m_dot_htf_ind.interpolate_x_col_0(i_col_Tamb, m_m_dot_htf_ref);
        m_Y_at_ref[i] = (Y_at_T_htf_ref[i] + Y_at_T_amb_ref[i] + Y_at_m_dot_htf_ND_ref[i]) / 3.0;

        // Calculate main effects
        int indi = 0; // Use this to skip calculating the main effect of the reference value
        for (int j = 0; j < nlevels_Thtf-1; j++) {
            if (j == T_htf_ref_ind) {
                indi = 1;
            }
            m_ME_T_htf(j, i) = mc_T_htf_ind.interpolate_x_col_0(i_col_mdot, T_htf_levels[j + indi]) - m_Y_at_ref[i];
            logfile << "Thtf ME (new)" << i << " , " << j << " , " << m_ME_T_htf(j, i) << "\n";
        }


        indi = 0;
        for (int j = 0; j < nlevels_Tamb-1; j++) {
            if (j == T_amb_ref_ind) {
                indi = 1;
            }
            m_ME_T_amb(j, i) = mc_T_amb_ind.interpolate_x_col_0(i_col_Thtf, T_amb_levels[j + indi]) - m_Y_at_ref[i];
            logfile << "Tamb ME (new)" << i << " , " << j << " , " << m_ME_T_amb(j, i) << "\n";
        }

        indi = 0;
        for (int j = 0; j < nlevels_mdot-1; j++) {
            if (j == mdot_ref_ind) {
                indi = 1;
            }
            m_ME_mdot_htf(j, i) = mc_m_dot_htf_ind.interpolate_x_col_0(i_col_Tamb, m_dot_htf_ND_levels[j + indi]) - m_Y_at_ref[i];
            logfile << "Mdot ME (new)" << i << " , " << j << " , " << m_ME_mdot_htf(j, i) << "\n";
        }

    }
    Y_avg_at_refs = m_Y_at_ref;

    logfile << "Main effects set up.\n";

    // Set up 2D tables to store calculated Interactions	
    int n_T_htf_runs = mc_T_htf_ind.get_number_of_rows();
    int n_T_amb_runs = mc_T_amb_ind.get_number_of_rows();
    int n_m_dot_htf_runs = mc_m_dot_htf_ind.get_number_of_rows();

    // Nlevels interaction effects for each output
    util::matrix_t<double> T_htf_int_on_T_amb(n_T_amb_runs, 1 + 4 * (nlevels_Thtf - 1));
    util::matrix_t<double> T_amb_int_on_m_dot_htf(n_m_dot_htf_runs, 1 + 4 * (nlevels_Tamb - 1));
    util::matrix_t<double> m_dot_htf_int_on_T_htf(n_T_htf_runs, 1 + 4 * (nlevels_mdot - 1));

    logfile << "Matrices for interaction effects set up.\n";
    

    // Initialization will create three new interpolation tables for interaction effects

    //    Independent |   Gross Power Output   |   HTF Thermal Power  |   Cooling Parasitics   |	 Water Use        |
    // 0)  Variable   |  1) lower   2) upper   |  3) lower  4) upper  |  5) lower   6) upper   |  7) lower  8) upper  |

    // Calculate interaction effects
    for (int i = 0; i < 4; i++)
    {

        // T_HTF interaction on ambient temperature
        for (int j = 0; j < n_T_amb_runs; j++)
        {
            if (i == 0)
            {
                T_htf_int_on_T_amb(j, 0) = mc_T_amb_ind.get_x_value_x_col_0(j);
            }

            // There are (nlevels_Thtf - 1) interactions
            int indi = 0; // This index helps identify the correct column to access
            int icolREF = i * nlevels_Thtf + T_htf_ref_ind + 1; // The column corresponding to the reference value
            for (int k = 0; k < nlevels_Thtf - 1; k++) {
                if (k == T_htf_ref_ind) {
                    indi = 1;
                }
                
                int icol = i * nlevels_Thtf + k + indi + 1; // The correct column to access for the interaction
                
                double aa = mc_T_amb_ind.Get_Value(icol, j); // replace *3 with nlevels_Thtf, and *2 with (nlevels_Thtf-1)
                double bb = m_ME_T_htf(k, i);
                double cc = mc_T_amb_ind.Get_Value(icolREF, j);
                T_htf_int_on_T_amb(j, i * (nlevels_Thtf - 1) + k + 1) = -(aa - m_Y_at_ref[i] - bb - (cc - m_Y_at_ref[i]));
                logfile << T_htf_int_on_T_amb(j, i * (nlevels_Thtf - 1) + k + 1) << " , " ;
            }
            logfile << "\n";
           
        }
        logfile << i << " , " << "Thtf on Tamb interactions.\n";

        // Ambient temperature interaction on HTF mass flow rate
        for (int j = 0; j < n_m_dot_htf_runs; j++)
        {
            if (i == 0)
            {
                T_amb_int_on_m_dot_htf(j, 0) = mc_m_dot_htf_ind.get_x_value_x_col_0(j);
            }

            // There are (nlevels_Thtf - 1) interactions
            int indi = 0; // This index helps identify the correct column to access
            int icolREF = i * nlevels_Tamb + T_amb_ref_ind + 1; // The column corresponding to the reference value
            for (int k = 0; k < nlevels_Tamb - 1; k++) {
                if (k == T_amb_ref_ind) {
                    indi = 1;
                }

                int icol = i * nlevels_Tamb + k + indi + 1; // The correct column to access for the interaction

                double aa = mc_m_dot_htf_ind.Get_Value(icol, j); // replace *3 with nlevels_Thtf, and *2 with (nlevels_Thtf-1)
                double bb = m_ME_T_amb(k, i);
                double cc = mc_m_dot_htf_ind.Get_Value(icolREF, j);
                T_amb_int_on_m_dot_htf(j, i * (nlevels_Tamb - 1) + k + 1) = -(aa - m_Y_at_ref[i] - bb - (cc - m_Y_at_ref[i]));
                logfile << T_amb_int_on_m_dot_htf(j, i * (nlevels_Tamb - 1) + k + 1) << " , ";
            }

            logfile << "\n";

        }

        logfile << i << " , " << "Tamb on mdot interactions.\n";

        // HTF mass flow
        for (int j = 0; j < n_T_htf_runs; j++)
        {
            if (i == 0)
            {
                m_dot_htf_int_on_T_htf(j, 0) = mc_T_htf_ind.get_x_value_x_col_0(j);
            }


            // There are (nlevels_Thtf - 1) interactions
            int indi = 0; // This index helps identify the correct column to access
            int icolREF = i * nlevels_mdot + mdot_ref_ind + 1; // The column corresponding to the reference value
            logfile << icolREF << "\n";
            for (int k = 0; k < nlevels_mdot - 1; k++) {
                if (k == mdot_ref_ind) {
                    indi = 1;
                }

                int icol = i * nlevels_mdot + k + indi + 1; // The correct column to access for the interaction

                double aa = mc_T_htf_ind.Get_Value(icol, j); // replace *3 with nlevels_Thtf, and *2 with (nlevels_Thtf-1)
                double bb = m_ME_mdot_htf(k, i);
                double cc = mc_T_htf_ind.Get_Value(icolREF, j);
                m_dot_htf_int_on_T_htf(j, i * (nlevels_mdot - 1) + k + 1) = -(aa - m_Y_at_ref[i] - bb - (cc - m_Y_at_ref[i]));
                logfile << m_dot_htf_int_on_T_htf(j, i * (nlevels_mdot - 1) + k + 1) << " , ";
            }
            logfile << "   New \n";
            
        }

        logfile << i << " , " << "mdot on Thtf interactions.\n";
    }

    // Initialize Linear_Interp classes for interaction effects
    if (!mc_T_htf_on_T_amb.Set_1D_Lookup_Table(T_htf_int_on_T_amb, column_index_array, 1, error_index))
    {
        throw(C_csp_exception("Initialization of interpolation table for the interaction effect of T_HTF levels"
            "on the ambient temperature failed", "User defined power cycle initialization"));
    }
    if (!mc_T_amb_on_m_dot_htf.Set_1D_Lookup_Table(T_amb_int_on_m_dot_htf, column_index_array, 1, error_index))
    {
        throw(C_csp_exception("Initialization of interpolation table for the interaction effect of T_amb levels"
            "on HTF mass flow rate failed", "User defined power cycle initialization"));
    }
    if (!mc_m_dot_htf_on_T_htf.Set_1D_Lookup_Table(m_dot_htf_int_on_T_htf, column_index_array, 1, error_index))
    {
        throw(C_csp_exception("Initialization of interpolation table for the interaction effect of m_dot_HTF levels"
            "on the HTF temperature failed", "User defined power cycle initialization"));
    }

}

double C_ud_power_cycle_jdm::get_W_dot_gross_ND(double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/)
{
	// This call needs to define which columns to search
	// Then use 'get_interpolated_ND_output' to get ND total effect
	
	return get_interpolated_ND_output(i_W_dot_gross, T_htf_hot, T_amb, m_dot_htf_ND);

	// Also, maybe want to check parameters against max/min, or if extrapolating, or something?
}

double C_ud_power_cycle_jdm::get_Q_dot_HTF_ND(double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/)
{
	// This call needs to define which columns to search
	// Then use 'get_interpolated_ND_output' to get ND total effect

	return get_interpolated_ND_output(i_Q_dot_HTF, T_htf_hot, T_amb, m_dot_htf_ND);

	// Also, maybe want to check parameters against max/min, or if extrapolating, or something?
}

double C_ud_power_cycle_jdm::get_W_dot_cooling_ND(double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/)
{
	// This call needs to define which columns to search
	// Then use 'get_interpolated_ND_output' to get ND total effect

	return get_interpolated_ND_output(i_W_dot_cooling, T_htf_hot, T_amb, m_dot_htf_ND);

	// Also, maybe want to check parameters against max/min, or if extrapolating, or something?
}

double C_ud_power_cycle_jdm::get_m_dot_water_ND(double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/)
{
	// This call needs to define which columns to search
	// Then use 'get_interpolated_ND_output' to get ND total effect

	return get_interpolated_ND_output(i_m_dot_water, T_htf_hot, T_amb, m_dot_htf_ND);

	// Also, maybe want to check parameters against max/min, or if extrapolating, or something?
}

double C_ud_power_cycle_jdm::get_interpolated_ND_output(int i_ME /*M.E. table index*/,
							double T_htf_hot /*C*/, double T_amb /*C*/, double m_dot_htf_ND /*-*/)
{


    // Create a log file
    std::ofstream logfile;
    logfile.open("get_interpolated_ND_output_log.txt");

    double ME_T_htf = mc_T_htf_ind.interpolate_x_col_0(i_ME * nlevels_mdot + mdot_ref_ind + 1, T_htf_hot) - m_Y_at_ref[i_ME];
    double ME_T_amb = mc_T_amb_ind.interpolate_x_col_0(i_ME * nlevels_Thtf + T_htf_ref_ind + 1, T_amb) - m_Y_at_ref[i_ME];
    double ME_m_dot_htf = mc_m_dot_htf_ind.interpolate_x_col_0(i_ME * nlevels_Tamb + T_amb_ref_ind + 1, m_dot_htf_ND) - m_Y_at_ref[i_ME];

    logfile << "Calculate main effects:\n";
    logfile << "ME_T_htf:  " << ME_T_htf << "\n";
    logfile << "ME_T_amb:  " << ME_T_amb << "\n";
    logfile << "ME_m_dot_htf:  " << ME_m_dot_htf << "\n\n";


    logfile << "Calculate interaction of Thtf on Tamb:\n";
    // Calculate the interaction of Thtf on Tamb
    double INT_T_htf_on_T_amb = 0.0;
    if (T_htf_hot != m_T_htf_ref){
        // Approach where two interaction effects are calculated and then weighted-averaged
        // Some tests are conducted in case only one interaction effect can be used
        size_t zone = count_if(T_htf_levels.begin(), T_htf_levels.end(), [T_htf_hot](double ii) {return T_htf_hot > ii; });
        double lower_int = 0;
        double upper_int = 0;
        size_t lower_flag = 0;
        size_t upper_flag = 0;

        logfile << "zone: " << zone << "\n";

        if (zone == 0) {
            zone = 1;
            lower_flag = 1;
        }

        if (zone >= nlevels_Thtf) {
            zone = (nlevels_Thtf - 1);
            upper_flag = 1;
        }

        size_t Thtf_col_lo = (nlevels_Thtf - 1) * i_ME + zone;  // Identify the correct column in the interaction table
        size_t Thtf_col_hi = Thtf_col_lo + 1;  // Identify the correct column in the interaction table
        if (T_htf_hot >= m_T_htf_ref) {
            Thtf_col_lo--;
            Thtf_col_hi--;
        }
        double Thtf_bound_lo = T_htf_levels[zone - 1]; // Identify the correct level to use
        double Thtf_bound_hi = T_htf_levels[zone]; // Identify the correct level to use

        lower_int = mc_T_htf_on_T_amb.interpolate_x_col_0(Thtf_col_lo, T_amb) * (T_htf_hot - m_T_htf_ref) / (m_T_htf_ref - Thtf_bound_lo);
        upper_int = mc_T_htf_on_T_amb.interpolate_x_col_0(Thtf_col_hi, T_amb) * (T_htf_hot - m_T_htf_ref) / (m_T_htf_ref - Thtf_bound_hi);

        if (Thtf_bound_lo == m_T_htf_ref || upper_flag == 1) {
            INT_T_htf_on_T_amb = upper_int;
        }
        else if (Thtf_bound_hi == m_T_htf_ref || lower_flag == 1) {
            INT_T_htf_on_T_amb = lower_int;
        }
        else {
            double frac = (T_htf_hot - Thtf_bound_lo) / (Thtf_bound_hi - Thtf_bound_lo);
            INT_T_htf_on_T_amb = (1.0 - frac) * lower_int + frac * upper_int;
        }
    }

    logfile << "The interaction effect = " << INT_T_htf_on_T_amb << "\n\n";


    logfile << "Calculate interaction of Tamb on mdot:\n";
    // Calculate the interaction of Tamb on mdot
    double INT_T_amb_on_m_dot_htf = 0.0;
    if (T_amb != m_T_amb_ref) {
        // Approach where two interaction effects are calculated and then weighted-averaged
        // Some tests are conducted in case only one interaction effect can be used
        size_t zone = count_if(T_amb_levels.begin(), T_amb_levels.end(), [T_amb](double ii) {return T_amb > ii; });
        double lower_int = 0;
        double upper_int = 0;
        size_t lower_flag = 0;
        size_t upper_flag = 0;

        logfile << "zone: " << zone << "\n";

        if (zone == 0) {
            zone = 1;
            lower_flag = 1;
        }

        if (zone >= nlevels_Tamb) {
            zone = (nlevels_Tamb - 1);
            upper_flag = 1;
        }

        size_t Tamb_col_lo = (nlevels_Tamb - 1) * i_ME + zone;  // Identify the correct column in the interaction table
        size_t Tamb_col_hi = Tamb_col_lo + 1;  // Identify the correct column in the interaction table
        if (T_amb >= m_T_amb_ref) {
            Tamb_col_lo--;
            Tamb_col_hi--;
        }
        double Tamb_bound_lo = T_amb_levels[zone - 1]; // Identify the correct level to use
        double Tamb_bound_hi = T_amb_levels[zone]; // Identify the correct level to use

        lower_int = mc_T_amb_on_m_dot_htf.interpolate_x_col_0(Tamb_col_lo, m_dot_htf_ND) * (T_amb - m_T_amb_ref) / (m_T_amb_ref - Tamb_bound_lo);
        upper_int = mc_T_amb_on_m_dot_htf.interpolate_x_col_0(Tamb_col_hi, m_dot_htf_ND) * (T_amb - m_T_amb_ref) / (m_T_amb_ref - Tamb_bound_hi);

        if (Tamb_bound_lo == m_T_amb_ref || upper_flag == 1) {
            INT_T_amb_on_m_dot_htf = upper_int;
        }
        else if (Tamb_bound_hi == m_T_amb_ref || lower_flag == 1) {
            INT_T_amb_on_m_dot_htf = lower_int;
        }
        else {
            double frac = (T_amb - Tamb_bound_lo) / (Tamb_bound_hi - Tamb_bound_lo);
            INT_T_amb_on_m_dot_htf = (1.0 - frac) * lower_int + frac * upper_int;
        }
    }
    logfile << "The interaction effect = " << INT_T_amb_on_m_dot_htf << "\n\n";

    logfile << "Calculate interaction of mdot on Thtf:\n";
    // Calculate the interaction of mdot on Thtf
    double INT_m_dot_htf_on_T_htf = 0.0;
    if (m_dot_htf_ND != m_m_dot_htf_ref) {
        
        // Approach where two interaction effects are calculated and then weighted-averaged
        // Some tests are conducted in case only one interaction effect can be used
        size_t zone = count_if(m_dot_htf_ND_levels.begin(), m_dot_htf_ND_levels.end(), [m_dot_htf_ND](double ii) {return m_dot_htf_ND > ii; });
        double lower_int = 0;
        double upper_int = 0;
        size_t lower_flag = 0;
        size_t upper_flag = 0;

        logfile << "zone: " << zone << "\n";

        if (zone == 0) {
            zone = 1;
            lower_flag = 1;
        }

        if (zone >= nlevels_mdot) {
            zone = (nlevels_mdot - 1);
            upper_flag = 1;
        }

        size_t mdot_col_lo = (nlevels_mdot - 1) * i_ME + zone;  // Identify the correct column in the interaction table
        size_t mdot_col_hi = mdot_col_lo+1;  // Identify the correct column in the interaction table
        if (m_dot_htf_ND >= m_m_dot_htf_ref) {
            mdot_col_lo--;
            mdot_col_hi--;
        }
        double mdot_bound_lo = m_dot_htf_ND_levels[zone-1]; // Identify the correct level to use
        double mdot_bound_hi = m_dot_htf_ND_levels[zone]; // Identify the correct level to use
        
        lower_int = mc_m_dot_htf_on_T_htf.interpolate_x_col_0(mdot_col_lo, T_htf_hot) * (m_dot_htf_ND - m_m_dot_htf_ref) / (m_m_dot_htf_ref - mdot_bound_lo);
        upper_int = mc_m_dot_htf_on_T_htf.interpolate_x_col_0(mdot_col_hi, T_htf_hot) * (m_dot_htf_ND - m_m_dot_htf_ref) / (m_m_dot_htf_ref - mdot_bound_hi);

        logfile << "mdot_col_lo: " << mdot_col_lo << "\n";
        logfile << "mdot_bound_lo: " << mdot_bound_lo << "\n";
        logfile << "lower_int: " << lower_int << "\n";

        logfile << "mdot_col_hi: " << mdot_col_hi << "\n";
        logfile << "mdot_bound_hi: " << mdot_bound_hi << "\n";
        logfile << "upper_int: " << upper_int << "\n";

        if (mdot_bound_lo == m_m_dot_htf_ref || upper_flag == 1) {
            INT_m_dot_htf_on_T_htf = upper_int;
        }
        else if (mdot_bound_hi == m_m_dot_htf_ref || lower_flag == 1) {
            INT_m_dot_htf_on_T_htf = lower_int;
        }
        else {
            double frac = (m_dot_htf_ND - mdot_bound_lo) / (mdot_bound_hi - mdot_bound_lo);
            INT_m_dot_htf_on_T_htf = (1.0 - frac) * lower_int + frac * upper_int;
        }
        logfile << "Interaction: " << INT_m_dot_htf_on_T_htf << "\n";
    }
    logfile << "The interaction effect = " << INT_m_dot_htf_on_T_htf << "\n\n";

    logfile.close();

	return m_Y_at_ref[i_ME] + ME_T_htf + ME_T_amb + ME_m_dot_htf + INT_T_htf_on_T_amb + INT_T_amb_on_m_dot_htf + INT_m_dot_htf_on_T_htf;
}



void N_udpc_common_jdm::get_var_setup(const std::vector<double>& vec_unique, const std::vector<double>& var_vec,
    double& var_des, double& var_low, double& var_high)
{
    std::unordered_map<double, int> var_val_count;
    std::vector<int> v_var_count;
    int n_var_unique = vec_unique.size();

    // For each unique value in vec_unique, count instances in var_vec
    for (int i = 0; i < n_var_unique; i++)
    {
        var_val_count.insert(std::pair<double, int>(vec_unique[i], std::count(var_vec.begin(), var_vec.end(), vec_unique[i])));
        v_var_count.push_back(std::count(var_vec.begin(), var_vec.end(), vec_unique[i]));
    }

    // Sort instance counts
    std::sort(v_var_count.begin(), v_var_count.end());
    int var_count_max = v_var_count[n_var_unique - 1];      // highest count
    int var_count_2 = v_var_count[n_var_unique - 2];        // 2nd highest count
    int var_count_3 = v_var_count[n_var_unique - 3];        // 3rd highest count
    int var_count_4 = v_var_count[n_var_unique - 4];        // 4th highest count

    // Map independent variable with count, then check against some udpc rules
    var_des = std::numeric_limits<double>::quiet_NaN();
    double var_level_1 = std::numeric_limits<double>::quiet_NaN();
    double var_level_2 = std::numeric_limits<double>::quiet_NaN();
    std::unordered_map<double, int>::iterator it_map = var_val_count.begin();
    for (int i = 0; i < n_var_unique; i++)
    {
        if (it_map->second == var_count_max)
        {
            var_des = it_map->first;
        }
        if (it_map->second == var_count_2 && !std::isfinite(var_level_1))
        {
            var_level_1 = it_map->first;
        }
        if (it_map->second == var_count_3)
        {
            var_level_2 = it_map->first;
        }
        it_map++;
    }
    if (var_level_1 < var_level_2)
    {
        var_low = var_level_1;
        var_high = var_level_2;
    }
    else
    {
        var_low = var_level_2;
        var_high = var_level_1;
    }

    if (var_count_3 < 4) {
        throw(C_csp_exception("UDPC parametric for each variable must contain at least 4 unique values"));
    }
    if (var_count_4 == var_count_3) {
        throw(C_csp_exception("UDPC parametric must have more instances for 3rd most common var then 4th most common var"));
    }
    if (var_count_max == var_count_2) {
        throw(C_csp_exception("UDPC parametric must have more instances for most common var (design) then 2nd most common var"));
    }
}

bool N_udpc_common_jdm::is_level_in_par(const std::vector<std::vector<double>> test_combs,
    const std::vector<std::vector<double>> full_table)
{
    int n_tbl_rows = full_table.size();
    bool des__low = false;
    bool des__des = false;
    bool des__high = false;
    for (int i = 0; i < n_tbl_rows; i++)
    {
        // Is T_amb_design in a row with T_htf_low and m_dot_des?
        if (test_combs[0] == std::vector<double>{ full_table[i][C_ud_power_cycle_jdm::E_COL_T_HTF], full_table[i][C_ud_power_cycle_jdm::E_COL_M_DOT], full_table[i][C_ud_power_cycle_jdm::E_COL_T_AMB] })
        {
            des__low = true;
        }
        // Is T_amb_design in a row with T_htf_des and m_dot_des?
        if (test_combs[1] == std::vector<double>{ full_table[i][C_ud_power_cycle_jdm::E_COL_T_HTF], full_table[i][C_ud_power_cycle_jdm::E_COL_M_DOT], full_table[i][C_ud_power_cycle_jdm::E_COL_T_AMB] })
        {
            des__des = true;
        }
        // Is T_amb_design in a row with T_htf_high and m_dot_des?
        if (test_combs[2] == std::vector<double>{ full_table[i][C_ud_power_cycle_jdm::E_COL_T_HTF], full_table[i][C_ud_power_cycle_jdm::E_COL_M_DOT], full_table[i][C_ud_power_cycle_jdm::E_COL_T_AMB] })
        {
            des__high = true;
        }

        if (des__low && des__des && des__high)
        {
            break;
        }
    }

    return des__low && des__des && des__high;
}


//int N_udpc_common_jdm::find_reference_location(std::vector<double>& levels, double& ref, int& ind)
//{
//    std::vector<double>::iterator it;
//    return find_reference_location(levels, ref, ind);
//}

int N_udpc_common_jdm::find_reference_location(std::vector<double>& levels, double& ref, int& ind)
{
    std::vector<double>::iterator it = std::find(levels.begin(), levels.end(), ref);
    ind = 1; // Set to default value of 1 (corresponds to three levels)
    if (it != levels.end()) {
        ind = std::distance(levels.begin(), it); // Get index of element from iterator
    }
    else {
        throw(C_csp_exception("Reference element not found in levels vector.",
            "User defined power cycle initialization"));
    }

    return 0;
}

int N_udpc_common_jdm::fill_data_tables(int& n_pars, size_t& nlevels, size_t& n_table_rows,
    double& m_low, double& dT, std::vector<double>& levels,
    double& ref_calc, const util::matrix_t<double>& udpc_table, util::matrix_t<double>& ind_table,
    size_t mode)
{
    // For each T_htf (i) and m_dot_htf_ND level (j) combination...
    size_t matched;
    for (int i = 0; i < n_pars; i++)
    {
        for (int j = 0; j < nlevels; j++)
        {
            matched = 0;
            double a, b, c;
            // Search through combined table and find corresponding row
            for (int k = 0; k < n_table_rows; k++)
            {
                if (mode == 0) { // For T_htf_ind_table, vary Thtf over several mdot levels
                    a = m_low + i * dT;
                    b = levels[j];
                    c = ref_calc;
                }
                else if (mode == 1) { // For m_dot_htf_ND_ind_table, vary mdot over several Tamb levels
                    a = ref_calc;
                    b = m_low + i * dT;
                    c = levels[j];
                }
                else if (mode == 2) { // For T_amb_ind_table, vary Tamb over several Thtf levels
                    a = levels[j];
                    b = ref_calc;
                    c = m_low + i * dT;
                }
                // vector{a,b,c} is in order {T_htf, mdot, Tamb}
                if (std::vector<double>{a, b, c} == 
                    std::vector<double>{ udpc_table(k, C_ud_power_cycle_jdm::E_COL_T_HTF), udpc_table(k, C_ud_power_cycle_jdm::E_COL_M_DOT), udpc_table(k, C_ud_power_cycle_jdm::E_COL_T_AMB) })
                {

                    // Set values in independent variable table
                    //    Independent |    Gross Power Output   |   HTF Thermal Power	|   Cooling Parasitics  |	 Water Use 
                    // 0)  Variable   |  1) -   2) 0     3) +   |  4) -   5) 0    6) +  |  7) -    8) 0    9) + | 10) -  11) 0   12) + 
                    if (mode == 0) {
                        ind_table.set_value(udpc_table(k, C_ud_power_cycle_jdm::E_COL_T_HTF), i, 0);
                    }
                    else if (mode == 1) {
                        ind_table.set_value(udpc_table(k, C_ud_power_cycle_jdm::E_COL_M_DOT), i, 0);
                    }
                    else if (mode == 2) {
                        ind_table.set_value(udpc_table(k, C_ud_power_cycle_jdm::E_COL_T_AMB), i, 0);
                    }
                    ind_table.set_value(udpc_table(k, C_ud_power_cycle_jdm::E_COL_W_CYL), i, nlevels * C_ud_power_cycle_jdm::i_W_dot_gross + 1 + j);
                    ind_table.set_value(udpc_table(k, C_ud_power_cycle_jdm::E_COL_Q_CYL), i, nlevels * C_ud_power_cycle_jdm::i_Q_dot_HTF + 1 + j);
                    ind_table.set_value(udpc_table(k, C_ud_power_cycle_jdm::E_COL_W_COOL), i, nlevels * C_ud_power_cycle_jdm::i_W_dot_cooling + 1 + j);
                    ind_table.set_value(udpc_table(k, C_ud_power_cycle_jdm::E_COL_M_H2O), i, nlevels * C_ud_power_cycle_jdm::i_m_dot_water + 1 + j);

                    matched = 1;
                }

            }
            if (matched == 0) {
                throw(C_csp_exception("A required data point could not be found in the supplied data. ",
                    "UDPC Table Importation"));
            }
        }
    }
    return 0;
}


int N_udpc_common_jdm::split_ind_tbl(const util::matrix_t<double>& cmbd_ind, util::matrix_t<double>& T_htf_ind,
    util::matrix_t<double>& m_dot_ind, util::matrix_t<double>& T_amb_ind)
{
    int n_T_htf_pars, n_T_amb_pars, n_m_dot_pars;
    n_T_htf_pars = n_T_amb_pars = n_m_dot_pars = -1;
    double m_dot_low, m_dot_des, m_dot_high, T_htf_low, T_htf_des, T_htf_high, T_amb_low, T_amb_des, T_amb_high;
    m_dot_low = m_dot_des = m_dot_high = T_htf_low = T_htf_des = T_htf_high = T_amb_low = T_amb_des = T_amb_high = std::numeric_limits<double>::quiet_NaN();

    return split_ind_tbl(cmbd_ind, T_htf_ind, m_dot_ind, T_amb_ind,
        n_T_htf_pars, n_T_amb_pars, n_m_dot_pars,
        m_dot_low, m_dot_des, m_dot_high,
        T_htf_low, T_htf_des, T_htf_high,
        T_amb_low, T_amb_des, T_amb_high);
}

int N_udpc_common_jdm::split_ind_tbl(const util::matrix_t<double>& cmbd_ind, util::matrix_t<double>& T_htf_ind,
    util::matrix_t<double>& m_dot_ind, util::matrix_t<double>& T_amb_ind,
    int& n_T_htf_pars, int& n_T_amb_pars, int& n_m_dot_pars,
    double& m_dot_low, double& m_dot_des, double& m_dot_high,
    double& T_htf_low, double& T_htf_des, double& T_htf_high,
    double& T_amb_low, double& T_amb_des, double& T_amb_high)
{
    // check for minimum length
    int n_par_min = 4;
    int n_levels = 3;
    int n_ind_vars = 3;
    int n_min_runs = n_par_min * n_levels * n_ind_vars;
    int n_table_rows = cmbd_ind.nrows();
    if (n_table_rows < n_min_runs)
    {
        throw(C_csp_exception("Not enough UDPC table rows", "UDPC Table Importation"));
    }

    // get T_htf, m_dot_htf, and T_amb vectors
    util::matrix_t<double> T_htf_col, m_dot_col, T_amb_col;
    T_htf_col = cmbd_ind.col(0);
    m_dot_col = cmbd_ind.col(1);
    T_amb_col = cmbd_ind.col(2);
    std::vector<double> T_htf_vec(T_htf_col.data(), T_htf_col.data() + T_htf_col.ncells());
    std::vector<double> m_dot_vec(m_dot_col.data(), m_dot_col.data() + m_dot_col.ncells());
    std::vector<double> T_amb_vec(T_amb_col.data(), T_amb_col.data() + T_amb_col.ncells());

    // get unique values for each independent variable
    set<double, std::less<double>> T_htf_unique(T_htf_col.data(), T_htf_col.data() + T_htf_col.ncells());
    set<double, std::less<double>> m_dot_unique(m_dot_col.data(), m_dot_col.data() + m_dot_col.ncells());
    set<double, std::less<double>> T_amb_unique(T_amb_col.data(), T_amb_col.data() + T_amb_col.ncells());
    std::vector<double> v_T_htf_unique(T_htf_unique.begin(), T_htf_unique.end());
    std::vector<double> v_m_dot_unique(m_dot_unique.begin(), m_dot_unique.end());
    std::vector<double> v_T_amb_unique(T_amb_unique.begin(), T_amb_unique.end());

    // Get HTF temperature levels
    T_htf_des = T_htf_low = T_htf_high = std::numeric_limits<double>::quiet_NaN();
    get_var_setup(v_T_htf_unique, T_htf_vec, T_htf_des, T_htf_low, T_htf_high);

    // Get HTF mass flow rate levels
    m_dot_des = m_dot_low = m_dot_high = std::numeric_limits<double>::quiet_NaN();
    get_var_setup(v_m_dot_unique, m_dot_vec, m_dot_des, m_dot_low, m_dot_high);

    // Get ambient temperature levels
    T_amb_des = T_amb_low = T_amb_high = std::numeric_limits<double>::quiet_NaN();
    get_var_setup(v_T_amb_unique, T_amb_vec, T_amb_des, T_amb_low, T_amb_high);

    // convert combined matrix_t to a vector of vectors
    // inner vector: single row, outer vector: rows
    // check and throw exception for rows with nan values 
    std::vector<std::vector<double>> cmbd_tbl;
    double* row_start;
    double* row_end;
    for (std::size_t i = 0; i < cmbd_ind.nrows(); i++) {
        util::matrix_t<double> i_row = cmbd_ind.row(i);
        row_start = i_row.data();
        row_end = row_start + i_row.ncols();

        std::vector<double> mat_row(row_start, row_end);
        for (size_t j = 0; j < mat_row.size(); j++) {
            if(std::isnan(mat_row[j])){
                throw(C_csp_exception("UDPC table data contains NaN inputs"));
            }
        }
        
        cmbd_tbl.push_back(mat_row);        
    }

    // Check for inputs runs that don't match udpc rules
    // e.g. parametric variable not available at all 3 levels of secondary var level with constant var
    std::vector<std::vector<double>> vv_test(3);
    std::vector<std::vector<double>::iterator> v_it_erase;

    // Check ambient temperatures
    for (std::vector<double>::iterator i_it = v_T_amb_unique.begin(); i_it < v_T_amb_unique.end(); i_it++){
        vv_test[0] = (std::vector<double>{T_htf_low, m_dot_des, * i_it});
        vv_test[1] = (std::vector<double>{T_htf_des, m_dot_des, * i_it});
        vv_test[2] = (std::vector<double>{T_htf_high, m_dot_des, * i_it});
        if (!is_level_in_par(vv_test, cmbd_tbl)){
            v_it_erase.push_back(i_it);
        }
    }
    for (int i = 0; i < v_it_erase.size(); i++){
        v_T_amb_unique.erase(v_it_erase[v_it_erase.size() - 1 - i]);
    }
    v_it_erase.resize(0);

    // Check HTF temperatures
    for (std::vector<double>::iterator i_it = v_T_htf_unique.begin(); i_it < v_T_htf_unique.end(); i_it++){
        vv_test[0] = std::vector<double>{ *i_it, m_dot_low, T_amb_des };
        vv_test[1] = std::vector<double>{ *i_it, m_dot_des, T_amb_des };
        vv_test[2] = std::vector<double>{ *i_it, m_dot_high, T_amb_des };
        if (!is_level_in_par(vv_test, cmbd_tbl)){
            v_it_erase.push_back(i_it);
        }
    }
    for (int i = 0; i < v_it_erase.size(); i++){
        v_T_htf_unique.erase(v_it_erase[v_it_erase.size() - 1 - i]);
    }
    v_it_erase.resize(0);

    // Check HTF mass flow rates
    for (std::vector<double>::iterator i_it = v_m_dot_unique.begin(); i_it < v_m_dot_unique.end(); i_it++){
        vv_test[0] = std::vector<double>{ T_htf_des, *i_it, T_amb_low };
        vv_test[1] = std::vector<double>{ T_htf_des, *i_it, T_amb_des };
        vv_test[2] = std::vector<double>{ T_htf_des, *i_it, T_amb_high };
        if (!is_level_in_par(vv_test, cmbd_tbl)){
            v_it_erase.push_back(i_it);
        }
    }
    for (int i = 0; i < v_it_erase.size(); i++){
        v_m_dot_unique.erase(v_it_erase[v_it_erase.size() - 1 - i]);
    }

    int total_row_check = 3 * (v_m_dot_unique.size() + v_T_amb_unique.size() + v_T_htf_unique.size());

    n_m_dot_pars = v_m_dot_unique.size();
    n_T_amb_pars = v_T_amb_unique.size();
    n_T_htf_pars = v_T_htf_unique.size();

    if (n_m_dot_pars < 4 || n_T_amb_pars < 4 || n_T_htf_pars < 4)
    {
        throw(C_csp_exception("Filtered UDPC parametric for each variable must contain at least 4 unique values"));
    }

    // Build tables for each independent variable
    const int ncols = 13;

    // HTF temperature table w/ HTF mass flow rate levels
    T_htf_ind.resize_fill(n_T_htf_pars, ncols, 0.0);
    std::vector<double> m_dot_levels = std::vector<double>{ m_dot_low, m_dot_des, m_dot_high };

    // For each T_htf (i) and m_dot_htf_ND level (j) combination...
    for (int i = 0; i < n_T_htf_pars; i++)
    {
        for (int j = 0; j < m_dot_levels.size(); j++)
        {
            // Search through combined table and find corresponding row
            for (int k = 0; k < n_table_rows; k++)
            {
                if (std::vector<double>{v_T_htf_unique[i], m_dot_levels[j], T_amb_des} ==
                    std::vector<double>{ cmbd_tbl[k][C_ud_power_cycle_jdm::E_COL_T_HTF], cmbd_tbl[k][C_ud_power_cycle_jdm::E_COL_M_DOT], cmbd_tbl[k][C_ud_power_cycle_jdm::E_COL_T_AMB] })
                {
                    // Set values in independent variable table
                    //    Independent |    Gross Power Output   |   HTF Thermal Power	|   Cooling Parasitics  |	 Water Use 
                    // 0)  Variable   |  1) -   2) 0     3) +   |  4) -   5) 0    6) +  |  7) -    8) 0    9) + | 10) -  11) 0   12) + 
                    T_htf_ind.set_value(cmbd_tbl[k][C_ud_power_cycle_jdm::E_COL_T_HTF], i, 0);
                    T_htf_ind.set_value(cmbd_tbl[k][C_ud_power_cycle_jdm::E_COL_W_CYL], i, 3 * C_ud_power_cycle_jdm::i_W_dot_gross + 1 + j);
                    T_htf_ind.set_value(cmbd_tbl[k][C_ud_power_cycle_jdm::E_COL_Q_CYL], i, 3 * C_ud_power_cycle_jdm::i_Q_dot_HTF + 1 + j);
                    T_htf_ind.set_value(cmbd_tbl[k][C_ud_power_cycle_jdm::E_COL_W_COOL], i, 3 * C_ud_power_cycle_jdm::i_W_dot_cooling + 1 + j);
                    T_htf_ind.set_value(cmbd_tbl[k][C_ud_power_cycle_jdm::E_COL_M_H2O], i, 3 * C_ud_power_cycle_jdm::i_m_dot_water + 1 + j);
                }
            }
        }
    }

    // HTF mass flow rate w/ ambient temperature levels
    m_dot_ind.resize_fill(n_m_dot_pars, ncols, 0.0);
    std::vector<double> T_amb_levels = std::vector<double>{ T_amb_low, T_amb_des, T_amb_high };

    // For each m_dot_htf_ND (i) and T_amb level (j) combination...
    for (int i = 0; i < n_m_dot_pars; i++)
    {
        for (int j = 0; j < T_amb_levels.size(); j++)
        {
            // Search through combined table and find corresponding row
            for (int k = 0; k < n_table_rows; k++)
            {
                if (std::vector<double>{T_htf_des, v_m_dot_unique[i], T_amb_levels[j]} ==
                    std::vector<double>{ cmbd_tbl[k][C_ud_power_cycle_jdm::E_COL_T_HTF], cmbd_tbl[k][C_ud_power_cycle_jdm::E_COL_M_DOT], cmbd_tbl[k][C_ud_power_cycle_jdm::E_COL_T_AMB] })
                {
                    // Set values in independent variable table
                    //    Independent |    Gross Power Output   |   HTF Thermal Power	|   Cooling Parasitics  |	 Water Use 
                    // 0)  Variable   |  1) -   2) 0     3) +   |  4) -   5) 0    6) +  |  7) -    8) 0    9) + | 10) -  11) 0   12) + 
                    m_dot_ind.set_value(cmbd_tbl[k][C_ud_power_cycle_jdm::E_COL_M_DOT], i, 0);
                    m_dot_ind.set_value(cmbd_tbl[k][C_ud_power_cycle_jdm::E_COL_W_CYL], i, 3 * C_ud_power_cycle_jdm::i_W_dot_gross + 1 + j);
                    m_dot_ind.set_value(cmbd_tbl[k][C_ud_power_cycle_jdm::E_COL_Q_CYL], i, 3 * C_ud_power_cycle_jdm::i_Q_dot_HTF + 1 + j);
                    m_dot_ind.set_value(cmbd_tbl[k][C_ud_power_cycle_jdm::E_COL_W_COOL], i, 3 * C_ud_power_cycle_jdm::i_W_dot_cooling + 1 + j);
                    m_dot_ind.set_value(cmbd_tbl[k][C_ud_power_cycle_jdm::E_COL_M_H2O], i, 3 * C_ud_power_cycle_jdm::i_m_dot_water + 1 + j);
                }
            }
        }
    }

    // Ambient temperature w/ HTF temperature levels
    T_amb_ind.resize_fill(n_T_amb_pars, ncols, 0.0);
    std::vector<double> T_htf_levels = std::vector<double>{ T_htf_low, T_htf_des, T_htf_high };

    // For each T_amb (i) and T_HTF level (j) combination...
    for (int i = 0; i < n_T_amb_pars; i++)
    {
        for (int j = 0; j < T_htf_levels.size(); j++)
        {
            // Search through combined table and find corresponding row
            for (int k = 0; k < n_table_rows; k++)
            {
                if (std::vector<double>{T_htf_levels[j], m_dot_des, v_T_amb_unique[i]} ==
                    std::vector<double>{ cmbd_tbl[k][C_ud_power_cycle_jdm::E_COL_T_HTF], cmbd_tbl[k][C_ud_power_cycle_jdm::E_COL_M_DOT], cmbd_tbl[k][C_ud_power_cycle_jdm::E_COL_T_AMB] })
                {
                    // Set values in independent variable table
                    //    Independent |    Gross Power Output   |   HTF Thermal Power	|   Cooling Parasitics  |	 Water Use 
                    // 0)  Variable   |  1) -   2) 0     3) +   |  4) -   5) 0    6) +  |  7) -    8) 0    9) + | 10) -  11) 0   12) + 
                    T_amb_ind.set_value(cmbd_tbl[k][C_ud_power_cycle_jdm::E_COL_T_AMB], i, 0);
                    T_amb_ind.set_value(cmbd_tbl[k][C_ud_power_cycle_jdm::E_COL_W_CYL], i, 3 * C_ud_power_cycle_jdm::i_W_dot_gross + 1 + j);
                    T_amb_ind.set_value(cmbd_tbl[k][C_ud_power_cycle_jdm::E_COL_Q_CYL], i, 3 * C_ud_power_cycle_jdm::i_Q_dot_HTF + 1 + j);
                    T_amb_ind.set_value(cmbd_tbl[k][C_ud_power_cycle_jdm::E_COL_W_COOL], i, 3 * C_ud_power_cycle_jdm::i_W_dot_cooling + 1 + j);
                    T_amb_ind.set_value(cmbd_tbl[k][C_ud_power_cycle_jdm::E_COL_M_H2O], i, 3 * C_ud_power_cycle_jdm::i_m_dot_water + 1 + j);
                }
            }
        }
    }

    return 0;
}

int N_udpc_common_jdm::combine_ind_tbl(util::matrix_t<double>& combined, const util::matrix_t<double>& T_htf_ind,
    const util::matrix_t<double>& m_dot_ind, const util::matrix_t<double>& T_amb_ind,
    double m_dot_low, double m_dot_des, double m_dot_high,
    double T_htf_low, double T_htf_des, double T_htf_high,
    double T_amb_low, double T_amb_des, double T_amb_high)
{
    // Get number of rows in each table
    int n_T_htf_pars = T_htf_ind.nrows();
    int n_m_dot_pars = m_dot_ind.nrows();
    int n_T_amb_pars = T_amb_ind.nrows();

    // Put the low, design, and high ind values in vectors
    std::vector<double> v_T_htf_levels = std::vector<double>{ T_htf_low, T_htf_des, T_htf_high };
    std::vector<double> v_m_dot_levels = std::vector<double>{ m_dot_low, m_dot_des, m_dot_high };
    std::vector<double> v_T_amb_levels = std::vector<double>{ T_amb_low, T_amb_des, T_amb_high };

    size_t total_rows = 3 * (n_T_htf_pars + n_m_dot_pars + n_T_amb_pars);
    const int ncols = 7;

    combined.resize_fill(total_rows, ncols, std::numeric_limits<double>::quiet_NaN());

    for (int j = 0; j < v_m_dot_levels.size(); j++)
    {
        for (int i = 0; i < n_T_htf_pars; i++)
        {
            int r_comb = j * n_T_htf_pars + i;
            double m_dot = v_m_dot_levels[j];

            combined.set_value(T_htf_ind(i, 0), r_comb, C_ud_power_cycle_jdm::E_COL_T_HTF);			// Independent variable
            combined.set_value(m_dot, r_comb, C_ud_power_cycle_jdm::E_COL_M_DOT);						// Level variable
            combined.set_value(T_amb_des, r_comb, C_ud_power_cycle_jdm::E_COL_T_AMB);					// Constant variable

            combined.set_value(T_htf_ind(i, 3 * C_ud_power_cycle_jdm::i_W_dot_gross + 1 + j), r_comb, C_ud_power_cycle_jdm::E_COL_W_CYL);
            combined.set_value(T_htf_ind(i, 3 * C_ud_power_cycle_jdm::i_Q_dot_HTF + 1 + j), r_comb, C_ud_power_cycle_jdm::E_COL_Q_CYL);
            combined.set_value(T_htf_ind(i, 3 * C_ud_power_cycle_jdm::i_W_dot_cooling + 1 + j), r_comb, C_ud_power_cycle_jdm::E_COL_W_COOL);
            combined.set_value(T_htf_ind(i, 3 * C_ud_power_cycle_jdm::i_m_dot_water + 1 + j), r_comb, C_ud_power_cycle_jdm::E_COL_M_H2O);
        }
    }

    for (int j = 0; j < v_T_amb_levels.size(); j++)
    {
        for (int i = 0; i < n_m_dot_pars; i++)
        {
            int r_comb = n_T_htf_pars * v_m_dot_levels.size() + j * n_m_dot_pars + i;
            double T_amb = v_T_amb_levels[j];

            combined.set_value(m_dot_ind(i, 0), r_comb, C_ud_power_cycle_jdm::E_COL_M_DOT);		// Independent variable
            combined.set_value(T_amb, r_comb, C_ud_power_cycle_jdm::E_COL_T_AMB);					// Level variable
            combined.set_value(T_htf_des, r_comb, C_ud_power_cycle_jdm::E_COL_T_HTF);				// Constant variable

            combined.set_value(m_dot_ind(i, 3 * C_ud_power_cycle_jdm::i_W_dot_gross + 1 + j), r_comb, C_ud_power_cycle_jdm::E_COL_W_CYL);
            combined.set_value(m_dot_ind(i, 3 * C_ud_power_cycle_jdm::i_Q_dot_HTF + 1 + j), r_comb, C_ud_power_cycle_jdm::E_COL_Q_CYL);
            combined.set_value(m_dot_ind(i, 3 * C_ud_power_cycle_jdm::i_W_dot_cooling + 1 + j), r_comb, C_ud_power_cycle_jdm::E_COL_W_COOL);
            combined.set_value(m_dot_ind(i, 3 * C_ud_power_cycle_jdm::i_m_dot_water + 1 + j), r_comb, C_ud_power_cycle_jdm::E_COL_M_H2O);
        }
    }

    for (int j = 0; j < v_T_htf_levels.size(); j++)
    {
        for (int i = 0; i < n_T_amb_pars; i++)
        {
            int r_comb = n_T_htf_pars * v_m_dot_levels.size() + n_m_dot_pars * v_T_amb_levels.size() + j * n_T_amb_pars + i;
            double T_htf = v_T_htf_levels[j];

            combined.set_value(T_amb_ind(i, 0), r_comb, C_ud_power_cycle_jdm::E_COL_T_AMB);		// Independent variable
            combined.set_value(T_htf, r_comb, C_ud_power_cycle_jdm::E_COL_T_HTF);					// Level variable
            combined.set_value(m_dot_des, r_comb, C_ud_power_cycle_jdm::E_COL_M_DOT);				// Constant variable

            combined.set_value(T_amb_ind(i, 3 * C_ud_power_cycle_jdm::i_W_dot_gross + 1 + j), r_comb, C_ud_power_cycle_jdm::E_COL_W_CYL);
            combined.set_value(T_amb_ind(i, 3 * C_ud_power_cycle_jdm::i_Q_dot_HTF + 1 + j), r_comb, C_ud_power_cycle_jdm::E_COL_Q_CYL);
            combined.set_value(T_amb_ind(i, 3 * C_ud_power_cycle_jdm::i_W_dot_cooling + 1 + j), r_comb, C_ud_power_cycle_jdm::E_COL_W_COOL);
            combined.set_value(T_amb_ind(i, 3 * C_ud_power_cycle_jdm::i_m_dot_water + 1 + j), r_comb, C_ud_power_cycle_jdm::E_COL_M_H2O);
        }
    }

    return 0;
}
