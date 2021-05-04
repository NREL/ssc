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

#include <vector>
#include <string>
#include "csp_solver_core.h"

#include <unordered_map>
using std::unordered_map;

#pragma warning(disable: 4290)  // ignore warning: 'C++ exception specification ignored except to indicate a function is not __declspec(nothrow)'

#include "lp_lib.h" 
//#include "glpk\src\glpk.h"

using namespace std;

#ifndef _ETES_DISPATCH      // TODO: What does this do exactly?
#define _ETES_DISPATCH

class etes_dispatch_opt
{
    int  m_nstep_opt;         //number of time steps in the optimized array
    bool m_is_weather_setup;  //bool indicating whether the weather has been copied

    void clear_params_arrays();
    void clear_output_arrays();

public:
    bool m_last_opt_successful;     //last optimization run was successful?
    int m_current_read_step;        //current step to read from optimization results
    vector<double> w_lim;			//[kWe] Limit on net electricity production
    C_csp_weatherreader m_weather;  //Local copy of weather reader object

    struct s_solver_params
    {
        bool is_abort_flag;         //optimization flagged for abort
        int iter_count;             //branch and bound iteration count
        string log_message;
        double obj_relaxed;

        //user settings
        int max_bb_iter;            //Maximum allowable iterations for B&B algorithm
        double mip_gap;             //convergence tolerance - gap between relaxed MIP solution and current best solution
        double solution_timeout;    //[s] Max solve time for each solution
        int presolve_type;          // presolve type for LPsolve
        int bb_type;  
        int disp_reporting;
        int scaling_type;

        bool is_write_ampl_dat;     //write ampl data files?
        bool is_ampl_engine;        //run with external AMPL engine
        std::string ampl_data_dir;  //directory to write ampl data files
        std::string ampl_exec_call; //system call for running ampl

        s_solver_params()
        {
            bb_type = -1;
            disp_reporting = -1;
            presolve_type = -1;
            scaling_type = -1;
        };

        void reset()
        {
            is_abort_flag = false;
            iter_count = 0;
            log_message.clear();
            obj_relaxed = 0.;
        };

    } solver_params;

    struct s_params
    {
        // Time-indexed parameters
        vector<double> time_elapsed;        //[hr] Cumulative time elapsed at the end of period t
        vector<double> sell_price;   //[$/kWh] Electricity sell price in time t
        vector<double> buy_price;    //[$/kWh] Electricity purchase price in time t

        // to remove
        double sf_effadj;           //[-] 0..1 Solar field efficiency adjustment

        // initial conditions
        bool is_pb_operating0;      //[-] Power block is operating at the initial time step
        double q_pb0;               //[kWt] Thermal power consumption in the cycle entering the initial time step

        bool is_eh_operating0;      //[-] Electric heaters are operating at the initial time step
        double q_eh0;               //[kWt] Thermal power from electric heaters at the initial time step

        bool is_pb_starting0;       //[-] Power block is starting at the initial time step
        double e_pb_start0;         //[-] Power block start energy consumed before initial time step 
        bool is_eh_starting0;       //[-] Electric heaters are starting at the initial time step 
        double e_eh_start0;         //[-] Power block start energy consumed before initial time step 

        double down_time0;          //[hr] Time that has passed since the cycle has been down before the initial time step
        double up_time0;            //[hr] Time that has passed since the cycle has been up before the initial time step

        double e_tes0;              //[kWht] current stored energy capacity

        // scaler parameters
        double disp_time_weighting; //[-] Weighting factor that discounts future decisions over more imminent ones
        double dt;                  //[hr] Time step
        double eta_cycle_ref;       //[kWe/kWt]  Design-point power cycle efficiency
        double eta_eh;              //[kWt/kWe]  Electric heater efficiency

        double dt_pb_startup_cold;       //[hr] time requirement to start up the power block
        double e_pb_startup_cold;        //[kWht] energy requirement to start up the power block

        double dt_rec_startup;       //[hr] time requirement to start up the electrical heaters
        double e_rec_startup;        //[kWht] energy requirement to start up the electrical heaters



        double e_tes_max;           //[kWht] maximum allowable energy capacity in TES
        double e_tes_min;           //[kWht] minimum allowable energy capacity in TES

        double q_pb_des;            //[kWt] design cycle thermal power input
        double q_pb_max;            //[kWt] Maximum allowable thermal energy rate to the cycle
        double q_pb_min;            //[kWt] Minimum allowable thermal energy rate to the cycle

        double q_eh_max;            //[kWt] Maximum allowable power delivery by the electrical heaters when operating
        double q_eh_min;            //[kWt] Minimum allowable power delivery by the electrical heaters when operating

        //double w_eh_pump;           //[kWe/kWt] Pumping parasitic power per thermal energy produced
        //double w_cycle_pump;		//[kWe/kWt] Cycle HTF pumping power per thermal energy consumed

        double csu_cost;            //[$/start] Cycle startup cost
        //double hsu_cost;            //[$/start] electrical heater startup cost
        double pen_delta_w;         //[$/kWe-change] Cycle production change penalty

        double down_time_min;       //[hr] Minimum required power cycle down-time
        double up_time_min;         //[hr] Minimum required power cycle up-time

        double info_time;           //[s] time of the year at sim start. informational only.

        C_csp_solver_sim_info *siminfo;      //Pointer to existing simulation info object
        C_csp_collector_receiver *col_rec;   //Pointer to collector/receiver object
		C_csp_power_cycle *mpc_pc;	         // Pointer to csp power cycle class object
		C_csp_messages *messages;            //Pointer to message structure

        struct s_efftable
        {
        private:
            struct s_effmember
            {
                double x;
                double eta;

                s_effmember(){};
                s_effmember(double _x, double _eta)
                {
                    x = _x;
                    eta = _eta;
                };
            };
            vector<s_effmember> table;

        public:

            void clear()
            {
                table.clear();
            }

            void add_point(double x, double eta)
            {
                table.push_back( s_effmember(x, eta) );
            };

            bool get_point(int index, double &x, double &eta)
            {
                if( index > (int)table.size()-1 || index < 0 ) return false;

                x = table.at(index).x;
                eta = table.at(index).eta;
				return true;
            }

            double get_point_eff(int index)
            {
                return table.at(index).eta;
            }

            double get_point_x(int index)
            {
                return table.at(index).x;
            }

            size_t get_size()
            {
                return table.size();
            }

            double interpolate(double x)
            {

                double eff = table.front().eta;

                int ind = 0;
                int ni = (int)table.size();
                while( true )
                {
                    if( ind ==  ni-1 )
                    {
                        eff = table.back().eta;
                        break;
                    }

                    if( x < table.at(ind).x )
                    {
                        if(ind == 0)
                        {
                            eff = table.front().eta;
                        }
                        else
                        {
                            eff = table.at(ind-1).eta + (table.at(ind).eta - table.at(ind-1).eta)*(x - table.at(ind-1).x)/(table.at(ind).x - table.at(ind-1).x);
                        }
                        break;
                    }

                    ind ++;
                }

                return eff;
            }

        } eff_table_load, eff_table_Tdb, wcondcoef_table_Tdb;        //Efficiency of the power cycle, condenser power coefs
        
    } params;

    struct s_outputs
    {
        double objective;
        double objective_relaxed;
        vector<bool> rec_operation;         //receiver startup ok?
        vector<bool> pb_operation;          //power block startup ok?
        vector<bool> pb_standby;            //power block standby ok?
        vector<double> q_pb_target;         //optimized energy generation (less startup loss)
        vector<double> q_pb_standby;        //standby energy allowed
        vector<double> q_sfavail_expected;  //Expected available solar field energy
        vector<double> q_sf_expected;       //Expected solar field energy generation
        vector<double> eta_pb_expected;     //[kWe/kWt] Expected power cycle conversion efficiency (normalized)
        vector<double> f_pb_op_limit;		//[-] Maximum normalized cycle output
		vector<double> eta_sf_expected;     //Expected solar field thermal efficiency (normalized)
        vector<double> tes_charge_expected; //Expected thermal energy storage charge state
        vector<double> q_pb_startup;        //thermal power going to startup
        vector<double> q_rec_startup;       //thermal power going to startup
        vector<double> w_pb_target;         //optimized electricity generation
        vector<double> w_condf_expected;    //Expected condenser loss coefficient
        vector<double> wnet_lim_min;        //minimum expected net power at time t before cycle gross falls before limit
        vector<double> delta_rs;            //expected proportion of time step used for receiver start up

        int solve_iter;                     //Number of iterations required to solve
        int solve_state;
        double solve_time;
        int presolve_nconstr;
        int presolve_nvar;
    } outputs;
    
    struct s_forecast_params
    {
        double coef;


    } forecast_params;

    struct s_forecast_outputs
    {

    } forecast_outputs;

    //----- public member functions ----

    etes_dispatch_opt();

    //check parameters and inputs to make sure everything has been set up correctly
    bool check_setup(int nstep);

    //copy the weather data over
    bool copy_weather_data(C_csp_weatherreader &weather_source);

    //multi-variate forecasts
    //bool dispatch_forecast();

    //Predict performance out nstep values. 
    bool predict_performance(int step_start, int ntimeints, int divs_per_int);    

    //declare dispatch function in etes_dispatch.cpp
    bool optimize();

    std::string write_ampl();
    bool optimize_ampl();



    //Get optimized variable states for this timestep
    //s_outputs *get_step_vars(int step);

};

// ----------------------------------------


//
//class optimization_vars
//{
//    int current_mem_pos;
//    int alloc_mem_size; 
//
//    REAL *data;
//public:
//    struct opt_var
//    {
//        string name;
//        int var_type;
//        int var_dim;
//        int var_dim_size;
//        int var_dim_size2;
//        int ind_start;
//        int ind_end;
//        REAL upper_bound;
//        REAL lower_bound;
//    };
//private: 
//    vector<opt_var> var_objects;
//
//    unordered_map<string, opt_var*> var_by_name;
//
//public:
//    struct VAR_TYPE { enum A {REAL_T, INT_T, BINARY_T}; };
//    struct VAR_DIM { enum A {DIM_T, DIM_NT, DIM_T2, DIM_2T_TRI}; };
//
//    optimization_vars();
//    //~optimization_vars();
//
//    void add_var(const string &vname, int var_type /* VAR_TYPE enum */, int var_dim /* VAR_DIM enum */, int var_dim_size, REAL lowbo=-DEF_INFINITE, REAL upbo=DEF_INFINITE);
//    void add_var(const string &vname, int var_type /* VAR_TYPE enum */, int var_dim /* VAR_DIM enum */, int var_dim_size, int var_dim_size2, REAL lowbo=-DEF_INFINITE, REAL upbo=DEF_INFINITE);
//
//    bool construct();
//
//    int get_num_varobjs();
//    int get_total_var_count();
//
//    REAL &operator()(char *varname, int ind);    //Access for 1D var
//    REAL &operator()(char *varname, int ind1, int ind2);     //Access for 2D var
//    REAL &operator()(int varindex, int ind);    
//    REAL &operator()(int varindex, int ind1, int ind2);
//
//    int column(const string &varname, int ind);
//    int column(const string &varname, int ind1, int ind2);
//    int column(int varindex, int ind);
//    int column(int varindex, int ind1, int ind2);
//
//    REAL *get_variable_array(); 
//
//    opt_var *get_var(const string &varname);
//    opt_var *get_var(int varindex);
//};
//




#endif
