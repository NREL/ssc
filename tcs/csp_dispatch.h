/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as “System Advisor Model” or “SAM”. Except
*  to comply with the foregoing, the terms “System Advisor Model”, “SAM”, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

#include <vector>
#include <string>
#include "csp_solver_core.h"

#include <unordered_map>
using std::unordered_map;

#pragma warning(disable: 4290)  // ignore warning: 'C++ exception specification ignored except to indicate a function is not __declspec(nothrow)'

#include "lp_lib.h" 
//#include "glpk\src\glpk.h"

using namespace std;

#ifndef _CSP_DISPATCH
#define _CSP_DISPATCH

class csp_dispatch_opt
{
    int  m_nstep_opt;              //number of time steps in the optimized array
    bool m_is_weather_setup;  //bool indicating whether the weather has been copied
    
    void clear_output_arrays();

public:
    bool m_last_opt_successful;   //last optimization run was successful?
    int m_current_read_step;        //current step to read from optimization results
    vector<double> price_signal;    //IN [- or $/MWh] Price factor indicating market value of generated energy
	vector<double> w_lim;			//[kWe] Limit on net electricity production
    C_csp_weatherreader m_weather;       //Local copy of weather reader object

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
        int presolve_type;
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
        bool is_rec_operating0;     //receiver is operating at the initial time step
        bool is_pb_operating0;      //Power block is operating at the initial time step
        bool is_pb_standby0;        //Power block is in standby at the initial time step
        double q_pb0;               //[kWt] Thermal power consumption in the cycle entering the initial time step
        double dt;                  //[hr] Time step
        double e_tes_init;          //[kWht] current stored energy capacity
        double e_tes_min;           //[kWht] minimum allowable energy capacity in TES
        double e_tes_max;           //[kWht] maximum allowable energy capacity in TES
        double e_pb_startup_cold;   //[kWht] energy requirement to start up the power block from cold state
        double e_pb_startup_hot;    //[kWht] energy requirement to start up the power block from standby
        double e_rec_startup;       //[kWht] energy requirement to start up the reciever
        double dt_pb_startup_cold;  //[hr] time requiremeent to start up the power block from cold state
        double dt_pb_startup_hot;   //[hr] time requiremeent to start up the power block from hot state
        double dt_rec_startup;      //[hr] time requirement to start up the reciever
        double tes_degrade_rate;    //IN [1/hr] Fractional energy loss from tes per hour
        double q_pb_standby;        //[kWt] power requirement to maintain the power block in standby mode
        double q_pb_des;            //[kWe] design cycle thermal power input
        double q_pb_max;            //[kWt] Maximum allowable thermal energy rate to the cycle
        double q_pb_min;            //[kWt] Minimum allowable thermal energy rate to the cycle
        double q_rec_min;           //[kWt] Minimum allowable power delivery by the receiver when operating
        double w_rec_pump;          //[kWe/kWt] Pumping parasitic power per thermal energy produced
        double sf_effadj;           //[-] 0..1 Solar field efficiency adjustment
        double info_time;           //[s] time of the year at sim start. informational only.
        double eta_cycle_ref;       //[kWe/kWt]  Design-point power cycle efficiency
        double disp_time_weighting; //[-] Weighting factor that discounts future decisions over more imminent ones
        double rsu_cost;            //[$/start] Receiver startup cost
        double csu_cost;            //[$/start] Cycle startup cost
        double pen_delta_w;         //[$/kWe-change] Cycle production change penalty
        double q_rec_standby;       //[kWt] Receiver standby thermal power consumption fraction

		double w_rec_ht;			//[kW-hr] Heat trace power during receiver startup
		double w_track;				//[kWe] Heliostat tracing power
		double w_stow;				//[kWe-hr] Heliostat stow electricity requirement
		double w_cycle_standby;		//[kWe] Cycle HTF pumping power during standby
		double w_cycle_pump;		//[kWe/kWt] Cycle HTF pumping power per thermal energy consumed
		double w_dc_field;		    //[kWe] Available DC power generated by the PV field in time t

		int counter;				//[-] Count variable used to keep track of iterations

		/***PV Hybrid new parameters***/
		
		//Battery parameters
		double batt_state0;			//[-] Initial state of charge of battery
		double batt_slope_coeff;	//[V] Battery linear voltage model slope coefficient
		double batt_int_coeff;		//[V] Battery linear voltage model intercept coefficient
		double slope_int_plus;		//[kWe] Bi-directional slope-intercept for charge		-- RENAME
		double slope_int_minus;		//[kWe] Bi-directional slope-intercept for discharge	-- RENAME
		double slope_plus;			//[-] Bi-directional slope for charge					-- RENAME
		double slope_minus;			//[-] Bi-directional slope for discharge				-- RENAME
		double batt_capacity;		//[kAh] Battery manufacturer-specified capacity
		double i_expected;			//[A] Typical current expected from the battery for both charge and discharge activities
		double batt_charge_lb;		//[kA] Battery charge current lower bound
		double batt_charge_ub;		//[kA] Battery charge current upper bound
		double batt_discharge_lb;	//[kA] Battery discharge current lower bound
		double batt_discharge_ub;	//[kA] Battery discharge current upper bound
		double batt_pr_min;			//[kWe] Battery minimum power rating
		double batt_pr_max;			//[kWe] Battery maximum power rating
		double batt_res;			//[Ω] Battery internal resistance
		double batt_soc_min;		//[-] Battery state of charge minimum
		double batt_soc_max;	    //[-] Battery state of charge maximum

		//Hybrid cost parameters
		double rec_helio_cost;		//[$/kWht] Operating cost of heliostat field and receiver
		double pc_cost;				//[$/kWhe] Operating cost of power cycle
		double pc_sb_cost;			//[$/kWht] Operating cost of power cycle standby operation
		double pv_field_cost;		//[$/kWhe] Operating cost of photovoltaic field
		double batt_charge_cost;	//[$/kWhe] Operating cost of charging battery
		double batt_discharge_cost; //[$/kWhe] Operating cost of discharging battery
		double batt_lifecycle_cost; //[$/lifecycle] Lifecycle cost for battery
		double pen_rec_hot_su;		//[$/start] Penalty for receiver hot start-up
		double pen_pc_hot_su;		//[$/start] Penalty for power cycle hot start-up

		//Hybrid power cycle parameters
		double w_ramp_limit;		//[kWe] Power cycle ramp limit per time step
		
		//PV parameters
		double alpha_pv, beta_pv;	//[kWe, -] Inverter slope-intercept linear approximation of DC-to-AC efficiency
		double w_dc_pl;				//[kWe] Inverter DC power limit

		//Misc parameters
		double alpha;				//[$] Conversion factor between unitless and monetary values

        C_csp_solver_sim_info *siminfo;     //Pointer to existing simulation info object
        C_csp_collector_receiver *col_rec;   //Pointer to collector/receiver object
		C_csp_power_cycle *mpc_pc;	// Pointer to csp power cycle class object
		C_csp_messages *messages;   //Pointer to message structure

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
        vector<bool> rec_operation;   //receiver startup ok?
        vector<bool> pb_operation;    //power block startup ok?
        vector<bool> pb_standby;    //power block standby ok?
        vector<double> q_pb_target;       //optimized energy generation (less startup loss)
        vector<double> q_pb_standby;      //standby energy allowed
        vector<double> q_sfavail_expected;       //Expected available solar field energy
        vector<double> q_sf_expected;           //Expected solar field energy generation
        vector<double> eta_pb_expected;     //Expected power cycle conversion efficiency (normalized)
		vector<double> f_pb_op_limit;		//[-] Maximum normalized cycle output
		vector<double> eta_sf_expected;     //Expected solar field thermal efficiency (normalized)
        vector<double> tes_charge_expected;     //Expected thermal energy storage charge state
        vector<double> q_pb_startup;    //thermal power going to startup
        vector<double> q_rec_startup;   //thermal power going to startup
        vector<double> w_pb_target;  //optimized electricity generation
        vector<double> w_condf_expected;  //Expected condenser loss coefficient
        vector<double> wnet_lim_min; //minimum expected net power at time t before cycle gross falls before limit
        vector<double> delta_rs;    //expected proportion of time step used for receiver start up
		//vector<double> e_sales_price;	//Electricity sales price in time t		//already a price output?
		vector<double> w_dc_field;	//Available DC power generated by the PV field in time t	//new output

        int solve_iter;             //Number of iterations required to solve
        int solve_state;
        double solve_time;
        int presolve_nconstr;
        int presolve_nvar;
    } outputs;
    
    struct s_forecast_params
    {
        double coef;
		double fc_gamma = 0.;

    } forecast_params;

    struct s_forecast_outputs
    {

    } forecast_outputs;

    //----- public member functions ----

    csp_dispatch_opt();

    //check parameters and inputs to make sure everything has been set up correctly
    bool check_setup(int nstep);

    //copy the weather data over
    bool copy_weather_data(C_csp_weatherreader &weather_source);

    //multi-variate forecasts
    //bool dispatch_forecast();

    //Predict performance out nstep values. 
    bool predict_performance(int step_start, int ntimeints, int divs_per_int);    

    //declare dispatch function in csp_dispatch.cpp
    bool optimize();

    std::string write_ampl();
    bool optimize_ampl();



    //Get optimized variable states for this timestep
    //s_outputs *get_step_vars(int step);

    
    

};

// ----------------------------------------



class optimization_vars
{
    int current_mem_pos;
    int alloc_mem_size; 

    REAL *data;
public:
    struct opt_var
    {
        string name;
        int var_type;
        int var_dim;
        int var_dim_size;
        int var_dim_size2;
        int ind_start;
        int ind_end;
        REAL upper_bound;
        REAL lower_bound;
    };
private: 
    vector<opt_var> var_objects;

    unordered_map<string, opt_var*> var_by_name;

public:
    struct VAR_TYPE { enum A {REAL_T, INT_T, BINARY_T}; };
    struct VAR_DIM { enum A {DIM_T, DIM_NT, DIM_T2, DIM_2T_TRI}; };

    optimization_vars();
    //~optimization_vars();

    void add_var(const string &vname, int var_type /* VAR_TYPE enum */, int var_dim /* VAR_DIM enum */, int var_dim_size, REAL lowbo=-DEF_INFINITE, REAL upbo=DEF_INFINITE);
    void add_var(const string &vname, int var_type /* VAR_TYPE enum */, int var_dim /* VAR_DIM enum */, int var_dim_size, int var_dim_size2, REAL lowbo=-DEF_INFINITE, REAL upbo=DEF_INFINITE);

    bool construct();

    int get_num_varobjs();
    int get_total_var_count();

    REAL &operator()(char *varname, int ind);    //Access for 1D var
    REAL &operator()(char *varname, int ind1, int ind2);     //Access for 2D var
    REAL &operator()(int varindex, int ind);    
    REAL &operator()(int varindex, int ind1, int ind2);

    int column(const string &varname, int ind);
    int column(const string &varname, int ind1, int ind2);
    int column(int varindex, int ind);
    int column(int varindex, int ind1, int ind2);

    REAL *get_variable_array(); 

    opt_var *get_var(const string &varname);
    opt_var *get_var(int varindex);
};





#endif
