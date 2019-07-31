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

#include <fstream>
#include <sstream>
#include <stdlib.h>
#include <algorithm>
#include "csp_dispatch.h"
#include "csp_solver_core.h"
#include "lp_lib.h" 
#include "lib_util.h"

//#define _WRITE_AMPL_DATA 1
#define SOS_NONE
//#define SOS_SEQUENCE
//#define SOS_MANUAL
//#define SOS_LPSOLVE

//#define MOD_CYCLE_SHUTDOWN

/*

Careful with namespaces in this file.. importing the LPsolve library introduces new macro definitions
and function definitions.

*/

void __WINAPI opt_logfunction(lprec *lp, void *userhandle, char *buf)
{

    /* do something with buf (the message) */
    csp_dispatch_opt::s_solver_params* par = static_cast<csp_dispatch_opt::s_solver_params*>(userhandle);
    std::string line = buf;
    par->log_message.append( line );
}

int __WINAPI opt_abortfunction(lprec *lp, void *userhandle)
{
    csp_dispatch_opt::s_solver_params* par = static_cast<csp_dispatch_opt::s_solver_params*>(userhandle);
    return par->is_abort_flag ? TRUE : FALSE;
}

void __WINAPI opt_iter_function(lprec *lp, void *userhandle, int msg)
{
    csp_dispatch_opt::s_solver_params* par = static_cast<csp_dispatch_opt::s_solver_params*>(userhandle);

    /*if( get_timeout(lp) > 0 )
        par->is_abort_flag = true;*/

    if( msg == MSG_MILPBETTER )
    {
        par->obj_relaxed = get_bb_relaxed_objective(lp);

        double cur = get_working_objective(lp);

        if(par->obj_relaxed > 0. )
            if( cur / par->obj_relaxed > 1. - par->mip_gap )
                par->is_abort_flag = true;
    }

    if(get_total_iter(lp) > par->max_bb_iter)
        par->is_abort_flag = true;
}


class optimization_vars
{
    int current_mem_pos;
    int alloc_mem_size; 

    REAL *data;
public:
    struct opt_var
    {
        std::string name;
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
    std::vector<opt_var> var_objects;

    unordered_map<std::string, opt_var*> var_by_name;

public:
    struct VAR_TYPE { enum A {REAL_T, INT_T, BINARY_T}; };
    struct VAR_DIM { enum A {DIM_T, DIM_NT, DIM_T2, DIM_2T_TRI}; };

    optimization_vars();
    //~optimization_vars();

    void add_var(char *vname, int var_type /* VAR_TYPE enum */, int var_dim /* VAR_DIM enum */, int var_dim_size, REAL lowbo=-DEF_INFINITE, REAL upbo=DEF_INFINITE);
    void add_var(char *vname, int var_type /* VAR_TYPE enum */, int var_dim /* VAR_DIM enum */, int var_dim_size, int var_dim_size2, REAL lowbo=-DEF_INFINITE, REAL upbo=DEF_INFINITE);

    bool construct();

    int get_num_varobjs();
    int get_total_var_count();

    REAL &operator()(char *varname, int ind);    //Access for 1D var
    REAL &operator()(char *varname, int ind1, int ind2);     //Access for 2D var
    REAL &operator()(int varindex, int ind);    
    REAL &operator()(int varindex, int ind1, int ind2);

    int column(char *varname, int ind);
    int column(char *varname, int ind1, int ind2);
    int column(int varindex, int ind);
    int column(int varindex, int ind1, int ind2);

    REAL *get_variable_array(); 

    opt_var *get_var(char *varname);
    opt_var *get_var(int varindex);
};


csp_dispatch_opt::~csp_dispatch_opt()
{
    if( m_weather )
        delete m_weather;
}


csp_dispatch_opt::csp_dispatch_opt()
{

    //initialize member data
    m_nstep_opt = 0;
    m_current_read_step = 0;
    m_last_opt_successful = false;
    clear_output_arrays();
    m_is_weather_setup = false;
    m_weather = 0;

    //parameters
    params.is_pb_operating0 = false;
    params.is_pb_standby0 = false;
	params.is_pb_startup0 = false;
    params.is_rec_operating0 = false;
	params.is_rec_startup0 = false;
    params.q_pb0 = std::numeric_limits<double>::quiet_NaN();
    params.w_pb0 = std::numeric_limits<double>::quiet_NaN();
	params.u_csu0 = std::numeric_limits<double>::quiet_NaN();
	params.u_rsu0 = std::numeric_limits<double>::quiet_NaN();
    params.dt = std::numeric_limits<double>::quiet_NaN();
    params.e_tes_init = std::numeric_limits<double>::quiet_NaN();          
    params.e_tes_min = std::numeric_limits<double>::quiet_NaN();           
    params.e_tes_max = std::numeric_limits<double>::quiet_NaN();           
    params.q_pb_standby = std::numeric_limits<double>::quiet_NaN();        
    params.e_pb_startup_cold = std::numeric_limits<double>::quiet_NaN();   
    params.e_pb_startup_hot = std::numeric_limits<double>::quiet_NaN();    
    params.e_rec_startup = std::numeric_limits<double>::quiet_NaN();       
    params.dt_pb_startup_cold = std::numeric_limits<double>::quiet_NaN();  
    params.dt_pb_startup_hot = std::numeric_limits<double>::quiet_NaN();   
    params.dt_rec_startup = std::numeric_limits<double>::quiet_NaN();      
    params.tes_degrade_rate = std::numeric_limits<double>::quiet_NaN();    
    params.q_pb_max = std::numeric_limits<double>::quiet_NaN();
    params.q_pb_min = std::numeric_limits<double>::quiet_NaN();
    params.q_rec_min = std::numeric_limits<double>::quiet_NaN();
    params.w_rec_pump = std::numeric_limits<double>::quiet_NaN();
    params.q_pb_des = std::numeric_limits<double>::quiet_NaN();
    params.siminfo = 0;   
    params.col_rec = 0;
	params.mpc_pc = 0;
    params.sf_effadj = 1.;
    params.info_time = 0.;
    params.eta_cycle_ref = std::numeric_limits<double>::quiet_NaN();
    params.disp_time_weighting = std::numeric_limits<double>::quiet_NaN();
    params.rsu_cost = params.csu_cost = params.pen_delta_w = params.q_rec_standby = std::numeric_limits<double>::quiet_NaN();

	params.is_uniform_dt = true;

    outputs.objective = 0.;
    outputs.objective_relaxed = 0.;
    outputs.solve_iter = 0;
    outputs.solve_state = NOTRUN;

    outputs.presolve_nconstr = 0;
    outputs.solve_time = 0.;
    outputs.presolve_nvar = 0;

}

void csp_dispatch_opt::clear_output_arrays()
{
    m_current_read_step = 0;
    m_last_opt_successful = false;

    outputs.objective = std::numeric_limits<double>::quiet_NaN();
    outputs.objective_relaxed = std::numeric_limits<double>::quiet_NaN();
    outputs.pb_standby.clear();
    outputs.pb_operation.clear();
    outputs.q_pb_standby.clear();
    outputs.q_pb_target.clear();
    outputs.rec_operation.clear();
    outputs.eta_pb_expected.clear();
	outputs.f_pb_op_limit.clear();
    outputs.eta_sf_expected.clear();
    outputs.q_sfavail_expected.clear();
    outputs.q_sf_expected.clear();
    outputs.tes_charge_expected.clear();
    outputs.q_pb_startup.clear();
    outputs.q_rec_startup.clear();
    outputs.w_condf_expected.clear();
	outputs.w_pb_target.clear();
    outputs.wnet_lim_min.clear();
    outputs.delta_rs.clear();
	outputs.Qc.clear();
	outputs.s_min.clear();
}

bool csp_dispatch_opt::check_setup(int nstep)
{
    //check parameters and inputs to make sure everything has been set up correctly
    if( !m_is_weather_setup ) return false;
    if( params.siminfo == 0 ) return false;
    
    return true;
}

bool csp_dispatch_opt::copy_weather_data(C_csp_weatherreader &weather_source)
{
    //Copy the weather data
    m_weather = new C_csp_weatherreader( weather_source );

    return m_is_weather_setup = true;
}


bool csp_dispatch_opt::set_up_timestep_array(double horizon, const std::vector<double>& steplengths, const std::vector<double>& steplength_end_time)
{
	dt_array.clear();
	t_elapsed.clear();
	t_weight.clear();

	size_t n = steplengths.size();
	double time = 0.0;
	int nsteps = 0;	
	for (int j = 0; j < n; j++)
	{
		double dt = steplengths.at(j) / 60.;   // Steplength [hr]
		double time_end = std::fmin(horizon, steplength_end_time.at(j));

		int ns = (int)ceil((time_end - time) / dt);  // Number of time steps with this length
		nsteps += ns;
		time += ns * dt;

		for (int i = 0; i < ns; i++)
			dt_array.push_back(dt);
		
		if (time > horizon)
		{
			dt_array[nsteps - 1] -= (time - horizon);
			break;
		}
	}

	m_nstep_opt = (int) dt_array.size();

	t_elapsed.resize(m_nstep_opt);
	t_elapsed.at(0) = dt_array.at(0);
	for (int j = 1; j < m_nstep_opt; j++)
		t_elapsed.at(j) = t_elapsed.at(j - 1) + dt_array.at(j);

	
	t_weight.resize(m_nstep_opt);
	for (int j = 0; j < m_nstep_opt; j++)
		t_weight.at(j) = pow(params.disp_time_weighting, t_elapsed.at(j));

	return true;
}


bool csp_dispatch_opt::translate_to_dispatch_timesteps(double wfstep, std::vector<double>& data)
{
	// Adjust time-resolution of data from constant time steps (wfstep in hr) to specified dispatch optimization time resolution
	// Assumes all dispatch time steps are an integer multiple of the weather file time step

	size_t n = dt_array.size();  // number of dispatch time steps
	std::vector<double> newdata(n, 0.0);
	int step_start = 0;
	int wfstep_sec = (int)ceil(wfstep*3600. - 0.001);  // weather file time step in seconds

	for (size_t j = 0; j < n; j++)
	{
		int dt_sec = (int)ceil(dt_array.at(j)*3600. - 0.001);  // Dispatch time step [s]
		int divs_per_dt = dt_sec / wfstep_sec;					// Number of weather-file time steps in this dispatch time step
		double ave_weight = 1. / (double)divs_per_dt;
		
		for (int i = 0; i < divs_per_dt; i++)
			newdata.at(j) += data.at(step_start + i) * ave_weight;
		step_start += divs_per_dt;
	}
	data = newdata;
	return true;
}

bool csp_dispatch_opt::translate_to_dispatch_timesteps(double wfstep, util::matrix_t<double>& data)
{
	// Adjust time-resolution of data from constant time steps (wfstep in hr) to specified dispatch optimization time resolution
	// Assumes all dispatch time steps are an integer multiple of the weather file time step

	size_t n = dt_array.size();  // number of dispatch time steps
	size_t nc = data.ncols();
	size_t nr = data.nrows();
	util::matrix_t<double> newdata(n, nc, 0.0);
	int step_start = 0;
	int wfstep_sec = (int)ceil(wfstep*3600. - 0.001);  // weather file time step in seconds

	for (size_t j = 0; j < n; j++)
	{
		int dt_sec = (int)ceil(dt_array.at(j)*3600. - 0.001);  // Dispatch time step [s]
		int divs_per_dt = dt_sec / wfstep_sec;					// Number of weather-file time steps in this dispatch time step
		double ave_weight = 1. / (double)divs_per_dt;
		
		for (size_t c = 0; c < nc; c++)
		{ 
			for (int i = 0; i < divs_per_dt; i++)
				newdata.at(j, c) += data.at(step_start + i, c) * ave_weight;
		}
		step_start += divs_per_dt;
	}
	data = newdata;
	return true;
}

template <typename T>
bool csp_dispatch_opt::translate_from_dispatch_timesteps(double wfstep, std::vector<T>& data)
{
	// Adjust time-resolution of data from dispatch time-step resolution to constant weather-file time step for use in ssc
	// Assumes all dispatch time steps are an integer multiple of the weather file time step

	int n_dispatch = (int)dt_array.size();		// Number of dispatch time steps

	if (data.size() != n_dispatch)
		return false;

	int horizon = (int)ceil(t_elapsed.back() * 3600. - 0.001);	// Full dispatch time horizon (s)
	int wfstep_sec = (int)ceil(wfstep*3600. - 0.001);		    // Weather file time step in seconds
	int n_wf = horizon / wfstep_sec;							// Number of time steps in dispatch horizon at weather-file time resolution	
	
	std::vector<T> newdata(n_wf, 0.0);
	int step_start = 0;
	for (int j = 0; j < n_dispatch; j++)
	{
		int dt_sec = (int)ceil(dt_array.at(j)*3600. - 0.001);   // Dispatch time step [s]
		int divs_per_dt = dt_sec / wfstep_sec;					// Number of weather-file time steps in this dispatch time step
		for (int i = 0; i < divs_per_dt; i++)
			newdata.at(step_start + i) = data.at(j);
		step_start += divs_per_dt;
	}
	data = newdata;
	return true;

}

bool csp_dispatch_opt::convert_outputs_to_weatherfile_timesteps(double wfstep)
{
	translate_from_dispatch_timesteps(wfstep, outputs.pb_standby);
	translate_from_dispatch_timesteps(wfstep, outputs.pb_operation);
	translate_from_dispatch_timesteps(wfstep, outputs.q_pb_standby);
	translate_from_dispatch_timesteps(wfstep, outputs.q_pb_target);
	translate_from_dispatch_timesteps(wfstep, outputs.rec_operation);
	translate_from_dispatch_timesteps(wfstep, outputs.q_sf_expected);
	translate_from_dispatch_timesteps(wfstep, outputs.tes_charge_expected);
	translate_from_dispatch_timesteps(wfstep, outputs.q_pb_startup);
	translate_from_dispatch_timesteps(wfstep, outputs.q_rec_startup);
	translate_from_dispatch_timesteps(wfstep, outputs.w_pb_target);
	return true;
}



bool csp_dispatch_opt::predict_performance(int step_start, double horizon, double wfstep)
{
    //Step number - 1-based index for first hour of the year.

    //save step count
	m_nstep_opt = dt_array.size();   // Number of time steps in dispatch optimization
	int nstep_wf =  (int)(horizon / wfstep);  // Number of time steps in optimization horizon at weather-file resolution

    //Predict performance out nstep values. 
    clear_output_arrays();

    if(! check_setup(m_nstep_opt) )
        throw C_csp_exception("Dispatch optimization precheck failed.");

    //create the sim info
    C_csp_solver_sim_info simloc;    // = *params.siminfo;
	simloc.ms_ts.m_step = params.siminfo->ms_ts.m_step;


    double Asf = params.col_rec->get_collector_area();

    //resize the arrays
    int ns = forecast_params.is_stochastic ? forecast_params.n_scenarios : 1;
    outputs.eta_sf_expected.resize(nstep_wf, ns );
    outputs.q_sfavail_expected.resize(nstep_wf, ns );
    outputs.eta_pb_expected.resize(nstep_wf, ns );
    outputs.w_condf_expected.resize(nstep_wf, ns );
	outputs.w_condf_expected.resize(nstep_wf, ns);
	outputs.f_pb_op_limit.resize(nstep_wf, ns);

    C_csp_weatherreader::S_outputs *weatherstep;
    if( forecast_params.is_stochastic )
        weatherstep = new C_csp_weatherreader::S_outputs();

    for(int w=0; w<ns; w++)
    {
        for(int t=0; t<nstep_wf; t++)
        {

            //jump to the current step
            if(! m_weather->read_time_step( step_start+t, simloc ) )
                return false;

            //if stochastic is used, adjust conditions for the scenario 's'
            if( forecast_params.is_stochastic )
            {
                *weatherstep = m_weather->ms_outputs;    //copy

                if( forecast_params.is_dni_scenarios )
                    //if DNI is stochastic, assign the current DNI (beam) to be the value provided in the scenarios table
                    weatherstep->m_beam = forecast_outputs.dni_scenarios.at(t, w);
                else
                    //if no stochastic data was provided, copy over the actual from the weather file here. Don't reassign the value in the weatherstep object.
                    forecast_outputs.dni_scenarios.at(t, w) = weatherstep->m_beam;  

                if( forecast_params.is_tdry_scenarios )
                    //if tdry is stochastic, assign the current tdry to be the value provided in the scenarios table
                    weatherstep->m_tdry = forecast_outputs.tdry_scenarios.at(t, w);
                else
                    //if no stochastic data was provided, copy over the actual from the weather file here. Don't reassign the value in the weatherstep object.
                    forecast_outputs.tdry_scenarios.at(t, w) = weatherstep->m_tdry;  

            }
            else
            {
                weatherstep = &m_weather->ms_outputs;   //point to
            }


            //get DNI
			double dni = weatherstep->m_beam;

            if( m_weather->ms_outputs.m_solzen > 90. || dni < 0. )
                dni = 0.;

            //get optical efficiency
            double opt_eff = params.col_rec->calculate_optical_efficiency(*weatherstep, simloc);

            double q_inc = Asf * opt_eff * dni * 1.e-3; //kW

            //get thermal efficiency
            double therm_eff = params.col_rec->calculate_thermal_efficiency_approx(*weatherstep, q_inc*0.001);
			therm_eff *= params.sf_effadj;


            //store the power cycle efficiency
            double cycle_eff = params.eff_table_Tdb.interpolate( weatherstep->m_tdry );
            cycle_eff *= params.eta_cycle_ref;  

			double f_pb_op_lim_local = std::numeric_limits<double>::quiet_NaN();
			double m_dot_htf_max_local = std::numeric_limits<double>::quiet_NaN();
			params.mpc_pc->get_max_power_output_operation_constraints(weatherstep->m_tdry, m_dot_htf_max_local, f_pb_op_lim_local);


            //store the condenser parasitic power fraction
            double wcond_f = params.wcondcoef_table_Tdb.interpolate( weatherstep->m_tdry );

		    simloc.ms_ts.m_time += simloc.ms_ts.m_step;
            m_weather->converged();

			// update arrays at weather-file resolution
			outputs.eta_sf_expected.at(t, w) = therm_eff;			  // thermal efficiency
			outputs.q_sfavail_expected.at(t, w) = q_inc * therm_eff;  // predicted field energy output
			outputs.eta_pb_expected.at(t, w) = cycle_eff;			  // power cycle efficiency
			outputs.f_pb_op_limit.at(t, w) = f_pb_op_lim_local;		  // maximum power cycle output (normalized)
			outputs.w_condf_expected.at(t, w) = wcond_f;			   //condenser power

        }

    }

	// convert arrays to dispatch time resolution
	translate_to_dispatch_timesteps(wfstep, outputs.eta_sf_expected);
	translate_to_dispatch_timesteps(wfstep, outputs.q_sfavail_expected);
	translate_to_dispatch_timesteps(wfstep, outputs.eta_pb_expected);
	translate_to_dispatch_timesteps(wfstep, outputs.f_pb_op_limit);
	translate_to_dispatch_timesteps(wfstep, outputs.w_condf_expected);

    //reset the weather data reader
    //m_weather->jump_to_timestep(step_start, simloc);
    
    if( forecast_params.is_stochastic )
        delete weatherstep;

    return true;
}

static void calculate_parameters(csp_dispatch_opt *optinst, unordered_map<std::string, double> &pars, int nt)
{
    /* 
    A central location for making sure the parameters from the model are accurately calculated for use in
    the dispatch optimization model. 
    */

        pars["T"] = nt ;

		if (optinst->params.is_uniform_dt)
		{
			pars["delta"] = optinst->params.dt;
		}
        
		pars["Eu"] = optinst->params.e_tes_max ;
        pars["Er"] = optinst->params.e_rec_startup ;
        pars["Ec"] = optinst->params.e_pb_startup_cold ;
        pars["Qu"] = optinst->params.q_pb_max ;
        pars["Ql"] = optinst->params.q_pb_min ;
        pars["Qru"] = optinst->params.e_rec_startup / optinst->params.dt_rec_startup;
        pars["Qrl"] = optinst->params.q_rec_min ;

        pars["Qb"] = optinst->params.q_pb_standby ;
        pars["Lr"] = optinst->params.w_rec_pump ;
        pars["Lc"] = optinst->params.w_cycle_pump;
        pars["Wh"] = optinst->params.w_track;
        pars["Wb"] = optinst->params.w_cycle_standby;
        pars["Ehs"] = optinst->params.w_stow;
        pars["Wht"] = optinst->params.w_rec_ht;
        pars["eta_cycle"] = optinst->params.eta_cycle_ref;
        pars["Qrsd"] = 0.;      //<< not yet modeled, passing temporarily as zero

        
		pars["s0"] = optinst->params.e_tes_init;

		pars["y0"] = (optinst->params.is_pb_operating0 ? 1 : 0);
		pars["ycsb0"] = (optinst->params.is_pb_standby0 ? 1 : 0);
		pars["ycsu0"] = (optinst->params.is_pb_startup0 ? 1 : 0);
		pars["q0"] = optinst->params.q_pb0;
		pars["Wdot0"] = optinst->params.w_pb0;
		pars["ucsu0"] = optinst->params.u_csu0;

		pars["yr0"] = (optinst->params.is_rec_operating0 ? 1 : 0);
		pars["yrsb0"] = 0;
		pars["yrsu0"] = (optinst->params.is_rec_startup0 ? 1 : 0);
		pars["ursu0"] = optinst->params.u_rsu0;
		
		pars["deltal"] = optinst->params.dt_rec_startup;
		
		
		pars["qrecmaxobs"] = 1.;
        for(int i=0; i<(int)optinst->outputs.q_sfavail_expected.nrows(); i++)
            pars["qrecmaxobs"] = optinst->outputs.q_sfavail_expected.at(i,0) > pars["qrecmaxobs"] ? optinst->outputs.q_sfavail_expected.at(i,0) : pars["qrecmaxobs"];

		pars["Qrsb"] = 1.e99; //optinst->params.q_rec_standby; // * dq_rsu;     //.02
        pars["M"] = 1.e6;
        pars["W_dot_cycle"] = optinst->params.q_pb_des * optinst->params.eta_cycle_ref;
		
		/*pars["wlim_min"] = 9.e99;
		for (int t = 0; t < nt; t++)
			pars["wlim_min"] = fmin(pars["wlim_min"], optinst->w_lim.at(t));*/

        //calculate Z parameters
        pars["Z_1"] = 0.;
        pars["Z_2"] = 0.;
        {
            double fi = 0.;
            double fhfi2 = 0.;
            double fhfi = 0.;
            double fhfi_2 = 0.;
            std::vector<double> fiv;
            int m = (int)optinst->params.eff_table_load.get_size();
            for(int i=0; i<m; i++)
            {
                if( i==0 ) continue; // first data point is zero, so skip

                double q, eta, f, fh;
                optinst->params.eff_table_load.get_point(i, q, eta);
                fh = optinst->params.eta_cycle_ref / eta;
                f = q / optinst->params.q_pb_des;
                fiv.push_back(f);

                fi += f;
                fhfi += fh*f;
                fhfi2 += fh*f*f;
                fhfi_2 += fh*fh*f*f;
            }
            m += -1;

            double fi_fhfi = 0.;
            for(int i=0; i<m; i++)
                fi_fhfi += fiv[i]*fhfi;

            pars["Z_1"] = (fhfi2 - 1./(double)m*fi_fhfi)/(fhfi_2 - 1./(double)m *fhfi * fhfi);

            pars["Z_2"] = 1./(double)m * ( fi - pars["Z_1"] * fhfi );
        }

        pars["etap"] = pars["Z_1"]*optinst->params.eta_cycle_ref; //rate2

        double limit1 = (-pars["Z_2"]*pars["W_dot_cycle"])/(pars["Z_1"]*optinst->params.eta_cycle_ref);  //q at point where power curve crosses x-axis

        pars["Wdot0"] = 0.;
        if( pars["q0"] >= pars["Ql"] )
            pars["Wdot0"]= pars["etap"]*pars["q0"]*optinst->outputs.eta_pb_expected.at(0,0);

        pars["Wdotu"] = (pars["Qu"] - limit1) * pars["etap"];
        pars["Wdotl"] = (pars["Ql"] - limit1) * pars["etap"];

        // TES weighting (gamma) and complement (gammac)
        pars["gamma"] = optinst->forecast_params.fc_gamma;
        pars["gammac"] = 1. - pars["gamma"];

        // Adjust wlim if specified value is too low to permit cycle operation
        int ns = optinst->forecast_params.n_scenarios;
        optinst->outputs.wnet_lim_min.resize(nt, ns);
        optinst->outputs.delta_rs.resize(nt, ns);
        for(int s=0; s<ns; s++)
        {
			for(int t=0; t<nt; t++)
			{
				double tstep = optinst->dt_array.at(t);

		        double wmin = (pars["Ql"] * pars["etap"]*optinst->outputs.eta_pb_expected.at(t,s) / optinst->params.eta_cycle_ref) + 
                                (pars["Wdotu"] - pars["etap"]*pars["Qu"])*optinst->outputs.eta_pb_expected.at(t,s) / optinst->params.eta_cycle_ref; // Electricity generation at minimum pb thermal input
				double max_parasitic = 
					pars["Lr"] * optinst->outputs.q_sfavail_expected.at(t,s) 
					+ (optinst->params.w_rec_ht / tstep)
					+ (optinst->params.w_stow / tstep)
					+ optinst->params.w_track 
					+ optinst->params.w_cycle_standby 
					+ optinst->params.w_cycle_pump*pars["Qu"]
						+ optinst->outputs.w_condf_expected.at(t,s)*pars["W_dot_cycle"];  // Largest possible parasitic load at time t

				//save for writing to ampl
                optinst->outputs.wnet_lim_min.at(t, s) =  wmin - max_parasitic;
				if( t < nt-1 )
				{
					double tstep_next = optinst->dt_array.at(t + 1);
					double delta_rec_startup = std::fmin(1., std::fmax(optinst->params.e_rec_startup / std::fmax(optinst->outputs.q_sfavail_expected.at(t + 1, s)*tstep_next, 1.), optinst->params.dt_rec_startup / tstep_next));
					optinst->outputs.delta_rs.at(t, s) = delta_rec_startup;
                }
            }
        }

        //temporary fixed constants
        pars["disp_time_weighting"] = optinst->params.disp_time_weighting;
        pars["rsu_cost"] = optinst->params.rsu_cost; //952.;
        pars["csu_cost"] = optinst->params.csu_cost; //10000.;
        pars["pen_delta_w"] = optinst->params.pen_delta_w; //0.1;



		// Allowable startup energy per period
		optinst->outputs.Qc.resize(nt);
		for (int t = 0; t < nt; t++)
		{
			if (optinst->params.is_uniform_dt)
			{
				pars["Qc"] = optinst->params.e_pb_startup_cold / ceil(optinst->params.dt_pb_startup_cold / pars["delta"]) / pars["delta"];
				optinst->outputs.Qc.at(t) = std::fmin(pars["Qc"], optinst->cap_frac.at(t) * optinst->params.q_pb_des);
			}
			else
			{
				optinst->outputs.Qc.at(t) = optinst->params.e_pb_startup_cold / ceil(optinst->params.dt_pb_startup_cold / optinst->dt_array.at(t)) / optinst->dt_array.at(t);
				optinst->outputs.Qc.at(t) = std::fmin(optinst->outputs.Qc.at(t), optinst->cap_frac.at(t) * optinst->params.q_pb_des);
			}
		}



		//--- Cycle ramp rates 
		pars["max_up"] = optinst->params.pb_max_rampup;     // Max allowable ramp-up rate (fraction of capacity per hour)
		pars["max_down"] = optinst->params.pb_max_rampdown; // Max allowable ramp-down rate (fraction of capacity per hour)

		pars["max_up_v"] = optinst->params.pb_rampup_violation_lim;     // Maximum allowable violation of cycle ramp-up constraint (fraction of capacity per hour)
		pars["max_down_v"] = optinst->params.pb_rampdown_violation_lim;  // Maximum allowable violation of cycle ramp-down constraint (fraction of capacity per hour)

												
		//--- Cycle min up/down time
		pars["Yu"] = optinst->params.pb_minup;		 // Minimum up time [hr]
		pars["Yd"] = optinst->params.pb_mindown;  // Minimum down time [hr]

		pars["tup0"] = (optinst->params.is_pb_operating0) ? optinst->params.pb_persist0 : 0;   // Time up entering this horizon [hr]
		pars["tstby0"] = (optinst->params.is_pb_standby0) ? optinst->params.pb_persist0 : 0;   // Time in standby entering this horizon [hr]
		pars["tdown0"] = (!optinst->params.is_pb_operating0 && !optinst->params.is_pb_standby0) ? optinst->params.pb_persist0 : 0; // Time down entering this horizon [hr]



		
		// Parameters not used in ampl
		if (!optinst->solver_params.is_ampl_engine)
		{
			//--- Decision permanence [hr]
			pars["P_onoff"] = optinst->perm.pb_onoff;
			pars["P_onoff_lookahead"] = optinst->perm.pb_onoff_lookahead;

			pars["P_level"] = optinst->perm.pb_level;
			pars["P_level_lookahead"] = optinst->perm.pb_level_lookahead;

			pars["P_onoff_rec"] = optinst->perm.rec_onoff;
			pars["P_onoff_rec_lookahead"] = optinst->perm.rec_onoff_lookahead;


			//--- Storage buffer
			optinst->outputs.s_min.resize(nt, ns);
			optinst->outputs.s_min.fill(optinst->params.e_tes_buffer);
			if (pars["s0"] < optinst->params.e_tes_buffer)  // Initial storage capacity is below allowable value -> relax constraint until receiver energy is available to make up the difference
			{
				for (int s = 0; s < ns; s++)
				{
					double rec_accum = 0.0;
					double startup_require = optinst->params.e_rec_startup;
					if (optinst->params.is_rec_operating0)
						startup_require = 0.0;

					for (int t = 0; t < nt; t++)
					{

						if (startup_require > 0.0)
						{
							if (optinst->outputs.q_sfavail_expected.at(t, s) < pars["Qru"])
								startup_require = optinst->params.e_rec_startup;
							else
								startup_require -= pars["Qru"] * pars["delta"];
						}

						else
						{
							if (optinst->outputs.q_sfavail_expected.at(t, s) < pars["Qrl"])
								startup_require = optinst->params.e_rec_startup;
							else
								rec_accum += optinst->outputs.q_sfavail_expected.at(t, s) * pars["delta"];  // Available accumulated energy from receiver [kWht]
						}

						if (rec_accum < (optinst->params.e_tes_buffer - pars["s0"]))			// Cumulative receiver energy is insufficient to increase storage above allowable min
							optinst->outputs.s_min.at(t, s) = optinst->params.e_tes_init;		// Relax storage lower bound to initial storage availability
						else
							break;
					}
				}
			}
		}



};

bool csp_dispatch_opt::optimize()
{

    //First check to see whether we should call the AMPL engine instead. 
    if( solver_params.is_ampl_engine )
    {
        return optimize_ampl();
    }
	
	// Skip optimization if non-uniform time steps are specified
	if (!params.is_uniform_dt)
	{
		return false;
	}


    /* 
    Formulate the optimization problem for dispatch generation. We are trying to maximize revenue subject to inventory
    constraints.
    
    
    Variables
    -------------------------------------------------------------
    Continuous
    -------------------------------------------------------------
    xr          kWt     Power delivered by the receiver at time t
    xrsu        kWt     Power used by the reciever for start up
    ursu        kWt     Receiver accumulated start-up thermal power at time t
    x           kWt	    Cycle thermal power consumption at time t 
    ucsu        kWt     Cycle accumulated start-up thermal power at time t
    s           kWht    TES reserve quantity at time t (auxiliary variable) 
    wdot        kWe     Electrical power production at time t
    delta_w     kWe     Positive change in power production at time t w/r/t t-1
    -------------------------------------------------------------
    Binary
    -------------------------------------------------------------
    yr              1 if receiver is generating ``usable'' thermal power at time t; 0 otherwise 
    yrsu            1 if receiver is starting up at time t; 0 otherwise 
    yrsb            1 if receiver is in standby at time t; 0 otherwise
    yrsup           1 if reciever startup penalty is enforced at time t; 0 otherwise
    yrhsp           1 if receiver hot startup penalty is enforced at time t; 0 otherwise
    y               1 if cycle is generating electric power at time t; 0 otherwise
    ycsu            1 if cycle is starting up at time t; 0 otherwise
    ycsb            1 if cycle is in standby mode at time t; 0 otherwise
    ycsup           1 if cycle startup penalty is enforced at time t; 0 otherwise
    ychsp           1 if cycle hot startup penalty is enforced at time t; 0 otherwise
    -------------------------------------------------------------
    */
    lprec *lp;
    int ret = 0;


    try{

        //Calculate the number of variables
        int nt = (int)m_nstep_opt;
		int nt_lookahead = (int)params.nstep_lookahead;

        //set up the variable structure
        optimization_vars O;
        O.add_var("xr", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0. );
        O.add_var("xrsu", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0. );
        O.add_var("ursu", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0. );
        O.add_var("yr", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("yrsu", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        //O.add_var("yrsb", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        //O.add_var("yrsd", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("yrsup", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        //O.add_var("yrhsp", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);

        O.add_var("x", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0.);
        O.add_var("y", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("s", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0. );
        O.add_var("ucsu", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0. );
        O.add_var("ycsu", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("ycsb", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
#ifdef MOD_CYCLE_SHUTDOWN
        O.add_var("ycsd", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
#endif
        O.add_var("ycsup", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("ychsp", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("wdot", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0. ); //0 lower bound?
        O.add_var("delta_w", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0. ); 
        
        unordered_map<std::string, double> P;
        calculate_parameters(this, P, nt);

        O.construct();  //allocates memory for data array

        int nvar = O.get_total_var_count(); //total number of variables in the problem

        lp = make_lp(0, nvar);  //build the context

        if(lp == NULL)
            throw C_csp_exception("Failed to create a new CSP dispatch optimization problem context.");

        //set variable names and types for each column
        for(int i=0; i<O.get_num_varobjs(); i++)
        {
            optimization_vars::opt_var *v = O.get_var(i);

            std::string name_base = v->name;

            if( v->var_dim == optimization_vars::VAR_DIM::DIM_T )
            {
                for(int t=0; t<nt; t++)
                {
                    char s[40];
                    sprintf(s, "%s-%d", name_base.c_str(), t);
                    set_col_name(lp, O.column(i, t), s);
                    
                }
            }
            else if( v->var_dim == optimization_vars::VAR_DIM::DIM_NT ) 
            {
                for(int t1=0; t1<v->var_dim_size; t1++)
                {
                    for(int t2=0; t2<v->var_dim_size2; t2++)
                    {
                        char s[40];
                        sprintf(s, "%s-%d-%d", name_base.c_str(), t1, t2);
                        set_col_name(lp, O.column(i, t1,t2 ), s);
                    }
                }
            }
            else
            {
                for(int t1=0; t1<nt; t1++)
                {
                    for(int t2=t1; t2<nt; t2++)
                    {
                        char s[40];
                        sprintf(s, "%s-%d-%d", name_base.c_str(), t1, t2);
                        set_col_name(lp, O.column(i, t1, t2 ), s);
                    }
                }
            }
        }

        /* 
        --------------------------------------------------------------------------------
        set up the objective function first (per lpsolve guidance)
        --------------------------------------------------------------------------------
        */
		{
            int *col = new int[13 * nt];
            REAL *row = new REAL[13 * nt];
            double tadj = P["disp_time_weighting"];
            int i = 0;

            //calculate the mean price to appropriately weight the receiver production timing derate
            double pmean =0;
            for(int t=0; t<(int)forecast_outputs.price_scenarios.nrows(); t++)
                pmean += forecast_outputs.price_scenarios.at(t,0);
            pmean /= (double)forecast_outputs.price_scenarios.nrows();
            //--

            // penalty for using non-physical consecutive startups to dump energy: -consec_su_pen * (ycsu - ycsup). 
			double consec_su_pen = 0.0;   // No need for a penalty in most cases because constraints force startup/operation to occur in the same time step
			double Qcmin = outputs.Qc.at(0);
			double Qcmax = outputs.Qc.at(0);
			double capfrac_min = cap_frac.at(0);
			for (int t = 1; t < nt; t++)
			{
				Qcmin = std::fmin(Qcmin, outputs.Qc.at(t));
				Qcmin = std::fmax(Qcmax, outputs.Qc.at(t));
				capfrac_min = std::fmin(capfrac_min, cap_frac.at(t));
			}
			if (P["delta"] * Qcmin > P["Ec"] || Qcmax + P["Ql"] > P["Qu"] * capfrac_min)  // Startup requires >1 time step or cycle cannot startup and operate in the same time step
				consec_su_pen = std::fmin(0.9*P["csu_cost"], P["delta"] * pmean*P["W_dot_cycle"]) / ceil(P["Ec"] / (P["delta"] * Qcmin));



            for(int t=0; t<nt; t++)
            {
                i = 0;
                col[ t + nt*(i  ) ] = O.column("wdot", t);
                row[ t + nt*(i++) ] = P["gammac"] * P["delta"] * forecast_outputs.price_scenarios.at(t,0)*tadj*(1.-outputs.w_condf_expected.at(t,0));

                col[ t + nt*(i  ) ] = O.column("xr", t);
                row[ t + nt*(i++) ] = P["gammac"] * (-(P["delta"] * forecast_outputs.price_scenarios.at(t,0) * P["Lr"])+0.1*tadj*pmean);  // tadj added to prefer receiver production sooner (i.e. delay dumping)

                col[ t + nt*(i  ) ] = O.column("xrsu", t);
                row[ t + nt*(i++) ] = P["gammac"] * (-P["delta"] * forecast_outputs.price_scenarios.at(t,0) * P["Lr"]);

                col[ t + nt*(i  ) ] = O.column("yrsu", t);
                row[ t + nt*(i++) ] = -P["gammac"] * forecast_outputs.price_scenarios.at(t,0) * (params.w_rec_ht + params.w_stow);

                col[ t + nt*(i  ) ] = O.column("yr", t);
                row[ t + nt*(i++) ] = P["gammac"] * (-(P["delta"] * forecast_outputs.price_scenarios.at(t,0) * params.w_track) + tadj);	// tadj added to prefer receiver operation in nearer term to longer term

                col[ t + nt*(i  ) ] = O.column("x", t);
                row[ t + nt*(i++) ] = -P["gammac"] * P["delta"] * forecast_outputs.price_scenarios.at(t,0) * params.w_cycle_pump;

                col[ t + nt*(i  ) ] = O.column("ycsb", t);
                row[ t + nt*(i++) ] = -P["gammac"] * P["delta"] * forecast_outputs.price_scenarios.at(t,0) * params.w_cycle_standby;

                //xxcol[ t + nt*(i   ] = O.column("yrsb", t);
                //xxrow[ t + nt*(i++) ] = -delta * forecast_outputs.price_scenarios.at(t,0) * (Lr * Qrl + (params.w_stow / delta));

                //xxcol[ t + nt*(i   ] = O.column("yrsd", t);
                //xxrow[ t + nt*(i++) ] = -0.5 - (params.w_stow);

                //xxcol[ t + nt*(i   ] = O.column("ycsd", t);
                //xxrow[ t + nt*(i++) ] = -0.5;

                col[ t + nt*(i  ) ] = O.column("yrsup", t);
                row[ t + nt*(i++) ] = -P["gammac"] * P["rsu_cost"]*tadj;

                //xxcol[ t + nt*(i   ] = O.column("yrhsp", t);
                //xxrow[ t + nt*(i++) ] = -tadj;

                col[ t + nt*(i  ) ] = O.column("ycsup", t);
				row[t + nt * (i++)] = P["gammac"] * (-P["csu_cost"] * tadj + consec_su_pen * tadj);

                col[ t + nt*(i  ) ] = O.column("ychsp", t);
                row[ t + nt*(i++) ] = P["gammac"] * (-P["csu_cost"]*tadj * 0.1);

                col[ t + nt*(i  ) ] = O.column("delta_w", t);
                row[ t + nt*(i++) ] = -P["gammac"] * P["pen_delta_w"]*tadj;

				col[t + nt * (i)] = O.column("ycsu", t);
				row[t + nt * (i++)] = -P["gammac"] * consec_su_pen *tadj;

                col[t + nt * (i)] = O.column("s", t);
                row[t + nt * (i++)] = t == forecast_params.n_to_update ? P["gamma"] * tadj : 0.;

                tadj *= P["disp_time_weighting"];
            }

            set_obj_fnex(lp, i*nt, row, col);

            delete[] col;
            delete[] row;
        }

        //set the row mode
        set_add_rowmode(lp, TRUE);

        /* 
        --------------------------------------------------------------------------------
        set up the variable properties
        --------------------------------------------------------------------------------
        */
        for(int i=0; i<O.get_num_varobjs(); i++)
        {
            optimization_vars::opt_var *v = O.get_var(i);
            if( v->var_type == optimization_vars::VAR_TYPE::BINARY_T )
            {
                for(int i=v->ind_start; i<v->ind_end; i++)
                    set_binary(lp, i+1, TRUE);
            }
            //upper and lower variable bounds
            for(int i=v->ind_start; i<v->ind_end; i++)
            {
                set_upbo(lp, i+1, v->upper_bound);
                set_lowbo(lp, i+1, v->lower_bound);
            }
        }


        /* 
        --------------------------------------------------------------------------------
        set up the constraints
        --------------------------------------------------------------------------------
        */
        //cycle production change
        {
            REAL row[3];
            int col[3];
            
            for(int t=0; t<nt; t++)
            {
                col[0] = O.column("delta_w", t);
                row[0] = 1.;

                col[1] = O.column("wdot", t);
                row[1] = -1.;

                if(t>0)
                {
                    col[2] = O.column("wdot", t-1);
                    row[2] = 1.;
                    
                    add_constraintex(lp, 3, row, col, GE, 0.);
                }
                else
                {
                    add_constraintex(lp, 2, row, col, GE, -P["Wdot0"]);
                }
            }
        }


        
        {
            //Linearization of the implementation of the piecewise efficiency equation 
            REAL row[3];
            int col[3];

            for(int t=0; t<nt; t++)
            {
                int i=0;
                //power production curve
                row[i  ] = 1.;
                col[i++] = O.column("wdot", t);

                row[i  ] = -P["etap"]*outputs.eta_pb_expected.at(t,0) * eff_frac.at(t)/params.eta_cycle_ref;
                col[i++] = O.column("x", t);

                row[i  ] = -(P["Wdotu"] - P["etap"]*P["Qu"])*outputs.eta_pb_expected.at(t,0)* eff_frac.at(t) /params.eta_cycle_ref;
                col[i++] = O.column("y", t);

                //row[i  ] = -outputs.eta_pb_expected.at(t);
                //col[i++] = O.column("x", t);

                add_constraintex(lp, i, row, col, EQ, 0.);

            }
        }

        // ******************** Receiver constraints *******************
        //{ //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        //    REAL row[5];
        //    int col[5];

        //    for(int t=0; t<nt; t++)
        //    {
        //        int i=0; 
        //        row[i  ] = qrecmaxobs*1.01;
        //        col[i++] = O.column("yd", t);

        //        row[i  ] = 1.;
        //        col[i++] = O.column("xr", t);

        //        row[i  ] = 1.;
        //        col[i++] = O.column("xrsu", t);

        //        add_constraintex(lp, i, row, col, GE, outputs.q_sfavail_expected.at(t, 0)*0.999 );
        //    }
        //} //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


        {
            REAL row[5];
            int col[5];


            for(int t=0; t<nt; t++)
            {

                //Receiver startup inventory
                row[0] = 1.;
                col[0] = O.column("ursu", t);

                row[1] = -P["delta"];
                col[1] = O.column("xrsu", t);

                if(t>0)
                {
                    row[2] = -1.;
                    col[2] = O.column("ursu", t-1);

                    add_constraintex(lp, 3, row, col, LE, 0);
                }
                else
                {
                    add_constraintex(lp, 2, row, col, LE, 0.);
                }

                //-----

                //inventory nonzero
                row[0] = 1.;
                col[0] = O.column("ursu", t);

                row[1] = -P["Er"];
                col[1] = O.column("yrsu", t);

                add_constraintex(lp, 2, row, col, LE, 0.);

                //Receiver operation allowed when:
                row[0] = 1.;
                col[0] = O.column("yr", t);
                
                row[1] = -1.0/P["Er"]; 
                col[1] = O.column("ursu", t);

                if(t>0)
                {
                    row[2] = -1.;
                    col[2] = O.column("yr", t-1);

                    add_constraintex(lp, 3, row, col, LE, 0.); 
                }
                else
                {
                    add_constraintex(lp, 2, row, col, LE, (params.is_rec_operating0 ? 1. : 0.) );
                }

                //Receiver startup can't be enabled after a time step where the Receiver was operating
                if(t>0)
                {
                    row[0] = 1.;
                    col[0] = O.column("yrsu", t);

                    row[1] = 1.;
                    col[1] = O.column("yr", t-1);

                    add_constraintex(lp, 2, row, col, LE, 1.);
                }

                //Receiver startup energy consumption
                row[0] = 1.;
                col[0] = O.column("xrsu", t);

                row[1] = -P["Qru"];
                col[1] = O.column("yrsu", t);

                add_constraintex(lp, 2, row, col, LE, 0.);

                //Receiver startup only during solar positive periods
                row[0] = 1.;
                col[0] = O.column("yrsu", t);

                add_constraintex(lp, 1, row, col, LE, std::fmin(P["M"]*outputs.q_sfavail_expected.at(t,0), 1.0) );

                //Receiver consumption limit
                row[0] = 1.;
                col[0] = O.column("xr", t);

                row[1] = 1.;
                col[1] = O.column("xrsu", t);
                
                add_constraintex(lp, 2, row, col, LE, outputs.q_sfavail_expected.at(t,0));

                //Receiver operation mode requirement
                row[0] = 1.;
                col[0] = O.column("xr", t);

                row[1] = -outputs.q_sfavail_expected.at(t,0);
                col[1] = O.column("yr", t);

                add_constraintex(lp, 2, row, col, LE, 0.);

                //Receiver minimum operation requirement
                row[0] = 1.;
                col[0] = O.column("xr", t);

                row[1] = -P["Qrl"];
                col[1] = O.column("yr", t);

                add_constraintex(lp, 2, row, col, GE, 0.);

                //Receiver can't continue operating when no energy is available
                row[0] = 1.;
                col[0] = O.column("yr", t);

                add_constraintex(lp, 1, row, col, LE, std::fmin(P["M"]*outputs.q_sfavail_expected.at(t,0), 1.0) );  //if any measurable energy, y^r can be 1


                // --- new constraints ---

                //receiver startup/standby persist
                /*row[0] = 1.;
                col[0] = O.column("yrsu", t);

                row[1] = 1.;
                col[1] = O.column("yrsb", t);

                add_constraintex(lp, 2, row, col, LE, 1.);*/

                //recever standby partition
                /*row[0] = 1.;
                col[0] = O.column("yr", t);

                row[1] = 1.;
                col[1] = O.column("yrsb", t);

                add_constraintex(lp, 2, row, col, LE, 1.);*/

                if( t > 0 )
                {
                    //rsb_persist
                    /*row[0] = 1.;
                    col[0] = O.column("yrsb", t);

                    row[1] = -1.;
                    col[1] = O.column("yr", t-1);

                    row[2] = -1.;
                    col[2] = O.column("yrsb", t-1);

                    add_constraintex(lp, 3, row, col, LE, 0.);*/

                    //receiver startup penalty
                    row[0] = 1.;
                    col[0] = O.column("yrsup", t);

                    row[1] = -1.;
                    col[1] = O.column("yrsu", t);

                    row[2] = 1.;
                    col[2] = O.column("yrsu", t-1);

                    add_constraintex(lp, 3, row, col, GE, 0.);

                    //receiver hot startup penalty
                    /*row[0] = 1.;
                    col[0] = O.column("yrhsp", t);

                    row[1] = -1.;
                    col[1] = O.column("yr", t);

                    row[2] = -1.;
                    col[2] = O.column("yrsb", t-1);

                    add_constraintex(lp, 3, row, col, GE, -1);*/

                    //receiver shutdown energy
                    /*row[0] = 1.;
                    col[0] = O.column("yrsd", t-1);

                    row[1] = -1.;
                    col[1] = O.column("yr", t-1);

                    row[2] = 1.;
                    col[2] = O.column("yr", t);

                    row[3] = -1.;
                    col[3] = O.column("yrsb", t-1);

                    row[4] = 1.;
                    col[4] = O.column("yrsb", t);

                    add_constraintex(lp, 5, row, col, GE, 0.);*/

                }
            }
        }

        
        // ******************** Power cycle constraints *******************
		{
			REAL row[5];
			int col[5];

			for (int t = 0; t < nt; t++)
			{

				int i = 0;
				//Startup Inventory balance
				row[i] = 1.;
				col[i++] = O.column("ucsu", t);

				row[i] = -P["delta"] * outputs.Qc.at(t); // -P["delta"] * P["Qc"];
				col[i++] = O.column("ycsu", t);

				if (t > 0)
				{
					row[i] = -1.;
					col[i++] = O.column("ucsu", t - 1);
				}

				add_constraintex(lp, i, row, col, LE, 0.);

				//Inventory nonzero
				row[0] = 1.;
				col[0] = O.column("ucsu", t);

				row[1] = -P["M"];
				col[1] = O.column("ycsu", t);

				add_constraintex(lp, 2, row, col, LE, 0.);


				// Original constraints: applied whenever cycle startup can be completed in a single time step and at least minimum operation can also occur in that timestep
				if (P["delta"] * outputs.Qc.at(t) >= 0.999*P["Ec"] && outputs.Qc.at(t) + P["Ql"] < P["Qu"] * cap_frac.at(t))
				{
					//Cycle operation allowed when:
					i = 0;
					row[i] = 1.;
					col[i++] = O.column("y", t);

					row[i] = -1.0 / P["Ec"];
					col[i++] = O.column("ucsu", t);

					if (t > 0)
					{
						row[i] = -1.;
						col[i++] = O.column("y", t - 1);

						row[i] = -1.;
						col[i++] = O.column("ycsb", t - 1);

						add_constraintex(lp, i, row, col, LE, 0.);
					}
					else
					{
						add_constraintex(lp, i, row, col, LE, (params.is_pb_operating0 ? 1. : 0.) + (params.is_pb_standby0 ? 1. : 0.));
					}


					//Cycle consumption limit
					i = 0;
					row[i] = 1.;
					col[i++] = O.column("x", t);

					//mjw 2016.12.2 --> This constraint seems to be problematic in identifying feasible solutions for subhourly runs. Needs attention.
					row[i] = outputs.Qc.at(t);  // P["Qc"];
					col[i++] = O.column("ycsu", t);

					row[i] = -P["Qu"] * cap_frac.at(t);
					col[i++] = O.column("y", t);

					add_constraintex(lp, i, row, col, LE, 0.);

				}

				// jm 10/2018: Modified constraints applied in cases when cycle startup requires multiple timesteps or full cycle startup and minimum operation can't occur in the same timestep
				else
				{
					//Cycle operation allowed when:
					i = 0;
					row[i] = 1.;
					col[i++] = O.column("y", t);
					row[i] = -P["delta"] * outputs.Qc.at(t) / P["Ec"]; // -P["delta"] * P["Qc"] / P["Ec"];
					col[i++] = O.column("ycsu", t);
					if (t > 0)
					{
						row[i] = -1.0 / P["Ec"];
						col[i++] = O.column("ucsu", t - 1);
						row[i] = -1.;
						col[i++] = O.column("y", t - 1);
						row[i] = -1.;
						col[i++] = O.column("ycsb", t - 1);
						add_constraintex(lp, i, row, col, LE, 0.);
					}
					else
					{
						add_constraintex(lp, i, row, col, LE, (params.is_pb_operating0 ? 1. : 0.) + (params.is_pb_standby0 ? 1. : 0.));
					}

					//Cycle consumption limit
					i = 0;
					row[i] = 1.;
					col[i++] = O.column("x", t);
					row[i] = outputs.Qc.at(t);  //P["Qc"];
					col[i++] = O.column("ycsu", t);
					add_constraintex(lp, 2, row, col, LE, P["Qu"] * cap_frac.at(t));

				}




				//cycle operation mode requirement
				row[0] = 1.;
				col[0] = O.column("x", t);

				row[1] = -P["Qu"] * cap_frac.at(t);
				col[1] = O.column("y", t);

				add_constraintex(lp, 2, row, col, LE, 0.);

				//Minimum cycle energy contribution
				i = 0;
				row[i] = 1.;
				col[i++] = O.column("x", t);

				row[i] = -P["Ql"];
				col[i++] = O.column("y", t);

				add_constraintex(lp, i, row, col, GE, 0);


				// Cycle standby not allowed if constrained capacity less than standby requirement
				row[0] = P["Qb"];
				col[0] = O.column("ycsb", t);
				add_constraintex(lp, 1, row, col, LE, P["Qu"] * cap_frac.at(t));


				//cycle startup can't be enabled after a time step where the cycle was operating
				if (t > 0)
				{
					row[0] = 1.;
					col[0] = O.column("ycsu", t);

					row[1] = 1.;
					col[1] = O.column("y", t - 1);

					add_constraintex(lp, 2, row, col, LE, 1.);
				}

				//cycle startup can't be enabled after a time step where the cycle was in standby
				if (t > 0)
				{
					row[0] = 1.;
					col[0] = O.column("ycsu", t);

					row[1] = 1.;
					col[1] = O.column("ycsb", t - 1);

					add_constraintex(lp, 2, row, col, LE, 1.);
				}

				// cycle startup can't be enabled during first time step if cycle is already operating or in standby
				if (t == 0)
				{
					row[0] = 1.;
					col[0] = O.column("ycsu", t);
					add_constraintex(lp, 1, row, col, LE, std::fmin((params.is_pb_operating0 ? 0. : 1.), (params.is_pb_standby0 ? 0. : 1.)));
				}

				//Standby mode entry
				i = 0;
				row[i] = 1.;
				col[i++] = O.column("ycsb", t);

				if (t > 0)
				{
					row[i] = -1.;
					col[i++] = O.column("y", t - 1);

					row[i] = -1.;
					col[i++] = O.column("ycsb", t - 1);

					add_constraintex(lp, i, row, col, LE, 0);
				}
				else
				{
					add_constraintex(lp, i, row, col, LE, (params.is_pb_standby0 ? 1 : 0) + (params.is_pb_operating0 ? 1 : 0));
				}

				//some modes can't coincide
				row[0] = 1.;
				col[0] = O.column("ycsu", t);
				row[1] = 1.;
				col[1] = O.column("ycsb", t);

				add_constraintex(lp, 2, row, col, LE, 1);

				row[0] = 1.;
				col[0] = O.column("y", t);
				row[1] = 1.;
				col[1] = O.column("ycsb", t);

				add_constraintex(lp, 2, row, col, LE, 1);

				if (t > 0)
				{
					//cycle start penalty
					row[0] = 1.;
					col[0] = O.column("ycsup", t);

					row[1] = -1.;
					col[1] = O.column("ycsu", t);

					row[2] = 1.;
					col[2] = O.column("ycsu", t - 1);

					add_constraintex(lp, 3, row, col, GE, 0.);

					//cycle standby start penalty
					row[0] = 1.;
					col[0] = O.column("ychsp", t);

					row[1] = -1.;
					col[1] = O.column("y", t);

					row[2] = -1.;
					col[2] = O.column("ycsb", t - 1);

					add_constraintex(lp, 3, row, col, GE, -1.);

#ifdef MOD_CYCLE_SHUTDOWN
					//cycle shutdown energy penalty
					row[0] = 1.;
					col[0] = O.column("ycsd", t - 1);

					row[1] = -1.;
					col[1] = O.column("y", t - 1);

					row[2] = 1.;
					col[2] = O.column("y", t);

					row[3] = -1.;
					col[3] = O.column("ycsb", t - 1);

					row[4] = 1.;
					col[4] = O.column("ycsb", t);

					add_constraintex(lp, 5, row, col, GE, 0.);
#endif

				}
			}
		}


		// ******************** Cycle ramp-up and ramp-down rates *******************
		
		{
			REAL row[5];
			int col[5];


			// Convert ramp rate from fraction of capacity per hour to allowable thermal energy per time step
			double Qup = P["max_up"] * P["Qu"] / P["delta"];			  // Maximum increase in thermal energy to the cycle per time step [kWt]
			double Qdown = P["max_down"] * P["Qu"] / P["delta"];		  // Maximum decrease in thermal energy to the cycle per time step [kWt]
			double Qup_incr_su = std::fmax(0., 1.001*P["Ql"] - Qup);      // Allowable increase in max ramp-up at startup (only > 0 if max ramp-up < min operational level) 
			double Qdown_incr_sd = std::fmax(0., 1.001*P["Ql"] - Qdown);  // Allowable increase in max ramp-down at shutdown (only > 0 if max ramp-down < min operational level) 			
			if (Qdown_incr_sd > 0.0 && P["q0"] > 0.0) // Relax ramp-down again if cycle cannot ramp down from initial state fast enough to turn off (occurs occasionally because of discrepancies in dispatch/solver solutions)
			{
				double s = P["s0"];
				double q = P["q0"];
				double qallow = Qdown + Qdown_incr_sd;
				int j = 0;
				while (j < nt && q>qallow)
				{
					q -= Qdown;
					s -= q * P["delta"];
					s += outputs.q_sfavail_expected.at(j, 0) * P["delta"];
					if (q > qallow && s < P["delta"] * P["Ql"])  // Cycle can't be shut off yet, but also can't be on or in standby during the next time step
					{
						Qdown_incr_sd = 1.001*q - Qdown;
						break;
					}
					j++;
				}
			}


			for (int t = 0; t < nt; t++)
			{
				//--- Max cycle ramp-up 
				row[0] = 1.;
				col[0] = O.column("x", t);
				if (t > 0)
				{
					row[1] = -1.;
					col[1] = O.column("x", t - 1);

					row[2] = Qup_incr_su;
					col[2] = O.column("y", t - 1);

					row[3] = Qup_incr_su;
					col[3] = O.column("ycsb", t - 1);

					add_constraintex(lp, 4, row, col, LE, Qup + Qup_incr_su);
				}
				else
				{
					double RHS = P["q0"] + Qup + (1.0 - P["y0"] - P["ycsb0"]) * Qup_incr_su;
					add_constraintex(lp, 1, row, col, LE, RHS);
				}


				//--- Max cycle ramp-down
				row[0] = 1.;
				col[0] = O.column("x", t);

				row[1] = -Qdown_incr_sd;
				col[1] = O.column("y", t);

				row[2] = -Qdown_incr_sd;
				col[2] = O.column("ycsb", t);

				if (t > 0)
				{
					row[3] = -1.;
					col[3] = O.column("x", t - 1);

					add_constraintex(lp, 4, row, col, GE, -Qdown - Qdown_incr_sd);
				}
				
				else
				{
					add_constraintex(lp, 3, row, col, GE, P["q0"] - Qdown - Qdown_incr_sd);
					//double max_avail = P["s0"] / P["delta"] + outputs.q_sfavail_expected.at(t, 0); 
					//if (P["q0"] <= Qdown + Qdown_incr_sd || max_avail > std::fmax(P["Ql"], P["q0"]-Qdown))  // Ramp-down is feasible
					//	add_constraintex(lp, 3, row, col, GE, P["q0"] - Qdown - Qdown_incr_sd);
				}
				
			}
	
        }
		





		// ******************** Cycle min up- and down- times *******************
		{
			REAL row[24];
			int col[24];

			//--- Minimum up time
			int Nup = (int)ceil(P["Yu"] / P["delta"]);  // minimum up time in number of time steps
			int Nup0 = (int)ceil(P["tup0"] / P["delta"]);
			if (Nup > 1)
			{
				int nforce = (P["y0"] == 1) ? (int)std::fmax(0, Nup - Nup0) : 0;	// Number of time steps at beginning of window that cyle has to be up
				double fract = 1. / (float)Nup;

				for (int t = 0; t < nt; t++)
				{
					int i = 0;

					row[i] = 1.;
					col[i++] = O.column("y", t);

					if (t < nforce)  // Cycle must be on (constraints below should also work for in this case, but forcing it here might be faster...)
						add_constraintex(lp, i, row, col, EQ, P["y0"]);  

					else if (t > 0)  // t == 0 case is either unconstrained, or forced to y = 1 above
					{
						row[i] = -1. + fract;
						col[i++] = O.column("y", t - 1);

						int k = (int)std::fmin(Nup, t);  
						for (int j = 2; j <= k; j++)
						{
							row[i] = fract;
							col[i++] = O.column("y", t - j);
						}

						if (k == Nup) // Full minimum up-time window is in this optimization window
							add_constraintex(lp, i, row, col, GE, 0.0);
						else
						{
							int n_up_prev = (int)std::fmin(Nup - t, Nup0);  // Number of time steps in previous window that contribute
							add_constraintex(lp, i, row, col, GE, -fract * n_up_prev);
						}
					}
				}
			}

			//--- Minimum down time
			int Ndown = (int)ceil(P["Yd"] / P["delta"]);
			int Ndown0 = (int)ceil(P["tdown0"] / P["delta"]);
			if (Ndown > 1)
			{
				int nforce = (P["y0"] == 0) ? (int)std::fmax(0, Ndown - Ndown0) : 0;	 // Number of time steps at beginning of window that cyle has to be down
				double fract = 1. / (float)Ndown;

				for (int t = 0; t < nt; t++)
				{
					int i = 0;

					row[i] = 1.;
					col[i++] = O.column("y", t);

					if (t < nforce)  // Cycle must be off (constraints below should also work for in this case, but forcing it here might be faster...)
						add_constraintex(lp, i, row, col, EQ, P["y0"]);

					else if (t > 0)  // t == 0 case is either unconstrained, or forced to y = 0 above
					{
						row[i] = -1. + fract;
						col[i++] = O.column("y", t - 1);

						int k = (int)std::fmin(Ndown, t);  
						for (int j = 2; j <= k; j++)
						{
							row[i] = fract;
							col[i++] = O.column("y", t - j);
						}

						if (k == Ndown) // Full minimum down-time window is in this optimization window
							add_constraintex(lp, i, row, col, LE, 1.0);
						else
						{
							int n_down_prev = (int)std::fmin(Ndown - t, Ndown0);  // Number of time steps in previous window that contribute
							add_constraintex(lp, i, row, col, LE, fract*(n_down_prev+t));
						}
					}
				}
			}

		}



        // ******************** Balance constraints *******************
        //Energy in, out, and stored in the TES system must balance.
        {
            REAL row[7];
            int col[7];

            for(int t=0; t<nt; t++)
            {
                int i=0;

                row[i  ] = P["delta"];
                col[i++] = O.column("xr", t);
                
                row[i  ] = -P["delta"] * outputs.Qc.at(t); //-P["delta"]*P["Qc"];
                col[i++] = O.column("ycsu", t);
                
                row[i  ] = -P["delta"]*P["Qb"]; 
                col[i++] = O.column("ycsb", t);
                
                row[i  ] = -P["delta"];
                col[i++] = O.column("x", t);
#ifdef MOD_REC_STANDBY                
                row[i  ] = -delta*Qrsb;
                col[i++] = O.column("yrsb", t);
#endif
                
                row[i  ] = -1.;
                col[i++] = O.column("s", t);
                
                if(t>0)
                {
                    row[i  ] = 1.;
                    col[i++] = O.column("s", t-1);

                    add_constraintex(lp, i, row, col, EQ, 0.);
                }
                else
                {
                    add_constraintex(lp, i, row, col, EQ, -P["s0"]);  //initial storage state (kWh)
                }
            }
        }
        
        //Energy in storage must be within limits
        {
            REAL row[8];
            int col[8];

            for(int t=0; t<nt; t++)
            {
                
                row[0] = 1.;
                col[0] = O.column("s", t);

                add_constraintex(lp, 1, row, col, LE, P["Eu"]);

				add_constraintex(lp, 1, row, col, GE, outputs.s_min.at(t,0));  // Storage buffer (default value is 0.0)


				//max cycle thermal input in time periods where cycle operates and receiver is starting up
                //outputs.delta_rs.resize(nt);
				if (t < nt - 1)
				{
					/*double delta_rec_startup = min(1., max(params.e_rec_startup / max(outputs.q_sfavail_expected.at(t + 1, 0)*P["delta"], 1.), params.dt_rec_startup / P["delta"]));
                    outputs.delta_rs.at(t) = delta_rec_startup;*/
					double t_rec_startup = outputs.delta_rs.at(t, 0) * P["delta"];
					double large = 5.0*params.q_pb_max;
					int i = 0;

					row[i] = 1.;
					col[i++] = O.column("x", t + 1);

					row[i] = params.q_pb_standby + large;
					col[i++] = O.column("ycsb", t + 1);

					row[i] = -1. / t_rec_startup;
					col[i++] = O.column("s", t);

					row[i] = large;
					col[i++] = O.column("yrsu", t + 1);

					row[i] = large;
					col[i++] = O.column("y", t + 1);

					row[i] = large;
					col[i++] = O.column("y", t);

					row[i] = large;
					col[i++] = O.column("ycsb", t);

					add_constraintex(lp, i, row, col, LE, 3.0*large);
				}

            }
        }

        // Maximum gross electricity production constraint
        {
            REAL row[1];
            int col[1];

            for( int t = 0; t<nt; t++ )
            {
                row[0] = 1.;
                col[0] = O.column("wdot", t);

				add_constraintex(lp, 1, row, col, LE, outputs.f_pb_op_limit.at(t,0) * P["W_dot_cycle"]);
            }
        }

		// Maximum net electricity production constraint
		{
			REAL row[9];
			int col[9];

			for (int t = 0; t<nt; t++)
			{
                
                //check if cycle should be able to operate
				//if (wmin - max_parasitic > w_lim.at(t))		// power cycle operation is impossible at t
                if( outputs.wnet_lim_min.at(t, 0) > w_lim.at(t) )
                {
                    if(w_lim.at(t) > 0)
                        params.messages->add_message(C_csp_messages::NOTICE, "Power cycle operation not possible at time "+ util::to_string(t+1) + ": power limit below minimum operation");                    
                    w_lim.at(t) = 0.;
                    
                }

				if (w_lim.at(t) > 0.)	// Power cycle operation is possible
				{
					int i = 0;

					row[i] = 1.0-outputs.w_condf_expected.at(t,0);
					col[i++] = O.column("wdot", t);

					row[i] = -params.w_rec_pump;
					col[i++] = O.column("xr", t);

					row[i] = -params.w_rec_pump;
					col[i++] = O.column("xrsu", t);

					row[i] = -(params.w_rec_ht / params.dt) - (params.w_stow / params.dt);	//kWe
					col[i++] = O.column("yrsu", t);

					row[i] = -params.w_track;
					col[i++] = O.column("yr", t);

					row[i] = -params.w_cycle_standby;
					col[i++] = O.column("ycsb", t);

					row[i] = -params.w_cycle_pump;
					col[i++] = O.column("x", t);

					//row[i] = -(params.w_rec_pump*params.q_rec_min) - (params.w_stow / params.dt); //kWe
					//col[i++] = O.column("yrsb", t);
					//row[i] - params.w_stow / params.dt;	//kWe
					//col[i++] = O.column("yrsd", t);

					add_constraintex(lp, 7, row, col, LE, w_lim.at(t));
				}
				else // Power cycle operation is impossible at current constrained wlim
				{
					row[0] = 1.0;
					col[0] = O.column("wdot", t);
					add_constraintex(lp, 1, row, col, EQ, 0.);
				}
			}
		}




		// ******************** Decision permanence *******************
		{
			REAL row[2];
			int col[2];

			bool is_decision;

			int np_onoff = (int)ceil(P["P_onoff"] / P["delta"]);
			int np_level = (int)ceil(P["P_level"] / P["delta"]);
			int np_onoff_rec = (int)ceil(P["P_onoff_rec"] / P["delta"]);

			int np_onoff_lookahead = (int)ceil(P["P_onoff_lookahead"] / P["delta"]);
			int np_level_lookahead = (int)ceil(P["P_level_lookahead"] / P["delta"]);
			int np_onoff_rec_lookahead = (int)ceil(P["P_onoff_rec_lookahead"] / P["delta"]);

			for (int t = 0; t < nt; t++)
			{
				// Cycle on/off/standby
				is_decision = true;
				if (t < nt - nt_lookahead && t % np_onoff != 0)  // Optimization window
					is_decision = false;
				if (t >= nt - nt_lookahead && t % np_onoff_lookahead != 0) // Lookahead period
					is_decision = false;

				if (!is_decision)  // Not allowed to change state in this step
				{
					row[0] = 1.;
					col[0] = O.column("y", t);
					row[1] = -1.;
					col[1] = O.column("y", t - 1);
					add_constraintex(lp, 2, row, col, EQ, 0.);

					row[0] = 1.;
					col[0] = O.column("ycsb", t);
					row[1] = -1.;
					col[1] = O.column("ycsb", t - 1);
					add_constraintex(lp, 2, row, col, EQ, 0.);
				}

				// Cycle operational level permanance
				is_decision = true;
				if (t < nt - nt_lookahead && t % np_level != 0)  // Optimization window
					is_decision = false;
				if (t >= nt - nt_lookahead && t % np_level_lookahead != 0) // Lookahead period
					is_decision = false;

				if (!is_decision)  // Not allowed to change operational level
				{
					row[0] = 1.;
					col[0] = O.column("x", t);
					row[1] = -1.;
					col[1] = O.column("x", t - 1);
					add_constraintex(lp, 2, row, col, EQ, 0.);
				}


				// Receiver on/off
				is_decision = true;
				if (t < nt - nt_lookahead && t % np_onoff_rec != 0)  // Optimization window
					is_decision = false;
				if (t >= nt - nt_lookahead && t % np_onoff_rec_lookahead != 0) // Lookahead period
					is_decision = false;

				if (!is_decision)
				{
					row[0] = 1.;
					col[0] = O.column("yr", t);
					row[1] = -1.;
					col[1] = O.column("yr", t - 1);
					add_constraintex(lp, 2, row, col, EQ, 0.);
				}

			}
		}





        
        //Set problem to maximize
        set_maxim(lp);

        //reset the row mode
        set_add_rowmode(lp, FALSE);

        //set the log function
        solver_params.reset();
        
        put_msgfunc(lp, opt_iter_function, (void*)(&solver_params), MSG_ITERATION | MSG_MILPBETTER | MSG_MILPFEASIBLE);
        put_abortfunc(lp, opt_abortfunction, (void*)(&solver_params));
        if( solver_params.disp_reporting > 0 )
        {
            put_logfunc(lp, opt_logfunction, (void*)(&solver_params));
            set_verbose(lp, solver_params.disp_reporting); //http://web.mit.edu/lpsolve/doc/set_verbose.htm
        }
        else
        {
            set_verbose(lp, 0);
        }

        /* 
        The presolve options have been tested and show that the optimal combination of options is as set below.

        Optimality was measured by observing the number of constraints + number of variables that resulted an an 
        annual-averaged basis from each combination.
        */

        /* 
        From the genetic algorithm:

        Presolve        512 
        Branch&Bound    0 32 64 128 256 1024 
        Scaling         7 16 32 64 128


        ----- keep a record of what's been tried historically for each setting ----

        >>> set_presolve
            PRESOLVE_ROWS + PRESOLVE_COLS + PRESOLVE_REDUCEMIP + PRESOLVE_ELIMEQ2 :: original from 2015
            PRESOLVE_ROWS + PRESOLVE_COLS + PRESOLVE_ELIMEQ2 + PRESOLVE_PROBEFIX :: version used as of 12/5/2016
            PRESOLVE_IMPLIEDFREE :: genetic algorithm from 2015

        >> set_bb_rule
            -- combos set for older problem formulation, appropriate as of mid-2015
            NODE_PSEUDOCOSTSELECT + NODE_RCOSTFIXING :: original
            NODE_PSEUDORATIOSELECT + NODE_BREADTHFIRSTMODE :: original v2
            NODE_PSEUDONONINTSELECT + NODE_GREEDYMODE + NODE_DYNAMICMODE + NODE_RCOSTFIXING :: 5m30s, 10.24c
            NODE_PSEUDOCOSTSELECT + NODE_RANDOMIZEMODE + NODE_RCOSTFIXING :: 5m20s, 10.17c
            NODE_GREEDYMODE + NODE_PSEUDOCOSTMODE + NODE_DEPTHFIRSTMODE + NODE_RANDOMIZEMODE + NODE_DYNAMICMODE :: optimal from genetic algorithm
            NODE_PSEUDOCOSTSELECT + NODE_RANDOMIZEMODE :: optimal from independent optimization, THIS VERSION CURRENT AS OF 12/5/2016
        */

        //presolve
        if(solver_params.presolve_type > 0)
            set_presolve(lp, solver_params.presolve_type, get_presolveloops(lp));
        else
            set_presolve(lp, PRESOLVE_ROWS + PRESOLVE_COLS + PRESOLVE_ELIMEQ2 + PRESOLVE_PROBEFIX, get_presolveloops(lp) );   //independent optimization

        //Debugging parameters
        set_timeout(lp, solver_params.solution_timeout);  //max solution time

        //set_bb_depthlimit(lp, -10);   //max branch depth
        //set_solutionlimit(lp, 1);     //only look for 1 optimal solution
        //set_outputfile(lp, "c://users//mwagner//documents//dropbox//nrel//formulation//trace.txt");
        //set_debug(lp, TRUE);

        //branch and bound rule. This one has a big impact on solver performance.
        if(solver_params.bb_type > 0)
            set_bb_rule(lp, solver_params.bb_type);
		else
		{
			set_bb_rule(lp, NODE_RCOSTFIXING + NODE_DYNAMICMODE + NODE_GREEDYMODE + NODE_PSEUDONONINTSELECT);
			if (P["wlim_min"] < 1.e20)
				set_bb_rule(lp, NODE_PSEUDOCOSTSELECT + NODE_DYNAMICMODE);
		}
        
		lprec *lp_original;
		lp_original = copy_lp(lp);
 
       //Problem scaling loop
        int scaling_iter = 0;
        bool return_ok = false;
        while(scaling_iter < 8)
        {
			delete_lp(lp);
			lp = copy_lp(lp_original);
			//set the log function
			solver_params.reset();
			put_msgfunc(lp, opt_iter_function, (void*)(&solver_params), MSG_ITERATION | MSG_MILPBETTER | MSG_MILPFEASIBLE);
			put_abortfunc(lp, opt_abortfunction, (void*)(&solver_params));
			if (solver_params.disp_reporting > 0)
			{
				put_logfunc(lp, opt_logfunction, (void*)(&solver_params));
				set_verbose(lp, solver_params.disp_reporting); //http://web.mit.edu/lpsolve/doc/set_verbose.htm
			}
			else
        {
				set_verbose(lp, 0);
			}


            if( solver_params.scaling_type < 0 && scaling_iter == 0)
            {
                scaling_iter ++;
                continue;
            }

            //Scaling algorithm
            switch(scaling_iter)
            {
            case 0:
                set_scaling(lp, solver_params.scaling_type);
                break;
            case 1:
                //set_scaling(lp,  SCALE_INTEGERS | SCALE_LINEAR | SCALE_GEOMETRIC | SCALE_EQUILIBRATE);  //default
                //set_scaling(lp, SCALE_EXTREME + SCALE_LOGARITHMIC + SCALE_POWER2 + SCALE_EQUILIBRATE + SCALE_INTEGERS + SCALE_DYNUPDATE + SCALE_ROWSONLY); //from noload run
                set_scaling(lp, SCALE_MEAN + SCALE_LOGARITHMIC + SCALE_POWER2 + SCALE_EQUILIBRATE + SCALE_INTEGERS );   //this works really well before trying modified bb weights
                //set_scaling(lp, SCALE_CURTISREID + SCALE_LOGARITHMIC + SCALE_POWER2 + SCALE_EQUILIBRATE + SCALE_INTEGERS );   //genetic algorithm
                break;
            case 2:
                set_scaling(lp, SCALE_NONE);
                break;
            case 3:
                set_scaling(lp, SCALE_CURTISREID | SCALE_LINEAR | SCALE_EQUILIBRATE | SCALE_INTEGERS);
                //set_scaling(lp, SCALE_FUTURE1); 
                break;
            case 4:
                set_scaling(lp,  SCALE_INTEGERS | SCALE_LINEAR | SCALE_GEOMETRIC | SCALE_EQUILIBRATE);  //default
                break;
			case 5:
				set_scaling(lp, SCALE_RANGE + SCALE_LOGARITHMIC + SCALE_POWER2 + SCALE_EQUILIBRATE + SCALE_INTEGERS);
				break;
			case 6:
				set_scaling(lp, SCALE_CURTISREID + SCALE_LOGARITHMIC + SCALE_POWER2 + SCALE_EQUILIBRATE + SCALE_INTEGERS);
				break;
			case 7:
				set_scaling(lp, SCALE_MEAN + SCALE_LOGARITHMIC + SCALE_POWER2 + SCALE_EQUILIBRATE);
				break;
            }


            ret = solve(lp);

            //Collect the dispatch profile and startup flags
            return_ok = ret == OPTIMAL || ret == SUBOPTIMAL;
            
            if(return_ok)
                break;      //break the scaling loop

            //If the problem was reported as unbounded, this probably has to do with poor scaling. Try again with no scaling.
            std::string fail_type;
            switch(ret)
            {
            case UNBOUNDED:
                fail_type = "... Unbounded";
                break;
            case NUMFAILURE:
                fail_type = "... Numerical failure in";
                break;
            case INFEASIBLE:
                fail_type = "... Infeasible";
                break;
            }
            params.messages->add_message(C_csp_messages::NOTICE, fail_type + " dispatch optimization problem. Retrying with modified problem scaling.");
            
            unscale(lp);
            default_basis(lp);

            scaling_iter ++;
        }

		delete_lp(lp_original);

        //keep track of problem efficiency
        outputs.presolve_nconstr = get_Nrows(lp);
        outputs.presolve_nvar = get_Ncolumns(lp);
        outputs.solve_time = time_elapsed(lp);

        //set_outputfile(lp, "C:\\Users\\mwagner\\Documents\\NREL\\SAM\\Dev\\ssc\\branches\\CSP_dev\\build_vc2013\\x64\\setup.txt");
        //print_lp(lp);

        //set_outputfile(lp, "C:\\Users\\mwagner\\Documents\\NREL\\OM Optimization\\cspopt\\software\\sdk\\scripts\\lpsolve_dump\\solution.txt");
        //print_solution(lp, 1);
        

        if(return_ok)
        {
            /*set_outputfile(lp, "C:\\Users\\mwagner\\Documents\\NREL\\OM Optimization\\cspopt\\software\\sdk\\scripts\\lpsolve\\setup.txt");
            print_lp(lp);
            set_outputfile(lp, "C:\\Users\\mwagner\\Documents\\NREL\\OM Optimization\\cspopt\\software\\sdk\\scripts\\lpsolve\\solution.txt");
            print_solution(lp, 1);
            throw;*/

            outputs.objective = get_objective(lp);
            outputs.objective_relaxed = get_bb_relaxed_objective(lp);

            outputs.pb_standby.resize(nt, false);
            outputs.pb_operation.resize(nt, false);
            outputs.q_pb_standby.resize(nt, 0.);
            outputs.q_pb_target.resize(nt, 0.);
            outputs.rec_operation.resize(nt, false);
            outputs.tes_charge_expected.resize(nt, 0.);
            outputs.q_sf_expected.resize(nt, 0.);
            outputs.q_pb_startup.resize(nt, 0.);
            outputs.q_rec_startup.resize(nt, 0.);
            outputs.w_pb_target.resize(nt, 0.);

			int ncols = get_Norig_columns(lp);  // all variables (including presolved variables)
			int nrows = get_Norig_rows(lp);     


            for(int c=1; c<=ncols; c++)
            {
				char *colname = get_origcol_name(lp, c);  // column names before presolve
				double val = get_var_primalresult(lp, nrows + c); // variable value (including presolved variables)

				if(! colname) continue;

                char root[15];

                int i;
                for(i=0; i<15; i++)
                {
                    if(colname[i] == '-')
                    {
                        root[i] = '\0';
                        break;
                    }
                    else
                        root[i] = colname[i];
                }
                int i1=1 + i++;
                char ind[4];
                bool not_interested = false;
                for(i=i1; i<15; i++)
                {
                    if(colname[i] == '-')
                    {
                        //2D variable. Not interested at the moment..
                        not_interested = true;
                        break;
                    }
                    else if(colname[i] == 0)
                    {
                        ind[i-i1] = '\0';
                        break;
                    }
                    else
                        ind[i-i1] = colname[i];
                }

                if(not_interested) continue;  //a 2D variable

                int t = atoi(ind);

                if(strcmp(root, "ycsb") == 0)  //Cycle standby
                {
                    outputs.pb_standby.at(t) = val == 1.;
                }
                else if(strcmp(root, "ycsu") == 0)     //Cycle start up
                {
                    bool su = (fabs(1 - val) < 0.001);
                    outputs.pb_operation.at(t) = outputs.pb_operation.at(t) || su;
					outputs.q_pb_startup.at(t) = su ? outputs.Qc.at(t) : 0.;   //outputs.q_pb_startup.at(t) = su ? P["Qc"] : 0.;
                }
                else if(strcmp(root, "y") == 0)     //Cycle operation
                {
                    outputs.pb_operation.at(t) = outputs.pb_operation.at(t) || ( fabs(1. - val) < 0.001 );
                }
                else if(strcmp(root, "x") == 0)     //Cycle thermal energy consumption
                {
                    outputs.q_pb_target.at(t) = val;
                }
                else if(strcmp(root, "yrsu") == 0)     //Receiver start up
                {
                    outputs.rec_operation.at(t) = outputs.rec_operation.at(t) || (fabs(1 - val) < 0.001);
                }
                else if(strcmp(root, "xrsu") == 0)
                {
                    outputs.q_rec_startup.at(t) = val;
                }
                else if(strcmp(root, "yr") == 0)
                {
                    outputs.rec_operation.at(t) = outputs.rec_operation.at(t) || (fabs(1 - val) < 0.001);
                }
                else if(strcmp(root, "s") == 0)         //Thermal storage charge state
                {
                    outputs.tes_charge_expected.at(t) = val;
                }
                else if(strcmp(root, "xr") == 0)   //receiver production
                {
                    outputs.q_sf_expected.at(t) = val;
                }
                else if(strcmp(root, "wdot") == 0) //electricity production
                {
                    outputs.w_pb_target.at(t) = val;
                }
            }

        }
        else
        {
            //if the optimization wasn't successful, just set the objective values to zero - otherwise they are NAN
            outputs.objective = 0.;
            outputs.objective_relaxed = 0.;
        }

        //record the solve state
        outputs.solve_state = ret;
        
        //get number of iterations
        outputs.solve_iter = (int)get_total_iter(lp);


        delete_lp(lp);
        lp = NULL;

        std::stringstream s;
        int time_start = (int)(params.info_time / 3600.);
        s << "Time " << time_start << " - " << time_start + nt << ": ";

        int type= OPTIMAL;

        switch(ret)
        {
        case UNKNOWNERROR:
            type = C_csp_messages::WARNING;
            s << "... An unknown error occurred while attempting to solve the dispatch optimization problem.";
            break;
        case DATAIGNORED:
            type = C_csp_messages::WARNING;
            s << "Dispatch optimization failed: Data ignored.";
            break;
        case NOBFP:
            type = C_csp_messages::WARNING;
            s << "Dispatch optimization failed: No BFP.";
            break;
        case NOMEMORY:
            type = C_csp_messages::WARNING;
            s << "Dispatch optimization failed: Out of memory.";
            break;
        case NOTRUN:
            type = C_csp_messages::WARNING;
            s << "Dispatch optimization failed: Simulation did not run.";
            break;
        case SUBOPTIMAL:
            type = C_csp_messages::NOTICE;
			s << "Suboptimal solution identified.";
            break;
        case INFEASIBLE:
            type = C_csp_messages::WARNING;
            s << "Dispatch optimization failed: Infeasible problem.";
            break;
        case UNBOUNDED:
            type = C_csp_messages::WARNING;
            s << "Dispatch optimization failed: Unbounded problem.";
            break;
        case DEGENERATE:
            type = C_csp_messages::WARNING;
            s << "Dispatch optimization failed: Degenerate problem.";
            break;
        case NUMFAILURE:
            type = C_csp_messages::WARNING;
            s << "Dispatch optimization failed: Numerical failure.";
            break;
        case USERABORT:
        case TIMEOUT:
            type = C_csp_messages::WARNING;
            s << "Dispatch optimization failed: Iteration or time limit reached before identifying a solution.";
            break;
        case OPTIMAL:
            type = C_csp_messages::NOTICE;
			s << "Optimal solution identified.";
        default:
            break;
        }

        params.messages->add_message(type, s.str() );
        

        if(return_ok)
            write_ampl();


        return return_ok;

    }
    catch(std::exception &e)
    {
        //clean up memory and pass on the exception
        if( lp != NULL )
            delete_lp(lp);
        
        throw e;

    }
    catch(...)
    {
        //clean up memory and pass on the exception
        if( lp != NULL )
            delete_lp(lp);

        return false;
    }

    return false;
}

bool strcompare(std::string a, std::string b)
{
    return util::lower_case(a) < util::lower_case(b);
};

void write_ampl_variable_matrix( std::ofstream &fs, util::matrix_t<double> &data, std::string name )
{
    fs << "param " << name << ": ";
    for(int r=0; r<data.ncols(); r++)
        fs << r+1 << "\t";

    fs << " := \n";

    for(int t=0; t<data.nrows(); t++)
    {
        fs << t+1;
        for( int s=0; s<data.ncols(); s++)
            fs << "\t" << data.at(t,s);

        fs << "\n";
    }
    fs << ";\n\n";

}

void write_ampl_variable_array( std::ofstream &fs, std::vector<double> &data, std::string name )
{
    fs << "param " << name << " := \n";
    for(int t=0; t<data.size(); t++)
        fs << t+1 << "\t" << data.at(t) << "\n";
    fs << ";\n\n";
}

void write_ampl_variable_array( std::ofstream &fs, util::matrix_t<double> &data, std::string name )
{
    fs << "param " << name << " := \n";
    for(int t=0; t<data.nrows(); t++)
        fs << t+1 << "\t" << data.at(t,0) << "\n";
    fs << ";\n\n";
}


std::string csp_dispatch_opt::write_ampl()
{
    /* 
    Write the par file for ampl input

    return name of output file, if error, return empty string.
    */

    std::string sname;

    //write out a data file
    if(solver_params.is_write_ampl_dat || solver_params.is_ampl_engine)
    {
		int day = (int)params.siminfo->ms_ts.m_time / 3600 / 24;
        //char outname[200];
        //sprintf(outname, "%sdata_%d.dat", solver_params.ampl_data_dir.c_str(), day);

        std::stringstream outname;
        //outname << solver_params.ampl_data_dir << "data_" << day << ".dat";        

        outname << solver_params.ampl_data_dir << (solver_params.ampl_data_dir.back() == '/' ? "" : "/") << "sdk_data";
        if( !solver_params.ampl_thread_id.empty() )
            outname << "_" << solver_params.ampl_thread_id;
        outname << ".dat";
        
        sname = outname.str();    //save string

        std::ofstream fout(outname.str().c_str());

        int nt = m_nstep_opt;

        unordered_map<std::string, double> pars;
        calculate_parameters(this, pars, nt);


        fout << "#data file\n\n";
        fout << "# --- scalar parameters ----\n";
        fout << "param day_of_year := " << day << ";\n";

        std::vector<std::string> keys;

        for( unordered_map<std::string, double>::iterator parval = pars.begin(); parval != pars.end(); parval++)
            keys.push_back( parval->first );
        
        std::sort( keys.begin(), keys.end(), strcompare );

        for(size_t k=0; k<keys.size(); k++)
            fout << "param " << keys.at(k) << " := " << pars[keys.at(k)] << ";\n";

        fout << "# --- indexed parameters ---\n";

		write_ampl_variable_array( fout, w_lim, "Wdotnet" ); //net power limit

        if( forecast_params.is_stochastic )
        {
            write_ampl_variable_matrix( fout, outputs.q_sfavail_expected, "Qin" );  //solar field avail energy 
            write_ampl_variable_matrix( fout, forecast_outputs.price_scenarios, "P" ); //price multiplier
            write_ampl_variable_matrix( fout, outputs.eta_pb_expected, "etaamb" ); //power block ambient adjustment
            write_ampl_variable_matrix( fout, outputs.w_condf_expected, "etac" ); //condenser parasitic loss coefficient
            write_ampl_variable_matrix( fout, outputs.wnet_lim_min, "wnet_lim_min" ); //cycle net production lower limit
            write_ampl_variable_matrix( fout, outputs.delta_rs, "delta_rs" ); //receiver expected startup timestep fraction
        }
        else
        {
            write_ampl_variable_array( fout, outputs.q_sfavail_expected, "Qin" );  //solar field avail energy 
            write_ampl_variable_array( fout, forecast_outputs.price_scenarios, "P" ); //price multiplier
            write_ampl_variable_array( fout, outputs.eta_pb_expected, "etaamb" ); //power block ambient adjustment
            write_ampl_variable_array( fout, outputs.w_condf_expected, "etac" ); //condenser parasitic loss coefficient
            write_ampl_variable_array( fout, outputs.wnet_lim_min, "wnet_lim_min" ); //cycle net production lower limit
            write_ampl_variable_array( fout, outputs.delta_rs, "delta_rs" ); //receiver expected startup timestep fraction
        }

		write_ampl_variable_array(fout, cap_frac, "cap_frac");
		write_ampl_variable_array(fout, eff_frac, "eff_frac");
		write_ampl_variable_array(fout, outputs.Qc, "Qc");

		if (!params.is_uniform_dt)
		{
			write_ampl_variable_array(fout, dt_array, "dt");   // Time-step durations [hr]
			write_ampl_variable_array(fout, t_elapsed, "dte"); // Cumulative time elapsed [hr]
			write_ampl_variable_array(fout, t_weight, "twt");  // Time weighting factor
		}


        fout.close();
    }

    return sname;
}

bool csp_dispatch_opt::optimize_ampl()
{
    /* 
    handle the process of writing an input file, running ampl, handling results, and loading solution

    writes
        dat_<day #>.dat
    runs
        sdk_dispatch.run
    expects
        sdk_solution.txt input file
    */

    //check whether the ampl parameters have been configured correctly. If not, throw.
    if(! util::dir_exists( solver_params.ampl_data_dir.c_str() ) )
        throw C_csp_exception("The specified AMPL data directory is invalid.");

    
    std::stringstream tstring;

    std::string datfile = write_ampl();
    if( datfile.empty() )
        throw C_csp_exception("An error occured when writing the AMPL input file.");
    
    //call ampl
    //tstring << "ampl \"" << solver_params.ampl_data_dir << "sdk_dispatch.run\"";

    if( ! system(NULL) ) //puts ("System Ok");
        exit(EXIT_FAILURE);
    
	std::string sday = "Optimizing day " + util::to_string( (int)(params.siminfo->ms_ts.m_time / 3600 / 24) );
	puts( sday.c_str() );

    std::stringstream outfile;
    std::stringstream ampl_call;

    if (solver_params.ampl_exec_call.empty())
    {

        ampl_call << "ampl sdk_solution";
        outfile << "sdk_solution";
        if (solver_params.ampl_thread_id.size() > 0)
        {
            ampl_call << "_" << solver_params.ampl_thread_id;
            outfile << "_" << solver_params.ampl_thread_id;
        }
        ampl_call << ".run >> log.txt;";
        outfile << ".txt";
		system(ampl_call.str().c_str()); 
    }
	else
	{
		system(solver_params.ampl_exec_call.c_str());

		//Use the run file name as the out file name
		int sufpos = solver_params.ampl_exec_call.find(".run");
		if (sufpos < solver_params.ampl_exec_call.size())
		{
			std::string execsub = solver_params.ampl_exec_call.substr(0, sufpos);
			std::vector<std::string> parse = util::split(execsub, "<");
			if (parse.size() > 1)
			{
                outfile << parse.at(1);
                if (solver_params.ampl_thread_id.size() > 0)
                    outfile << "_" << solver_params.ampl_thread_id;
                outfile << ".txt";
			}
		}
	}
    //read back ampl solution
    tstring.str(std::string()); //clear

    tstring << solver_params.ampl_data_dir << (solver_params.ampl_data_dir.back() == '/' ? "" : "/") << outfile.str();
    std::ifstream infile(tstring.str().c_str());

    if(! infile.is_open() )
        return false;

    std::vector< std::string > F;

    std::string line;
    while( std::getline(infile, line ) )
    {
        F.push_back( line );
    }
    /* 
    expects:
    1.  objective (1)
    2.  relaxed objective (1)
    3.  y_t^{csb} (nt)
    4.  y_t (nt)
    5.  energy used for cycle standby (nt)
    6.  x_t (nt)
    7.  y_t^r (nt)
    8.  s_t (nt)
    9.  energy used for cycle startup (nt)
    10. energy used for receiver startup (nt)
    11. x_t^r   solar field energy produced (nt)

    Each item will be on it's own line. Values in arrays are separated by commas.
    */
    
    int nt = m_nstep_opt;

    outputs.pb_standby.resize(nt, false);
    outputs.pb_operation.resize(nt, false);
    outputs.q_pb_standby.resize(nt, 0.);
    outputs.q_pb_target.resize(nt, 0.);
    outputs.rec_operation.resize(nt, false);
    outputs.tes_charge_expected.resize(nt, 0.);
    outputs.q_pb_startup.resize(nt, 0.);
    outputs.q_rec_startup.resize(nt, 0.);
    outputs.q_sf_expected.resize(nt, 0.);
    outputs.w_pb_target.resize(nt, 0.);
    
    util::to_double(F.at(0), &outputs.objective);
    util::to_double(F.at(1), &outputs.objective_relaxed);
    
    std::vector< std::string > svals;

    svals = util::split( F.at(2), "," );

    
    for(int i=0; i<nt; i++)
    {
        int v;
        util::to_integer(svals.at(i), &v );
        outputs.pb_standby.at(i) = v == 1;
    }
    svals = util::split( F.at(3), "," );
    for(int i=0; i<nt; i++)
    {
        int v;
        util::to_integer(svals.at(i), &v );
        outputs.pb_operation.at(i) = v == 1;
    }
    svals = util::split( F.at(4), "," );
    for(int i=0; i<nt; i++)
        util::to_double( svals.at(i), &outputs.q_pb_standby.at(i) );
    svals = util::split( F.at(5), "," );
    for(int i=0; i<nt; i++)
        util::to_double( svals.at(i), &outputs.q_pb_target.at(i) );
    svals = util::split( F.at(6), "," );
    for(int i=0; i<nt; i++)
    {
        int v;
        util::to_integer(svals.at(i), &v );
        outputs.rec_operation.at(i) = v == 1;
    }
    svals = util::split( F.at(7), "," );
    for(int i=0; i<nt; i++)
        util::to_double( svals.at(i), &outputs.tes_charge_expected.at(i) );
    svals = util::split( F.at(8), "," );
    for(int i=0; i<nt; i++)
        util::to_double( svals.at(i), &outputs.q_pb_startup.at(i) );
    svals = util::split( F.at(9), "," );
    for(int i=0; i<nt; i++)
        util::to_double( svals.at(i), &outputs.q_rec_startup.at(i) );
    svals = util::split( F.at(10), "," );
    for(int i=0; i<nt; i++)
        util::to_double( svals.at(i), &outputs.q_sf_expected.at(i) );
    svals = util::split( F.at(11), "," );
    for(int i=0; i<nt; i++)
        util::to_double( svals.at(i), &outputs.w_pb_target.at(i) );

    return true;
}


// ----------------------------------------
// ----------------------------------------


optimization_vars::optimization_vars()
{
    current_mem_pos = 0;
    alloc_mem_size = 0;
}
void optimization_vars::add_var(char *vname, int var_type /* VAR_TYPE enum */, int var_dim /* VAR_DIM enum */, int var_dim_size, REAL lobo, REAL upbo)
{
    if(var_dim == VAR_DIM::DIM_T2)
        add_var(vname, var_type, VAR_DIM::DIM_NT, var_dim_size, var_dim_size, lobo, upbo);
    else
        add_var(vname, var_type, var_dim, var_dim_size, 1, lobo, upbo);

}

void optimization_vars::add_var(char *vname, int var_type /* VAR_TYPE enum */, int var_dim /* VAR_DIM enum */, int var_dim_size, int var_dim_size2, REAL lobo, REAL upbo)
{
    var_objects.push_back( optimization_vars::opt_var() );
    optimization_vars::opt_var *v = &var_objects.back();
    v->name = (std::string)vname;
    v->ind_start = current_mem_pos;
    v->var_type = var_type;
    v->var_dim = var_dim;
    v->var_dim_size = var_dim_size;
    v->var_dim_size2 = var_dim_size2;
    if(v->var_type == optimization_vars::VAR_TYPE::BINARY_T)
    {
        v->upper_bound = 1.;
        v->lower_bound = 0.;
    }
    else
    {
        v->upper_bound = upbo;
        v->lower_bound = lobo;
    }

    //calculate the required memory space for this type of variable
    int mem_size;
    switch (var_dim)
    {
    case optimization_vars::VAR_DIM::DIM_T:
        mem_size = var_dim_size;
        break;
    case optimization_vars::VAR_DIM::DIM_NT:
        mem_size = var_dim_size * var_dim_size2;
        break;
    case optimization_vars::VAR_DIM::DIM_T2:
        throw C_csp_exception("invalid var dimension in add_var");
    case optimization_vars::VAR_DIM::DIM_2T_TRI:
        mem_size = (var_dim_size+1) * var_dim_size/2;
        break;
    }

    v->ind_end = v->ind_start + mem_size;
    
    current_mem_pos += mem_size;

    
}

bool optimization_vars::construct()
{
    if( current_mem_pos < 0 || current_mem_pos > 1000000 )
        throw C_csp_exception("Bad memory allocation when constructing variable table for dispatch optimization.");

    data = new REAL[current_mem_pos];

    alloc_mem_size = current_mem_pos;

    for(int i=0; i<(int)var_objects.size(); i++)
        var_by_name[ var_objects.at(i).name ] = &var_objects.at(i);

	return true;
}

REAL &optimization_vars::operator()(char *varname, int ind)    //Access for 1D var
{
    return data[ var_by_name[varname]->ind_start + ind ];

}

REAL &optimization_vars::operator()(char *varname, int ind1, int ind2)     //Access for 2D var
{
    return data[ column(varname, ind1, ind1)-1 ];
}

REAL &optimization_vars::operator()(int varind, int ind)    //Access for 1D var
{
    return data[ var_objects.at(varind).ind_start + ind ];

}

REAL &optimization_vars::operator()(int varind, int ind1, int ind2)     //Access for 2D var
{
    return data[ column(varind, ind1, ind2)-1 ];
}


int optimization_vars::column(char *varname, int ind)
{
    return var_by_name[varname]->ind_start + ind +1;
}

int optimization_vars::column(char *varname, int ind1, int ind2)
{
    opt_var *v = var_by_name[ std::string(varname) ];
    switch (v->var_dim)
    {
    case VAR_DIM::DIM_T:
        throw C_csp_exception("Attempting to access optimization variable memory via 2D call when referenced variable is 1D.");
    case VAR_DIM::DIM_NT:
        return v->ind_start + v->var_dim_size2 * ind1 + ind2 +1;
    default:
    {
        int ind = v->var_dim_size * ind1 + ind2 - ((ind1-1)*ind1/2);
        return v->ind_start + ind +1;
    }
        break;
    }
}

int optimization_vars::column(int varindex, int ind)
{
    return var_objects[varindex].ind_start + ind +1;
}

int optimization_vars::column(int varindex, int ind1, int ind2)
{
     opt_var *v = &var_objects[varindex];
    switch (v->var_dim)
    {
    case VAR_DIM::DIM_T:
        throw C_csp_exception("Attempting to access optimization variable memory via 2D call when referenced variable is 1D.");
    case VAR_DIM::DIM_NT:        
    	return v->ind_start + v->var_dim_size2 * ind1 + ind2 +1;  
    default:   
    	{       
			int ind = v->var_dim_size * ind1 + ind2 - ((ind1-1)*ind1/2);
			return v->ind_start + ind +1;
    	} break;
    }
}

int optimization_vars::get_num_varobjs()
{
    return (int)var_objects.size();
}

int optimization_vars::get_total_var_count()
{
    return alloc_mem_size;
}

REAL *optimization_vars::get_variable_array()
{
    return data;
}

optimization_vars::opt_var *optimization_vars::get_var(char *varname)
{
    return var_by_name[ varname ];
}

optimization_vars::opt_var *optimization_vars::get_var(int varindex)
{
    return &var_objects[varindex];
}
