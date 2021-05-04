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

#include <fstream>
#include <sstream>
#include <stdlib.h>
#include <algorithm>
#include "csp_dispatch.h"   //TODO: including for optimization_vars class -> Needs to move to a new file
#include "etes_dispatch.h"
#include "lp_lib.h" 
#include "lib_util.h"

#define SOS_NONE

/*

Careful with namespaces in this file.. importing the LPsolve library introduces new macro definitions
and function definitions.

*/

//TODO: NEED to move this? So to be accessed by both CSP_dispatch and ETES_dispatch
void __WINAPI etes_opt_logfunction(lprec *lp, void *userhandle, char *buf)
{

    /* do something with buf (the message) */
    etes_dispatch_opt::s_solver_params* par = static_cast<etes_dispatch_opt::s_solver_params*>(userhandle);
    string line = buf;
    par->log_message.append( line );
}

int __WINAPI etes_opt_abortfunction(lprec *lp, void *userhandle)
{
    etes_dispatch_opt::s_solver_params* par = static_cast<etes_dispatch_opt::s_solver_params*>(userhandle);
    return par->is_abort_flag ? TRUE : FALSE;
}

void __WINAPI etes_opt_iter_function(lprec *lp, void *userhandle, int msg)
{
    etes_dispatch_opt::s_solver_params* par = static_cast<etes_dispatch_opt::s_solver_params*>(userhandle);

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


etes_dispatch_opt::etes_dispatch_opt()
{
    //initialize member data
    m_nstep_opt = 0;
    m_current_read_step = 0;
    m_last_opt_successful = false;

    w_lim.clear();  //TODO: Figure out what to do with this
    clear_output_arrays();

    clear_params_arrays();
    //parameters
    params.is_pb_operating0 = false;
    params.q_pb0 = numeric_limits<double>::quiet_NaN();

    params.is_eh_operating0 = false;
    params.q_eh0 = numeric_limits<double>::quiet_NaN();

    params.is_pb_starting0 = false;
    params.e_pb_start0 = numeric_limits<double>::quiet_NaN();
    params.is_eh_starting0 = false;
    params.e_eh_start0 = numeric_limits<double>::quiet_NaN();

    params.down_time0 = numeric_limits<double>::quiet_NaN();
    params.up_time0 = numeric_limits<double>::quiet_NaN();

    params.e_tes0 = numeric_limits<double>::quiet_NaN();

    params.disp_time_weighting = numeric_limits<double>::quiet_NaN();
    params.dt = numeric_limits<double>::quiet_NaN();
    params.eta_cycle_ref = numeric_limits<double>::quiet_NaN();
    params.eta_eh = numeric_limits<double>::quiet_NaN();

    params.dt_pb_startup_cold = numeric_limits<double>::quiet_NaN();
    params.e_pb_startup_cold = numeric_limits<double>::quiet_NaN();

    //params.dt_eh_startup = numeric_limits<double>::quiet_NaN();
    //params.e_eh_startup = numeric_limits<double>::quiet_NaN();

    params.dt_rec_startup = numeric_limits<double>::quiet_NaN();   // TODO: Change naming
    params.e_rec_startup = numeric_limits<double>::quiet_NaN();

    params.e_tes_min = numeric_limits<double>::quiet_NaN();     // TODO: this needs to be accounted for       
    params.e_tes_max = numeric_limits<double>::quiet_NaN();           

    params.q_pb_des = numeric_limits<double>::quiet_NaN();
    params.q_pb_max = numeric_limits<double>::quiet_NaN();
    params.q_pb_min = numeric_limits<double>::quiet_NaN();

    params.q_eh_max = numeric_limits<double>::quiet_NaN();
    params.q_eh_min = numeric_limits<double>::quiet_NaN();

    //params.w_eh_pump = numeric_limits<double>::quiet_NaN();
    //params.w_cycle_pump = numeric_limits<double>::quiet_NaN();

    params.csu_cost = numeric_limits<double>::quiet_NaN();
    //params.hsu_cost = numeric_limits<double>::quiet_NaN();
    params.pen_delta_w = numeric_limits<double>::quiet_NaN();

    params.down_time_min = numeric_limits<double>::quiet_NaN();
    params.up_time_min = numeric_limits<double>::quiet_NaN();

    //TODO: Remove
    params.sf_effadj = 1.;

    params.info_time = 0.;

    params.siminfo = 0;   
    params.col_rec = 0;
	params.mpc_pc = 0;
    params.messages = 0;
    
    outputs.objective = 0.;
    outputs.objective_relaxed = 0.;
    outputs.solve_iter = 0;
    outputs.solve_state = NOTRUN;

    outputs.presolve_nconstr = 0;
    outputs.solve_time = 0.;
    outputs.presolve_nvar = 0;
}

void etes_dispatch_opt::clear_params_arrays()
{
    params.time_elapsed.clear();
    params.sell_price.clear();
    params.buy_price.clear();
}

void etes_dispatch_opt::clear_output_arrays()
{
    m_current_read_step = 0;
    m_last_opt_successful = false;

    outputs.objective = numeric_limits<double>::quiet_NaN();
    outputs.objective_relaxed = numeric_limits<double>::quiet_NaN();
    outputs.pb_standby.clear();
    outputs.pb_operation.clear();
    outputs.q_pb_standby.clear();
    outputs.q_pb_target.clear();
    outputs.rec_operation.clear();
	outputs.f_pb_op_limit.clear();
    outputs.eta_sf_expected.clear();
    outputs.eta_pb_expected.clear();
    outputs.q_sfavail_expected.clear();
    outputs.q_sf_expected.clear();
    outputs.tes_charge_expected.clear();
    outputs.q_pb_startup.clear();
    outputs.q_rec_startup.clear();
    outputs.w_condf_expected.clear();
	outputs.w_pb_target.clear();
    outputs.wnet_lim_min.clear();
    outputs.delta_rs.clear();
}

bool etes_dispatch_opt::check_setup(int nstep)
{
    //check parameters and inputs to make sure everything has been set up correctly
    if( (int)params.sell_price.size() < nstep )   return false;
    if( (int)params.buy_price.size() < nstep )   return false;

    if( !m_is_weather_setup ) return false;
    if( params.siminfo == 0 ) return false;
    
    return true;
}

bool etes_dispatch_opt::copy_weather_data(C_csp_weatherreader &weather_source)
{
    //Copy the weather data
    m_weather = weather_source;
    m_is_weather_setup = true;
    return m_is_weather_setup;
}

bool etes_dispatch_opt::predict_performance(int step_start, int ntimeints, int divs_per_int)
{
    //Step number - 1-based index for first hour of the year.

    //save step count
    m_nstep_opt = ntimeints;

    //Predict performance out nstep values. 
    clear_output_arrays();

    if (!check_setup(m_nstep_opt))
        throw C_csp_exception("Dispatch optimization precheck failed.");

    //create the sim info
    C_csp_solver_sim_info simloc;    // = *params.siminfo;
    simloc.ms_ts.m_step = params.siminfo->ms_ts.m_step;


    double Asf = params.col_rec->get_collector_area();

    double ave_weight = 1. / (double)divs_per_int;

    for (int i = 0; i < m_nstep_opt; i++)
    {
        //initialize hourly average values
        double therm_eff_ave = 0.;
        double cycle_eff_ave = 0.;
        double q_inc_ave = 0.;
        double wcond_ave = 0.;
        double f_pb_op_lim_ave = 0.0;

        for (int j = 0; j < divs_per_int; j++)     //take averages over hour if needed
        {

            //jump to the current step
            if (!m_weather.read_time_step(step_start + i * divs_per_int + j, simloc))
                return false;

            //get DNI
            double dni = m_weather.ms_outputs.m_beam;
            if (m_weather.ms_outputs.m_solzen > 90. || dni < 0.)
                dni = 0.;

            //get optical efficiency
            double opt_eff = params.col_rec->calculate_optical_efficiency(m_weather.ms_outputs, simloc);

            double q_inc = Asf * opt_eff * dni * 1.e-3; //kW

            //get thermal efficiency
            double therm_eff = 1.0; // params.col_rec->calculate_thermal_efficiency_approx(m_weather.ms_outputs, q_inc * 0.001);
            therm_eff *= params.sf_effadj;
            therm_eff_ave += therm_eff * ave_weight;

            //store the predicted field energy output
            q_inc_ave += q_inc * therm_eff * ave_weight;

            //store the power cycle efficiency
            double cycle_eff = params.eff_table_Tdb.interpolate(m_weather.ms_outputs.m_tdry);
            cycle_eff *= params.eta_cycle_ref;
            cycle_eff_ave += cycle_eff * ave_weight;

            double f_pb_op_lim_local = std::numeric_limits<double>::quiet_NaN();
            double m_dot_htf_max_local = std::numeric_limits<double>::quiet_NaN();
            params.mpc_pc->get_max_power_output_operation_constraints(m_weather.ms_outputs.m_tdry, m_dot_htf_max_local, f_pb_op_lim_local);
            f_pb_op_lim_ave += f_pb_op_lim_local * ave_weight;	//[-]

            //store the condenser parasitic power fraction
            double wcond_f = params.wcondcoef_table_Tdb.interpolate(m_weather.ms_outputs.m_tdry);
            wcond_ave += wcond_f * ave_weight;

            simloc.ms_ts.m_time += simloc.ms_ts.m_step;
            m_weather.converged();
        }

        //-----report hourly averages
        //thermal efficiency
        outputs.eta_sf_expected.push_back(therm_eff_ave);
        //predicted field energy output
        outputs.q_sfavail_expected.push_back(q_inc_ave);
        //power cycle efficiency
        outputs.eta_pb_expected.push_back(cycle_eff_ave);
        // Maximum power cycle output (normalized)
        outputs.f_pb_op_limit.push_back(f_pb_op_lim_ave);		//[-]
        //condenser power
        outputs.w_condf_expected.push_back(wcond_ave);
    }

    //reset the weather data reader
    //m_weather.jump_to_timestep(step_start, simloc);

    return true;
}

static void calculate_parameters(etes_dispatch_opt *optinst, unordered_map<std::string, double> &pars, int nt)
{
    /* 
    A central location for making sure the parameters from the model are accurately calculated for use in
    the dispatch optimization model. 
    */

        pars["T"] = nt ;
        pars["delta"] = optinst->params.dt;

        optinst->params.time_elapsed.clear();
        for (int t = 0; t < nt; t++)
        {
            optinst->params.time_elapsed.push_back(pars["delta"] * (t + 1));    //TODO: variable step size? if of interest
        }

        pars["eta_cycle"] = optinst->params.eta_cycle_ref;
        pars["eta_eh"] = optinst->params.eta_eh;
        pars["Ec"] = optinst->params.e_pb_startup_cold;
        pars["Eeh"] = optinst->params.e_rec_startup;

        pars["Eu"] = optinst->params.e_tes_max;
        //pars["El"] = optinst->params.e_tes_min;

        pars["Qu"] = optinst->params.q_pb_max ;
        pars["Ql"] = optinst->params.q_pb_min ;
        pars["Qcsu"] = optinst->params.e_pb_startup_cold / ceil(optinst->params.dt_pb_startup_cold / pars["delta"]) / pars["delta"];

        pars["Qehu"] = optinst->params.q_eh_max;
        pars["Qehl"] = optinst->params.q_eh_min;
        pars["Qhsu"] = optinst->params.e_rec_startup / ceil(optinst->params.dt_rec_startup / pars["delta"]) / pars["delta"];

        //pars["Leh"] = optinst->params.w_eh_pump ;
        //pars["Lc"] = optinst->params.w_cycle_pump;

        pars["disp_time_weighting"] = optinst->params.disp_time_weighting;
        pars["csu_cost"] = optinst->params.csu_cost;
        //pars["hsu_cost"] = optinst->params.hsu_cost;
        pars["pen_delta_w"] = optinst->params.pen_delta_w;

        pars["W_dot_cycle"] = optinst->params.q_pb_des * optinst->params.eta_cycle_ref;
        pars["Yd"] = optinst->params.down_time_min;
        pars["Yu"] = optinst->params.up_time_min;

        // Initial conditions
        pars["y0"] = (optinst->params.is_pb_operating0 ? 1 : 0);
        pars["q0"] = optinst->params.q_pb0;

        pars["yeh0"] = (optinst->params.is_eh_operating0 ? 1 : 0);
        pars["qeh0"] = optinst->params.q_eh0;

        pars["ycsu0"] = (optinst->params.is_pb_starting0 ? 1 : 0);
        pars["ucsu0"] = optinst->params.e_pb_start0;
        pars["yhsu0"] = (optinst->params.is_eh_starting0 ? 1 : 0);
        pars["uhsu0"] = optinst->params.e_eh_start0;

        pars["Yd0"] = optinst->params.down_time0;
        pars["Yu0"] = optinst->params.up_time0;

        pars["s0"] = optinst->params.e_tes0 ;

        for(int i=0; i<(int)optinst->outputs.q_sfavail_expected.size(); i++)
            pars["qrecmaxobs"] = optinst->outputs.q_sfavail_expected.at(i) > pars["qrecmaxobs"] ? optinst->outputs.q_sfavail_expected.at(i) : pars["qrecmaxobs"];

        //linear power-heat fit requires that the efficiency table has 3 points.. 0->zero point, 1->min load point, 2->max load point. This is created in csp_solver_core::Ssimulate().
        int m = optinst->params.eff_table_load.get_size()-1;
        if (m != 2)
            throw C_csp_exception("Model failure during dispatch optimization problem formulation. Ill-formed load table.");
        //get the two points used to create the linear fit
        double q[2], eta[2];
        optinst->params.eff_table_load.get_point(1, q[0], eta[0]);
        optinst->params.eff_table_load.get_point(2, q[1], eta[1]);
        //calculate the rate of change in power output versus heat input
        pars["etap"] = (q[1] * eta[1] - q[0] * eta[0]) / (q[1] - q[0]);
        //calculate the y-intercept of the linear fit at 'b'
        double b = q[1] * eta[1] - q[1] * pars["etap"];
        //locate the heat input at which the linear fit crosses zero power
        //double limit1 = -b / pars["etap"];

        pars["Wdot0"] = 0.;
        if( pars["q0"] >= pars["Ql"] )
            pars["Wdot0"]= (pars["etap"] * pars["q0"] + b) * optinst->outputs.eta_pb_expected.at(0) / optinst->params.eta_cycle_ref;

        //maximum power based on linear fit
        pars["Wdotu"] = (pars["etap"] * pars["Qu"] + b);
        // minimum power based on linear fit
        pars["Wdotl"] = (pars["etap"] * pars["Ql"] + b);

        //TODO: Fixed parameters
        pars["eta_eh"] = 0.95;
        pars["Qhsu"] = 1000.0;
        pars["Eeh"] = 1000.0;
        pars["Qehu"] = pars["Qu"]*2.4;
        pars["Qehl"] = pars["Ql"]*2.4;
        pars["Yd"] = 2.;
        pars["Yu"] = 3.;

        pars["uhsu0"] = 0.;
        pars["ucsu0"] = 0.;
        pars["Yd0"] = 0.;
        pars["Yu0"] = 0.;
        pars["qeh0"] = 0.;

        //pars["s0"] = 0.0;  pars["Eu"];
};

bool etes_dispatch_opt::optimize()
{

    //First check to see whether we should call the AMPL engine instead. 
    if( solver_params.is_ampl_engine )
    {
        return optimize_ampl();
    }

    /* 
    Formulate the optimization problem for dispatch generation. We are trying to maximize revenue subject to inventory
    constraints.
    
    
    Variables
    -------------------------------------------------------------
    Continuous
    -------------------------------------------------------------
    s           kWht    TES reserve quantity at time t
    wdot        kWe     Electrical power production at time t
    delta_w     kWe     Change in power production at time t w/r/t t-1
    qdot        kWt	    Cycle thermal power consumption at time t
    qeh         kWt     Thermal power delivered by the electric heaters at time t
    ucsu        kWt     Cycle accumulated start-up thermal power at time t
    uhsu        kWt     Heaters accumulated start-up thermal power at time t
    -------------------------------------------------------------
    Binary
    -------------------------------------------------------------
    y               1 if cycle is generating electric power at time t; 0 otherwise
    ycgb            1 if cycle begins electric power generation at time t; 0 otherwise
    ycge            1 if cycle stops electric power generation at time t; 0 otherwise
    ycsu            1 if cycle is starting up at time t; 0 otherwise
    ycsup           1 if cycle startup penalty is enforced at time t; 0 otherwise
    yeh             1 if electrical heaters are operating at time t; 0 otherwise 
    yhsu            1 if electrical heaters are starting up at time t; 0 otherwise
    -------------------------------------------------------------
    */
    lprec *lp;
    int ret = 0;

    try{
        // number of timesteps in dispatch horzion
        int nt = (int)m_nstep_opt;
        //set up the variable structure  //TODO: We could make dispatch builder class
        optimization_vars O;
        O.add_var("s", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0.);
        O.add_var("wdot", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0.);
        O.add_var("delta_w", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0.);
        O.add_var("qdot", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0.);
        O.add_var("qeh", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0.);
        O.add_var("ucsu", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0.);
        O.add_var("uhsu", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0.);

        O.add_var("y", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("ycgb", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("ycge", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("ycsu", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("ycsup", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("yeh", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("yhsu", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        
        unordered_map<std::string, double> P;
        calculate_parameters(this, P, nt);
        // TODO: Why use this P unordered_map? Is this due to calculation requirements for some parameters?

        O.construct();  //allocates memory for data array
        int nvar = O.get_total_var_count(); //total number of variables in the problem
        lp = make_lp(0, nvar);  //build the context

        if(lp == NULL)
            throw C_csp_exception("Failed to create a new dispatch optimization problem context.");

        //set variable names and types for each column //TODO: moved to a function
        for(int i=0; i<O.get_num_varobjs(); i++)
        {
            optimization_vars::opt_var *v = O.get_var(i);

            string name_base = v->name;

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
            int *col = new int[6 * nt];
            REAL *row = new REAL[6 * nt];
            double tadj = P["disp_time_weighting"];
            int i = 0;

            //calculate the mean price to appropriately weight the receiver production timing derate
            double pmean =0;
            for(int t=0; t<(int)params.sell_price.size(); t++)
                pmean += params.sell_price.at(t);
            pmean /= (double)params.sell_price.size();
            //--
            
            for(int t=0; t<nt; t++)
            {
                i = 0;
                col[ t + nt*(i  ) ] = O.column("wdot", t);
                row[ t + nt*(i++) ] = P["delta"] * tadj * params.sell_price.at(t)*(1.-outputs.w_condf_expected.at(t));

                col[ t + nt*(i  ) ] = O.column("qeh", t);
                row[ t + nt*(i++) ] = - (P["delta"] * (1 / tadj) * params.buy_price.at(t) * ( 1 / P["eta_eh"]));

                col[ t + nt*(i  ) ] = O.column("yhsu", t);
                row[ t + nt*(i++) ] = - (P["delta"] * (1 / tadj) * params.buy_price.at(t) * (1 / P["eta_eh"]) * P["Qhsu"]);

                col[ t + nt*(i  ) ] = O.column("ycsup", t);
                row[ t + nt*(i++) ] = - (1/tadj) * P["csu_cost"];

                col[ t + nt*(i  ) ] = O.column("delta_w", t);
                row[ t + nt*(i++) ] = - (1/tadj) * P["pen_delta_w"];

                ////col[ t + nt*(i  ) ] = O.column("x", t);
                ////row[ t + nt*(i++) ] = -P["delta"] * params.sell_price.at(t)* (1/tadj) * params.w_cycle_pump;

                tadj *= P["disp_time_weighting"];
            }


            //col[i * nt] = O.column("s", nt - 1);       //terminal inventory
            //row[i * nt] = P["delta"] * tadj * pmean * P["eta_cycle"] * params.disp_inventory_incentive;  // new terminal inventory 

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
        // TODO: Move to a function
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

        // ******************** Electric heater constraints *******************
        {
            REAL row[5];
            int col[5];

            for (int t = 0; t < nt; t++)
            {
                //Electric heater startup inventory
                int i = 0;

                row[i] = 1.;
                col[i++] = O.column("uhsu", t);

                row[i] = -P["delta"] * P["Qhsu"];
                col[i++] = O.column("yhsu", t);

                if (t > 0)
                {
                    row[i] = -1.;
                    col[i++] = O.column("uhsu", t - 1);

                    add_constraintex(lp, i, row, col, LE, 0);
                }
                else
                {
                    add_constraintex(lp, i, row, col, LE, P["uhsu0"]);
                }

                //inventory nonzero
                i = 0;
                row[i  ] = 1.;
                col[i++] = O.column("uhsu", t);

                row[i  ] = -P["Eeh"]*1.00001;
                col[i++] = O.column("yhsu", t);

                add_constraintex(lp, i, row, col, LE, 0.);

                //Heaters operation allowed when:
                i = 0;
                row[i  ] = P["Eeh"];
                col[i++] = O.column("yeh", t);

                row[i  ] = -1.0;
                col[i++] = O.column("uhsu", t);

                if (t > 0)
                {
                    row[i  ] = - P["Eeh"];
                    col[i++] = O.column("yeh", t - 1);

                    add_constraintex(lp, i, row, col, LE, 0.);
                }
                else
                {
                    add_constraintex(lp, i, row, col, LE, P["Eeh"] * P["yeh0"]);
                }

                //Heater power limit
                i = 0;
                row[i  ] = 1.;
                col[i++] = O.column("qeh", t);

                row[i  ] = P["Qhsu"];
                col[i++] = O.column("yhsu", t);

                row[i  ] = -P["Qehu"];
                col[i++] = O.column("yeh", t);
                
                add_constraintex(lp, i, row, col, LE, 0.);

                //Heater minimum operation requirement
                i = 0;
                row[i  ] = 1.;
                col[i++] = O.column("qeh", t);

                row[i  ] = -P["Qehl"];
                col[i++] = O.column("yeh", t);

                add_constraintex(lp, i, row, col, GE, 0.);

                //Heaters startup can't be enabled after a time step where the heaters was operating
                i = 0;
                row[i  ] = 1.;
                col[i++] = O.column("yhsu", t);

                if(t>0)
                {
                    row[i  ] = 1.;
                    col[i++] = O.column("yeh", t-1);

                    add_constraintex(lp, i, row, col, LE, 1.);
                }
                else
                {
                    add_constraintex(lp, i, row, col, LE, 1. - P["yeh0"]);
                }

                //Heaters and cycle cannot coincide
                i = 0;
                row[i  ] = 1.;
                col[i++] = O.column("yeh", t);

                row[i  ] = 1.;
                col[i++] = O.column("y", t);

                add_constraintex(lp, i, row, col, LE, 1.);
            }
        }
        
        // ******************** Power cycle constraints *******************

        //cycle production change
        //{
        //    REAL row[3];
        //    int col[3];
        //    
        //    for(int t=0; t<nt; t++)
        //    {
        //        // Ramp up
        //        int i = 0;
        //        row[i  ] = 1.;
        //        col[i++] = O.column("delta_w", t);

        //        row[i  ] = -1.;
        //        col[i++] = O.column("wdot", t);

        //        if(t>0)
        //        {
        //            row[i  ] = 1.;
        //            col[i++] = O.column("wdot", t-1);
        //            
        //            add_constraintex(lp, i, row, col, GE, 0.);
        //        }
        //        else
        //        {
        //            add_constraintex(lp, i, row, col, GE, -P["Wdot0"]);
        //        }

        //        // Ramp down
        //        i = 0;
        //        row[i  ] = 1.;
        //        col[i++] = O.column("delta_w", t);

        //        row[i  ] = 1.;
        //        col[i++] = O.column("wdot", t);

        //        if (t > 0)
        //        {
        //            row[i  ] = -1.;
        //            col[i++] = O.column("wdot", t - 1);

        //            add_constraintex(lp, i, row, col, GE, 0.);
        //        }
        //        else
        //        {
        //            add_constraintex(lp, i, row, col, GE, P["Wdot0"]);
        //        }

        //        // Cycle ramping limit (sub-hourly model Delta < 1)
        //     //   if (P["delta"] < 1.)
        //     //   {
        //     //       double temp_coef = (outputs.eta_pb_expected.at(t) / params.eta_cycle_ref) * P["Wdotl"] - P["Wdlim"];

        //     //       i = 0;
        //     //       row[i  ] = 1.;
        //     //       col[i++] = O.column("delta_w", t);

        //     //       row[i  ] = -temp_coef * 2.;
        //     //       col[i++] = O.column("ycsb", t);
        //     //
        //     ///* #ifdef MOD_CYCLE_SHUTDOWN
        //     //       row[i  ] = -temp_coef * 2.;
        //     //       col[i++] = O.column("ycsd", t);
        //     //   #endif   */

        //     //       row[i  ] = -temp_coef * 2.;
        //     //       col[i++] = O.column("yoff", t);

        //     //       row[i  ] = -temp_coef;
        //     //       col[i++] = O.column("y", t);

        //     //       if (t>0)
        //     //       {
        //     //           row[i  ] = temp_coef;
        //     //           col[i++] = O.column("y", t-1);

        //     //           add_constraintex(lp, i, row, col, LE, P["Wdlim"]);
        //     //       }
        //     //       else
        //     //       {
        //     //           add_constraintex(lp, i, row, col, LE, P["Wdlim"] - temp_coef * P["y0"]);
        //     //       }
        //     //   }
        //    }
        //}

        {
            //Linearization of the implementation of the piecewise efficiency equation 
            REAL row[3];
            int col[3];

            for (int t = 0; t < nt; t++)
            {
                //power production curve
                int i = 0;
                row[i] = 1.;
                col[i++] = O.column("wdot", t);

                row[i] = -P["etap"] * outputs.eta_pb_expected.at(t) / params.eta_cycle_ref;
                col[i++] = O.column("qdot", t);

                row[i] = -(P["Wdotu"] - P["etap"] * P["Qu"]) * outputs.eta_pb_expected.at(t) / params.eta_cycle_ref;
                col[i++] = O.column("y", t);

                //add_constraintex(lp, i, row, col, EQ, 0.);
                add_constraintex(lp, i, row, col, LE, 0.);
            }
        }

        {
            REAL row[5];
            int col[5];


            for(int t=0; t<nt; t++)
            {

                int i=0;
                ////Startup Inventory balance
                //row[i  ] = 1.;
                //col[i++] = O.column("ucsu", t);
                //
                //row[i  ] = - P["delta"] * P["Qcsu"];
                //col[i++] = O.column("ycsu", t);

                //if(t>0)
                //{
                //    row[i  ] = -1.;
                //    col[i++] = O.column("ucsu", t-1);

                //    add_constraintex(lp, i, row, col, LE, 0.);
                //}
                //else
                //{
                //    add_constraintex(lp, i, row, col, LE, P["ucsu0"]);
                //}

                ////Inventory nonzero
                //i = 0;
                //row[i  ] = 1.;
                //col[i++] = O.column("ucsu", t);

                //row[i  ] = -P["Ec"]*1.00001; //tighter formulation
                //col[i++] = O.column("ycsu", t);

                //add_constraintex(lp, i, row, col, LE, 0.);

                ////Cycle operation allowed when:
                //i = 0;
                //row[i  ] = P["Ec"];
                //col[i++] = O.column("y", t);

                //row[i  ] = -1.0;
                //if (P["delta"] >= 1.)
                //{
                //    col[i++] = O.column("ucsu", t);                 // for hourly model (delta = 1)
                //}
                //else
                //{
                //    col[i++] = O.column("ucsu", t - 1);             // for sub-hourly model (delta < 1)
                //}

                //if (t > 0)
                //{
                //    row[i  ] = -P["Ec"];
                //    col[i++] = O.column("y", t - 1);

                //    add_constraintex(lp, i, row, col, LE, 0.);
                //}
                //else
                //{
                //    add_constraintex(lp, i, row, col, LE, P["Ec"] * P["y0"]);
                //}

                //Cycle consumption limit (valid only for hourly model -> Delta == 1)
                if (P["delta"] >= 1.)
                {
                    i = 0;
                    row[i  ] = 1.;
                    col[i++] = O.column("qdot", t);

                    //row[i  ] = P["Qcsu"];
                    //col[i++] = O.column("ycsu", t);

                    row[i  ] = -P["Qu"];
                    col[i++] = O.column("y", t);

                    add_constraintex(lp, i, row, col, LE, 0.);
                }

                //cycle operation mode requirement //TODO: This constraint seems redundant
                //i = 0;
                //row[i  ] = 1.;
                //col[i++] = O.column("qdot", t);

                //row[i  ] = -P["Qu"];
                //col[i++] = O.column("y", t);

                //add_constraintex(lp, i, row, col, LE, 0.);

                //Minimum cycle energy contribution
                i=0;
                row[i  ] = 1.;
                col[i++] = O.column("qdot", t);

                row[i  ] = -P["Ql"];
                col[i++] = O.column("y", t);

                add_constraintex(lp, i, row, col, GE, 0);

                //cycle startup and operation cannot coincide (valid for sub-hourly model Delta < 1)
                //if (P["delta"] < 1)
                //{
                //    i = 0;
                //    row[i  ] = 1.;
                //    col[i++] = O.column("ycsu", t);

                //    row[i  ] = 1.;
                //    col[i++] = O.column("y", t);

                //    add_constraintex(lp, i, row, col, LE, 1.);
                //}

                //cycle startup can't be enabled after a time step where the cycle was operating
 /*               i = 0;
                row[i  ] = 1.;
                col[i++] = O.column("ycsu", t);

                if (t > 0)
                {
                    row[i  ] = 1.;
                    col[i++] = O.column("y", t-1);

                    add_constraintex(lp, i, row, col, LE, 1.);
                }
                else
                {
                    add_constraintex(lp, i, row, col, LE, 1 - P["y0"]);
                }*/

                //cycle start penalty
                //i = 0;
                //row[i  ] = 1.;
                //col[i++] = O.column("ycsup", t);

                //row[i  ] = -1.;
                //col[i++] = O.column("ycsu", t);

                //if (t > 0)
                //{
                //    row[i  ] = 1.;
                //    col[i++] = O.column("ycsu", t-1);

                //    add_constraintex(lp, i, row, col, GE, 0.);
                //}
                //else
                //{
                //    add_constraintex(lp, i, row, col, GE, -P["ycsu0"]);
                //}
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
                col[i++] = O.column("qeh", t);

                row[i  ] = -P["delta"];
                col[i++] = O.column("qdot", t);
                
                //row[i  ] = -P["delta"]*P["Qcsu"];
                //col[i++] = O.column("ycsu", t);
                //
                //row[i  ] = -P["delta"]*P["Qhsu"]; 
                //col[i++] = O.column("yhsu", t);
                
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
            REAL row[2];
            int col[2];

            for(int t=0; t<nt; t++)
            {
                int i = 0;

                row[i  ] = 1.;
                col[i++] = O.column("s", t);

                add_constraintex(lp, i, row, col, LE, P["Eu"]);
            }
        }

    //    // Maximum gross electricity production constraint //TODO: Is this needed?
    //    {
    //        REAL row[1];
    //        int col[1];

    //        for( int t = 0; t<nt; t++ )
    //        {
    //            int i = 0;
    //            row[i  ] = 1.;
    //            col[i++] = O.column("wdot", t);

				//add_constraintex(lp, i, row, col, LE, outputs.f_pb_op_limit.at(t) * P["W_dot_cycle"]);
    //        }
    //    }

        // Minimum up- and down-times
        //{
        //    REAL row[4];
        //    int col[4];

        //    // binary logic when switching power cycle state
        //    for (int t = 0; t < nt; t++)
        //    {
        //        int i = 0;
        //        row[i  ] = 1.;
        //        col[i++] = O.column("ycgb", t);

        //        row[i  ] = -1.;
        //        col[i++] = O.column("ycge", t);

        //        row[i  ] = -1.;
        //        col[i++] = O.column("y", t);

        //        if (t > 0)
        //        {
        //            row[i  ] = 1.;
        //            col[i++] = O.column("y", t - 1);

        //            add_constraintex(lp, i, row, col, EQ, 0.);
        //        }
        //        else
        //        {
        //            add_constraintex(lp, i, row, col, EQ, -P["y0"]);
        //        }
        //    }

        //    // minimum up-time constraint
        //    for (int t = 0; t < nt; t++)
        //    {
        //        if (params.time_elapsed.at(t) > (P["Yu"] - P["Yu0"])* P["y0"])
        //        {
        //            REAL* row = new REAL[nt+2];
        //            int* col = new int[nt+2]; 

        //            int i = 0;
        //            for (int tp = 0; tp < nt; tp++)
        //            {
        //                double delta_time = params.time_elapsed.at(t) - params.time_elapsed.at(tp);
        //                if ((delta_time >= 0) && (delta_time < P["Yu"]))
        //                {
        //                    row[i  ] = 1.;
        //                    col[i++] = O.column("ycgb", tp);
        //                }
        //            }
        //            row[i  ] = -1.;
        //            col[i++] = O.column("y", t);

        //            add_constraintex(lp, i, row, col, LE, 0.);

        //            delete[] row;
        //            delete[] col;
        //        }
        //    }

        //    // minimum down-time constraint
        //    for (int t = 0; t < nt; t++)
        //    {
        //        if (params.time_elapsed.at(t) > (P["Yd"] - P["Yd0"]) * (1 - P["y0"]))
        //        {
        //            REAL* row = new REAL[nt + 2];
        //            int* col = new int[nt + 2];

        //            int i = 0;
        //            for (int tp = 0; tp < nt; tp++)
        //            {
        //                double delta_time = params.time_elapsed.at(t) - params.time_elapsed.at(tp);
        //                if ((delta_time >= 0) && (delta_time < P["Yd"]))
        //                {
        //                    row[i  ] = 1.;
        //                    col[i++] = O.column("ycge", tp);
        //                }
        //            }
        //            row[i  ] = 1.;
        //            col[i++] = O.column("y", t);

        //            add_constraintex(lp, i, row, col, LE, 1.);

        //            delete[] row;
        //            delete[] col;
        //        }
        //    }

        //    // cycle minimum up time initial enforcement
        //    for (int t = 0; t < nt; t++)
        //    {
        //        if (params.time_elapsed.at(t) <= max((P["Yu"] - P["Yu0"]) * P["y0"], (P["Yd"] - P["Yd0"]) * (1 - P["y0"])))
        //        {
        //            int i = 0;
        //            row[i  ] = 1.;
        //            col[i++] = O.column("y", t);

        //            add_constraintex(lp, i, row, col, EQ, P["y0"]);
        //        }
        //        else
        //        {
        //            break;
        //        }
        //    }
        //}
        
        //Set problem to maximize
        set_maxim(lp);

        //reset the row mode
        set_add_rowmode(lp, FALSE);

        //set the log function
        solver_params.reset();

        put_msgfunc(lp, etes_opt_iter_function, (void*)(&solver_params), MSG_ITERATION | MSG_MILPBETTER | MSG_MILPFEASIBLE);
        put_abortfunc(lp, etes_opt_abortfunction, (void*)(&solver_params));
        if( solver_params.disp_reporting > 0 )
        {
            put_logfunc(lp, etes_opt_logfunction, (void*)(&solver_params));
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
        
        //Different Basis Factorization Package:
            /* Using these packages did not seem to help reduce solution times. */
        //set_BFP(lp, "bfp_etaPFI");    // original lp_solve product form of the inverse.
        //set_BFP(lp, "bfp_LUSOL");     // LU decomposition. (LP solve manual suggests using this one) Seems to increase solve times
        //set_BFP(lp, "bfp_GLPK");      // GLPK LU decomposition.

        //branch and bound rule. This one has a big impact on solver performance.
        if(solver_params.bb_type > 0)
            set_bb_rule(lp, solver_params.bb_type);
		else
		{
			set_bb_rule(lp, NODE_RCOSTFIXING + NODE_DYNAMICMODE + NODE_GREEDYMODE + NODE_PSEUDONONINTSELECT);
            if (P["wlim_min"] < 1.e20)  // TODO: why?
				set_bb_rule(lp, NODE_PSEUDOCOSTSELECT + NODE_DYNAMICMODE);
		}
        
 
       //Problem scaling loop
        int scaling_iter = 0;
        bool return_ok = false;
        while(scaling_iter < 5)
        {

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
            }


            ret = solve(lp);

            //Collect the dispatch profile and startup flags
            return_ok = ret == OPTIMAL || ret == SUBOPTIMAL;
            
            if(return_ok)
                break;      //break the scaling loop

            //If the problem was reported as unbounded, this probably has to do with poor scaling. Try again with no scaling.
            string fail_type;
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

        //keep track of problem efficiency
        outputs.presolve_nconstr = get_Nrows(lp);
        outputs.presolve_nvar = get_Ncolumns(lp);
        outputs.solve_time = time_elapsed(lp);

        // Saving problem and solution for debugging
        set_outputfile(lp, "C:\\Users\\WHamilt2\\Documents\\SAM\\ZZZ_working_directory\\setup.txt");
        print_lp(lp);
        set_outputfile(lp, "C:\\Users\\WHamilt2\\Documents\\SAM\\ZZZ_working_directory\\solution.txt");
        print_solution(lp, 1);

        if(return_ok)
        {
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

            int ncols = get_Ncolumns(lp);

            REAL *vars = new REAL[ncols];
            get_variables(lp, vars);

            for(int c=1; c<ncols; c++)
            {
                char *colname = get_col_name(lp, c);
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

                //if(strcmp(root, "ycsb") == 0)  //Cycle standby
                //{
                //    outputs.pb_standby.at(t) = vars[ c-1 ] == 1.;
                //}
                if(strcmp(root, "ycsu") == 0)     //Cycle start up
                {
                    bool su = (fabs(1 - vars[ c-1 ]) < 0.001);
                    outputs.pb_operation.at(t) = outputs.pb_operation.at(t) || su;
                    outputs.q_pb_startup.at(t) = su ? P["Qcsu"] : 0.;
                }
                else if(strcmp(root, "y") == 0)     //Cycle operation
                {
                    outputs.pb_operation.at(t) = outputs.pb_operation.at(t) || ( fabs(1. - vars[ c-1 ]) < 0.001 );
                }
                else if(strcmp(root, "qdot") == 0)     //Cycle thermal energy consumption
                {
                    outputs.q_pb_target.at(t) = vars[ c-1 ];
                }
                else if(strcmp(root, "yhsu") == 0)     //Receiver start up
                {
                    bool su = (fabs(1 - vars[c - 1]) < 0.001);
                    outputs.rec_operation.at(t) = outputs.rec_operation.at(t) || su;
                    outputs.q_rec_startup.at(t) = su ? P["Qhsu"] : 0.;
                }
                //else if(strcmp(root, "xrsu") == 0)
                //{
                //    outputs.q_rec_startup.at(t) = vars[ c-1 ];
                //}
                else if(strcmp(root, "yeh") == 0)
                {
                    outputs.rec_operation.at(t) = outputs.rec_operation.at(t) || (fabs(1 - vars[ c-1 ]) < 0.001);
                }
                else if(strcmp(root, "s") == 0)         //Thermal storage charge state
                {
                    outputs.tes_charge_expected.at(t) = vars[ c-1 ];
                }
                else if(strcmp(root, "qeh") == 0)   //receiver production
                {
                    outputs.q_sf_expected.at(t) = vars[ c-1 ];
                }
                else if(strcmp(root, "wdot") == 0) //electricity production
                {
                    outputs.w_pb_target.at(t) = vars[ c-1 ];
                }
            }

            delete [] vars;
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

        stringstream s;
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
    catch(exception &e)
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

// ========================================
//       Exporting the problem to AMPL
// ========================================

//TODO: Fix this
bool etes_strcompare(std::string a, std::string b)
{
    return util::lower_case(a) < util::lower_case(b);
};

std::string etes_dispatch_opt::write_ampl()
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

        stringstream outname;
        //outname << solver_params.ampl_data_dir << "data_" << day << ".dat";        
        outname << solver_params.ampl_data_dir << "sdk_data.dat";
        
        sname = outname.str();    //save string

        ofstream fout(outname.str().c_str());

        int nt = m_nstep_opt;

        unordered_map<std::string, double> pars;
        calculate_parameters(this, pars, nt);


        //double dq_rsu = params.e_rec_startup / params.dt_rec_startup;
        //double dq_csu = params.e_pb_startup_cold / ceil(params.dt_pb_startup_cold/params.dt) / params.dt;

        fout << "#data file\n\n";
        fout << "# --- scalar parameters ----\n";
        fout << "param day_of_year := " << day << ";\n";

        std::vector<std::string> keys;

        for( unordered_map<std::string, double>::iterator parval = pars.begin(); parval != pars.end(); parval++)
            keys.push_back( parval->first );
        
        std::sort( keys.begin(), keys.end(), etes_strcompare );

        for(size_t k=0; k<keys.size(); k++)
            fout << "param " << keys.at(k) << " := " << pars[keys.at(k)] << ";\n";

        fout << "# --- indexed parameters ---\n";
        fout << "param Qin := \n";
        for(int t=0; t<nt; t++)
            fout << t+1 << "\t" << outputs.q_sfavail_expected.at(t) << "\n";
        fout << ";\n\n";

        fout << "param P := \n";
        for(int t=0; t<nt; t++)
            fout << t+1 << "\t" << params.sell_price.at(t) << "\n";
        fout << ";\n\n";

        fout << "param etaamb := \n";   //power block ambient adjustment
        for(int t=0; t<nt; t++)
            fout << t+1 << "\t" << outputs.eta_pb_expected.at(t) << "\n";
        fout << ";\n\n";

        ////net power limit
        //fout << "param Wdotnet := \n";
        //for(int t=0; t<nt; t++)
        //    fout << t+1 << "\t" << w_lim.at(t) << "\n";
        //fout << ";\n\n";
        
        //condenser parasitic loss coefficient
        fout << "param etac := \n";
        for(int t=0; t<nt; t++)
            fout << t+1 << "\t" << outputs.w_condf_expected.at(t) << "\n";
        fout << ";\n\n";

        //cycle net production lower limit
        fout << "param wnet_lim_min := \n";
        for(int t=0; t<nt; t++)
            fout << t+1 << "\t" << outputs.wnet_lim_min.at(t) << "\n";
        fout << ";\n\n";

        //cycle net production lower limit
        fout << "param delta_rs := \n";
        for(int t=0; t<nt; t++)
            fout << t+1 << "\t" << outputs.delta_rs.at(t) << "\n";
        fout << ";\n\n";

        fout.close();
    }

    return sname;
}

bool etes_dispatch_opt::optimize_ampl()
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

    
    stringstream tstring;

    std::string datfile = write_ampl();
    if( datfile.empty() )
        throw C_csp_exception("An error occured when writing the AMPL input file.");
    
    ////call ampl
    //tstring << "ampl \"" << solver_params.ampl_data_dir << "sdk_dispatch.run\"";

    if( system(NULL) ) puts ("Ok");
    else exit(EXIT_FAILURE);

    //int sysret = system(tstring.str().c_str());
    int sysret = system( solver_params.ampl_exec_call.c_str() );
    


    //read back ampl solution
    tstring.str(std::string()); //clear

    tstring << solver_params.ampl_data_dir << "sdk_solution.txt";
    ifstream infile(tstring.str().c_str());

    if(! infile.is_open() )
        return false;
    
    std::vector< std::string > F;

    char line[1000];
    while( true )
    {
        infile.getline( line, 1000 );

        if( infile.eof() )
            break;

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

//
//optimization_vars::optimization_vars()
//{
//    current_mem_pos = 0;
//    alloc_mem_size = 0;
//}
//
//void optimization_vars::add_var(const string &vname, int var_type /* VAR_TYPE enum */, int var_dim /* VAR_DIM enum */, int var_dim_size, REAL lobo, REAL upbo)
//{
//    if(var_dim == VAR_DIM::DIM_T2)
//        add_var(vname, var_type, VAR_DIM::DIM_NT, var_dim_size, var_dim_size, lobo, upbo);
//    else
//        add_var(vname, var_type, var_dim, var_dim_size, 1, lobo, upbo);
//
//}
//
//void optimization_vars::add_var(const string &vname, int var_type /* VAR_TYPE enum */, int var_dim /* VAR_DIM enum */, int var_dim_size, int var_dim_size2, REAL lobo, REAL upbo)
//{
//    var_objects.push_back( optimization_vars::opt_var() );
//    optimization_vars::opt_var *v = &var_objects.back();
//    v->name = vname;
//    v->ind_start = current_mem_pos;
//    v->var_type = var_type;
//    v->var_dim = var_dim;
//    v->var_dim_size = var_dim_size;
//    v->var_dim_size2 = var_dim_size2;
//    if(v->var_type == optimization_vars::VAR_TYPE::BINARY_T)
//    {
//        v->upper_bound = 1.;
//        v->lower_bound = 0.;
//    }
//    else
//    {
//        v->upper_bound = upbo;
//        v->lower_bound = lobo;
//    }
//
//    //calculate the required memory space for this type of variable
//    int mem_size;
//    switch (var_dim)
//    {
//    case optimization_vars::VAR_DIM::DIM_T:
//        mem_size = var_dim_size;
//        break;
//    case optimization_vars::VAR_DIM::DIM_NT:
//        mem_size = var_dim_size * var_dim_size2;
//        break;
//    case optimization_vars::VAR_DIM::DIM_T2:
//        throw C_csp_exception("invalid var dimension in add_var");
//    case optimization_vars::VAR_DIM::DIM_2T_TRI:
//        mem_size = (var_dim_size+1) * var_dim_size/2;
//        break;
//    }
//
//    v->ind_end = v->ind_start + mem_size;
//    
//    current_mem_pos += mem_size;
//
//    
//}
//
//bool optimization_vars::construct()
//{
//    if( current_mem_pos < 0 || current_mem_pos > 1000000 )
//        throw C_csp_exception("Bad memory allocation when constructing variable table for dispatch optimization.");
//
//    data = new REAL[current_mem_pos];
//
//    alloc_mem_size = current_mem_pos;
//
//    for(int i=0; i<(int)var_objects.size(); i++)
//        var_by_name[ var_objects.at(i).name ] = &var_objects.at(i);
//
//	return true;
//}
//
//REAL &optimization_vars::operator()(char *varname, int ind)    //Access for 1D var
//{
//    return data[ var_by_name[varname]->ind_start + ind ];
//
//}
//
//REAL &optimization_vars::operator()(char *varname, int ind1, int ind2)     //Access for 2D var
//{
//    return data[ column(varname, ind1, ind1)-1 ];
//}
//
//REAL &optimization_vars::operator()(int varind, int ind)    //Access for 1D var
//{
//    return data[ var_objects.at(varind).ind_start + ind ];
//
//}
//
//REAL &optimization_vars::operator()(int varind, int ind1, int ind2)     //Access for 2D var
//{
//    return data[ column(varind, ind1, ind2)-1 ];
//}
//
//int optimization_vars::column(const string &varname, int ind)
//{
//    return var_by_name[varname]->ind_start + ind +1;
//}
//
//int optimization_vars::column(const string &varname, int ind1, int ind2)
//{
//    opt_var *v = var_by_name[ string(varname) ];
//    switch (v->var_dim)
//    {
//    case VAR_DIM::DIM_T:
//        throw C_csp_exception("Attempting to access optimization variable memory via 2D call when referenced variable is 1D.");
//    case VAR_DIM::DIM_NT:
//        return v->ind_start + v->var_dim_size2 * ind1 + ind2 +1;
//    default:
//    {
//        int ind = v->var_dim_size * ind1 + ind2 - ((ind1-1)*ind1/2);
//        return v->ind_start + ind +1;
//    }
//        break;
//    }
//}
//
//int optimization_vars::column(int varindex, int ind)
//{
//    return var_objects[varindex].ind_start + ind +1;
//}
//
//int optimization_vars::column(int varindex, int ind1, int ind2)
//{
//     opt_var *v = &var_objects[varindex];
//    switch (v->var_dim)
//    {
//    case VAR_DIM::DIM_T:
//        throw C_csp_exception("Attempting to access optimization variable memory via 2D call when referenced variable is 1D.");
//    case VAR_DIM::DIM_NT:        
//    	return v->ind_start + v->var_dim_size2 * ind1 + ind2 +1;  
//    default:   
//    	{       
//			int ind = v->var_dim_size * ind1 + ind2 - ((ind1-1)*ind1/2);
//			return v->ind_start + ind +1;
//    	} break;
//    }
//}
//
//int optimization_vars::get_num_varobjs()
//{
//    return (int)var_objects.size();
//}
//
//int optimization_vars::get_total_var_count()
//{
//    return alloc_mem_size;
//}
//
//REAL *optimization_vars::get_variable_array()
//{
//    return data;
//}
//
//optimization_vars::opt_var *optimization_vars::get_var(const string &varname)
//{
//    return var_by_name[ varname ];
//}
//
//optimization_vars::opt_var *optimization_vars::get_var(int varindex)
//{
//    return &var_objects[varindex];
//}
