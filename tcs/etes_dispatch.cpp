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
#include "etes_dispatch.h"
#include "lp_lib.h" 
#include "lib_util.h"

#define SOS_NONE

#undef min
#undef max

/*

Careful with namespaces in this file.. importing the LPsolve library introduces new macro definitions
and function definitions.

*/

etes_dispatch_opt::etes_dispatch_opt()
{
    outputs.clear();
    params.clear();
}

void etes_dispatch_opt::init(double cycle_q_dot_des, double cycle_eta_des, double cycle_w_dot_des)
{
    // TODO: I don't like having to pass these in, why can't I access them via the cycle pointer? ask Ty
    // TODO: we could pass in C_csp_power_cycle::S_solved_params instead which contains these 3 items?
    // TODO: Can we create getters for design point infromation?
    params.clear();

    params.dt = 1. / (double)solver_params.steps_per_hour;  //hr

    params.dt_pb_startup_cold = pointers.mpc_pc->get_cold_startup_time();
    params.e_pb_startup_cold = pointers.mpc_pc->get_cold_startup_energy() * 1000.;
    params.q_pb_max = pointers.mpc_pc->get_max_thermal_power() * 1000;
    params.q_pb_min = pointers.mpc_pc->get_min_thermal_power() * 1000;
    params.eta_cycle_ref = pointers.mpc_pc->get_efficiency_at_load(1.);

    params.dt_rec_startup = pointers.col_rec->get_startup_time(); // / 3600.;
    params.e_rec_startup = pointers.col_rec->get_startup_energy() * 1000;
    params.q_eh_min = 0.0; //pointers.col_rec->get_min_power_delivery() * 1000.;
    params.q_eh_max = pointers.col_rec->get_max_thermal_power() * 1000;

    params.e_tes0 = pointers.tes->get_initial_charge_energy() * 1000;
    params.e_tes_min = pointers.tes->get_min_charge_energy() * 1000;
    params.e_tes_max = pointers.tes->get_max_charge_energy() * 1000;
    //params.tes_degrade_rate = pointers.tes->get_degradation_rate();

    params.q_pb_des = cycle_q_dot_des * 1000.;
    params.eta_pb_des = cycle_eta_des;

    // TODO: This code below should be moved to a function...
    //Cycle efficiency
    params.eff_table_load.clear();
    //add zero point
    params.eff_table_load.add_point(0., 0.);    //this is required to allow the model to converge

    int neff = 2;   //mjw: if using something other than 2, the linear approximation assumption and associated code in csp_dispatch.cpp/calculate_parameters() needs to be reformulated.
    for (int i = 0; i < neff; i++)
    {
        double x = params.q_pb_min + (params.q_pb_max - params.q_pb_min) / (double)(neff - 1) * i;
        double xf = x * 1.e-3 / cycle_q_dot_des;  //MW

        double eta;
        eta = pointers.mpc_pc->get_efficiency_at_load(xf);

        params.eff_table_load.add_point(x, eta);
    }

    //cycle efficiency vs temperature
    params.eff_table_Tdb.clear();
    params.wcondcoef_table_Tdb.clear();
    int neffT = 40;

    for (int i = 0; i < neffT; i++)
    {
        double T = -10. + 60. / (double)(neffT - 1) * i;
        double wcond;
        double eta = pointers.mpc_pc->get_efficiency_at_TPH(T, 1., 30., &wcond) / cycle_eta_des;

        params.eff_table_Tdb.add_point(T, eta);
        params.wcondcoef_table_Tdb.add_point(T, wcond / cycle_w_dot_des); //fraction of rated gross gen
    }
}

bool etes_dispatch_opt::check_setup(int nstep)
{
    //check parameters and inputs to make sure everything has been set up correctly
    if ((int)params.sell_price.size() < nstep)   return false;
    // TODO: add other checks

    return base_dispatch_opt::check_setup();
}

bool etes_dispatch_opt::update_horizon_parameters(C_csp_tou& mc_tou)
{
    // TODO: update the horizon parameters correctly

    //get the new price signal
    params.sell_price.clear();
    params.sell_price.resize(solver_params.optimize_horizon * solver_params.steps_per_hour, 1.);
    params.buy_price.clear();
    params.buy_price.resize(solver_params.optimize_horizon * solver_params.steps_per_hour, 1.);

    for (int t = 0; t < solver_params.optimize_horizon * solver_params.steps_per_hour; t++)
    {
        C_csp_tou::S_csp_tou_outputs mc_tou_outputs;

        mc_tou.call(pointers.siminfo->ms_ts.m_time + t * 3600. / (double)solver_params.steps_per_hour, mc_tou_outputs);
        params.sell_price.at(t) = mc_tou_outputs.m_price_mult;
        params.buy_price.at(t) = params.sell_price.at(t);     //TODO: make these unique if specified by user
    }
    return true;
}

void etes_dispatch_opt::update_initial_conditions(double q_dot_to_pb, double T_htf_cold_des)
{
    //TODO: Update with etes initial conditions

    //note the states of the power cycle and receiver
    params.is_pb_operating0 = pointers.mpc_pc->get_operating_state() == 1;
    params.is_eh_operating0 = pointers.col_rec->get_operating_state() == C_csp_collector_receiver::ON;

    params.q_pb0 = q_dot_to_pb * 1000.;

    //Note the state of the thermal energy storage system
    double q_disch, m_dot_disch, T_tes_return;
    pointers.tes->discharge_avail_est(T_htf_cold_des, pointers.siminfo->ms_ts.m_step, q_disch, m_dot_disch, T_tes_return);
    params.e_tes0 = q_disch * 1000. * pointers.siminfo->ms_ts.m_step / 3600. + params.e_tes_min;        //kWh
    if (params.e_tes0 < params.e_tes_min)
        params.e_tes0 = params.e_tes_min;
    if (params.e_tes0 > params.e_tes_max)
        params.e_tes0 = params.e_tes_max;
}

bool etes_dispatch_opt::predict_performance(int step_start, int ntimeints, int divs_per_int)
{
    //TODO: clean up code that is not required for eTES

    //Step number - 1-based index for first hour of the year.

    //save step count
    m_nstep_opt = ntimeints;

    //Predict performance out nstep values.
    params.eta_pb_expected.clear();
    params.w_condf_expected.clear();

    if (!check_setup(m_nstep_opt))
        throw C_csp_exception("Dispatch optimization precheck failed.");

    //create the sim info
    C_csp_solver_sim_info simloc;
    simloc.ms_ts.m_step = pointers.siminfo->ms_ts.m_step;

    double Asf = pointers.col_rec->get_collector_area();

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
            if (!pointers.m_weather.read_time_step(step_start + i * divs_per_int + j, simloc))
                return false;

            //store the power cycle efficiency
            double cycle_eff = params.eff_table_Tdb.interpolate(pointers.m_weather.ms_outputs.m_tdry);
            cycle_eff *= params.eta_cycle_ref;
            cycle_eff_ave += cycle_eff * ave_weight;

            double f_pb_op_lim_local = std::numeric_limits<double>::quiet_NaN();
            double m_dot_htf_max_local = std::numeric_limits<double>::quiet_NaN();
            pointers.mpc_pc->get_max_power_output_operation_constraints(pointers.m_weather.ms_outputs.m_tdry, m_dot_htf_max_local, f_pb_op_lim_local);
            f_pb_op_lim_ave += f_pb_op_lim_local * ave_weight;	//[-]

            //store the condenser parasitic power fraction
            double wcond_f = params.wcondcoef_table_Tdb.interpolate(pointers.m_weather.ms_outputs.m_tdry);
            wcond_ave += wcond_f * ave_weight;

            simloc.ms_ts.m_time += simloc.ms_ts.m_step;
            pointers.m_weather.converged();
        }

        //-----report hourly averages
        //power cycle efficiency
        params.eta_pb_expected.push_back(cycle_eff_ave);
        //condenser power
        params.w_condf_expected.push_back(wcond_ave);
    }
    return true;
}

static void calculate_parameters(etes_dispatch_opt *optinst, unordered_map<std::string, double> &pars, int nt)
{
    /* 
    A central location for making sure the parameters from the model are accurately calculated for use in
    the dispatch optimization model.

    TODO: Why did I make this static?
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

        double delta_hsu = optinst->params.dt_rec_startup;
        while (delta_hsu > pars["delta"])
        {
            delta_hsu -= pars["delta"];
        }
        pars["delta_hsu"] = delta_hsu;

        double delta_csu = optinst->params.dt_pb_startup_cold;  // TODO: wrong but works for now
        while (delta_csu > pars["delta"])
        {
            delta_csu -= pars["delta"];
        }
        pars["delta_csu"] = delta_csu;

        //pars["Leh"] = optinst->params.w_eh_pump ;
        //pars["Lc"] = optinst->params.w_cycle_pump;

        pars["disp_time_weighting"] = optinst->params.time_weighting;
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
        //pars["qeh0"] = optinst->params.q_eh0;

        pars["ycsu0"] = (optinst->params.is_pb_starting0 ? 1 : 0);
        pars["ucsu0"] = optinst->params.e_pb_start0;
        pars["yhsu0"] = (optinst->params.is_eh_starting0 ? 1 : 0);
        pars["uhsu0"] = optinst->params.e_eh_start0;

        pars["Yd0"] = optinst->params.down_time0;
        pars["Yu0"] = optinst->params.up_time0;

        pars["s0"] = optinst->params.e_tes0 ;

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

        //maximum power based on linear fit
        pars["Wdotu"] = (pars["etap"] * pars["Qu"] + b);
        // minimum power based on linear fit
        pars["Wdotl"] = (pars["etap"] * pars["Ql"] + b);

        pars["Wdot0"] = 0.;
        if (pars["q0"] >= pars["Ql"])
            pars["Wdot0"] = (pars["etap"] * pars["q0"] + b) * optinst->params.eta_pb_expected.at(0) / optinst->params.eta_cycle_ref;

        //TODO: Fixed parameters
        pars["eta_eh"] = 1.0;   //0.95;
        //pars["Qhsu"] = pars["Qcsu"];    // 1000.0;
        //pars["Eeh"] = pars["Qcsu"];     // 1000.0;
        //pars["Qehu"] = pars["Qu"] * 2.4;
        pars["Qehl"] = pars["Qehu"]*0.25;
        pars["Yd"] = 2.;
        pars["Yu"] = 2.;

        pars["uhsu0"] = 0.;          // Assuming start up within an hour
        pars["ucsu0"] = 0.;           // Assuming start up within an hour
        pars["Yd0"] = pars["Yd"];    // Over riding these constraints
        pars["Yu0"] = pars["Yu"];    // Over riding these constraints

        pars["hsu_cost"] = 10.;
        //pars["qeh0"] = 0.;    // Not needed

        //pars["s0"] = 0.0;  pars["Eu"];  // For testing
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

    try{
        // number of timesteps in dispatch horzion
        int nt = (int)m_nstep_opt;

        unordered_map<std::string, double> P;
        calculate_parameters(this, P, nt);

        //set up the variable structure
        optimization_vars O;
        O.add_var("s", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0., P["Eu"]);
        O.add_var("wdot", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0., P["Wdotu"]*1.1);
        O.add_var("delta_w", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0., P["Wdotu"] * 1.1);
        O.add_var("qdot", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0., P["Qu"]);
        O.add_var("qeh", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0., P["Qehu"]);
        O.add_var("ucsu", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0., P["Ec"] * 1.0001);
        O.add_var("uhsu", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0., P["Eeh"] * 1.0001);
        O.add_var("zcsu", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0., P["Qu"]);
        O.add_var("zhsu", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0., P["Qehu"]);

        O.add_var("y", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("ycgb", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("ycge", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("ycsu", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("ycsup", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("yeh", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("yhsu", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);

        // Construct LP model and set up variable properties
        lp = construct_lp_model(&O);

        /* 
        --------------------------------------------------------------------------------
        set up the objective function first (per lpsolve guidance)
        --------------------------------------------------------------------------------
        */
		{
            int *col = new int[10 * nt];
            REAL *row = new REAL[10 * nt];
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
                row[ t + nt*(i++) ] = P["delta"] * tadj * params.sell_price.at(t)*(1.-params.w_condf_expected.at(t));

                col[ t + nt*(i  ) ] = O.column("qeh", t);
                row[ t + nt*(i++) ] = - (P["delta"] * (1 / tadj) * params.buy_price.at(t) * ( 1 / P["eta_eh"]));

                //col[ t + nt*(i  ) ] = O.column("zhsu", t);
                //row[ t + nt*(i++) ] = (P["delta_hsu"] * (1 / tadj) * params.buy_price.at(t) * (1 / P["eta_eh"]));
                //TODO: Convince yourself this is needed in the objective...

                col[ t + nt*(i  ) ] = O.column("yhsu", t);
                row[ t + nt*(i++) ] = - (P["delta"] * (1 / tadj) * params.buy_price.at(t) * (1 / P["eta_eh"]) * P["Qhsu"]);

                col[ t + nt*(i  ) ] = O.column("ycsup", t);
                row[ t + nt*(i++) ] = - (1/tadj) * P["csu_cost"];

                col[ t + nt*(i  ) ] = O.column("delta_w", t);
                row[ t + nt*(i++) ] = - (1/tadj) * P["pen_delta_w"];

                col[t + nt * (i)] = O.column("yhsu", t);
                row[t + nt * (i++)] = -(1 / tadj) * P["hsu_cost"];

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
        {
            REAL row[3];
            int col[3];

            //cycle production change
            for (int t = 0; t < nt; t++)
            {
                // Ramp up
                int i = 0;
                row[i] = 1.;
                col[i++] = O.column("delta_w", t);

                row[i] = -1.;
                col[i++] = O.column("wdot", t);

                if (t > 0)
                {
                    row[i] = 1.;
                    col[i++] = O.column("wdot", t - 1);

                    add_constraintex(lp, i, row, col, GE, 0.);
                }
                else
                {
                    add_constraintex(lp, i, row, col, GE, -P["Wdot0"]);
                }
            }

            for (int t = 0; t < nt; t++)
            {
                // Ramp down
                int i = 0;

                row[i] = 1.;
                col[i++] = O.column("delta_w", t);

                row[i] = 1.;
                col[i++] = O.column("wdot", t);

                if (t > 0)
                {
                    row[i] = -1.;
                    col[i++] = O.column("wdot", t - 1);

                    add_constraintex(lp, i, row, col, GE, 0.);
                }
                else
                {
                    add_constraintex(lp, i, row, col, GE, P["Wdot0"]);
                }
            }
        }

        //Linearization of the implementation of the piecewise efficiency equation 
        {
            REAL row[3];
            int col[3];

            for (int t = 0; t < nt; t++)
            {
                //power production curve
                int i = 0;
                row[i] = 1.;
                col[i++] = O.column("wdot", t);

                row[i] = -P["etap"] * params.eta_pb_expected.at(t) / params.eta_cycle_ref;
                col[i++] = O.column("qdot", t);

                row[i] = -(P["Wdotu"] - P["etap"] * P["Qu"]) * params.eta_pb_expected.at(t) / params.eta_cycle_ref;
                col[i++] = O.column("y", t);

                //add_constraintex(lp, i, row, col, EQ, 0.);
                add_constraintex(lp, i, row, col, LE, 0.);
            }
        }

        // Power cycle startup and operations
        {
            REAL row[5];
            int col[5];


            for(int t=0; t<nt; t++)
            {

                int i=0;
                //Startup Inventory balance
                row[i  ] = 1.;
                col[i++] = O.column("ucsu", t);
                
                row[i  ] = - P["delta"] * P["Qcsu"];
                col[i++] = O.column("ycsu", t);

                if(t>0)
                {
                    row[i  ] = -1.;
                    col[i++] = O.column("ucsu", t-1);

                    add_constraintex(lp, i, row, col, LE, 0.);
                }
                else
                {
                    add_constraintex(lp, i, row, col, LE, P["ucsu0"]);
                }

                //Inventory nonzero
                i = 0;
                row[i  ] = 1.;
                col[i++] = O.column("ucsu", t);

                row[i  ] = -P["Ec"]*1.00001; //tighter formulation
                col[i++] = O.column("ycsu", t);

                add_constraintex(lp, i, row, col, LE, 0.);

                //Cycle operation allowed when:
                i = 0;
                row[i  ] = P["Ec"];
                col[i++] = O.column("y", t);

                row[i  ] = -1.0;
                if (P["delta"] >= 1.)
                {
                    col[i++] = O.column("ucsu", t);                 // for hourly model (delta = 1)
                }
                else
                {
                    col[i++] = O.column("ucsu", t - 1);             // for sub-hourly model (delta < 1)
                }

                if (t > 0)
                {
                    row[i  ] = -P["Ec"];
                    col[i++] = O.column("y", t - 1);

                    add_constraintex(lp, i, row, col, LE, 0.);
                }
                else
                {
                    add_constraintex(lp, i, row, col, LE, P["Ec"] * P["y0"]);
                }

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
                i = 0;
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
                }

                //cycle start penalty
                i = 0;
                row[i  ] = 1.;
                col[i++] = O.column("ycsup", t);

                row[i  ] = -1.;
                col[i++] = O.column("ycsu", t);

                if (t > 0)
                {
                    row[i  ] = 1.;
                    col[i++] = O.column("ycsu", t-1);

                    add_constraintex(lp, i, row, col, GE, 0.);
                }
                else
                {
                    add_constraintex(lp, i, row, col, GE, -P["ycsu0"]);
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
                col[i++] = O.column("qeh", t);

                row[i] = -P["delta_hsu"];
                col[i++] = O.column("zhsu", t);

                row[i  ] = -P["delta"];
                col[i++] = O.column("qdot", t);

                row[i] = P["delta_csu"];
                col[i++] = O.column("zcsu", t);
                
                row[i  ] = -P["delta"]*P["Qcsu"];
                col[i++] = O.column("ycsu", t);
                
                row[i  ] = -1.;
                col[i++] = O.column("s", t);
                
                if(t>0)
                {
                    row[i  ] = 1.;
                    col[i++] = O.column("s", t-1);

                    // Heat loss term would need to be on the RHS +
                    add_constraintex(lp, i, row, col, EQ, 0.);
                }
                else
                {
                    add_constraintex(lp, i, row, col, EQ, -P["s0"]);  //initial storage state (kWh)
                }
            }
        }
        // Auxiliary variable for heaters start-up
        {
            REAL row[3];
            int col[3];

            for (int t = 0; t < nt; t++)
            {
                int i = 0;

                row[i] = 1.;
                col[i++] = O.column("zhsu", t);

                row[i] = -P["Qehu"];
                col[i++] = O.column("yhsu", t);

                add_constraintex(lp, i, row, col, LE, 0);
            }

            for (int t = 0; t < nt; t++)
            {
                int i = 0;

                row[i] = 1.;
                col[i++] = O.column("zhsu", t);

                row[i] = -1;
                col[i++] = O.column("qeh", t);

                add_constraintex(lp, i, row, col, LE, 0);
            }

            for (int t = 0; t < nt; t++)
            {
                int i = 0;

                row[i] = 1.;
                col[i++] = O.column("zhsu", t);

                row[i] = -1;
                col[i++] = O.column("qeh", t);

                row[i] = -P["Qehu"];
                col[i++] = O.column("yhsu", t);

                add_constraintex(lp, i, row, col, GE, -P["Qehu"]);
            }
        }
        // Auxiliary variable for cycle start-up
        {
            REAL row[3];
            int col[3];

            for (int t = 0; t < nt; t++)
            {
                int i = 0;

                row[i] = 1.;
                col[i++] = O.column("zcsu", t);

                row[i] = -P["Qu"];
                col[i++] = O.column("ycsu", t);

                add_constraintex(lp, i, row, col, LE, 0);
            }

            for (int t = 0; t < nt; t++)
            {
                int i = 0;

                row[i] = 1.;
                col[i++] = O.column("zcsu", t);

                row[i] = -1;
                col[i++] = O.column("qdot", t);

                add_constraintex(lp, i, row, col, LE, 0);
            }

            for (int t = 0; t < nt; t++)
            {
                int i = 0;

                row[i] = 1.;
                col[i++] = O.column("zcsu", t);

                row[i] = -1;
                col[i++] = O.column("qdot", t);

                row[i] = -P["Qu"];
                col[i++] = O.column("ycsu", t);

                add_constraintex(lp, i, row, col, GE, -P["Qu"]);
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

        // Minimum up- and down-times
        {
            REAL row[4];
            int col[4];

            // binary logic when switching power cycle state
            for (int t = 0; t < nt; t++)
            {
                int i = 0;
                row[i  ] = 1.;
                col[i++] = O.column("ycgb", t);

                row[i  ] = -1.;
                col[i++] = O.column("ycge", t);

                row[i  ] = -1.;
                col[i++] = O.column("y", t);

                if (t > 0)
                {
                    row[i  ] = 1.;
                    col[i++] = O.column("y", t - 1);

                    add_constraintex(lp, i, row, col, EQ, 0.);
                }
                else
                {
                    add_constraintex(lp, i, row, col, EQ, -P["y0"]);
                }
            }

            // minimum up-time constraint
            for (int t = 0; t < nt; t++)
            {
                if (params.time_elapsed.at(t) > (P["Yu"] - P["Yu0"])* P["y0"])
                {
                    REAL* row = new REAL[nt+2];
                    int* col = new int[nt+2]; 

                    int i = 0;
                    for (int tp = 0; tp < nt; tp++)
                    {
                        double delta_time = params.time_elapsed.at(t) - params.time_elapsed.at(tp);
                        if ((delta_time >= 0) && (delta_time < P["Yu"]))
                        {
                            row[i  ] = 1.;
                            col[i++] = O.column("ycgb", tp);
                        }
                    }
                    row[i  ] = -1.;
                    col[i++] = O.column("y", t);

                    add_constraintex(lp, i, row, col, LE, 0.);

                    delete[] row;
                    delete[] col;
                }
            }

            // minimum down-time constraint
            for (int t = 0; t < nt; t++)
            {
                if (params.time_elapsed.at(t) > (P["Yd"] - P["Yd0"]) * (1 - P["y0"]))
                {
                    REAL* row = new REAL[nt + 2];
                    int* col = new int[nt + 2];

                    int i = 0;
                    for (int tp = 0; tp < nt; tp++)
                    {
                        double delta_time = params.time_elapsed.at(t) - params.time_elapsed.at(tp);
                        if ((delta_time >= 0) && (delta_time < P["Yd"]))
                        {
                            row[i  ] = 1.;
                            col[i++] = O.column("ycge", tp);
                        }
                    }
                    row[i  ] = 1.;
                    col[i++] = O.column("y", t);

                    add_constraintex(lp, i, row, col, LE, 1.);

                    delete[] row;
                    delete[] col;
                }
            }

            // cycle minimum up time initial enforcement
            for (int t = 0; t < nt; t++)
            {
                if (params.time_elapsed.at(t) <= std::max((P["Yu"] - P["Yu0"]) * P["y0"], (P["Yd"] - P["Yd0"]) * (1 - P["y0"])))
                {
                    int i = 0;
                    row[i  ] = 1.;
                    col[i++] = O.column("y", t);

                    add_constraintex(lp, i, row, col, EQ, P["y0"]);
                }
                else
                {
                    break;
                }
            }
        }
        
        //Set problem to maximize
        set_maxim(lp);

        setup_solver_presolve_bbrules(lp);
        bool return_ok = problem_scaling_solve_loop(lp);
        set_lp_solve_outputs(lp);

        // Saving problem and solution for DEBUGGING formulation
        //save_problem_solution_debug(lp);

        //TODO: specific to etes...
        if(return_ok)
        {
            outputs.clear();
            outputs.resize(nt);

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

        delete_lp(lp);
        lp = NULL;
        print_dispatch_update();
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

// ========================================
//       Exporting the problem to AMPL
// ========================================

std::string etes_dispatch_opt::write_ampl()
{
    /* 
    Write the par file for ampl input

    return name of output file, if error, return empty string.
    */
    throw std::runtime_error((std::string)__func__ + " is not implemented.");
    return "";
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
    throw std::runtime_error((std::string)__func__ + " is not implemented.");
    return false;
}

bool etes_dispatch_opt::set_dispatch_outputs()
{
    //TODO: customize for etes
    if (lp_outputs.last_opt_successful && m_current_read_step < (int)outputs.q_pb_target.size())
    {
        //calculate the current read step, account for number of dispatch steps per hour and the simulation time step
        m_current_read_step = (int)(pointers.siminfo->ms_ts.m_time * solver_params.steps_per_hour / 3600. - .001)
            % (solver_params.optimize_frequency * solver_params.steps_per_hour);

        disp_outputs.is_rec_su_allowed = outputs.rec_operation.at(m_current_read_step);
        disp_outputs.is_pc_sb_allowed = outputs.pb_standby.at(m_current_read_step);
        disp_outputs.is_pc_su_allowed = outputs.pb_operation.at(m_current_read_step) || disp_outputs.is_pc_sb_allowed;

        disp_outputs.q_pc_target = outputs.q_pb_target.at(m_current_read_step) / 1000.;
        disp_outputs.q_dot_elec_to_CR_heat = outputs.q_sf_expected.at(m_current_read_step) / 1000.;

        if (disp_outputs.q_pc_target + 1.e-5 < params.q_pb_min/1.e3)
        {
            disp_outputs.is_pc_su_allowed = false;
            disp_outputs.q_pc_target = 0.0;
        }

        disp_outputs.q_dot_pc_max = params.q_pb_max;   // disp_outputs.q_pc_target;
        disp_outputs.etasf_expect = 0.0;
        disp_outputs.qsf_expect = 0.0;
        disp_outputs.qsfprod_expect = outputs.q_sf_expected.at(m_current_read_step) * 1.e-3;
        disp_outputs.qsfsu_expect = outputs.q_rec_startup.at(m_current_read_step) * 1.e-3;
        disp_outputs.tes_expect = outputs.tes_charge_expected.at(m_current_read_step) * 1.e-3;
        disp_outputs.qpbsu_expect = outputs.q_pb_startup.at(m_current_read_step) * 1.e-3;
        disp_outputs.wpb_expect = outputs.w_pb_target.at(m_current_read_step) * 1.e-3;
        disp_outputs.rev_expect = disp_outputs.wpb_expect * params.sell_price.at(m_current_read_step);
        disp_outputs.etapb_expect = disp_outputs.wpb_expect / std::max(1.e-6, outputs.q_pb_target.at(m_current_read_step)) * 1.e3
            * (outputs.pb_operation.at(m_current_read_step) ? 1. : 0.);

        if (m_current_read_step > solver_params.optimize_frequency* solver_params.steps_per_hour)
            throw C_csp_exception("Counter synchronization error in dispatch optimization routine.", "csp_dispatch");
    }
    disp_outputs.time_last = pointers.siminfo->ms_ts.m_time;

    return true;
}
