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
#include <string>
#include <iostream>
#include <sstream>
#include <stdlib.h>
#include <algorithm>
#include "csp_dispatch.h"
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
    string line = buf;
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


csp_dispatch_opt::csp_dispatch_opt()
{

    //initialize member data
    m_nstep_opt = 0;
    m_current_read_step = 0;
    m_last_opt_successful = false;
    price_signal.clear();
    clear_output_arrays();
    m_is_weather_setup = false;

    //parameters
    params.is_pb_operating0 = false;
    params.is_pb_standby0 = false;
    params.is_rec_operating0 = false;
    params.q_pb0 = numeric_limits<double>::quiet_NaN();
    params.dt = numeric_limits<double>::quiet_NaN();
    params.e_tes_init = numeric_limits<double>::quiet_NaN();          
    params.e_tes_min = numeric_limits<double>::quiet_NaN();           
    params.e_tes_max = numeric_limits<double>::quiet_NaN();           
    params.q_pb_standby = numeric_limits<double>::quiet_NaN();        
    params.e_pb_startup_cold = numeric_limits<double>::quiet_NaN();   
    params.e_pb_startup_hot = numeric_limits<double>::quiet_NaN();    
    params.e_rec_startup = numeric_limits<double>::quiet_NaN();       
    params.dt_pb_startup_cold = numeric_limits<double>::quiet_NaN();  
    params.dt_pb_startup_hot = numeric_limits<double>::quiet_NaN();   
    params.dt_rec_startup = numeric_limits<double>::quiet_NaN();      
    params.tes_degrade_rate = numeric_limits<double>::quiet_NaN();    
    params.q_pb_max = numeric_limits<double>::quiet_NaN();
    params.q_pb_min = numeric_limits<double>::quiet_NaN();
    params.q_rec_min = numeric_limits<double>::quiet_NaN();
    params.w_rec_pump = numeric_limits<double>::quiet_NaN();
    params.q_pb_des = numeric_limits<double>::quiet_NaN();
    params.siminfo = 0;   
    params.col_rec = 0;
	params.mpc_pc = 0;
    params.sf_effadj = 1.;
    params.info_time = 0.;
    params.eta_cycle_ref = numeric_limits<double>::quiet_NaN();
    params.disp_time_weighting = numeric_limits<double>::quiet_NaN();
    params.rsu_cost = params.csu_cost = params.pen_delta_w = params.q_rec_standby = numeric_limits<double>::quiet_NaN();
	//params.w_dc_field = numeric_limits<double>::quiet_NaN();'
	params.counter = 0;

	params.batt_slope_coeff = numeric_limits<double>::quiet_NaN();
	params.batt_int_coeff = numeric_limits<double>::quiet_NaN();
	params.slope_int_plus = numeric_limits<double>::quiet_NaN();
	params.slope_int_minus = numeric_limits<double>::quiet_NaN();
	params.slope_plus = numeric_limits<double>::quiet_NaN();
	params.slope_minus = numeric_limits<double>::quiet_NaN();
	params.batt_capacity = numeric_limits<double>::quiet_NaN();
	params.i_expected = numeric_limits<double>::quiet_NaN();
	params.batt_charge_lb = numeric_limits<double>::quiet_NaN();
    params.batt_charge_ub = numeric_limits<double>::quiet_NaN();
	params.batt_discharge_lb = numeric_limits<double>::quiet_NaN();
	params.batt_discharge_ub = numeric_limits<double>::quiet_NaN();
	params.batt_pr_min = numeric_limits<double>::quiet_NaN();
	params.batt_pr_max = numeric_limits<double>::quiet_NaN();
	params.batt_res = numeric_limits<double>::quiet_NaN();
	params.batt_soc_min = numeric_limits<double>::quiet_NaN();
	params.batt_soc_max = numeric_limits<double>::quiet_NaN();
	params.rec_helio_cost = numeric_limits<double>::quiet_NaN();
	params.pc_cost = numeric_limits<double>::quiet_NaN();
	params.pc_sb_cost = numeric_limits<double>::quiet_NaN();
	params.pv_field_cost = numeric_limits<double>::quiet_NaN();
	params.batt_charge_cost = numeric_limits<double>::quiet_NaN();
	params.batt_discharge_cost = numeric_limits<double>::quiet_NaN();
	params.batt_lifecycle_cost = numeric_limits<double>::quiet_NaN();
	params.pen_rec_hot_su = numeric_limits<double>::quiet_NaN();
	params.pen_pc_hot_su = numeric_limits<double>::quiet_NaN();
	params.w_ramp_limit = numeric_limits<double>::quiet_NaN();
	params.alpha_pv = params.beta_pv = numeric_limits<double>::quiet_NaN();
	params.w_dc_pl = numeric_limits<double>::quiet_NaN();
	params.alpha = numeric_limits<double>::quiet_NaN();
	params.batt_state0 = numeric_limits<double>::quiet_NaN();

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

    outputs.objective = numeric_limits<double>::quiet_NaN();
    outputs.objective_relaxed = numeric_limits<double>::quiet_NaN();
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
	//outputs.e_sales_price.clear();	//already a price output? check csp_solver or cmod_tcsmolten_salt
	outputs.w_dc_field.clear();		//new output
}

bool csp_dispatch_opt::check_setup(int nstep)
{
    //check parameters and inputs to make sure everything has been set up correctly
    if( (int)price_signal.size() < nstep )   return false;

    if( !m_is_weather_setup ) return false;
    if( params.siminfo == 0 ) return false;
    
    return true;
}

bool csp_dispatch_opt::copy_weather_data(C_csp_weatherreader &weather_source)
{
    //Copy the weather data
    m_weather = weather_source;

    return m_is_weather_setup = true;
}

bool csp_dispatch_opt::predict_performance(int step_start, int ntimeints, int divs_per_int)
{
    //Step number - 1-based index for first hour of the year.

    //save step count
    m_nstep_opt = ntimeints;

    //Predict performance out nstep values. 
    clear_output_arrays();

    if(! check_setup(m_nstep_opt) )
        throw C_csp_exception("Dispatch optimization precheck failed.");

    //create the sim info
    C_csp_solver_sim_info simloc;    // = *params.siminfo;
	simloc.ms_ts.m_step = params.siminfo->ms_ts.m_step;


    double Asf = params.col_rec->get_collector_area();

    double ave_weight = 1./(double)divs_per_int;

    for(int i=0; i<m_nstep_opt; i++)
    {
        //initialize hourly average values
        double therm_eff_ave = 0.;
        double cycle_eff_ave = 0.;
        double q_inc_ave = 0.;
        double wcond_ave = 0.;
		double f_pb_op_lim_ave = 0.0;

        for(int j=0; j<divs_per_int; j++)     //take averages over hour if needed
        {

            //jump to the current step
            if(! m_weather.read_time_step( step_start+i*divs_per_int+j, simloc ) )
                return false;

            //get DNI
            double dni = m_weather.ms_outputs.m_beam;
            if( m_weather.ms_outputs.m_solzen > 90. || dni < 0. )
                dni = 0.;

            //get optical efficiency
            double opt_eff = params.col_rec->calculate_optical_efficiency(m_weather.ms_outputs, simloc);

            double q_inc = Asf * opt_eff * dni * 1.e-3; //kW

            //get thermal efficiency
            double therm_eff = params.col_rec->calculate_thermal_efficiency_approx(m_weather.ms_outputs, q_inc*0.001);
            therm_eff *= params.sf_effadj;
            therm_eff_ave += therm_eff * ave_weight;

            //store the predicted field energy output
            q_inc_ave += q_inc * therm_eff * ave_weight;

            //store the power cycle efficiency
            double cycle_eff = params.eff_table_Tdb.interpolate( m_weather.ms_outputs.m_tdry );
            cycle_eff *= params.eta_cycle_ref;  
            cycle_eff_ave += cycle_eff * ave_weight;

			double f_pb_op_lim_local = std::numeric_limits<double>::quiet_NaN();
			double m_dot_htf_max_local = std::numeric_limits<double>::quiet_NaN();
			params.mpc_pc->get_max_power_output_operation_constraints(m_weather.ms_outputs.m_tdry, m_dot_htf_max_local, f_pb_op_lim_local);
			f_pb_op_lim_ave += f_pb_op_lim_local * ave_weight;	//[-]

            //store the condenser parasitic power fraction
            double wcond_f = params.wcondcoef_table_Tdb.interpolate( m_weather.ms_outputs.m_tdry );
            wcond_ave += wcond_f * ave_weight;

		    simloc.ms_ts.m_time += simloc.ms_ts.m_step;
            m_weather.converged();
        }

        //-----report hourly averages
        //thermal efficiency
        outputs.eta_sf_expected.push_back(therm_eff_ave);
        //predicted field energy output
        outputs.q_sfavail_expected.push_back( q_inc_ave );
        //power cycle efficiency
        outputs.eta_pb_expected.push_back( cycle_eff_ave );
		// Maximum power cycle output (normalized)
		outputs.f_pb_op_limit.push_back(f_pb_op_lim_ave);		//[-]
        //condenser power
        outputs.w_condf_expected.push_back( wcond_ave );
    }

    //reset the weather data reader
    //m_weather.jump_to_timestep(step_start, simloc);
    
    return true;
}

static void calculate_parameters(csp_dispatch_opt *optinst, unordered_map<std::string, double> &pars, int nt)
{
    /* 
    A central location for making sure the parameters from the model are accurately calculated for use in
    the dispatch optimization model. 
    */

        pars["T"] = nt ;
        pars["delta"] = optinst->params.dt;
        pars["Eu"] = optinst->params.e_tes_max ;
        pars["Er"] = optinst->params.e_rec_startup ;
        pars["Ec"] = optinst->params.e_pb_startup_cold ;
        pars["Qu"] = optinst->params.q_pb_max ;
        pars["Ql"] = optinst->params.q_pb_min ;
        pars["Qru"] = optinst->params.e_rec_startup / optinst->params.dt_rec_startup;
        pars["Qrl"] = optinst->params.q_rec_min ;
        pars["Qc"] = optinst->params.e_pb_startup_cold / ceil(optinst->params.dt_pb_startup_cold/pars["delta"]) / pars["delta"];
        pars["Qb"] = optinst->params.q_pb_standby ;
        pars["Lr"] = optinst->params.w_rec_pump ;
        pars["Lc"] = optinst->params.w_cycle_pump;
        pars["Wh"] = optinst->params.w_track;
        pars["Wb"] = optinst->params.w_cycle_standby;
        pars["Ehs"] = optinst->params.w_stow;
        pars["Wrsb"] = optinst->params.w_rec_ht;
        pars["eta_cycle"] = optinst->params.eta_cycle_ref;
        pars["Qrsd"] = 0.;      //<< not yet modeled, passing temporarily as zero

		pars["count"] = optinst->params.counter; //arbitrary counter parameter, keeps track of iterations of optimize()
        //pars["s0"] = optinst->params.e_tes_init ;
		//pars["s0"] = 816919.;
		pars["s0"] = 1.65495e6;
		pars["ursu0"] = 0.;		//
        pars["ucsu0"] = 0.;		//
        pars["y0"] = (optinst->params.is_pb_operating0 ? 1 : 0) ;	//
        pars["ycsb0"] = (optinst->params.is_pb_standby0 ? 1 : 0) ;	//
        pars["q0"] =  optinst->params.q_pb0 ;
		//pars["q0"] = 392407;
		//pars["bsoc0"] = optinst->params.batt_state0;
		pars["bsoc0"] = 0.2;
        pars["qrecmaxobs"] = 1.;
        for(int i=0; i<(int)optinst->outputs.q_sfavail_expected.size(); i++)
            pars["qrecmaxobs"] = optinst->outputs.q_sfavail_expected.at(i) > pars["qrecmaxobs"] ? optinst->outputs.q_sfavail_expected.at(i) : pars["qrecmaxobs"];

        pars["Qrsb"] = optinst->params.q_rec_standby; // * dq_rsu;     //.02
        pars["M"] = 1.e6;
        pars["W_dot_cycle"] = optinst->params.q_pb_des * optinst->params.eta_cycle_ref;

		/***PV Hybrid parameters***/

		//Battery parameters
		pars["Av"] = optinst->params.batt_slope_coeff;
		pars["Bv"] = optinst->params.batt_int_coeff;
		pars["Cb"] = optinst->params.batt_capacity;
		pars["Iavg"] = optinst->params.i_expected;
		pars["Il+"] = optinst->params.batt_charge_lb;
		pars["Iu+"] = optinst->params.batt_charge_ub;
		pars["Il-"] = optinst->params.batt_discharge_lb;
		pars["Iu-"] = optinst->params.batt_discharge_ub;
		pars["Pb_min"] = optinst->params.batt_pr_min;
		pars["Pb_max"] = optinst->params.batt_pr_max;
		pars["Rint"] = optinst->params.batt_res;
		pars["Sb_min"] = optinst->params.batt_soc_min;
		pars["Sb_max"] = optinst->params.batt_soc_max;

		//Cost parameters
		pars["Crhsp"] = optinst->params.pen_rec_hot_su;
		pars["Cchsp"] = optinst->params.pen_pc_hot_su;

		//Power cycle parameters
		pars["w_delta_lim"] = optinst->params.w_ramp_limit;	//might use wlim_series instead?
		
		//PV parameters
		pars["Apv"] = optinst->params.alpha_pv;
		pars["Bpv"] = optinst->params.beta_pv;
		pars["Wi"] = optinst->params.w_dc_pl;

		//Misc. parameters
		pars["eps"] = 1.e-6;

		pars["wlim_min"] = 9.e99;
		for (int t = 0; t < nt; t++)
			pars["wlim_min"] = fmin(pars["wlim_min"], optinst->w_lim.at(t));

        //calculate Z parameters
        pars["Z_1"] = 0.;
        pars["Z_2"] = 0.;
        {
            double fi = 0.;
            double fhfi2 = 0.;
            double fhfi = 0.;
            double fhfi_2 = 0.;
            vector<double> fiv;
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
            pars["Wdot0"]= pars["etap"]*pars["q0"]*optinst->outputs.eta_pb_expected.at(0);

        pars["Wdotu"] = (pars["Qu"] - limit1) * pars["etap"];
        //pars["Wdotu"] = 170392;
		pars["Wdotl"] = (pars["Ql"] - limit1) * pars["etap"];

		// TES weighting (gamma) and complement (gammac)
		pars["gamma"] = optinst->forecast_params.fc_gamma;
		pars["gammac"] = 1. - pars["gamma"];

        // Adjust wlim if specified value is too low to permit cycle operation
        optinst->outputs.wnet_lim_min.resize(nt);
        optinst->outputs.delta_rs.resize(nt);
        for(int t=0; t<nt; t++)
        {
		    double wmin = (pars["Ql"] * pars["etap"]*optinst->outputs.eta_pb_expected.at(t) / optinst->params.eta_cycle_ref) + 
                            (pars["Wdotu"] - pars["etap"]*pars["Qu"])*optinst->outputs.eta_pb_expected.at(t) / optinst->params.eta_cycle_ref; // Electricity generation at minimum pb thermal input
		    double max_parasitic = 
                    pars["Lr"] * optinst->outputs.q_sfavail_expected.at(t) 
                + (optinst->params.w_rec_ht / optinst->params.dt) 
                + (optinst->params.w_stow / optinst->params.dt) 
                + optinst->params.w_track 
                + optinst->params.w_cycle_standby 
                + optinst->params.w_cycle_pump*pars["Qu"]
                + optinst->outputs.w_condf_expected.at(t)*pars["W_dot_cycle"];  // Largest possible parasitic load at time t

            //save for writing to ampl
            optinst->outputs.wnet_lim_min.at(t) =  wmin - max_parasitic;
            if( t < nt-1 )
            {
                double delta_rec_startup = min(1., max(optinst->params.e_rec_startup / max(optinst->outputs.q_sfavail_expected.at(t + 1)*pars["delta"], 1.), optinst->params.dt_rec_startup / pars["delta"]));
                optinst->outputs.delta_rs.at(t) = delta_rec_startup;
            }
        }

		optinst->outputs.w_dc_field.resize(nt);

        //temporary fixed constants
        pars["disp_time_weighting"] = optinst->params.disp_time_weighting;
        pars["rsu_cost"] = optinst->params.rsu_cost; //5650;
        pars["csu_cost"] = optinst->params.csu_cost; //6520.;
		pars["Crec"] = optinst->params.rec_helio_cost; //0.003;
		pars["Cpc"] = optinst->params.pc_cost; //0.002; 
		pars["Ccsb"] = optinst->params.pc_sb_cost; //0.008;
		pars["Cpv"] = optinst->params.pv_field_cost; //0.005;
		pars["Cbc"] = optinst->params.batt_charge_cost; //0.001; 
		pars["Cbd"] = optinst->params.batt_discharge_cost; //0.001; 
		pars["Cbl"] = optinst->params.batt_lifecycle_cost; //250.; 
        pars["pen_delta_w"] = optinst->params.pen_delta_w; //0.00064; 
};

bool csp_dispatch_opt::optimize()
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
    xr          kWt     Power delivered by the receiver at time t
    xrsu        kWt     Power used by the reciever for start up
    ursu        kWt     Receiver accumulated start-up thermal power at time t
    x           kWt	    Cycle thermal power consumption at time t 
    ucsu        kWt     Cycle accumulated start-up thermal power at time t
    s           kWht    TES reserve quantity at time t (auxiliary variable) 
    wdot        kWe     Electrical power production at time t
    delta_w     kWe     Positive change in power production at time t w/r/t t-1
	bc			-		Battery cycle count
	bsoc		-		State of charge of battery in time period t
	i+			kA		Battery current for charge in time period t
	i-			kA		Battery current for discharge in time period t
	vsoc		V		Battery voltage in time period t
	wdot+		kWe		Power into the battery at time t
	wdot-		kWe		Power out of the battery at time t
	wdot_pv		kWe		Power from the photovoltaic field at time t
	wdot_pv+	kWe		Power from PV directly charging the battery at time t
	wdot_s		kWe		Electrical power sold to the grid at time t
	wdot_p		kWe		Electrical power purchased from the grid at time t
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
	y+				1 if battery is charging in time period t; 0 otherwise
	y-				1 if battery is discharging in time period t; 0 otherwise
	ypv				1 if the PV field is generating power at time t; 0 otherwise
	yoff			1 if cycle is in an "off" state; 0 otherwise
    -------------------------------------------------------------
    */
    lprec *lp;
    int ret = 0;

    try{

        //Calculate the number of variables
        int nt = (int)m_nstep_opt;

        //set up the variable structure
        optimization_vars O;
        O.add_var("xr", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0. );
        O.add_var("xrsu", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0. );
        O.add_var("ursu", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0. );
        O.add_var("yr", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("yrsu", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("yrsb", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);	//commented originally
        O.add_var("yrsd", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);	//commented originally
        O.add_var("yrsup", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("yrhsp", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);	//commented originally

        O.add_var("x", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0.);
        O.add_var("y", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("s", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0. );
        O.add_var("ucsu", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0. );
        O.add_var("ycsu", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("ycsb", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
//#ifdef MOD_CYCLE_SHUTDOWN
        O.add_var("ycsd", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
//#endif
        O.add_var("ycsup", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("ychsp", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("wdot", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0. ); //0 lower bound
        O.add_var("delta_w", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0. ); 

		O.add_var("bc", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, 1, 0.);	//has no time index in paper
		O.add_var("bsoc", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0.);
		O.add_var("i+", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0.);
		O.add_var("i-", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0.);
		O.add_var("vsoc", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0.);
		O.add_var("wdot+", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0.);
		O.add_var("wdot-", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0.);
		O.add_var("wdot_pv", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0.);
		O.add_var("wdot_pv+", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0.);
		O.add_var("wdot_s", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0.);
		O.add_var("wdot_p", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0.);

		O.add_var("y+", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
		O.add_var("y-", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
		O.add_var("ypv", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);

		O.add_var("yoff", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);

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

		//check Qin and Qrl?
		//still working on this
		int tempCount = floor(params.counter / 2.0);	//counter used to track number of PV_data_<tempCount>.dat file; since params.counter increments by two for each time period
		string line;	//arbitrary line string
		string dataFile = "PV_data_" + to_string(tempCount) + ".dat";	//stores name of .dat file
		//for the .dat naming logic to work, must have 3 base cases
		if (params.counter == 0 || params.counter == 1)
			dataFile = "PV_data_0.dat";
	
		if (params.counter == 2)
			dataFile = "PV_data_1.dat";
		//save the file path (save same place as .lk file for sdktool)
		string filePath = "pv_data/" + dataFile;
		ifstream pv_file(filePath);	//open .dat file
		int i = 0;
		if (pv_file.is_open())
		{
			while (getline(pv_file, line))
			{
				string num;
				double value;
				//divide two strings and save second one into w_dc_field as a double
				pv_file >> num >> value;
				if (num == ";")
					break;
				outputs.w_dc_field.at(i) = value;
				i++;
			}
		}
		//write output of w_dc_field to a file
		ofstream pv_out("w_dc_field.txt");
		pv_out << "wpv_dc:" << endl;
		for (int i = 0; i < outputs.w_dc_field.size(); i++) {
			pv_out << outputs.w_dc_field.at(i) << endl;
		}

        /* 
        --------------------------------------------------------------------------------
        set up the objective function first (per lpsolve guidance)
        --------------------------------------------------------------------------------
        */
		{
            int *col = new int[17 * nt];
            REAL *row = new REAL[17 * nt];
            double tadj = P["disp_time_weighting"];
            int i = 0;

            //calculate the mean price to appropriately weight the receiver production timing derate
            double pmean =0;
            for(int t=0; t<(int)price_signal.size(); t++)
                pmean += price_signal.at(t);
            pmean /= (double)price_signal.size();
            //--
            
            for(int t=0; t<nt; t++)
            {
                i = 0;
                //col[ t + nt*(i  ) ] = O.column("wdot", t);
                //row[ t + nt*(i++) ] = P["delta"] * price_signal.at(t)*tadj*(1.-outputs.w_condf_expected.at(t));

                //col[ t + nt*(i  ) ] = O.column("xr", t);
                //row[ t + nt*(i++) ] = -(P["delta"] * price_signal.at(t) * P["Lr"])+tadj*pmean;  // tadj added to prefer receiver production sooner (i.e. delay dumping)

                //col[ t + nt*(i  ) ] = O.column("xrsu", t);
                //row[ t + nt*(i++) ] = -P["delta"] * price_signal.at(t) * P["Lr"];

                //col[ t + nt*(i  ) ] = O.column("yrsu", t);
                //row[ t + nt*(i++) ] = -price_signal.at(t) * (params.w_rec_ht + params.w_stow);

                //col[ t + nt*(i  ) ] = O.column("yr", t);
                //row[ t + nt*(i++) ] = -(P["delta"] * price_signal.at(t) * params.w_track) + tadj;	// tadj added to prefer receiver operation in nearer term to longer term

                //col[ t + nt*(i  ) ] = O.column("x", t);
                //row[ t + nt*(i++) ] = -P["delta"] * price_signal.at(t) * params.w_cycle_pump;

                //col[ t + nt*(i  ) ] = O.column("ycsb", t);
                //row[ t + nt*(i++) ] = -P["delta"] * price_signal.at(t) * params.w_cycle_standby;

                ////xxcol[ t + nt*(i   ] = O.column("yrsb", t);
                ////xxrow[ t + nt*(i++) ] = -delta * price_signal.at(t) * (Lr * Qrl + (params.w_stow / delta));

                ////xxcol[ t + nt*(i   ] = O.column("yrsd", t);
                ////xxrow[ t + nt*(i++) ] = -0.5 - (params.w_stow);

                ////xxcol[ t + nt*(i   ] = O.column("ycsd", t);
                ////xxrow[ t + nt*(i++) ] = -0.5;

                //col[ t + nt*(i  ) ] = O.column("yrsup", t);
                //row[ t + nt*(i++) ] = -P["rsu_cost"]*tadj;

                ////xxcol[ t + nt*(i   ] = O.column("yrhsp", t);
                ////xxrow[ t + nt*(i++) ] = -tadj;

                //col[ t + nt*(i  ) ] = O.column("ycsup", t);
                //row[ t + nt*(i++) ] = -P["csu_cost"]*tadj;

                //col[ t + nt*(i  ) ] = O.column("ychsp", t);
                //row[ t + nt*(i++) ] = -P["csu_cost"]*tadj * 0.1;

                //col[ t + nt*(i  ) ] = O.column("delta_w", t);
                //row[ t + nt*(i++) ] = -P["pen_delta_w"]*tadj;

                //tadj *= P["disp_time_weighting"];

				col[t + nt * (i)] = O.column("wdot_s", t);
				row[t + nt * (i++)] = P["delta"] * price_signal.at(t);

				col[t + nt * (i)] = O.column("wdot_p", t);
				row[t + nt * (i++)] = -P["delta"] * price_signal.at(t);

				col[t + nt * (i)] = O.column("ycsup", t);
				row[t + nt * (i++)] = -(1. / pow(tadj, t)) * P["csu_cost"];

				col[t + nt * (i)] = O.column("ychsp", t);
				row[t + nt * (i++)] = -(1. / pow(tadj, t)) * P["Cchsp"];	//change P["Cchsp"] to 0.1*P["csu_cost"]?
//#ifdef MOD_CYCLE_SHUTDOWN
				col[t + nt * (i)] = O.column("ycsd", t);
				row[t + nt * (i++)] = -(1. / pow(tadj, t)) * params.alpha;
//#endif
				col[t + nt * (i)] = O.column("delta_w", t);
				row[t + nt * (i++)] = -(1. / pow(tadj, t)) * P["pen_delta_w"];

				col[t + nt * (i)] = O.column("yrsup", t);
				row[t + nt * (i++)] = -(1. / pow(tadj, t)) * P["rsu_cost"];

				col[t + nt * (i)] = O.column("yrhsp", t);
				row[t + nt * (i++)] = -(1. / pow(tadj, t)) * P["Crhsp"];	//change P["Crhsp"] to 0.1*P["rsu_cost"]?

				col[t + nt * (i)] = O.column("yrsd", t);
				row[t + nt * (i++)] = -(1. / pow(tadj, t)) * params.alpha;

				col[t + nt * (i)] = O.column("wdot", t);
				row[t + nt * (i++)] = -(1. / pow(tadj, t)) * P["delta"] * P["Cpc"];

				col[t + nt * (i)] = O.column("ycsb", t);
				row[t + nt * (i++)] = -(1. / pow(tadj, t)) * P["delta"] * P["Ccsb"] * P["Qb"];

				col[t + nt * (i)] = O.column("wdot_pv", t);
				row[t + nt * (i++)] = -(1. / pow(tadj, t)) * P["delta"] * P["Cpv"];

				col[t + nt * (i)] = O.column("wdot-", t);
				row[t + nt * (i++)] = -(1. / pow(tadj, t)) * P["delta"] * P["Cbd"];

				col[t + nt * (i)] = O.column("wdot+", t);
				row[t + nt * (i++)] = -(1. / pow(tadj, t)) * P["delta"] * P["Cbc"];

				col[t + nt * (i)] = O.column("xr", t);
				row[t + nt * (i++)] = -(1. / pow(tadj, t)) * P["delta"] * P["Crec"];

				col[t + nt * (i)] = O.column("bc", 0);
				row[t + nt * (i++)] = -P["Cbl"];

				//tadj *= P["disp_time_weighting"];
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

		/*ofstream bounds;
		bounds.open("c:/users/dquintan/Documents/Work for Alex/Code/bounds.txt");
		for (int i = 0; i < O.get_num_varobjs(); i++)
		{
			optimization_vars::opt_var *v = O.get_var(i);
			bounds << v->name << ": " << v->lower_bound << " " << v->upper_bound << endl;
		}
		bounds.close();*/

		ofstream qin;
		qin.open("c:/users/dquintan/Documents/Work for Alex/Code/Qin.txt");
		qin << "Qin" << " " << ceil(params.counter / 2.0) << endl;
		for (int i = 0; i < nt; i++) {
			qin << outputs.q_sfavail_expected.at(i) << endl;
		}

		ofstream qrl;
		qrl.open("c:/users/dquintan/Documents/Work for Alex/Code/Qrl.txt");
		qrl << "Qrl: " << endl;
		qrl << P["Qrl"];

		ofstream wnet;
		wnet.open("c:/users/dquintan/Documents/Work for Alex/Code/Wnet.txt");
		wnet << "Wnet: " << endl;
		for (int i = 0; i < nt; i++) {
			wnet << w_lim.at(i) << endl;
		}

        /* 
        --------------------------------------------------------------------------------
        set up the constraints
        --------------------------------------------------------------------------------t
        */
		{
			//Linearization of the implementation of the piecewise efficiency equation 
			REAL row[3];
			int col[3];

			for (int t = 0; t < nt; t++)
			{
				int i = 0;
				//power production curve
				row[i] = 1.;
				col[i++] = O.column("wdot", t);

				row[i] = -P["etap"] * (outputs.eta_pb_expected.at(t) / params.eta_cycle_ref);
				col[i++] = O.column("x", t);

				row[i] = -(P["Wdotu"] - P["etap"] * P["Qu"])*(outputs.eta_pb_expected.at(t) / params.eta_cycle_ref);
				col[i++] = O.column("y", t);

				/*row[i] = -P["Wdotu"] * (outputs.eta_pb_expected.at(t) / params.eta_cycle_ref);
				col[i++] = O.column("y", t);

				row[i] = P["etap"] * P["Qu"] * (outputs.eta_pb_expected.at(t) / params.eta_cycle_ref);
				col[i++] = O.column("y", t);*/

				//row[i  ] = -outputs.eta_pb_expected.at(t);
				//col[i++] = O.column("x", t);

				add_constraintex(lp, i, row, col, EQ, 0.);

			}
		}

        //cycle production change	 completed
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

		//cycle production change
		{
			REAL row[3];
			int col[3];

			for (int t = 0; t < nt; t++)
			{
				col[0] = O.column("delta_w", t);
				row[0] = 1.;

				col[1] = O.column("wdot", t);
				row[1] = 1.;

				if (t > 0)
				{
					col[2] = O.column("wdot", t - 1);
					row[2] = -1.;

					add_constraintex(lp, 3, row, col, GE, 0.);
				}
				else
				{
					add_constraintex(lp, 2, row, col, GE, P["Wdot0"]);
				}
			}
		}

		//cycle ramping rate limit
		{
			REAL row[1];
			int col[1];

			for (int t = 0; t < nt; t++)
			{
				row[0] = 1.;
				col[0] = O.column("delta_w", t);

				add_constraintex(lp, 1, row, col, LE, P["w_delta_lim"]);
			}
		}

	    //cycle ramping rate limit
//		{
//			REAL row[3];
//			int col[3];
//
//			for (int t = 0; t < nt; t++)
//			{
//				double rhs = (P["w_delta_lim"] / 100.)*(60 * P["delta"])*P["Wdotu"] + (outputs.eta_pb_expected.at(t) / params.eta_cycle_ref)*(P["etap"] * P["Ql"] + (P["Wdotu"] - P["etap"] * P["Qu"]));
//				double rhs2 = (P["w_delta_lim"] / 100.)*(60 * P["delta"])*P["Wdotu"];
//
//				row[0] = rhs2;
//				col[0] = O.column("y", t);
//
//				row[1] = 2*rhs2;
//				col[1] = O.column("yoff", t);
//
//				if (t > 0)
//				{	
//					row[2] = -rhs2;
//					col[2] = O.column("y", t - 1);
//
//					add_constraintex(lp, 3, row, col, LE, rhs);
//				}
//				else
//				{
//					add_constraintex(lp, 2, row, col, LE, rhs - rhs2 * (params.is_pb_operating0 ? 1. : 0.));
//				}
//			}
//		}
   
		//grid transmission limit
		{
			REAL row[1];
			int col[1];

			for (int t = 0; t < nt; t++)
			{
				row[0] = 1.;
				col[0] = O.column("wdot_s", t);

				add_constraintex(lp, 1, row, col, LE, w_lim.at(t));
			}
		}

		//positive and negative power flow (determined by a power balance on the AC bus of the hybrid system)		checked
		{
			REAL row[20];
			int col[20];

			for (int t = 0; t < nt; t++)
			{
				int i = 0;
				//power cycle generation less condenser parasitic power,
				row[i] = 1.0 - outputs.w_condf_expected.at(t);
				col[i++] = O.column("wdot", t);

				//battery discharge power accounting for DC-to-AC conversion losses
				row[i] = 1.0 / (1.0 + params.slope_minus);
				col[i++] = O.column("wdot-", t);
				row[i] = -params.slope_int_minus / (1.0 + params.slope_minus);
				col[i++] = O.column("y-", t);

				//battery charge power accounting for AC-to-DC conversion losses
				row[i] = -(1.0 + params.slope_plus);
				col[i++] = O.column("wdot+", t);
				row[i] = -params.slope_int_plus;
				col[i++] = O.column("y+", t);

				//PV field generation less power used for battery charging directly from the field, accounting for inverter losses
				row[i] = 1.0 / (1.0 + params.beta_pv);
				col[i++] = O.column("wdot_pv", t);
				row[i] = -1.0 / (1.0 + params.beta_pv);
				col[i++] = O.column("wdot_pv+", t);
				row[i] = -params.alpha_pv / (1.0 + params.beta_pv);
				col[i++] = O.column("ypv", t);

				//TES pumping power requirements for receiver operations
				row[i] = -P["Lr"];
				col[i++] = O.column("xr", t);
				row[i] = -P["Lr"];
				col[i++] = O.column("xrsu", t);
				row[i] = -P["Lr"] * P["Qrl"];
				col[i++] = O.column("yrsb", t);

				//TES pumping power requirements for cycle operation
				row[i] = -P["Lc"];
				col[i++] = O.column("x", t);

				//heliostat tracking power
				row[i] = -P["Wh"];
				col[i++] = O.column("yr", t);

				//power cycle standby parasitic power
				row[i] = -P["Wb"];
				col[i++] = O.column("ycsb", t);

				//tower piping heat trace for receiver start-up
				row[i] = -P["Wrsb"];
				col[i++] = O.column("yrsu", t);

				//heliostat field stow power for different receiver operations
				row[i] = -P["Ehs"] / P["delta"];
				col[i++] = O.column("yrsu", t);
				row[i] = -P["Ehs"] / P["delta"];
				col[i++] = O.column("yrsb", t);
				row[i] = -P["Ehs"] / P["delta"];
				col[i++] = O.column("yrsd", t);

				row[i] = -1.;
				col[i++] = O.column("wdot_s", t);

				row[i] = 1.;
				col[i++] = O.column("wdot_p", t);

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

        //        add_constraintex(lp, i, row, col, GE, outputs.q_sfavail_expected.at(t)*0.999 );
        //    }
        //} //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


        {
            REAL row[5];
            int col[5];

            for(int t=0; t<nt; t++)
            {

                //Receiver startup inventory	complete
                row[0] = 1.;
                col[0] = O.column("ursu", t);

                row[1] = -P["delta"];
                col[1] = O.column("xrsu", t);

                if(t>0)
                {
                    row[2] = -1.;
                    col[2] = O.column("ursu", t-1);

                    add_constraintex(lp, 3, row, col, LE, 0.);
                }
                else
                {
                    add_constraintex(lp, 2, row, col, LE, 0.);
                }

                //-----

                //inventory nonzero		complete
                row[0] = 1.;
                col[0] = O.column("ursu", t);

                row[1] = -P["Er"];
                col[1] = O.column("yrsu", t);

                add_constraintex(lp, 2, row, col, LE, 0.);

                if(t>0)
                {
					//Receiver operation allowed when:	  checked: not sure if first two rows/cols should be outside or inside if statement
					row[0] = 1.;
					col[0] = O.column("yr", t);

					row[1] = -1.0 / P["Er"];
					col[1] = O.column("ursu", t);

                    row[2] = -1.;
                    col[2] = O.column("yr", t-1);

					row[3] = -1.;
					col[3] = O.column("yrsb", t - 1);		//keep this in mind when changing objective function to hybrid system

                    add_constraintex(lp, 4, row, col, LE, 0.); 
                }
                /*else
                {
                    add_constraintex(lp, 2, row, col, LE, (params.is_rec_operating0 ? 1. : 0.) );
                }*/

                //Receiver startup can't be enabled after a time step where the Receiver was operating		checked
                if(t>0)
                {
					row[0] = 1.;
					col[0] = O.column("yrsu", t);

                    row[1] = 1.;
                    col[1] = O.column("yr", t-1);

                    add_constraintex(lp, 2, row, col, LE, 1.);
                }
				/*else
				{	
					add_constraintex(lp, 1, row, col, LE, 1.);
				}*/

                //Receiver startup energy consumption	  complete
                row[0] = 1.;
                col[0] = O.column("xrsu", t);

                row[1] = -P["Qru"];
                col[1] = O.column("yrsu", t);

                add_constraintex(lp, 2, row, col, LE, 0.);

                //Receiver startup only during solar positive periods -- was uncommented originally
                /*row[0] = 1.;
                col[0] = O.column("yrsu", t);

                add_constraintex(lp, 1, row, col, LE, min(P["M"]*outputs.q_sfavail_expected.at(t), 1.0) );*/

				//Trivial solar resource prevents receiver start - up	 complete
				row[0] = 1.;
				col[0] = O.column("yrsu", t);

				add_constraintex(lp, 1, row, col, LE, (outputs.q_sfavail_expected.at(t) / P["Qrl"]));

                //Receiver consumption limit	complete	(3a) Problematic constraint
                row[0] = 1.;
                col[0] = O.column("xr", t);

                row[1] = 1.;
                col[1] = O.column("xrsu", t);
//#ifdef MOD_REC_STANDBY 
				row[2] = P["Qrsd"];				
				col[2] = O.column("yrsd", t);		
//#endif          
                add_constraintex(lp, 3, row, col, LE, outputs.q_sfavail_expected.at(t));

                //Receiver operation mode requirement	complete	(3b) problematic constraint
                row[0] = 1.;
                col[0] = O.column("xr", t);

                row[1] = -outputs.q_sfavail_expected.at(t);
                col[1] = O.column("yr", t);

                add_constraintex(lp, 2, row, col, LE, 0.);

                //Receiver minimum operation requirement	complete
                row[0] = 1.;
                col[0] = O.column("xr", t);

                row[1] = -P["Qrl"];
                col[1] = O.column("yr", t);

                add_constraintex(lp, 2, row, col, GE, 0.);

                //Receiver can't continue operating when no energy is available	-- uncommented originally
                /*row[0] = 1.;
                col[0] = O.column("yr", t);

                add_constraintex(lp, 1, row, col, LE, min(P["M"]*outputs.q_sfavail_expected.at(t), 1.0) );*/  //if any measurable energy, y^r can be 1

				//In the absence of thermal power, the receiver is not able to operate		complete
				row[0] = 1.;
				col[0] = O.column("yr", t);

				add_constraintex(lp, 1, row, col, LE, outputs.q_sfavail_expected.at(t) / P["Qrl"]);

                // --- new constraints ---

                //receiver startup/standby persist		commented originally
                row[0] = 1.;
                col[0] = O.column("yrsu", t);

                row[1] = 1.;
                col[1] = O.column("yrsb", t);

                add_constraintex(lp, 2, row, col, LE, 1.);

                //reciever standby partition		commented originally
                row[0] = 1.;
                col[0] = O.column("yr", t);

                row[1] = 1.;
                col[1] = O.column("yrsb", t);

                add_constraintex(lp, 2, row, col, LE, 1.);

                if( t > 0 )
                {
                    //rsb_persist		commented originally
                    row[0] = 1.;
                    col[0] = O.column("yrsb", t);

                    row[1] = -1.;
                    col[1] = O.column("yr", t-1);

                    row[2] = -1.;
                    col[2] = O.column("yrsb", t-1);

                    add_constraintex(lp, 3, row, col, LE, 0.);

                    //receiver startup penalty
                    row[0] = 1.;
                    col[0] = O.column("yrsup", t);

                    row[1] = -1.;
                    col[1] = O.column("yrsu", t);

                    row[2] = 1.;
                    col[2] = O.column("yrsu", t-1);

                    add_constraintex(lp, 3, row, col, GE, 0.);

                    //receiver hot startup penalty		commented originally
                    row[0] = 1.;
                    col[0] = O.column("yrhsp", t);

                    row[1] = -1.;
                    col[1] = O.column("yr", t);

                    row[2] = -1.;
                    col[2] = O.column("yrsb", t-1);

                    add_constraintex(lp, 3, row, col, GE, -1);

					//receiver shutdown energy
					row[0] = 1.;
					col[0] = O.column("yrsd", t);

					row[1] = -1.;
					col[1] = O.column("yr", t - 1);

					row[2] = 1.;
					col[2] = O.column("yr", t);

					row[3] = -1.;
					col[3] = O.column("yrsb", t - 1);

					row[4] = 1.;
					col[4] = O.column("yrsb", t);

					add_constraintex(lp, 5, row, col, GE, 0.);

                    //receiver shutdown energy -- this constraint happens on second time set
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


            for(int t=0; t<nt; t++)
            {
                int i=0;

                if(t>0)
                {
					//Startup Inventory balance		complete
					row[i] = 1.;
					col[i++] = O.column("ucsu", t);

                    row[i  ] = -1.;
                    col[i++] = O.column("ucsu", t-1);

					row[i] = -P["delta"] * P["Qc"];
					col[i++] = O.column("ycsu", t);

					add_constraintex(lp, i, row, col, LE, 0.);
                }

                //Inventory nonzero		complete
                row[0] = 1.;
                col[0] = O.column("ucsu", t);

                row[1] = -P["Ec"];				//change from -P["M"] to -P["Ec"]
                col[1] = O.column("ycsu", t);

                add_constraintex(lp, 2, row, col, LE, 0.);

    //            //Cycle operation allowed when:		uncomment when switching to hybrid time format (happens on second time set)
    //            i=0;
    //            row[i  ] = 1.;
    //            col[i++] = O.column("y", t);
    //            
    //            row[i  ] = -1.0/P["Ec"];					
    //            col[i++] = O.column("ucsu", t);

    //            if(t>0)
    //            {
    //                row[i  ] = -1.;
    //                col[i++] = O.column("y", t-1);

    //                row[i  ] = -1.;
    //                col[i++] = O.column("ycsb", t-1);

    //                add_constraintex(lp, i, row, col, LE, 0.); 
    //            }
    //            else
    //            {
    //                add_constraintex(lp, i, row, col, LE, (params.is_pb_operating0 ? 1. : 0.) + (params.is_pb_standby0 ? 1. : 0.) );
    //            }

				//UNCOMMENTED ORIGINALLY. may be problematic when it comes to infeasability
				//Normal cycle operation can occur upon completion of start-up energy requirements, if the cycle is operating normally, or directly after stand-by mode
				i = 0;

				if (t > 0)
				{
					row[i] = 1.;
					col[i++] = O.column("y", t);

					row[i] = -1.0 / P["Ec"];
					col[i++] = O.column("ucsu", t - 1); 

					row[i] = -1.;
					col[i++] = O.column("y", t - 1);

					row[i] = -1.;
					col[i++] = O.column("ycsb", t - 1);

					add_constraintex(lp, i, row, col, LE, 0.);
				}
				//this else was making many more solutions infeasible
				//else
				//{
				//	add_constraintex(lp, i, row, col, LE, (params.is_pb_operating0 ? 1. : 0.) + (params.is_pb_standby0 ? 1. : 0.));
				//}

                //Cycle consumption limit -- happens on second time set
    //            i=0;
    //            row[i  ] = 1.;
    //            col[i++] = O.column("x", t);

    //            //mjw 2016.12.2 --> This constraint seems to be problematic in identifying feasible solutions for subhourly runs. Needs attention.
    //            row[i  ] = P["Qc"];
    //            col[i++] = O.column("ycsu", t);
    //            
    //            /*row[i  ] = -P["Qu"];
    //            col[i++] = O.column("y", t);
				////col[i++] = 1.;*/				//uncomment this when switching to hybrid system? makes all solns optimal

    //            add_constraintex(lp, i, row, col, LE, P["Qu"]);

                //cycle operation mode requirement	***makes more solutions optimal when commented
                row[0] = 1.;
                col[0] = O.column("x", t);

                row[1] = -P["Qu"];
                col[1] = O.column("y", t);

                add_constraintex(lp, 2, row, col, LE, 0.);

                //Minimum cycle energy contribution
                i=0;
                row[i  ] = 1.;
                col[i++] = O.column("x", t);

                row[i  ] = -P["Ql"];
                col[i++] = O.column("y", t);

                add_constraintex(lp, i, row, col, GE, 0.);

				// ******************** Logic Governing Cycle Modes *******************

                //cycle startup can't be enabled after a time step where the cycle was operating	completed
                if(t>0)
                {
                    row[0] = 1.;
                    col[0] = O.column("ycsu", t);

                    row[1] = 1.;
                    col[1] = O.column("y", t-1);

                    add_constraintex(lp, 2, row, col, LE, 1.);
                }

				//precludes power cycle start-up and operation from coinciding		completed
				row[0] = 1.;
				col[0] = O.column("y", t);

				row[1] = 1.;
				col[1] = O.column("ycsu", t);

				add_constraintex(lp, 2, row, col, LE, 1.);

				//not in Hamilton paper, but in model file
				/*row[0] = 1.;
				col[0] = O.column("y", t);

				row[1] = 1.;
				col[1] = O.column("ycsu", t);

				row[2] = 1.;
				col[2] = O.column("ycsb", t);

				add_constraintex(lp, 3, row, col, LE, 1.);*/

                //Standby mode entry	completed
                i=0;

				row[i] = 1.;
				col[i++] = O.column("ycsb", t);

                if(t>0)
				{
                    row[i  ] = -1.;
                    col[i++] = O.column("y", t-1);

                    row[i  ] = -1.;
                    col[i++] = O.column("ycsb", t-1);

                    add_constraintex(lp, i, row, col, LE, 0.);
                }
                else
                {
                    add_constraintex(lp, i, row, col, LE, (params.is_pb_standby0 ? 1 : 0) + (params.is_pb_operating0 ? 1 : 0));
                }

                //some modes can't coincide		completed
                row[0] = 1.;
                col[0] = O.column("ycsu", t);

                row[1] = 1.;
                col[1] = O.column("ycsb", t);    

                add_constraintex(lp, 2, row, col, LE, 1.);   

				//standby and power-producing modes can't coincide
                row[0] = 1.;
                col[0] = O.column("y", t);

                row[1] = 1.;
                col[1] = O.column("ycsb", t);    

                add_constraintex(lp, 2, row, col, LE, 1.);   

				//cycle start penalty	completed
				row[0] = 1.;
				col[0] = O.column("ycsup", t);

				row[1] = -1.;
				col[1] = O.column("ycsu", t);

				if (t > 0)
				{
					row[2] = 1.;
					col[2] = O.column("ycsu", t - 1);

					add_constraintex(lp, 3, row, col, GE, 0.);
				}
				else
				{
					add_constraintex(lp, 2, row, col, GE, 0.);
				}
				
				if(t>0)
				{
                    //cycle standby start penalty
                    row[0] = 1.;
                    col[0] = O.column("ychsp", t);

                    row[1] = -1.;
                    col[1] = O.column("y", t);

                    row[2] = -1.;
                    col[2] = O.column("ycsb", t-1);

                    add_constraintex(lp, 3, row, col, GE, -1);

//#ifdef MOD_CYCLE_SHUTDOWN
                    //cycle shutdown energy penalty
                    row[0] = 1.;
                    col[0] = O.column("ycsd", t-1);

                    row[1] = -1.;
                    col[1] = O.column("y", t-1);
                    
                    row[2] = 1.;
                    col[2] = O.column("y", t);
                    
                    row[3] = -1.;
                    col[3] = O.column("ycsb", t-1);
                    
                    row[4] = 1.;
                    col[4] = O.column("ycsb", t);

                    add_constraintex(lp, 5, row, col, GE, 0.);
//#endif
                }
            }
        }

		// ******************** Tighter LP Relaxtion *******************
		{
			REAL row[4];
			int col[4];


			for (int t = 0; t < nt; t++)
			{
				row[0] = 1.;
				col[0] = O.column("ycsb", 1);

				row[1] = 1. / P["Qu"];
				col[1] = O.column("x", t);

				add_constraintex(lp, 2, row, col, LE, 1.);

				//may or may not need to stay in code, was in ampl .mod file
				/*row[0] = 1.;
				col[0] = O.column("ycsb", t);
				
				row[1] = 1.0 / (P["Wdotu"] * (outputs.eta_pb_expected.at(t) / params.eta_cycle_ref));
				col[1] = O.column("wdot", t);

				add_constraintex(lp, 2, row, col, LE, 1.);*/

				row[0] = 1.;
				col[0] = O.column("ycsu", t);

				row[1] = 1.;
				col[1] = O.column("y", t);

				row[2] = 1.;
				col[2] = O.column("ycsb", t);

				row[3] = 1.;
				col[3] = O.column("yoff", t);

				add_constraintex(lp, 4, row, col, EQ, 1.);

				//this only happens on time set #2
				/*row[0] = 1.;
				col[0] = O.column("y", t);

				row[1] = 1.;
				col[1] = O.column("ycsb", t);

				row[2] = 1.;
				col[2] = O.column("yoff", t);

				add_constraintex(lp, 3, row, col, EQ, 1.);*/

				/*row[0] = 1.;
				col[0] = O.column("ycsu", t);

				row[1] = 1.;
				col[1] = O.column("yoff", t);

				add_constraintex(lp, 2, row, col, LE, 1.);*/

				/*row[0] = 1.;
				col[0] = O.column("y", t);

				row[1] = 1.;
				col[1] = O.column("yoff", t);

				add_constraintex(lp, 2, row, col, LE, 1.);*/

				/*row[0] = 1.;
				col[0] = O.column("ycsb", t);

				row[1] = 1.;
				col[1] = O.column("yoff", t);

				add_constraintex(lp, 2, row, col, LE, 1.);*/
			}
		}

        // ******************** Balance constraints *******************
		
		//Upper bound to TES charge state
		{
			REAL row[1];
			int col[1];
			for (int t = 0; t < nt; t++)
			{
				row[0] = 1.;
				col[0] = O.column("s", t);

				add_constraintex(lp, 1, row, col, LE, P["Eu"]);
			}
		}

        //Energy in, out, and stored in the TES system must balance.	completed	CHECK THIS
        {
            REAL row[7];
            int col[7];
				for (int t = 0; t < nt; t++)
				{
					int i = 0;

					row[i] = 1.;
					col[i++] = O.column("s", t);

					row[i] = -P["delta"];
					col[i++] = O.column("xr", t);

					row[i] = P["delta"] * P["Qc"];
					col[i++] = O.column("ycsu", t);

					row[i] = P["delta"] * P["Qb"];
					col[i++] = O.column("ycsb", t);

					row[i] = P["delta"];
					col[i++] = O.column("x", t);
#ifdef MOD_REC_STANDBY                
					row[i] = P["delta"] * P["Qrsb"];
					col[i++] = O.column("yrsb", t);
#endif
					if (t > 0)
					{
						row[i] = -1.;
						col[i++] = O.column("s", t - 1);

						add_constraintex(lp, i, row, col, EQ, 0.);
					}
					else
					{
						add_constraintex(lp, i, row, col, EQ, P["s0"]);  //initial storage state (kWh)
					}
				}
        }

		//There must be a sufficient charge level in the TES in time t-1 to ensure that the receiver can operate through its start-up period	completed
		{
			REAL row[8];
			int col[8];

			for (int t = 0; t < nt; t++)
			{
				int i = 0;
				double t_rec_startup = outputs.delta_rs.at(t) * P["delta"];
				//double large = 5.0*params.q_pb_max;
				double large = (P["Qu"] + P["Qb"]);

				if (t > 0)
				{
					row[i] = t_rec_startup * large;
					col[i++] = O.column("yrsu", t);

					row[i] = t_rec_startup * large;
					col[i++] = O.column("y", t - 1);

					row[i] = t_rec_startup * large;
					col[i++] = O.column("y", t);

					row[i] = t_rec_startup * large;
					col[i++] = O.column("ycsb", t - 1);

					row[i] = t_rec_startup * large;
					col[i++] = O.column("ycsb", t);

					row[i] = t_rec_startup;
					col[i++] = O.column("x", t);

					row[i] = t_rec_startup * P["Qb"];
					col[i++] = O.column("ycsb", t);

					row[i] = -1.;
					col[i++] = O.column("s", t - 1);

					add_constraintex(lp, i, row, col, LE, 3.0*t_rec_startup*large);
				}
			}
		}
        
        //Energy in storage must be within limits -- in Bill's model file, not in paper
        {
            REAL row[8];
            int col[8];

            for(int t=0; t<nt; t++)
            {               
				//max cycle thermal input in time periods where cycle operates and receiver is starting up
                //outputs.delta_rs.resize(nt);
				if (t < nt - 1)
				{
					/*double delta_rec_startup = min(1., max(params.e_rec_startup / max(outputs.q_sfavail_expected.at(t + 1)*P["delta"], 1.), params.dt_rec_startup / P["delta"]));
                    outputs.delta_rs.at(t) = delta_rec_startup;*/
					double t_rec_startup = outputs.delta_rs.at(t) * P["delta"];
					//double large = 5.0*params.q_pb_max;
					double large = (P["Qu"] + P["Qb"]);
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

        // Maximum gross electricity production constraint -- not in hybrid paper
        /*{
            REAL row[1];
            int col[1];

            for( int t = 0; t<nt; t++ )
            {
                row[0] = 1.;
                col[0] = O.column("wdot", t);

				add_constraintex(lp, 1, row, col, LE, outputs.f_pb_op_limit.at(t) * P["W_dot_cycle"]);
            }
        }*/

		// Maximum net electricity production constraint -- not in hybrid paper
		{
			REAL row[9];
			int col[9];

			for (int t = 0; t<nt; t++)
			{
                
                //check if cycle should be able to operate
                if( outputs.wnet_lim_min.at(t) > w_lim.at(t) )      // power cycle operation is impossible at t
                {
                    if(w_lim.at(t) > 0)
                        params.messages->add_message(C_csp_messages::NOTICE, "Power cycle operation not possible at time "+ util::to_string(t+1) + ": power limit below minimum operation");                    
                    w_lim.at(t) = 0.;
                    
                }

				if (w_lim.at(t) > 0.)	// Power cycle operation is possible
				{
					int i = 0;

					row[i] = 1.0-outputs.w_condf_expected.at(t);
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

		// ********************Photovoltaic Field Operations********************

		//Allow for curtailment by imposing only an upper limit on PV field generation		completed
		{
			REAL row[2];
			int col[2];

			for (int t = 0; t < nt; t++)
			{
				row[0] = 1.;
				col[0] = O.column("wdot_pv", t);

				row[1] = -outputs.w_dc_field.at(t);
				col[1] = O.column("ypv", t);

				add_constraintex(lp, 2, row, col, LE, 0.);
			}
		}

		//Excess is lost (difference btwn power produced by PV & power sent to battery > inverter-rated power)
		{
			REAL row[3];
			int col[3];

			for (int t = 0; t < nt; t++)
			{
				row[0] = 1.;
				col[0] = O.column("wdot_pv", t);

				row[1] = -1.;
				col[1] = O.column("wdot_pv+", t);

				row[2] = -P["Wi"];
				col[2] = O.column("ypv", t);

				add_constraintex(lp, 3, row, col, LE, 0.);
			}
		}

		//Power sent to charge the battery directly from the PV field is limited by PV field generation		completed
		{
			REAL row[2];
			int col[2];

			for (int t = 0; t < nt; t++)
			{
				row[0] = 1.;
				col[0] = O.column("wdot_pv+", t);

				row[1] = -1.;
				col[1] = O.column("wdot_pv", t);

				add_constraintex(lp, 2, row, col, LE, 0.);
			}
		}

		// ********************Battery Operations********************

		//Representation of nonlinear relationships between power, current, and voltage for charging
		{
			REAL row[3];
			int col[3];

			for (int t = 0; t < nt; t++)
			{
				row[0] = 1.;
				col[0] = O.column("wdot+", t);

				row[1] = 1.;
				col[1] = O.column("wdot_pv+", t);

				row[2] = -1.;
				col[2] = (O.column("i+", t) * O.column("vsoc", t));

				add_constraintex(lp, 3, row, col, EQ, 0.);
			}
		}

		//Representation of nonlinear relationships between power, current, and voltage for discharging
		{
			REAL row[2];
			int col[2];

			for (int t = 0; t < nt; t++)
			{
				row[0] = 1.;
				col[0] = O.column("wdot-", t);

				row[1] = -1.;
				col[1] = (O.column("i-", t) * O.column("vsoc", t));

				add_constraintex(lp, 2, row, col, EQ, 0.);
			}
		}

		//Update battery state of charge	complete
		{
			REAL row[4];
			int col[4];

			for (int t = 0; t < nt; t++)
			{
				row[0] = 1.;
				col[0] = O.column("bsoc", t);

				row[1] = -P["delta"] / P["Cb"];
				col[1] = O.column("i+", t);

				row[2] = P["delta"] / P["Cb"];
				col[2] = O.column("i-", t);

				if (t > 0) 
				{
					row[3] = -1.;
					col[3] = O.column("bsoc", t - 1);

					add_constraintex(lp, 4, row, col, EQ, 0.);
				}
				else
				{
					add_constraintex(lp, 3, row, col, EQ, P["bsoc0"]);
				}
			}
		}

		//Bounds for battery state of charge	complete
		{
			REAL row[1];
			int col[1];

			for (int t = 0; t < nt; t++)
			{
				row[0] = 1.;
				col[0] = O.column("bsoc", t);

				add_constraintex(lp, 1, row, col, GE, P["Sb_min"]);

				row[0] = 1.;
				col[0] = O.column("bsoc", t);

				add_constraintex(lp, 1, row, col, LE, P["Sb_max"]);
			}
		}

		//Battery voltage
		{
			REAL row[6];
			int col[6];

			for (int t = 0; t < nt; t++)
			{
				int i = 0;

				if (t > 0)
				{
					row[i] = 1.;
					col[i++] = O.column("vsoc", t);

					row[i] = -P["Av"];
					col[i++] = O.column("bsoc", t - 1);

					row[i] = -P["Bv"];
					col[i++] = O.column("y+", t);

					row[i] = -P["Bv"];
					col[i++] = O.column("y-", t);

					row[i] = -P["Iavg"] * P["Rint"];
					col[i++] = O.column("y+", t);

					row[i] = P["Iavg"] * P["Rint"];
					col[i++] = O.column("y-", t);

					add_constraintex(lp, i, row, col, EQ, 0.);
				}
			}
		}

		//Net power flow bounds		completed
		{
			REAL row[2];
			int col[2];

			for (int t = 0; t < nt; t++)
			{
				row[0] = 1.;
				col[0] = O.column("wdot-", t);

				row[1] = -P["Pb_min"];
				col[1] = O.column("y-", t);

				add_constraintex(lp, 2, row, col, GE, 0.);

				row[0] = 1.;
				col[0] = O.column("wdot-", t);

				row[1] = -P["Pb_max"];
				col[1] = O.column("y-", t);

				add_constraintex(lp, 2, row, col, LE, 0.);
			}
		}

		//Net power flow bounds (2)		completed
		{
			REAL row[3];
			int col[3];

			for (int t = 0; t < nt; t++)
			{
				row[0] = 1.;
				col[0] = O.column("wdot+", t);

				row[1] = 1.;
				col[1] = O.column("wdot_pv+", t);

				row[2] = -P["Pb_min"];
				col[2] = O.column("y+", t);

				add_constraintex(lp, 3, row, col, GE, 0.);

				row[0] = 1.;
				col[0] = O.column("wdot+", t);

				row[1] = 1.;
				col[1] = O.column("wdot_pv+", t);

				row[2] = -P["Pb_max"];
				col[2] = O.column("y+", t);

				add_constraintex(lp, 3, row, col, LE, 0.);
			}
		}

		//Restrict current flow		completed
		{
			REAL row[2];
			int col[2];

			for (int t = 0; t < nt; t++)
			{
				row[0] = 1.;
				col[0] = O.column("i-", t);

				if (t > 0)
				{
					row[1] = -P["Iu-"];
					col[1] = O.column("bsoc", t - 1);

					add_constraintex(lp, 2, row, col, LE, 0.);
				}
				else
				{
					add_constraintex(lp, 1, row, col, LE, P["Iu-"] * P["bsoc0"]);
				}
			}
		}

		//Restrict current flow (2)		completed
		{
			REAL row[2];
			int col[2];

			for (int t = 0; t < nt; t++)
			{
				row[0] = 1.;
				col[0] = O.column("i+", t);

				if (t > 0) {
					row[1] = P["Cb"] / P["delta"];
					col[1] = O.column("bsoc", t - 1);

					add_constraintex(lp, 2, row, col, LE, P["Cb"] / P["delta"]);
				}
				else
				{
					//add_constraintex(lp, 1, row, col, LE, (P["Cb"] - (P["Cb"] * P["bsoc0"])) / P["delta"]);
					add_constraintex(lp, 1, row, col, LE, P["Cb"] * ((1 - P["bsoc0"]) / P["delta"]));
				}
			}
		}

		//Restrict current flow (3)		completed
		{
			REAL row[2];
			int col[2];

			for (int t = 0; t < nt; t++)
			{
				row[0] = 1.;
				col[0] = O.column("i-", t);

				row[1] = -P["Il-"];
				col[1] = O.column("y-", t);

				add_constraintex(lp, 2, row, col, GE, 0.);

				row[0] = 1.;
				col[0] = O.column("i-", t);

				row[1] = -P["Iu-"];
				col[1] = O.column("y-", t);

				add_constraintex(lp, 2, row, col, LE, 0.);
			}
		}
        
		//Restrict current flow (4)		completed
		{
			REAL row[2];
			int col[2];

			for (int t = 0; t < nt; t++)
			{
				row[0] = 1.;
				col[0] = O.column("i+", t);

				row[1] = -P["Il+"];
				col[1] = O.column("y+", t);

				add_constraintex(lp, 2, row, col, GE, 0.);

				row[0] = 1.;
				col[0] = O.column("i+", t);

				row[1] = -P["Iu+"];
				col[1] = O.column("y+", t);

				add_constraintex(lp, 2, row, col, LE, 0.);
			}
		}

		//Battery cannot be charging and discharging simultaneously		completed
		{
			REAL row[2];
			int col[2];

			for (int t = 0; t < nt; t++)
			{
				row[0] = 1.;
				col[0] = O.column("y+", t);

				row[1] = 1.;
				col[1] = O.column("y-", t);

				add_constraintex(lp, 2, row, col, LE, 1.);
			}
		}

		//Battery cycle bound	completed
		{
			REAL row[1];
			int col[1];

			row[0] = 1.;
			col[0] = O.column("bc", 0);

			add_constraintex(lp, 1, row, col, GE, 0.);
		}

		//Measure battery cycle count	completed
		{
			REAL row[1];
			int col[1];

			double tadj = P["disp_time_weighting"];

			//rewritten starting line 2210
			/*for (int t = 0; t < nt; t++)
			{
				int i = 0;

				row[i] = 1.;
				col[i++] = O.column("bc", t);

				row[i] = (-P["delta"] * tadj) / P["Cb"];
				col[i++] = O.column("i+", t);

				if (t > 0)
				{
					row[i] = (-P["delta"] * tadj) / P["Cb"];
					col[i++] = O.column("i+", t)*O.column("bsoc", t-1);
				}
				add_constraintex(lp, i, row, col, GE, 0.);
			}*/
			double sum = 0.0;
			for (int t = 0; t < nt; t++)
			{
				if (t > 0)
				{
					sum += pow(tadj, t) * (O.column("i+", t) - (O.column("i+", t) * O.column("bsoc", t - 1)));
				}
				else
					sum += pow(tadj, t) * (O.column("i+", t) - (O.column("i+", t) * -P["bsoc0"]));
			}

			sum = sum * (P["delta"] / P["Cb"]);

			row[0] = 1.;
			col[0] = O.column("bc", 0);

			add_constraintex(lp, 1, row, col, GE, sum);
		}

		params.counter++;	
		
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
        //set_outputfile(lp, "c://users//dquintan//Documents//Work for Alex//Code//out.txt");
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

        //set_outputfile(lp, "c://users//dquintan//Documents//Work for Alex//Code//setup.txt");
        //print_lp(lp);

		set_outputfile(lp, "c://users//dquintan//Documents//Work for Alex//Code//solution.txt");
        print_solution(lp, 1);
        

        if(return_ok)
        {
            /*set_outputfile(lp, "C:\\Users\\mwagner\\Documents\\NREL\\OM Optimization\\cspopt\\software\\sdk\\scripts\\lpsolve\\setup.txt");
            print_lp(lp);*/
            set_outputfile(lp, "c://users//dquintan//Documents//Work for Alex//Code//solution.txt");
            print_solution(lp, 1);
            //throw;

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
			outputs.w_dc_field.resize(nt, 0.);	//added new output

            int ncols = get_Ncolumns(lp);

//            char name[15];
            REAL *vars = new REAL[ncols];
            get_variables(lp, vars);
//            int col;


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

                if(strcmp(root, "ycsb") == 0)  //Cycle standby
                {
                    outputs.pb_standby.at(t) = vars[ c-1 ] == 1.;
                }
                else if(strcmp(root, "ycsu") == 0)     //Cycle start up
                {
                    bool su = (fabs(1 - vars[ c-1 ]) < 0.001);
                    outputs.pb_operation.at(t) = outputs.pb_operation.at(t) || su;
                    outputs.q_pb_startup.at(t) = su ? P["Qc"] : 0.;
                }
                else if(strcmp(root, "y") == 0)     //Cycle operation
                {
                    outputs.pb_operation.at(t) = outputs.pb_operation.at(t) || ( fabs(1. - vars[ c-1 ]) < 0.001 );
                }
                else if(strcmp(root, "x") == 0)     //Cycle thermal energy consumption
                {
                    outputs.q_pb_target.at(t) = vars[ c-1 ];
                }
                else if(strcmp(root, "yrsu") == 0)     //Receiver start up
                {
                    outputs.rec_operation.at(t) = outputs.rec_operation.at(t) || (fabs(1 - vars[ c-1 ]) < 0.001);
                }
                else if(strcmp(root, "xrsu") == 0)
                {
                    outputs.q_rec_startup.at(t) = vars[ c-1 ];
                }
                else if(strcmp(root, "yr") == 0)
                {
                    outputs.rec_operation.at(t) = outputs.rec_operation.at(t) || (fabs(1 - vars[ c-1 ]) < 0.001);
                }
                else if(strcmp(root, "s") == 0)         //Thermal storage charge state
                {
                    outputs.tes_charge_expected.at(t) = vars[ c-1 ];
                }
                else if(strcmp(root, "xr") == 0)   //receiver production
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

bool strcompare(std::string a, std::string b)
{
    return util::lower_case(a) < util::lower_case(b);
};

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
        
        std::sort( keys.begin(), keys.end(), strcompare );

        for(size_t k=0; k<keys.size(); k++)
            fout << "param " << keys.at(k) << " := " << pars[keys.at(k)] << ";\n";

        fout << "# --- indexed parameters ---\n";
        fout << "param Qin := \n";
        for(int t=0; t<nt; t++)
            fout << t+1 << "\t" << outputs.q_sfavail_expected.at(t) << "\n";
        fout << ";\n\n";

        fout << "param P := \n";
        for(int t=0; t<nt; t++)
            fout << t+1 << "\t" << price_signal.at(t) << "\n";
        fout << ";\n\n";

        fout << "param etaamb := \n";   //power block ambient adjustment
        for(int t=0; t<nt; t++)
            fout << t+1 << "\t" << outputs.eta_pb_expected.at(t) << "\n";
        fout << ";\n\n";

        //net power limit
        fout << "param Wdotnet := \n";
        for(int t=0; t<nt; t++)
            fout << t+1 << "\t" << w_lim.at(t) << "\n";
        fout << ";\n\n";
        
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
	outputs.w_dc_field.resize(nt, 0.); //new output
    
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
	svals = util::split(F.at(12), ",");
	for (int i = 0; i < nt; i++)
		util::to_double(svals.at(i), &outputs.w_dc_field.at(i));

    return true;
}


// ----------------------------------------
// ----------------------------------------


optimization_vars::optimization_vars()
{
    current_mem_pos = 0;
    alloc_mem_size = 0;
}
void optimization_vars::add_var(const string &vname, int var_type /* VAR_TYPE enum */, int var_dim /* VAR_DIM enum */, int var_dim_size, REAL lobo, REAL upbo)
{
    if(var_dim == VAR_DIM::DIM_T2)
        add_var(vname, var_type, VAR_DIM::DIM_NT, var_dim_size, var_dim_size, lobo, upbo);
    else
        add_var(vname, var_type, var_dim, var_dim_size, 1, lobo, upbo);

}

void optimization_vars::add_var(const string &vname, int var_type /* VAR_TYPE enum */, int var_dim /* VAR_DIM enum */, int var_dim_size, int var_dim_size2, REAL lobo, REAL upbo)
{
    var_objects.push_back( optimization_vars::opt_var() );
    optimization_vars::opt_var *v = &var_objects.back();
    v->name = vname;
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


int optimization_vars::column(const string &varname, int ind)
{
    return var_by_name[varname]->ind_start + ind +1;
}

int optimization_vars::column(const string &varname, int ind1, int ind2)
{
    opt_var *v = var_by_name[ string(varname) ];
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

optimization_vars::opt_var *optimization_vars::get_var(const string &varname)
{
    return var_by_name[ varname ];
}

optimization_vars::opt_var *optimization_vars::get_var(int varindex)
{
    return &var_objects[varindex];
}
