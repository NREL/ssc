#include "csp_dispatch.h"
#include "lp_lib.h" 
#include <fstream>
#include <sstream>

#define SOS_NONE
//#define SOS_SEQUENCE
//#define SOS_MANUAL
//#define SOS_LPSOLVE

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
    params.sf_effadj = 1.;
    params.info_time = 0.;
    
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
    outputs.eta_sf_expected.clear();
    outputs.q_sfavail_expected.clear();
    outputs.q_sf_expected.clear();
    outputs.tes_charge_expected.clear();
    outputs.q_pb_startup.clear();
    outputs.q_rec_startup.clear();
}

bool csp_dispatch_opt::check_setup(int nstep)
{
    //check parameters and inputs to make sure everything has been set up correctly
    bool ok = true;

    if( price_signal.size() < nstep )   return false;

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

bool csp_dispatch_opt::predict_performance(int step_start, int nstep)
{
    //Step number - 1-based index for first hour of the year.

    //save step count
    m_nstep_opt = nstep;

    //Predict performance out nstep values. 
    clear_output_arrays();

    if(! check_setup(m_nstep_opt) )
        throw C_csp_exception("Dispatch optimization precheck failed.");

    //create the sim info
    C_csp_solver_sim_info simloc;    // = *params.siminfo;
    simloc.m_step = params.siminfo->m_step;


    double Asf = params.col_rec->get_collector_area();

    for(int i=0; i<m_nstep_opt; i++)
    {
        
        //check to see if we're past the end of the weather file
        if( simloc.m_time > 8760*3600)
            return false;

        //jump to the current step
        m_weather.read_time_step( step_start+i, simloc );
        //m_weather.timestep_call(simloc);

        //get DNI
        double dni = m_weather.ms_outputs.m_beam;

        //get optical efficiency
        double opt_eff = params.col_rec->calculate_optical_efficiency(m_weather.ms_outputs, simloc);

        double q_inc = Asf * opt_eff * dni * 1.e-3; //kW

        //get thermal efficiency
        double therm_eff = params.col_rec->calculate_thermal_efficiency_approx(m_weather.ms_outputs, q_inc*0.001);
        therm_eff *= params.sf_effadj;
        outputs.eta_sf_expected.push_back(therm_eff);

        //store the predicted field energy output
        outputs.q_sfavail_expected.push_back( q_inc * therm_eff * simloc.m_step/3600.);

        //store the power cycle efficiency
        double cycle_eff = params.eff_table_Tdb.interpolate( m_weather.ms_outputs.m_tdry );
        outputs.eta_pb_expected.push_back( cycle_eff );

        simloc.m_time+= simloc.m_step;
        m_weather.converged();
    }

    //reset the weather data reader
    //m_weather.jump_to_timestep(step_start, simloc);
    
    return true;
}


bool csp_dispatch_opt::optimize()
{
    /* 
    Formulate the optimization problem for dispatch generation. We are trying to maximize revenue subject to inventory
    constraints.
    
    
    Variables
    -------------------------------------------------------------
    Continuous
    -------------------------------------------------------------
    x^r_t       kWt     Power delivered by the receiver at time t
    x^rsu_t     kWt     Power used by the reciever for start up
    u^rsu_t     kWt     Receiver accumulated start-up thermal power at time t
    u^csu_t     kWt     Cycle accumulated start-up thermal power at time t
    x_t         kWt	    Cycle thermal power consumption at time t 
    s_t         kWht    TES reserve quantity at time t (auxiliary variable) 
    wdot_t      kWe     Electrical power production at time t
    z_nt        kWe     Variables for piecewise approximation of cycle efficiency
    -------------------------------------------------------------
    Binary
    -------------------------------------------------------------
    y^r_t             1 if receiver is generating ``usable'' thermal power at time t; 0 otherwise 
    y^rsu_t           1 if receiver is starting up at time t; 0 otherwise 
    y^csu_t           1 if cycle is starting up at time t; 0 otherwise
    y^csb_t           1 if cycle is in standby mode at time t; 0 otherwise
    y_t               1 if cycle is generating electric power at time t; 0 otherwise
    zhat_n-1 t        1 if piece n of the efficiency function is active at time t; 0 otherwise
    -------------------------------------------------------------
    */
    lprec *lp;
    int ret = 0;


    try{

        //Calculate the number of variables
        int nt = m_nstep_opt;
        int nz = (int)params.eff_table_load.get_size();

        //set up the variable structure
        optimization_vars O;
        O.add_var("x^r", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0. );
        O.add_var("y^r", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("x^rsu", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0. );
        O.add_var("y^rsu", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("u^rsu", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0. );
        
        O.add_var("x", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0.);
        O.add_var("y", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("y^csu", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("u^csu", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0. );
        O.add_var("y^csb", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_T, nt);
        O.add_var("s", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0. );

#ifndef SOS_NONE
        O.add_var("wdot", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_T, nt, 0. );
#endif
#ifdef SOS_LPSOLVE
        O.add_var("z", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_NT, nz, nt, 0., 1.);
#endif
#ifdef SOS_MANUAL
        O.add_var("z", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_NT, nz, nt, 0., 1.);
        O.add_var("zhat", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_NT, nz-1, nt );
#endif
#ifdef SOS_SEQUENCE
        O.add_var("z", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_NT, nz-1, nt, 0.);
        O.add_var("yz", optimization_vars::VAR_TYPE::BINARY_T, optimization_vars::VAR_DIM::DIM_NT, nz-1, nt );
#endif

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

        /* 
        --------------------------------------------------------------------------------
        set up the objective function first (per lpsolve guidance)
        --------------------------------------------------------------------------------
        */
        {
            int *col = new int[3*nt];
            REAL *row = new REAL[3*nt];

            for(int t=0; t<nt; t++)
            {
#ifdef SOS_NONE
                col[t] = O.column("x", t);
#else
                col[t] = O.column("wdot", t);
#endif
                row[t] = price_signal.at(t) * outputs.eta_pb_expected.at(t); // * ((double)nt-0.25*t)/(double)nt;

                col[nt+t] = O.column("x^r", t);
                row[nt+t] = -price_signal.at(t) * params.w_rec_pump;

                col[2*nt+t] = O.column("x^rsu", t);
                row[2*nt+t] = -price_signal.at(t) * params.w_rec_pump;
            }

            set_obj_fnex(lp, 3*nt, row, col);
        
            delete [] col;
            delete [] row;
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

        //******************** Receiver constraints *******************
        //double dq_rsu = params.e_rec_startup / ceil(params.dt_rec_startup/params.dt) / params.dt;
        double dq_rsu = params.e_rec_startup / params.dt_rec_startup;

        {
            REAL row[3];
            int col[3];

            for(int t=0; t<nt; t++)
            {

                //Receiver startup inventory
                row[0] = 1.;
                col[0] = O.column("u^rsu", t);

                /*row[1] = -dq_rsu;
                col[1] = O.column("y^rsu", t);*/
                row[1] = -1.;
                col[1] = O.column("x^rsu", t);

                if(t>0)
                {
                    row[2] = -1.;
                    col[2] = O.column("u^rsu", t-1);

                    add_constraintex(lp, 3, row, col, LE, 0);
                }
                else
                {
                    add_constraintex(lp, 2, row, col, LE, 0.);
                }

                //-----

                //Receiver startup energy consumption
                row[0] = 1.;
                col[0] = O.column("x^rsu", t);

                row[1] = -dq_rsu;
                col[1] = O.column("y^rsu", t);

                add_constraintex(lp, 2, row, col, LE, 0.);

                //Receiver startup only during solar positive periods
                row[0] = 1.;
                col[0] = O.column("y^rsu", t);

                add_constraintex(lp, 1, row, col, LE, min(1.e6*outputs.q_sfavail_expected.at(t), 1.01) );
                
                //-----

                //inventory nonzero
                row[0] = 1.;
                col[0] = O.column("u^rsu", t);

                //row[1] = -1.e6;
                row[1] = -params.e_rec_startup;
                col[1] = O.column("y^rsu", t);

                add_constraintex(lp, 2, row, col, LE, 0.);

                //Receiver operation allowed when:
                row[0] = 1.;
                col[0] = O.column("y^r", t);
                
                row[1] = -1.01/params.e_rec_startup; 
                col[1] = O.column("u^rsu", t);

                if(t>0)
                {
                    row[2] = -1.;
                    col[2] = O.column("y^r", t-1);

                    add_constraintex(lp, 3, row, col, LE, 0.); 
                }
                else
                {
                    add_constraintex(lp, 2, row, col, LE, (params.is_rec_operating0 ? 1. : 0.) );
                }

                //Receiver consumption limit
                row[0] = 1.;
                col[0] = O.column("x^r", t);

                /*row[1] = dq_rsu;
                col[1] = O.column("y^rsu", t);*/
                row[1] = 1.;
                col[1] = O.column("x^rsu", t);
                
                add_constraintex(lp, 2, row, col, LE, outputs.q_sfavail_expected.at(t));

                //Receiver operation mode requirement
                row[0] = 1.;
                col[0] = O.column("x^r", t);

                row[1] = -outputs.q_sfavail_expected.at(t);
                col[1] = O.column("y^r", t);

                add_constraintex(lp, 2, row, col, LE, 0.);

                //Receiver minimum operation requirement
                row[0] = 1.;
                col[0] = O.column("x^r", t);

                row[1] = -params.q_rec_min;
                col[1] = O.column("y^r", t);

                add_constraintex(lp, 2, row, col, GE, 0.);

                //Receiver startup can't be enabled after a time step where the Receiver was operating
                if(t>0)
                {
                    row[0] = 1.;
                    col[0] = O.column("y^rsu", t);

                    row[1] = 1.;
                    col[1] = O.column("y^r", t-1);

                    add_constraintex(lp, 2, row, col, LE, 1.);
                }

                //Receiver can't continue operating when no energy is available
                row[0] = 1.;
                col[0] = O.column("y^r", t);

                add_constraintex(lp, 1, row, col, LE, min(1.e6*outputs.q_sfavail_expected.at(t), 1.01) );  //if any measurable energy, y^r can be 1

            }
        }

        
        //******************** Power cycle constraints *******************
        
        double dq_csu = params.e_pb_startup_cold / ceil(params.dt_pb_startup_cold/params.dt) / params.dt;
        
        {
            REAL row[4];
            int col[4];


            for(int t=0; t<nt; t++)
            {

                //Startup Inventory balance
                row[0] = 1.;
                col[0] = O.column("u^csu", t);
                
                row[1] = -dq_csu;
                col[1] = O.column("y^csu", t);

                if(t>0)
                {
                    row[2] = -1.;
                    col[2] = O.column("u^csu", t-1);

                    add_constraintex(lp, 3, row, col, LE, 0.);
                }
                else
                {
                    add_constraintex(lp, 2, row, col, LE, 0.);
                }

                //Inventory nonzero
                row[0] = 1.;
                col[0] = O.column("u^csu", t);

                row[1] = -1.e6;
                col[1] = O.column("y^csu", t);

                add_constraintex(lp, 2, row, col, LE, 0.);

                //Cycle operation allowed when:
                row[0] = 1.;
                col[0] = O.column("y", t);
                
                row[1] = -1.01/params.e_pb_startup_cold; 
                col[1] = O.column("u^csu", t);

                if(t>0)
                {
                    row[2] = -1.;
                    col[2] = O.column("y", t-1);

                    row[3] = -1.;
                    col[3] = O.column("y^csb", t-1);

                    add_constraintex(lp, 4, row, col, LE, 0.); 
                }
                else
                {
                    add_constraintex(lp, 2, row, col, LE, (params.is_pb_operating0 ? 1. : 0.) + (params.is_pb_standby0 ? 1. : 0.) );
                }

                //Cycle consumption limit
                row[0] = 1.;
                col[0] = O.column("x", t);

                row[1] = dq_csu;
                col[1] = O.column("y^csu", t);
                
                add_constraintex(lp, 2, row, col, LE, params.q_pb_max);

                //cycle operation mode requirement
                row[0] = 1.;
                col[0] = O.column("x", t);

                row[1] = -params.q_pb_max;
                col[1] = O.column("y", t);

                add_constraintex(lp, 2, row, col, LE, 0.);

                //Minimum cycle energy contribution
                row[0] = 1.;
                col[0] = O.column("x", t);

                row[1] = -params.q_pb_min;
                col[1] = O.column("y", t);

               /* row[2] = dq_csu;
                col[2] = O.column("y^csu", t);*/

                add_constraintex(lp, 2, row, col, GE, 0);

                //cycle startup can't be enabled after a time step where the cycle was operating
                if(t>0)
                {
                    row[0] = 1.;
                    col[0] = O.column("y^csu", t);

                    row[1] = 1.;
                    col[1] = O.column("y", t-1);

                    add_constraintex(lp, 2, row, col, LE, 1.);
                }


                //Standby mode entry
                row[0] = 1.;
                col[0] = O.column("y^csb", t);
                

                if(t>0)
                {
                    row[1] = -1.;
                    col[1] = O.column("y", t-1);

                    row[2] = -1.;
                    col[2] = O.column("y^csb", t-1);

                    add_constraintex(lp, 3, row, col, LE, 0);
                }
                else
                {
                    add_constraintex(lp, 1, row, col, LE, (params.is_pb_standby0 ? 1 : 0) + (params.is_pb_standby0 ? 1 : 0));
                }

                //some modes can't coincide
                row[0] = 1.;
                col[0] = O.column("y^csu", t);
                row[1] = 1.;
                col[1] = O.column("y^csb", t);    

                add_constraintex(lp, 2, row, col, LE, 1);   

                row[0] = 1.;
                col[0] = O.column("y", t);
                row[1] = 1.;
                col[1] = O.column("y^csb", t);    

                add_constraintex(lp, 2, row, col, LE, 1);   
            }
        }

#ifndef SOS_NONE
        // power cycle efficiency
        {

            REAL *row = new REAL[nz+1];
            int *col = new int[nz+1];

            for(int t=0; t<nt; t++)
            {
#ifndef SOS_SEQUENCE
                //thermal input is equal to the weighted sum of the z's
                row[0] = 1.;
                col[0] = O.column("x", t);

                for(int z=0; z<nz; z++)
                {
                    row[z+1] = -params.eff_table_load.get_point_x(z);
                    col[z+1] = O.column("z", z, t);
                }

                add_constraintex(lp, nz+1, row, col, EQ, 0.);

                //power equal to the sum of auxiliary variables "z"
                row[0] = 1.;
                col[0] = O.column("wdot", t);

                for(int z=0; z<nz; z++)
                {
                    double eff, x;
                    params.eff_table_load.get_point(z, x, eff);
                    row[z+1] = - x * eff;
                    col[z+1] = O.column("z", z, t);
                }

                add_constraintex(lp, nz+1, row, col, EQ, 0.);
#endif 

#ifdef SOS_SEQUENCE
                //power is the summation of power segments
                row[0] = 1.;
                col[0] = O.column("wdot", t);

                for(int z=0; z<nz-1; z++)
                {
                    row[z+1] = -params.eff_table_load.get_point_eff(z);
                    col[z+1] = O.column("z", z, t);
                }

                add_constraintex(lp, nz, row, col, EQ, 0.);

                //thermal input is equal to the sum of z's
                row[0] = 1.;
                col[0] = O.column("x", t);

                for(int z=0; z<nz-1; z++)
                {
                    row[z+1] = -1.;
                    col[z+1] = O.column("z", z, t);
                }

                add_constraintex(lp, nz, row, col, EQ, 0.);

                //binary sequence
                for(int z=0; z<nz-2; z++)
                {
                    row[0] = 1.;
                    col[0] = O.column("yz", z+1, t);

                    row[1] = -1.;
                    col[1] = O.column("yz", z, t);

                    add_constraintex(lp, 2, row, col, LE, 0.);
                }

                //power in a segment controlled by corresponding binary
                for(int z=0; z<nz-1; z++)
                {
                    row[0] = 1.;
                    col[0] = O.column("z", z, t);

                    row[1] = -(params.eff_table_load.get_point_x(z+1) - params.eff_table_load.get_point_x(z));
                    col[1] = O.column("yz", z, t);

                    add_constraintex(lp, 2, row, col, LE, 0.);
                }

                //Binary can't be 1 unless previous binary quota is satisfied
                for(int z=1; z<nz-1; z++)
                {
                    row[0] = 1.;
                    col[0] = O.column("yz", z, t);

                    row[1] = -1./(params.eff_table_load.get_point_x(z) - params.eff_table_load.get_point_x(z-1) - 0.1);
                    col[1] = O.column("z", z-1, t);

                    add_constraintex(lp, 2, row, col, LE, 0.);
                }


#endif

                //-------------------------
#ifdef SOS_LPSOLVE
                string s = "sos-constr-" + to_string(t);
                char sname[20];
                sprintf(sname, "sos-%d", t); 

                for(int z=0; z<nz; z++)
                {
                    col[z] = O.column("z", z, t);
                    row[z] = params.eff_table_load.get_point_x(z);
                }

                add_SOS(lp, sname, 2, 1, nz, col, row);
#endif
                //-------------------------
#ifdef SOS_MANUAL
                //Enforce piecewise limit
                //-- z==0
                row[0] = 1.;
                col[0] = O.column("z", 0, t);

                row[1] = -1.;
                col[1] = O.column("zhat", 0, t);

                add_constraintex(lp, 2, row, col, LE, 0.);

                //-- z==nz-1
                row[0] = 1.;
                col[0] = O.column("z", nz-1, t);

                row[1] = -1.;
                col[1] = O.column("zhat", nz-2, t);

                add_constraintex(lp, 2, row, col, LE, 0.);

                //-- others
                for(int z=1; z<nz-1; z++)
                {
                    row[0] = 1.;
                    col[0] = O.column("z", z, t);

                    row[1] = -1.;
                    col[1] = O.column("zhat", z-1, t);

                    row[2] = -1.;
                    col[2] = O.column("zhat", z, t);

                    add_constraintex(lp, 3, row, col, LE, 0.);
                }
               
                //piecewise summation
                for(int z=0; z<nz; z++)
                {
                    row[z] = 1.;
                    col[z] = O.column("z", z, t);
                }

                add_constraintex(lp, nz, row, col, EQ, 1.);

                //piecewise binary
                for(int z=0; z<nz-1; z++)
                {
                    row[z] = 1.;
                    col[z] = O.column("zhat", z, t);
                }

                add_constraintex(lp, nz-1, row, col, LE, 1.);
#endif
            }
            
            delete [] row;
            delete [] col;
        }
#endif
        //******************** Balance constraints *******************
        //Energy in, out, and stored in the TES system must balance.
        {
            REAL row[7];
            int col[7];

            for(int t=0; t<nt; t++)
            {
                row[0] = params.dt;
                row[1] = -params.dt*dq_csu;
                row[2] = -params.dt*params.q_pb_standby; 
                row[3] = -params.dt;
                row[4] = -1.;
                row[5] = 1.;
            
                col[0] = O.column("x^r", t);
                col[1] = O.column("y^csu", t);
                col[2] = O.column("y^csb", t);
                col[3] = O.column("x", t);
                col[4] = O.column("s", t);

                
                if(t>0)
                {
                    col[5] = O.column("s", t-1);
                    add_constraintex(lp, 6, row, col, EQ, 0.);
                }
                else
                {
                    add_constraintex(lp, 5, row, col, EQ, -params.e_tes_init);  //initial storage state (kWh)
                }
            }
        }
        
        //Energy in storage must be within limits
        {
            REAL row[5];
            int col[5];

            for(int t=0; t<nt; t++)
            {
                
                row[0] = 1.;
                col[0] = O.column("s", t);

                add_constraintex(lp, 1, row, col, LE, params.e_tes_max);
                //add_constraintex(lp, 1, &row, &col, GE, params.e_tes_min);

                //min charge state in time periods where cycle operates and receiver is starting up
                if(t < nt-1)
                {
                    double smin = ( 1. - max(outputs.q_sfavail_expected.at(t+1) - params.e_rec_startup/params.dt, 0.) / max(outputs.q_sfavail_expected.at(t+1), 1.e-6) ) * params.q_pb_max * params.dt;



                    row[0] = 1./smin;
                    col[0] = O.column("s", t);

                    row[1] = -1;
                    col[1] = O.column("y", t+1);

                    row[2] = -1;
                    col[2] = O.column("y^rsu", t+1);

                    row[3] = -1;
                    col[3] = O.column("y", t);

                    row[4] = -1;
                    col[4] = O.column("y^csb", t);

                    add_constraintex(lp, 5, row, col, GE, -2);
                }

            }
        }

        //Set problem to maximize
        set_maxim(lp);

        //reset the row mode
        set_add_rowmode(lp, FALSE);

        //set the log function
        solver_params.reset();
        
        //put_logfunc(lp, opt_logfunction, (void*)(&solver_params));
        put_msgfunc(lp, opt_iter_function, (void*)(&solver_params), MSG_ITERATION | MSG_MILPBETTER | MSG_MILPFEASIBLE);
        put_abortfunc(lp, opt_abortfunction, (void*)(&solver_params));

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

        */

        //presolve
        if(solver_params.presolve_type > 0)
            set_presolve(lp, solver_params.presolve_type, get_presolveloops(lp));
        else
            //set_presolve(lp, PRESOLVE_ROWS + PRESOLVE_COLS + PRESOLVE_REDUCEMIP + PRESOLVE_ELIMEQ2 /*+ PRESOLVE_SOS*/, get_presolveloops(lp) );       //original
            set_presolve(lp, PRESOLVE_ROWS + PRESOLVE_COLS + PRESOLVE_ELIMEQ2 + PRESOLVE_PROBEFIX /*+ PRESOLVE_IMPLIEDSLK*/, get_presolveloops(lp) );   //independent optimization
            //set_presolve(lp, PRESOLVE_IMPLIEDFREE, get_presolveloops(lp) );     //genetic algorithm

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
            //set_bb_rule(lp, NODE_PSEUDOCOSTSELECT + NODE_RCOSTFIXING);        //original
            //set_bb_rule(lp, NODE_PSEUDORATIOSELECT + NODE_BREADTHFIRSTMODE);  //original 2
            set_bb_rule(lp, NODE_PSEUDOCOSTSELECT + NODE_RANDOMIZEMODE);    //independent optimization
            //set_bb_rule(lp, NODE_GREEDYMODE + NODE_PSEUDOCOSTMODE + NODE_DEPTHFIRSTMODE + NODE_RANDOMIZEMODE + NODE_DYNAMICMODE );  //genetic algorithm
 
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

        //set_outputfile(lp, "C:\\Users\\mwagner\\Documents\\NREL\\SAM\\Dev\\ssc\\branches\\CSP_dev\\build_vc2013\\x64\\setup.txt");
        //print_lp(lp);

        ////print_lp(lp); //, "c://users//mwagner//documents//dropbox//nrel//formulation//dump.txt");
        //set_outputfile(lp, "C:\\Users\\mwagner\\Documents\\NREL\\SAM\\Dev\\ssc\\branches\\CSP_dev\\build_vc2013\\x64\\solution.txt");
        //print_solution(lp, 1);
        //int x=0;
        
/*
        //write out a data file
        ofstream fout("C://Users//mwagner//Documents//Dropbox//NREL//Formulation//f.dat");

        fout << "#data file\n\n";

        fout << "param nt := " << nt << ";\n";
        fout << "param Eu := " << params.e_tes_max << ";\n";
        fout << "param El := " << params.e_tes_min << ";\n";
        fout << "param Er := " << params.e_rec_startup << ";\n";
        fout << "param Ec := " << params.e_pb_startup_cold << ";\n";
        fout << "param Esinit := " << params.e_tes_init << ";\n";
        fout << "param Cu := " << params.q_pb_max << ";\n";
        fout << "param Cl := " << params.q_pb_min << ";\n";
        fout << "param Qr := " << dq_rsu << ";\n";
        fout << "param Qc := " << dq_csu << ";\n";
        fout << "param Qb := " << params.q_pb_standby << ";\n";
        fout << "param is_rec_operating := " << (params.is_rec_operating0 ? 1 : 0) << ";\n";
        fout << "param is_pc_operating := " << (params.is_pb_operating0 ? 1 : 0) << ";\n";
        fout << "param is_pc_standby := " << (params.is_pb_standby0 ? 1 : 0) << ";\n";

        fout << "\nparam Q := \n";
        for(int t=0; t<nt; t++)
            fout << t+1 << "\t" << esf_predict.at(t) << "\n";
        fout << ";\n\n";

        fout << "param P := \n";
        for(int t=0; t<nt; t++)
            fout << t+1 << "\t" << price_signal.at(t) << "\n";
        fout << ";\n\n";

        fout << "param eta := \n";
        for(int t=0; t<nt; t++)
            fout << t+1 << "\t" << eta_pb_predict.at(t) << "\n";
        fout << ";";

        fout.close();*/

        //write the log file
        /*ofstream flog("C://Users//mwagner//Documents//Dropbox//NREL//Formulation//f.log");
        flog << solver_params.log_message;
        flog.close();*/



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

            int ncols = get_Ncolumns(lp);

            char name[15];
            REAL *vars = new REAL[ncols];
            get_variables(lp, vars);
            int col;


            for(int c=0; c<ncols; c++)
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

                if(strcmp(root, "y^csb") == 0)  //Cycle standby
                {
                    outputs.pb_standby.at(t) = vars[ c-1 ] == 1.;
                }
                else if(strcmp(root, "y^csu") == 0)     //Cycle start up
                {
                    bool su = (fabs(1 - vars[ c-1 ]) < 0.001);
                    outputs.pb_operation.at(t) = outputs.pb_operation.at(t) || su;
                    outputs.q_pb_startup.at(t) = su ? dq_csu : 0.;
                }
                else if(strcmp(root, "y") == 0)     //Cycle operation
                {
                    outputs.pb_operation.at(t) = outputs.pb_operation.at(t) || ( fabs(1. - vars[ c-1 ]) < 0.001 );
                }
                else if(strcmp(root, "x") == 0)     //Cycle thermal energy consumption
                {
                    outputs.q_pb_target.at(t) = vars[ c-1 ];
                }
                else if(strcmp(root, "y^rsu") == 0)     //Receiver start up
                {
                    outputs.rec_operation.at(t) = outputs.rec_operation.at(t) || (fabs(1 - vars[ c-1 ]) < 0.001);
                }
                else if(strcmp(root, "x^rsu") == 0)
                {
                    outputs.q_rec_startup.at(t) = vars[ c-1 ];
                }
                else if(strcmp(root, "y^r") == 0)
                {
                    outputs.rec_operation.at(t) = outputs.rec_operation.at(t) || (fabs(1 - vars[ c-1 ]) < 0.001);
                }
                else if(strcmp(root, "s") == 0)         //Thermal storage charge state
                {
                    outputs.tes_charge_expected.at(t) = vars[ c-1 ];
                }
                else if(strcmp(root, "x^r") == 0)   //receiver production
                {
                    outputs.q_sf_expected.at(t) = vars[ c-1 ];
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

//csp_dispatch_opt::s_outputs *csp_dispatch_opt::get_step_vars(int step)
//{
//    //Get optimized variable states for this timestep
//
//    outputs.e_gen = e_gen_opt.at(step);
//    outputs.e_tes = e_tes_opt.at(step);
//    outputs.pb_standby = is_pb_sb.at(step);
//    outputs.pb_operation = is_pb_su.at(step);
//    outputs.rec_operation = is_rec_su.at(step);
//
//    return &outputs;
//}


// ----------------------------------------


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
    v->name = (string)vname;
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
    default:
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

optimization_vars::opt_var *optimization_vars::get_var(char *varname)
{
    return var_by_name[ varname ];
}

optimization_vars::opt_var *optimization_vars::get_var(int varindex)
{
    return &var_objects[varindex];
}