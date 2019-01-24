/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (�Alliance�) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
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
*  the underlying software originally provided by Alliance as �System Advisor Model� or �SAM�. Except
*  to comply with the foregoing, the terms �System Advisor Model�, �SAM�, or any confusingly similar
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

#ifndef __csp_solver_two_tank_tes_
#define __csp_solver_two_tank_tes_

#include "csp_solver_core.h"
#include "csp_solver_util.h"

#include "sam_csp_util.h"

class C_heat_exchanger
{
private:
	HTFProperties mc_field_htfProps;
	HTFProperties mc_store_htfProps;

	double m_m_dot_des_ave;		    //[kg/s] Average (field and storage sides) mass flow rate
	double m_eff_des;				//[-] Heat exchanger effectiveness
	double m_UA_des;				//[W/K] Heat exchanger conductance

    // Stored values from previous timestep
    double m_T_hot_field_prev;		//[K] Hotter temperature on field side (opposite tank side)
    double m_T_cold_field_prev;		//[K] Colder temperature on field side (opposite tank side)
    double m_m_dot_field_prev;      //[kg/s] Mass flow rate on field side (opposite tank side)
    double m_T_hot_tes_prev;		//[K] Hotter temperature on TES side (tank side)
    double m_T_cold_tes_prev;       //[K] Colder temperature on TES side (tank side)
    double m_m_dot_tes_prev;        //[kg/s] Mass flow rate on TES side (tank side)

    // Calculated values for current timestep
    double m_T_hot_field_calc;		//[K] Hotter temperature on field side (opposite tank side)
    double m_T_cold_field_calc;		//[K] Colder temperature on field side (opposite tank side)
    double m_m_dot_field_calc;      //[kg/s] Mass flow rate on field side (opposite tank side)
    double m_T_hot_tes_calc;		//[K] Hotter temperature on TES side (tank side)
    double m_T_cold_tes_calc;       //[K] Colder temperature on TES side (tank side)
    double m_m_dot_tes_calc;        //[kg/s] Mass flow rate on TES side (tank side)

	void hx_performance(bool is_hot_side_mdot, bool is_storage_side, double T_hot_in, double m_dot_known, double T_cold_in,
		double &eff, double &T_hot_out, double &T_cold_out, double &q_trans, double &m_dot_solved);

public:

	C_heat_exchanger();

	void init(const HTFProperties &fluid_field, const HTFProperties &fluid_store, double q_transfer_des,
		double dt_des, double T_h_in_des, double T_h_out_des);

	void hx_charge_mdot_tes(double T_cold_tes, double m_dot_tes, double T_hot_field, 
		double &eff, double &T_hot_tes, double &T_cold_field, double &q_trans, double &m_dot_field);

	void hx_discharge_mdot_tes(double T_hot_tes, double m_dot_tes, double T_cold_field,
		double &eff, double &T_cold_tes, double &T_hot_field, double &q_trans, double &m_dot_field);

	void hx_charge_mdot_field(double T_hot_field, double m_dot_field, double T_cold_tes,
		double &eff, double &T_cold_field, double &T_hot_tes, double &q_trans, double &m_dot_tes);

	void hx_discharge_mdot_field(double T_cold_field, double m_dot_field, double T_hot_tes,
		double &eff, double &T_hot_field, double &T_cold_tes, double &q_trans, double &m_dot_tes);

    void converged();
};

class C_storage_tank
{
private:
	HTFProperties mc_htf;

	double m_V_total;			//[m^3] Total volume for *one temperature* tank
	double m_V_active;			//[m^3] active volume of *one temperature* tank (either cold or hot)
	double m_V_inactive;		//[m^3] INactive volume of *one temperature* tank (either cold or hot)
	double m_UA;				//[W/K] Tank loss conductance

	double m_T_htr;				//[K] Tank heater set point
	double m_max_q_htr;			//[MWt] Max tank heater capacity

	// Stored values from end of previous timestep
	double m_V_prev;		    //[m^3] Volume of storage fluid in tank
	double m_T_prev;		    //[K] Temperature of storage fluid in tank
	double m_m_prev;		    //[kg] Mass of storage fluid in tank

	// Calculated values for current timestep
	double m_V_calc;		    //[m^3] Volume of storage fluid in tank
	double m_T_calc;		    //[K] Temperature of storage fluid in tank
	double m_m_calc;		    //[kg] Mass of storage fluid in tank

public:

	C_storage_tank();

	double calc_mass_at_prev();

    double calc_cp_at_prev();

    double calc_enth_at_prev();

    double get_m_UA();

	double get_m_T_prev();

	double get_m_T_calc();

	void init(HTFProperties htf_class_in, double V_tank_one_temp, double h_tank, double h_min, double u_tank, 
		double tank_pairs, double T_htr, double max_q_htr, double V_ini, double T_ini);

	double m_dot_available(double f_unavail, double timestep);	

	void energy_balance(double timestep /*s*/, double m_dot_in, double m_dot_out, double T_in /*K*/, double T_amb /*K*/, 
		double &T_ave /*K*/, double &q_heater /*MW*/, double &q_dot_loss /*MW*/);

	void converged();
};

class C_csp_two_tank_tes : public C_csp_tes
{
private:

	HTFProperties mc_field_htfProps;		// Instance of HTFProperties class for field HTF
	HTFProperties mc_store_htfProps;		// Instance of HTFProperties class for storage HTF
	
	C_heat_exchanger mc_hx;				

	//Storage_HX mc_hx_storage;				// Instance of Storage_HX class for heat exchanger between storage and field HTFs
	
	C_storage_tank mc_cold_tank;			// Instance of storage tank class for the cold tank
	C_storage_tank mc_hot_tank;				// Instance of storage tank class for the hot tank	

	// member string for exception messages
	std::string error_msg;

	// Timestep data
	double m_m_dot_tes_dc_max;  //[kg/s] TES discharge available from the SYSTEM (field side of HX if there is one)
	double m_m_dot_tes_ch_max;  //[kg/s] TES charge that can be sent to the SYSTEM (field side of HX if there is one)

	// Member data
	bool m_is_tes;
	double m_vol_tank;			//[m3] volume of *one temperature*, i.e. vol_tank = total cold storage = total hot storage
	double m_V_tank_active;		//[m^3] available volume (considering h_min) of *one temperature*
	double m_q_pb_design;		//[Wt] thermal power to power cycle at design
	double m_V_tank_hot_ini;	//[m^3] Initial volume in hot storage tank


    // Monotonic equation solvers
    class C_MEQ_indirect_tes_discharge : public C_monotonic_equation
    {
    private:
        C_csp_two_tank_tes *mpc_csp_two_tank_tes;
        double m_timestep;
        double m_T_amb;
        double m_T_cold_field;
        double m_m_dot_field;

    public:
        C_MEQ_indirect_tes_discharge(C_csp_two_tank_tes *pc_csp_two_tank_tes, double timestep, double T_amb,
            double T_cold_field, double m_dot_field)
        {
            mpc_csp_two_tank_tes = pc_csp_two_tank_tes;
            m_timestep = timestep;
            m_T_amb = T_amb;
            m_T_cold_field = T_cold_field;
            m_m_dot_field = m_dot_field;
        }

        virtual int operator()(double m_dot_tank /*kg/s*/, double *m_dot_bal /*-*/);
    };

    class C_MEQ_indirect_tes_charge : public C_monotonic_equation
    {
    private:
        C_csp_two_tank_tes *mpc_csp_two_tank_tes;
        double m_timestep;
        double m_T_amb;
        double m_T_hot_field;
        double m_m_dot_field;

    public:
        C_MEQ_indirect_tes_charge(C_csp_two_tank_tes *pc_csp_two_tank_tes, double timestep, double T_amb,
            double T_hot_field, double m_dot_field)
        {
            mpc_csp_two_tank_tes = pc_csp_two_tank_tes;
            m_timestep = timestep;
            m_T_amb = T_amb;
            m_T_hot_field = T_hot_field;
            m_m_dot_field = m_dot_field;
        }

        virtual int operator()(double m_dot_tank /*kg/s*/, double *m_dot_bal /*-*/);
    };

public:

	// Class to save messages for up stream classes
	C_csp_messages mc_csp_messages;

	struct S_params
	{
		int m_field_fl;
		util::matrix_t<double> m_field_fl_props;

		int m_tes_fl;
		util::matrix_t<double> m_tes_fl_props;

		bool m_is_hx;

		double m_W_dot_pc_design;   //[MWe] Design point gross power cycle output
		double m_eta_pc;            //[-] Design point power cycle thermal efficiency
		double m_solarm;			//[-] solar multiple
		double m_ts_hours;			//[hr] hours of storage at design power cycle operation		
		double m_h_tank;			//[m] tank height
		double m_u_tank;			//[W/m^2-K]
		int m_tank_pairs;			//[-]
		double m_hot_tank_Thtr;		//[C] convert to K in init()
		double m_hot_tank_max_heat;	//[MW]
		double m_cold_tank_Thtr;	//[C] convert to K in init()
		double m_cold_tank_max_heat;//[MW]
		double m_dt_hot;			//[C] Temperature difference across heat exchanger - assume hot and cold deltaTs are equal
		double m_T_field_in_des;	//[C] convert to K in init()
		double m_T_field_out_des;	//[C] convert to K in init()
		double m_T_tank_hot_ini;	//[C] Initial temperature in hot storage tank
		double m_T_tank_cold_ini;	//[C] Initial temperature in cold storage cold
		double m_h_tank_min;		//[m] Minimum allowable HTF height in storage tank
		double m_f_V_hot_ini;       //[%] Initial fraction of available volume that is hot
		double m_htf_pump_coef;		//[kW/kg/s] Pumping power to move 1 kg/s of HTF through power cycle
        double m_tes_pump_coef;		//[kW/kg/s] Pumping power to move 1 kg/s of HTF through tes loop
        bool tanks_in_parallel;     //[-] Whether the tanks are in series or parallel with the solar field. Series means field htf must go through storage tanks.
        bool has_hot_tank_bypass;   //[-] True if the bypass valve causes the field htf to bypass just the hot tank and enter the cold tank before flowing back to the field.
        double T_tank_hot_inlet_min; //[C] Minimum field htf temperature that may enter the hot tank
        double V_tes_des;           //[m/s] Design-point velocity for sizing the diameters of the TES piping
        bool custom_tes_p_loss;     //[-] True if the TES piping losses should be calculated using the TES pipe lengths and minor loss coeffs, false if using the pumping loss parameters
        util::matrix_t<double> k_tes_loss_coeffs; //[-] Combined minor loss coefficients of the fittings and valves in the collection (including bypass) and generation loops in the TES 
        bool custom_sgs_pipe_sizes;               //[-] True if the SGS diameters and wall thicknesses parameters should be used instead of calculating them
        util::matrix_t<double> sgs_diams;         //[m] Imported inner diameters for the SGS piping as read from the modified output files
        util::matrix_t<double> sgs_wallthicks;    //[m] Imported wall thicknesses for the SGS piping as read from the modified output files
        util::matrix_t<double> sgs_lengths;       //[m] Imported lengths for the SGS piping as read from the modified output files


		S_params()
		{
			m_field_fl = m_tes_fl = m_tank_pairs = -1;		
			m_is_hx = true;

			m_ts_hours = 0.0;		//[hr] Default to 0 so that if storage isn't defined, simulation won't crash

			m_W_dot_pc_design = m_eta_pc = m_solarm = m_h_tank = m_u_tank = m_hot_tank_Thtr = m_hot_tank_max_heat = m_cold_tank_Thtr =
				m_cold_tank_max_heat = m_dt_hot = m_T_field_in_des = m_T_field_out_des = m_T_tank_hot_ini =
				m_T_tank_cold_ini = m_h_tank_min = m_f_V_hot_ini = m_htf_pump_coef = std::numeric_limits<double>::quiet_NaN();
		}
	};

	S_params ms_params;

	C_csp_two_tank_tes();

	~C_csp_two_tank_tes(){};

	virtual void init();

	virtual bool does_tes_exist();

	virtual double get_hot_temp();

	virtual double get_cold_temp();
	
    virtual double get_initial_charge_energy(); //MWh

    virtual double get_min_charge_energy(); //MWh

    virtual double get_max_charge_energy(); //MWh

    virtual double get_degradation_rate();  // s^-1

	virtual void discharge_avail_est(double T_cold_K, double step_s, double &q_dot_dc_est, double &m_dot_field_est, double &T_hot_field_est);

	virtual void charge_avail_est(double T_hot_K, double step_s, double &q_dot_ch_est, double &m_dot_field_est, double &T_cold_field_est);

	// Calculate pumping power...???
	virtual bool discharge(double timestep /*s*/, double T_amb /*K*/, double m_dot_htf_in /*kg/s*/, double T_htf_cold_in, double & T_htf_hot_out /*K*/, C_csp_tes::S_csp_tes_outputs &outputs);

	virtual void discharge_full(double timestep /*s*/, double T_amb /*K*/, double T_htf_cold_in, double & T_htf_hot_out /*K*/, double & m_dot_htf_out /*kg/s*/, C_csp_tes::S_csp_tes_outputs &outputs);

	virtual bool charge(double timestep /*s*/, double T_amb /*K*/, double m_dot_htf_in /*kg/s*/, double T_htf_hot_in, double & T_htf_cold_out /*K*/, C_csp_tes::S_csp_tes_outputs &outputs);

	virtual void charge_full(double timestep /*s*/, double T_amb /*K*/, double T_htf_hot_in /*K*/, double & T_htf_cold_out /*K*/, double & m_dot_htf_out /*kg/s*/, C_csp_tes::S_csp_tes_outputs &outputs);
	
	virtual void idle(double timestep, double T_amb, C_csp_tes::S_csp_tes_outputs &outputs);
	
	virtual void converged();
};


void two_tank_tes_sizing(HTFProperties &tes_htf_props, double Q_tes_des /*MWt-hr*/, double T_tes_hot /*K*/,
		double T_tes_cold /*K*/, double h_min /*m*/, double h_tank /*m*/, int tank_pairs /*-*/, double u_tank /*W/m^2-K*/,
		double & vol_one_temp_avail /*m3*/, double & vol_one_temp_total /*m3*/, double & d_tank /*m*/,
		double & q_dot_loss_des /*MWt*/  );


#endif   //__csp_solver_two_tank_tes_