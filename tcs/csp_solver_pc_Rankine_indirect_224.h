#ifndef __csp_solver_pc_Rankine_indirect_224_
#define __csp_solver_pc_Rankine_indirect_224_

#include "csp_solver_util.h"
#include "csp_solver_core.h"

#include "lib_util.h"
#include "htf_props.h"

#include "user_defined_power_cycle.h"

class C_pc_Rankine_indirect_224 : public C_csp_power_cycle
{
private:
	bool m_is_initialized;

	double m_F_wcMax;
	double m_F_wcMin;
	double m_delta_h_steam;
	double m_startup_energy_required;
	double m_eta_adj;

	double m_m_dot_design;				//[kg/hr]
	double m_q_dot_design;				//[MWt]
	double m_cp_htf_design;				//[kJ/kg-K]

	int m_standby_control_prev;
	double m_startup_time_remain_prev;		//[hr]
	double m_startup_energy_remain_prev;	//[kW-hr]

	int m_standby_control_calc;
	double m_startup_time_remain_calc;
	double m_startup_energy_remain_calc;

	util::matrix_t<double> m_db;

	HTFProperties mc_pc_htfProps;

	// member string for exception messages
	std::string m_error_msg;

	// member class for User Defined Power Cycle
	C_user_defined_pc mc_user_defined_pc;

	// track number of calls per timestep, reset = -1 in converged() call
	int m_ncall;

	double GetFieldToTurbineTemperatureDropC() { return 25.0; }

	void RankineCycle(/*double time,*/double P_ref, double eta_ref, double T_htf_hot_ref, double T_htf_cold_ref, double T_db, double T_wb,
		double P_amb, double dT_cw_ref, /*double HTF,*/ double c_p_w, double T_htf_hot, double m_dot_htf, int /*double*/ mode,
		double demand_var, double P_boil, /*double tech_type,*/ double T_amb_des, double T_approach, double F_wc, double F_wcmin,
		double F_wcmax, double T_ITD_des, double P_cond_ratio, /*double CT,*/ double P_cond_min, /*double n_pl_inc,*/
		/*double& fcall, */ double& P_cycle, double& eta, double& T_htf_cold, double& m_dot_demand, double& m_dot_htf_ref,
		double& m_dot_makeup, double& W_cool_par, double& f_hrsys, double& P_cond);

	double Interpolate(int YT, int XT, double X);

	// Isopentane
	double T_sat4(double P/*Bar*/) 
	{
		return 284.482349 + 20.8848464*P - 1.5898147*P*P + 0.0655241456*P*P*P - 0.0010168822*P*P*P*P; /*return value in Kelvin*/
	}

public:
	// Class to save messages for up stream classes
	C_csp_messages mc_csp_messages;

	struct S_params
	{
			// Parameters with common SSCINPUT name for both Rankine Cycle and User Defined Cycle
		double m_P_ref;				//[MW] design electric power output, converted to kW in init()
		double m_eta_ref;			//[%] design conversion efficiency
		double m_T_htf_hot_ref;		//[C] design HTF inlet temperature
		double m_T_htf_cold_ref;	//[C] design HTF output temperature
		double m_cycle_max_frac;	//[-] Maximum turbine over-design operation fraction
		double m_cycle_cutoff_frac;	//[-] Minimum turbine operation fraction
		double m_q_sby_frac;		//[-] fraction of thermal power required for standby mode
		double m_startup_time;		//[hr] time needed for power block startup
		double m_startup_frac;		//[-] fraction of design thermal power needed for startup
		double m_htf_pump_coef;		//[kW/kg/s] Pumping power to move 1 kg/s of HTF through power cycle

		int m_pc_fl;				//[-] integer flag identifying Heat Transfer Fluid (HTF) in power block {1-27}
		util::matrix_t<double> m_pc_fl_props;

		// Steam Rankine or User-Defined
		bool m_is_user_defined_pc;				//[-] True: user-defined power cycle, False: Built-in Rankine Cycle model

			// Parameters that have different SSCINPUT names for Rankine Cycle and User Defined Cycle
		double m_dT_cw_ref;			//[C] design temp difference between cooling water inlet/outlet
		double m_T_amb_des;			//[C] design ambient temperature
		double m_P_boil;			//[bar] boiler operating pressure
		int m_CT;					//[-] integer flag for cooling technology type {1=evaporative cooling, 2=air cooling, 3=hybrid cooling}
		int m_tech_type;			//[-] Flag indicating which coef. set to use. (1=tower,2=trough,3=user) 
		double m_T_approach;		//[C] cooling tower approach temp
		double m_T_ITD_des;			//[C] design ITD for dry system
		double m_P_cond_ratio;		//[-] condenser pressure ratio
		double m_pb_bd_frac;		//[-] blowdown steam fraction
		double m_P_cond_min;		//[inHG] minimum condenser pressure, converted to Pa in code
		int m_n_pl_inc;				//[-] Number of part-load increments for the heat rejection system
		
		
		std::vector<double> m_F_wc;		//[-] hybrid cooling dispatch fractions 1 thru 9 (array index 0-8)	
		
		// Parameters for user-defined power cycle
			// Lookup table with dependent variables corresponding to parametric on independent variable T_htf_hot [C] (first column)
		util::matrix_t<double> mc_T_htf_ind;	// Interaction w/ m_dot_htf
			// Lookup table with dependent variables corresponding to parametric on independent variable T_amb [C] (first column)
		util::matrix_t<double> mc_T_amb_ind;	// Interaction w/ T_htf
			// Lookup table with dependent variables corresponding to parametric on independent variable m_dot_htf [ND] (first column)
		util::matrix_t<double> mc_m_dot_htf_ind;	// Interaction w/ T_amb

		double m_W_dot_cooling_des;		//[MW] Cooling parasitic at design conditions
		double m_m_dot_water_des;		//[kg/s] Power cycle water use at design conditions

		S_params()
		{
			m_P_ref = m_eta_ref = m_T_htf_hot_ref = m_T_htf_cold_ref = m_dT_cw_ref = m_T_amb_des =
				m_q_sby_frac = m_P_boil = m_startup_time = m_startup_frac = m_T_approach = m_T_ITD_des =
				m_P_cond_ratio = m_pb_bd_frac = m_P_cond_min = m_htf_pump_coef = std::numeric_limits<double>::quiet_NaN();

			m_pc_fl = m_CT = m_tech_type = m_n_pl_inc = -1;

			// Initialize parameters for user-defined power cycle
			m_is_user_defined_pc = false;
			m_W_dot_cooling_des = m_m_dot_water_des = std::numeric_limits<double>::quiet_NaN();
		}
	};

	//struct S_outputs
	//{
	//	double m_P_cycle;			//[MWe] Cycle power output
	//	double m_eta;				//[-] Cycle thermal efficiency
	//	double m_T_htf_cold;		//[C] Heat transfer fluid outlet temperature
	//	double m_m_dot_makeup;		//[kg/hr] Cooling water makeup flow rate
	//	double m_m_dot_demand;		//[kg/hr] HTF required flow rate to meet power load
	//	double m_m_dot_htf;			//[kg/hr] Actual HTF flow rate passing through the power cycle
	//	double m_m_dot_htf_ref;		//[kg/hr] Calculated reference HTF flow rate at design
	//	double m_W_cool_par;		//[MWe] Cooling system parasitic load
	//	double m_P_ref;				//[MWe] Reference power level output at design
	//	double m_f_hrsys;			//[-] Fraction of operating heat rejection system
	//	double m_P_cond;			//[Pa] Condenser pressure

	//	S_outputs()
	//	{
	//		m_P_cycle = m_eta = m_T_htf_cold = m_m_dot_makeup = m_m_dot_demand = m_m_dot_htf = m_m_dot_htf_ref =
	//			m_W_cool_par = m_P_ref = m_f_hrsys = m_P_cond = std::numeric_limits<double>::quiet_NaN();
	//	}
	//};

	S_params ms_params;

	//S_outputs ms_outputs;

	C_pc_Rankine_indirect_224();

	~C_pc_Rankine_indirect_224(){};

	virtual void init();

	virtual int get_operating_state();

	virtual void get_design_parameters(C_csp_power_cycle::S_solved_params &solved_params);

    virtual double get_cold_startup_time(); 
    virtual double get_warm_startup_time();
    virtual double get_hot_startup_time();
    virtual double get_standby_energy_requirement();    //[MW]
    virtual double get_cold_startup_energy(double step /*sec*/);    //[MWh]
    virtual double get_warm_startup_energy(double step /*sec*/);    //[MWh]
    virtual double get_hot_startup_energy(double step /*sec*/);    //[MWh]
    virtual double get_max_thermal_power();     //MW
    virtual double get_min_thermal_power();     //MW
    virtual double get_efficiency_at_TPH(double T_degC, double P_atm, double relhum_pct);
    virtual double get_efficiency_at_load(double load_frac);

	// This can vary between timesteps for Type224, depending on remaining startup energy and time
	virtual double get_max_q_pc_startup();		//[MWt]


	virtual void call(const C_csp_weatherreader::S_outputs &weather, 
		C_csp_solver_htf_state &htf_state,
		const C_csp_power_cycle::S_control_inputs &inputs,
		C_csp_power_cycle::S_csp_pc_outputs &outputs,
		const C_csp_solver_sim_info &sim_info);

	virtual void converged();

};








#endif //__csp_solver_pc_Rankine_indirect_224_