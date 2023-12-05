#ifndef __csp_solver_NTHeatTrap_tes_
#define __csp_solver_NTHeatTrap_tes_

#include "csp_solver_core.h"
#include "csp_solver_util.h"
#include "sam_csp_util.h"
#include "csp_solver_two_tank_tes.h"

class C_csp_NTHeatTrap_tes : public C_csp_tes
{
private:

	HTFProperties mc_external_htfProps;		// Instance of HTFProperties class for external HTF
	HTFProperties mc_store_htfProps;		// Instance of HTFProperties class for storage HTF

	C_hx_two_tank_tes mc_hx;

	C_storage_tank mc_cold_tank;			// Instance of storage tank class for the cold tank
	C_storage_tank mc_hot_tank;				// Instance of storage tank class for the hot tank	

	// member string for exception messages
	std::string error_msg;

	// Timestep data
	// double m_m_dot_tes_dc_max;  //[kg/s] TES discharge available from the SYSTEM (external side of HX if there is one)
	// double m_m_dot_tes_ch_max;  //[kg/s] TES charge that can be sent to the SYSTEM (external side of HX if there is one)

	// Member data
	bool m_is_tes;
	bool m_is_cr_to_cold_tank_allowed;
	double m_vol_tank;			//[m3] volume of *one temperature*, i.e. vol_tank = total cold storage = total hot storage
	double m_V_tank_active;		//[m^3] available volume (considering h_min) of *one temperature*
	double m_q_pb_design;		//[Wt] thermal power to sink at design
	double m_V_tank_hot_ini;	//[m^3] Initial volume in hot storage tank
	double m_mass_total_active; //[kg] Total HTF mass at design point inlet/outlet T
	double m_d_tank;            //[m] diameter of a single tank
	double m_q_dot_loss_des;    //[MWt] design tank heat loss
	double m_ts_hours;          //[hr] hours of storage at design sink operation		

	double m_cp_external_avg;	//[kJ/kg-K]
	double m_rho_store_avg;     //[kg/m3]

	double m_m_dot_tes_des_over_m_dot_external_des;	//[-]

public:

	enum
	{
		E_Q_DOT_LOSS,		//[MWt] TES thermal losses
		E_W_DOT_HEATER,		//[MWe] TES freeze protection power
		E_TES_T_HOT,		//[C] TES final hot tank temperature
		E_TES_T_COLD,		//[C] TES final cold tank temperature
		E_M_DOT_TANK_TO_TANK,	//[kg/s] Tank to tank mass flow rate (indirect TES)
		E_MASS_COLD_TANK,	//[kg] Mass in cold tank at end of timestep
		E_MASS_HOT_TANK,		//[kg] Mass in hot tank at end of timestep
		E_HOT_TANK_HTF_PERC_FINAL,   //[%] Final percent fill of available hot tank mass
		E_W_DOT_HTF_PUMP    //[MWe]
	};

	C_csp_NTHeatTrap_tes();

	virtual void init(const C_csp_tes::S_csp_tes_init_inputs init_inputs);

	virtual bool does_tes_exist();

	virtual bool is_cr_to_cold_allowed();

	virtual double get_hot_temp();

	virtual double get_cold_temp();

	virtual double get_hot_tank_vol_frac();

	virtual double get_initial_charge_energy(); //MWh

	virtual double get_min_charge_energy(); //MWh

	virtual double get_max_charge_energy(); //MWh

	virtual double get_degradation_rate();  // s^-1

	virtual void reset_storage_to_initial_state();

	virtual void discharge_avail_est(double T_cold_K, double step_s,
		double& q_dot_dc_est /*MWt*/, double& m_dot_external_est /*kg/s*/, double& T_hot_external_est /*K*/);

	virtual void charge_avail_est(double T_hot_K, double step_s,
		double& q_dot_ch_est /*MWt*/, double& m_dot_external_est /*kg/s*/, double& T_cold_external_est /*K*/);

	virtual int solve_tes_off_design(double timestep /*s*/, double  T_amb /*K*/,
		double m_dot_cr_to_cv_hot /*kg/s*/, double m_dot_cv_hot_to_sink /*kg/s*/, double m_dot_cr_to_cv_cold /*kg/s*/,
		double T_cr_out_hot /*K*/, double T_sink_out_cold /*K*/,
		double& T_sink_htf_in_hot /*K*/, double& T_cr_in_cold /*K*/,
		C_csp_tes::S_csp_tes_outputs& outputs);

	virtual void converged();

	virtual void write_output_intervals(double report_time_start,
		const std::vector<double>& v_temp_ts_time_end, double report_time_end);

	virtual void assign(int index, double* p_reporting_ts_array, size_t n_reporting_ts_array);

	virtual /*MWe*/ double pumping_power(double m_dot_sf /*kg/s*/, double m_dot_pb /*kg/s*/, double m_dot_tank /*kg/s*/,
		double T_sf_in /*K*/, double T_sf_out /*K*/, double T_pb_in /*K*/, double T_pb_out /*K*/, bool recirculating);

};


#endif
