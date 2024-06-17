/*
BSD 3-Clause License

Copyright (c) Alliance for Sustainable Energy, LLC. See also https://github.com/NREL/ssc/blob/develop/LICENSE
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef __csp_solver_particlecline_tes_
#define __csp_solver_particlecline_tes_

#include "csp_solver_core.h"
#include "csp_solver_util.h"
#include "sam_csp_util.h"
#include "csp_solver_tes_core.h"

class C_csp_particlecline_tes : public C_csp_tes
{
private:

	// member string for exception messages
	std::string error_msg;

    // Constructor Inputs
    int m_external_fl;
    util::matrix_t<double> m_external_fl_props;
    double m_h_tank;            // [m] Tank height
    double m_T_cold_des;	    // [K] Design cold temperature
    double m_T_hot_des;	        // [K] Design hot temperature
    double m_T_tank_hot_ini;    // [K] Initial hot temperature
    double m_T_tank_cold_ini;   // [K] Initial cold temperature
    double m_f_V_hot_ini;       // [] Initial fraction of hot storage
    int m_n_xstep;              // [] Number spatial steps
    int m_n_subtimestep;        // [] Number sub timesteps
    double m_k_eff;             // [W/m K] effective conductivity
    double m_void_frac;         // [] Void fraction
    double m_dens_solid;        // [kg/m3] density of particles
    double m_cp_solid;          // [J/kg K] specific heat of particles
    double m_tes_pump_coef;		// [kW/kg/s] Pumping power to move 1 kg/s of HTF through tes loop

    // Time step carryover
    std::vector<double> m_T_prev_vec;   // [K] Temperatures in space, starting at CHARGE inlet (hot)
    std::vector<double> m_T_calc_vec;   // [K] Temperatures in space, starting at CHARGE inlet (hot)

    // Calculated in init()
    double m_d_tank;    // [m] Tank diameter
    double m_Ac;        // [m2] Tank cross sectional area

    // Private members
    bool m_use_T_grad_init = false;
    HTFProperties mc_external_htfProps;		// Instance of HTFProperties class for external HTF

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
		E_W_DOT_HTF_PUMP,    //[MWe]
	};


    C_csp_reported_outputs mc_reported_outputs;


    C_csp_particlecline_tes(
        int external_fl,                            // [-] external fluid identifier
        util::matrix_t<double> external_fl_props,   // [-] external fluid properties
        double h_tank_in,			                // [m] tank height input
        double T_cold_des_C,	                    // [C] convert to K in constructor()
        double T_hot_des_C,	                        // [C] convert to K in constructor()
        double T_tank_hot_ini_C,	                // [C] Initial temperature in hot storage tank
        double T_tank_cold_ini_C,	                // [C] Initial temperature in cold storage cold
        double f_V_hot_ini,                         // [%] Initial fraction of available volume that is hot
        int n_xstep,                                // number spatial sub steps
        int n_subtimestep,                          // number subtimesteps
        double tes_pump_coef,		                // [kW/kg/s] Pumping power to move 1 kg/s of HTF through tes loop
        double k_eff_solid,                         // [W/m K] Solid effective thermal conductivity
        double void_frac,                           // [] Packed bed void fraction
        double dens_solid,                          // [kg/m3] solid specific heat 
        double cp_solid,                            // [J/kg K] solid specific heat
        double d_tank
            );

    C_csp_particlecline_tes();

    ~C_csp_particlecline_tes() {};

    void set_T_grad_init(std::vector<double> T_grad_init_C);

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

    void get_design_parameters(double& vol_one_temp_avail /*m3*/, double& vol_one_temp_total /*m3*/,
        double& h_tank /*m*/, double& d_tank /*m*/,
        double& q_dot_loss_des /*MWt*/, double& dens_store_htf_at_T_ave /*kg/m3*/, double& Q_tes /*MWt-hr*/);

    bool charge(double timestep /*s*/, double T_amb /*K*/, double m_dot_htf_in /*kg/s*/,
        double T_htf_hot_in, double& T_htf_cold_out /*K*/,
        double& q_dot_heater /*MWe*/, double& m_dot /*kg/s*/, double& W_dot_rhtf_pump /*MWe*/,
        double& q_dot_loss /*MWt*/, double& q_dot_dc_to_htf /*MWt*/, double& q_dot_ch_from_htf /*MWt*/,
        double& T_hot_ave /*K*/, double& T_cold_ave /*K*/, double& T_hot_final /*K*/, double& T_cold_final /*K*/);

    bool discharge(double timestep /*s*/, double T_amb /*K*/, double m_dot_htf_in /*kg/s*/,
        double T_htf_cold_in, double& T_htf_hot_out /*K*/,
        double& q_dot_heater /*MWe*/, double& m_dot /*kg/s*/, double& W_dot_rhtf_pump /*MWe*/,
        double& q_dot_loss /*MWt*/, double& q_dot_dc_to_htf /*MWt*/, double& q_dot_ch_from_htf /*MWt*/,
        double& T_hot_ave /*K*/, double& T_cold_ave /*K*/, double& T_hot_final /*K*/, double& T_cold_final /*K*/);

    int pressure_drops(double m_dot_sf, double m_dot_pb,
        double T_sf_in, double T_sf_out, double T_pb_in, double T_pb_out, bool recirculating,
        double& P_drop_col, double& P_drop_gen);

    double get_max_storage_htf_temp();

    double get_min_storage_htf_temp();

    double get_storage_htf_density();

    double get_storage_htf_cp();

    std::vector<double> get_T_prev_vec() { return m_T_prev_vec; };
};


#endif
