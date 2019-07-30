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

#ifndef __csp_solver_gen_collector_receiver_
#define __csp_solver_gen_collector_receiver_

#include "sam_csp_util.h"
#include "lib_util.h"
#include "csp_solver_core.h"

class C_csp_gen_collector_receiver : public C_csp_collector_receiver
{

private:
	// *************************************************************
	// Define temperature and cp values so code interfaces with solver
	// *************************************************************
	double m_T_htf_cold_fixed;		//[K]
	double m_T_htf_hot_fixed;		//[K]
	double m_cp_htf_fixed;			//[kJ/kg-K]
	// *************************************************************

	OpticalDataTable mc_optical_table;
	GaussMarkov *mpc_optical_table_uns;

	//Constants for scaling GM table
	double m_eff_scale;   //defined later based on max value

	double m_f_qsf;

	int m_mode;
	int m_mode_prev;
	
	void check_double_params_are_set();	

public:

	enum
	{
		E_Q_DOT_FIELD_INC,	//[W/m2]
		E_ETA_FIELD,		//[-]
		E_Q_DOT_REC_INC,	//[W/m2]
		E_ETA_THERMAL,		//[-]
        E_F_SFHL_QDNI,
        E_F_SFHL_QWSPD,
        E_F_SFHL_QTDRY,
	};

	C_csp_reported_outputs mc_reported_outputs;

	//C_csp_messages mc_csp_messages;

	struct S_params
	{
		double m_latitude;		//[deg] Site latitude, convert to radians in init()
		double m_longitude;		//[deg] Site longitude, convert to radians in init()
		double m_theta_stow;	//[deg] Solar elevation angle at which the solar field stops operating, convert to radians in init()
		double m_theta_dep;		//[deg] Solar elevation angle at which the solar field begins operating, convert to radians in init()
		int m_interp_arr;		//[-] Interpolate the array or find nearest neighbor? (1=interp,2=no)
		int m_rad_type;			//[-] Solar resource radiation type (1=DNI,2=horiz.beam,3=tot.horiz)
		double m_solarm;		//[-] Solar multiple
		double m_T_sfdes;		//[C] Solar field design point temperature (dry bulb), convert to K in init()
		double m_irr_des;		//[W/m2] Irradiation design point 
		double m_eta_opt_soil;	//[-] Soiling optical derate factor
		double m_eta_opt_gen;	//[-] General/other optical derate
		double m_f_sfhl_ref;	//[MW/MWcap] Reference solar field thermal loss fraction
		std::vector<double> mv_sfhlQ_coefs;	//[1/MWt] Irr-based solar field thermal loss adjustment coefficients
		std::vector<double> mv_sfhlT_coefs; //[1/C] Temp.-based solar field thermal loss adjustment coefficients
		std::vector<double> mv_sfhlV_coefs; //[1/[m/s]] Wind-based solar field thermal loss adjustment coefficients
		double m_qsf_des;		//[MWt] Solar field thermal production at design

		util::matrix_t<double> m_optical_table;
		bool m_is_table_unsorted;
	
		S_params()
		{
			m_latitude = m_longitude = m_theta_stow = m_theta_dep = m_solarm = m_T_sfdes = 
				m_irr_des = m_eta_opt_soil = m_eta_opt_gen = m_f_sfhl_ref = m_qsf_des = std::numeric_limits<double>::quiet_NaN();

			m_interp_arr = m_rad_type = -1;

			mv_sfhlQ_coefs.resize(0);
			mv_sfhlT_coefs.resize(0);
			mv_sfhlV_coefs.resize(0);
		}
	};

	S_params ms_params;

	C_csp_gen_collector_receiver();

	~C_csp_gen_collector_receiver();

	// Done
	virtual void init(const C_csp_collector_receiver::S_csp_cr_init_inputs init_inputs, 
			C_csp_collector_receiver::S_csp_cr_solved_params & solved_params);

	// Done
	void init_sf();

	virtual int get_operating_state();

	virtual double get_startup_time();
	virtual double get_startup_energy(); //MWh
	virtual double get_pumping_parasitic_coef();  //MWe/MWt
	virtual double get_min_power_delivery();    //MWt
	virtual double get_tracking_power();		//MWe
	virtual double get_col_startup_power();		//MWe-hr
	virtual double get_remaining_startup_energy();		//kWh

	virtual void off(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		//C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
		const C_csp_solver_sim_info &sim_info);

	virtual void startup(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		//C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
		const C_csp_solver_sim_info &sim_info);

	virtual void on(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		double field_control,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		//C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
		const C_csp_solver_sim_info &sim_info);

	virtual void estimates(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		C_csp_collector_receiver::S_csp_cr_est_out &est_out,
		const C_csp_solver_sim_info &sim_info);

	virtual void converged();

	virtual void write_output_intervals(double report_time_start,
		const std::vector<double> & v_temp_ts_time_end, double report_time_end);

	virtual double calculate_optical_efficiency(const C_csp_weatherreader::S_outputs &weather, const C_csp_solver_sim_info &sim);

	virtual double calculate_thermal_efficiency_approx(const C_csp_weatherreader::S_outputs &weather, double q_incident /*MW*/);

	virtual double get_collector_area();



	void call(const C_csp_weatherreader::S_outputs &weather,
		const C_csp_solver_htf_1state &htf_state_in,
		const C_csp_collector_receiver::S_csp_cr_inputs &inputs,
		C_csp_collector_receiver::S_csp_cr_out_solver &cr_out_solver,
		//C_csp_collector_receiver::S_csp_cr_out_report &cr_out_report,
		const C_csp_solver_sim_info &sim_info);

};

#endif	// __csp_solver_gen_collector_receiver_