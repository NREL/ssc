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

#include "sco2_partialcooling_cycle.h"

#include <algorithm>

int C_PartialCooling_Cycle::design(S_des_params & des_par_in)
{
	ms_des_par = des_par_in;

	int des_err_code = design_core();

	return des_err_code;
}

int C_PartialCooling_Cycle::design_core()
{
	// Check that the recompression fraction is not too close to 0
	if (ms_des_par.m_recomp_frac < 0.01)
	{
		ms_des_par.m_recomp_frac = 0.0;
		double UA_tot = ms_des_par.m_UA_LTR + ms_des_par.m_UA_HTR;		//[kW/K]
		ms_des_par.m_UA_LTR = UA_tot;		//[kW/K]
		ms_des_par.m_UA_HTR = 0.0;			//[kW/K]
	}

	// Initialize Recuperators
	mc_LTR.initialize(ms_des_par.m_N_sub_hxrs);
	mc_HTR.initialize(ms_des_par.m_N_sub_hxrs);

	// Initialize known temps and pressures from design parameters
	m_temp_last[MC_IN] = ms_des_par.m_T_mc_in;	//[K]
	m_pres_last[MC_IN] = ms_des_par.m_P_mc_in;	//[kPa]
	m_temp_last[PC_IN] = ms_des_par.m_T_pc_in;	//[K]
	m_pres_last[PC_IN] = ms_des_par.m_P_pc_in;	//[kPa]
	m_temp_last[TURB_IN] = ms_des_par.m_T_t_in;	//[K]
	m_pres_last[MC_OUT] = ms_des_par.m_P_mc_out;//[kPa]

	// Apply design pressure drops to heat exchangers to fully define pressures at all states
	if (ms_des_par.m_DP_LTR[0] < 0.0)
		m_pres_last[LTR_HP_OUT] = m_pres_last[MC_OUT] * (1.0 - fabs(ms_des_par.m_DP_LTR[0]));	//[kPa]
	else
		m_pres_last[LTR_HP_OUT] = m_pres_last[MC_OUT] - ms_des_par.m_DP_LTR[0];		//[kPa]

	if (ms_des_par.m_UA_LTR < 1.0E-12)
		m_pres_last[LTR_HP_OUT] = m_pres_last[MC_OUT];	//[kPa] If no LTR then no pressure drop

	m_pres_last[MIXER_OUT] = m_pres_last[LTR_HP_OUT];	//[kPa] assume no pressure drop in mixer
	m_pres_last[RC_OUT] = m_pres_last[LTR_HP_OUT];		//[kPa] assume no pressure drop in mixer

	if (ms_des_par.m_DP_HTR[0] < 0.0)
		m_pres_last[HTR_HP_OUT] = m_pres_last[MIXER_OUT] * (1.0 - fabs(ms_des_par.m_DP_HTR[0]));	//[kPa]
	else
		m_pres_last[HTR_HP_OUT] = m_pres_last[MIXER_OUT] - ms_des_par.m_DP_HTR[0];	//[kPa]

	if (ms_des_par.m_UA_HTR < 1.0E-12)
		m_pres_last[HTR_HP_OUT] = m_pres_last[MIXER_OUT];	//[kPa] If no HTR then no pressure drop

	if (ms_des_par.m_DP_PHX[0] < 0.0)
		m_pres_last[TURB_IN] = m_pres_last[HTR_HP_OUT] * (1.0 - fabs(ms_des_par.m_DP_PHX[0]));	//[kPa]
	else
		m_pres_last[TURB_IN] = m_pres_last[HTR_HP_OUT] - ms_des_par.m_DP_PHX[0];	//[kPa]

	if (ms_des_par.m_DP_PC_partial[1] < 0.0)
		m_pres_last[PC_OUT] = m_pres_last[MC_IN] / (1.0 - fabs(ms_des_par.m_DP_PC_partial[1]));	//[kPa]
	else
		m_pres_last[PC_OUT] = m_pres_last[MC_IN] + ms_des_par.m_DP_PC_partial[1];	//[kPa]

	if (ms_des_par.m_DP_PC_full[1] < 0.0)
		m_pres_last[LTR_LP_OUT] = m_pres_last[PC_IN] / (1.0 - fabs(ms_des_par.m_DP_PC_partial[1]));	//[kPa]
	else
		m_pres_last[LTR_LP_OUT] = m_pres_last[PC_IN] + ms_des_par.m_DP_PC_partial[1];	//[kPa]

	if (ms_des_par.m_DP_LTR[1] < 0.0)
		m_pres_last[HTR_LP_OUT] = m_pres_last[LTR_LP_OUT] / (1.0 - fabs(ms_des_par.m_DP_LTR[1]));	//[kPa]
	else
		m_pres_last[HTR_LP_OUT] = m_pres_last[LTR_LP_OUT] + ms_des_par.m_DP_LTR[1];		//[kPa]

	if (ms_des_par.m_UA_LTR < 1.0E-12)
		m_pres_last[HTR_LP_OUT] = m_pres_last[LTR_LP_OUT];	//[kPa] if no LTR then no pressure drop

	if (ms_des_par.m_DP_HTR[1] < 0.0)
		m_pres_last[TURB_OUT] = m_pres_last[HTR_LP_OUT] / (1.0 - fabs(ms_des_par.m_DP_HTR[1]));	//[kPa]
	else
		m_pres_last[TURB_OUT] = m_pres_last[HTR_LP_OUT] + ms_des_par.m_DP_HTR[1];	//[kPa]

	if (ms_des_par.m_UA_HTR < 1.0E-12)
		m_pres_last[TURB_OUT] = m_pres_last[HTR_LP_OUT];

	// Calculate equivalent isentropic efficiencies for turbomachinery, if necessary
	double eta_mc_isen = ms_des_par.m_eta_mc;		//[-]
	if (ms_des_par.m_eta_mc < 0.0)
	{
		int poly_error_code = 0;

		isen_eta_from_poly_eta(m_temp_last[MC_IN], m_pres_last[MC_IN], m_pres_last[MC_OUT], fabs(ms_des_par.m_eta_mc),
			true, poly_error_code, eta_mc_isen);

		if (poly_error_code != 0)
			return poly_error_code;
	}

	double eta_rc_isen = ms_des_par.m_eta_rc;		//[-]
	if (ms_des_par.m_eta_rc < 0.0)
	{
		int poly_error_code = 0;

		isen_eta_from_poly_eta(m_temp_last[PC_OUT], m_pres_last[PC_OUT], m_pres_last[RC_OUT], fabs(ms_des_par.m_eta_rc),
			true, poly_error_code, eta_rc_isen);

		if (poly_error_code != 0)
			return poly_error_code;
	}

	double eta_pc_isen = ms_des_par.m_eta_pc;		//[-]
	if (ms_des_par.m_eta_pc < 0.0)
	{
		int poly_error_code = 0;

		isen_eta_from_poly_eta(m_temp_last[PC_IN], m_pres_last[PC_IN], m_pres_last[PC_OUT], fabs(ms_des_par.m_eta_pc),
			true, poly_error_code, eta_pc_isen);

		if (poly_error_code != 0)
			return poly_error_code;
	}

	double eta_t_isen = ms_des_par.m_eta_t;		//[-]
	if (ms_des_par.m_eta_t < 0.0)
	{
		int poly_error_code = 0;

		isen_eta_from_poly_eta(m_temp_last[TURB_IN], m_pres_last[TURB_IN], m_pres_last[TURB_OUT], fabs(ms_des_par.m_eta_t),
			false, poly_error_code, eta_t_isen);

		if (poly_error_code != 0)
			return poly_error_code;
	}

	// Determine the outlet state and specific work for the turbomachinery
	int comp_error_code = 0;
	double w_mc = std::numeric_limits<double>::quiet_NaN();
	calculate_turbomachinery_outlet_1(m_temp_last[MC_IN], m_pres_last[MC_IN], m_pres_last[MC_OUT], eta_mc_isen, true,
		comp_error_code, m_enth_last[MC_IN], m_entr_last[MC_IN], m_dens_last[MC_IN], m_temp_last[MC_OUT],
		m_enth_last[MC_OUT], m_entr_last[MC_OUT], m_dens_last[MC_OUT], w_mc);

	if (comp_error_code != 0)
		return comp_error_code;

	double w_pc = std::numeric_limits<double>::quiet_NaN();
	calculate_turbomachinery_outlet_1(m_temp_last[PC_IN], m_pres_last[PC_IN], m_pres_last[PC_OUT], eta_pc_isen, true,
		comp_error_code, m_enth_last[PC_IN], m_entr_last[PC_IN], m_dens_last[PC_IN], m_temp_last[PC_OUT],
		m_enth_last[PC_OUT], m_entr_last[PC_OUT], m_dens_last[PC_OUT], w_pc);

	if (comp_error_code != 0)
		return comp_error_code;

	double w_rc = std::numeric_limits<double>::quiet_NaN();
	calculate_turbomachinery_outlet_1(m_temp_last[PC_OUT], m_pres_last[PC_OUT], m_pres_last[RC_OUT], eta_rc_isen, true,
		comp_error_code, m_enth_last[PC_OUT], m_entr_last[PC_OUT], m_dens_last[PC_OUT], m_temp_last[RC_OUT],
		m_enth_last[RC_OUT], m_entr_last[RC_OUT], m_dens_last[RC_OUT], w_rc);

	if (comp_error_code != 0)
		return comp_error_code;

	double w_t = std::numeric_limits<double>::quiet_NaN();
	calculate_turbomachinery_outlet_1(m_temp_last[TURB_IN], m_pres_last[TURB_IN], m_pres_last[TURB_OUT], eta_t_isen, false,
		comp_error_code, m_enth_last[TURB_IN], m_entr_last[TURB_IN], m_dens_last[TURB_IN], m_temp_last[TURB_OUT],
		m_enth_last[TURB_OUT], m_entr_last[TURB_OUT], m_dens_last[TURB_OUT], w_t);

	if (comp_error_code != 0)
		return comp_error_code;

	// know all turbomachinery specific work, so can calculate mass flow rate required to hit target power
	m_m_dot_t = ms_des_par.m_W_dot_net / (w_t + w_pc + ms_des_par.m_recomp_frac*w_rc + (1.0 - ms_des_par.m_recomp_frac)*w_mc);	//[kg/s]
	
	if (m_m_dot_t <= 0.0 || !isfinite(m_m_dot_t))	// positive net power is impossible; return an error
		return 25;

	m_m_dot_pc = m_m_dot_t;	//[kg/s]
	m_m_dot_rc = m_m_dot_t * ms_des_par.m_recomp_frac;	//[kg/s]
	m_m_dot_mc = m_m_dot_t *(1.0 - ms_des_par.m_recomp_frac);	//[kg/s]

	// Solve the recuperator performance
	double T_HTR_LP_out_lower = m_temp_last[MC_OUT];		//[K] Coldest possible temperature
	double T_HTR_LP_out_upper = m_temp_last[TURB_OUT];		//[K] Hottest possible temperature

	double T_HTR_LP_out_guess_lower = std::min(T_HTR_LP_out_upper - 2.0, std::max(T_HTR_LP_out_lower+15.0, 220.0 + 273.15));	//[K] There is nothing magic about 15
	double T_HTR_LP_out_guess_upper = std::min(T_HTR_LP_out_guess_lower + 20.0, T_HTR_LP_out_upper - 1.0);		//[K] There is nothing magic about 20

	C_MEQ_HTR_des HTR_des_eq(this);
	C_monotonic_eq_solver HTR_des_solver(HTR_des_eq);

	HTR_des_solver.settings(ms_des_par.m_tol*m_temp_last[MC_IN], 1000, T_HTR_LP_out_lower, T_HTR_LP_out_upper, false);

	double T_HTR_LP_out_solved, tol_T_HTR_LP_out_solved;
	T_HTR_LP_out_solved = tol_T_HTR_LP_out_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_T_HTR_LP_out = -1;

	int T_HTR_LP_out_code = HTR_des_solver.solve(T_HTR_LP_out_guess_lower, T_HTR_LP_out_guess_upper, 0,
								T_HTR_LP_out_solved, tol_T_HTR_LP_out_solved, iter_T_HTR_LP_out);

	if (T_HTR_LP_out_code != C_monotonic_eq_solver::CONVERGED)
	{
		return 35;
	}

	double Q_dot_HTR = HTR_des_eq.m_Q_dot_HTR;		//[kWt]

	// Can now define HTR HP outlet state
	m_enth_last[HTR_HP_OUT] = m_enth_last[MIXER_OUT] + Q_dot_HTR / m_m_dot_t;		//[kJ/kg]
	int prop_error_code = CO2_PH(m_pres_last[HTR_HP_OUT], m_enth_last[HTR_HP_OUT], &mc_co2_props);
	if (prop_error_code != 0)
	{
		return prop_error_code;
	}
	m_temp_last[HTR_HP_OUT] = mc_co2_props.temp;	//[K]
	m_entr_last[HTR_HP_OUT] = mc_co2_props.entr;	//[kJ/kg-K]
	m_dens_last[HTR_HP_OUT] = mc_co2_props.dens;	//[kg/m^3]

	// Set design values for PHX and coolers
	C_HeatExchanger::S_design_parameters PHX_des_par;
	PHX_des_par.m_DP_design[0] = m_pres_last[HTR_HP_OUT] - m_pres_last[TURB_IN];		//[kPa]
	PHX_des_par.m_DP_design[1] = 0.0;
	PHX_des_par.m_m_dot_design[0] = m_m_dot_t;		//[kg/s]
	PHX_des_par.m_m_dot_design[1] = 0.0;			//[kg/s]
	PHX_des_par.m_Q_dot_design = m_m_dot_t*(m_enth_last[TURB_IN] - m_enth_last[HTR_HP_OUT]);	//[kWt]
	mc_PHX.initialize(PHX_des_par);

	C_HeatExchanger::S_design_parameters PC_full_des_par;
	PC_full_des_par.m_DP_design[0] = 0.0;
	PC_full_des_par.m_DP_design[1] = m_pres_last[LTR_LP_OUT] - m_pres_last[PC_IN];	//[kPa]
	PC_full_des_par.m_m_dot_design[0] = 0.0;		//[kg/s]
	PC_full_des_par.m_m_dot_design[1] = m_m_dot_pc;	//[kg/s]
	PC_full_des_par.m_Q_dot_design = m_m_dot_pc*(m_enth_last[LTR_LP_OUT] - m_enth_last[PC_IN]);	//[kWt]
	mc_PC_full.initialize(PC_full_des_par);

	C_HeatExchanger::S_design_parameters PC_partial_des_par;
	PC_partial_des_par.m_DP_design[0] = 0.0;
	PC_partial_des_par.m_DP_design[1] = m_pres_last[PC_OUT] - m_pres_last[MC_IN];	//[kPa]
	PC_partial_des_par.m_m_dot_design[0] = 0.0;
	PC_partial_des_par.m_m_dot_design[1] = m_m_dot_mc;	//[kg/s]
	PC_partial_des_par.m_Q_dot_design = m_m_dot_mc*(m_enth_last[PC_OUT] - m_enth_last[MC_IN]);	//[kWt]
	mc_PC_partial.initialize(PC_partial_des_par);

	// Calculate and set cycle performance metrics
	m_W_dot_net_last = m_m_dot_t*w_t + m_m_dot_pc*w_pc + m_m_dot_rc*w_rc + m_m_dot_mc*w_mc;		//[kWe]
	m_eta_thermal_calc_last = m_W_dot_net_last / PHX_des_par.m_Q_dot_design;	//[-]
	m_energy_bal_last = (PHX_des_par.m_Q_dot_design - m_W_dot_net_last - PC_partial_des_par.m_Q_dot_design - PC_full_des_par.m_Q_dot_design) / PHX_des_par.m_Q_dot_design;	//[-]

	return 0;
}

int C_PartialCooling_Cycle::C_MEQ_HTR_des::operator()(double T_HTR_LP_out /*K*/, double *diff_T_HTR_LP_out /*K*/)
{
	m_Q_dot_LTR = m_Q_dot_HTR = std::numeric_limits<double>::quiet_NaN();

	mpc_pc_cycle->m_temp_last[HTR_LP_OUT] = T_HTR_LP_out;	//[K]

	int prop_error_code = CO2_TP(mpc_pc_cycle->m_temp_last[HTR_LP_OUT], mpc_pc_cycle->m_pres_last[HTR_LP_OUT], &mpc_pc_cycle->mc_co2_props);
	if (prop_error_code != 0)
	{
		*diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
		return prop_error_code;
	}
	mpc_pc_cycle->m_enth_last[HTR_LP_OUT] = mpc_pc_cycle->mc_co2_props.enth;	//[kJ/kg]
	mpc_pc_cycle->m_entr_last[HTR_LP_OUT] = mpc_pc_cycle->mc_co2_props.entr;	//[kJ/kg-K]
	mpc_pc_cycle->m_dens_last[HTR_LP_OUT] = mpc_pc_cycle->mc_co2_props.dens;	//[kg/m^3]

	// ******************************************************************************
	// ******************************************************************************
	// Solve for the LTR solution
	double T_LTR_LP_out_lower = mpc_pc_cycle->m_temp_last[MC_OUT];		//[K] Coldest possible outlet temperature
	double T_LTR_LP_out_upper = mpc_pc_cycle->m_temp_last[HTR_LP_OUT];	//[K] Hottest possible outlet temperature

	double T_LTR_LP_out_guess_upper = std::min(T_LTR_LP_out_upper, T_LTR_LP_out_lower + 15.0);	//[K] there is nothing magic about using 15 here...
	double T_LTR_LP_out_guess_lower = std::min(T_LTR_LP_out_guess_upper*0.99, T_LTR_LP_out_lower + 2.0);	//[K] there is nothing magic about using 2 here...

	C_MEQ_LTR_des LTR_des_eq(mpc_pc_cycle);
	C_monotonic_eq_solver LTR_des_solver(LTR_des_eq);

	LTR_des_solver.settings(mpc_pc_cycle->ms_des_par.m_tol*mpc_pc_cycle->m_temp_last[MC_IN], 1000, T_LTR_LP_out_lower, T_LTR_LP_out_upper, false);

	double T_LTR_LP_out_solved, tol_T_LTR_LP_out_solved;
	T_LTR_LP_out_solved = tol_T_LTR_LP_out_solved = std::numeric_limits<double>::quiet_NaN();
	int iter_T_LTR_LP_out = -1;

	int T_LTR_LP_out_code = LTR_des_solver.solve(T_LTR_LP_out_guess_lower, T_LTR_LP_out_guess_upper, 0,
		T_LTR_LP_out_solved, tol_T_LTR_LP_out_solved, iter_T_LTR_LP_out);

	if (T_LTR_LP_out_code != C_monotonic_eq_solver::CONVERGED)
		return 31;

	m_Q_dot_LTR = LTR_des_eq.m_Q_dot_LTR;

	// *****************************************************************************
		// Energy balance on the LTR HP stream
	mpc_pc_cycle->m_enth_last[LTR_HP_OUT] = mpc_pc_cycle->m_enth_last[MC_OUT] + m_Q_dot_LTR / mpc_pc_cycle->m_m_dot_mc;	//[kJ/kg]
	prop_error_code = CO2_PH(mpc_pc_cycle->m_pres_last[LTR_HP_OUT], mpc_pc_cycle->m_enth_last[LTR_HP_OUT], &mpc_pc_cycle->mc_co2_props);
	if (prop_error_code != 0)
	{
		*diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
		return prop_error_code;
	}
	mpc_pc_cycle->m_temp_last[LTR_HP_OUT] = mpc_pc_cycle->mc_co2_props.temp;	//[K]
	mpc_pc_cycle->m_entr_last[LTR_HP_OUT] = mpc_pc_cycle->mc_co2_props.entr;	//[kJ/kg-K]
	mpc_pc_cycle->m_dens_last[LTR_HP_OUT] = mpc_pc_cycle->mc_co2_props.dens;	//[kg/m^3]

	// Solve enthalpy balance for the mixer
	if (mpc_pc_cycle->ms_des_par.m_recomp_frac >= 1.E-12)
	{
		mpc_pc_cycle->m_enth_last[MIXER_OUT] = (1.0 - mpc_pc_cycle->ms_des_par.m_recomp_frac)*mpc_pc_cycle->m_enth_last[LTR_HP_OUT] + mpc_pc_cycle->ms_des_par.m_recomp_frac*mpc_pc_cycle->m_enth_last[RC_OUT];	//[kJ/kg]
		prop_error_code = CO2_PH(mpc_pc_cycle->m_pres_last[MIXER_OUT], mpc_pc_cycle->m_enth_last[MIXER_OUT], &mpc_pc_cycle->mc_co2_props);
		if (prop_error_code != 0)
		{
			*diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
			return prop_error_code;
		}
		mpc_pc_cycle->m_temp_last[MIXER_OUT] = mpc_pc_cycle->mc_co2_props.temp;		//[K]
		mpc_pc_cycle->m_entr_last[MIXER_OUT] = mpc_pc_cycle->mc_co2_props.entr;		//[kJ/kg-K]
		mpc_pc_cycle->m_dens_last[MIXER_OUT] = mpc_pc_cycle->mc_co2_props.dens;		//[kg/m^3]
	}
	else
	{
		mpc_pc_cycle->m_temp_last[MIXER_OUT] = mpc_pc_cycle->m_temp_last[LTR_HP_OUT];	//[K]
		mpc_pc_cycle->m_enth_last[MIXER_OUT] = mpc_pc_cycle->m_enth_last[LTR_HP_OUT];	//[kJ/kg]
		mpc_pc_cycle->m_entr_last[MIXER_OUT] = mpc_pc_cycle->m_entr_last[LTR_HP_OUT];	//[kJ/kg-K]
		mpc_pc_cycle->m_dens_last[MIXER_OUT] = mpc_pc_cycle->m_dens_last[LTR_HP_OUT];	//[kg/m^3]
	}

	// Calculate the HTR design performance
	double T_HTR_LP_out_calc = std::numeric_limits<double>::quiet_NaN();

	try
	{
		mpc_pc_cycle->mc_HTR.design_fix_UA_calc_outlet(mpc_pc_cycle->ms_des_par.m_UA_HTR, mpc_pc_cycle->ms_des_par.m_HTR_eff_max,
			mpc_pc_cycle->m_temp_last[MIXER_OUT], mpc_pc_cycle->m_pres_last[MIXER_OUT], mpc_pc_cycle->m_m_dot_t, mpc_pc_cycle->m_pres_last[HTR_HP_OUT],
			mpc_pc_cycle->m_temp_last[TURB_OUT], mpc_pc_cycle->m_pres_last[TURB_OUT], mpc_pc_cycle->m_m_dot_t, mpc_pc_cycle->m_pres_last[HTR_LP_OUT],
			m_Q_dot_HTR, mpc_pc_cycle->m_temp_last[HTR_HP_OUT], T_HTR_LP_out_calc);
	}
	catch (C_csp_exception &)
	{
		*diff_T_HTR_LP_out = std::numeric_limits<double>::quiet_NaN();
		return -1;
	}

	*diff_T_HTR_LP_out = T_HTR_LP_out_calc - mpc_pc_cycle->m_temp_last[HTR_LP_OUT];		//[K]

	return 0;

}

int C_PartialCooling_Cycle::C_MEQ_LTR_des::operator()(double T_LTR_LP_out /*K*/, double *diff_T_LTR_LP_out /*K*/)
{
	m_Q_dot_LTR = std::numeric_limits<double>::quiet_NaN();		//[kWt]
	
	mpc_pc_cycle->m_temp_last[LTR_LP_OUT] = T_LTR_LP_out;		//[K]

	int prop_error_code = CO2_TP(mpc_pc_cycle->m_temp_last[LTR_LP_OUT], mpc_pc_cycle->m_pres_last[LTR_LP_OUT], &mpc_pc_cycle->mc_co2_props);
	if (prop_error_code)
	{
		*diff_T_LTR_LP_out = std::numeric_limits<double>::quiet_NaN();
		return prop_error_code;
	}
	mpc_pc_cycle->m_enth_last[LTR_LP_OUT] = mpc_pc_cycle->mc_co2_props.enth;	//[kJ/kg]
	mpc_pc_cycle->m_entr_last[LTR_LP_OUT] = mpc_pc_cycle->mc_co2_props.entr;	//[kJ/kg-K]
	mpc_pc_cycle->m_dens_last[LTR_LP_OUT] = mpc_pc_cycle->mc_co2_props.dens;	//[kg/m^3]

	double T_LTR_LP_out_calc = std::numeric_limits<double>::quiet_NaN();

	try
	{
		mpc_pc_cycle->mc_LTR.design_fix_UA_calc_outlet(mpc_pc_cycle->ms_des_par.m_UA_LTR, mpc_pc_cycle->ms_des_par.m_LTR_eff_max,
			mpc_pc_cycle->m_temp_last[MC_OUT], mpc_pc_cycle->m_pres_last[MC_OUT], mpc_pc_cycle->m_m_dot_mc, mpc_pc_cycle->m_pres_last[LTR_HP_OUT],
			mpc_pc_cycle->m_temp_last[HTR_LP_OUT], mpc_pc_cycle->m_pres_last[HTR_LP_OUT], mpc_pc_cycle->m_m_dot_t, mpc_pc_cycle->m_pres_last[LTR_LP_OUT],
			m_Q_dot_LTR, mpc_pc_cycle->m_temp_last[LTR_HP_OUT], T_LTR_LP_out_calc);
	}
	catch (C_csp_exception &)
	{
		*diff_T_LTR_LP_out = std::numeric_limits<double>::quiet_NaN();

		return -1;
	}

	*diff_T_LTR_LP_out = T_LTR_LP_out_calc - mpc_pc_cycle->m_temp_last[LTR_LP_OUT];

	return 0;
}
