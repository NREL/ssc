#ifndef __SCO2_CYCLE_TEMPLATES_
#define __SCO2_CYCLE_TEMPLATES_

#include "sco2_cycle_components.h"
#include "heat_exchangers.h"

class C_sco2_cycle_core
{
public:

	enum E_cycle_state_points
	{
		// index values for c++ 0-based vectors for temperature, pressure, etc.
		MC_IN = 0,		// Main compressor inlet
		MC_OUT,			// Main compressor outlet
		LTR_HP_OUT,		// Low temp recuperator high pressure outlet
		MIXER_OUT,		// Mixer: LTR_HP_OUT + Recompressor outlet
		HTR_HP_OUT,		// High temp recuperator high pressure outlet
		TURB_IN,		// Turbine inlet
		TURB_OUT,		// Turbine outlet
		HTR_LP_OUT,		// High temp recuperator low pressure outlet
		LTR_LP_OUT,		// Low temp recuperator low pressure outlet
		RC_OUT,			// Recompresor outlet
		PC_IN,			// Precompressor inlet (partial cooling cycle)
		PC_OUT,			// Precompressor outlet (partial cooling cycle)

		END_SCO2_STATES
	};

	struct S_design_solved
	{
		std::vector<double> m_temp, m_pres, m_enth, m_entr, m_dens;		// thermodynamic states (K, kPa, kJ/kg, kJ/kg-K, kg/m3)
		double m_eta_thermal;	//[-]
		double m_W_dot_net;		//[kWe]
		double m_m_dot_mc;		//[kg/s]
		double m_m_dot_rc;		//[kg/s]
		double m_m_dot_pc;		//[kg/s]
		double m_m_dot_t;		//[kg/s]
		double m_recomp_frac;	//[-]
		double m_UA_LTR;			//[kW/K]
		double m_UA_HTR;			//[kW/K]

		bool m_is_rc;

		C_comp_multi_stage::S_des_solved ms_mc_ms_des_solved;
		C_comp_multi_stage::S_des_solved ms_rc_ms_des_solved;
		C_comp_multi_stage::S_des_solved ms_pc_ms_des_solved;
		C_turbine::S_design_solved ms_t_des_solved;
		C_HX_counterflow::S_des_solved ms_LTR_des_solved;
		C_HX_counterflow::S_des_solved ms_HTR_des_solved;

		S_design_solved()
		{
			m_eta_thermal = m_W_dot_net = m_m_dot_mc = m_m_dot_rc = m_m_dot_t = m_recomp_frac =
				m_UA_LTR = m_UA_HTR = std::numeric_limits<double>::quiet_NaN();

			m_is_rc = true;
		}
	};
	
protected:

	S_design_solved ms_des_solved;

public:

	const S_design_solved * get_design_solved()
	{
		return &ms_des_solved;
	}


};


#endif // !__SCO2_CYCLE_TEMPLATES_
