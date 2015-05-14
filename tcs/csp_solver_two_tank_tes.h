#ifndef __csp_solver_two_tank_tes_
#define __csp_solver_two_tank_tes_

#include "csp_solver_core.h"
#include "csp_solver_util.h"

#include "sam_csp_util.h"

#include "storage_hx.h"

class C_csp_two_tank_tes : public C_csp_tes
{
private:

	HTFProperties mc_field_htfProps;		// Instance of HTFProperties class for field HTF
	HTFProperties mc_store_htfProps;		// Instance of HTFProperties class for storage HTF
	Storage_HX mc_hx_storage;				// Instance of Storage_HX class for heat exchanger between storage and field HTFs

	// Member data
	double m_V_tank_active;				//[m^3]

	// member string for exception messages
	std::string error_msg;

	// Stored data
	double m_V_tank_hot_prev;			//[m^3]
	double m_T_tank_hot_prev;			//[K]
	double m_V_tank_cold_prev;			//[m^3]
	double m_T_tank_cold_prev;			//[K]	
	double m_m_tank_hot_prev;			//[kg]
	double m_m_tank_cold_prev;			//[kg]

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

		double m_q_pb_design;		//[MW] thermal power to power cycle at design; convert to W in init()
		double m_solarm;			//[-] solar multiple
		double m_ts_hours;			//[hr] hours of storage at design power cycle operation
		double m_vol_tank;			//[m^3] 
		double m_h_tank;			//[m]
		double m_u_tank;			//[W/m^2-K]
		int m_tank_pairs;			//[-]
		double m_hot_tank_Thtr;		//[C] convert to K in init()
		double m_cold_tank_Thtr;	//[C] convert to K in init()
		double m_tank_max_heat;		//[MW]
		double m_dt_hot;			//[C] Temperature difference across heat exchanger - assume hot and cold deltaTs are equal
		double m_T_field_in_des;	//[C] convert to K in init()
		double m_T_field_out_des;	//[C] convert to K in init()
		double m_V_tank_hot_ini;	//[m^3] Initial volume in hot storage tank
		double m_T_tank_hot_ini;	//[C] Initial temperature in hot storage tank
		double m_T_tank_cold_ini;	//[C] Initial temperature in cold storage cold
		double m_h_tank_min;		//[m] Minimum allowable HTF height in storage tank

		S_params()
		{
			m_field_fl = m_tes_fl = m_is_hx = -1;		
		}
	};

	S_params ms_params;

	C_csp_two_tank_tes();

	~C_csp_two_tank_tes(){};

	virtual void init();

};




#endif   //__csp_solver_two_tank_tes_