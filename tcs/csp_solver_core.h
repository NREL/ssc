#ifndef __csp_solver_core_
#define __csp_solver_core_

#include "lib_weatherfile.h"
#include "csp_solver_util.h"

class C_csp_solver_htf_state
{
public:
	double m_m_dot;		//[kg/hr]
	double m_temp_in;	//[C]
	double m_pres_in;	//[kPa]
	double m_qual_in;	//[-]
	double m_temp_out;	//[C]
	double m_pres_out;	//[kPa]
	double m_qual_out;	//[-]

	C_csp_solver_htf_state()
	{
		m_m_dot = 
			m_temp_in = m_pres_in = m_qual_in = 
			m_temp_out = m_pres_out = m_qual_out = std::numeric_limits<double>::quiet_NaN();
	}
};

class C_csp_solver_sim_info
{
public:
	double m_time;		//[s]
	double m_step;		//[s]
	int m_ncall;		//[-]

	C_csp_solver_sim_info()
	{
		m_time = m_step = std::numeric_limits<double>::quiet_NaN();
		m_ncall = -1;
	}
};

class C_csp_collector_receiver
{

public:
	C_csp_collector_receiver(){};

	~C_csp_collector_receiver(){};

	virtual void init() = 0;
};

class C_csp_power_cycle
{

public:
	C_csp_power_cycle(){};

	~C_csp_power_cycle(){};

	virtual void init() = 0;

	struct S_control_inputs
	{
		int m_standby_control;		//[-] Control signal indicating standby mode
		int m_tou;					//[-] Time-of-use period: ONE BASED, converted to 0-based in code

		S_control_inputs()
		{
			m_standby_control = m_tou = -1;
		}
	};

};

class C_csp_weatherreader
{
private:
	weatherfile m_wf;
	bool m_first;		// flag to indicate whether this is the first call

	// member string for exception messages
	std::string m_error_msg;

public:
	C_csp_weatherreader();

	~C_csp_weatherreader(){};

	void init();

	void timestep_call(const C_csp_solver_sim_info *p_sim_info);

	// Class to save messages for up stream classes
	C_csp_messages mc_csp_messages;

	struct S_outputs
	{
		int m_year;				//[yr]
		int m_month;			//[mn]
		int m_day;				//[day]
		int m_hour;				//[hr]
		double m_minute;		//[min]

		double m_global;		//[W/m2]
		double m_beam;			//[W/m2]
		double m_diffuse;		//[W/m2]
		double m_tdry;			//[C]
		double m_twet;			//[C]
		double m_tdew;			//[C]
		double m_wspd;			//[m/s]
		double m_wdir;			//[deg]
		double m_rhum;			//[%]
		double m_pres;			//[mbar]
		double m_snow;			//[cm]
		double m_albedo;		//[-] (0..1)

		double m_poa;			//[W/m2]
		double m_solazi;		//[deg]
		double m_solzen;		//[deg]
		double m_lat;			//[deg]
		double m_lon;			//[deg]
		double m_tz;			//[deg]
		double m_shift;			//[deg]
		double m_elev;			//[m]

		S_outputs()
		{
			m_year = m_month = m_day = m_hour = -1;

			m_global = m_beam = m_diffuse = m_tdry = m_twet = m_tdew = m_wspd = m_wdir = m_rhum = m_pres = m_snow = m_albedo =
				m_poa = m_solazi = m_solzen = m_lat = m_lon = m_tz = m_shift = m_elev = std::numeric_limits<double>::quiet_NaN();
		}
	};

	// Member data - public so can be set from up stream code
	std::string m_filename;
	int m_trackmode;
	double m_tilt;
	double m_azimuth;
	
	S_outputs ms_outputs;
};





#endif //__csp_solver_core_