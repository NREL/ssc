#ifndef __csp_solver_core_
#define __csp_solver_core_

#include "lib_weatherfile.h"
#include "csp_solver_util.h"

class C_csp_collector_receiver
{

public:
	C_csp_collector_receiver(){};

	~C_csp_collector_receiver(){};

	virtual void init() = 0;
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

	void timestep_call(double time, double step, int ncall);

	// Class to save messages for up stream classes
	C_csp_messages mc_csp_messages;

	struct S_outputs
	{
		int m_year;
		int m_month;
		int m_day;
		int m_hour;
		double m_minute;

		double m_global;
		double m_beam;
		double m_diffuse;
		double m_tdry;
		double m_twet;
		double m_tdew;
		double m_wspd;
		double m_wdir;
		double m_rhum;
		double m_pres;
		double m_snow;
		double m_albedo;

		double m_poa;
		double m_solazi;
		double m_solzen;
		double m_lat;
		double m_lon;
		double m_tz;
		double m_shift;
		double m_elev;

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