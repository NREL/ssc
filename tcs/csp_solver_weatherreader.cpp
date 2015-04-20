#include "csp_solver_core.h"
#include "csp_solver_util.h"
#include "sam_csp_util.h"

#include <shared/lib_weatherfile.h>
#include <shared/lib_irradproc.h>

C_csp_weatherreader::C_csp_weatherreader()
{
	m_filename = "";
	m_trackmode = -1;
	m_tilt = std::numeric_limits<double>::quiet_NaN();
	m_azimuth = std::numeric_limits<double>::quiet_NaN();

	m_ncall = -1;
}


void C_csp_weatherreader::init()
{
	if( !m_wf.open(m_filename) )
	{
		m_error_msg = util::format("Could not open %s for reading", m_filename);
		throw(C_csp_exception(m_error_msg, ""));
	}
	m_first = true;		// True the first time call() is accessed

	if(m_trackmode < 0 || m_trackmode > 2)
	{
		m_error_msg = util::format("invalid tracking mode specified %d [0..2]", m_trackmode);
		throw(C_csp_exception(m_error_msg, ""));
	}
}


void C_csp_weatherreader::timestep_call(const C_csp_solver_sim_info &p_sim_info)
{
	// Increase call-per-timestep counter
	// Converge() sets it to -1, so on first call this line will adjust it = 0
	m_ncall++;
	
	double time = p_sim_info.m_time;
	double step = p_sim_info.m_step;
	//int ncall = p_sim_info->m_ncall;

	if( m_ncall == 0 ) // only read data values once per timestep
	{
		//If the start time does not correspond to the first record in the weather file, loop to the correct record
		int nread = 1;
		if( m_first )
		{
			nread = (int)time / step;
			m_first = false;
		}

		for( int i = 0; i<nread; i++ )		//for all calls except the first, nread=1
		{
			if( !m_wf.read() )
			{
				m_error_msg = util::format("failed to read from weather file %s at time %lg", m_wf.filename().c_str(), time);
				throw(C_csp_exception(m_error_msg, ""));
			}
		}
	}

	double sunn[9], angle[5], poa[3], diffc[3];

	poa[0] = poa[1] = poa[2] = 0;
	angle[0] = angle[1] = angle[2] = angle[3] = angle[4] = 0;
	diffc[0] = diffc[1] = diffc[2] = 0;

	solarpos(m_wf.year, m_wf.month, m_wf.day, m_wf.hour, m_wf.minute,
		m_wf.lat, m_wf.lon, m_wf.tz, sunn);

	if( sunn[2] > 0.0087 )
	{
		/* sun elevation > 0.5 degrees */
		incidence(m_trackmode, m_tilt, m_azimuth, 45.0, sunn[1], sunn[0], 0, 0, angle);
		perez(sunn[8], m_wf.dn, m_wf.df, 0.2, angle[0], angle[1], sunn[1], poa, diffc);
	}
	
	ms_outputs.m_year = m_wf.year;
	ms_outputs.m_month = m_wf.month;
	ms_outputs.m_day = m_wf.day;
	ms_outputs.m_hour = m_wf.hour;
	ms_outputs.m_minute = m_wf.minute;

	ms_outputs.m_global = m_wf.gh;
	ms_outputs.m_beam = m_wf.dn;
	ms_outputs.m_diffuse = m_wf.df;
	ms_outputs.m_tdry = m_wf.tdry;
	ms_outputs.m_twet = m_wf.twet;
	ms_outputs.m_tdew = m_wf.tdew;
	ms_outputs.m_wspd = m_wf.wspd;
	ms_outputs.m_wdir = m_wf.wdir;
	ms_outputs.m_rhum = m_wf.rhum;
	ms_outputs.m_pres = m_wf.pres;
	ms_outputs.m_snow = m_wf.snow;
	ms_outputs.m_albedo = m_wf.albedo;

	ms_outputs.m_poa = poa[0] + poa[1] + poa[2];
	ms_outputs.m_solazi = sunn[0] * 180 / CSP::pi;
	ms_outputs.m_solzen = sunn[1] * 180 / CSP::pi;
	ms_outputs.m_lat = m_wf.lat;
	ms_outputs.m_lon = m_wf.lon;
	ms_outputs.m_tz = m_wf.tz;
	ms_outputs.m_shift = (m_wf.lon - m_wf.tz*15.0);
	ms_outputs.m_elev = m_wf.elev;

}

void C_csp_weatherreader::converged()
{
	m_ncall = -1;
}