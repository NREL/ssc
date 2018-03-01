/* ARD This is a new class for radiative cooling panel model. */

#ifndef __csp_solver_radiator_
#define __csp_solver_radiator_

#include "csp_solver_core.h"
#include "csp_solver_util.h"

#include "sam_csp_util.h"
#include "water_properties.h"

class C_csp_radiator
{
private:

	water_state mc_coldhtf;
	HTFProperties mc_air;


public:

	double T_S_measured[8760] = {};	//measured sky temperature [K], initially zeros.
	int T_S_localhr[8760] = {};	//local time in hours for measured sky temp, initially zeros.
	double T_S_time[8760] = {};		//time in seconds at end of timestep for measured sky temp, initially zeros.

	struct S_params
	{
		int m_field_fl;
		util::matrix_t<double> m_field_fl_props;
		double m_dot_panel;		//Total mass flow rate through panel : m_dot[kg / sec]
		int n;					//Number of parallel tubes on a single panel : n
		double W;				//Distance between two parallel tubes : W[m]
		double L;				//Length of tubes : L[m]
		double L_c;				//Characteristic length for forced convection, typically equal to n*W
			//unless wind direction is known to determine flow path : Lc[m]
		double th;				//Thickness of plate : th[m]
		double D;				//Diameter of tube : D[m]
		double k_panel;			//Conductivity of plate : k[W / m - K]
		double epsilon;			//Emissivity of plate top surface : epsilon[-]
		double epsilonb;		//Emissivity of plate bottom surface : epsilonb[-]
		double epsilong;		//Emissivity of ground : epsilong[-]
		double Lsec;			//Length of series - connected sections of panels(if single panel, set equal to L) : Lsec[m]
		double m_night_hrs;		//Number of hours plant will run at summer peak
		double m_power_hrs;		//Number of hours plant operates in one day at summer peak
		double Afield, RM;
		int Np;
		S_params()
		{
			n = Np= 0;
			m_dot_panel=W=L=L_c=th=D=k_panel=epsilon=epsilonb=epsilong=Lsec=m_night_hrs=Afield=RM= std::numeric_limits<double>::quiet_NaN();
		}
	};

	S_params ms_params;

	C_csp_radiator();

	void init();

	void night_cool(double T_db /*K*/, double T_rad_in /*K*/, double u /*m/s*/, double T_s /*K*/, double m_dot_rad /*K*/,
		//outputs
		double &T_rad_out /*K*/);

	void analytical_panel_calc(double T_db /*K*/, double T_rad_in /*K*/, double Tp_est /*K*/, double u /*m/s*/, double T_s /*K*/, double m_dot_rad /*K*/,
		//outputs
		double &T_rad_out /*K*/,double &T_p /*K*/);

};

#endif //__csp_solver_radiator_
