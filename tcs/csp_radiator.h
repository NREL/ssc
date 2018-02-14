/* ARD This is a new class for radiative cooling panel model. */

#ifndef __csp_solver_radiator_
#define __csp_solver_radiator_

#include "csp_solver_core.h"
#include "csp_solver_util.h"

#include "sam_csp_util.h"

class C_csp_radiator
{
private:
	double UL;
	HTFProperties mc_coldhtf;
	HTFProperties mc_air;
public:

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
		double cp_circ;			//Specific heat capacity of circulating fluid : cp[J / kg - K]
		double th;				//Thickness of plate : th[m]
		double D;				//Diameter of tube : D[m]
		double k_panel;			//Conductivity of plate : k[W / m - K]
		double epsilon;			//Emissivity of plate top surface : epsilon[-]
		double epsilonb;		//Emissivity of plate bottom surface : epsilonb[-]
		double epsilong;		//Emissivity of ground : epsilong[-]
		double Lsec;			//Length of series - connected sections of panels(if single panel, set equal
				//to L) : Lsec[m]

		S_params()
		{
			n = 0;
			m_dot_panel=W=L=L_c=cp_circ=th=D=k_panel=epsilon=epsilonb=epsilong=Lsec= std::numeric_limits<double>::quiet_NaN();
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
