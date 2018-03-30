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
	int T_EG30[1][71] = { -10, -9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60 };	// Temperatures [C] for EG properties
	double cp_EG30[1][71] = { 3.627,3.63,3.633,3.636,3.64,3.643,3.646,3.649,3.652,3.655,3.658,3.661,3.664,3.667,3.67,3.673,3.676,3.679,3.682,3.685,3.689,3.692,3.695,3.698,3.7,3.703,3.706,3.709,3.712,3.715,3.718,3.721,3.724,3.727,3.73,3.733,3.736,3.739,3.741,3.744,3.747,3.75,3.753,3.756,3.759,3.761,3.764,3.767,3.77,3.773,3.775,3.778,3.781,3.784,3.786,3.789,3.792,3.794,3.797,3.8,3.803,3.805,3.808,3.811,3.813,3.816,3.818,3.821,3.824,3.826,3.829 };		//Specific heat [kJ/kg-K] of ethyene glycol 30% by mass from -10 C to 60 C
	double rho_EG30[1][71] = { 1047,1047,1047,1047,1047,1046,1046,1046,1046,1045,1045,1045,1044,1044,1044,1043,1043,1043,1042,1042,1042,1041,1041,1041,1040,1040,1040,1039,1039,1038,1038,1038,1037,1037,1036,1036,1036,1035,1035,1034,1034,1033,1033,1032,1032,1031,1031,1030,1030,1029,1029,1028,1028,1027,1027,1026,1026,1025,1025,1024,1023,1023,1022,1022,1021,1020,1020,1019,1019,1018,1017 };	//Density [kg/m^3]
	double mu_EG30[1][71] = { 0.006508,0.006228,0.005964,0.005715,0.005478,0.005254,0.005042,0.004841,0.00465,0.004469,0.004298,0.004135,0.00398,0.003832,0.003692,0.003559,0.003433,0.003312,0.003197,0.003087,0.002983,0.002883,0.002788,0.002698,0.002611,0.002529,0.002449,0.002374,0.002302,0.002233,0.002166,0.002103,0.002042,0.001984,0.001929,0.001875,0.001824,0.001775,0.001728,0.001682,0.001639,0.001597,0.001557,0.001518,0.001481,0.001445,0.001411,0.001378,0.001346,0.001315,0.001286,0.001257,0.001229,0.001203,0.001177,0.001153,0.001129,0.001106,0.001083,0.001062,0.001041,0.001021,0.001001,0.0009824,0.0009642,0.0009465,0.0009294,0.0009128,0.0008967,0.0008812,0.000866 };		//Viscosity [kg/m-sec]
	double alpha_EG30[1][71] = { 1.15E-07,1.15E-07,1.15E-07,1.15E-07,1.16E-07,1.16E-07,1.16E-07,1.16E-07,1.16E-07,1.17E-07,1.17E-07,1.17E-07,1.17E-07,1.17E-07,1.17E-07,1.18E-07,1.18E-07,1.18E-07,1.18E-07,1.18E-07,1.19E-07,1.19E-07,1.19E-07,1.19E-07,1.19E-07,1.20E-07,1.20E-07,1.20E-07,1.20E-07,1.20E-07,1.20E-07,1.21E-07,1.21E-07,1.21E-07,1.21E-07,1.21E-07,1.22E-07,1.22E-07,1.22E-07,1.22E-07,1.22E-07,1.23E-07,1.23E-07,1.23E-07,1.23E-07,1.23E-07,1.24E-07,1.24E-07,1.24E-07,1.24E-07,1.24E-07,1.25E-07,1.25E-07,1.25E-07,1.25E-07,1.25E-07,1.26E-07,1.26E-07,1.26E-07,1.26E-07,1.26E-07,1.27E-07,1.27E-07,1.27E-07,1.27E-07,1.27E-07,1.28E-07,1.28E-07,1.28E-07,1.28E-07,1.28E-07 };	//Thermal diffusivity [m^2/sec]
	double k_EG30[1][71] = {0.4362, 0.4371, 0.4381, 0.4391, 0.4401, 0.4411, 0.442, 0.443, 0.444, 0.445, 0.4459, 0.4469, 0.4479, 0.4488, 0.4498, 0.4507, 0.4517, 0.4527, 0.4536, 0.4546, 0.4555, 0.4565, 0.4574, 0.4583, 0.4593, 0.4602, 0.4612, 0.4621, 0.463, 0.464, 0.4649, 0.4658, 0.4668, 0.4677, 0.4686, 0.4695, 0.4704, 0.4713, 0.4723, 0.4732, 0.4741, 0.475, 0.4759, 0.4768, 0.4777, 0.4786, 0.4795, 0.4804, 0.4813, 0.4821, 0.483, 0.4839, 0.4848, 0.4857, 0.4865, 0.4874, 0.4883, 0.4891, 0.49, 0.4909, 0.4917, 0.4926, 0.4934, 0.4943, 0.4951, 0.496, 0.4968, 0.4977, 0.4985, 0.4994, 0.5002}; //Thermal conductivity [W/m-K]

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
		double Afield, RM, Asolar_refl;
		int Np;
		double epsilon_HX;
		S_params()
		{
			n = Np= 0;
			m_dot_panel = W = L = L_c = th = D = k_panel = epsilon = epsilonb = epsilong = Lsec = m_night_hrs = Afield = RM = epsilon_HX = Asolar_refl = std::numeric_limits<double>::quiet_NaN();
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

	void analytical_panel_calc_HX(double T_db /*K*/, double T_rad_in /*K*/, double Tp_est /*K*/, double u /*m/s*/, double T_s /*K*/, double m_dot_rad /*K*/,
		//outputs
		double &T_rad_out /*K*/, double &T_p /*K*/);

};

#endif //__csp_solver_radiator_
