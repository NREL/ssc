#include "compact_hx_discretized.h"
#include <math.h>
#include <limits>
#include <algorithm>

#include "htf_props.h"
#include "CO2_properties.h"
#include <shared/lib_util.h>
#include "sam_csp_util.h"

using namespace std;

bool get_compact_hx_geom(int enum_compact_hx_config, double & d_out, double & fin_pitch, double & D_h,
	double & fin_thk, double & sigma, double & alpha, double & A_fin_to_surf,
	double & s_h, double & s_v)
{

	switch(enum_compact_hx_config)
	{
	case fc_tubes_s80_38T:
		d_out = 0.0102;		//[m] Outer tube diameter
		fin_pitch = 315;	//[1/m] Number of fins per meter of tube
		D_h = 0.003632;		//[m] Hydraulic diameter of air side
		fin_thk = 0.0003302;//[m] Fin thickness
		sigma = 0.534;		//[-] Ratio of free-flow to frontal area
		alpha = 587;		//[1/m] Ratio of gas-side heat transfer area to core volume
		A_fin_to_surf = 0.913;	//[-] Ratio of finned to total surface area on gas-side
		s_h = 0.022;		//[m] Distance between tubes in air flow direction
		s_v = 0.0254;		//[m] Distance between tubes perpendicular to air flow direction

		return true;

	case fc_tubes_sCF_88_10Jb:
		d_out = 0.02601;		//[m] Outer tube diameter
		fin_pitch = 346;	//[1/m] Number of fins per meter of tube
		D_h = 0.01321;		//[m] Hydraulic diameter of air side
		fin_thk = 0.000305;//[m] Fin thickness
		sigma = 0.642;		//[-] Ratio of free-flow to frontal area
		alpha = 191;		//[1/m] Ratio of gas-side heat transfer area to core volume
		A_fin_to_surf = 0.825;	//[-] Ratio of finned to total surface area on gas-side
		s_h = 0.0524;		//[m] Distance between tubes in air flow direction
		s_v = 0.07818;		//[m] Distance between tubes perpendicular to air flow direction

		return true;
	
	default:
		return false;
	}

};

bool get_compact_hx_f_j(int enum_compact_hx_config, double Re, double & f, double & j_H)
{
	double Re_mm = max(0.001, Re*10e-3);

	switch( enum_compact_hx_config )
	{
	case fc_tubes_s80_38T:
		f = 0.02949346*pow(Re_mm, -0.208110211);
		j_H = 0.0105331507*pow(Re_mm, -0.400092073);
		return true;

	case fc_tubes_sCF_88_10Jb:
		f = 0.0606753986*pow(Re_mm, -0.256298233);
		j_H = 0.0148711552*pow(Re_mm, -0.382144871);
		return true;

	default:
		return false;
	}

};

compact_hx::compact_hx()
{
	m_th = m_eta_fan = m_roughness = 
		m_d_in = m_A_cs = m_relRough = m_Depth = m_W_par = m_N_par = m_N_tubes = m_L_tube = m_L_path = m_A_surf_total = m_UA_total = m_V_total =
		m_T_amb_des = m_P_amb_des =
		m_T_hot_in_des = m_P_hot_in_des = m_m_dot_total = m_W_dot_fan_des = m_delta_P_des = m_T_hot_out_des = m_P_hot_out_des = 
		m_d_out = m_fin_pitch = m_D_h = m_fin_thk = m_sigma = m_alpha = m_A_fin_to_surf = m_s_h = m_s_v = numeric_limits<double>::quiet_NaN();

	m_N_loops = m_N_nodes = m_enum_compact_hx_config = -1;
}

bool compact_hx::design_hx(double T_amb_K, double P_amb_Pa, double T_hot_in_K, double P_hot_in_kPa, 
	double m_dot_hot_kg_s, 	double W_dot_fan_MW, double deltaP_kPa, double T_hot_out_K)
{
	// double T_amb_K, double P_amb_Pa, double T_hot_in_K, double P_hot_in_kPa, double m_dot_hot_kg_s
	// double W_dot_fan_MW, double deltaP_kPa, double T_hot_out_K

	//m_enum_compact_hx_config = fc_tubes_s80_38T;
	m_enum_compact_hx_config = fc_tubes_sCF_88_10Jb;

	// Get HX Geometry
	get_compact_hx_geom(m_enum_compact_hx_config, m_d_out, m_fin_pitch, m_D_h, m_fin_thk,
		m_sigma, m_alpha, m_A_fin_to_surf, m_s_h, m_s_v);

	// Thickness should really be tied to HX config
	//m_th = 0.001;		//fc_tubes_s80-38T
	m_th = 0.0024;		//fc_tubes_sCF-88-10Jb

	// Get Remaining Design Info: hardcode for now, but eventually will be inputs
		// Air-Cooler Specs
	m_N_loops = 3;
	m_N_nodes = 5;
	m_eta_fan = 0.5;
	m_d_in = m_d_out - 2 * m_th;
	m_roughness = 4.5E-5;					//[m] absolute roughness of material
	m_A_cs = 0.25*CSP::pi*pow(m_d_in, 2);	//[m2] flow cross-section area
	m_relRough = m_roughness / m_d_in;		//[-] Relative Roughness

	/* Set These With Function Arguments
		// Design Ambient Conditions
	m_T_amb_des = 32.0 + 273.15;		//[K] Air Prop routine needs K
	m_P_amb_des = 101325.0;				//[Pa] Air prop routine needs Pa

		// Hot-side Inlet Conditions
	m_T_hot_in_des = 100.0 + 273.15;	//[K] Other htf props routines need K
	m_P_hot_in_des = 8000.0;			//[kPa] CO2 props need kPa
	m_m_dot_total = 938.9;				//[kg/s] Total sCO2 mass flow into air-cooler

		// Design Performance Targets
	m_W_dot_fan_des = 0.35;				//[MW]
	m_delta_P_des = 62.5;				//[kPa]
	m_T_hot_out_des = 48.0 + 273.15;	//[K]
	*/
	m_T_amb_des = T_amb_K;
	m_P_amb_des = P_amb_Pa;

	m_T_hot_in_des = T_hot_in_K;
	m_P_hot_in_des = P_hot_in_kPa;
	m_m_dot_total = m_dot_hot_kg_s;

	m_W_dot_fan_des = W_dot_fan_MW;
	m_delta_P_des = deltaP_kPa;
	m_T_hot_out_des = T_hot_out_K;

	m_P_hot_out_des = m_P_hot_in_des - m_delta_P_des;
	double P_hot_ave = 0.5*(m_P_hot_out_des + m_P_hot_in_des);
		// Set up 'matrix_t's for temperature and pressure
		// Using index 1 for m_N_nodes, so 0 index remains undefined
		// Also, each node requires inlet&outlet temp, so in total, m_N_nodes + 2 required
	util::matrix_t<double>    T_co2(m_N_nodes+2, m_N_loops + 1);
	util::matrix_t<double>    P_co2(m_N_nodes+2, m_N_loops + 1);
	util::matrix_t<double>    T_air(m_N_nodes+2, m_N_loops + 1);
	T_co2.fill(numeric_limits<double>::quiet_NaN());
	P_co2.fill(numeric_limits<double>::quiet_NaN());
	T_air.fill(numeric_limits<double>::quiet_NaN());

	// index that gives outlet temperatur and pressure: depends on whether odd or even loops
	double final_outlet_index = ((m_N_loops + 2) % 2)*m_N_nodes + 1;

	HTFProperties air;				// Instance of HTFProperties class for ambient air
	air.SetFluid(air.Air);
		// Assume air props don't change significantly in air cooler
	double mu_air = air.visc(m_T_amb_des);
	double v_air = 1.0/air.dens(m_T_amb_des,m_P_amb_des);
	double cp_air = air.Cp(m_T_amb_des)*1000.0;
	double k_air = air.cond(m_T_amb_des);
	double Pr_air = (cp_air*mu_air/k_air);

		// Calculate the required heat rejection
	CO2_state co2_props;
	CO2_TP(m_T_hot_in_des, P_hot_ave, &co2_props);
	double h_in_des = co2_props.enth*1000.0;					//[J/kg]
	CO2_TP(m_T_hot_out_des, m_P_hot_in_des, &co2_props);
	double h_out_des = co2_props.enth*1000.0;					//[J/kg]
	double Q_dot_des = m_m_dot_total*(h_in_des - h_out_des);	//[W]
	double deltaT_hot = m_T_hot_in_des - m_T_hot_out_des;		//[K,C] Hot side temperature difference

	m_Depth = m_s_h * m_N_loops;	//[m] Dimension parallel to air flow

	// 1) Guess dimension perpendicular to air AND hot fluid flow
			// (basically the number of parallel flow paths)
		
	// ********************************************************************************
	// ** Set up guesses and control for bisection and false-position **
	// ** Try to get better guess by estimating length required to hit pressure drop **
	// ********************************************************************************
	double T_co2_deltaP_eval = 0.75*m_T_hot_in_des + 0.25*m_T_hot_out_des;
	CO2_TP(T_co2_deltaP_eval, m_P_hot_in_des, &co2_props);
	double visc_dyn_co2_g = CO2_visc(co2_props.dens, co2_props.temp)*1.E-6;

	// Just try hitting a "reasonable" Reynolds number?
	// This sets the mass flow rate in the tube, which then sets the number of required tubes
	//  to contain the defined mass flow rate
		//double Re_co2_g = m_dot_tube*m_d_in / (m_A_cs*visc_dyn_co2_g);
	double Re_g = 5.E6;		//[-] A "reasonable" Reynolds number
	double m_dot_tube_g1 = Re_g*m_A_cs*visc_dyn_co2_g/m_d_in;	//[kg/s] Mass flow rate to achieve Reynolds number
	
	double N_par_g = m_m_dot_total/m_dot_tube_g1;	//[-] Number of parallel flow paths required to contain all mass flow
	
	double W_par = N_par_g * m_s_v;		//[m] Dimension perpendicular to air AND hot fluid flow... parallel paths dimension

	//**********************************************************************************
	//**********************************************************************************
	//**********************************************************************************
		// Overwrite here to test against EES code
		W_par = 171.8;
		// ***************************************
	//**********************************************************************************

	double tol = 0.001;		//[-] Relative tolerance for convergence
	double diff_T_hot_out = 2.0*tol;	//[-] Set diff > tol
	int iter_W_par = 0;

	bool is_lowbound_W_par = false;
	double x_lower_W_par = numeric_limits<double>::quiet_NaN();
	bool is_lowdiff_W_par = false;
	double y_lower_W_par = numeric_limits<double>::quiet_NaN();

	bool is_upbound_W_par = false;
	double x_upper_W_par = numeric_limits<double>::quiet_NaN();
	bool is_updiff_W_par = false;
	double y_upper_W_par = numeric_limits<double>::quiet_NaN();

	// Values defined in inner nests that are need at this level
	double L_tube = numeric_limits<double>::quiet_NaN();
	double h_conv_air = numeric_limits<double>::quiet_NaN();
	double V_total = numeric_limits<double>::quiet_NaN();
	double N_par = numeric_limits<double>::quiet_NaN();
	double N_tubes = numeric_limits<double>::quiet_NaN();

	while( abs(diff_T_hot_out) > tol )
	{
		iter_W_par++;		// Increase iteration counter

		// Guess new W_parallel!
		if(iter_W_par > 1)
		{
			if( diff_T_hot_out > 0.0 )	// Calculated inlet temperature is too high, decrease length
			{
				is_upbound_W_par = true;
				x_upper_W_par = W_par;
				y_upper_W_par = diff_T_hot_out;
				if( is_lowbound_W_par )
				{
					W_par = -y_upper_W_par*(x_lower_W_par - x_upper_W_par) / (y_lower_W_par - y_upper_W_par) + x_upper_W_par;
				}
				else
				{
					W_par *= 0.5;
				}
			}
			else						// Calculated inlet tempearture is too low, increase length
			{
				is_lowbound_W_par = true;
				x_lower_W_par = W_par;
				y_lower_W_par = diff_T_hot_out;
				if( is_upbound_W_par )
				{
					W_par = -y_upper_W_par*(x_lower_W_par - x_upper_W_par) / (y_lower_W_par - y_upper_W_par) + x_upper_W_par;
				}
				else
				{
					W_par *= 2.0;
				}
			}
		}

		// Divide by the distance between tubes to get number of parallel units
		// This is number of tube connected to the inlet headers
		// Can be any positive rational number so a continuous solution space is available
		N_par = W_par / m_s_v;		
		N_tubes = N_par*m_N_loops;
				/// Can now calculate the mass flow rate per tube
		double m_dot_tube = m_m_dot_total/N_par;

		// 2) Guess the length of the hot side tube for one pass/loop
		// ********************************************************************************
		// ** Try to estimate length required to hit pressure drop **
		// ********************************************************************************
		CO2_TP(T_co2_deltaP_eval, P_hot_ave, &co2_props);
		double visc_dyn_co2_g = CO2_visc(co2_props.dens, co2_props.temp)*1.E-6;
		double Re_co2_g = m_dot_tube*m_d_in / (m_A_cs*visc_dyn_co2_g);

		double rho_co2_g = co2_props.dens;
		double visc_kin_co2_g = visc_dyn_co2_g / rho_co2_g;
		double cond_co2_g = CO2_cond(co2_props.dens, co2_props.temp);
		double specheat_co2_g = co2_props.cp*1000.0;
		double alpha_co2_g = cond_co2_g / (specheat_co2_g*rho_co2_g);
		double Pr_co2 = visc_kin_co2_g / alpha_co2_g;

		double Nusselt_co2_g = -999.9;
		double f_co2_g = -999.9;

		// Specifying the length over diameter = 1000 sets the problem as Fully Developed Flow
		CSP::PipeFlow(Re_co2_g, Pr_co2, 1000.0, m_relRough, Nusselt_co2_g, f_co2_g);

		double u_m = m_dot_tube / (rho_co2_g*m_A_cs);
			//m_delta_P_des*1000.0 = f_co2_g*L_node*rho_co2_g*pow(u_m,2)/(2.0*m_d_in)
		L_tube = m_delta_P_des*1000.0*(2.0*m_d_in)/(f_co2_g*rho_co2_g*pow(u_m,2))/m_N_loops;
		//**********************************************************************************
		//**********************************************************************************
		//**********************************************************************************
			// Overwrite here to test against EES code
		L_tube = 8.871;
			// ***************************************
		//**********************************************************************************

		// Want to set increasingly tight tolerances to help convergence of outer loops
		double tol_L_tube = tol/5.0;
		double diff_deltaP = 2.0*tol_L_tube;
		int iter_L_tube = 0;

		bool is_lowbound_L_tube = false;
		double x_lower_L_tube = numeric_limits<double>::quiet_NaN();
		bool is_lowdiff_L_tube = false;
		double y_lower_L_tube = numeric_limits<double>::quiet_NaN();

		bool is_upbound_L_tube = false;
		double x_upper_L_tube = numeric_limits<double>::quiet_NaN();
		bool is_updiff_L_tube = false;
		double y_upper_L_tube = numeric_limits<double>::quiet_NaN();

		while( abs(diff_deltaP) > tol_L_tube )
		{
			iter_L_tube++;
			
			if(iter_L_tube > 1)
			{
				if( diff_deltaP > 0.0 )	// Calculated pressure drop too high - decrease length
				{
					is_upbound_L_tube = true;
					x_upper_L_tube = L_tube;
					y_upper_L_tube = diff_deltaP;
					if( is_lowbound_L_tube )
					{
						L_tube = -y_upper_L_tube*(x_lower_L_tube - x_upper_L_tube) / (y_lower_L_tube - y_upper_L_tube) + x_upper_L_tube;
					}
					else
					{
						L_tube *= 0.5;
					}
				}
				else						// Calculated pressure drop too low - increase length
				{
					is_lowbound_L_tube = true;
					x_lower_L_tube = L_tube;
					y_lower_L_tube = diff_deltaP;
					if( is_upbound_L_tube )
					{
						L_tube = -y_upper_L_tube*(x_lower_L_tube - x_upper_L_tube) / (y_lower_L_tube - y_upper_L_tube) + x_upper_L_tube;
					}
					else
					{
						L_tube *= 2.0;
					}
				}
			}

			double L_total = L_tube*m_N_loops;		//[m] Total length of flow path including loops
			double L_node = L_tube / m_N_nodes;		//[m] Length of one node
			double V_node = L_node*m_s_v*m_s_h;		//[m^3] Volume of one node
			V_total = L_tube*m_Depth*W_par;	//[m^3] Total HX volume

			// 2.5) Iterative loop to find air mass flow rate resulting in target fan power
			//double m_dot_air_total = 3668.0;
				// Try reasonably overestimating air mass flow rate by energy balance assuming small increase in air temp
				// Q_dot_des = m_dot_air_total*cp_air*deltaT
				// *** After 1st iteration can do something smarter here ****
			double m_dot_air_total = Q_dot_des / (5.0*cp_air);		// Assume 5K temp difference

			bool is_lowbound_m_dot = false;
			double x_lower_m_dot = numeric_limits<double>::quiet_NaN();
			double y_lower_m_dot = numeric_limits<double>::quiet_NaN();

			bool is_upbound_m_dot = false;
			double x_upper_m_dot = numeric_limits<double>::quiet_NaN();
			double y_upper_m_dot = numeric_limits<double>::quiet_NaN();

			// Another loop in, so tighten convergence
			double tol_m_dot = tol_L_tube / 2.0;					//[-] Relative tolerance for convergence
			double diff_W_dot_fan = 2.0*tol_m_dot;			//[-] Set diff > tol
			int iter_m_dot = 0;

			// Variable solved in additional nests that are required at this level
			h_conv_air = numeric_limits<double>::quiet_NaN();

			while( abs(diff_W_dot_fan) > tol_m_dot )
			{
				iter_m_dot++;

				if( iter_m_dot > 1 )
				{
					if( diff_W_dot_fan > 0.0 )	// Calculated fan power too high - decrease m_dot
					{
						is_upbound_m_dot = true;
						x_upper_m_dot = m_dot_air_total;
						y_upper_m_dot = diff_W_dot_fan;
						if( is_lowbound_m_dot )
						{
							if( max(y_upper_m_dot, y_lower_m_dot) < 0.75 )
								m_dot_air_total = -y_upper_m_dot*(x_lower_m_dot - x_upper_m_dot) / (y_lower_m_dot - y_upper_m_dot) + x_upper_m_dot;
							else
								m_dot_air_total = 0.5*(x_lower_m_dot + x_upper_m_dot);
							//-y_upper    *(x_lower       - x_upper     )/(y_lower      -     y_upper) +x_upper;
						}
						else
						{
							m_dot_air_total *= 0.1;
						}
					}
					else						// Calculated fan power too low - increase m_dot
					{
						is_lowbound_m_dot = true;
						x_lower_m_dot = m_dot_air_total;
						y_lower_m_dot = diff_W_dot_fan;
						if( is_upbound_m_dot )
						{
							if( max(y_upper_m_dot, y_lower_m_dot) < 0.75 )
								m_dot_air_total = -y_upper_m_dot*(x_lower_m_dot - x_upper_m_dot) / (y_lower_m_dot - y_upper_m_dot) + x_upper_m_dot;
							else
								m_dot_air_total = 0.5*(x_lower_m_dot + x_upper_m_dot);
						}
						else
						{
							m_dot_air_total *= 10.0;
						}
					}
				}

				double G_air = m_dot_air_total / (m_sigma*L_tube*W_par);
				double Re_air = G_air*m_D_h / mu_air;
				double f_air, j_H_air;
				f_air, j_H_air = numeric_limits<double>::quiet_NaN();

				if( !get_compact_hx_f_j(m_enum_compact_hx_config, Re_air, f_air, j_H_air) )
					return false;

				double deltaP_air = pow(G_air, 2.0)*v_air*0.5*f_air*m_alpha*V_total / (m_sigma*L_tube*W_par);
				h_conv_air = j_H_air*G_air*cp_air / pow(Pr_air, (2.0 / 3.0));	//[W/m^2-K]

				double V_dot_air_total = m_dot_air_total*v_air;
				double W_dot_fan = deltaP_air*V_dot_air_total / m_eta_fan / 1.E6;

				diff_W_dot_fan = (W_dot_fan - m_W_dot_fan_des) / m_W_dot_fan_des;

			}	// Iteration on air mass flow rate

			double A_surf_node = V_node*m_alpha;		//[-] Air-side surface area of node
			double UA_node = A_surf_node*h_conv_air;	//[W/K] Conductance of node - assuming air convective heat transfer is governing resistance

			// Set known inlet conditions: iteration thru # of loops needs previous loop info
			T_co2(1, 0) = m_T_hot_out_des;
			P_co2(1, 0) = m_P_hot_in_des - m_delta_P_des;
			for( int i = 1; i < m_N_nodes + 2; i++ )
				T_air(i, 0) = m_T_amb_des;

			// Assuming constant air props, so can set those
			double m_dot_air_tube = m_dot_air_total/(N_par*m_N_nodes);
			double C_dot_air = cp_air*m_dot_air_tube;

			for( int j = 1; j < m_N_loops + 1; j++ )
			{
				// Set up constants and multipliers to switch direction of flow
				double mult_const = (j+1) % 2;
				double constant = m_N_nodes + 2;
				double mult_index = 1.0 - 2.0*mult_const;
				double out_const = mult_index;
			
				// Set inlet temperatures & pressures of current row
				double mult_inlet = (j+1) % 2;
				double const_inlet = m_N_nodes;
				double inlet = mult_inlet*const_inlet + 1;

				// Set loop inlet conditions
				T_co2(inlet,j) = T_co2(inlet,j-1);
				P_co2(inlet,j) = P_co2(inlet,j-1);

				double deltaT_prev = numeric_limits<double>::quiet_NaN();

				for( int i = 1; i < m_N_nodes + 1; i++ )
				{
					double in = mult_const*constant + mult_index*i;
					double out = in + out_const;
					double air_in = min(in, out);

					// Guess outlet temperature
					double T_out_guess = numeric_limits<double>::quiet_NaN();
					if(deltaT_prev != deltaT_prev)
						T_out_guess = T_co2(in,j) + 0.5*deltaT_hot/(m_N_loops*m_N_nodes);
					else
						T_out_guess = T_co2(in,j) + 0.8*deltaT_prev;					

					double tol_T_out = tol_m_dot / 5.0;		//[-] Relative tolerance for convergence
					double diff_T_out = 2.0*tol_T_out;		//[-] Set diff > tol
					int iter_T_out = 0;

					bool is_lowbound_T_out = false;
					double x_lower_T_out = numeric_limits<double>::quiet_NaN();
					double y_lower_T_out = numeric_limits<double>::quiet_NaN();

					bool is_upbound_T_out = false;
					double x_upper_T_out = numeric_limits<double>::quiet_NaN();
					double y_upper_T_out = numeric_limits<double>::quiet_NaN();

					// Values solved in inner nest required later in code
					double Q_dot_node = numeric_limits<double>::quiet_NaN();

					while( abs(diff_T_out) > tol_T_out )
					{
						iter_T_out++;

						if(iter_T_out > 1)
						{
							if( diff_T_out > 0.0 )	// Guessed temperature too high - reduce guess
							{
								is_upbound_T_out = true;
								x_upper_T_out = T_out_guess;
								y_upper_T_out = diff_T_out;
								if( is_lowbound_T_out )
								{
									T_out_guess = -y_upper_T_out*(x_lower_T_out - x_upper_T_out) / (y_lower_T_out - y_upper_T_out) + x_upper_T_out;
								}
								else
								{
									T_out_guess = T_co2(out, j);
								}
							}
							else						// Calculated fan power too low - increase m_dot
							{
								is_lowbound_T_out = true;
								x_lower_T_out = T_out_guess;
								y_lower_T_out = diff_T_out;
								if( is_upbound_T_out )
								{
									T_out_guess = -y_upper_T_out*(x_lower_T_out - x_upper_T_out) / (y_lower_T_out - y_upper_T_out) + x_upper_T_out;
								}
								else
								{
									T_out_guess = T_co2(out, j);
								}
							}

						}

						double T_out_ave = 0.5*(T_out_guess + T_co2(in, j));

						CO2_TP(T_out_ave, P_hot_ave, &co2_props);
						double cp_co2_ave = co2_props.cp*1000.0;

						// Capacitance rates
						double C_dot_co2 = cp_co2_ave*m_dot_tube;
						double C_dot_min = min(C_dot_air, C_dot_co2);
						double C_dot_max = max(C_dot_air, C_dot_co2);
						double Q_dot_max = C_dot_min*(T_co2(out, j) - T_air(air_in, j - 1));
						double NTU = UA_node / C_dot_min;
						double CR = C_dot_min / C_dot_max;
						// Unmixed cross-flow
						double epsilon = 1 - exp(pow(NTU, 0.22) / CR*(exp(-CR*pow(NTU, 0.78)) - 1));
						Q_dot_node = epsilon*Q_dot_max;
						T_co2(out, j) = T_co2(in, j) + Q_dot_node / C_dot_co2;
						deltaT_prev = T_co2(out, j) - T_co2(in, j);

						diff_T_out = (T_out_guess - T_co2(out, j)) / T_co2(out, j);

					}	// **** End T_out (node) iteration ***********************
					
					T_air(air_in,j) = T_air(air_in,j-1) + Q_dot_node/C_dot_air;

					// Add pressure drop calcs (co2_props is up-to-date)
					// ** Could also move this to a function if also called to guess length
					double visc_dyn_co2 = CO2_visc(co2_props.dens, co2_props.temp)*1.E-6;
					double Re_co2 = m_dot_tube*m_d_in / (m_A_cs*visc_dyn_co2);

					double rho_co2 = co2_props.dens;
					double visc_kin_co2 = visc_dyn_co2 / rho_co2;
					double cond_co2 = CO2_cond(co2_props.dens, co2_props.temp);
					double specheat_co2 = co2_props.cp*1000.0;
					double alpha_co2 = cond_co2 / (specheat_co2*rho_co2);
					double Pr_co2 = visc_kin_co2 / alpha_co2;

					double Nusselt_co2 = -999.9;
					double f_co2 = -999.9;

					// Specifying the length over diameter = 1000 sets the problem as Fully Developed Flow
					CSP::PipeFlow(Re_co2, Pr_co2, 1000.0, m_relRough, Nusselt_co2, f_co2);

					double u_m = m_dot_tube / (rho_co2*m_A_cs);
					P_co2(out, j) = P_co2(in, j) + f_co2*L_node*rho_co2*pow(u_m, 2) / (2.0*m_d_in) / 1000.0;

				}	// End iteration through nodes in flow path				
			}	// End iteration through loop in flow path

			double T_co2_hot_calc = T_co2(final_outlet_index, m_N_loops);

			double deltaP_co2_calc = P_co2(final_outlet_index, m_N_loops) - m_P_hot_out_des;

			diff_deltaP = (deltaP_co2_calc - m_delta_P_des) / m_delta_P_des;

		}	// Iteration on length of 1 tube length

		diff_T_hot_out = (T_co2(final_outlet_index,m_N_loops)- m_T_hot_in_des)/m_T_hot_in_des;

	};

	// Final reporting metrics
	m_W_par = W_par;			//[m] Dimension in loop/air flow direction
	m_N_par = N_par;			//[-] Number of parallel flow paths
	m_N_tubes = N_tubes;		//[-] Number of tubes
	m_L_tube = L_tube;			//[m] Tube length
	m_L_path = L_tube*m_N_loops;	//[m] Total flow path length
	m_A_surf_total = V_total*m_alpha;			//[-] Air-side surface area of node
	m_UA_total = m_A_surf_total*h_conv_air;		//[W/K] Total HX Conductance
	m_V_total = V_total;		//[m^3] Total HX Volume


	return true;
};
