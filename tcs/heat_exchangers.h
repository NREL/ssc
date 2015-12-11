#ifndef __HEAT_EXCHANGERS_
#define __HEAT_EXCHANGERS_

#include "CO2_properties.h"
#include "htf_props.h"
#include "lib_util.h"

class C_HX_counterflow
{

private:

public:

	enum
	{
		CO2 = 200
	};

	struct S_des_par
	{
		int m_N_sub_hx;				//[-] Number of sub-heat exchangers used in the model

		int m_hot_fl;				//[-] Integer code for hot fluid - assumed be HTF in library or w/ lookup unless = CO2 enumeration
		util::matrix_t<double> mc_hot_fl_props;		//[-] If applicable, user-defined properties

		int m_cold_fl;				//[-] Integer code for cold fluid - assumed be HTF in library or w/ lookup unless = CO2 enumeration
		util::matrix_t<double> mc_cold_fl_props;	//[-] If applicable, user-defined properties



		S_des_par()
		{
			m_N_sub_hx = m_hot_fl = m_cold_fl = -1;
		}
	};

	struct S_des_solved
	{
		double m_T_h_in;						//[K] Design-point hot inlet temperature
		double m_T_c_in;						//[K] Design-point cold inlet temperature
		std::vector<double> m_m_dot_design;		//[kg/s] Design-point mass flow rates of the two streams (cold, hot)
		std::vector<double> m_DP_design;		//[kPa] Design-point pressure drops across the heat exchanger (cold, hot)
		double m_UA_design;						//[kW/K] Design-point conductance
		double m_Q_dot_design;					//[kW] Design-point heat transfer
		double m_min_DT_design;					//[K] Minimum temperature difference in heat exchanger
		double m_eff_design;					//[-] Effectiveness at design
		double m_T_h_out;						//[K] Design-point hot outlet temperature
		double m_T_c_out;						//[K] Design-point cold outlet temperature

		S_des_solved()
		{
			m_m_dot_design.resize(2);
			std::fill(m_m_dot_design.begin(), m_m_dot_design.end(), std::numeric_limits<double>::quiet_NaN());
			m_DP_design.resize(2);
			std::fill(m_DP_design.begin(), m_DP_design.end(), std::numeric_limits<double>::quiet_NaN());

			m_UA_design = m_Q_dot_design = m_min_DT_design = m_eff_design =
				m_T_h_out = m_T_c_out = std::numeric_limits<double>::quiet_NaN();
		}
	};

	bool m_is_HX_initialized;		//[-] True = yes!
	bool m_is_HX_designed;			//[-] True = yes!

	S_des_par ms_des_par;
	S_des_solved ms_des_solved;

	HTFProperties mc_hot_fl;
	HTFProperties mc_cold_fl;
	CO2_state mc_co2_props;

	C_HX_counterflow();

	void design(double Q_dot /*kWt*/, double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/,
		double T_c_in /*K*/, double T_h_in /*K*/, double P_c_in /*kPa*/, double P_c_out /*kPa*/, double P_h_in /*kPa*/, double P_h_out /*kPa*/,
		double & UA /*kW/K*/, double & min_DT /*C*/);

	void od_delta_p(double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/,
		double delta_P_c /*kPa*/, double delta_P_h /*kPa*/);

	void od_UA(double m_dot_c /*kg/s*/, double m_dot_h /*kg/s*/,
		double delta_P_c /*kW/K*/, double delta_P_h /*kW/K*/);

	virtual void initialize(const S_des_par & des_par_in);


};

class C_HX_co2_to_htf : public C_HX_counterflow
{

private:



public:

	virtual void initialize(int hot_fl, util::matrix_t<double> hot_fl_props);

	virtual void initialize(int hot_fl);


};


namespace N_compact_hx
{
	enum
	{
		fc_tubes_s80_38T = 1,
		fc_tubes_sCF_88_10Jb
	};

	bool get_compact_hx_geom(int enum_compact_hx_config, double & d_out, double & fin_pitch, double & D_h,
		double & fin_thk, double & sigma, double & alpha, double & A_fin_to_surf,
		double & s_h, double & s_v, double & fin_V_per_m);

	bool get_compact_hx_f_j(int enum_compact_hx_config, double Re, double & f, double & j_H);

};

class C_CO2_to_air_cooler
{

public:
	struct S_hx_design_solved
	{
		double m_material_V;	//[m^3]		Total Material volume - no headers

		S_hx_design_solved()
		{
			m_material_V = std::numeric_limits<double>::quiet_NaN();
		}
	};

private:

	// Classes
	HTFProperties mc_air;				// Instance of HTFProperties class for ambient air

	// Remaining Air-Cooler Specs
	// Inputs
	int m_N_loops;
	int m_N_nodes;
	double m_th;			//[m]
	double m_eta_fan;		//[-]
	double m_roughness;		//[m]
	// Calculated
	double m_d_in;			//[m]
	double m_A_cs;			//[m^2]
	double m_relRough;		//[-]
	double m_Depth;			//[m]		Dimension in loop/air flow direction
	double m_W_par;			//[m]		Dimension of parallel flow paths
	double m_N_par;			//[-]		Number of parallel flow paths
	double m_N_tubes;		//[-]		Number of tubes
	double m_L_tube;		//[m]		Tube length
	double m_L_path;		//[m]		Total flow path length
	double m_A_surf_total;	//[m^2]		Total air-side surface area
	double m_UA_total;		//[W/K]		Total air-side conductance at design
	double m_V_total;		//[m^3]		Total HX volume
	double m_material_V;	//[m^3]		Total Material volume - no headers
	double m_A_surf_node;	//[m^2]

	// Design Ambient Conditions
	double m_T_amb_des;		//[K]
	double m_P_amb_des;		//[Pa]

	// Hot-side Inlet Conditions
	double m_T_hot_in_des;		//[K]
	double m_P_hot_in_des;		//[kPa]
	double m_m_dot_total;		//[kg/s] Total sCO2 mass flow into air-cooler

	// Design Performance Targets
	double m_W_dot_fan_des;		//[MW]
	double m_delta_P_des;		//[kPa]
	double m_T_hot_out_des;		//[K]
	double m_m_dot_air_des;		//[kg/s]
	double m_Q_dot_des;			//[W]

	// Calculated Performance Target
	double m_P_hot_out_des;		//[kPa]

	// HX geometry
	// Input
	int m_enum_compact_hx_config;
	// Defined from Config
	double m_d_out;		//[m]
	double m_fin_pitch;	//[1/m]
	double m_D_h;		//[m]
	double m_fin_thk;	//[m]
	double m_sigma;		//[-]
	double m_alpha;		//[1/m]
	double m_A_fin_to_surf;	//[-]
	double m_s_h;		//[m]
	double m_s_v;		//[m]
	double m_fin_V_per_m;	//[1/m]

	int m_final_outlet_index;

	// Structures
	S_hx_design_solved m_hx_design_solved;

public:

	C_CO2_to_air_cooler();

	~C_CO2_to_air_cooler(){};

	bool design_hx(double T_amb_K, double P_amb_Pa, double T_hot_in_K, double P_hot_in_kPa,
		double m_dot_hot_kg_s, double W_dot_fan_MW, double deltaP_kPa, double T_hot_out_K);

	void off_design_hx(double T_amb_K, double P_amb_Pa, double T_hot_in_K, double P_hot_in_kPa,
		double m_dot_hot_kg_s, double T_hot_out_K, double & W_dot_fan_MW, int & error_code);

	const S_hx_design_solved * get_hx_design_solved()
	{
		m_hx_design_solved.m_material_V = m_material_V;

		return &m_hx_design_solved;
	}

};






#endif