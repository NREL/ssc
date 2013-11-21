#define _TCSTYPEINTERFACE_
#include "tcstype.h"

using namespace std;

enum{	//Parameters
		P_W_HTF_PC_PUMP,
		P_PIPING_LOSS,
		P_PIPING_LENGTH,
		P_Q_SF_DES,
		P_PB_FIXED_PAR,
		P_BOP_PAR,
		P_BOP_PAR_F,
		P_BOP_PAR_0,
		P_BOP_PAR_1,
		P_BOP_PAR_2,
		P_W_DOT_FOSSIL_DES,
		P_W_DOT_SOLAR_DES,

		//Inputs
		I_W_DOT_TRACKING,
		I_W_DOT_REC_PUMP,
		I_M_DOT_HTF_SS,
		I_W_DOT_PC_NET_SOLAR,
		I_W_DOT_PC_NET_BASELINE,
		I_F_TIMESTEP,
		I_Q_SOLAR_SS,

		//Outputs
		O_W_DOT_NET_SOLAR,   
		O_W_DOT_NET_BASELINE,
		O_ETA_SOLAR_NET,     

		//N_MAX
		N_MAX};

tcsvarinfo sam_iscc_parasitics_variables[] = {
	//PARAMETERS
	{TCS_PARAM, TCS_NUMBER, P_W_HTF_PC_PUMP,      "W_htf_pc_pump",          "Required pumping power for HTF through power block",       "kJ/kg", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_PIPING_LOSS,        "Piping_loss",            "Thermal loss per meter of piping",                         "Wt/m",  "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_PIPING_LENGTH,      "Piping_length",          "Total length of exposed piping",                           "m",     "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_Q_SF_DES,           "Q_sf_des",               "Design point solar field thermal output",                  "MW",    "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_PB_FIXED_PAR,       "pb_fixed_par",           "Fixed parasitic load - runs at all times",                       "MWe/MWcap", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_BOP_PAR,            "bop_par",                "Balance of plant parasitic power fraction",                      "MWe/MWcap", "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_BOP_PAR_F,          "bop_par_f",              "Balance of plant parasitic power fraction - mult frac",          "none",      "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_BOP_PAR_0,          "bop_par_0",              "Balance of plant parasitic power fraction - const coeff",        "none",      "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_BOP_PAR_1,          "bop_par_1",              "Balance of plant parasitic power fraction - linear coeff",       "none",      "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_BOP_PAR_2,          "bop_par_2",              "Balance of plant parasitic power fraction - quadratic coeff",    "none",      "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_W_DOT_FOSSIL_DES,   "W_dot_fossil_des",       "Fossil-only cycle output at design",                       "MWe",   "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_W_DOT_SOLAR_DES,    "W_dot_solar_des",        "Solar contribution to cycle output at design"              "MWe",   "", "", ""},

	//INPUTS
	{TCS_INPUT, TCS_NUMBER, I_W_DOT_TRACKING,     "W_dot_tracking",         "Heliostat tracking power",                                 "MWe",   "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_W_DOT_REC_PUMP,     "W_dot_rec_pump",         "Receiver pumping power",                                   "MWe",   "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_M_DOT_HTF_SS,       "m_dot_htf_ss",           "HTF mass flow rate through PC HX at steady state - no derate for startup",                         "kg/s",  "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_W_DOT_PC_NET_SOLAR, "W_dot_pc_net_solar",     "Net PC power with solar",                                  "MWe",   "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_W_DOT_PC_NET_BASELINE, "W_dot_pc_net_baseline", "Net PC power at no-solar baseline",                      "MWe",   "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_F_TIMESTEP,         "f_timestep",             "Fraction of timestep that receiver is operational (not starting-up)",      "-",        "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_Q_SOLAR_SS,         "q_solar_ss",             "Solar thermal power at steady state - no derate for startup", "MWe", "", "", ""},         

	//OUTPUTS
	{TCS_OUTPUT, TCS_NUMBER, O_W_DOT_NET_SOLAR,   "W_dot_net_solar",        "Net power output with solar",                              "MWe",   "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_W_DOT_NET_BASELINE,"W_dot_net_baseline",     "Net power output at baseline",                             "MWe",   "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_ETA_SOLAR_NET,     "eta_solar_net",          "Solar use efficiency considering parasitics",              "MWe",   "", "", ""},

	//N_MAX
	{TCS_INVALID, TCS_INVALID, N_MAX,			0,					0, 0, 0, 0, 0	} } ;
	
	
class sam_iscc_parasitics : public tcstypeinterface
{
private:
	double W_htf_pc_pump;
	double Piping_loss;
	double Piping_length;
	double q_solar_design;
	double pb_fixed_par;
	double bop_par;
	double bop_par_f;
	double bop_par_0;
	double bop_par_1;
	double bop_par_2;
	double W_dot_fossil_des;
	double W_dot_solar_des;
	double W_dot_total_des;

public:
	sam_iscc_parasitics( tcscontext *cst, tcstypeinfo *ti)
		: tcstypeinterface( cst, ti)
	{

	}

	virtual ~sam_iscc_parasitics()
	{
		W_htf_pc_pump = std::numeric_limits<double>::quiet_NaN();
		Piping_loss = std::numeric_limits<double>::quiet_NaN();
		Piping_length = std::numeric_limits<double>::quiet_NaN();
		q_solar_design = std::numeric_limits<double>::quiet_NaN();
		pb_fixed_par = std::numeric_limits<double>::quiet_NaN();
		bop_par = std::numeric_limits<double>::quiet_NaN();
		bop_par_f = std::numeric_limits<double>::quiet_NaN();
		bop_par_0 = std::numeric_limits<double>::quiet_NaN();
		bop_par_1 = std::numeric_limits<double>::quiet_NaN();
		bop_par_2 = std::numeric_limits<double>::quiet_NaN();
		W_dot_fossil_des = std::numeric_limits<double>::quiet_NaN();
		W_dot_solar_des = std::numeric_limits<double>::quiet_NaN();
		W_dot_total_des = std::numeric_limits<double>::quiet_NaN();
	}

	virtual int init()
	{
		W_htf_pc_pump = value( P_W_HTF_PC_PUMP );				//[kJ/kg]
		Piping_loss = value( P_PIPING_LOSS );					//[Wt/m]
		Piping_length = value( P_PIPING_LENGTH );				//[m]
		q_solar_design = value( P_Q_SF_DES );					//[MWt]
		pb_fixed_par = value( P_PB_FIXED_PAR );					//[-]
		bop_par = value( P_BOP_PAR );							//[MWe/MWcap]
		bop_par_f = value( P_BOP_PAR_F );						//[-]
		bop_par_0 = value( P_BOP_PAR_0 );						//[-]
		bop_par_1 = value(P_BOP_PAR_1);							//[-]
		bop_par_2 = value(P_BOP_PAR_2);							//[-]
		W_dot_fossil_des = value( P_W_DOT_FOSSIL_DES );			//[MWe]
		W_dot_solar_des = value( P_W_DOT_SOLAR_DES );			//[MWe]
		W_dot_total_des = W_dot_fossil_des + W_dot_solar_des;	//[MWe]

		return 0;
	}

	virtual int call( double time, double step, int ncall )
	{	
		double W_dot_tracking = value( I_W_DOT_TRACKING );		//[MWe]
		double W_dot_rec_pump = value( I_W_DOT_REC_PUMP );		//[MWe]
		double m_dot_htf = value( I_M_DOT_HTF_SS );				//[kg/hr]
		double W_dot_pc_net_solar = value( I_W_DOT_PC_NET_SOLAR );	//[MWe]
		double W_dot_pc_net_baseline = value( I_W_DOT_PC_NET_BASELINE ); //[MWe]
		double f_timestep = value( I_F_TIMESTEP );
		double q_solar = value( I_Q_SOLAR_SS );					//[MWt]

		double P_ratio = W_dot_pc_net_solar / W_dot_total_des;	//[-]

		double W_dot_htf = f_timestep*W_htf_pc_pump*m_dot_htf/(3600.0)/1000.0;		//[MWe] HTF pumping power through power cycle heat exchanger

		double eta_cycle_base = 0.0;
		double W_dot_piping_tot = 0.0;
		if( q_solar > 0.0 )
		{
			eta_cycle_base = (W_dot_pc_net_solar - W_dot_pc_net_baseline) / (f_timestep*q_solar);
			W_dot_piping_tot = Piping_loss * Piping_length * eta_cycle_base * (q_solar / q_solar_design)*1.E-6;	//[MWe] Electric equivalent loss from receiver piping heat loss
		}

		double W_dot_BOP = 0.0;
		if( P_ratio > 0.0 )
			W_dot_BOP = W_dot_total_des * bop_par * bop_par_f * (bop_par_0 + bop_par_1*(P_ratio) + bop_par_2*pow(P_ratio, 2));

		double W_dot_fixed = pb_fixed_par * W_dot_total_des;		//[MWe]
		
		double W_dot_net_solar = W_dot_pc_net_solar - W_dot_rec_pump - W_dot_tracking - W_dot_htf - W_dot_piping_tot - W_dot_BOP - W_dot_fixed;	//[MWe] Net power output with solar

		double eta_solar_use = 0.0;
		if( q_solar > 0.0 )
			eta_solar_use = (W_dot_net_solar - W_dot_pc_net_baseline)/(f_timestep*q_solar);		//[-] Solar use fraction with parasitics

		value( O_W_DOT_NET_SOLAR, W_dot_net_solar );		//[MWe]
		value( O_W_DOT_NET_BASELINE, W_dot_pc_net_baseline );	//[MWe]
		value( O_ETA_SOLAR_NET, eta_solar_use );			//[MWe]

		return 0;

	}

	virtual int converged( double time )
	{

		return 0;
	}

};

TCS_IMPLEMENT_TYPE( sam_iscc_parasitics, "ISCC Powerblock ", "Ty Neises", 1, sam_iscc_parasitics_variables, NULL, 1 )

