#define _TCSTYPEINTERFACE_
#include "tcstype.h"

using namespace std;

enum{	//Parameters
		P_F_CONST_PARASITIC,
		P_W_HTF_PC_PUMP,

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
	{TCS_PARAM, TCS_NUMBER, P_F_CONST_PARASITIC,  "f_const_parasitic",      "Fraction of rated gross power consumed at all times",      "-",     "", "", ""},
	{TCS_PARAM, TCS_NUMBER, P_W_HTF_PC_PUMP,      "W_htf_pc_pump",          "Required pumping power for HTF through power block",       "kJ/kg", "", "", ""},

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
	double f_const_parasitic;
	double W_htf_pc_pump;

public:
	sam_iscc_parasitics( tcscontext *cst, tcstypeinfo *ti)
		: tcstypeinterface( cst, ti)
	{

	}

	virtual ~sam_iscc_parasitics()
	{
		f_const_parasitic = std::numeric_limits<double>::quiet_NaN();
		W_htf_pc_pump = std::numeric_limits<double>::quiet_NaN();
	}

	virtual int init()
	{
		f_const_parasitic = value( P_F_CONST_PARASITIC );		//[-]
		W_htf_pc_pump = value( P_W_HTF_PC_PUMP );				//[kJ/kg]

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

		double W_dot_htf = f_timestep*W_htf_pc_pump*m_dot_htf/(3600.0)/1000.0;		//[MWe] HTF pumping power through power cycle heat exchanger
		
		double W_dot_net_solar = W_dot_pc_net_solar - W_dot_rec_pump - W_dot_tracking - W_dot_htf;	//[MWe] Net power output with solar

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

