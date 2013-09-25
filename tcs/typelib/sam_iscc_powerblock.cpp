#define _TCSTYPEINTERFACE_
#include "tcstype.h"
#include "htf_props.h"
#include "sam_csp_util.h"

#include <waterprop/waterprop.h>

using namespace std;

enum{	//Parameters
		P_HTF,
		P_USER_HTF_PROPS,

		//Inputs
		I_T_AMB,
		I_M_DOT_MS,
		I_Q_DOT_REC,
		I_T_REC_IN, 
		I_T_REC_OUT,

		//Outputs
		O_T_HX_OUT_MS,
		O_T_HX_IN_MS,

		//N_MAX
		N_MAX};

tcsvarinfo sam_iscc_powerblock_variables[] = {
	//PARAMETERS
	{TCS_PARAM, TCS_MATRIX, P_HTF,             "HTF_code",          "HTF fluid code",	                                    "-",     "", "", ""},
	{TCS_PARAM, TCS_MATRIX, P_USER_HTF_PROPS,  "User_htf_props",    "User defined field fluid property data",               "-",     "7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows",        "",        ""},			
	
	//INPUTS
	{TCS_INPUT, TCS_NUMBER, I_T_AMB,           "T_amb",             "Ambient temperature",                                  "C",     "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_M_DOT_MS,        "m_dot_ms",          "Molten salt mass flow rate from receiver",             "kg/hr"  "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_Q_DOT_REC,       "q_dot_rec",         "Receiver thermal output",                              "MWt",   "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_T_REC_IN,        "T_rec_in",          "Receiver inlet temperature",                           "C",     "", "", ""},
	{TCS_INPUT, TCS_NUMBER, I_T_REC_OUT,       "T_rec_out",         "Receiver outlet temperature",                          "C",     "", "", ""},

	//OUTPUTS
	{TCS_OUTPUT, TCS_NUMBER, O_T_HX_OUT_MS,    "T_ms_out",          "Outlet molten salt temp - inlet rec. temp",           "C",     "", "", ""},
	{TCS_OUTPUT, TCS_NUMBER, O_T_HX_IN_MS,     "T_ms_in",           "Inlet molten salt temp - outlet rec. temp",           "C",     "", "", ""},

	//N_MAX
	{TCS_INVALID, TCS_INVALID, N_MAX,			0,					0, 0, 0, 0, 0	} } ;
	
	
class sam_iscc_powerblock : public tcstypeinterface
{
private:

	property_info wp;
	HTFProperties htfProps;		// Instance of HTFProperties class for receiver/HX htf

	double m_m_dot_st_des;
	double m_m_dot_ms_des;
	double m_UA_econo_des;
	double m_UA_sh_des;
	double m_UA_evap_des;
	double m_T_approach;
	double m_cp_ms;

public:
	sam_iscc_powerblock( tcscontext *cst, tcstypeinfo *ti)
		: tcstypeinterface( cst, ti)
	{
		m_m_dot_st_des = std::numeric_limits<double>::quiet_NaN();
		m_m_dot_ms_des = std::numeric_limits<double>::quiet_NaN();
		m_UA_econo_des = std::numeric_limits<double>::quiet_NaN();
		m_UA_sh_des = std::numeric_limits<double>::quiet_NaN();
		m_UA_evap_des = std::numeric_limits<double>::quiet_NaN();
		m_T_approach = std::numeric_limits<double>::quiet_NaN();
		m_cp_ms = std::numeric_limits<double>::quiet_NaN();
	}

	virtual ~sam_iscc_powerblock()
	{
	}

	virtual int init()
	{
		// Initialize heat transfer fluid
		int field_fl	= (int) value(P_HTF);
		if( field_fl != HTFProperties::User_defined )
		{
			htfProps.SetFluid( field_fl ); // field_fl should match up with the constants
		}
		else
		{
			int nrows = 0, ncols = 0;
			double *htf_mat = value( P_USER_HTF_PROPS, &nrows, &ncols );
			if( htf_mat != 0 && nrows > 2 && ncols == 7 )
			{
				util::matrix_t<double> mat;
				mat.assign( htf_mat, nrows, ncols );
				if( !htfProps.SetUserDefinedFluid( mat ) )
				{
					message( htfProps.UserFluidErrMessage(), nrows, ncols );
					return -1;
				}
			}
			else
			{
				message( "The htf properties matrix must have more than 2 rows and exactly 7 columns - the input matrix has %d rows and %d columns", nrows, ncols );
				return -1;
			}
		}

		const double solar_eff[5][6][8] = 
        {   // blocks for varying ambient pressure
            // rows for constant solar mass
            // columns for constant ambient temperature
            //    
            //Ambient P (bar)     Mininum     d_spacing       Max           n      
            //                    0.80         0.05         1.00            5
            //    
            //Solar Mass (kg/s)   Mininum     d_spacing       Max           n      
            //                    0.00        10.00        50.00            6
            //    
            //Ambient T (C)       Mininum     d_spacing       Max           n      
            //                    0.00         5.00        35.00            8
            //    
                {  // amibent pressure =  0.80 bar
                  {  0.0000,   0.0000,   0.0000,   0.0000,   0.0000,   0.0000,   0.0000, 1.2 }, 
                  {  0.4355,   0.4356,   0.4354,   0.4343,   0.4321,   0.4352,   0.4405, 3.2 }, 
                  {  0.4319,   0.4317,   0.4312,   0.4300,   0.4276,   0.4316,   0.4369, 3.3 }, 
                  {  0.4151,   0.4188,   0.4218,   0.4240,   0.4234,   0.4273,   0.4332, 4.1 }, 
                  {  0.4033,   0.4058,   0.4078,   0.4089,   0.4098,   0.4164,   0.4242, 5.2 }, 
                  {  0.3931,   0.3949,   0.3962,   0.3967,   0.3976,   0.4038,   0.4107, 6.1 }
                  }, 
                {  // amibent pressure =  0.85 bar
                  {  0.0000,   0.0000,   0.0000,   0.0000,   0.0000,   0.0000,   0.0000, 1.2  }, 
                  {  0.4358,   0.4356,   0.4349,   0.4329,   0.4300,   0.4349,   0.4401, 3.2  }, 
                  {  0.4307,   0.4309,   0.4304,   0.4288,   0.4266,   0.4310,   0.4365, 3.3  }, 
                  {  0.4152,   0.4190,   0.4221,   0.4242,   0.4228,   0.4273,   0.4321, 4.1  }, 
                  {  0.4034,   0.4060,   0.4080,   0.4092,   0.4108,   0.4178,   0.4246, 5.2  }, 
                  {  0.3934,   0.3952,   0.3965,   0.3970,   0.3985,   0.4051,   0.4111, 6.1  }
                  }, 
                {  // amibent pressure =  0.90 bar
                  {  0.0000,   0.0000,   0.0000,   0.0000,   0.0000,   0.0000,   0.0000, 1.2  }, 
                  {  0.4340,   0.4341,   0.4337,   0.4326,   0.4305,   0.4334,   0.4402, 3.2  }, 
                  {  0.4297,   0.4296,   0.4291,   0.4279,   0.4266,   0.4303,   0.4356, 3.3  }, 
                  {  0.4148,   0.4188,   0.4222,   0.4239,   0.4231,   0.4272,   0.4316, 4.1  }, 
                  {  0.4031,   0.4059,   0.4081,   0.4095,   0.4120,   0.4190,   0.4256, 5.2  }, 
                  {  0.3930,   0.3951,   0.3966,   0.3976,   0.4001,   0.4065,   0.4122, 6.1  }
                  }, 
                {  // amibent pressure =  0.95 bar
                  {  0.0000,   0.0000,   0.0000,   0.0000,   0.0000,   0.0000,   0.0000, 1.2  }, 
                  {  0.4334,   0.4327,   0.4321,   0.4306,   0.4296,   0.4336,   0.4381, 3.2  }, 
                  {  0.4313,   0.4290,   0.4284,   0.4271,   0.4264,   0.4306,   0.4345, 3.3  }, 
                  {  0.4146,   0.4185,   0.4220,   0.4231,   0.4231,   0.4272,   0.4308, 4.1  }, 
                  {  0.4028,   0.4056,   0.4079,   0.4096,   0.4131,   0.4204,   0.4261, 5.2  }, 
                  {  0.3928,   0.3946,   0.3963,   0.3979,   0.4016,   0.4078,   0.4129, 6.1  }
                  }, 
                {  // amibent pressure =  1.00 bar
                  {  0.0000,   0.0000,   0.0000,   0.0000,   0.0000,   0.0000,   0.0000, 1.2  }, 
                  {  0.4330,   0.4325,   0.4321,   0.4308,   0.4300,   0.4338,   0.4372, 3.2  }, 
                  {  0.4290,   0.4287,   0.4281,   0.4268,   0.4270,   0.4309,   0.4341, 3.3  }, 
                  {  0.4143,   0.4186,   0.4224,   0.4231,   0.4236,   0.4278,   0.4450, 4.1  }, 
                  {  0.4023,   0.4053,   0.4079,   0.4104,   0.4150,   0.4217,   0.4270, 5.2  }, 
                  {  0.3926,   0.3947,   0.3963,   0.3989,   0.4034,   0.4091,   0.4137, 6.1  }
                  }
        }; // end of data.
        util::block_t<double> m_solar_eff;
        //m_solar_eff.assign( solar_eff[0][0], 5, 6, 8 );
		m_solar_eff.assign( solar_eff[0][0], 6, 8, 5 );

		/*
		const double * p_array = solar_eff[0][0];
		while( true )
		{
			p_array++;
			double arr_val = p_array[0];
			double pladaf = 1.23;
		}
		*/
		// row, column, block
		// double solar_eff_001 = m_solar_eff.at(1,0,0);
		// double solar_eff_011 = m_solar_eff.at(1,1,0);
		// double solar_eff_111 = m_solar_eff.at(1,1,1);


		// ********************************************************************************************************
		// Get Steam Pressure, Extraction, and Injection At Full Solar Input from Regression Model
		// Need to define "Full Solar Input" with a mass flow rate
		// ********************************************************************************************************

		// ********************************************************************************************************
		// For now, use estimates...
		// ********************************************************************************************************
		double P_st_des = 10000.0;			//[kPa] Extraction/Injection at "full solar input"
		water_PQ( P_st_des, 0.0, &wp );		// Steam props at design pressure and quality = 0
		double T_st_des = wp.T;			//[C] Saturation temperature
		double T_iscc_extraction = T_st_des - 15.0;		//[C] Extraction temperature FROM power cycle
		double T_iscc_injection = T_st_des + 20.0;		//[C] Injection temperature TO power cycle
		m_m_dot_st_des = 200.0;						//[kg/s] Steam mass flow rate extracted TO solar HX
		// ********************************************************************************************************

		// Calculate evaporator and superheater duty
		double h_x0 = wp.H;									//[kJ/kg] Steam enthalpy at evaporator inlet
		double cp_x0 = wp.Cp;								//[kJ/kg-K] Thermal capacitance at evap inlet
		water_PQ( P_st_des, 1.0, &wp );					// Steam props at design pressure and quality = 1
		double h_x1 = wp.H;									// [kJ/kg] Steam enthalpy at evaporator exit
		double cp_x1 = wp.Cp;								//[kJ/kg-K] Thermal capacitance at evap outlet
		water_TP( T_iscc_injection, P_st_des, &wp );		// Steam props at superheater exit
		double h_sh_out = wp.H;								//[kJ/kg] Steam enthalpy at sh exit
		double cp_sh_out =wp.Cp;							//[kJ/kg-k] Thermal capacitance at sh exit
		water_TP( T_iscc_extraction, P_st_des, &wp );	// Steam props at economizer inlet
		double h_econo_in = wp.H;							//[kJ/kg] Steam enthalpy at econo inlet
		double cp_econo_in = wp.Cp;							//[kJ/kg-K] Thermal capacitance at econo inlet
		double q_dot_econo = m_m_dot_st_des*(h_x0 - h_econo_in);				//[kW] design point duty of economizer
		double cp_st_econo = (cp_x0 + cp_econo_in)/2.0;							//[kJ/kg-K] Average thermal capacitance of steam in economizer
		double q_dot_evap = m_m_dot_st_des*(h_x1 - h_x0);					//[kW] design point duty of evaporator
		double q_dot_sh_des = m_m_dot_st_des*(h_sh_out - h_x1);					//[kW] design point duty of superheater
		double cp_st_sh = (cp_sh_out + cp_x1)/2.0;									//[kJ/kg-K] Average thermal capacitance of steam in superheater
		double q_dot_evap_and_sh = q_dot_evap + q_dot_sh_des;	//[kW] 
		// *********************************************************************************************************

		// Calculate MS temperatures
		double T_pinch_point = 10.0;								//[C] Set pinch point at design in evaporator
		double T_ms_evap_out = T_st_des + T_pinch_point;		//[C] Molten Salt evaporator outlet temperature
		m_T_approach = 30.0;									//[C] Set molten salt approach temperature to superheater 
		double T_ms_sh_in = T_iscc_injection + m_T_approach;			//[C] Molten salt superheater inlet temperature = receiver outlet temperature
			// m_dot_ms * cp_ms * delta_T_ms = m_dot_steam * delta_h_steam
		m_cp_ms = htfProps.Cp( (T_ms_evap_out + T_ms_sh_in)/2.0 );				//[kJ/kg-K] Specific heat of molten salt
		m_m_dot_ms_des = q_dot_evap_and_sh/(m_cp_ms*(T_ms_sh_in - T_ms_evap_out));	//[kg/s] Mass flow rate of molten salt
			// Economizer
			// m_dot_ms * cp_ms * (T_ms_evap_out - T_ms_econo_out) = q_dot_econo
		double T_ms_econo_out = T_ms_evap_out - q_dot_econo/(m_m_dot_ms_des*m_cp_ms);		//[C] Temperature of molten salt at outlet of economizer
			// Evaporator
			// m_dot_ms * cp_ms * (T_ms_evap_in - T_ms_evap_out) = q_dot_evap
		double T_ms_evap_in = q_dot_evap/(m_m_dot_ms_des*m_cp_ms) + T_ms_evap_out;			//[C] Temperature of molten salt inlet of evaporator

		// Calculate heat exchanger UA
		double C_dot_ms = m_m_dot_ms_des * m_cp_ms;											//[kW/K] Capacitance rate of molten salt in economizer
			// Economizer		
		double C_dot_st = m_m_dot_st_des * cp_st_econo;								//[kW/K] Capacitance rate of steam in economizer
		double C_dot_min = min( C_dot_ms, C_dot_st );								//[kJ/kg-K] Minimum capacitance rate of economizer
		double C_dot_max = max( C_dot_ms, C_dot_st );								//[kJ/kg-K] Maximum capacitance rate of economizer
		double q_dot_max = C_dot_min * (T_ms_evap_out - T_iscc_extraction);			//[kW] Maximum possible heat transfer in economizer
		double epsilon_econo = q_dot_econo/q_dot_max;								//[-] Effectiveness of economizer
		double CR = C_dot_min/C_dot_max;											//[-] Capacitance ratio of econo.
		double NTU = log( (epsilon_econo - 1.0)/(epsilon_econo*CR - 1.0) )/(CR - 1.0);	//[-] NTU
		m_UA_econo_des = NTU * C_dot_min;											//[kW/K] Conductance
			// Superheater															
		double C_dot_st_sh_des = m_m_dot_st_des * cp_st_sh;										//[kW/K] Capacitance rate of steam in superheater
		double C_dot_min_sh_des = min( C_dot_ms, C_dot_st_sh_des );										//[kJ/kg-K] Minimum capacitance rate of superheater
		double C_dot_max_sh_des = max( C_dot_ms, C_dot_st_sh_des );										//[kJ/kg-K] Maximum capacitance rate of superheater
		double q_dot_max_sh_des = C_dot_min_sh_des * (T_ms_sh_in - T_st_des);							//[kW] Maximum possible heat transfer in superheater
		double epsilon_sh_des = q_dot_sh_des/q_dot_max_sh_des;										//[-] Effectiveness of superheater
		double CR_sh_des = C_dot_min_sh_des/C_dot_max_sh_des;													//[-] Capacitance ratio of superheater
		double NTU_sh_des = log( (epsilon_sh_des - 1.0)/(epsilon_sh_des*CR_sh_des - 1.0) )/(CR_sh_des - 1.0);			//[-] NTU
		m_UA_sh_des = NTU_sh_des * C_dot_min_sh_des;										//[kW/K] Conductance of superheater
			// Evaporator
		C_dot_min = C_dot_ms;														//[kJ/kg-K] Minimum capacitance rate of evap
		q_dot_max = C_dot_min * (T_ms_evap_in - T_st_des);							//[kW] Max possible heat transfer in evap
		double epsilon_evap = q_dot_evap/q_dot_max;									//[-] Effectiveness of evaporator
		NTU = -log(1 - epsilon_evap);												//[-] NTU
		m_UA_evap_des = NTU * C_dot_min;											//[kW/K] Conductance of evaporator

		return 0;
	}

	virtual int call( double time, double step, int ncall )
	{	
		// 1) Get inputs from receiver and weather reader
		double T_amb = value( I_T_AMB );				//[C] Ambient temperature
		double m_dot_ms = value( I_M_DOT_MS )/3600.0;	//[kg/s] Molten salt mass flow rate from receiver, convert from [kg/hr]
		double q_dot_rec = value( I_Q_DOT_REC )*1000.0;	//[kWt] Receiver thermal output, convert from [MWt]
		double T_rec_in_prev = value( I_T_REC_IN );			//[C] Receiver inlet molten salt temperature - used to solve previous call to tower model
		double T_rec_out_prev = value( I_T_REC_OUT );		//[C] Receiver outlet molten salt temperature - used to solve previous call to tower model

		if( q_dot_rec == 0 )
		{
			value( O_T_HX_OUT_MS, T_rec_in_prev );
			value( O_T_HX_IN_MS, T_rec_out_prev );
			return 0;
		}

		// 2) Use regression curves to find steam state points in power cycle
				// Either iterate on steam mass flow rate until q_dot_steam = q_dot_ms
				// Or use 2nd version of regression data that is a function of q_dot_st instead of m_dot_st

		// ********************************************************************************************************
		// For now, use estimates...
		// ********************************************************************************************************
		double P_st_des = 10000.0;						//[kPa] Extraction/Injection at "full solar input"
		water_PQ( P_st_des, 0.0, &wp );					// Steam props at design pressure and quality = 0
		double T_st_des = wp.T;							//[C] Saturation temperature
		
		double T_st_econo_in = T_st_des - 15.0;			//[C] Extraction temperature FROM power cycle
		double P_st_econo_in = P_st_des;				//[kPa] Extraction pressure FROM power cycle
		double T_st_sh_out = T_st_des + 20.0;			//[C] Injection temperature TO power cycle
		double P_st_sh_out = P_st_des;					//[kPa] Injection temperature TO power cycle		
		// ********************************************************************************************************
		
		// 3) Calculate remaining steam cycle state points
		double P_st_evap_in = P_st_econo_in;			//[kPa] Inlet pressure to evaporator
		double P_st_sh_in = P_st_econo_in;				//[kPa] Inlet pressure to superheater	
			// Superheater
		water_TP( T_st_sh_out, P_st_sh_out, &wp );		// Water props at sh outlet
		double h_st_sh_out = wp.H;						//[kJ/kg] Enthalpy at superheater outlet
		double cp_st_sh_out = wp.Cp;					//[kJ/kg-K] Specific heat at superheater outlet
		water_PQ( P_st_sh_in, 1.0, &wp );				// Water props at sh inlet
		double h_st_sh_in = wp.H;						//[kJ/kg] Enthalpy at superheater inlet
		double cp_st_sh_in = wp.Cp;						//[kJ/kg-K] Specific heat at superheater inlet
		double T_st_sh_in = wp.T;						//[C] Temperature at superheater inlet
		double cp_st_sh = (cp_st_sh_in + cp_st_sh_out)/2.0;	//[kJ/kg-K] Average specific heat in superheater
			// Economizer
		water_PQ( P_st_evap_in, 0.0, &wp );				// Water props at evap inlet
		double h_st_econo_out = wp.H;					//[kJ/kg] Enthalpy at evaporator inlet
		double cp_st_econo_out = wp.Cp;					//[kJ/kg-K]
		water_TP( T_st_econo_in, P_st_econo_in, &wp );	// Water props at econo inlet
		double h_st_econo_in = wp.H;					//[kJ/kg]
		double cp_st_econo_in = wp.Cp;					//[kJ/kg-K]
		double cp_st_econo = (cp_st_econo_in+cp_st_econo_out)/2.0;	//[kJ/kg-K]

		// 4) Calculate steam mass flow rate and here if necessary (depends on final regression curves)
		water_TP( T_st_econo_in, P_st_econo_in, &wp );		// Water props at econo inlet
		double h_econo_in = wp.H;							//[kJ/kg] enthalpy at econo inlet
		water_TP( T_st_sh_out, P_st_sh_in, &wp );			// Water props at sh outlet
		double h_sh_out = wp.H;								//[kJ/kg] enthalpy at superheater outlet
		double m_dot_st = q_dot_rec/(h_sh_out - h_econo_in);	//[kg/s] Mass flow rate of steam

		// 5) Calculate capacitance rates and duty
		double C_dot_ms = m_dot_ms * m_cp_ms;				//[kW/K] Capacitance rate of molten salt
			// Superheater
		double C_dot_st_sh = cp_st_sh * m_dot_st;			//[kW/K] Capacitance rate of steam in superheater
		double C_dot_min_sh = min( C_dot_ms, C_dot_st_sh );	//[kW/K] Minimum capacitance rate in superheater
		double C_dot_max_sh = max( C_dot_ms, C_dot_st_sh );	//[kW/K] Maximum capacitance rate in superheater
		double CR_sh = C_dot_min_sh / C_dot_max_sh;				//[-] Capacitance ratio of superheater
		double q_dot_sh = m_dot_st*(h_st_sh_out - h_st_sh_in);	//[kW] Superheater heater duty
			// Evaporator
		double q_dot_evap = m_dot_st*(h_st_sh_in - h_st_econo_out);	//[kW] Evaporator duty
			// Economizer
		double C_dot_st_econo = cp_st_econo*m_dot_st;				//[kW/K]
		double C_dot_min_econo = min( C_dot_ms, C_dot_st_econo );	//[kW/K]
		double C_dot_max_econo = max( C_dot_ms, C_dot_st_econo );	//[kW/K]
		double CR_econo = C_dot_min_econo/C_dot_max_econo;			//[-]
		double q_dot_econo = m_dot_st*(h_st_econo_out-h_st_econo_in);	//[kW]

		// 6) Update design point UAs
			// UA = UA_ref*( (m1^0.8 * m2^0.8)/(m1_ref^0.8 * m2_ref^0.8) )*( (m1_ref^0.8+m2_ref^0.8)/(m1^0.8+m2^0.8) )
		double UA_mult = ( (pow(m_dot_ms,0.8)*pow(m_dot_st,0.8))/(pow(m_m_dot_ms_des,0.8)*pow(m_m_dot_st_des,0.8)) )*( (pow(m_m_dot_ms_des,0.8)+pow(m_m_dot_st_des,0.8))/(pow(m_dot_ms,0.8)+pow(m_dot_st,0.8)));
		double UA_econo_phys = m_UA_econo_des*UA_mult;
		double UA_evap_phys = m_UA_evap_des*UA_mult;
		double UA_sh_phys = m_UA_sh_des*UA_mult;
		double UA_total_phys = UA_econo_phys + UA_evap_phys + UA_sh_phys;

		// 7) Solve performance for HX train (assuming constant MS specific heat for all timesteps)	
			// Set up iteration on molten salt inlet temperature
		bool T_lowflag = true;
		double T_lower = T_st_sh_out;						//[C]
		bool T_upflag = false;
		double T_upper = std::numeric_limits<double>::quiet_NaN();	//[C]

		bool UAdiff_T_lowflag = false;
		double UAdiff_T_lower = std::numeric_limits<double>::quiet_NaN();	//[C]
		bool UAdiff_T_upflag = false;
		double UAdiff_T_upper = std::numeric_limits<double>::quiet_NaN();	//[C]

		bool pinch_point_break = false;

		int iter_UA = 0;
		double diff_UA = -999.9;	//[-]

		// Update these with previous values?
		double T_ms_in_guess = T_st_sh_out + m_T_approach;						//[C] Guess molten salt inlet temperature
		double T_ms_out_guess = std::numeric_limits<double>::quiet_NaN();		//[C]
		// *******************************************************************************

		while( abs(diff_UA) > 0.005 && iter_UA < 50 )
		{
			iter_UA++;
			if( iter_UA > 1 )
			{

				if( UAdiff_T_lowflag && UAdiff_T_upflag )
				{
					if( diff_UA > 0.0 )
					{
						T_lower = T_ms_in_guess;
						UAdiff_T_lower = diff_UA;
					}
					else
					{
						T_upper = T_ms_in_guess;
						UAdiff_T_upper = diff_UA;
					}
					T_ms_in_guess = UAdiff_T_upper/(UAdiff_T_upper-UAdiff_T_lower)*(T_lower-T_upper) + T_upper;		//[C] False position method
				}
				else if( pinch_point_break )
				{
					pinch_point_break = false;
					T_lower = T_ms_in_guess;
					T_ms_in_guess += 15.0;			//[C] Don't have upper bound yet, so just temp until we get there
				}
				else if( diff_UA > 0.0 && !T_upflag )
				{
					T_lower = T_ms_in_guess;
					UAdiff_T_lowflag = true;
					T_ms_in_guess += 15.0;			//[C] Don't have upper bound yet, so just temp until we get there
				}
				else
				{
					if( diff_UA > 0.0 )
					{
						T_lower = T_ms_in_guess;
						UAdiff_T_lower = diff_UA;
						UAdiff_T_lowflag = true;
					}
					else
					{
						T_upper = T_ms_in_guess;
						T_upflag = true;
						UAdiff_T_upper = diff_UA;
						UAdiff_T_upflag = true;
					}
					if( UAdiff_T_lowflag && UAdiff_T_upflag )
						T_ms_in_guess = UAdiff_T_upper/(UAdiff_T_upper-UAdiff_T_lower)*(T_lower-T_upper) + T_upper;		//[C] False position method
					else
						T_ms_in_guess = 0.5*T_lower + 0.5*T_upper;		//[C] Biscetion method
				}
			}
	
				// Superheater performance
			double T_ms_sh_out = T_ms_in_guess - q_dot_sh/(m_dot_ms*m_cp_ms);		//[C] Outlet temperature of superheater
			double q_dot_max_sh = C_dot_min_sh*(T_ms_in_guess - T_st_sh_in);			//[kW] Maximum possible heat transfer in superheater
			double epsilon_sh = q_dot_sh/q_dot_max_sh;								//[-] Superheater effectiveness
			double NTU_sh = log( (epsilon_sh - 1.0)/(epsilon_sh*CR_sh - 1.0) )/(CR_sh - 1.0);		//[-] NTU
			double UA_sh_guess = NTU_sh * C_dot_min_sh;								//[kW/K] Conductance of superheater
				// Evaporator performance
			double T_ms_evap_out = T_ms_sh_out - q_dot_evap/(m_dot_ms*m_cp_ms);		//[C]
			if( T_ms_evap_out < T_st_sh_in )
			{
				T_lower = T_ms_in_guess;			//[C] Set lower limit on ms inlet temp
				UAdiff_T_lower = false;				//[-] Don't have UA error for this
				pinch_point_break = true;
				continue;
			}
			double q_dot_max_evap = C_dot_ms*(T_ms_sh_out - T_st_sh_in);			//[kW] Maximum possible heat transfer in evap
			double epsilon_evap = q_dot_evap/q_dot_max_evap;						//[-] Effectiveness of evaporator
			double NTU_evap = -log(1 - epsilon_evap);								//[-] NTU of evaporator
			double UA_evap_guess = NTU_evap * C_dot_ms;								//[kW/K] Conductance of evaporator
				// Economizer performance
			double T_ms_econo_out = T_ms_evap_out - q_dot_econo/(m_dot_ms*m_cp_ms);		//[C]
			double q_dot_max_econo = C_dot_min_econo*(T_ms_evap_out - T_st_econo_in);	//[kW]
			double epsilon_econo = q_dot_econo/q_dot_max_econo;							//[-]
			double NTU_econo = log( (epsilon_econo - 1.0)/(epsilon_econo*CR_econo - 1.0) )/(CR_econo - 1.0);		//[-] NTU
			double UA_econo_guess = NTU_econo * C_dot_min_econo;						//[kW/K]
			T_ms_out_guess = T_ms_econo_out;										//[C]
				// Sum UAs
			double UA_total_guess = UA_sh_guess + UA_evap_guess + UA_econo_guess;		//[kW/K]
			diff_UA = (UA_total_guess - UA_total_phys)/UA_total_phys;			//[-]
		}

		value( O_T_HX_OUT_MS, T_ms_in_guess );
		value( O_T_HX_IN_MS, T_ms_out_guess );

		return 0;

	}

	virtual int converged( double time )
	{
		
		return 0;
	}

};

TCS_IMPLEMENT_TYPE( sam_iscc_powerblock, "ISCC Powerblock ", "Ty Neises", 1, sam_iscc_powerblock_variables, NULL, 1 )

