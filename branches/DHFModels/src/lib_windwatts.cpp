#include "lib_windwatts.h"

#include <vector>
#include <math.h>

#ifndef M_PI
#define M_PI 3.1415926535
#endif

#ifndef DTOR
#define DTOR 0.0174532925
#endif

static inline double max_of(double a, double b)
{
	return (a > b) ? a : b;
}

static inline double min_of(double a, double b)
{
	return (a < b) ? a : b;
}


void turbine_power( double Vel_T, double Alpha_T, double Hub_Ht, double DataHt,
	double Spd_CtIn, double Rotor_Di,
	double Air_Dens, double Rho_T, double Spd_Ratd, double Pwr_Ratd,
	int Ctl_Mode, double LossC, double LossP, double PC_wspd[], double PC_pwr[], int Num_Pair,
	double *PWECS, double *CT, double *CP )
{

	double out_pwr, out_ct, out_cp;

	// CORRECT SITE WIND SPEED TO ROTOR CENTER HEIGHT 
	if (Alpha_T > 1.0) Alpha_T = 1.0/7.0;

	double V_Hub = Vel_T * pow(Hub_Ht/DataHt, Alpha_T);
	
	// POWER OUTPUT CALCULATION

	if (V_Hub < PC_wspd[0])	
		out_pwr = 0.0; // power is 0 when winds are calm
	else if (V_Hub > PC_wspd[Num_Pair-1]) 
		out_pwr = PC_pwr[Num_Pair-1]; // wind speed greater than maximum: power output is last value
	else 
	{
		int j = 1;
		while ( PC_wspd[j] <= V_Hub )
			j++; // find first PC_wspd > V_Hub

		out_pwr = PC_pwr[j-1] 
				+  ( (V_Hub-PC_wspd[j-1])
					* (PC_pwr[j] - PC_pwr[j-1])/( PC_wspd[j]-PC_wspd[j-1]) ); // !Linear interpolation
	}

	if ( V_Hub < Spd_CtIn) out_pwr = 0.0; // Check against turbine cut-in speed

	// wind turbine output corrected for site air density
	out_pwr *= Rho_T/Air_Dens;
	double NewVRat = Spd_Ratd*pow(Rho_T/Air_Dens, 1.0/3.0);

	if (Ctl_Mode == 1) // pitch control
	{
		if (out_pwr > Pwr_Ratd)
			out_pwr = Pwr_Ratd;
		else if (V_Hub > NewVRat)
			out_pwr = Pwr_Ratd;
		else
			out_pwr *= Rho_T/Air_Dens;
	}
	else if (Ctl_Mode == 0) // var speed control
	{
		if (out_pwr > Pwr_Ratd)
			out_pwr = Pwr_Ratd;
		else if (V_Hub > NewVRat)
			out_pwr = Pwr_Ratd;
		else
			out_pwr *= Rho_T/Air_Dens;
	} // !stall defaults to simple density ratio

	//C    Hours, Cp, and Adjustments by Number of WTs and by Loss Coefficient 

	if (out_pwr > (Pwr_Ratd * 0.01))
	{
		out_pwr = out_pwr*(1.0-LossP)-LossC;
		double pden = 0.5*Rho_T*pow(V_Hub, 3.0);
		double area = M_PI/4.0*Rotor_Di*Rotor_Di;
		out_cp = max_of( 0.0, 1000.0*out_pwr/(pden*area) );
		if (out_cp < 0.0)
			out_ct = 0.0;
		else
			out_ct = max_of( 0.0, -1.453989e-2+1.473506*out_cp-2.330823*out_cp*out_cp+
                     3.885123*out_cp*out_cp*out_cp );
	}
	else
	{
		out_pwr = 0;
		out_cp = 0;
		out_ct = 0;
	}

	// set output variables;
	*PWECS = out_pwr;
	*CT = out_ct;
	*CP = out_cp;
}

void vel_delta_loc( double RR, double DD, double Sigma, double CT, 
	double *SigLocal, double *Vdelta)
{
	if (RR > 20.0 || Sigma <= 0.0 || DD <= 0.0 || CT <= 0.0)
	{
		*Vdelta = 0.0;
		*SigLocal = Sigma;
	}
	else
	{
		double SigAdd = (CT/7.0)*(1.0-(2.0/5.0)*log(2*DD));
		*SigLocal = sqrt( SigAdd*SigAdd + Sigma*Sigma );
		double AA = (*SigLocal)*(*SigLocal)*DD*DD;
		double ArgExp = max_of( -99.0, (-(RR*RR)/(2*AA)) );
		*Vdelta = (CT/(4*AA))*exp(ArgExp);
	}
}

void coordtrans( double N, double E, double thetaRada, double *D, double *C)
{
//C     THIS SUBROUTINE TRANSFORMS THE MAP EAST,NORTH COORDINATE SYSTEM TO A 
//C     DOWNWIND,CROSSWIND ORIENTATION ORTHOGONAL TO CURRENT WIND DIRECTION.
	double thetaRadb = thetaRada +(M_PI/2); //! Rotates to 270 deg (N,E) = 0 deg in (D,E)
	*D = E*cos(thetaRadb)-N*sin(thetaRadb); //!northerly = FROM North"
	*C = E*sin(thetaRadb)+N*cos(thetaRadb);
}

int wind_power(
			// INPUTS
				double Vel_T,  // wind velocity m/s
				double Theta_T, // wind direction 0-360, 0=N
				double Alpha_T, // shear exponent
				double Sigma_T, // turbulence intensity (%)
				double BarPAtm,    // barometric pressure (Atm)
				double TdryC,    // dry bulb temp ('C)
				int NumWT,      // number of wind turbines
				double WT_x[],  // x coordinates of wind turbines
				double WT_y[],  // y coordinates of wind turbines
				int PC_len,    // number of power curve points
				double PC_w[],  // Power curve wind speeds m/s
				double PC_p[],  // Power curve output power (kW)
				double Data_Ht,  // Site data collection height (m)
				double Hub_Ht, // each turbine's hub height (m)
				double Rotor_Di, // turbine rotor diameter (m)
				int Ctl_Mode,   // control mode 0=pitch, 1=variable, 2=simple
				double Spd_CtIn, // wind speed Cut in (m/s)
				double Spd_Ratd, // rated wind speed
				double Pwr_Ratd, // rated power
				double LossC,    // constant loss
				double LossP,    // loss as percent
			
			// OUTPUTS
				double *FarmP,     // total farm power output
				double Dn[],       // downwind coordinate of each WT
				double Cs[],       // crosswind coordinate of each WT
				double Power[],    // calculated power of each WT
				double Thrust[],   // thrust calculation at each WT
				double Eff[],      // downwind efficiency of each WT
				double Wind[],     // wind speed at each WT
				double Turbul[]    // turbulence coeff at each WT

				)
{
	if (NumWT > 256)
		return 0;

	int i,j;
	unsigned char wt_id[256], wid;

	for (i=0;i<256;i++)
		wt_id[i] = (unsigned char)i;

//	C    CALCULATE AIR DENSITY FROM TEMPERATURE AND PRESSURE
//	double T_STD = 288.16;                          //!Standard reference temperature, deg.C
//	double B = 0.0065;                              //!Standard elevation lapse rate deg.C/m
//	double G = 9.8066;                              //!Gravitational constant, m/s^2
	double R = 287.0;                              //!Universal gas constant
	double BP_STP = 101350;                         //!Standard sea-level pressure,Pa
//	double GAMMA = G/(R*B);                         //!Lapse Rate Manipulation
	double T_KELV = TdryC+273.15;                 //!Convert temperatures to absolute, deg.K
	double Rho_T = (BarPAtm * BP_STP)/(R*T_KELV);   //!Air Density, kg/m^3
	
	//CPQ  SPATIAL DATA TRANSFORMATIONS AND SORTING
	double Theta_TR = Theta_T*DTOR;  // convert degrees to radians
//	double Area = (M_PI/4)*Rotor_Di*Rotor_Di; // !square meters

	double d, c;
	
	//!Convert to d,c coordinates and initialize others
	for (i=0;i<NumWT;i++)
	{
		coordtrans( WT_y[i], WT_x[i], Theta_TR, &d, &c );

		Dn[i] = d;
		Cs[i] = c;

		Power[i] = 0.0;
		Thrust[i] = 0.0;
		Eff[i] = 0.0;

		Wind[i] = Vel_T;
		Turbul[i] = Sigma_T;
	}
		
 	double Dmin = Dn[0];
	double Cmin = Cs[0];

	for (j=1;j<NumWT;j++) //!Translate out of neg.#s 
	{
		Dmin = min_of(Dn[j],Dmin);
		Cmin = min_of(Cs[j],Cmin);
	}

	for (j=0;j<NumWT;j++)
	{
		Dn[j] = Dn[j]-Dmin; // Final downwind coordinates, meters
		Cs[j] = Cs[j]-Cmin; // Final crosswind coordinates, meters
	}

	for (i=0;i<NumWT;i++) // Convert from meteres to WT radii
	{
		Dn[i] = 2.0*Dn[i]/Rotor_Di;
		Cs[i] = 2.0*Cs[i]/Rotor_Di;
	}
	
	double pwecs, ct, cp;

	turbine_power( Vel_T,
				Alpha_T,
				Hub_Ht,
				Data_Ht,
				Spd_CtIn,
				Rotor_Di,
				1.249,  // air density
				Rho_T,
				Spd_Ratd,
				Pwr_Ratd,
				Ctl_Mode,
				LossC,
				LossP,
				PC_w,
				PC_p,
				PC_len,
				&pwecs,
				&ct,
				&cp );

	

	double UpWTkW = pwecs;
	Power[0] = pwecs;
	Thrust[0] = ct;
	Eff[0] = pwecs < 1.0 ? 0.0 : 100.0;

	if (NumWT < 2)
	{
		*FarmP = pwecs;
		return 1;
	}

	

	// Sort Dn,Cs arrays by downwind distance 
	for (j=1;j<NumWT;j++) 
	{
		d = Dn[j]; // pick out each element
		c = Cs[j];
		wid = wt_id[j];

		i=j;
		while (i > 0 && Dn[i-1] > d) // look for place to insert item
		{
			Dn[i] = Dn[i-1];
			Cs[i] = Cs[i-1];
			wt_id[i] = wt_id[i-1];
			i--;
		}

		Dn[i] = d; // insert it
		Cs[i] = c;
		wt_id[i] = wid;
	}

	// downwind propagation of wind speed reduction due to WTs

	for (i=0;i<NumWT-1;i++)
	{
		for (j=i+1;j<NumWT;j++)
		{
			// 'i' represents up-wind turbine
			// 'j' represents down-wind turbine

			// distance downwind, upwind to downwind WT
			double dd = Dn[j] - Dn[i]; 

			// separation crosswind between upwind and downwind WTs

			// EQN SIMPLIFIED B/C all hub heights the same currently
			//  F: RR(j) = ((DA(4,j)-DA(4,i))**2.0+(DA(5,j)-DA(5,i))**2.0)**0.5
			//
			//  C: rr    = sqrt((Cs[j]-Cs[i])*(Cs[j]-Cs[i]) + (HtRad[j]-HtRad[i])*(HtRad[j]-HtRad[i]));
			//    where HtRad = HubHt/Rotor_Di for each WT
			double rr = Cs[j] - Cs[i];
				
			double sig, delt;
			vel_delta_loc( rr, dd, Turbul[j], Thrust[i], &sig, &delt);

			Wind[j] = Wind[j]*(1-delt);
			Turbul[j] = sig;

			if (j==i+1)
			{
				// call turbine power
				turbine_power( Wind[j],
					Alpha_T,
					Hub_Ht,
					Data_Ht,
					Spd_CtIn,
					Rotor_Di,
					1.249,
					Rho_T,
					Spd_Ratd,
					Pwr_Ratd,
					Ctl_Mode,
					LossC,
					LossP,
					PC_w,
					PC_p,
					PC_len,
					&pwecs,
					&ct,
					&cp );

				Power[j] = pwecs;
				Thrust[j] = ct;

				if (UpWTkW < 0.0)
					Eff[j] = 0.0;
				else
					Eff[j] = 100*(pwecs+0.0001)/(UpWTkW+0.0001);
			}

		}
	} 

	*FarmP = 0;

	for (i=0;i<NumWT;i++)
		*FarmP += Power[i];

	double p,t,e,w,b;

	// Resort output arrays by wind turbine ID (0..nwt-1)
	// for consistent reporting
	for (j=1;j<NumWT;j++) 
	{
		d = Dn[j]; // pick out each element
		c = Cs[j];
		p = Power[j];
		t = Thrust[j];
		e = Eff[j];
		w = Wind[j];
		b = Turbul[j];
		wid = wt_id[j];

		i=j;
		while (i > 0 && wt_id[i-1] > wid) // look for place to insert item
		{
			Dn[i] = Dn[i-1];
			Cs[i] = Cs[i-1];
			Power[i] = Power[i-1];
			Thrust[i] = Thrust[i-1];
			Eff[i] = Eff[i-1];
			Wind[i] = Wind[i-1];
			Turbul[i] = Turbul[i-1];
			wt_id[i] = wt_id[i-1];
			i--;
		}

		Dn[i] = d; // insert it
		Cs[i] = c;
		Power[i] = p;
		Thrust[i] = t;
		Eff[i] = e;
		Wind[i] = w;
		Turbul[i] = b;
		wt_id[i] = wid;
	}
	
	return NumWT;
}
