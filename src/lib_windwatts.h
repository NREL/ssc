#ifndef __lib_windwatts_h
#define __lib_windwatts_h

int wind_power(

			// INPUTS
				double Vel_T,    // wind velocity m/s
				double Theta_T,  // wind direction 0-360, 0=N
				double Alpha_T,  // shear exponent
				double Sigma_T,  // turbulence intensity (%)
				double BarPAtm,  // barometric pressure (Atm)
				double TdryC,    // dry bulb temp ('C)
				int NumWT,       // number of wind turbines
				double WT_x[],   // x coordinates of wind turbines
				double WT_y[],   // y coordinates of wind turbines
				int PC_len,      // number of power curve points
				double PC_w[],   // Power curve wind speeds m/s
				double PC_p[],   // Power curve output power (kW)
				double Data_Ht,  // Site data collection height (m)
				double Hub_Ht,   // each turbine's hub height (m)
				double Rotor_Di, // turbine rotor diameter (m)
				int Ctl_Mode,    // control mode 0=pitch, 1=variable, 2=simple/stall
				double Spd_CtIn, // wind speed Cut in (m/s)
				double Spd_Ratd, // rated wind speed
				double Pwr_Ratd, // rated power
				double LossC,    // constant loss
				double LossP,    // loss as percent
			
			// OUTPUTS
				double *FarmP,   // total farm power output
				double Dn[],     // downwind coordinate of each WT
				double Cs[],     // crosswind coordinate of each WT
				double Power[],  // calculated power of each WT
				double Thrust[], // thrust calculation at each WT
				double Eff[],    // downwind efficiency of each WT
				double Wind[],   // wind speed at each WT
				double Turbul[]  // turbulence coeff at each WT

				);

#endif
