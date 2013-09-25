#include "sam_csp_util.h"
#include <waterprop/waterprop.h>

using namespace std;

/* 
Define functions and methods that are useful in CSP modules
*/


//--- generalized interpolation functions ---

double CSP::interp(util::matrix_t<double> *data, double x, int low_bound, int up_bound, bool increasing){
	/* 
	Given a matrix with 2 rows and N columns, interpolate along row 0 to find a corresponding 
	value in row 1. 

	-----------------------------------------------------------------------------
	data.at(0,:)	|	X - independent variable data
	data.at(1,:)	|	Y - dependent variable data
	x				|	independent variable
	low_bound		|	{optional} Minimum index of interest for interpolation
	up_bound		|	{optional} Maximum index of interest for interpolation
	increasing		|	The data is in increasing order
	-----------------------------------------------------------------------------

	Unlike the methods used in HTFProperties, this assumes no storage of indices from call to call.

	Method uses bisection.
	*/

	if(low_bound < 0) low_bound = 0;
	if(up_bound < 0) up_bound = data->ncols()-1;	//Index of the last entry

	if(up_bound < low_bound) return NULL;
	if(up_bound == low_bound) return data->at(1,low_bound);
		
	int jl = low_bound, ju = up_bound;
	int jm;
	while (ju - jl > 1){
		jm = (ju + jl)/2;	//middle index of the range

		if(x < data->at(0,jm)) {
			if(increasing){
				ju = jm;
			}
			else{
				jl = jm;
			}
		}
		else{
			if(increasing){
				jl = jm;
			}
			else{
				ju = jm;
			}
		}
	}
	//now interpolate between the upper and lower bounds
	double y = data->at(1,jl) + (x - data->at(0,jl))/(data->at(0,ju) - data->at(0,jl))*(data->at(1,ju) - data->at(1,jl));
	if( (increasing && y<data->at(1,low_bound)) || (!increasing && y>data->at(1,low_bound)) ){ 
		y = data->at(1,low_bound);
	}
	else if( (!increasing && y < data->at(1, low_bound)) || (increasing && y>data->at(1, up_bound)) ){
		y = data->at(1, up_bound);
	}
	return y;
};

double CSP::interp(double *xdat, double *ydat, double x, int low_bound, int up_bound, bool increasing){
	/* 
	Given X and Y data arrays, interpolate along X to find a corresponding 
	value in Y. 

	This is an overload of the matrix_t<> call above. 

	-----------------------------------------------------------------------------
	xdat			|	X - independent variable data
	ydat			|	Y - dependent variable data
	x				|	independent variable
	low_bound		|	Minimum index of interest for interpolation
	up_bound		|	Maximum index of interest for interpolation
	increasing		|	The data is in increasing order
	-----------------------------------------------------------------------------

	Unlike the methods used in HTFProperties, this assumes no storage of indices from call to call.

	Method uses bisection.
	*/

	if(up_bound < low_bound) return NULL;
	if(up_bound == low_bound) return ydat[up_bound];
		
	int jl = low_bound, ju = up_bound;
	int jm;
	while (ju - jl > 1){
		jm = (ju + jl)/2;	//middle index of the range

		if(x < xdat[jm]) {
			if(increasing){
				ju = jm;
			}
			else{
				jl = jm;
			}
		}
		else{
			if(increasing){
				jl = jm;
			}
			else{
				ju = jm;
			}
		}
	}
	//now interpolate between the upper and lower bounds
	double y = ydat[jl] + (x - xdat[jl])/(xdat[ju] - xdat[jl])*(ydat[ju] - ydat[jl]);
	if( (increasing && y<ydat[low_bound]) || (!increasing && y>ydat[low_bound]) ){ 
		y = ydat[low_bound];
	}
	else if( (!increasing && y < ydat[low_bound]) || (increasing && y>ydat[up_bound]) ){
		y = ydat[up_bound];
	}
	return y;
};

double CSP::interp2D(double *xvals, int &nx, double *yvals, int &ny, double *data2D, double x, double y, bool strict_range){
	/* 
	This method interpolates a 2D array (as a list of information) based on the values of x and y.

	xvals -> size(nx)	|	Positional data along the X dimension. Provide in ascending order
	yvals -> size(ny)	|	Positional data along the Y dimension. Provide in ascending order
	data2D-> size(nx*ny)|	Array containing values to be interpolated. Data provided as (1->ny) for rows (1->nx).
	x					|	basis of interpolation along X
	y					|	basis of interpolation along Y
	strict_range		|	Throw an error if either 'x' or 'y' are outside of the bounds of X and Y
		
	*/

	//Use a bisection approach

	//first in x
	int xlow = 0, xhi = nx-1;
	int xrange = xhi - xlow;
	int xmid = xrange/2;
	
	//Check for x in range
	if(strict_range && (x < xlow || x > xhi))
		return std::numeric_limits<double>::quiet_NaN();

	while(xrange>1){
		if(x > xvals[xmid]){
			xlow = xmid;
		}
		else
		{
			xhi = xmid;
		}
		//Check for bounds
		if(xlow > nx-2) break;
		if(xhi < 1) break;
		xmid = (xhi + xlow)/2;
		xrange = xhi-xlow;
	}



	//in y
	int ylow = 0, yhi = ny-1;
	int yrange = yhi - ylow;
	int ymid = yrange/2;
	
	//Check for y in range
	if(strict_range && (y < ylow || y > yhi))
		return std::numeric_limits<double>::quiet_NaN();

	while(yrange>1){
		if(y > yvals[ymid]){
			ylow = ymid;
		}
		else
		{
			yhi = ymid;
		}
		//Check for bounds
		if(ylow > ny-2) break;
		if(yhi < 1) break;
		ymid = (yhi + ylow)/2;
		yrange = yhi-ylow;
	}


	//interpolate
	double
		xf = (x - xvals[xlow])/(xvals[xhi] - xvals[xlow]),
		yf = (y - yvals[ylow])/(yvals[yhi] - yvals[ylow]);

	//Get the 4 surrounding points for interpolation
	double
		p11 = data2D[ ylow*nx + xlow ],
		p12 = data2D[ ylow*nx + xhi ],
		p21 = data2D[ yhi*nx + xlow ],
		p22 = data2D[ yhi*nx + xhi ];
	
	//Calculate the x-interpolated points
	double
		x1 = p11 + xf*(p12 - p11),
		x2 = p21 + xf*(p22 - p21);
	//calculate the final interpolated value
	return x1 + yf*(x2 - x1);
	
}

void CSP::theta_trans(double alpha_sun, double phi_sun, double alpha_fix, double &phi_t, double &theta){
	/*
	Take solar position and convert it into longitudinal and transversal incidence angles
    Reference: G. Zhu (2011). Incidence Angle Modifier for Parabolic Trough Collector and its 
                Measurement at SIMTA. Internal communication, NREL, August, 2011.
        
    ------------------------------------------------------------------------------------------
    INPUTS:
    ------------------------------------------------------------------------------------------
        *   alpha_sun       Solar azimuth angle, range is (-90=E..0=S..+90=W)
        *   phi_sun         Solar zenith angle, zero is directly overhead
        *   alpha_fix       Angle of rotation of the collector axis. Zero when aligned north-
                            south, positive clockwise
    OUTPUTS:
    ------------------------------------------------------------------------------------------
        *   phi_t           Collector angle in the transversal plane
        *   theta           Collector angle in the longitudinal plane
        *   is_deg          Optional boolean flag for specifying that units are in degrees 
                            (default is radians)
    ------------------------------------------------------------------------------------------
    */
    double pi, d2r, alpha_sunX;
        
    //Check to see if the user has specified the units to be degree
    pi=3.1415926;
    d2r = 1.;
        
        
    //if the sun is below the horizon, return zeros
    if(phi_sun*d2r >= pi/2.) {
        phi_t=0.; 
		theta=0.;
        return;
	}
        
    //Convert the solar azimuth to 0=N
    alpha_sunX = alpha_sun*d2r+pi;
        
    //Calculate angles
    phi_t = abs(atan(tan(phi_sun*d2r)*sin(alpha_sunX*d2r - alpha_fix*d2r)))/d2r; //collector angle in transversal plane
    theta = abs(asin(sin(phi_sun*d2r)*cos(alpha_sunX*d2r - alpha_fix*d2r)))/d2r; //collector angle in the longitudinal plane
        
    //check for NaN
    if(theta!=theta || phi_t != phi_t) {
        phi_t = 0.; 
		theta=0.;
	}
        
	return;
}

//sky temp function
double CSP::skytemp(double T_amb_K, double T_dp_K, double hour){
		
	/*
	**********************************************************************
		This function uses the correlation for Sky Temperature             *
		that was provided in Duffie & Beckman (2006), and was              *
		also implemented in EES.                                           *
																			*
		This function takes as inputs:                                     *
		- T_amb -> ambient air temperature, dry bulb [K]                 *
		- T_dp  -> the ambient dewpoint temperature [K]                  *
		- hour  -> the hour, in solar time starting at midnight           *
		The function outputs:                                              *
		- skytemp -> the effective temperature of the sky, in degrees [K]*
																			*
	**********************************************************************
	*/

	double T_dpC, time;
	double pi=acos(-1.);

	//express "time" in terms of an angle  [rad]
	time = hour*15.*pi/180.;

	//The inputs are in terms of degrees K, but the dewpoint temperature is in terms of degrees C
	T_dpC = T_dp_K-273.15;

	//The sky temperature relationship
	return T_amb_K*pow(.711+.0056*T_dpC+.000073*T_dpC*T_dpC+.013*cos(time), .25);
};

double CSP::sign(double val){
	if(val < 0.) { return -1.0; }
	else{ return 1.0; }
};

double CSP::nint(double val){
	// returns the nearest integer
	return fmod(val,1.) < 0.5 ? floor(val) : ceil(val);
}

int CSP::TOU_Reader(double *TOUSched, double time_sec, int nTOUSched){
	/* Returns the current Time of Use period */
	/* TOUSched should have zero indexed value (all values should be between 0 and 8) */
	int hr = (int)(floor(time_sec/3600.+1.e-6)-1);
	if(hr>nTOUSched-1 || hr<0){
		return -1;	//ERROR
	}
	return (int)TOUSched[hr];
}

double CSP::poly_eval(double x, const double *coefs, const int &order){
	/* 
	Evaluate a polynomial at 'x' with coefficients 'coefs[size=order]'. Return the evaluated result
	*/
	double y = 0.;

	for(int i=0; i<order; i++){
		y += coefs[i] * pow(x, i);
	}
	return y;

}

double CSP::Nusselt_FC( double ksDin, double Re )
{
	// This is the forced convection correlation presented in [Siebers and Kraabel, 1984]
	// The value of ks\D determines the interpolation that takes place between these 4 data points

	double ksD = ksDin;
	double Nomval = ksD;
	int rerun = 0;

	double Nu_FC, ValHi, ValLo, Nu_Lo, Nu_Hi, ValHi2, ValLo2;
	// Select the bounding conditions
	
	bool repeat_loop = true;

	do
	{
		repeat_loop = false;
	
		// Point 1: ksD = 0.0
		if(ksD < 75.E-5)
		{
			Nu_FC = 0.3 + 0.488*pow( Re, 0.5 )*pow( (1.0+pow( (Re/282000), 0.625 )), 0.8);
			ValHi = 75.E-5;
			ValLo = 0.0;
		}
		else		// Point 2: ksD = 75 E-5
		{
			if(  ksD>=75.E-5 && ksD<300.E-5 )
			{
				ValHi = 300.E-5;
				ValLo = 75.E-5;
				if( Re <= 7.E5 )
					Nu_FC = 0.3 + 0.488*pow( Re, 0.5 )*pow( (1.0+pow( (Re/282000), 0.625 )), 0.8);
				else
				{
					if( Re>7.0E5 && Re<2.2E7 )
						Nu_FC = 2.57E-3*pow( Re, 0.98 );
					else
						Nu_FC = 0.0455*pow( Re, 0.81 );
				}
			}	
			else	// Point 3: ksD = 300E-5
			{
				if( ksD>=300.E-5 && ksD<900.E-5 )
				{
					ValHi = 900.E-5;
					ValLo = 300.E-5;
					if( Re <= 1.8E5 )
						Nu_FC = 0.3 + 0.488*pow( Re, 0.5 )*pow( (1.0+pow( (Re/282000), 0.625 )), 0.8);
					else
					{
						if( Re>1.8E5 && Re<4.E6 )
							Nu_FC = 0.0135*pow( Re, 0.89 );
						else
							Nu_FC = 0.0455*pow( Re, 0.81 );
					}			
				}
				else	// Point 4: ksD = 900 E -5
				{
					if( ksD >= 900.0E-5 )
					{
						ValHi = 900.0E-5;
						ValLo = 900.0E-5;
						if( Re <= 1E5 )
							Nu_FC = 0.3 + 0.488*pow( Re, 0.5 )*pow( (1.0+pow( (Re/282000), 0.625 )), 0.8);
						else
							Nu_FC = 0.0455*pow( Re, 0.81 );
					}
				}
			}
		}

		if( rerun != 1 )
		{
			rerun = 1;
			Nu_Lo = Nu_FC;
			ksD = ValHi;
			ValLo2 = ValLo;
			ValHi2 = ValHi;
			repeat_loop = true;
		}

	} while( repeat_loop );

	Nu_Hi = Nu_FC;

	double chi;
	if( Nomval >= 900.E-5 )
		chi = 0.0;
	else
		chi = (Nomval - ValLo2)/(ValHi2 - ValLo2);

	Nu_FC = Nu_Lo + (Nu_Hi - Nu_Lo)*chi;

	return Nu_FC;
}

void CSP::PipeFlow(double Re, double Pr, double LoverD, double relRough, double &Nusselt, double &f){

	/*********************************************************************
	* PipeFlow_turbulent:                                               *
	* This procedure calculates the average Nusselt number and friction *
	* factor for turbulent flow in a pipe given Reynolds number (Re),   *
	* Prandtl number (Pr), the pipe length diameter ratio (LoverD) and  *
	* the relative roughness}                                           *
	*********************************************************************/
	double f_fd,Nusselt_L, Gz, Gm, Nusselt_T, Nusselt_H,fR,X;

	//Correlation for laminar flow.. Note that no transitional effects are considered
	if (Re < 2300.) {
		//This procedure calculates the average Nusselt number and friction factor for laminar flow in a pipe 
		//..given Reynolds number (Re), Prandtl number (Pr), the pipe length diameter ratio (LoverD) 
		//..and the relative roughness}
		Gz=Re*Pr/LoverD;
		X=LoverD/Re;
		fR=3.44/sqrt(X)+(1.25/(4*X)+16-3.44/sqrt(X))/(1+0.00021*pow(X,-2));
		f=4.*fR/Re;
		//{f$='Shah' {Shah, R.K.  and London, A.L. "Laminar Flow Forced Convection in Ducts", 
		//..Academic Press, 1978 ,Eqn 192, p98}}
		Gm=pow(Gz,1./3.);
		Nusselt_T=3.66+((0.049+0.02/Pr)*pow(Gz,1.12))/(1+0.065*pow(Gz,0.7));
		Nusselt_H=4.36+((0.1156 +0.08569 /pow(Pr,0.4))*Gz)/(1+0.1158*pow(Gz,0.6));
		//{Nusselt$='Nellis and Klein fit to Hornbeck'  {Shah, R.K.  and London, A.L. "Laminar Flow Forced Convection in Ducts",
		//..Academic Press, 1978 ,Tables  20 and 22}}
		Nusselt = Nusselt_T;  //Constant temperature Nu is better approximation
	}
	else { //Correlation for turbulent flow
		f_fd = pow(0.79*log(Re)-1.64, -2); //Petukhov, B.S., in Advances in Heat Transfer, Vol. 6, Irvine and Hartnett, Academic Press, 1970
		Nusselt_L= ((f_fd/8.)*(Re-1000)*Pr)/(1.+12.7*sqrt(f_fd/8.)*(pow(Pr, 2/3.)-1.)); //Gnielinski, V.,, Int. Chem. Eng., 16, 359, 1976

		if (relRough > 1e-5) {

		  //f=8.*((8./Re)**12+((2.457*log(1./((7./Re)**0.9+0.27*(RelRough))))**16+(37530./Re)**16)**(-1.5))**(1./12.)
		  //mjw 8.30.2010 :: not used  
    
		  f_fd=pow(-2.*log10(2*relRough/7.4-5.02*log10(2*relRough/7.4+13/Re)/Re), -2);

		  Nusselt_L= ((f_fd/8.)*(Re-1000.)*Pr)/(1.+12.7*sqrt(f_fd/8.)*(pow(Pr, 2/3.)-1.)); //Gnielinski, V.,, Int. Chem. Eng., 16, 359, 1976}
		}
		f=f_fd*(1.+pow(1./LoverD, 0.7)); //account for developing flow
		Nusselt= Nusselt_L*(1.+pow(1./LoverD, 0.7));  //account for developing flow
	}
}
	// CSP Cooling functions
double CSP::P_sat4(double T_celcius)
{
	double T_K = T_celcius + 273.15; 
	return (-99.7450105 + 1.02450484*T_K - 0.00360264243*T_K*T_K + 0.00000435512698*T_K*T_K*T_K)*1.e5;
}

// Calculates enthalpy of air [J/kg] as a function of temperature [C]
double CSP::f_h_air_T(double T_C) 
{ 
	return 273474.659 + (1002.9404*T_C) + (0.0326819988*T_C*T_C); 
} 

// Evaporative cooling calculations
void CSP::evap_tower(int tech_type, double P_cond_min, int n_pl_inc, double DeltaT_cw_des, double T_approach, double P_cycle, 
							 double eta_ref, double T_db_K, double T_wb_K, double P_amb, double q_reject, double &m_dot_water, 
							 double &W_dot_tot, double &P_cond, double &T_cond, double &f_hrsys)
{
	/*
	double c_air, c_cw, deltah_evap, deltat_cw, dp_evap, drift_loss_frac, dt_out, eta_fan, eta_fan_s,
          eta_pcw_s, eta_pump, h_fan_in, h_fan_out, h_fan_out_s, h_pcw_in, h_pcw_out,
          h_pcw_out_s, m_dot_air, m_dot_blowdown, m_dot_cw, m_dot_cw_des, m_dot_drift, blowdown_frac,
          m_dot_evap, mass_ratio_fan, p_ratio_fan, q_reject_des, R, rho_cw, s_pcw_in, t_fan_in, 
		  t_fan_in_k, t_fan_out, t_fan_out_k, w_dot_cw_pump, w_dot_fan;*/
	/*
	!------------------------------------------------------------------------------------------------------------
	!--Inputs
	!   * P_cond_min    [Pa]    Minimum allowable condenser pressure
	!   * n_pl_inc      [-]     Number of part load heat rejection levels
	!   * DeltaT_cw_des [K]     Cooling water temperature rise across condenser
	!   * T_approach    [K]     Cooling tower approach temperature, difference between cw out and wet bulb temp
	!   * P_cycle       [W]     Rated power block capacity
	!   * eta_ref       [-]     Rated gross conversion efficiency
	!   * T_db          [K]     Dry bulb temperature (converted to C)
	!   * P_amb         [Pa]    Atmospheric pressure
	!------------------------------------------------------------------------------------------------------------
	!--Output
	!   * m_dot_water   [kg/s]  Total cooling tower water usage
	!   * W_dot_tot     [MW]    Total parasitic power for cooling tower model
	!   * P_cond        [Pa]    Condenser steam pressure
	!   * T_cond        [K]     Condenser steam temperature
	!   * f_hrsys       [-]     Fraction of the cooling system operating
	!------------------------------------------------------------------------------------------------------------
	*/

	// Unit conversions
	double T_db = T_db_K - 273.15;    //[C] Converted dry bulb temp
	double T_wb = T_wb_K - 273.15;    //[C] Converted wet bulb temp

	// Values that can be estimated
	double dt_out = 3.0;				// Temperature difference at hot side of the condenser
	double drift_loss_frac = 0.001;    // Drift loss fraction
	double blowdown_frac = 0.003;      // Blowdown fraction
	double dp_evap = 0.37*1.0e5;       // [Pa] Pressure drop across the condenser and cooling tower
	double eta_pump = 0.75;            // Total pump efficiency
	double eta_pcw_s = 0.8;            // Isentropic cooling water pump efficiency
	double eta_fan = 0.75;             // Fan mechanical efficiency
	double eta_fan_s = 0.8;            // Fan isentropic efficiency
	double p_ratio_fan = 1.0025;       // Fan pressure ratio
	double mass_ratio_fan = 1.01;      // Ratio of air flow to water flow in the cooling tower

	// Cooling water specific heat
	property_info wp;
	water_TP( max( T_wb, 10.0 ), P_amb/1000.0, &wp );
	double c_cw = wp.Cp * 1000.0;		// Convert to J/kg-K

	// **** Calculations for design conditions
	double q_reject_des = P_cycle*(1./eta_ref-1.0);    	    // Heat rejection from the cycle
	double m_dot_cw_des = q_reject_des/(c_cw*DeltaT_cw_des);	// Mass flow rate of cooling water required to absorb the rejected heat
	f_hrsys = 1.0;   // Initial fraction of cooling system operating

	// **** Calculations for performance
	// Calculate the cooling water temp. rise associated with normal cooling system operation
	double m_dot_cw = m_dot_cw_des;
	double deltat_cw = q_reject/(m_dot_cw*c_cw);

	// Condenser saturation temperature
	T_cond = T_wb + deltat_cw + dt_out + T_approach; // celcius

	// Condenser back pressure
	if(tech_type != 4)
	{	
		water_TQ( T_cond, 1.0, &wp );
		P_cond = wp.P * 1000.0;
	}
	else
		P_cond = CSP::P_sat4(T_cond); // isopentane


	// MJW 7.19.2010 :: Cooling system part-load strategy uses the number of part-load increments to determine how the coolign system is
	// partially shut down during under design operation. The condenser pressure is reduced with the cooling system running
	// at full load until it reaches the minimum condenser pressure. The cooling system then incrementally shuts off bays until
	// the condenser temperature/pressure rise above their minimum level. Default cond. pressure is 1.25 inHg (4233 Pa).
	if ( (P_cond < P_cond_min) && (tech_type != 4) ) // Aug 3, 2011: No lower limit on Isopentane
	{
		for (int i=2; i <=n_pl_inc; i++)
		{
			f_hrsys = (1.0 - (float)((i-1.0)/n_pl_inc));
			m_dot_cw = m_dot_cw_des*f_hrsys;
			deltat_cw = q_reject/(m_dot_cw*c_cw);
			T_cond = T_wb + deltat_cw + dt_out + T_approach;

			water_TQ( T_cond, 1.0, &wp );
			P_cond = wp.P * 1000.0;
			
			if(P_cond > P_cond_min) break;
		}
		if(P_cond <= P_cond_min)
		{
			// Still below min. fix to min condenser pressure and recalc. temp.
			
			P_cond = P_cond_min;

			water_PQ( P_cond/1000.0, 1.0, &wp );
			T_cond = wp.T;
			
			deltat_cw = T_cond - (T_wb + dt_out + T_approach);
			m_dot_cw = q_reject/(deltat_cw * c_cw);
		}
	}
	water_TP( T_cond - 3.0, P_amb/1000.0, &wp );
	double h_pcw_in = wp.H*1000.0;
	double s_pcw_in = wp.S*1000.0;
	double rho_cw = wp.dens;	
	
	double h_pcw_out_s = (dp_evap/rho_cw) + h_pcw_in;								// [J/kg] isentropic outlet enthalpy.. incompressible fluid
	double h_pcw_out = h_pcw_in + ((h_pcw_out_s - h_pcw_in)/eta_pcw_s);			// [J/kg] Outlet enthalpy accounting for irreversibility
	double w_dot_cw_pump = (h_pcw_out - h_pcw_in) * m_dot_cw/eta_pump * 1.0E-6;	// [MW] Cooling water circulating pump power

	// Fan power
	double m_dot_air = m_dot_cw*mass_ratio_fan;
	double t_fan_in = (T_db + T_wb + T_approach)/2.0;
	double h_fan_in = f_h_air_T(t_fan_in);

	double c_air = 1003.0;		// [J/kg-K] specific heat of air (This is relatively constant)
	double R = 8314./28.97;	// [J/kmol-K]/[kg/kmol] Gas constant over the molar mass of air

	double t_fan_in_k = t_fan_in + 273.15;										// Fan inlet temp, in K
	double t_fan_out_k = t_fan_in_k * pow(p_ratio_fan,(R/c_air));				// [K] isentropic temperature rise
	double t_fan_out = t_fan_out_k - 273.15;									// [C] Convert isentropic temperature rise to deg C
	double h_fan_out_s = f_h_air_T(t_fan_out);									// [J/kg] Calculate isentropic enthalpy at fan outlet
	double h_fan_out = h_fan_in + (h_fan_out_s - h_fan_in)/eta_fan_s;			// [J/kg] Actual enthalpy, accounting for irreversibility

	double w_dot_fan = (h_fan_out - h_fan_in)*m_dot_air/eta_fan*1.0E-6;  // [MW] Fan parasitic power

	// Total cooling tower parasitic power
	W_dot_tot = w_dot_cw_pump + w_dot_fan;   // [MW]
		
	// Enthalpy of evaporation
	// 1/28/13, twn: replace call to curve fit with call to steam properties routine
	//deltah_evap = f_dh_evap(P_amb);
	water_PQ( P_amb/1000.0, 0.0, &wp );
	double dh_low = wp.H;
	water_PQ( P_amb/1000.0, 1.0, &wp );
	double dh_high = wp.H;
	double deltah_evap = (dh_high - dh_low)*1000.0;	// [J/kg]

	// Evaporative water loss
	double m_dot_evap = q_reject/deltah_evap;

	// Other water losses
	double m_dot_drift = drift_loss_frac * m_dot_cw;			// Drift loss fraction, based on cooling water mass flow rate
	double m_dot_blowdown = blowdown_frac * m_dot_cw;			// Blow down fraction

	// Total power block water usage
	m_dot_water = m_dot_evap + m_dot_drift + m_dot_blowdown;

	// Unit conversions
	T_db = T_db + 273.15;		// [C] Converted dry bulb temp (TFF - I think this is irrelevant, since it's not passed back out)
	T_wb = T_wb + 273.15;		// [C] Converted wet bulb temp (TFF - I think this is irrelevant, since it's not passed back out)
	T_cond = T_cond + 273.15;	// [K] Convert to K for output
}


// Air cooling calculations
void CSP::ACC( int tech_type, double P_cond_min, int n_pl_inc, double T_ITD_des, double P_cond_ratio, double P_cycle, double eta_ref, 
		 double T_db, double P_amb, double q_reject, double& m_dot_air, double& W_dot_fan, double& P_cond, double& T_cond, 
		 double& f_hrsys)
{
	/*
	!------------------------------------------------------------------------------------------------------------
	!--Inputs
	!   * P_cond_min    [Pa]    Minimum allowable condenser pressure
	!   * n_pl_inc      [-]     Number of part load heat rejection levels
	!   * T_ITD         [K]     ACC initial temperature difference, difference between dry bulb and steam inlet temp
	!   * P_cond_ratio  [-]     Condenser air inlet/outlet pressure ratio
	!   * P_cycle       [W]     Rated power block capacity
	!   * eta_ref       [-]     Rated gross conversion efficiency
	!   * T_db          [K]     Dry bulb temperature (converted to C)
	!   * P_amb         [Pa]    Atmospheric pressure
	!------------------------------------------------------------------------------------------------------------
	!--Output
	!   * m_dot_air     [kg/s]  Total ACC air mass flow rate
	!   * W_dot_fan     [MW]    Total parasitic power for ACC model
	!   * P_cond        [Pa]    Condenser steam pressure
	!   * T_cond        [K]     Condenser steam temperature
	!------------------------------------------------------------------------------------------------------------
	*/

	// Unit conversions
	T_db = T_db - 273.15;		// [C] Converted dry bulb temp

	// Values that can be estimated
	double T_hot_diff = 3.0;           // [C] Temperature difference between saturation steam and condenser outlet air temp
	double eta_fan_s = 0.8;            // [-] Fan isentropic efficiency
	double eta_fan = pow(0.98,3.0);	// [-] Fan mechanical efficiency
	double c_air = 1005.0;				// [J/kg-K] Specific heat of air, relatively constant over dry bulb range

	// **** Calculations for design conditions
	double Q_reject_des = P_cycle*(1.0/eta_ref-1.0);							// Heat rejection from the cycle
	double m_dot_air_des = Q_reject_des/(c_air*(T_ITD_des - T_hot_diff));
	f_hrsys = 1.0;

	// Fan power
	double dT_air = q_reject/(m_dot_air_des*c_air);
	double T_ITD = T_hot_diff + dT_air;	// [C] Calculate the actual ITD during off-design operation

	// Calculated output
	T_cond = T_db + T_ITD;		// Condensation temperature

	property_info wp;
	// Turbine back pressure
	if(tech_type != 4)
	{	
		water_TQ( T_cond, 1.0, &wp );
		P_cond = wp.P * 1000.0;
	}
	else
		P_cond = CSP::P_sat4(T_cond); // isopentane


	// MJW 7.19.2010 :: Cooling system part-load strategy uses the number of part-load increments to determine how the coolign system is
	// partially shut down during under design operation. The condenser pressure is reduced with the cooling system running
	// at full load until it reaches the minimum condenser pressure. The cooling system then incrementally shuts off bays until
	// the condenser temperature/pressure rise above their minimum level. Default cond. pressure is 2.0 inHg (6772 Pa).
	if ( (P_cond < P_cond_min) && (tech_type != 4) ) // Aug 3, 2011: No lower limit on Isopentane
	{
		for (int i=2; i<=n_pl_inc; i++)
		{
			f_hrsys = (1.0 - (float)((i-1.0)/n_pl_inc));
			m_dot_air = m_dot_air_des*f_hrsys;
			dT_air = q_reject/(m_dot_air*c_air);
			T_cond = T_db + T_hot_diff + dT_air;

			water_TQ( T_cond, 1.0, &wp );
			P_cond = wp.P * 1000.0;
			
			if(P_cond > P_cond_min) break;
		}
		if (P_cond <= P_cond_min)
		{
			// Still below min. fix to min condenser pressure and recalc. temp.
			P_cond = P_cond_min;

			water_PQ( P_cond/1000.0, 1.0, &wp );
			T_cond = wp.T;
			
			dT_air = T_cond - (T_db + T_hot_diff);
			m_dot_air = q_reject/(dT_air*c_air);
		}
	}
	else
		m_dot_air = m_dot_air_des;	// 2/22/13, twn: updated to match fixes to TRNSYS version

	//100 continue
	double h_fan_in = f_h_air_T(T_db);		// [J/kg] Fan inlet enthalpy

	double mm = 28.97;						// [kg/kmol] molar mass of air
	double R = 8314.0/mm;					// [J/kg-K] Gas constant for air

	// These temperature calculations are for the isentropic expansion across the fan, not accounting for heat gain in the ACC
	double T_fan_in_K = T_db + 273.15;									// [K] Fan inlet temperature
	double T_fan_out_K = T_fan_in_K * pow(P_cond_ratio,(R/c_air));
	double T_fan_out = T_fan_out_K - 273.15;							// [C] Fan outlet temperature
	double dT_fan = T_fan_out - T_db;									// [C] Difference in temperature including irreversibilities in fan

	double h_fan_out_s = f_h_air_T(T_fan_out);							// [J/kg] Isentropic fan outlet temperature
	double h_fan_out = h_fan_in + (h_fan_out_s - h_fan_in)/eta_fan_s;	// [J/kg] Actual fan outlet temperature
	// Total ACC parasitic power
	W_dot_fan = (h_fan_out - h_fan_in)*m_dot_air/eta_fan*1.0e-6;// [MW] Fan power

	// Unit conversions
	T_db = T_db + 273.15;		// [C] Converted dry bulb temp (TFF - I think this is irrelevant, since it's not passed back out)
	T_cond = T_cond + 273.15;    // [K] Convert to K for output
}

void CSP::HybridHR( int tech_type, double P_cond_min, int n_pl_inc, double F_wc, double F_wcmax, double F_wcmin, double T_ITD_des, double T_approach, 
				  double dT_cw_ref, double P_cond_ratio, double P_cycle, double eta_ref, 
				  double T_db, double T_wb, double P_amb, double q_reject, double& m_dot_water, double& W_dot_acfan, 
				  double& W_dot_wctot, double& W_dot_tot, double& P_cond, double& T_cond, double& f_hrsys)
{
	/*
	!------------------------------------------------------------------------------------------------------------
	!This subroutine models a hybrid wet/dry cooling heat rejection system. In this system, a dry-cooled condenser
	!is responsible for rejecting the thermal load, except a supplemental wet-cooled system is placed in parallel
	!to aid in heat rejection during the hottest hours of the day. The wet cooled system can reject heat based
	!on the wetbulb temperature, and thus will have much lower parasitics in rejecting a fraction of the heat than
	!the dry cooled system will, and the dry cooled system running at normal power will result in a lower
	!condenser temperature and pressure.
	!
	!Several assumptions are made in the control of this system. The user can specify a cooling distribution factor
	!on the thermal storage page with the other TOU factors. The fraction indicates what the distribution of
	!the heat rejection load will be. If the fraction is 0.2 for example, then the wet cooling tower will reject
	!20% of the load.
	!
	!The wet-cooling system is a forced-draft tower, and is sized based on the largest TOU fraction supplied in the
	!control array.
	!
	!--Inputs----------------------------------------------------------------------------------------------------
	!   * P_cond_min    [Pa]    Minimum allowable condenser pressure
	!   * n_pl_inc      [-]     Number of part load heat rejection levels
	!   * time          [-]     hour of the year
	!   * F_wc          [-]     Wet cooling fraction
	!   * F_wcmax       [-]     Maximum annual wet cooling fraction
	!   * F_wcmin       [-]     Minimum annual wet cooling fraction
	!   * T_ITD_des     [K]     ACC initial temperature difference, difference between dry bulb and steam inlet temp
	!   * T_approach    [K]     Wet cooling tower approach temperature, difference between cw out and wet bulb temp
	!   * P_cond_ratio  [-]     Condenser air inlet/outlet pressure ratio
	!   * P_cycle       [W]     Rated power block capacity
	!   * eta_ref       [-]     Rated gross conversion efficiency
	!   * T_db          [K]     Dry bulb temperature (converted to C)
	!   * T_wb          [K]     Wet bulb temperature (converted to C)
	!   * P_amb         [Pa]    Atmospheric pressure
	!   * q_reject      [W]     Total required heat rejection load
	!------------------------------------------------------------------------------------------------------------
	!--Output
	!   * m_dot_water   [kg/s]  Total cooling tower water usage
	!   * W_dot_acfan   [MW]    Total parasitic power for ACC fan
	!   * W_dot_wctot   [MW]    Total parasitic power for cooling tower
	!   * W_dot_tot     [MW]    Total overall parasitic power
	!   * P_cond        [Pa]    Condenser steam pressure
	!   * T_cond        [K]     Condenser steam temperature
	!------------------------------------------------------------------------------------------------------------
	*/

	// Values that can be estimated--------
	//-dry
	double T_hot_diff = 3.0;				//[C] Temperature difference between saturation steam and condenser outlet air temp
	double eta_acfan_s = 0.8;				//[-] Fan isentropic efficiency
	double eta_acfan = pow(0.98,3);		//[-] Fan mechanical efficiency
	double C_air = 1005.0;					//[J/kg-K] specific heat of air (This is relatively constant)
	double R = 286.986538;					//[J/kg-K] Gas constant for air = 8314./28.97

	//-wet
	double drift_loss_frac = 0.001;		//Drift loss fraction
	double blowdown_frac = 0.003;			//Blowdown fraction
	double dP_evap = 0.37*1.e5;			//[Pa] Pressure drop across the condenser and cooling tower
	double eta_pump = 0.75;				//Total pump efficiency
	double eta_pcw_s = 0.8;				//Isentropic cooling water pump efficiency
	double eta_wcfan = 0.75;				//Fan mechanical efficiency
	double eta_wcfan_s = 0.8;				//Fan isentropic efficiency
	double P_ratio_wcfan = 1.0025;			//Fan pressure ratio
	double mass_ratio_wcfan = 1.01;		//Ratio of air flow to water flow in the cooling tower

	//**** Calculations for design conditions
	double Q_reject_des = P_cycle*(1.0/eta_ref - 1.0);    	    //Heat rejection from the cycle
	//-dry
	double q_ac_des = Q_reject_des*(1.0 - F_wcmin);    //Size the ACC to always be able to handle the load that isn't going to the wet cooler
	double m_dot_acair_des = q_ac_des/(C_air*(T_ITD_des - T_hot_diff));
	//-wet
	double q_wc_des = Q_reject_des*F_wcmax;			//Size the wet cooler to handle the maximum fraction in the control array
	//c_cw = f_c_psat(P_amb);						//Cooling water specific heat

	//Unit conversions
	T_db = T_db - 273.15;        //[C] Converted dry bulb temp
	T_wb = T_wb - 273.15;
	
	// 1/28/13, twn: replace call to curve fit with call to steam properties routine
	// c_cw = f_c_psat(P_amb);      //Cooling water specific heat (TFF, this is also calculated above.)
	property_info wp;
	water_TP( max(T_wb, 10.0), P_amb/1000.0, &wp );
	double c_cw = wp.Cp * 1000.0;		// [J/kg-K]

	double m_dot_cw_des = q_wc_des/(c_cw*dT_cw_ref);	//Mass flow rate of cooling water required to absorb the rejected heat 

	//Calculate the cooling loads
	double q_ac_rej = q_reject*(1.0 - F_wc);
	double q_wc_rej = q_reject*F_wc;
	double f_hrsyswc = 1.0;
	double f_hrsysair = 1.0;

	//-ACC
	double dT_air = q_ac_rej/(m_dot_acair_des * C_air);
	double T_ITD = T_hot_diff + dT_air;  //[C] Calculate the actual ITD during off-design operation
	//-WC
	double DeltaT_cw = q_wc_rej/(m_dot_cw_des * c_cw);

	//***Calculated output
	//Condensation temperature is the maximum of either the wet or dry system cooling stream outlet temperature (plus hot side dT)
	double T_condwc = T_wb + DeltaT_cw + T_hot_diff + T_approach;
	double T_condair = T_db + T_ITD;
	if (F_wc > 0.0) //MJW 7.23.2010
		T_cond = max(T_condwc, T_condair);
	else
		T_cond = T_condair;

	if(tech_type != 4)
	{	
		// 1/28/13, twn: replace call to curve fit with call to steam properties routine
		// P_cond =  f_psat_T(T_cond); // steam
		water_TQ( T_cond, 1.0, &wp );
		P_cond = wp.P * 1000.0;
	}
	else
		P_cond = CSP::P_sat4(T_cond); // isopentane

	// MJW 7.19.2010 :: Cooling system part-load strategy uses the number of part-load increments to determine how the coolign system is
	// partially shut down during under-design operation. The condenser pressure is reduced with the cooling system running
	// at full load until it reaches the minimum condenser pressure. The cooling system then incrementally shuts off bays until
	// the condenser temperature/pressure rise above their minimum level. Default cond. pressure is 2.0 inHg (6772 Pa).
	int i=1; int j=1;
	double m_dot_acair = m_dot_acair_des;
	double m_dot_cw = m_dot_cw_des;
	if ( (P_cond < P_cond_min) && (tech_type != 4) ) // Aug 3, 2011: No lower limit on Isopentane
	{
		do
		{
			if(T_condwc > T_condair)
			{
				i++;
				//Reduce just wet cooled
				f_hrsyswc = (1.0 - (float)((i-1.0)/n_pl_inc));
				double m_dot_cw = m_dot_cw_des*f_hrsyswc;
				DeltaT_cw = q_wc_rej/(m_dot_cw*c_cw);
				T_condwc = T_wb + DeltaT_cw + T_hot_diff + T_approach;
			}
			else
			{
				i++;
				j++;
				//Reduce both wet and dry cooled
				f_hrsysair = (1.0 - (float)((j-1.0)/n_pl_inc));
				double m_dot_acair = m_dot_acair_des*f_hrsysair;
				dT_air = q_ac_rej/(m_dot_acair*C_air);
				T_condair = T_db + dT_air + T_hot_diff;
				//--
				f_hrsyswc = (1.0 - (float)((i-1.0)/n_pl_inc));
				double m_dot_cw = m_dot_cw_des*f_hrsyswc;
				DeltaT_cw = q_wc_rej/(m_dot_cw*c_cw);
				T_condwc = T_wb + DeltaT_cw + T_hot_diff + T_approach;
			}

			if(F_wc > 0.0) //MJW 7.23.2010
				T_cond = max(T_condwc, T_condair);
			else
				T_cond = T_condair;
			
			// 1/28/13, twn: replace call to curve fit with call to steam properties routine
			// P_cond = f_psat_T(T_cond);
			water_TQ( T_cond, 1.0, &wp );
			P_cond = wp.P * 1000.0;

			//if(P_cond > P_cond_min) goto 100
			if((i >= n_pl_inc) || (j >= n_pl_inc) ) break;

		} while (P_cond < P_cond_min);

		if (P_cond <= P_cond_min)
		{
			//Still below min. fix to min condenser pressure and recalc. temp.
			P_cond = P_cond_min;
			
			// 1/28/13, twn: replace call to curve fit with call to steam properties routine
			// T_cond = f_Tsat_p(P_cond);
			water_PQ( P_cond/1000.0, 1.0, &wp );
			T_cond = wp.T;
			
			if(T_condwc > T_condair)
			{
				DeltaT_cw = T_cond - (T_wb + T_hot_diff + T_approach);
				m_dot_cw = q_reject/(DeltaT_cw*c_cw);
			}
			else
			{
				dT_air = T_cond - (T_db + T_hot_diff);
				m_dot_acair = q_reject/(dT_air*C_air);
			}
		}
	}

//100	f_hrsys = (f_hrsyswc + f_hrsysair)/2;
	f_hrsys = (f_hrsyswc + f_hrsysair)/2.0;

	//-----ACC Fan power---------
	double h_acfan_in = f_h_air_T(T_db);  //[J/kg] Fan inlet enthalpy

	//These temperature calculations are for the isentropic expansion across the fan, not accounting for heat gain in the ACC
	double T_acfan_in_K = T_db + 273.15;  //[K] Fan inlet temperature
	double T_acfan_out_K = T_acfan_in_K * pow(P_cond_ratio,(R/C_air));
	double T_acfan_out = T_acfan_out_K - 273.15;    //[C] Fan outlet temperature
	double dT_acfan = T_acfan_out - T_db;   //[C] Difference in temperature including irreversibilities in fan

	double h_acfan_out_s = f_h_air_T(T_acfan_out);	//[J/kg] Isentropic fan outlet temperature
	double h_acfan_out = h_acfan_in + (h_acfan_out_s - h_acfan_in)/eta_acfan_s;   //[J/kg] Actual fan outlet temperature
	//Total ACC parasitic power
	W_dot_acfan = (h_acfan_out - h_acfan_in) * m_dot_acair/eta_acfan*1.e-6;  //[MW] Fan power


	//-----Wet cooling parasitics --------
	if(q_wc_rej > 0.001)
	{
		//Circulating water pump power
		
		// 1/28/13, twn: replace call to curve fit with call to steam properties routine
		// h_pcw_in = f_hw_psat(P_amb);     //[J/kg] cw pump inlet enthalpy
		// s_pcw_in = f_s_hw_psat(P_amb);     //[J/kg-K] cw pump inlet entropy
		// rho_cw = f_rho_P(P_amb);         //[kg/m3] cooling water density in the pump
		water_TP( T_cond - 3.0, P_amb/1000.0, &wp );
		double h_pcw_in = wp.H * 1000.0;
		double s_pcw_in = wp.S * 1000.0;
		double rho_cw = wp.dens;
		
		double h_pcw_out_s = dP_evap/rho_cw + h_pcw_in;                         //[J/kg] isentropic outlet enthalpy.. incompressible fluid
		double h_pcw_out = h_pcw_in + (h_pcw_out_s - h_pcw_in)/eta_pcw_s;       //[J/kg] Outlet enthalpy accounting for irreversibility
		double W_dot_cw_pump = (h_pcw_out - h_pcw_in)*m_dot_cw/eta_pump*1.e-6;  //[MW] Cooling water circulating pump power

		//Fan power
		double m_dot_wcair = m_dot_cw*mass_ratio_wcfan;
		double T_wcfan_in = (T_db + T_wb + T_approach)/2.0;
		double h_wcfan_in = f_h_air_T(T_wcfan_in);

		double T_wcfan_in_K = T_wcfan_in + 273.15;  //Fan inlet temp, in K
		double T_wcfan_out_K = T_wcfan_in_K * pow(P_ratio_wcfan,(R/C_air));    //[K] isentropic temperature rise
		double T_wcfan_out = T_wcfan_out_K - 273.15;    //[C] Convert isentropic temperature rise to deg C
		double h_wcfan_out_s = f_h_air_T(T_wcfan_out);  //[J/kg] Calculate isentropic enthalpy at fan outlet
		double h_wcfan_out = h_wcfan_in + (h_wcfan_out_s - h_wcfan_in)/eta_wcfan_s;   //[J/kg] Actual enthalpy, accounting for irreversibility

		double W_dot_wcfan = (h_wcfan_out - h_wcfan_in)*m_dot_wcair/eta_wcfan*1.0E-6;  //[MW] Fan parasitic power

		//Total wet cooling tower parasitic power
		W_dot_wctot = W_dot_cw_pump + W_dot_wcfan;   //[MW]

		//Enthalpy of evaporation
		// 1/28/13, twn: replace call to curve fit with call to steam properties routine
		// deltaH_evap = f_dh_evap(P_amb);
		water_PQ( P_amb/1000.0, 0.0, &wp );
		double dh_low = wp.H;
		water_PQ( P_amb/1000.0, 1.0, &wp );
		double dh_high = wp.H;
		double deltaH_evap = (dh_high - dh_low)*1000.0;

		//Evaporative water loss
		double m_dot_evap = q_wc_rej/deltaH_evap;

		//Other water losses
		double m_dot_drift = drift_loss_frac*m_dot_cw;	//Drift loss fraction, based on cooling water mass flow rate
		double m_dot_blowdown = blowdown_frac*m_dot_cw;	//Blow down fraction

		//Total power block water usage
		m_dot_water = m_dot_evap + m_dot_drift + m_dot_blowdown;
		}
	else
	{
		//Otherwise set the wet-cooling outputs to zero
		m_dot_water = 0.0;
		W_dot_wctot = 0.0;
	}

	W_dot_tot = W_dot_wctot + W_dot_acfan;

	//Unit conversions
	T_db = T_db + 273.15;    //[C] Converted dry bulb temp
	T_wb = T_wb + 273.15;    //[C] Converted wet bulb temp
	T_cond = T_cond + 273.15;    //[K] Convert to K for output
}


double CSP::eta_pl(double mf)
{
	return 1.0 - (0.191 - 0.409*mf + 0.218*pow(mf,2.0));	// This number should be multiplied by the design-point isen. eff to get the final.
}


void P_max_check::set_P_max( double P_max_set )
{
	P_max = P_max_set;		//[bar]
	P_save = 0.0;
	is_error = false;

	return;
}

void P_max_check::report_and_reset()
{
	if( is_error )
	{
		// Write message
	}

	P_save = 0.0;
	is_error = false;

	return;
}

double P_max_check::P_check( double P )
{
	
	//Check the pressure, store the highest value
    
	if( P > P_max )
	{
		is_error = true;
		if( P > P_save )
			P_save = P;
		return P_max;
	}
	
	return P;
}

void enth_lim::set_enth_limits( double h_min_in, double h_max_in )
{
	h_min = h_min_in;
	h_max = h_max_in;
	return;
}

double enth_lim::check( double h_in )
{
	if( h_in > h_max )
		return h_max;
	if( h_in < h_min )
		return h_min;
	else
		return h_in;
}

void CSP::flow_patterns( int n_panels, int flow_type, util::matrix_t<int> & flow_pattern )
{
	/* !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	! This subroutine takes the number of panels, the requested flow type, and 
	! returns the corresponding flow pattern (the order of panels through which the
	! WF passes, this code is modified from the version in Type222
	!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
	
	int n_lines;
	int n_p_quarter = n_panels/4;

	switch( flow_type )
	{
	case 1:
		/* This flow pattern begins at the northmost 2 panels, splits into 2 flows, and crosses over
		at the quarter position, exiting in 2 flows on the southmost 2 panels. This is the flow
		configuration that was used for SOLAR II
		!Example = [13,14,15,16,17,18,6,5,4,3,2,1] [12,11,10,9,8,7,19,20,21,22,23,24] */
		n_lines = 2;
		//flow_pattern.resize( n_lines, n_panels/n_lines );

		for( int i = 0; i < n_p_quarter; i++)
		{
			flow_pattern.at( 0, n_p_quarter + i) = n_p_quarter - 1 - i;			// NE Quadrant - final half of flow path 0
			flow_pattern.at( 1, i ) = 2*n_p_quarter - 1 - i;					// SE Quadrant - first half of flow path 1
			flow_pattern.at( 0, i ) = 2*n_p_quarter + i;						// SW Quadrant - first half of flow path 0
			flow_pattern.at( 1, n_p_quarter + i) = 3*n_p_quarter + i;			// NW Quadrant - final half of flow path 1
		}
		return;
	case 2:
		 /* This flow pattern is the same as flow pattern #1, but in reverse. The salt enters
		on the 2 southmost panels, crosses over, and exits on the 2 northmost panels.
		Example = [1,2,3,4,5,6,17,16,15,14,13,12] [24,23,22,21,20,19,18,7,8,9,10,11,12] */
		n_lines = 2;
		//flow_pattern.resize( n_lines, n_panels/n_lines );

		for( int i = 0; i < n_p_quarter; i++)
		{
			flow_pattern.at( 0, i ) = i;										// NE Quadrant - first half of flow path 0
			flow_pattern.at( 1, n_p_quarter + i ) = n_p_quarter + i;			// SE Quadrant - final half of flow path 1
			flow_pattern.at( 0, n_p_quarter + i ) = 3*n_p_quarter - 1 - i;		// SW Quadrant - final half of flow path 0
			flow_pattern.at( 1, i ) = 4*n_p_quarter - 1 - i;					// NW Quadrant - first half of flow path 1
		}
		return;
	};
	return;
}