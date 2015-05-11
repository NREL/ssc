#include <string>
#include <numeric>
#include <limits>
#include <math.h>
#include <float.h>

#include "lsqfit.h"
#include "lib_iec61853.h"


const char *iec61853_module_t::module_type_names[_maxTypeNames] = { "monoSi", "multiSi", "CdTe", "CIS", "CIGS", "Amorphous" };
const char *iec61853_module_t::col_names[COL_MAX] = { "Irr (W/m2)", "Temp (C)", "Pmp (W)", "Vmp (V)", "Voc (V)", "Isc (A)" };
const char *iec61853_module_t::par_names[PARMAX] = { "IL", "IO", "RS", "RSH" };

iec61853_module_t::iec61853_module_t()
{
	_imsg = 0;
	alphaIsc = n = Il = Io = C1 = C2 = C3
			= D1 = D2 = D3 = Egref = std::numeric_limits<double>::quiet_NaN();
		
	betaVoc = gammaPmp = Area = std::numeric_limits<double>::quiet_NaN();
	
	Vmp0 = Imp0 = Voc0 = Isc0 = std::numeric_limits<double>::quiet_NaN();

	NcellSer = 0;
	GlassAR = false;
	for( int i=0;i<5;i++ )
		AMA[i] = std::numeric_limits<double>::quiet_NaN();

}

void iec61853_module_t::set_fs267_from_matlab()
{		
	alphaIsc=0.000472; n=1.451; Il=1.18952; Io=2.08556e-09;
	C1=1932.09; C2=474.895; C3=1.48756;
	D1=11.6276; D2=-0.0770137; D3=0.237277;
	Egref=0.73769;
}


#define OUTLN(text) if (_imsg) _imsg->Outln(text)
#define PRINTF(format, ...) if(_imsg) _imsg->Printf(format, __VA_ARGS__)

#define NCONDITIONS_MIN 5

void sort_2vec( std::vector<double> &a, std::vector<double> &b )
{
	// quick & dirty selection sort
	double buf;
	int count = (int)a.size();
	for (int i=0;i<count-1;i++)
	{
		int smallest = i;

		for (int j=i+1;j<count;j++)
			if ( a[j] < a[smallest] )
				smallest = j;

		// swap
		buf = a[i];
		a[i] = a[smallest];
		a[smallest] = buf;

		buf = b[i];
		b[i] = b[smallest];
		b[smallest] = buf;

	}
}

bool linfit( std::vector<double> &yvec, std::vector<double> &xvec, double *mout, double *bout )
{
	// adapted from http://david.swaim.com/cpp/linreg.htm

	if ( xvec.size() != yvec.size() ) return false;

	double a, b, sumX, sumY, sumXsquared, sumYsquared, sumXY, n;
	double coefD = 0, coefC = 0, stdError = 0;
	a = b = sumX = sumY = sumXsquared = sumYsquared = sumXY = 0.0;
	n = 0L;

	for( size_t i=0;i<yvec.size();i++ )
	{
		double x = xvec[i];
		double y = yvec[i];
		n++;
		sumX += x;
		sumY += y;
		sumXsquared += x * x;
		sumYsquared += y * y;
		sumXY += x * y;
		
		if ( i == 0 ) continue;

        if (fabs( double(n) * sumXsquared - sumX * sumX) > DBL_EPSILON)
        {
            b = ( double(n) * sumXY - sumY * sumX) /
                ( double(n) * sumXsquared - sumX * sumX);
            a = (sumY - b * sumX) / double(n);

            double sx = b * ( sumXY - sumX * sumY / double(n) );
            double sy2 = sumYsquared - sumY * sumY / double(n);
            double sy = sy2 - sx;

            coefD = sx / sy2;
            coefC = sqrt(coefD);
            stdError = sqrt(sy / double(n - 2));
        }
        else
        {
            a = b = coefD = coefC = stdError = 0.0;
        }
    }

	*mout = b;
	*bout = a;
	return true;
}

bool iec61853_module_t::tcoeff( util::matrix_t<double> &input, size_t icol, double irr, double *tempc, bool print_table )
{
	*tempc = std::numeric_limits<double>::quiet_NaN();
	
	// obtain list of Voc/Isc/Pmp values at 1000 W/2 for all given temperatures
	std::vector<double> Val_stc;
	std::vector<double> Tc_stc;
	for( size_t i=0;i<input.nrows();i++ )
	{
		if ( input(i,COL_IRR) == irr )
		{
			Val_stc.push_back( input(i, icol ) );
			Tc_stc.push_back( input(i, COL_TC ) );
		}
	}

	if ( Val_stc.size() < 3 )
	{
		PRINTF("insufficient measurements at %lg W/m2, at least 3 required at different temperatures to calculate temperature coefficient of %s.  only %d detected", irr, col_names[icol], (int)Val_stc.size() );
		return false;
	}

	// sort the values by temperature
	sort_2vec( Tc_stc, Val_stc );

	if ( print_table )
	{
		for( size_t i=0;i<Tc_stc.size();i++ )
			PRINTF("%d\tTc,%s @ %lg\t%lg\t%lg", i, col_names[icol], irr, Tc_stc[i], Val_stc[i] );
	}

	// do a linear fit y=mx+b
	double m, b;
	if ( !linfit( Val_stc, Tc_stc, &m, &b ) )
	{
		PRINTF("linear regression failed for temperature coefficient of %s calculation", col_names[icol]);
		return false;
	}

	*tempc = m;
	return true;
}

int gauss( double A[4][4], double B[4] )
{
	int I,J,K,KP1,IBIG;
	double BIG, TERM;
	
	for( K=0;K<3;K++)
	{
		KP1=K+1;
		BIG = fabs( A[K][K] ); 
		if ( BIG < 1.0e-05)
		{
			IBIG=K;                                                                  

			for( I=KP1; I < 4; I++ )
			{
				if( fabs( A[I][K] ) <= BIG )
					continue;
					
				BIG = fabs( A[I][K] );
				IBIG = I;
			}

			if( BIG <= 0. )
				return 5;

			if( IBIG != K )
			{
				for(J=K;J<4;J++)
				{
					TERM = A[K][J];
					A[K][J] = A[IBIG][J];
					A[IBIG][J] = TERM;
				}
				TERM = B[K];
				B[K] = B[IBIG];
				B[IBIG] = TERM;
			}
		}

		for( I=KP1;I<4;I++ )                                                            
		{
			TERM = A[I][K]/A[K][K];

			for (J=KP1;J<4;J++)                                           
				A[I][J] = A[I][J]-A[K][J]*TERM;                                 

			B[I] = B[I]-B[K]*TERM;
		}
	}

	if( fabs( A[3][3] ) > 0.0 )       
	{
		B[3] =  B[3]/A[3][3];
		B[2] = (B[2]-A[2][3]*B[3])/A[2][2];
		B[1] = (B[1]-A[1][2]*B[2]-A[1][3]*B[3])/A[1][1];
		B[0] = (B[0]-A[0][1]*B[1]-A[0][2]*B[2]-A[0][3]*B[3])/A[0][0];
	}
	else
		return 5;

	return 0;
}

bool iec61853_module_t::solve( double Voc, double Isc, double Vmp, double Imp, double a,
			double *p_Il, double *p_Io, double *p_Rs, double *p_Rsh )
{
	// initial guesses must be passed in
	double Il = *p_Il;
	double Io = *p_Io;
	double Rs = *p_Rs;
	double Rsh = *p_Rsh;

	double A[4][4], B[4], T[4];

	for( size_t i=0;i<4;i++ ) 
		T[i] = std::numeric_limits<double>::quiet_NaN();
	
	static const double tol = 0.001;
	static const int MaxIter = 250;
	static const double urelax = 15;

	//PRINTF("iterative solution... max iterations %d, underrelaxation %lg", MaxIter, urelax);
	int jj = 0;
	while ( jj < MaxIter )
	{
		// limit the parameter values a bit
		if ( Il < 0.01 ) Il = 0.01;
		if ( Rs < 0.001 ) Rs = 0.001;
		if ( Rs > 100 ) Rs = 100;
		if ( Rsh < 0.1 ) Rsh = 0.1;
		if ( Rsh > 100000 ) Rsh = 100000;
		if ( Io < 1e-30 ) Io = 1e-30;
		if ( Io > 1e-4 ) Io = 1e-4;
		
		
		//PRINTF("iteration %d:  Il=%lg Io=%lg Rs=%lg Rsh=%lg", jj, Il, Io, Rs, Rsh );


		// generated by MATLAB symbolic_jacobian_4x4.m
		
		double Rshsq = Rsh*Rsh;
		double asq = a*a;
		double Iosq = Io*Io;
		double temp1 = (Rs/Rsh + (Io*Rs*exp((Vmp + Imp*Rs)/a))/a + 1);
		double temp1sq = temp1*temp1;

		A[0][0] = 1;
		A[0][1] = 1 - exp((Isc*Rs)/a);
		A[0][2] = - Isc/Rsh - (Io*Isc*exp((Isc*Rs)/a))/a;
		A[0][3] = (Isc*Rs)/Rshsq;

		A[1][0] = -1;
		A[1][1] = exp(Voc/a) - 1;
		A[1][2] = 0;
		A[1][3] = -Voc/Rshsq;

		A[2][0] = 1;
		A[2][1] = 1 - exp((Vmp + Imp*Rs)/a);
		A[2][2] = - Imp/Rsh - (Imp*Io*exp((Vmp + Imp*Rs)/a))/a;
		A[2][3] = (Vmp + Imp*Rs)/Rshsq;

		A[3][0] = 0;
		A[3][1] = (Rs*Vmp*exp((Vmp + Imp*Rs)/a)*(1/Rsh + (Io*exp((Vmp + Imp*Rs)/a))/a))/(a*temp1sq) - (Vmp*exp((Vmp + Imp*Rs)/a))/(a*temp1);
		A[3][2] = (Vmp*(asq + Iosq*Rshsq*exp((2*(Vmp + Imp*Rs))/a) + 2*Io*Rsh*a*exp((Vmp + Imp*Rs)/a) - Imp*Io*Rshsq*exp((Vmp + Imp*Rs)/a)))/pow( (Rs*a + Rsh*a + Io*Rs*Rsh*exp((Vmp + Imp*Rs)/a)), 2.0 );
		A[3][3] = Vmp/(Rshsq*temp1) - (Rs*Vmp*(1/Rsh + (Io*exp((Vmp + Imp*Rs)/a))/a))/(Rshsq*temp1sq);


		B[0] = Il - Isc - Io*(exp((Isc*Rs)/a) - 1) - (Isc*Rs)/Rsh;
		B[1] = Voc/Rsh - Il + Io*(exp(Voc/a) - 1);
		B[2] = Il - Imp - Io*(exp((Vmp + Imp*Rs)/a) - 1) - (Vmp + Imp*Rs)/Rsh;
		B[3] = Imp - (Vmp*(1/Rsh + (Io*exp((Vmp + Imp*Rs)/a))/a))/(Rs/Rsh + (Io*Rs*exp((Vmp + Imp*Rs)/a))/a + 1);

					
		int ierr = gauss( A, B );
		
		if ( ierr != 0 )
		{
			PRINTF( "singularity in gauss() in solution of four parameter nonlinear equation, iteration %d", jj );
			
			OUTLN( "A matrix:");
			for (int i=0;i<4;i++)
				for( int j=0;j<4;j++ )
					PRINTF( "%lg%c", A[i][j], j<3 ? '\t' : '\n' );

				
			OUTLN( "B vector:");
			for( int j=0;j<4;j++ )
				PRINTF( "%lg", B[j]);

			OUTLN( "tolerances:" );
			for( int j=0;j<4;j++ )
				PRINTF( "%lg", T[j]);

			OUTLN("current guesses:" );
			PRINTF("Il=%lg Io=%lg Rs=%lg Rsh=%lg", Il, Io, Rs, Rsh );


			return false;  // singularity
		}
			
		Il = Il - B[0]/urelax;
		Io = Io - B[1]/urelax;
		Rs = Rs - B[2]/urelax;
		Rsh = Rsh - B[3]/urelax;
		
		T[0] = fabs(B[0]/Il);
		T[1] = fabs(B[1]/Io);
		T[2] = fabs(B[2]/Rs);
		T[3] = fabs(B[3]/Rsh);

		int nck = 0;
		for( size_t i=0;i<4;i++ )
			if ( T[i] > tol ) nck++;
		
		if( nck == 0 )
			break;
		
		jj++;
	}

	// check convergence
	if ( jj == MaxIter )
	{
		PRINTF("Reached maximum iterations of %d in solve()", MaxIter );
		return false;
	}

	// save outputs

	*p_Il = Il;
	*p_Io = Io;
	*p_Rs = Rs;
	*p_Rsh = Rsh;

	return true;
}

double Io_fit_eqn( double _x, double *par, void * )
{
	double T = _x; 
	
    const double Tref = 298.15;
    double dT = (T+273.15)-Tref;
    double Egref = par[0];    
    
    double Eg = (1-0.0002677*T)*Egref;
	return pow((Tref+dT)/Tref,3)*exp( 11600 * (Egref/Tref - Eg/(Tref+dT)));
}

double Rsh_fit_eqn( double _x, double *par, void * )
{
	return par[0] + par[1]*( pow(1000/_x, par[2]) - 1 );
}

double Rs_fit_eqn( double _x, double *par, void * )
{
	return par[0] + ( 1-_x/1000) *par[1]*pow(1000/_x, 2.0);
}

bool iec61853_module_t::calculate( util::matrix_t<double> &input, int nseries, int Type, 
	util::matrix_t<double> &par, bool verbose )
{
	if (input.ncols() != COL_MAX) {
		PRINTF( "incorrect number of data columns in input.  %d required", COL_MAX );
		return false;
	}

	if ( input.nrows() < NCONDITIONS_MIN ) {
		PRINTF( "insufficient number of test conditions, %d minimum", NCONDITIONS_MIN );
		return false;
	}

	// get STC module conditions (1000 W/m2, 25 deg C)
	// assume these are the 'official' STC ratings
	double Pmp0=-1;
	int idx_stc = -1;
	for( size_t i=0;i<input.nrows();i++ )
	{
		if ( input(i,COL_IRR) == 1000.0
			&& input(i,COL_TC) == 25.0 )
		{
			Pmp0 = input(i,COL_PMP);
			Vmp0 = input(i,COL_VMP);
			Imp0 = Pmp0/Vmp0;
			Voc0 = input(i,COL_VOC);
			Isc0 = input(i,COL_ISC);
			idx_stc = i;
			break;
		}
	}

	if ( idx_stc < 0 )
	{
		OUTLN("a measurement at STC conditions (1000 W/m2, 25 C) is required, but could not be found.");
		return false;
	}

	if( verbose ) PRINTF("module STC ratings: Pmp=%lg Vmp=%lg Imp=%lg Voc=%lg Isc=%lg", Pmp0, Vmp0, Imp0, Voc0, Isc0 );
	
	// estimate beta VOC at STC irradiance (1000 W/m2)
	if (!tcoeff( input, COL_VOC, 1000.0, &betaVoc, false ))
		return false;

	// estimate the alpha ISC at STC conditions
	if (!tcoeff( input, COL_ISC, 1000.0, &alphaIsc, false ))
		return false;

	// estimate the gamma PMP at STC conditions
	if ( !tcoeff( input, COL_PMP, 1000.0, &gammaPmp, false ))
		return false;


	gammaPmp *= 100.0 / (Vmp0 * Imp0 );

	if( verbose ) PRINTF("betaVoc=%lg (V/'C)  alphaIsc=%lg (A/'C)   gammaPmp=%lg (%/'C)", betaVoc, alphaIsc, gammaPmp);

	// calculate diode nonideality factor 'n' at each condition
	std::vector<double> nfac( input.nrows(), std::numeric_limits<double>::quiet_NaN() );
	const double q = 1.6e-19;
	const double k = 1.38e-23;
	double nsum = 0, ncount = 0;
	if( verbose ) OUTLN("estimated diode nonideality factors at each condition:");
	for( size_t i=0;i<input.nrows();i++ )
	{
        double VT = k*( input(i,COL_TC) + 273.15 )/q;    
        
        // SNL Voc equation solved for diode factor
        nfac[i] =  ( ( input(i,COL_VOC) - betaVoc*( input(i,COL_TC) - 25 ) - Voc0)) 
            / ( nseries * VT * log( input(i,COL_IRR)/1000 ) );
        


        if ( std::isfinite( nfac[i] ) )
		{
			ncount++;
			nsum += nfac[i];
			if( verbose ) PRINTF( "%lg  ", nfac[i] );
		}
	}

	double navg = nsum/ncount;

	if( verbose ) PRINTF("\naverage n=%lg",navg);
	
	for( size_t i=0;i<input.nrows();i++ )
	{
		if ( !std::isfinite( nfac[i] ) )
		{
			if( verbose ) PRINTF(" non-finite diode factor at condition %d (%lg W/m2, %lg C)", i+1, input(i,COL_IRR), input(i,COL_TC) );
			nfac[i] = navg;
		}
	}

	if( verbose ) PRINTF("non-finite diode nonideality factors at %d conditions filled with average value.", input.nrows() - (int)ncount);

	// reference guesses for module resistances, based on Dobos 2012.	
	double Rs_scale, Rsh_scale;
	switch( Type )
	{
	case Amorphous:
		Rs_scale = 0.59;
		Rsh_scale = 0.922;
		break;
	case CdTe:
		Rs_scale = 0.46;
		Rsh_scale = 1.11;
		break;
	case CIGS:
		Rs_scale = 0.55;
		Rsh_scale = 1.22;
		break;
	case CIS:
		Rs_scale = 0.61;
		Rsh_scale = 1.07;
		break;
	case monoSi:
		Rs_scale = 0.32;
		Rsh_scale = 4.92;
		break;
	case multiSi:
	default:
		Rs_scale = 0.34;
		Rsh_scale = 5.36;
		break;
	}

	double Rs_ref0 = Rs_scale * (Voc0 - Vmp0)/Imp0;

	if (Rs_ref0 < 0.02) Rs_ref0 = 0.02;
	if (Rs_ref0 > 60) Rs_ref0 = 60;
		
	double Rsh_ref0 = Rsh_scale * Voc0/(Isc0-Imp0);

	if( verbose ) PRINTF("reference guess for module resistances @ STC  Rs=%lg Rsh=%lg", Rs_ref0, Rsh_ref0 );

	// storage for parameters at each condition
	par.resize_fill( input.nrows(), PARMAX, std::numeric_limits<double>::quiet_NaN() );
	int nsuccess = 0;
	
	if( verbose ) PRINTF("solving for Il, Io, Rs, Rsh at %d conditions...", (int)input.nrows() );


	// now solve for four parameters (Il, Io, Rs, Rsh) at each test condition	
	for( size_t i=0;i<input.nrows();i++ )
	{
		double Voc = input(i,COL_VOC);
		double Isc = input(i,COL_ISC);
		double Vmp = input(i,COL_VMP);
		double Imp = input(i,COL_PMP)/Vmp;
		double TcK = input(i,COL_TC)+273.15;
		double Irr = input(i,COL_IRR);
		
		// make a guess at the parameters
		double a =  nseries*nfac[i]*k*TcK/q;
		double Il = (Isc + alphaIsc*(TcK-298))*Irr/1000.0;
		double Rsh = Rsh_ref0 * 1000.0/Irr;
		double Io = ( Il - Voc/Rsh )/( exp(Voc/a) - 1 );
		double Rs = Rs_ref0;		
		
		if ( solve( Voc, Isc, Vmp, Imp, a,
			&Il, &Io, &Rs, &Rsh ) )
		{
			par(i,IL) = Il;
			par(i,IO) = Io;
			par(i,RS) = Rs;
			par(i,RSH) = Rsh;
			nsuccess++;
		}
		else
		{
			PRINTF("solution failed at condition %d", i );
		}
		

	}

	if ( verbose )
	{
		PRINTF( "%d of %d conditions successfully solved. parameter table:\n#\tIL\tIO\tRS\tRSH", nsuccess, (int)input.nrows() );
		for( size_t i=0;i<par.nrows();i++ )
			PRINTF("%d\t%lg\t%lg\t%lg\t%lg", (int)i+1, par(i,IL), par(i,IO), par(i,RS), par(i,RSH) );
	}


	// fit Io equation to data - assume only temperature dependence
	// first, create list of all unique temperatures in input
	std::vector<double> temps;
	for( size_t i=0;i<input.nrows();i++ )
	{
		double T = input(i,COL_TC);
		if ( std::find( temps.begin(), temps.end(), T ) == temps.end() )
			temps.push_back( T );
	}

	if ( temps.size() < 3 )
	{
		OUTLN("insufficient test data, at least three different temperature conditions are required for fitting.");
		return false;
	}

	double Io_stc = par( idx_stc, IO );

	// find average Io value at each temp value
	std::vector<double> Io_avgs( temps.size() );
	for( size_t i=0;i<temps.size();i++ )
	{
		double T = temps[i];

		double accum = 0;
		double nvals = 0;
		for( size_t j=0;j<input.nrows();j++ )
		{
			if ( input(j,COL_TC) == T )
			{
				accum += par(j,IO);
				nvals++;
			}
		}
		Io_avgs[i] = ( accum / nvals / Io_stc );
	}

	// do a nonlinear least squares to fit the Io equation as a function of temperature
	// free parameter is Egref. initial guess is 1.0
	double Egref_fit[1] = { 1.0 };
	if ( !lsqfit( Io_fit_eqn, 0, Egref_fit, 1, &temps[0], &Io_avgs[0], temps.size() ) )
	{
		OUTLN("error in nonlinear least squares fit for Io equation");
		return false;
	}

	if( verbose ) PRINTF("determined parameter Egref=%lg.  Io_stc=%lg", Egref_fit[0], Io_stc );



	// determine Rsh fitting C1, C2, C3
	// get list of unique irradiance values
	std::vector<double> irrads;
	for( size_t j=0;j<input.nrows();j++ )
	{
		double I = input(j,COL_IRR);
		if ( std::find( irrads.begin(), irrads.end(), I ) == irrads.end() )
			irrads.push_back( I );
	}

	// get average Rsh from each condition with the corresponding irradiance
	std::vector<double> Rsh_avgs( irrads.size() );
	for( size_t i=0;i<irrads.size();i++ )
	{
		double Irr = irrads[i];
		double accum = 0;
		double nvals = 0;
		for( size_t j=0;j<input.nrows();j++ )
		{
			if( input(j,COL_IRR) == Irr )
			{
				accum += par(j,RSH);
				nvals++;
			}
		}
		Rsh_avgs[i] = accum / nvals;

		if( verbose ) PRINTF("Rsh_avg[@ %lg W/m2] = %lg", irrads[i], Rsh_avgs[i] );
	}


	double C[3] = { 5000, 500, 0.5 }; // initial guesses for lsqfit
	if ( !lsqfit( Rsh_fit_eqn, 0, C, 3, &irrads[0], &Rsh_avgs[0], irrads.size() ) )
	{
		OUTLN("error in nonlinear least squares fit for Rsh equation");
		return false;
	}

	if ( verbose ) PRINTF( "determined Rsh equation parameters C1=%lg C2=%lg C3=%lg", C[0], C[1], C[2] );
	
	// determine series resistance Rs fitting

	// first, do a least square parameter fit for Rs as a function of irradiance
	// at each temperature condition
	std::vector<double> Dpar0( temps.size() );	
	D1 = D2 = D3 = 0;

	for( size_t i=0;i<temps.size();i++ )
	{
		// find all irradiance and Rs values at this temperature
		std::vector<double> Ivec, Rsvec;
		for( size_t j=0;j<input.nrows();j++ )
		{
			if ( input(j,COL_TC) == temps[i] )
			{
				Ivec.push_back( input(j,COL_IRR) );
				Rsvec.push_back( par(j, RS ) );
			}
		}

		if ( Ivec.size() < 3 )
		{
			PRINTF("must have measurements at two different irradiance levels for each unique temperature considered, only %d found at %lg C", (int) Ivec.size(), temps[i] );
			return false;
		}

		double Dpr[2] = { 10.0, 1.0 };
		if ( !lsqfit( Rs_fit_eqn, 0, Dpr, 2, &Ivec[0], &Rsvec[0], Ivec.size() ) )
		{
			PRINTF("error in nonlinear least squares fit for Rs equation at %lg C", temps[i] );
			return false;
		}

		Dpar0[i] = Dpr[0];
		D3 += Dpr[1];
	}

	D3 /= temps.size();

	// now do a linear fit on the first parameter as a function of temperature	
	if ( !linfit( Dpar0, temps, &D2, &D1 ) )
	{
		OUTLN("error in linear fit for Rs equation D1 and D2 parameters");
		return false;
	}

	if( verbose ) PRINTF("determined Rs equation parameters D1=%lg D2=%lg D3=%lg", D1, D2, D3 );

	
	if( verbose ) OUTLN("parameter fitting to iec61853 test data successful." );
	
	// set outputs
	n = nfac[idx_stc];
	Il = par(idx_stc, IL);
	Io = par(idx_stc, IO);
	Egref = Egref_fit[0];
	C1 = C[0];
	C2 = C[1];
	C3 = C[2];

	return true;
}

bool iec61853_module_t::operator() ( pvinput_t &input, double TcellC, double opvoltage, pvoutput_t &out )
{
	/* initialize output first */
	out.Power = out.Voltage = out.Current = out.Efficiency = out.Voc_oper = out.Isc_oper = 0.0;
	
	// plane of array irradiance, W/m2
	double poa = input.Ibeam + input.Idiff + input.Ignd; 

	// transmitted poa through module cover
	double iamf = iam( input.IncAng, GlassAR );
	double tpoa = poa - ( 1.0 - iamf )*input.Ibeam*cos(input.IncAng*3.1415926/180.0);
	if( tpoa < 0.0 ) tpoa = 0.0;
	
	// spectral effect via AM modifier
	double ama = air_mass_modifier( input.Zenith, input.Elev, AMA );
	tpoa *= ama;	
	
	double Tc = input.Tdry + 273.15;
	if ( poa >= 0.5 && tpoa >= 0.5 )
	{
		Tc = TcellC + 273.15;
		double q = 1.6e-19;
		double k = 1.38e-23;
		double aop = NcellSer*n*k*Tc/q;
		double Ilop = tpoa/1000*(Il + alphaIsc*(Tc-298.15));
		double Egop = (1-0.0002677*(Tc-298.15))*Egref;
		double Ioop = Io*pow(Tc/298.15,3.0)*exp( 11600 * (Egref/298.15 - Egop/Tc));
		double Rsop = D1 + D2*(Tc-298.15) + D3*( 1-tpoa/1000.0)*pow(1000.0/poa,2.0);
		double Rshop = C1 + C2*( pow(1000.0/tpoa,C3)-1 );
					

		// at some very low irradiances, these parameters can blow up due to
		// equations and keep the model from solving
		//if ( Rsop > 1000 ) Rsop = 10000;
		//if ( Rshop > 25000 ) Rshop = 25000;

		double V_oc = openvoltage_5par( Voc0, aop, Ilop, Ioop, Rshop );
		double I_sc = Ilop/(1+Rsop/Rshop);
		
		double P, V, I;
		
		if ( opvoltage < 0 )
		{
			P = maxpower_5par( V_oc, aop, Ilop, Ioop, Rsop, Rshop, &V, &I );
			if ( P < 0 ) P = 0;
		}
		else
		{ // calculate power at specified operating voltage
			V = opvoltage;
			if (V >= V_oc) I = 0;
			else I = current_5par( V, 0.9*Ilop, aop, Ilop, Ioop, Rsop, Rshop );

			if ( I < 0 ) { I=0; V=0; }
			P = V*I;
		}
						
		out.Power = P;
		out.Voltage  = V;
		out.Current = I;
		out.Efficiency = P/(Area*poa);
		out.Voc_oper = V_oc;
		out.Isc_oper = I_sc;
		out.CellTemp = Tc - 273.15;
	}

	return out.Power >= 0;
}
