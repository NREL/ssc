/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as “System Advisor Model” or “SAM”. Except
*  to comply with the foregoing, the terms “System Advisor Model”, “SAM”, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

template< typename Real >
class __PTnonlinear
{
private:
	Real Tc, Io, Il, a, Rs, Rsh;
	Real Vguess, Iguess;		

public:
	__PTnonlinear( Real _T, Real _Io, Real _Il, Real _a, Real _aIsc, Real _Adj, Real _V, Real _I, Real _Rs, Real _Rsh, Real Egref, Real Tref )
	{	
		Tc = _T+273.15;
		
		Rs = _Rs;
		Rsh = _Rsh;
		
		// initial conditions
		Vguess = _V;
		Iguess = _I;
		
		// calculate 3 temp dependent parameters		
		Real Eg = (1-0.0002677*(Tc-Tref))*Egref;
			
		a = _a*Tc/Tref;
		Io = _Io* pow( Tc/Tref, 3)*exp( 11600 * (Egref/Tref - Eg/Tc));
		Il = _Il + (_aIsc*(1-_Adj/100))*(Tc-Tref);
	}
	
	void operator() ( const Real x[2], Real f[2] )
	{	
		Real V = x[0];
		Real I = x[1];
		
		f[0] = V*( Io/a*exp( (V+I*Rs)/a ) + 1/Rsh ) / ( 1 + Rs/Rsh + Io*Rs/a*exp( (V+I*Rs)/a ) ) - I;
		f[1] = Il - Io*(exp( (V+I*Rs)/a ) - 1) - (V + I*Rs)/Rsh - I;
	}
	
	int solve( Real *Pt )
	{
		Real x[2], resid[2];
		x[0] = Vguess;
		x[1] = Iguess;
		
		bool check = false;
		int niter = newton<Real, __PTnonlinear, 2>(x, resid, check, *this, 100, 1e-7, 1e-7, 0.9);
		if (check) niter = -5;
		
		if (niter >= 0 && check == false)
		{
			*Pt = x[0] * x[1]; // calculate power (P=V*I)
			return niter;
		}
		else
			return niter;
	}
};

template< typename Real >
bool mod6par_gamma_approx( Real *result, 
	Real Io, Real Il, Real a, 
	Real aIsc, Real Adj, Real Vmp, Real Imp, Real Rs, Real Rsh, Real Egref, Real Tref )
{
	Real Tc_last=-999, Pmax_last=-999;
	Real gamma, gamma_sum = 0.0;
	int nsum = 0;
	float nfail = 0;
	float ntotal = 0;
	
	for (Real Tc = -10; Tc <= 50.0; Tc += 3.0 )
	{
		__PTnonlinear<Real> power_t( Tc,
			Io, Il, a, aIsc, Adj, Vmp, Imp, Rs, Rsh, Egref, Tref );
		
		Real Pmax = 0.0;
		int niter = power_t.solve( &Pmax );
		if ( niter >= 0 )
		{
			if (Tc_last != -999)
			{
				gamma = (Pmax-Pmax_last)*100/(Vmp*Imp*(Tc-Tc_last));
				gamma_sum += gamma;
				nsum ++;
			}
			
			Tc_last = Tc;
			Pmax_last = Pmax;
		}
		else
		{
			nfail++;
		}
		
		ntotal++;		
	}
	
	if (nsum > 2)
		*result = gamma_sum / nsum;
			
	return ( nsum > 2 && nfail/ntotal < 0.3f ); // accept 1/3 of solutions fail over course of Tc iterations
}
