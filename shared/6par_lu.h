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

#ifndef __6_PAR_LU_H__
#define __6_PAR_LU_H__

template< typename Real, int n >
bool lu_decomp( const Real a[n][n], 
				Real lu[n][n],
				int permute[n] )
{
	const Real TINY = 1.0e-40;
	int i, imax, j, k;
	Real big, temp;
	Real vv[n];
	
	// copy over A matrix to LU
	for ( i=0; i < n; i++)
		for ( j=0; j < n; j++)
			lu[i][j] = a[i][j]; 
	
	// obtain implicit scaling information of each row, stored in vv
	for ( i=0; i < n; i++ )
	{
		big = 0.0;
		for ( j=0; j < n ; j++ )
			if ( (temp = fabs(lu[i][j])) > big )
				big = temp;
				
		if ( big == 0.0 ) return false;
		vv[i] = 1.0 / big;
	}
	
	for ( k=0; k < n; k++ )
	{
		big = 0.0;
		imax = k;
		for (i=k;i<n;i++)
		{
			temp=vv[i]*fabs(lu[i][k]);
			if (temp > big)
			{
				big=temp;
				imax=i;
			}
		}
		
		if (k != imax)
		{
			for (j=0;j<n;j++)
			{
				temp=lu[imax][j];
				lu[imax][j]=lu[k][j];
				lu[k][j]=temp;
			}
			vv[imax]=vv[k];
		}
		
		permute[k] = imax;
		if (lu[k][k] == 0.0)
			lu[k][k] = TINY;
		
		for (i=k+1;i<n;i++)
		{
			temp = lu[i][k] /= lu[k][k];
			for (j=k+1;j<n;j++)
				lu[i][j] -= temp*lu[k][j];
		}
	}
	
	return true;
}

template< typename Real, int n >
void lu_solve( const Real lu[n][n], const int permute[n],
		Real b[n], Real x[n])
{
// given LU decomposition and permutation vector, solve the 
// set of linear equations Ax = b.  The solution is returned in x,
// It is allowed for b and x to be the same vector, in which case
// b (and x) are overwritten with the solution in place

	int i, ii=0, ip, j;
	Real sum;
	
	for ( i=0; i < n; i++ )
		x[i] = b[i];
		
	for ( i=0; i < n; i++ )
	{
		ip = permute[i];
		sum = x[ip];
		x[ip] = x[i];
		if (ii != 0)
		{
			for (j=ii-1;j<i;j++)
				sum -= lu[i][j]*x[j];
		}
		else if (sum != 0.0)
		{
			ii = i+1;
		}
		
		x[i] = sum;
	}
	
	for ( i=n-1; i >= 0; i-- )
	{
		sum=x[i];
		for (j=i+1;j<n;j++)
			sum -= lu[i][j]*x[j];
			
		x[i]=sum/lu[i][i];
	}
}

#endif