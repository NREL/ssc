// Functions for interpolation routines
#include <algorithm>

#include "interpolation_routines.h"

using std::min;
using std::max;

bool Linear_Interp::Set_1D_Lookup_Table( const util::matrix_t<double> &table, int * ind_var_index, int n_ind_var, int & error_index )
{
	//If a user defined fluid, check for minimum number of rows
	if ( table.nrows() < 3 ) 
	{
		error_index = -1;
		return false;
	}

	// check monotonically increasing independent variables
	for ( int i = 0; i < n_ind_var; i++ )
		for ( int r = 1; r < table.nrows(); r++ )
			if ( table.at(r, ind_var_index[i]) < table.at(r-1, ind_var_index[i]) )
			{
				error_index = i;
				return false;
			}

	// Set class member data
	m_userTable = table;	
	m_rows = table.nrows();
	m_lastIndex = m_rows * 2;	// So "hunt" scheme cannot be called 3rd property call
	m_dj = min(1, (int) pow(m_rows, 0.25) );
	m_cor = false;

	return true;
}

bool Bilinear_Interp::Set_2D_Lookup_Table( const util::matrix_t<double> &table )
{
	// Initialize class member data
	m_2axis_table = table;
	double nrows = table.nrows();
	if( nrows < 9 )
		return false;
	
	// Find number of x values in table
	double first_val = table.at(0,0);
	int i = 1;
	for( i; i < table.nrows(); i++ )
		if( table.at(i,0) == first_val )	break;
	m_nx = i;
	if( m_nx < 3 )
		return false;

	// Find number of y values in table
	m_ny = 1;
	i = 0;
	for( int j = 0; j < nrows - 1; j++ )
	{
		if( table.at(j+1,1) != table.at(j,1))
			m_ny++;
	}
	if( m_ny < 3 )
		return false;

	// Create 1D table for x values
	util::matrix_t<double> x_matrix( m_nx, 1, 0.0 );
	for( int j = 0; j < m_nx; j++ )
		x_matrix.at(j,0) = table.at( j, 0 );

	// Create 1D table for y values
	util::matrix_t<double> y_matrix( m_ny, 1, 0.0 );
	for( int j = 0; j < m_ny; j++ )
		y_matrix.at(j,0) = table.at( m_nx*j, 1 );

	// Set up 1D interpolation class instances for x and y values
	int ind_var_index[1] = {0};
	int error_index = -99;
	if( !x_vals.Set_1D_Lookup_Table( x_matrix, ind_var_index, 1, error_index ) )
		return false;
	if( !y_vals.Set_1D_Lookup_Table( y_matrix, ind_var_index, 1, error_index ) )
		return false;

	return true;
}

double Bilinear_Interp::bilinear_2D_interp( double x, double y )
{
	int i_x1 = x_vals.Get_Index( 0, x );
	int i_y1 = y_vals.Get_Index( 0, y );

	int i_x2 = i_x1 + 1;
	int i_y2 = i_y1 + 1;

	int i1 = m_nx*i_y1 + i_x1;
	double x1 = m_2axis_table.at( i1, 0 );
	double y1 = m_2axis_table.at( i1, 1 );
	double z1 = m_2axis_table.at( i1, 2 );

	int i2 = m_nx*i_y2 + i_x1;
	double x2 = m_2axis_table.at( i2, 0 );
	double y2 = m_2axis_table.at( i2, 1 );
	double z2 = m_2axis_table.at( i2, 2 );

	int i3 = m_nx*i_y2 + i_x2;
	double x3 = m_2axis_table.at( i3, 0 );
	double y3 = m_2axis_table.at( i3, 1 );
	double z3 = m_2axis_table.at( i3, 2 );

	int i4 = m_nx*i_y1 + i_x2;
	double x4 = m_2axis_table.at( i4, 0 );
	double y4 = m_2axis_table.at( i4, 1 );
	double z4 = m_2axis_table.at( i4, 2 );

	double x_frac = (x - x1)/(x4 - x1);
	double y_frac = (y - y1)/(y2 - y1);

	return (1.0-x_frac)*(1.0-y_frac)*z1 + (1.0-x_frac)*y_frac*z2 + x_frac*y_frac*z3 + x_frac*(1.0-y_frac)*z4;
}

double Linear_Interp::linear_1D_interp( int x_col, int y_col, double x )
{
	/*Given a value x, return an interpolated value y, using data xx and yy of size n. Save   **
    previous table location as jsav, and return it to the subroutine when calling. 
	Code is adapted from Numerical Recipes, 3rd Edition
	Converted to c++ from Fortran code "sam_mw_pt_propmod.f90" in November 2012 by Ty Neises */  
			
	int j = Get_Index( x_col, x );
		
	// Calculate y value using linear interpolation
	double y = m_userTable.at(j,y_col) + ((x - m_userTable.at(j,x_col))/(m_userTable.at(j+1,x_col)-m_userTable.at(j,x_col)))*(m_userTable.at(j+1,y_col) - m_userTable.at(j,y_col));

	return y;
}

int Linear_Interp::Get_Index( int x_col, double x )
{
	// Find starting index for interpolation
	int j;
	if(m_cor) {j = hunt( x_col, x );}
	else {j = locate( x_col, x );}
	
	return j;
}

int Linear_Interp::locate( int col, double x )
{
	/* Given a value x, return a value j such that x is (insofar as possible) centered in the   
    subrange xx[j..j+mm-1], where xx is the stored pointer.  The values in xx must be mono-  
    tonic, either increasing or decreasing.  The returned value is not less than 0, nor      
    greater than n-1. 
	Adapted from Numerical Recipes, 3rd Edition 
	Converted to c++ from Fortran code "sam_mw_pt_propmod.f90" in November 2012 by Ty Neises */

	int ju, jm, jl;
	// *** Assuming monotonically increasing temperature, per SetUserDefinedFluid function logic ***
	jl = 0;
	ju = m_rows - 1;
	while (ju - jl > 1)
	{
		jm = (ju + jl) / 2;
		if(x >= m_userTable.at(jm, col))
		{
			jl = jm;
		}
		else
		{
			ju = jm;
		}
	}
	if(abs(jl - m_lastIndex) > m_dj)
	{
		m_cor = false;
	}
	else
	{
		m_cor = true;
	}
	m_lastIndex = jl;
	return max(0, min(m_rows - m_m, jl - ((m_m - 2)/2)));
}

int Linear_Interp::hunt( int col, double x )
{
	/* Given a value x, return a value j such that x is (insofar as possible) centered in the
    subrange xx(j..j+mm-1), where xx is the stored data array.  The values in xx must be
    monatonic, either increasing or decreasing.  The returned value is not less than 0, nor
    greater than n-1. 
	Code is adapted from Numerical Recipes, 3rd Edition
	Converted to c++ from Fortran code "sam_mw_pt_propmod.f90" in November 2012 by Ty Neises */

	int jl = m_lastIndex, jm, ju, inc=1;
	if( jl < 0 || jl > m_rows - 1 )
	{
		jl = 0, ju = m_rows - 1;
	}
	else
	{
		if ( x >= m_userTable.at(jl, col) )	// Hunt up
		{
			ju = jl + inc;
			while( ju < m_rows - 1 && x > m_userTable.at(ju, col) )
			{
				jl = ju;
				inc += inc;
				ju = jl + inc;
			}
		}
		else		// Hunt down
		{
			ju = jl;
			jl = ju - inc;
			while( jl > 0 && x < m_userTable.at(jl, col) )
			{
				ju = jl;
				inc += inc;
				jl = ju - inc;
			}
		}

	}
	while (ju - jl > 1)	// Hunt is done, begin final bisection phase
	{
		jm = (ju + jl) / 2;
		if(x >= m_userTable.at(jm, col))
		{
			jl = jm;
		}
		else
		{
			ju = jm;
		}
	}
	if(abs(jl - m_lastIndex) > m_dj)
	{
		m_cor = false;
	}
	else
	{
		m_cor = true;
	}
	m_lastIndex = jl;
	return max(0, min(m_rows - m_m, jl - ((m_m - 2)/2)));
}