// 1D linear interpolation and 2D bilinear interpolation routines
#ifndef __interpolation_routines_
#define __interpolation_routines_

#include <recore/lib_util.h>

#include "cavity_calcs.h"		// for access to "block_t"

class Linear_Interp
{
public:
	bool Set_1D_Lookup_Table( const util::matrix_t<double> &table, int * ind_var_index, int n_ind_var, int & error_index );	
	double linear_1D_interp( int x_col, int y_col, double x );
	int Get_Index( int x_col, double x );	

private:
	static const int m_m = 2;		// Integer for interpolation routine

	util::matrix_t<double> m_userTable; // 1D User table

	int m_rows;			// Number of rows in table
	int m_lastIndex;	// Integer tracking index used by interpolation routine
	int m_dj;			// Integer for interpolation routine
	bool m_cor;			// Boolean value for interpolation routine

	int locate( int col, double T_C );		// Function for interpolation routine
	int hunt( int col, double x );			// Function for interpolation routine
};

class Bilinear_Interp
{
// 3 columns by X rows
// 0 column: 1..nx, 1..nx, 1..nx, -> repeat ny times
// 1 column: 1..1 (nx times), 2..2 (nx times), ... , ny..ny (nx times)
// 2 column: (nx*ny) values
public: 
	bool Set_2D_Lookup_Table( const util::matrix_t<double> &table );
	double bilinear_2D_interp( double x, double y );

private:
	util::matrix_t<double> m_2axis_table;	// 2D (ind. x and y cols, z dependent var col)

	// 1D interpolation instances for values of each independent variable
	//Interp y_vals;
	
	int m_nx;		// Number of x values in table
	int m_ny;		// Number of y values in table

	Linear_Interp x_vals;
	Linear_Interp y_vals;
};

#endif