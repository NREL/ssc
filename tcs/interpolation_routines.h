// 1D linear interpolation and 2D bilinear interpolation routines
#ifndef __interpolation_routines_
#define __interpolation_routines_

#include <shared/lib_util.h>

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

class Trilinear_Interp
{
	// 4 columns by X rows
	// 0 column: x value - [1..nx, 1..nx, 1..nx, -> repeat ny times ] 
	// 1 column: y value - [1..1 (nx times), 2..2 (nx times), ... , ny..ny (nx times) ] 
	// 2 column: z value - [1..1 (nx*ny) times]
	// 3 column: result value	[(nx*ny) values]
	// Repeat list for each layer

public:
	bool Set_3D_Lookup_Table( const util::block_t<double> &table );
	double trilinear_3D_interp( double x, double y, double z);

private:
	util::block_t<double> m_3axis_table;

	int 
		m_nx,
		m_ny,
		m_nz;

	Linear_Interp x_vals;
	Linear_Interp y_vals;
	Linear_Interp z_vals;

};


#endif