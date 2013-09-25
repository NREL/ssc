#include "waterprop.h"

WPEXPORT int water_TQ( double T, double Q, property_info *data )
{
extern void water_tq_f90( double *temp, double *qual, int *error_code,
				double *pres,
				double *dens,
				double *vol,
				double *inte,
				double *enth,
				double *entr,
				double *cv,
				double *cp,
				double *cond,
				double *visc,
				double *ssnd );
				
	int err = 0;
	
	water_tq_f90( &T, &Q, &err,
		&data->P,
		&data->dens,
		&data->V,
		&data->U,
		&data->H,
		&data->S,
		&data->Cv,
		&data->Cp,
		&data->cond,
		&data->visc,
		&data->ssnd );
	
	data->T = T;
	data->Q = Q;
	
	return err;
}

WPEXPORT int water_PQ( double P, double Q, property_info *data )
{
extern void water_pq_f90( double *pres, double *qual, int *error_code,
				double *temp,
				double *dens,
				double *vol,
				double *inte,
				double *enth,
				double *entr,
				double *cv,
				double *cp,
				double *cond,
				double *visc,
				double *ssnd );
				
	int err = 0;
	
	water_pq_f90( &P, &Q, &err,
		&data->T,
		&data->dens,
		&data->V,
		&data->U,
		&data->H,
		&data->S,
		&data->Cv,
		&data->Cp,
		&data->cond,
		&data->visc,
		&data->ssnd );
	
	data->P = P;
	data->Q = Q;
	
	return err;
}

WPEXPORT int water_TP( double T, double P, property_info *data )
{
extern void water_tp_f90( double *temp, double *pres, int *error_code,
		double *dens,
		double *vol,
		double *inte,
		double *enth,
		double *entr,
		double *cv,
		double *cp,
		double *cond,
		double *visc,
		double *qual,
		double *ssnd );

	int err = 0;
	
	water_tp_f90( &T, &P, &err,
		&data->dens,
		&data->V,
		&data->U,
		&data->H,
		&data->S,
		&data->Cv,
		&data->Cp,
		&data->cond,
		&data->visc,
		&data->Q,
		&data->ssnd );
	
	data->T = T;
	data->P = P;
	
	return err;
}

WPEXPORT int water_PH( double P, double H, property_info *data )
{
extern void water_ph_f90( double *pres, double *enth, int *error_code,
		double *temp,
		double *dens,
		double *vol,
		double *inte,
		double *entr,
		double *cv,
		double *cp,
		double *cond,
		double *visc,
		double *qual,
		double *ssnd );
	
	int err = 0;
	
	water_ph_f90( &P, &H, &err,
		&data->T,
		&data->dens,
		&data->V,
		&data->U,
		&data->S,
		&data->Cv,
		&data->Cp,
		&data->cond,
		&data->visc,
		&data->Q,
		&data->ssnd );
	
	data->P = P;
	data->H = H;
	
	return err;
	
}

WPEXPORT int water_PS( double P, double S, property_info *data )
{
extern void water_ps_f90( double *pres, double *entr, int *error_code,
		double *temp,
		double *dens,
		double *vol,
		double *inte,
		double *enth,
		double *cv,
		double *cp,
		double *cond,
		double *visc,
		double *qual,
		double *ssnd );
	
	int err = 0;
	
	water_ps_f90( &P, &S, &err,
		&data->T,
		&data->dens,
		&data->V,
		&data->U,
		&data->H,
		&data->Cv,
		&data->Cp,
		&data->cond,
		&data->visc,
		&data->Q,
		&data->ssnd );
	
	data->P = P;
	data->S = S;
	
	return err;	

}


#ifdef __WITHLK__

#include <lk_invoke.h>

static void set_result( struct __lk_invoke_t *lk, property_info *data )
{
	lk_var_t ret = lk_result();
	
	lk_make_table( ret );
	lk_table_set_number( ret, "T", data->T );
	lk_table_set_number( ret, "Q", data->Q );
	lk_table_set_number( ret, "P", data->P );
	lk_table_set_number( ret, "V", data->V );
	lk_table_set_number( ret, "U", data->U );
	lk_table_set_number( ret, "H", data->H );
	lk_table_set_number( ret, "S", data->S );
	lk_table_set_number( ret, "dens", data->dens );
	lk_table_set_number( ret, "Cv", data->Cv );
	lk_table_set_number( ret, "Cp", data->Cp );
	lk_table_set_number( ret, "cond", data->cond );
	lk_table_set_number( ret, "visc", data->visc );
	lk_table_set_number( ret, "ssnd", data->ssnd );
}

LK_FUNCTION( water_tq_lk )
{
	LK_DOCUMENT( "water_TQ", "Returns water properties at the given temperature (C) and quality (0..1).", "(real:T, real:Q):table");
	double T = lk_as_number( lk_arg(0) );
	double Q = lk_as_number( lk_arg(1) );
	property_info data;	
	int code = water_TQ( T, Q, &data );
	if ( 0 == code ) set_result( lk, &data );
	else lk_return_number( code );
}


LK_FUNCTION( water_tp_lk )
{
	LK_DOCUMENT( "water_TP", "Returns water properties at the given temperature (C) and pressure (kPa).", "(real:T, real:P):table" );
	double T = lk_as_number( lk_arg(0) );
	double P = lk_as_number( lk_arg(1) );
	property_info data;
	int code = water_TP( T, P, &data );
	if ( 0 == code ) set_result( lk, &data );
	else lk_return_number( code );
}

LK_FUNCTION( water_ph_lk )
{
	LK_DOCUMENT( "water_PH", "Returns water properties at the given pressure (kPa) and enthalpy (kJ/kg).", "(real:P, real:H):table" );
	double P = lk_as_number( lk_arg(0) );
	double H = lk_as_number( lk_arg(1) );
	property_info data;
	int code = water_PH( P, H, &data );
	if ( 0 == code ) set_result( lk, &data );
	else lk_return_number( code );
}

LK_FUNCTION( water_ps_lk )
{
	LK_DOCUMENT( "water_PS", "Returns water properties at the given pressure (kPa) and entropy (kJ/kg-K).", "(real:P, real:S):table" );
	double P = lk_as_number( lk_arg(0) );
	double S = lk_as_number( lk_arg(1) );
	property_info data;
	int code = water_PS( P, S, &data );
	if ( 0 == code ) set_result( lk, &data );
	else lk_return_number( code );
}

LK_FUNCTION( water_pq_lk )
{
	LK_DOCUMENT( "water_PQ", "Returns water properties at the given pressure (kPa) and quality (0..1).", "(real:P, real:Q):table" );
	double P = lk_as_number( lk_arg(0) );
	double Q = lk_as_number( lk_arg(1) );
	property_info data;
	int code = water_PQ( P, Q, &data );
	if ( 0 == code ) set_result( lk, &data );
	else lk_return_number( code );
}

LK_BEGIN_EXTENSION()
	water_tq_lk,
	water_tp_lk,
	water_ph_lk,
	water_ps_lk,
	water_pq_lk
LK_END_EXTENSION()

#endif

