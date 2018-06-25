#ifndef _LIB_PV_INCIDENCE_MODIFIER_H_
#define _LIB_PV_INCIDENCE_MODIFIER_H_

#include "lib_util.h"

#define AOI_MIN 0.5
#define AOI_MAX 89.5

static const double n_cover = 1.526;   // !refractive index of glass
static const double l_thick = 0.002;   // !thickness of glass cover
static const double k_trans = 4; // proportionality constant for 

double transmittance(double theta1_deg, /* incidence angle of incoming radiation (deg) */
	double n_cover,  /* refractive index of cover material, n_glass = 1.586 */
	double n_incoming, /* refractive index of incoming material, typically n_air = 1.0 */
	double k,        /* proportionality constant assumed to be 4 (1/m) for derivation of Bouguer's law (set to zero to skip bougeur's law */
	double l_thick,  /* material thickness (set to zero to skip Bouguer's law */
	double *_theta2_deg = 0); /* thickness of cover material (m), usually 2 mm for typical module */

double iam(double theta_deg, bool ar_glass); // incidence angle modifier factor relative to normal incidence

double iam_nonorm(double theta_deg, bool ar_glass);  // non-normalized cover loss (typically use one above!)

double irradiance_through_cover(
	double theta,
	double theta_z,
	double tilt,
	double G_beam,
	double G_sky,
	double G_gnd);

#endif
