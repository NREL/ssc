/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "csp_solver_pt_receiver.h"
#include "csp_solver_core.h"

#include "Ambient.h"
#include "definitions.h"

C_pt_receiver::C_pt_receiver(double h_tower /*m*/, double epsilon /*-*/,
    double T_htf_hot_des /*C*/, double T_htf_cold_des /*C*/,
    double f_rec_min /*-*/, double q_dot_rec_des /*MWt*/,
    double rec_su_delay /*hr*/, double rec_qf_delay /*-*/,
    double m_dot_htf_max_frac /*-*/, double eta_pump /*-*/,
    double od_tube /*mm*/, double th_tube /*mm*/,
    double piping_loss_coef /*Wt/m2-K*/, double pipe_length_add /*m*/,
    double pipe_length_mult /*-*/,
    int field_fl /*-*/, util::matrix_t<double> field_fl_props,
    int tube_mat_code /*-*/,
    int night_recirc /*-*/, int clearsky_model /*-*/,
    std::vector<double> clearsky_data)
{
    // Design parameters
    m_h_tower = h_tower;    //[m]
    m_epsilon = epsilon;    //[-]
    m_T_htf_hot_des = T_htf_hot_des + 273.15;       //[K] convert from C
    m_T_htf_cold_des = T_htf_cold_des + 273.15;     //[K] convert from C
    m_f_rec_min = f_rec_min;    //[-]
    m_q_rec_des = q_dot_rec_des*1.E6;       //[Wt] convert from MWt
    m_rec_su_delay = rec_su_delay;  //[hr]
    m_rec_qf_delay = rec_qf_delay;  //[-]
    m_m_dot_htf_max_frac = m_dot_htf_max_frac;  //[-]
    m_eta_pump = eta_pump;  //[-]
    m_od_tube = od_tube*1.E-3;  //[m] convert from mm
    m_th_tube = th_tube*1.E-3;  //[m] convert from mm

    m_piping_loss_coefficient = piping_loss_coef;    //[Wt/m2-K]
    m_pipe_length_add = pipe_length_add;    //[m]
    m_pipe_length_mult = pipe_length_mult;  //[-]

    m_field_fl = field_fl;  //[-]
    m_field_fl_props = field_fl_props;

    m_tube_mat_code = tube_mat_code;    //[-]

    m_night_recirc = night_recirc;  //[-]
    m_clearsky_model = clearsky_model;   //[-]
    m_clearsky_data = clearsky_data;

    // State variables
    m_mode = C_csp_collector_receiver::E_csp_cr_modes::OFF;
    m_mode_prev = C_csp_collector_receiver::E_csp_cr_modes::OFF;

	error_msg = "";
	m_m_dot_htf_des = std::numeric_limits<double>::quiet_NaN();
}

C_csp_collector_receiver::E_csp_cr_modes C_pt_receiver::get_operating_state()
{
    return m_mode_prev;
}

void C_pt_receiver::init()
{
    ambient_air.SetFluid(ambient_air.Air);

    // Declare instance of fluid class for FIELD fluid
    if (m_field_fl != HTFProperties::User_defined && m_field_fl < HTFProperties::End_Library_Fluids)
    {
        if (!field_htfProps.SetFluid(m_field_fl))
        {
            throw(C_csp_exception("Receiver HTF code is not recognized", "MSPT receiver"));
        }
    }
    else if (m_field_fl == HTFProperties::User_defined)
    {
        // Check that 'm_field_fl_props' is allocated and correct dimensions
        int n_rows = (int)m_field_fl_props.nrows();
        int n_cols = (int)m_field_fl_props.ncols();
        if (n_rows > 2 && n_cols == 7)
        {
            if (!field_htfProps.SetUserDefinedFluid(m_field_fl_props))
            {
                error_msg = util::format(field_htfProps.UserFluidErrMessage(), n_rows, n_cols);
                throw(C_csp_exception(error_msg, "MSPT receiver"));
            }
        }
        else
        {
            error_msg = util::format("The user defined field HTF table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", n_rows, n_cols);
            throw(C_csp_exception(error_msg, "MSPT receiver"));
        }
    }
    else
    {
        throw(C_csp_exception("Receiver HTF code is not recognized", "MSPT receiver"));
    }

    // Declare instance of htf class for receiver tube material
    if (m_tube_mat_code == HTFProperties::Stainless_AISI316 || m_tube_mat_code == HTFProperties::T91_Steel ||
        m_tube_mat_code == HTFProperties::N06230 || m_tube_mat_code == HTFProperties::N07740)
    {
        if (!tube_material.SetFluid(m_tube_mat_code))
        {
            throw(C_csp_exception("Tube material code not recognized", "MSPT receiver"));
        }
    }
    else if (m_tube_mat_code == HTFProperties::User_defined)
    {
        throw(C_csp_exception("Receiver material currently does not accept user defined properties", "MSPT receiver"));
    }
    else
    {
        error_msg = util::format("Receiver material code, %d, is not recognized", m_tube_mat_code);
        throw(C_csp_exception(error_msg, "MSPT receiver"));
    }

    return;
}

HTFProperties *C_pt_receiver::get_htf_property_object()
{
    return &field_htfProps;
}

double C_pt_receiver::get_startup_time()
{
    return m_rec_su_delay * 3600.; // sec
}

double C_pt_receiver::get_startup_energy()
{
    return m_rec_qf_delay * m_q_rec_des * 1.e-6;  // MWh
}

double C_pt_receiver::get_clearsky(const C_csp_weatherreader::S_outputs &weather, double hour)
{
	if (m_clearsky_model == -1 || weather.m_solzen >= 90.0)
		return 0.0;

	double clearsky;
	if (m_clearsky_model == 0)  // Use user-defined array
	{
		int nsteps = (int)m_clearsky_data.size();
		double baseline_step = 8760. / double(nsteps);  // Weather file time step size (hr)
		int step = (int)((hour - 1.e-6) / baseline_step);
		step = std::min(step, nsteps - 1);
		clearsky = m_clearsky_data.at(step);
	}
	else  // use methods in SolarPILOT
	{ 
		std::vector<int> monthlen{ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
		int doy = weather.m_day;
		int m = weather.m_month - 1;
		for (int j = 0; j < m; j++)
			doy += monthlen[j];

		double pres = weather.m_pres;
		if (pres < 20. && pres > 1.0)				// Some weather files seem to have inconsistent pressure units... make sure that value is of correct order of magnitude
			pres = weather.m_pres * 100.;			// convert to mbar
		double dpres = pres * 1.e-3 * 0.986923;		// Ambient pressure in atm
		double del_h2o = exp(0.058 * weather.m_tdew + 2.413);  // Correlation for precipitable water in mm H20 (from Choudhoury INTERNATIONAL JOURNAL OF CLIMATOLOGY, VOL. 16, 663-475 (1996))

		// Methods taken from SolarPilot Ambient class
		double S0 = 1.353 * (1. + .0335 * cos(2. * PI * (doy + 10.) / 365.));
		double zenith = weather.m_solzen * 3.14159 / 180.;
		double azimuth = weather.m_solazi * 3.14159 / 180.;
		double szen = sin(zenith);
		double czen = cos(zenith);
		double save2 = 90. - atan2(szen, czen) * R2D;
		double save = 1.0 / czen;
		if (save2 <= 30.)
			save = save - 41.972213 * pow(save2, (-2.0936381 - 0.04117341 * save2 + 0.000849854 * pow(save2, 2)));

		double alt = weather.m_elev / 1000.;
		double csky = 0.0;
		if (m_clearsky_model == 1)  // Meinel
			csky = (1. - .14 * alt) * exp(-.357 / pow(czen, .678)) + .14 * alt;
		else if (m_clearsky_model == 2)  // Hottel
			csky = 0.4237 - 0.00821 * pow(6. - alt, 2) + (0.5055 + 0.00595 * pow(6.5 - alt, 2)) * exp(-(0.2711 + 0.01858 * pow(2.5 - alt, 2)) / (czen + .00001));
		else if (m_clearsky_model == 3)  // Allen
			csky = 1.0 - 0.263 * ((del_h2o + 2.72) / (del_h2o + 5.0)) * pow((save * dpres), (0.367 * ((del_h2o + 11.53) / (del_h2o + 7.88))));
		else if (m_clearsky_model == 4)  // Moon
			csky = 0.183 * exp(-save * dpres / 0.48) + 0.715 * exp(-save * dpres / 4.15) + .102;

		clearsky = std::fmax(0.0, csky * S0 * 1000.);
	}

	return clearsky;
}

double C_pt_receiver::estimate_thermal_efficiency(const C_csp_weatherreader::S_outputs& weather, double q_inc)
{
    /*
    A very approximate thermal efficiency used for quick optimization performance projections
    */

    double T_eff = (m_T_htf_cold_des + m_T_htf_hot_des) * .55;  //[K]

    double T_amb = weather.m_tdry + 273.15;
    double T_eff4 = T_eff * T_eff;
    T_eff4 *= T_eff4;
    double T_amb4 = T_amb * T_amb;
    T_amb4 *= T_amb4;

    double Arec = area_proj();

    double q_rad = 5.67e-8 * m_epsilon * Arec * (T_eff4 - T_amb4) * 1.e-6;   //MWt

    double v = weather.m_wspd;
    double v2 = v * v;
    double v3 = v2 * v;

    double q_conv = q_rad / 2. * (-0.001129 * v3 + 0.031229 * v2 - 0.01822 * v + 0.962476);  //convection is about half radiation, scale by wind speed. surrogate regression from molten salt run.

    return std::max(1. - (q_rad + q_conv) / q_inc, 0.);

}

double C_pt_receiver::get_min_power_delivery() //[MWt]
{
    return m_f_rec_min * m_q_rec_des * 1.E-6;   //[MWt]
}

double C_pt_receiver::get_max_power_delivery() //[MWt]
{
    return m_m_dot_htf_max_frac * m_q_rec_des * 1.E-6;  //[MWt]
}

double C_pt_receiver::get_T_htf_cold_des()    //[K]
{
    return m_T_htf_cold_des;    //[K]
}

double C_pt_receiver::get_q_dot_rec_des()     //[MWt]
{
    return m_q_rec_des * 1.E-6;     //[MWt]
}