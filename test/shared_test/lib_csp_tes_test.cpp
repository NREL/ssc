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

#include <gtest/gtest.h>

#include "lib_csp_tes_test.h"
#include "vs_google_test_explorer_namespace.h"

using namespace csp_common;

//========Tests===================================================================================
//=== Using factory patterns to create the different physical and non-physical components=========

// Test draining storage tank
NAMESPACE_TEST(csp_common, StorageTank, DrainingTank)
{
    bool is_hot_tank = false;
    double dt = 3600.;

    DefaultTankFactory default_tank_factory = DefaultTankFactory();
    std::unique_ptr<TankSpecifications> tank_specifications = default_tank_factory.MakeSpecifications();
    std::unique_ptr<Tank> tank = default_tank_factory.MakeTank(tank_specifications.get());
    TankState tank_state = default_tank_factory.MakeTankState();
    TankExternalConditions external_conditions = default_tank_factory.MakeExternalConditions();

    double T_ave, vol_ave, q_loss, T_fin, vol_fin, m_fin, q_heater;     // outputs

    tank->mixed_tank(
        is_hot_tank, dt,
        tank_state.m_prev, tank_state.T_prev,
        external_conditions.m_dot_in, external_conditions.m_dot_out,
        external_conditions.T_in, external_conditions.T_amb,
        T_ave, vol_ave, q_loss, T_fin, vol_fin, m_fin, q_heater);

    EXPECT_NEAR(T_ave, 563.7, 563.7 * kErrorToleranceLo);
    EXPECT_NEAR(vol_ave, 892.30, 892.30 * kErrorToleranceLo);
    EXPECT_NEAR(q_loss, 0.331, 0.331 * kErrorToleranceLo);
    EXPECT_NEAR(T_fin, 558.9, 558.9 * kErrorToleranceLo);
    EXPECT_NEAR(vol_fin, 0., 0. * kErrorToleranceLo);
    EXPECT_NEAR(m_fin, 0., 0. * kErrorToleranceLo);
    EXPECT_NEAR(q_heater, 0., 0. * kErrorToleranceLo);
}

// Test an initially drained storage tank
NAMESPACE_TEST(csp_common, StorageTank, InitiallyDrainedTank)
{
    bool is_hot_tank = false;
    double dt = 3600.;

    DefaultTankFactory default_tank_factory = DefaultTankFactory();
    std::unique_ptr<TankSpecifications> tank_specifications = default_tank_factory.MakeSpecifications();
    std::unique_ptr<Tank> tank = default_tank_factory.MakeTank(tank_specifications.get());
    TankState tank_state = default_tank_factory.MakeTankState();
    TankExternalConditions external_conditions = default_tank_factory.MakeExternalConditions();

    tank_state.m_prev = 0.;

    double T_ave, vol_ave, q_loss, T_fin, vol_fin, m_fin, q_heater;     // outputs

    tank->mixed_tank(
        is_hot_tank, dt,
        tank_state.m_prev, tank_state.T_prev,
        external_conditions.m_dot_in, external_conditions.m_dot_out,
        external_conditions.T_in, external_conditions.T_amb,
        T_ave, vol_ave, q_loss, T_fin, vol_fin, m_fin, q_heater);

    EXPECT_NEAR(T_ave, 563.97, 563.97 * kErrorToleranceLo);
    EXPECT_NEAR(vol_ave, 0., 0. * kErrorToleranceLo);
    EXPECT_NEAR(q_loss, 0., 0. * kErrorToleranceLo);
    EXPECT_NEAR(T_fin, 563.97, 563.97 * kErrorToleranceLo);
    EXPECT_NEAR(vol_fin, 0., 0. * kErrorToleranceLo);
    EXPECT_NEAR(m_fin, 0., 0. * kErrorToleranceLo);
    EXPECT_NEAR(q_heater, 0., 0. * kErrorToleranceLo);
}

//========/Tests==================================================================================

//========TankFactory (super class)===============================================================
//========/TankFactory (super class)==============================================================

//========DefaultTankFactory (subclass)===========================================================
std::unique_ptr<Tank> DefaultTankFactory::MakeTank(TankSpecifications* tank_specifications) const
{
    auto tank = std::unique_ptr<Tank>(new Tank);

    tank->define_storage(
        tank_specifications->fluid_field,
        tank_specifications->fluid_store,
        tank_specifications->is_direct,
        tank_specifications->config,
        tank_specifications->duty_des,
        tank_specifications->vol_des,
        tank_specifications->h_des,
        tank_specifications->u_des,
        tank_specifications->tank_pairs_des,
        tank_specifications->hot_htr_set_point_des,
        tank_specifications->cold_htr_set_point_des,
        tank_specifications->max_q_htr_cold,
        tank_specifications->max_q_htr_hot,
        tank_specifications->dt_hot_des,
        tank_specifications->dt_cold_des,
        tank_specifications->T_h_in_des,
        tank_specifications->T_h_out_des);

    return tank;
}

std::unique_ptr<TankSpecifications> DefaultTankFactory::MakeSpecifications() const
{
    auto tank_specifications = std::unique_ptr<TankSpecifications>(new TankSpecifications());
    tank_specifications->field_fluid = 18;
    tank_specifications->store_fluid = 18;
    tank_specifications->fluid_field.SetFluid(tank_specifications->field_fluid);
    tank_specifications->fluid_store.SetFluid(tank_specifications->store_fluid);
    tank_specifications->is_direct = true;
    tank_specifications->config = 2;
    tank_specifications->duty_des = 623595520.;
    tank_specifications->vol_des = 17558.4;
    tank_specifications->h_des = 12.;
    tank_specifications->u_des = 0.4;
    tank_specifications->tank_pairs_des = 1.;
    tank_specifications->hot_htr_set_point_des = 638.15;
    tank_specifications->cold_htr_set_point_des = 523.15;
    tank_specifications->max_q_htr_cold = 25.;
    tank_specifications->max_q_htr_hot = 25.;
    tank_specifications->dt_hot_des = 5.;
    tank_specifications->dt_cold_des = 5.;
    tank_specifications->T_h_in_des = 703.15;
    tank_specifications->T_h_out_des = 566.15;

    return tank_specifications;
}

TankState DefaultTankFactory::MakeTankState() const
{
    TankState tank_state;
    tank_state.m_prev = 3399727.;
    tank_state.T_prev = 563.97;
    return tank_state;
}

TankExternalConditions DefaultTankFactory::MakeExternalConditions() const
{
    TankExternalConditions external_conditions;
    external_conditions.m_dot_in = 0.;
    external_conditions.m_dot_out = 1239.16;      // this will more than drain the tank
    external_conditions.T_in = 566.15;
    external_conditions.T_amb = 296.15;
    return external_conditions;
}
//========/DefaultTankFactory (subclass)==========================================================

//======== csp_solver TES ========================================================================
double C_to_K(double T) {
    return T + 273.15;
}

NAMESPACE_TEST(csp_common, TesCspSolver, Default)
{
    std::vector<double> tes_lengths{ 0, 90, 100, 120, 0, 0, 0, 0, 80, 120, 80 };

    C_csp_two_tank_tes tes(
        21,                                           //[-]
        util::matrix_t<double>(1, 1, 0.),             //[-]
        18,                                           //[-]
        util::matrix_t<double>(1, 1, 1.),             //[-]
        311.8,                                        //[MWt]
        2,                                            //[-]
        311.8 * 6,                                    //[MWht]
        12,                                           //[m]
        0.4,                                          //[W/m^2-K]
        1,                                            //[-]
        365,                                          //[C]
        25,                                           //[MWe]
        250,                                          //[C]
        25,                                           //[MWe]
        5,                                            //[C]
        293,                                          //[C]
        391,                                          //[C]
        391,                                          //[C]
        293,                                          //[C]
        1.,                                           //[m]
        30.,                                          //[-]
        0.55,                                         //[kWe/kg/s]
        true,                                         //[-]
        1.85,                                         //[m/s]
        true,                                         //[-]
        0.15,                                         //[kWe/kg/s]
        0.85,                                         //[-]
        false,                                        //[-]
        400,                                          //[C]
        false,                                        //[-]
        false,                                        //[-]
        util::matrix_t<double>(1, 11, 0.),            //[-]
        util::matrix_t<double>(1, 1, -1),             //[m]
        util::matrix_t<double>(1, 1, -1),             //[m]
        util::matrix_t<double>(1, 11, &tes_lengths),
        4.57e-5,                                      //[m]
        0.                                            //[bar]
    );


    // Initialization   -> this is necessary to fully instantiate the TES
    C_csp_tes::S_csp_tes_init_inputs init_inputs;
    init_inputs.T_to_cr_at_des = C_to_K(293.);
    init_inputs.T_from_cr_at_des = C_to_K(391.);
    init_inputs.P_to_cr_at_des = 19.64;
    tes.init(init_inputs);

    EXPECT_TRUE(tes.does_tes_exist());
    EXPECT_FALSE(tes.is_cr_to_cold_allowed());
    EXPECT_NEAR(tes.get_hot_temp(), C_to_K(391.), 0.1);
    EXPECT_NEAR(tes.get_cold_temp(), C_to_K(293.), 0.1);
    EXPECT_NEAR(tes.get_hot_tank_vol_frac(), 0.305, 0.001);
    EXPECT_NEAR(tes.get_initial_charge_energy(), 561.2, 0.1);

    // Discharge estimate
    double T_cold_K = C_to_K(293.);
    double t_step = 3600.;
    double q_dot_dc_est, m_dot_field_est_dchg, T_hot_field_est;
    tes.discharge_avail_est(T_cold_K /*K*/, t_step /*s*/, q_dot_dc_est /*MWt*/, m_dot_field_est_dchg /*kg/s*/, T_hot_field_est /*K*/);
    EXPECT_NEAR(q_dot_dc_est, 594.7, 0.1);          // FIX: why is this different and higher than the initial charge energy of 561.2?
    EXPECT_NEAR(m_dot_field_est_dchg, 2598., 1.);
    EXPECT_NEAR(T_hot_field_est, C_to_K(386.3), 0.1);

    // Charge estimate
    double T_htf_hot_in = C_to_K(391.);
    double q_dot_ch_est, m_dot_field_est_chg, T_cold_field_est;
    tes.charge_avail_est(T_htf_hot_in /*K*/, t_step /*s*/, q_dot_ch_est /*MWt*/, m_dot_field_est_chg /*kg/s*/, T_cold_field_est /*K*/);
    EXPECT_NEAR(q_dot_ch_est, 1375., 1.);
    EXPECT_NEAR(m_dot_field_est_chg, 6062., 1.);
    EXPECT_NEAR(T_cold_field_est, 571.7, 0.1);

    // Discharge
    double T_amb = C_to_K(23.);
    double T_htf_cold_in = C_to_K(293.);
    double T_htf_hot_out, q_dot_heater, m_dot, W_dot_rhtf_pump, q_dot_loss, q_dot_dc_to_htf,
        q_dot_ch_from_htf, T_hot_ave, T_cold_ave, T_hot_final, T_cold_final;
    tes.discharge(t_step /*s*/, T_amb /*K*/, m_dot_field_est_dchg /*kg/s*/, T_htf_cold_in /*K*/,
        T_htf_hot_out /*K*/, q_dot_heater /*MWe*/, m_dot /*kg/s*/, W_dot_rhtf_pump /*MWe*/,
        q_dot_loss /*MWt*/, q_dot_dc_to_htf /*MWt*/, q_dot_ch_from_htf /*MWt*/,
        T_hot_ave /*K*/, T_cold_ave /*K*/, T_hot_final /*K*/, T_cold_final /*K*/);
    EXPECT_NEAR(T_htf_hot_out, 659.3, 0.1);
    EXPECT_NEAR(q_dot_heater, 0., 0.1);
    EXPECT_NEAR(m_dot, 4247., 1.);
    EXPECT_NEAR(W_dot_rhtf_pump, 0.6370, 0.001);
    EXPECT_NEAR(q_dot_loss, 1.172, 0.001);
    EXPECT_NEAR(q_dot_dc_to_htf, 592.5, 0.1);
    EXPECT_NEAR(q_dot_ch_from_htf, 0., 0.1);
    EXPECT_NEAR(T_hot_ave, 664.1, 0.1);
    EXPECT_NEAR(T_cold_ave, 566.9, 0.1);
    EXPECT_NEAR(T_hot_final, 664.0, 0.1);
    EXPECT_NEAR(T_cold_final, 567.4, 0.1);

    // Reset to initial state
    tes.reset_storage_to_initial_state();
    EXPECT_NEAR(tes.get_hot_temp(), C_to_K(391.), 0.1);
    EXPECT_NEAR(tes.get_cold_temp(), C_to_K(293.), 0.1);
    EXPECT_NEAR(tes.get_hot_tank_vol_frac(), 0.273, 0.001);         // FIX: why is this different than the initial hot tank vol frac?
    EXPECT_NEAR(tes.get_initial_charge_energy(), 561.2, 0.1);

    // Charge
    double T_htf_cold_out;
    // FIX: Anything greater than 0.89 * m_dot_field_est_chg will give invalid results
    tes.charge(t_step /*s*/, T_amb /*K*/, 0.89 * m_dot_field_est_chg /*kg/s*/, T_htf_hot_in /*K*/,
        T_htf_cold_out /*K*/, q_dot_heater /*MWe*/, m_dot /*kg/s*/, W_dot_rhtf_pump /*MWe*/,
        q_dot_loss /*MWt*/, q_dot_dc_to_htf /*MWt*/, q_dot_ch_from_htf /*MWt*/,
        T_hot_ave /*K*/, T_cold_ave /*K*/, T_hot_final /*K*/, T_cold_final /*K*/);
    EXPECT_NEAR(T_htf_cold_out, 571.6, 0.1);
    EXPECT_NEAR(q_dot_heater, 0., 0.1);
    EXPECT_NEAR(m_dot, 8819., 1.);
    EXPECT_NEAR(W_dot_rhtf_pump, 1.323, 0.001);
    EXPECT_NEAR(q_dot_loss, 1.170, 0.01);
    EXPECT_NEAR(q_dot_dc_to_htf, 0., 0.1);
    EXPECT_NEAR(q_dot_ch_from_htf, 1230, 0.1);                     // TODO: lower than q_dot_ch_est, but expected due to 0.89 factor
    EXPECT_NEAR(T_hot_ave, 661.8, 0.1);
    EXPECT_NEAR(T_cold_ave, 566.1, 0.1);
    EXPECT_NEAR(T_hot_final, 660.6, 0.1);
    EXPECT_NEAR(T_cold_final, 566.1, 0.1);
}
