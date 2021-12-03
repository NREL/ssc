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
