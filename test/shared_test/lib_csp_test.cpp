#include <gtest/gtest.h>

#include "lib_csp_test.h"

void StorageTankTest::SetUp()
{
    m_storage = new Storage_HX();

    m_field_fluid = 18;
    m_store_fluid = 18;
    m_fluid_field;
    m_fluid_store;
    m_is_direct = true;
    m_config = 2;
    m_duty_des = 623595520.;
    m_vol_des = 17558.4;
    m_h_des = 12.;
    m_u_des = 0.4;
    m_tank_pairs_des = 1.;
    m_hot_htr_set_point_des = 638.15;
    m_cold_htr_set_point_des = 523.15;
    m_max_q_htr_cold = 25.;
    m_max_q_htr_hot = 25.;
    m_dt_hot_des = 5.;
    m_dt_cold_des = 5.;
    m_T_h_in_des = 703.15;
    m_T_h_out_des = 566.15;

    m_fluid_field.SetFluid(m_field_fluid);
    m_fluid_store.SetFluid(m_store_fluid);

    m_storage->define_storage(m_fluid_field, m_fluid_store, m_is_direct,
        m_config, m_duty_des, m_vol_des, m_h_des,
        m_u_des, m_tank_pairs_des, m_hot_htr_set_point_des, m_cold_htr_set_point_des,
        m_max_q_htr_cold, m_max_q_htr_hot, m_dt_hot_des, m_dt_cold_des, m_T_h_in_des, m_T_h_out_des);
}

TEST_F(StorageTankTest, TestDrainingTank)
{
    m_is_hot_tank = false;
    m_dt = 3600;
    m_m_prev = 3399727.;
    m_T_prev = 563.97;
    m_m_dot_in = 0.;
    m_m_dot_out = 1239.16;      // this will more than drain the tank
    m_T_in = 566.15;
    m_T_amb = 296.15;
    
    m_storage->mixed_tank(m_is_hot_tank, m_dt, m_m_prev, m_T_prev, m_m_dot_in, m_m_dot_out, m_T_in, m_T_amb,
        m_T_ave, m_vol_ave, m_q_loss, m_T_fin, m_vol_fin, m_m_fin, m_q_heater);

    EXPECT_NEAR(m_T_ave, 563.7, 563.7 * m_error_tolerance_lo);
    EXPECT_NEAR(m_vol_ave, 892.30, 892.30 * m_error_tolerance_lo);
    EXPECT_NEAR(m_q_loss, 0.331, 0.331 * m_error_tolerance_lo);
    EXPECT_NEAR(m_T_fin, 561.3, 561.3 * m_error_tolerance_lo);
    EXPECT_NEAR(m_vol_fin, 0., 0. * m_error_tolerance_lo);
    EXPECT_NEAR(m_m_fin, 0., 0. * m_error_tolerance_lo);
    EXPECT_NEAR(m_q_heater, 0., 0. * m_error_tolerance_lo);
}

TEST_F(StorageTankTest, TestDrainedTank)
{
    m_is_hot_tank = false;
    m_dt = 3600;
    m_m_prev = 0.;
    m_T_prev = 563.97;
    m_m_dot_in = 0.;
    m_m_dot_out = 1239.16;
    m_T_in = 566.15;
    m_T_amb = 296.15;

    m_storage->mixed_tank(m_is_hot_tank, m_dt, m_m_prev, m_T_prev, m_m_dot_in, m_m_dot_out, m_T_in, m_T_amb,
        m_T_ave, m_vol_ave, m_q_loss, m_T_fin, m_vol_fin, m_m_fin, m_q_heater);

    EXPECT_NEAR(m_T_ave, 566.15, 566.15 * m_error_tolerance_lo);
    EXPECT_NEAR(m_vol_ave, 0., 0. * m_error_tolerance_lo);
    EXPECT_NEAR(m_q_loss, 0., 0. * m_error_tolerance_lo);
    EXPECT_NEAR(m_T_fin, 566.15, 566.15 * m_error_tolerance_lo);
    EXPECT_NEAR(m_vol_fin, 0., 0. * m_error_tolerance_lo);
    EXPECT_NEAR(m_m_fin, 0., 0. * m_error_tolerance_lo);
    EXPECT_NEAR(m_q_heater, 0., 0. * m_error_tolerance_lo);
}