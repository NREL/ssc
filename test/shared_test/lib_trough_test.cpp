#include <gtest/gtest.h>

#define private public              // for setting private data members
#include "lib_trough_test.h"

TEST_F(TroughTest, DefocusTest)
{
    // at time 1476 (1477 end time listed in output)
    weatherValues.m_year = 2009;
    weatherValues.m_month = 3;
    weatherValues.m_day = 3;
    weatherValues.m_hour = 12;
    weatherValues.m_minute = 0;
    weatherValues.m_beam = 488;
    weatherValues.m_tdry = 27;
    weatherValues.m_tdew = -4;
    weatherValues.m_wspd = 3;
    weatherValues.m_pres = 920;
    weatherValues.m_solazi = 166.04812459961;
    weatherValues.m_solzen = 39.5823887774745;

    htfInletState.m_temp = 297.95980959101303; //297.95980959101303;
    defocus = 0.77316;

    troughInfo.ms_ts.m_time_start = 5313600.;
    troughInfo.ms_ts.m_time = 5317200.;
    troughInfo.ms_ts.m_step = 3600.;
    troughInfo.m_tou = 1.;

    // previous HTF temperatures [K]
    troughModel->m_T_sys_c_t_end_converged = 571.979992782819;      // this sets m_T_sys_c_t_end_last
    troughModel->m_T_sys_h_t_end_converged = 663.79113603342;       // this sets m_T_sys_h_t_end_last
    // SCA temperatures - these set the values of m_T_htf_out_t_end_last[i]
    troughModel->m_T_htf_out_t_end_converged[0] = 585.2012765;
    troughModel->m_T_htf_out_t_end_converged[1] = 597.5706153;
    troughModel->m_T_htf_out_t_end_converged[2] = 609.5327793;
    troughModel->m_T_htf_out_t_end_converged[3] = 621.1103321;
    troughModel->m_T_htf_out_t_end_converged[4] = 632.2659289;
    troughModel->m_T_htf_out_t_end_converged[5] = 643.1282423;
    troughModel->m_T_htf_out_t_end_converged[6] = 653.6533931;
    troughModel->m_T_htf_out_t_end_converged[7] = 663.8068978;


    troughModel->on(weatherValues, htfInletState, defocus, troughOutputs, troughInfo);

    EXPECT_NEAR(troughOutputs.m_T_salt_hot, 390.98, 390.98 * m_error_tolerance_lo);
    EXPECT_NEAR(troughOutputs.m_m_dot_salt_tot, 2443500., 2443500. * m_error_tolerance_lo);


    // Change defocus, increase by 0.0013%
    defocus = 0.77317;
    troughModel->on(weatherValues, htfInletState, defocus, troughOutputs, troughInfo);

    EXPECT_NEAR(troughOutputs.m_T_salt_hot, 390.39, 390.39 * m_error_tolerance_lo);
    EXPECT_NEAR(troughOutputs.m_m_dot_salt_tot, 2494962., 2494962. * m_error_tolerance_lo);
    // mass flow increases by 2.1%
}

TEST_F(TroughTest, DefocusTest2)
{
    // at time 1068 (1069 end time listed in output)
    weatherValues.m_year = 2009;
    weatherValues.m_month = 2;
    weatherValues.m_day = 14;
    weatherValues.m_hour = 12;
    weatherValues.m_minute = 0;
    weatherValues.m_beam = 1016.0;
    weatherValues.m_tdry = 16.0;
    weatherValues.m_tdew = -14.0;
    weatherValues.m_wspd = 1.2;
    weatherValues.m_pres = 920.0;
    weatherValues.m_solazi = 167.05961724080794;
    weatherValues.m_solzen = 45.793932659900818;

    htfInletState.m_temp = 296.47387218057872; // 289.38622217774684;
    defocus = 0.31113892655015696;

    troughInfo.ms_ts.m_time_start = 3844800.;
    troughInfo.ms_ts.m_time = 3848400.;
    troughInfo.ms_ts.m_step = 3600.;
    troughInfo.m_tou = 1.;

    // previous HTF temperatures [K]
    troughModel->m_T_sys_c_t_end_converged = 574.56624498543692;      // this sets m_T_sys_c_t_end_last
    troughModel->m_T_sys_h_t_end_converged = 664.49041436768960;       // this sets m_T_sys_h_t_end_last
    // SCA temperatures - these set the values of m_T_htf_out_t_end_last[i]
    troughModel->m_T_htf_out_t_end_converged[0] = 586.54320898265996;
    troughModel->m_T_htf_out_t_end_converged[1] = 598.40229621263472;
    troughModel->m_T_htf_out_t_end_converged[2] = 610.02021991454296;
    troughModel->m_T_htf_out_t_end_converged[3] = 621.40305775456318;
    troughModel->m_T_htf_out_t_end_converged[4] = 632.53459357521194;
    troughModel->m_T_htf_out_t_end_converged[5] = 643.46237590525539;
    troughModel->m_T_htf_out_t_end_converged[6] = 654.17081551318063;
    troughModel->m_T_htf_out_t_end_converged[7] = 664.58682197556709;


    troughModel->on(weatherValues, htfInletState, defocus, troughOutputs, troughInfo);

    EXPECT_NEAR(troughOutputs.m_T_salt_hot, 390.96, 390.96 * m_error_tolerance_lo);
    EXPECT_NEAR(troughOutputs.m_m_dot_salt_tot, 1713283.5, 1713283.5 * m_error_tolerance_lo);
}