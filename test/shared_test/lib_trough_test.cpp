#include <gtest/gtest.h>

#define private public              // for setting private data members
#include "lib_trough_test.h"

TEST_F(TroughTest, DefocusTest)
{
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

    htfInletState.m_temp = 297.95980959101303; //290.997496581585;
    defocus = 0.77316;

    troughInfo.ms_ts.m_time_start = 5313600.;
    troughInfo.ms_ts.m_time = 5317200.;
    troughInfo.ms_ts.m_step = 3600.;
    troughInfo.m_tou = 1.;

    // previous state values
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


    // Change defocus, increase by 0.0013%, change inlet temperature, decrease by even less
    htfInletState.m_temp = 297.95980001082023;
    defocus = 0.77317;
    troughModel->on(weatherValues, htfInletState, defocus, troughOutputs, troughInfo);

    EXPECT_NEAR(troughOutputs.m_T_salt_hot, 390.39, 390.39 * m_error_tolerance_lo);
    EXPECT_NEAR(troughOutputs.m_m_dot_salt_tot, 2494962., 2494962. * m_error_tolerance_lo);
    // mass flow changes by 2.1%
}
