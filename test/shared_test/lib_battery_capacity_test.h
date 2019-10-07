#ifndef SAM_SIMULATION_CORE_LIB_BATTERY_CAPACITY_TEST_H
#define SAM_SIMULATION_CORE_LIB_BATTERY_CAPACITY_TEST_H

#include <gtest/gtest.h>

//#include "lib_battery_capacity.h"
#include "lib_battery.h"

struct capacity_state {
    enum {
        CHARGE, NO_CHARGE, DISCHARGE
    };

    double q0;  // [Ah] - Total capacity at timestep
    double qmax; // [Ah] - maximum possible capacity
    double qmax_thermal; // [Ah] - maximum capacity adjusted for temperature affects
    double I;   // [A]  - Current draw
    double I_loss; // [A] - Lifetime and thermal losses
    double SOC; // [%] - State of Charge
    double DOD; // [%] - Depth of Discharge
    double DOD_prev; // [%] - Depth of Discharge of previous step
    int charge_mode; // {CHARGE, NO_CHARGE, DISCHARGE}
    int prev_charge_mode; // {CHARGE, NO_CHARGE, DISCHARGE}
};

static void compareState(capacity_t* old_cap, const capacity_state& state, const std::string& msg){
    double tol = 0.01;
    EXPECT_NEAR(old_cap->q0(), state.q0, tol) << msg;
    EXPECT_NEAR(old_cap->qmax_thermal(), state.qmax_thermal, tol) << msg;
    EXPECT_NEAR(old_cap->qmax(), state.qmax, tol) << msg;
    EXPECT_NEAR(old_cap->I(), state.I, tol) << msg;
    EXPECT_NEAR(old_cap->I_loss(), state.I_loss, tol) << msg;
    EXPECT_NEAR(old_cap->SOC(), state.SOC, tol) << msg;
    EXPECT_NEAR(old_cap->DOD(), state.DOD, tol) << msg;
//        EXPECT_NEAR(old_cap->DOD(), state.DOD_prev, tol) << msg;
    EXPECT_NEAR(old_cap->charge_operation(), state.charge_mode, tol) << msg;
}

static void compareState(std::unique_ptr<capacity_t>& old_cap, const capacity_state& state, const std::string& msg) {
    compareState(old_cap.get(), state, msg);
}

class lib_battery_capacity_test : public ::testing::Test
{
protected:
//    std::unique_ptr<battery_capacity_interface> new_cap;
    std::unique_ptr<capacity_t> old_cap;
//    std::shared_ptr<storage_time_params> time;

//    battery_capacity_params params;
    double tol = 0.1;
    double error;

    double q = 1000;
    double SOC_init = 50;
    double SOC_min = 15;
    double SOC_max = 95;

    double dt_hour = 1;
    int nyears = 1;

public:

//    void compareStates(capacity_state &s, std::string msg) {
//        auto state = new_cap->get_state();
//        EXPECT_NEAR(s.q0, state.q0, tol) << msg;
//        EXPECT_NEAR(s.qmax_thermal, state.qmax_thermal, tol) << msg;
//        EXPECT_NEAR(s.qmax, state.qmax, tol) << msg;
//        EXPECT_NEAR(s.I, state.I, tol) << msg;
//        EXPECT_NEAR(s.I_loss, state.I_loss, tol) << msg;
//        EXPECT_NEAR(s.SOC, state.SOC, tol) << msg;
//        EXPECT_NEAR(s.DOD, state.DOD, tol) << msg;
//        EXPECT_NEAR(s.charge_mode, state.charge_mode, tol) << msg;
//    }
};

/**
 * Tests the Lithium Ion capacity model for Li-Ion, Vanadium Redox and Iron flow
 */


class LiIon_lib_battery_capacity_test : public lib_battery_capacity_test
{
public:

    void SetUp()override {
        old_cap = std::unique_ptr<capacity_lithium_ion_t>(new capacity_lithium_ion_t(q, SOC_init, SOC_max, SOC_min));
    }
};

/**
 * Tests the Kinetic battery model for Lead acid
 */


class KiBam_lib_battery_capacity_test : public lib_battery_capacity_test
{
protected:
    double q10 = 93.;
    double t1 = 1;
    double q1 = 60.;
    double q20 = 100;
public:

    void SetUp() override {
        old_cap = std::unique_ptr<capacity_kibam_t>(new capacity_kibam_t(q20, t1, q1, q10,SOC_init,
                SOC_max, SOC_min, dt_hour));
    }
};
#endif //SAM_SIMULATION_CORE_LIB_BATTERY_CAPACITY_TEST_H
