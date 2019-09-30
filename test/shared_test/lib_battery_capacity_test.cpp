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

class LiIon_lib_battery_capacity_test : public ::testing::Test
{
protected:
//    std::unique_ptr<battery_capacity_interface> new_cap;
    std::unique_ptr<capacity_t> old_cap;
//    std::shared_ptr<storage_time_params> time;

//    battery_capacity_params params;
    double tol = 0.01;
    double error;

    double q = 1000;
    double SOC_init = 50;
    double SOC_min = 15;
    double SOC_max = 95;

    double dt_hour = 1;
    int nyears = 1;

public:

    void SetUp(){
//        time = std::make_shared<storage_time_params>(dt_hour, nyears);
//        params = battery_capacity_params({time, q, SOC_init,SOC_min, SOC_max});
//        new_cap = std::unique_ptr<capacity_lithium_ion>(new capacity_lithium_ion(params));
        old_cap = std::unique_ptr<capacity_lithium_ion_t>(new capacity_lithium_ion_t(q, SOC_init, SOC_max, SOC_min));
    }

    void compareState(capacity_state state, const std::string& msg){
        EXPECT_NEAR(old_cap->q0(), state.q0, tol) << msg;
        EXPECT_NEAR(old_cap->qmax_thermal(), state.qmax_thermal, tol) << msg;
        EXPECT_NEAR(old_cap->qmax(), state.qmax, tol) << msg;
        EXPECT_NEAR(old_cap->I(), state.I, tol) << msg;
        EXPECT_NEAR(old_cap->I_loss(), state.I_loss, tol) << msg;
        EXPECT_NEAR(old_cap->SOC(), state.SOC, tol) << msg;
        EXPECT_NEAR(old_cap->DOD(), state.DOD, tol) << msg;
        EXPECT_NEAR(old_cap->charge_operation(), state.charge_mode, tol) << msg;
    }

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

TEST_F(LiIon_lib_battery_capacity_test, SetUpTest) {
    EXPECT_EQ(old_cap->q1(), 500);
    EXPECT_EQ(old_cap->q10(), 1000);
}

TEST_F(LiIon_lib_battery_capacity_test, updateCapacityTest){
    double I = 1.5;
    old_cap->updateCapacity(I, dt_hour);
    auto s1 = capacity_state({498.5, 1000, 1000, 1.5, 0,
                             49.85, 50.15, 0, 2});
    compareState(s1, "updateCapacityTest: 1");

    I = 3;
    old_cap->updateCapacity(I, dt_hour);

    s1 = {495.5, 1000, 1000, 3, 0,
          49.55, 50.45, 50.15, 2};
    compareState(s1, "updateCapacityTest: 2");

    I = 490;
    old_cap->updateCapacity(I, dt_hour);
    s1 = {150, 1000, 1000, 345.5, 0,
          15, 85, 50.45, 2};
    compareState(s1, "updateCapacityTest: 3");

    I = 490;
    old_cap->updateCapacity(I, dt_hour);
    s1 = {150, 1000, 1000, 0, 0,
          15, 85, 85, 1};
    compareState(s1, "updateCapacityTest: 4");
}

TEST_F(LiIon_lib_battery_capacity_test, updateCapacityThermalTest){
    double percent = 80;
    old_cap->updateCapacityForThermal(percent);
    auto s1 = capacity_state({500, 1000, 800, 0, 0,
                              62.5, 37.5, 0, 2});
    compareState(s1, "updateCapacityThermalTest: 1");

    percent = 50;
    old_cap->updateCapacityForThermal(percent);
    s1 = {500, 1000, 500, 0, 0, 100, 0, 0, 2};
    compareState(s1, "updateCapacityThermalTest: 2");

    percent = 10;
    old_cap->updateCapacityForThermal(percent);
    s1 = {100, 1000, 100, 0, 400, 100, 0, 0, 2};
    compareState(s1, "updateCapacityThermalTest: 3");

    percent = 110;
    old_cap->updateCapacityForThermal(percent);
    s1 = {100, 1000, 1100, 0, 400, 10, 90, 0, 2};
    compareState(s1, "updateCapacityThermalTest: 4");

    percent = -110;
    old_cap->updateCapacityForThermal(percent);
    s1 = {0, 1000, 0, 0, 500, 0, 100, 0, 2};
    compareState(s1, "updateCapacityThermalTest: 4");

}

TEST_F(LiIon_lib_battery_capacity_test, updateCapacityLifetimeTest){
    double percent = 80;
    old_cap->updateCapacityForLifetime(percent);
    auto s1 = capacity_state({500, 800, 1000, 0, 0,62.5, 37.5, 0, 2});
    compareState(s1, "updateCapacityLifetimeTest: 1");

    percent = 50;
    old_cap->updateCapacityForLifetime(percent);
    s1 = {500, 500, 1000, 0, 0, 100, 0, 0, 2};
    compareState(s1, "updateCapacityLifetimeTest: 2");

    percent = 10;
    old_cap->updateCapacityForLifetime(percent);
    s1 = {100, 100, 1000, 0, 400, 100, 0, 0, 2};
    compareState(s1, "updateCapacityLifetimeTest: 3");

    percent = 110;
    old_cap->updateCapacityForLifetime(percent);
    s1 = {100, 100, 1000, 0, 400, 100, 0, 0, 2};
    compareState(s1, "updateCapacityLifetimeTest: 4");

    percent = -110;
    old_cap->updateCapacityForLifetime(percent);
    s1 = {0, 0, 1000, 0, 500, 0, 100, 0, 2};
    compareState(s1, "updateCapacityLifetimeTest: 5");
}