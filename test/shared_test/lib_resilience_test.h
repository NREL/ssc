#ifndef SYSTEM_ADVISOR_MODEL_LIB_RESILIENCE_TEST_H
#define SYSTEM_ADVISOR_MODEL_LIB_RESILIENCE_TEST_H

#include <gtest/gtest.h>
#include <lib_battery_dispatch.h>
#include <lib_power_electronics.h>

#include "cmod_battery.h"
#include "cmod_battwatts.h"
#include "lib_resilience.h"
#include "lib_battery.h"

#define n_recs 8760

class ResilienceTest_lib_resilience : public ::testing::Test {
protected:
    int chem;
    int pos;
    int dispatch_mode;
    double size_kw;
    double size_kwh;
    double inv_eff;
    double dispatch_custom[n_recs];
    std::vector<double> ac;
    std::vector<double> load;

    batt_variables* batt_vars;
    var_table* vartab;
    battstor* batt;
    dispatch_t* dispatch;

    void SetUp() override {
        chem = battery_t::LITHIUM_ION;
        pos = ChargeController::DC_CONNECTED;
        dispatch_mode = 2;
        size_kw = 4.0;
        size_kwh = 16.0;
        inv_eff = 96.0;
        for (size_t i = 0; i < n_recs; i++){
            ac.push_back(2.);
            load.push_back(1.);
            // charge to full SOC at 95%
            if (i < 9)
                dispatch_custom[i] = -1;
            // then do nothing
            else
                dispatch_custom[i] = 0;
        }
        batt_vars = battwatts_create(n_recs, chem, pos, size_kwh, size_kw, inv_eff, dispatch_mode, dispatch_custom);

        vartab = new var_table;
        batt = new battstor(*vartab, true, n_recs, 1., batt_vars);
        batt->initialize_automated_dispatch(ac, load);
        dispatch = batt->dispatch_model;
    }

    void TearDown() override {
        delete vartab;
        delete batt_vars;
        delete batt;
    }
};


#endif //SYSTEM_ADVISOR_MODEL_LIB_RESILIENCE_TEST_H
