#ifndef SYSTEM_ADVISOR_MODEL_LIB_RESILIENCE_TEST_H
#define SYSTEM_ADVISOR_MODEL_LIB_RESILIENCE_TEST_H

#include <gtest/gtest.h>
#include <lib_battery_dispatch.h>
#include <lib_power_electronics.h>
#include <lib_shared_inverter.h>

#include "cmod_battery.h"
#include "cmod_battwatts.h"
#include "lib_resilience.h"
#include "lib_battery.h"

class ResilienceTest_lib_resilience : public ::testing::Test {
protected:
    int chem;
    int pos;
    int dispatch_mode;
    double size_kw;
    double size_kwh;
    double inv_eff;
    std::vector<double> ac;
    std::vector<double> load;
    std::vector<double> dispatch_custom;


    batt_variables* batt_vars = nullptr;
    var_table* vartab = nullptr;
    battstor* batt = nullptr;
    dispatch_t* dispatch = nullptr;
    SharedInverter* inverter = nullptr;

    class fakeInverter : public SharedInverter{
    public:
        fakeInverter():SharedInverter(-1, 1, nullptr, nullptr, nullptr){
            efficiencyAC = 96;
        }
        void calculateACPower(const double, const double, double) {}
    };

    void SetUp() override{
        CreateBattery(false);
    }

    void CreateBattery(bool ac_not_dc_connected) {
        delete batt_vars;
        delete vartab;
        delete batt;
        ac.clear();
        load.clear();
        dispatch_custom.clear();
        chem = battery_t::LITHIUM_ION;
        if (ac_not_dc_connected)
            pos = ChargeController::AC_CONNECTED;
        else{
            pos = ChargeController::DC_CONNECTED;
            inverter = new fakeInverter;
        }
        dispatch_mode = 2;
        size_kw = 4.0;
        size_kwh = 16.0;
        inv_eff = 96.0;
        for (size_t i = 0; i < 8760; i++){
            ac.push_back(0);
            load.push_back(1.);
            dispatch_custom.push_back(1.);
        }
        size_t n_recs = 8760;
        batt_vars = battwatts_create(n_recs, chem, pos, size_kwh, size_kw, inv_eff, dispatch_mode, dispatch_custom);

        vartab = new var_table;
        batt = new battstor(*vartab, true, n_recs, 1., batt_vars);
        batt->initialize_automated_dispatch(ac, load);
        batt->setSharedInverter(inverter);
        dispatch = batt->dispatch_model;
    }

    void TearDown() override {
        delete vartab;
        delete batt_vars;
        delete batt;
    }
};


#endif //SYSTEM_ADVISOR_MODEL_LIB_RESILIENCE_TEST_H
