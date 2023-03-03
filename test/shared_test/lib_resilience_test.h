/*
BSD 3-Clause License

Copyright (c) Alliance for Sustainable Energy, LLC. See also https://github.com/NREL/ssc/blob/develop/LICENSE
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


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


    std::shared_ptr<batt_variables> batt_vars = nullptr;
    var_table* vartab = nullptr;
    std::shared_ptr<battstor> batt = nullptr;
    dispatch_t* dispatch = nullptr;
    SharedInverter* inverter = nullptr;

    class fakeInverter : public SharedInverter{
    public:
        fakeInverter():SharedInverter(NONE, 1, nullptr, nullptr, nullptr){
            efficiencyAC = 96;
            powerAC_kW = 0.;
        }
    };

    void CreateBattery(bool ac_not_dc_connected, size_t steps_per_hour, double pv_ac, double load_ac, double batt_dc) {
        delete vartab;
        ac.clear();
        load.clear();
        dispatch_custom.clear();
        chem = battery_params::LITHIUM_ION;
        pos = dispatch_t::BEHIND;
        dispatch_mode = 2;
        size_kw = 4.0;
        size_kwh = 16.0;
        inv_eff = 96.0;
        double dt_hr = 1. / (double)steps_per_hour;
        for (size_t i = 0; i < 8760 * steps_per_hour; i++){
            ac.push_back(pv_ac);
            load.push_back(load_ac);
            dispatch_custom.push_back(batt_dc);
        }
        size_t n_recs = 8760 * steps_per_hour;
        batt_vars = battwatts_create(n_recs, 1, chem, pos, size_kwh, size_kw, inv_eff, dispatch_mode, dispatch_custom);
        if (ac_not_dc_connected)
            batt_vars->batt_topology = ChargeController::AC_CONNECTED;
        else{
            batt_vars->batt_topology = ChargeController::DC_CONNECTED;
            inverter = new fakeInverter;
        }

        std::vector<bool> outage_vars(n_recs, true);
        batt_vars->grid_outage_steps = std::move(outage_vars);

        vartab = new var_table;
        batt = std::make_shared<battstor>(*vartab, true, n_recs, dt_hr, batt_vars);
        batt->initialize_automated_dispatch(ac, load);
        batt->setSharedInverter(inverter);
        dispatch = batt->dispatch_model;
    }

    void TearDown() override {
        delete vartab;
        if (inverter)
            delete inverter;
    }
};


#endif //SYSTEM_ADVISOR_MODEL_LIB_RESILIENCE_TEST_H
