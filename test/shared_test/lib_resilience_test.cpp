#include "lib_resilience_test.h"
TEST_F(ResilienceTest_lib_resilience, DischargePower)
{
    auto cap = batt->battery_model->capacity_model();
    auto vol = batt->battery_model->voltage_model();
    cap->change_SOC_limits(0, 100);
    while (cap->SOC() > 10)
        batt->battery_model->run(0, 1);

    battery_t initial_batt = battery_t(*batt->battery_model);
    double max_power = vol->calculate_max_discharge_w(cap->q0(), cap->qmax(), nullptr);

    double desired_power = 0.;
    while (desired_power < max_power * 1.2){
        cap = batt->battery_model->capacity_model();
        vol = batt->battery_model->voltage_model();

        double current = vol->calculate_current_for_target_w(desired_power, cap->q0(), cap->qmax());

        batt->battery_model->run(1, current);

        if (desired_power < max_power)
            EXPECT_NEAR(cap->I() * vol->battery_voltage(), desired_power, 1e-2);
        else{
            EXPECT_LT(vol->cell_voltage(), 0.75 * batt->batt_vars->batt_Vnom + 0.01);
        }

        desired_power += max_power / 100.;
        delete batt->battery_model;
        batt->battery_model = new battery_t(initial_batt);
    }
}

TEST_F(ResilienceTest_lib_resilience, ChargePower)
{
    auto cap = batt->battery_model->capacity_model();
    auto vol = batt->battery_model->voltage_model();
    cap->change_SOC_limits(0, 100);
    while (cap->SOC() > 90)
        batt->battery_model->run(0, -1);

    battery_t initial_batt = battery_t(*batt->battery_model);
    double max_power = vol->calculate_max_charge_w(cap->q0(), cap->qmax(), nullptr);

    double desired_power = 0.;
    while (desired_power < max_power * 1.2){
        cap = batt->battery_model->capacity_model();
        vol = batt->battery_model->voltage_model();

        double current = vol->calculate_current_for_target_w(-desired_power, cap->q0(), cap->qmax());
        batt->battery_model->run(1, current);

        printf("%f\t target p, %f\t q0, %f\t soc, %f\t current, %f\t voltage, %f\t power\n", desired_power, cap->q0(), cap->SOC(),
               cap->I(), batt->battery_model->voltage_model()->battery_voltage(),
               cap->I() * batt->battery_model->voltage_model()->battery_voltage());

        if (desired_power < max_power)
            EXPECT_NEAR(cap->I() * vol->battery_voltage(), -desired_power, 1e-2);
        else{
            EXPECT_NEAR(cap->I() * vol->battery_voltage(), -max_power, 1e-2);
        }

        desired_power += max_power / 100.;
        delete batt->battery_model;
        batt->battery_model = new battery_t(initial_batt);
    }
}

TEST_F(ResilienceTest_lib_resilience, PVWattsSetUp)
{
    auto cap_model = batt->battery_model->capacity_model();
    auto voltage_model = batt->battery_model->voltage_model();
    const double voltage = 500;
    std::vector<double> batt_power, soc, charge;
    for (size_t i = 0; i < 10; i++){
        batt->initialize_time(0, i, 0);
        batt->advance(vartab, ac[i], voltage, load[i]);
        batt_power.push_back(batt->outBatteryPower[i]);
        soc.push_back(batt->outSOC[i]);
        charge.push_back(batt->outTotalCharge[i]);
        printf("%f ac, %f load, %f dispatch, %f charge, %f nominal power, %f max discharge, %f timestep power \n",
                ac[i], load[i], batt_vars->batt_custom_dispatch[i], charge.back(),
               voltage_model->battery_voltage_nominal() * cap_model->q0() / 1000.,
               voltage_model->calculate_max_discharge_w(cap_model->q0(), cap_model->qmax(), nullptr) / 1000.,
                batt->outBatteryVoltage[i] * batt->outCurrent[i]/1000.);
    }
    cap_model->change_SOC_limits(0, 100.);

    double current;
    double max_discharge = voltage_model->calculate_max_discharge_w(cap_model->q0(),
                                                                    cap_model->qmax(), &current);
    batt->battery_model->run(10, current);

    double vol_cur = voltage_model->battery_voltage();
    double power = cap_model->I() * vol_cur;
    printf("%f batt current, %f batt soc, %f batt power, %f target power\n", cap_model->I(),
           cap_model->SOC(), power, max_discharge);

    double max_charge = voltage_model->calculate_max_charge_w(cap_model->q0(),
                                                              cap_model->qmax(), &current);
    batt->battery_model->run(10, current);

    vol_cur = voltage_model->battery_voltage();
    power = cap_model->I() * vol_cur;
    printf("%f batt current, %f batt soc, %f batt power, %f target power\n", cap_model->I(),
           cap_model->SOC(), power, max_charge);
}

TEST_F(ResilienceTest_lib_resilience, PVWattsResilience)
{
//    resiliency_runner resilience(batt);
//    const double voltage = 500;
//    std::vector<double> batt_power, soc;
//    std::vector<int> outage_days_survived = {17, 18, 20};
//    for (size_t i = 0; i < 3; i++){
//        batt->initialize_time(0, i, 0);
//        batt->advance(vartab, ac[i], voltage, load[i]);
//        resilience.compute_metrics();
//        EXPECT_TRUE(resilience.get_outage_days_survived() == outage_days_survived[i]);
//    }
}