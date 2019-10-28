#include "lib_resilience_test.h"
TEST_F(ResilienceTest_lib_resilience, DischargePower)
{
    batt->advance(vartab, 0, 500, 0);
    printf("%f q0, %f soc, %f current, %f voltage, %f power\n", batt->capacity_model->q0(), batt->capacity_model->SOC(),
           batt->capacity_model->I(), batt->outBatteryVoltage[0], batt->outBatteryPower[0] * batt->batt_vars->batt_ac_dc_efficiency * 0.01);
    batt->battery_model->capacity_model()->change_SOC_limits(1., 99.);
    while (batt->battery_model->capacity_model()->SOC() > 10)
        batt->battery_model->run(0, 2);
    battery_t initial_batt = battery_t(*batt->battery_model);
    double desired_power = 0.;
    double max_power = initial_batt.capacity_model()->q0() * initial_batt.voltage_model()->battery_voltage_nominal();
    while (desired_power < max_power){
        printf("%f\t starting q0, ", batt->battery_model->capacity_model()->q0());
        if (desired_power > 100)
            int x = 0;
        batt->battery_model->run(1, desired_power/batt->voltage_model->battery_voltage_nominal());
        printf("%f\t target p, %f\t q0, %f\t soc, %f\t current, %f\t voltage, %f\t power\n", desired_power, batt->capacity_model->q0(), batt->capacity_model->SOC(),
               batt->battery_model->capacity_model()->I(), batt->battery_model->voltage_model()->battery_voltage(),
               batt->battery_model->capacity_model()->I() * batt->battery_model->voltage_model()->battery_voltage());
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
               voltage_model->calculate_max_discharge_kw(cap_model->q0(), cap_model->qmax(),
                                                               1, nullptr) / 1000.,
                batt->outBatteryVoltage[i] * batt->outCurrent[i]/1000.);
    }
    cap_model->change_SOC_limits(0, 99.);
    double current;
    double max_discharge = voltage_model->calculate_max_discharge_kw(cap_model->q0(),
                                                                     cap_model->qmax(), 1, &current);
    batt->battery_model->run(10, current);

    double vol_cur = voltage_model->battery_voltage();
    double power = cap_model->I() * vol_cur;
    printf("%f batt current, %f batt soc, %f batt power, %f target power\n", cap_model->I(),
           cap_model->SOC(), power, max_discharge);
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