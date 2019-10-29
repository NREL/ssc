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

        printf("%f\t target p, %f\t q0, %f\t soc, %f\t current, %f\t voltage, %f\t power, %f\t temp\n", desired_power, cap->q0(), cap->SOC(),
               cap->I(), batt->battery_model->voltage_model()->battery_voltage(),
               cap->I() * batt->battery_model->voltage_model()->battery_voltage(), batt->battery_model->thermal_model()->T_battery());

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
    auto cap = batt->battery_model->capacity_model();
    auto vol = batt->battery_model->voltage_model();
    cap->change_SOC_limits(0, 100);

    battery_t initial_batt = battery_t(*batt->battery_model);
    double max_power = vol->calculate_max_charge_w(cap->q0(), cap->qmax(), nullptr);

    double desired_power = max_power;
    while (desired_power > 0){
//        cap = batt->battery_model->capacity_model();
//        vol = batt->battery_model->voltage_model();

        double current = vol->calculate_current_for_target_w(-desired_power, cap->q0(), cap->qmax());
        batt->battery_model->run(1, current);

        printf("%f\t target p, %f\t q0, %f\t soc, %f\t current, %f\t voltage, %f\t power, %f\t temp\n", desired_power, cap->q0(), cap->SOC(),
               cap->I(), batt->battery_model->voltage_model()->battery_voltage(),
               cap->I() * batt->battery_model->voltage_model()->battery_voltage(), batt->battery_model->thermal_model()->T_battery());

        batt->battery_model->run(1, -current);

        printf("%f\t target p, %f\t q0, %f\t soc, %f\t current, %f\t voltage, %f\t power, %f\t temp\n", desired_power, cap->q0(), cap->SOC(),
               cap->I(), batt->battery_model->voltage_model()->battery_voltage(),
               cap->I() * batt->battery_model->voltage_model()->battery_voltage(), batt->battery_model->thermal_model()->T_battery());


        desired_power -= max_power / 100.;
//        delete batt->battery_model;
//        batt->battery_model = new battery_t(initial_batt);
    }
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