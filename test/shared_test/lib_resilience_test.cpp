#include "lib_resilience_test.h"

TEST_F(ResilienceTest_lib_resilience, DischargeBatteryModel)
{
    auto cap = batt->battery_model->capacity_model();
    auto vol = batt->battery_model->voltage_model();
    cap->change_SOC_limits(0, 100);
    while (cap->SOC() > 10)
        batt->battery_model->run(0, 1);

    battery_t initial_batt = battery_t(*batt->battery_model);
    double max_power = vol->calculate_max_discharge_w(cap->q0(), cap->qmax(), 0, nullptr);

    double desired_power = 0.;
    while (desired_power < max_power * 1.2){
        cap = batt->battery_model->capacity_model();
        vol = batt->battery_model->voltage_model();

        double current = vol->calculate_current_for_target_w(desired_power, cap->q0(), cap->qmax(), 0);

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

TEST_F(ResilienceTest_lib_resilience, ChargeBatteryModel)
{
    auto cap = batt->battery_model->capacity_model();
    auto vol = batt->battery_model->voltage_model();
    cap->change_SOC_limits(0, 100);
    while (cap->SOC() > 90)
        batt->battery_model->run(0, -1);

    battery_t initial_batt = battery_t(*batt->battery_model);
    double max_power = vol->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, nullptr);

    double desired_power = 0.;
    while (desired_power < max_power * 1.2){
        cap = batt->battery_model->capacity_model();
        vol = batt->battery_model->voltage_model();

        double current = vol->calculate_current_for_target_w(-desired_power, cap->q0(), cap->qmax(), 0);
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

    batt_vars->batt_loss_choice = losses_t::TIMESERIES;
    for (size_t n = 1; n < 8760; n++)
        batt_vars->batt_losses_charging.emplace_back(n*5);
    batt_vars->batt_losses_discharging.emplace_back(0);
    batt_vars->batt_losses_idle.emplace_back(0);
    auto losses = losses_t(1, batt->lifetime_model, batt->thermal_model, batt->capacity_model, batt_vars->batt_loss_choice,
            batt_vars->batt_losses_charging, batt_vars->batt_losses_discharging, batt_vars->batt_losses_idle, batt_vars->batt_losses_charging);

    batt->battery_model->initialize(batt->capacity_model, batt->voltage_model, batt->lifetime_model, batt->thermal_model, &losses);

    auto power_model = batt->dispatch_model->getBatteryPowerFlow()->getBatteryPower();

    size_t count = 0;
    while (batt->battery_model->losses_model()->getLoss(count) < 100.){
        batt->advance(vartab, ac[count], 500);

//        printf("%f\t current, %f\t voltage, %f\t losses, %f\t power\n",
//               cap->I(), vol->battery_voltage(), batt->battery_model->losses_model()->getLoss(count), power_model->powerBatteryDC);

        count ++;
    }
}

TEST_F(ResilienceTest_lib_resilience, VoltageTable)
{
    std::vector<double> vals = {99, 0, 50, 2, 0, 3};
    util::matrix_t<double> table(3, 2, &vals);
    auto volt = voltage_table_t(1, 1, 3, table, 0.1);
    auto cap = capacity_lithium_ion_t(2.25, 50, 100, 0);

    volt.updateVoltage(&cap, nullptr, 0.);
    EXPECT_NEAR(cap.DOD(), 50, 1e-3);
    EXPECT_NEAR(volt.cell_voltage(), 2, 1e-3);


    double current = -2.;
    cap.updateCapacity(current, 1);
    volt.updateVoltage(&cap, nullptr, 0.);
    EXPECT_NEAR(cap.DOD(), 0, 1e-3);
    EXPECT_NEAR(volt.cell_voltage(), 3, 1e-3);

    current = 4.;
    cap.updateCapacity(current, 1);
    volt.updateVoltage(&cap, nullptr, 0.);
    EXPECT_NEAR(cap.DOD(), 100, 1e-3);
    EXPECT_NEAR(volt.cell_voltage(), 0, 1e-3);

    current = -1;
    cap.updateCapacity(current, 1);
    volt.updateVoltage(&cap, nullptr, 0.);
    EXPECT_NEAR(cap.DOD(), 55.555, 1e-3);
    EXPECT_NEAR(volt.cell_voltage(), 1.773, 1e-3);

    current = -1;
    cap.updateCapacity(current, 1);
    volt.updateVoltage(&cap, nullptr, 0.);
    EXPECT_NEAR(cap.DOD(), 11.111, 1e-3);
    EXPECT_NEAR(volt.cell_voltage(), 2.777, 1e-3);
}

TEST_F(ResilienceTest_lib_resilience, DischargeVoltageTable){
    std::vector<double> vals = {99, 0, 50, 2, 0, 3};
    util::matrix_t<double> table(3, 2, &vals);
    auto volt = voltage_table_t(1, 1, 3, table, 0.1);
    auto cap = capacity_lithium_ion_t(2.25, 50, 100, 0);

    // test discharging
    double req_cur = volt.calculate_current_for_target_w(2.2386, 2.25, 2.25, 0);
    EXPECT_NEAR(req_cur, 1.11375, 1e-2);

    req_cur = volt.calculate_current_for_target_w(1.791, 2.25, 2.25, 0);
    EXPECT_NEAR(req_cur, 0.7748, 1e-2);

    req_cur = volt.calculate_current_for_target_w(1.343, 2.25, 2.25, 0);
    EXPECT_NEAR(req_cur, 0.5313, 1e-2);

    req_cur = volt.calculate_current_for_target_w(0.5, cap.q0(), cap.qmax(), 0);
    cap.updateCapacity(req_cur, 1);
    volt.updateVoltage(&cap, nullptr, 1);
    double v = volt.cell_voltage();
    EXPECT_NEAR(req_cur * v, 0.5, 1e-2);
}

TEST_F(ResilienceTest_lib_resilience, ChargeVoltageTable){
    std::vector<double> vals = {99, 0, 50, 2, 0, 3};
    util::matrix_t<double> table(3, 2, &vals);
    auto volt = voltage_table_t(1, 1, 3, table, 0.1);
    auto cap = capacity_lithium_ion_t(2.25, 50, 100, 0);

    // test charging
    double current = 10;
    cap.updateCapacity(current, 1);
    double req_cur = volt.calculate_current_for_target_w(-1.5, 0, 2.25, 0);
    cap.updateCapacity(req_cur, 1);
    volt.updateVoltage(&cap, nullptr, 1);
    double v = volt.cell_voltage();
    EXPECT_NEAR(req_cur * v, -1.5, 1e-2);

    double max_p = volt.calculate_max_charge_w(cap.q0(), cap.qmax(), 0, &current);
    cap.updateCapacity(current, 1);
    volt.updateVoltage(&cap, nullptr, 1);
    EXPECT_NEAR(max_p, cap.I() * volt.cell_voltage(), 1e-3);
}

class thermal_test : public thermal_t{
public:
    thermal_test(){_T_battery = 33 + 273.15;};
    ~thermal_test(){};
};

TEST_F(ResilienceTest_lib_resilience, VoltageVanadium){
    auto volt = voltage_vanadium_redox_t(1, 1, 1.41, 0.001, 1);
    auto cap = capacity_lithium_ion_t(11, 30, 100, 0);
    auto temp = thermal_test();

    volt.updateVoltage(&cap, &temp, 1);
    double v = volt.cell_voltage();


    double req_cur = volt.calculate_current_for_target_w(1.5, 3.3, 11, temp.T_battery());
    cap.updateCapacity(req_cur, 1);
    volt.updateVoltage(&cap, &temp, 1);
    v = volt.cell_voltage();
    EXPECT_NEAR(req_cur * v, 1.5, 1e-2);

    req_cur = volt.calculate_current_for_target_w(-1.5, cap.q0(), cap.qmax(), temp.T_battery());
    cap.updateCapacity(req_cur, 1);
    volt.updateVoltage(&cap, &temp, 1);
    v = volt.cell_voltage();
    EXPECT_NEAR(req_cur * v, -1.5, 1e-2);

    double max_p = volt.calculate_max_charge_w(cap.q0(), cap.qmax(), temp.T_battery(), &req_cur);
    cap.updateCapacity(req_cur, 1);
    volt.updateVoltage(&cap, &temp, 1);
    EXPECT_NEAR(max_p, cap.I() * volt.cell_voltage(), 1e-3);

    max_p = volt.calculate_max_discharge_w(cap.q0(), cap.qmax(), temp.T_battery(), &req_cur);
    cap.updateCapacity(req_cur, 1);
    volt.updateVoltage(&cap, &temp, 1);
    EXPECT_NEAR(max_p, cap.I() * volt.cell_voltage(), 1e-3);
}

TEST_F(ResilienceTest_lib_resilience, RoundtripEffModel){
    auto cap = batt->battery_model->capacity_model();
    auto vol = batt->battery_model->voltage_model();
    cap->change_SOC_limits(0, 100);


    double full_current = 1000;
    double max_current;
    vol->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);

    double current = abs(max_current) * 0.01;
    while (current < abs(max_current)){
        cap->updateCapacity(full_current, 1);   //discharge to empty

        size_t n_t = 0;
        current *= -1;
        double input_power = 0.;
        while(cap->SOC() < 100 ){
            double input_current = current;
            cap->updateCapacity(input_current, 1);
            vol->updateVoltage(cap, nullptr, 1);
            input_power += cap->I() * vol->battery_voltage();
            n_t += 1;

        }

        current *= -1;
        double output_power = 0.;
        while(vol->calculate_max_discharge_w(cap->q0(), cap->qmax(), 0, nullptr) > 0 ){
            double output_current = current;
            cap->updateCapacity(output_current, 1);
            vol->updateVoltage(cap, nullptr, 1);
            output_power += cap->I() * vol->battery_voltage();
            n_t += 1;

        }

//        printf("current %f, eff %f, n %d\n", current, -output_power/input_power, n_t);

        current += abs(max_current) / 100.;
    }
}

TEST_F(ResilienceTest_lib_resilience, RoundtripEffTable){
    std::vector<double> vals = {0, batt_vars->batt_Vfull, 1.78, batt_vars->batt_Vexp,
                                88.9, batt_vars->batt_Vnom, 99, 0};
    util::matrix_t<double> table(4, 2, &vals);
    auto vol = std::unique_ptr<voltage_table_t>(new voltage_table_t(batt_vars->batt_computed_series,
            batt_vars->batt_computed_strings, batt_vars->batt_Vnom_default, table, batt_vars->batt_resistance));
    auto cap = batt->battery_model->capacity_model();
    cap->change_SOC_limits(0, 100);


    double full_current = 1000;
    double max_current;
    vol->calculate_max_charge_w(cap->q0(), cap->qmax(), 0, &max_current);

    double current = abs(max_current) * 0.01;
    while (current < abs(max_current)){
        cap->updateCapacity(full_current, 1);   //discharge to empty

        size_t n_t = 0;
        current *= -1;
        double input_power = 0.;
        while(cap->SOC() < 100 ){
            double input_current = current;
            cap->updateCapacity(input_current, 1);
            vol->updateVoltage(cap, nullptr, 1);
            input_power += cap->I() * vol->battery_voltage();
            n_t += 1;
        }

        current *= -1;
        double output_power = 0.;
        while(vol->calculate_max_discharge_w(cap->q0(), cap->qmax(), 0, nullptr) > 0 ){
            double output_current = current;
            cap->updateCapacity(output_current, 1);
            vol->updateVoltage(cap, nullptr, 1);
            output_power += cap->I() * vol->battery_voltage();
            n_t += 1;
        }

//        printf("current %f, eff %f, n %d\n", current, -output_power/input_power, n_t);

        current += abs(max_current) / 100.;
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