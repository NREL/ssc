#include "lib_resilience_test.h"


TEST_F(ResilienceTest, PVWattsSetUp)
{
    const double voltage = 500;
    std::vector<double> batt_power, soc, charge;
    for (size_t i = 0; i < 10; i++){
        batt->initialize_time(0, i, 0);
        batt->advance(vartab, ac[i], voltage, load[i]);
        batt_power.push_back(batt->outBatteryPower[i]);
        soc.push_back(batt->outSOC[i]);
        charge.push_back(batt->outTotalCharge[i]);
        printf("%f charge & %f soc, ", charge.back(), soc.back());
    }
}

TEST_F(ResilienceTest, PVWattsResilience)
{
    resiliency_runner resilience(batt);
    const double voltage = 500;
    std::vector<double> batt_power, soc;
    std::vector<int> outage_days_survived = {17, 18, 20};
    for (size_t i = 0; i < 3; i++){
        batt->initialize_time(0, i, 0);
        batt->advance(vartab, ac[i], voltage, load[i]);
        resilience.compute_metrics();
        EXPECT_TRUE(resilience.get_outage_days_survived() == outage_days_survived[i]);
    }
}