#include <gtest/gtest.h>

#include <lib_storage_capacity.h>

/**
* Storage Capacity Class test
*/

class sharedInverterTest : public ::testing::Test {
protected:
    SharedInverter* inv;
    sandia_inverter_t sinv;
    partload_inverter_t plinv;
    ond_inverter ondinv;
    double pAC = 100.;
    double eff = 0.;
    double loss = 0.;
    double e = 0.01;
public:
    void reset() {
        pAC = 100;
        eff = 1.;
        loss = 0.;
    }
    void SetUp() {
        inv = new SharedInverter(0, 1, &sinv, &plinv, &ondinv);
    }
    void TearDown() {
        if (inv) delete inv;
    }
};