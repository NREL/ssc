#ifndef __LIB_CSP_TEST_H__
#define __LIB_CSP_TEST_H__

#include <gtest/gtest.h>
#include "../tcs/storage_hx.h"
#include "../tcs/flat_plate_solar_collector.h"

class FpcFactory {
public:
    FpcFactory() {};

    virtual FlatPlateArray* MakeFpcArray() const = 0;
    FlatPlateArray* MakeFpcArray(FlatPlateCollector* flat_plate_collector,
                                 CollectorLocation* collector_location,
                                 CollectorOrientation* collector_orientation,
                                 ArrayDimensions* array_dimensions,
                                 Pipe* inlet_pipe,
                                 Pipe* outlet_pipe) const;
    virtual FlatPlateCollector* MakeCollector() const = 0;
    FlatPlateCollector* MakeCollector(CollectorTestSpecifications* collector_test_specifications) const;
    TimeAndPosition* MakeTimeAndPosition() const;

    virtual CollectorTestSpecifications* MakeTestSpecifications() const = 0;
    virtual CollectorLocation* MakeLocation() const = 0;
    virtual CollectorOrientation* MakeOrientation() const = 0;
    virtual Pipe* MakePipe() const = 0;
    virtual ExternalConditions* MakeExternalConditions() const = 0;
    virtual tm* MakeTime() const = 0;
    virtual ArrayDimensions* MakeArrayDimensions() const = 0;
};

class DefaultFpcFactory : public FpcFactory {
public:
    DefaultFpcFactory() {};

    virtual FlatPlateArray* MakeFpcArray() const;
    virtual FlatPlateCollector* MakeCollector() const;

    virtual CollectorTestSpecifications* MakeTestSpecifications() const;
    virtual CollectorLocation* MakeLocation() const;
    virtual CollectorOrientation* MakeOrientation() const;
    virtual Pipe* MakePipe() const;
    virtual ExternalConditions* MakeExternalConditions() const;
    virtual tm* MakeTime() const;
    virtual ArrayDimensions* MakeArrayDimensions() const;
};

class FlatPlateCollectorTest : public ::testing::Test
{
public:
    const double m_error_tolerance_lo = 0.001;    // 0.1%
    const double m_error_tolerance_hi = 0.01;     // 1.0%

    void SetUp() {}

    void TearDown() {}
};

class FlatPlateArrayTest : public ::testing::Test
{
public:
    const double m_error_tolerance_lo = 0.001;    // 0.1%
    const double m_error_tolerance_hi = 0.01;     // 1.0%

    void SetUp() {}

    void TearDown() {}
};


class StorageTankTest : public ::testing::Test
{
protected:
    Storage_HX *m_storage;

    // inputs
    bool m_is_hot_tank;
    double m_dt;
    double m_m_prev;
    double m_T_prev;
    double m_m_dot_in;
    double m_m_dot_out;
    double m_T_in;
    double m_T_amb;

    //outputs
    double m_T_ave;
    double m_vol_ave;
    double m_q_loss;
    double m_T_fin;
    double m_vol_fin;
    double m_m_fin;
    double m_q_heater;

    // parameters for initialization
    int m_field_fluid;
    int m_store_fluid;
    HTFProperties m_fluid_field;
    HTFProperties m_fluid_store;
    bool m_is_direct;
    int m_config;
    double m_duty_des;
    double m_vol_des;
    double m_h_des;
    double m_u_des;
    double m_tank_pairs_des;
    double m_hot_htr_set_point_des;
    double m_cold_htr_set_point_des;
    double m_max_q_htr_cold;
    double m_max_q_htr_hot;
    double m_dt_hot_des;
    double m_dt_cold_des;
    double m_T_h_in_des;
    double m_T_h_out_des;

public:
    double m_error_tolerance_lo = 0.001;    // 0.1%
    double m_error_tolerance_hi = 0.01;     // 1.0%

    void SetUp();

    void TearDown()
    {
        if (m_storage) {
            delete m_storage;
            m_storage = nullptr;
        }
    }
};

#endif
