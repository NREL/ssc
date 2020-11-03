#include <gtest/gtest.h>

#include "lib_csp_test.h"


FlatPlateArray* FpcFactory::MakeFpcArray(FlatPlateCollector* flat_plate_collector,
                                         CollectorLocation* collector_location,
                                         CollectorOrientation* collector_orientation,
                                         ArrayDimensions* array_dimensions,
                                         Pipe* inlet_pipe,
                                         Pipe* outlet_pipe) const
{
    return new FlatPlateArray(*flat_plate_collector, *collector_location,
        *collector_orientation, *array_dimensions, *inlet_pipe, *outlet_pipe);
}

FlatPlateCollector* FpcFactory::MakeCollector(CollectorTestSpecifications* collector_test_specifications) const
{
    return new FlatPlateCollector(*collector_test_specifications);
}

TimeAndPosition* FpcFactory::MakeTimeAndPosition() const
{
    TimeAndPosition* time_and_position = new TimeAndPosition;
    time_and_position->timestamp = *this->MakeTime();
    time_and_position->collector_location = *this->MakeLocation();
    time_and_position->collector_orientation = *this->MakeOrientation();

    return time_and_position;
}

FlatPlateArray* DefaultFpcFactory::MakeFpcArray() const
{
    FlatPlateCollector* flat_plate_collector = this->MakeCollector();
    CollectorLocation* collector_location = this->MakeLocation();
    CollectorOrientation* collector_orientation = this->MakeOrientation();
    ArrayDimensions* array_dimensions = this->MakeArrayDimensions();

    Pipe* inlet_pipe = this->MakePipe();
    Pipe* outlet_pipe = this->MakePipe();

    return new FlatPlateArray(*flat_plate_collector, *collector_location,
        *collector_orientation, *array_dimensions, *inlet_pipe, *outlet_pipe);
}

FlatPlateCollector* DefaultFpcFactory::MakeCollector() const
{
    CollectorTestSpecifications* collector_test_specifications = this->MakeTestSpecifications();
    return new FlatPlateCollector(*collector_test_specifications);
}

CollectorTestSpecifications* DefaultFpcFactory::MakeTestSpecifications() const
{
    CollectorTestSpecifications* collector_test_specifications = new CollectorTestSpecifications();
    collector_test_specifications->FRta = 0.689;
    collector_test_specifications->FRUL = 3.85;
    collector_test_specifications->iam = 0.2;
    collector_test_specifications->area_coll = 2.98;
    collector_test_specifications->m_dot = 0.045528;         // kg/s   
    collector_test_specifications->heat_capacity = 4.182;    // kJ/kg-K

    return collector_test_specifications;
}

CollectorLocation* DefaultFpcFactory::MakeLocation() const
{
    CollectorLocation* collector_location = new CollectorLocation;
    collector_location->latitude = 33.45000;
    collector_location->longitude = -111.98000;
    collector_location->timezone = -7;

    return collector_location;
}

CollectorOrientation* DefaultFpcFactory::MakeOrientation() const
{
    CollectorOrientation* collector_orientation = new CollectorOrientation;
    collector_orientation->tilt = 30.;
    collector_orientation->azimuth = 180.;

    return collector_orientation;
}

Pipe* DefaultFpcFactory::MakePipe() const
{
    double inner_diameter = 0.019;
    double insulation_conductivity = 0.03;
    double insulation_thickness = 0.006;
    double length = 5;

    return new Pipe(inner_diameter, insulation_conductivity, insulation_thickness, length);
}

ExternalConditions* DefaultFpcFactory::MakeExternalConditions() const
{
    ExternalConditions* external_conditions = new ExternalConditions;
    external_conditions->weather.ambient_temp = 25.;
    external_conditions->weather.dni = 935.;
    external_conditions->weather.dhi = 84.;
    external_conditions->weather.ghi = std::numeric_limits<double>::quiet_NaN();
    external_conditions->weather.wind_speed = std::numeric_limits<double>::quiet_NaN();
    external_conditions->weather.wind_direction = std::numeric_limits<double>::quiet_NaN();
    external_conditions->inlet_fluid_flow.m_dot = 0.091056;          // kg/s
    external_conditions->inlet_fluid_flow.specific_heat = 4.182;     // kJ/kg-K
    external_conditions->inlet_fluid_flow.temp = 45.9;               // from previous timestep
    external_conditions->albedo = 0.2;

    return external_conditions;
}

tm* DefaultFpcFactory::MakeTime() const
{
    tm* time = new tm;
    // TODO - The timestamp should be generated from a string so all attributes are valid
    time->tm_year = 2012 - 1900;  // years since 1900
    time->tm_mon = 1 - 1;         // months since Jan. (Jan. = 0)
    time->tm_mday = 1;
    time->tm_hour = 12;
    time->tm_min = 30;
    time->tm_sec = 0;

    return time;
}

ArrayDimensions* DefaultFpcFactory::MakeArrayDimensions() const
{
    ArrayDimensions* array_dimensions = new ArrayDimensions;
    array_dimensions->num_in_parallel = 1;
    array_dimensions->num_in_series = 1;

    return array_dimensions;
}


TEST_F(FlatPlateCollectorTest, TestFlatPlateCollectorNominalOperation)
{
    DefaultFpcFactory default_fpc_factory = DefaultFpcFactory();
    FlatPlateCollector* flat_plate_collector = default_fpc_factory.MakeCollector();
    TimeAndPosition* time_and_position = default_fpc_factory.MakeTimeAndPosition();
    ExternalConditions* external_conditions = default_fpc_factory.MakeExternalConditions();

    double useful_power_gain = flat_plate_collector->UsefulPowerGain(*time_and_position, *external_conditions);  // [W]
    double T_out = flat_plate_collector->T_out(*time_and_position, *external_conditions);                        // [C]

    EXPECT_NEAR(useful_power_gain, 1.659e3, 1.659e3 * m_error_tolerance_hi);
    EXPECT_NEAR(T_out, 50.26, 50.26 * m_error_tolerance_hi);
}

TEST_F(FlatPlateArrayTest, TestFlatPlateArrayOfOneNominalOperation)
{
    DefaultFpcFactory default_fpc_factory = DefaultFpcFactory();
    FlatPlateArray* flat_plate_array = default_fpc_factory.MakeFpcArray();
    tm* timestamp = default_fpc_factory.MakeTime();
    ExternalConditions* external_conditions = default_fpc_factory.MakeExternalConditions();    
    external_conditions->inlet_fluid_flow.temp = 44.86;

    double useful_power_gain = flat_plate_array->UsefulPowerGain(*timestamp, *external_conditions);  // [W]
    double T_out = flat_plate_array->T_out(*timestamp, *external_conditions);                        // [C]

    EXPECT_NEAR(useful_power_gain, 1.587e3, 1.587e3 * m_error_tolerance_hi);
    EXPECT_NEAR(T_out, 49.03, 49.03 * m_error_tolerance_hi);
}



void StorageTankTest::SetUp()
{
    m_storage = new Storage_HX();

    m_field_fluid = 18;
    m_store_fluid = 18;
    m_fluid_field;
    m_fluid_store;
    m_is_direct = true;
    m_config = 2;
    m_duty_des = 623595520.;
    m_vol_des = 17558.4;
    m_h_des = 12.;
    m_u_des = 0.4;
    m_tank_pairs_des = 1.;
    m_hot_htr_set_point_des = 638.15;
    m_cold_htr_set_point_des = 523.15;
    m_max_q_htr_cold = 25.;
    m_max_q_htr_hot = 25.;
    m_dt_hot_des = 5.;
    m_dt_cold_des = 5.;
    m_T_h_in_des = 703.15;
    m_T_h_out_des = 566.15;

    m_fluid_field.SetFluid(m_field_fluid);
    m_fluid_store.SetFluid(m_store_fluid);

    m_storage->define_storage(m_fluid_field, m_fluid_store, m_is_direct,
        m_config, m_duty_des, m_vol_des, m_h_des,
        m_u_des, m_tank_pairs_des, m_hot_htr_set_point_des, m_cold_htr_set_point_des,
        m_max_q_htr_cold, m_max_q_htr_hot, m_dt_hot_des, m_dt_cold_des, m_T_h_in_des, m_T_h_out_des);
}

TEST_F(StorageTankTest, TestDrainingTank_storage_hx)
{
    m_is_hot_tank = false;
    m_dt = 3600;
    m_m_prev = 3399727.;
    m_T_prev = 563.97;
    m_m_dot_in = 0.;
    m_m_dot_out = 1239.16;      // this will more than drain the tank
    m_T_in = 566.15;
    m_T_amb = 296.15;
    
    m_storage->mixed_tank(m_is_hot_tank, m_dt, m_m_prev, m_T_prev, m_m_dot_in, m_m_dot_out, m_T_in, m_T_amb,
        m_T_ave, m_vol_ave, m_q_loss, m_T_fin, m_vol_fin, m_m_fin, m_q_heater);

    EXPECT_NEAR(m_T_ave, 563.7, 563.7 * m_error_tolerance_lo);
    EXPECT_NEAR(m_vol_ave, 892.30, 892.30 * m_error_tolerance_lo);
    EXPECT_NEAR(m_q_loss, 0.331, 0.331 * m_error_tolerance_lo);
    EXPECT_NEAR(m_T_fin, 558.9, 558.9 * m_error_tolerance_lo);
    EXPECT_NEAR(m_vol_fin, 0., 0. * m_error_tolerance_lo);
    EXPECT_NEAR(m_m_fin, 0., 0. * m_error_tolerance_lo);
    EXPECT_NEAR(m_q_heater, 0., 0. * m_error_tolerance_lo);
}

TEST_F(StorageTankTest, TestDrainedTank_storage_hx)
{
    m_is_hot_tank = false;
    m_dt = 3600;
    m_m_prev = 0.;
    m_T_prev = 563.97;
    m_m_dot_in = 0.;
    m_m_dot_out = 1239.16;
    m_T_in = 566.15;
    m_T_amb = 296.15;

    m_storage->mixed_tank(m_is_hot_tank, m_dt, m_m_prev, m_T_prev, m_m_dot_in, m_m_dot_out, m_T_in, m_T_amb,
        m_T_ave, m_vol_ave, m_q_loss, m_T_fin, m_vol_fin, m_m_fin, m_q_heater);

    EXPECT_NEAR(m_T_ave, 563.97, 563.97 * m_error_tolerance_lo);
    EXPECT_NEAR(m_vol_ave, 0., 0. * m_error_tolerance_lo);
    EXPECT_NEAR(m_q_loss, 0., 0. * m_error_tolerance_lo);
    EXPECT_NEAR(m_T_fin, 563.97, 563.97 * m_error_tolerance_lo);
    EXPECT_NEAR(m_vol_fin, 0., 0. * m_error_tolerance_lo);
    EXPECT_NEAR(m_m_fin, 0., 0. * m_error_tolerance_lo);
    EXPECT_NEAR(m_q_heater, 0., 0. * m_error_tolerance_lo);
}
