#include "cmod_solar_thermal_eqns.h"
//#include "cmod_solar_thermal_common_eqns.h"   // doesn't yet exist
#include "flat_plate_solar_collector.h"         // bypassing a common eqns intermediary for now
#include "vartab.h"
#include "htf_props.h"

#pragma warning(disable: 4297)  // ignore warning: 'function assumed not to throw an exception but does'

void Flat_Plate_Array_Design_Equations(ssc_data_t data)
{
    auto vt = static_cast<var_table*>(data);
    if (!vt) {
        throw std::runtime_error("ssc_data_t data invalid");
    }
    double m_dot_array_design /*kg/s*/, specific_heat /*kJ/kg-K*/, temp_rise_array_design /*K*/;

    CollectorTestSpecifications collector_test_specifications;
    vt_get_number(vt, "frta", &collector_test_specifications.FRta);
    vt_get_number(vt, "frul", &collector_test_specifications.FRUL);
    vt_get_number(vt, "iam", &collector_test_specifications.iam);
    vt_get_number(vt, "area_coll", &collector_test_specifications.area_coll);
    vt_get_number(vt, "m_dot_tested", &collector_test_specifications.m_dot);
    vt_get_number(vt, "heat_capacity_tested", &collector_test_specifications.heat_capacity);

    CollectorLocation collector_location;
    vt_get_number(vt, "latitude", &collector_location.latitude);
    vt_get_number(vt, "longitude", &collector_location.longitude);
    vt_get_int(vt, "timezone", &collector_location.timezone);

    CollectorOrientation collector_orientation;
    vt_get_number(vt, "tilt", &collector_orientation.tilt);
    vt_get_number(vt, "azimuth", &collector_orientation.azimuth);

    ArrayDimensions array_dimensions;
    array_dimensions.num_in_series = 1;     // placeholder, will resize
    array_dimensions.num_in_parallel = 1;   // placeholder, will resize

    // As hardcoded in csp_solver_trough_collector_receiver.cpp, line 453
    double pipe_diam = 0.019;       // [m]
    double pipe_k = 0.03;           // [W/m2-K]
    double pipe_insul = 0.006;      // [m]
    double pipe_length = 5.;        // [m]
    Pipe inlet_pipe = Pipe(pipe_diam, pipe_k, pipe_insul, pipe_length);
    Pipe outlet_pipe(inlet_pipe);

    FlatPlateArray flat_plate_array = FlatPlateArray(
        collector_test_specifications,
        collector_location,
        collector_orientation,
        array_dimensions,
        inlet_pipe,
        outlet_pipe);

    vt_get_number(vt, "m_dot_array_design", &m_dot_array_design);
    vt_get_number(vt, "specific_heat", &specific_heat);       // [kJ/kg-K]
    vt_get_number(vt, "temp_rise_array_design", &temp_rise_array_design);
    double dT_approach = 0.;    // 0 = for no HX

    HTFProperties fluid;
    fluid.SetFluid(HTFProperties::Water_liquid);
    flat_plate_array.resize_array(m_dot_array_design, specific_heat, temp_rise_array_design, dT_approach, fluid);
    array_dimensions = flat_plate_array.array_size();
    double ncoll = flat_plate_array.ncoll();
    double area_total = flat_plate_array.area_total();

    vt->assign("num_in_series", array_dimensions.num_in_series);
    vt->assign("num_in_parallel", array_dimensions.num_in_parallel);
    vt->assign("num_collectors", ncoll);
    vt->assign("aperture_array", area_total);

    double dni_des, poa_des, frta, frul, Ti_minus_Ta, conversion_efficiency, thermal_output;
    //double poa_irradiance = FlatPlateCollector::IncidentIrradiance();
    vt_get_number(vt, "dni_des", &dni_des);
    vt_get_number(vt, "frta", &frta);
    vt_get_number(vt, "frul", &frul);
    vt_get_number(vt, "ti_minus_ta", &Ti_minus_Ta);

    // from SHW page:
    // system capacity [kW] = A_c * N_c * (FRta - FRul * 30 / 1000)
    //                 [W]  = A_c * N_c * (FRta * 1000 - FRul * 30)
    //                 [W]  = A_c * N_c * (FRta * G_T - FRul * 30)

    poa_des = dni_des;      // not sure about this;
    double system_capacity = area_total * (frta * poa_des - frul * Ti_minus_Ta) * 1.e-3;    // [kW]
    vt->assign("system_capacity", system_capacity);
}

//void Flat_Plate_Array_Operation_Equations(ssc_data_t data)
//{
//
//}
