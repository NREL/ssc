/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef __FLAT_PLATE_SOLAR_COLLECTOR__
#define __FLAT_PLATE_SOLAR_COLLECTOR__

#include <limits>
#include <time.h>
#include <vector>
#include "htf_props.h"
#include "numeric_solvers.h"
#include "csp_solver_two_tank_tes.h"

struct CollectorTestSpecifications
{
    double FRta;                        // [-]
    double FRUL;                        // [W/m2-K]
    double iam;                         // [-]
    double area_coll;                   // [m2]
    double m_dot;                       // [kg/s]
    double heat_capacity;               // [kJ/kg-K]
};

struct CollectorLocation
{
    double latitude;                    // [deg N]
    double longitude;                   // [deg E]
    int timezone;                       // [hr]
};

struct CollectorOrientation
{
    double tilt;                        // [deg]
    double azimuth;                     // [deg] Clockwise from North
};

struct ArrayDimensions
{
    int num_in_series;
    int num_in_parallel;
};

struct TimeAndPosition
{
    tm timestamp;
    CollectorLocation collector_location;
    CollectorOrientation collector_orientation;
};

struct Weather
{
    double dni;                         // [W/m2]
    double dhi;                         // [W/m2]
    double ghi;                         // [W/m2]
    double ambient_temp;                // [C]
    double wind_speed;                  // [m/s]
    double wind_direction;              // [deg] Clockwise from North
};

struct FluidFlow
{
    HTFProperties fluid;
    double temp;                        // [C]
    double m_dot;                       // [kg/s]
    double specific_heat;               // [kJ/kg-K]    TODO: remove this
};

struct ExternalConditions
{
    Weather weather;
    FluidFlow inlet_fluid_flow;
    double albedo;                      // [-]
};

struct PoaIrradianceComponents
{
    std::vector<double> beam_with_aoi;                          // {[W/m2], [deg]}
    std::vector<double> sky_diffuse_with_aoi;                   // {[W/m2], [deg]}
    std::vector<double> ground_reflected_diffuse_with_aoi;      // {[W/m2], [deg]}

    PoaIrradianceComponents()
    {
        beam_with_aoi.resize(2, std::numeric_limits<double>::quiet_NaN());
        sky_diffuse_with_aoi.resize(2, std::numeric_limits<double>::quiet_NaN());
        ground_reflected_diffuse_with_aoi.resize(2, std::numeric_limits<double>::quiet_NaN());
    }
};



class FlatPlateCollector
{
public:
    FlatPlateCollector();
    FlatPlateCollector(const CollectorTestSpecifications &collector_test_specifications);
    const static PoaIrradianceComponents IncidentIrradiances(const TimeAndPosition &time_and_position,
        const Weather &weather,
        double albedo /*-*/);
    const static double IncidentIrradiance(const TimeAndPosition& time_and_position,
        const Weather& weather,
        double albedo  /*-*/);   // [W/m2]
    const double RatedPowerGain();      // [W]
    const double RatedMassFlow();       // [kg/s]
    const double MaxAllowedTemp();      // [C]
    const double MaxMassFlow();         // [kg/s]
    const double EstimatePowerGain(double POA /*W/m2*/, double T_in /*C*/, double T_amb /*C*/);   // [W]
    const double UsefulPowerGain(const TimeAndPosition &time_and_position, const ExternalConditions &external_conditions);  // [W]
    const double T_out(const TimeAndPosition &time_and_position, const ExternalConditions &external_conditions);            // [C]
    const double area_coll();           // [m2]
    void area_coll(double collector_area /*m2*/);
    const CollectorTestSpecifications TestSpecifications();
private:
    double FRta_;                       // [-] flow rate correction
    double FRUL_;                       // [W/m2-K] flow rate correction
    double iam_;                        // [-] incidence angle modifier
    double area_coll_;                  // [m2] collector area
    double m_dot_test_;                 // [kg/s] mass flow through collector during test
    double heat_capacity_rate_test_;    // [kW/K] m_dot * c_p during ratings test
    const static double kMDotRated_  ; // [kg/s] based on published Heliodyne specs for Gobi 410
    const double AbsorbedIrradianceOverTauAlphaN(const CollectorOrientation &collector_orientation,
        const PoaIrradianceComponents &poa_irradiance_components);      // [W/m2]
    const double AbsorbedRadiantPower(double transmitted_irradiance /*W/m2*/,
        const FluidFlow &inlet_fluid_flow,
        double T_amb /*C*/);    // [W]
    const double ThermalPowerLoss(const FluidFlow &inlet_fluid_flow,
        double T_amb /*C*/);    // [W]
};



class Pipe
{
public:
    Pipe();
    Pipe(double pipe_diam /*m*/, double pipe_k /*W/m2-K*/, double pipe_insul /*m*/, double pipe_length /*m*/);
    const double pipe_od();             // [m]
    const double ThermalPowerLoss(double T_in /*C*/, double T_amb /*C*/);    // [W]
    const double T_out(double T_in /*C*/, double T_amb /*C*/, double heat_capacity_rate /*kW/K*/);  // [C]
private:
    double pipe_diam_;                  // [m]
    double pipe_k_;                     // [W/m-K]
    double pipe_insul_;                 // [m]
    double pipe_length_;                // [m] in whole system
    const double UA_pipe();             // [W/K]
};

struct HxDesignProps
{
    int external_fluid_id;     // HTFProperties::enum
    int subsystem_fluid_id;    // HTFProperties::enum
    double duty;            // [kW]
    double dT_approach;     // [K]
    double T_in_hot;        // [C]
    double T_out_hot;       // [C]
};

class HeatExchanger : public C_hx_two_tank_tes
{
public:
    HeatExchanger(const HxDesignProps &hx_design_props);
    HeatExchanger();
    void SetHxDesignProps(const HxDesignProps &hx_design_props);
    const HxDesignProps* GetHxDesignProps() const;
private:
    HxDesignProps hx_design_props_;
};


class FlatPlateArray
{
public:
    FlatPlateArray();
    FlatPlateArray(const FlatPlateCollector &flat_plate_collector, const CollectorLocation &collector_location,
        const CollectorOrientation &collector_orientation, const ArrayDimensions &array_dimensions,
        const Pipe &inlet_pipe, const Pipe &outlet_pipe);
    FlatPlateArray(const CollectorTestSpecifications &collector_test_specifications, const CollectorLocation &collector_location,
        const CollectorOrientation &collector_orientation, const ArrayDimensions &array_dimensions,
        const Pipe &inlet_pipe, const Pipe &outlet_pipe);
    void SetHxDesignProps(const HxDesignProps &hx_design_props);
    FluidFlow RunWithHx(tm& timestamp, ExternalConditions& external_conditions, double T_out_target);
    FluidFlow RunSimplifiedWithHx(tm& timestamp, ExternalConditions& external_conditions);
    const int ncoll();
    const double area_total();                             // [m2]
    void resize_array(ArrayDimensions array_dimensions);
    void resize_array(double m_dot_array_design /*kg/s*/, double specific_heat /*kJ/kg-K*/, double temp_rise_array_design /*K*/);
    void resize_num_in_parallel(double m_dot_array_design /*kg/s*/);
    void resize_num_in_series(double m_dot_array_design /*kg/s*/, double specific_heat /*kJ/kg-K*/, double temp_rise_array_design /*K*/);
    ArrayDimensions array_size() const;
    const double IncidentIrradiance(const tm& timestamp, const ExternalConditions& external_conditions);   // [W/m2] POA
    const double RatedPowerGain();      // [W]
    const double RatedMassFlow();       // [kg/s]
    const double MaxAllowedTemp();      // [C]
    const double MaxMassFlow();         // [kg/s]
    const double EstimatePowerGain(double POA /*W/m2*/, double T_in /*C*/, double T_amb /*C*/);            // [W]
    const double UsefulPowerGain(const tm &timestamp, const ExternalConditions &external_conditions);      // [W]
    const double T_out(const tm &timestamp, const ExternalConditions &external_conditions);                // [C]
    HTFProperties* GetFluid();
private:
    FlatPlateCollector flat_plate_collector_;       // just scale a single collector for now -> premature optimization??
    HeatExchanger heat_exchanger_;
    HTFProperties fluid_;                           // used only if there's a HX
    CollectorLocation collector_location_;
    CollectorOrientation collector_orientation_;
    ArrayDimensions array_dimensions_;
    Pipe inlet_pipe_;
    Pipe outlet_pipe_;
};


class C_MEQ__mdot_fp : public C_monotonic_equation
{
public:
    C_MEQ__mdot_fp(
        double T_approach,
        double m_T_loop_in,
        double T_f_hx_out_target,
        double m_m_dot_process_heat,
        FlatPlateArray* flat_plate_array,
        C_hx_two_tank_tes* heat_exchanger,
        HTFProperties* m_htfProps,
        HTFProperties* flat_plate_htf,
        tm* timestamp,
        ExternalConditions* external_conditions
    ) :
        T_approach_(T_approach),
        m_T_loop_in_(m_T_loop_in),
        T_f_hx_out_target_(T_f_hx_out_target),
        m_m_dot_process_heat_(m_m_dot_process_heat),
        flat_plate_array_(flat_plate_array),
        heat_exchanger_(heat_exchanger),
        m_htfProps_(m_htfProps),
        flat_plate_htf_(flat_plate_htf),
        timestamp_(timestamp),
        external_conditions_(external_conditions)

    {
        fp_array_is_on_ = true;
        T_closest_f_hx_out_iter_ = -std::numeric_limits<double>::infinity();
        T_out_fp_at_T_closest_iter_ = std::numeric_limits<double>::quiet_NaN();
        mdot_fp_at_T_closest_iter_ = std::numeric_limits<double>::quiet_NaN();
    }

    bool fp_array_is_on_;
    double T_f_hx_out_;
    double T_out_fp_;
    double eff_hx_;
    double q_dot_hx_;
    double dT_hot_;
    double dT_cold_;
    double T_closest_f_hx_out_iter_;		// max converged temperature found during iteration of outer MEQ
    double T_out_fp_at_T_closest_iter_;		// flat plate outlet temperature at max converged system temperature
    double mdot_fp_at_T_closest_iter_;		// flat plate mass flow at max converged system temperature
    virtual int operator()(double mdot_fp /*kg/s*/, double* diff_T_out_f /*C*/);

private:
    double T_approach_;
    double mdot_fp_;
    double m_T_loop_in_;
    double T_f_hx_out_target_;
    double m_m_dot_process_heat_;
    FlatPlateArray* flat_plate_array_;
    C_hx_two_tank_tes* heat_exchanger_;
    HTFProperties* m_htfProps_;
    HTFProperties* flat_plate_htf_;
    tm* timestamp_;
    ExternalConditions* external_conditions_;
};


class C_MEQ__T_in_fp : public C_monotonic_equation
{
public:
    C_MEQ__T_in_fp(
        double T_approach,
        double mdot_fp,					// this is provided by the outer MEQ, constrained to max mass flow
        double m_T_loop_in,
        double T_f_hx_out_target,
        double m_m_dot_process_heat,
        FlatPlateArray* flat_plate_array,
        C_hx_two_tank_tes* heat_exchanger,
        HTFProperties* m_htfProps,
        HTFProperties* flat_plate_htf,
        tm* timestamp,
        ExternalConditions* external_conditions
    ) :
        T_approach_(T_approach),
        mdot_fp_(mdot_fp),
        m_T_loop_in_(m_T_loop_in),
        T_f_hx_out_target_(T_f_hx_out_target),
        m_m_dot_process_heat_(m_m_dot_process_heat),
        flat_plate_array_(flat_plate_array),
        heat_exchanger_(heat_exchanger),
        m_htfProps_(m_htfProps),
        flat_plate_htf_(flat_plate_htf),
        timestamp_(timestamp),
        external_conditions_(external_conditions)
    {
        fp_array_is_on_ = true;
    }

    bool fp_array_is_on_;
    double T_f_hx_out_;		// this is used by the outer MEQ
    double T_out_fp_;
    double eff_hx_;
    double q_dot_hx_;
    double dT_hot_;
    double dT_cold_;
    virtual int operator()(double T_in_fp /*C*/, double* diff_T_in_fp /*C*/);

private:
    double T_approach_;
    double mdot_fp_;
    double m_T_loop_in_;
    double T_f_hx_out_target_;
    double m_m_dot_process_heat_;
    FlatPlateArray* flat_plate_array_;
    C_hx_two_tank_tes* heat_exchanger_;
    HTFProperties* m_htfProps_;
    HTFProperties* flat_plate_htf_;
    tm* timestamp_;
    ExternalConditions* external_conditions_;
};

#endif // __FLAT_PLATE_SOLAR_COLLECTOR__
