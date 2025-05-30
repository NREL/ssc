/*
BSD 3-Clause License

Copyright (c) Alliance for Sustainable Energy, LLC. See also https://github.com/NREL/ssc/blob/develop/LICENSE
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef __LIB_BATTERY_DISPATCH_H__
#define __LIB_BATTERY_DISPATCH_H__

#include <memory>

#include "lib_battery.h"

// Forward declarations to speed up build
struct BatteryPower;
class BatteryPowerFlow;
class UtilityRate;
class UtilityRateCalculator;
class outage_manager;

namespace battery_dispatch
{
	const size_t constraintCount = 10;
}

/*
Dispatch Base Class - can envision many potential modifications. Goal is to define standard API
*/
class dispatch_t
{
public:

	enum FOM_MODES { FOM_AUTOMATED_ECONOMIC, FOM_PV_SMOOTHING, FOM_CUSTOM_DISPATCH, FOM_MANUAL };
	enum BTM_MODES { PEAK_SHAVING, MAINTAIN_TARGET, CUSTOM_DISPATCH, MANUAL, RETAIL_RATE, SELF_CONSUMPTION };
	enum METERING { BEHIND, FRONT };
    enum WEATHER_FORECAST_CHOICE { WF_LOOK_AHEAD, WF_LOOK_BEHIND, WF_CUSTOM };
    enum LOAD_FORECAST_CHOICE { LOAD_LOOK_AHEAD, LOAD_LOOK_BEHIND, LOAD_CUSTOM };
	enum PV_PRIORITY { MEET_LOAD, CHARGE_BATTERY };
	enum CURRENT_CHOICE { RESTRICT_POWER, RESTRICT_CURRENT, RESTRICT_BOTH };
	enum CYCLE_COST {MODEL_CYCLE_COST, INPUT_CYCLE_COST};
	enum CONNECTION { DC_CONNECTED, AC_CONNECTED };
    enum CAPACITY_FORECAST_TYPE { PRICE_ONLY, CAPACITY_ONLY, PRICE_AND_CAPACITY };

	dispatch_t(battery_t * Battery,
		double dt,
		double SOC_min,
		double SOC_max,
		int current_choice,
		double Ic_max,
		double Id_max,
		double Pc_max_kwdc,
		double Pd_max_kwdc,
		double Pc_max_kwac,
		double Pd_max_kwac,
		double t_min,
		int dispatch_mode,
		int meter_position,
        double interconnection_limit,
        bool chargeOnlySystemExceedLoad = true, // Optional so FOM doesn't have to specify them
        bool dischargeOnlyLoadExceedSystem = true,
        double SOC_min_outage = 0.0);

	// deep copy constructor (new memory), from dispatch to this
	dispatch_t(const dispatch_t& dispatch);

	// copy members from dispatch to this
	virtual void copy(const dispatch_t * dispatch);

	void delete_clone();

	virtual ~dispatch_t();

	/// Public API to run the battery dispatch model for the current timestep, given the system power, and optionally the electric load and amount of system clipping
	virtual void dispatch(size_t year,
		size_t hour_of_year,
		size_t step) = 0;

	/// Method to check any operational constraints and modify the battery current if needed
	virtual bool check_constraints(double &I, size_t count);

	/// Method to recalculate the battery state based upon the final constrained current
	virtual void finalize(size_t index, double &I);

	battery_t * battery_model(){ return _Battery; }

	// ac outputs
	double power_tofrom_battery_ac();
    double power_tofrom_battery_dc();
	double power_tofrom_grid();
	double power_gen();
	double power_pv_to_load();
	double power_battery_to_load();
	double power_grid_to_load();
	double power_fuelcell_to_load();
	double power_pv_to_batt_ac();
	double power_pv_to_batt_dc();
	double power_grid_to_batt();
	double power_fuelcell_to_batt();
	double power_pv_to_grid();
	double power_battery_to_grid();
    double power_battery_to_system_load();
    double power_battery_to_inverter_dc();
	double power_fuelcell_to_grid();
	double power_conversion_loss();
	double power_system_loss();
    double power_interconnection_loss();
    double power_crit_load_unmet();
    double power_crit_load();
    double power_losses_unmet();

	virtual double power_grid_target(){	return 0;}
	virtual double power_batt_target(){ return 0.;}
	virtual double cost_to_cycle() { return 0.;}
    virtual double cost_to_cycle_per_kwh() { return 0.; }

    virtual size_t get_dispatch_period() { return 0; }

	// control settings
	double battery_power_to_fill();

	// test data
	double battery_soc();

	/// Return a pointer to the underlying calculated power quantities
	BatteryPower * getBatteryPower();

	/// Return a pointer to the object which calculates the battery power flow
	BatteryPowerFlow * getBatteryPowerFlow();

    double _min_outage_soc;

protected:

	/// Helper function to run common dispatch tasks.  Requires that m_batteryPower->powerBattery is previously defined
	virtual void runDispatch(size_t lifetimeIndex);

	// Initialization help
	void init(battery_t * Battery,
		double dt_hour,
		int current_choice,
		double t_min,
		int mode);

	// Controllers
	virtual	void SOC_controller();
	void switch_controller();
	double current_controller(double power_kw);
	bool restrict_current(double &I);
	bool restrict_power(double &I);

    void run_outage_step(size_t lifetimeIndex);
    void dispatch_dc_outage_step(size_t lifetimeIndex);
    void dispatch_ac_outage_step(size_t lifetimeIndex);

	battery_t * _Battery;
	battery_t * _Battery_initial;

	double _dt_hour;

	/**
	The dispatch mode.
	For behind-the-meter dispatch: 0 = PEAK_SHAVING, 1 = MAINTAIN_TARGET, 2 = CUSTOM, 3 = MANUAL, 4 = FORECAST, 5 = SELF_CONSUMPTION
	For front-of-meter dispatch: 0 = FOM_AUTOMATED_ECONOMIC, 1 = FOM_PV_SMOOTHING, 2 = FOM_CUSTOM_DISPATCH, 3 = FOM_MANUAL
	*/
	int _mode;

	// allocated and managed internally
	std::unique_ptr<BatteryPowerFlow> m_batteryPowerFlow;

	// managed by BatteryPowerFlow
	BatteryPower * m_batteryPower;

    std::unique_ptr<outage_manager> m_outage_manager;

	// Charge & current limits controllers
	int _current_choice;
	double _t_min;
	double _e_max;
	double _P_target;

	// rapid charge change controller
	int _t_at_mode; // [minutes]
	bool _charging;
	bool _prev_charging;
	bool _grid_recharge;
};

/*! Class responsible for changing and storing outage related variables for when the grid goes out or comes back */
class outage_manager
{
public:
    outage_manager(BatteryPower* batteryPower, battery_t* battery);

    ~outage_manager();

    // Use this to copy member variables other than the m_batteryPower pointer. Use the pointer created by dispatch_t's copy constructor.
    void copy(const outage_manager& tmp);

    void update(bool isAutomated, double min_outage_soc);

    void startOutage(double min_outage_soc);

    void endOutage(bool isAutomated);

    bool recover_from_outage; // Tells the dispatch algorithms to re-plan given outage recovery

private:
    // Managed by dispatch_t::m_batteryPowerFlow
    BatteryPower* m_batteryPower;
    battery_t* _Battery; // Managed by dispatch_t

    bool canSystemChargeWhenGrid;	///< A boolean specifying whether the battery is allowed to charge from PV when the grid is on
    bool canClipChargeWhenGrid;	///< A boolean specifying whether the battery is allowed to charge from otherwise clipped PV when the grid is on
    bool canGridChargeWhenGrid; ///< A boolean specifying whether the battery is allowed to charge from the Grid when the grid is on
    bool canDischargeWhenGrid;  ///< A boolean specifying whether the battery is allowed to discharge when the grid is on

    double stateOfChargeMaxWhenGrid;   ///< The maximum state of charge when the grid is on (0-100)
    double stateOfChargeMinWhenGrid;   ///< The minimum state of charge when the grid is on (0-100)

    bool last_step_was_outage; // Used by outage manager to determine when to call endOutage
};

/*! Class containing calculated grid power at a single time step */
class grid_point
{
    /**
    Class for behind-the-meter dispatch which encapsulates the required grid power, cost, hour, and step:
    grid_point = [grid_power, hour, step, cost, marginal_cost]
    */
public:
    grid_point(double grid = 0., size_t hour = 0, size_t step = 0, double cost = 0., double marginal_cost = 0., double export_price = 0., double export_per_kwh = 0.) :
        _grid(grid), _hour(hour), _step(step), _cost(cost), _marginal_cost(marginal_cost), _export_price(export_price), _export_per_kwh(export_per_kwh) {}
    double Grid() const { return _grid; }
    size_t Hour() const { return _hour; } 
    size_t Step() const { return _step; } 
    double Cost() const { return _cost; }
    double MarginalCost() const { return _marginal_cost; }
    double ExportPrice() const { return _export_price; }
    double ExportPerKWh() const { return _export_per_kwh;  }

private:
	double _grid; // Power in kW, + is net load, - is net generation
	size_t _hour; // Hours from time of forecast
	size_t _step; // Steps from time of forecast
    double _cost; // Total $ add to or subtracted from the utility bill for this step, given _grid
    double _marginal_cost; // $ for 1 kW of load in this step
    double _export_price; // $ paid by utility to discharge battery at full power
    double _export_per_kwh; // $/kWh from grid export in this step
};

struct byGrid
{
    bool operator()(grid_point const& a, grid_point const& b);
};
typedef std::vector<grid_point> grid_vec;

struct byCost
{
    bool operator() (grid_point const& a, grid_point const& b);
};

// Sorts low to high
struct byLowestMarginalCost
{
    bool operator() (grid_point const& a, grid_point const& b);
};

struct byExportPerKWh
{
    bool operator() (grid_point const& a, grid_point const& b);
};

/*! Automated dispatch base class */
class dispatch_automatic_t : public dispatch_t
{
	/**
	Class contains methods and data common to all automated dispatch strategies in SAM.
	This includes:
		1. Dispatch method which discharges battery based on a power setpoint for each time step rather than energy amount used in manual mode
		2. Initialization of PV power forecast used for automation
	*/
public:
	dispatch_automatic_t(
		battery_t * Battery,
		double dt,
		double SOC_min,
		double SOC_max,
		int current_choice,
		double Ic_max,
		double Id_max,
		double Pc_max_kwdc,
		double Pd_max_kwdc,
		double Pc_max_kwac,
		double Pd_max_kwac,
		double t_min,
		int dispatch_mode,
        int weather_forecast_mode,
		int pv_dispatch,
		size_t nyears,
		size_t look_ahead_hours,
		double dispatch_update_frequency_hours,
		bool can_charge,
		bool can_clipcharge,
		bool can_grid_charge,
		bool can_fuelcell_charge,
        bool can_curtail_charge,
        std::vector<double> battReplacementCostPerkWh,
        int battCycleCostChoice,
        std::vector<double> battCycleCost,
        std::vector<double> battOMCost, // required for base class
        double interconnection_limit,
        bool chargeOnlySystemExceedLoad = true,  // Optional so FOM doesn't have to specify them
        bool dischargeOnlyLoadExceedSystem = true,
        bool behindTheMeterDischargeToGrid = true,
        double SOC_min_outage = 0.0
		);

	virtual ~dispatch_automatic_t(){};

	// deep copy constructor (new memory), from dispatch to this
	dispatch_automatic_t(const dispatch_t& dispatch);

	// copy members from dispatch to this
	virtual void copy(const dispatch_t * dispatch);

	/// Public API to run the battery dispatch model for the current timestep, given the system power, and optionally the electric load, amount of system clipping, or specified battery power
	virtual void dispatch(size_t year,
		size_t hour_of_year,
		size_t step);

	/*! Compute the updated power to send to the battery over the next N hours */
	virtual void update_dispatch(size_t year, size_t hour_of_year, size_t step, size_t idx)=0;

	/*! Pass in the PV power forecast */
	virtual void update_pv_data(std::vector<double> P_pv_ac);

    /// Update cliploss data [kW]
    void update_cliploss_data(double_vec P_cliploss);

	/*! Pass in the user-defined dispatch power vector */
	virtual void set_custom_dispatch(std::vector<double> P_batt_dc);

	/* Check constraints and re-dispatch if needed */
	virtual bool check_constraints(double &I, size_t count);

	/// Return the battery power target set by the controller
	double power_batt_target();

    /*! Return the calculated cost to cycle ($/cycle-kWh for all dispatch)*/
    double cost_to_cycle_per_kwh() { return cost_to_cycle(); }

protected:

	/*! Initialize with a pointer*/
	void init_with_pointer(const dispatch_automatic_t * tmp);

	/*! Return the dispatch mode */
	int get_mode();

    /*! Return the calculated cost to cycle ($/cycle for behind the meter, $/cycle-kWh for front of the meter) - used internally*/
    double cost_to_cycle() { return m_cycleCost; }

	/*! Full time-series of PV production [kW] */
	double_vec _P_pv_ac;

    /*! Full clipping loss due to AC power limits vector [kW] */
    double_vec _P_cliploss_dc;

	/*! The index of the current day (hour * steps_per_hour + step) */
	size_t _day_index;

	/*! The index of the current month (0-11) */
	size_t _month;

	/*! The number of steps in the look ahead, assumed to be 24 hours * steps_per_hour */
	size_t _num_steps;

	/*! Time series of length (24 hours * steps_per_hour) of battery powers [kW] */
	double_vec _P_battery_use;

	/*! The index of year the dispatch was last updated */
	size_t _hour_last_updated;

	/*! The timestep in hours (hourly = 1, half_hourly = 0.5, etc) */
	double _dt_hour;

	/*! The frequency to update the dispatch [hour] */
	double _dt_hour_update;

	/*! The number of steps per hour*/
	size_t _steps_per_hour;

	/*! The number of years in the simulation */
	size_t _nyears;

    /*! The current year of the simulation */
    size_t curr_year;

	/*! The dispatch mode, described by dispatch_t::BTM_MODES or dispatch_t::FOM_MODES*/
	int _mode;

    /*! What weather forecast to use, described by dispatch_t::WEATHER_FORECAST_CHOICE*/
    int _weather_forecast_mode;

	/*! An internal factor to describe how conservative the peak shaving algorithm should be */
	double _safety_factor;

	/*! The hours to look ahead in the simulation [hour] */
	size_t _forecast_hours;

    /*! Cost to replace battery per kWh */
    std::vector<double> m_battReplacementCostPerKWH;

    /*! Cycling cost inputs */
    int m_battCycleCostChoice;
    std::vector<double> cycle_costs_by_year;
    double m_cycleCost; // $/cycle for behind the meter, $/cycle-kWh for front of the meter

    /*! O and M cost inputs */
    std::vector<double> om_costs_by_year; //(convert input to $/kWh per year)
    double m_omCost; // $/kWh (input is $/MWh) for update_dispatch use

};

/*! Battery metrics class */
class battery_metrics_t
{
	/**
	Class which computes ac or dc energy metrics such as:
	1. Annual energy sent to charge battery or discharged from battery
	2. Annual energy charged from the grid or PV
	3. Annual energy imported or exported to the grid annually
	4. Average roundtrip and conversion efficiency
	5. Percentage of energy charged from PV
	*/
public:
	battery_metrics_t(double dt_hour);
	~battery_metrics_t(){};

	void compute_metrics_ac(const BatteryPower * batteryPower);
	//void compute_metrics_dc(const BatteryPower * batteryPower);
	void compute_annual_loss();

	void accumulate_energy_charge(double P_tofrom_batt);
	void accumulate_energy_discharge(double P_tofrom_batt);
	void accumulate_energy_system_loss(double P_system_loss);
	void accumulate_battery_charge_components(double P_tofrom_batt, double P_pv_to_batt, double P_grid_to_batt);
	void accumulate_grid_annual(double P_tofrom_grid);
	void new_year();


	// outputs
	double energy_pv_charge_annual();
	double energy_grid_charge_annual();
	double energy_charge_annual();
	double energy_discharge_annual();
	double energy_grid_import_annual();
	double energy_grid_export_annual();
	double energy_system_loss_annual();
	double energy_loss_annual();
	double average_battery_conversion_efficiency();
	double average_battery_roundtrip_efficiency();
	double pv_charge_percent();
    double grid_charge_percent();

protected:

	// single value metrics
	double _e_charge_accumulated;	 // [Kwh]
	double _e_discharge_accumulated; // [Kwh]
	double _e_charge_from_pv;		 // [Kwh]
	double _e_charge_from_grid;		 // [Kwh]
	double _e_loss_system;			 // [Kwh]

	/*! Efficiency includes the battery internal efficiency and conversion efficiencies [%] */
	double _average_efficiency;

	/*! Efficiency includes auxilliary system losses [%] */
	double _average_roundtrip_efficiency;

	/*! This is the percentage of energy charge from the PV system [%] */
	double _pv_charge_percent;
    double _grid_charge_percent;

	// annual metrics
	double _e_charge_from_pv_annual;   // [Kwh]
	double _e_charge_from_grid_annual; // [Kwh]
	double _e_loss_system_annual;	   // [Kwh]
	double _e_charge_annual;		   // [Kwh]
	double _e_discharge_annual;		   // [Kwh]
	double _e_grid_import_annual;	   // [Kwh]
	double _e_grid_export_annual;	   // [Kwh]
	double _e_loss_annual;			   // [kWh]

	double _dt_hour;
};

#endif
