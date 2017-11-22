/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as “System Advisor Model” or “SAM”. Except
*  to comply with the foregoing, the terms “System Advisor Model”, “SAM”, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

#include "lib_battery.h"
#include "lib_utility_rate.h"

#ifndef __LIB_BATTERY_DISPATCH_H__
#define __LIB_BATTERY_DISPATCH_H__



/*
Dispatch Base Class - can envision many potential modifications. Goal is to define standard API
*/
class dispatch_t
{
public:

	enum FOM_MODES { FOM_LOOK_AHEAD, FOM_LOOK_BEHIND, FOM_FORECAST, FOM_MANUAL };
	enum BTM_MODES { LOOK_AHEAD, LOOK_BEHIND, MAINTAIN_TARGET, MANUAL };
	enum METERING { BEHIND, FRONT };
	enum PV_PRIORITY { MEET_LOAD, CHARGE_BATTERY };
	enum CURRENT_CHOICE { RESTRICT_POWER, RESTRICT_CURRENT, RESTRICT_BOTH };

	dispatch_t(battery_t * Battery,
		double dt,
		double SOC_min,
		double SOC_max,
		int current_choice,
		double Ic_max,
		double Id_max,
		double Pc_max,
		double Pd_max,
		double t_min,
		int dispatch_mode,
		int pv_dispatch);

	// deep copy constructor (new memory), from dispatch to this
	dispatch_t(const dispatch_t& dispatch);

	// copy members from dispatch to this
	virtual void copy(const dispatch_t * dispatch);

	void delete_clone();

	virtual ~dispatch_t();

	// Public APIs
	virtual void dispatch(size_t year,
		size_t hour_of_year,
		size_t step,
		double P_pv_dc_charging,
		double P_pv_dc_discharging,
		double P_load_dc_charging,
		double P_load_dc_discharging,
		double P_pv_dc_clipped=0,
		double P_battery=0) = 0;

	virtual void prepare_dispatch(size_t hour_of_year, size_t step, double P_pv_dc_charging, double P_pv_dc_discharging, double P_load_dc_charging, double P_load_dc_discharging, double P_pv_dc_clipped);

	virtual bool check_constraints(double &I, int count);

	battery_t * battery_model(){ return _Battery; }

	virtual void compute_grid_net();

	virtual double costToCycle(double battCostPerKWH);

	// dc powers
	double power_tofrom_battery();
	double power_tofrom_grid();
	double power_gen();
	double power_pv_to_load();
	double power_battery_to_load();
	double power_grid_to_load();
	double power_pv_to_batt();
	double power_grid_to_batt();
	double power_pv_to_grid();
	double power_battery_to_grid();

	virtual double power_grid_target(){	return 0;}
	virtual double power_batt_target(){ return 0.;}

	// control settings
	int pv_dispatch_priority(){ return _pv_dispatch_to_battery_first; }
	double battery_power_to_fill(){ return _Battery->battery_power_to_fill(); }

	message get_messages();

protected:

	// Initialization help
	void init(battery_t * Battery,
		double dt_hour,
		double SOC_min,
		double SOC_max,
		int current_choice,
		double Ic_max,
		double Id_max,
		double Pc_max,
		double Pd_max,
		double t_min,
		int mode,
		int pv_dispatch);

	// Controllers
	void switch_controller();
	double current_controller(double battery_voltage);
	bool restrict_current(double &I);
	bool restrict_power(double &I);

	// compute totals
	void compute_battery_state();
	void compute_to_batt();
	void compute_to_load();
	void compute_to_grid();
	void compute_generation();

	battery_t * _Battery;
	battery_t * _Battery_initial;

	double _dt_hour;

	/** 
	The dispatch mode. 
	For behind-the-meter dispatch: 0 = LOOK_AHEAD, 1 = LOOK_BEHIND, 2 = MAINTAIN_TARGET, 3 = MANUAL
	For front-of-meter dispatch: 0 = LOOK_AHEAD, 1 = LOOK_BEHIND, 2 = INPUT FORECAST, 3 = MANUAL
	*/
	int _mode; 

	/**
	The priority for how PV power should be directed.
	0 = meet load first, 1 = charge battery first
	*/
	int _pv_dispatch_to_battery_first; 

	// dc power quantities
	double _P_gen;				 // DC
	double _P_tofrom_batt;		 // DC
	double _P_grid;              // DC
	double _P_pv_to_load;		 // DC
	double _P_battery_to_load;   // DC
	double _P_grid_to_load;      // DC
	double _P_pv_to_batt;	     // DC
	double _P_grid_to_batt;      // DC
	double _P_pv_to_grid;		 // DC
	double _P_battery_to_grid;   // DC

	// the actual power inputs chosen based on charging/discharging
	double _P_pv;
	double _P_load;

	// the input options of what the PV and load power is that the battery sees depending on scenario
	double _P_pv_charging;
	double _P_pv_discharging;
	double _P_load_charging;
	double _P_load_discharging;
	double _P_pv_clipping;

	// Charge & current limits controllers
	double _SOC_min;
	double _SOC_max;
	int _current_choice;
	double _Pc_max;
	double _Pd_max;
	double _Ic_max;
	double _Id_max;
	double _t_min;
	double _e_max;
	double _P_target;

	// rapid charge change controller
	int _t_at_mode; // [minutes]
	bool _charging;
	bool _prev_charging;
	bool _grid_recharge;

	// Charging rules
	bool  _can_charge;
	bool  _can_clip_charge;
	bool  _can_discharge;
	bool  _can_grid_charge;

	// messages
	message _message;
};

/*
Manual dispatch class
*/
class dispatch_manual_t : public dispatch_t
{
public:
	dispatch_manual_t(battery_t * Battery,
		double dt_hour,
		double SOC_min,
		double SOC_max,
		int current_choice,
		double Ic_max,
		double Id_max,
		double Pc_max,
		double Pd_max,
		double t_min,
		int mode,
		int pv_dispatch,
		util::matrix_t<size_t> dm_dynamic_sched,
		util::matrix_t<size_t> dm_dynamic_sched_weekend,
		std::vector<bool> can_charge,
		std::vector<bool> can_discharge,
		std::vector<bool> can_gridcharge,
		std::map<size_t, double> dm_percent_discharge,
		std::map<size_t, double> dm_percent_gridcharge);

	// deep copy constructor (new memory), from dispatch to this
	dispatch_manual_t(const dispatch_t& dispatch);

	// copy members from dispatch to this
	virtual void copy(const dispatch_t * dispatch);

	virtual ~dispatch_manual_t(){};

	virtual void dispatch(size_t year,
		size_t hour_of_year,
		size_t step,
		double P_pv_dc_charging,
		double P_pv_dc_discharging,
		double P_load_dc_charging,
		double P_load_dc_discharging,
		double P_pv_dc_clipped = 0,
		double P_battery = 0);

protected:

	// Initialization help
	void init(util::matrix_t<float> dm_dynamic_sched,
		util::matrix_t<float> dm_dynamic_sched_weekend,
		std::map<size_t, double> dm_percent_discharge,
		std::map<size_t, double> dm_percent_gridcharge);

	void init_with_vects(
		util::matrix_t<size_t> dm_dynamic_sched,
		util::matrix_t<size_t> dm_dynamic_sched_weekend,
		std::vector<bool>,
		std::vector<bool>,
		std::vector<bool>,
		std::map<size_t, double> dm_percent_discharge,
		std::map<size_t, double> dm_percent_gridcharge);

	void prepare_dispatch(size_t hour_of_year, size_t step, double P_pv_dc_charging, double P_pv_dc_discharging, double P_load_dc_charging, double P_load_dc_discharging);
	void reset();
	void SOC_controller();
	void compute_energy_load_priority(double energy_needed);
	void compute_energy_battery_priority(double energy_needed);
	bool compute_energy_battery_priority_charging(double energy_needed);
	bool check_constraints(double &I, int count);

	util::matrix_t < size_t > _sched;
	util::matrix_t < size_t > _sched_weekend;

	std::vector<bool> _charge_array;
	std::vector<bool> _discharge_array;
	std::vector<bool> _gridcharge_array;

	double _percent_discharge;
	double _percent_charge;

	std::map<size_t, double>  _percent_discharge_array;
	std::map<size_t, double> _percent_charge_array;
};
/* Manual dispatch for utility scale (front of meter)*/
class dispatch_manual_front_of_meter_t : public dispatch_manual_t
{
public:
	dispatch_manual_front_of_meter_t(battery_t * Battery,
		double dt_hour,
		double SOC_min,
		double SOC_max,
		int current_choice,
		double Ic_max,
		double Id_max,
		double Pc_max,
		double Pd_max,
		double t_min,
		int mode,
		int pv_dispatch,
		util::matrix_t<size_t> dm_dynamic_sched,
		util::matrix_t<size_t> dm_dynamic_sched_weekend,
		std::vector<bool> can_charge,
		std::vector<bool> can_discharge,
		std::vector<bool> can_gridcharge,
		std::map<size_t, double> dm_percent_discharge,
		std::map<size_t, double> dm_percent_gridcharge);

	~dispatch_manual_front_of_meter_t(){};

	// deep copy constructor (new memory), from dispatch to this
	dispatch_manual_front_of_meter_t(const dispatch_t & dispatch);

	// copy members from dispatch to this
	virtual void copy(const dispatch_t * dispatch);


	virtual void dispatch(size_t year,
		size_t hour_of_year,
		size_t step,
		double P_pv_dc_charging,
		double P_pv_dc_discharging,
		double P_load_dc_charging,
		double P_load_dc_discharging,
		double P_pv_dc_clipped = 0,
		double P_battery = 0);

	void compute_grid_net();

protected:
	void compute_energy_no_load(double energy_needed);
	void compute_to_grid();
};


/*! Class containing calculated grid power at a single time step */
class grid_point
{
	/**
	Class for behind-the-meter dispatch which encapsulates the required grid power, hour, and step:
	grid_point = [grid_power, hour, step]
	*/
public:
	grid_point(double grid = 0., int hour = 0, int step = 0) :
		_grid(grid), _hour(hour), _step(step){}
	double Grid() const { return _grid; }
	int Hour() const { return _hour; }
	int Step() const { return _step; }

private:
	double _grid;
	int _hour;
	int _step;
};

struct byGrid
{
	bool operator()(grid_point const  &a, grid_point const &b)
	{
		return a.Grid() > b.Grid();
	}
};
typedef std::vector<grid_point> grid_vec;

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
		double Pc_max,
		double Pd_max,
		double t_min,
		int dispatch_mode,
		int pv_dispatch,
		size_t nyears,
		size_t look_ahead_hours,
		size_t dispatch_update_frequency_hours,
		bool can_charge,
		bool can_clipcharge,
		bool can_grid_charge
		);

	virtual ~dispatch_automatic_t(){};

	// deep copy constructor (new memory), from dispatch to this
	dispatch_automatic_t(const dispatch_t& dispatch);

	// copy members from dispatch to this
	virtual void copy(const dispatch_t * dispatch);

	virtual void dispatch(size_t year,
		size_t hour_of_year,
		size_t step,
		double P_pv_dc_charging,
		double P_pv_dc_discharging,
		double P_load_dc_charging,
		double P_load_dc_discharging,
		double P_pv_dc_clipped,
		double P_battery);

	/*! Compute the updated power to send to the battery over the next N hours */
	virtual void update_dispatch(size_t hour_of_year, size_t step, size_t idx)=0;

	/*! Pass in the PV power forecast */
	virtual void update_pv_data(std::vector<double> P_pv_dc);

protected:

	/*! Initialize with a pointer*/
	void init_with_pointer(const dispatch_automatic_t * tmp);

	/*! Return the dispatch mode */
	int get_mode();

	/*! Full time-series of PV production [kW] */
	double_vec _P_pv_dc;		
	
	/*! The index of the current day (hour * steps_per_hour + step) */
	size_t _day_index;				

	/*! The index of the current month (0-11) */
	size_t _month;

	/*! The number of steps in the look ahead, assumed to be 24 hours * steps_per_hour */
	size_t _num_steps;

	/*! Time series of length (24 hours * steps_per_hour) of batter powers [kW] */
	double_vec _P_battery_use;

	/*! The battery power target at the current time [kW] */
	double _P_battery_current;

	size_t _hour_last_updated;
	double _dt_hour;
	size_t _steps_per_hour;
	size_t _nyears;
	int _mode;
	double _safety_factor;

	/*! The hours to look ahead in the simulation [hour] */
	size_t _look_ahead_hours;

	/*! The frequency to update the dispatch [hour] */
	size_t _dispatch_update_hours;
};

/*! Automated dispatch class for behind-the-meter connections */
class dispatch_automatic_behind_the_meter_t : public dispatch_automatic_t
{
	/**
	Class contains methods and data required to automate dispatch for a behind-the-meter battery targeting peak-shaving applications.
	This includes:
		1. Methods to set or compute the grid power target (desired grid power at every step over the next 24 hours)
		2. Methods to program the dispatch to acheive the target
		3. Method to update the electric load forecast
	*/
public:
	dispatch_automatic_behind_the_meter_t(
		battery_t * Battery,
		double dt,
		double SOC_min,
		double SOC_max,
		int current_choice,
		double Ic_max,
		double Id_max,
		double Pc_max,
		double Pd_max,
		double t_min,
		int dispatch_mode,
		int pv_dispatch,
		size_t nyears,
		size_t look_ahead_hours,
		size_t dispatch_update_frequency_hours,
		bool can_charge,
		bool can_clipcharge,
		bool can_grid_charge
		);

	virtual ~dispatch_automatic_behind_the_meter_t(){};

	// deep copy constructor (new memory), from dispatch to this
	dispatch_automatic_behind_the_meter_t(const dispatch_t& dispatch);

	// copy members from dispatch to this
	virtual void copy(const dispatch_t * dispatch);

	virtual void dispatch(size_t year,
		size_t hour_of_year,
		size_t step,
		double P_pv_dc_charging,
		double P_pv_dc_discharging,
		double P_load_dc_charging,
		double P_load_dc_discharging,
		double P_pv_dc_clipped,
		double P_battery);

	/*! Compute the updated power to send to the battery over the next N hours */
	void update_dispatch(size_t hour_of_year, size_t step, size_t idx);

	/*! Pass in the load forecast */
	void update_load_data(std::vector<double> P_load_dc);

	void set_target_power(std::vector<double> P_target);
	double power_grid_target(){ return _P_target_current; };
	double power_batt_target(){ return _P_battery_current; };


protected:
	
	/*! Initialize with a pointer*/
	void init_with_pointer(const dispatch_automatic_behind_the_meter_t * tmp);

	void initialize(size_t hour_of_year);
	void check_debug(FILE *&p, bool & debug, size_t hour_of_year, size_t idx);
	void sort_grid(FILE *p, bool debug, size_t idx);
	void compute_energy(FILE *p, bool debug, double & E_max);
	void target_power(FILE*p, bool debug, double E_max, size_t idx);
	void set_battery_power(FILE *p, bool debug);
	void check_new_month(size_t hour_of_year, size_t step);

	/*! Full time-series of loads [kW] */
	double_vec _P_load_dc;

	/*! Full time-series of target power [kW] */
	double_vec _P_target_input; 

	/*! Time series of length (24 hours * steps_per_hour) of target powers [kW] */
	double_vec _P_target_use;

	/*! The target grid power for the month [kW] */
	double _P_target_month; 

	/*! The grid power target at the current time [kW] */
	double _P_target_current;

	/* Vector of length (24 hours * steps_per_hour) containing grid calculation [P_grid, hour, step] */
	grid_vec grid; 

	/* Vector of length (24 hours * steps_per_hour) containing sorted grid calculation [P_grid, hour, step] */
	grid_vec sorted_grid;
};

/*! Automated Front of Meter DC-connected battery dispatch */
class dispatch_automatic_front_of_meter_t : public dispatch_automatic_t
{
public:
	/**
	 Class takes forecast information about the PV production and Load Profile, plus PPA sell rate and electricity buy-rate signals
	 and programs battery to strategically dispatch to maximize economic benefit by:
	 1. Discharging during times of high PPA sell rates
	 2. Charging from the grid during times of low electricity buy-rates (if grid charging allowed)
	 3. Charging from the PV array during times of low PPA sell rates
	 4. Charging from the PV array during times where the PV power would be clipped due to inverter limits
	*/
	dispatch_automatic_front_of_meter_t(
		battery_t * Battery,
		double dt,
		double SOC_min,
		double SOC_max,
		int current_choice,
		double Ic_max,
		double Id_max,
		double Pc_max,
		double Pd_max,
		double t_min,
		int dispatch_mode,
		int pv_dispatch,
		size_t nyears,
		size_t look_ahead_hours,
		size_t dispatch_update_frequency_hours,
		bool can_charge,
		bool can_clipcharge,
		bool can_grid_charge,
		double inverter_paco,
		double battCostPerkWh,
		std::vector<double> ppa_factors,
		util::matrix_t<size_t> ppa_weekday_schedule,
		util::matrix_t<size_t> ppa_weekend_schedule,
		UtilityRate * utilityRate
		);

	virtual ~dispatch_automatic_front_of_meter_t();

	/*! deep copy constructor (new memory), from dispatch to this */
	dispatch_automatic_front_of_meter_t(const dispatch_t& dispatch);

	/*! shallow copy from dispatch to this */
	virtual void copy(const dispatch_t* dispatch);

	/*! Dispatch the battery */
	void dispatch(size_t year,
		size_t hour_of_year,
		size_t step,
		double P_pv_dc_charging,
		double P_pv_dc_discharging,
		double P_load_dc_charging,
		double P_load_dc_discharging,
		double P_pv_dc_clipped,
		double P_battery);

	/*! Compute the updated power to send to the battery over the next N hours */
	void update_dispatch(size_t hour_of_year, size_t step, size_t idx);

protected:
	
	void init_with_pointer(const dispatch_automatic_front_of_meter_t* tmp);
	void setup_cost_vector(util::matrix_t<size_t> ppa_weekday_schedule, util::matrix_t<size_t> ppa_weekend_schedule);

	double _inverter_paco;
	std::vector<double> _ppa_factors;
	std::vector<double> _ppa_cost_vector;
	UtilityRateCalculator * _utilityRateCalculator;
	double m_battCostPerKWH;
	double m_cycleCost;
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
	battery_metrics_t(battery_t * Battery, double dt_hour);
	~battery_metrics_t(){};

	void compute_metrics_ac(double P_tofrom_batt, double P_system_loss, double P_pv_to_batt, double P_grid_to_batt, double P_tofrom_grid);
	void compute_metrics_dc(dispatch_t * dispatch);
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

	// annual metrics
	double _e_charge_from_pv_annual;   // [Kwh]
	double _e_charge_from_grid_annual; // [Kwh]
	double _e_loss_system_annual;	   // [Kwh]
	double _e_charge_annual;		   // [Kwh]
	double _e_discharge_annual;		   // [Kwh]
	double _e_grid_import_annual;	   // [Kwh]
	double _e_grid_export_annual;	   // [Kwh]
	double _e_loss_annual;			   // [kWh]

	battery_t * _Battery;
	double _dt_hour;
};

#endif
