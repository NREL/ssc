#ifndef battery_h
#define battery_h

#include "lib_util.h"
#include "lsqfit.h"

#include <vector>
#include <map>
#include <string>
#include <stdio.h>
#include <algorithm>

const double tolerance = 0.001;

typedef std::vector<double> double_vec;
typedef std::vector<int> int_vec;

// Messages
class message
{
public:
	message(){};
	virtual ~message(){};


	void add(std::string message);
	int total_message_count();
	size_t message_count(int index);
	std::string get_message(int index);
	std::string construct_log_count_string(int index);

protected:
	std::vector<std::string> messages;
	std::vector<int> count;
};

/*
Base class from which capacity models derive
Note, all capacity models are based on the capacity of one battery
*/
class voltage_t;
class capacity_t
{
public:
	capacity_t(double q, double SOC_max);
	virtual capacity_t * clone() = 0;
	virtual void copy(capacity_t *&);
	virtual ~capacity_t(){};
	
	// pure virtual functions (abstract) which need to be defined in derived classes
	virtual void updateCapacity(double I, double dt) = 0;
	virtual void updateCapacityForThermal(double capacity_percent)=0;
	virtual void updateCapacityForLifetime(double capacity_percent)=0;
	virtual void replace_battery()=0;

	virtual double q1() = 0; // available charge
	virtual double q10() = 0; // capacity at 10 hour discharge rate

	void check_charge_change(); 
	void update_SOC();

	// common outputs
	double SOC();
	double DOD();
	double prev_DOD();
	double q0();
	double qmax(); 
	double I();
	bool chargeChanged();
	double I_loss();

protected:
	double _q0;  // [Ah] - Total capacity at timestep 
	double _qmax; // [Ah] - maximum possible capacity
	double _qmax0; // [Ah] - original maximum capacity
	double _I;   // [A]  - Current draw during last step
	double _I_loss; // [A] - Lifetime and thermal losses
	double _SOC; // [%] - State of Charge
	double _SOC_max; // [%] - Maximum SOC
	double _DOD; // [%] - Depth of Discharge
	double _DOD_prev; // [%] - Depth of Discharge of previous step
	double _dt_hour; // [hr] - Timestep in hours
	bool _chargeChange; // [true/false] - indicates if charging state has changed since last step
	int _prev_charge; // {CHARGE, NO_CHARGE, DISCHARGE}

	enum {CHARGE, NO_CHARGE, DISCHARGE};
};

/*
KiBaM specific capacity model
*/
class capacity_kibam_t : public capacity_t
{
public:

	// Public APIs 
	capacity_kibam_t(double q20, double t1, double q1, double q10, double SOC_max);
	~capacity_kibam_t(){}
	capacity_kibam_t * clone();
	void copy(capacity_kibam_t *&);

	void updateCapacity(double I, double dt);
	void updateCapacityForThermal(double capacity_percent);
	void updateCapacityForLifetime(double capacity_percent);
	void replace_battery();
	double q1(); // Available charge
	double q2(); // Bound charge
	double q10(); // Capacity at 10 hour discharge rate
	double q20(); // Capacity at 20 hour discharge rate

protected:
	// unique to kibam
	double c_compute(double F, double t1, double t2, double k_guess);
	double q1_compute(double q10, double q0, double dt, double I); // may remove some inputs, use class variables
	double q2_compute(double q20, double q0, double dt, double I); // may remove some inputs, use class variables
	double Icmax_compute(double q10, double q0, double dt);
	double Idmax_compute(double q10, double q0, double dt);
	double qmax_compute();
	double qmax_of_i_compute(double T);
	void parameter_compute();
	
	// parameters for finding c, k, qmax
	double _t1;  // [h] - discharge rate for capacity at _q1
	double _t2;  // [h] - discharge rate for capacity at _q2
	double _q1;  // [Ah]- capacity at discharge rate t1
	double _q2;  // [Ah] - capacity at discharge rate t2
	double _F1;  // [unitless] - internal ratio computation
	double _F2;  // [unitless] - internal ratio computation

	// model parameters
	double _c;  // [0-1] - capacity fraction
	double _k;  // [1/hour] - rate constant

	// charge which changes with time
	double _q1_0; // [Ah] - charge available
	double _q2_0; // [Ah] - charge bound
	double _q10; //  [Ah] - Capacity at 10 hour discharge rate
	double _q20; // [Ah] - Capacity at 20 hour discharge rate
	double _I20; // [A]  - Current at 20 hour discharge rate
};

/*
Lithium Ion specific capacity model
*/
class capacity_lithium_ion_t : public capacity_t
{
public:
	capacity_lithium_ion_t(double q, double SOC_max);
	~capacity_lithium_ion_t(){};
	capacity_lithium_ion_t * clone();
	void copy(capacity_lithium_ion_t *&);

	// override public api
	void updateCapacity(double I, double dt);
	void updateCapacityForThermal(double capacity_percent);
	void updateCapacityForLifetime(double capacity_percent);
	void replace_battery();

	double q1(); // Available charge
	double q10(); // Capacity at 10 hour discharge rate

protected:
};


/*
Voltage Base class.  
All voltage models are based on one-cell, but return the voltage for one battery
*/
class thermal_t;
class voltage_t
{
public:
	voltage_t(int num_cells_series, int num_strings, double voltage);
	virtual voltage_t * clone()=0;
	void copy(voltage_t *&);
	virtual ~voltage_t(){};

	virtual void updateVoltage(capacity_t * capacity, thermal_t * thermal, double dt)=0;
	virtual double battery_voltage(); // voltage of one battery
	double cell_voltage(); // voltage of one cell
	double R(); // computed resistance

protected:
	int _num_cells_series;    // number of cells in series
	int _num_strings;  // addition number in parallel
	double _cell_voltage; // closed circuit voltage per cell [V]
	double _R;

};

// Shepard + Tremblay Model
class voltage_dynamic_t : public voltage_t
{
public:
	voltage_dynamic_t(int num_cells_series, int num_strings, double voltage, double Vfull, double Vexp, double Vnom, double Qfull, double Qexp, double Qnom, double C_rate, double R);
	voltage_dynamic_t * clone();
	void copy(voltage_dynamic_t *&);

	void parameter_compute();
	void updateVoltage(capacity_t * capacity, thermal_t * thermal, double dt);

protected:
	double voltage_model_tremblay_hybrid(double capacity, double current, double q0);

private:
	double _Vfull;
	double _Vexp;
	double _Vnom;
	double _Qfull;
	double _Qexp;
	double _Qnom;
	double _C_rate;
	double _A;
	double _B;
	double _E0;
	double _K;

};

// D'Agostino Vanadium Redox Flow Model
class voltage_vanadium_redox_t : public voltage_t
{
public:
	voltage_vanadium_redox_t(int num_cells_series, int num_strings, double V_ref_50, double R);
	voltage_vanadium_redox_t * clone();
	void copy(voltage_vanadium_redox_t *&);

	void updateVoltage(capacity_t * capacity, thermal_t * thermal, double dt);
	//double battery_voltage();

protected:
	double voltage_model(double q0, double qmax, double T);

private:
	double _V_ref_50;				// Reference voltage at 50% SOC
	double _R;						// Internal resistance [Ohm]
	double _I;						// Current level [A]
	double _R_molar;
	double _F;
	double _C;
};

/*
Lifetime class.  Currently only one lifetime model anticipated
*/

class lifetime_t
{

public:
	lifetime_t(const util::matrix_t<double> &cyles_vs_DOD, const int replacement_option, const double replacement_capacity  );
	~lifetime_t();
	lifetime_t * clone();
	void copy(lifetime_t *&);

	void rainflow(double DOD);
	bool check_replaced();
	void reset_replacements();

	int replacements();
	int cycles_elapsed();
	double capacity_percent();
	int forty_percent_cycles();
	int hundred_percent_cycles();
	double cycle_range();

	// for user replacement schedule
	void force_replacement();

protected:
	void rainflow_ranges();
	void rainflow_ranges_circular(int index);
	int rainflow_compareRanges();
	double bilinear(double DOD, int cycle_number);

	util::matrix_t<double> _cycles_vs_DOD;
	util::matrix_t<double> _batt_lifetime_matrix;
	std::vector<double> _DOD_vect;
	std::vector<double> _cycles_vect;
	std::vector<double> _capacities_vect;


	int _nCycles;
	double _Dlt;			// % damage according to rainflow
	double _Clt;			// % capacity 
	double _jlt;			// last index in Peaks, i.e, if Peaks = [0,1], then _jlt = 1
	double _Xlt;
	double _Ylt;
	std::vector<double> _Peaks;
	double _Range;
	double _average_range;

	// battery replacement
	int _replacement_option;
	double _replacement_capacity;
	int _replacements;
	bool _replacement_scheduled;

	enum RETURN_CODES
	{
		LT_SUCCESS,
		LT_GET_DATA,
		LT_RERANGE
	};
};

/*
Thermal classes
*/
class thermal_t
{
public:
	thermal_t(double mass, double length, double width, double height,
		double Cp, double h, double T_room,
		const util::matrix_t<double> &cap_vs_temp);
	thermal_t * clone();
	void copy(thermal_t *&);

	void updateTemperature(double I, double R, double dt);
	void replace_battery();

	// outputs
	double T_battery();
	double capacity_percent();
	message get_messages(){ return _message; }

protected:
	double f(double T_battery, double I);
	double rk4(double I, double dt);
	double trapezoidal(double I, double dt);
	double implicit_euler(double I, double dt);

protected:

	util::matrix_t<double> _cap_vs_temp;

	double _mass;		// [kg]
	double _length;		// [m]
	double _width;		// [m]
	double _height;		// [m]
	double _Cp;			// [J/KgK] - battery specific heat capacity
	double _h;			// [Wm2K] - general heat transfer coefficient
	double _T_room;		// [K] - storage room temperature
	double _R;			// [Ohm] - internal resistance
	double _A;			// [m2] - exposed surface area
	double _T_battery;   // [K]
	double _capacity_percent; //[%]
	message _message;
	double _T_max;    //(mw) can't initialize here - breaks on gcc 
};
/*
Losses Base class
*/
class losses_t
{
public:
	losses_t(lifetime_t *, thermal_t *, capacity_t*);
	losses_t * clone();
	void copy(losses_t *&);

	void run_losses(double dt_hour);
	void replace_battery();

protected:
	lifetime_t * _lifetime;
	thermal_t * _thermal;
	capacity_t * _capacity;
	int _nCycle;
};

/*
Class which encapsulates a battery and all its models
*/

class battery_t
{
public:
	battery_t();
	battery_t(double dt, int battery_chemistry);

	// deep copy constructor (new memory), from battery to this
	battery_t(const battery_t& battery);

	// copy members from battery to this
	void copy(const battery_t& battery);
	~battery_t(){};
	void delete_clone();

	void initialize(capacity_t *, voltage_t *, lifetime_t *, thermal_t *, losses_t *);

	// Run all
	void run(double P);

	// Run a component level model
	void runCapacityModel(double I);
	void runVoltageModel();
	void runThermalModel(double I);
	void runLifetimeModel(double DOD);
	void runLossesModel();

	capacity_t * capacity_model() const;
	voltage_t * voltage_model() const;
	lifetime_t * lifetime_model() const;
	thermal_t * thermal_model() const;
	losses_t * losses_model() const;

	// Get capacity quantities
	double battery_charge_needed();
	double battery_charge_total();
	double battery_charge_maximum();
	double battery_energy_to_fill();
	double battery_power_to_fill();
	double battery_soc();

	// Get Voltage
	double cell_voltage();
	double battery_voltage();

	double timestep_hour();

	enum CHEMS{ LEAD_ACID, LITHIUM_ION, VANADIUM_REDOX };


private:
	capacity_t * _capacity;
	lifetime_t * _lifetime;
	voltage_t * _voltage;
	thermal_t * _thermal;
	losses_t * _losses;
	int _battery_chemistry;
	double _dt_hour;			// [hr] - timestep
	double _dt_min;				// [min] - timestep
	bool _firstStep;
};
/*
Dispatch Base Class - can envision many potential modifications. Goal is to define standard API
*/
class dispatch_t
{
public:
	dispatch_t(battery_t * Battery, 
			   double dt, 
			   double SOC_min, 
			   double SOC_max, 
			   double Ic_max, 
			   double Id_max, 
			   double t_min, 
			   int mode, 
			   int pv_dispatch);
	
	virtual ~dispatch_t();

	// Public APIs
	virtual void dispatch(size_t year,
		size_t hour_of_year,
		size_t step,
		double P_pv_dc_charging,      
		double P_pv_dc_discharging,
		double P_load_dc_charging,
		double P_load_dc_discharging)=0;   
						  
	virtual void compute_grid_net();

	enum MODES{LOOK_AHEAD, LOOK_BEHIND, MAINTAIN_TARGET, MANUAL};
	enum METERING{ BEHIND, FRONT };
	enum PV_PRIORITY{ MEET_LOAD, CHARGE_BATTERY};

	// Outputs
	double cycle_efficiency();

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

	// control settings
	int pv_dispatch_priority(){ return _pv_dispatch_to_battery_first; }
	double battery_power_to_fill(){ return _Battery->battery_power_to_fill(); }

	message get_messages();

protected:

	// Controllers
	void SOC_controller(double battery_voltage, double charge_total, double charge_max);
	void energy_controller();
	void switch_controller();
	double current_controller(double battery_voltage);
	void restrict_current(double &I);

	// compute totals
	void compute_battery_state();
	void compute_to_batt();
	void compute_to_load();
	void compute_to_grid();
	void compute_generation();

	battery_t * _Battery;
	battery_t * _Battery_initial;

	double _dt_hour;

	// configuration
	int _mode; // 0 = look ahead, 1 = look behind, 2 = maintain target power, 3 = manual dispatch
	int _pv_dispatch_to_battery_first; // 0 = meet load first, 1 = meet battery first

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

	// the options of what the PV and load power is that the battery sees depending on scenario
	double _P_pv_charging;
	double _P_pv_discharging;
	double _P_load_charging;
	double _P_load_discharging;

	// Charge & current limits controllers
	double _SOC_min;
	double _SOC_max;
	double _Ic_max;
	double _Id_max;
	double _t_min;
	double _e_max;
	double _percent_discharge;
	double _percent_charge;
	double _P_target;

	// rapid charge change controller
	int _t_at_mode; // [minutes]
	bool _charging;
	bool _prev_charging;
	bool _grid_recharge;

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
					  double Ic_max, 
					  double Id_max, 
					  double t_min, 
					  int mode, 
					  bool pv_dispatch,
					  util::matrix_t<float> dm_dynamic_sched, 
					  util::matrix_t<float> dm_dynamic_sched_weekend,
					  bool * dm_charge, 
					  bool *dm_discharge, 
					  bool * dm_gridcharge, 
					  std::map<int, double> dm_percent_discharge, 
					  std::map<int, double> dm_percent_gridcharge);
	virtual ~dispatch_manual_t(){};
	virtual void dispatch(size_t year,
		size_t hour_of_year,
		size_t step,
		double P_pv_dc_charging,
		double P_pv_dc_discharging,
		double P_load_dc_charging,
		double P_load_dc_discharging);

protected:
	
	void initialize_dispatch(size_t hour_of_year, size_t step, double P_pv_dc_charging, double P_pv_dc_discharging, double P_load_dc_charging, double P_load_dc_discharging);
	void reset();
	void compute_energy_load_priority(double energy_needed);
	void compute_energy_battery_priority(double energy_needed);
	bool compute_energy_battery_priority_charging(double energy_needed);
	bool check_constraints(double &I, int count);


	util::matrix_t < float > _sched;
	util::matrix_t < float > _sched_weekend;
	std::vector<bool> _charge_array;
	std::vector<bool> _discharge_array;
	std::vector<bool> _gridcharge_array;
	std::map<int, double>  _percent_discharge_array;
	std::map<int, double> _percent_charge_array;
	bool  _can_charge;
	bool  _can_discharge;
	bool  _can_grid_charge;
};
/* Manual dispatch for utility scale (front of meter)*/
class dispatch_manual_front_of_meter_t : public dispatch_manual_t
{
public:
	dispatch_manual_front_of_meter_t(battery_t * Battery,
		double dt_hour,
		double SOC_min,
		double SOC_max,
		double Ic_max,
		double Id_max,
		double t_min,
		int mode,
		bool pv_dispatch,
		util::matrix_t<float> dm_dynamic_sched,
		util::matrix_t<float> dm_dynamic_sched_weekend,
		bool * dm_charge,
		bool *dm_discharge,
		bool * dm_gridcharge,
		std::map<int, double> dm_percent_discharge,
		std::map<int, double> dm_percent_gridcharge);
	~dispatch_manual_front_of_meter_t(){};

	virtual void dispatch(size_t year,
		size_t hour_of_year,
		size_t step,
		double P_pv_dc_charging,
		double P_pv_dc_discharging,
		double P_load_dc_charging = 0,
		double P_load_dc_discharging = 0);
	void compute_grid_net();

protected:
	void compute_energy_no_load(double energy_needed);
	void compute_to_grid();
};


/*
Automated dispatch classes
*/
class grid_point
{
	// Class to encapsulate the required grid power, hour, and step, i.e
	// grid_point = [grid_power, hour, step]
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
class automate_dispatch_t : public dispatch_manual_t
{
public:
	automate_dispatch_t(
		battery_t * Battery, 
		double dt_hour, 
		double SOC_min, 
		double SOC_max, 
		double Ic_max, 
		double Id_max,
		double t_min, 
		int mode,   // 0/1/2
		bool pv_dispatch,
		util::matrix_t<float> dm_dynamic_sched, 
		util::matrix_t<float> dm_dynamic_sched_weekend,
		bool * dm_charge, 
		bool *dm_discharge, 
		bool * dm_gridcharge, 
		std::map<int, double> dm_percent_discharge, 
		std::map<int, double> dm_percent_gridcharge,
		int nyears
		);				  

	void dispatch(size_t year,
		size_t hour_of_year,
		size_t step,
		double P_pv_dc_charging,
		double P_pv_dc_discharging,
		double P_load_dc_charging,
		double P_load_dc_discharging);
	
	void update_pv_load_data(std::vector<double> P_pv_dc, std::vector<double> P_load_dc);
	void set_target_power(std::vector<double> P_target);

protected:
	void update_dispatch(int hour_of_year, int step, int idx);
	int get_mode();
	void initialize(int hour_of_year);
	void check_debug(FILE *&p, bool & debug, int hour_of_year, int idx);
	void sort_grid(FILE *p, bool debug, int idx );
	void compute_energy(FILE *p, bool debug, double & E_max);
	void target_power(FILE*p, bool debug, double & P_target, double E_max, int idx);
	void set_charge(int profile);
	int set_discharge(FILE *p, bool debug, int hour_of_year, double P_target, double E_max);
	void set_gridcharge(FILE *p, bool debug, int hour_of_year, int profile, double P_target, double E_max);
	void check_new_month(int hour_of_year, int step);
	
	std::vector<double> _P_pv_dc;	 // [kW]
	std::vector<double> _P_load_dc;  // [kW]
	std::vector<double> _P_target;   // [kW]
	double _P_target_month;	    	 // [kW]
	int _month;						 // [0-11]
	int _hour_last_updated;
	double _dt_hour;
	int _steps_per_hour;
	int _num_steps;
	int _nyears; 
	int _mode;
	double _safety_factor;
	
	
	grid_vec grid; // [P_grid, hour, step]
};

// battery_metrics (report as AC or DC energy quanitities)
class battery_metrics_t
{
public:
	battery_metrics_t(battery_t * Battery, double dt_hour);
	~battery_metrics_t(){};

	void compute_metrics_ac(double P_tofrom_batt, double P_pv_to_batt, double P_grid_to_batt, double P_tofrom_grid);
	void compute_metrics_dc(dispatch_t * dispatch);
	void compute_annual_loss();

	void accumulate_energy_charge(double P_tofrom_batt);
	void accumulate_energy_discharge(double P_tofrom_batt);
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
	double energy_loss_annual();
	double average_efficiency();
	double pv_charge_percent();

protected:

	// single value metrics
	double _e_charge_accumulated;	 // [Kwh]
	double _e_discharge_accumulated; // [Kwh]
	double _e_charge_from_pv;		 // [Kwh]
	double _e_charge_from_grid;		 // [Kwh]
	double _average_efficiency;		 // [%]
	double _pv_charge_percent;		 // [%]

	// annual metrics
	double _e_charge_from_pv_annual;   // [Kwh]
	double _e_charge_from_grid_annual; // [Kwh]
	double _e_charge_annual;		   // [Kwh]
	double _e_discharge_annual;		   // [Kwh]
	double _e_grid_import_annual;	   // [Kwh]
	double _e_grid_export_annual;	   // [Kwh]
	double _e_loss_annual;			   // [kWh]

	battery_t * _Battery;
	double _dt_hour;
};

#endif
