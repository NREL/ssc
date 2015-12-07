#ifndef battery_h
#define battery_h

#include "lib_util.h"
#include "lsqfit.h"

#include <vector>
#include <map>
#include <string>
#include <stdio.h>
#include <algorithm>

const double watt_to_kilowatt = 1. / 1000;
const double kilowatt_to_watt = 1000;
const double hour_to_min = 60.;
const double tolerance = 0.001;

typedef std::vector<double> double_vec;
typedef std::vector<int> int_vec;
/*
Base class from which capacity models derive
Note, all capacity models are based on the capacity of one battery
*/
class voltage_t;
class capacity_t
{
public:
	capacity_t(double q, double SOC_max);
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
	~capacity_lithium_ion_t();

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
class voltage_t
{
public:
	voltage_t(int num_cells_series, int num_strings, double voltage);

	virtual void updateVoltage(capacity_t * capacity, double dt)=0;
	double battery_voltage(); // voltage of one battery
	double cell_voltage(); // voltage of one cell
	double R(); // computed resistance

protected:
	int _num_cells_series;    // number of cells in series
	int _num_strings;  // addition number in parallel
	double _cell_voltage; // closed circuit voltage per cell [V]
	double _R;

};

class voltage_basic_t : public voltage_t
{
public:
	voltage_basic_t(int num_cells_series, int num_strings, double voltage);
	void updateVoltage(capacity_t * capacity, double dt);
};

// Shepard + Tremblay Model
class voltage_dynamic_t : public voltage_t
{
public:
	voltage_dynamic_t(int num_cells_series, int num_strings, double voltage, double Vfull, double Vexp, double Vnom, double Qfull, double Qexp, double Qnom, double C_rate, double R);
	void parameter_compute();
	void updateVoltage(capacity_t * capacity, double dt);

protected:
	double voltage_model(double capacity, double current,  double q0);
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

/*
Lifetime class.  Currently only one lifetime model anticipated
*/

class lifetime_t
{

public:
	lifetime_t(const util::matrix_t<double> &cyles_vs_DOD, const int replacement_option, const double replacement_capacity  );
	~lifetime_t();
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

	void updateTemperature(double I, double R, double dt);
	void replace_battery();

	// outputs
	double T_battery();
	double capacity_percent();

protected:
	double f(double T_battery, double I);
	double rk4(double I, double dt);
	double trapezoidal(double I, double dt);

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
};
/*
Losses Base class
*/
class losses_t
{
public:
	losses_t(lifetime_t *, thermal_t *, capacity_t*);
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
	void initialize(capacity_t *, voltage_t *, lifetime_t *, thermal_t *, losses_t *);

	// Run all
	void run(double P);

	// Run a component level model
	void runCapacityModel(double I);
	void runVoltageModel();
	void runThermalModel(double I);
	void runLifetimeModel(double DOD);
	void runLossesModel();

	capacity_t * capacity_model();
	voltage_t * voltage_model();
	lifetime_t * lifetime_model();

	// Get capacity quantities
	double battery_charge_needed();
	double battery_charge_total();
	double battery_charge_maximum();

	// Get Voltage
	double cell_voltage();
	double battery_voltage();

	double timestep_hour();

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
	dispatch_t(battery_t * Battery, double dt, double SOC_min, double SOC_max, double Ic_max, double Id_max, 
			   double t_min, bool ac_or_dc, double dc_dc, double ac_dc, double dc_ac);

	// Public APIs
	virtual void dispatch(size_t hour_of_year, 
						  size_t step, 
						  double e_pv,     // PV energy [kWh]
						  double e_load)   // Load energy [kWh]
						  = 0;

	// Controllers
	void SOC_controller(double battery_voltage, double charge_total, double charge_max);
	void energy_controller();
	void switch_controller();
	double current_controller(double battery_voltage);
	
	// Losses at AC or DC connection points and internal loss
	double conversion_loss_in(double);
	double conversion_loss_out(double);
	void total_loss(double, double,double);

	// Outputs
	void new_year();
	double cycle_efficiency();
	double average_efficiency();

	double energy_tofrom_battery();
	double energy_tofrom_grid();
	double gen();
	double pv_to_load();
	double battery_to_load();
	double grid_to_load();
	double pv_to_batt();
	double grid_to_batt();

	double charge_annual();
	double discharge_annual();
	double grid_import_annual();
	double grid_export_annual();
	double energy_loss_annual();

	void compute_grid_net( double e_pv, double e_load);

	const enum MODES
	{
		LOOK_AHEAD, LOOK_BEHIND, MAINTAIN_TARGET, MANUAL
	};

protected:

	void compute_efficiency();

	battery_t * _Battery;
	double _dt_hour;

	// configuration
	bool _ac_or_dc;
	double _dc_dc;
	double _dc_ac;
	double _ac_dc;

	// energy quantities
	double _e_tofrom_batt;
	double _e_grid;
	double _e_gen;
	double _e_loss_annual; 
	double _pv_to_load;
	double _battery_to_load;
	double _grid_to_load;
	double _pv_to_batt;
	double _grid_to_batt;
	double _battery_fraction;
	double _pv_fraction;

	// Charge & current limits controllers
	double _SOC_min;
	double _SOC_max;
	double _Ic_max;
	double _Id_max;
	double _I_loss;
	double _t_min;
	double _e_max_discharge;
	double _e_max_charge;
	double _percent_discharge;
	double _percent_charge;
	double _P_target;

	// rapid charge change controller
	int _t_at_mode; // [minutes]
	bool _charging;
	bool _prev_charging;
	bool _grid_recharge;

	// efficiency
	double _charge_accumulated;		 // [Kwh]
	double _discharge_accumulated;   // [Kwh]
	double _charge_annual;			 // [Kwh]
	double _discharge_annual;		 // [Kwh]
	double _grid_import_annual;		 // [Kwh]
	double _grid_export_annual;		 // [Kwh]
	double _average_efficiency;		 // [%]
};

/*
Manual dispatch class
*/
class dispatch_manual_t : public dispatch_t
{
public:
	friend class automate_dispatch_t;
	dispatch_manual_t(battery_t * Battery, double dt_hour, double SOC_min, double SOC_max, double Ic_max, double Id_max, 
					 double t_min, bool ac_or_dc, double dc_dc, double ac_dc, double dc_ac,
					 int mode, util::matrix_t<float> dm_dynamic_sched, util::matrix_t<float> dm_dynamic_sched_weekend,
					 bool * dm_charge, bool *dm_discharge, bool * dm_gridcharge, std::map<int, double> dm_percent_discharge, std::map<int, double> dm_percent_gridcharge);
	void dispatch(size_t hour_of_year, size_t step, double e_pv, double e_load);

protected:
	
	void reset();
	
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
	
	int _mode; // 0 = look ahead, 1 = look behind, 2 = maintain target power, 3 = manual dispatch
	
};
/*
Automate dispatch classes
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
typedef std::vector<grid_point> grid_vec;

struct byGrid
{
	bool operator()(grid_point const  &a, grid_point const &b)
	{
		return a.Grid() > b.Grid();
	}
};

class automate_dispatch_t
{
public:
	automate_dispatch_t(dispatch_manual_t * Dispatch,
		int nyears,
		double dt_hour,			   // [hr]
		double * pv, double *load, // input as power [kW] not energy
		int mode);				   // 0/1/2
	void update_pv_load_data(double *pv, double *load);
	void update_dispatch(int hour_of_year, int idx);
	void set_target_power(std::vector<double> target_power);
	int get_mode();

protected:
	void initialize(int hour_of_year, int idx );
	void check_debug(FILE *&p, bool & debug, int hour_of_year, int idx);
	void sort_grid(FILE *p, bool debug, int idx );
	void compute_energy(FILE *p, bool debug, double & E_max);
	void target_power(FILE*p, bool debug, double & P_target, double E_max, int idx);
	void set_charge(int profile);
	int set_discharge(FILE *p, bool debug, int hour_of_year, double P_target, double E_max);
	void set_gridcharge(FILE *p, bool debug, int hour_of_year, int profile, double P_target, double E_max);


	dispatch_manual_t * _dispatch;
	double * _pv;						 // [kW]
	double * _load;                      // [kW]
	std::vector<double> _target_power;   // [kW]
	int _hour_last_updated;
	double _dt_hour;
	int _steps_per_hour;
	int _num_steps;
	int _nyears;
	int _mode;
	
	grid_vec grid; // [grid_power, hour, step]
};
#endif
