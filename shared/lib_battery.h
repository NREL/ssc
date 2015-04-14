#ifndef battery_h
#define battery_h

#include "lib_util.h"
#include "lsqfit.h"

#include <vector>
#include <map>
#include <string>

const double watt_to_kilowatt = 1. / 1000;
const double kilowatt_to_watt = 1000;

/*
Thermal classes
*/
class thermal_t
{
public:
	thermal_t(double mass, double length, double width, double height, 
		double Cp, double h, double T_room, double R,
		const util::matrix_t<double> &cap_vs_temp );

	void updateTemperature(double I, double dt);
	double getCapacityPercent();

	// outputs
	double T_battery();
	double CapacityPercent();

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
	double _thickness;	// [m] - wall thickness
	double _Cp;			// [J/KgK] - battery specific heat capacity
	double _h;			// [Wm2K] - general heat transfer coefficient
	double _T_room;		// [K] - storage room temperature
	double _R;			// [Ohm] - internal resistance
	double _A;			// [m2] - exposed surface area
	double _T_battery;   // [K]
	double _capacity_percent; //[%]
	const double _hours_to_seconds = 3600;
};


/*
Base class from which capacity models derive
Note, all capacity models are based on the capacity of one battery
*/
class voltage_t;
class capacity_t
{
public:
	capacity_t(double q);
	virtual ~capacity_t(){};
	
	// pure virtual functions (abstract) which need to be defined in derived classes
	virtual void updateCapacity(double P, voltage_t * V, double dt, int cycles) = 0;
	virtual void updateCapacityForThermal(thermal_t * thermal)=0;
	virtual void updateCapacityForLifetime(double capacity_percent)=0;

	virtual double qmax() = 0; // max capacity
	virtual double qmaxI() = 0; // max capacity at current
	virtual double q1() = 0; // available charge
	virtual double q10() = 0; // capacity at 10 hour discharge rate

	void check_charge_change(); 


	// common outputs
	double SOC();
	double DOD();
	double prev_DOD();
	double q0();
	double I();
	double P();
	bool chargeChanged();

protected:
	double _q0;  // [Ah] - Total capacity at timestep 
	double _I;   // [A]  - Current draw during last step
	double _P;   // [Ah] - Power draw during last step [ P > 0 discharge, P < 0 charge]
	double _SOC; // [%] - State of Charge
	double _DOD; // [%] - Depth of Discharge
	double _DOD_prev; // [%] - Depth of Discharge of previous step
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
	capacity_kibam_t(double q20, double t1, double q1, double q10);
	void updateCapacity(double P, voltage_t * V, double dt, int cycles);
	void updateCapacityForThermal(thermal_t * thermal);
	void updateCapacityForLifetime(double capacity_percent);
	double q1(); // Available charge
	double q2(); // Bound charge
	double qmax(); // Max charge
	double qmaxI(); // Max charge at current
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
	double _qmax; // [Ah] - maximum possible capacity

	// charge which changes with time
	double _q1_0; // [Ah] - charge available
	double _q2_0; // [Ah] - charge bound
	double _q10; //  [Ah] - Capacity at 10 hour discharge rate
	double _q20; // [Ah] - Capacity at 20 hour discharge rate
	double _I20; // [A]  - Current at 20 hour discharge rate
	double _qmaxI;// [Ah] - theoretical max charge at this current
};

/*
Lithium Ion specific capacity model
*/
class capacity_lithium_ion_t : public capacity_t
{
public:
	capacity_lithium_ion_t(double q);
	~capacity_lithium_ion_t();

	// override public api
	void updateCapacity(double P, voltage_t *, double dt, int cycles);
	void updateCapacityForThermal(thermal_t * thermal);
	void updateCapacityForLifetime(double capacity_percent);

	double q1(); // Available charge
	double qmax(); // Max charge
	double qmaxI(); // Max charge at current
	double q10(); // Capacity at 10 hour discharge rate

protected:
	double _qmax; // [Ah] - maximum possible capacity
	double _qmax0; // [Ah] - original maximum capacity
};


/*
Voltage Base class.  
All voltage models are based on one-cell, but return the voltage for one battery
*/
class voltage_t
{
public:
	voltage_t(int num_cells, double voltage, double cutoff);

	virtual void updateVoltage(capacity_t * capacity, double dt)=0;
	double battery_voltage(); // voltage of one battery
	double cell_voltage(); // voltage of one cell
	double cutoff_voltage(); // cutoff voltage of one cell


protected:
	int _num_cells;    // number of cells per battery
	double _cell_voltage; // closed circuit voltage per cell [V]
	double _cutoff_voltage; // cutoff voltage of one cell
};

class voltage_basic_t : public voltage_t
{
public:
	voltage_basic_t(int num_cells, double voltage);
	void updateVoltage(capacity_t * capacity, double dt);
};

// Shepard + Tremblay Model
class voltage_dynamic_t : public voltage_t
{
public:
	voltage_dynamic_t(int num_cells, double voltage, double Vfull, double Vexp, double Vnom, double Qfull, double Qexp, double Qnom, double C_rate, double cutoff);
	void parameter_compute();
	void updateVoltage(capacity_t * capacity, double dt);

protected:
	double voltage_model(double capacity, double current,  double q0);
	double voltage_model_tremblay_hybrid(double capacity, double current, double q0, double dt);


private:
	double _Vfull;
	double _Vexp;
	double _Vnom;
	double _Qfull;
	double _Qexp;
	double _Qnom;
	double _C_rate;
	double _R;
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
	lifetime_t(const util::matrix_t<double> &cyles_vs_DOD);
	~lifetime_t();
	void rainflow(double DOD);
	void rainflow_finish();
	int cycles_elapsed();
	double capacity_percent();
	int lifetime_t::forty_percent_cycles();
	int lifetime_t::hundred_percent_cycles();

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
	double _klt;			// current index in Peaks where _Slt is stored
	double _Xlt;
	double _Ylt;
	double _Slt;
	int _fortyPercent;
	int _hundredPercent;
	std::vector<double> _Peaks;
	double _Range;
	double _average_range;

	enum RETURN_CODES
	{
		LT_SUCCESS,
		LT_GET_DATA,
		LT_RERANGE
	};
};

/*
Losses Base class.
*/
class losses_t
{

};

/*
Class which encapsulates a battery and all its models
*/

class battery_t
{
public:
	battery_t();
	battery_t(double power_conversion_efficiency, double dt);
	void initialize(capacity_t *, voltage_t *, lifetime_t *, thermal_t *);

	// Run all
	void run(double P);
	void finish();

	// Run a component level model
	void runCapacityModel(double P, voltage_t * voltage);
	void runVoltageModel();
	void runThermalModel(double I);
	void runLifetimeModel(double DOD);

	capacity_t * capacity_model();
	voltage_t * voltage_model();

	// Get capacity quantities
	double chargeNeededToFill();
	double getCurrentCharge();

	// Get Voltage
	double cell_voltage();
	double battery_voltage();

private:
	capacity_t * _capacity;
	lifetime_t * _lifetime;
	voltage_t * _voltage;
	thermal_t * _thermal;
	double _dt;
	double _power_conversion_efficiency;
	bool _firstStep;
};

/* 
Battery bank class
Accounts for multiple batteries and power conversion efficiency
Currently, only bank is treated as one large battery in series, so class isn't very useful
*/
class battery_bank_t
{
public:
	battery_bank_t(battery_t * battery, int num_batteries_series, int num_batteries_parallel, int battery_chemistry, double power_conversion_efficiency);
	void run(double P);
	void finish();
	double bank_charge_needed();
	double bank_charge_available();
	double bank_voltage();
	int num_batteries();
	battery_t * battery();

protected:
	battery_t * _battery;
	int _num_batteries_series;
	int _num_batteries_parallel;
	int _num_batteries;
	int _battery_chemistry;
	double _power_conversion_efficiency;

};

/*
Dispatch Base Class - can envision many potential modifications. Goal is to define standard API
*/
class dispatch_t
{
public:
	dispatch_t(battery_bank_t * BatteryBank, double dt);

	// Public APIs
	virtual void dispatch(size_t hour_of_year, double e_pv, double e_load) = 0;

	double energy_tofrom_battery();
	double energy_tofrom_grid();
	double pv_to_load();
	double battery_to_load();
	double grid_to_load();

protected:
	battery_bank_t * _BatteryBank;
	double _dt;

	double _e_tofrom_batt;
	double _e_grid;
	double _pv_to_load;
	double _battery_to_load;
	double _grid_to_load;

};

/*
Manual dispatch class
*/
class dispatch_manual_t : public dispatch_t
{
public:
	dispatch_manual_t(battery_bank_t * BatteryBank, double dt, util::matrix_static_t<float, 12, 24> dm_sched, bool * dm_charge, bool *dm_discharge, bool * dm_gridcharge);
	void dispatch(size_t hour_of_year, double e_pv, double e_load);

protected:
	util::matrix_static_t<float, 12, 24> _sched;
	bool * _charge_array;
	bool * _discharge_array;
	bool * _gridcharge_array;
	bool  _can_charge;
	bool  _can_discharge;
	bool  _can_grid_charge;


};


/*
Non-class functions
*/
void getMonthHour(int hourOfYear, int * month, int * hour);
bool compare(int, int);

#endif
