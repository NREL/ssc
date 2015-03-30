#ifndef battery_h
#define battery_h

#include "lib_util.h"
#include "lsqfit.h"

#include <vector>
#include <map>
#include <string>

const double watt_to_kilowatt = 1. / 1000;
const double kilowatt_to_watt = 1000;

typedef std::map<std::string, double> output_map;

/*
Thermal Base class.
*/
class thermal_t
{
public:
	thermal_t(double mass, double length, double width, double height, 
		double Cp, double h, double T_room, double R,
		std::vector<double> temperature_vect, std::vector<double> capacity_vect);
	~thermal_t();

	output_map updateTemperature(double I, double dt);
	double getCapacityPercent();

protected:
	double f(double T_battery, double I);
	double thermal_t::rk4(double I, double dt);

protected:

	double * _capacity_vect; // [0-1] - capacity percents
	double *_temperature_vect; // [K] - temperatures
	double *_a;

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
	output_map _output;
	const double _hours_to_seconds = 3600;
};

/*
Base class from which capacity models derive
Note, all capacity models are based on the capacity of one battery
*/

class capacity_t
{
public:
	capacity_t(double q, double V);
	virtual ~capacity_t(){};
	
	// pure virtual functions (abstract) which need to be defined in derived classes
	virtual output_map updateCapacity(double P, double V, double dt, int cycles) = 0;
	virtual output_map updateCapacityForThermal(thermal_t * thermal)=0;
	virtual double getMaxCapacity() = 0;
	virtual double getMaxCapacityAtCurrent() = 0;
	virtual double getAvailableCapacity() = 0;
	virtual double get10HourCapacity() = 0;


	// functions which should be able to be constant across all derived classes
	double getDOD();
	double getTotalCapacity();
	double getCurrent();
	bool chargeChanged();

protected:
	double _q0;  // [Ah] - Total capacity at timestep 
	double _I;   // [A]  - Current draw during last step
	double _P;   // [Ah] - Power draw during last step [ P > 0 discharge, P < 0 charge]
	double _V;   // [V]  - Voltage (maybe will be dynamic eventually)
	double _SOC; // [0-1] - State of Charge
	double _DOD; // [0-1] - Depth of Discharge
	bool _chargeChange; // [true/false] - indicates if charging state has changed since last step
	bool _prev_charging; // [true/false] - indicates if last state was charging;
	output_map _output; // Output structure
};

/*
KiBaM specific capacity model
*/
class capacity_kibam_t : public capacity_t
{
public:

	// Public APIs 
	capacity_kibam_t(double q10, double q20, double I20, double V, double t1, double t2, double q1, double q2);
	output_map updateCapacity(double P, double V, double dt, int cycles = 0);
	output_map updateCapacityForThermal(thermal_t * thermal);
	double getAvailableCapacity();
	double getMaxCapacity();
	double getMaxCapacityAtCurrent();
	double get10HourCapacity();
	double get20HourCapacity();

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
	capacity_lithium_ion_t(double q, double V, std::vector<double> capacities, std::vector<double> cycles);
	~capacity_lithium_ion_t();

	// override public api
	output_map updateCapacity(double P, double V, double dt, int cycles);
	output_map updateCapacityForThermal(thermal_t * thermal);
	double getMaxCapacity();
	double getMaxCapacityAtCurrent();
	double getAvailableCapacity();
	double get10HourCapacity();


protected:
	double _qmax; // [Ah] - maximum possible capacity
	double _qmax0; // [Ah] - original maximum capacity
	double * _capacities_vect; // % of original
	double *_cycle_vect; 
	double *_a;
	int _n; // number of points

};


/*
Voltage Base class.  
All voltage models are based on one-cell, but return the voltage for one battery
*/
class voltage_t
{
public:
	voltage_t(int num_cells, double voltage, double * other=0);

	virtual output_map updateVoltage(capacity_t * capacity, double dt)=0;
	double getVoltage();
	double getCellVoltage();

protected:
	int _num_cells;    // number of cells per battery
	double _cell_voltage; // closed circuit voltage per cell [V]
	output_map _output;   // output structure
};

class voltage_basic_t : public voltage_t
{
public:
	voltage_basic_t(int num_cells, double voltage);
	output_map updateVoltage(capacity_t * capacity, double dt);
};

// Unnewehr Universal Model
class voltage_dynamic_t : public voltage_t
{
public:
	voltage_dynamic_t(int num_cells, double voltage, double *other);
	void parameter_compute();
	output_map updateVoltage(capacity_t * capacity, double dt);

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
	lifetime_t(std::vector<double> DOD_vect, std::vector<double> cycle_vect, int n);
	~lifetime_t();
	output_map rainflow(double DOD);
	output_map rainflow_finish();
	int getNumberOfCycles();

protected:
	void rainflow_ranges();
	void rainflow_ranges_circular(int index);
	int rainflow_compareRanges();

	double * _DOD_vect;
	double *_cycle_vect;
	double *_a;
	double _nCycles;
	double _Dlt;
	double _jlt;			// last index in Peaks, i.e, if Peaks = [0,1], then _jlt = 1
	double _klt;			// current index in Peaks where _Slt is stored
	double _Xlt;
	double _Ylt;
	double _Slt;
	std::vector<double> _Peaks;
	double _Range;
	output_map _output;

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
	battery_t(int num_batteries, double power_conversion_efficiency, double dt);
	void initialize(capacity_t *, voltage_t *, lifetime_t *, thermal_t *);

	// Run all
	void run(double P);
	void finish();

	// Run a component level model
	output_map runCapacityModel(double P, double V);
	output_map runVoltageModel();
	output_map runThermalModel(double I);
	output_map runLifetimeModel(double DOD);

	output_map getCapacityOutput();
	output_map getLifetimeOutput();
	output_map getVoltageOutput();
	output_map getThermalOutput();

	// Get capacity quantities
	double chargeNeededToFill();
	double getCurrentCharge();

	// Get Voltage
	double cellVoltage();
	double batteryVoltage();

private:
	capacity_t * _capacity;
	lifetime_t * _lifetime;
	voltage_t * _voltage;
	thermal_t * _thermal;
	double _dt;
	int _num_batteries;
	double _power_conversion_efficiency;
	bool _firstStep;
	output_map _CapacityOutput;
	output_map _LifetimeOutput;
	output_map _VoltageOutput;
	output_map _ThermalOutput;
};

/* 
Battery bank class
Accounts for multiple batteries and power conversion efficiency
*/
class battery_bank_t
{
public:
	battery_bank_t(battery_t * battery, int num_batteries, int battery_chemistry, double power_conversion_efficiency);
	output_map run(double P);
	output_map finish();
	double chargeNeededToFill();
	double getCurrentCharge();
	double getBankVoltage();
	output_map getOutputs();

protected:
	void adjustOutputs();
	battery_t * _battery;
	int _num_batteries;
	int _battery_chemistry;
	double _power_conversion_efficiency;
	output_map _output;

};

/*
Dispatch Base Class - can envision many potential modifications. Goal is to define standard API
*/
class dispatch_t
{
public:
	dispatch_t(battery_bank_t * BatteryBank, double dt);

	// Public APIs
	virtual void set_profiles(bool can_charge, bool can_discharge, bool grid_charge) = 0;
	virtual output_map dispatch(double e_pv, double e_load) = 0;

	output_map getBatteryBankOutput();

protected:
	battery_bank_t * _BatteryBank;
	output_map _output;
	bool _can_charge;
	bool _can_discharge;
	bool _can_grid_charge;
	int _mode;
	double _dt;

};

/*
Manual dispatch class
*/
class dispatch_manual_t : public dispatch_t
{
public:
	dispatch_manual_t(battery_bank_t * BatteryBank, double dt);
	void set_profiles(bool can_charge, bool can_discharge, bool grid_charge);
	output_map dispatch(double e_pv, double e_load);
};


/*
Non-class functions
*/
double life_vs_DOD(double R, double *a, void * user_data);
double third_order_polynomial(double independent_variable, double *a, void *user_data);
void getMonthHour(int hourOfYear, int * month, int * hour);
bool compare(int, int);

#endif
