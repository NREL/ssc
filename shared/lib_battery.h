#ifndef battery_h
#define battery_h

#include "lib_util.h"
#include "lsqfit.h"

#include <vector>

const double watt_to_kilowatt = 1. / 1000;
const double kilowatt_to_watt = 1000;


/*
Output Structure 
*/
struct output
{
	const char *name;
	double value;
};

/*
Output Enumerations
*/

enum capacity_out
{	
	TOTAL_CHARGE, // Total Charge [Ah]
	AVAILABLE_CHARGE, // Available Charge [Ah] (Debugging Only)
	BOUND_CHARGE, // Bound Charge [Ah] (Debugging Only)
	POWER_DURING_STEP, // Power [W]
	STATE_OF_CHARGE,// State of Charge [%]
	DEPTH_OF_DISCHARGE, // Depth of Discharge [%] 
	MAX_CHARGE_AT_CURRENT, // Max Charge at Current [Ah]
	CURRENT, // Current [A]

	// ALWAYS LEAVE THIS AT END
	TOTAL_CAPACITY_OUT
};

enum voltage_out
{
	CELL_VOLTAGE, // Voltage per cell [V]
	TOTAL_VOLTAGE, // Total battery voltage [V]

	// ALWAYS LEAVE THIS AT END
	TOTAL_VOLTAGE_OUT
};
enum lifetime_out
{
	FRACTIONAL_DAMAGE, // Damage between 0 & 1.  1 indicates replacement needed
	NUMBER_OF_CYCLES, // Number of cycles battery has gone through

	// ALWAYS LEAVE THIS AT END
	TOTAL_LIFETIME_OUT
};

/*
Base class from which capacity models derive
*/

class capacity_t
{
public:
	capacity_t();
	capacity_t(double q20, double I20, double V);
	virtual ~capacity_t(){};
	
	// pure virtual functions which need to be defined in derived classes
	virtual output* updateCapacity(double P, double V, double dt)=0;
	virtual double getMaxCapacityAtCurrent() = 0;
	virtual double getAvailableCapacity() = 0;
	virtual double get10HourCapacity() = 0;

	// functions which should be able to be constant across all derived classes
	double getDOD();
	double get20HourCapacity();
	double getTotalCapacity();
	double getCurrent();
	bool chargeChanged();

protected:
	double _q20; // [Ah] - Capacity at 20 hour discharge rate
	double _q0;  // [Ah] - Total capacity at timestep 
	double _I20; // [A]  - Current at 20 hour discharge rate
	double _I;   // [A]  - Current draw during last step
	double _P;   // [Ah] - Power draw during last step [ P > 0 discharge, P < 0 charge]
	double _V;   // [V]  - Voltage (maybe will be dynamic eventually)
	double _SOC; // [0-1] - State of Charge
	double _DOD; // [0-1] - Depth of Discharge
	bool _chargeChange; // [true/false] - indicates if charging state has changed since last step
	output *_output; // Output structure
};

/*
KiBaM specific capacity model
*/
class capacity_kibam_t : public capacity_t
{
public:

	// Public APIs 
	capacity_kibam_t(double q20, double I20, double V, double t1, double t2, double q1, double q2);
	output* updateCapacity(double P, double V, double dt);
	double getAvailableCapacity();
	double getMaxCapacityAtCurrent();
	double get10HourCapacity();
	~capacity_kibam_t();

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
	double _qmaxI;// [Ah] - theoretical max charge at this current
	bool _prev_charging; // [true/false] - indicates if last state was charging;
};

/*
Voltage Base class.  
*/
class voltage_t
{
public:
	voltage_t(int num_cells, double voltage);

	virtual output* updateVoltage(capacity_t * capacity, double dT)=0;
	double getVoltage();
	double getCellVoltage();

protected:
	int _num_cells;    // number of cells per battery
	double _cell_voltage; // closed circuit voltage per cell [V]
	output* _output;   // output structure
};

class voltage_copetti_t : public voltage_t
{
public:
	voltage_copetti_t(int num_cells, double voltage);
	~voltage_copetti_t();

	output* updateVoltage(capacity_t * capacity, double dT);
	double voltage_charge(double DOD, double q10, double I, double dT);
	double voltage_discharge(double DOD, double q10, double I, double dT);

};



/*
Lifetime class.  Currently only one lifetime model anticipated
*/

class lifetime_t
{

public:
	lifetime_t(std::vector<double> DOD_vect, std::vector<double> cycle_vect, int n);
	~lifetime_t();
	output* rainflow(double DOD);
	output* rainflow_finish();

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
	output* _output;

	enum RETURN_CODES
	{
		LT_SUCCESS,
		LT_GET_DATA,
		LT_RERANGE
	};
};

/*
Thermal Base class.
*/
class thermal_t
{

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
	battery_t(capacity_t *, voltage_t *, lifetime_t *, double dt);

	// Run all
	void run(double P, double dT);
	void finish();

	// Run a component level model
	output* runCapacityModel(double P, double V);
	output* runVoltageModel(double dT);
	output* runLifetimeModel(double DOD);

	output* getCapacityOutput();
	output* getLifetimeOutput();
	output* getVoltageOutput();

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
	double _dt;
	bool _firstStep;
	output* _CapacityOutput;
	output* _LifetimeOutput;
	output* _VoltageOutput;
};

#endif

/*
Non-class functions
*/
double life_vs_DOD(double R, double *a, void * user_data);
void getMonthHour(int hourOfYear, int * month, int * hour);