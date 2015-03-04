#ifndef battery_h
#define battery_h

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


/*
Base class from which capacity models derive
*/

class capacity_t
{
public:
	capacity_t();
	capacity_t(double q20, double I20, double V);
	virtual output* updateCapacity(double P, double V, double dt)=0;


protected:
	double _q20;
	double _I20;
	double _V;
	double _SOC;
	double _DOD;
	output *_output;
};

/*
KiBaM specific capacity model
*/
class capacity_kibam_t : public capacity_t
{
public:

	~capacity_kibam_t();

	// common (inherited but re-defined)
	output* updateCapacity(double P, double V, double dt);

	// unique to kibam
	capacity_kibam_t(double q20, double I20, double V, double t1, double t2, double q1, double q2);
	double c_compute(double F, double t1, double t2, double k_guess);
	double q1_compute(double q10, double q0, double dt, double I); // may remove some inputs, use class variables
	double q2_compute(double q20, double q0, double dt, double I); // may remove some inputs, use class variables
	double Icmax_compute(double q10, double q0, double dt);
	double Idmax_compute(double q10, double q0, double dt);
	double qmax_compute();
	double qmax_of_i_compute(double T);
	void parameter_compute();
	
protected:

	// parameters for finding c, k, qmax
	double _t1;
	double _t2;
	double _q1;
	double _q2;
	double _F1;
	double _F2;

	// model parameters
	double _c;
	double _k;
	double _qmax;

	// charge which changes with time
	double _q1_0; // charge available
	double _q2_0; // charge bound
	double _q0;   // total charge
	double _qmaxI;// theoretical max charge at this current
};


/* 
Lifetime class.  Currently only one lifetime model anticipated
*/
/*
class lifetime_t
{

public:
	lifetime_t( double * DOD_vect, double * cycle_vect );
	double life_vs_DOD(double R);
	double rainflow(double DOD);
	double rainflow_finish();

protected:
	void rainflow_ranges();
	int rainflow_compareRanges();

	double * _DOD_vect;
	double *_cycle_vect;
	double *_a;
	double _nCycles;
	double _Dlt; 
	double _jlt;
	double _klt; 
	double _Xlt;
	double _Ylt;
	double _Slt;
	double * _Peaks;
	double _Range;
	int _S_rereadCount;
};
*/

/*
Class which encapsulates a battery and all its models
*/

class battery_t
{
public:
	battery_t();
	battery_t(capacity_t *, double dt);
	output* runCapacityModel(double P, double V);

private:
	capacity_t * _capacity;
	double _dt;
};

#endif