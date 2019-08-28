#ifndef SYSTEM_ADVISOR_MODEL_LIB_STORAGE_CAPACITY_H
#define SYSTEM_ADVISOR_MODEL_LIB_STORAGE_CAPACITY_H

const double low_tolerance = 0.01;
const double tolerance = 0.001;

struct capacity_state_params{
    double q0;  // [Ah] - Total capacity at timestep
    double qmax; // [Ah] - maximum possible capacity
    double qmax_thermal; // [Ah] - maximum capacity adjusted for temperature affects
    double I;   // [A]  - Current draw during last step
    double I_loss; // [A] - Lifetime and thermal losses
    double SOC; // [%] - State of Charge
    double DOD; // [%] - Depth of Discharge
    double DOD_prev; // [%] - Depth of Discharge of previous step
    bool chargeChange; // [true/false] - indicates if charging state has changed since last step
    int prev_charge; // {CHARGE, NO_CHARGE, DISCHARGE}
    int charge; // {CHARGE, NO_CHARGE, DISCHARGE}
};

/*
Base class from which capacity models derive
Note, all capacity models are based on the capacity of one battery
*/
class voltage_t;
class capacity_t
{
public:

    capacity_t();
    capacity_t(double q, double SOC_init, double SOC_max, double SOC_min);

    // deep copy
    virtual capacity_t * clone() = 0;

    // shallow copy from capacity to this
    virtual void copy(capacity_t *);

    // virtual destructor
    virtual ~capacity_t(){};

    // pure virtual functions (abstract) which need to be defined in derived classes
    virtual void updateCapacity(double &I, double dt) = 0;
    virtual void updateCapacityForThermal(double capacity_percent)=0;
    virtual void updateCapacityForLifetime(double capacity_percent)=0;
    virtual void replace_battery()=0;

    virtual double q1() = 0; // available charge
    virtual double q10() = 0; // capacity at 10 hour discharge rate

    capacity_state_params getState() { return _state; }

    void check_charge_change();
    void check_SOC();
    void update_SOC();

    // common outputs
    double SOC();
    double DOD_max();
    double DOD();
    double prev_DOD();
    double q0();
    double qmax();
    double qmax_thermal();
    double I();
    bool chargeChanged();
    double I_loss();
    int charge_operation();

    enum { CHARGE, NO_CHARGE, DISCHARGE };

protected:
    capacity_state_params _state;

    double _qmax0; // [Ah] - original maximum capacity
    double _SOC_init; // [%] - Initial SOC
    double _SOC_max; // [%] - Maximum SOC
    double _SOC_min; // [%] - Minimum SOC
    double _dt_hour; // [hr] - Timestep in hours
};

/*
KiBaM specific capacity model
*/

struct kibam_struct_params{
    double q1_0; // [Ah] - charge available
    double q2_0; // [Ah] - charge bound
    double q10; //  [Ah] - Capacity at 10 hour discharge rate
    double q20; // [Ah] - Capacity at 20 hour discharge rate
    double I20; // [A]  - Current at 20 hour discharge rate
};

class capacity_kibam_t : public capacity_t
{
public:

    // Public APIs
    capacity_kibam_t();
    capacity_kibam_t(double q20, double t1, double q1, double q10, double SOC_init, double SOC_max, double SOC_min);
    ~capacity_kibam_t(){}

    // deep copy
    capacity_kibam_t * clone();

    // copy from capacity to this
    void copy(capacity_t *);

    void updateCapacity(double &I, double dt);
    void updateCapacityForThermal(double capacity_percent);
    void updateCapacityForLifetime(double capacity_percent);
    void replace_battery();
    double q1(); // Available charge
    double q2(); // Bound charge
    double q10(); // Capacity at 10 hour discharge rate
    double q20(); // Capacity at 20 hour discharge rate

    kibam_struct_params getKibamState() {return _kibam_state; }
    void setState(capacity_state_params cap_state, kibam_struct_params kibam_state) {
        _state = capacity_state_params(cap_state);
        _kibam_state = kibam_struct_params(kibam_state);
    }

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
    kibam_struct_params _kibam_state;
};

/*
Lithium Ion specific capacity model
*/
class capacity_lithium_ion_t : public capacity_t
{
public:
    capacity_lithium_ion_t();
    capacity_lithium_ion_t(double q, double SOC_init, double SOC_max, double SOC_min);
    ~capacity_lithium_ion_t(){};

    // deep copy
    capacity_lithium_ion_t * clone();

    // copy from capacity to this
    void copy(capacity_t *);

    // override public api
    void updateCapacity(double &I, double dt);
    void updateCapacityForThermal(double capacity_percent);
    void updateCapacityForLifetime(double capacity_percent);
    void replace_battery();

    double q1(); // Available charge
    double q10(); // Capacity at 10 hour discharge rate

    void setState(capacity_state_params cap_state) {
        _state = capacity_state_params(cap_state);
    }

protected:
};


#endif //SYSTEM_ADVISOR_MODEL_LIB_STORAGE_CAPACITY_H
