#ifndef SYSTEM_ADVISOR_MODEL_LIB_STORAGE_CAPACITY_H
#define SYSTEM_ADVISOR_MODEL_LIB_STORAGE_CAPACITY_H

#include <memory>

extern double tolerance;
extern double low_tolerance;

struct capacity_state{
    double q0;  // [Ah] - Total capacity at timestep
    double qmax_lifetime; // [Ah] - maximum possible capacity
    double qmax_thermal; // [Ah] - maximum capacity adjusted for temperature affects
    double I;   // [A]  - Current draw during last step
    double I_loss; // [A] - Lifetime and thermal losses
    double SOC; // [%] - State of Charge
    double DOD; // [%] - Depth of Discharge
    double DOD_prev; // [%] - Depth of Discharge of previous step
    int charge_mode; // {CHARGE, NO_CHARGE, DISCHARGE}
    int prev_charge; // {CHARGE, NO_CHARGE, DISCHARGE}
    bool chargeChange; // [true/false] - indicates if charging state has changed since last step

    struct {
        double q1_0; // [Ah] - charge available
        double q2_0; // [Ah] - charge bound
        double q1;  // [Ah]- capacity at discharge rate t1
        double q2;  // [Ah] - capacity at discharge rate t2
    } leadacid;

    friend std::ostream& operator<<(std::ostream& os , const capacity_state& p);
};

struct capacity_params{
    double qmax_init; // [Ah] - original maximum capacity
    double SOC_init; // [%] - Initial SOC
    double SOC_max; // [%] - Maximum SOC
    double SOC_min; // [%] - Minimum SOC
    double dt_hr; // [hr] - Timestep in hours

    struct {
        // parameters for finding c, k, qmax
        double t1;  // [h] - discharge rate for capacity at _q1
        double t2;  // [h] - discharge rate for capacity at _q2
        double F1;  // [unitless] - internal ratio computation
        double F2;  // [unitless] - internal ratio computation

        double q10; //  [Ah] - Capacity at 10 hour discharge rate
        double q20; // [Ah] - Capacity at 20 hour discharge rate
        double I20; // [A]  - Current at 20 hour discharge rate
    } leadacid;

    friend std::ostream& operator<<(std::ostream& os , const capacity_params& p);
};

/*
Base class from which capacity models derive
Note, all capacity models are based on the capacity of one battery
*/
class capacity_t
{
public:

    capacity_t();
    capacity_t(double q, double SOC_init, double SOC_max, double SOC_min, double dt_hour);
    capacity_t(const capacity_t& rhs);

    virtual capacity_t& operator= (const capacity_t& rhs);

    virtual capacity_t* clone() = 0;

    virtual ~capacity_t() = default;

    // pure virtual functions (abstract) which need to be defined in derived classes
    virtual void updateCapacity(double &I, double dt) = 0;
    virtual void updateCapacityForThermal(double capacity_percent)=0;
    virtual void updateCapacityForLifetime(double capacity_percent)=0;
    virtual void replace_battery(double replacement_percent)=0;

    void change_SOC_limits(double min, double max){
        params->SOC_min = min;
        params->SOC_max = max;
    }

    virtual double q1() = 0; // available charge
    virtual double q10() = 0; // capacity at 10 hour discharge rate

    void check_charge_change();
    void check_SOC();
    void update_SOC();

    // common outputs
    double SOC_min();
    double SOC_max();
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

    std::shared_ptr<capacity_params> params;
    capacity_state state{};
};

/*
KiBaM specific capacity model
*/
class capacity_kibam_t : public capacity_t
{
public:

    // Public APIs
    capacity_kibam_t();
    capacity_kibam_t(double q20, double t1, double q1, double q10, double SOC_init, double SOC_max, double SOC_min, double dt_hr);
    capacity_kibam_t(const capacity_kibam_t& rhs);

    capacity_kibam_t& operator= (const capacity_t& rhs) override;

    capacity_t* clone() override;

    ~capacity_kibam_t() override = default;

    void updateCapacity(double &I, double dt) override;
    void updateCapacityForThermal(double capacity_percent) override;
    void updateCapacityForLifetime(double capacity_percent) override;
    void replace_battery(double replacement_percent) override;
    double q1() override; // Available charge
    double q2(); // Bound charge
    double q10() override; // Capacity at 10 hour discharge rate
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

    // model parameters
    double c;  // [0-1] - capacity fraction
    double k;  // [1/hour] - rate constant
};

/*
Lithium Ion specific capacity model
*/
class capacity_lithium_ion_t : public capacity_t
{
public:
    capacity_lithium_ion_t();
    capacity_lithium_ion_t(double q, double SOC_init, double SOC_max, double SOC_min, double dt_hr);
    capacity_lithium_ion_t(const capacity_lithium_ion_t& rhs);

    capacity_lithium_ion_t& operator= (const capacity_t& rhs) override;

    capacity_t* clone() override;

    ~capacity_lithium_ion_t() override = default;

    void updateCapacity(double &I, double dt) override;
    void updateCapacityForThermal(double capacity_percent) override;
    void updateCapacityForLifetime(double capacity_percent) override;
    void replace_battery(double replacement_percent) override;

    double q1() override; // Available charge
    double q10() override; // Capacity at 10 hour discharge rate
};



#endif //SYSTEM_ADVISOR_MODEL_LIB_STORAGE_CAPACITY_H
