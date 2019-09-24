#ifndef SYSTEM_ADVISOR_MODEL_LIB_RESILIENCE_H
#define SYSTEM_ADVISOR_MODEL_LIB_RESILIENCE_H


#include "../ssc/cmod_battery.h"

class resilience {
private:
    battstor* batt;

    int days_survived;

public:
    explicit resilience(battstor* battery){
        batt = new battstor(*battery);
        if (auto auto_btm = dynamic_cast<dispatch_automatic_behind_the_meter_t*>(batt->dispatch_model)){

         }
        days_survived = -1;
    }

    void simulate_outage(){
        double batt_capacity = 1;
    }

    void compute_metrics(){

    }

    int get_outage_days_survived() {
        return days_survived;
    }
};


#endif //SYSTEM_ADVISOR_MODEL_LIB_RESILIENCE_H
