#include "logger.h"

#include "lib_battery_capacity.h"
#include "lib_battery_voltage.h"

/**
* Helper fx
*/

std::ostream& operator<<(std::ostream& os , const voltage_params& p){
    char buf[256];
    sprintf(buf, "voltage_params: { \"mode\": %d, \"num_cell_series\": %d, \"num_strings\": %d, "
                 "\"cell_voltage_nominal\": %.3f, \"R\": %.3f, \"dt_hr\": %.3f, "
                 "\"dynamic\": { \"Vfull\": %.3f, \"Vexp\": %.3f, \"Vnom\": %.3f, "
                 "\"Qfull\": %.3f, \"Qexp\": %.3f, \"Qnom\": %.3f, \"C_rate\": %.3f} }", p.mode, p.num_cells_series, p.num_strings,
            p.cell_voltage_nominal, p.R, p.dt_hr,
            p.dynamic.Vfull, p.dynamic.Vexp, p.dynamic.Vnom,
            p.dynamic.Qfull, p.dynamic.Qexp, p.dynamic.Qnom, p.dynamic.C_rate);
    os << buf;
}

std::ostream& operator<<(std::ostream& os , const voltage_state& p){
    char buf[128];
    sprintf(buf, "voltage_state: { \"cell_voltage\": %.3f }", p.cell_voltage);
    os << buf;
}

std::ostream& operator<<(std::ostream& os , const capacity_state& p){
    char buf[256];
    sprintf(buf, "capacity_params: { \"q0\": %.2f, \"qmax_lifetime\": %.2f, \"qmax_thermal\": %.2f, \"I\": %.3f, "
                 "\"I_loss\": %.3f, \"SOC\": %.3f, \"DOD\": %.3f, \"DOD_prev\": %.3f, "
                 "\"charge_mode\": %d, \"prev_charge\": %d, \"chargeChange\": %d, "
                 "\"leadacid\": { \"q1_0\": %.3f, \"q2_0\": %.3f, \"q1\": %.3f, \"q2\": %.3f, ",
            p.q0, p.qmax_lifetime, p.qmax_thermal, p.I,
            p.I_loss, p.SOC, p.DOD, p.DOD_prev,
            p.charge_mode, p.prev_charge, p.chargeChange,
            p.leadacid.q1_0, p.leadacid.q2_0, p.leadacid.q1, p.leadacid.q2);
    os << buf;
}

std::ostream& operator<<(std::ostream& os , const capacity_params& p){
    char buf[256];
    sprintf(buf, "capacity_params: { \"qmax_init\": %.2f, \"SOC_init\": %.2f, \"SOC_max\": %.2f, "
                 "\"SOC_min\": %.3f, \"dt_hr\": %.3f, "
                 "\"leadacid\": { \"t1\": %.3f, \"t2\": %.3f, \"F1\": %.3f, \"F2\": %.3f, "
                 "\"q10\": %.3f, \"q20\": %.3f, \"I20\": %.3f} }", p.qmax_init, p.SOC_init, p.SOC_max,
            p.SOC_min, p.dt_hr, p.leadacid.t1, p.leadacid.t2, p.leadacid.F1, p.leadacid.F2,
            p.leadacid.q10, p.leadacid.q20, p.leadacid.I20);
    os << buf;
}
