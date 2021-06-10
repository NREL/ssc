/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse
or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <algorithm>
#include <cmath>

#include "lib_battery_lifetime_calendar_cycle.h"
#include "lib_battery_lifetime_nmc.h"
#include "lib_battery_lifetime.h"
#include "logger.h"

void lifetime_nmc_t::initialize() {
    state = std::make_shared<lifetime_state>();
    // cycle model for counting cycles only, no cycle-only degradation
    cycle_model = std::unique_ptr<lifetime_cycle_t>(new lifetime_cycle_t(params, state));
    // do any state initialization here
    state->nmc_li_neg->DOD_min = -1;
    state->nmc_li_neg->DOD_max = -1;
    state->nmc_li_neg->dq_relative_li1 = 0;
    state->nmc_li_neg->dq_relative_li2 = 0;
    state->nmc_li_neg->dq_relative_li3 = 0;
    state->nmc_li_neg->dq_relative_neg = 0;
    state->nmc_li_neg->cum_dt = 0;
    state->nmc_li_neg->b1_dt = 0;
    state->nmc_li_neg->b2_dt = 0;
    state->nmc_li_neg->b3_dt = 0;
    state->nmc_li_neg->c0_dt = 0;
    state->nmc_li_neg->c2_dt = 0;
    state->nmc_li_neg->q_relative_li = 100;
    state->nmc_li_neg->q_relative_neg = 100;
    state->nmc_li_neg->cycle_DOD_range.clear();
    state->nmc_li_neg->cycle_DOD_max.clear();
    state->q_relative = fmin(state->nmc_li_neg->q_relative_li, state->nmc_li_neg->q_relative_neg);
}

lifetime_nmc_t::lifetime_nmc_t(double dt_hr) {
    params = std::make_shared<lifetime_params>();
    params->model_choice = lifetime_params::NMCNREL;
    params->dt_hr = dt_hr;
    initialize();
}

lifetime_nmc_t::lifetime_nmc_t(std::shared_ptr<lifetime_params> params_pt) {
    params = std::move(params_pt);
    initialize();
}

lifetime_nmc_t::lifetime_nmc_t(std::shared_ptr<lifetime_params> params_pt, std::shared_ptr<lifetime_state> state_pt) {
    params = std::move(params_pt);
    state = std::move(state_pt);
    cycle_model = std::unique_ptr<lifetime_cycle_t>(new lifetime_cycle_t(params, state));
}

lifetime_nmc_t::lifetime_nmc_t(const lifetime_nmc_t &rhs) :
        lifetime_t(rhs){
    operator=(rhs);
}

lifetime_nmc_t& lifetime_nmc_t::operator=(const lifetime_nmc_t& rhs) {
    if (this != &rhs) {
        *params = *rhs.params;
        *state = *rhs.state;
    }
    return *this;
}

lifetime_t * lifetime_nmc_t::clone() {
    return new lifetime_nmc_t(*this);
}

double lifetime_nmc_t::calculate_Uneg(double SOC) {
    SOC = fmin(1., fmax(0., SOC));
    size_t prev_ind = floor(SOC * 10);
    size_t next_ind = prev_ind + 1;
    if (next_ind >= unegs.size())
        return unegs.back();
    double Uneg = unegs[prev_ind] + (unegs[next_ind] - unegs[prev_ind]) / 0.1 * (SOC - (double)prev_ind / 10);
    return Uneg;
}

double lifetime_nmc_t::calculate_Voc(double SOC) {
    SOC = fmin(1., fmax(0., SOC));
    size_t prev_ind = floor(SOC * 10);
    size_t next_ind = prev_ind + 1;
    if (next_ind >= ocvs.size())
        return ocvs.back();
    double Voc = ocvs[prev_ind] + (ocvs[next_ind] - ocvs[prev_ind]) / 0.1 * (SOC - (double)prev_ind / 10);
    return Voc;
}

double lifetime_nmc_t::runQli(double T_battery_K) {
    size_t dn_cycles = state->nmc_li_neg->cycle_DOD_range.size();

    double b1 = state->nmc_li_neg->b1_dt;
    double b2 = state->nmc_li_neg->b2_dt;
    double b3 = state->nmc_li_neg->b3_dt;

    state->nmc_li_neg->b1_dt = 0;
    state->nmc_li_neg->b2_dt = 0;
    state->nmc_li_neg->b3_dt = 0;

    // Reversible thermal capacity dependence
    double d0_t = d0_ref * exp(-(Ea_d0_1 / Rug) * (1 / T_battery_K - 1 / T_ref) -
                      (Ea_d0_2 / Rug) * pow(1 / T_battery_K - 1 / T_ref, 2));

    double dQLi1dt = 0;
    if (state->nmc_li_neg->dq_relative_li1 == 0) {
        if (state->day_age_of_battery > 0)
            dQLi1dt = b1 / sqrt(state->day_age_of_battery);
    }
    else
        dQLi1dt = 0.5 * b1 * b1 / state->nmc_li_neg->dq_relative_li1;
    double dQLi2dt = b2 * (double)dn_cycles;
    double dQLi3dt = fmax(0.0, b3 - state->nmc_li_neg->dq_relative_li3) / tau_b3;

    state->nmc_li_neg->dq_relative_li1 += dQLi1dt;
    state->nmc_li_neg->dq_relative_li2 += dQLi2dt;
    state->nmc_li_neg->dq_relative_li3 += dQLi3dt;
    state->nmc_li_neg->q_relative_li = d0_t / Ah_ref  * 100. * (
            b0 - state->nmc_li_neg->dq_relative_li1 - state->nmc_li_neg->dq_relative_li2 - state->nmc_li_neg->dq_relative_li3);
    return state->nmc_li_neg->q_relative_li;
}

double lifetime_nmc_t::runQneg() {

    double c0 = state->nmc_li_neg->c0_dt;
    double c2 = 0;
    for (double i : state->nmc_li_neg->cycle_DOD_range) {
        c2 += pow(i * 0.01, beta_c2);
    }
    c2 *= state->nmc_li_neg->c2_dt;

    state->nmc_li_neg->c0_dt = 0;
    state->nmc_li_neg->c2_dt = 0;

    double dqNeg = 0;
    if (state->nmc_li_neg->dq_relative_neg < c0_ref)
        dqNeg = c2 * c0_ref / (c0_ref - state->nmc_li_neg->dq_relative_neg);

    state->nmc_li_neg->dq_relative_neg += dqNeg;
    state->nmc_li_neg->q_relative_neg = c0 / Ah_ref * (1 - state->nmc_li_neg->dq_relative_neg) * 100;
    return state->nmc_li_neg->q_relative_neg;
}

void lifetime_nmc_t::integrateDegParams(double dt_day, double DOD, double T_battery) {
    // try to predict range of coming cycle if no cycles have yet elapsed
    double DOD_range = state->nmc_li_neg->DOD_max - state->nmc_li_neg->DOD_min;
    if (!state->nmc_li_neg->cycle_DOD_range.empty()) {
        DOD_range = fmax(DOD_range, *std::max_element(state->nmc_li_neg->cycle_DOD_range.begin(), state->nmc_li_neg->cycle_DOD_range.end()) * 0.01);
    }

    double SOC_avg = 0;
    if (state->nmc_li_neg->cycle_DOD_max.empty()) {
        SOC_avg = 1 - DOD * 0.01;
    }
    else {
        for (size_t i = 0; i < state->nmc_li_neg->cycle_DOD_max.size(); i++) {
            double cycle_DOD_max = state->nmc_li_neg->cycle_DOD_max[i] * 0.01;
            double cycle_DOD_rng = state->nmc_li_neg->cycle_DOD_range[i] * 0.01;
            SOC_avg += 1 - (cycle_DOD_max + (cycle_DOD_max - cycle_DOD_rng)) / 2;
        }
        SOC_avg /= (double)state->nmc_li_neg->cycle_DOD_max.size();
    }
    double U_neg = calculate_Uneg(SOC_avg);
    double V_oc = calculate_Voc(SOC_avg);

    // multiply by timestep in days and populate corresponding vectors
    double Arr_b1 = exp(-(Ea_b1 / Rug) * (1. / T_battery - 1. / T_ref));
    double Tfl_b1 = exp((alpha_a_b1 * F / Rug) * (U_neg / T_battery - Uneg_ref / T_ref));
    double b1_dt_el = b1_ref * Arr_b1 * Tfl_b1 * exp(gamma * pow(DOD_range, beta_b1)) * dt_day;

    double Arr_b2 = exp(-(Ea_b2 / Rug) * (1. / T_battery - 1. / T_ref));
    double b2_dt_el = b2_ref * Arr_b2 * dt_day;

    double Arr_b3 = exp(-(Ea_b3 / Rug) * (1. / T_battery - 1. / T_ref));
    double Tfl_b3 = exp((alpha_a_b3 * F / Rug) * (V_oc / T_battery - V_ref / T_ref));
    double b3_dt_el = b3_ref * Arr_b3 * Tfl_b3
                      * (1 + theta * DOD_range) * dt_day;

    state->nmc_li_neg->b1_dt += b1_dt_el;
    state->nmc_li_neg->b2_dt += b2_dt_el;
    state->nmc_li_neg->b3_dt += b3_dt_el;

    // computations for q_neg
    double Arr_c2 = exp(-(Ea_c2 / Rug) * (1. / T_battery - 1. / T_ref));
    double c2_dt_el = c2_ref * Arr_c2 * dt_day;
    double c0_dt_el = c0_ref * exp(-Ea_c0_ref / Rug * (1 / T_battery - 1 / T_ref)) * dt_day;
    state->nmc_li_neg->c0_dt += c0_dt_el;
    state->nmc_li_neg->c2_dt += c2_dt_el;

    state->nmc_li_neg->cum_dt += dt_day;
}

void lifetime_nmc_t::integrateDegLoss(double DOD, double T_battery) {
    state->nmc_li_neg->q_relative_li = runQli(T_battery);
    state->nmc_li_neg->q_relative_neg = runQneg();
    state->q_relative = fmin(state->nmc_li_neg->q_relative_li, state->nmc_li_neg->q_relative_neg);

    // reset cycle tracking
    state->nmc_li_neg->cum_dt = 0;
    state->nmc_li_neg->DOD_min = -1;
    state->nmc_li_neg->DOD_max = -1;
    state->nmc_li_neg->cycle_DOD_max.clear();
    state->nmc_li_neg->cycle_DOD_range.clear();
}

void lifetime_nmc_t::runLifetimeModels(size_t _, bool charge_changed, double prev_DOD, double DOD,
                                       double T_battery) {
    prev_DOD = fmax(fmin(prev_DOD, 100), 0);
    DOD = fmax(fmin(DOD, 100), 0);
    T_battery += 273.15;
    if (state->nmc_li_neg->DOD_min == -1) {
        state->nmc_li_neg->DOD_max = fmax(prev_DOD, DOD) * 0.01;
        state->nmc_li_neg->DOD_min = fmin(prev_DOD, DOD) * 0.01;
    }
    else {
        state->nmc_li_neg->DOD_max = fmax(state->nmc_li_neg->DOD_max, DOD * 0.01);
        state->nmc_li_neg->DOD_min = fmin(state->nmc_li_neg->DOD_min, DOD * 0.01);
    }

    if (charge_changed){
        size_t n_cyc_prev = cycle_model->cycles_elapsed();
        cycle_model->rainflow(prev_DOD);
        if (cycle_model->cycles_elapsed() > n_cyc_prev) {
            state->nmc_li_neg->cycle_DOD_range.emplace_back(cycle_model->cycle_range());
            state->nmc_li_neg->cycle_DOD_max.emplace_back(cycle_model->cycle_depth());
        }
    }

    double dt_day = (1. / (double)util::hours_per_day) * params->dt_hr;
    // Run capacity degradation model after every 24 hours
    double new_cum_dt = state->nmc_li_neg->cum_dt + dt_day;
    if (new_cum_dt > 1 + 1e-7) {
        double dt_day_to_end_of_day = 1 - state->nmc_li_neg->cum_dt;
        double DOD_at_end_of_day = (DOD - prev_DOD) / dt_day * dt_day_to_end_of_day + prev_DOD;
        state->day_age_of_battery += dt_day_to_end_of_day;

        integrateDegParams(dt_day_to_end_of_day, DOD_at_end_of_day, T_battery);
        integrateDegLoss(DOD_at_end_of_day, T_battery);

        dt_day = new_cum_dt - 1;
    }

    state->day_age_of_battery += dt_day;
    integrateDegParams(dt_day, DOD, T_battery);

    if (fabs(state->nmc_li_neg->cum_dt - 1.) < 1e-7) {
        integrateDegLoss(DOD, T_battery);
    }
}

double lifetime_nmc_t::estimateCycleDamage() {
    // Use cumulative damage from cycling and average it over elapsed cycles
    double QLi_cycle_damage = state->nmc_li_neg->dq_relative_li2 / (double)state->n_cycles;
    double QNeg_cycle_damage = state->nmc_li_neg->dq_relative_neg / (double)state->n_cycles;
    return fmax(QLi_cycle_damage, QNeg_cycle_damage) * 100;
}

void lifetime_nmc_t::replaceBattery(double percent_to_replace) {
    state->day_age_of_battery = 0;
    state->nmc_li_neg->DOD_min = -1;
    state->nmc_li_neg->DOD_max = -1;
    state->nmc_li_neg->dq_relative_li1 = 0;
    state->nmc_li_neg->dq_relative_li2 = 0;
    state->nmc_li_neg->dq_relative_li3 = 0;
    state->nmc_li_neg->dq_relative_neg = 0;
    state->nmc_li_neg->q_relative_li += percent_to_replace;
    state->nmc_li_neg->q_relative_neg += percent_to_replace;
    state->nmc_li_neg->q_relative_li = fmin(100, state->nmc_li_neg->q_relative_li);
    state->nmc_li_neg->q_relative_neg = fmin(100, state->nmc_li_neg->q_relative_neg);
    state->nmc_li_neg->cycle_DOD_max.clear();
    state->nmc_li_neg->cycle_DOD_range.clear();
    state->q_relative = fmin(state->nmc_li_neg->q_relative_li, state->nmc_li_neg->q_relative_neg);
    cycle_model->replaceBattery(percent_to_replace);
    state->cycle->q_relative_cycle = 0;
}
