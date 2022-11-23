/*
BSD 3-Clause License

Copyright (c) Alliance for Sustainable Energy, LLC. See also https://github.com/NREL/ssc/blob/develop/LICENSE
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


#ifndef __LIB_BATTERY_PROPERTIES_H__
#define __LIB_BATTERY_PROPERTIES_H__

#include <gtest/gtest.h>

#include <lib_util.h>
#include <lib_utility_rate.h>


// Generic Lithium-ion battery system to be re-used
class BatteryProperties : public ::testing::Test
{
public:

	// capacity
	double q;
	double SOC_min;
	double SOC_max;
	double SOC_init;

	// voltage
	double Vnom_default;
	double Vfull;
    double Vcut;
	double Vexp;
	double Vnom;
	double Qfull;
	double Qexp;
	double Qnom;
	double C_rate;
	double resistance;

	// lifetime
	util::matrix_t<double> cycleLifeMatrix;
	util::matrix_t<double> calendarLifeMatrix;
	int calendarChoice;
	int replacementOption = 0;
	double replacementCapacity = 0;
	double calendar_q0 = 1.02;
	double calendar_a = 2.66e-3;
	double calendar_b = -7280;
	double calendar_c = 930;

	// thermal
	double mass;
	double Cp;
	double h;
	std::vector<double> T_room;
	util::matrix_t<double> capacityVsTemperature;

	// losses
	std::vector<double> monthlyLosses;
	std::vector<double> fullLosses;
	std::vector<double> fullLossesMinute;
	int lossChoice;

	// battery
	int chemistry;

	void SetUp() override
	{
		// cell capacity
		q = 2.25;
		SOC_init = 50;
		SOC_min = 15;
		SOC_max = 95;

		// voltage
		Vnom_default = 3.6;
		Vfull = 4.1;
		Vexp = 4.05;
		Vnom = 3.4;
        Vcut = 0.66 * Vfull;
		Qfull = 2.25;
		Qexp = 0.04;
		Qnom = 2.0;
		C_rate = 0.2;
		resistance = 0.0002;

		// lifetime
		double vals[] = { 20, 0, 100, 20, 5000, 80, 20, 10000, 60, 80, 0, 100, 80, 1000, 80, 80, 2000, 60 };
		cycleLifeMatrix.assign(vals, 6, 3);
		double vals2[] = { 0, 100, 3650, 80, 7300, 50 };
		calendarLifeMatrix.assign(vals2, 3, 2);

		calendarChoice = calendar_cycle_params::CALENDAR_CHOICE::MODEL;

		// thermal
		mass = 507;
		Cp = 1004;
		h = 20;
		for (size_t i = 0; i < 8760; i++) {
			T_room.push_back(20);
		}
		double vals3[] = { 0, 60, 1, 100, 25, 100, 40, 100 };
		capacityVsTemperature.assign(vals3, 4, 2);

		// losses
		for (size_t m = 0; m < 12; m++) {
			monthlyLosses.push_back((double)m);
		}
		for (size_t i = 0; i < 8760; i++) {
			fullLosses.push_back(0);
		}
		for (size_t i = 0; i < 8760 * 60; i++) {
			fullLossesMinute.push_back(0);
		}
		lossChoice = losses_params::MONTHLY;

		// battery
		chemistry = 1;
	}
};

#endif
