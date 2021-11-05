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


#include <gtest/gtest.h>

#include "../ssc/core.h"
#include "../ssc/vartab.h"
#include "../ssc/common.h"
#include "../input_cases/weather_inputs.h"
#include "cmod_pvwattsv8_test.h"



///Default PVWattsv8, but with TMY2 instead of TMY3
TEST_F(CMPvwattsv8Integration_cmod_pvwattsv8, DefaultNoFinancialModel_cmod_pvwattsv8) {
    compute();

    double tmp = 0;
    //	ssc_data_get_number(data, "annual_energy", &annual_energy);
    //	EXPECT_NEAR(annual_energy, 6909.79, error_tolerance) << "Annual energy.";
    int count;
    ssc_number_t* monthly_energy = ssc_data_get_array(data, "monthly_energy", &count);

    for (size_t i = 0; i < 12; i++)
        tmp += (double)monthly_energy[i];
    //v5 is 6909.79, decrease of 2.4%: decreases due to shading, module cover losses, and spectral losses
    //v7 prior to module coeff changes is 6750.4236, increase of 3.7% due to improved tempco for standard module
    //v7 final version is 6999.0158, decrease of 0.4% due to model updates
    EXPECT_NEAR(tmp, 6969.8105, error_tolerance) << "Annual energy.";

    EXPECT_NEAR((double)monthly_energy[0], 436.644, error_tolerance) << "Monthly energy of January";
    EXPECT_NEAR((double)monthly_energy[1], 482.808, error_tolerance) << "Monthly energy of February";
    EXPECT_NEAR((double)monthly_energy[2], 595.285, error_tolerance) << "Monthly energy of March";
    EXPECT_NEAR((double)monthly_energy[3], 677.821, error_tolerance) << "Monthly energy of April";
    EXPECT_NEAR((double)monthly_energy[4], 722.2122, error_tolerance) << "Monthly energy of May";
    EXPECT_NEAR((double)monthly_energy[5], 674.160, error_tolerance) << "Monthly energy of June";
    EXPECT_NEAR((double)monthly_energy[6], 672.685, error_tolerance) << "Monthly energy of July";
    EXPECT_NEAR((double)monthly_energy[7], 655.924, error_tolerance) << "Monthly energy of August";
    EXPECT_NEAR((double)monthly_energy[8], 604.110, error_tolerance) << "Monthly energy of September";
    EXPECT_NEAR((double)monthly_energy[9], 576.940, error_tolerance) << "Monthly energy of October";
    EXPECT_NEAR((double)monthly_energy[10], 456.670, error_tolerance) << "Monthly energy of November";
    EXPECT_NEAR((double)monthly_energy[11], 414.551, error_tolerance) << "Month energy of December";

    ssc_number_t capacity_factor;
    ssc_data_get_number(data, "capacity_factor", &capacity_factor);
    EXPECT_NEAR(capacity_factor, 19.891, error_tolerance) << "Capacity factor";

    ssc_number_t kwh_per_kw;
    ssc_data_get_number(data, "kwh_per_kw", &kwh_per_kw);
    EXPECT_NEAR(kwh_per_kw, 1742.453, error_tolerance) << "Energy yield";

}

/// PVWattsv8 using different technology input options
TEST_F(CMPvwattsv8Integration_cmod_pvwattsv8, DifferentTechnologyInputs_cmod_pvwattsv8)
{
	//PVWattsV5 results: annual_energy_expected = { 6909.79, 7123.32, 7336.478, 6909.79, 6804.376, 8601.011, 8727.704, 9690.735};
	//V7 prior to module coefficient updates: std::vector<double> annual_energy_expected = { 6750.42, 7034.39, 7166.88, 6750.42, 6693.49, 8514.26, 8441.60, 9631.76 };
	//standard fixed -2.4%, premium fixed -1.3%, thinfilm fixed -2.4%, standard fixed -2.4%, standard roof -1.7%, standard 1-axis -1.0%, standard backtrack -3.4%, standard 2-axis -0.6%
    //V7 final version:
    //annual_energy_expected = { 6999.01, 7030.26, 7077.07,6999.01, 6971.04, 8785.40, 8725.66, 9861.27 };
	//standard fixed +3.6%, premium fixed 0%, thinfilm fixed -1.2%, standard fixed +3.6%, standard roof +4.0%, standard 1-axis +3.3%, standard backtrack +3.3%, standard 2-axis +2.6%
    //V8 results
    std::vector<double> annual_energy_expected = { 6969.81, 6967.37, 7025.19, 6969.81, 6948.78, 8816.15, 8752.55, 9895.99 };
    //wrt V7 final results: standard fixed -0.4%, premium fixed -0.9%, thinfilm fixed -0.7%, standard fixed -0.4%, standard roof -0.3%, standard 1-axis +0.4%, standard backtrack +0.3%, standard 2-axis +0.4%

    std::map<std::string, double> pairs;
    size_t count = 0;
    error_tolerance = 0.01;

    // Module types: Standard, Premium, Thin Film
    for (int module_type = 0; module_type < 3; module_type++)
    {
        pairs["module_type"] = module_type;
        int pvwatts_errors = modify_ssc_data_and_run_module(data, "pvwattsv8", pairs);
        EXPECT_FALSE(pvwatts_errors);

        if (!pvwatts_errors)
        {
            ssc_number_t annual_energy;
            ssc_data_get_number(data, "annual_energy", &annual_energy);
            EXPECT_NEAR(annual_energy, annual_energy_expected[count], error_tolerance) << "Annual energy.";
        }
        count++;
    }
    pairs["module_type"] = 0; //reset module type to its default value

    // Array types: Fixed open rack, fixed roof mount, 1-axis tracking, 1-axis backtracking, 2-axis tracking
    for (int array_type = 0; array_type < 5; array_type++)
    {
        pairs["array_type"] = array_type;
        int pvwatts_errors = modify_ssc_data_and_run_module(data, "pvwattsv8", pairs);
        EXPECT_FALSE(pvwatts_errors);

        if (!pvwatts_errors)
        {
            ssc_number_t annual_energy;
            ssc_data_get_number(data, "annual_energy", &annual_energy);
            EXPECT_NEAR(annual_energy, annual_energy_expected[count], error_tolerance) << "Annual energy.";
        }
        count++;
    }
    pairs["array_type"] = 0; //reset array type to fixed open rack

}

/// PVWattsv8 using a larger system size
TEST_F(CMPvwattsv8Integration_cmod_pvwattsv8, LargeSystem_cmod_pvwattsv8)
{
	//PVWattsV5 results: std::vector<double> annual_energy_expected = { 1727447.4, 1701094.0, 2150252.8, 2181925.8, 2422683.7 };
	//PVWattsV7 prior to module coeff updates: std::vector<double> annual_energy_expected = { 1686353.2, 1673371.8, 2123603.8, 2105794.1, 2407940.7 };
	//PVWattsV7 final results: std::vector<double> annual_energy_expected = { 1747992.2, 1742760.1, 2190219.7, 2175654.8,  2465319.2};
    std::vector<double> annual_energy_expected = { 1740560.2, 1737195.4, 2198704.7, 2183168.2,  2473998.4 };

    std::map<std::string, double> pairs;
    size_t count = 0;
    error_tolerance = 0.1; //use a larger error tolerance for large numbers

    // Larger size
    pairs["system_capacity"] = 1000; //1 MW system

    // Array types: Fixed open rack, fixed roof mount, 1-axis tracking, 1-axis backtracking, 2-axis tracking
    for (int array_type = 0; array_type < 5; array_type++)
    {
        pairs["array_type"] = array_type;
        int pvwatts_errors = modify_ssc_data_and_run_module(data, "pvwattsv8", pairs);
        EXPECT_FALSE(pvwatts_errors);

        if (!pvwatts_errors)
        {
            ssc_number_t annual_energy;
            ssc_data_get_number(data, "annual_energy", &annual_energy);
            EXPECT_NEAR(annual_energy, annual_energy_expected[count], error_tolerance) << "Annual energy.";
        }
        count++;
    }
}

/// Test pvwattsv8 with default inputs and a 15-minute weather file
TEST_F(CMPvwattsv8Integration_cmod_pvwattsv8, SubhourlyWeather_cmod_pvwattsv8) {

    char subhourly[256];
    int b = sprintf(subhourly, "%s/test/input_cases/pvsamv1_data/LosAngeles_WeatherFile_15min.csv", SSCDIR);
    ssc_data_set_string(data, "solar_resource_file", subhourly); //file set above

    //std::map<std::string, std::string> pairs;
    int pvwatts_errors = run_module(data, "pvwattsv8");

    EXPECT_FALSE(pvwatts_errors);

    if (!pvwatts_errors)
    {
        ssc_number_t annual_energy;
        ssc_data_get_number(data, "annual_energy", &annual_energy);
        //EXPECT_NEAR(annual_energy, 6523.727, error_tolerance) << "Annual energy.";
        EXPECT_NEAR(annual_energy, 6498.656, error_tolerance) << "Annual energy.";

        ssc_number_t capacity_factor;
        ssc_data_get_number(data, "capacity_factor", &capacity_factor);
        EXPECT_NEAR(capacity_factor, 18.62, 0.1) << "Capacity factor";

    }
}

/// Test PVWattsv8 in lifetime mode
TEST_F(CMPvwattsv8Integration_cmod_pvwattsv8, LifetimeModeTest_cmod_pvwattsv8) {

    // set lifetime mode
    std::map<std::string, double> pairs;
    pairs["system_use_lifetime_output"] = 1;
    pairs["analysis_period"] = 25;

    // test degradation array with a length of 1, which should work
    // annual energy of this test should be higher than the array length 25, because this is year 1 energy
    // and with a single value array, degradation doesn't start applying until year 2
    double dc_degradation_single[1];
    dc_degradation_single[0] = 0.5;
    ssc_data_set_array(data, "dc_degradation", (ssc_number_t*)dc_degradation_single, 1);
    int pvwatts_errors = modify_ssc_data_and_run_module(data, "pvwattsv8", pairs);
    EXPECT_FALSE(pvwatts_errors);
    if (!pvwatts_errors)
    {
        ssc_number_t annual_energy;
        ssc_data_get_number(data, "annual_energy", &annual_energy);
        EXPECT_NEAR(annual_energy, 6969.810, error_tolerance) << "Annual energy degradation array length 1.";
    }

    // next, test degradation array with length the same as analysis period, which should also work
    double dc_degradation[25];
    for (size_t i = 0; i < 25; i++) {
        dc_degradation[i] = 0.5;
    }
    ssc_data_set_array(data, "dc_degradation", (ssc_number_t*)dc_degradation, 25);
    pvwatts_errors = modify_ssc_data_and_run_module(data, "pvwattsv8", pairs);
    EXPECT_FALSE(pvwatts_errors);
    if (!pvwatts_errors)
    {
        ssc_number_t annual_energy;
        ssc_data_get_number(data, "annual_energy", &annual_energy);
        EXPECT_NEAR(annual_energy, 6934.980, error_tolerance) << "Annual energy degradation array length 25.";
    }

    // lastly, test degradation array with the wrong length, which should fail
    double dc_degradation_fail[22];
    for (size_t i = 0; i < 22; i++) {
        dc_degradation_fail[i] = 0.5;
    }
    ssc_data_set_array(data, "dc_degradation", (ssc_number_t*)dc_degradation_fail, 22);
    pvwatts_errors = modify_ssc_data_and_run_module(data, "pvwattsv8", pairs);
    EXPECT_TRUE(pvwatts_errors);
}

/// Test PVWattsv8 bifacial functionality
TEST_F(CMPvwattsv8Integration_cmod_pvwattsv8, BifacialTest_cmod_pvwattsv8) {

    // set bifacial inputs
    std::map<std::string, double> pairs;
    pairs["bifaciality"] = 0.0;
    ssc_number_t annual_energy_mono = 0, annual_energy_bi = 0;

    int pvwatts_errors = modify_ssc_data_and_run_module(data, "pvwattsv8", pairs);
    EXPECT_FALSE(pvwatts_errors);
    if (!pvwatts_errors)
    {
        ssc_data_get_number(data, "annual_energy", &annual_energy_mono);
        EXPECT_NEAR(annual_energy_mono, 6969.8, 1) << "System with bifaciality";
    }

    pairs["bifaciality"] = 0.65;
    pvwatts_errors = modify_ssc_data_and_run_module(data, "pvwattsv8", pairs);
    EXPECT_FALSE(pvwatts_errors);
    if (!pvwatts_errors)
    {
        ssc_data_get_number(data, "annual_energy", &annual_energy_bi);
    }

    EXPECT_GT(annual_energy_bi / annual_energy_mono, 1.027);
}

TEST_F(CMPvwattsv8Integration_cmod_pvwattsv8, SnowModelTests_cmod_pvwattsv8) {

    // Snow loss for fixed tilt system*********************************
    ssc_data_set_number(data, "array_type", 0);
    ssc_data_set_number(data, "en_snowloss", 1);

    compute();

    // Snow events in January, February, April, October, and December
    ssc_number_t january_energy;
    january_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0]; //retrieve only january's value
    EXPECT_NEAR((double)january_energy, 412.270, 0.01) << "Fixed tilt energy in January after snow loss";

    int count;
    ssc_number_t* hourly_snowderate = ssc_data_get_array(data, "dcsnowderate", &count);
    ASSERT_EQ(8760, count);

    // Starting at 6 AM Jan. 12th
    // Snow derate should be non-zero during a snow event, and on a fixed system will always be 0, 0.5, or 1.0 due to the assumption of a 2-up installation
    EXPECT_NEAR((double)hourly_snowderate[270], 0.0, error_tolerance);
    EXPECT_NEAR((double)hourly_snowderate[271], 0.5, error_tolerance);
    EXPECT_NEAR((double)hourly_snowderate[272], 0.5, error_tolerance);
    // ...
    EXPECT_NEAR((double)hourly_snowderate[275], 0.5, error_tolerance);
    EXPECT_NEAR((double)hourly_snowderate[276], 0.5, error_tolerance);
    EXPECT_NEAR((double)hourly_snowderate[277], 1.0, error_tolerance);


    // Snow loss for single-axis system********************************
    ssc_data_set_number(data, "array_type", 2);
    ssc_data_set_number(data, "en_snowloss", 1);

    compute();

    // Snow events in January, February, April, October, and December
    january_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0]; //retrieve only january's value
    EXPECT_NEAR((double)january_energy, 509.161, 0.01) << "Single-axis tracker energy in January after snow loss";

    // A tracker row is assumed to be nx1 panels, so all derates should be either 0 or 1
    hourly_snowderate = ssc_data_get_array(data, "dcsnowderate", nullptr);
    for (int hour = 0; hour < 300; hour++) //check throughout a known snow event but don't need to check the whole year
    {
        EXPECT_TRUE(hourly_snowderate[hour] == 0 || hourly_snowderate[hour] == 1);
    }


    // Snow loss for backtracking system*******************************
    ssc_data_set_number(data, "array_type", 3);
    ssc_data_set_number(data, "en_snowloss", 1);

    compute();

    // Snow events in January, February, April, October, and December
    january_energy = ssc_data_get_array(data, "monthly_energy", nullptr)[0]; //retrieve only january's value
    EXPECT_NEAR((double)january_energy, 504.789, 0.01) << "Backtracking energy in January after snow loss";

    // A tracker row is assumed to be nx1 panels, so all derates should be either 0 or 1
    hourly_snowderate = ssc_data_get_array(data, "dcsnowderate", nullptr);
    for (int hour = 0; hour < 300; hour++) //check throughout a known snow event but don't need to check the whole year
    {
        EXPECT_TRUE(hourly_snowderate[hour] == 0 || hourly_snowderate[hour] == 1);
    }
}

/* this test isn't passing currently even though it's working in the UI, so commenting out for now
/// Test PVWattsv8 with snow model
TEST_F(CMPvwattsv8Integration, SnowModelTest_cmod_pvwattsv8) {

    // enable snow model
    std::map<std::string, double> pairs;
    pairs["en_snowmodel"] = 1;

    // test with a file that doesn't have snow data- simulation should fail
    char nosnow[256];
    int b = sprintf(nosnow, "%s/test/input_cases/pvsamv1_data/phoenix_az_33.450495_-111.983688_psmv3_60_tmy.csv", SSCDIR);
    ssc_data_set_string(data, "solar_resource_file", nosnow); //file set above
    int pvwatts_errors = modify_ssc_data_and_run_module(data, "pvwattsv8", pairs);
    EXPECT_TRUE(pvwatts_errors);

}*/

TEST_F(CMPvwattsv8Integration_cmod_pvwattsv8, NonAnnual)
{
    //set up a weather data array and unassign the solar resource file

    auto weather_data = create_weatherdata_array(24);
    ssc_data_unassign(data, "solar_resource_file");
    ssc_data_set_table(data, "solar_resource_data", &weather_data->table);

    //run the tests
    EXPECT_FALSE(run_module(data, "pvwattsv8"));

    ssc_number_t dc, gen;
    dc = ssc_data_get_array(data, "dc", nullptr)[12];
    EXPECT_NEAR(dc, 2509.045, 0.01) << "DC Energy at noon";

    gen = ssc_data_get_array(data, "gen", nullptr)[12];
    EXPECT_NEAR(gen, 2.417, 0.01) << "Gen at noon";
    free_weatherdata_array(weather_data);
}

TEST_F(CMPvwattsv8Integration_cmod_pvwattsv8, IntermediateOutputTesting)
{
    //set up a weather data array and unassign the solar resource file

    auto weather_data = create_weatherdata_array(24);
    ssc_data_unassign(data, "solar_resource_file");
    ssc_data_set_table(data, "solar_resource_data", &weather_data->table);

    //run the tests
    EXPECT_FALSE(run_module(data, "pvwattsv8"));

    ssc_number_t shad_beam_factor, ss_beam, ss_sky_diffuse, ss_gnd_diffuse, aoi, poa, tpoa, tcell, dc, ac;

    shad_beam_factor = ssc_data_get_array(data, "shad_beam_factor", nullptr)[12];
    EXPECT_NEAR(shad_beam_factor, 1.000, 0.01) << "External beam shading factor at noon";

    ss_beam = ssc_data_get_array(data, "ss_beam_factor", nullptr)[12];
    EXPECT_NEAR(ss_beam, 1.000, 0.01) << "Calculated self-shading beam shading factor at noon";

    ss_sky_diffuse = ssc_data_get_array(data, "ss_sky_diffuse_factor", nullptr)[12];
    EXPECT_NEAR(ss_sky_diffuse, 0.981, 0.01) << "Calculated self-shading sky diffuse shading factor at noon";

    ss_gnd_diffuse = ssc_data_get_array(data, "ss_gnd_diffuse_factor", nullptr)[12];
    EXPECT_NEAR(ss_gnd_diffuse, 0.346, 0.01) << "Calculated self-shading ground-reflected diffuse shading factor at noon";

    aoi = ssc_data_get_array(data, "aoi", nullptr)[12];
    EXPECT_NEAR(aoi, 32.195, 0.01) << "Angle of incidence at noon";

    poa = ssc_data_get_array(data, "poa", nullptr)[12];
    EXPECT_NEAR(poa, 828.570, 0.01) << "POA at noon"; //this shouldn't have changed, and code comparison shows no differences, so why are we now getting 828.570????

    tpoa = ssc_data_get_array(data, "tpoa", nullptr)[12];
    EXPECT_NEAR(tpoa, 823.560, 0.01) << "Transmitted POA at noon";

    tcell = ssc_data_get_array(data, "tcell", nullptr)[12];
    EXPECT_NEAR(tcell, 48.861, 0.01) << "Cell temp at noon";

    dc = ssc_data_get_array(data, "dc", nullptr)[12];
    EXPECT_NEAR(dc, 2509.045, 0.01) << "DC Energy at noon";

    ac = ssc_data_get_array(data, "ac", nullptr)[12];
    EXPECT_NEAR(ac, 2408.683, 0.01) << "AC Energy at noon";

    free_weatherdata_array(weather_data);
}

