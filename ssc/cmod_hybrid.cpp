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


#include "core.h"



static var_info _cm_vtab_hybrid[] = {
/*   VARTYPE           DATATYPE         NAME                           LABEL                                UNITS     META                      GROUP                      REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,         SSC_TABLE,      "input",               "input_table for multiple technologies and one financial market",             "","","",      "*",        "",      "" },
    { SSC_OUTPUT,        SSC_TABLE,      "output",               "output_table for multiple technologies and one financial market",           "","","",      "*",        "",      "" },

var_info_invalid };

/*moved to common.cpp 
// for cost outputs calculated below
static var_info _cm_vtab_hybrid_om[] = {
        { SSC_OUTPUT,         SSC_ARRAY,      "cf_om_production",     "production O&M costs",             "$","","",      "",        "",      "" },
        { SSC_OUTPUT,         SSC_ARRAY,      "cf_om_capacity",     "capacity O&M costs",             "$","","",      "",        "",      "" },
        { SSC_OUTPUT,         SSC_ARRAY,      "cf_om_fixed",     "fixed O&M costs",             "$","","",      "",        "",      "" },
        { SSC_OUTPUT,         SSC_ARRAY,      "cf_om_land_lease",     "land lease O&M costs",             "$","","",      "",        "",      "" },
        { SSC_OUTPUT,         SSC_ARRAY,      "cf_energy_net",     "annual energy",             "kWh","","",      "",        "",      "" },

    var_info_invalid };
*/

class cm_hybrid : public compute_module
{
public:

	cm_hybrid()
	{
		add_var_info( _cm_vtab_hybrid );
	}
    void exec()
    {
        ssc_number_t percent = 0;
        var_data *input_table = lookup("input");
        if (input_table->type != SSC_TABLE)
            throw exec_error("hybrid", "No input input_table found.");

        // container for output tables
        ssc_data_t outputs = ssc_data_create();

        // setup generators and battery
        if (input_table->table.is_assigned("compute_modules")) { // list of compute modules from configuration

            std::vector<var_data>& vec_cms = input_table->table.lookup("compute_modules")->vec;

            std::vector<std::string> generators;
            std::vector<std::string> batteries;
            std::vector<std::string> fuelcells;
            std::vector<std::string> financials;  // remainder of compute modules e.g.  'grid', 'utilityrate5', 'singleowner' in above example "Hybrid" VarTable from SAM

            for (size_t i = 0; i < vec_cms.size(); i++) {
                std::string computemodulename = vec_cms[i].str;
                if ((computemodulename == "pvwattsv8") || (computemodulename == "windpower"))
                    generators.push_back(computemodulename);
                else if (computemodulename == "battery")
                    batteries.push_back(computemodulename);
                else if (computemodulename == "fuelcell")
                    fuelcells.push_back(computemodulename);
                else
                    financials.push_back(computemodulename);
            }

            // Hybrid system precheck
            if (generators.size() < 1)
                throw exec_error("hybrid", "Less than one generator specified.");
            if (batteries.size() > 1)
                throw exec_error("hybrid", "Only one battery bank allowed at this time.");
            if (fuelcells.size() > 1)
                throw exec_error("hybrid", "Only one fuel cell allowed at this time.");

            // run all generators and collect outputs and compute outputs
            size_t maximumTimeStepsPerHour = 1, currentTimeStepsPerHour;
            double hybridSystemCapacity = 0, hybridTotalInstalledCost = 0;
            ssc_number_t inflation_rate;
            int len, analysisPeriod;
            std::vector<size_t> genTimestepsPerHour;


            for (size_t igen = 0; igen < generators.size(); igen++) {

                std::string& compute_module = generators[igen];
                var_data *compute_module_inputs = input_table->table.lookup(compute_module);
                if (compute_module_inputs->type != SSC_TABLE)
                    throw exec_error("hybrid", "No input input_table found for ." + compute_module);

                ssc_number_t system_capacity = compute_module_inputs->table.lookup("system_capacity")->num;

                hybridSystemCapacity += system_capacity;
                hybridTotalInstalledCost += compute_module_inputs->table.lookup("total_installed_cost")->num;
                analysisPeriod = (int)compute_module_inputs->table.lookup("analysis_period")->num;

                ssc_module_t module = ssc_module_create(compute_module.c_str());

                var_table& input = compute_module_inputs->table;
                //ssc_data_set_number(static_cast<ssc_data_t>(&input), "is_hybrid", 1);


                ssc_module_exec_set_print(1);
                ssc_module_exec(module, static_cast<ssc_data_t>(&input));

                ssc_data_t compute_module_outputs = ssc_data_create();

                int pidx = 0;
                while (const ssc_info_t p_inf = ssc_module_var_info(module, pidx++))  {
                    int var_type = ssc_info_var_type(p_inf);   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
                    if ((var_type == SSC_OUTPUT) || (var_type == SSC_INOUT)) { // maybe remove INOUT
                        auto var_name = ssc_info_name(p_inf);
                        auto var_value = input.lookup(var_name);
                        ssc_data_set_var(compute_module_outputs, var_name, var_value);
                    }
                }

                // add o and m outputs
                //ssc_module_add_var_info(module, _cm_vtab_hybrid_om);

                // get minimum timestep from gen vector
                ssc_number_t* curGen = ssc_data_get_array(compute_module_outputs, "gen", &len);
                currentTimeStepsPerHour = len / 8760;
                if (compute_module_inputs->table.lookup("system_use_lifetime_output")->num > 0) // below - assuming single year only
                    currentTimeStepsPerHour /= analysisPeriod;
                if (currentTimeStepsPerHour > maximumTimeStepsPerHour)
                    maximumTimeStepsPerHour = currentTimeStepsPerHour;
                genTimestepsPerHour.push_back(currentTimeStepsPerHour);

                // add production O and M calculations - done below before financial calculations, production, capacity, annual and land lease...
                ssc_number_t* pOMProduction = ((var_table*)compute_module_outputs)->allocate("cf_om_production", analysisPeriod+1);
                ssc_number_t* pOMCapacity = ((var_table*)compute_module_outputs)->allocate("cf_om_capacity", analysisPeriod+1);
                ssc_number_t* pOMFixed = ((var_table*)compute_module_outputs)->allocate("cf_om_fixed", analysisPeriod+1); 
                inflation_rate = compute_module_inputs->table.lookup("inflation_rate")->num * 0.01;

                escal_or_annual(input, pOMFixed, analysisPeriod, "om_fixed", inflation_rate, 1.0, false, input.as_double("om_fixed_escal") * 0.01); // $ 
                escal_or_annual(input, pOMProduction, analysisPeriod, "om_production", inflation_rate, 0.001, false, input.as_double("om_production_escal") * 0.01); // $/kWh after conversion
                escal_or_annual(input, pOMCapacity, analysisPeriod, "om_capacity", inflation_rate, system_capacity, false, input.as_double("om_capacity_escal") * 0.01); // $ after multiplying by system capacity

                // production - multiply by yearly gen (initially assume single year) - use degradation - specific to each generator
                // pvwattsv8 - "degradation" applied in financial model - assuming single year analysis like standalone pvwatts/single owner configuration
                // wind - "degradation" applied in financial model - assumes system availability already applied to "gen" output
                ssc_number_t* pEnergyNet = ((var_table*)compute_module_outputs)->allocate("cf_energy_net", analysisPeriod + 1);
                ssc_number_t* pDegradation = ((var_table*)compute_module_outputs)->allocate("cf_degradation", analysisPeriod + 1);
                size_t count_degrad = 0;
                ssc_number_t* degrad = 0;
                degrad = input.as_array("degradation", &count_degrad);
                if (count_degrad == 1) {
                    for (int i = 1; i <= analysisPeriod; i++)
                        pDegradation[i] = pow((1.0 - degrad[0] / 100.0), i - 1);
                }
                else if (count_degrad > 0) {
                    for (int i = 0; i < analysisPeriod && i < (int)count_degrad; i++)
                        pDegradation[i + 1] = (1.0 - degrad[i] / 100.0);
                }
                ssc_number_t first_year_energy = ((var_table*)compute_module_outputs)->as_double("annual_energy"); // first year energy value
                for (int i = 1; i <= analysisPeriod; i++) {
                    pEnergyNet[i] = first_year_energy * pDegradation[i];
                    pOMProduction[i] *= pEnergyNet[i];
                }

                if (compute_module == "pvwattsv8") {
                    ssc_number_t* pOMLandLease = ((var_table*)compute_module_outputs)->allocate("cf_om_land_lease", analysisPeriod + 1);
                    ssc_number_t total_land_area = compute_module_inputs->table.lookup("land_area")->num;
                    escal_or_annual(input, pOMLandLease, analysisPeriod, "om_land_lease", inflation_rate, total_land_area, false, input.as_double("om_land_lease_escal") * 0.01);
                }
                // add calculations to compute module outputs - done above for regular compute module outputs - done above with allocate to compute_module_outputs

                ssc_data_set_table(outputs, compute_module.c_str(), compute_module_outputs);
                ssc_module_free(module);
                ssc_data_free(compute_module_outputs);

                percent = 100.0f * ((float)igen / (float)(generators.size() + fuelcells.size() + batteries.size() + financials.size()));
                update("", percent);

            } // end of generators

            /*
                //add timeseries gen from all generators

    for I = 0, i<8760 * minimumTimestep, i++
        for j=0; j<#generators; j++
            add generation from j for the correct timestep I
            need lifetime "gen" for utility rate and financial modules with system_use_lifetime_output set to 1 so that degradation is accounted for.
*/
            size_t genLength = 8760*maximumTimeStepsPerHour*analysisPeriod;// assumes single year gen
            ssc_number_t *pGen = ((var_table*)outputs)->allocate("gen", genLength); // add to top level "output" - assumes analysis period the same for all generators


            size_t idx = 0;
            for (size_t i=0; i<genLength; i++) 
                pGen[i] = 0.0;
            for (size_t g = 0; g < generators.size(); g++) {
                var_table generator_outputs = ((var_table*)outputs)->lookup(generators[g])->table;
                // retrieve each generator "gen" and "cf_degradation"
                size_t count_gen;
                ssc_number_t* gen = generator_outputs.as_array("gen", &count_gen);
                size_t count_degrade;
                ssc_number_t* cf_degradation = generator_outputs.as_array("cf_degradation", &count_degrade);
                size_t idx = 0;
                for (size_t y = 1; y<=analysisPeriod; y++) {
                    for (size_t h = 0; h < 8760; h++) {
                        for (size_t sph = 0; sph < maximumTimeStepsPerHour; sph++) {
                            size_t offset = sph / maximumTimeStepsPerHour / genTimestepsPerHour[g];
                            if (offset > genTimestepsPerHour[g]) offset = genTimestepsPerHour[g];
                            pGen[idx] += gen[h + offset] * cf_degradation[y];
                            idx++;
                        }
                    }
                }
            }
            /*
            //Run fuel cell with combined output like in PV-FuelCell-Battery configuration
            run fuel cell
                hybridtotalinstalledcost += fuelcell_totalinstalledcost
Note that thermal rate compute module also will need to be run
// Fuel Cell /////////////////////////////////////////////////////////
setconfig( 'Fuel Cell', 'Commercial');
setmodules( ['pvwattsv8', 'fuelcell', 'battery', 'grid', 'utilityrate5', 'thermalrate', 'cashloan'] );


                */

            if (fuelcells.size()>0) { // run single fuel cell if present 
                std::string& compute_module = fuelcells[0];
                var_data *compute_module_inputs = input_table->table.lookup(compute_module);
                if (compute_module_inputs->type != SSC_TABLE)
                    throw exec_error("hybrid", "No input input_table found for ." + compute_module);

                ssc_number_t system_capacity = compute_module_inputs->table.lookup("system_capacity")->num; // TODO: check capacity definitions for batteries and hybrid systems
                hybridSystemCapacity += system_capacity;
                hybridTotalInstalledCost += compute_module_inputs->table.lookup("total_installed_cost")->num;
                analysisPeriod = (int)compute_module_inputs->table.lookup("analysis_period")->num;

                ssc_module_t module = ssc_module_create(compute_module.c_str());

                var_table& input = compute_module_inputs->table;
                ssc_data_set_array(static_cast<ssc_data_t>(&input), "gen", pGen, (int)genLength);  // check if issue with hourly PV and subhourly wind
                //ssc_data_set_number(static_cast<ssc_data_t>(&input), "is_hybrid", 1);

                ssc_module_exec(module, static_cast<ssc_data_t>(&input));

                ssc_data_t compute_module_outputs = ssc_data_create();

                int pidx = 0;
                while (const ssc_info_t p_inf = ssc_module_var_info(module, pidx++))  {
                    int var_type = ssc_info_var_type(p_inf);   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
                    if ((var_type == SSC_OUTPUT) || (var_type == SSC_INOUT)) { // maybe remove INOUT
                        auto var_name = ssc_info_name(p_inf);
                        auto var_value = input.lookup(var_name);
                        ssc_data_set_var(compute_module_outputs, var_name, var_value);
                    }
                }

                // add production O and M calculations - done below before financial calculations
                ssc_number_t nameplate = 0;
                std::vector<double> fuelcell_discharged(analysisPeriod + 1, 0);
                ssc_number_t* pOMProduction = ((var_table*)compute_module_outputs)->allocate("cf_om_production", analysisPeriod+1);
                ssc_number_t* pOMCapacity = ((var_table*)compute_module_outputs)->allocate("cf_om_capacity", analysisPeriod+1);
                ssc_number_t* pOMFixed = ((var_table*)compute_module_outputs)->allocate("cf_om_fixed", analysisPeriod+1);
                inflation_rate = compute_module_inputs->table.lookup("inflation_rate")->num * 0.01; // can retrieve from "Hybrid" vartable directly
                escal_or_annual(input, pOMFixed, analysisPeriod, "om_fuelcell_fixed_cost", inflation_rate, 1.0, false, input.as_double("om_fixed_escal") * 0.01); // $
                escal_or_annual(input, pOMProduction, analysisPeriod, "om_fuelcell_variable_cost", inflation_rate, 0.001, false, input.as_double("om_production_escal") * 0.01); // $/kW
                escal_or_annual(input, pOMCapacity, analysisPeriod, "om_fuelcell_capacity_cost", inflation_rate, system_capacity, false, input.as_double("om_capacity_escal") * 0.01); // $
                nameplate = ((var_table*)compute_module_outputs)->as_number("om_fuelcell_nameplate");
                fuelcell_discharged = ((var_table*)compute_module_outputs)->as_vector_double("fuelcell_annual_energy_discharged");
                if (fuelcell_discharged.size() == 2) { // ssc #992
                    double first_val = fuelcell_discharged[1];
                    fuelcell_discharged.resize(analysisPeriod + 1, first_val);
                }
                if (fuelcell_discharged.size() != (size_t)analysisPeriod + 1)
                    throw exec_error("hybrid", util::format("fuelcell_discharged size (%d) incorrect", (int)fuelcell_discharged.size()));
                // fuelcell cost - replacement from lifetime analysis
                if (((var_table*)compute_module_outputs)->is_assigned("fuelcell_replacement_option") && (((var_table*)compute_module_outputs)->as_integer("fuelcell_replacement_option") > 0))
                {
                    size_t count;
                    ssc_number_t* fuelcell_rep = 0;
                    if (((var_table*)compute_module_outputs)->as_integer("fuelcell_replacement_option") == 1)
                        fuelcell_rep = ((var_table*)compute_module_outputs)->as_array("fuelcell_replacement", &count); // replacements per year calculated
                    else // user specified
                        fuelcell_rep = ((var_table*)compute_module_outputs)->as_array("fuelcell_replacement_schedule", &count); // replacements per year user-defined

                    ssc_number_t* pFuelCellReplacement = ((var_table*)compute_module_outputs)->allocate("cf_fuelcell_replacement_cost_schedule", analysisPeriod+1);
                    escal_or_annual(input, pFuelCellReplacement, analysisPeriod, "om_fuelcell_replacement_cost", inflation_rate, nameplate, false, input.as_double("om_replacement_cost_escal") * 0.01);

                    for (size_t i = 0; i < (size_t)analysisPeriod && i < count; i++) {
                        pFuelCellReplacement[i + 1] = fuelcell_rep[i] * pFuelCellReplacement[i + 1];
                    }
                }
                // production O and M conversion to $
                for (size_t i = 0; i <= (size_t)analysisPeriod; i++)
                    pOMProduction[i] *= fuelcell_discharged[i];


                // add calculations to compute module outputs - done above for regular ompute module outputs

                ssc_data_set_table(outputs, compute_module.c_str(), compute_module_outputs);
                ssc_module_free(module);
                ssc_data_free(compute_module_outputs);

                percent = 100.0f * ((float)(generators.size() + fuelcells.size()) / (float)(generators.size() + fuelcells.size() + batteries.size() + financials.size()));
                update("", percent);

            }

            /* TODO - test hybrid output from fuel cell in future
    //Run battery with combined gen output from fuel cell if present, battery specs, and utility rate if it can charge from grid
    run battery
    hybridtotalinstalledcost += battery_totalinstalledcost
*/
            if (batteries.size()>0) { // run single battery (refator running code below)
                std::string& compute_module = batteries[0];
                var_data *compute_module_inputs = input_table->table.lookup(compute_module);
                if (compute_module_inputs->type != SSC_TABLE)
                    throw exec_error("hybrid", "No input input_table found for ." + compute_module);

                hybridSystemCapacity += compute_module_inputs->table.lookup("system_capacity")->num; // TODO: check capacity definitions for batteries and hybrid systems
                hybridTotalInstalledCost += compute_module_inputs->table.lookup("total_installed_cost")->num;
                analysisPeriod = (int)compute_module_inputs->table.lookup("analysis_period")->num;

                ssc_module_t module = ssc_module_create(compute_module.c_str());

                var_table& input = compute_module_inputs->table;
                ssc_data_set_array(static_cast<ssc_data_t>(&input), "gen", pGen, (int)genLength);  // check if issue with lookahead dispatch with hourly PV and subhourly wind
                ssc_data_set_number(static_cast<ssc_data_t>(&input), "system_use_lifetime_output", 1);
                //ssc_data_set_number(static_cast<ssc_data_t>(&input), "is_hybrid", 1);


                ssc_module_exec(module, static_cast<ssc_data_t>(&input));

                ssc_data_t compute_module_outputs = ssc_data_create();

                int pidx = 0;
                while (const ssc_info_t p_inf = ssc_module_var_info(module, pidx++))  {
                    int var_type = ssc_info_var_type(p_inf);   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
                    if ((var_type == SSC_OUTPUT) || (var_type == SSC_INOUT)) { // maybe remove INOUT
                        auto var_name = ssc_info_name(p_inf);
                        auto var_value = input.lookup(var_name);
                        ssc_data_set_var(compute_module_outputs, var_name, var_value);
                    }
                }
                // get latest output
                // add production O and M calculations - done below before financial calculations - note that these values do not have ssc_module_var_info values - need to be INOUT to subsequent compute modules
                ssc_number_t nameplate = 0;
                ssc_number_t* pOMProduction = ((var_table*)compute_module_outputs)->allocate("cf_om_production", analysisPeriod+1);
                ssc_number_t* pOMCapacity = ((var_table*)compute_module_outputs)->allocate("cf_om_capacity", analysisPeriod+1);
                ssc_number_t* pOMFixed = ((var_table*)compute_module_outputs)->allocate("cf_om_fixed", analysisPeriod+1);
                inflation_rate = compute_module_inputs->table.lookup("inflation_rate")->num * 0.01; // can retrieve from "Hybrid" vartable directly
                escal_or_annual(input, pOMFixed, analysisPeriod, "om_batt_fixed_cost", inflation_rate, 1.0, false, input.as_double("om_fixed_escal") * 0.01);
                escal_or_annual(input, pOMProduction, analysisPeriod, "om_batt_variable_cost", inflation_rate, 0.001, false, input.as_double("om_production_escal") * 0.01);
                std::vector<double> battery_discharged(analysisPeriod, 0);
                nameplate = compute_module_inputs->table.lookup("om_batt_nameplate")->num;
                //if (as_integer("en_batt") == 1 || as_integer("en_standalone_batt") == 1)
                ssc_number_t *battery_discharged_array = ssc_data_get_array(compute_module_outputs,"batt_annual_discharge_energy", &len);
                if (len == 1) { // ssc #992
                    double first_val = battery_discharged[0];
                    battery_discharged.resize(analysisPeriod, first_val);
                }
                else if (len != analysisPeriod) {
                    throw exec_error("hybrid", util::format("battery_discharged size (%d) incorrect", (int)battery_discharged.size()));
                }
                else {
                    for (int i = 0; i < len; i++)
                        battery_discharged[i] = battery_discharged_array[i];
                }

                // battery cost - replacement from lifetime analysis
//                if ((as_integer("en_batt") == 1 || as_integer("en_standalone_batt") == 1 || as_integer("en_wave_batt") == 1) && (as_integer("batt_replacement_option") > 0))
                double batt_cap = 0.0;
                if (input.as_integer("batt_replacement_option") > 0) {
                    ssc_number_t* batt_rep = 0;
                    std::vector<ssc_number_t> replacement_percent;
                    size_t count;
                    batt_rep = ((var_table*)compute_module_outputs)->as_array("batt_bank_replacement", &count); // replacements per year calculated

                    // replace at capacity percent
                    if (input.as_integer("batt_replacement_option") == 1) {

                        for (int i = 0; i < (int)count; i++) {
                            replacement_percent.push_back(100);
                        }
                    }
                    else {// user specified
                        replacement_percent = input.as_vector_ssc_number_t("batt_replacement_schedule_percent");
                    }
                    batt_cap = ((var_table*)compute_module_outputs)->as_double("batt_computed_bank_capacity");
                    // updated 10/17/15 per 10/14/15 meeting
                    ssc_number_t* pOMBattReplacementCost = ((var_table*)compute_module_outputs)->allocate("cf_battery_replacement_cost_schedule", analysisPeriod + 1);
                    escal_or_annual(input, pOMBattReplacementCost, analysisPeriod, "om_batt_replacement_cost", inflation_rate, batt_cap, false, input.as_double("om_replacement_cost_escal") * 0.01);

                    for (int i = 0; i < analysisPeriod && i < (int)count; i++) {
                        // the cash flow sheets are 1 indexed, batt_rep and replacement_percent is zero indexed
                        pOMBattReplacementCost[i + 1] = batt_rep[i] * replacement_percent[i] * 0.01 * pOMBattReplacementCost[ i + 1];
                    }
                }
                else
                {
                    batt_cap = ((var_table*)compute_module_outputs)->as_double("batt_computed_bank_capacity");
                    // updated 10/17/15 per 10/14/15 meeting
                    ssc_number_t* pOMBattReplacementCost = ((var_table*)compute_module_outputs)->allocate("cf_battery_replacement_cost_schedule", analysisPeriod + 1);
                    escal_or_annual(input, pOMBattReplacementCost, analysisPeriod, "om_batt_replacement_cost", inflation_rate, batt_cap, false, input.as_double("om_replacement_cost_escal") * 0.01);
                }
                escal_or_annual(input, pOMCapacity, analysisPeriod, "om_batt_capacity_cost", inflation_rate, batt_cap, false, input.as_double("om_capacity_escal") * 0.01);

                // production O and M conversion to $
                for (size_t i = 1; i <= (size_t)analysisPeriod; i++)
                    pOMProduction[i] *= battery_discharged[i - 1];

                // add calculations to compute module outputs - done above for regular compute module outputs

                ssc_data_set_table(outputs, compute_module.c_str(), compute_module_outputs);
                ssc_module_free(module);
                ssc_data_free(compute_module_outputs);

                percent = 100.0f * ((float)(generators.size() + fuelcells.size() + batteries.size()) / (float)(generators.size() + fuelcells.size() + batteries.size() + financials.size()));
                update("", percent);
            }

            bool use_batt_output = false;
            ssc_number_t* pBattGen = 0;
            size_t battGenLen = 0;
            // update gen to battery output
            if (batteries.size() > 0) {
                use_batt_output = true;
                pBattGen = ((var_table*)outputs)->lookup(batteries[0])->table.as_array("gen", &battGenLen);
            }

 
/*
    //Run grid compute module with fuel cell output
    run grid

    //Run utilityrate if applicable
    run utilityrate
    save utilityratecosts

    //Run financial model - is no financial model an option?
    for year = 0; year<analysis period, year++
        omcosts[year] = 0
        incentives[year] = 0
        for I = 0; i<#generators; i++
            omcosts[year] += j_omcosts[year] *do we want all rolled into one or separate line items
            incentives[year] = j_incentives[year] *do we want all rolled into one or separate line items
    run financial model (omcosts, incentives, utilityratecosts, hybridtotalinstalledcost)
    get financial model outputs

            */
            ssc_number_t* pHybridOMSum = ((var_table*)outputs)->allocate("cf_hybrid_om_sum", analysisPeriod + 1); // add to top level "output" - assumes analysis period the same for all generators

             for (size_t i = 0; i <= analysisPeriod; i++)
                pHybridOMSum[i] = 0.0;
            for (size_t g = 0; g < generators.size(); g++) {
                var_table generator_outputs = ((var_table*)outputs)->lookup(generators[g])->table;
                size_t count_gen;
                ssc_number_t* om_production = generator_outputs.as_array("cf_om_production", &count_gen);
                ssc_number_t* om_fixed = generator_outputs.as_array("cf_om_fixed", &count_gen);
                ssc_number_t* om_capacity = generator_outputs.as_array("cf_om_capacity", &count_gen);
                ssc_number_t* om_landlease = NULL;
                if (generators[g] == "pvwattsv8")
                    om_landlease = generator_outputs.as_array("cf_om_land_lease", &count_gen);
                for (size_t y = 1; y <= analysisPeriod; y++) {
                    pHybridOMSum[y] += om_production[y] + om_fixed[y] + om_capacity[y];
                    if (generators[g] == "pvwattsv8")
                        pHybridOMSum[y] += om_landlease[y];
                }
            }
            for (size_t f = 0;f < fuelcells.size(); f++) {
                var_table fuelcell_outputs = ((var_table*)outputs)->lookup(fuelcells[f])->table;
                size_t count_fc;
                ssc_number_t* om_production = fuelcell_outputs.as_array("cf_om_production", &count_fc);
                ssc_number_t* om_fixed = fuelcell_outputs.as_array("cf_om_fixed", &count_fc);
                ssc_number_t* om_capacity = fuelcell_outputs.as_array("cf_om_capacity", &count_fc);
                for (size_t y = 1; y <= analysisPeriod; y++) {
                    pHybridOMSum[y] += om_production[y] + om_fixed[y] + om_capacity[y];
                }
            }

            for (size_t b = 0; b < batteries.size(); b++) {
                var_table batteries_outputs = ((var_table*)outputs)->lookup(batteries[b])->table;
                size_t count_b;
                ssc_number_t* om_production = batteries_outputs.as_array("cf_om_production", &count_b);
                ssc_number_t* om_fixed = batteries_outputs.as_array("cf_om_fixed", &count_b);
                ssc_number_t* om_capacity = batteries_outputs.as_array("cf_om_capacity", &count_b);
                ssc_number_t* om_replacement = batteries_outputs.as_array("cf_battery_replacement_cost_schedule", &count_b);
                for (size_t y = 1; y <= analysisPeriod; y++) {
                    pHybridOMSum[y] += om_production[y] + om_fixed[y] + om_capacity[y] + om_replacement[y];
                }
            }




            if (financials.size()>0) { // run remaining compute modules with necessary inputs
                // note that single vartable is used to run multiple compute modules
                // battery outputs passed in if present
                // setup test with modified defaults from hybrid system cmod_hybrid_test
                // then update simulation.cpp to call hybrid
                std::string hybridVarTable("Hybrid");
                var_data* compute_module_inputs = input_table->table.lookup(hybridVarTable);// TODO - better naming of combined vartable?
                if (compute_module_inputs->type != SSC_TABLE)
                    throw exec_error("hybrid", "No input input_table found for ." + hybridVarTable);

                var_table& input = compute_module_inputs->table;

                if (use_batt_output)
                    ssc_data_set_array(static_cast<ssc_data_t>(&input), "gen", pBattGen, (int)battGenLen);  // check if need to update to battery output
                else
                    ssc_data_set_array(static_cast<ssc_data_t>(&input), "gen", pGen, (int)genLength);
                ssc_data_set_number(static_cast<ssc_data_t>(&input), "system_use_lifetime_output", 1);

//                ssc_data_set_array(&(compute_module_inputs->table), "gen", pGen, (int)genLength);
                ssc_data_set_number(static_cast<ssc_data_t>(&input), "system_use_lifetime_output", 1);
 
                // set additional inputs from previous results - note - remove these from UI?
                ssc_data_set_number(static_cast<ssc_data_t>(&input), "total_installed_cost", hybridTotalInstalledCost);
                ssc_data_set_number(static_cast<ssc_data_t>(&input), "system_capacity", hybridSystemCapacity);

                ssc_data_set_array(&(compute_module_inputs->table), "cf_hybrid_om_sum", pHybridOMSum, (int)(analysisPeriod+1));

                // run remaining compute modules in sequence and add results to "Hybrid" VarTable
                ssc_data_t hybridFinancialOutputs = ssc_data_create();

                for (size_t i = 0; i < financials.size(); i++) {
                    std::string compute_module = financials[i];

                    ssc_module_t module = ssc_module_create(compute_module.c_str());
                    //ssc_module_exec_set_print(1);
                    ssc_module_exec(module, static_cast<ssc_data_t>(&input));
                    //ssc_module_exec_simple_nothread(module, static_cast<ssc_data_t>(&input)


                    int pidx = 0;
                    while (const ssc_info_t p_inf = ssc_module_var_info(module, pidx++)) {
                        int var_type = ssc_info_var_type(p_inf);   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
                        if ((var_type == SSC_OUTPUT) || (var_type == SSC_INOUT)) { // maybe remove INOUT
                            auto var_name = ssc_info_name(p_inf);
                            auto var_value = input.lookup(var_name);
                            ssc_data_set_var(hybridFinancialOutputs, var_name, var_value);
                        }
                    }
 
                    ssc_module_free(module);

                    percent = 100.0f *((float)(i + generators.size() + fuelcells.size() + batteries.size()) / (float)(generators.size() + fuelcells.size() + batteries.size() + financials.size()));
                    update("", percent);

                }

                ssc_data_set_table(outputs, hybridVarTable.c_str(), hybridFinancialOutputs);
                ssc_data_free(hybridFinancialOutputs);


            }
            // add Hybrid calculations to output - in "Hybrid" vartable output above
            //ssc_data_set_number(outputs, "total_installed_cost", hybridTotalInstalledCost);
            //ssc_data_set_number(outputs, "system_capacity", hybridSystemCapacity);

            assign("output", var_data(*(static_cast<var_table*>(outputs))));
            ssc_data_free(outputs);

        }
        else {
            throw exec_error("hybrid", "No compute modules specified.");
        }
	}

    void escal_or_annual(var_table& vt, ssc_number_t *cf, int nyears, const std::string& variable,
        double inflation_rate, double scale, bool as_rate = true, double escal = 0.0)
    {
        size_t count;
        ssc_number_t* arrp = vt.as_array(variable, &count);

        if (as_rate)
        {
            if (count == 1)
            {
                escal = inflation_rate + scale * arrp[0];
                for (int i = 0; i < nyears; i++)
                    cf[ i + 1] = pow(1 + escal, i);
            }
            else
            {
                for (int i = 0; i < nyears && i < (int)count; i++)
                    cf[ i + 1] = 1 + arrp[i] * scale;
            }
        }
        else
        {
            if (count == 1)
            {
                for (int i = 0; i < nyears; i++)
                    cf[ i + 1] = arrp[0] * scale * pow(1 + escal + inflation_rate, i);
            }
            else
            {
                for (int i = 0; i < nyears && i < (int)count; i++)
                    cf[ i + 1] = arrp[i] * scale;
            }
        }
    }


};

DEFINE_MODULE_ENTRY( hybrid, "Hybrid processing", 1 )
