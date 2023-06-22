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

class cm_hybrid : public compute_module
{
public:

	cm_hybrid()
	{
		add_var_info( _cm_vtab_hybrid );
	}
/*
pseudocode

    //set up a list of the tables that have been passed in, and error check the list
    Declare objects: generators, battery, fuel cell, grid, utility rate, financial- all null     *does utility rate table need to be separate from financial model table? or are they one? - separate tables - utility rates can be used as inputs to dispatch models.
    For i=0, i<number of tables, i++
        If PV or Wind table
            Add to list of generators
        elseif battery
            If batteryList == null
                batteryList = battery
            Else
                error (only one battery allowed)
        elseif fuel cell
            If fuelcellList == null
                fuelcellList = fuel cell
            Else
                error (only one fuel cell allowed)
        elseif grid
            if gridlist == null
                gridlist = grid
            else
                error (only one grid cmod allowed)
        elseif utility rate
            if utilityrateList == null
                utilityrateList = utilityrate
            Else
                error (only one utility rate allowed)
        elseif allowed financial model
            If financialList == null
                financialList = financial
            Else
                error (only one financial model allowed)     *this may not be true? are there some instances where >1 financial model is run? We currently do not run more than one financial model and the hybrid configuraiton should all be combined to the financial model inputs after the generators and grid and utilityrate compute modules are called
        else
            error (disallowed technology or financial model, report name)

    //Check that all required cmods are defined
    //We can consider battery, fuel cell, grid, and financial models all optional for max flexibility from SDK
    if #generators <= 1
        error
    if utilityrate required but not defined     *how to tell? - in starup.lk the simlist of compute modules for each hybrid configuration
        error

    //Run all instances of PV, wind cmods (this would allow >1 PV or wind location from SDK)
    //also find minimum timestep, installed cost, and total capacity
    minimumTimestep = quietnan
    hybridtotalinstalledcost, hybridcapacity = 0
    for i=0, i<#generators, i++
        hybridtotalinstalledcost += i_totalinstalledcost
        hybridcapacity += i_capacity
        run pv or wind cmod with inputs
        get timeseries gen
        get timestep
            if timestep < minimumTimestep
                minimumTimestep = timestep
        run O&M calcs with O&M costs and gen     *how can O&M costs be tagged to a particular technology? this requires breaking om calcs into a library function if they're not already - they can be post tech run in the hybrids compute module using the o and m functions.
        run incentives calcs with incentives and gen     *how can incentives be tagged to a particular technology? this requires breaking incentive calcs into a library function if they're not already - they can be post tech run calculated using the incentives function.

    //add timeseries gen from all generators

    for I = 0, i<8760 * minimumTimestep, i++
        for j=0; j<#generators; j++
            add generation from j for the correct timestep I

    //Run battery with combined gen, battery specs, and utility rate if it can charge from grid
    run battery
    hybridtotalinstalledcost += battery_totalinstalledcost

    //Run fuel cell with battery output
    run fuel cell
    hybridtotalinstalledcost += fuelcell_totalinstalledcost

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

*where is depreciation handled? In the financial model using the EBITDA line item and applying the selected depreciation schedule


exmaple configuration from startup.lk from SAM
setconfig('PVWatts Wind Battery Hybrid', 'Single Owner' );
setmodules( ['pvwattsv8', 'wind', 'battery', 'grid', 'utilityrate5', 'singleowner']);

*/
    void exec()
    {
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
                if ((computemodulename == "pvwattsv8") || (computemodulename == "wind"))
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
            int len, analysisPeriod;
            std::vector<size_t> genTimestepsPerHour;
            std::vector<ssc_number_t *> genVecs;
            for (size_t i = 0; i < generators.size(); i++) {

                std::string& compute_module = generators[i];
                var_data *compute_module_inputs = input_table->table.lookup(compute_module);
                if (compute_module_inputs->type != SSC_TABLE)
                    throw exec_error("hybrid", "No input input_table found for ." + compute_module);

                hybridSystemCapacity += compute_module_inputs->table.lookup("system_capacity")->num;
                hybridTotalInstalledCost += compute_module_inputs->table.lookup("total_installed_cost")->num;
                analysisPeriod = compute_module_inputs->table.lookup("analysis_period")->num;

                ssc_module_t module = ssc_module_create(compute_module.c_str());

                var_table& input = compute_module_inputs->table;
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
                
                // get minimum timestep from gen vector
                genVecs.push_back(ssc_data_get_array(compute_module_outputs, "gen", &len));
                currentTimeStepsPerHour = (size_t)len / 8760;
                if (compute_module_inputs->table.lookup("system_use_lifetime_output")->num > 0) // below - assuning single year only
                    currentTimeStepsPerHour /= analysisPeriod;
                if (currentTimeStepsPerHour > maximumTimeStepsPerHour)
                    maximumTimeStepsPerHour = currentTimeStepsPerHour;
                genTimestepsPerHour.push_back(currentTimeStepsPerHour);
                // add production O and M calculations - done below before financial calculations
                
                // add calculations to compute module outputs - done above for regular ompute module outputs

                ssc_data_set_table(outputs, compute_module.c_str(), compute_module_outputs);
                ssc_module_free(module);
                ssc_data_free(compute_module_outputs);

            } // end of generators

            /*
                //add timeseries gen from all generators

    for I = 0, i<8760 * minimumTimestep, i++
        for j=0; j<#generators; j++
            add generation from j for the correct timestep I
*/
            size_t genLength = 8760*maximumTimeStepsPerHour;// assumes single year gen
            ssc_number_t *pGen = allocate("gen", genLength);

            for (size_t i=0; i<genLength; i++) {
                pGen[i] = 0.0;
                for (size_t j=0; j<genVecs.size(); j++) {
                    pGen[i] += genVecs[j][i/(genLength/genTimestepsPerHour[j])];
                }
            }
            
            
            /*
    //Run battery with combined gen, battery specs, and utility rate if it can charge from grid
    run battery
    hybridtotalinstalledcost += battery_totalinstalledcost
*/
            if (batteries.size()>0) { // run single battery (refator running code below
                std::string& compute_module = batteries[0];
                var_data *compute_module_inputs = input_table->table.lookup(compute_module);
                if (compute_module_inputs->type != SSC_TABLE)
                    throw exec_error("hybrid", "No input input_table found for ." + compute_module);

                hybridSystemCapacity += compute_module_inputs->table.lookup("system_capacity")->num;
                hybridTotalInstalledCost += compute_module_inputs->table.lookup("total_installed_cost")->num;
  
                ssc_module_t module = ssc_module_create(compute_module.c_str());

                var_table& input = compute_module_inputs->table;
                ssc_data_set_array(static_cast<ssc_data_t>(&input), "gen", pGen, (int)genLength);
                
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
                
                // add calculations to compute module outputs - done above for regular ompute module outputs

                ssc_data_set_table(outputs, compute_module.c_str(), compute_module_outputs);
                ssc_module_free(module);
                ssc_data_free(compute_module_outputs);

            }
            /*
    //Run fuel cell with battery output
    run fuel cell
    hybridtotalinstalledcost += fuelcell_totalinstalledcost

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
            if (financials.size()>0) { // run remaining compute modules with necessary inputs
                // TODO - note that single vartable is used to run multiple compute modules
                // battery outputs need to be passed in
                // setup test with modified defaults from hybrid system
                // then update simulation.cpp to call hybrid
                std::string& compute_module = financials[0];
                var_data *compute_module_inputs = input_table->table.lookup(compute_module);
                if (compute_module_inputs->type != SSC_TABLE)
                    throw exec_error("hybrid", "No input input_table found for ." + compute_module);

                hybridSystemCapacity += compute_module_inputs->table.lookup("system_capacity")->num;
                hybridTotalInstalledCost += compute_module_inputs->table.lookup("total_installed_cost")->num;
  
                ssc_module_t module = ssc_module_create(compute_module.c_str());

                var_table& input = compute_module_inputs->table;
                ssc_data_set_array(static_cast<ssc_data_t>(&input), "gen", pGen, (int)genLength);
                
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
                
                // add calculations to compute module outputs - done above for regular ompute module outputs

                ssc_data_set_table(outputs, compute_module.c_str(), compute_module_outputs);
                ssc_module_free(module);
                ssc_data_free(compute_module_outputs);

            }


            // add Hybrid calculations to output
            ssc_data_set_number(outputs, "total_installed_cost", hybridTotalInstalledCost);
            ssc_data_set_number(outputs, "system_capacity", hybridSystemCapacity);

            assign("output", var_data(*(static_cast<var_table*>(outputs))));
            ssc_data_free(outputs);

        }
        else {
            throw exec_error("hybrid", "No compute modules specified.");
        }
	}
};

DEFINE_MODULE_ENTRY( hybrid, "Hybrid processing", 1 )
