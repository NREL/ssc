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

#ifndef __ssc_eqn_h
#define __ssc_eqn_h

#include "sscapi.h"
#include "cmod_battery_eqns.h"
#include "cmod_windpower_eqns.h"
#include "cmod_mhk_eqns.h"
#include "cmod_merchantplant_eqns.h"
#include "cmod_pvsamv1_eqns.h"
#include "cmod_csp_tower_eqns.h"
#include "cmod_csp_trough_eqns.h"
#include "cmod_financial_eqns.h"
#include "cmod_utilityrate5_eqns.h"

/**
 *  Returns true if completed successfully. For failures, query the "error" string that has been assigned to the `data`.
 *  For non-fatal issues that result in successful completion, a `warning` string will be provided.
 */
typedef bool (*ssc_equation_ptr)(ssc_data_t data);

/**
 * name:
 *      If the equation only affects variables from a single SSC Group, then you can name it <Group>_<fx>
 *          It'll show up in PySAM as <CMOD>.<Group>.<fx>
 *      Otherwise, you can name it <fx> for it to show up as <CMOD>.<fx>
 * cmod:
 *      Which compute module the equation should be associated with. At the moment, if you want to associate it with
 *      multiple compute modules, a new entry for each is required.
 * auto_eval:
 *      True means the equation is run in core.cpp before exec()
 * PySAM_export:
 *      True to export to PySAM (only)
 */

struct ssc_equation_entry{
    const char* name;
    ssc_equation_ptr func;
    const char* cmod;
    const char* doc;
    bool auto_eval;
    bool PySAM_export;
};

/**
 * Table of ssc_equations for SDK access
 *
 * #TODO: separate this logically if it gets big enough
 */

static ssc_equation_entry ssc_equation_table [] = {
        // Marine energy
		{"me_array_cable_length", me_array_cable_length,
            "Marine energy", me_array_cable_length_doc,
            false, true},
		{"mp_ancillary_services", mp_ancillary_services,
            "Merchant plant", mp_ancillary_services_doc,
            false, true},

        // PV
        {"Reopt_size_battery_post", Reopt_size_battery_params,
            "Pvsamv1", Reopt_size_battery_params_doc,
            false, true},
        {"Reopt_size_battery_post", Reopt_size_battery_params,
            "Pvwattsv7", Reopt_size_battery_params_doc,
            false, true},

        // Battery
        {"Calculate_thermal_params", Calculate_thermal_params,
            "Battery", calculate_thermal_params_doc,
            false, false},

        // Battery stateful
        {"Calculate_thermal_params", Calculate_thermal_params,
            "battery_stateful", calculate_thermal_params_doc,
            false, false},

        {"Size_batterystateful", Size_batterystateful,
            "battery_stateful", size_batterystateful_doc,
            false, false},

        // Wind
        {"Turbine_calculate_powercurve", Turbine_calculate_powercurve,
            "Windpower", Turbine_calculate_powercurve_doc,
            false, true},

        // CSP
        // MSPT:
        {"MSPT_System_Design_Equations", MSPT_System_Design_Equations,
            "Tcsmolten_salt", MSPT_System_Design_Equations_doc,
            true, false},
        {"Tower_SolarPilot_Solar_Field_Equations", Tower_SolarPilot_Solar_Field_Equations,
            "Tcsmolten_salt", Tower_SolarPilot_Solar_Field_Equations_doc,
            true, false},
        {"MSPT_Receiver_Equations", MSPT_Receiver_Equations,
            "Tcsmolten_salt", MSPT_Receiver_Equations_doc,
            true, false},
        {"MSPT_System_Control_Equations", MSPT_System_Control_Equations,
            "Tcsmolten_salt", MSPT_System_Control_Equations_doc,
            true, false},
        {"Tower_SolarPilot_Capital_Costs_MSPT_Equations", Tower_SolarPilot_Capital_Costs_MSPT_Equations,
            "Tcsmolten_salt", Tower_SolarPilot_Capital_Costs_MSPT_Equations_doc,
            true, false},
        //{"Tower_SolarPilot_Capital_Costs_DSPT_Equations", Tower_SolarPilot_Capital_Costs_DSPT_Equations,
        //    "Tcsdirect_steam", Tower_SolarPilot_Capital_Costs_DSPT_Equations_doc,
        //    true, false},
        //{"Tower_SolarPilot_Capital_Costs_ISCC_Equations", Tower_SolarPilot_Capital_Costs_ISCC_Equations,
        //    "Tcsiscc", Tower_SolarPilot_Capital_Costs_ISCC_Equations_doc,
        //    true, false},

        // Physical Trough:
        {"Physical_Trough_System_Design_Equations", Physical_Trough_System_Design_Equations,
            "Trough_physical", Physical_Trough_System_Design_Equations_doc,
            true, false},
        {"Physical_Trough_Solar_Field_Equations", Physical_Trough_Solar_Field_Equations,
            "Trough_physical", Physical_Trough_Solar_Field_Equations_doc,
            true, false},
        {"Physical_Trough_Collector_Type_Equations", Physical_Trough_Collector_Type_Equations,
            "Trough_physical", Physical_Trough_Collector_Type_Equations_doc,
            true, false},
        {"Physical_Trough_System_Control_Equations", Physical_Trough_System_Control_Equations,
            "Trough_physical", Physical_Trough_System_Control_Equations_doc,
            true, false},

        // Trough IPH
        {"Physical_Trough_Solar_Field_Equations", Physical_Trough_Solar_Field_Equations,
            "Trough_physical_process_heat", Physical_Trough_Solar_Field_Equations_doc,
            true, false},
        {"Physical_Trough_Collector_Type_Equations", Physical_Trough_Collector_Type_Equations,
            "Trough_physical_process_heat", Physical_Trough_Collector_Type_Equations_doc,
            true, false},
        {"Physical_Trough_Collector_Type_UI_Only_Equations", Physical_Trough_Collector_Type_UI_Only_Equations,
            "Trough_physical_process_heat", Physical_Trough_Collector_Type_UI_Only_Equations_doc,
            false, false},
        {"Physical_Trough_System_Control_Equations", Physical_Trough_System_Control_Equations,
            "Trough_physical_process_heat", Physical_Trough_System_Control_Equations_doc,
            true, false},


        // Single owner
        {"Financial_Construction_Financing_Equations", Financial_Construction_Financing_Equations,
            "Tcsmolten_salt", Financial_Construction_Financing_Equations_doc,
            true, false},
        {"Financial_Capacity_Payments_Equations", Financial_Capacity_Payments_Equations,
            "Singleowner", Financial_Capacity_Payments_Equations_doc,
            true, false},

        // Utility Rate
        {"ElectricityRates_format_as_URDBv7", ElectricityRates_format_as_URDBv7,
            "UtilityRate5", ElectricityRates_format_as_URDBv7_doc,
            false, true},
        {nullptr, nullptr, nullptr, nullptr, false, false}
};


#endif
