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

#ifndef __csp_system_costs_
#define __csp_system_costs_

#include <limits>

namespace N_mspt
{
    void calculate_mspt_etes_costs(
        // Heliostat Field
        double A_sf_refl,				//[m^2] Total solar field reflective area
        double site_improv_spec_cost,	//[$/m^2_reflect] Site improvement specific cost
        double heliostat_spec_cost,		//[$/m^2_reflect] Heliostat specific cost
        double heliostat_fixed_cost,	//[$] Heliostat fixed cost

        // Tower
        double h_tower,					//[m] Tower height
        double h_rec,					//[m] Receiver height
        double h_helio,					//[m] Heliostat height
        double tower_fixed_cost,		//[$] Tower fixed cost
        double tower_cost_scaling_exp,	//[-] Tower cost scaling exponent

        // Receiver
        double A_rec,					//[m^2] Receiver area
        double rec_ref_cost,			//[$] Receiver reference cost
        double A_rec_ref,				//[m^2] Receiver reference area
        double rec_cost_scaling_exp,	//[-] Receiver cost scaling exponent

        // TES
        double Q_storage,				//[MWt-hr] Storage capacity
        double tes_spec_cost,			//[$/kWt-hr] TES specific cost

        // Power Cycle
        double W_dot_design,			//[MWe] Power cycle design output (w/o subtracting plant parasitics)
        double power_cycle_spec_cost,	//[$/kWe] Power cycle specific cost

        // Heater
        double q_dot_heater_design,     //[MWt] Heater design thermal power
        double heater_spec_cost,        //[$/kWe] Heater specific cost

        //Radiative cooling & cold storage
        double radfield_area,			//[m^2] Radiator field area if any 
        double coldstorage_vol,			//[m^3] Cold storage tank volume (total of all tanks)
        double radfield_vol,			//[m^3] Volume of fluid in radiator panels
        double rad_unitcost,			//[$/m^2] Radiator area
        double rad_installcost,			//[$/m^2] Radiator installation cost per square meter
        double rad_volmulti,			//[-]	Multiplier for the volume of fluid in delivery versus in radiators
        double rad_fluidcost,			//[$/L]	Cooling fluid unit cost
        double coldstorage_unitcost,	//[$/L] Cold storage construction cost

        // Balance Of Plant
        double bop_spec_cost,			//[$/kWe] BOP specific cost

        // Fossil Backup Cost
        double fossil_backup_spec_cost,	//[$/kWe] Fossil backup specific cost

        // Contingency Cost
        double contingency_rate,		//[%] Of precontingency direct capital costs

        // Indirect Capital Costs
        double total_land_area,			    //[acres]
        double plant_net_capacity,		    //[MWe] Nameplate plant capacity (Net cycle output less estimated parasitics)
        double EPC_land_spec_cost,		    //[$/acre]
        double EPC_land_perc_direct_cost,	//[%] Of calculated direct cost
        double EPC_land_per_power_cost,		//[$/We] Of plant net capacity
        double EPC_land_fixed_cost,		    //[$]
        double total_land_spec_cost,	    //[$/acre]
        double total_land_perc_direct_cost,	//[%] Of calculated direct cost
        double total_land_per_power_cost,	//[$/We] Of plant net capacity
        double total_land_fixed_cost,	    //[$]
        double sales_tax_basis,			    //[%] Of total direct cost
        double sales_tax_rate,			    //[%]

        // Calculated Outputs
        double& site_improvement_cost,                  //[$]
        double& heliostat_cost,                         //[$]
        double& tower_cost,                             //[$]
        double& receiver_cost,                          //[$]
        double& tes_cost,                               //[$]
        double& power_cycle_cost,                       //[$]
        double& heater_cost,                            //[$]
        double& rad_field_totcost,                      //[$]
        double& rad_fluid_totcost,                      //[$]
        double& rad_storage_totcost,                    //[$]
        double& bop_cost,                               //[$]
        double& fossil_backup_cost,                     //[$]
        double& direct_capital_precontingency_cost,     //[$]
        double& contingency_cost,                       //[$]
        double& total_direct_cost,                      //[$]
        double& total_land_cost,                        //[$]
        double& epc_and_owner_cost,                     //[$]
        double& sales_tax_cost,                         //[$]
        double& total_indirect_cost,                    //[$]
        double& total_installed_cost,                   //[$]
        double& estimated_installed_cost_per_cap        //[$/kWe]
    );

    void calculate_mspt__no_rad_cool__costs(
        // Heliostat Field
        double A_sf_refl,				//[m^2] Total solar field reflective area
        double site_improv_spec_cost,	//[$/m^2_reflect] Site improvement specific cost
        double heliostat_spec_cost,		//[$/m^2_reflect] Heliostat specific cost
        double heliostat_fixed_cost,	//[$] Heliostat fixed cost

        // Tower
        double h_tower,					//[m] Tower height
        double h_rec,					//[m] Receiver height
        double h_helio,					//[m] Heliostat height
        double tower_fixed_cost,		//[$] Tower fixed cost
        double tower_cost_scaling_exp,	//[-] Tower cost scaling exponent

        // Receiver
        double A_rec,					//[m^2] Receiver area
        double rec_ref_cost,			//[$] Receiver reference cost
        double A_rec_ref,				//[m^2] Receiver reference area
        double rec_cost_scaling_exp,	//[-] Receiver cost scaling exponent

        // TES
        double Q_storage,				//[MWt-hr] Storage capacity
        double tes_spec_cost,			//[$/kWt-hr] TES specific cost

        // Power Cycle
        double W_dot_design,			//[MWe] Power cycle design output (w/o subtracting plant parasitics)
        double power_cycle_spec_cost,	//[$/kWe] Power cycle specific cost

        // Balance Of Plant
        double bop_spec_cost,			//[$/kWe] BOP specific cost

        // Fossil Backup Cost
        double fossil_backup_spec_cost,	//[$/kWe] Fossil backup specific cost

        // Contingency Cost
        double contingency_rate,		//[%] Of precontingency direct capital costs

        // Indirect Capital Costs
        double total_land_area,			    //[acres]
        double plant_net_capacity,		    //[MWe] Nameplate plant capacity (Net cycle output less estimated parasitics)
        double EPC_land_spec_cost,		    //[$/acre]
        double EPC_land_perc_direct_cost,	//[%] Of calculated direct cost
        double EPC_land_per_power_cost,		//[$/We] Of plant net capacity
        double EPC_land_fixed_cost,		    //[$]
        double total_land_spec_cost,	    //[$/acre]
        double total_land_perc_direct_cost,	//[%] Of calculated direct cost
        double total_land_per_power_cost,	//[$/We] Of plant net capacity
        double total_land_fixed_cost,	    //[$]
        double sales_tax_basis,			    //[%] Of total direct cost
        double sales_tax_rate,			    //[%]

        // Calculated Outputs
        double& site_improvement_cost,                  //[$]
        double& heliostat_cost,                         //[$]
        double& tower_cost,                             //[$]
        double& receiver_cost,                          //[$]
        double& tes_cost,                               //[$]
        double& power_cycle_cost,                       //[$]
        double& bop_cost,                               //[$]
        double& fossil_backup_cost,                     //[$]
        double& direct_capital_precontingency_cost,     //[$]
        double& contingency_cost,                       //[$]
        double& total_direct_cost,                      //[$]
        double& total_land_cost,                        //[$]
        double& epc_and_owner_cost,                     //[$]
        double& sales_tax_cost,                         //[$]
        double& total_indirect_cost,                    //[$]
        double& total_installed_cost,                   //[$]
        double& estimated_installed_cost_per_cap        //[$/kWe]
    );

    void calculate_etes_costs(

        // TES
        double Q_storage,				//[MWt-hr] Storage capacity
        double tes_spec_cost,			//[$/kWt-hr] TES specific cost

        // Power Cycle
        double W_dot_design,			//[MWe] Power cycle design output (w/o subtracting plant parasitics)
        double power_cycle_spec_cost,	//[$/kWe] Power cycle specific cost

        // Heater
        double q_dot_heater_design,     //[MWt] Heater design thermal power
        double heater_spec_cost,        //[$/kWe] Heater specific cost

        // Balance Of Plant
        double bop_spec_cost,			//[$/kWe] BOP specific cost

        // Contingency Cost
        double contingency_rate,		//[%] Of precontingency direct capital costs

        // Indirect Capital Costs
        double plant_net_capacity,		    //[MWe] Nameplate plant capacity (Net cycle output less estimated parasitics)
        double EPC_land_perc_direct_cost,	//[%] Of calculated direct cost
        double EPC_land_per_power_cost,		//[$/We] Of plant net capacity
        double EPC_land_fixed_cost,		    //[$]
        double total_land_perc_direct_cost,	//[%] Of calculated direct cost
        double total_land_per_power_cost,	//[$/We] Of plant net capacity
        double total_land_fixed_cost,	    //[$]
        double sales_tax_basis,			    //[%] Of total direct cost
        double sales_tax_rate,			    //[%]

        // Calculated Outputs
        double& tes_cost,                               //[$]
        double& power_cycle_cost,                       //[$]
        double& heater_cost,                            //[$]
        double& bop_cost,                               //[$]
        double& direct_capital_precontingency_cost,     //[$]
        double& contingency_cost,                       //[$]
        double& total_direct_cost,                      //[$]
        double& total_land_cost,                        //[$]
        double& epc_and_owner_cost,                     //[$]
        double& sales_tax_cost,                         //[$]
        double& total_indirect_cost,                    //[$]
        double& total_installed_cost,                   //[$]
        double& estimated_installed_cost_per_cap        //[$/kWe]
    );

    double site_improvement_cost(double A_refl /*m^2*/, double site_improv_spec_cost /*$/m^2_reflect*/);

	double heliostat_cost(double A_refl /*m^2*/, double heliostat_spec_cost /*$/m^2*/, double heliostate_fixed_cost /*$*/ );

	double tower_cost(double h_tower /*m*/, double h_rec /*m*/, double h_helio /*m*/, double tower_fixed_cost /*$*/, double tower_cost_scaling_exp /*-*/);

	double receiver_cost(double A_rec /*m^2*/, double rec_ref_cost /*$*/, double rec_ref_area /*m^2*/, double rec_cost_scaling_exp /*-*/);
	
	double tes_cost(double Q_storage /*MWt-hr*/, double tes_spec_cost /*$/kWt-hr*/);

	double power_cycle_cost(double W_dot_design /*MWe*/, double power_cycle_spec_cost /*$/kWe*/);

    double heater_cost(double q_dot_heater_design /*MWt*/, double heater_spec_cost /*$/kWe*/);

	double rad_field_totcost(double rad_area /*m^2*/, double panelcost /*$/m^2*/, double panelinstallcost /*$/m^2*/);
	double rad_fluid_totcost(double rad_field /*m^3*/,  double fluidcost /*$/L*/, double muliplier_volume /*-*/);
	double rad_storage_totcost(double cold_volume /*m^3*/, double storagecost /*$/L*/);

	double bop_cost(double W_dot_design /*MWe*/, double bop_spec_cost /*$/kWe*/);

	double fossil_backup_cost(double W_dot_design /*MWe*/, double fossil_backup_spec_cost /*$/kWe*/);

	double direct_capital_precontingency_cost(double site_improvement_cost /*$*/,
		double heliostat_cost /*$*/,
		double tower_cost /*$*/,
		double receiver_cost /*$*/,
		double tes_cost /*$*/,
		double power_cycle_cost /*$*/,
		double rad_field_totcost /*$*/,
		double rad_fluid_totcost /*$*/,
		double rad_storage_totcost /*$*/,
		double bop_cost /*$*/,
		double fossil_backup_cost /*$*/);
	
	double contingency_cost( double contingency_rate /*%*/, double direct_capital_precontingency_cost /*$*/);

	double total_direct_cost(double direct_capital_precontingency_cost /*$*/, double contingency_cost /*$*/);

	double total_land_cost(double total_land_area /*acres*/, double total_direct_cost /*$*/, double plant_net_capacity /*MWe*/,
		double land_spec_cost /*$/acre*/, double land_perc_direct_cost /*%*/, double land_spec_per_power_cost /*$/We*/, double land_fixed_cost /*$*/);

	double epc_and_owner_cost(double total_land_area /*acres*/, double total_direct_cost /*$*/, double plant_net_capacity /*MWe*/,
		double land_spec_cost /*$/acre*/, double land_perc_direct_cost /*%*/, double land_spec_per_power_cost /*$/We*/, double land_fixed_cost /*$*/);

	double sales_tax_cost(double total_direct_cost /*$*/, double sales_tax_basis /*% of tot. direct cost*/, double sales_tax_rate /*%*/);

	double total_indirect_cost(double total_land_cost /*$*/, double epc_and_owner_cost /*$*/, double sales_tax_cost /*$*/);

	double total_installed_cost(double total_direct_cost /*$*/, double total_indirect_cost /*$*/);

	double estimated_installed_cost_per_cap(double total_installed_cost /*$*/, double plant_net_capacity /*$*/);
}

namespace N_financial_parameters
{
	void construction_financing_total_cost(double total_installed_cost /*$*/,
		double const_per_interest_rate1 /*%*/, double const_per_interest_rate2 /*%*/, double const_per_interest_rate3 /*%*/, double const_per_interest_rate4 /*%*/, double const_per_interest_rate5 /*%*/,
		double const_per_months1 /*-*/, double const_per_months2 /*-*/, double const_per_months3 /*-*/, double const_per_months4 /*-*/, double const_per_months5 /*-*/,
		double const_per_percent1 /*%*/, double const_per_percent2 /*%*/, double const_per_percent3 /*%*/, double const_per_percent4 /*%*/, double const_per_percent5 /*%*/,
		double const_per_upfront_rate1 /*%*/, double const_per_upfront_rate2 /*%*/, double const_per_upfront_rate3 /*%*/, double const_per_upfront_rate4 /*%*/, double const_per_upfront_rate5 /*%*/,
		double & const_per_principal1 /*$*/, double & const_per_principal2 /*$*/, double & const_per_principal3 /*$*/, double & const_per_principal4 /*$*/, double & const_per_principal5 /*$*/,
		double & const_per_interest1 /*$*/, double & const_per_interest2 /*$*/, double & const_per_interest3 /*$*/, double & const_per_interest4 /*$*/, double & const_per_interest5 /*$*/,
		double & const_per_total1 /*$*/, double & const_per_total2 /*$*/, double & const_per_total3 /*$*/, double & const_per_total4 /*$*/, double & const_per_total5 /*$*/,
		double & const_per_percent_total /*%*/, double & const_per_principal_total /*$*/, double & const_per_interest_total /*$*/, double & construction_financing_cost /*$*/);

	void construction_financing_loan_cost(double principal /*$*/, double interest_rate /*%*/, double term_months /*-*/, double upfront_rate /*%*/,
		double & interest /*$*/, double & total_cost /*$*/);

}


#endif
