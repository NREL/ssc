#define _TCSTYPEINTERFACE_
#include "tcstype.h"

using namespace std;

enum{	//Parameters
		P_P_storage_pump,
		P_piping_loss,   
		P_piping_length, 
		P_design_power,  
		P_recirc_htr_eff,
		P_design_eff,    
		P_night_recirc,  
		P_pb_fixed_par,  
		P_aux_par,       
		P_aux_par_f,     
		P_aux_par_0,     
		P_aux_par_1,     
		P_aux_par_2,     
		P_bop_par,       
		P_bop_par_f,     
		P_bop_par_0,     
		P_bop_par_1,     
		P_bop_par_2,     
		P_storage_bypass,

		//Inputs
		I_flow_from_storage,
		I_P_cooling_tower,  
		I_P_tower_pump,     
		I_P_helio_track,    
		I_P_plant_output,   
		I_eta_cycle,        
		I_P_cold_tank,      
		I_P_hot_tank,       
		I_P_tower_conv,     
		I_P_tower_rad,      
		I_recirc_source,    
		I_ref_htf_flow,     
		I_aux_power,     
		I_P_htf_pump,

		//Outputs
		O_P_storage_pump_tot, 
		O_P_plant_balance_tot,
		O_P_cooling_tower_tot,
		O_P_piping_tot,       
		O_P_parasitics,       
		O_P_out_net,          
		O_P_tank_heater,      
		O_P_tower_par,        
		O_P_fixed,            
		O_P_aux,              

		//N_MAX
		N_MAX};

tcsvarinfo sam_mw_pt_type228_variables[] = {
	//PARAMETERS
	{TCS_PARAM,   TCS_NUMBER,   P_P_storage_pump,   "P_storage_pump",      "Storage pump power, rated per MWt of storage use",               "MWe/MWt",   "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_piping_loss,      "Piping_loss",         "Thermal loss per meter of piping",                               "Wt/m",      "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_piping_length,    "Piping_length",       "Total length of exposed piping",                                 "m",         "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_design_power,     "Design_power",        "Power production at design conditions",                          "MWe",       "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_recirc_htr_eff,   "recirc_htr_eff",      "Recirculation heater efficiency",                                "none",      "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_design_eff,       "design_eff",          "Power cycle efficiency at design",                               "none",      "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_night_recirc,     "night_recirc",        "Flag indicating whether night recirculation is allowed",         "none",      "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_pb_fixed_par,     "pb_fixed_par",        "Fixed parasitic load - runs at all times",                       "MWe/MWcap", "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_aux_par,          "aux_par",             "Aux heater, boiler parasitic",                                   "MWe/MWcap", "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_aux_par_f,        "aux_par_f",           "Aux heater, boiler parasitic - multiplying fraction",            "none",      "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_aux_par_0,        "aux_par_0",           "Aux heater, boiler parasitic - constant coefficient",            "none",      "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_aux_par_1,        "aux_par_1",           "Aux heater, boiler parasitic - linear coefficient",              "none",      "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_aux_par_2,        "aux_par_2",           "Aux heater, boiler parasitic - quadratic coefficient",           "none",      "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_bop_par,          "bop_par",             "Balance of plant parasitic power fraction",                      "MWe/MWcap", "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_bop_par_f,        "bop_par_f",           "Balance of plant parasitic power fraction - mult frac",          "none",      "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_bop_par_0,        "bop_par_0",           "Balance of plant parasitic power fraction - const coeff",        "none",      "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_bop_par_1,        "bop_par_1",           "Balance of plant parasitic power fraction - linear coeff",       "none",      "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_bop_par_2,        "bop_par_2",           "Balance of plant parasitic power fraction - quadratic coeff",    "none",      "", "", ""},
	{TCS_PARAM,   TCS_NUMBER,   P_storage_bypass,   "storage_bypass",      "Flag indicating whether the hot salt pump always runs w/ PB",    "none",      "", "", ""},

	//INPUTS
	{TCS_INPUT,   TCS_NUMBER,   I_flow_from_storage, "flow_from_storage",  "Flow rate from storage",                                         "kg/hr",     "", "", ""},
	{TCS_INPUT,   TCS_NUMBER,   I_P_cooling_tower,   "P_cooling_tower",    "Cooling tower parasitic power fraction",                         "MWe",       "", "", ""},
	{TCS_INPUT,   TCS_NUMBER,   I_P_tower_pump,      "P_tower_pump",       "Reported tower pump power",                                      "MWe",       "", "", ""},
	{TCS_INPUT,   TCS_NUMBER,   I_P_helio_track,     "P_helio_track",      "Reported heliostat tracking power",                              "MWe",       "", "", ""},
	{TCS_INPUT,   TCS_NUMBER,   I_P_plant_output,    "P_plant_output",     "Reported plant power output",                                    "MWe",       "", "", ""},
	{TCS_INPUT,   TCS_NUMBER,   I_eta_cycle,         "eta_cycle",          "Power cycle efficiency",                                         "none",      "", "", ""},
	{TCS_INPUT,   TCS_NUMBER,   I_P_cold_tank,       "P_cold_tank",        "Cold tank heater parasitic power",                               "MWe",       "", "", "0.0"},
	{TCS_INPUT,   TCS_NUMBER,   I_P_hot_tank,        "P_hot_tank",         "Hot tank heater parasitic power",                                "MWe",       "", "", "0.0"},
	{TCS_INPUT,   TCS_NUMBER,   I_P_tower_conv,      "P_tower_conv",       "Reported tower convection loss",                                 "MWt",       "", "", ""},
	{TCS_INPUT,   TCS_NUMBER,   I_P_tower_rad,       "P_tower_rad",        "Reported tower radiation loss",                                  "MWt",       "", "", ""},
	{TCS_INPUT,   TCS_NUMBER,   I_recirc_source,     "recirc_source",      "Recirculation heater control",                                   "none",      "", "", ""},
	{TCS_INPUT,   TCS_NUMBER,   I_ref_htf_flow,      "ref_htf_flow",       "HTF flow rate through the power cycle at design",                "kg/hr",     "", "", ""},
	{TCS_INPUT,   TCS_NUMBER,   I_aux_power,         "aux_power",          "Auxiliary heater thermal power output",                          "MWt",       "", "", ""},
	{TCS_INPUT,   TCS_NUMBER,   I_P_htf_pump,        "P_htf_pump",         "HTF pumping power",                                              "MWe",       "", "", ""},

	//OUTPUTS
	{TCS_OUTPUT,  TCS_NUMBER,   O_P_storage_pump_tot,   "P_storage_pump_tot",   "Total storage pump parasitic power",                        "MWe",       "", "", ""},
	{TCS_OUTPUT,  TCS_NUMBER,   O_P_plant_balance_tot,  "P_plant_balance_tot",  "Total balance of plant parasitic power",                    "MWe",       "", "", ""},
	{TCS_OUTPUT,  TCS_NUMBER,   O_P_cooling_tower_tot,  "P_cooling_tower_tot",  "Total cooling tower parasitic power",                       "MWe",       "", "", ""},
	{TCS_OUTPUT,  TCS_NUMBER,   O_P_piping_tot,         "P_piping_tot",         "Total piping loss parasitic power",                         "MWe",       "", "", ""},
	{TCS_OUTPUT,  TCS_NUMBER,   O_P_parasitics,         "P_parasitics",         "Overall parasitic losses",                                  "MWe",       "", "", ""},
	{TCS_OUTPUT,  TCS_NUMBER,   O_P_out_net,            "P_out_net",            "Power to the grid after parasitic losses",                  "MWe",       "", "", ""},
	{TCS_OUTPUT,  TCS_NUMBER,   O_P_tank_heater,        "P_tank_heater",        "Total tank heater parasitic power",                         "MWe",       "", "", ""},
	{TCS_OUTPUT,  TCS_NUMBER,   O_P_tower_par,          "P_tower_par",          "Total tower heater parasitic loss",                         "MWe",       "", "", ""},
	{TCS_OUTPUT,  TCS_NUMBER,   O_P_fixed,              "P_fixed",              "Total fixed parasitic loss",                                "MWe",       "", "", ""},
	{TCS_OUTPUT,  TCS_NUMBER,   O_P_aux,                "P_aux",                "Total auxiliary heater parasitic loss",                     "MWe",       "", "", ""},

	//N_MAX
	{TCS_INVALID, TCS_INVALID, N_MAX,			0,					0, 0, 0, 0, 0	} } ;
	
	
class sam_mw_pt_type228 : public tcstypeinterface
{
private:

	double P_storage_pump;
	double Piping_loss;
	double Piping_length;
	double Design_power;
	double recirc_htr_eff;
	double design_eff;
	bool night_recirc;
	double pb_fixed_par;
	double aux_par;
	double aux_par_f;
	double aux_par_0;
	double aux_par_1;
	double aux_par_2;
	double bop_par;
	double bop_par_f;
	double bop_par_0;
	double bop_par_1;
	double bop_par_2;
	bool storage_bypass;

public:
	sam_mw_pt_type228( tcscontext *cst, tcstypeinfo *ti)
		: tcstypeinterface( cst, ti)
	{
		P_storage_pump = std::numeric_limits<double>::quiet_NaN();
		Piping_loss = std::numeric_limits<double>::quiet_NaN();
		Piping_length = std::numeric_limits<double>::quiet_NaN();
		Design_power = std::numeric_limits<double>::quiet_NaN();
		recirc_htr_eff = std::numeric_limits<double>::quiet_NaN();
		design_eff = std::numeric_limits<double>::quiet_NaN();
		pb_fixed_par = std::numeric_limits<double>::quiet_NaN();
		aux_par = std::numeric_limits<double>::quiet_NaN();
		aux_par_f = std::numeric_limits<double>::quiet_NaN();
		aux_par_0 = std::numeric_limits<double>::quiet_NaN();
		aux_par_1 = std::numeric_limits<double>::quiet_NaN();
		aux_par_2 = std::numeric_limits<double>::quiet_NaN();
		bop_par = std::numeric_limits<double>::quiet_NaN();
		bop_par_f = std::numeric_limits<double>::quiet_NaN();
		bop_par_0 = std::numeric_limits<double>::quiet_NaN();
		bop_par_1 = std::numeric_limits<double>::quiet_NaN();
		bop_par_2 = std::numeric_limits<double>::quiet_NaN();
	 
	}

	virtual ~sam_mw_pt_type228()
	{

	}

	virtual int init()
	{
		P_storage_pump = value( P_P_storage_pump );
		Piping_loss = value( P_piping_loss );
		Piping_length = value( P_piping_length );
		Design_power = value( P_design_power );
		recirc_htr_eff = value( P_recirc_htr_eff );
		design_eff = value( P_design_eff );
		night_recirc = (bool) value( P_night_recirc );
		pb_fixed_par = value( P_pb_fixed_par );
		aux_par = value( P_aux_par );
		aux_par_f = value( P_aux_par_f );
		aux_par_0 = value( P_aux_par_0 );     
		aux_par_1 = value( P_aux_par_1 );
		aux_par_2 = value( P_aux_par_2 );     
		bop_par = value( P_bop_par ); 
		bop_par_f = value( P_bop_par_f );     
		bop_par_0 = value( P_bop_par_0 ); 
		bop_par_1 = value( P_bop_par_1 );     
		bop_par_2 = value( P_bop_par_2 ); 
		storage_bypass = (bool) value( P_storage_bypass );

		return 0;
	}

	virtual int call( double time, double step, int ncall )
	{						
		double flow_from_storage = value( I_flow_from_storage );
		double P_cooling_tower = value( I_P_cooling_tower );
		double P_tower_pump = value( I_P_tower_pump );
		double P_helio_track = value( I_P_helio_track );
		double P_plant_output = value( I_P_plant_output );
		double eta_cycle = value( I_eta_cycle );
		double P_cold_tank = value( I_P_cold_tank );
		double P_hot_tank = value( I_P_hot_tank );
		double P_tower_conv = value( I_P_tower_conv );
		double P_tower_rad = value( I_P_tower_rad );
		int recirc_source = (int) value( I_recirc_source );
		double ref_htf_flow = value( I_ref_htf_flow );
		double aux_power = value( I_aux_power );
		double P_htf_pump = value( I_P_htf_pump );

		double P_ratio = P_plant_output/Design_power;
		double aux_ratio = aux_power/Design_power/design_eff;

		double P_storage_pump_tot;
		if( storage_bypass )
			P_storage_pump_tot = P_storage_pump*flow_from_storage/max( ref_htf_flow, 1.E-6 )*Design_power/design_eff;	//Hot pump operates only when storage is dispatched
		else
			P_storage_pump_tot = P_storage_pump*P_plant_output/design_eff;	//Hot pump operates when any hot HTF is sent to the power block

		double P_fixed = pb_fixed_par * Design_power;

		double P_plant_balance_tot;
		if( P_plant_output > 0.0 )
			P_plant_balance_tot = Design_power * bop_par * bop_par_f * (bop_par_0 + bop_par_1*(P_ratio) + bop_par_2*pow(P_ratio,2));
		else
			P_plant_balance_tot = 0.0;

		double P_aux;
		if( aux_ratio > 0.0 )
			P_aux = Design_power * aux_par * aux_par_f * (aux_par_0 + aux_par_1*aux_ratio + aux_par_2*pow(aux_ratio,2));	
		else
			P_aux = 0.0;

		double P_cooling_tower_tot = P_cooling_tower;

		double P_piping_tot = Piping_loss * Piping_length * eta_cycle * P_plant_output / (Design_power*1.E6);	//MWe

		double P_tank_heater = (P_cold_tank + P_hot_tank);		//MWe

		double P_tower_par;
		if( recirc_source == 2 && night_recirc )
			P_tower_par = (P_tower_conv + P_tower_rad)/recirc_htr_eff;
		else
			P_tower_par = 0.0;

		// 7.8.13, twn: Add htf pumping power to parasitic calcs
		double P_parasitics = P_storage_pump_tot + P_plant_balance_tot + P_cooling_tower_tot + P_fixed + P_tower_pump + P_helio_track + P_piping_tot + P_tank_heater + P_tower_par + P_aux + P_htf_pump;

		value( O_P_storage_pump_tot, P_storage_pump_tot );
		value( O_P_plant_balance_tot, P_plant_balance_tot );
		value( O_P_cooling_tower_tot, P_cooling_tower_tot );
		value( O_P_piping_tot, P_piping_tot );
		value( O_P_parasitics, P_parasitics );
		value( O_P_out_net, P_plant_output - P_parasitics );
		value( O_P_tank_heater, P_tank_heater );
		value( O_P_tower_par, P_tower_par );
		value( O_P_fixed, P_fixed );
		value( O_P_aux, P_aux );

		//value( O_pparasi, pparasi/3.6e6 );	// [MW], convert from kJ/hr: Parasitic power for tracking
		//value( O_eta_field, eta_field );	// [-], field efficiency


		return 0;
	}

	virtual int converged( double time )
	{
		
		return 0;
	}

};

TCS_IMPLEMENT_TYPE( sam_mw_pt_type228, "Power Tower Parasitics", "Ty Neises", 1, sam_mw_pt_type228_variables, NULL, 1 )