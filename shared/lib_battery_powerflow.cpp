#include "lib_battery_powerflow.h"

BatteryPower::BatteryPower() :
		powerPV(0),
		powerLoad(0),
		powerBattery(0),
		powerGrid(0),
		powerGeneratedBySystem(0),
		powerPVToLoad(0),
		powerPVToBattery(0),
		powerPVToGrid(0),
		powerGridToBattery(0),
		powerGridToLoad(0),
		powerBatteryToLoad(0),
		powerBatteryToGrid(0),
		powerPVInverterDraw(0),
		powerSystemLoss(0),
		singlePointEfficiencyACToDC(0),
		singlePointEfficiencyDCToAC(0), 
		tolerance(0.001){}

BatteryPowerFlow::BatteryPowerFlow()
{
	std::unique_ptr<BatteryPower> tmp(new BatteryPower());
	m_powerFlowAC = std::move(tmp);

	std::unique_ptr<BatteryPower> tmp2(new BatteryPower());
	m_powerFlowDC = std::move(tmp2);
}

void BatteryPowerFlow::calculateACConnected()
{
	double P_battery = m_powerFlowAC->powerBattery;
	double P_pv = m_powerFlowAC->powerPV;
	double P_inverter_draw = m_powerFlowAC->powerPVInverterDraw;
	double P_load = m_powerFlowAC->powerLoad;
	double P_system_loss = m_powerFlowAC->powerSystemLoss;
	double P_pv_to_batt, P_grid_to_batt, P_batt_to_load, P_grid_to_load, P_pv_to_load, P_pv_to_grid, P_batt_to_grid, P_gen, P_grid, P_grid_to_batt_loss, P_batt_to_load_loss, P_pv_to_batt_loss;
    P_pv_to_batt = P_grid_to_batt = P_batt_to_load = P_grid_to_load = P_pv_to_load = P_pv_to_grid = P_batt_to_grid = P_gen = P_grid = P_grid_to_batt_loss = P_batt_to_load_loss = P_pv_to_batt_loss = 0;

	// charging 
	if (P_battery <= 0)
	{
		// PV always charges battery first
		P_pv_to_batt = fabs(P_battery);

		// don't include any conversion efficiencies, want to compare AC to AC
		if (P_pv_to_batt > P_pv)
		{
			P_pv_to_batt = P_pv;
			P_grid_to_batt = fabs(P_battery) - P_pv_to_batt;
		}

		P_pv_to_load = P_pv - P_pv_to_batt;
		if (P_pv_to_load > P_load)
		{
			P_pv_to_load = P_load;
			P_pv_to_grid = P_pv - P_pv_to_batt - P_pv_to_load;
		}
	}
	else
	{
		P_pv_to_load = P_pv;
		if (P_pv >= P_load)
		{
			P_pv_to_load = P_load;
			P_batt_to_load = 0;

			// discharging to grid
			P_pv_to_grid = P_pv - P_pv_to_load;
			P_batt_to_grid = P_battery - P_batt_to_load;
		}
		else if (P_battery < P_load - P_pv_to_load)
			P_batt_to_load = P_battery;
		else
			// probably shouldn't happen, need to iterate and reduce I from battery
			P_batt_to_load = P_load - P_pv_to_load;
	}

	// compute losses
	P_pv_to_batt_loss = P_pv_to_batt * (1 - m_powerFlowAC->singlePointEfficiencyACToDC);
	P_grid_to_batt_loss = P_grid_to_batt *(1 - m_powerFlowAC->singlePointEfficiencyACToDC);


	P_grid_to_load = P_load - P_pv_to_load - P_batt_to_load;
	P_gen = P_pv + P_battery + P_inverter_draw - P_system_loss;

	// Grid charging loss accounted for in P_battery_ac 
	P_grid = P_gen - P_load;

	// check tolerances
	if (fabs(P_grid_to_load) < m_powerFlowAC->tolerance)
		P_grid_to_load = 0;
	if (fabs(P_grid_to_batt) <  m_powerFlowAC->tolerance)
		P_grid_to_batt = 0;
	if (fabs(P_grid) <  m_powerFlowAC->tolerance)
		P_grid = 0;

	// assign outputs
	m_powerFlowAC->powerBattery = P_battery;
	m_powerFlowAC->powerGrid = P_grid;
	m_powerFlowAC->powerGeneratedBySystem = P_gen;
	m_powerFlowAC->powerPVToLoad = P_pv_to_load;
	m_powerFlowAC->powerPVToBattery = P_pv_to_batt;
	m_powerFlowAC->powerPVToGrid = P_pv_to_grid;
	m_powerFlowAC->powerGridToBattery = P_grid_to_batt;
	m_powerFlowAC->powerGridToLoad = P_grid_to_load;
	m_powerFlowAC->powerBatteryToLoad = P_batt_to_load;
	m_powerFlowAC->powerBatteryToGrid = P_batt_to_grid;
}

void BatteryPowerFlow::calculateDCConnected()
{

}

