#include "lib_battery_powerflow.h"
#include "lib_power_electronics.h"

BatteryPower::BatteryPower() :
		powerPV(0),
		powerLoad(0),
		powerBattery(0),
		powerGrid(0),
		powerGeneratedBySystem(0),
		powerPVToLoad(0),
		powerPVToBattery(0),
		powerPVToGrid(0),
		powerPVClipped(0),
		powerClippedToBattery(0),
		powerGridToBattery(0),
		powerGridToLoad(0),
		powerBatteryToLoad(0),
		powerBatteryToGrid(0),
		powerPVInverterDraw(0),
		powerSystemLoss(0),
		powerConversionLoss(0),
		powerBatteryChargeMax(0),
		powerBatteryDischargeMax(0),
		singlePointEfficiencyACToDC(0),
		singlePointEfficiencyDCToAC(0), 
		tolerance(0.001){}

BatteryPowerFlow::BatteryPowerFlow()
{
	std::unique_ptr<BatteryPower> tmp(new BatteryPower());
	m_powerFlowAC = std::move(tmp);
}
BatteryPower * BatteryPowerFlow::getBatteryPower()
{
	return m_powerFlowAC.get();
}
void BatteryPowerFlow::calculate()
{
	if (m_powerFlowAC->connectionMode == ChargeController::AC_CONNECTED){
		calculateACConnected();
	}
}

void BatteryPowerFlow::initialize()
{
	// Is there extra power from array
	if (m_powerFlowAC->powerPV > m_powerFlowAC->powerLoad)
	{
		if (m_powerFlowAC->canPVCharge)
		{
			// use all power available, it will only use what it can handle
			m_powerFlowAC->powerBattery = -(m_powerFlowAC->powerPV - m_powerFlowAC->powerLoad);
		}
		// if we want to charge from grid without charging from array
		else if (m_powerFlowAC->canGridCharge)
			m_powerFlowAC->powerBattery = -m_powerFlowAC->powerBatteryChargeMax;
	}
	// Or, is the demand greater than or equal to what the array provides
	else if (m_powerFlowAC->powerLoad >= m_powerFlowAC->powerPV)
	{
		// try to discharge full amount.  Will only use what battery can provide
		if (m_powerFlowAC->canDischarge)
		{
			m_powerFlowAC->powerBattery = (m_powerFlowAC->powerLoad - m_powerFlowAC->powerPV) * 1.1;
		}
		// if we want to charge from grid instead of discharging
		else if (m_powerFlowAC->canGridCharge)
			m_powerFlowAC->powerBattery = -m_powerFlowAC->powerBatteryChargeMax;
	}
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
		if (m_powerFlowAC->canPVCharge){
			P_pv_to_batt = fabs(P_battery);
			if (P_pv_to_batt > P_pv)
			{
				P_pv_to_batt = P_pv;
			}
		}
		if (m_powerFlowAC->canGridCharge){
			P_grid_to_batt = fabs(P_battery) - P_pv_to_batt;
		}

		// Now calculate other power flow quantities
		P_pv_to_load = P_pv - P_pv_to_batt;
		if (P_pv_to_load > P_load){
			P_pv_to_load = P_load;
			P_pv_to_grid = P_pv - P_pv_to_batt - P_pv_to_load;
		}
	}
	else
	{
		// Quickly test if battery is discharging erroneously
		if (!m_powerFlowAC->canDischarge & P_battery > 0) {
			P_batt_to_grid = P_batt_to_load = 0;
			P_battery = 0;
		}

		P_pv_to_load = P_pv;
		if (P_pv >= P_load)
		{
			P_pv_to_load = P_load;
			P_batt_to_load = 0;

			// discharging to grid
			P_pv_to_grid = P_pv - P_pv_to_load;
		}
		else {
			P_batt_to_load = std::fmin(P_battery, P_load - P_pv_to_load);
		}
		P_batt_to_grid = P_battery - P_batt_to_load;

	}

	// compute losses
	P_pv_to_batt_loss = P_pv_to_batt * (1 - m_powerFlowAC->singlePointEfficiencyACToDC);
	P_grid_to_batt_loss = P_grid_to_batt *(1 - m_powerFlowAC->singlePointEfficiencyACToDC);

	// Compute total system output and grid power flow
	P_grid_to_load = P_load - P_pv_to_load - P_batt_to_load;
	P_gen = P_pv + P_battery + P_inverter_draw - P_system_loss;

	// Grid charging loss accounted for in P_battery_ac 
	P_grid = P_gen - P_load;

	// Error checking for battery charging
	if (P_pv_to_batt + P_grid_to_batt != fabs(P_battery)) {
		P_grid_to_batt = fabs(P_battery) - P_pv_to_batt;
	}

	// Error checking for power to load
	if (P_pv_to_load + P_grid_to_load + P_batt_to_load != P_load)
		P_grid_to_load = P_load - P_pv_to_load - P_batt_to_load;

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

