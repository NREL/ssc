#include "lib_battery_powerflow.h"
#include "lib_power_electronics.h"

BatteryPower::BatteryPower(double dtHour) :
		dtHour(dtHour),
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
		powerBatteryChargeMax(0),
		powerBatteryDischargeMax(0),
		powerSystemLoss(0),
		powerConversionLoss(0),
		connectionMode(0),
		singlePointEfficiencyACToDC(0.96),
		singlePointEfficiencyDCToAC(0.96), 
		singlePointEfficiencyDCToDC(0.99),
		canPVCharge(false),
		canClipCharge(false),
		canGridCharge(false),
		canDischarge(false),
		stateOfChargeMax(1),
		stateOfChargeMin(0),
		tolerance(0.001){}

BatteryPower::BatteryPower(const BatteryPower& batteryPower) { /* nothing to do */ }

void BatteryPower::setSharedInverter(SharedInverter * a_sharedInverter) {
	sharedInverter = a_sharedInverter;
}

BatteryPowerFlow::BatteryPowerFlow(double dtHour)
{
	std::unique_ptr<BatteryPower> tmp(new BatteryPower(dtHour));
	m_BatteryPower = std::move(tmp);
}
BatteryPowerFlow::BatteryPowerFlow(const BatteryPowerFlow& powerFlow)
{
	std::unique_ptr<BatteryPower> tmp(new BatteryPower(*powerFlow.m_BatteryPower));
	m_BatteryPower = std::move(tmp);
}
BatteryPower * BatteryPowerFlow::getBatteryPower()
{
	return m_BatteryPower.get();
}
void BatteryPowerFlow::calculate()
{
	if (m_BatteryPower->connectionMode == ChargeController::AC_CONNECTED) {
		calculateACConnected();
	}
	else if (m_BatteryPower->connectionMode == ChargeController::DC_CONNECTED) {
		calculateDCConnected();
	}
}
void BatteryPowerFlow::initialize(double stateOfCharge)
{
	if (m_BatteryPower->connectionMode == ChargeController::AC_CONNECTED) {
		initializeAC(stateOfCharge);
	}
	// current set to the same initialization, any reason for two?
	else if (m_BatteryPower->connectionMode == ChargeController::DC_CONNECTED) {
		initializeAC(stateOfCharge);
	}
}
void BatteryPowerFlow::initializeAC(double stateOfCharge)
{
	// If the battery is allowed to discharge, do so
	if (m_BatteryPower->canDischarge && stateOfCharge > m_BatteryPower->stateOfChargeMin + 1.0)
	{
		// try to discharge full amount.  Will only use what battery can provide
		m_BatteryPower->powerBattery = m_BatteryPower->powerBatteryDischargeMax;
	}
	// Is there extra power from system
	else if (m_BatteryPower->powerPV > m_BatteryPower->powerLoad && m_BatteryPower->canPVCharge || m_BatteryPower->canGridCharge)
	{
		if (m_BatteryPower->canPVCharge)
		{
			// use all power available, it will only use what it can handle
			m_BatteryPower->powerBattery = -(m_BatteryPower->powerPV - m_BatteryPower->powerLoad);
		}
		// if we want to charge from grid in addition to, or without array, we can always charge at max power
		if (m_BatteryPower->canGridCharge) {
			m_BatteryPower->powerBattery = -m_BatteryPower->powerBatteryChargeMax;
		}
	}

}
void BatteryPowerFlow::initializeDC(double stateOfCharge) 
{
}


void BatteryPowerFlow::calculateACConnected()
{
	// The battery power is initially a DC power, which must be converted to AC for powerflow
	double P_battery_dc = m_BatteryPower->powerBattery;

	// These quantities are all AC quantities unless otherwise specified
	double P_pv = m_BatteryPower->powerPV;
	double P_inverter_draw = m_BatteryPower->powerPVInverterDraw;
	double P_load = m_BatteryPower->powerLoad;
	double P_system_loss = m_BatteryPower->powerSystemLoss;
	double P_pv_to_batt, P_grid_to_batt, P_batt_to_load, P_grid_to_load, P_pv_to_load, P_pv_to_grid, P_batt_to_grid, P_gen, P_grid, P_grid_to_batt_loss, P_batt_to_load_loss, P_pv_to_batt_loss;
    P_pv_to_batt = P_grid_to_batt = P_batt_to_load = P_grid_to_load = P_pv_to_load = P_pv_to_grid = P_batt_to_grid = P_gen = P_grid = P_grid_to_batt_loss = P_batt_to_load_loss = P_pv_to_batt_loss = 0;

	// convert the calculated DC power to AC, considering the microinverter efficiences
	double P_battery_ac = 0;
	if (P_battery_dc < 0)
		P_battery_ac = P_battery_dc / m_BatteryPower->singlePointEfficiencyACToDC;
	else if (P_battery_dc > 0)
		P_battery_ac = P_battery_dc * m_BatteryPower->singlePointEfficiencyDCToAC;


	// charging 
	if (P_battery_ac <= 0)
	{
		// PV always goes to load first
		P_pv_to_load = P_pv;
		if (P_pv_to_load > P_load) {
			P_pv_to_load = P_load;

		}
		// Excess PV can go to battery
		if (m_BatteryPower->canPVCharge){
			P_pv_to_batt = fabs(P_battery_ac);
			if (P_pv_to_batt > P_pv - P_pv_to_load)
			{
				P_pv_to_batt = P_pv - P_pv_to_load;
			}
		}
		// Grid can also charge battery
		if (m_BatteryPower->canGridCharge){
			P_grid_to_batt = fabs(P_battery_ac) - P_pv_to_batt;
		}

		P_pv_to_grid = P_pv - P_pv_to_batt - P_pv_to_load;

		// Error checking for battery charging
		if (P_pv_to_batt + P_grid_to_batt != fabs(P_battery_ac)) {
			P_grid_to_batt = fabs(P_battery_ac) - P_pv_to_batt;
		}
	}
	else
	{
		// Test if battery is discharging erroneously
		if (!m_BatteryPower->canDischarge & P_battery_ac > 0) {
			P_batt_to_grid = P_batt_to_load = 0;
			P_battery_ac = 0;
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
			P_batt_to_load = std::fmin(P_battery_ac, P_load - P_pv_to_load);
		}
		P_batt_to_grid = P_battery_ac - P_batt_to_load;
	}

	// compute losses
	P_pv_to_batt_loss = P_pv_to_batt * (1 - m_BatteryPower->singlePointEfficiencyACToDC);
	P_grid_to_batt_loss = P_grid_to_batt *(1 - m_BatteryPower->singlePointEfficiencyACToDC);
	P_batt_to_load_loss = P_batt_to_load * (1 / m_BatteryPower->singlePointEfficiencyDCToAC - 1);

	// Compute total system output and grid power flow
	P_grid_to_load = P_load - P_pv_to_load - P_batt_to_load;
	P_gen = P_pv + P_battery_ac + P_inverter_draw - P_system_loss;

	// Grid charging loss accounted for in P_battery_ac 
	P_grid = P_gen - P_load;

	// Error checking for power to load
	if (P_pv_to_load + P_grid_to_load + P_batt_to_load != P_load)
		P_grid_to_load = P_load - P_pv_to_load - P_batt_to_load;

	// check tolerances
	if (fabs(P_grid_to_load) < m_BatteryPower->tolerance)
		P_grid_to_load = 0;
	if (fabs(P_grid_to_batt) <  m_BatteryPower->tolerance)
		P_grid_to_batt = 0;
	if (fabs(P_grid) <  m_BatteryPower->tolerance)
		P_grid = 0;

	// assign outputs
	m_BatteryPower->powerBattery = P_battery_ac;
	m_BatteryPower->powerGrid = P_grid;
	m_BatteryPower->powerGeneratedBySystem = P_gen;
	m_BatteryPower->powerPVToLoad = P_pv_to_load;
	m_BatteryPower->powerPVToBattery = P_pv_to_batt;
	m_BatteryPower->powerPVToGrid = P_pv_to_grid;
	m_BatteryPower->powerGridToBattery = P_grid_to_batt;
	m_BatteryPower->powerGridToLoad = P_grid_to_load;
	m_BatteryPower->powerBatteryToLoad = P_batt_to_load;
	m_BatteryPower->powerBatteryToGrid = P_batt_to_grid;
	m_BatteryPower->powerConversionLoss = P_batt_to_load_loss + P_grid_to_batt_loss + P_pv_to_batt_loss;
}

void BatteryPowerFlow::calculateDCConnected()
{
	double P_inverter_draw = m_BatteryPower->powerPVInverterDraw;
	double P_load = m_BatteryPower->powerLoad;
	double P_system_loss = m_BatteryPower->powerSystemLoss;
	double P_pv_to_batt, P_grid_to_batt, P_batt_to_load, P_grid_to_load, P_pv_to_load, P_pv_to_grid, P_batt_to_grid, P_gen, P_grid, P_conversion_loss;
	P_pv_to_batt = P_grid_to_batt = P_batt_to_load = P_grid_to_load = P_pv_to_load = P_pv_to_grid = P_batt_to_grid = P_gen = P_grid = P_conversion_loss = 0;

	// The battery power and PV power are initially DC, which must be converted to AC for powerflow
	double P_battery_dc_pre_bms = m_BatteryPower->powerBattery;
	double P_battery_dc = m_BatteryPower->powerBattery;
	double P_pv_dc = m_BatteryPower->powerPV;

	// convert the calculated DC power to DC at the PV system voltage
	if (P_battery_dc_pre_bms < 0)
		P_battery_dc = P_battery_dc_pre_bms / m_BatteryPower->singlePointEfficiencyDCToDC;
	else if (P_battery_dc > 0)
		P_battery_dc = P_battery_dc_pre_bms * m_BatteryPower->singlePointEfficiencyDCToDC;

	
	double P_battery_ac, P_pv_ac, P_gen_ac;
	P_battery_ac = P_pv_ac = P_gen_ac = 0;


	double P_gen_dc = P_pv_dc + P_battery_dc;
	
	// if battery is charging, must covert negative power to positive for inverter
	bool charging = false;
	double powerToInvert = P_gen_dc * util::kilowatt_to_watt;
	if (P_gen_dc < 0) {
		powerToInvert *= -1;
		charging = true;
	}
	// in the event that PV system isn't operating, assume battery BMS converts battery voltage to nominal inverter input
	double voltage = m_BatteryPower->voltageSystem;
	if (voltage <= 0)
		voltage = m_BatteryPower->sharedInverter->getInverterDCNominalVoltage();

	// convert the DC power to AC
	m_BatteryPower->sharedInverter->calculateACPower(powerToInvert, voltage);
	P_gen_ac = m_BatteryPower->sharedInverter->powerAC_kW;


	if (charging)
		P_gen_ac *= -1;

	P_battery_ac = m_BatteryPower->sharedInverter->efficiencyAC * 0.01 * P_battery_dc;

	// charging 
	if (P_battery_ac <= 0)
	{
		// PV always goes to load first
		P_pv_to_load = P_pv_ac;
		if (P_pv_to_load > P_load) {
			P_pv_to_load = P_load;

		}
		// Excess PV can go to battery
		if (m_BatteryPower->canPVCharge) {
			P_pv_to_batt = fabs(P_battery_ac);
			if (P_pv_to_batt > P_pv_ac - P_pv_to_load)
			{
				P_pv_to_batt = P_pv_ac - P_pv_to_load;
			}
		}
		// Grid can also charge battery
		if (m_BatteryPower->canGridCharge) {
			P_grid_to_batt = fabs(P_battery_ac) - P_pv_to_batt;
		}

		P_pv_to_grid = P_pv_ac - P_pv_to_batt - P_pv_to_load;

		// Error checking for battery charging
		if (P_pv_to_batt + P_grid_to_batt != fabs(P_battery_ac)) {
			P_grid_to_batt = fabs(P_battery_ac) - P_pv_to_batt;
		}
	}
	else
	{
		// Test if battery is discharging erroneously
		if (!m_BatteryPower->canDischarge & P_battery_ac > 0) {
			P_batt_to_grid = P_batt_to_load = 0;
			P_battery_ac = 0;
		}

		P_pv_to_load = P_pv_ac;
		if (P_pv_ac >= P_load)
		{
			P_pv_to_load = P_load;
			P_batt_to_load = 0;

			// discharging to grid
			P_pv_to_grid = P_pv_ac - P_pv_to_load;
		}
		else {
			P_batt_to_load = std::fmin(P_battery_ac, P_load - P_pv_to_load);
		}
		P_batt_to_grid = P_battery_ac - P_batt_to_load;
	}

	// compute losses
	P_conversion_loss = P_gen_dc - P_gen_ac + P_battery_dc_pre_bms - P_battery_dc;
	
	// Compute total system output and grid power flow, inverter draw is built into P_pv_ac
	P_grid_to_load = P_load - P_pv_to_load - P_batt_to_load;
	P_gen_ac -= P_system_loss;

	// Grid charging loss accounted for in P_battery_ac 
	P_grid = P_gen - P_load;

	// Error checking for power to load
	if (P_pv_to_load + P_grid_to_load + P_batt_to_load != P_load)
		P_grid_to_load = P_load - P_pv_to_load - P_batt_to_load;

	// check tolerances
	if (fabs(P_grid_to_load) < m_BatteryPower->tolerance)
		P_grid_to_load = 0;
	if (fabs(P_grid_to_batt) <  m_BatteryPower->tolerance)
		P_grid_to_batt = 0;
	if (fabs(P_grid) <  m_BatteryPower->tolerance)
		P_grid = 0;

	// assign outputs
	m_BatteryPower->powerBattery = P_battery_ac;
	m_BatteryPower->powerGrid = P_grid;
	m_BatteryPower->powerGeneratedBySystem = P_gen;
	m_BatteryPower->powerPVToLoad = P_pv_to_load;
	m_BatteryPower->powerPVToBattery = P_pv_to_batt;
	m_BatteryPower->powerPVToGrid = P_pv_to_grid;
	m_BatteryPower->powerGridToBattery = P_grid_to_batt;
	m_BatteryPower->powerGridToLoad = P_grid_to_load;
	m_BatteryPower->powerBatteryToLoad = P_batt_to_load;
	m_BatteryPower->powerBatteryToGrid = P_batt_to_grid;
	m_BatteryPower->powerConversionLoss = P_conversion_loss;
}

