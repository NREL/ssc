#include "lib_battery_powerflow.h"
#include "lib_power_electronics.h"

BatteryPower::BatteryPower(double dtHour) :
		dtHour(dtHour),
		powerPV(0),
		powerLoad(0),
		powerBattery(0),
		powerBatteryTarget(0),
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

BatteryPower::BatteryPower(const BatteryPower& ) { /* nothing to do */ }

void BatteryPower::setSharedInverter(SharedInverter * a_sharedInverter) {
	sharedInverter = a_sharedInverter;
}

void BatteryPower::reset()
{
	powerBattery = 0;
	powerBatteryTarget = 0;
	powerBatteryToGrid = 0;
	powerBatteryToLoad = 0;
	powerClippedToBattery = 0;
	powerConversionLoss = 0;
	powerGeneratedBySystem = 0;
	powerGrid = 0;
	powerGridToBattery = 0;
	powerGridToLoad = 0;
	powerLoad = 0;
	powerPV = 0;
	powerPVClipped = 0;
	powerPVInverterDraw = 0;
	powerPVToBattery = 0;
	powerPVToGrid = 0;
	powerPVToLoad = 0;
	voltageSystem = 0;
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
	// If the battery is allowed to discharge, do so
	if (m_BatteryPower->canDischarge && stateOfCharge > m_BatteryPower->stateOfChargeMin + 1.0 &&
		(m_BatteryPower->powerPV < m_BatteryPower->powerLoad || m_BatteryPower->meterPosition == dispatch_t::FRONT))
	{
		// try to discharge full amount.  Will only use what battery can provide
		m_BatteryPower->powerBattery = m_BatteryPower->powerBatteryDischargeMax;
	}
	// Is there extra power from system
	else if ((m_BatteryPower->powerPV > m_BatteryPower->powerLoad && m_BatteryPower->canPVCharge) || m_BatteryPower->canGridCharge)
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

void BatteryPowerFlow::reset()
{
	m_BatteryPower->reset();
}


void BatteryPowerFlow::calculateACConnected()
{
	// The battery power is initially a DC power, which must be converted to AC for powerflow
	double P_battery_dc = m_BatteryPower->powerBattery;

	// These quantities are all AC quantities in KW unless otherwise specified
	double P_pv_ac = m_BatteryPower->powerPV;
	double P_inverter_draw_ac = m_BatteryPower->powerPVInverterDraw;
	double P_load_ac = m_BatteryPower->powerLoad;
	double P_system_loss_ac = m_BatteryPower->powerSystemLoss;
	double P_pv_to_batt_ac, P_grid_to_batt_ac, P_batt_to_load_ac, P_grid_to_load_ac, P_pv_to_load_ac, P_pv_to_grid_ac, P_batt_to_grid_ac, P_gen_ac, P_grid_ac, P_grid_to_batt_loss_ac, P_batt_to_load_loss_ac, P_pv_to_batt_loss_ac;
    P_pv_to_batt_ac = P_grid_to_batt_ac = P_batt_to_load_ac = P_grid_to_load_ac = P_pv_to_load_ac = P_pv_to_grid_ac = P_batt_to_grid_ac = P_gen_ac = P_grid_ac = P_grid_to_batt_loss_ac = P_batt_to_load_loss_ac = P_pv_to_batt_loss_ac = 0;

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
		P_pv_to_load_ac = P_pv_ac;
		if (P_pv_to_load_ac > P_load_ac) {
			P_pv_to_load_ac = P_load_ac;

		}
		// Excess PV can go to battery
		if (m_BatteryPower->canPVCharge){
			P_pv_to_batt_ac = fabs(P_battery_ac);
			if (P_pv_to_batt_ac > P_pv_ac - P_pv_to_load_ac)
			{
				P_pv_to_batt_ac = P_pv_ac - P_pv_to_load_ac;
			}
		}
		// Grid can also charge battery
		if (m_BatteryPower->canGridCharge){
			P_grid_to_batt_ac = fabs(P_battery_ac) - P_pv_to_batt_ac;
		}

		P_pv_to_grid_ac = P_pv_ac - P_pv_to_batt_ac - P_pv_to_load_ac;

		// Error checking for battery charging
		if (P_pv_to_batt_ac + P_grid_to_batt_ac != fabs(P_battery_ac)) {
			P_grid_to_batt_ac = fabs(P_battery_ac) - P_pv_to_batt_ac;
		}
	}
	else
	{
		// Test if battery is discharging erroneously
		if (!m_BatteryPower->canDischarge && P_battery_ac > 0) {
			P_batt_to_grid_ac = P_batt_to_load_ac = 0;
			P_battery_ac = 0;
		}

		P_pv_to_load_ac = P_pv_ac;
		if (P_pv_ac >= P_load_ac)
		{
			P_pv_to_load_ac = P_load_ac;
			P_batt_to_load_ac = 0;

			// discharging to grid
			P_pv_to_grid_ac = P_pv_ac - P_pv_to_load_ac;
		}
		else {
			P_batt_to_load_ac = std::fmin(P_battery_ac, P_load_ac - P_pv_to_load_ac);
		}
		P_batt_to_grid_ac = P_battery_ac - P_batt_to_load_ac;
	}

	// compute losses
	P_pv_to_batt_loss_ac = P_pv_to_batt_ac * (1 - m_BatteryPower->singlePointEfficiencyACToDC);
	P_grid_to_batt_loss_ac = P_grid_to_batt_ac *(1 - m_BatteryPower->singlePointEfficiencyACToDC);
	P_batt_to_load_loss_ac = P_batt_to_load_ac * (1 / m_BatteryPower->singlePointEfficiencyDCToAC - 1);

	// Compute total system output and grid power flow
	P_grid_to_load_ac = P_load_ac - P_pv_to_load_ac - P_batt_to_load_ac;
	P_gen_ac = P_pv_ac + P_battery_ac + P_inverter_draw_ac - P_system_loss_ac;

	// Grid charging loss accounted for in P_battery_ac 
	P_grid_ac = P_gen_ac - P_load_ac;

	// Error checking for power to load
	if (P_pv_to_load_ac + P_grid_to_load_ac + P_batt_to_load_ac != P_load_ac)
		P_grid_to_load_ac = P_load_ac - P_pv_to_load_ac - P_batt_to_load_ac;

	// check tolerances
	if (fabs(P_grid_to_load_ac) < m_BatteryPower->tolerance)
		P_grid_to_load_ac = 0;
	if (fabs(P_grid_to_batt_ac) <  m_BatteryPower->tolerance)
		P_grid_to_batt_ac = 0;
	if (fabs(P_grid_ac) <  m_BatteryPower->tolerance)
		P_grid_ac = 0;

	// assign outputs
	m_BatteryPower->powerBattery = P_battery_ac;
	m_BatteryPower->powerGrid = P_grid_ac;
	m_BatteryPower->powerGeneratedBySystem = P_gen_ac;
	m_BatteryPower->powerPVToLoad = P_pv_to_load_ac;
	m_BatteryPower->powerPVToBattery = P_pv_to_batt_ac;
	m_BatteryPower->powerPVToGrid = P_pv_to_grid_ac;
	m_BatteryPower->powerGridToBattery = P_grid_to_batt_ac;
	m_BatteryPower->powerGridToLoad = P_grid_to_load_ac;
	m_BatteryPower->powerBatteryToLoad = P_batt_to_load_ac;
	m_BatteryPower->powerBatteryToGrid = P_batt_to_grid_ac;
	m_BatteryPower->powerConversionLoss = P_batt_to_load_loss_ac + P_grid_to_batt_loss_ac + P_pv_to_batt_loss_ac;
}

void BatteryPowerFlow::calculateDCConnected()
{
	// Quantities are AC in KW unless otherwise specified
	double P_load_ac = m_BatteryPower->powerLoad;
	double P_system_loss_ac = m_BatteryPower->powerSystemLoss;
	double P_battery_ac, P_pv_ac, P_gen_ac, P_pv_to_batt_ac, P_grid_to_batt_ac, P_batt_to_load_ac, P_grid_to_load_ac, P_pv_to_load_ac, P_pv_to_grid_ac, P_batt_to_grid_ac, P_grid_ac, P_conversion_loss_ac;
	P_battery_ac = P_pv_ac = P_pv_to_batt_ac = P_grid_to_batt_ac = P_batt_to_load_ac = P_grid_to_load_ac = P_pv_to_load_ac = P_pv_to_grid_ac = P_batt_to_grid_ac = P_gen_ac = P_grid_ac = P_conversion_loss_ac = 0;
	
	// Quantitites are DC in KW unless otherwise specified
	double P_pv_to_batt_dc, P_grid_to_batt_dc, P_pv_to_inverter_dc;
	P_pv_to_batt_dc = P_grid_to_batt_dc = P_pv_to_inverter_dc = 0;

	// The battery power and PV power are initially DC, which must be converted to AC for powerflow
	double P_battery_dc_pre_bms = m_BatteryPower->powerBattery;
	double P_battery_dc = m_BatteryPower->powerBattery;
	double P_pv_dc = m_BatteryPower->powerPV;

	// convert the calculated DC power to DC at the PV system voltage
	if (P_battery_dc_pre_bms < 0)
		P_battery_dc = P_battery_dc_pre_bms / m_BatteryPower->singlePointEfficiencyDCToDC;
	else if (P_battery_dc > 0)
		P_battery_dc = P_battery_dc_pre_bms * m_BatteryPower->singlePointEfficiencyDCToDC;

	double P_gen_dc = P_pv_dc + P_battery_dc;

	// in the event that PV system isn't operating, assume battery BMS converts battery voltage to nominal inverter input at the weighted efficiency
	double voltage = m_BatteryPower->voltageSystem;
	double efficiencyDCAC = m_BatteryPower->sharedInverter->efficiencyAC * 0.01;
	if (voltage <= 0) {
		voltage = m_BatteryPower->sharedInverter->getInverterDCNominalVoltage();
	}
	if (std::isnan(efficiencyDCAC) || m_BatteryPower->sharedInverter->efficiencyAC <= 0) {
		efficiencyDCAC = m_BatteryPower->sharedInverter->getMaxPowerEfficiency() * 0.01;
	}


	// charging 
	if (P_battery_dc < 0)
	{
		// First check whether battery charging came from PV.  
		// Assumes that if battery is charging and can charge from PV, that it will charge from PV before the using the grid
		if (m_BatteryPower->canPVCharge) {
			P_pv_to_batt_dc = fabs(P_battery_dc);
			if (P_pv_to_batt_dc > P_pv_dc) {
				P_pv_to_batt_dc = P_pv_dc;
			}
		}
		P_pv_to_inverter_dc = P_pv_dc - P_pv_to_batt_dc;

		// Any remaining charge comes from grid regardless of whether allowed or not.
		P_grid_to_batt_dc = fabs(P_battery_dc) - P_pv_to_batt_dc;
		
		// Assume inverter only "sees" the net flow in one direction
		double P_gen_dc_inverter = P_pv_dc - P_pv_to_batt_dc - P_grid_to_batt_dc;

		// convert the DC power to AC
		m_BatteryPower->sharedInverter->calculateACPower(P_gen_dc_inverter * util::kilowatt_to_watt, voltage, 0.0);
		efficiencyDCAC = m_BatteryPower->sharedInverter->efficiencyAC * 0.01;


		// For now, treat the AC/DC conversion as a single point efficiency until gain clarification on real behavior.
		if (efficiencyDCAC <= 0.05 && P_grid_to_batt_dc > 0) {
 			efficiencyDCAC = m_BatteryPower->sharedInverter->getMaxPowerEfficiency() * 0.01;
			m_BatteryPower->sharedInverter->powerAC_kW = P_gen_dc_inverter * efficiencyDCAC;
		}
		else if (efficiencyDCAC <= 0.05 && P_pv_to_inverter_dc > 0) {
			efficiencyDCAC = 0.05;
			m_BatteryPower->sharedInverter->powerAC_kW = P_gen_dc_inverter * efficiencyDCAC;
		}

		// Compute the AC quantities
		P_gen_ac = m_BatteryPower->sharedInverter->powerAC_kW;
		P_grid_to_batt_ac = P_grid_to_batt_dc / efficiencyDCAC;
		P_pv_ac = P_pv_to_inverter_dc * efficiencyDCAC;
		P_pv_to_load_ac = P_load_ac;
		if (P_pv_to_load_ac > P_pv_ac) {
			P_pv_to_load_ac = P_pv_ac;
		}
		P_grid_to_load_ac = P_load_ac - P_pv_to_load_ac;
		P_pv_to_grid_ac = P_pv_ac - P_pv_to_load_ac;

		// In this case, we have a combo of Battery DC power from the PV array, and potentially AC power from the grid
		if (P_pv_to_batt_dc + P_grid_to_batt_ac > 0) {
			P_battery_ac = -(P_pv_to_batt_dc + P_grid_to_batt_ac);
		}
		
		// Assign this as AC values, even though they are fully DC
		P_pv_to_batt_ac = P_pv_to_batt_dc;
	}
	else
	{
		// convert the DC power to AC
		m_BatteryPower->sharedInverter->calculateACPower(P_gen_dc * util::kilowatt_to_watt, voltage, 0.0);
		efficiencyDCAC = m_BatteryPower->sharedInverter->efficiencyAC * 0.01;
		P_gen_ac = m_BatteryPower->sharedInverter->powerAC_kW;

		P_battery_ac = P_battery_dc * efficiencyDCAC;
		P_pv_ac = P_pv_dc * efficiencyDCAC;

		// Test if battery is discharging erroneously
		if (!m_BatteryPower->canDischarge && P_battery_ac > 0) {
			P_batt_to_grid_ac = P_batt_to_load_ac = 0;
			P_battery_ac = 0;
		}

		P_pv_to_load_ac = P_pv_ac;
		if (P_pv_ac >= P_load_ac)
		{
			P_pv_to_load_ac = P_load_ac;
			P_batt_to_load_ac = 0;

			// discharging to grid
			P_pv_to_grid_ac = P_pv_ac - P_pv_to_load_ac;
		}
		else {
			P_batt_to_load_ac = std::fmin(P_battery_ac, P_load_ac - P_pv_to_load_ac);
		}
		P_batt_to_grid_ac = P_battery_ac - P_batt_to_load_ac; 
	}

	// compute losses
	P_conversion_loss_ac = P_gen_dc - P_gen_ac + P_battery_dc_pre_bms - P_battery_dc;
	
	// Compute total system output and grid power flow, inverter draw is built into P_pv_ac
	P_grid_to_load_ac = P_load_ac - P_pv_to_load_ac - P_batt_to_load_ac;
	P_gen_ac -= P_system_loss_ac;

	// Grid charging loss accounted for in P_battery_ac 
	P_grid_ac = P_gen_ac - P_load_ac;

	// Error checking for power to load
	if (P_pv_to_load_ac + P_grid_to_load_ac + P_batt_to_load_ac != P_load_ac)
		P_grid_to_load_ac = P_load_ac - P_pv_to_load_ac - P_batt_to_load_ac;

	// check tolerances
	if (fabs(P_grid_to_load_ac) < m_BatteryPower->tolerance)
		P_grid_to_load_ac = 0;
	if (fabs(P_grid_to_batt_ac) <  m_BatteryPower->tolerance)
		P_grid_to_batt_ac = 0;
	if (fabs(P_grid_ac) <  m_BatteryPower->tolerance)
		P_grid_ac = 0;

	// assign outputs
	m_BatteryPower->powerBattery = P_battery_ac;
	m_BatteryPower->powerGrid = P_grid_ac;
	m_BatteryPower->powerGeneratedBySystem = P_gen_ac;
	m_BatteryPower->powerPVToLoad = P_pv_to_load_ac;
	m_BatteryPower->powerPVToBattery = P_pv_to_batt_ac;
	m_BatteryPower->powerPVToGrid = P_pv_to_grid_ac;
	m_BatteryPower->powerGridToBattery = P_grid_to_batt_ac;
	m_BatteryPower->powerGridToLoad = P_grid_to_load_ac;
	m_BatteryPower->powerBatteryToLoad = P_batt_to_load_ac;
	m_BatteryPower->powerBatteryToGrid = P_batt_to_grid_ac;
	m_BatteryPower->powerConversionLoss = P_conversion_loss_ac;
}

