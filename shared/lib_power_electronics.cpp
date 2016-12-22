#include "lib_power_electronics.h"
#include "lib_sandia.h"
#include <cmath>

double bidirectional_inverter::convert_to_dc(double P_ac, double * P_dc)
{
	double P_loss = P_ac * (1 - _ac_dc_efficiency);
	*P_dc = P_ac * _ac_dc_efficiency;
	return P_loss;
}
double bidirectional_inverter::convert_to_ac(double P_dc, double * P_ac)
{
	double P_loss = P_dc * (1 - _dc_ac_efficiency);
	*P_ac = P_dc * _dc_ac_efficiency;
	return P_loss;
}
double bidirectional_inverter::compute_dc_from_ac(double P_ac)
{
	return P_ac / _dc_ac_efficiency;
}

double rectifier::convert_to_dc(double P_ac, double * P_dc)
{
	double P_loss = P_ac * (1 - _ac_dc_efficiency);
	*P_dc = P_ac * _ac_dc_efficiency;
	return P_loss;
}

charge_controller::charge_controller(dispatch_t * dispatch, battery_metrics_t * battery_metrics, double efficiency_1, double efficiency_2)
{	
	_dispatch = dispatch;
	_battery_metrics = battery_metrics;
	initialize(0,0);
}
void charge_controller::initialize(double P_pv, double P_load_ac)
{
	_P_load = P_load_ac;
	_P_grid = 0;
	_P_grid_to_load = 0;
	_P_grid_to_batt = 0;
	_P_gen = 0;
	_P_battery_to_load = 0;
	_P_pv_to_load = 0;
	_P_pv_to_battery = 0;
	_P_pv_to_grid = 0;
	_P_battery_to_grid = 0;
	_P_battery = 0;

	// ac for ac-connected, dc for dc-connected
	_P_pv = P_pv;
}

double charge_controller::grid_ac(double dc_ac_efficiency)
{
	// dc powers
	double P_grid_to_batt_0 = _dispatch->power_grid_to_batt();
	double P_grid_to_load_0 = _dispatch->power_grid_to_load();

	// ac grid power required to meet dc charging
	if (_P_battery < 0)
		_P_grid_to_batt = fabs(_P_battery + _P_pv_to_battery);
	double P_grid_loss = (_P_grid_to_batt - P_grid_to_batt_0);

	// reconcile any conversion with required load 
	// P_grid < 0 -> using grid; _P_grid > 0 -> exporting to grid
	_P_grid_to_load = _P_load - _P_pv_to_load - _P_battery_to_load;
	_P_grid = -_P_grid_to_load - _P_grid_to_batt + _P_pv_to_grid;
	
	if (_P_grid_to_load < 0)
		_P_grid_to_load = 0.;
	if (_P_grid_to_batt < 0)
		_P_grid_to_batt = 0.;
		
	return P_grid_loss; 
}

void charge_controller::compute_to_batt_load_grid(double P_battery_ac, double P_pv_ac, double P_load_ac)
{

	double P_pv_to_batt_ac = 0;
	double P_pv_to_load_ac = 0;
	double P_pv_to_grid_ac = 0;
	double P_grid_to_batt_ac = 0;
	double P_batt_to_load_ac = 0;
	double P_grid_to_load_ac = 0;
	double P_gen_ac = 0;
	double P_grid_ac = 0;

	// compute to battery, to load, to grid
	if (P_battery_ac <= 0)
	{
		// allowed to charge with PV, grid?  Check in some way

		// pv priority to charging battery
		if (_dispatch->pv_dispatch_priority() == dispatch_t::CHARGE_BATTERY)
		{
			P_pv_to_batt_ac = fabs(P_battery_ac);

			if (P_pv_to_batt_ac > P_pv_ac)
			{
				P_pv_to_batt_ac = P_pv_ac;
				P_grid_to_batt_ac = fabs(P_battery_ac) - P_pv_ac;
			}

			P_pv_to_load_ac = P_pv_ac - P_pv_to_batt_ac;
			if (P_pv_to_load_ac > P_load_ac)
			{
				P_pv_to_load_ac = P_load_ac;
				P_pv_to_grid_ac = P_pv_ac - P_pv_to_batt_ac - P_pv_to_load_ac;
			}
		}
		// pv priority to meeting load
		else
		{
			P_pv_to_load_ac = P_load_ac;

			if (P_pv_to_load_ac > P_pv_ac)
				P_pv_to_load_ac = P_pv_ac;

			P_pv_to_batt_ac = P_pv_ac - P_pv_to_load_ac;

			if (P_pv_to_batt_ac > fabs(P_battery_ac))
			{
				P_pv_to_batt_ac = fabs(P_battery_ac);
				P_pv_to_grid_ac = P_pv_ac - P_pv_to_load_ac - P_pv_to_batt_ac;
			}
			else
				P_grid_to_batt_ac = fabs(P_battery_ac) - P_pv_to_batt_ac;
		}
	}
	else
	{
		P_pv_to_load_ac = P_pv_ac;
		if (P_pv_ac > P_load_ac)
		{
			P_pv_to_load_ac = P_load_ac;
			P_batt_to_load_ac = 0;
		}
		else if (P_battery_ac < P_load_ac - P_pv_to_load_ac)
			P_batt_to_load_ac = P_battery_ac;
		else
			// probably shouldn't happen, need to iterate and reduce I from battery
			P_batt_to_load_ac = P_load_ac - P_pv_to_load_ac;
	}

	P_grid_to_load_ac = P_load_ac - P_pv_to_load_ac - P_batt_to_load_ac;
	P_gen_ac = P_pv_ac + P_battery_ac;
	P_grid_ac = _P_load - P_gen_ac;

	// assign outputs
	_P_battery = P_battery_ac;
	_P_pv = P_pv_ac;
	_P_gen = P_gen_ac;
	_P_grid = P_grid_ac;
	_P_battery_to_load = P_batt_to_load_ac;
	_P_pv_to_battery = P_pv_to_batt_ac;
	_P_pv_to_load = P_pv_to_load_ac;
	_P_pv_to_grid = P_pv_to_grid_ac;
	_P_grid_to_load = P_grid_to_load_ac;
	_P_grid_to_batt = P_grid_to_batt_ac;
}

dc_connected_battery_controller::dc_connected_battery_controller(dispatch_t * dispatch, 
																 battery_metrics_t * battery_metrics, 
																 double batt_dc_dc_bms_efficiency, 
																 double pv_dc_dc_mppt_efficiency,
																 double inverter_efficiency) :
charge_controller(dispatch, battery_metrics, 100, batt_dc_dc_bms_efficiency)
{
	_dc_dc_charge_controller = new dc_dc_charge_controller(batt_dc_dc_bms_efficiency, pv_dc_dc_mppt_efficiency);
	_inverter_efficiency = inverter_efficiency * 0.01;
}
dc_connected_battery_controller::~dc_connected_battery_controller()
{
	if (_dc_dc_charge_controller)
		delete _dc_dc_charge_controller;
}

double dc_connected_battery_controller::run(size_t year, size_t hour_of_year, size_t step_of_hour, double P_pv_dc, double P_load_ac)
{
	
	initialize(P_pv_dc, P_load_ac);

	// derate PV power being passed to battery, is squared since passes through DC/DC w/MPPT and DC/DC w/BMS
	double P_pv_dc_converted = P_pv_dc * _dc_dc_charge_controller->batt_dc_dc_bms_efficiency() * _dc_dc_charge_controller->pv_dc_dc_mppt_efficiency();
	
	// compute what dc load would have to be to match load, i.e
	double P_load_dc =  (1./_inverter_efficiency) * P_load_ac;

	// compute what the dc load before the dc/dc/bms converter would have to be
	P_load_dc *= (1. / _dc_dc_charge_controller->batt_dc_dc_bms_efficiency());

	// dispatch battery, load must be in dc.  This computes dc battery power to load, dc pv power to load, dc grid power to dc load, dc grid power to battery
	_dispatch->dispatch(year, hour_of_year, step_of_hour, P_pv_dc_converted, P_load_dc);

	// loss is due to conversion in dc-dc bms
	_P_loss = process_dispatch();

	return _P_loss;
}
double dc_connected_battery_controller::process_dispatch()
{
	// post DC/DC w/MPPT
	double P_pv_dc = _P_pv * _dc_dc_charge_controller->pv_dc_dc_mppt_efficiency();
	double P_load_ac = _P_load;

	double P_battery_dc = _dispatch->power_tofrom_battery();
	double P_battery_dc_post_bms = 0;
	double P_battery_ac = 0;

	// post DC/DC w/BMS
	if (P_battery_dc > 0)
		P_battery_dc_post_bms = P_battery_dc * _dc_dc_charge_controller->batt_dc_dc_bms_efficiency();
	else if (P_battery_dc < 0)
		P_battery_dc_post_bms = P_battery_dc / _dc_dc_charge_controller->batt_dc_dc_bms_efficiency();
	
	// compute generation
	double P_gen_dc = P_pv_dc + P_battery_dc_post_bms;

	// dc output quantities	
	_P_battery = P_battery_dc_post_bms;
	_P_gen = P_gen_dc;

	// dc-dc bms conversion loss
	double P_loss = fabs(P_battery_dc - P_battery_dc_post_bms);

	return P_loss;
}

double dc_connected_battery_controller::update_gen_ac(double P_gen_ac)
{
	double P_battery_dc = _P_battery;
	double P_gen_dc = _P_gen;

	// get pv power post DC/DC with MPPT
	double P_pv_dc = _P_pv * _dc_dc_charge_controller->pv_dc_dc_mppt_efficiency();
	
	// if there is no "gen", then either no dispatch or all PV went to battery
	double inverter_efficiency = 1.00;
	if (fabs(P_gen_dc) > tolerance)
		inverter_efficiency = P_gen_ac / P_gen_dc;

	// An edge case with large implications on efficiency, investigate further 
	if (inverter_efficiency < 0)
		inverter_efficiency = 1.00;

	double P_battery_ac = 0;	
	if (P_battery_dc > 0)
		P_battery_ac = P_battery_dc * inverter_efficiency;
	else if (P_battery_dc < 0)
		P_battery_ac = P_battery_dc / inverter_efficiency;
	
	double P_pv_ac = P_pv_dc * inverter_efficiency; 
	double P_load_ac = _P_load;

	compute_to_batt_load_grid(P_battery_ac, P_pv_ac, P_load_ac);
	_P_gen = P_gen_ac;

	// add battery power inversion loss to total loss
	_P_loss += fabs(P_battery_ac - P_battery_dc);

	// AC charging metrics
	_battery_metrics->compute_metrics_ac(_P_battery, _P_pv_to_battery, _P_grid_to_batt, _P_grid);

	return _P_loss;
}

ac_connected_battery_controller::ac_connected_battery_controller(dispatch_t * dispatch, battery_metrics_t * battery_metrics, double ac_dc_efficiency, double dc_ac_efficiency) :
charge_controller(dispatch, battery_metrics, ac_dc_efficiency, dc_ac_efficiency)
{
	_bidirectional_inverter = new bidirectional_inverter(ac_dc_efficiency, dc_ac_efficiency);
}
ac_connected_battery_controller::~ac_connected_battery_controller()
{
	if (_bidirectional_inverter)
		delete _bidirectional_inverter;
}
double ac_connected_battery_controller::run( size_t year, size_t hour_of_year, size_t step_of_hour, double P_pv_ac, double P_load_ac)
{
	
	initialize(P_pv_ac, P_load_ac);

	// compute the effective PV power that the battery sees after losses
	double P_max_battery = 0.;
	double P_pv_effective = P_pv_ac;
	double P_diff_ac = P_load_ac - P_pv_ac;

	if (P_diff_ac < 0)
	{
		P_max_battery = P_diff_ac * _bidirectional_inverter->dc_ac_efficiency();
		P_pv_effective = P_pv_ac - fabs(P_max_battery - P_diff_ac);
	}
	else if (P_diff_ac > 0)
		P_max_battery = P_diff_ac * _bidirectional_inverter->ac_dc_efficiency();
		
	if (P_pv_effective < 0)
		P_pv_effective = 0;

	// dispatch battery
	_dispatch->dispatch(year, hour_of_year, step_of_hour, P_pv_effective, P_load_ac);

	// Take resulting battery power and compute components
	double P_loss = process_dispatch();

	// AC charging metrics
	_battery_metrics->compute_metrics_ac(_P_battery, _P_pv_to_battery, _P_grid_to_batt, _P_grid);

	return P_loss;
}
double ac_connected_battery_controller::grid_ac()
{
	double P_loss_grid = charge_controller::grid_ac(_bidirectional_inverter->ac_dc_efficiency());
	return P_loss_grid;
}

double ac_connected_battery_controller::process_dispatch()
{
	double P_pv_ac = _P_pv;
	double P_load_ac = _P_load;
	double P_battery_dc = _dispatch->power_tofrom_battery();
	double P_battery_ac = 0;


	if (P_battery_dc > 0)
		P_battery_ac = P_battery_dc * _bidirectional_inverter->dc_ac_efficiency();
	else if (P_battery_dc < 0)
		P_battery_ac = P_battery_dc / _bidirectional_inverter->ac_dc_efficiency();

	compute_to_batt_load_grid(P_battery_dc, P_battery_ac, P_pv_ac, P_load_ac);

	double P_battery_loss = P_battery_dc - P_battery_ac;
	return P_battery_loss;
}

void ac_connected_battery_controller::compute_to_batt_load_grid(double P_battery_dc, double P_battery_ac, double P_pv_ac, double P_load_ac)
{
	double P_pv_to_batt_ac = 0;
	double P_pv_to_load_ac = 0;
	double P_pv_to_grid_ac = 0;
	double P_grid_to_batt_ac = 0;
	double P_batt_to_load_ac = 0;
	double P_grid_to_load_ac = 0;
	double P_gen_ac = 0;
	double P_grid_ac = 0;

	// compute to battery, to load, to grid
	if (P_battery_ac <= 0)
	{
		// allowed to charge with PV, grid?  Check in some way

		// pv priority to charging battery
		if (_dispatch->pv_dispatch_priority() == dispatch_t::CHARGE_BATTERY)
		{
			P_pv_to_batt_ac = fabs(P_battery_ac);

			if (P_pv_to_batt_ac > P_pv_ac * _bidirectional_inverter->ac_dc_efficiency())
			{
				P_pv_to_batt_ac = P_pv_ac * _bidirectional_inverter->ac_dc_efficiency();
				P_grid_to_batt_ac = fabs(P_battery_ac) - P_pv_ac * _bidirectional_inverter->ac_dc_efficiency();
			}

			P_pv_to_load_ac = P_pv_ac - P_pv_to_batt_ac;
			if (P_pv_to_load_ac > P_load_ac)
			{
				P_pv_to_load_ac = P_load_ac;
				P_pv_to_grid_ac = P_pv_ac - P_pv_to_batt_ac - P_pv_to_load_ac;
			}
		}
		// pv priority to meeting load
		else
		{
			P_pv_to_load_ac = P_load_ac;

			if (P_pv_to_load_ac > P_pv_ac)
				P_pv_to_load_ac = P_pv_ac;

			P_pv_to_batt_ac = (P_pv_ac - P_pv_to_load_ac) * _bidirectional_inverter->ac_dc_efficiency();

			if (P_pv_to_batt_ac > fabs(P_battery_ac))
			{
				P_pv_to_batt_ac = fabs(P_battery_ac);
				P_pv_to_grid_ac = P_pv_ac - P_pv_to_load_ac - P_pv_to_batt_ac;
			}
			// use DC component of battery, since P_pv_to_batt_ac incorporates loss
			else
				P_grid_to_batt_ac = fabs(P_battery_dc) - P_pv_to_batt_ac;
		}
	}
	else
	{
		P_pv_to_load_ac = P_pv_ac;
		if (P_pv_ac > P_load_ac)
		{
			P_pv_to_load_ac = P_load_ac;
			P_batt_to_load_ac = 0;
		}
		else if (P_battery_ac < P_load_ac - P_pv_to_load_ac)
			P_batt_to_load_ac = P_battery_ac;
		else
			// probably shouldn't happen, need to iterate and reduce I from battery
			P_batt_to_load_ac = P_load_ac - P_pv_to_load_ac;
	}

	P_grid_to_load_ac = P_load_ac - P_pv_to_load_ac - P_batt_to_load_ac;
	P_gen_ac = P_pv_ac + P_battery_ac;
	P_grid_ac = _P_load - P_gen_ac;

	// check tolerances
	if (fabs(P_grid_to_load_ac) < tolerance)
		P_grid_to_load_ac = 0;
	if (fabs(P_grid_to_batt_ac) < tolerance)
		P_grid_to_batt_ac = 0;
	if (fabs(P_grid_ac) < tolerance)
		P_grid_ac = 0;

	// assign outputs
	_P_battery = P_battery_ac;
	_P_pv = P_pv_ac;
	_P_gen = P_gen_ac;
	_P_grid = P_grid_ac;
	_P_battery_to_load = P_batt_to_load_ac;
	_P_pv_to_battery = P_pv_to_batt_ac;
	_P_pv_to_load = P_pv_to_load_ac;
	_P_pv_to_grid = P_pv_to_grid_ac;
	_P_grid_to_load = P_grid_to_load_ac;
	_P_grid_to_batt = P_grid_to_batt_ac;
}

double ac_connected_battery_controller::gen_ac()
{
	// dc quantities
	double P_gen_dc = _dispatch->power_gen();
	double P_battery_dc = _dispatch->power_tofrom_battery();
	double P_battery_to_load_dc = _dispatch->power_battery_to_load();
	double P_pv_to_load_dc = _dispatch->power_pv_to_load();
	double P_pv_to_battery_dc = _dispatch->power_pv_to_batt();
	double P_pv_to_grid_dc = _dispatch->power_pv_to_grid();
	double P_battery_to_grid_dc = _dispatch->power_battery_to_grid();

	// ac quantities
	_P_gen = P_gen_dc * _bidirectional_inverter->dc_ac_efficiency();
	_P_battery_to_load = P_battery_to_load_dc * _bidirectional_inverter->dc_ac_efficiency();
	_P_pv_to_load = P_pv_to_load_dc * _bidirectional_inverter->dc_ac_efficiency();
	_P_pv_to_grid = P_pv_to_grid_dc * _bidirectional_inverter->dc_ac_efficiency();

	if (P_battery_dc > 0)
	{
		_P_battery = P_battery_dc * _bidirectional_inverter->dc_ac_efficiency();
		_P_pv_to_battery = P_pv_to_battery_dc * _bidirectional_inverter->dc_ac_efficiency();
		_P_battery_to_grid = P_battery_to_grid_dc * _bidirectional_inverter->dc_ac_efficiency();
	}
	else if (P_battery_dc < 0)
	{
		_P_battery = P_battery_dc / _bidirectional_inverter->ac_dc_efficiency();
		_P_pv_to_battery = P_pv_to_battery_dc / _bidirectional_inverter->ac_dc_efficiency();

		if (_P_pv_to_battery > _P_pv)
		{
			double dPv = _P_pv_to_battery - _P_pv;
			_P_pv_to_battery -= dPv;
			_P_battery -= dPv;
		}
		else if (_P_pv_to_battery + _P_pv_to_load > _P_pv)
		{
			double dPv = _P_pv_to_battery + _P_pv_to_load - _P_pv;
			_P_pv_to_battery -= dPv;
			_P_battery -= dPv;
		}
	}
	double P_loss_gen = P_gen_dc - _P_gen;

	// extra metrics if desired
	double P_loss_battery_to_load = P_battery_to_load_dc - _P_battery_to_load;
	double P_loss_pv_to_load = P_pv_to_load_dc - _P_pv_to_load;
	double P_loss_pv_to_battery = P_pv_to_battery_dc - _P_pv_to_battery;
	double P_loss_battery = P_battery_dc - _P_battery;


	return P_loss_gen;
}
