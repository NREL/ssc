#ifndef __LIB_IRRADPROC_TEST_H_
#define __LIB_IRRADPROC_TEST_H_

#include <gtest/gtest.h>

#include "lib_irradproc.h"
#include "core.h"

/**
* \class IrradTest
*
* Month: 1-12, Hour: 0-23, Minute: 0-59.
*
*/

class IrradTest : public ::testing::Test {
protected:
	double lat, lon, tz, alb, tilt, azim, rotlim, gcr;
	int year, month, day, skymodel, tracking;
	bool backtrack_on;
	double calc_sunrise, calc_sunset;
	double e;

	void SetUp() {
		// parameters
		lat = 31.6340;
		lon = 74.8723;
		tz = 5.5;
		year = 2017;
		month = 7;
		day = 19;
		skymodel = 2;
		alb = 0.2;
		tracking = 0;
		tilt = 10;
		azim = 180;
		rotlim = 0;
		backtrack_on = false;
		gcr = 0;
		e = 0.0001;

		// correct sunrise and sunset times
		calc_sunrise = 5.70924; // 5:43 am
		calc_sunset = 19.5179;  // 7:31 pm
	}
};

class NightCaseIrradProc : public IrradTest {
protected:
	// Test time: 1:30 am
	irrad irr_hourly_night;
	// Test time: 1:15 am
	irrad irr_15m_night;

	void SetUp() {
		IrradTest::SetUp();
		int night_hr(1);
		irr_hourly_night.set_time(year, month, day, night_hr, 30, 1);
		irr_hourly_night.set_location(lat, lon, tz);
		irr_hourly_night.set_sky_model(skymodel, alb);
		irr_hourly_night.set_beam_diffuse(0, 0);
		irr_hourly_night.set_surface(tracking, tilt, azim, rotlim, backtrack_on, gcr);
		irr_15m_night.set_time(year, month, day, night_hr, 15, -1);
		irr_15m_night.set_location(lat, lon, tz);
		irr_15m_night.set_sky_model(skymodel, alb);
		irr_15m_night.set_beam_diffuse(0, 0);
		irr_15m_night.set_surface(tracking, tilt, azim, rotlim, backtrack_on, gcr);
	}
};

class SunriseCaseIrradProc : public IrradTest {
protected:
	// Test time: 5:30 am
	irrad irr_hourly_sunrise;
	// Test time: 5:30 am
	irrad irr_15m_sunrise;

	void SetUp() {
		IrradTest::SetUp();
		int sr_hr(5);
		irr_hourly_sunrise.set_time(year, month, day, sr_hr, 30, 1);
		irr_hourly_sunrise.set_location(lat, lon, tz);
		irr_hourly_sunrise.set_sky_model(skymodel, alb);
		irr_hourly_sunrise.set_beam_diffuse(0, 1);
		irr_hourly_sunrise.set_surface(tracking, tilt, azim, rotlim, backtrack_on, gcr);
		irr_15m_sunrise.set_time(year, month, day, sr_hr, 30, 1);
		irr_15m_sunrise.set_location(lat, lon, tz);
		irr_15m_sunrise.set_sky_model(skymodel, alb);
		irr_15m_sunrise.set_beam_diffuse(0, 1);
		irr_15m_sunrise.set_surface(tracking, tilt, azim, rotlim, backtrack_on, gcr);
	}
};

class DayCaseIrradProc : public IrradTest {
protected:
	// Test time: 12:30 pm
	irrad irr_hourly_day;
	// Test time: 12:45 pm
	irrad irr_15m_day;

	void SetUp() {
		IrradTest::SetUp();
		int day_hr(12);
		irr_hourly_day.set_time(year, month, day, day_hr, 30, 1);
		irr_hourly_day.set_location(lat, lon, tz);
		irr_hourly_day.set_sky_model(skymodel, alb);
		irr_hourly_day.set_beam_diffuse(2, 2);
		irr_hourly_day.set_surface(tracking, tilt, azim, rotlim, backtrack_on, gcr);
		irr_15m_day.set_time(year, month, day, day_hr, 45, 1);
		irr_15m_day.set_location(lat, lon, tz);
		irr_15m_day.set_sky_model(skymodel, alb);
		irr_15m_day.set_beam_diffuse(2, 2);
		irr_15m_day.set_surface(tracking, tilt, azim, rotlim, backtrack_on, gcr);
	}
};

class SunsetCaseIrradProc : public IrradTest {
protected:
	// Test time: 7:30 pm
	irrad irr_hourly_sunset;
	// Test time: 7:30 pm
	irrad irr_15m_sunset;

	virtual void SetUp() {
		IrradTest::SetUp();
		int ss_hr(19);
		irr_hourly_sunset.set_time(year, month, day, ss_hr, 30, 1);
		irr_hourly_sunset.set_location(lat, lon, tz);
		irr_hourly_sunset.set_sky_model(skymodel, alb);
		irr_hourly_sunset.set_beam_diffuse(0, 1);
		irr_hourly_sunset.set_surface(tracking, tilt, azim, rotlim, backtrack_on, gcr);
		irr_15m_sunset.set_time(year, month, day, ss_hr, 30, 1);
		irr_15m_sunset.set_location(lat, lon, tz);
		irr_15m_sunset.set_sky_model(skymodel, alb);
		irr_15m_sunset.set_beam_diffuse(0, 1);
		irr_15m_sunset.set_surface(tracking, tilt, azim, rotlim, backtrack_on, gcr);
	}
};
/**
*    Test which uses the example in bifacialvf.py within github.com/NREL/bifacialvf
*/
class BifacialIrradTest : public ::testing::Test {
protected:
	double tilt, azim, transmissionFactor, bifaciality, gcr, rotlim;
	double slopeLength, rowToRow, clearanceGround, distanceBetweenRows, verticalHeight, horizontalLength;
	int tracking;
	bool backtrack;
	irrad * irr;
	double beam, diffuse;
	double e;

	void SetUp() {
		
		// parameters
		tilt = 10;
		azim = 180;
		gcr = 0.666667;
		e = 0.0001;
		tracking = 0;
		rotlim = 90;
		backtrack = false;

		// bifacial
		transmissionFactor = 0.013;
		bifaciality = 0.65;

		slopeLength = 1.;										/// The unit slope length of the panel
		rowToRow = slopeLength / this->gcr;						/// Row to row spacing between the front of one row to the front of the next row
		double tiltRadian = this->tilt * M_PI / 180.;
		clearanceGround = slopeLength;							/// The normalized clearance from the bottom edge of module to ground
		distanceBetweenRows = rowToRow - std::cos(tiltRadian);	/// The normalized distance from the read of module to front of module in next row
		verticalHeight = std::sin(tiltRadian);
		horizontalLength = std::cos(tiltRadian);

		// for January 1, 11:00 AM (add for a full 8760)
		beam = 476; 
		diffuse = 149;
		irr = new irrad();
		irr->set_surface(tracking, tilt, azim, rotlim, backtrack, gcr);
		irr->set_beam_diffuse(beam, diffuse);
	}
	void TearDown() {
		if (irr) {
			delete irr;
		}
	}
};



#endif // !__LIB_IRRADPROC_TEST_H_

