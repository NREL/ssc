#ifndef __LIB_IRRADPROC_TEST_H_
#define __LIB_IRRADPROC_TEST_H_

#include <gtest/gtest.h>

#include "lib_irradproc.h"
#include "core.h"

#include "input_cases/bifacialvf_data/expectedWeather.h"
/*
#include "input_cases/bifacialvf_data/expectedFrontGroundGHI.h"
#include "input_cases/bifacialvf_data/expectedRearGroundGHI.h"
#include "input_cases/bifacialvf_data/expectedFrontIrradiance.h"
#include "input_cases/bifacialvf_data/expectedFrontReflected.h"
#include "input_cases/bifacialvf_data/expectedRearIrradiance.h"
#include "input_cases/bifacialvf_data/expectedScalarValues.h"
*/

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
	double tilt, azim, transmissionFactor, bifaciality, gcr, rotlim, albedo;
	double slopeLength, rowToRow, clearanceGround, distanceBetweenRows, verticalHeight, horizontalLength;
	int year, month, day, hour, minute;
	double lat, lon, tz;
	int tracking, skyModel;
	bool backtrack;
	irrad * irr;
	double beam, diffuse;
	double e;

	void SetUp() {
		
		// parameters
		tilt = 10;
		azim = 180;
		gcr = 0.666667;
		e = 0.001;
		tracking = 0;
		rotlim = 90;
		backtrack = false;
		albedo = 0.62;
		skyModel = 2; // perez

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

		lat = 37.517;
		lon = -77.317;
		tz = -5.0;

		irr = new irrad();
		runIrradCalc(0);
	}
	void TearDown() {
		if (irr) {
			delete irr;
		}
	}

	void runIrradCalc(size_t index)
	{
		year = expectedWeather[index][0];
		month = expectedWeather[index][1];
		day = expectedWeather[index][2];
		hour = expectedWeather[index][3];
		minute = expectedWeather[index][4];
		beam = expectedWeather[index][5];
		diffuse = expectedWeather[index][6];

		irr->set_surface(tracking, tilt, azim, rotlim, backtrack, gcr);
		irr->set_beam_diffuse(beam, diffuse);
		irr->set_time(year, month, day, hour, minute, 1);
		irr->set_location(lat, lon, tz);
		irr->set_sky_model(skyModel, albedo);
		irr->calc();
	}
};

// Calculated factors for specified case using Chris Deline bifacialvf: github.com/NREL/bifacialvf, non time varying
static std::vector<double> expectedSkyConfigFactors = { 0.31746528768349, 0.3166898597414707, 0.3158845617514987, 0.31505447092609884, 0.31420468029924176, 0.313340276953247, 0.3124663205405842, 0.3115878222383039, 0.31070972426429816, 0.3098368800746455, 0.3089740353501973, 0.3081258098684984, 0.3072966803444087, 0.30649096430957745, 0.30571280508756976, 0.30496615790809833, 0.30425477719079363, 0.303582205016401, 0.3029517607914656, 0.30236653210158737, 0.30182936673836325, 0.30134286587627956, 0.30090937836815723, 0.30053099612133727, 0.30020955051164644, 0.299946609788294, 0.299743477420189, 0.29960119133268404, 0.29952052398337214, 0.29950198322619537, 0.2995458139146673, 0.2996520001973526, 0.2998202684617832, 0.3000500908865539, 0.30034068956536697, 0.300691041171112, 0.30109988213257166, 0.3015657143009256, 0.3020868110877593, 0.30266122406068663, 0.3032867899868308, 0.30396113831827176, 0.30468169911696935, 0.305445711419659, 0.3062502320456547, 0.30709214485236613, 0.30796817044461977, 0.3088748763445053, 0.30980868762847524, 0.3107658980377686, 0.31174268156691937, 0.3127351045331717, 0.31373913812706444, 0.31475067144128155, 0.315765524971164, 0.31677946457604256, 0.31778821588586126, 0.3187874791324413, 0.3197729443792757, 0.3207403071179795, 0.3216852841935352, 0.3226036300143336, 0.3234911529967789, 0.32434373218801665, 0.3251573340041737, 0.32592802901550877, 0.32665200870411437, 0.3273256021143607, 0.3279452923112385, 0.32850773255720117, 0.3290097621141306, 0.32944842157370274, 0.32982096761685376, 0.3301248871012388, 0.33035791037468787, 0.3305180237127191, 0.33060348077924484, 0.33061281301177303, 0.3305448388356977, 0.3303986716167246, 0.3301737262661392, 0.3298697244204676, 0.32948669812513764, 0.3290249919609623, 0.3284852635626013, 0.32786848248955747, 0.3271759274226039, 0.3264091816717319, 0.32557012699560445, 0.32466093574692756, 0.3236840613729327, 0.32264222731510417, 0.3215384143671419, 0.3203758465647328, 0.3191579756947288, 0.3178884645246083, 0.3165711688653665, 0.3152101185920348, 0.3138094977556616, 0.3123736239286202 };
static std::vector<int> expectedGroundShade = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };


// Calculated factors which are time varying
static std::vector<double> expectedRearGroundGHI = { 2.85850546438486, 2.8515266129066861, 2.8442789309969383, 2.8368081135683396, 2.829159997926626, 2.8213803678126732, 2.8135147600987076, 2.805608275378185, 2.7977053936121332, 2.7898497959052593, 2.7820841933852258, 2.7744501640499357, 2.7669879983331285, 2.7597365540196468, 2.7527331210215777, 2.7460132964063351, 2.7396108699505928, 2.7335577203810586, 2.7278837223566401, 2.7226166641477363, 2.7177821758787193, 2.713403668119966, 2.7095022805468649, 2.7060968403254853, 2.7032038298382681, 2.700837363328096, 2.6990091720151508, 2.6977285972276062, 2.6970025910837991, 2.6968357242692083, 2.6972302004654556, 2.6981858770096232, 2.6997002913894987, 2.701768693212435, 2.7043840813217526, 2.7075372457734579, 2.7112168144265949, 2.7154093039417804, 2.7200991750232837, 2.7252688917796295, 2.7308989851149268, 2.7369681200978957, 2.7434531672861739, 2.7503292780103807, 2.7575699636443423, 2.765147178904745, 2.7730314092350281, 2.7811917623339975, 2.7895960638897273, 2.7982109575733674, 2.8070020093357244, 2.8159338160319956, 2.8249701183770299, 2.8340739182049837, 2.8432075999739261, 2.8523330564178329, 2.8614118182062014, 2.8704051874254217, 2.8792743746469314, 2.8879806392952658, 2.8964854329752669, 2.9047505453624525, 2.91273825220446, 2.9204114649255999, 2.927733881271013, 2.934670136373029, 2.9411859535704794, 2.9472482942626961, 2.9528255060345963, 2.9578874682482605, 2.9624057342606251, 2.9663536693967747, 2.969706583785134, 2.9724418591445989, 2.9745390686056408, 2.9759800886479217, 2.9767492022466535, 2.9768331923394071, 2.976221424754729, 2.9749059197839713, 2.9728814116287028, 2.9701453950176582, 2.9666981583596885, 2.9625428028821106, 2.9576852472968618, 2.9521342176394669, 2.945901222036885, 2.9390005102790369, 2.9314490181938901, 2.9232662969557981, 2.9144744275898442, 2.9050979210693875, 2.8951636045377271, 2.884700494316045, 2.8737396564860092, 2.8623140559549243, 2.8504583950217488, 2.8382089425617631, 2.8256033550344042, 2.8126804905910321 };
static std::vector<double> expectedFrontGroundGHI = { 2.85850546438486, 2.8515266129066861, 2.8442789309969383, 2.8368081135683396, 2.829159997926626, 2.8213803678126732, 2.8135147600987076, 2.805608275378185, 2.7977053936121332, 2.7898497959052593, 2.7820841933852258, 2.7744501640499357, 2.7669879983331285, 2.7597365540196468, 2.7527331210215777, 2.7460132964063351, 2.7396108699505928, 2.7335577203810586, 2.7278837223566401, 2.7226166641477363, 2.7177821758787193, 2.713403668119966, 2.7095022805468649, 2.7060968403254853, 2.7032038298382681, 2.700837363328096, 2.6990091720151508, 2.6977285972276062, 2.6970025910837991, 2.6968357242692083, 2.6972302004654556, 2.6981858770096232, 2.6997002913894987, 2.701768693212435, 2.7043840813217526, 2.7075372457734579, 2.7112168144265949, 2.7154093039417804, 2.7200991750232837, 2.7252688917796295, 2.7308989851149268, 2.7369681200978957, 2.7434531672861739, 2.7503292780103807, 2.7575699636443423, 2.765147178904745, 2.7730314092350281, 2.7811917623339975, 2.7895960638897273, 2.7982109575733674, 2.8070020093357244, 2.8159338160319956, 2.8249701183770299, 2.8340739182049837, 2.8432075999739261, 2.8523330564178329, 2.8614118182062014, 2.8704051874254217, 2.8792743746469314, 2.8879806392952658, 2.8964854329752669, 2.9047505453624525, 2.91273825220446, 2.9204114649255999, 2.927733881271013, 2.934670136373029, 2.9411859535704794, 2.9472482942626961, 2.9528255060345963, 2.9578874682482605, 2.9624057342606251, 2.9663536693967747, 2.969706583785134, 2.9724418591445989, 2.9745390686056408, 2.9759800886479217, 2.9767492022466535, 2.9768331923394071, 2.976221424754729, 2.9749059197839713, 2.9728814116287028, 2.9701453950176582, 2.9666981583596885, 2.9625428028821106, 2.9576852472968618, 2.9521342176394669, 2.945901222036885, 2.9390005102790369, 2.9314490181938901, 2.9232662969557981, 2.9144744275898442, 2.9050979210693875, 2.8951636045377271, 2.884700494316045, 2.8737396564860092, 2.8623140559549243, 2.8504583950217488, 2.8382089425617631, 2.8256033550344042, 2.8126804905910321 };
static std::vector<double> expectedFrontReflected = { 0.73140551543302934, 0.76183946100670097, 0.78638530769297077, 0.77693134536222652, 0.76862728509964684, 0.76350752215051088};
static std::vector<double> expectedFrontIrradiance = { 8.2139527096901688, 8.3391775580430103, 8.4481532809739548, 8.5524446926833768, 8.5375118385044679, 8.5305744653534585};
static std::vector<double> expectedRearIrradiance = { 1.6532738313180309, 1.647998581232474, 1.6452285067017434, 1.6395619141053668, 1.6284524933989373, 1.6154143854261722};

static double expectedPVFrontShadeFraction = 0.462809265051;
static double expectedPVRearShadeFraction = 1.0;

static double expectedFrontAverageIrradiance = 8.437192;
static double expectedRearAverageIrradiance = 1.63822;




#endif // !__LIB_IRRADPROC_TEST_H_

