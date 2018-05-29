#include <stdlib.h>

#include "lib_irradproc_test.h"

using std::vector;

/**
 * Solar Position Function Tests
 * Output: sun[] = azimuth (rad), zenith(rad), elevation(rad), declination(rad), sunrise time, sunset time, 
 * eccentricity correction factor, true solar time, extraterrestrial solar irradiance on horizontal (W/m2) 
 */

TEST_F(NightCaseIrradProc, solarposTest_lib_irradproc){
	
	double sun[9];
	vector<double> sunrise_times;
	vector<double> sunset_times;

	/* Just before sunrise test case */
	solarpos(year, month, day, 4, 30, lat, lon, tz, sun);
	vector<double> solution = { 0.95662, 1.79457, -0.223771, 0.363938, 5.70882, 19.5183, 0.968276, 3.88646, 0 };
	sunrise_times.push_back(solution[4]);
	sunset_times.push_back(solution[5]);
	for (int i = 0; i < 9; i++){
		EXPECT_NEAR((double)sun[i], solution[i], e) << "hourly before-sunrise case, parameter " << i << " fail\n";
	}
	solarpos(year, month, day, 5, 15, lat, lon, tz, sun);
	solution = { 1.0744, 1.65255, -0.0817513, 0.363839, 5.7091, 19.518, 0.96828, 4.63642, 0 };
	sunrise_times.push_back(solution[4]);
	sunset_times.push_back(solution[5]);
	for (int i = 0; i < 9; i++){
		EXPECT_NEAR((double)sun[i], solution[i], e) << "15m before-sunrise case, parameter " << i << " fail\n";
	}

	/* Just after sunset test case */
	solarpos(year, month, day, 20, 30, lat, lon, tz, sun);
	solution = { 5.28748, 1.75391, -0.183117, 0.361807, 5.71544, 19.5131, 0.968361, 19.8857, 0 };
	sunrise_times.push_back(solution[4]);
	sunset_times.push_back(solution[5]);
	for (int i = 0; i < 9; i++){
		EXPECT_NEAR((double)sun[i], solution[i], e) << "hourly after-sunset case, parameter " << i << " fail\n";
	}
	solarpos(year, month, day, 19, 45, lat, lon, tz, sun);
	solution = { 5.17431, 1.60864, -0.0378397, 0.361908, 5.71513, 19.5133, 0.968357, 19.1358, 0 };
	sunrise_times.push_back(solution[4]);
	sunset_times.push_back(solution[5]);
	for (int i = 0; i < 9; i++){
		EXPECT_NEAR((double)sun[i], solution[i], e) << "15m after-sunrise case, parameter " << i << " fail\n";
	}
}

TEST_F(SunriseCaseIrradProc, solarposTest_lib_irradproc){
	double sun[9];
	vector<double> sunrise_times;
	vector<double> sunset_times;

	solarpos(year, month, day, 5, 30, lat, lon, tz, sun);
	vector<double> solution = { 1.11047, 1.6031, -0.0323028, 0.363806, 5.70924, 19.5179, 0.968281, 4.88641, 0 };
	sunrise_times.push_back(solution[4]);
	sunset_times.push_back(solution[5]);
	for (int i = 0; i < 9; i++){
		EXPECT_NEAR((double)sun[i], solution[i], e) << "sunrise case, parameter " << i << " fail\n";
	}
}

TEST_F(DayCaseIrradProc, solarposTest_lib_irradproc){
	double sun[9];
	vector<double> sunrise_times;
	vector<double> sunset_times;

	/* Just before sunset test case */
	solarpos(year, month, day, 18, 30, lat, lon, tz, sun);
	vector<double>solution = { 5.01022, 1.3584, 0.212397, 0.362076, 5.71461, 19.5137, 0.96835, 17.8858, 279.08756 };
	sunrise_times.push_back(solution[4]);
	sunset_times.push_back(solution[5]);
	for (int i = 0; i < 9; i++){
		EXPECT_NEAR((double)sun[i], solution[i], e) << "hourly before-sunset case, parameter " << i << " fail\n";
	}
	solarpos(year, month, day, 19, 15, lat, lon, tz, sun);
	solution = { 5.10579, 1.51295, 0.0578472, 0.361975, 5.71492, 19.5135, 0.968354, 18.6358, 76.5423 };
	sunrise_times.push_back(solution[4]);
	sunset_times.push_back(solution[5]);
	for (int i = 0; i < 9; i++){
		EXPECT_NEAR((double)sun[i], solution[i], e) << "15m before-sunset case, parameter " << i << " fail\n";
	}

	/* Sunset time test case */
	solarpos(year, month, day, 19, 30, lat, lon, tz, sun);
	solution = { 5.13947, 1.55886, 0.0119379, 0.361941, 5.71503, 19.5134, 0.968356, 18.8858, 15.8044 };
	sunrise_times.push_back(solution[4]);
	sunset_times.push_back(solution[5]);
	for (int i = 0; i < 9; i++){
		EXPECT_NEAR((double)sun[i], solution[i], e) << "sunset case, parameter " << i << " fail\n";
	}
}

TEST_F(SunsetCaseIrradProc, solarposTest_lib_irradproc){
	double sun[9];
	vector<double> sunrise_times;
	vector<double> sunset_times;

	/* Sunset time test case */
	solarpos(year, month, day, 19, 30, lat, lon, tz, sun);
	vector<double>solution = { 5.13947, 1.55886, 0.0119379, 0.361941, 5.71503, 19.5134, 0.968356, 18.8858, 15.8044 };
	sunrise_times.push_back(solution[4]);
	sunset_times.push_back(solution[5]);
	for (int i = 0; i < 9; i++){
		EXPECT_NEAR((double)sun[i], solution[i], e) << "sunset case, parameter " << i << " fail\n";
	}
}

/**
* Solar Incidence Function Test
* Mode = 0 for fixed tilt.
* Output: angle[] = incident angle (rad), tilt angle (rad), surface azimuth (rad), tracking axis rotation angle for single axis tracker (rad),
* backtracking angle difference: rot - ideal_rot (rad)
*/

TEST_F(NightCaseIrradProc, incidenceTest_lib_irradproc){
	int mode = 0;
	double angle[5] = { 0 };
	double sun_zen, sun_azm;
	vector<double> solutions;

	/* Just before sunrise test case */
	sun_azm = 0.95662;
	sun_zen = 1.79457;
	incidence(mode, tilt, azim, rotlim, sun_zen, sun_azm, backtrack_on, gcr, angle);
	solutions = { 1.89243, 0.174533, 3.14159, 0, 0 };
	for (int i = 0; i < 5; i++){
		EXPECT_NEAR(angle[i], solutions[i], e) << "before-sunrise case";
	}
}

TEST_F(SunriseCaseIrradProc, incidenceTest_lib_irradproc){
	int mode = 0;
	double angle[5] = { 0 };
	double sun_zen, sun_azm;
	double solution;
	vector<double> solutions;

	sun_azm = 1.11047;
	sun_zen = 1.6031;
	incidence(mode, tilt, azim, rotlim, sun_zen, sun_azm, backtrack_on, gcr, angle);
	solution = 1.67992;
	EXPECT_NEAR(angle[0], solution, e) << "sunrise case";
}

TEST_F(DayCaseIrradProc, incidenceTest_lib_irradproc){
	int mode = 0;
	double angle[5] = { 0 };
	double sun_zen, sun_azm;
	double solution;
	vector<double> solutions;

	sun_azm = 0;
	sun_zen = 0;
	incidence(mode, tilt, azim, rotlim, sun_zen, sun_azm, backtrack_on, gcr, angle);
	solution = 0.174533;
	EXPECT_NEAR(angle[0], solution, e) << "noon case";
}

TEST_F(SunsetCaseIrradProc, incidenceTest_lib_irradproc){
	int mode = 0;
	double angle[5] = { 0 };
	double sun_zen, sun_azm;
	double solution;
	vector<double> solutions;

	sun_azm = 5.13947;
	sun_zen = 1.55886;
	incidence(mode, tilt, azim, rotlim, sun_zen, sun_azm, backtrack_on, gcr, angle);
	solution = 1.631;
	EXPECT_NEAR(angle[0], solution, e) << "sunset case";
}

/**
* Calc Function Tests
* Output:
* sun[] =	azimuth (rad), zenith(rad), elevation(rad), declination(rad), sunrise time, sunset time,
*			eccentricity correction factor, true solar time, extraterrestrial solar irradiance on horizontal (W/m2);
* angle_p[] = incident angle (rad), tilt angle (rad), surface azimuth (rad), tracking axis rotation angle for single axis tracker (rad),
*			backtracking angle difference: rot - ideal_rot (rad);
* poa_p[] = incident beam, incident sky diffuse, incident ground diffuse, diffuse isotropic, diffuse circumsolar, horizon brightening (W/m2);
* irrad parameters: ghi, dni, dhi
*/

TEST_F(NightCaseIrradProc, CalcTestRadMode0_lib_irradproc){
	vector<double> sun_p;
	sun_p.resize(10);
	int sunup = false;
	vector<double> angle_p;
	angle_p.resize(5);
	vector<double> poa_p;
	poa_p.resize(6);
	vector<double> rad_p = { 1, 1, 1 };

	irr_hourly_night.set_beam_diffuse(rad_p[1], rad_p[2]);
	irr_15m_night.set_beam_diffuse(rad_p[1], rad_p[2]);

	/* Hourly during the night */
	irr_hourly_night.calc();
	irr_hourly_night.get_sun(&sun_p[0], &sun_p[1], &sun_p[2], &sun_p[3], &sun_p[4], &sun_p[5], &sunup, &sun_p[7], &sun_p[8], &sun_p[9]);
	irr_hourly_night.get_angles(&angle_p[0], &angle_p[1], &angle_p[2], &angle_p[3], &angle_p[4]);
	irr_hourly_night.get_poa(&poa_p[0], &poa_p[1], &poa_p[2], &poa_p[3], &poa_p[4], &poa_p[5]);
	irr_hourly_night.get_irrad(&rad_p[0], &rad_p[1], &rad_p[2]);

	sun_p[6] = (double)sunup;
	vector<double> sun_solution = { -999, -999, -999, 20.795182, 5.711921, 19.515852, 0, 0.968315, 11.386113, 1286.786711 };
	for (int i = 0; i < 10; i++){
		EXPECT_NEAR(sun_p[i], sun_solution[i], e) << "hourly_night, sun parameter " << i << " fail\n";
	}
	vector<double> angle_solution = { 0, 0, 0, 0, 0 };	// azim & tilt returned as 0 when sun is down
	for (int i = 0; i < 5; i++){
		EXPECT_NEAR(angle_p[i], angle_solution[i], e) << "hourly_night, angle parameter " << i << " fail\n";
	}
	vector<double> poa_solution = { 0, 0, 0, 0, 0, 0 };
	for (int i = 0; i < 6; i++){
		EXPECT_NEAR(poa_p[i], poa_solution[i], e) << "hourly_night, poa parameter " << i << " fail\n";
	}
	vector<double>  rad_solution = { 0, 0, 0 };
	for (int i = 0; i < 3; i++){
		EXPECT_NEAR(rad_p[i], rad_solution[i], e) << "hourly_night, irradiance parameter " << i << " fail\n";
	}

	/* 15m during the night */
	irr_15m_night.calc();
	irr_15m_night.get_sun(&sun_p[0], &sun_p[1], &sun_p[2], &sun_p[3], &sun_p[4], &sun_p[5], &sunup, &sun_p[7], &sun_p[8], &sun_p[9]);
	irr_15m_night.get_angles(&angle_p[0], &angle_p[1], &angle_p[2], &angle_p[3], &angle_p[4]);
	irr_15m_night.get_poa(&poa_p[0], &poa_p[1], &poa_p[2], &poa_p[3], &poa_p[4], &poa_p[5]);
	irr_15m_night.get_irrad(&rad_p[0], &rad_p[1], &rad_p[2]);

	sun_p[6] = (double)sunup;
	sun_solution = { -999, -999, -999, 20.795182, 5.711921, 19.515852, 0, 0.968315, 11.386113, 1286.786711 };
	for (int i = 0; i < 10; i++){
		EXPECT_NEAR(sun_p[i], sun_solution[i], e) << "15m_night, sun parameter " << i << " fail\n";
	}
	angle_solution = { 0, 0, 0, 0, 0 };
	for (int i = 0; i < 5; i++){
		EXPECT_NEAR(angle_p[i], angle_solution[i], e) << "15m_night, angle parameter " << i << " fail\n";
	}
	poa_solution = { 0, 0, 0, 0, 0, 0 };
	for (int i = 0; i < 6; i++){
		EXPECT_NEAR(poa_p[i], poa_solution[i], e) << "15m_night, poa parameter " << i << " fail\n";
	}
	rad_solution = { 0, 0, 0 };
	for (int i = 0; i < 3; i++){
		EXPECT_NEAR(rad_p[i], rad_solution[i], e) << "15m_night, irradiance parameter " << i << " fail\n";
	}
}

TEST_F(SunriseCaseIrradProc, CalcTestRadMode0_lib_irradproc){
	vector<double> sun_p;
	sun_p.resize(10);
	int sunup = true;
	vector<double> angle_p;
	angle_p.resize(5);
	vector<double> poa_p;
	poa_p.resize(6);
	vector<double> rad_p = { 1, 1, 1 };

	irr_hourly_sunrise.set_beam_diffuse(rad_p[1], rad_p[2]);
	irr_15m_sunrise.set_beam_diffuse(rad_p[1], rad_p[2]);

	/* hourly during sunrise */
	irr_hourly_sunrise.calc();
	irr_hourly_sunrise.get_sun(&sun_p[0], &sun_p[1], &sun_p[2], &sun_p[3], &sun_p[4], &sun_p[5], &sunup, &sun_p[7], &sun_p[8], &sun_p[9]);
	irr_hourly_sunrise.get_angles(&angle_p[0], &angle_p[1], &angle_p[2], &angle_p[3], &angle_p[4]);
	irr_hourly_sunrise.get_poa(&poa_p[0], &poa_p[1], &poa_p[2], &poa_p[3], &poa_p[4], &poa_p[5]);
	irr_hourly_sunrise.get_irrad(&rad_p[0], &rad_p[1], &rad_p[2]);

	sun_p[6] = (double)sunup;
	vector<double> sun_solution = { 66.441256, 87.969757, 2.030243, 20.841844, 5.709384, 19.517827, 2.0, 0.968283, 5.242355, 46.902536 };
	for (int i = 0; i < 10; i++){
		EXPECT_NEAR(sun_p[i], sun_solution[i], e) << "hourly_sunrise, sun parameter " << i << " fail\n";
	}
	vector<double> angle_solution = { 91.975545, tilt, azim, 0, 0 };
	for (int i = 0; i < 5; i++){
		EXPECT_NEAR(angle_p[i], angle_solution[i], e) << "hourly_sunrise, angle parameter " << i << " fail\n";
	}
	vector<double> poa_solution = { 0, 0.992404, 0, 0.992404, 0, 0 };
	for (int i = 0; i < 6; i++){
		EXPECT_NEAR(poa_p[i], poa_solution[i], e) << "hourly_sunrise, poa parameter " << i << " fail\n";
	}
	vector<double> rad_solution = { -999, 1, 1 };
	for (int i = 0; i < 3; i++){
		EXPECT_NEAR(rad_p[i], rad_solution[i], e) << "hourly_sunrise, irradiance parameter " << i << " fail\n";
	}

	/* 15m during sunrise */
	irr_15m_sunrise.calc();
	irr_15m_sunrise.get_sun(&sun_p[0], &sun_p[1], &sun_p[2], &sun_p[3], &sun_p[4], &sun_p[5], &sunup, &sun_p[7], &sun_p[8], &sun_p[9]);
	irr_15m_sunrise.get_angles(&angle_p[0], &angle_p[1], &angle_p[2], &angle_p[3], &angle_p[4]);
	irr_15m_sunrise.get_poa(&poa_p[0], &poa_p[1], &poa_p[2], &poa_p[3], &poa_p[4], &poa_p[5]);
	irr_15m_sunrise.get_irrad(&rad_p[0], &rad_p[1], &rad_p[2]);

	sun_p[6] = (double)sunup;
	sun_solution = { 66.441256, 87.969757, 2.030243, 20.841844, 5.709384, 19.517827, 2.0, 0.968283, 5.242355, 46.902536 };
	for (int i = 0; i < 10; i++){
		EXPECT_NEAR(sun_p[i], sun_solution[i], e) << "15m_sunrise, sun parameter " << i << " fail\n";
	}
	angle_solution = { 91.975545, tilt, azim, 0, 0 };
	for (int i = 0; i < 5; i++){
		EXPECT_NEAR(angle_p[i], angle_solution[i], e) << "15m_sunrise, angle parameter " << i << " fail\n";
	}
	poa_solution = { 0, 0.992404, 0, 0.992404, 0, 0 };
	for (int i = 0; i < 6; i++){
		EXPECT_NEAR(poa_p[i], poa_solution[i], e) << "15m_sunrise, poa parameter " << i << " fail\n";
	}
	rad_solution = { -999, 1, 1 };
	for (int i = 0; i < 3; i++){
		EXPECT_NEAR(rad_p[i], rad_solution[i], e) << "15m_sunrise, irradiance parameter " << i << " fail\n";
	}
}

TEST_F(DayCaseIrradProc, CalcTestRadMode0_lib_irradproc){
	vector<double> sun_p;
	sun_p.resize(10);
	int sunup = true;
	vector<double> angle_p;
	angle_p.resize(5);
	vector<double> poa_p;
	poa_p.resize(6);
	vector<double> rad_p = { 1, 1, 1 };

	irr_hourly_day.set_beam_diffuse(rad_p[1], rad_p[2]);
	irr_15m_day.set_beam_diffuse(rad_p[1], rad_p[2]);

	/* Hourly during the day */
	irr_hourly_day.calc();
	irr_hourly_day.get_sun(&sun_p[0], &sun_p[1], &sun_p[2], &sun_p[3], &sun_p[4], &sun_p[5], &sunup, &sun_p[7], &sun_p[8], &sun_p[9]);
	irr_hourly_day.get_angles(&angle_p[0], &angle_p[1], &angle_p[2], &angle_p[3], &angle_p[4]);
	irr_hourly_day.get_poa(&poa_p[0], &poa_p[1], &poa_p[2], &poa_p[3], &poa_p[4], &poa_p[5]);
	irr_hourly_day.get_irrad(&rad_p[0], &rad_p[1], &rad_p[2]);

	sun_p[6] = (double)sunup;
	vector<double> sun_solution = { 171.563258, 10.938523, 79.061477, 20.791368, 5.712128, 19.515691, 1.0, 0.968318, 11.886091, 1299.866650 };
	for (int i = 0; i < 10; i++){
		EXPECT_NEAR(sun_p[i], sun_solution[i], e) << "hourly_day, sun parameter " << i << " fail\n";
	}
	vector<double> angle_solution = { 1.795054, tilt, azim, 0, 0 };
	for (int i = 0; i < 5; i++){
		EXPECT_NEAR(angle_p[i], angle_solution[i], e) << "hourly_day, angle parameter " << i << " fail\n";
	}
	vector<double> poa_solution = { 0.999509, 1.052130, 0.003011, 0.194810, 0.818170, 0.039150 };
	for (int i = 0; i < 6; i++){
		EXPECT_NEAR(poa_p[i], poa_solution[i], e) << "hourly_day, poa parameter " << i << " fail\n";
	}
	vector<double> rad_solution = { -999, 1.0, 1.0 };
	for (int i = 0; i < 3; i++){
		EXPECT_NEAR(rad_p[i], rad_solution[i], e) << "hourly_day, irradiance parameter " << i << " fail\n";
	}

	/* 15m during the day */
	irr_15m_day.calc();
	irr_15m_day.get_sun(&sun_p[0], &sun_p[1], &sun_p[2], &sun_p[3], &sun_p[4], &sun_p[5], &sunup, &sun_p[7], &sun_p[8], &sun_p[9]);
	irr_15m_day.get_angles(&angle_p[0], &angle_p[1], &angle_p[2], &angle_p[3], &angle_p[4]);
	irr_15m_day.get_poa(&poa_p[0], &poa_p[1], &poa_p[2], &poa_p[3], &poa_p[4], &poa_p[5]);
	irr_15m_day.get_irrad(&rad_p[0], &rad_p[1], &rad_p[2]);

	sun_p[6] = (double)sunup;
	sun_solution = { 190.054756, 10.986005, 79.013994, 20.789459, 5.712231, 19.515691, 1.000000, 0.968318, 12.136079, 1299.658007 };
	for (int i = 0; i < 10; i++){
		EXPECT_NEAR(sun_p[i], sun_solution[i], e) << "15m_day, sun parameter " << i << " fail\n";
	}
	angle_solution = { 2.075962, tilt, azim, 0, 0 };
	for (int i = 0; i < 5; i++){
		EXPECT_NEAR(angle_p[i], angle_solution[i], e) << "15m_day, angle parameter " << i << " fail\n";
	}
	poa_solution = { 0.999343, 1.052130, 0.003011, 0.195107, 0.817860, 0.039150 };
	for (int i = 0; i < 6; i++){
		EXPECT_NEAR(poa_p[i], poa_solution[i], e) << "15m_day, poa parameter " << i << " fail\n";
	}
	rad_solution = { -999, 1.0, 1.0 };
	for (int i = 0; i < 3; i++){
		EXPECT_NEAR(rad_p[i], rad_solution[i], e) << "15m_day, irradiance parameter " << i << " fail\n";
	}
}

TEST_F(SunsetCaseIrradProc, CalcTestRadMode0_lib_irradproc){
	vector<double> sun_p;
	sun_p.resize(10);
	int sunup = false;
	vector<double> angle_p;
	angle_p.resize(5);
	vector<double> poa_p;
	poa_p.resize(6); 
	vector<double> rad_p = { 1, 1, 1 };
	irr_hourly_sunset.set_beam_diffuse(rad_p[1], rad_p[2]);
	irr_15m_sunset.set_beam_diffuse(rad_p[1], rad_p[2]);
	
	/* hourly during sunset */
	irr_hourly_sunset.calc();
	irr_hourly_sunset.get_sun(&sun_p[0], &sun_p[1], &sun_p[2], &sun_p[3], &sun_p[4], &sun_p[5], &sunup, &sun_p[7], &sun_p[8], &sun_p[9]);
	irr_hourly_sunset.get_angles(&angle_p[0], &angle_p[1], &angle_p[2], &angle_p[3], &angle_p[4]);
	irr_hourly_sunset.get_poa(&poa_p[0], &poa_p[1], &poa_p[2], &poa_p[3], &poa_p[4], &poa_p[5]);
	irr_hourly_sunset.get_irrad(&rad_p[0], &rad_p[1], &rad_p[2]);

	sun_p[6] = (double)sunup;
	vector<double>sun_solution = { 292.600441, 86.774381, 3.225619, 20.739568, 5.714928, 19.513485, 3.0, 0.968355, 18.643720, 74.494280 };
	for (int i = 0; i < 10; i++){
		EXPECT_NEAR(sun_p[i], sun_solution[i], e) << "hourly_sunset, sun parameter " << i << " fail\n";
	}
	vector<double>angle_solution = { 90.642562, tilt, azim, 0, 0 };
	for (int i = 0; i < 5; i++){
		EXPECT_NEAR(angle_p[i], angle_solution[i], e) << "hourly_sunset, angle parameter " << i << " fail\n";
	}
	vector<double>poa_solution = { 0, 0.981644, 0.001605, 0.992404, 0, -0.010760 };
	for (int i = 0; i < 6; i++){
		EXPECT_NEAR(poa_p[i], poa_solution[i], e) << "hourly_sunset, poa parameter " << i << " fail\n";
	}
	vector<double>rad_solution = { -999, 1, 1 };
	for (int i = 0; i < 3; i++){
		EXPECT_NEAR(rad_p[i], rad_solution[i], e) << "hourly_sunset, irradiance parameter " << i << " fail\n";
	}

	/* 15m during sunset */
	irr_15m_sunset.calc();
	irr_15m_sunset.get_sun(&sun_p[0], &sun_p[1], &sun_p[2], &sun_p[3], &sun_p[4], &sun_p[5], &sunup, &sun_p[7], &sun_p[8], &sun_p[9]);
	irr_15m_sunset.get_angles(&angle_p[0], &angle_p[1], &angle_p[2], &angle_p[3], &angle_p[4]);
	irr_15m_sunset.get_poa(&poa_p[0], &poa_p[1], &poa_p[2], &poa_p[3], &poa_p[4], &poa_p[5]);
	irr_15m_sunset.get_irrad(&rad_p[0], &rad_p[1], &rad_p[2]);

	sun_p[6] = (double)sunup;
	sun_solution = { 292.600441, 86.774381, 3.225619, 20.739568, 5.714928, 19.513485, 3.0, 0.968355, 18.643720, 74.494280 };
	for (int i = 0; i < 10; i++){
		EXPECT_NEAR(sun_p[i], sun_solution[i], e) << "15m_sunset, sun parameter " << i << " fail\n";
	}
	angle_solution = { 90.642562, tilt, azim, 0, 0 };
	for (int i = 0; i < 5; i++){
		EXPECT_NEAR(angle_p[i], angle_solution[i], e) << "15m_sunset, angle parameter " << i << " fail\n";
	}
	poa_solution = { 0, 0.981644, 0.001605, 0.992404, 0, -0.010760 };
	for (int i = 0; i < 6; i++){
		EXPECT_NEAR(poa_p[i], poa_solution[i], e) << "15m_sunset, poa parameter " << i << " fail\n";
	}
	rad_solution = { -999, 1, 1 };
	for (int i = 0; i < 3; i++){
		EXPECT_NEAR(rad_p[i], rad_solution[i], e) << "15m_sunset, irradiance parameter " << i << " fail\n";
	}

	/*
	printf("sun:%f, %f, %f, %f, %f, %f, %f, %f, %f, %f", sun_p[0], sun_p[1], sun_p[2], sun_p[3], sun_p[4], sun_p[5], (double)sunup, sun_p[6], sun_p[7], sun_p[8]);
	printf("angles: %f, %f, %f, %f, %f \n", angle_p[0], angle_p[1], angle_p[2], angle_p[3], angle_p[4]);
	printf("poa: %f, %f, %f, %f, %f, %f \n", poa_p[0], poa_p[1], poa_p[2], poa_p[3], poa_p[4], poa_p[5]);
	printf("irrad: %f, %f, %f \n", &rad_p[0], &rad_p[1], &rad_p[2]);
	*/
}

/**
*   Test Sky Configuration factors.  These factors do not change with time, just system geometry
*/
TEST_F(BifacialIrradTest, TestSkyConfigFactors)
{
	// Determine the factors for points on the ground from the leading edge of one row of PV panels to the edge of the next row of panels behind
	std::vector<double> rearSkyConfigFactors;
	irr->getSkyConfigurationFactors(rowToRow, verticalHeight, clearanceGround, distanceBetweenRows, horizontalLength, rearSkyConfigFactors);
	std::vector<double> expectedSkyConfigFactors = { 0.31746528768349, 0.3166898597414707, 0.3158845617514987, 0.31505447092609884, 0.31420468029924176, 0.313340276953247, 0.3124663205405842, 0.3115878222383039, 0.31070972426429816, 0.3098368800746455, 0.3089740353501973, 0.3081258098684984, 0.3072966803444087, 0.30649096430957745, 0.30571280508756976, 0.30496615790809833, 0.30425477719079363, 0.303582205016401, 0.3029517607914656, 0.30236653210158737, 0.30182936673836325, 0.30134286587627956, 0.30090937836815723, 0.30053099612133727, 0.30020955051164644, 0.299946609788294, 0.299743477420189, 0.29960119133268404, 0.29952052398337214, 0.29950198322619537, 0.2995458139146673, 0.2996520001973526, 0.2998202684617832, 0.3000500908865539, 0.30034068956536697, 0.300691041171112, 0.30109988213257166, 0.3015657143009256, 0.3020868110877593, 0.30266122406068663, 0.3032867899868308, 0.30396113831827176, 0.30468169911696935, 0.305445711419659, 0.3062502320456547, 0.30709214485236613, 0.30796817044461977, 0.3088748763445053, 0.30980868762847524, 0.3107658980377686, 0.31174268156691937, 0.3127351045331717, 0.31373913812706444, 0.31475067144128155, 0.315765524971164, 0.31677946457604256, 0.31778821588586126, 0.3187874791324413, 0.3197729443792757, 0.3207403071179795, 0.3216852841935352, 0.3226036300143336, 0.3234911529967789, 0.32434373218801665, 0.3251573340041737, 0.32592802901550877, 0.32665200870411437, 0.3273256021143607, 0.3279452923112385, 0.32850773255720117, 0.3290097621141306, 0.32944842157370274, 0.32982096761685376, 0.3301248871012388, 0.33035791037468787, 0.3305180237127191, 0.33060348077924484, 0.33061281301177303, 0.3305448388356977, 0.3303986716167246, 0.3301737262661392, 0.3298697244204676, 0.32948669812513764, 0.3290249919609623, 0.3284852635626013, 0.32786848248955747, 0.3271759274226039, 0.3264091816717319, 0.32557012699560445, 0.32466093574692756, 0.3236840613729327, 0.32264222731510417, 0.3215384143671419, 0.3203758465647328, 0.3191579756947288, 0.3178884645246083, 0.3165711688653665, 0.3152101185920348, 0.3138094977556616, 0.3123736239286202 };
	
	ASSERT_EQ(rearSkyConfigFactors.size(), expectedSkyConfigFactors.size());

	// Can eventually use GoogleMock to do this more easily
	for (size_t i = 0; i != rearSkyConfigFactors.size(); i++){
		ASSERT_NEAR(rearSkyConfigFactors[i], expectedSkyConfigFactors[i], e);
	}
}