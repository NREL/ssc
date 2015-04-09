#include "lib_snowmodel.h"
#include "lib_util.h"
#include <cmath>
#include <iostream>


#ifndef M_PI
#define M_PI 3.14159265358979323846264338327
#endif

/**********************************************************************************
************************************************************************************
**
**	6 April 2015
**
**	Implementation of Bill Marion's snow model [1] to C++ for use in SAM
**	Original author: David Severin Ryberg
**
**
**********************************************************************************
**  References:
**  1: Marion, Bill, et al. "Measured and modeled photovoltaic system
**     energy losses from snow for Colorado and Wisconsin locations."
**     Solar Energy 97 (2013): 112-121.
**
**********************************************************************************
***********************************************************************************/


pvsnowmodel::pvsnowmodel()
{
	mSlope = 80;
	sSlope = 1.97;
	deltaThreshold = 1.00;
	depthThreshold = 1.00;
	previousDepth = 0;
	badValues = 0;
	maxBadValues = 500;
	coverage = 0;
	pCvg = 0;

	good = true;
	msg = "";

}

bool pvsnowmodel::setup(int nmody_in, float baseTilt_in){

	nmody = nmody_in;
	baseTilt = baseTilt_in;
	return true;
}


bool pvsnowmodel::getLoss(float poa, float tilt, float wspd, float tdry, float snowDepth, int sunup, float dt, float *returnLoss){

	bool isGood = true;

	// Check if snow depth value is valid
	if (snowDepth == -9999 || snowDepth == -999 || isnan(snowDepth)){
		isGood = false;
		snowDepth = 0;
		badValues++;
		if (badValues == maxBadValues){
			good = false;
			msg = util::format("The maximum acceptable number of bad snow depth values (%d) has been reached. Terminating...", maxBadValues);
			return false;
		} 
	}
		
	/////////////////////////////
	// Step 1
	// look for snow fall and set current snow coverage amount accordingly
	if ((snowDepth - previousDepth) >= deltaThreshold*dt && snowDepth >= depthThreshold){
		coverage = 1;
	}
	else{
		coverage = pCvg;
	}

	// Coverage Override #1:
	//  This override assumes that if the current snow depth is less than the threshold,
	// then the snow coverage on on the PV arrays should be set to zero (even if the model
	// says otherwise).
	if (snowDepth < depthThreshold) coverage = 0;

	/////////////////////////////
	// Step 2
	//
	// Calculating Energy output is done outside this function

	/////////////////////////////
	// Step 3
	//
	// This model is adapted for hourly snow data, and so is not analyzed with
	// with respect to individual days, but instead for all 8760 yearly hours.
	// Hence, this step is skipped

	/////////////////////////////
	// Step 4
	//
	
	// If the day-time flag is not set, assume the tilt value should be equal to the system's base tilt
	//   This will fix the issue in sam where system tilt values during nightime hours is zero (even 
	//   for static systems). This may need to be altered for 1-axis tracking and 2-axis tracking systems
	if (sunup == 0){
		tilt = baseTilt;
	}

	// check if conditions are right for sliding
	if (tdry - poa / mSlope > 0){
		coverage -= 0.1 * sin(tilt * M_PI / 180 * sSlope) * dt;
	}
			// Coverage Override #2
	//  This override prevents the snow coverage from going below 0
	if (coverage < 0) coverage = 0;
			/////////////////////////////
	// Step 5

	*returnLoss = ((float)ceil(coverage*nmody))/nmody;
	
	/////////////////////////////
	// Step 6 & 7
	//
	// Calculating Energy output is done outside this function

	/////////////////////////////
	// Step 8

	// These values are reported to SAM after this function completes

	/////////////////////////////
	// Step 9

	//  Current snow depth and previous snow depth are set to the correct values once
	// this function is run again

	previousDepth = snowDepth;
	pCvg = coverage;

	if (isGood) return true;
	else return false;
}
