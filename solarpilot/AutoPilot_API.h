/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as “System Advisor Model” or “SAM”. Except
*  to comply with the foregoing, the terms “System Advisor Model”, “SAM”, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

#ifndef _SOLARFIELD_API_
#define _SOLARFIELD_API_ 1


#include "API_structures.h"
#include "definitions.h"


#if defined(__WINDOWS__)&&defined(__DLL__)
#define SPEXPORT __declspec(dllexport)
#else
#define SPEXPORT
#endif


class simulation_info;
class sim_result;
class SolarField;
class LayoutSimThread;



//-------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------

class SPEXPORT AutoPilot 
{

protected:
	bool (*_summary_callback)(simulation_info *siminfo, void *data);
	void *_summary_callback_data;
	bool (*_detail_callback)(simulation_info *siminfo, void *data);
	void *_detail_callback_data;

	SolarField *_SF;
	//var_map _variables;
	int _sim_total;
	int _sim_complete;
	
	bool _cancel_simulation;	//changing this flag to "true" will cause the current simulation to terminate
	bool _has_summary_callback;			//A callback function has been provided to the API.
	bool _has_detail_callback;
	bool _is_solarfield_external;		//Is the SolarField object provided externally? Otherwise, it will be created and destroyed locally
	bool
		_setup_ok,	//The variable structure has been created
		_simflag;	//add bool flags here to indicate simulation/setup status

    sp_optimize *_opt;

	vector<double> interpolate_vectors( vector<double> &A, vector<double> &B, double alpha);


	void PrepareFluxSimulation(sp_flux_table &fluxtab, int flux_res_x, int flux_res_y, bool is_normalized);
	void PostProcessLayout(sp_layout &layout);
	void PostProcessFlux(sim_result &result, sp_flux_map &fluxmap, int flux_layer = 0);
	

	bool CalculateFluxMapsOV1(vector<vector<double> > &sunpos, vector<vector<double> > &fluxtab, vector<double> &efficiency, 
		int flux_res_x = 12, int flux_res_y = 10, bool is_normalized = true);

	simulation_info *_summary_siminfo;
	simulation_info *_detail_siminfo;

public:
	AutoPilot();
	virtual ~AutoPilot();
	//Callbacks for progress updates
	void SetSummaryCallback( bool (*callback)(simulation_info* siminfo, void *data), void *cdata);
	void SetDetailCallback( bool (*callback)(simulation_info* siminfo, void *data), void *cdata);
	void SetSummaryCallbackStatus(bool is_enabled);
	void SetDetailCallbackStatus(bool is_enabled);
	//setup
	void PreSimCallbackUpdate();
	void SetExternalSFObject(SolarField *SF);
	bool Setup(var_map &V, bool for_optimize = false);
	//generate weather data
	void GenerateDesignPointSimulations(var_map &V, vector<string> &hourly_weather_data);
	//Simulation methods
	bool EvaluateDesign(double &obj_metric, double &flux_max, double &tot_cost);
	void PostEvaluationUpdate(int iter, vector<double> &pos,/* vector<double> &normalizers,*/ double &obj, double &flux, double &cost, std::string *note=0);
	virtual bool CreateLayout(sp_layout &layout, bool do_post_process = true)=0;
	virtual bool CalculateOpticalEfficiencyTable(sp_optical_table &opttab)=0;
	virtual bool CalculateFluxMaps(sp_flux_table &fluxtab, int flux_res_x = 12, int flux_res_y = 10, bool is_normalized = true)=0;
	virtual bool CalculateFluxMaps(vector<vector<double> > &sunpos, vector<vector<double> > &fluxtab, vector<double> &efficiency, 
		int flux_res_x = 12, int flux_res_y = 10, bool is_normalized = true)=0;
	bool Optimize(int method, vector<double*> &optvars, vector<double> &upper_range, vector<double> &lower_range, vector<double> &stepsize, vector<string> *names=0);
	bool OptimizeRSGS(vector<double*> &optvars, vector<double> &upper_range, vector<double> &lower_range, vector<bool> &is_range_constr, vector<string> *names=0);
    bool OptimizeAuto(vector<double*> &optvars, vector<double> &upper_range, vector<double> &lower_range, vector<double> &stepsize, vector<string> *names=0);
    bool OptimizeSemiAuto(vector<double*> &optvars, vector<double> &upper_range, vector<double> &lower_range, vector<bool> &is_range_constr, vector<string> *names=0);
	//cancellation methods
	void CancelSimulation();
	bool IsSimulationCancelled();
    //other
    sp_optimize *GetOptimizationObject();
    
    struct API_CANT_TYPE { enum A {NONE, ON_AXIS, EQUINOX, SOLSTICE_SUMMER, SOLSTICE_WINTER }; };
	//struct FOCUS_TYPE { enum A { FLAT, AT_SLANT, USER_DEFINED }; };
	//struct ATTEN_MODEL { enum A { DELSOL_CLEAR_DAY, DELSOL_HAZY_DAY, USER_DEFINED }; };
	
};


class SPEXPORT AutoPilot_S : public AutoPilot
{
	
	
public:
	//methods
	bool CreateLayout(sp_layout &layout, bool do_post_process = true);
	bool CalculateOpticalEfficiencyTable(sp_optical_table &opttab);
	bool CalculateFluxMaps(sp_flux_table &fluxtab, int flux_res_x = 12, int flux_res_y = 10, bool is_normalized = true);
	bool CalculateFluxMaps(vector<vector<double> > &sunpos, vector<vector<double> > &fluxtab, vector<double> &efficiency, 
		int flux_res_x = 12, int flux_res_y = 10, bool is_normalized = true);

};

#ifdef SP_USE_THREADS

class SPEXPORT AutoPilot_MT : public AutoPilot
{
	int _n_threads;	//the maximum number of threads to simulate
	int _n_threads_active;	//the number of threads currently used for simulation
	LayoutSimThread *_simthread;
	bool _in_mt_simulation;
	void CancelMTSimulation();

public:
	//constructor
	AutoPilot_MT();

	//methods
	bool CreateLayout(sp_layout &layout, bool do_post_process = true);
	bool CalculateOpticalEfficiencyTable(sp_optical_table &opttab);
	bool CalculateFluxMaps(sp_flux_table &fluxtab, int flux_res_x = 12, int flux_res_y = 10, bool is_normalized = true);
	bool CalculateFluxMaps(vector<vector<double> > &sunpos, vector<vector<double> > &fluxtab, vector<double> &efficiency, 
		int flux_res_x = 12, int flux_res_y = 10, bool is_normalized = true);

	//other methods
	bool SetMaxThreadCount(int nt);
	void CancelSimulation();
	
};

#endif // SP_USE_THREADS

#endif
