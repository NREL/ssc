#ifndef _SOLARFIELD_API_
#define _SOLARFIELD_API_ 1

#include "SolarField.h"
#include "API_structures.h"


//-------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------

class AutoPilot 
{

protected:
	void (*_callback)(simulation_info *siminfo, void *data);
	void *_callback_data;

	SolarField *_SF;
	var_set _variables;
	vector<sim_result> _results;
	int _sim_total;
	int _sim_complete;
	
	bool _cancel_simulation;	//changing this flag to "true" will cause the current simulation to terminate
	bool _has_callback;			//A callback function has been provided to the API.
	bool _is_deep_callback;		//Should the callback function be passed down to the solarfield object? Otherwise just use it at the top level.
	bool _is_solarfield_external;		//Is the SolarField object provided externally? Otherwise, it will be created and destroyed locally
	bool
		_setup_ok,	//The variable structure has been created
		_simflag;	//add bool flags here to indicate simulation/setup status

	sp_layout *_layout;

	void update_ambient(var_set &vset, sp_ambient &ambient);
	void update_cost(var_set &vset, sp_cost &cost);
	void update_layout(var_set &vset, sp_layout &layout);
	void update_heliostats(var_set &vset, sp_heliostats &helios);
	void update_receivers(var_set &vset, sp_receivers &recs);

	vector<double> interpolate_vectors( vector<double> &A, vector<double> &B, double alpha);

	void PrepareFluxSimulation(sp_flux_table &fluxtab, int flux_res_x, int flux_res_y, bool is_normalized);
	void PostProcessLayout();
	void PostProcessFlux(sim_result &result, sp_flux_map &fluxmap, int flux_layer = 0);
	
	void GenerateSurfaceEvalPoints( vector<double> &point, vector<vector<double>> &sim_points, double tolerance = 0.05 );
	
	simulation_info *_local_siminfo;

public:
	AutoPilot();
	~AutoPilot();
	void SetCallback( void (*callback)(simulation_info* siminfo, void *data), void *cdata, bool is_deep_callback = false);
	void SetExternalSFObject(SolarField *SF);
	bool Setup(sp_ambient &ambient, sp_cost &cost, sp_layout &layout, sp_heliostats &helios, sp_receivers &recs);
	void SetupExpert(var_set &variables);
	void GenerateDesignPointSimulations(sp_ambient &amb, var_set &variables, vector<string> &hourly_weather_data);
		
	bool SimulateFlux(sp_flux_map &fluxmap);  //individual flux simulation - not multi-threaded
	virtual bool EvaluateDesign(sp_optimize &opt, sp_receivers &recs, sp_layout &layout, double &obj_metric, double &flux_max);
	virtual bool CreateLayout();
	virtual bool CalculateOpticalEfficiencyTable(sp_optical_table &opttab);
	virtual bool CalculateFluxMaps(sp_flux_table &fluxtab, int flux_res_x = 12, int flux_res_y = 10, bool is_normalized = true);
	bool Optimize(sp_optimize &opt, sp_receivers &recs, sp_layout &layout);

	void CancelSimulation();
	
};


class AutoPilot_S : public AutoPilot
{
	bool EvaluateDesign(sp_optimize &opt, sp_receivers &recs, sp_layout &layout, double &obj_metric, double &flux_max);
	
public:
	//methods
	bool CreateLayout();
	bool CalculateOpticalEfficiencyTable(sp_optical_table &opttab);
	bool CalculateFluxMaps(sp_flux_table &fluxtab, int flux_res_x = 12, int flux_res_y = 10, bool is_normalized = true);
};

class AutoPilot_MT : public AutoPilot
{
	int _n_threads;	//the maximum number of threads to simulate
	int _n_threads_active;	//the number of threads currently used for simulation
	LayoutSimThread *_simthread;
	bool _in_mt_simulation;

public:
	//constructor
	AutoPilot_MT();

	//methods
	bool CreateLayout();
	bool CalculateOpticalEfficiencyTable(sp_optical_table &opttab);
	bool CalculateFluxMaps(sp_flux_table &fluxtab, int flux_res_x = 12, int flux_res_y = 10, bool is_normalized = true);

	//other methods
	bool SetMaxThreadCount(int nt);
	bool EvaluateDesign(sp_optimize &opt, sp_receivers &recs, sp_layout &layout, double &obj_metric, double &flux_max);

	void CancelMTSimulation();
};

#endif