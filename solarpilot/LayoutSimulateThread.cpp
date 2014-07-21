#include "LayoutSimulateThread.h"
#include "SolarField.h"

#ifdef SP_USE_THREADS

using namespace std;
	
void LayoutSimThread::Setup(SolarField *SF, var_set *vset, sim_results *results, WeatherData *wdata, 
	int sim_first, int sim_last, bool is_shadow_detail, bool is_flux_detail)
{
	/* 
	Assign all of the arguments to local memory
	*/
	_SF = SF;
	_results = results;
	_wdata = wdata;
	_sol_azzen = nullptr;
	_sim_first = sim_first;
	_sim_last = sim_last;
	Finished = false;
	CancelFlag = false;
	Nsim_complete = 0;
	Nsim_total = _sim_last - _sim_first;
	_is_user_sun_pos = false;
	_is_shadow_detail = is_shadow_detail;
	_is_flux_detail = is_flux_detail;
	_vset = vset;
	_is_flux_normalized = true;
};

void LayoutSimThread::Setup(SolarField *SF, var_set *vset, sim_results *results, matrix_t<double> *sol_azzen, 
	double args[4], int sim_first, int sim_last, bool is_shadow_detail, bool is_flux_detail)
{
	/* 
	overload to allow specification of simulation sun positions. 
	Sun positions provided in a matrix_t
			|	Azimuth	|	Elevation
	Row		|	(rad)	|	(rad)
	--------------------------------
	1		|	az1		|	el1
	2		|	az2		|	el2
	...
	
	Args:
	args[0]	|	DNI		|	W/m2
	args[1]	|	Tdb		|	C
	args[2]	|	Vwind	|	m/s
	args[3]	|	Pres	|	bar

	*/
	_SF = SF;
	_results = results;
	_wdata = nullptr;
	_sol_azzen = sol_azzen;
	_sim_first = sim_first;
	_sim_last = sim_last;
	Finished = false;
	CancelFlag = false;
	Nsim_complete = 0;
	Nsim_total = _sim_last - _sim_first;
	_is_user_sun_pos = true;
	for(int i=0; i<4; i++)
		_user_args[i] = args[i];
	_is_shadow_detail = is_shadow_detail;
	_is_flux_detail = is_flux_detail;
	_vset = vset;
	_is_flux_normalized = true;
};

void LayoutSimThread::IsFluxmapNormalized(bool is_normal)
{
	_is_flux_normalized = is_normal;
}

void LayoutSimThread::CancelSimulation()
{
	CancelLock.lock();
	CancelFlag = true;
	CancelLock.unlock();
}

bool LayoutSimThread::IsSimulationCancelled()
{
	bool r;
	CancelLock.lock();
	r = CancelFlag;
	CancelLock.unlock();
	return r;
}

bool LayoutSimThread::IsFinished()
{
	bool f;
	FinishedLock.lock();
	f = Finished;
	FinishedLock.unlock();
	return f;
}

void LayoutSimThread::UpdateStatus(int nsim_complete, int nsim_total)
{
	StatusLock.lock();
	Nsim_complete = nsim_complete;
	Nsim_total = nsim_total;
	StatusLock.unlock();
}

void LayoutSimThread::GetStatus( int *nsim_complete, int *nsim_total)
{
	StatusLock.lock();
	*nsim_complete = this->Nsim_complete;
	*nsim_total = this->Nsim_total;
	StatusLock.unlock();
}

void LayoutSimThread::StartThread() //Entry()
{
	/* 
	This method duplicates the functionality of SolarField::LayoutSimulate(...)

	This method is intended to be thread safe and can be called by the GUI directly. Each thread must have 
	its own instance of _SF. Before running multiple threads, create a solar field object, prepare it with
	PrepareFieldLayout(...), and use the deep copy constructor in SolarField to create as many duplicate
	objects as there are threads. Call this method for each duplicate object.

	*/
	double pi = acos(-1.);
	//Run the simulation 
	double dni, dom, doy, hour, month, tdb, pres, wind, step_weight;
	double az, zen, azzen[2];
			
	int Npos = _SF->getHeliostats()->size();
				
	//Simulate for each time
	StatusLock.lock();
	bool is_cancel = this->CancelFlag; //check for cancelled simulation
	StatusLock.unlock();
	if(is_cancel){
		FinishedLock.lock();
		Finished = true;
		FinishedLock.unlock();
		return; // (wxThread::ExitCode)-1;
	}

	if(_sim_first < 0) _sim_first = 0;
	if(_sim_last < 0) _sim_last = _wdata->size();

	int nsim = _sim_last - _sim_first + 1;
	for(int i=_sim_first; i<_sim_last; i++){
		//_SF->getSimInfoObject()->setCurrentSimulation(i+1);
		double args[5];

		//either calculate the sun position based on weather data steps, or use user-defined values
		if(! _is_user_sun_pos){

			//---- Calculate sun positions

			//Get the design-point day, hour, and DNI
			_wdata->getStep(i, dom, hour, month, dni, tdb, pres, wind, step_weight);

			//Convert the day of the month to a day of year
			doy = _SF->getAmbientObject()->getDateTimeObj()->GetDayOfYear(2011,int(month),int(dom));
				

			//Calculate the sun position
			_SF->getAmbientObject()->setDateTime(hour, doy);
			//latitude, longitude, and elevation should be set in the input file
			_SF->getAmbientObject()->calcSunPosition(azzen);
			az = azzen[0]; 
			zen = azzen[1];
			//If the sun is not above the horizon, don't continue
			if( zen > pi*0.5 ) 
					continue;
				
			//Simulate field performance
			args[0] = dni;
			args[1] = tdb;
			args[2] = wind;
			args[3] = pres/1000.;
			args[4] = step_weight;
		}
		else{
			//set the user-specified values
			az = _sol_azzen->at(i,0);
			zen = _sol_azzen->at(i,1);

			//Update the solar field to match specified sun position
			_SF->getAmbientObject()->setSolarPosition(az, zen);

			for(int j=0; j<4; j++)
				args[j] = _user_args[j];
		}

		if(_is_shadow_detail || _is_flux_detail)
			interop::AimpointUpdateHandler(*_SF, *_vset);
			
		if(_is_flux_detail)
			_SF->HermiteFluxSimulation( *_SF->getHeliostats() );
		else
			_SF->Simulate(args, 5, !_is_shadow_detail);
			
		//store the _results
		_results->at(i).process_analytical_simulation(*_SF, _is_flux_detail ? 2 : 0); //2);
		
		//optionally post-process the flux results as well
		if(_is_flux_detail)
			_results->at(i).process_flux(_SF, _is_flux_normalized);

		//Update progress
		UpdateStatus(i-_sim_first+1,nsim);
		//Check for user cancel
		StatusLock.lock();
		bool is_cancel = this->CancelFlag; 
		StatusLock.unlock();
		if(is_cancel){
			FinishedLock.lock();
			Finished = true;
			FinishedLock.unlock();
			return;
		}			
	}
	FinishedLock.lock();
	Finished = true;
	FinishedLock.unlock();

	return;

};

	
#endif // SP_USE_THREADS
