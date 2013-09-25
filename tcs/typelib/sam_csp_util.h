#ifndef __CSP_UTIL_
#define __CSP_UTIL_

#include "recore/lib_util.h"

using namespace std;

/* 
Define functions and methods that are useful in CSP modules
*/

namespace CSP
{
	static double sigma = 5.67E-8;		//[W/m2K4] stefan boltzmann constant
	static double grav = 9.81;			//[m/s2] gravitational constant
	static double pi = 3.1415926;		//[-]

	//--- generalized interpolation functions ---

	double interp(util::matrix_t<double> *data, double x, int low_bound = -1, int up_bound = -1, bool increasing = true);

	double interp(double *xdat, double *ydat, double x, int low_bound, int up_bound, bool increasing = true);

	double interp2D(double *xvals, int &nx, double *yvals, int &ny, double *data2D, double x, double y, bool strict_range=false); 

	void theta_trans(double alpha_sun, double phi_sun, double alpha_fix, double &phi_t, double &theta);

	//sky temp function
	double skytemp(double T_amb, double T_dp, double hour);

	double sign(double val);

	double nint(double val);

	int TOU_Reader(double *TOUSched, double time_sec, int nTOUSched=8760);

	double poly_eval(double x, const double *coefs, const int &order);

	double Nusselt_FC( double ksDin, double Re );

	void PipeFlow(double Re, double Pr, double LoverD, double relRough, double &Nusselt, double &f);

	void flow_patterns( int n_panels, int flow_type, util::matrix_t<int> & flow_pattern );

	// CSP Cooling functions
	double P_sat4(double T_celcius);

	// Calculates enthalpy of air [J/kg] as a function of temperature [C]
	double f_h_air_T(double T_C);

	// Turbine isentropic efficiency penalty as a function of mass flow fraction (Patnode thesis)
	double eta_pl(double mf);

	// Evaporative cooling calculations
	void evap_tower(int tech_type, double P_cond_min, int n_pl_inc, double DeltaT_cw_des, double T_approach, double P_cycle, 
					double eta_ref, double T_db_K, double T_wb_K, double P_amb_Pa, double q_reject, double &m_dot_water,
					double &W_dot_tot, double &P_cond, double &T_cond, double &f_hrsys);

	// Air cooling calculations
	void ACC( int tech_type, double P_cond_min, int n_pl_inc, double T_ITD_des, double P_cond_ratio, double P_cycle, double eta_ref, 
		 double T_db_K, double P_amb_Pa, double q_reject, double& m_dot_air, double& W_dot_fan, double& P_cond, double& T_cond, 
		 double &f_hrsys);

	// Hybrid cooling calculations
	void HybridHR( int tech_type, double P_cond_min, int n_pl_inc, double F_wc, double F_wcmax, double F_wcmin,
				  double T_ITD_des, double T_approach, double dT_cw_ref, double P_cond_ratio, double P_cycle, double eta_ref, 
				  double T_db_K, double T_wb_K, double P_amb_Pa, double q_reject, double& m_dot_water, double& W_dot_acfan, 
				  double& W_dot_wctot, double& W_dot_tot, double& P_cond, double& T_cond, double& f_hrsys);

};

// Set up class for Pmax function so we can save maximum pressure for various CSP types
class P_max_check
{
	double P_max;
	double P_save;
	double P_return;
	bool is_error;

public:
	P_max_check(){};

	~P_max_check(){};

	void set_P_max( double P_max_set );

	void report_and_reset();

	double P_check( double P );

};

// Set up class for enthalpy limit function
class enth_lim
{
private:
	double h_min;
	double h_max;

public:
	enth_lim() {};
	~enth_lim() {};

	void set_enth_limits( double h_min_in, double h_max_in );

	double check( double h_in );
};

// ------- define the emittance table class for the physical trough and linear fresnel-molten salt models
class emit_table
{
	double *T;	//Temperature
	double *E;	//Emittance
	int *lengths;	//Array of lengths of each table
	int *starts;	//Start position of each table
	int memsize, datasize, nloaded, ntables, nt, nv;

public:
	emit_table() {
		T = E = 0;
		lengths = starts = 0;
		memsize = datasize = nloaded = ntables = nt = nv = 0;
	};
	
	~emit_table(){
		if(lengths) delete [] lengths;
		if(starts) delete [] starts;
		if(T) delete[] T;
		if(E) delete[] E;
	};
	
	void init(int nHCEVars){
		init(1, nHCEVars);
	};


	void init(int nHCEtypes, int nHCEvars){
		ntables = nHCEtypes * nHCEvars;
		nt = nHCEtypes;
		nv = nHCEvars;
		lengths = new int[ntables];
		lengths[0] = 0;
		starts = new int[ntables];
		starts[0] = 0;

		//initialize the data arrays with a preallocation of 15 entries per table.
		memsize = 15*ntables;
		T = new double[memsize];
		E = new double[memsize];
		datasize = 0;	//The current number of total entries 
		nloaded = 0;	//The number of tables currently loaded
	};

	bool addTable(util::matrix_t<double> *table){
		/*
		Take the data from the matrix_t entry for a single emittance table and load it into the emit_table object

		Column 0: Temperature [C]
		Column 1: Emittance(T)	[-]
		*/

		//Is the object full?
		if(nloaded + 1 == ntables) return false;
		
		//Get the dimensions of the table thats up for addition
		int nr = table->nrows();
		int nc = table->ncols();
		if( nr != 2) return false;

		//if we need to add space, copy data to a new array
		if(datasize + nc > memsize){
			memsize = datasize + nr;
			if(T) delete[] T;
			double *Ttemp = new double[memsize];
			if(E) delete[] E;
			double *Etemp = new double[memsize];
			
			for(int i=0; i<datasize; i++){
				Ttemp[i] = T[i];
				Etemp[i] = E[i];
			}

			delete [] T;
			delete [] E;
			T = Ttemp;
			E = Etemp;
			//Not sure this is right...
		}

		//Add the table
		lengths[nloaded] = nc;
		starts[nloaded] = datasize;
		for(int i=0; i<nc; i++){
			T[datasize + i] = table->at(0, i);
			E[datasize + i] = table->at(1, i);
		}
		datasize += nc;

		nloaded ++;
		return true;
	}

	int getTableSize(int hce_var){
		return getTableSize(0, hce_var);
	}

	int getTableSize(int hce_type, int hce_var){
		/* inputs should be indexed at 0 */
		int n = hce_type * nv + hce_var;
		return lengths[n];
	}

	bool isTable(int hce_var){
		return isTable(0, hce_var);
	}
	
	bool isTable(int hce_type, int hce_var){
		return getTableSize(hce_type, hce_var) == 1;
	}

	double interpolate(int hce_var, double Temp){
		return interpolate(0, hce_var, Temp);
	}
	
	double interpolate(int hce_type, int hce_var, double Temp){
		int n = hce_type * nt + hce_var;	//Index
		int lb = starts[n],
			ub = starts[n] + lengths[n]-1;
		return CSP::interp(T, E, Temp, lb, ub);
	}

	double getSingleValue(int hce_var){
		return getSingleValue(0, hce_var);
	}

	double getSingleValue(int hce_type, int hce_var){
		/*Get the emittance value for a single-entry table. For full tables, return NULL. */
		int n = hce_type * nt + hce_var;
		if(lengths[n] > 1){
			return std::numeric_limits<double>::quiet_NaN();
		}
		else{
			return E[starts[n]];
		}
	}
};

// Optical data table for 2D interpolation
class OpticalDataTable
{
	/* 
	Structure for providing optical efficiency in tabular form. 

	Use the following solar angle conventions:

	Azimuth		|	-pi & +pi are North 
				|	-pi/2 is East
				|	0 is South
				|	+pi/2 is West
	Zenith		|	0 is vertical
				|	pi/2 is horizon
				|	>pi/2. is below horizon
	*/
	
	double *xvals, *yvals, *data;
	bool xax_allocated, yax_allocated, data_allocated;
	int sizex, sizey;

public:
	OpticalDataTable(){
		xax_allocated = false;
		yax_allocated = false;
		data_allocated = false;
	}

	~OpticalDataTable(){
		if(xax_allocated) delete [] xvals;
		if(yax_allocated) delete [] yvals;
		if(data_allocated) delete [] data;
	}

	void AddXAxis(double *xdata, int nx){
		if(xax_allocated) delete [] xvals;
		sizex = nx;
		xvals = new double[nx];
		xax_allocated = true;
		for(int i=0; i<nx; i++)
			xvals[i] = xdata[i];
	}

	void AddYAxis(double *ydata, int ny){
		if(yax_allocated) delete [] yvals;
		sizey = ny; 
		yvals = new double[ny];
		yax_allocated = true;
		for(int i=0; i<ny; i++)
			yvals[i] = ydata[i];
	}

	void AddData(double *dat){
		if(data_allocated) delete [] data;
		data = new double[sizex * sizey];
		data_allocated = true;
		for(int i=0; i<sizey; i++){
			for(int j=0; j<sizex; j++){
				data[ i*sizex + j ] = dat[ i*sizex + j ];
			}
		}
	}

	bool AddData(util::matrix_t<double> &dat){
		if( (int)dat.nrows() != sizey || (int)dat.ncols() != sizex )
			return false;

		if(data_allocated) delete [] data;
		data = new double[sizex * sizey];
		data_allocated = true;
		for(int i=0; i<sizey; i++){
			for(int j=0; j<sizex; j++){
				data[j*sizex + i] = dat.at(i, j);
			}
		}
		return true;
	}

	double interpolate(double x, double y){
		return CSP::interp2D(xvals, sizex, yvals, sizey, data, x, y);
	}

	double nearest(double x, double y){
		int nearx=0, neary=0;
		double rx=9.e9, ry=9.e9;
		for(int i=0; i<sizex; i++){
			double r = abs(x - xvals[i]);
			if(r < rx){
				rx = r;
				nearx = i;
			}
		}
		for(int i=0; i<sizey; i++){
			double r = abs(y - yvals[i]);
			if(r < ry){
				ry = r;
				neary = i;
			}
		}
		return data[neary*sizex + nearx];
	}

};

class TwoOptTables
{
	// Collect and manage two instances of OpticalDataTable class
private:
	OpticalDataTable * table0;
	OpticalDataTable * table1;

public:
	TwoOptTables(){};

	~TwoOptTables(){};

	bool Set_Table( OpticalDataTable * table_in, int table_index )
	{
		if( table_index == 0 )
		{
			table0 = table_in;
			return true;
		}
		else if( table_index == 1 )
		{
			table1 = table_in;
			return true;
		}
		else
			return false;
	}

	double interpolate( double x, double y, int table_index )
	{
		if( table_index == 0 )
			return table0->interpolate( x, y );
		else if( table_index == 1 )
			return table1->interpolate( x, y );
		else
			return -999.9;
	}

};

#endif
