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

#ifndef __NGCC_PB_
#define __NGCC_PB_

#include "sam_csp_util.h"

class ngcc_power_cycle
{
private:
	int m_cycle_key;
	double m_delta_P, m_delta_q, m_delta_T;
	int m_N_P, m_N_q, m_N_T;
	double m_P_amb_start, m_q_sf_start, m_T_amb_start;
	double m_P_amb_end, m_q_sf_end, m_T_amb_end;
	double m_q_MW, m_T_amb_C, m_P_amb_bar;

	// Performance data 3D tables
	util::block_t<double> m_solar_steam_mass;
	util::block_t<double> m_solar_extraction_p;
	util::block_t<double> m_solar_injection_p;
	util::block_t<double> m_solar_injection_t;
	util::block_t<double> m_solar_extraction_t;
	util::block_t<double> m_solar_extraction_h;
	util::block_t<double> m_solar_injection_h;
	util::block_t<double> m_plant_power_net;
	util::block_t<double> m_solar_heat_max;
	util::block_t<double> m_plant_fuel_mass;
	
	// Cycle 1 calls
	void set_cycle_table_props();
	
	// Generic Cycle Calls
	double get_performance_results( util::block_t<double> * p_current_table );


public:
	ngcc_power_cycle(){};

	~ngcc_power_cycle(){};

	bool set_cycle_config( int cycle_key )
	{
		// cycle_key == 1: NREL's NGCC model
		// cycle_key == 2: GE's NGCC model

		if( cycle_key != E_nrel_hp_evap )		// Check that cycle key is valid
			return false;

		m_cycle_key = cycle_key;

		set_cycle_table_props();
		return true;

	}

	void get_table_range(double & T_amb_low, double & T_amb_high, double & P_amb_low, double & P_amb_high )
	{
		T_amb_low = m_T_amb_start + 0.001*fabs(m_delta_T);
		T_amb_high = m_T_amb_end - 0.001*fabs(m_delta_T);

		P_amb_low = m_P_amb_end + 0.001*fabs(m_delta_P);
		P_amb_high = m_P_amb_start - 0.001*fabs(m_delta_P);
	}

	/*
	//output gross power from engine
		double P_SE_out = (Beale_max_fit*(engine_pressure_fit*10.0e6 
			* m_V_displaced*frequency)*(1.0-pow(T_compression/T_heater_head_operate,0.5)))/1000.0;
			*/

	double get_ngcc_data( double q_MW, double T_amb_C, double P_amb_bar, int use_enum_data_descript )
	{
		m_q_MW = q_MW;     m_T_amb_C = T_amb_C;     m_P_amb_bar = P_amb_bar;

		switch( use_enum_data_descript )
		{
		case E_solar_steam_mass:
			return get_performance_results( &m_solar_steam_mass );
		case E_solar_extraction_p:
			return get_performance_results( &m_solar_extraction_p );
		case E_solar_injection_p:
			return get_performance_results( &m_solar_injection_p );
		case E_solar_injection_t:
			return get_performance_results( &m_solar_injection_t );
		case E_solar_extraction_t:
			return get_performance_results( &m_solar_extraction_t );
		case E_plant_power_net:
			return get_performance_results( &m_plant_power_net );
		case E_plant_fuel_mass:
			return get_performance_results( &m_plant_fuel_mass) ;
		case E_solar_heat_max:
			return get_performance_results( &m_solar_heat_max );
		default:
			return -999.9;
		}
	}

	enum data_descript
	{
		E_solar_steam_mass,
		E_solar_extraction_p,
		E_solar_injection_p,
		E_solar_injection_t,
		E_solar_extraction_t,
		E_solar_extraction_h,
		E_solar_injection_h,
		E_plant_power_net,
		E_plant_fuel_mass,
		E_solar_heat_max
	};

	enum iscc_cycle_config
	{
		E_nrel_hp_evap = 1,
		//E_ge	
	};

};


#endif
