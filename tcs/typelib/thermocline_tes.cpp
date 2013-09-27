#include "thermocline_tes.h"
//#include "sam_csp_util.h"
#include <algorithm>

using namespace std;

bool Thermocline_TES::Initialize_TC( double H_m, double A_m2, int Fill_in, double U_kJ_hrm2K, double Utop_kJ_hrm2K, double Ubot_kJ_hrm2K,
										double f_void, double capfac, double Thmin_C, double Tcmax_C, int nodes, double T_hot_init_C,
										double T_cold_init_C, double TC_break, double T_htr_set_C, double tank_max_heat_MW, int tank_pairs,
										HTFProperties & htf_fluid_props )
{
	htfProps = htf_fluid_props;	//[-] Set HTF property class to fluid property class from calling method

	m_num_TC_max = 10000;		//[-] Maximum number of timesteps that can be evaluated during 1 call	

	m_H = H_m;					//[m] Height of the rock bed storage tank  
	m_A = A_m2;					//[m^2] Cross-sectional area of storage tank
	double Fill = Fill_in;				//[-] Filler material number
	m_U = U_kJ_hrm2K;			//[kJ/hr-m2-K] loss coefficient   
		//gjk 4 new parameters
	m_U_top = Utop_kJ_hrm2K;	//[kJ/hr-m2-K] top surface conductance
	m_U_bot = Ubot_kJ_hrm2K;	//[kJ/hr-m2-K] bottom surface conductance
	m_void = f_void;			//[-] rock bed void fraction
		//gjk capfac to add thermal mass to bottom of tank
	m_capfac = capfac;			//[-] Bottom thermal mass capacitance factor multiplier
		//mjw add temperature control parameters
	m_Thmin = Thmin_C;				//[C] Minimum allowable hot side outlet temperature during discharge
	m_Th_avail_min = m_Thmin + 5.0;	//[C] Min allowable hot side outlet temp used for availability calcs
		
	m_Tcmax = Tcmax_C;				//[C] Maximum allowable cold side outlet temperature during charge
	m_Tc_avail_max = m_Tcmax - 5.0; //[C] Max allowable cold side outlet temp used for availability calcs
		
	m_nodes = nodes;					//[-] Number of nodes in thermocline

	m_T_hot_init = T_hot_init_C;		//[C] Initial TC hot temp
	m_T_cold_init = T_cold_init_C;		//[C] Initial TC cold temp
	m_TC_break = TC_break;				//[-] Fraction into tank where TC exists (0: entire tank is hot, 1: entire tank is cold)
	m_T_htr_set = T_htr_set_C;			//[C] Min allowable cold tank fluid temp before aux heater turns on
	m_tank_max_heat = tank_max_heat_MW; //[MW] Capacity of tank heater
	m_tank_pairs = tank_pairs;			//[-] Number of equivalent tank pairs

	int nodes_break = (int)( (1.0 - m_TC_break)*(m_nodes) ) - 1;

	m_T_hot_in_min = 0.9*m_T_hot_init + 0.1*m_T_cold_init;		//[C] Min allowable inlet charging temp
	m_T_cold_in_max = 0.1*m_T_hot_init + 0.9*m_T_cold_init;	//[C] Max allowable inlet discharging temp

	m_T_prev.resize(m_nodes,1);
	m_T_start.resize(m_nodes,1);
	m_T_middle.resize(m_nodes,1);
	m_T_end.resize(m_nodes,1);

	// Define initial thermocline array based on initial hot and cold temperatures and cold fraction
	if( nodes_break <= 0 )
		m_T_prev.fill( m_T_hot_init );
	else if( nodes_break >= m_nodes - 1 )
		m_T_prev.fill( m_T_cold_init );
	else
	{
		for( int i = 0; i < nodes_break; i++ )
			m_T_prev.at(i,0) = m_T_hot_init;
		for( int i = nodes_break; i < m_nodes; i++ )
			m_T_prev.at(i,0) = m_T_cold_init;
	}

	// Determine the average rockbed temperature
	m_T_ave_prev = 0.0;
	for( int i = 0; i < m_nodes; i++ )
		m_T_ave_prev += m_T_prev.at(i,0);
	m_T_ave_prev /= m_nodes;

	if( !fillProps.Set_TC_Material( Fill ) )
		return false;		// Invalid fill material number

	double T_prop = 0.5*(m_T_hot_init + m_T_cold_init);
	double cond_htf = htfProps.cond( T_prop + 273.15 );
	double cond_fill = fillProps.k_bed();
	m_cond = f_void*cond_htf + (1.0 - f_void)*cond_fill;	//[W/m-K]
	m_cp_a = htfProps.Cp( T_prop + 273.15 );					//[kJ/kg-K]
	m_rho_a = htfProps.dens( T_prop + 273.15, 1.0 );			//[kg/m^3]
	m_cp_r = fillProps.cp_bed();								//[kJ/kg-K]
	m_rho_r = fillProps.dens_bed();							//[kg/m^3]

	//mjw 4.26.11 Assume a cylindrical tank and calculate perimeter
	m_P = sqrt(m_A/CSP::pi)*2*CSP::pi;				//[m] Perimeter of cylindrical tank
	m_vol = m_A*m_H;								//[m^3] Volume of tank
	m_UA = m_U * m_P * m_H / m_nodes;				//[kJ/hr-K]->[kJ/hr-m2-K]*surface area of node[m^2] (P*H/XNODES)
	// gjk add UATOP and UABOT
	m_UA_top = m_U_top * m_A;                       //[kJ/hr-K]->[kJ/hr-m2-K]*top surface area[m^2] (A)
	m_UA_bot = m_U_bot * m_A;						//[kJ/hr-K]->[kJ/hr-m2-K]*bottom surface area[m^2] (A)

	m_ef_cond = m_cond*m_A*m_nodes/m_H;
	m_cap = m_vol*m_void*m_cp_a*m_rho_a + m_vol*(1.0-m_void)*m_cp_r*m_rho_r;	//[kJ/K] Tank
	m_e_tes = m_cap*(m_T_hot_init - m_T_cold_init);								//[kJ] Available energy in completely hot tank
	m_cap_node = m_cap / m_nodes;

	double tol_q = 0.01;		//[-]
	m_tol_TC = 0.5*0.5*tol_q*m_UA / (m_cond*m_A/(m_H/m_nodes));
	m_tol_TC = max(1.E-10, min( 0.001, m_tol_TC ) );

	return true;
}

bool Thermocline_TES::Solve_TC( double T_hot_in_C, double flow_h_kghr, double T_cold_in_C, double flow_c_kghr, double T_env_C, int mode_in,
		              double Q_dis_target_W, double Q_cha_target_W, double f_storage_in, double time_hr,
					  double & m_dis_avail_tot, double & T_dis_avail, double & m_ch_avail_tot, double & T_ch_avail )
{

	double T_hot = T_hot_in_C;					//[C] Temperature into the top of the tank (charging)
	double flow_h = flow_h_kghr/m_tank_pairs;	//[kg/hr] Flowrate into top (charging)
	double T_cold = T_cold_in_C;				//[C] Temperature into the bottom of the tank (discharging)
	double flow_c = flow_c_kghr/m_tank_pairs;	//[kg/hr] Flowrate into bottom (charging)
	double T_env = T_env_C;						//[C] Environmental temperature
	int mode = mode_in;							//[-] Flag for whether call is to check availability (=2 then only check availability)
	double Q_dis_target = Q_dis_target_W/m_tank_pairs;	//[W] Discharge rate required by controller
	double Q_cha_target = Q_dis_target_W/m_tank_pairs;  //[W] Charge rate required by controller
	double f_storage = f_storage_in;			//[-] Storage dispatch for timestep
	double delt = time_hr;

	// Reset beginning and middle TC node temperatures to previous timestep final values
	m_T_start = m_T_prev;
	m_T_middle = m_T_prev;
	m_T_end = m_T_prev;

	int I_flow = -1;
	bool know_mdot = false;
	double q_target = std::numeric_limits<double>::quiet_NaN();
	double m_dot = std::numeric_limits<double>::quiet_NaN();
	// If discharge thermal power rate is specified then
	if(Q_dis_target > 0.0)
	{
		I_flow = 2;				//[-] Set flow direction to discharge
		know_mdot = false;		//[-] Need to solve for mass flow
		q_target = Q_dis_target;	//[W] Target thermal power is discharge taraget
	}
	else if(Q_cha_target > 0.0)		//If charge thermal power rate is specified then
	{
		I_flow = 1;				//[-] Set flow direction to charge
		know_mdot = false;		//[-] Need to solve for mass flow
		q_target = Q_cha_target;	//[W] Target thermal power is charge target
	}
	else if(flow_c > 0.0)			//If discharge flow rate is specified then
	{
		I_flow = 2;				//[-] Set flow direction to discharge
		know_mdot = true;		//[-] Know mass flow rate
		m_dot = flow_c;			//[kg/hr] Set mass flow rate
	}
	else if(flow_h > 0.0)			//If charge flow rate is specified then
	{
		I_flow = 1;				//[-] Set flow direction to charge
		know_mdot = true;		//[-] Know mass flow rate
		m_dot = flow_h;			//[kg/hr] Set mass flow rate
	}
	else							// "Idle" mode
	{
		I_flow = 1;				//[-] Set flow direction to charge, but doesn't matter
		know_mdot = false;		//[-]
		m_dot = flow_h;			//[kg/hr] Set mass flow rate: should be 0
		q_target = 0.0;			//[W] Target thermal power is 0
	}


	//***********************************************************************************************************
	//mjw 4.25.11 Calulcate the available volumes for charge/discharge given the control temperature limitations
	//Updated 11/28/11, TN
	//   - The availability check should be completed before the performance simulation, so use StoreTransfer(1:Nodes)
	//***********************************************************************************************************
	
	int iclim = 0;
	int ihlim = 0;
	double Thtemp = 0.0;
	double Tctemp = 0.0;
	for( int k = 0; k < m_nodes; k++ )
	{
		// Find the last node where the HTF temperature is above the hot side limit
		if( m_T_start.at(k,0) > m_Th_avail_min )
		{
			ihlim = k;						//[-]
			Thtemp += m_T_start.at(k,0);	//[C]
		}
		// Find the last node where the HTF temperature is below the cold side limit
		if( m_T_start.at(m_nodes-1-k,0) < m_Tc_avail_max )
		{
			iclim = k;						//[-]
			Tctemp += m_T_start.at(m_nodes-1-k,0);
		}
	}

	double fhlim = (double) ihlim;
	double fclim = (double) iclim;

	// Average temperature of TC above limit (assumes uniform props)
	Thtemp /= max( fhlim+1, 1.0 );
	// Average temperature of TC below limit
	Tctemp /= max( fclim+1, 1.0 );
	
	// Calculate the fraction of the HTF that the hot filler can bring up to temperature
	fhlim = (fhlim+1)/m_nodes;
	fclim = (fclim+1)/m_nodes;

	int ChargeNodes = max( 0, (int)floor(f_storage*m_nodes) );

	double Qd_fill = std::numeric_limits<double>::quiet_NaN();
	double flc = std::numeric_limits<double>::quiet_NaN();
	// If cold inlet temperature is greater than the cold maximum, then no thermal energy is available for discharage
	if( T_cold > m_T_cold_in_max )
	{
		Qd_fill = 0.0;
		flow_c = 0.0;
		flc = m_cp_a*flow_c;		//[kJ/hr-K]
	}
	else	// Otherwise, estimate the maximum thermal energy available for discharge
	{
		Qd_fill = max( 0.0, m_vol*fhlim*m_rho_r*m_cp_r*(1.0-m_void)*max( Thtemp - T_cold, 0.0 ) ) + max( 0.0, m_vol*fhlim*m_void*m_rho_a*m_cp_a*max( Thtemp - T_cold, 0.0 ) );	//[kJ]
	}

	double Qc_fill = std::numeric_limits<double>::quiet_NaN();
	double flh = std::numeric_limits<double>::quiet_NaN();
	// If hot inlet temperature is less than hot minimum, then no thermal energy is available for charging
	if( T_hot < m_T_hot_in_min )
	{
		Qc_fill = 0.0;
		flow_h = 0.0;
		flh = m_cp_a*flow_h;
	}
	else    // Otherwise, estimate the maximum thermal energy available for charge
	{
		Qc_fill = max( 0.0, m_vol*fclim*m_rho_r*m_cp_r*(1.0-m_void)*max(T_hot-Tctemp,0.0)) + max( 0.0, m_vol*fclim*m_void*m_rho_a*m_cp_a*max(T_hot-Tctemp,0.0));	//[kJ]
	}

	double m_disch_avail = 0.0;
	// ******Discharging****************************************************************
	// Convert maximum discharge energy to thermal power over timestep
	Qd_fill = Qd_fill/(3.6*delt);					//[kJ]*[1/hr]*1000[J/kJ]*(1/3600)[hr/s]=>[W] 
	//Approximate mass flow available over hour
	m_disch_avail = Qd_fill / (m_cp_a*max(Thtemp - T_cold, 1.0))*3.6;      //[J/s]*[kg-K/kJ]*[1/K]*(1/1000)[J/kJ]*3600[s/hr]=>[kg/hr]
	// ******************************************************************************
	
	double m_charge_avail = 0.0;
	// ******Charging****************************************************************
	Qc_fill = Qc_fill/(3.6*delt);                  //[kJ]*[1/hr]*1000[J/kJ]*(1/3600)[hr/s]=>[W]
	m_charge_avail = Qc_fill / (m_cp_a*max( T_hot - Tctemp, 1.0 ))*3.6;    //[J/s]*[kg-K/kJ]*[1/K]*(1/1000)[J/kJ]*3600[s/hr]=>[kg/hr]
	// ******************************************************************************
	
	// If call to subroutine is only interested in availability, then set outputs and exit
	if( mode == 2 )
	{
		m_dis_avail_tot = m_disch_avail * m_tank_pairs;
		T_dis_avail = m_T_start.at(0,0);
		m_ch_avail_tot = m_charge_avail * m_tank_pairs;
		T_ch_avail = m_T_start.at(m_nodes-1,0);
		return true;
	}
	
	// If mass flow rate is known, there is still the possibility of that it will over-(discharge) the thermocline,
	//     so we need to establish a range of possible mass flow rates for the solver
	double m_dot_lower = std::numeric_limits<double>::quiet_NaN();
	double m_dot_upper = std::numeric_limits<double>::quiet_NaN();
	if( know_mdot )
	{
		m_dot_lower = 0.0;
		m_dot_upper = m_dot;
	}
	else if( I_flow == 2 )
		m_dot = min( Q_dis_target, Qd_fill ) / (m_cp_a*max( Thtemp - T_cold, 1.0 ))*3.6;		//[kg/hr]
	else if( I_flow == 3 )
		m_dot = min( Q_cha_target, Qc_fill ) / (m_cp_a*max( T_hot - Tctemp, 1.0 ))*3.6;			//[kg/hr]

	
	// Set bounds a bit past realistic bounds so if the correct mass flow rate is at the bounds, iteration gets there faster
	m_dot_upper = 1.5*m_dot;
	m_dot_lower = 0.5*m_dot;
	
	// Keep track of initial upper and lower mass flow rate estimates
	double m_dot_low0 = m_dot_lower;
	double m_dot_up0 = m_dot_upper;
	
	double diff_q_target = 999.0; //[-] Set difference greater than tolerance
	int q_iter = 0;               //[-] Iteration counter on achieving target (dis)charge energy
	int TC_limit = 0;             //[-] Flag signaling whether current mass flow rate has over-(dis)charged thermocline
	bool upflag = false;          //[-] Flag signaling that upper limit on mass flow has been found with a corresponding 'diff_q_target'
	bool lowflag = false;         //[-] Flag signaling that lower limit on mass flow has been found with a corresponding 'diff_q_target'
	bool mdot_iter = false;       //[-] Flag signaling that allow a mass flow rate was specified, it must be iterated on (when = true)
	
	double q_tol = 0.01;          //[-] Relative tolerance for energy rate in/out
	double t_tol = 3.0;			  //[C] Absolute tolerance for packed bed filling/depletion limits

	double y_upper = std::numeric_limits<double>::quiet_NaN();
	double y_lower = std::numeric_limits<double>::quiet_NaN();

	while( (abs(diff_q_target)>q_tol || TC_limit != 0) && q_iter < 40 )
	{
		q_iter++;		//[-] Increase iteration counter
		bool full = true;	//[-] Reset target energy flag

		// After first run, begin to iterate on mass flow rate
		if( q_iter > 1 )
		{
			if(TC_limit==1)
			{
				m_dot_upper = m_dot;
				upflag = false;
				m_dot = 0.5*m_dot_upper + 0.5*m_dot_lower;
			}
			else if(TC_limit==2)
			{
				m_dot_lower = m_dot;
				lowflag = false;
				m_dot = 0.5*m_dot_upper + 0.5*m_dot_lower;
			}
			else if( upflag && lowflag )
			{
				if(diff_q_target < q_tol)
				{
					m_dot_upper = m_dot;
					y_upper = diff_q_target;
				}
				else
				{
					m_dot_lower = m_dot;
					y_lower = diff_q_target;
				}
				m_dot = y_upper/(y_upper - y_lower)*(m_dot_lower - m_dot_upper) + m_dot_upper;
			}
			else
			{
				if(diff_q_target > q_tol)
				{
					m_dot_upper = m_dot;
					upflag = true;
					y_upper = diff_q_target;
				}
				else if(diff_q_target < q_tol)
				{
					m_dot_lower = m_dot;
					lowflag = true;
					y_lower = diff_q_target;
				}
				if( upflag && lowflag )
					m_dot = y_upper/(y_upper - y_lower)*(m_dot_lower - m_dot_upper) + m_dot_upper;
				else
					m_dot = 0.5*m_dot_upper + 0.5*m_dot_lower;
			}
		}

		// If iterated m_dot approaches lower bound, adjust lower bound to zero
		if( (m_dot - m_dot_low0)/m_dot_low0 < 0.0005 )
		{
			m_dot_low0 = 0.0;
			m_dot_lower = m_dot_low0;
			m_dot = 0.5*m_dot_upper + 0.5*m_dot_lower;
			lowflag = false;
		}

		// If iterated m_dot approaches upper bound, increase upper bound
		if( (m_dot_up0 - m_dot)/m_dot_up0 < 0.0005 )
		{
			m_dot_up0 = 2.0*m_dot_up0;
			m_dot_upper = m_dot_up0;
			m_dot = 0.5*m_dot_upper + 0.5*m_dot_lower;
			upflag = true;
		}

		if( I_flow == 1 )
			flow_h = m_dot;			//[kg/hr]
		else if( I_flow == 2 )
			flow_c = m_dot;			//[kg/hr]

		// Compute capacitance flow rates
		flh = m_cp_a*flow_h;		//[kJ/hr-K]
		flc = m_cp_a*flow_c;		//[kJ/hr-K]

		// Calculate time constant: This value will change every mass flow rate iteration
		double tau = (0.632)*(m_cap/m_nodes) / max(flh,flc);		//[kJ/kg]*[hr-K/kJ] => hr
		double TC_timestep = tau;		//[hr]

		// If calculated timestep is less than calling method timestep, adjust it so tht there is a whole number of TC timesteps
	}



/*
DO WHILE( ((abs(diff_q_target)>q_tol).or.(TC_limit/=0)) .and. (q_iter < 40) )

    !If calculated timestep is less than TRNSYS timestep, adjust it so that there is a whole number of TC timesteps every TRNSYS timestep
    IF(TC_timestep < DELT)THEN
        !Okay, so if the simulation timestep is not divisible by the thermocline timestep then we need to adjust the TC timestep so it is.  Lower TC timestep to accomplish this
        IF( (DELT - INT(DELT/TC_timestep)*TC_timestep) /= 0.d0 )THEN
        !IF( mod(DELT,TC_timestep) /= 0)THEN
            num_TC = jfix(DELT/TC_timestep) + 1     ![-] Number of thermocline timesteps per simulation timestep
        ELSE
            num_TC = DELT/TC_timestep               ![-]
        ENDIF

        num_TC = min(num_TC_max, num_TC)
        TC_timestep = DELT / dble(num_TC)           ![hr] Corrected timestep (such that the number of TC timesteps in a sim timesteps is an integer) at which to evaluate thermocline 
        
    ELSE        !If calculated timestep is greater than TRNSYS timestep - Maximum TC timestep is the simulation timestep

        num_TC = 1                  ![-] 
        TC_timestep = DELT          ![hr]
        
    ENDIF

    !Reset beginning, average, and end arrays to initial values
    StoreTransfer(1:Nodes) = StoreTransfer0(:)
    StoreTransfer(Nodes+1:2*Nodes) = StoreTransfer0(:)
    StoreTransfer(2*Nodes+1:3*Nodes) = StoreTransfer0(:)
    
    T_ts_ave(:) = 0.d0          ![C] Array storing average packed bed temperature at each timestep
    Q_losses(:) = 0.d0          ![kJ/hr] Array storing summed rate of heat loss at each timestep
    Q_htr(:)    = 0.d0          ![kJ] Array storing summed (nodal) thermal energy required by heater

    !Okay, add outer loop for number of thermocline timesteps in a simulation timestep
    DO tcn = 1,num_TC

        !Set convergence parameters for current run of packed bed simulation
        iter = 0                ![-]
        max_T_diff = 999.d0     ![-]
    
        ! SET COEFFICIENTS FOR ANALYTICAL SOLUTIONS OF INDIVIDUAL NODES
        !0.0005d0
        DO WHILE((iter < 30).and.(max_T_diff > tol_TC))

            iter = iter + 1     ![-] Increase iteration counter
            
            T_ts_ave(tcn) = 0.d0          ![C] Array storing average packed bed temperature at each timestep
            Q_losses(tcn) = 0.d0          ![kJ/hr] Array storing summed rate of heat loss at each timestep
            
            !This is only here for debugging........
            !IF(iter == 30) THEN
            !    continue
            !ENDIF

            !Orginal Type504 code to step through thermocline nodes           
            DO J = 1,NODES
                GO TO (100,150) ,IFLOW

                ! FLOW IS DOWNWARD
                100 continue
                K = J
                IF(K > 1) GO TO 110

                !TOP NODE
                ! gjk ADD UATOP TO AA and BB
                AA = -(FLH+UA+UATOP+EFCOND)/CAPN
                BB = (FLH*THOT+(UA+UATOP)*TENV+EFCOND*StoreTransfer(2*NODES+2))/CAPN
                UA_HL = UA + UATOP      !3/6/12, TN: Keep UA term at every node so losses can be calculated once T_avg is known
                GO TO 200
                110 continue
                IF(K < NODES) GO TO 120

                !BOTTOM NODE
                ! gjk ADD UABOT TO AA and BB
                ! increase capacitance of bottom node BY CAPFAC
                AA = -(FLH+UA+UABOT+EFCOND)/(CAPN*CAPFAC)
                BB = (FLH*StoreTransfer(3*NODES-1)+(UA+UABOT)*TENV+EFCOND*StoreTransfer(3*NODES-1))/(CAPN*CAPFAC)
                UA_HL = UA + UABOT      !3/6/12, TN: Keep UA term at every node so losses can be calculated once T_avg is known
                GO TO 200

                !MIDDLE NODES
                120 continue
                AA = -(FLH+UA+2.d0*EFCOND)/CAPN
                BB = (FLH*StoreTransfer(2*NODES+K-1)+UA*TENV+EFCOND*(StoreTransfer(2*NODES+K-1)+StoreTransfer(2*NODES+K+1)))/CAPN
                UA_HL = UA                 !3/6/12, TN: Keep UA term at every node so losses can be calculated once T_avg is known
                GO TO 200

                !FLOW IS UPWARD
                150 continue
                K = NODES - J + 1
                IF(K < NODES) GO TO 160

                !BOTTOM NODE
                ! gjk ADD UABOT TO AA and BB
                ! increase capacitance of bottom node BY CAPFAC
                AA = -(FLC+UA+UABOT+EFCOND)/(CAPN*CAPFAC)
                BB = (FLC*TCOLD+(UA+UABOT)*TENV+EFCOND*StoreTransfer(3*NODES-1))/(CAPN*CAPFAC)
                UA_HL = UA + UABOT          !3/6/12, TN: Keep UA term at every node so losses can be calculated once T_avg is known
                GO TO 200
                160 continue
                IF(K > 1) GO TO 170

                !TOP NODE
                ! gjk add UATOP TO AA and BB
                AA = -(FLC+UA+UATOP+EFCOND)/CAPN
                BB =  (FLC*StoreTransfer(2*NODES+2)+(UA+UATOP)*TENV+EFCOND*StoreTransfer(2*NODES+2))/CAPN
                UA_HL = UA + UATOP      !3/6/12, TN: Keep UA term at every node so losses can be calculated once T_avg is known
                GO TO 200

                !MIDDLE NODES
                170 continue
                AA = -(FLC+UA+2.d0*EFCOND)/CAPN
                BB = (FLC*StoreTransfer(2*NODES+K+1)+UA*TENV+EFCOND*(StoreTransfer(2*NODES+K+1)+StoreTransfer(2*NODES+K-1)))/CAPN
                UA_HL = UA              !3/6/12, TN: Keep UA term at every node so losses can be calculated once T_avg is known

                !ANALYTICAL SOLUTION
                200 continue
                TI = StoreTransfer(K)
                CALL DIFF_EQ_SUBTS(TIME,TC_timestep,AA,BB,TI,TF,TBAR)
                
                !If final node temperature is below cold limit, then apply heater
                IF(TF < T_htr_set)THEN
                    Q_htr_max = tank_max_heat*1000.d0*TC_timestep*3600.d0   ![MW]*1000(kJ/MJ)*dt(hr)*3600(s/hr)=>kg  Maximum heat rate of tank heater
                    
                    !IF heat has capacity to increase node temperature to setpoint
                    IF( (Q_htr(tcn) + (CAPN*(T_htr_set - TF))) < tank_max_heat )THEN
                        Q_htr(tcn) = (Q_htr(tcn) + (CAPN*(T_htr_set - TF)))   ![kJ]   Thermal energy required by heater to maintain cold limit in tank
                        TF = T_htr_set  ![C] Node hits setpoint
                    
                    !If heater does not have capacity
                    ELSE
                        !Q_htr_node = tank_max_heat - Q_htr(tcn)
                        !CAPN*(TF_new - TF) = Q_htr_node
                        !TF_new = Q_htr_node / CAPN + TF
                        
                        TF = (tank_max_heat - Q_htr(tcn))/CAPN + TF     ![C] Node does not hit setpoint
                    ENDIF
                ENDIF
                
                StoreTransfer(NODES+K) = TF
                !250 continue
                Diff_T_ave(K) = abs(StoreTransfer(2*NODES+K) - TBAR)    ![C] Difference between old average node temp and new average node temp
                StoreTransfer(2*NODES+K) = TBAR                         ![C] Update guess on average node temp now that difference is calculated
                T_ts_ave(tcn) = T_ts_ave(tcn) + TBAR                    ![C] Add average node temps
                Q_losses(tcn) = Q_losses(tcn) + UA_HL * (TBAR - TENV)   ![kJ/hr-K]*[K] -> [kJ/hr] Heat loss
            ENDDO       !End step through thermocline

            T_ts_ave(tcn) = T_ts_ave(tcn)/dble(Nodes)                   ![C] Spatial average of nodal time averaged temps
            max_T_diff = maxval(Diff_T_ave)/290.d0                      ![-] Max nodal relative temperature difference

        ENDDO       !End iteration on average (time) nodal temperatures
        
        T_cout_ave(tcn) = StoreTransfer(3*Nodes)    ![C] Store the average cold outlet temperature of each sub timestep
        T_hout_ave(tcn) = StoreTransfer(2*Nodes+1)  ![C] Store the average hot outlet temperature of each sub timestep
        
        !Check here for over(dis)charge?  Could save time when solving for correct mass flow rate
        
        !After timestep has solved, set initial nodes of next timestep to end nodes of current timestep
        StoreTransfer(1:NODES) = StoreTransfer(NODES+1:2*NODES)
        
    ENDDO

    ! DETERMINE AVERAGE ROCKBED TEMPERATURES AND AVERAGE DERIVATIVES
    TFBAR = 0.d0
    DO K = 1,NODES
        TFBAR = TFBAR + StoreTransfer(NODES+K)  
    ENDDO
    TFBAR = TFBAR/DBLE(NODES)       ![C] Spatial average of temps at end of final timestep

    !TN: Average rockbed temp is now the average (sub timesteps in simulation) of averages (nodal in each sub timestep) of averages (time based in each sub timestep)
    TAVG = 0.d0
    DO tcn=1,num_TC
        TAVG = TAVG + T_ts_ave(tcn)
    ENDDO
    TAVG = TAVG/dble(num_TC)    ![C] Time-average, spatial averaged nodal temperature (how useful is this?)
    !********************************************************************************

    !TN: Outlet temperature to pass should be average (sub timesteps in simulation) of averages (time based in each sub timestep)
    T_disch_avail = 0.d0
    T_charge_avail = 0.d0
    DO tcn=1,num_TC
        T_disch_avail = T_disch_avail + T_hout_ave(tcn)
        T_charge_avail = T_charge_avail + T_cout_ave(tcn)
    ENDDO
    T_disch_avail = T_disch_avail/dble(num_TC)      ![C] Time-averaged discharging OUTLET temperature (this is a CONFUSING name)
    T_charge_avail = T_charge_avail/dble(num_TC)    ![C] Time-averaged charging OUTLET temperature (this is a CONFUSING name)

    !Check if tank has been over-(dis)charged with current mass flow rate
    IF(IFLOW==1)THEN        !Charging, so look for min cold temp hotter than allowed
        diff_Tcmax = StoreTransfer(2*Nodes) - Tcmax

    ELSEIF(IFLOW==2)THEN    !Discharging, so look for max hot temp cooler than allowed
        !diff_Thmin = StoreTransfer(Nodes+1) - Thmin
        !4/23/12, TN: Account for storage discharge fraction
        diff_Thmin = StoreTransfer(Nodes+ChargeNodes) - Thmin

    ENDIF
       
    TC_limit = 0    ![-] Reset Flag
    
    !If solving thermocline for specified mass flow rate:
    IF(know_mdot)THEN
        full = .false.      !Code did not result in target energy (dis)charge because in different solving mode
        
        !Charging, specified mass flow rate
        IF(IFLOW==1)THEN
            q_calc  = m_dot*CPA*(THOT - T_charge_avail)/3.6d0     ![W] Calculated rate of energy charge            
            
            !If specified mass flow caused over-charging and thus iteration:
            IF(mdot_iter)THEN                   
                
                !If within tolerance on max cold temperature then get out
                IF( (diff_Tcmax > -t_tol).and.(diff_Tcmax <= 0.d0) )THEN           
                    !Set convergence criteria to 0 to get out
                    diff_q_target = 0.d0
                    TC_limit = 0
                
                !If min cold temp is greater than allowed then:
                ELSEIF(diff_Tcmax > 0)THEN  
                    TC_limit = 1    ![-] Flag noting that upper limit on mass flow should be set
                
                !Else, if tank is not "close enough" to being fully charged:
                ELSE                        
                    TC_limit = 2    ![-] Flag noting that lower limit on mass flow should be set
                ENDIF
            
            !If first iteration for a specified mass flow rate
            ELSE    
                
                !If tank does NOT overcharge, then get out
                IF(diff_Tcmax <= 0.d0)THEN
                    !Set convergence criteria to 0 to get out  
                    diff_q_target = 0.d0
                    TC_limit = 0
                    
                !Else, know that mass flow rate is too high    
                ELSE    
                    mdot_iter = .true.  ![-] Flag noting that correct mass flow rate must be found via iteration
                    TC_limit = 1        ![-] Flag noting that upper limit on mass flow should be set
                ENDIF
            ENDIF                    
        
        !Discharging, specified mass flow rate
        ELSE
            q_calc  = m_dot*CPA*(T_disch_avail - TCOLD)/3.6d0     ![W]: Calculated rate of energy discharge
            
            !If specified mass flow caused over-charging and thus iteration:
            IF(mdot_iter)THEN   !If specified mass flow caused over-discharging:
            
                !If within tolerance on min hot temperature then get out
                IF( (diff_Thmin >= 0.d0).and.(diff_Thmin < t_tol) )THEN 
                    !Set convergence criteria to 0 to get out
                    diff_q_target = 0.d0
                    TC_limit = 0.d0
                    
                !If max hot temp is less than allowed then:
                ELSEIF(diff_Thmin < 0)THEN  
                    TC_limit = 1    ![-] Flag noting that upper limit on mass flow should be set
                    
                !Else, if tank is not "close enough" to being fully discharged:    
                ELSE                        
                    TC_limit = 2    ![-] Flag noting that lower limit on mass flow should be set
                ENDIF
                
            !If first iteration for a specified mass flow rate    
            ELSE   
            
                !If tank does NOT over-discharge, then get out
                IF(diff_Thmin >= 0.d0)THEN
                    !Set convergence criteria to 0 to get out
                    diff_q_target = 0.d0
                    TC_limit = 0
                    
                !Else, know that mass flow rate is too high    
                ELSE    
                    mdot_iter = .true.  ![-] Flag noting that correct mass flow rate must be found via iteration
                    TC_limit = 1        ![-] Flag noting that upper limit on mass flow should be set
                ENDIF
            ENDIF
        ENDIF
    
    !Only calling model to calculate new thermocline (dwelling, so still need to calculate losses and conduction), get out
    ELSEIF(q_target==0.d0)THEN
        !Set convergence criteria to 0 to get out  
        diff_q_target = 0.d0
        TC_limit = 0
    
    !Solving thermocline for specified charging energy
    ELSEIF(IFLOW==1)THEN
        q_calc  = m_dot*CPA*(THOT - T_charge_avail)/3.6d0     ![kg/hr]*[kJ/kg-K]*[K]*(1000)[J/kJ]*(1/3600)[hr/s]->[W] Calculated rate of energy charge        
        
        !If not removing enough energy and "close enough (but not going over)" to completely filling tank, then get out
        IF((q_calc < q_target).and.(diff_Tcmax > -t_tol).and.(diff_Tcmax <= 0.d0))THEN       
            !Set convergence criteria to 0 to get out
            diff_q_target = 0.d0        ![W]
            TC_limit = 0                ![-]
            full = .false.              ![-] Note that code did not result in target energy rate
                
        ELSEIF(diff_TCmax > 0.d0)THEN    ! Over-charging tank, flag to indicate mass flow is too high
            TC_limit = 1
            
        ELSE        ! Not over-charging tank -> compare calculated to target        
            diff_q_target = (q_calc - q_target)/q_target       ![W] Relative difference between calculated and required rate of energy
        ENDIF            
    
    !Solving thermocline for specified discharging energy
    ELSE
        q_calc  = m_dot*CPA*(T_disch_avail - TCOLD)/3.6d0     ![kg/hr]*[kJ/kg-K]*K*[W/kW][hr/s]=>[W]: Calculated rate of energy discharge
        
        !If not getting enough energy and "close enough (but not going over)" to depleting tank, then get out
        IF((q_calc < q_target).and.(diff_Thmin < t_tol).and.(diff_Thmin >= 0.d0))THEN
            !Set convergence criteria to 0 to get out
            diff_q_target = 0.d0        ![W]
            TC_limit = 0                ![-] 
            full = .false.              ![-] Note that code did not result in target energy rate
        
        ELSEIF(diff_Thmin < 0.d0)THEN   !Over-discharging tank, flag to indicate mass flow is too high
            TC_limit = 1  
                     
        ELSE        ! Not over-discharging tank -> compare calculated to target
            diff_q_target = (q_calc - q_target)/q_target       ![W] Relative difference between calculated and required rate of energy
        ENDIF                    
    ENDIF  
    
ENDDO       !End of iteration on mass flow rate */


	return true;
}
