SUBROUTINE TYPE234 (TIME,XIN,OUT,T,DTDT,PAR,INFO,ICNTRL,*) 
!************************************************************************
! Object: Solar Power Tower Rankine Component - Direct steam w/ reheat
! Simulation Studio Model: TYPE234
 
! Author: Michael J. Wagner
! Editor: -
! Date:	 August 18, 2011
! Last modified: September 30, 2011
! COPYRIGHT 2011 NATIONAL RENEWABLE ENERGY LABORATORY

! Doc. tables updated 2011-09-06 - MJW
!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Parameters
!    1| P_ref                            | Reference output electric power at design condition               | MW               | kW               
!    2| eta_ref                          | Reference conversion efficiency at design condition               | none             | none             
!    3| T_hot_ref                        | Reference HTF inlet temperature at design                         | C                | C                
!    4| T_cold_ref                       | Reference HTF outlet temperature at design                        | C                | C                
!    5| dT_cw_ref                        | Reference condenser cooling water inlet/outlet T diff             | C                | C                
!    6| T_amb_des                        | Reference ambient temperature at design point                     | C                | C                
!    7| q_sby_frac                       | Fraction of thermal power required for standby mode               | none             | none             
!    8| P_boil_ref                       | Boiler operating pressure at design                               | bar              | bar              
!    9| is_rh                            | flag for indicating whether reheat is used                        | none             | none             
!   10| P_rh_ref                         | Reheater operating pressure at design                             | bar              | bar              
!   11| T_rh_hot_ref                     | Reheater design outlet temperature                                | C                | C                
!   12| rh_frac_ref                      | Reheater flow fraction at design                                  | none             | none             
!   13| is_sh                            | Flag for indicating whether superheat is used                     | none             | none             
!   14| CT                               | Flag for using dry cooling or wet cooling system                  | none             | none             
!   15| startup_time                     | Time needed for power block startup                               | hr               | hr               
!   16| startup_frac                     | Fraction of design thermal power needed for startup               | none             | none             
!   17| tech_type                        | Flag indicating which coef. set to use. (1=tower..2=trough..3=user)| none             | none             
!   18| T_approach                       | Cooling tower approach temperature                                | C                | C                
!   19| T_ITD_des                        | ITD at design for dry system                                      | C                | C                
!   20| P_cond_ratio                     | Condenser pressure ratio                                          | none             | none             
!   21| pb_bd_frac                       | Power block blowdown steam fraction                               | none             | none             
!   22| LU_pb                            | Logical unit for the power block file                             | none             | none             
!   23| P_cond_min                       | Minimum condenser pressure                                        | inHg             | Pa               
!   24| n_pl_inc                         | Number of part-load increments for the heat rejection system      | none             | none             
!   25| F_wc                             | Fraction indicating wet cooling use for hybrid system             | none             | none             

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Inputs
!    1| mode                             | Cycle part load control.. from plant controller                   | none             | none             
!    2| T_hot                            | Hot steam inlet temperature.. from tower                          | C                | C                
!    3| m_dot_st                         | Steam mass flow rate                                              | kg/hr            | kg/hr            
!    4| T_wb                             | Ambient wet bulb temperature                                      | C                | K                
!    5| demand_var                       | Variable used to indicate operating level of the PB (mass flow or power)| none             | none             
!    6| standby_control                  | Control signal indicating standby mode                            | none             | none             
!    7| T_db                             | Ambient dry bulb temperature                                      | C                | K                
!    8| P_amb                            | Ambient pressure                                                  | atm              | Pa               
!    9| TOU                              | Current Time-of-use period                                        | none             | none             
!   10| rh                               | Relative humidity of the ambient air                              | none             | none             
!   11| f_restart                        | Fraction of the hour that the turbine can operate during restart  | none             | none             
!   12| dP_sh                            | Superheater pressure drop                                         | Pa               | Bar              
!   13| dP_rh                            | Reheater pressure drop                                            | Pa               | Bar              

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Outputs
!    1| P_cycle                          | Cycle power output                                                | MWe              | kWe              
!    2| eta                              | Cycle thermal efficiency                                          | none             | none             
!    3| T_htf_cold                       | Heat transfer fluid outlet temperature                            | C                | C                
!    4| m_dot_makeup                     | Cooling water makeup flow rate                                    | kg/hr            | kg/s             
!    5| m_dot_demand                     | HTF required flow rate to meet power load                         | kg/hr            | kg/hr            
!    6| m_dot_st                         | Actual HTF flow rate passing through the power cycle              | kg/hr            | kg/hr            
!    7| m_dot_ref                        | Calculated reference HTF flow rate at design                      | kg/hr            | kg/hr            
!    8| W_cool_par                       | Cooling system parasitic load                                     | MWe              | MWe              
!    9| P_ref                            | Reference power level output at design (mirror param)             | MWe              | kWe              
!   10| f_bays                           | Fraction of operating heat rejection bays                         | none             | none             
!   11| P_cond                           | Condenser pressure                                                | Pa               | Pa               
!   12| P_turb_in                        | Turbine inlet pressure during operation                           | bar              | bar              
!   13| m_dot_rh                         | Mass flow rate through the reheater                               | kg/hr            | kg/hr            
!   14| P_rh_in                          | Steam pressure entering the reheater                              | bar              | bar              
!   15| T_rh_in                          | Reheater steam inlet temperature                                  | C                | C                
!   16| T_rh_out                         | Reheater steam requested outlet temperature                       | C                | C                
!   17| x_rh_in                          | Reheater inlet quality                                            | none             | none             


!************************************************************************

!    TRNSYS acess functions (allow to acess TIME etc.) 
USE TrnsysConstants
USE TrnsysFunctions
use global_props  !user global module
use water_properties

!REQUIRED BY THE MULTI-DLL VERSION OF TRNSYS
!DEC$ATTRIBUTES DLLEXPORT :: TYPE234				!SET THE CORRECT TYPE NUMBER HERE

!-----------------------------------------------------------------------------------------------------------------------
implicit none !real(8) (a-z) 

!     Parameters
real(8):: P_ref, eta_ref,  T_hot_ref, HTF, T_cold_ref, dT_cw_ref, q_sby_frac, Is_dry_bulb, &
          startup_time, startup_frac, T_amb_des, T_approach, T_ITD_des, P_cond_ratio, CT, P_cond_min, n_pl_inc, F_wc(9)

!    Inputs
real(8):: mode, T_hot, T_wb, T_db, m_dot_cw, demand_var, m_dot_st,standby_control, last_P_cycle, P_amb
 
!    Outputs / Local variables
real(8):: P_cycle, eta, T_cold, m_dot_demand,m_dot_ref, q_ND_tot, &
          q_sby_needed, m_dot_sby, q_tot, specheat, T_cw_in, last_standby_control, tstep, startup_remain ,&
          tech_type, W_cool_par, m_dot_makeup, F_wcmin, F_wcmax, P_cond, startup_energy, startup_e_remain, &
          startup_e_used, Q_cycle, f_st, rh, p_boil_ref, p_rh_ref, t_rh_hot_ref, rh_frac_ref, &
          P_turb_in, m_dot_rh, P_rh_in, T_rh_in, T_rh_out, dp_SH, dp_RH, f_recSU, P_max, P_check, cyclemap_dsg
logical::is_rh, is_sh

real(8):: xin, out, time, par, stored, T, dTdt, prop(7), pb_bd_frac, h_st_hot, h_st_cold, dh_steam, m_dot_st_bd, f_hrsys, fcall
integer(4)::INFO(15), np, ni, nout, nd, npar, nin, nder, iunit, itype, icntrl, nstored, ierr, TOU, i
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!SET THE MAXIMUM NUMBER OF PARAMETERS (NP), INPUTS (NI),OUTPUTS (NOUT), AND DERIVATIVES (ND) 
PARAMETER (NP=33,NI=13,NOUT=16,ND=0,NSTORED=5)        

!REQUIRED TRNSYS DIMENSIONS
DIMENSION XIN(NI),OUT(NOUT),PAR(NP), STORED(NSTORED),T(ND),DTDT(ND)
INTEGER NITEMS
logical called
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!SET THE VERSION INFORMATION FOR TRNSYS
IF(INFO(7).EQ.-2) THEN
    INFO(12)=16
    RETURN 1
ENDIF
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!DO ALL THE VERY LAST CALL OF THE SIMULATION MANIPULATIONS HERE
IF (INFO(8).EQ.-1) THEN
    if(allocated(db)) deallocate(db)  !deallocate the power cycle coef. array
    if(allocated(datx)) deallocate(datx, daty) !Deallocate the data arrays from the interp function
    fcall = CycleMap_DSG(-1,0,0,0.d0) !cleanup call on the cyclemap function
    RETURN 1
ENDIF
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!'AFTER-ITERATION' MANIPULATIONS 
IF (INFO(13).GT.0) THEN
    STORED(1)=standby_control
    STORED(2)=dmax1(startup_remain-tstep,0.d0)
    STORED(3)=P_cycle
    STORED(4)= startup_e_remain 
    STORED(5)=fcall

    CALL setStorageVars(STORED,NSTORED,INFO)
    
    !MJW 9.8.2010 :: Call the property range check subroutine with the inlet and outlet HTF temps to make sure they're in the valid range
    if(standby_control /= 1.) T_hot = T_hot + 273.15d0  !mjw 3.10.11 If the main cycle subroutine isn't called, adjust the temperature for the unit check
    
    RETURN 1
ENDIF
!
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!    DO ALL THE VERY FIRST CALL OF THE SIMULATION MANIPULATIONS HERE
IF (INFO(7).EQ.-1) THEN
tstep=getSimulationTimeStep()

    !-----------------------------------------------------------------------------------------------------------------------
    !Read in parameters
    P_ref = par(1)*1000.000000 		![MW] Reference output electric power at design condition
    eta_ref = par(2) 		![none] Reference conversion efficiency at design condition
    T_hot_ref = par(3) 		![C] Reference inlet temperature at design
    T_cold_ref = par(4) 		![C] Reference outlet temperature at design
    dT_cw_ref = par(5) 		![C] Reference condenser cooling water inlet/outlet T diff
    T_amb_des = par(6) 		![C] Reference ambient temperature at design point
    q_sby_frac = par(7) 		![none] Fraction of thermal power required for standby mode
    P_boil_ref = par(8) 		![bar] Boiler operating pressure at design
    is_rh = .false.
    if(par(9)==1.d0) is_rh = .true. 		![none] flag for indicating whether reheat is used
    P_rh_ref = par(10) 		![bar] Reheater operating pressure at design
    T_rh_hot_ref = par(11) 		![C] Reheater design outlet temperature
    rh_frac_ref = par(12) 		![none] Reheater flow fraction at design
    is_sh = .false.
    if(par(13)==1.d0) is_sh = .true. 		![none] Flag for indicating whether superheat is used
    CT = par(14) 		![none] Flag for using dry cooling or wet cooling system               
    startup_time = par(15) 		![hr] Time needed for power block startup
    startup_frac = par(16) 		![none] Fraction of design thermal power needed for startup
    tech_type = par(17) 		![none] Flag indicating which coef. set to use. (1=tower..2=trough..3=user)
    T_approach = par(18) 		![C] Cooling tower approach temperature
    T_ITD_des = par(19) 		![C] ITD at design for dry system
    P_cond_ratio = par(20) 		![none] Condenser pressure ratio
    pb_bd_frac = par(21) 		![none] Power block blowdown steam fraction 
    LU_pb = par(22) 		![none] Logical unit for the power block file
    P_cond_min = par(23)*3386.388667 		![inHg] Minimum condenser pressure
    n_pl_inc = par(24) 		![none] Number of part-load increments for the heat rejection system
    do i=1,9
	    F_wc(i) = dmin1(1.d0,dmax1(PAR(24+i),0.d0)) 		![none] Fraction indicating wet cooling use for hybrid system
    enddo
    F_wcmin = minval(F_wc)
    F_wcmax = maxval(F_wc)

    !SET SOME INFO ARRAY VARIABLES TO TELL THE TRNSYS ENGINE HOW THIS TYPE IS TO WORK
    INFO(6)=NOUT				
    INFO(9)=1				
    INFO(10)=0	!STORAGE FOR VERSION 16 HAS BEEN CHANGED				

    !SET THE REQUIRED NUMBER OF INPUTS, PARAMETERS AND DERIVATIVES THAT THE USER SHOULD SUPPLY IN THE INPUT FILE
    NIN=NI
    NPAR=NP
    NDER=ND

    !CALL THE TYPE CHECK SUBROUTINE TO COMPARE WHAT THIS COMPONENT REQUIRES TO WHAT IS SUPPLIED IN 
    !THE TRNSYS INPUT FILE
    CALL TYPECK(1,INFO,NIN,NPAR,NDER)

    !SET THE NUMBER OF STORAGE SPOTS NEEDED FOR THIS COMPONENT
    CALL setStorageSize(NSTORED,INFO)
    
    !MJW 9.1.2010 :: Initialize the first-call logical variable
    fcall = 1.  !1=true, 0=false
    P_max = 190. ![bar]
    !Set initial stored values
    STORED(1)=0.d0
    STORED(2)=startup_remain
    STORED(3)=0.d0
    STORED(4)=startup_energy
    STORED(5)=fcall

    CALL setStorageVars(STORED,NSTORED,INFO)


    !RETURN TO THE CALLING PROGRAM
    RETURN 1

ENDIF
!-----------------------------------------------------------------------------------------------------------------------

!DO ALL OF THE INITIAL TIMESTEP MANIPULATIONS HERE 
IF (TIME .LT. (getSimulationStartTime() + getSimulationTimeStep()/2.D0)) THEN
    !SET THE UNIT NUMBER FOR FUTURE CALLS
    IUNIT=INFO(1)
    ITYPE=INFO(2)

    !PERFORM ANY REQUIRED CALCULATIONS TO SET THE INITIAL VALUES OF THE OUTPUTS HERE
    !Cycle power output
    out(:)=0.d0
    !Heat transfer fluid outlet temp
    OUT(3)=T_hot
    !reheat temperatures
    OUT(15)=T_hot
    OUT(16)=T_hot
    !-----------------------------------------------------------------------
    !the steam calls contain subroutines for the following relationships. 
    ! '*' indicates a required input.
    !-----------------------------------------------------------------------
    !    function  |  1   |  2  | 3  | 4  | 5  | 6  |7 |8 | 9  | 10 | 11 |
    !-----------------------------------------------------------------------
    !    water_PS   (pres*,entr*,temp,dens,inte,enth,cv,cp,cond,visc,qual)
    !    water_TP   (temp*,pres*,dens,inte,enth,entr,cv,cp,cond,visc,qual)
    !    water_PH   (pres*,enth*,temp,dens,inte,entr,cv,cp,cond,visc,qual)
    !    water_TS   (temp*,entr*,pres,dens,inte,enth,cv,cp,cond,visc,qual)
    !    water_HS   (enth*,entr*,temp,pres,dens,inte,cv,cp,cond,visc,qual)
    !    water_TQ   (temp*,qual*,pres,dens,inte,enth,entr,cv,cp,cond,visc)
    !    water_PQ   (pres*,qual*,temp,dens,inte,enth,entr,cv,cp,cond,visc)
    !-----------------------------------------------------------------------
    ! units: 
    !   pres    kPa
    !   temp    C
    !   entr    kJ/kg-K
    !   enth    kJ/kg
    !   dens    kg/m3
    !   inte    kJ/kg
    !   cv      kJ/kg-K
    !   cp      kJ/kg-K
    !   cond    W/m-K
    !   visc    Pa-s
    !   qual    -
    !-----------------------------------------------------------------------

    
    !Calculate the power block side steam enthalpy rise for blowdown calculations
    !Limit the boiler pressure to below the supercritical point.  If a supercritical pressure is used,
    !notify the user that the pressure value is being switched.
    if(P_boil_ref > 215.) then
        P_boil_ref = 215.  !Set to 215 bar
        call Messages(-1,"Boiler pressure provided by the user requires a supercritical system. The pressure value has been reset to 215 bar.",'Warning',info(1),info(2)) 
    endif
            
    !hot steam
    call water_TP(T_hot_ref,P_check(P_max, P_boil_ref)*100.d0,enth=h_st_hot)
    
    !Use the cold steam enthalpy according to design-point boiler inlet temp. assume 5% pressure drop.
    call water_TP(T_cold_ref,P_check(P_max, P_boil_ref)*100.d0*1.05d0,enth=h_st_cold)

    dh_steam = (h_st_hot - h_st_cold)
    
    !8.30.2010 :: Calculate the startup energy needed
    startup_energy = startup_frac*P_ref/eta_ref         ![kWt]
    
    RETURN 1

ENDIF

!-----------------------------------------------------------------------------------------------------------------------
!    *** Iterative section ***
!-----------------------------------------------------------------------------------------------------------------------

!RETRIEVE THE VALUES IN THE STORAGE ARRAY FOR THIS ITERATION
CALL getStorageVars(STORED,NSTORED,INFO)
last_standby_control=STORED(1)
startup_remain=STORED(2)
last_P_cycle=STORED(3)
startup_e_remain=STORED(4)
fcall = STORED(5)

!Get inputs
mode = xin(1) 		![none] Cycle part load control.. from plant controller
T_hot = xin(2) 		![C] Hot inlet temperature.. from storage tank
m_dot_st = xin(3) 		![kg/hr] mass flow rate
T_wb = xin(4)+273.15 		![C] --> [K] Ambient wet bulb temperature
demand_var = xin(5) 		![none] Control signal indicating operational mode
standby_control = xin(6) 		![none] Control signal indicating standby mode
T_db = xin(7)+273.15 		![C] --> [K] Ambient dry bulb temperature
P_amb = xin(8)*101325.010000 		![atm] --> [Pa] Ambient pressure
TOU = int(xin(9)) 		![none] Current Time-of-use period
rh = xin(10) 		![none] Relative humidity of the ambient air

!11/1/11 TN: Added for steam receiver 
IF(jfix(tech_type + .0001)==5)THEN
    f_recSU     = xin(11) 
ELSE
    f_recSU     = 1.d0
ENDIF

dp_SH = xin(12)/1.e5     ![Bar] Pressure drop in superheater
dp_RH = xin(13)/1.e5     ![Bar] Pressure drop in reheater

    IUNIT=INFO(1)
    ITYPE=INFO(2)
    if(mode.eq.1.) demand_var = demand_var*1000.  !If the mode is to operate in power demand, convert from MW to kW
if(info(7)>10) then
    m_dot_rh = m_dot_st*rh_frac_ref !The steam mass flow rate changes, so estimate the reheat mass flow
    goto 900 !MJW 12.10.2010 Don't recalculate
endif
!Specific heat of water
m_dot_st_bd = 0.d0

!8/2/11 TN
P_turb_in   = 0.d0  !Reset turbine inlet pressure so that if not in normal operation, returns 0
P_rh_in     = 0.d0  !Reset reheat turbine inlet pressure as well
m_dot_rh    = 0.d0  !And this one..
T_rh_in     = 0.d0
T_rh_out    = 0.d0

      
select case (int(standby_control))
case(1)  !The cycle is in normal operation

    call DSGRankineCycle(time,P_ref, eta_ref, T_hot_ref, T_cold_ref, T_db, T_wb, P_amb, dT_cw_ref, is_rh, T_rh_hot_ref, rh_frac_ref, &
                      T_hot, m_dot_st, mode, demand_var,P_boil_ref, P_rh_ref, tech_type,T_amb_des, T_approach, F_wc(TOU), F_wcmin, F_wcmax, &
                      T_ITD_des, dP_RH, P_cond_ratio, CT, P_cond_min, n_pl_inc, fcall, P_cycle, eta, T_cold, m_dot_demand, m_dot_ref, &
                      m_dot_makeup, W_cool_par, f_hrsys, P_cond, P_turb_in, m_dot_rh, P_rh_in, T_rh_in, T_rh_out)
    !Check the output to make sure it's reasonable. If not, return zeros.
    if(((eta>1.).or.(eta<0.)).or.((T_cold > T_hot).or.(T_cold < T_cold_ref - 100.))) then
        P_cycle = 0.d0
        eta = 0.d0
        T_cold = T_cold_ref
    endif
    
    P_cycle = f_recSU * P_cycle
    
    !-----Calculate the blowdown fraction-----
    m_dot_st_bd = m_dot_st/3600.d0*pb_bd_frac
    
    if(ErrorFound()) return 1

case(2)  !The cycle is in standby operation
    q_tot = P_ref/eta_ref

    !Calculate the actual q_sby_needed from the reference flows
    q_sby_needed = q_tot*q_sby_frac

    !now calculate the mass flow rate knowing the inlet temperature of the salt,
    !..and holding the outlet temperature at the reference outlet temperature
    
    call water_TP(T_hot, P_check(P_max, P_boil_ref)*100.d0, enth=h_st_hot)
    call water_TP(T_cold_ref, P_check(P_max, P_boil_ref)*100.d0, enth=h_st_cold)
    m_dot_sby = q_sby_needed/(h_st_hot - h_st_cold)
    
    !Set other output values
    P_cycle = 0.
    eta = 0.
    T_cold = T_cold_ref
    m_dot_demand = m_dot_sby
    m_dot_makeup = 0.
    W_cool_par = 0.
    f_hrsys = 0.
    P_cond = 0.
    
case(3)  !The cycle has been completely shut down
    P_cycle = 0.
    eta = 0.
    T_cold = T_cold_ref  !Changed from T_hot 12/18/2009 was causing problems with T250
    m_dot_demand = 0.0
    m_dot_makeup = 0.
    W_cool_par = 0.
    f_hrsys = 0.
    P_cond = 0.
end select
      
      
!If the cycle is going from completely shut down to starting up, set the remaining startup
!time to be equal to the designated startup time
if((last_standby_control.eq.3.).and.(standby_control.eq.1.)) then
    startup_remain=startup_time
    startup_e_remain = startup_energy
endif

!If the cycle is starting up beginning in this time period, or it is continuing to start
!up from the last time period, then subtract the appropriate fraction of electric power
!from the output.  Note that during the startup time, not only is the cycle not producing power,
!but it is also consuming thermal energy
if(P_cycle > 0.d0) then
    if(((last_standby_control == 3.).and.(standby_control == 1.)).or.((startup_remain+startup_e_remain) > 0.)) then
        
        !Adjust the power cycle output. Both the energy and time requirement must be met before power is produced,
        !so subtract the maximum of these two values
        Q_cycle = P_cycle/eta
        startup_e_used = dmin1(Q_cycle*tstep, startup_e_remain)       !The used startup energy is the less of the energy to the power block and the remaining startup requirement
        
        f_st = 1.d0 - dmax1(dmin1(1.d0,startup_remain/tstep), startup_e_used/Q_cycle)
        P_cycle = P_cycle*f_st

        !Fraction of the timestep running at full capacity
        !The power cycle still requires mass flow to satisfy the energy demand requirement, so only subtract demand mass flow
        !for the case when startup time exceeds startup energy.
        m_dot_demand = m_dot_demand*(1. - dmax1(dmin1(1.d0,startup_remain/tstep) - startup_e_used/Q_cycle, 0.d0))

        !9/21/11, TN: Average the reported outlet temperature based on the amount of time the cycle has been operating
        IF(f_st > 0.d0)THEN
            T_cold = f_st*T_cold + (1.d0 - f_st)*T_cold_ref
        ENDIF
        
        startup_remain = dmax1(startup_remain-tstep,0.d0)
        startup_e_remain= dmax1(startup_e_remain - startup_e_used, 0.d0)
    endif
else
    startup_e_used = 0.d0
endif
      
!SET THE OUTPUTS FROM THIS MODEL
900 continue !MJW 12.10.2010

!Cycle power output
OUT(1)=P_cycle/1000.    !Convert from kW to MW 
!Cycle thermal efficiency
OUT(2)=eta
!Heat transfer fluid outlet temp
OUT(3)=T_cold
!Wet cooling makeup water flow rate
OUT(4)=(m_dot_makeup + m_dot_st_bd)*3600.
!Heat transfer fluid demand flow rate
OUT(5)=m_dot_demand
!Heat transfer fluid flow rate [kg/hr]
OUT(6)=m_dot_st			
!Calculated reference flow rate			
OUT(7)=m_dot_ref
!Cooling tower parasitic load [MW]
OUT(8)=W_cool_par
!Reference power level output
OUT(9)=P_ref/1000.   !Convert from kW to MW
!Fraction of cooling system in operation
OUT(10)=f_hrsys
!Condenser pressure (Pa)
OUT(11)=P_cond

!add new outputs for reheater
OUT(12)= P_turb_in + dp_SH      ![bar] Turbine inlet pressure: TN 9/2/11: Add SH pressure drop
!OUT(13)= m_dot_rh       ![kg/hr] Reheat steam mass flow rate

!8/2/11 TN: Reheat mass flow fraction is more useful than mass flow rate
OUT(13)= m_dot_rh / max(1.d0, m_dot_st)  ![-] Reheat steam mass flow rate fraction

OUT(14)= P_rh_in     ![bar] reheater inlet pressure
OUT(15)= T_rh_in     ![C] reheater inlet temperature
OUT(16)= T_rh_out    ![C] reheater outlet temperature

    RETURN 1
END
!-----------------------------------------------------------------------------------------------------------------------

subroutine DSGRankineCycle(time,P_ref, eta_ref, T_hot_ref, T_cold_ref, T_db, T_wb, P_amb, dT_cw_ref, is_rh, T_rh_hot_ref, rh_frac_ref,&
                        T_hot, m_dot_st, mode, demand_var,P_boil_ref, P_rh_ref, tech_type,T_amb_des, T_approach, F_wc, F_wcmin, F_wcmax,&
                        T_ITD_des, dP_RH, P_cond_ratio, CTr, P_cond_min, n_pl_inc, fcall, P_cycle, eta, T_cold, m_dot_demand, &
                        m_dot_ref, m_dot_makeup,W_cool_par, f_hrsys, P_cond, P_turb_in, m_dot_rh, P_rh_in, T_rh_in, T_rh_out)
!subroutine DSGRankineCycle(time[hr],P_ref[kW], eta_ref[-], T_hot_ref[C], T_cold_ref[C], T_db[K], T_wb[K], P_amb[Pa], dT_cw_ref[K], is_rh[logical], T_rh_hot_ref[C], rh_frac_ref[-],&
!                        T_hot[C], m_dot_st[kg/hr], mode[-], demand_var[kg/hr], P_boil_ref[bar], P_rh_ref[bar], tech_type[-], T_amb_des[C], T_approach[C], F_wc[-], F_wcmin[-], F_wcmax[-],&
!                        T_ITD_des[C], dP_RH[bar], P_cond_ratio[-], CTr[-], P_cond_min[Pa], n_pl_inc[-], fcall[-], P_cycle[kW], eta[-], T_cold[C], m_dot_demand, &
!                        m_dot_ref, m_dot_makeup,W_cool_par, f_hrsys, P_cond[Pa], P_turb_in[bar], m_dot_rh[kg/hr], P_rh_in[bar], T_rh_in[C], T_rh_out[C])
use CSP_cooling_functions
use trnsysfunctions
use CSP_HR_mod
use water_properties
implicit none 

real(8),intent(inout)::fcall
real(8):: P_ref, eta_ref, T_hot_ref, T_cold_ref, T_cw_in, dT_cw_ref, T_hot, m_dot_st, m_dot_cw, mode, demand_var, &
          P_boil_ref, tech_type,T_amb_des,q_dot_ref, T_hot_ND, m_dot_ND, m_dot_cw_ND,adj, err, P_dem_ND, P_ND_tot, Q_ND_tot, CTr
real(8):: P_ND(3), Q_ND(3), R_ND(3), G_ND(3),T_ref, specheat, P_AB, P_CA, P_BC, Q_AB, Q_CA, Q_BC, &
          R_AB, R_CA, R_BC, R_ND_tot, W_cool_par, P_cond, T_cond, q_reject_est, m_dot_makeup, q_reject, m_dot_air, T_ITD_des, P_cond_ratio,&
          T_approach, P_amb, T_db, T_wb, time, W_cool_parhwc, W_cool_parhac, F_wc, F_wcmin, F_wcmax, f_hrsys, &
          P_cond_min, n_pl_inc, eta_adj, rand, xrand,Psat_ref, h_hot_ref, h_cold_ref, h_hot, h_cold, c_htf_ref, &
          P_rh_ref, h_t_in, h_t_out, h_t_outs, s_t, eta_t, T_rh_in, P_turb_in, m_dot_rh, P_rh_in, T_rh_out,&
          T_rh_hot_ref, rh_frac_ref, h_rh_out, q_dot_rh_ref, q_dot_st_ref, T_s_rh, dP_RH,&
          s_rh_out, h_LP_out_isen, h_LP_out, h_sh_in, q_dot_sh_ref, q_b_des, P_check, P_max, CycleMap_DSG
save:: eta_adj  !,Psat_ref
!Declare inputs
integer::i, qq, TT, CT
logical:: float_pres, is_rh
!Declare output variables
real(8),intent(out):: P_cycle, eta, T_cold, m_dot_demand, m_dot_ref

!______________________________________________________________________________
P_max = 190. ![bar]
!Select the technology type
!if(tech_type.eq.1.) TT=1  !high temp
!if(tech_type.eq.2.) TT=2  !low temp
!if(tech_type.eq.3.) TT=3  !user coefficients
!if(tech_type.eq.4.) TT=4  !isopentane Rankine cycle for geothermal
!if(tech_type.eq.5.) TT=5 !DSG w/ reheat
TT=jfix(tech_type + .0001)

!Convert units
!Mass flow rates from kg/hr to kg/s
m_dot_st = m_dot_st/3600. ![kg/s]

!The user provides a reference efficiency, ambient temperature, and cooling system parameters. Using
!this information, we have to adjust the provided reference efficiency to match the normalized efficiency
!that is part of the power block regression coefficients. I.e. if the user provides a ref. ambient temperature
!of 25degC, but the power block coefficients indicate that the normalized efficiency equals 1.0 at an ambient 
!temp of 20degC, we have to adjust the user's efficiency value back to the coefficient set.
CT = int(CTr)
if(fcall==1.d0) then
    select case(CT)
    case(1) !Wet cooled case
        if(TT/=4) then
            !steam
            Psat_ref = f_psat_T(dT_cw_ref + 3. + T_approach + T_amb_des)
        else
            !Isopentane
            Psat_ref = P_sat4(dT_cw_ref + 3. + T_approach + T_amb_des)
        endif
        eta_adj = eta_ref/(CycleMap_DSG(TT,12,2,Psat_ref)/CycleMap_DSG(TT,22,2,Psat_ref))
    case(2:3) !Dry cooled case and Hybrid case
        if(TT/=4) then
            !steam
            Psat_ref = f_psat_T(T_ITD_des + T_amb_des)
        else
            !Isopentane
            Psat_ref = P_sat4(T_ITD_des + T_amb_des)
        endif
        eta_adj = eta_ref/(CycleMap_DSG(TT,12,2,Psat_ref)/CycleMap_DSG(TT,22,2,Psat_ref))
    end select
    
    !****Calculate the reference values
    q_dot_ref = P_ref/eta_adj   ![kW] The reference heat flow

    IF(tech_type==5)THEN
        !TN 2011.10.12 For Direct steam receiver ONLY
        !This section is used to recalculate the reference cold feedwater inlet temperature given other
        !constraints and design point values provided by the user. This results in an adjusted T_cold_ref
        !value, and it could be different than the value provided by the user... Will remove T_cold_ref from GUI
        call water_TP(T_hot_ref,P_check(P_max, P_boil_ref)*100.d0, enth=h_hot_ref, entr=s_t)  !HP turbine inlet enthalpy (kJ/kg) and entropy (kJ/kg)
        
        IF(is_rh)THEN
            call water_PS(P_check(P_max, P_rh_ref)*100.d0, s_t, enth=h_t_outs)      ![kJ/kg] Isentropic HP outlet enthalpy
            h_t_out = h_hot_ref - (h_hot_ref - h_t_outs)*0.88       ![kJ/kg] HP outlet enthalpy
            call water_PH(P_check(P_max, P_rh_ref)*100.d0, h_t_out, temp = T_rh_in) ![C] Reheat inlet temperature
            call water_TP(T_rh_hot_ref, P_check(P_max, P_rh_ref)*100.d0, enth = h_rh_out, entr = s_rh_out)  !LP turbine inlet enthalpy (kJ/kg) and entropy (kJ/kg-K)
            call water_PS(P_check(P_max, Psat_ref*1e-5)*100.d0,s_rh_out, enth = h_LP_out_isen)  ![kJ/kg] Isentropic LP outlet enthalpy
            h_LP_out = h_rh_out - (h_rh_out - h_LP_out_isen)*0.88   ![kJ/kg] LP outlet enthalpy
            call water_PQ(P_check(P_max, P_boil_ref)*100.d0,1.d0, enth=h_sh_in)     ![kJ/kg] SH inlet enthalpy
        ELSE
            rh_frac_ref = 0.d0
            call water_PS(P_check(P_max, Psat_ref*1.e-5)*100.d0,s_t, enth=h_t_outs)      ![kJ/kg] Isentropic turbine outlet enthalpy
            h_t_out = h_hot_ref - (h_hot_ref - h_t_outs)*0.88       ![kJ/kg] Turbine outlet enthalpy
            h_rh_out = 0.d0                                         !just set this value to something        
        ENDIF
        
        m_dot_ref   = P_ref/ ( (h_hot_ref - h_t_out) + rh_frac_ref*(h_rh_out - h_LP_out) )      ![kg/s] Reference mass flow rate
        
        q_dot_rh_ref    = m_dot_ref*rh_frac_ref*(h_rh_out - h_t_out)        ![kW] Reference heat input to reheater
        q_dot_sh_ref    = m_dot_ref*(h_hot_ref - h_sh_in)                   ![kW] Reference heat input to superheater
        
        q_b_des     = q_dot_ref - q_dot_rh_ref - q_dot_sh_ref               ![kW] Reference heat input to boiler
        
        h_cold_ref  = h_sh_in - q_b_des/m_dot_ref                           ![kJ/kg] Feedwater
        call Water_PH(P_check(P_max, P_boil_ref)*100.d0,h_cold_ref, temp = T_cold_ref)      ![C] Design feedwater outlet temperature
          
        q_dot_st_ref    = m_dot_ref*(h_hot_ref - h_cold_ref)                ![kW] Reference heat input between feedwater and HP turbine

    ELSE

        call water_TP(T_hot_ref,P_check(P_max, P_boil_ref)*100.d0, enth=h_hot_ref, entr=s_t)  !turbine inlet enthalpy and entropy
        call water_TP(T_cold_ref,P_check(P_max, P_boil_ref)*100.d0, enth=h_cold_ref)
            
        if(is_rh) then
            !Calculate the reheater inlet temperature assuming an isentropic efficiency model
            call water_PS(P_check(P_max, P_rh_ref)*100.d0, s_t, enth=h_t_outs) !reheat extraction enthalpy assuming isentropic expansion
            h_t_in = h_hot_ref
            h_t_out = h_t_in - (h_t_in - h_t_outs)*0.88  !The actual reheat inlet enthalpy
            call water_PH(P_check(P_max, P_rh_ref)*100.d0, h_t_out, temp=T_rh_in) !Reheat inlet temperature    
            call water_TP(T_rh_hot_ref, P_check(P_max, P_rh_ref-dP_RH)*100.d0, enth=h_rh_out) !Reheat outlet enthalpy
        else
            rh_frac_ref = 0.d0
        endif

        !design-point mass flow rate
        m_dot_ref = q_dot_ref/((h_rh_out - h_t_out)*rh_frac_ref + (h_hot_ref - h_cold_ref))

        !Design-point reheater thermal input
        q_dot_rh_ref = m_dot_ref*(h_rh_out - h_t_out)*rh_frac_ref
        q_dot_st_ref = m_dot_ref*(h_hot_ref - h_cold_ref)

    ENDIF
    
endif

!!****Calculate the reference values
!q_dot_ref = P_ref/eta_adj   ![kW] The reference heat flow
!
!IF(tech_type==5)THEN
!    !TN 2011.10.12 For Direct steam receiver ONLY
!    !This section is used to recalculate the reference cold feedwater inlet temperature given other
!    !constraints and design point values provided by the user. This results in an adjusted T_cold_ref
!    !value, and it could be different than the value provided by the user... Will remove T_cold_ref from GUI
!    call water_TP(T_hot_ref,P_check(P_max, P_boil_ref)*100.d0, enth=h_hot_ref, entr=s_t)  !HP turbine inlet enthalpy (kJ/kg) and entropy (kJ/kg)
!    
!    IF(is_rh)THEN
!        call water_PS(P_check(P_max, P_rh_ref)*100.d0, s_t, enth=h_t_outs)      ![kJ/kg] Isentropic HP outlet enthalpy
!        h_t_out = h_hot_ref - (h_hot_ref - h_t_outs)*0.88       ![kJ/kg] HP outlet enthalpy
!        call water_PH(P_check(P_max, P_rh_ref)*100.d0, h_t_out, temp = T_rh_in) ![C] Reheat inlet temperature
!        call water_TP(T_rh_hot_ref, P_check(P_max, P_rh_ref)*100.d0, enth = h_rh_out, entr = s_rh_out)  !LP turbine inlet enthalpy (kJ/kg) and entropy (kJ/kg-K)
!        call water_PS(P_check(P_max, Psat_ref*1e-5)*100.d0,s_rh_out, enth = h_LP_out_isen)  ![kJ/kg] Isentropic LP outlet enthalpy
!        h_LP_out = h_rh_out - (h_rh_out - h_LP_out_isen)*0.88   ![kJ/kg] LP outlet enthalpy
!        call water_PQ(P_check(P_max, P_boil_ref)*100.d0,1.d0, enth=h_sh_in)     ![kJ/kg] SH inlet enthalpy
!    ELSE
!        rh_frac_ref = 0.d0
!        call water_PS(P_check(P_max, Psat_ref*1.e-5)*100.d0,s_t, enth=h_t_outs)      ![kJ/kg] Isentropic turbine outlet enthalpy
!        h_t_out = h_hot_ref - (h_hot_ref - h_t_outs)*0.88       ![kJ/kg] Turbine outlet enthalpy
!        h_rh_out = 0.d0                                         !just set this value to something        
!    ENDIF
!    
!    m_dot_ref   = P_ref/ ( (h_hot_ref - h_t_out) + rh_frac_ref*(h_rh_out - h_LP_out) )      ![kg/s] Reference mass flow rate
!    
!    q_dot_rh_ref    = m_dot_ref*rh_frac_ref*(h_rh_out - h_t_out)        ![kW] Reference heat input to reheater
!    q_dot_sh_ref    = m_dot_ref*(h_hot_ref - h_sh_in)                   ![kW] Reference heat input to superheater
!    
!    q_b_des     = q_dot_ref - q_dot_rh_ref - q_dot_sh_ref               ![kW] Reference heat input to boiler
!    
!    h_cold_ref  = h_sh_in - q_b_des/m_dot_ref                           ![kJ/kg] Feedwater
!    call Water_PH(P_check(P_max, P_boil_ref)*100.d0,h_cold_ref, temp = T_cold_ref)      ![C] Design feedwater outlet temperature
!      
!    q_dot_st_ref    = m_dot_ref*(h_hot_ref - h_cold_ref)                ![kW] Reference heat input between feedwater and HP turbine
!
!ELSE
!
!    call water_TP(T_hot_ref,P_check(P_max, P_boil_ref)*100.d0, enth=h_hot_ref, entr=s_t)  !turbine inlet enthalpy and entropy
!    call water_TP(T_cold_ref,P_check(P_max, P_boil_ref)*100.d0, enth=h_cold_ref)
!        
!    if(is_rh) then
!        !Calculate the reheater inlet temperature assuming an isentropic efficiency model
!        call water_PS(P_check(P_max, P_rh_ref)*100.d0, s_t, enth=h_t_outs) !reheat extraction enthalpy assuming isentropic expansion
!        h_t_in = h_hot_ref
!        h_t_out = h_t_in - (h_t_in - h_t_outs)*0.88  !The actual reheat inlet enthalpy
!        call water_PH(P_check(P_max, P_rh_ref)*100.d0, h_t_out, temp=T_rh_in) !Reheat inlet temperature    
!        call water_TP(T_rh_hot_ref, P_check(P_max, P_rh_ref-dP_RH)*100.d0, enth=h_rh_out) !Reheat outlet enthalpy
!    else
!        rh_frac_ref = 0.d0
!    endif
!
!    !design-point mass flow rate
!    m_dot_ref = q_dot_ref/((h_rh_out - h_t_out)*rh_frac_ref + (h_hot_ref - h_cold_ref))
!
!    !Design-point reheater thermal input
!    q_dot_rh_ref = m_dot_ref*(h_rh_out - h_t_out)*rh_frac_ref
!    q_dot_st_ref = m_dot_ref*(h_hot_ref - h_cold_ref)
!
!ENDIF


!Calculate the htf mass flow rate in non-dimensional form
m_dot_ND = m_dot_st/m_dot_ref

!Calculate the reheater and boiler pressures based on the mass flow fraction
Psat_ref = Psat_ref*1.e-5 !Convert Pa to bar
if(is_rh) then
    P_rh_in = (Psat_ref**2. + dmax1(0.5d0, m_dot_ND)**2.*(P_rh_ref**2. - Psat_ref**2.))**0.5  !Patnode thesis, p. 69
else
    P_rh_in = Psat_ref
    P_rh_ref = Psat_ref
endif
P_turb_in = (P_rh_in**2. + dmax1(0.5d0, m_dot_ND)**2.*(P_boil_ref**2. - P_rh_ref**2.))**0.5 !MJW 10.12.11 Limit to 50% rated pressure

!Determine HP turbine outlet conditions
if(is_rh) then
    !Calculate the reheater inlet temperature assuming an isentropic efficiency model
    call water_TP(T_hot,P_check(P_max, P_turb_in)*100.d0, enth=h_t_in, entr=s_t)   !turbine inlet enthalpy and entropy
    call water_PS(P_check(P_max, P_rh_in)*100.d0, s_t, enth=h_t_outs) !reheat extraction enthalpy assuming isentropic expansion
    eta_t = 0.88*eta_pl(m_dot_ND)
    h_t_out = h_t_in - (h_t_in - h_t_outs)*eta_t  !The actual reheat inlet enthalpy
    call water_PH(P_check(P_max, P_rh_in)*100.d0, h_t_out, temp=T_rh_in) !Reheat inlet temperature
endif

!The saturation temperature at the boiler. Using the floating pressure value is consistent with the regression model formulation in this case.
call water_PQ(P_check(P_max, P_turb_in)*100., 0.5d0, temp=T_ref)

!Calculate the hot inlet steam temperature, in non-dimensional form
T_hot_ND = (T_hot - T_ref)/(T_hot_ref - T_ref)

if(is_rh) then
    !Calculate the reheat outlet temperature, assuming reheat ND is equal to hot ND
    T_s_rh = T_sat(P_rh_in)-273.15 ![K]
    T_rh_out = T_s_rh + (T_rh_hot_ref - T_s_rh)*T_hot_ND
    !Using temperature and pressure, calculate reheat outlet enthalpy
    call water_TP(T_rh_out, P_check(P_max, P_rh_in-dP_rh)*100.d0, enth=h_rh_out)
endif

!Do an initial cooling tower call to estimate the turbine back pressure. 
q_reject_est = q_dot_ref*1000.*(1.-eta_adj)*m_dot_ND*T_hot_ND

select case (CT)  !Cooling technology type {1=evaporative cooling, 2=air cooling, 3=hybrid cooling}
case(1)
    !For a wet-cooled system
    call evap_tower(TT,P_cond_min, n_pl_inc, dT_cw_ref, T_approach, (P_ref*1000.), eta_adj, T_db, T_wb, P_amb, q_reject_est, m_dot_makeup, W_cool_par, P_cond, T_cond, f_hrsys)
case(2)
    !For a dry-cooled system
    call ACC(TT,P_cond_min, n_pl_inc, T_ITD_des, P_cond_ratio, (P_ref*1000.), eta_adj, T_db, P_amb, q_reject_est, m_dot_air, W_cool_par, P_cond, T_cond, f_hrsys)
    m_dot_makeup = 0.d0
case(3)
    !for a hybrid cooled system
    call HybridHR(TT, fcall, P_cond_min, n_pl_inc, time, F_wc, F_wcmax, F_wcmin, T_ITD_des, T_approach, dT_cw_ref, P_cond_ratio, (P_ref*1000.), eta_adj, T_db, T_wb, &
                    P_amb, q_reject_est, m_dot_makeup, W_cool_parhac, W_cool_parhwc, W_cool_par, P_cond, T_cond, f_hrsys)
end select



!  Set initial values
ADJ = 1. ; err = 1. ; qq=0

!Do a quick check to see if there is actually a mass flow being supplied 
!  to the cycle. If not, go to the end.
if(abs(m_dot_ND).lt.1.e-3) then
    P_cycle = 0.
    eta = 0.
    T_cold = T_hot_ref
    m_dot_demand = m_dot_ref
    W_cool_par = 0.
    m_dot_makeup = 0.
    !Set the error to zero, since we don't want to iterate
    err=0.
endif

!Begin iterations
do while ((err.gt.1.e-6).and.(qq.lt.100)) 
    qq=qq+1
    !Now use the constrained variable to calculate the demand mass flow rate
    if(mode.eq.1.) then
        P_dem_ND = demand_var/P_ref
        if(qq.eq.1) m_dot_ND = P_dem_ND   !An initial guess (function of power)
        !if(qq.gt.1) m_dot_ND = m_dot_ND*ADJ
    elseif(mode.eq.2.) then
        continue     ! do nothing
    endif

    !++++++++++++++Correlations++++++++++++++++++
    !Calculate the correlations
    !++++++++++++++++++++++++++++++++++++++++++++
    !POWER  
    !Main effects
    P_ND(1) = CycleMap_DSG(TT,11,1,T_hot_ND)-1.
    P_ND(2) = CycleMap_DSG(TT,12,2,P_cond)-1.
    P_ND(3) = CycleMap_DSG(TT,13,3,m_dot_ND)-1.
    
    !Interactions
    P_CA = CycleMap_DSG(TT,113,13,T_hot_ND)
    P_AB = CycleMap_DSG(TT,112,12,P_cond)
    P_BC = CycleMap_DSG(TT,123,23,m_dot_ND)

    P_ND(1) = P_ND(1)*P_AB
    P_ND(2) = P_ND(2)*P_BC
    P_ND(3) = P_ND(3)*P_CA
    
    !HEAT               
    !Main effects
    Q_ND(1) = CycleMap_DSG(TT,21,1,T_hot_ND)-1.
    Q_ND(2) = CycleMap_DSG(TT,22,2,P_cond)-1.
    Q_ND(3) = CycleMap_DSG(TT,23,3,m_dot_ND)-1.

    !Interactions
    Q_CA = CycleMap_DSG(TT,213,13,T_hot_ND)
    Q_AB = CycleMap_DSG(TT,212,12,P_cond)
    Q_BC = CycleMap_DSG(TT,223,23,m_dot_ND)

    Q_ND(1) = Q_ND(1)*Q_AB
    Q_ND(2) = Q_ND(2)*Q_BC
    Q_ND(3) = Q_ND(3)*Q_CA
    
    
    if(is_rh) then
        !Reheat
        !main effects
        R_ND(1) = CycleMap_DSG(TT,31,1,T_hot_ND)-1.
        R_ND(2) = CycleMap_DSG(TT,32,2,P_cond)-1.
        R_ND(3) = CycleMap_DSG(TT,33,3,m_dot_ND)-1.

        !Interactions
        R_CA = CycleMap_DSG(TT,313,13,T_hot_ND)
        R_AB = CycleMap_DSG(TT,312,12,P_cond)
        R_BC = CycleMap_DSG(TT,323,23,m_dot_ND)

        R_ND(1) = R_ND(1)*R_AB
        R_ND(2) = R_ND(2)*R_BC
        R_ND(3) = R_ND(3)*R_CA
    else
        R_ND(:) = 0.d0
    endif

    !Calculate the cumulative values
    P_ND_tot = 1.d0
    Q_ND_tot = 1.d0
    R_ND_tot = 1.d0    
    
    !Increment main effects. MJW 8.11.2010 :: For this system, the effects are multiplicative.
    do i=1,size(P_ND)
        P_ND_tot = P_ND_tot *(1.d0+ P_ND(i))
        Q_ND_tot = Q_ND_tot *(1.d0+ Q_ND(i))
        R_ND_tot = R_ND_tot *(1.d0+ R_ND(i))
    enddo

    !Calculate the output values:
    P_cycle = P_ND_tot*P_ref
    call water_TP(T_hot,P_check(P_max, P_turb_in)*100.d0, enth=h_hot)      !9/21/11, TN: Need to use current pressure, not reference
    h_cold = h_hot-Q_ND_tot*q_dot_st_ref/m_dot_st 
    call water_PH(P_check(P_max, P_turb_in)*100.d0, h_cold, temp=T_cold) 
    !*****************************
    !T_cold = 300.d0
    !****************************
    
    eta = P_cycle/(Q_ND_tot*q_dot_st_ref + R_ND_tot*q_dot_rh_ref)
    m_dot_demand = dmax1(m_dot_ND*m_dot_ref,0.00001d0)   ![kg/s]
    
    !Call the cooling tower model to update the condenser pressure
    q_reject = (1.-eta)*(q_dot_st_ref*Q_ND_tot+q_dot_rh_ref*R_ND_tot)*1000.   
    if (qq<10) then !MJW 10.31.2010    
    select case(CT)
    case(1)
        call evap_tower(TT,P_cond_min, n_pl_inc, dT_cw_ref, T_approach, (P_ref*1000.), eta_adj, T_db, T_wb, P_amb, q_reject, m_dot_makeup, W_cool_par, P_cond, T_cond, f_hrsys)
    case(2)
        call ACC(TT,P_cond_min, n_pl_inc, T_ITD_des, P_cond_ratio, (P_ref*1000.), eta_adj, T_db, P_amb, q_reject, m_dot_air, W_cool_par, P_cond, T_cond, f_hrsys)
    case(3)
        call HybridHR(TT,fcall, P_cond_min, n_pl_inc, time, F_wc, F_wcmax, F_wcmin, T_ITD_des, T_approach, dT_cw_ref, P_cond_ratio, (P_ref*1000.), eta_adj, T_db, T_wb, &
                      P_amb, q_reject, m_dot_makeup, W_cool_parhac, W_cool_parhwc, W_cool_par, P_cond, T_cond, f_hrsys)
    end select
    endif
    
    !Check to see if the calculated and demand values match
    !If they don't match, calculate the "ADJ" factor
    if(mode.eq.1.) then
        !err = (P_cycle - demand_var)/demand_var
        !ADJ = 1.+(demand_var-P_cycle)/(3.*demand_var)
        ADJ = (demand_var-P_cycle)/demand_var !MJW 10.31.2010: Adjustment factor 
        err = dabs(ADJ) !MJW 10.31.2010: Take the absolute value of the error.. 
        m_dot_ND = m_dot_ND + ADJ*.75   !MJW 10.31.2010: Iterate the mass flow rate. Take a step smaller than the calculated adjustment
    elseif(mode.eq.2.) then
        err = 0.
    endif

    if(qq.eq.99) then
        call messages(-1,"Power cycle model did not converge after 100 iterations","WARNING",234,0)
        P_cycle = 0.
        eta = 0.
        T_cold = T_hot_ref
        m_dot_demand = m_dot_ref
        if(errorfound()) return
    endif
    !If this is not true, the cycle has not yet converged, and we should return
    ! to continue in the iterations
enddo

!Calculate the reheat mass flow rate, convert temperatures
if(is_rh) then
    m_dot_rh = q_dot_rh_ref*R_ND_tot/(h_rh_out - h_t_out)*3600.d0
    continue
else
    m_dot_rh = 0.d0
    T_rh_in = 0.d0
    T_rh_out = 0.d0
endif

!Finally, convert the values back to their original units
!T_cold = T_cold - 273.15 ![K]-->[C]
!T_cold_ref = T_cold_ref - 273.15 ![K]->[C]
!T_hot_ref = T_hot_ref - 273.15 ![K]->[C]
!T_rh_hot_ref = T_rh_hot_ref - 273.15
m_dot_demand = m_dot_demand*3600.  ![kg/s]->[kg/hr]
m_dot_st = m_dot_st*3600.        ![kg/s]->[kg/hr]


!m_dot_ref = m_dot_ref*3600. ![kg/s]->[kg/hr]

!Set the "been called" variable
fcall = 0.d0

contains
    real(8) function eta_pl(mf)
        implicit none
        real(8)::mf
        !turbine isentropic efficiency penalty as a function of mass flow fraction (Patnode thesis)
        eta_pl = 1.-(0.191-0.409*mf + 0.218*mf**2.) !this number should be multiplied by the design-point isen. eff to get the final.
    end function
    
    real(8) function T_sat(P) !Steam
        implicit none !real(8) (a-z)
        real(8)::P
        !P is in [bar], T is in [K]
        !The saturation temp for pressures at the boiler
        T_sat = 439.486188 + 2.88644991*P - 0.0243308856*P**2 + 0.000125910226*P**3 - 2.66233987E-07*P**4
    end function
    
    real(8) function P_sat(T)
        implicit none !real(8) (a-z)
        real(8)::T
        !T is in [K], P is in [bar]
        !P_sat = -1551.35 + 9.94705*T - 0.0218711*T**2 + 0.0000165747*T**3
        P_sat = 1965.19859 - 15.1689701*T + 0.0452997046*T**2 - 0.000063150801*T**3 + 3.54340123E-08*T**4
    end function
    
end subroutine
!end "contains"

    !Subroutine with cycle performance information. Scope is global.
    real(8) function CycleMap_DSG(TT,YT,XT,X)
    use global_props
    implicit none

    !This function interpolates the data of one data list based on the value provided to a 
    !..corresponding list of the same length. 
    !TT: The flag indicating whether the High temperature or Low temperature tables should be used
    !YT: The name of the dependent Y variable being interpolated
    !XT: The name of the independent X variable which the Y variable is a function of
    !X: The value of the X variable
    !Interpolate: is returned as a double precision value of Y

    integer,intent(in)::YT,XT,TT
    real(8),intent(in)::X
    real(8)::Y,ind,temp(200)
    real(8),allocatable::dd(:,:)
    integer::XI,YI,i,j,lbi,ubi,jsav,n
    character::line*300
    
    !if the function is called with TT<0, this is a cleanup call. Deallocate.
    if(TT<0) then
        if(allocated(dd)) deallocate(dd)
        CycleMap_DSG = 0.d0
        return
    endif
    
    if(.not.allocated(dd)) then
    if(TT==2)then !Low temperature parabolic trough applications
        ! On first call, allocate the array and define coefficients
        allocate(dd(18,20))

	    dd(1,:)=(/ &
		    0.10000, 0.16842, 0.23684, 0.30526, 0.37368, 0.44211,  &
		    0.51053, 0.57895, 0.64737, 0.71579, 0.78421, 0.85263,  &
		    0.92105, 0.98947, 1.05789, 1.12632, 1.19474, 1.26316,  &
		    1.33158, 1.40000 /)
	    dd(2,:)=(/ &
		    0.08547, 0.14823, 0.21378, 0.28166, 0.35143, 0.42264,  &
		    0.49482, 0.56747, 0.64012, 0.71236, 0.78378, 0.85406,  &
		    0.92284, 0.98989, 1.05685, 1.12369, 1.19018, 1.25624,  &
		    1.32197, 1.38744 /)
	    dd(3,:)=(/ &
		    0.10051, 0.16934, 0.23822, 0.30718, 0.37623, 0.44534,  &
		    0.51443, 0.58338, 0.65209, 0.72048, 0.78848, 0.85606,  &
		    0.92317, 0.98983, 1.05604, 1.12182, 1.18718, 1.25200,  &
		    1.31641, 1.38047 /)
	    dd(4,:)=(/ &
		    3000.00, 4263.16, 5526.32, 6789.47, 8052.63, 9315.79,  &
		    10578.95, 11842.11, 13105.26, 14368.42, 15631.58, 16894.74,  &
		    18157.89, 19421.05, 20684.21, 21947.37, 23210.53, 24473.68,  &
		    25736.84, 27000.00 /)
	    dd(5,:)=(/ &
		    1.08827, 1.06020, 1.03882, 1.02145, 1.00692, 0.99416,  &
		    0.98288, 0.97273, 0.96350, 0.95504, 0.94721, 0.93996,  &
		    0.93314, 0.92673, 0.92069, 0.91496, 0.90952, 0.90433,  &
		    0.89938, 0.89464 /)
	    dd(6,:)=(/ &
		    1.01276, 1.00877, 1.00570, 1.00318, 1.00106, 0.99918,  &
		    0.99751, 0.99601, 0.99463, 0.99335, 0.99218, 0.99107,  &
		    0.99004, 0.98907, 0.98814, 0.98727, 0.98643, 0.98563,  &
		    0.98487, 0.98413 /)
	    dd(7,:)=(/ &
		    0.10000, 0.17368, 0.24737, 0.32105, 0.39474, 0.46842,  &
		    0.54211, 0.61579, 0.68947, 0.76316, 0.83684, 0.91053,  &
		    0.98421, 1.05789, 1.13158, 1.20526, 1.27895, 1.35263,  &
		    1.42632, 1.50000 /)
	    dd(8,:)=(/ &
		    0.09307, 0.16421, 0.23730, 0.31194, 0.38772, 0.46420,  &
		    0.54098, 0.61763, 0.69374, 0.76896, 0.84287, 0.91511,  &
		    0.98530, 1.05512, 1.12494, 1.19447, 1.26373, 1.33273,  &
		    1.40148, 1.46999 /)
	    dd(9,:)=(/ &
		    0.10741, 0.18443, 0.26031, 0.33528, 0.40950, 0.48308,  &
		    0.55610, 0.62861, 0.70066, 0.77229, 0.84354, 0.91443,  &
		    0.98497, 1.05520, 1.12514, 1.19478, 1.26416, 1.33329,  &
		    1.40217, 1.47081 /)
	    dd(10,:)=(/ &
		    0.10000, 0.16842, 0.23684, 0.30526, 0.37368, 0.44211,  &
		    0.51053, 0.57895, 0.64737, 0.71579, 0.78421, 0.85263,  &
		    0.92105, 0.98947, 1.05789, 1.12632, 1.19474, 1.26316,  &
		    1.33158, 1.40000 /)
	    dd(11,:)=(/ &
		    1.01749, 1.03327, 1.04339, 1.04900, 1.05051, 1.04825,  &
		    1.04249, 1.03343, 1.02126, 1.01162, 1.00500, 1.00084,  &
		    0.99912, 0.99966, 0.99972, 0.99942, 0.99920, 0.99911,  &
		    0.99885, 0.99861 /)
	    dd(12,:)=(/ &
		    0.99137, 0.99297, 0.99431, 0.99564, 0.99681, 0.99778,  &
		    0.99855, 0.99910, 0.99948, 0.99971, 0.99984, 0.99989,  &
		    0.99993, 0.99993, 0.99992, 0.99992, 0.99992, 1.00009,  &
		    1.00010, 1.00012 /)
	    dd(13,:)=(/ &
		    3000.00, 4263.16, 5526.32, 6789.47, 8052.63, 9315.79,  &
		    10578.95, 11842.11, 13105.26, 14368.42, 15631.58, 16894.74,  &
		    18157.89, 19421.05, 20684.21, 21947.37, 23210.53, 24473.68,  &
		    25736.84, 27000.00 /)
	    dd(14,:)=(/ &
		    0.99653, 0.99756, 0.99839, 0.99906, 0.99965, 1.00017,  &
		    1.00063, 1.00106, 1.00146, 1.00183, 1.00218, 1.00246,  &
		    1.00277, 1.00306, 1.00334, 1.00361, 1.00387, 1.00411,  &
		    1.00435, 1.00458 /)
	    dd(15,:)=(/ &
		    0.99760, 0.99831, 0.99888, 0.99934, 0.99973, 1.00008,  &
		    1.00039, 1.00067, 1.00093, 1.00118, 1.00140, 1.00161,  &
		    1.00180, 1.00199, 1.00217, 1.00234, 1.00250, 1.00265,  &
		    1.00280, 1.00294 /)
	    dd(16,:)=(/ &
		    0.10000, 0.17368, 0.24737, 0.32105, 0.39474, 0.46842,  &
		    0.54211, 0.61579, 0.68947, 0.76316, 0.83684, 0.91053,  &
		    0.98421, 1.05789, 1.13158, 1.20526, 1.27895, 1.35263,  &
		    1.42632, 1.50000 /)
	    dd(17,:)=(/ &
		    1.01994, 1.01645, 1.01350, 1.01073, 1.00801, 1.00553,  &
		    1.00354, 1.00192, 1.00077, 0.99995, 0.99956, 0.99957,  &
		    1.00000, 0.99964, 0.99955, 0.99945, 0.99937, 0.99928,  &
		    0.99919, 0.99918 /)
	    dd(18,:)=(/ &
		    1.02055, 1.01864, 1.01869, 1.01783, 1.01508, 1.01265,  &
		    1.01031, 1.00832, 1.00637, 1.00454, 1.00301, 1.00141,  &
		    1.00008, 0.99851, 0.99715, 0.99586, 0.99464, 0.99347,  &
		    0.99227, 0.99177 /)

    elseif (TT==1) then  !Power tower applications
        ! On first call, allocate the array and define coefficients
        allocate(dd(18,20))
		    dd(1,:)=(/ &
		    0.20000, 0.25263, 0.30526, 0.35789, 0.41053, 0.46316,  &
		    0.51579, 0.56842, 0.62105, 0.67368, 0.72632, 0.77895,  &
		    0.83158, 0.88421, 0.93684, 0.98947, 1.04211, 1.09474,  &
		    1.14737, 1.20000 /)
	    dd(2,:)=(/ &
		    0.16759, 0.21750, 0.26932, 0.32275, 0.37743, 0.43300,  &
		    0.48910, 0.54545, 0.60181, 0.65815, 0.71431, 0.77018,  &
		    0.82541, 0.88019, 0.93444, 0.98886, 1.04378, 1.09890,  &
		    1.15425, 1.20982 /)
	    dd(3,:)=(/ &
		    0.19656, 0.24969, 0.30325, 0.35710, 0.41106, 0.46497,  &
		    0.51869, 0.57215, 0.62529, 0.67822, 0.73091, 0.78333,  &
		    0.83526, 0.88694, 0.93838, 0.98960, 1.04065, 1.09154,  &
		    1.14230, 1.19294 /)
	    dd(4,:)=(/ &
		    3000.00, 4263.16, 5526.32, 6789.47, 8052.63, 9315.79,  &
		    10578.95, 11842.11, 13105.26, 14368.42, 15631.58, 16894.74,  &
		    18157.89, 19421.05, 20684.21, 21947.37, 23210.53, 24473.68,  &
		    25736.84, 27000.00 /)
	    dd(5,:)=(/ &
		    1.07401, 1.04917, 1.03025, 1.01488, 1.00201, 0.99072,  &
		    0.98072, 0.97174, 0.96357, 0.95607, 0.94914, 0.94269,  &
		    0.93666, 0.93098, 0.92563, 0.92056, 0.91573, 0.91114,  &
		    0.90675, 0.90255 /)
	    dd(6,:)=(/ &
		    1.00880, 1.00583, 1.00355, 1.00168, 1.00010, 0.99870,  &
		    0.99746, 0.99635, 0.99532, 0.99438, 0.99351, 0.99269,  &
		    0.99193, 0.99121, 0.99052, 0.98988, 0.98926, 0.98867,  &
		    0.98810, 0.98756 /)
	    dd(7,:)=(/ &
		    0.10000, 0.17368, 0.24737, 0.32105, 0.39474, 0.46842,  &
		    0.54211, 0.61579, 0.68947, 0.76316, 0.83684, 0.91053,  &
		    0.98421, 1.05789, 1.13158, 1.20526, 1.27895, 1.35263,  &
		    1.42632, 1.50000 /)
	    dd(8,:)=(/ &
		    0.09403, 0.16542, 0.23861, 0.31328, 0.38901, 0.46540,  &
		    0.54203, 0.61849, 0.69437, 0.76928, 0.84282, 0.91458,  &
		    0.98470, 1.05517, 1.12536, 1.19531, 1.26502, 1.33450,  &
		    1.40376, 1.47282 /)
	    dd(9,:)=(/ &
		    0.10659, 0.18303, 0.25848, 0.33316, 0.40722, 0.48075,  &
		    0.55381, 0.62646, 0.69873, 0.77066, 0.84228, 0.91360,  &
		    0.98464, 1.05542, 1.12596, 1.19627, 1.26637, 1.33625,  &
		    1.40593, 1.47542 /)
	    dd(10,:)=(/ &
		    0.20000, 0.25263, 0.30526, 0.35789, 0.41053, 0.46316,  &
		    0.51579, 0.56842, 0.62105, 0.67368, 0.72632, 0.77895,  &
		    0.83158, 0.88421, 0.93684, 0.98947, 1.04211, 1.09474,  &
		    1.14737, 1.20000 /)
	    dd(11,:)=(/ &
		    1.03323, 1.04058, 1.04456, 1.04544, 1.04357, 1.03926,  &
		    1.03282, 1.02446, 1.01554, 1.00944, 1.00487, 1.00169,  &
		    0.99986, 0.99926, 0.99980, 1.00027, 1.00021, 1.00015,  &
		    1.00006, 0.99995 /)
	    dd(12,:)=(/ &
		    0.98344, 0.98630, 0.98876, 0.99081, 0.99247, 0.99379,  &
		    0.99486, 0.99574, 0.99649, 0.99716, 0.99774, 0.99826,  &
		    0.99877, 0.99926, 0.99972, 1.00017, 1.00060, 1.00103,  &
		    1.00143, 1.00182 /)
	    dd(13,:)=(/ &
		    3000.00, 4263.16, 5526.32, 6789.47, 8052.63, 9315.79,  &
		    10578.95, 11842.11, 13105.26, 14368.42, 15631.58, 16894.74,  &
		    18157.89, 19421.05, 20684.21, 21947.37, 23210.53, 24473.68,  &
		    25736.84, 27000.00 /)
	    dd(14,:)=(/ &
		    0.99269, 0.99520, 0.99718, 0.99882, 1.00024, 1.00150,  &
		    1.00264, 1.00368, 1.00464, 1.00554, 1.00637, 1.00716,  &
		    1.00790, 1.00840, 1.00905, 1.00965, 1.01022, 1.01075,  &
		    1.01126, 1.01173 /)
	    dd(15,:)=(/ &
		    0.99768, 0.99861, 0.99933, 0.99992, 1.00043, 1.00087,  &
		    1.00127, 1.00164, 1.00197, 1.00227, 1.00255, 1.00282,  &
		    1.00307, 1.00331, 1.00353, 1.00375, 1.00395, 1.00415,  &
		    1.00433, 1.00451 /)
	    dd(16,:)=(/ &
		    0.10000, 0.17368, 0.24737, 0.32105, 0.39474, 0.46842,  &
		    0.54211, 0.61579, 0.68947, 0.76316, 0.83684, 0.91053,  &
		    0.98421, 1.05789, 1.13158, 1.20526, 1.27895, 1.35263,  &
		    1.42632, 1.50000 /)
	    dd(17,:)=(/ &
		    1.00812, 1.00513, 1.00294, 1.00128, 0.99980, 0.99901,  &
		    0.99855, 0.99836, 0.99846, 0.99883, 0.99944, 1.00033,  &
		    1.00042, 1.00056, 1.00069, 1.00081, 1.00093, 1.00104,  &
		    1.00115, 1.00125 /)
	    dd(18,:)=(/ &
		    1.09816, 1.07859, 1.06487, 1.05438, 1.04550, 1.03816,  &
		    1.03159, 1.02579, 1.02061, 1.01587, 1.01157, 1.00751,  &
		    1.00380, 1.00033, 0.99705, 0.99400, 0.99104, 0.98832,  &
		    0.98565, 0.98316 /)
    elseif (TT==3) then !Sliding pressure power cycle formulation mjw 3.31.11/ Bug fix 7.22.11 - cond pressure units incorrect for items 4 and 13
        allocate(dd(18,10))
	    dd(1,:)=(/ &
		    0.10000, 0.21111, 0.32222, 0.43333, 0.54444, 0.65556,  &
		    0.76667, 0.87778, 0.98889, 1.10000 /)
	    dd(2,:)=(/ &
		    0.89280, 0.90760, 0.92160, 0.93510, 0.94820, 0.96110,  &
		    0.97370, 0.98620, 0.99860, 1.01100 /)
	    dd(3,:)=(/ &
		    0.93030, 0.94020, 0.94950, 0.95830, 0.96690, 0.97520,  &
		    0.98330, 0.99130, 0.99910, 1.00700 /)
	    dd(4,:)=(/ &    
		    4000.00, 6556.00, 9111.00, 11677.0, 14222.0, 16778.0,  &
		    19333.0, 21889.0, 24444.0, 27000.0 /)
	    dd(5,:)=(/ &
		    1.04800, 1.01400, 0.99020, 0.97140, 0.95580, 0.94240,  &
		    0.93070, 0.92020, 0.91060, 0.90190 /)
	    dd(6,:)=(/ &
		    0.99880, 0.99960, 1.00000, 1.00100, 1.00100, 1.00100,  &
		    1.00100, 1.00200, 1.00200, 1.00200 /)
	    dd(7,:)=(/ &
		    0.20000, 0.31667, 0.43333, 0.55000, 0.66667, 0.78333,  &
		    0.90000, 1.01667, 1.13333, 1.25000 /)
	    dd(8,:)=(/ &
		    0.16030, 0.27430, 0.39630, 0.52310, 0.65140, 0.77820,  &
		    0.90060, 1.01600, 1.12100, 1.21400 /)
	    dd(9,:)=(/ &
		    0.22410, 0.34700, 0.46640, 0.58270, 0.69570, 0.80550,  &
		    0.91180, 1.01400, 1.11300, 1.20700 /)
	    dd(10,:)=(/ &
		    0.10000, 0.21111, 0.32222, 0.43333, 0.54444, 0.65556,  &
		    0.76667, 0.87778, 0.98889, 1.10000 /)
	    dd(11,:)=(/ &
		    1.05802, 1.05127, 1.04709, 1.03940, 1.03297, 1.02480,  &
		    1.01758, 1.00833, 1.00180, 0.99307 /)
	    dd(12,:)=(/ &
		    1.03671, 1.03314, 1.02894, 1.02370, 1.01912, 1.01549,  &
		    1.01002, 1.00486, 1.00034, 0.99554 /)
	    dd(13,:)=(/ &
		    4000.00, 6556.00, 9111.00, 11677.0, 14222.0, 16778.0,  &
		    19333.0, 21889.0, 24444.0, 27000.0 /)
	    dd(14,:)=(/ &
		    1.00825, 0.98849, 0.99742, 1.02080, 1.02831, 1.03415,  &
		    1.03926, 1.04808, 1.05554, 1.05862 /)
	    dd(15,:)=(/ &
		    !tweaked entry #4 to be the average of 3 and 5. it was an outlier in the simulation. mjw 3.31.11
		    1.01838, 1.02970, 0.99785, 0.99663, 0.99542, 0.99183,  &  
		    0.98897, 0.99299, 0.99013, 0.98798 /)
	    dd(16,:)=(/ &
		    0.20000, 0.31667, 0.43333, 0.55000, 0.66667, 0.78333,  &
		    0.90000, 1.01667, 1.13333, 1.25000 /)
	    dd(17,:)=(/ &
		    1.43311, 1.27347, 1.19090, 1.13367, 1.09073, 1.05602,  &
		    1.02693, 1.00103, 0.97899, 0.95912 /)
	    dd(18,:)=(/ &
	        !tweaked entry #9 to be the average of 8 and 10. it was an outlier in the simulation mjw 3.31.11
		    0.48342, 0.64841, 0.64322, 0.74366, 0.76661, 0.82764,  &
		    0.97792, 1.15056, 1.23117, 1.31179 /)
    elseif (TT==4) then !Geothermal Isopentane rankine cycle
        call messages(-1,"Geothermal isopentane cycle is not included in Type234. Use Type224 instead.","FATAL",234,0)
        return
    elseif (TT==5) then !reheat cycle
        allocate(dd(24,20))
	    	dd(1,:)=(/ &
		        0.20000, 0.25263, 0.30526, 0.35789, 0.41053, 0.46316,  &
		        0.51579, 0.56842, 0.62105, 0.67368, 0.72632, 0.77895,  &
		        0.83158, 0.88421, 0.93684, 0.98947, 1.04211, 1.09474,  &
		        1.14737, 1.20000 /)
	        dd(2,:)=(/ &
		        0.74230, 0.76080, 0.77870, 0.79620, 0.81340, 0.83050,  &
		        0.84740, 0.86440, 0.88120, 0.89780, 0.91440, 0.93090,  &
		        0.94730, 0.96380, 0.98030, 0.99670, 1.01300, 1.03000,  &
		        1.04700, 1.06300 /)
	        dd(3,:)=(/ &
		        0.80770, 0.82580, 0.84220, 0.85740, 0.87170, 0.88510,  &
		        0.89800, 0.91030, 0.92220, 0.93380, 0.94500, 0.95600,  &
		        0.96670, 0.97730, 0.98770, 0.99790, 1.00800, 1.01800,  &
		        1.02800, 1.03800 /)
	        dd(4,:)=(/ &
		        0.86950, 0.85270, 0.84380, 0.84070, 0.84210, 0.84730,  &
		        0.85550, 0.86630, 0.87890, 0.89280, 0.90780, 0.92390,  &
		        0.94090, 0.95860, 0.97710, 0.99620, 1.01600, 1.03600,  &
		        1.05700, 1.07800 /)
	        dd(5,:)=(/ &
		        4000.00, 5368.42, 6736.84, 8105.26, 9473.68, 10842.11,  &
		        12210.53, 13578.95, 14947.37, 16315.79, 17684.21, 19052.63,  &
		        20421.05, 21789.47, 23157.89, 24526.32, 25894.74, 27263.16,  &
		        28631.58, 30000.00 /)
	        dd(6,:)=(/ &
		        1.03800, 1.02200, 1.01000, 0.99930, 0.99030, 0.98230,  &
		        0.97520, 0.96870, 0.96280, 0.95730, 0.95220, 0.94740,  &
		        0.94300, 0.93870, 0.93480, 0.93100, 0.92730, 0.92390,  &
		        0.92060, 0.91740 /)
	        dd(7,:)=(/ &
		        1.00100, 1.00100, 1.00000, 0.99990, 0.99950, 0.99920,  &
		        0.99890, 0.99860, 0.99840, 0.99810, 0.99790, 0.99770,  &
		        0.99750, 0.99730, 0.99720, 0.99700, 0.99690, 0.99670,  &
		        0.99660, 0.99640 /)
	        dd(8,:)=(/ &
		        0.99430, 0.99670, 0.99860, 1.00000, 1.00200, 1.00300,  &
		        1.00400, 1.00500, 1.00600, 1.00700, 1.00700, 1.00800,  &
		        1.00900, 1.01000, 1.01000, 1.01100, 1.01100, 1.01200,  &
		        1.01200, 1.01300 /)
	        dd(9,:)=(/ &
		        0.10000, 0.15263, 0.20526, 0.25789, 0.31053, 0.36316,  &
		        0.41579, 0.46842, 0.52105, 0.57368, 0.62632, 0.67895,  &
		        0.73158, 0.78421, 0.83684, 0.88947, 0.94211, 0.99474,  &
		        1.04737, 1.10000 /)
	        dd(10,:)=(/ &
		        0.08098, 0.12760, 0.17660, 0.22780, 0.28070, 0.33520,  &
		        0.39090, 0.44730, 0.50030, 0.55780, 0.61510, 0.67200,  &
		        0.72820, 0.78370, 0.83820, 0.89160, 0.94400, 0.99500,  &
		        1.04500, 1.09300 /)
	        dd(11,:)=(/ &
		        0.10940, 0.16700, 0.22450, 0.28210, 0.33970, 0.39730,  &
		        0.45490, 0.51240, 0.56520, 0.61620, 0.66630, 0.71560,  &
		        0.76420, 0.81190, 0.85890, 0.90520, 0.95070, 0.99550,  &
		        1.04000, 1.08300 /)
	        dd(12,:)=(/ &
		        0.07722, 0.12110, 0.16690, 0.21440, 0.26350, 0.31380,  &
		        0.36520, 0.41720, 0.44960, 0.50670, 0.56500, 0.62440,  &
		        0.68480, 0.74580, 0.80740, 0.86940, 0.93160, 0.99390,  &
		        1.05600, 1.11800 /)
	        dd(13,:)=(/ &
		        0.20000, 0.25263, 0.30526, 0.35789, 0.41053, 0.46316,  &
		        0.51579, 0.56842, 0.62105, 0.67368, 0.72632, 0.77895,  &
		        0.83158, 0.88421, 0.93684, 0.98947, 1.04211, 1.09474,  &
		        1.14737, 1.20000 /)
	        dd(14,:)=(/ &
		        0.98450, 0.98907, 0.99169, 0.99305, 0.99451, 0.99467,  &
		        0.99555, 0.99573, 0.99599, 0.99695, 0.99727, 0.99763,  &
		        0.99802, 0.99778, 0.99755, 0.99790, 1.00412, 1.00094,  &
		        0.99786, 1.00541 /)
	        dd(15,:)=(/ &
		        0.96043, 0.96885, 0.97567, 0.98076, 0.98406, 0.98772,  &
		        0.99019, 0.99223, 0.99443, 0.99800, 0.99722, 0.99780,  &
		        1.00043, 0.99745, 1.00199, 1.00164, 1.00199, 1.00298,  &
		        0.99803, 0.99903 /)
	        dd(16,:)=(/ &
		        1.13799, 1.12215, 1.10761, 1.09474, 1.08321, 1.07117,  &
		        1.06082, 1.05054, 1.04231, 1.03449, 1.03001, 1.02000,  &
		        1.01481, 1.01075, 1.00730, 1.00112, 0.99609, 0.99440,  &
		        0.98833, 0.98641 /)
	        dd(17,:)=(/ &
		        4000.00, 5368.42, 6736.84, 8105.26, 9473.68, 10842.11,  &
		        12210.53, 13578.95, 14947.37, 16315.79, 17684.21, 19052.63,  &
		        20421.05, 21789.47, 23157.89, 24526.32, 25894.74, 27263.16,  &
		        28631.58, 30000.00 /)
	        dd(18,:)=(/ &
		        0.99027, 1.00027, 1.00141, 1.00607, 1.00445, 1.01006,  &
		        1.00791, 1.01656, 1.02015, 1.02004, 1.02471, 1.02659,  &
		        1.02510, 1.02729, 1.02884, 1.03094, 1.03294, 1.03397,  &
		        1.03576, 1.03745 /)
	        dd(19,:)=(/ &
		        0.99692, 0.99900, 0.99870, 1.00116, 0.99392, 0.99817,  &
		        1.00268, 0.99376, 0.99685, 1.00111, 1.00394, 0.99386,  &
		        0.99669, 0.99953, 1.00121, 1.00405, 1.00573, 0.99511,  &
		        0.99679, 0.99964 /)
	        dd(20,:)=(/ &
		        1.02527, 1.01447, 1.00819, 0.99962, 0.99609, 0.99128,  &
		        0.98686, 0.98321, 0.97395, 0.97108, 0.97250, 0.97040,  &
		        0.96232, 0.96099, 0.95682, 0.95587, 0.95208, 0.95152,  &
		        0.94811, 0.94756 /)
	        dd(21,:)=(/ &
		        0.10000, 0.15263, 0.20526, 0.25789, 0.31053, 0.36316,  &
		        0.41579, 0.46842, 0.52105, 0.57368, 0.62632, 0.67895,  &
		        0.73158, 0.78421, 0.83684, 0.88947, 0.94211, 0.99474,  &
		        1.04737, 1.10000 /)
	        dd(22,:)=(/ &
		        1.09510, 1.10157, 1.10663, 1.10675, 1.11166, 1.10757,  &
		        1.10278, 1.13312, 1.14401, 1.11992, 1.09445, 1.07305,  &
		        1.05616, 1.03925, 1.02666, 1.01617, 1.00719, 1.00320,  &
		        1.00355, 1.00405 /)
	        dd(23,:)=(/ &
		        0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,  &
		        0.00000, 0.35237, 0.98785, 0.97371, 1.07142, 1.10630,  &
		        1.09046, 1.18378, 1.20956, 1.17838, 1.21840, 1.24170,  &
		        1.01496, 1.43634 /)
	        dd(24,:)=(/ &
		        3.04989, 3.04563, 3.06817, 3.07452, 3.07856, 3.06611,  &
		        3.07191, 1.77603, 0.66771, 0.72244, 0.75574, 0.79547,  &
		        0.83991, 0.86983, 0.90542, 0.93997, 0.96970, 0.98014,  &
		        1.00831, 1.06526 /)

       
    else
        !Read the coefficients from a user-file
        !Do a test read to make sure the file is available
        read(LU_pb,fmt="(A)",advance="NO",err=200,eor=200)
        rewind(LU_pb)
        goto 205

        200 continue !There was a problem
            call messages(-1,"Power block coefficient file was not found","FATAL",234,0)    
            stop
        205 continue

        !Continue reading the file
        !how long is the string containing values? We need to determine the number of entries
        line=''
        read(LU_pb,fmt='(A)') line

        !SAM is expecting each entry to be 8 long, with 1 column of header info
        n=(len(trim(line))+1)/8-1

        !Now allocate the db array
        if(allocated(dd)) deallocate(dd)
        allocate(dd(18,n))

        !Rewind and read in the file. There are 18 effects, 1st column is label text
        rewind(LU_pb)
        do i=1,18
            read(LU_pb,fmt="(8X)",advance="NO")
            do j=1,n
                read(LU_pb,fmt="(F8.5)",advance="NO") dd(i,j)
            enddo
            read(LU_pb,fmt="(X)",advance="YES")
        enddo

        close(LU_pb)
        
    endif

    !Now select which to interpolate
    n=size(dd(1,:))  !All of the sub arrays should be of the same length

    !Allocate data arrays
    if(.not.allocated(datx)) allocate(datx(n),daty(n))


    endif 


    if(TT/=5) then
        select case(XT)
        case(1) ; XI=1    !A
        case(2) ; XI=4    !B
        case(3) ; XI=7    !C

        case(13) ; XI=10  !AC
        case(12) ; XI=13  !AB
        case(23) ; XI=16  !BC
        end select

        select case(YT)
        case(11) ;  YI=2      !PA
        case(12) ;  YI=5      !PB
        case(13) ;  YI=8      !PC
        case(112) ;  YI=14    !PAB
        case(113) ;  YI=11    !PAC
        case(123) ;  YI=17    !PBC
        case(21) ;  YI=3      !QA
        case(22) ;  YI=6      !QB
        case(23) ;  YI=9      !QC
        case(212) ;  YI=15    !QAB
        case(213) ;  YI=12    !QAC
        case(223) ;  YI=18    !QBC
        end select
    else
        select case(XT)
        case(1) ; XI = 1
        case(2) ; XI = 5
        case(3) ; XI = 9
        case(13) ; XI = 13
        case(12) ; XI = 17
        case(23) ; XI = 21
        end select
        
        select case(YT)
        case(11) ; YI=2        !PA
        case(12) ; YI=6        !PB
        case(13) ; YI=10       !PC
        case(112) ; YI=18      !PAB
        case(113) ; YI=14      !PAC
        case(123) ; YI=22      !PBC
        case(21) ; YI=3        !QA
        case(22) ; YI=7        !QB
        case(23) ; YI=11       !QC
        case(212) ; YI=19      !QAB
        case(213) ; YI=15      !QAC
        case(223) ; YI=23      !QBC
        case(31) ; YI=4        !RA
        case(32) ; YI=8        !RB
        case(33) ; YI=12       !RC
        case(312) ; YI=20      !RAB
        case(313) ; YI=16      !RAC
        case(323) ; YI=24      !RBC
        end select
    
    endif

    !Set the data to be interpolated
    datx(1:n)=dd(XI,1:n)
    daty(1:n)=dd(YI,1:n)
    	
    !Use brute force interpolation.. it is faster in this case than
    !..bisection or hunting methods used in the user-specified HTF case		
    do i=1,n
        !if the index is equal to n, then set bounds and end loop
        if(i.eq.n) then
            lbi=n
            ubi=n
            exit
        endif

        !if the x variable is outside the table range, set the bounds and get out
        if(i.eq.1) then
            if(datx(2).gt.datx(1)) then !The table is in ascending order
                if(X.le.datx(1)) then
                    lbi=1; ubi=1; exit;
                endif
                if(X.ge.datx(n)) then
                    lbi=n; ubi=n; exit;
                endif
            else  !the table is in descending order
                if(x.ge.datx(1)) then
                    lbi=1; ubi=1; exit;
                endif
                if(X.le.datx(n)) then
                    lbi=n; ubi=n; exit;
                endif
            endif
        endif


        if(((X.ge.datx(i)).and.(X.lt.datx(i+1))).or.((X.le.datx(i)).and.(X.gt.datx(i+1)))) then
            lbi=i
            ubi=i+1
            exit
        endif
    enddo

    if(datx(ubi).eq.datx(lbi)) then
        ind=0.
    else
        ind=(X-datx(lbi))/(datx(ubi)-datx(lbi))
    endif

    Y = daty(lbi)+ind*(daty(ubi)-daty(lbi))

    CycleMap_DSG=Y


    end function CycleMap_DSG 

