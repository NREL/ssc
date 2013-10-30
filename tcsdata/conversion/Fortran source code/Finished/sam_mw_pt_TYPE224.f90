SUBROUTINE TYPE224 (TIME,XIN,OUT,T,DTDT,PAR,INFO,ICNTRL,*) 
!************************************************************************
! Object: Solar Power Tower Rankine Component
! Simulation Studio Model: TYPE224
 
! Author: Michael J. Wagner
! Editor: -
! Date:	 July 03, 2008 
! Last modified: March 30, 2011
! COPYRIGHT 2010 NATIONAL RENEWABLE ENERGY LABORATORY

! Doc. tables updated 2011-11-01 - MJW
!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Parameters
!    1| P_ref                            | Reference output electric power at design condition               | MW               | kW               
!    2| eta_ref                          | Reference conversion efficiency at design condition               | none             | none             
!    3| T_htf_hot_ref                    | Reference HTF inlet temperature at design                         | C                | K                
!    4| T_htf_cold_ref                   | Reference HTF outlet temperature at design                        | C                | K                
!    5| dT_cw_ref                        | Reference condenser cooling water inlet/outlet T diff             | C                | C                
!    6| T_amb_des                        | Reference ambient temperature at design point                     | C                | C                
!    7| HTF                              | Integer flag identifying HTF in power block                       | none             | none             
!    8| q_sby_frac                       | Fraction of thermal power required for standby mode               | none             | none             
!    9| P_boil                           | Boiler operating pressure                                         | bar              | bar              
!   10| CT                               | Flag for using dry cooling or wet cooling system                  | none             | none             
!   11| startup_time                     | Time needed for power block startup                               | hr               | hr               
!   12| startup_frac                     | Fraction of design thermal power needed for startup               | none             | none             
!   13| tech_type                        | Flag indicating which coef. set to use. (1=tower,2=trough,3=user) | none             | none             
!   14| T_approach                       | Cooling tower approach temperature                                | C                | C                
!   15| T_ITD_des                        | ITD at design for dry system                                      | C                | C                
!   16| P_cond_ratio                     | Condenser pressure ratio                                          | none             | none             
!   17| pb_bd_frac                       | Power block blowdown steam fraction                               | none             | none             
!   18| LU_pb                            | Logical unit for the power block file                             | none             | none             
!   19| P_cond_min                       | Minimum condenser pressure                                        | inHg             | Pa               
!   20| n_pl_inc                         | Number of part-load increments for the heat rejection system      | none             | none             
!   21| F_wc(1-9)                        | Fraction indicating wet cooling use for hybrid system             | none             | none             

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Inputs
!    1| mode                             | Cycle part load control, from plant controller                    | none             | none             
!    2| T_htf_hot                        | Hot HTF inlet temperature, from storage tank                      | C                | K                
!    3| m_dot_htf                        | HTF mass flow rate                                                | kg/hr            | kg/hr            
!    4| T_wb                             | Ambient wet bulb temperature                                      | C                | C                
!    5| demand_var                       | Control signal indicating operational mode                        | none             | none             
!    6| standby_control                  | Control signal indicating standby mode                            | none             | none             
!    7| T_db                             | Ambient dry bulb temperature                                      | C                | C                
!    8| P_amb                            | Ambient pressure                                                  | atm              | Pa               
!    9| TOU                              | Current Time-of-use period                                        | none             | none             
!   10| rh                               | Relative humidity of the ambient air                              | none             | none             

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Outputs
!    1| P_cycle                          | Cycle power output                                                | MWe              | kWe              
!    2| eta                              | Cycle thermal efficiency                                          | none             | none             
!    3| T_htf_cold                       | Heat transfer fluid outlet temperature                            | C                | C                
!    4| m_dot_makeup                     | Cooling water makeup flow rate                                    | kg/hr            | kg/s             
!    5| m_dot_demand                     | HTF required flow rate to meet power load                         | kg/hr            | kg/hr            
!    6| m_dot_htf                        | Actual HTF flow rate passing through the power cycle              | kg/hr            | kg/hr            
!    7| m_dot_htf_ref                    | Calculated reference HTF flow rate at design                      | kg/hr            | kg/hr            
!    8| W_cool_par                       | Cooling system parasitic load                                     | MWe              | MWe              
!    9| P_ref                            | Reference power level output at design (mirror param)             | MWe              | kWe              
!   10| f_bays                           | Fraction of operating heat rejection bays                         | none             | none             
!   11| P_cond                           | Condenser pressure                                                | Pa               | Pa               


!************************************************************************

!    TRNSYS acess functions (allow to acess TIME etc.) 
USE TrnsysConstants
USE TrnsysFunctions
!USE TrnsysData, ONLY:OPENED_LU, JSTORE_LU
use global_props  !user global module
USE CSP_cooling_functions   !8/27/11

!REQUIRED BY THE MULTI-DLL VERSION OF TRNSYS
!DEC$ATTRIBUTES DLLEXPORT :: TYPE224				!SET THE CORRECT TYPE NUMBER HERE

!-----------------------------------------------------------------------------------------------------------------------
implicit none !real(8) (a-z) 

!     Parameters
real(8):: P_ref, eta_ref,  T_htf_hot_ref, HTF, T_htf_cold_ref, dT_cw_ref, q_sby_frac, P_boil,Is_dry_bulb, &
          startup_time, startup_frac, T_amb_des, T_approach, T_ITD_des, P_cond_ratio, CT, P_cond_min, n_pl_inc, F_wc(9)

!    Inputs
real(8):: mode, T_htf_hot, T_wb, T_db, m_dot_cw, demand_var, m_dot_htf,standby_control, last_P_cycle, P_amb
 
!    Outputs / Local variables
real(8):: P_cycle, eta, T_htf_cold, m_dot_demand,m_dot_htf_ref, q_ND_tot, c_p_w, &
          q_sby_needed, m_dot_sby, c_htf, q_tot, specheat, T_cw_in, last_standby_control, tstep, startup_remain ,&
          tech_type, W_cool_par, m_dot_makeup, F_wcmin, F_wcmax, P_cond, startup_energy, startup_e_remain, &
          startup_e_used, Q_cycle, f_st, rh


real(8):: xin, out, time, par, stored, T, dTdt, prop(7), pb_bd_frac, h_st_hot, h_st_cold, dh_steam, m_dot_st_bd, f_hrsys, fcall
integer(4)::INFO(15), np, ni, nout, nd, npar, nin, nder, iunit, itype, icntrl, nstored, ierr, TOU, i
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!SET THE MAXIMUM NUMBER OF PARAMETERS (NP), INPUTS (NI),OUTPUTS (NOUT), AND DERIVATIVES (ND) 
PARAMETER (NP=29,NI=10,NOUT=11,ND=0,NSTORED=5)

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
    if(standby_control /= 1.) T_htf_hot = T_htf_hot + 273.15d0  !mjw 3.10.11 If the main cycle subroutine isn't called, adjust the temperature for the unit check
    call check_htf(HTF,T_htf_hot) !T_htf_hot is in [K]
    call check_htf(HTF,T_htf_cold+273.15d0) !T_htf_cold is in [C]

    RETURN 1
ENDIF
!
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!    DO ALL THE VERY FIRST CALL OF THE SIMULATION MANIPULATIONS HERE
IF (INFO(7).EQ.-1) THEN
tstep=getSimulationTimeStep()

    !-----------------------------------------------------------------------------------------------------------------------
    !READ IN THE VALUES OF THE PARAMETERS IN SEQUENTIAL ORDER
    P_ref = PAR(1)*1000. !Convert from MW to kW
    eta_ref = PAR(2)
    T_htf_hot_ref = PAR(3)
    T_htf_cold_ref = PAR(4)
    dT_cw_ref = PAR(5)
    T_amb_des = PAR(6)
    HTF=PAR(7)
    q_sby_frac=PAR(8)
    P_boil=PAR(9)
    CT=PAR(10) !Cooling type
    startup_time=PAR(11)
    startup_frac=PAR(12)
    tech_type=PAR(13)
    T_approach=PAR(14)
    T_ITD_des=PAR(15)
    P_cond_ratio=PAR(16)
    pb_bd_frac=PAR(17)
    LU_pb = int(PAR(18))  !This sets the value in the global_props module
    P_cond_min = PAR(19)*3386. !Convert inHg to Pa
    n_pl_inc = PAR(20)
    !read in the hybrid cooling dispatch fractions
    do i=1,9
        F_wc(i)=dmin1(1.d0,dmax1(PAR(20+i),0.d0))
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
    OUT(3)=T_htf_hot
    
    !Calculate the power block side steam enthalpy rise for blowdown calculations
    !Steam properties are as follows:
    !=======================================================================
    ! | T(C) | P(MPa) | h(kJ/kg) | s(kJ/kg-K) | x(-) | v(m3/kg) | U(kJ/kg) |
    !=======================================================================
    !Limit the boiler pressure to below the supercritical point.  If a supercritical pressure is used,
    !notify the user that the pressure value is being switched.
    if(P_boil > 220.) then
        P_boil = 220.  !Set to 220 bar
        call Messages(-1,"Boiler pressure provided by the user requires a supercritical system. The pressure value has been reset to 215 bar.",'Warning',info(1),info(2)) 
    endif
            
    !hot steam
    prop = (/(T_htf_hot_ref - 25.d0),(P_boil*0.1d0),1.d0,1.d0,1.d0,1.d0,1.d0/)
    call steam_props("SI",prop,12,ierr)
    h_st_hot = prop(3) !Use the hot steam enthalpy
    prop = (/1.d0,(P_boil*0.1d0),1.d0,1.d0,0.d0,1.d0,1.d0/)
    call steam_props("SI",prop,25,ierr)
    !Use the cold steam enthalpy at x=0, subtract subcooled enthalpy with specific heat. Cp is based
    !on an integral of specific heat for water in the slightly subcooled range of ~100degC
    h_st_cold = prop(3) - 4.91*100. 
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
mode = XIN(1)
T_htf_hot = XIN(2)
m_dot_htf = XIN(3)
T_wb = XIN(4) + 273.15      !Convert to K
demand_var = XIN(5)
standby_control = XIN(6)
T_db = XIN(7) + 273.15      !Convert to K
P_amb = xin(8)*101300. ![atm] -> [Pa]
TOU = int(xin(9))
rh = xin(10)  !relative humidity
    IUNIT=INFO(1)
    ITYPE=INFO(2)
    if(mode.eq.1.) demand_var = demand_var*1000.  !If the mode is to operate in power demand, convert from MW to kW
if(info(7)>10) goto 900 !MJW 12.10.2010 Don't recalculate
!Specific heat of water
c_p_w=4.183  ![kJ/kg-C]
m_dot_st_bd = 0.d0
      
select case (int(standby_control))
case(1)  !The cycle is in normal operation

    call RankineCycle(time,P_ref, eta_ref, T_htf_hot_ref, T_htf_cold_ref, T_db, T_wb, P_amb, dT_cw_ref, HTF, c_p_w, &
                      T_htf_hot, m_dot_htf, mode, demand_var,P_boil, tech_type,T_amb_des, T_approach, F_wc(TOU), F_wcmin, F_wcmax, &
                      T_ITD_des, P_cond_ratio, CT, P_cond_min, n_pl_inc, fcall, P_cycle, eta, T_htf_cold, m_dot_demand, m_dot_htf_ref, &
                      m_dot_makeup, W_cool_par, f_hrsys, P_cond)
    !Check the output to make sure it's reasonable. If not, return zeros.
    if(((eta>1.).or.(eta<0.)).or.((T_htf_cold > T_htf_hot).or.(T_htf_cold < T_htf_cold_ref - 50.))) then
        P_cycle = 0.d0
        eta = 0.d0
        T_htf_cold = T_htf_cold_ref
    endif
    
    !-----Calculate the blowdown fraction-----
    if(tech_type<4.) then
        m_dot_st_bd = P_cycle/dmax1((eta*dh_steam),1.e-6)*pb_bd_frac
    else
        m_dot_st_bd = 0.
    endif
    
    if(ErrorFound()) return 1

case(2)  !The cycle is in standby operation
    c_htf=specheat(HTF,(T_htf_hot+T_htf_cold_ref)/2.+273.15,1.d0)
    q_tot = P_ref/eta_ref

    !Calculate the actual q_sby_needed from the reference flows
    q_sby_needed = q_tot*q_sby_frac

    !now calculate the mass flow rate knowing the inlet temperature of the salt,
    !..and holding the outlet temperature at the reference outlet temperature
    m_dot_sby = q_sby_needed/(c_htf*(T_htf_hot - T_htf_cold_ref))*3600.

    
    !Set other output values
    P_cycle = 0.
    eta = 0.
    T_htf_cold = T_htf_cold_ref
    m_dot_demand = m_dot_sby
    m_dot_makeup = 0.
    W_cool_par = 0.
    f_hrsys = 0.
    P_cond = 0.
case(3)  !The cycle has been completely shut down
    P_cycle = 0.
    eta = 0.
    T_htf_cold = T_htf_cold_ref  !Changed from T_htf_hot 12/18/2009 was causing problems with T250
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

        eta = eta_ref
        T_htf_cold = T_htf_cold_ref
        m_dot_htf_ref = m_dot_htf_ref
        
        startup_remain = dmax1(startup_remain-tstep,0.d0)
        startup_e_remain= dmax1(startup_e_remain - startup_e_used, 0.d0)
    endif
else
    startup_e_used = 0.d0
endif
      
!SET THE OUTPUTS FROM THIS MODEL
900 continue !MJW 12.10.2010
!Cycle power output
OUT(1)=P_cycle/1000.  !Convert from kW to MW 
!Cycle thermal efficiency
OUT(2)=eta
!Heat transfer fluid outlet temp
OUT(3)=T_htf_cold
!Wet cooling makeup water flow rate
OUT(4)=(m_dot_makeup + m_dot_st_bd)*3600.
!Heat transfer fluid demand flow rate
OUT(5)=m_dot_demand
!Heat transfer fluid flow rate
OUT(6)=m_dot_htf			
!Calculated reference htf flow rate			
OUT(7)=m_dot_htf_ref
!Cooling tower parasitic load [MW]
OUT(8)=W_cool_par
!Reference power level output
OUT(9)=P_ref/1000.   !Convert from kW to MW
!Fraction of cooling system in operation
OUT(10)=f_hrsys
!Condenser pressure (Pa)
OUT(11)=P_cond

!if (OPENED_LU(JSTORE_LU) .eq. 99) then
!    WRITE(OPENED_LU(JSTORE_LU),'(I4,A1,I4,A1,50(E25.16,A1))',ADVANCE='NO') int(TIME),Char(9), Info(7),Char(9), (XIN(i),Char(9),i=1,NI)
!    WRITE(OPENED_LU(JSTORE_LU),'(50(E25.16,A1))',ADVANCE='YES') (OUT(i),Char(9),i=1,NOUT)
!endif

    RETURN 1
END
!-----------------------------------------------------------------------------------------------------------------------


!************************************************************************************************************

subroutine RankineCycle(time,P_ref, eta_ref, T_htf_hot_ref, T_htf_cold_ref, T_db, T_wb, P_amb, dT_cw_ref, HTF, c_p_w, &
                        T_htf_hot, m_dot_htf, mode, demand_var,P_boil, tech_type,T_amb_des, T_approach, F_wc, F_wcmin, F_wcmax,&
                        T_ITD_des, P_cond_ratio, CTr, P_cond_min, n_pl_inc, fcall, P_cycle, eta, T_htf_cold, m_dot_demand, &
                        m_dot_htf_ref, m_dot_makeup,W_cool_par, f_hrsys, P_cond)
use CSP_cooling_functions
use trnsysfunctions
use CSP_HR_mod
implicit none 

real(8),intent(inout)::fcall
real(8):: P_ref, eta_ref, T_htf_hot_ref, T_htf_cold_ref, T_cw_in, dT_cw_ref, c_htf, HTF, c_htf_ref, &
          T_htf_hot, m_dot_htf, m_dot_cw, mode, demand_var, P_boil, tech_type,T_amb_des,q_dot_ref, &
          c_p_w, T_htf_hot_ND, m_dot_htf_ND, m_dot_cw_ND,adj, err, P_dem_ND, P_ND_tot, Q_ND_tot, CTr
real(8):: P_ND(3), Q_ND(3), T_ref, specheat, P_AB, P_CA, P_BC, Q_AB, Q_CA, Q_BC, &
          W_cool_par, P_cond, T_cond, q_reject_est, m_dot_makeup, q_reject, m_dot_air, T_ITD_des, P_cond_ratio,&
          T_approach, P_amb, T_db, T_wb, time, W_cool_parhwc, W_cool_parhac, F_wc, F_wcmin, F_wcmax, f_hrsys, &
          P_cond_min, n_pl_inc, eta_adj, rand, xrand,Psat_ref
save:: eta_adj
!Declare inputs
integer::i, qq, TT, CT
logical:: float_pres
!Declare output variables
real(8),intent(out):: P_cycle, eta, T_htf_cold, m_dot_demand, m_dot_htf_ref

!______________________________________________________________________________

!Select the technology type
!	tech_type=1. !TEMP
if(tech_type.eq.1.) TT=1  !high temp
if(tech_type.eq.2.) TT=2  !low temp
if(tech_type.eq.3.) TT=3  !user coefficients
if(tech_type.eq.4.) TT=4  !isopentane Rankine cycle for geothermal

!The user provides a reference efficiency, ambient temperature, and cooling system parameters. Using
!this information, we have to adjust the provided reference efficiency to match the normalized efficiency
!that is part of the power block regression coefficients. I.e. if the user provides a ref. ambient temperature
!of 25degC, but the power block coefficients indicate that the normalized efficiency equals 1.0 at an ambient 
!temp of 20degC, we have to adjust the user's efficiency value back to the coefficient set.
CT = int(CTr)
if(fcall==1.d0) then
    select case(CT)
    case(1) !Wet cooled case
        if(TT<4) then
            !steam
            Psat_ref = f_psat_T(dT_cw_ref + 3. + T_approach + T_amb_des)
        else
            !Isopentane
            Psat_ref = P_sat4(dT_cw_ref + 3. + T_approach + T_amb_des)
        endif
        eta_adj = eta_ref/(Interpolate(TT,12,2,Psat_ref)/Interpolate(TT,22,2,Psat_ref))
    case(2:3) !Dry cooled case and Hybrid case
        if(TT<4) then
            !steam
            Psat_ref = f_psat_T(T_ITD_des + T_amb_des)
        else
            !Isopentane
            Psat_ref = P_sat4(T_ITD_des + T_amb_des)
        endif
        eta_adj = eta_ref/(Interpolate(TT,12,2,Psat_ref)/Interpolate(TT,22,2,Psat_ref))
    end select
endif


!Calculate the specific heat before converting to Kelvin
c_htf_ref = specheat(HTF,(T_htf_hot_ref+T_htf_cold_ref)/2.+273.15,1.d0)
c_htf = specheat(HTF,(T_htf_hot+T_htf_cold_ref)/2.+273.15,1.d0)

!Convert units
!**Temperatures from Celcius to Kelvin
T_htf_hot = T_htf_hot + 273.15  ![K]
T_htf_hot_ref = T_htf_hot_ref + 273.15 ![K]
T_htf_cold_ref = T_htf_cold_ref + 273.15 ![K]
!Mass flow rates from kg/hr to kg/s
m_dot_htf = m_dot_htf/3600. ![kg/s]

!****Calculate the reference values
q_dot_ref = P_ref/eta_adj   !The reference heat flow
m_dot_htf_ref = q_dot_ref/(c_htf_ref*(T_htf_hot_ref - T_htf_cold_ref))  !The HTF mass flow rate [kg/s]

select case(TT)
case(1:3)
    T_ref = T_sat(P_boil) !The saturation temp at the boiler
case(4)
    T_ref = T_sat4(P_boil) !Isopentane - saturation temp in boiler
end select    

!Calculate the htf hot temperature, in non-dimensional form
T_htf_hot_ND = (T_htf_hot - T_ref)/(T_htf_hot_ref - T_ref)

!Calculate the htf mass flow rate in non-dimensional form
m_dot_htf_ND = m_dot_htf/m_dot_htf_ref

!Do an initial cooling tower call to estimate the turbine back pressure. 
q_reject_est = q_dot_ref*1000.*(1.-eta_adj)*m_dot_htf_ND*T_htf_hot_ND

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
if(abs(m_dot_htf_ND).lt.1.e-3) then
    P_cycle = 0.
    eta = 0.
    T_htf_cold = T_htf_hot_ref
    m_dot_demand = m_dot_htf_ref
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
        if(qq.eq.1) m_dot_htf_ND = P_dem_ND   !An initial guess (function of power)
        !if(qq.gt.1) m_dot_htf_ND = m_dot_htf_ND*ADJ
    elseif(mode.eq.2.) then
        continue     ! do nothing
    endif

    !++++++++++++++Correlations++++++++++++++++++
    !Calculate the correlations
    !++++++++++++++++++++++++++++++++++++++++++++
    !POWER  
    !Main effects
    P_ND(1) = Interpolate(TT,11,1,T_htf_hot_ND)-1.
    P_ND(2) = Interpolate(TT,12,2,P_cond)-1.
    P_ND(3) = Interpolate(TT,13,3,m_dot_htf_ND)-1.
    
    !Interactions
    P_CA = Interpolate(TT,113,13,T_htf_hot_ND)
    P_AB = Interpolate(TT,112,12,P_cond)
    P_BC = Interpolate(TT,123,23,m_dot_htf_ND)

    P_ND(1) = P_ND(1)*P_AB
    P_ND(2) = P_ND(2)*P_BC
    P_ND(3) = P_ND(3)*P_CA
    
    !HEAT               
    !Main effects
    Q_ND(1) = Interpolate(TT,21,1,T_htf_hot_ND)-1.
    Q_ND(2) = Interpolate(TT,22,2,P_cond)-1.
    Q_ND(3) = Interpolate(TT,23,3,m_dot_htf_ND)-1.

    !Interactions
    Q_CA = Interpolate(TT,213,13,T_htf_hot_ND)
    Q_AB = Interpolate(TT,212,12,P_cond)
    Q_BC = Interpolate(TT,223,23,m_dot_htf_ND)

    Q_ND(1) = Q_ND(1)*Q_AB
    Q_ND(2) = Q_ND(2)*Q_BC
    Q_ND(3) = Q_ND(3)*Q_CA

    !Calculate the cumulative values
    P_ND_tot = 1.d0
    Q_ND_tot = 1.d0

    !Increment main effects. MJW 8.11.2010 :: For this system, the effects are multiplicative.
    do i=1,size(P_ND)
        P_ND_tot = P_ND_tot *(1.d0+ P_ND(i))
        Q_ND_tot = Q_ND_tot *(1.d0+ Q_ND(i))
    enddo

    !Calculate the output values:
    P_cycle = P_ND_tot*P_ref
    T_htf_cold = T_htf_hot-Q_ND_tot*q_dot_ref/(m_dot_htf*c_htf)
    eta = P_cycle/(Q_ND_tot*q_dot_ref)
    m_dot_demand = dmax1(m_dot_htf_ND*m_dot_htf_ref,0.00001d0)   ![kg/s]
    
    !Call the cooling tower model to update the condenser pressure
    q_reject = (1.-eta)*q_dot_ref*Q_ND_tot*1000.   
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
        m_dot_htf_ND = m_dot_htf_ND + ADJ*.75   !MJW 10.31.2010: Iterate the mass flow rate. Take a step smaller than the calculated adjustment
    elseif(mode.eq.2.) then
        err = 0.
    endif

    if(qq.eq.99) then
        call messages(-1,"Power cycle model did not converge after 100 iterations","WARNING",0,224)
        P_cycle = 0.
        eta = 0.
        T_htf_cold = T_htf_hot_ref
        m_dot_demand = m_dot_htf_ref
        if(errorfound()) return
    endif
    !If this is not true, the cycle has not yet converged, and we should return
    ! to continue in the iterations
enddo

!Finally, convert the values back to their original units
T_htf_cold = T_htf_cold - 273.15 ![K]-->[C]
T_htf_cold_ref = T_htf_cold_ref - 273.15 ![K]->[C]
T_htf_hot_ref = T_htf_hot_ref - 273.15 ![K]->[C]
m_dot_demand = m_dot_demand*3600.  ![kg/s]->[kg/hr]
m_dot_htf = m_dot_htf*3600.        ![kg/s]->[kg/hr]
m_dot_htf_ref = m_dot_htf_ref*3600. ![kg/s]->[kg/hr]

!Set the "been called" variable
fcall = 0.d0

contains

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
    

    !Interpolation subroutine
    real(8) function Interpolate(TT,YT,XT,X)
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
    integer::XI,YI,i,j,lbi,ubi,jsav,n
    character::line*300

    if(.not.allocated(db)) then
    if(TT==2)then !Low temperature parabolic trough applications
        ! On first call, allocate the array and define coefficients
        allocate(db(18,20))

	    db(1,:)=(/ &
		    0.10000, 0.16842, 0.23684, 0.30526, 0.37368, 0.44211,  &
		    0.51053, 0.57895, 0.64737, 0.71579, 0.78421, 0.85263,  &
		    0.92105, 0.98947, 1.05789, 1.12632, 1.19474, 1.26316,  &
		    1.33158, 1.40000 /)
	    db(2,:)=(/ &
		    0.08547, 0.14823, 0.21378, 0.28166, 0.35143, 0.42264,  &
		    0.49482, 0.56747, 0.64012, 0.71236, 0.78378, 0.85406,  &
		    0.92284, 0.98989, 1.05685, 1.12369, 1.19018, 1.25624,  &
		    1.32197, 1.38744 /)
	    db(3,:)=(/ &
		    0.10051, 0.16934, 0.23822, 0.30718, 0.37623, 0.44534,  &
		    0.51443, 0.58338, 0.65209, 0.72048, 0.78848, 0.85606,  &
		    0.92317, 0.98983, 1.05604, 1.12182, 1.18718, 1.25200,  &
		    1.31641, 1.38047 /)
	    db(4,:)=(/ &
		    3000.00, 4263.16, 5526.32, 6789.47, 8052.63, 9315.79,  &
		    10578.95, 11842.11, 13105.26, 14368.42, 15631.58, 16894.74,  &
		    18157.89, 19421.05, 20684.21, 21947.37, 23210.53, 24473.68,  &
		    25736.84, 27000.00 /)
	    db(5,:)=(/ &
		    1.08827, 1.06020, 1.03882, 1.02145, 1.00692, 0.99416,  &
		    0.98288, 0.97273, 0.96350, 0.95504, 0.94721, 0.93996,  &
		    0.93314, 0.92673, 0.92069, 0.91496, 0.90952, 0.90433,  &
		    0.89938, 0.89464 /)
	    db(6,:)=(/ &
		    1.01276, 1.00877, 1.00570, 1.00318, 1.00106, 0.99918,  &
		    0.99751, 0.99601, 0.99463, 0.99335, 0.99218, 0.99107,  &
		    0.99004, 0.98907, 0.98814, 0.98727, 0.98643, 0.98563,  &
		    0.98487, 0.98413 /)
	    db(7,:)=(/ &
		    0.10000, 0.17368, 0.24737, 0.32105, 0.39474, 0.46842,  &
		    0.54211, 0.61579, 0.68947, 0.76316, 0.83684, 0.91053,  &
		    0.98421, 1.05789, 1.13158, 1.20526, 1.27895, 1.35263,  &
		    1.42632, 1.50000 /)
	    db(8,:)=(/ &
		    0.09307, 0.16421, 0.23730, 0.31194, 0.38772, 0.46420,  &
		    0.54098, 0.61763, 0.69374, 0.76896, 0.84287, 0.91511,  &
		    0.98530, 1.05512, 1.12494, 1.19447, 1.26373, 1.33273,  &
		    1.40148, 1.46999 /)
	    db(9,:)=(/ &
		    0.10741, 0.18443, 0.26031, 0.33528, 0.40950, 0.48308,  &
		    0.55610, 0.62861, 0.70066, 0.77229, 0.84354, 0.91443,  &
		    0.98497, 1.05520, 1.12514, 1.19478, 1.26416, 1.33329,  &
		    1.40217, 1.47081 /)
	    db(10,:)=(/ &
		    0.10000, 0.16842, 0.23684, 0.30526, 0.37368, 0.44211,  &
		    0.51053, 0.57895, 0.64737, 0.71579, 0.78421, 0.85263,  &
		    0.92105, 0.98947, 1.05789, 1.12632, 1.19474, 1.26316,  &
		    1.33158, 1.40000 /)
	    db(11,:)=(/ &
		    1.01749, 1.03327, 1.04339, 1.04900, 1.05051, 1.04825,  &
		    1.04249, 1.03343, 1.02126, 1.01162, 1.00500, 1.00084,  &
		    0.99912, 0.99966, 0.99972, 0.99942, 0.99920, 0.99911,  &
		    0.99885, 0.99861 /)
	    db(12,:)=(/ &
		    0.99137, 0.99297, 0.99431, 0.99564, 0.99681, 0.99778,  &
		    0.99855, 0.99910, 0.99948, 0.99971, 0.99984, 0.99989,  &
		    0.99993, 0.99993, 0.99992, 0.99992, 0.99992, 1.00009,  &
		    1.00010, 1.00012 /)
	    db(13,:)=(/ &
		    3000.00, 4263.16, 5526.32, 6789.47, 8052.63, 9315.79,  &
		    10578.95, 11842.11, 13105.26, 14368.42, 15631.58, 16894.74,  &
		    18157.89, 19421.05, 20684.21, 21947.37, 23210.53, 24473.68,  &
		    25736.84, 27000.00 /)
	    db(14,:)=(/ &
		    0.99653, 0.99756, 0.99839, 0.99906, 0.99965, 1.00017,  &
		    1.00063, 1.00106, 1.00146, 1.00183, 1.00218, 1.00246,  &
		    1.00277, 1.00306, 1.00334, 1.00361, 1.00387, 1.00411,  &
		    1.00435, 1.00458 /)
	    db(15,:)=(/ &
		    0.99760, 0.99831, 0.99888, 0.99934, 0.99973, 1.00008,  &
		    1.00039, 1.00067, 1.00093, 1.00118, 1.00140, 1.00161,  &
		    1.00180, 1.00199, 1.00217, 1.00234, 1.00250, 1.00265,  &
		    1.00280, 1.00294 /)
	    db(16,:)=(/ &
		    0.10000, 0.17368, 0.24737, 0.32105, 0.39474, 0.46842,  &
		    0.54211, 0.61579, 0.68947, 0.76316, 0.83684, 0.91053,  &
		    0.98421, 1.05789, 1.13158, 1.20526, 1.27895, 1.35263,  &
		    1.42632, 1.50000 /)
	    db(17,:)=(/ &
		    1.01994, 1.01645, 1.01350, 1.01073, 1.00801, 1.00553,  &
		    1.00354, 1.00192, 1.00077, 0.99995, 0.99956, 0.99957,  &
		    1.00000, 0.99964, 0.99955, 0.99945, 0.99937, 0.99928,  &
		    0.99919, 0.99918 /)
	    db(18,:)=(/ &
		    1.02055, 1.01864, 1.01869, 1.01783, 1.01508, 1.01265,  &
		    1.01031, 1.00832, 1.00637, 1.00454, 1.00301, 1.00141,  &
		    1.00008, 0.99851, 0.99715, 0.99586, 0.99464, 0.99347,  &
		    0.99227, 0.99177 /)

    elseif (TT==1) then  !Power tower applications
        ! On first call, allocate the array and define coefficients
        allocate(db(18,20))
		    db(1,:)=(/ &
		    0.20000, 0.25263, 0.30526, 0.35789, 0.41053, 0.46316,  &
		    0.51579, 0.56842, 0.62105, 0.67368, 0.72632, 0.77895,  &
		    0.83158, 0.88421, 0.93684, 0.98947, 1.04211, 1.09474,  &
		    1.14737, 1.20000 /)
	    db(2,:)=(/ &
		    0.16759, 0.21750, 0.26932, 0.32275, 0.37743, 0.43300,  &
		    0.48910, 0.54545, 0.60181, 0.65815, 0.71431, 0.77018,  &
		    0.82541, 0.88019, 0.93444, 0.98886, 1.04378, 1.09890,  &
		    1.15425, 1.20982 /)
	    db(3,:)=(/ &
		    0.19656, 0.24969, 0.30325, 0.35710, 0.41106, 0.46497,  &
		    0.51869, 0.57215, 0.62529, 0.67822, 0.73091, 0.78333,  &
		    0.83526, 0.88694, 0.93838, 0.98960, 1.04065, 1.09154,  &
		    1.14230, 1.19294 /)
	    db(4,:)=(/ &
		    3000.00, 4263.16, 5526.32, 6789.47, 8052.63, 9315.79,  &
		    10578.95, 11842.11, 13105.26, 14368.42, 15631.58, 16894.74,  &
		    18157.89, 19421.05, 20684.21, 21947.37, 23210.53, 24473.68,  &
		    25736.84, 27000.00 /)
	    db(5,:)=(/ &
		    1.07401, 1.04917, 1.03025, 1.01488, 1.00201, 0.99072,  &
		    0.98072, 0.97174, 0.96357, 0.95607, 0.94914, 0.94269,  &
		    0.93666, 0.93098, 0.92563, 0.92056, 0.91573, 0.91114,  &
		    0.90675, 0.90255 /)
	    db(6,:)=(/ &
		    1.00880, 1.00583, 1.00355, 1.00168, 1.00010, 0.99870,  &
		    0.99746, 0.99635, 0.99532, 0.99438, 0.99351, 0.99269,  &
		    0.99193, 0.99121, 0.99052, 0.98988, 0.98926, 0.98867,  &
		    0.98810, 0.98756 /)
	    db(7,:)=(/ &
		    0.10000, 0.17368, 0.24737, 0.32105, 0.39474, 0.46842,  &
		    0.54211, 0.61579, 0.68947, 0.76316, 0.83684, 0.91053,  &
		    0.98421, 1.05789, 1.13158, 1.20526, 1.27895, 1.35263,  &
		    1.42632, 1.50000 /)
	    db(8,:)=(/ &
		    0.09403, 0.16542, 0.23861, 0.31328, 0.38901, 0.46540,  &
		    0.54203, 0.61849, 0.69437, 0.76928, 0.84282, 0.91458,  &
		    0.98470, 1.05517, 1.12536, 1.19531, 1.26502, 1.33450,  &
		    1.40376, 1.47282 /)
	    db(9,:)=(/ &
		    0.10659, 0.18303, 0.25848, 0.33316, 0.40722, 0.48075,  &
		    0.55381, 0.62646, 0.69873, 0.77066, 0.84228, 0.91360,  &
		    0.98464, 1.05542, 1.12596, 1.19627, 1.26637, 1.33625,  &
		    1.40593, 1.47542 /)
	    db(10,:)=(/ &
		    0.20000, 0.25263, 0.30526, 0.35789, 0.41053, 0.46316,  &
		    0.51579, 0.56842, 0.62105, 0.67368, 0.72632, 0.77895,  &
		    0.83158, 0.88421, 0.93684, 0.98947, 1.04211, 1.09474,  &
		    1.14737, 1.20000 /)
	    db(11,:)=(/ &
		    1.03323, 1.04058, 1.04456, 1.04544, 1.04357, 1.03926,  &
		    1.03282, 1.02446, 1.01554, 1.00944, 1.00487, 1.00169,  &
		    0.99986, 0.99926, 0.99980, 1.00027, 1.00021, 1.00015,  &
		    1.00006, 0.99995 /)
	    db(12,:)=(/ &
		    0.98344, 0.98630, 0.98876, 0.99081, 0.99247, 0.99379,  &
		    0.99486, 0.99574, 0.99649, 0.99716, 0.99774, 0.99826,  &
		    0.99877, 0.99926, 0.99972, 1.00017, 1.00060, 1.00103,  &
		    1.00143, 1.00182 /)
	    db(13,:)=(/ &
		    3000.00, 4263.16, 5526.32, 6789.47, 8052.63, 9315.79,  &
		    10578.95, 11842.11, 13105.26, 14368.42, 15631.58, 16894.74,  &
		    18157.89, 19421.05, 20684.21, 21947.37, 23210.53, 24473.68,  &
		    25736.84, 27000.00 /)
	    db(14,:)=(/ &
		    0.99269, 0.99520, 0.99718, 0.99882, 1.00024, 1.00150,  &
		    1.00264, 1.00368, 1.00464, 1.00554, 1.00637, 1.00716,  &
		    1.00790, 1.00840, 1.00905, 1.00965, 1.01022, 1.01075,  &
		    1.01126, 1.01173 /)
	    db(15,:)=(/ &
		    0.99768, 0.99861, 0.99933, 0.99992, 1.00043, 1.00087,  &
		    1.00127, 1.00164, 1.00197, 1.00227, 1.00255, 1.00282,  &
		    1.00307, 1.00331, 1.00353, 1.00375, 1.00395, 1.00415,  &
		    1.00433, 1.00451 /)
	    db(16,:)=(/ &
		    0.10000, 0.17368, 0.24737, 0.32105, 0.39474, 0.46842,  &
		    0.54211, 0.61579, 0.68947, 0.76316, 0.83684, 0.91053,  &
		    0.98421, 1.05789, 1.13158, 1.20526, 1.27895, 1.35263,  &
		    1.42632, 1.50000 /)
	    db(17,:)=(/ &
		    1.00812, 1.00513, 1.00294, 1.00128, 0.99980, 0.99901,  &
		    0.99855, 0.99836, 0.99846, 0.99883, 0.99944, 1.00033,  &
		    1.00042, 1.00056, 1.00069, 1.00081, 1.00093, 1.00104,  &
		    1.00115, 1.00125 /)
	    db(18,:)=(/ &
		    1.09816, 1.07859, 1.06487, 1.05438, 1.04550, 1.03816,  &
		    1.03159, 1.02579, 1.02061, 1.01587, 1.01157, 1.00751,  &
		    1.00380, 1.00033, 0.99705, 0.99400, 0.99104, 0.98832,  &
		    0.98565, 0.98316 /)
    elseif (TT==3) then !Sliding pressure power cycle formulation mjw 3.31.11/ Bug fix 7.22.11 - cond pressure units incorrect for items 4 and 13
        allocate(db(18,10))
	    db(1,:)=(/ &
		    0.10000, 0.21111, 0.32222, 0.43333, 0.54444, 0.65556,  &
		    0.76667, 0.87778, 0.98889, 1.10000 /)
	    db(2,:)=(/ &
		    0.89280, 0.90760, 0.92160, 0.93510, 0.94820, 0.96110,  &
		    0.97370, 0.98620, 0.99860, 1.01100 /)
	    db(3,:)=(/ &
		    0.93030, 0.94020, 0.94950, 0.95830, 0.96690, 0.97520,  &
		    0.98330, 0.99130, 0.99910, 1.00700 /)
	    db(4,:)=(/ &    
		    4000.00, 6556.00, 9111.00, 11677.0, 14222.0, 16778.0,  &
		    19333.0, 21889.0, 24444.0, 27000.0 /)
	    db(5,:)=(/ &
		    1.04800, 1.01400, 0.99020, 0.97140, 0.95580, 0.94240,  &
		    0.93070, 0.92020, 0.91060, 0.90190 /)
	    db(6,:)=(/ &
		    0.99880, 0.99960, 1.00000, 1.00100, 1.00100, 1.00100,  &
		    1.00100, 1.00200, 1.00200, 1.00200 /)
	    db(7,:)=(/ &
		    0.20000, 0.31667, 0.43333, 0.55000, 0.66667, 0.78333,  &
		    0.90000, 1.01667, 1.13333, 1.25000 /)
	    db(8,:)=(/ &
		    0.16030, 0.27430, 0.39630, 0.52310, 0.65140, 0.77820,  &
		    0.90060, 1.01600, 1.12100, 1.21400 /)
	    db(9,:)=(/ &
		    0.22410, 0.34700, 0.46640, 0.58270, 0.69570, 0.80550,  &
		    0.91180, 1.01400, 1.11300, 1.20700 /)
	    db(10,:)=(/ &
		    0.10000, 0.21111, 0.32222, 0.43333, 0.54444, 0.65556,  &
		    0.76667, 0.87778, 0.98889, 1.10000 /)
	    db(11,:)=(/ &
		    1.05802, 1.05127, 1.04709, 1.03940, 1.03297, 1.02480,  &
		    1.01758, 1.00833, 1.00180, 0.99307 /)
	    db(12,:)=(/ &
		    1.03671, 1.03314, 1.02894, 1.02370, 1.01912, 1.01549,  &
		    1.01002, 1.00486, 1.00034, 0.99554 /)
	    db(13,:)=(/ &
		    4000.00, 6556.00, 9111.00, 11677.0, 14222.0, 16778.0,  &
		    19333.0, 21889.0, 24444.0, 27000.0 /)
	    db(14,:)=(/ &
		    1.00825, 0.98849, 0.99742, 1.02080, 1.02831, 1.03415,  &
		    1.03926, 1.04808, 1.05554, 1.05862 /)
	    db(15,:)=(/ &
		    !tweaked entry #4 to be the average of 3 and 5. it was an outlier in the simulation. mjw 3.31.11
		    1.01838, 1.02970, 0.99785, 0.99663, 0.99542, 0.99183,  &  
		    0.98897, 0.99299, 0.99013, 0.98798 /)
	    db(16,:)=(/ &
		    0.20000, 0.31667, 0.43333, 0.55000, 0.66667, 0.78333,  &
		    0.90000, 1.01667, 1.13333, 1.25000 /)
	    db(17,:)=(/ &
		    1.43311, 1.27347, 1.19090, 1.13367, 1.09073, 1.05602,  &
		    1.02693, 1.00103, 0.97899, 0.95912 /)
	    db(18,:)=(/ &
	        !tweaked entry #9 to be the average of 8 and 10. it was an outlier in the simulation mjw 3.31.11
		    0.48342, 0.64841, 0.64322, 0.74366, 0.76661, 0.82764,  &
		    0.97792, 1.15056, 1.23117, 1.31179 /)
    elseif (TT==4) then !Geothermal Isopentane rankine cycle
        allocate(db(18,20))
	    db(1,:)=(/ &
		    0.50000, 0.53158, 0.56316, 0.59474, 0.62632, 0.65789,  &
		    0.68947, 0.72105, 0.75263, 0.78421, 0.81579, 0.84737,  &
		    0.87895, 0.91053, 0.94211, 0.97368, 1.00526, 1.03684,  &
		    1.06842, 1.10000 /)
	    db(2,:)=(/ &
		    0.55720, 0.58320, 0.60960, 0.63630, 0.66330, 0.69070,  &
		    0.71840, 0.74630, 0.77440, 0.80270, 0.83130, 0.85990,  &
		    0.88870, 0.91760, 0.94670, 0.97570, 1.00500, 1.03400,  &
		    1.06300, 1.09200 /)
	    db(3,:)=(/ &
		    0.67620, 0.69590, 0.71570, 0.73570, 0.75580, 0.77600,  &
		    0.79630, 0.81670, 0.83720, 0.85780, 0.87840, 0.89910,  &
		    0.91990, 0.94070, 0.96150, 0.98230, 1.00300, 1.02400,  &
		    1.04500, 1.06600 /)
	    db(4,:)=(/ &
		    35000.00, 46315.79, 57631.58, 68947.37, 80263.16, 91578.95,  &
		    102894.74, 114210.53, 125526.32, 136842.11, 148157.89, 159473.68,  &
		    170789.47, 182105.26, 193421.05, 204736.84, 216052.63, 227368.42,  &
		    238684.21, 250000.00 /)
	    db(5,:)=(/ &
		    1.94000, 1.77900, 1.65200, 1.54600, 1.45600, 1.37800,  &
		    1.30800, 1.24600, 1.18900, 1.13700, 1.08800, 1.04400,  &
		    1.00200, 0.96290, 0.92620, 0.89150, 0.85860, 0.82740,  &
		    0.79770, 0.76940 /)
	    db(6,:)=(/ &
		    1.22400, 1.19100, 1.16400, 1.14000, 1.11900, 1.10000,  &
		    1.08300, 1.06700, 1.05200, 1.03800, 1.02500, 1.01200,  &
		    1.00000, 0.98880, 0.97780, 0.96720, 0.95710, 0.94720,  &
		    0.93770, 0.92850 /)
	    db(7,:)=(/ &
		    0.80000, 0.81316, 0.82632, 0.83947, 0.85263, 0.86579,  &
		    0.87895, 0.89211, 0.90526, 0.91842, 0.93158, 0.94474,  &
		    0.95789, 0.97105, 0.98421, 0.99737, 1.01053, 1.02368,  &
		    1.03684, 1.05000 /)
	    db(8,:)=(/ &
		    0.84760, 0.85880, 0.86970, 0.88050, 0.89120, 0.90160,  &
		    0.91200, 0.92210, 0.93220, 0.94200, 0.95180, 0.96130,  &
		    0.97080, 0.98010, 0.98920, 0.99820, 1.00700, 1.01600,  &
		    1.02400, 1.03300 /)
	    db(9,:)=(/ &
		    0.89590, 0.90350, 0.91100, 0.91840, 0.92570, 0.93290,  &
		    0.93990, 0.94680, 0.95370, 0.96040, 0.96700, 0.97350,  &
		    0.97990, 0.98620, 0.99240, 0.99850, 1.00400, 1.01000,  &
		    1.01500, 1.02100 /)
	    db(10,:)=(/ &
		    0.50000, 0.53158, 0.56316, 0.59474, 0.62632, 0.65789,  &
		    0.68947, 0.72105, 0.75263, 0.78421, 0.81579, 0.84737,  &
		    0.87895, 0.91053, 0.94211, 0.97368, 1.00526, 1.03684,  &
		    1.06842, 1.10000 /)
	    db(11,:)=(/ &
		    0.79042, 0.80556, 0.82439, 0.84177, 0.85786, 0.87485,  &
		    0.88898, 0.90182, 0.91783, 0.93019, 0.93955, 0.95105,  &
		    0.96233, 0.97150, 0.98059, 0.98237, 0.99829, 1.00271,  &
		    1.02084, 1.02413 /)
	    db(12,:)=(/ &
		    0.67400, 0.69477, 0.71830, 0.73778, 0.75991, 0.78079,  &
		    0.80052, 0.82622, 0.88152, 0.92737, 0.93608, 0.94800,  &
		    0.95774, 0.96653, 0.97792, 0.99852, 0.99701, 1.01295,  &
		    1.02825, 1.04294 /)
	    db(13,:)=(/ &
		    35000.00, 46315.79, 57631.58, 68947.37, 80263.16, 91578.95,  &
		    102894.74, 114210.53, 125526.32, 136842.11, 148157.89, 159473.68,  &
		    170789.47, 182105.26, 193421.05, 204736.84, 216052.63, 227368.42,  &
		    238684.21, 250000.00 /)
	    db(14,:)=(/ &
		    0.80313, 0.82344, 0.83980, 0.86140, 0.87652, 0.89274,  &
		    0.91079, 0.92325, 0.93832, 0.95229, 0.97004, 0.98211,  &
		    1.00399, 1.01514, 1.03494, 1.04962, 1.06646, 1.08374,  &
		    1.10088, 1.11789 /)
	    db(15,:)=(/ &
		    0.93426, 0.94458, 0.94618, 0.95878, 0.96352, 0.96738,  &
		    0.97058, 0.98007, 0.98185, 0.99048, 0.99144, 0.99914,  &
		    1.00696, 1.00849, 1.01573, 1.01973, 1.01982, 1.02577,  &
		    1.02850, 1.03585 /)
	    db(16,:)=(/ &
		    0.80000, 0.81316, 0.82632, 0.83947, 0.85263, 0.86579,  &
		    0.87895, 0.89211, 0.90526, 0.91842, 0.93158, 0.94474,  &
		    0.95789, 0.97105, 0.98421, 0.99737, 1.01053, 1.02368,  &
		    1.03684, 1.05000 /)
	    db(17,:)=(/ &
		    1.06790, 1.06247, 1.05688, 1.05185, 1.04687, 1.04230,  &
		    1.03748, 1.03281, 1.02871, 1.02473, 1.02050, 1.01639,  &
		    1.01204, 1.00863, 1.00461, 1.00051, 0.99710, 0.99352,  &
		    0.98974, 0.98692 /)
	    db(18,:)=(/ &
		    1.02335, 1.02130, 1.02041, 1.01912, 1.01655, 1.01601,  &
		    1.01379, 1.01431, 1.01321, 1.01207, 1.01129, 1.00784,  &
		    1.00548, 1.00348, 1.00183, 0.99982, 0.99698, 0.99457,  &
		    0.99124, 0.99016 /)
        
    else
        !Read the coefficients from a user-file
        !Do a test read to make sure the file is available
        read(LU_pb,fmt="(A)",advance="NO",err=200,eor=200)
        rewind(LU_pb)
        goto 205

        200 continue !There was a problem
            call messages(-1,"Power block coefficient file was not found","FATAL",224,0)    
            stop
        205 continue

        !Continue reading the file
        !how long is the string containing values? We need to determine the number of entries
        line=''
        read(LU_pb,fmt='(A)') line

        !SAM is expecting each entry to be 8 long, with 1 column of header info
        n=(len(trim(line))+1)/8-1

        !Now allocate the db array
        if(allocated(db)) deallocate(db)
        allocate(db(18,n))

        !Rewind and read in the file. There are 18 effects, 1st column is label text
        rewind(LU_pb)
        do i=1,18
            read(LU_pb,fmt="(8X)",advance="NO")
            do j=1,n
                read(LU_pb,fmt="(F8.5)",advance="NO") db(i,j)
            enddo
            read(LU_pb,fmt="(X)",advance="YES")
        enddo

        close(LU_pb)
        
    endif

    !Now select which to interpolate
    n=size(db(1,:))  !All of the sub arrays should be of the same length

    !Allocate data arrays
    if(.not.allocated(datx)) allocate(datx(n),daty(n))


    endif 



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

    !Set the data to be interpolated
    datx(1:n)=db(XI,1:n)
    daty(1:n)=db(YI,1:n)
    	
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

    Interpolate=Y


    end function Interpolate 

end subroutine
!end "contains"
!------------------------------------------------------------------------------------------------------------
