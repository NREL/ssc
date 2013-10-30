SUBROUTINE TYPE225 (TIME,XIN,OUT,T,DTDT,PAR,INFO,ICNTRL,*) 
!************************************************************************
! Object: Power Tower Logic (Single stratified tank)
! Simulation Studio Model: Type225
! 
! Author: Michael J. Wagner
! Editor: Michael J. Wagner
! Date:	 August 17, 2008 last modified: Nov 24, 2009

! COPYRIGHT 2009 NATIONAL RENEWABLE ENERGY LABORATORY


! Parameters
!-----------------------------------------------------------------------------------------------------------------------------------------
!Nb |  Variable                      |  Description                                                    |  Output Units  |  Internal Units
!---|--------------------------------|-----------------------------------------------------------------|----------------|-----------------
!  1| TSHOURS                        | Number of full-load thermal storage hours                       | hours          | hours
!  2| NUMTOU                         | Number of time-of-use periods                                   | none           | none
! c3| TSLOGIC                        | Dispatch logic for time of use period                           | none           | none
!c21| FFRAC                          | Fossil-fill fraction control                                    | none           | none
! 27| Design_HTF_T_Hot               | Design HTF hot temperature from the tower                       | C              | C
! 28| Design_HTF_T_cold              | Design HTF cold temperature from the power cycle                | C              | C

! Inputs
!-----------------------------------------------------------------------------------------------------------------------------------------
!Nb |  Variable                      |  Description                                                    |  Output Units  |  Internal Units
!---|--------------------------------|-----------------------------------------------------------------|----------------|-----------------
!  1|                                |                                                                 |                |  
 
! Outputs
!-----------------------------------------------------------------------------------------------------------------------------------------
!Nb |  Variable                      |  Description                                                    |  Output Units  |  Internal Units
!---|--------------------------------|-----------------------------------------------------------------|----------------|-----------------
!  1|                                |                                                                 |                |  

!************************************************************************
!    TRNSYS acess functions (allow to acess TIME etc.) 
USE TrnsysConstants
USE TrnsysFunctions

!    REQUIRED BY THE MULTI-DLL VERSION OF TRNSYS
!DEC$ATTRIBUTES DLLEXPORT :: TYPE225				!SET THE CORRECT TYPE NUMBER HERE

implicit none

real(8):: xin, out, time, par, stored, t, dtdt
integer:: info(15), np, ni, nout, nd, npar, nin, nder, iunit, itype, icntrl, nstored
CHARACTER*3 OCHECK, YCHECK

PARAMETER (NP=100,NI=21,NOUT=13,ND=0,NSTORED=5)
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!    REQUIRED TRNSYS DIMENSIONS
DIMENSION XIN(NI),OUT(NOUT),PAR(NP),YCHECK(NI),OCHECK(NOUT),STORED(NSTORED),T(ND),DTDT(ND)

!-----------------------------------------------------------------------------------------------------------------------
!    PARAMETERS
integer:: NUMTOU
real(8):: TSHOURS
real(8), allocatable::  TSLOGIC_A(:),TSLOGIC_B(:),TSLOGIC_C(:),TSLOGIC(:), FFRAC(:), TSELECT(:)

!    INPUTS
real(8)::  Hour_of_the_day, zenith, Storage_hot_outlet_temp, Min_temp_to_load, Load_flow_demand, Storage_cold_outlet_temp,&
                 Max_temp_to_heat_source, Flow_from_heat_source, Temp_from_heat_source, t_standby, day_shutdown, &
                 m_dot_max, DNI_hrly, total_tank_vol, HTF_spcheat, HTF_density, TES_energy_change, Q_TES_ini, &
                 P_ref, TES_T_ave, Design_HTF_T_Hot,Design_HTF_T_Cold, HTF, specheat, density, c_aux, q_aux, q_aux_hv, &
                 lhv_eff, temp_to_load
integer:: touperiod

!     Local variables
real(8):: Stored_hour, Hour_to_store, t_standby_remain,timestep, DNI, TES_charge, derate
integer:: i, Stored_mode
logical:: is_hybrid

!     OUTPUTS
real(8):: Flow_to_power_cycle, Flow_to_heat_source, Field_control, hybrid_control, Pump_on, standby_control, &
          demand_var_control, cycle_pl_control
integer:: mode

!-----------------------------------------------------------------------------------------------------------------------
! RETRIEVE THE CURRENT VALUES OF THE INPUTS TO THIS MODEL FROM THE XIN ARRAY IN SEQUENTIAL ORDER

Hour_of_the_day = XIN(1)
zenith=XIN(2)
Storage_hot_outlet_temp = XIN(3)+273.15
Min_temp_to_load = XIN(4)+273.15
Load_flow_demand = XIN(5)
Storage_cold_outlet_temp = XIN(6)+273.15
Max_temp_to_heat_source = XIN(7)+273.15
Flow_from_heat_source = XIN(8)
Temp_from_heat_source = XIN(9)+273.15
t_standby = XIN(10)
m_dot_max = XIN(11)
TOUPeriod = int(XIN(12))
DNI_hrly = XIN(13); DNI=DNI_hrly/3.6;
total_tank_vol = XIN(14)
TES_energy_change = XIN(15) !kJ
P_ref = XIN(16) !kW
TES_T_ave = XIN(17)+273.15
Design_HTF_T_Hot=XIN(18)+273.15  ![K]
Design_HTF_T_cold=XIN(19)+273.15 ![K]
HTF = XIN(20)
lhv_eff = XIN(21)
    IUNIT=INFO(1)
    ITYPE=INFO(2)
	

!-----------------------------------------------------------------------------------------------------------------------
!    SET THE VERSION INFORMATION FOR TRNSYS
IF(INFO(7)==-2) THEN
    INFO(12)=16
    RETURN 1
ENDIF
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!    DO ALL THE VERY LAST CALL OF THE SIMULATION MANIPULATIONS HERE
IF (INFO(8)==-1) THEN

    if(allocated(TSLOGIC)) deallocate(TSLOGIC, TSLOGIC_A, TSLOGIC_B, TSLOGIC_C, FFRAC, TSELECT)

    RETURN 1
ENDIF
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!    PERFORM ANY 'AFTER-ITERATION' MANIPULATIONS THAT ARE REQUIRED HERE
IF (INFO(13)>0) THEN
    STORED(3)=t_standby_remain
    STORED(4)=day_shutdown
    CALL setStorageVars(STORED,NSTORED,INFO)
    RETURN 1
ENDIF
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!    DO ALL THE VERY FIRST CALL OF THE SIMULATION MANIPULATIONS HERE
IF (INFO(7)==-1) THEN


    ! READ IN THE VALUES OF THE PARAMETERS IN SEQUENTIAL ORDER
    TSHOURS=PAR(1)
    NUMTOU=int(PAR(2))
    
    !Allocate variables
    if(.not.allocated(TSLOGIC)) allocate(TSLOGIC(NUMTOU*3), TSLOGIC_A(NUMTOU), &
                        TSLOGIC_B(NUMTOU), TSLOGIC_C(NUMTOU), FFRAC(NUMTOU), TSELECT(NUMTOU))
    
    !Initialize TSLOGIC 
    TSLOGIC=0.d0; TSLOGIC_A=0.d0; TSLOGIC_B=0.d0; TSLOGIC_C=0.d0; TSELECT=0.d0
    TSLOGIC(1:(3*NUMTOU))=PAR(3:(3*NUMTOU+2))         
    FFRAC(1:NUMTOU)=PAR((3*(NUMTOU+1)):(3*(NUMTOU+1)+5))

    do i=1,NUMTOU
        TSLOGIC_A(i)=TSLOGIC(1+(i-1)*3)
        TSLOGIC_B(i)=TSLOGIC(2+(i-1)*3)
        TSLOGIC_C(i)=TSLOGIC(3+(i-1)*3)
    enddo

    ! SET SOME INFO ARRAY VARIABLES TO TELL THE TRNSYS ENGINE HOW THIS TYPE IS TO WORK
    INFO(6)=NOUT				
    INFO(9)=1				
    INFO(10)=0	!STORAGE FOR VERSION 16 HAS BEEN CHANGED				

    ! SET THE REQUIRED NUMBER OF INPUTS, PARAMETERS AND DERIVATIVES THAT THE USER SHOULD SUPPLY IN THE INPUT FILE
    ! IN SOME CASES, THE NUMBER OF VARIABLES MAY DEPEND ON THE VALUE OF PARAMETERS TO THIS MODEL....
    NIN=NI
    NPAR=4*NUMTOU+2
    NDER=ND

    ! CALL THE TYPE CHECK SUBROUTINE TO COMPARE WHAT THIS COMPONENT REQUIRES TO WHAT IS SUPPLIED IN THE TRNSYS INPUT FILE
    CALL TYPECK(1,INFO,NIN,NPAR,NDER)

    ! SET THE NUMBER OF STORAGE SPOTS NEEDED FOR THIS COMPONENT
    CALL setStorageSize(NSTORED,INFO)

    ! initialize the value of the day_shutdown variable
    day_shutdown=0
    ! RETURN TO THE CALLING PROGRAM
    RETURN 1

ENDIF
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!    DO ALL OF THE INITIAL TIMESTEP MANIPULATIONS HERE - THERE ARE NO ITERATIONS AT THE INTIAL TIME
IF (TIME < (getSimulationStartTime() + getSimulationTimeStep()/2.D0)) THEN
    !Set timestep
    timestep=getSimulationTimeStep()
    
    !Check if the system has hybridization
    is_hybrid = .false.
    if(sum(ffrac(:)) > 0.d0) is_hybrid=.true.
    
    !Get the initial averaged HTF properties
    HTF_spcheat = specheat(HTF, TES_T_ave, 101325.d0)
    HTF_density = density(HTF, TES_T_ave, 101325.d0)

    !Calculate the intital storage energy
    Q_TES_ini=(TES_T_ave-Design_HTF_T_Cold)*HTF_spcheat*HTF_density*total_tank_vol
    
    ! SET THE UNIT NUMBER FOR FUTURE CALLS
    IUNIT=INFO(1); ITYPE=INFO(2)
    
    
    ! PERFORM ANY REQUIRED CALCULATIONS TO SET THE INITIAL VALUES OF THE OUTPUTS HERE
    out(:)=0.d0
    !Field_control
    OUT(3)=1.d0
    !Standby control
    OUT(6)=1.d0
    !Demand variable controller (for Type224)
    OUT(8)=P_ref
    !HTF temperature to power cycle
    OUT(12)=Design_HTF_T_hot-273.15

    ! PERFORM ANY REQUIRED CALCULATIONS TO SET THE INITIAL STORAGE VARIABLES HERE
    t_standby_remain = t_standby
    STORED(1)=0.d0  !mode
    STORED(2)=0.d0  !initial hour to store
    STORED(3)=t_standby_remain   !The initial standby time period
    STORED(4)=day_shutdown  !is the cycle off for the day
    STORED(5)=0.d0  !The initial storage thermal energy
    ! PUT THE STORED ARRAY IN THE GLOBAL STORED ARRAY
    CALL setStorageVars(STORED,NSTORED,INFO)

    ! RETURN TO THE CALLING PROGRAM
    RETURN 1

ENDIF
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
! Called every iteration
!-----------------------------------------------------------------------------------------------------------------------
! Get stored variables
CALL getStorageVars(STORED,NSTORED,INFO)
    stored_mode=int(STORED(1))
    stored_hour=STORED(2)
    t_standby_remain = STORED(3)
    day_shutdown=STORED(4)
    Q_TES_ini=STORED(5)


!Get the averaged HTF properties
HTF_spcheat = specheat(HTF, TES_T_ave, 101325.d0)
HTF_density = density(HTF, TES_T_ave, 101325.d0)

!Calculate the tank charge level, between 0 and 1
TES_charge = (Q_TES_ini + TES_energy_change)/(HTF_spcheat*HTF_density*total_tank_vol*(Design_HTF_T_Hot - Design_HTF_T_Cold)) 

!do a check to make sure of convergence
Hour_to_store = Hour_of_the_day
if (Hour_to_store==Stored_hour) then 
    Mode = Stored_mode
    goto 100
!else
!    derate=0.97
endif

if(DNI>0.01d0) then   !The solar resource is available above 150 W/m^2
    TSELECT=TSLOGIC_B
else                  !Use the dispatch for no solar resource
    TSELECT=TSLOGIC_A
endif

!********logic here**********
if (TES_charge>=TSELECT(TOUperiod)) then
    if (Storage_hot_outlet_temp>Min_temp_to_load) then       !Is the power cycle temp requirement met?
        if(Storage_cold_outlet_temp<Max_temp_to_heat_source) then   !Is the storage-to-tower temp ok?
            Mode = 1  !Normal operation
        else
            Mode = 2   !Tank is overcharged, tower OFF and let the power cycle run
        endif
    else    !Power cycle inlet temperature is too low
        if(is_hybrid) then  !The plant is hybrid
            Mode = 3 !supplement the flow with an external heat source
        else
            if((t_standby_remain>0).and.(zenith<80).and.(zenith.ne.0.)) then 
                !--> the standby time has expired, or its after sunset (zenith is set to zero when >90)
                Mode = 6 !go to standby mode
            else
                Mode = 4  !Let the tower cycle fluid to charge the tank
            endif
        endif
    endif
else   !The charge requirement is not met
    if (Storage_cold_outlet_temp<Max_temp_to_heat_source) then   !Is the storage-to-tower temp ok?
        Mode = 4  !Let the tank charge during the cycle-off period
    else
        Mode = 5  !The  tank is fully charged, now OFF and wait for operating hours to begin
    endif
endif


!___Define the modes of operation___
100   select case (Mode)
case(1) !Mode 1: Normal operation
    Flow_to_power_cycle = Load_flow_demand
    Flow_to_heat_source = Flow_from_heat_source
    !If the flow exceeds the maximum flow rate to the tower, 
    !..partially defocus the field
    if (hour_to_store/=stored_hour) then  !First call, start at FC=1
        Field_control=1
    else        !Iterative calls, calculate a defocus factor
        if (Flow_to_heat_source>=m_dot_max) then
            Field_control=Field_control*m_dot_max/Flow_to_heat_source
        else
            Field_control=Field_control
        endif
    endif
    hybrid_control = 0.
    Pump_on = 1.
    standby_control = 1.
    t_standby_remain = t_standby
    demand_var_control=P_ref*TSLOGIC_C(TOUperiod)
    cycle_pl_control=1.  !1=demand, 2=part load
    temp_to_load = Storage_hot_outlet_temp
case(2) !Mode 2: Hot flow
    Flow_to_power_cycle = Load_flow_demand
    Flow_to_heat_source = Flow_from_heat_source
    Field_control = 0.                                          !MJW 11.8.2010 This probably isn't the best approach.. maybe partially defocusing would be better.
    hybrid_control = 0.
    Pump_on = 1.
    standby_control = 1.
    t_standby_remain = t_standby
    demand_var_control=P_ref*TSLOGIC_C(TOUperiod)
    cycle_pl_control=1.  !1=demand, 2=part load
    temp_to_load = Storage_hot_outlet_temp
case(3)  !Mode 3: Running cycle with supplemental HS 
    Flow_to_power_cycle = Load_flow_demand
    Flow_to_heat_source = Flow_from_heat_source
    Field_control = 1. 
    hybrid_control = 1. 
    Pump_on = 1. 
    standby_control = 1.
    t_standby_remain = t_standby
    demand_var_control=P_ref*TSLOGIC_C(TOUperiod)
    cycle_pl_control=1.  !1=demand, 2=part load
    !Determine the load on the aux heater
    c_aux = specheat(HTF, (Storage_hot_outlet_temp + Design_HTF_T_Hot)/2.d0, 101325.d0)
    q_aux = flow_to_power_cycle*c_aux*(Design_HTF_T_Hot - Storage_hot_outlet_temp)/1.e6
    q_aux_hv = q_aux/lhv_eff*timestep*3.41214116
    temp_to_load = Design_HTF_T_Hot
case(4)  !Mode 4: Charging, cycle off
    Flow_to_power_cycle = 0.
    Flow_to_heat_source = Flow_from_heat_source
    !If the flow exceeds the maximum flow rate to the tower, 
    !..partially defocus the field
    if (hour_to_store.ne.stored_hour) then
        Field_control=1
    else
        if (Flow_to_heat_source>=m_dot_max)then
            Field_control=min(derate*m_dot_max/Flow_to_heat_source,Field_control)
            derate=derate-.035
        else
            Field_control=Field_control
        endif
    endif
    hybrid_control = 0.
    Pump_on = 0.
    standby_control = 3.
    t_standby_remain = t_standby
    demand_var_control=P_ref
    cycle_pl_control=1.  !1=demand, 2=part load
    temp_to_load = Storage_hot_outlet_temp
case(5)  !Mode 5: All off
    Flow_to_power_cycle = 0.
    Flow_to_heat_source = 0.
    Field_control = 0.
    hybrid_control = 0.
    Pump_on = 0.
    standby_control = 3.
    t_standby_remain = t_standby
    demand_var_control=P_ref
    cycle_pl_control=1.  !1=demand, 2=part load
    temp_to_load = Storage_hot_outlet_temp
case(6) !Mode 6: Standby
    Flow_to_power_cycle = Load_flow_demand
    Flow_to_heat_source = Flow_from_heat_source
    Field_control = 1. 
    hybrid_control = 1. 
    Pump_on = 1. 
    t_standby_remain = t_standby_remain-timestep
    if(t_standby_remain<=0.) then
        standby_control = 3.
        flow_to_power_cycle = 0.
    else
        standby_control = 2.
    endif
    demand_var_control=0.
    cycle_pl_control=1.  !1=demand, 2=part load
    temp_to_load = Storage_hot_outlet_temp
end select


continue

!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
!SET THE STORAGE ARRAY AT THE END OF THIS ITERATION IF NECESSARY
!The (mode) and (hour_to_store) are used for convergence, meaning they are called 
!..and set during each iteration. The third parameter is not called until after convergence.
STORED(1)=float(Mode)
STORED(2)=Hour_to_store
STORED(4)=day_shutdown
CALL setStorageVars(STORED,NSTORED,INFO)
!-----------------------------------------------------------------------------------------------------------------------


!    SET THE OUTPUTS FROM THIS MODEL IN SEQUENTIAL ORDER AND GET OUT
! Flow_to_power_cycle
OUT(1)=Flow_to_power_cycle
! Flow_to_heat_source
OUT(2)=Flow_to_heat_source
! Field_control
OUT(3)=Field_control
! Supplemental heat source control
OUT(4)=hybrid_control			
! Pump_on
OUT(5)=Pump_on
! Standby control
OUT(6)=standby_control			
! Mode
OUT(7)=float(Mode)
! Demand variable control
OUT(8)=demand_var_control
! Cycle part-load control flag
OUT(9)=cycle_pl_control
! aux heater load
OUT(10)=q_aux ![MWt]
! aux heater fuel use
OUT(11)=q_aux_hv    ![MMBTU]
! temperature to the load (power cycle)
OUT(12)=temp_to_load-273.15d0  ![C]
! TES charge level
OUT(13)=TES_charge

!EVERYTHING IS DONE - RETURN FROM THIS SUBROUTINE AND MOVE ON
RETURN 1
END
!-----------------------------------------------------------------------------------------------------------------------