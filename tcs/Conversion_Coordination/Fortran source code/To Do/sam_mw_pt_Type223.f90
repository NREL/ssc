SUBROUTINE TYPE223 (TIME,XIN,OUT,T,DTDT,PAR,INFO,ICNTRL,*) 
!************************************************************************
! Object: Power Tower Logic - 2-tank storage
! Simulation Studio Model: Type223
! 
! Author: Michael J. Wagner
! Editor: Michael J. Wagner
! Date:	 June 17, 2008 
! Last modified: March 7, 2011
! COPYRIGHT 2009-2011 NATIONAL RENEWABLE ENERGY LABORATORY


! Doc. tables updated 7/1/2010 - MJW
!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Parameters
!    1| TSHOURS                          | Number of thermal storage hours                                   | hr               | hr               
!    2| NUMTOU                           | The number of Time-of-use periods                                 | none             | none             
!    3| TSLOGIC_A()                      | Array of solar dispatch fractions (with solar)                    | none             | none             
!    4| TSLOGIC_B()                      | Array of solar dispatch fractions (without solar)                 | none             | none             
!    5| TSLOGIC_C()                      | Array of turbine output fractions                                 | none             | none             
!    6| FFRAC()                          | Array of fossil fill fractions                                    | none             | none             

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Inputs
!    1| Hour_of_the_day                  | The hour of the day, from 1-24                                    | hr               | hr               
!    2| Start_hour                       | Not used in this model                                            | none             | none             
!    3| End_hour                         | Not used in this model                                            | none             | none             
!    4| Zenith                           | The solar zenith angle                                            | deg              | deg              
!    5| Storage_hot_outlet_temp          | The outlet temperature of the hot storage tank                    | C                | C                
!    6| min_temp_to_load                 | The minimum allowable HTF temperature to the load                 | C                | C                
!    7| Load_flow_demand                 | The HTF flow rate required by the load                            | kg/hr            | kg/hr            
!    8| Storage_cold_outlet_temp         | The outlet temperature of the cold storage tank                   | C                | C                
!    9| Max_temp_to_heat_source          | The maximum allowable HTF temperature to the load                 | C                | C                
!   10| Flow_from_heat_source            | The flow rate from the heat source                                | kg/hr            | kg/hr            
!   11| Temp_from_heat_source            | The temperature of HTF from the heat source                       | C                | C                
!   12| T_standby                        | The allowable length of time for cycle standby                    | hr               | hr               
!   13| m_dot_max                        | The maximum allowable HTF flow rate to the heat source            | kg/hr            | kg/hr            
!   14| Hot_tank_storage_level           | An indicator providing the hot tank fill status                   | none             | none             
!   15| Cold_tank_storage_level          | An indicator providing the cold tank fill status                  | none             | none             
!   16| Actual_hot_tank_volume           | The volume of HTF in the hot tank                                 | m3               | m3               
!   17| q_sby_frac                       | Fraction indicating thermal load requried for standby             | none             | none             
!   18| m_dot_htf_ref                    | The design HTF flow rate through the load                         | kg/hr            | kg/hr            
!   19| rho_htf                          | HTF density in the hot storage tank                               | kg/m3            | kg/m3            
!   20| TOUPeriod                        | The time-of-use period, per utility schedule                      | none             | none             
!   21| DNI_hrly                         | Hourly integrated DNI                                             | kJ/m2            | W/m2             
!   22| total_hot_tank_vol               | Total volume of a single storage tank                             | m3               | m3               
!   23| P_ref                            | Power produced by the power block at design                       | MWe              | MWe              
!   24| cycle_cutoff_frac                | The lower limit on power cycle operation (fraction)               | none             | none             
!   25| cycle_max_fraction               | The upper limit on power cycle operation (fraction)               | none             | none             
!   26| night_recirc                     | Flag indicating night-time receiver recirculation                 | none             | none             

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Outputs
!    1| Flow_to_power_cycle              | The flow rate to the power cycle (load)                           | kg/hr            | kg/hr            
!    2| Flow_to_heat_source              | The flow rate to the heat source (tower)                          | kg/hr            | kg/hr            
!    3| Field_control                    | Defocus control for field                                         | none             | none             
!    4| hybrid_control                   | Control for auxiliary (fossil) heat source                        | none             | none             
!    5| Pump_on                          | Control indicating operation of HTF pumps                         | none             | none             
!    6| standby_control                  | Control indicating power production mode                          | none             | none             
!    7| Mode                             | Control indicating operational scheme mode                        | none             | none             
!    8| demand_var_control               | Indicates either required power or provided mass flow             | MW or kg/hr      | MW or kg/hr      
!    9| cycle_pl_control                 | Control indicating which demand var is used                       | none             | none             
!   10| flow_to_storage                  | HTF flow rate into hot storage, from cold storage                 | kg/hr            | kg/hr            
!   11| flow_from_storage                | HTF flow rate into cold storage, from hot storage                 | kg/hr            | kg/hr            
!   12| recirc_source                    | Control for using HTF from hot tank, or elec. htr                 | none             | none             

!************************************************************************

!TRNSYS acess functions (allow to acess TIME etc.) 
USE TrnsysConstants
USE TrnsysFunctions

!-----------------------------------------------------------------------------------------------------------------------
!    REQUIRED BY THE MULTI-DLL VERSION OF TRNSYS
!DEC$ATTRIBUTES DLLEXPORT :: TYPE223				!SET THE CORRECT TYPE NUMBER HERE
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
!    TRNSYS DECLARATIONS
IMPLICIT NONE			!REQUIRES THE USER TO DEFINE ALL VARIABLES BEFORE USING THEM

real(8)::XIN, OUT, TIME, PAR, STORED, T, DTDT
INTEGER*4::INFO(15), np, ni, nout, nd, npar, nin, nder, iunit, itype, icntrl, nstored
CHARACTER*3::OCHECK, YCHECK
!
!-----------------------------------------------------------------------------------------------------------------------
!USER DECLARATIONS - SET THE MAXIMUM NUMBER OF PARAMETERS (NP), INPUTS (NI),
!OUTPUTS (NOUT), AND DERIVATIVES (ND) THAT MAY BE SUPPLIED FOR THIS TYPE
PARAMETER (NP=38,NI=26,NOUT=12,ND=0,NSTORED=7)
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!REQUIRED TRNSYS DIMENSIONS
DIMENSION XIN(NI),OUT(NOUT),PAR(NP),YCHECK(NI),OCHECK(NOUT),STORED(NSTORED),T(ND),DTDT(ND)
INTEGER:: i
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
!PARAMETERS
real(8),allocatable:: TSLOGIC_A(:),TSLOGIC_B(:),TSLOGIC_C(:),FFRAC(:), TSLOGIC(:), TSELECT(:)
real(8)::TSHOURS
integer::NUMTOU


!INPUTS
real(8)::Hour_of_the_day, Start_hour, End_hour, Zenith, Storage_hot_outlet_temp, min_temp_to_load, Load_flow_demand, &
         Storage_cold_outlet_temp, Max_temp_to_heat_source, Flow_from_heat_source,Temp_from_heat_source,T_standby, &
         m_dot_max, Hot_tank_storage_level, Cold_tank_storage_level, Actual_hot_tank_volume, q_sby_frac, m_dot_htf_ref, rho_htf,&
         TOUPeriod, DNI_hrly, total_hot_tank_vol, P_ref, cycle_cutoff_frac, cycle_max_fraction, night_recirc

!Local variables
integer::nitermax, j
real(8)::Stored_mode, Stored_hour, Hour_to_store, t_standby_remain,timestep, day_shutdown, DNI, Is_hybrid, niter, hot_tank_vol0,&
         proj_vol, charge_level, field_controlX, control_derate

!OUTPUTS
real(8)::Flow_to_power_cycle, Flow_to_heat_source, Field_control, hybrid_control, Pump_on, standby_control, Mode,&
         Demand_var_control, cycle_pl_control, flow_to_storage, flow_from_storage, recirc_source

save::field_controlX

!-----------------------------------------------------------------------------------------------------------------------
!RETRIEVE THE CURRENT VALUES OF THE INPUTS TO THIS MODEL FROM THE XIN ARRAY IN SEQUENTIAL ORDER

Hour_of_the_day=XIN(1)
Start_hour=XIN(2)
End_hour=XIN(3)
Zenith=XIN(4)
Storage_hot_outlet_temp=XIN(5)
min_temp_to_load=XIN(6)
Load_flow_demand=XIN(7)
Storage_cold_outlet_temp=XIN(8)
Max_temp_to_heat_source=XIN(9)
Flow_from_heat_source=XIN(10)
Temp_from_heat_source=XIN(11)
T_standby=XIN(12)
m_dot_max=XIN(13)
Hot_tank_storage_level=XIN(14)
Cold_tank_storage_level=XIN(15)
Actual_hot_tank_volume=XIN(16)
q_sby_frac=XIN(17)
m_dot_htf_ref=XIN(18)
rho_htf=XIN(19)
TOUPeriod=XIN(20)
DNI_hrly=XIN(21) ; DNI=DNI_hrly/3.6;
total_hot_tank_vol=XIN(22)
P_ref=XIN(23)
cycle_cutoff_frac=XIN(24)
cycle_max_fraction=XIN(25)
night_recirc=XIN(26)
    IUNIT=INFO(1)
    ITYPE=INFO(2)

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
    if(allocated(TSLOGIC)) deallocate(TSLOGIC_A,TSLOGIC_B,TSLOGIC_C,FFRAC, TSLOGIC, TSELECT)
    RETURN 1
ENDIF
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!    PERFORM ANY 'AFTER-ITERATION' MANIPULATIONS THAT ARE REQUIRED HERE
!    e.g. save variables to storage array for the next timestep
IF (INFO(13).GT.0) THEN
    STORED(3)=t_standby_remain
    STORED(4)=day_shutdown
    STORED(5)=1
    STORED(6)=actual_hot_tank_volume
    STORED(7)=is_hybrid
    CALL setStorageVars(STORED,NSTORED,INFO)
    RETURN 1
ENDIF
!
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!DO ALL THE VERY FIRST CALL OF THE SIMULATION MANIPULATIONS HERE
IF (INFO(7).EQ.-1) THEN

    !-----------------------------------------------------------------------------------------------------------------------
    !READ IN THE VALUES OF THE PARAMETERS IN SEQUENTIAL ORDER
    TSHOURS=PAR(1)
    NUMTOU=int(PAR(2))
    !Initialize TSLOGIC 
    allocate(TSLOGIC_A(NUMTOU),TSLOGIC_B(NUMTOU),TSLOGIC_C(NUMTOU),FFRAC(NUMTOU), TSLOGIC(3*NUMTOU), TSELECT(NUMTOU))
    TSLOGIC=0.; TSLOGIC_A=0.; TSLOGIC_B=0.; TSLOGIC_C=0.; TSELECT=0.d0
    TSLOGIC(1:(3*NUMTOU))=PAR(3:(3*NUMTOU+2))             !MJW: Number of TOU periods changed 4/21/2010 
    FFRAC(1:NUMTOU)=PAR((3*(NUMTOU+1)):(3*(NUMTOU+1)+5))


    do i=1,NUMTOU
        TSLOGIC_A(i)=TSLOGIC(1+(i-1)*3)
        TSLOGIC_B(i)=TSLOGIC(2+(i-1)*3)
        TSLOGIC_C(i)=TSLOGIC(3+(i-1)*3)
    enddo

    INFO(6)=NOUT				
    INFO(9)=1				
    INFO(10)=0	!STORAGE FOR VERSION 16 HAS BEEN CHANGED				
    NIN=NI
    NPAR=2+4*NUMTOU
    NDER=ND
    	       
    !CALL THE TYPE CHECK SUBROUTINE TO COMPARE WHAT THIS COMPONENT REQUIRES TO WHAT IS SUPPLIED IN 
    !THE TRNSYS INPUT FILE
    CALL TYPECK(1,INFO,NIN,NPAR,NDER)

    !SET THE NUMBER OF STORAGE SPOTS NEEDED FOR THIS COMPONENT
    CALL setStorageSize(NSTORED,INFO)

    !initialize the value of the day_shutdown variable
    day_shutdown=0

    !RETURN TO THE CALLING PROGRAM
    RETURN 1

ENDIF
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!DO ALL OF THE INITIAL TIMESTEP MANIPULATIONS HERE - THERE ARE NO ITERATIONS AT THE INTIAL TIME
IF (TIME .LT. (getSimulationStartTime() + getSimulationTimeStep()/2.D0)) THEN

    !SET THE UNIT NUMBER FOR FUTURE CALLS
    IUNIT=INFO(1)
    ITYPE=INFO(2)

    ! Initial values
    OUT(:) = 0.d0
    OUT(3) = 1.d0
    OUT(6) = 1.d0
    OUT(9) = 1.d0

    !Check to see whether to use the fossil fuel backup scheme
    is_hybrid=0.
    if(sum(FFRAC(1:6)).gt.0.001) is_hybrid=1.

    !PERFORM ANY REQUIRED CALCULATIONS TO SET THE INITIAL STORAGE VARIABLES HERE
    t_standby_remain = t_standby
    STORED(1)=0.  !mode
    STORED(2)=0.  !initial hour to store
    STORED(3)=t_standby_remain   !The initial standby time period
    STORED(4)=day_shutdown  !is the cycle off for the day
    STORED(6)=Actual_hot_tank_volume  !initial setting for storage volume
    STORED(7)=is_hybrid

    !PUT THE STORED ARRAY IN THE GLOBAL STORED ARRAY
    CALL setStorageVars(STORED,NSTORED,INFO)        !MJW 10.19.2010:: NITEMS was used instead of NSTORED, but it hadn't been set to any value

    !RETURN TO THE CALLING PROGRAM
    RETURN 1

ENDIF
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!RETRIEVE THE VALUES IN THE STORAGE ARRAY FOR THIS ITERATION
CALL getStorageVars(STORED,NSTORED,INFO)
stored_mode=STORED(1)
stored_hour=STORED(2)
t_standby_remain = STORED(3)
day_shutdown=STORED(4)
niter=STORED(5)
hot_tank_vol0=STORED(6)
is_hybrid = STORED(7)

timestep = getSimulationTimeStep()
      
!----------------------------------------------------------------------
!Set variables for every iteration
nitermax = 3.
day_shutdown=0
!****************************************
!limit the requested power to the cycle max fraction
do j=1,NUMTOU
    TSLOGIC_C(j)=dmin1(TSLOGIC_C(j),cycle_max_fraction)
enddo

!Approximate the projected volume level for a plant running at full capacity for the next timestep
!Assume the minimum allowable volume in the tank is 10% of the max volume
proj_vol=Actual_hot_tank_volume-m_dot_htf_ref/rho_htf- total_hot_tank_vol*0.1 	

!Calculate the charge level in the hot storage tank
charge_level=actual_hot_tank_volume/total_hot_tank_vol

!___Define mode selection logic___

!Use a "sticky mode" strategy.  Always use the mode that is calculated in the
!..third iteration.  This prevents non-convergence. Stick at 4 iterations 
Hour_to_store = Hour_of_the_day
if(info(7) > 0) then !mjw 3.7.11
    if(niter > nitermax) then
        Mode = Stored_mode
        goto 100
    endif
else
    !mjw 3.7.11 Set the initial field defocus value to 1.
    field_control = 1.d0
endif
	

if(DNI.gt.1.) then   !The solar resource is available above 1 W/m^2
    TSELECT=TSLOGIC_B
else                  !Use the dispatch for no solar resource
    TSELECT=TSLOGIC_A
endif

!Do a check to see if the user has intentionally zeroed out turbine production for the hour. 
!If so (and this only applies for systems with storage), switch to a dedicated mode (11)
if((TSLOGIC_C(int(TOUPeriod)).lt.cycle_cutoff_frac).and.(TSHOURS.gt.0.01)) then
    mode=11
    goto 100
endif
      

if(flow_from_heat_source > m_dot_htf_ref) then !Flow from the tower exceeds the rated cycle flow rate
    if(TSHOURS.gt.0.01) then  !The system has storage
        if((hot_tank_storage_level.eq.1.).or.(cold_tank_storage_level.eq.-1.)) then  !if the hot tank is full or the cold tank is empty
            mode=6 !Cycle on, tower on defocus to demand
        else
            mode=7 !Cycle on, tower on, charge storage
        endif
    else
        mode=6 !Cycle on, tower on, defocus to demand
    endif
else  !Flow from the tower does not meet the design cycle mass flow rate
    if(TSHOURS.gt.0.01) then !The system has storage
        if((charge_level.ge.TSELECT(int(TOUperiod))).and.((hot_tank_storage_level.ne.-1.).and.(cold_tank_storage_level.ne.1.))) then !Storage charge is above dispatch limit
            if(is_hybrid.eq.1.) then
                if(TSLOGIC_C(int(TOUperiod)).gt.FFRAC(int(TOUperiod))) then !The turbine output fraction is greater than the fossil fill fraction
                    mode=4 !Cycle on, tower on, discharge storage
                else  !The fossil fill fraction is greater than the turbine output fraction
                    mode=5 !All on, discharge storage, hybrid on
                endif
            else
                mode=4 !Cycle on, tower on, discharge storage
            endif
        else !Storage charge level is below dispatch limit
            if(is_hybrid.eq.1.) then
                if(flow_from_heat_source.ge.(FFRAC(int(TOUperiod))*m_dot_htf_ref)) then !flow to the load is greater than the fossil fraction
                    mode=2 !Cycle on at continuous part load, tower on
                else  !The fossil fill fraction is greater than the flow to the load
                    mode=3 !Cycle on, tower on, meet demand with hybrid
                endif
            else !The plant doesn't have hybrid backup
                if(flow_from_heat_source.ge.(m_dot_htf_ref*cycle_cutoff_frac)) then !The flow from the heat source exceeds the cutoff fraction
                    mode=2  !Cycle on at continuous part load, tower on
                else !Flow from the heat source is less than the cutoff fraction
                    if(t_standby.gt.0.) then
                        if((hot_tank_storage_level.ne.-1.).and.(cold_tank_storage_level.ne.1.)) then  !if storage is not completely empty
                            mode=8
                        else  !No storage charge remains
                            mode=9
                        endif
                    else
                        if((hot_tank_storage_level.eq.1.).or.(cold_tank_storage_level.eq.-1.)) then !if storage is completely full
                            mode=1
                        else !Storage is not full, keep charging with tower flow, keep cycle off
                            mode=9
                        endif
                    endif
                endif
            endif
        endif
    else  !The system does not have storage
        if(is_hybrid.eq.1.) then
            if(flow_from_heat_source.ge.(FFRAC(int(TOUperiod))*m_dot_htf_ref)) then !flow to the load is greater than the fossil fraction
                mode=2 !Cycle on at continuous part load, tower on
            else  !The fossil fill fraction is greater than the flow to the load
                mode=3 !Cycle on, tower on, meet demand with hybrid
            endif
        else !The plant doesn't have hybrid backup
            if(flow_from_heat_source.ge.(m_dot_htf_ref*cycle_cutoff_frac)) then !The flow from the heat source exceeds the cutoff fraction
                if(flow_from_heat_source.ge.(m_dot_htf_ref*cycle_max_fraction)) then  !The flow from the heat source exceeds the limit
                    mode=6 !Cycle on at part load, tower on, no storage
                else  !
                    mode=2  !Cycle on at continuous part load, tower on
                endif
            else !Flow from the heat source is less than the cutoff fraction
                mode=10 !Cycle off, defocus field
            endif
        endif
    endif
endif




!Select case
100	select case(int(mode))
!Define cases
case(1) !All off, defocus field. Storage is completely full. 
    Flow_to_power_cycle = 0.
    Flow_to_heat_source = 0.
    Field_control = 0.
    hybrid_control = 0.
    Pump_on = 0.
    standby_control = 3.  !1=normal operation, 2=standby, 3=shut down
    t_standby_remain = t_standby
    demand_var_control=0.
    cycle_pl_control=1.  !1=demand, 2=part load
    flow_to_storage=0.
    flow_from_storage=0.
    recirc_source = 0.  !Note in this case that no recirculation is needed because
                        !..in theory, the field could provide flux to offset freezing
case(2) !Cycle on at continuous part load, no hybrid or storage discharge
    Flow_to_power_cycle = Flow_from_heat_source
    Flow_to_heat_source = Flow_from_heat_source
    !Field_control = 1. mjw 3.9.11
    hybrid_control = 0.
    pump_on = 1.
    standby_control = 1.
    t_standby_remain = t_standby
    demand_var_control=Flow_from_heat_source
    cycle_pl_control=2.
    flow_to_storage=0.
    flow_from_storage=0.      
    recirc_source = 0.  !DNI>0 for this case
    !For the special case where no storage exists, switch to mode 6 if
    !..the flow to the power cycle exceeds the maximum
    if(((flow_from_heat_source.gt.(m_dot_htf_ref*cycle_max_fraction)).and.(niter.gt.nitermax)).and.(TSHOURS.le..01)) then
        mode = 6
        goto 100
    endif
     
case(3) !Cycle on, tower on, meet demand with hybrid
    Flow_to_power_cycle = Load_flow_demand
    Flow_to_heat_source = Flow_from_heat_source
    Field_control = 1.
    hybrid_control = 1.
    pump_on = 1.
    standby_control = 1.
    t_standby_remain = t_standby
    demand_var_control=P_ref*FFRAC(int(TOUperiod))
    cycle_pl_control=1.
    !Choose recirculation method
    flow_to_storage=0.
    flow_from_storage=0.
    recirc_source = 0.  !DNI>0 for this case

case(4) !Cycle on, tower on, discharge storage
    Flow_to_power_cycle = Load_flow_demand
    Flow_to_heat_source = Flow_from_heat_source
    hybrid_control = 0.
    pump_on = 1.
    standby_control = 1.
    t_standby_remain = t_standby
    demand_var_control=P_ref*TSLOGIC_C(int(TOUperiod))
    cycle_pl_control=1.
    flow_to_storage=0.
    flow_from_storage=Load_flow_demand-Flow_from_heat_source
    recirc_source = 0.  !DNI>0
    if(info(7)==0) then !mjw 3.7.11
        Field_control = 1.
    endif
    
    !MJW 7.12.2010
    if(field_control < 1.d0) then
        mode = 6
        goto 100
    endif
    
    if(flow_from_storage.lt.0.) then !.and.(niter.gt.nitermax)) then 
        if(Load_flow_demand.gt.0.) then
            Mode=7
        else
            if((t_standby.gt.0.).and.((hot_tank_storage_level.ne.-1.).and.(cold_tank_storage_level.ne.1.))) then
                Mode=8
            else
                Mode=9
            endif
        endif
        goto 100
    endif
      
case(5) !All on, discharge storage, hybrid on
    Flow_to_power_cycle = Load_flow_demand
    Flow_to_heat_source = Flow_from_heat_source
    Field_control = 1.
    hybrid_control = 1.
    pump_on = 1.
    standby_control = 1.
    t_standby_remain = t_standby
    demand_var_control = P_ref*FFRAC(int(TOUperiod))
    cycle_pl_control = 1.
    if(DNI.gt.1.)then
        flow_to_storage=0.
        flow_from_storage=Load_flow_demand*TSLOGIC_C(int(TOUperiod))-Flow_from_heat_source
    else
        flow_to_storage=0.
        flow_from_storage=Load_flow_demand*TSLOGIC_C(int(TOUperiod))+Flow_from_heat_source
    endif
    recirc_source = 1. !Storage
      
case(6) !cycle on, tower on, defocus to demand
        Flow_to_power_cycle = Load_flow_demand
        Flow_to_heat_source = Flow_from_heat_source
    if(TSHOURS.gt.0.01) then !MJW 7.12.2010
        flow_to_storage= dmax1(flow_from_heat_source - flow_to_power_cycle, 0.d0) !MJW 7.12.2010 !0.
        flow_to_storage = dmin1(flow_to_storage, dmax1((total_hot_tank_vol - hot_tank_vol0)/timestep*rho_htf,0.d0)) !MJW 7.12.2010
    else
        flow_to_storage=0.d0
    endif
    !If the hot storage is full, but the cycle can run, 
    !..partially defocus the field to avoid overfilling the tank
    if(info(7)==0) then !mjw 3.7.11
        Field_control=1.d0; field_controlX=1.d0
    else        !Iterative calls, calculate a defocus factor
        if(niter.gt.22.) then   !After a certain number of iterations, it is likely that the derate factor is not sufficient.  Modify this factor to speed convergence
            control_derate=.01
        elseif(niter.gt.13.) then
            control_derate=.005
        elseif(niter.gt.7.) then
            control_derate=.002
        else
            control_derate=.001
        endif        
        if (dabs((Flow_to_heat_source - (Load_flow_demand + flow_to_storage))/(Load_flow_demand + flow_to_storage)) > control_derate)then  !MJW 7.12.2010
            !Field_control=(Load_flow_demand + flow_to_storage)/Flow_to_heat_source  !MJW 7.12.2010
            !Field_control=dmin1(1.d0, Field_controlX*((Load_flow_demand + flow_to_storage)/dmax1(1.d0, Flow_to_heat_source)))    !MJW 10.31.2010
            Field_control=dmin1(1.d0, Field_controlX*((m_dot_htf_ref + flow_to_storage)/dmax1(1.d0, Flow_to_heat_source)))    !mjw 3.9.11
            Field_controlX = Field_control
        else
            Field_control=Field_control
        endif
    endif
    hybrid_control = 0.
    Pump_on = 1.
    standby_control = 1.
    t_standby_remain = t_standby
    demand_var_control=P_ref*TSLOGIC_C(int(TOUperiod))
    cycle_pl_control = 1.
    flow_from_storage=0.
    recirc_source = 0. !DNI>0
      
case(7)  !Cycle on, tower on, charge storage
    Flow_to_power_cycle = Load_flow_demand
    Flow_to_heat_source = Flow_from_heat_source
    !Do a check here to make sure the HTF flow rate through the receiver does not exceed the maximum flow rate
    !.. If it does, defocus the field accordingly and recalculate outputs.
    if(info(7)==0) then !mjw 3.7.11
        Field_control=1
    else        !Iterative calls, calculate a defocus factor
        if (Flow_to_heat_source.ge.m_dot_max)then
            if(niter.gt.19.) then   !After a certain number of iterations, it is likely that the derate factor is not sufficient.  Modify this factor to speed convergence
                control_derate=.75
            elseif(niter.gt.13.) then
                control_derate=.85
            elseif(niter.gt.7.) then
                control_derate=.92
            else
                control_derate=.96
            endif
            Field_control=control_derate*m_dot_max/Flow_to_heat_source
        else
            Field_control=Field_control
        endif
    endif
    hybrid_control = 0.
    pump_on = 1.
    standby_control = 1.
    t_standby_remain = t_standby
    demand_var_control=P_ref*TSLOGIC_C(int(TOUperiod))
    cycle_pl_control=1.
    flow_to_storage=Flow_from_heat_source-Load_flow_demand
    flow_from_storage=0.
    recirc_source = 0.  !DNI>0

    if(flow_to_storage.lt.0.) then 
        if(.not.((t_standby.gt.0.).and.((hot_tank_storage_level.ne.-1.).and.(cold_tank_storage_level.ne.1.)))) then
            Mode=9
            goto 100
        endif
    endif
      
      
case(8) !cycle standby, tower on
    Flow_to_power_cycle = Load_flow_demand
    Flow_to_heat_source = Flow_from_heat_source
    Field_control = 1. 
    !MJW 8.5.2010
    if(is_hybrid==1.) then
        hybrid_control = 1. 
    else
        hybrid_control = 0.
    endif
    Pump_on = 1. 
    t_standby_remain = t_standby_remain-timestep
    if(t_standby_remain.le.0.) then
        standby_control = 3.
        flow_to_power_cycle = 0.
    else
        standby_control = 2.
    endif
    demand_var_control=0.
    cycle_pl_control=1.
    flow_to_storage=0.
    !Choose recirculation method
    if(DNI.gt.1.) then
        recirc_source = 0.
        flow_from_storage=max(Flow_from_heat_source - Load_flow_demand, 0.)
    else
        recirc_source = 1. !from hot tank. We know there is storage for mode 8
        flow_from_storage = Flow_from_heat_source + Load_flow_demand
    endif

case(9) !Cycle off, tower on, charge storage
    Flow_to_power_cycle = 0.
    Flow_to_heat_source = Flow_from_heat_source
    !Do a check here to make sure the HTF flow rate through the receiver does not exceed the maximum flow rate
    !.. If it does, defocus the field accordingly and recalculate outputs.
    if(info(7)==0) then !mjw 3.7.11
        Field_control=1
    else        !Iterative calls, calculate a defocus factor
        if (Flow_to_heat_source.ge.m_dot_max)then
            if(niter.gt.19.) then   !After a certain number of iterations, it is likely that the derate factor is not sufficient.  Modify this factor to speed convergence
                control_derate=.75
            elseif(niter.gt.13.) then
                control_derate=.85
            elseif(niter.gt.7.) then
                control_derate=.92
            else
                control_derate=.96
            endif
            Field_control=control_derate*m_dot_max/Flow_to_heat_source
        else
            Field_control=Field_control
        endif
    endif
    hybrid_control = 0.
    pump_on = 1.
    standby_control = 3.
    t_standby_remain = t_standby
    demand_var_control=0.
    cycle_pl_control=1.
    
    if(dni.gt.1.) then
        flow_to_storage=Flow_from_heat_source
        flow_from_storage=0.
    else
        flow_to_storage=0.
        flow_from_storage=Flow_from_heat_source
    endif
    
    if((hot_tank_storage_level.eq.-1.).or.(cold_tank_storage_level.eq.1.)) then !if storage is completely empty
        recirc_source = 2. !Electric heater
    else
        recirc_source = 1. !from hot tank
    endif
      
case(10) !Special case for no storage, no hybrid.  The flow 
        !..provided by the tower is less than the cutoff fraction.  Make sure this is the case
        !..when converged.  If so, defocus the field for the time step only.
    Flow_to_power_cycle = 0.
    Flow_to_heat_source = Flow_from_heat_source
    Field_control = 1.
    hybrid_control = 0.
    pump_on = 1.
    standby_control = 3.
    t_standby_remain = t_standby
    demand_var_control=Flow_from_heat_source
    cycle_pl_control=2.
    flow_to_storage=0.
    flow_from_storage=0.      
    if(niter.gt.nitermax+1) then
        Mode = 1
        goto 100
    endif
    recirc_source = 2. !electric heater
      
case(11) !special case where the system has storage, but the user has specifically set the
    !..dispatch level for the turbine during this time period to zero.  The tower will still 
    !..generate thermal energy, so long as storage is not full. The power block will not run.
    Flow_to_power_cycle = 0.
    Flow_to_heat_source = Flow_from_heat_source
    !Do a check here to make sure the HTF flow rate through the receiver does not exceed the maximum flow rate
    !.. If it does, defocus the field accordingly and recalculate outputs.
    if(info(7)==0) then !mjw 3.7.11
        Field_control=1
    else        !Iterative calls, calculate a defocus factor
        if (Flow_to_heat_source.ge.m_dot_max)then
            if(niter.gt.19.) then   !After a certain number of iterations, it is likely 
                                    !that the derate factor is not sufficient.  Modify
                                    !this factor to speed convergence
                control_derate=.75
            elseif(niter.gt.13.) then
                control_derate=.85
            elseif(niter.gt.7.) then
                control_derate=.92
            else
                control_derate=.96
            endif
            Field_control=control_derate*m_dot_max/Flow_to_heat_source
        else
            Field_control=Field_control
        endif
    endif
    field_control = 1.
    hybrid_control = 0.
    pump_on = 1.
    standby_control = 3.
    t_standby_remain = t_standby
    demand_var_control = 0.
    day_shutdown = 0.
    cycle_pl_control = 2.
    if(dni.gt.1.) then
        flow_to_storage=Flow_from_heat_source
        flow_from_storage=0.
    else
        flow_to_storage=0.
        flow_from_storage=Flow_from_heat_source
    endif
    if((hot_tank_storage_level.eq.-1.).or.(cold_tank_storage_level.eq.1.)) then !if storage is completely empty
        recirc_source = 2. !Electric heater
    else
        recirc_source = 1. !from hot tank
    endif

end select

!-----------------------------------------------------------------------------------------------------------------------
!    SET THE STORAGE ARRAY AT THE END OF THIS ITERATION IF NECESSARY
!     The (mode) and (hour_to_store) are used for convergence, meaning they are called 
!     ..and set during each iteration. The third parameter is not called until after convergence.
STORED(1)=Mode
STORED(2)=Hour_to_store
STORED(4)=day_shutdown
STORED(5)=niter+1
CALL setStorageVars(STORED,NSTORED,INFO)

!Set outputs

!Flow_to_power_cycle
OUT(1)=Flow_to_power_cycle
!Flow_to_heat_source
OUT(2)=Flow_to_heat_source
!Field_control
OUT(3)=Field_control
!Supplemental heat source control
OUT(4)=hybrid_control			
!Pump_on
OUT(5)=Pump_on
!Standby control
OUT(6)=standby_control			
!Mode
OUT(7)=Mode         			
!Rankine cycle demand variable controller
OUT(8)=demand_var_control
!Cycle part load control
OUT(9)=cycle_pl_control
!Hot HTF flow from heat source diverted to storage
OUT(10)=flow_to_storage
!Hot HTF flow from storage to power block
OUT(11)=flow_from_storage
!Recirculation source [0=no recirculation, 1=storage, 2=electric heater]
OUT(12)=recirc_source            

!EVERYTHING IS DONE - RETURN FROM THIS SUBROUTINE AND MOVE ON
    RETURN 1
END
!-----------------------------------------------------------------------------------------------------------------------

