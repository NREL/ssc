SUBROUTINE TYPE228 (TIME,XIN,OUT,T,DTDT,PAR,INFO,ICNTRL,*) 
!************************************************************************
! Object: Power Tower Parasitics
! Simulation Studio Model: Type228
! 
! Author: Michael J Wagner
! Editor: 
! Date:	 January 27, 2009 
! Last modified: August 5th, 2010
! COPYRIGHT 2010 NATIONAL RENEWABLE ENERGY LABORATORY

! Doc. tables updated 2010-08-05 - MJW
!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Parameters
!    1| P_storage_pump                   | Storage pump power, rated per MWt of storage use                  | MWe/MWt          | MWe/MWt          
!    2| Piping_loss                      | Thermal loss per meter of piping                                  | Wt/m             | Wt/m             
!    3| Piping_length                    | Total length of exposed piping                                    | m                | m                
!    4| design_power                     | Power production at design conditions                             | MWe              | MWe              
!    5| Recirc_htr_eff                   | Recirculation heater efficiency                                   | none             | none             
!    6| design_eff                       | Power cycle efficiency at design                                  | none             | none             
!    7| night_recirc                     | Flag indicating whether night recirculation is allowed            | none             | none             
!    8| pb_fixed_par                     | Fixed parasitic load - runs at all times                          | none             | none             
!    9| aux_par                          | Aux heater, boiler parasitic                                      | MWe/MWcap        | MWe/MWcap        
!   10| aux_par_f                        | Aux heater, boiler parasitic - multiplying fraction               | none             | none             
!   11| aux_par_0                        | Aux heater, boiler parasitic - constant coefficient               | none             | none             
!   12| aux_par_1                        | Aux heater, boiler parasitic - linear coefficient                 | none             | none             
!   13| aux_par_2                        | Aux heater, boiler parasitic - quadratic coefficient              | none             | none             
!   14| bop_par                          | Balance of plant parasitic power fraction                         | MWe/MWe          | MWe/MWe          
!   15| bop_par_f                        | Balance of plant parasitic power fraction - multiplying fraction  | none             | none             
!   16| bop_par_0                        | Balance of plant parasitic power fraction - constant coeff        | none             | none             
!   17| bop_par_1                        | Balance of plant parasitic power fraction - linear coeff          | none             | none             
!   18| bop_par_2                        | Balance of plant parasitic power fraction - quadratic coeff       | none             | none             
!   19| storage_bypass                   | Flag indicating whether the hot salt pump always runs w/ PB       | none             | none             

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Inputs
!    1| Flow_from_storage                | Flow rate from storage                                            | kg/hr            | kg/hr            
!    2| P_cooling_tower                  | Cooling tower parasitic power fraction                            | MWe/MWe          | MWe/MWe          
!    3| Tower_pump_power                 | Reported tower pump power                                         | MWe              | MWe              
!    4| P_helio_track                    | Reported heliostat tracking power                                 | MWe              | MWe              
!    5| Plant_power_output               | Reported plant power output                                       | MWe              | MWe              
!    6| Power_cycle_efficiency           | Power cycle efficiency                                            | none             | none             
!    7| P_cold_tank                      | Cold tank heater parasitic power                                  | MWe              | MWe              
!    8| P_hot_tank                       | Hot tank heater parasitic power                                   | MWe              | MWe              
!    9| P_tower_conv                     | Reported tower convection loss                                    | MWt              | MWt              
!   10| P_tower_rad                      | Reported tower radiation loss                                     | MWt              | MWt              
!   11| Recirc_source                    | Recirculation heater control                                      | none             | none             
!   12| ref_htf_flow                     | HTF flow rate through the power cycle at design                   | kg/hr            | kg/hr            
!   13| aux_power                        | Auxiliary heater thermal power output                             | MWt              | MWt              

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Outputs
!    1| P_storage_pump_tot               | Total storage pump parasitic power                                | MWe              | MWe              
!    2| P_plant_balance_tot              | Total balance of plant parasitic power                            | MWe              | MWe              
!    3| P_cooling_tower_tot              | Total cooling tower parasitic power                               | MWe              | MWe              
!    4| P_piping_tot                     | Total piping loss parasitic power                                 | MWe              | MWe              
!    5| P_parasitics                     | Overall parasitic losses                                          | MWe              | MWe              
!    6| -not named-                      | Power to the grid after parasitic losses                          | MWe              | MWe              
!    7| P_tank_heater                    | Total tank heater parasitic power                                 | MWe              | MWe              
!    8| P_power_par                      | Total tower heater parasitic loss                                 | MWe              | MWe              
!    9| P_Fixed                          | Total fixed parasitic loss                                        | MWe              | MWe              
!   10| P_aux                            | Total auxiliary heater parasitic loss                             | MWe              | MWe              

!************************************************************************

!    TRNSYS acess functions (allow to acess TIME etc.) 
USE TrnsysConstants
USE TrnsysFunctions

!-----------------------------------------------------------------------------------------------------------------------
!    REQUIRED BY THE MULTI-DLL VERSION OF TRNSYS
!DEC$ATTRIBUTES DLLEXPORT :: TYPE228				!SET THE CORRECT TYPE NUMBER HERE
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
!    TRNSYS DECLARATIONS
IMPLICIT NONE			!REQUIRES THE USER TO DEFINE ALL VARIABLES BEFORE USING THEM

real(8):: xin, out, time, par, stored, t, dtdt
integer:: INFO(15), np, ni, nout, nd, npar, nin, nder, iunit, itype, icntrl, nstored
CHARACTER*3 OCHECK		!AN ARRAY TO BE FILLED WITH THE CORRECT VARIABLE TYPES FOR THE OUTPUTS
CHARACTER*3 YCHECK		!AN ARRAY TO BE FILLED WITH THE CORRECT VARIABLE TYPES FOR THE INPUTS
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!    USER DECLARATIONS - SET THE MAXIMUM NUMBER OF PARAMETERS (NP), INPUTS (NI),
!    OUTPUTS (NOUT), AND DERIVATIVES (ND) THAT MAY BE SUPPLIED FOR THIS TYPE
PARAMETER (NP=19,NI=13,NOUT=10,ND=0,NSTORED=0)
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!    REQUIRED TRNSYS DIMENSIONS
DIMENSION XIN(NI),OUT(NOUT),PAR(NP),YCHECK(NI),OCHECK(NOUT),STORED(NSTORED),T(ND),DTDT(ND)
INTEGER NITEMS
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
!    ADD DECLARATIONS AND DEFINITIONS FOR THE USER-VARIABLES HERE

!    INPUTS
real(8):: P_storage_pump, Flow_from_storage, P_plant_balance, P_cooling_tower, Plant_power_output,&
        Tower_pump_power, P_helio_track,Power_cycle_efficiency, Piping_loss, Piping_length, design_power,&
        P_tank_heater,P_cold_tank, P_hot_tank, P_tower_conv, P_tower_rad, recirc_htr_eff, recirc_source, &
        design_eff, ref_htf_flow, night_recirc, pb_fixed_par, aux_par, aux_par_f, aux_par_0, aux_par_1,&
        aux_par_2, bop_par, bop_par_f, bop_par_0, bop_par_1, bop_par_2, storage_bypass, aux_power
     
!     LOCALS
DOUBLE PRECISION P_piping_tot, P_fixed, P_ratio, aux_ratio, P_aux

!     OUTPUTS
DOUBLE PRECISION P_storage_pump_tot, P_plant_balance_tot, P_cooling_tower_tot, P_parasitics, P_tower_par

!-----------------------------------------------------------------------------------------------------------------------
!    RETRIEVE THE CURRENT VALUES OF THE INPUTS TO THIS MODEL FROM THE XIN ARRAY IN SEQUENTIAL ORDER

Flow_from_storage = XIN(1)
P_cooling_tower = XIN(2)
Tower_pump_power = XIN(3)
P_helio_track = XIN(4)
Plant_power_output = XIN(5)
Power_cycle_efficiency = XIN(6)
P_cold_tank = XIN(7)
P_hot_tank = XIN(8)
P_tower_conv = XIN(9)
P_tower_rad = XIN(10)
Recirc_source = XIN(11)     !-> 0=No recirculation, 1=Storage, 2=electric heater**
ref_htf_flow = XIN(12)      !HTF flow rate at design [kg/hr]
aux_power = XIN(13)
    IUNIT=INFO(1)
    ITYPE=INFO(2)

!-----------------------------------------------------------------------------------------------------------------------
!    SET THE VERSION INFORMATION FOR TRNSYS
IF(INFO(7).EQ.-2) THEN
    INFO(12)=16
    RETURN 1
ENDIF
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!    DO ALL THE VERY LAST CALL OF THE SIMULATION MANIPULATIONS HERE
IF (INFO(8).EQ.-1) THEN
    RETURN 1
ENDIF
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!    PERFORM ANY 'AFTER-ITERATION' MANIPULATIONS THAT ARE REQUIRED HERE
!    e.g. save variables to storage array for the next timestep
IF (INFO(13).GT.0) THEN
    !STORED(1)=cycle_mode
    !CALL setStorageVars(STORED,NSTORED,INFO)
    RETURN 1
ENDIF
!
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!    DO ALL THE VERY FIRST CALL OF THE SIMULATION MANIPULATIONS HERE
IF (INFO(7).EQ.-1) THEN

    !       SET SOME INFO ARRAY VARIABLES TO TELL THE TRNSYS ENGINE HOW THIS TYPE IS TO WORK
    INFO(6)=NOUT				
    INFO(9)=1				
    INFO(10)=0	!STORAGE FOR VERSION 16 HAS BEEN CHANGED				

    !SET THE REQUIRED NUMBER OF INPUTS, PARAMETERS AND DERIVATIVES THAT THE USER SHOULD SUPPLY IN THE INPUT FILE
    !IN SOME CASES, THE NUMBER OF VARIABLES MAY DEPEND ON THE VALUE OF PARAMETERS TO THIS MODEL....
    NIN=NI
    NPAR=NP
    NDER=ND

    !CALL THE TYPE CHECK SUBROUTINE TO COMPARE WHAT THIS COMPONENT REQUIRES TO WHAT IS SUPPLIED IN 
    !THE TRNSYS INPUT FILE
    CALL TYPECK(1,INFO,NIN,NPAR,NDER)
    !SET THE NUMBER OF STORAGE SPOTS NEEDED FOR THIS COMPONENT
    !CALL setStorageSize(NSTORED,INFO)

    !RETURN TO THE CALLING PROGRAM
    RETURN 1

ENDIF
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!    DO ALL OF THE INITIAL TIMESTEP MANIPULATIONS HERE - THERE ARE NO ITERATIONS AT THE INTIAL TIME
IF (TIME .LT. (getSimulationStartTime() + getSimulationTimeStep()/2.D0)) THEN

    !SET THE UNIT NUMBER FOR FUTURE CALLS
    IUNIT=INFO(1)
    ITYPE=INFO(2)

    !Retrieve the parameters
    P_storage_pump = PAR(1)
    Piping_loss = PAR(2)
    Piping_length = PAR(3)
    design_power = PAR(4)
    Recirc_htr_eff = PAR(5)
    design_eff = PAR(6)
    night_recirc = PAR(7)
    pb_fixed_par = PAR(8)
    aux_par = PAR(9)
    aux_par_f = PAR(10)
    aux_par_0 = PAR(11)
    aux_par_1 = PAR(12)
    aux_par_2 = PAR(13)
    bop_par = PAR(14)
    bop_par_f = PAR(15)
    bop_par_0 = PAR(16)
    bop_par_1 = PAR(17)
    bop_par_2 = PAR(18)
    storage_bypass = PAR(19)

    !initialize outputs
    OUT(:) = 0.d0
    
    !RETURN TO THE CALLING PROGRAM
    RETURN 1

ENDIF

!-------Called every time----------

P_ratio = plant_power_output/design_power
aux_ratio = aux_power/design_power/design_eff

if(storage_bypass == 1.d0) then 
    !Hot pump only operates when storage is dispatched
    P_storage_pump_tot = P_storage_pump*Flow_from_storage/max(Ref_HTF_flow,1.e-6)*design_power/design_eff  !MWe
else
    !Hot pump operates when any hot HTF is sent to the power block
    P_storage_pump_tot = P_storage_pump*Plant_power_output/design_eff !MWe   MJW 8.5.2010
endif

P_fixed = pb_fixed_par * design_power   !MWe    MJW 8.5.2010

!P_plant_balance_tot = P_plant_balance*Plant_power_output !MWe
if(plant_power_output > 0.d0) then
    !MJW 8.5.2010 :: Balance of plant parasitic
    P_plant_balance_tot = design_power * bop_par * bop_par_f * (bop_par_0 + bop_par_1*(P_ratio) + bop_par_2*(P_ratio)**2)
else
    P_plant_balance_tot = 0.d0
endif

if(aux_ratio > 0.d0) then
    !MJW 8.5.2010 :: Auxiliary heater parasitic
    P_aux = design_power * aux_par * aux_par_f * (aux_par_0 + aux_par_1*aux_ratio + aux_par_2*aux_ratio**2)
else
    P_aux = 0.d0
endif

P_cooling_tower_tot=P_cooling_tower   !Report back what is provided by the power block component

P_piping_tot = Piping_loss*Piping_length*Power_cycle_efficiency*Plant_power_output/(design_power*1.e6)  !MWe

P_tank_heater = (P_cold_tank + P_hot_tank) !MWe 

if((recirc_source.eq.2.).and.(night_recirc.eq.1.)) then  !Electric heater
    P_tower_par = (P_tower_conv+P_tower_rad)/recirc_htr_eff
else   !No recirculation needed, or its from the hot tank.. so no action here
    P_tower_par = 0.
endif

P_parasitics = P_storage_pump_tot + P_plant_balance_tot + P_cooling_tower_tot + P_fixed +&
               Tower_pump_power+ P_helio_track+P_piping_tot+P_tank_heater+P_tower_par+P_aux


!-----------------------------------------------------------------------------------------------------------------------
!    SET THE OUTPUTS FROM THIS MODEL IN SEQUENTIAL ORDER AND GET OUT

!P_storage_pump_tot
OUT(1)=P_storage_pump_tot
!P_plant_balance_tot
OUT(2)=P_plant_balance_tot
!P_cooling_tower_tot
OUT(3)=P_cooling_tower_tot
!Piping loss
OUT(4)=P_piping_tot  
!P_parasitics
OUT(5)=P_parasitics
!Power to the grid
OUT(6)=Plant_power_output-P_parasitics
!P_tank_heater
OUT(7)=P_tank_heater              
!P_tower_par
OUT(8)=P_tower_par
OUT(9)=P_fixed
OUT(10)=P_aux
          
!-----------------------------------------------------------------------------------------------------------------------
!EVERYTHING IS DONE - RETURN FROM THIS SUBROUTINE AND MOVE ON
RETURN 1
END
!-----------------------------------------------------------------------------------------------------------------------
