SUBROUTINE TYPE514(TIME,XIN,OUT,T,DTDT,PAR,INFO,ICNTRL,*) 
!************************************************************************
!    TRNSYS acess functions (allow to acess TIME etc.) 
USE TrnsysConstants
USE TrnsysFunctions     

!-----------------------------------------------------------------------------------------------------------------------
!    REQUIRED BY THE MULTI-DLL VERSION OF TRNSYS
!DEC$ATTRIBUTES DLLEXPORT :: TYPE514				!SET THE CORRECT TYPE NUMBER HERE
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
!    TRNSYS DECLARATIONS
IMPLICIT NONE			!REQUIRES THE USER TO DEFINE ALL VARIABLES BEFORE USING THEM

DOUBLE PRECISION XIN	!THE ARRAY FROM WHICH THE INPUTS TO THIS TYPE WILL BE RETRIEVED
DOUBLE PRECISION OUT	!THE ARRAY WHICH WILL BE USED TO STORE THE OUTPUTS FROM THIS TYPE
DOUBLE PRECISION TIME	!THE CURRENT SIMULATION TIME - YOU MAY USE THIS VARIABLE BUT DO NOT SET IT!
DOUBLE PRECISION PAR	!THE ARRAY FROM WHICH THE PARAMETERS FOR THIS TYPE WILL BE RETRIEVED
DOUBLE PRECISION STORED !THE STORAGE ARRAY FOR HOLDING VARIABLES FROM TIMESTEP TO TIMESTEP
DOUBLE PRECISION T		!AN ARRAY CONTAINING THE RESULTS FROM THE DIFFERENTIAL EQUATION SOLVER
DOUBLE PRECISION DTDT	!AN ARRAY CONTAINING THE DERIVATIVES TO BE PASSED TO THE DIFF.EQ. SOLVER
INTEGER*4 INFO(15)		!THE INFO ARRAY STORES AND PASSES VALUABLE INFORMATION TO AND FROM THIS TYPE
INTEGER*4 NP,NI,NOUT,ND	!VARIABLES FOR THE MAXIMUM NUMBER OF PARAMETERS,INPUTS,OUTPUTS AND DERIVATIVES
INTEGER*4 NPAR,NIN,NDER	!VARIABLES FOR THE CORRECT NUMBER OF PARAMETERS,INPUTS,OUTPUTS AND DERIVATIVES
INTEGER*4 IUNIT,ITYPE	!THE UNIT NUMBER AND TYPE NUMBER FOR THIS COMPONENT
INTEGER*4 ICNTRL		!AN ARRAY FOR HOLDING VALUES OF CONTROL FUNCTIONS WITH THE NEW SOLVER
INTEGER*4 NSTORED		!THE NUMBER OF VARIABLES THAT WILL BE PASSED INTO AND OUT OF STORAGE
CHARACTER*3 OCHECK		!AN ARRAY TO BE FILLED WITH THE CORRECT VARIABLE TYPES FOR THE OUTPUTS
CHARACTER*3 YCHECK		!AN ARRAY TO BE FILLED WITH THE CORRECT VARIABLE TYPES FOR THE INPUTS
! ----------------------------------------------------------------------------------------------------------------------

double precision:: m_dot_base, m_dot_cold, m_dot_hot, T_field_out_des, T_field_in_des, T_amb, T_hot, T_cold,&
                   PAR_TC(20), XIN_TC(8), OUT_TC(11)

! ----------------------------------------------------------------------------------------------------------------------
!    USER DECLARATIONS - SET THE MAXIMUM NUMBER OF PARAMETERS (NP), INPUTS (NI),
!    OUTPUTS (NOUT), AND DERIVATIVES (ND) THAT MAY BE SUPPLIED FOR THIS TYPE
PARAMETER (NP=18,NI=0,NOUT=5,ND=0,NSTORED=0)
! ----------------------------------------------------------------------------------------------------------------------


! ----------------------------------------------------------------------------------------------------------------------
!    REQUIRED TRNSYS DIMENSIONS
DIMENSION XIN(NI),OUT(NOUT),PAR(NP),YCHECK(NI),OCHECK(NOUT),STORED(NSTORED),T(ND),DTDT(ND)
INTEGER LU_SCH
LOGICAL IS_THERE,IS_OPEN

! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!    ADD DECLARATIONS AND DEFINITIONS FOR THE USER-VARIABLES HERE

! ----------------------------------------------------------------------------------------------------------------------
!    SET THE VERSION INFORMATION FOR TRNSYS
IF(INFO(7).EQ.-2) THEN
    INFO(12)=16
    RETURN 1
ENDIF
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!    DO ALL THE VERY LAST CALL OF THE SIMULATION MANIPULATIONS HERE
IF (INFO(8).EQ.-1) THEN

    CALL PackedBed(TIME,XIN_TC,OUT_TC,PAR_TC,INFO)

    RETURN 1
ENDIF
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!    PERFORM ANY 'AFTER-ITERATION' MANIPULATIONS THAT ARE REQUIRED HERE
!    e.g. save variables to storage array for the next timestep
IF (INFO(13).GT.0) THEN

    CALL PackedBed(TIME,XIN_TC,OUT_TC,PAR_TC,INFO)

    RETURN 1
ENDIF

! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!    DO ALL THE VERY FIRST CALL OF THE SIMULATION MANIPULATIONS HERE
IF (INFO(7).EQ.-1) THEN

    !SET SOME INFO ARRAY VARIABLES TO TELL THE TRNSYS ENGINE HOW THIS TYPE IS TO WORK
    INFO(6) = NOUT				
    INFO(9) = 1				
    INFO(10)= 0	!STORAGE FOR VERSION 16 HAS BEEN CHANGED				

    !SET THE REQUIRED NUMBER OF INPUTS, PARAMETERS AND DERIVATIVES THAT THE USER SHOULD SUPPLY IN THE INPUT FILE
    !       IN SOME CASES, THE NUMBER OF VARIABLES MAY DEPEND ON THE VALUE OF PARAMETERS TO THIS MODEL....
    NIN     = NI
    NPAR    = NP
    NDER    = ND
    
    call readFluidPropFile(95)

!    !       CALL THE TYPE CHECK SUBROUTINE TO COMPARE WHAT THIS COMPONENT REQUIRES TO WHAT IS SUPPLIED IN 
!    !       THE TRNSYS INPUT FILE
!    CALL TYPECK(1,INFO,NIN,NPAR,NDER)
!
!    !       SET THE NUMBER OF STORAGE SPOTS NEEDED FOR THIS COMPONENT
!    CALL setStorageSize(NSTORED,INFO)
    
!    LU_SCH = 35
!    inquire(file="C:\Documents and Settings\tneises\My Documents\NREL\Thermal Storage\TC_schedule.in",opened=is_open,exist=is_there)    
!    if((is_there).and.(.not.is_open)) then
!        open(unit=LU_SCH,file="C:\Documents and Settings\tneises\My Documents\NREL\Thermal Storage\TC_schedule.in")
!    elseif(.not.is_there) then
!        call messages(-1,"TC_schedule input file does not exist in specified location",'FATAL',INFO(1),INFO(2))
!    elseif(is_open) then
!        call messages(-1,"TC_schedule input file is already open",'FATAL',INFO(1),INFO(2))
!    endif

!    PAR_TC(1)   = 17.d0     ![-] HTF fluid number
!    PAR_TC(2)   = 20.d0     ![m] Height of rock bed storage tank
!    PAR_TC(3)   = 377.659d0 ![m^2] Cross-sectional area of tank
!    PAR_TC(3)   = 377.659d0*2.d0    
!    PAR_TC(4)   = 7.d0      ![-] Filler material number
!    PAR_TC(5)   = 0.d0      !1.44d0    ![kJ/hr-m2-K] Loss Coefficient
!    PAR_TC(6)   = 0.d0      !0.36d0    ![kJ/hr-m2-K] Top Surface Loss Coefficient
!    PAR_TC(7)   = 0.d0      !0.36d0    ![kJ/hr-m2-K] Bottom Surface Loss Coefficient
!    PAR_TC(8)   = 0.25d0    ![-] Void Fraction
!    PAR_TC(9)   = 1.5d0     ![-] Bottom thermal mass capacitance factor multiplier
!    PAR_TC(10)  = 525.d0    ![C] Minimum allowable hot side outlet temperature during discharge
!    PAR_TC(10)  = T_field_out_des - 273.15d0 - 20.d0    ![C]
!    PAR_TC(11)  = 310.d0    ![C] Maximum allowable cold side outlet temperature during charge
!    PAR_TC(11)  = T_field_in_des - 273.15d0 + 20.d0    ![C]
!    PAR_TC(12)  = 2000.d0   ![-] NODES!!!     
!    PAR_TC(13)  = 574.d0    ![C] Initial thermocline hot temperature
!    PAR_TC(13)  = T_field_out_des - 273.15d0    ![C]
!    PAR_TC(14)  = 290.d0    ![C] Initial thermocline cold temperature
!    PAR_TC(14)  = T_field_in_des - 273.15d0     ![C]
!    PAR_TC(15)  = 2.d0      ![-] Fraction into tank where thermocline exists (0: entire tank is hot, 1: entire tank is cold)
!    PAR_TC(16)  = 0.5

    T_hot   = PAR(13)       ![C] Initial thermocline hot temperature
    T_cold  = PAR(14)       ![C] Initial thermocline cold temperature
    
    
    
    T_amb   = 20.d0         ![C] Ambient temperature

    PAR_TC(1:18) = PAR    
    PAR_TC(19)  = getSimulationStartTime()    ![hr] Simulation Start Time
    PAR_TC(20)  = getSimulationTimeStep()     ![hr] Simulation Time Step

    CALL PackedBed(TIME,XIN_TC,OUT_TC,PAR_TC,INFO)

    !       RETURN TO THE CALLING PROGRAM
    RETURN 1

ENDIF
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!    DO ALL OF THE INITIAL TIMESTEP MANIPULATIONS HERE - THERE ARE NO ITERATIONS AT THE INTIAL TIME
IF (TIME .LT. (getSimulationStartTime() + getSimulationTimeStep()/2.D0)) THEN

    m_dot_base = 2000000.d0
                       
    CALL PackedBed(TIME,XIN_TC,OUT_TC,PAR_TC,INFO)                

!       RETURN TO THE CALLING PROGRAM
    RETURN 1

ENDIF

!-----------------------------------------------------------------------------------------------------------------------

!    SET THE OUTPUTS FROM THIS MODEL IN SEQUENTIAL ORDER AND GET OUT

    XIN_TC(1)   = T_hot                     ![C] Charging temperature must come from field outlet
    XIN_TC(2)   = m_dot_base                ![kg/hr] Don't need to know charging flow rate because this call is just checking availability
    XIN_TC(3)   = T_cold                    ![C] Discharging temperature must come from power block
    XIN_TC(4)   = 0.d0                      ![kg/hr] Don't need to know discharging flow rate because this call is just check availability
    XIN_TC(5)   = T_amb                     ![C] Ambient temperature 
    XIN_TC(6)   = 0.d0                      ![-] Solve for thermocline
    XIN_TC(7)   = 0.d0                      ![W] Discharge energy rate demanded
    XIN_TC(8)   = 0.d0                      ![W] Charge energy rate demanded

    CALL PackedBed(TIME,XIN_TC,OUT_TC,PAR_TC,INFO)
    
    continue

!-----------------------------------------------------------------------------------------------------------------------
!    EVERYTHING IS DONE - RETURN FROM THIS SUBROUTINE AND MOVE ON
RETURN 1

END
!-----------------------------------------------------------------------------------------------------------------------
