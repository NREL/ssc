      SUBROUTINE TYPE231(TIME,XIN,OUT,T,DTDT,PAR,INFO,ICNTRL,*)

C************************************************************************************************
C  THIS SUBROUTINE MODELS AN AUXILIARY HEATER. WHENEVER THE INPUT CONTROL FUNCTION 
C   IGAM IS 1, HEAT IS ADDED AT A RATE LESS THAN OR EQUAL TO QMAX IN ORDER TO 
C   BRING THE FLOWSTREAM TEMPERATURE TO TSET.  
C           
C  LAST MODIFIED 
C   2/28/93  -- JWT - NEW FEATURES FOR 14.0
C   12/14/98 -- DEB - MOVED SETPOINT TEMP FROM PAR 2 TO INPUT 4 FOR TRNSYS 15.0
C   7/19/02  -- DEB - PREPARED FOR TRNSYS 16.0
C   5/29/06  -- DAA/JWT - CHANGED INFO(9) TO 1, BECAUSE AT SMALL TIME STEPS A SMALL CHANGE IN 
C                         THE INPUTS (SMALLER THAN TOLERANCE) RESULTS IN THE COMPONENT NOT 
C                         BEING CALLED
C   2/5/09   -- MJW - Ported from Type6 to Type231, modified to fit tower model      
C************************************************************************************************
! Copyright © 2004 Solar Energy Laboratory, University of Wisconsin-Madison. All rights reserved.
 ! COPYRIGHT 2009 NATIONAL RENEWABLE ENERGY LABORATORY

! Doc. tables updated 7/1/2010 - MJW
!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Parameters
!    1| QMAX                             | Maximum heating rate                                              | kJ/hr            | kJ/hr            
!    2| UA                               | Overall loss coefficient for heater during operation              | kJ/hr            | kJ/hr            
!    3| HTREFF                           | Efficiency of auxiliary heater                                    | none             | none             

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Inputs
!    1| TIN                              | Inlet fluid temperature                                           | C                | C                
!    2| FLOW                             | Fluid mass flow rate                                              | kg/hr            | kg/hr            
!    3| IGAM                             | Control Function                                                  | none             | none             
!    4| TSET                             | Set point temperature                                             | C                | C                
!    5| TAMB                             | Temperature of surroundings                                       | C                | C                
!    6| CP                               | HTF specific heat                                                 | kJ/kg.K          | kJ/kg.K          

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Outputs
!    1| TOUT                             | Outlet fluid temperature                                          | C                | C                
!    2| FLOW                             | Outlet fluid flow rate                                            | kg/hr            | kg/hr            
!    3| QAUX                             | Required heating rate                                             | MWt              | kJ/hr            
!    4| QLOSS                            | Losses from the auxiliary heater                                  | MWt              | kJ/hr            
!    5| QFLUID                           | Rate of energy delivery to fluid stream                           | MWt              | kJ/hr            
!    6| -not named-                      | Average fluid temperature                                         | C                | C                



!export this subroutine for its use in external DLLs.
!DEC$ATTRIBUTES DLLEXPORT :: TYPE231

C    USE STATEMENTS
	USE TrnsysFunctions

      IMPLICIT NONE

C    TRNSYS DECLARATIONS
	DOUBLE PRECISION XIN,OUT,TIME,PAR,T,DTDT,TIME0,TFINAL,DELT
      INTEGER*4 INFO(15),NP,NI,NOUT,ND,IUNIT,ITYPE,ICNTRL
      CHARACTER*3 OCHECK,YCHECK

C    SET THE MAXIMUM NUMBER OF PARAMETERS(NP),INPUTS(NI),OUTPUTS(NOUT),AND DERIVATIVES(ND)
C    THAT MAY BE SUPPLIED FOR THIS TYPE
      PARAMETER (NP=4,NI=6,NOUT=6,ND=0)

C    REQUIRED TRNSYS DIMENSIONS
      DIMENSION XIN(NI),OUT(NOUT),PAR(NP),YCHECK(NI),OCHECK(NOUT)

C    LOCAL VARIABLE DECLARATIONS
      INTEGER IGAM
	DOUBLE PRECISION TIN, !temperature of fluid at heater inlet [C]
     & TOUT,   !temperature of fluid at heater outlet [C]
     & TBAR,   !average temperature of fluid in heater [C]
     & TAMB,   !ambient temperature of heater surroundings [C]
     & TSET,   !heater setpoint temperature [C]
     & TON,    !set temporarily to outlet temperature before check on TOUT>TSET [C]
     & QMAX,   !heater capacity [kJ/hr]
     & QAUX,   !required heating rate [kJ/hr]
     & QLOSS,  !rate of thermal losses to surroundings [kJ/hr]
     & FLOW,   !fluid flow rate through heater [kg/hr]
     & CP,     !fluid specific heat [kJ/kg.K] 
     & HTREFF, !heater efficiency [-]
     & UA,     !overall loss coefficienct for heater during operation [kJ/hr.K] 
     & QFLUID, !rate of energy delivered to fluid [kJ/hr]
     & LHV_EFF !LHV fuel efficiency [-] 

C    TRNSYS FUNCTIONS
      TIME0=getSimulationStartTime()
      TFINAL=getSimulationStopTime()
      DELT=getSimulationTimeStep()

C    SET THE VERSION INFORMATION FOR TRNSYS
      IF(INFO(7).EQ.-2) THEN
	   INFO(12)=16
	   RETURN 1
	ENDIF

C    PERFORM LAST CALL MANIPULATIONS 
      IF(INFO(8).EQ.-1) RETURN 1 !none are required for this TYPE
 
C    PERFORM POST CONVERGENCE MANIPULATIONS
      IF(INFO(13).GT.0) THEN
	   RETURN 1 !none are required for this TYPE
	ENDIF

C    PERFORM FIRST CALL MANIPULATIONS
      IF(INFO(7).EQ.-1) THEN
        !retrieve unit and type number for this component from the INFO array
        IUNIT=INFO(1)
	  ITYPE=INFO(2)
        !set some info array variables to tell the trnsys engine how this type is to work
        INFO(6)=NOUT !reserve space in the OUT array using INFO(6)
        INFO(9)=1    !this TYPE should be called until convergence is reached
	  INFO(10)=0   !no storage spots are required
        !CALL THE TYPE CHECK SUBROUTINE TO COMPARE WHAT THIS TYPE REQUIRES TO WHAT IS SUPPLIED IN 
        !THE TRNSYS INPUT FILE
	  CALL TYPECK(1,INFO,NI,NP,ND)
        !SET THE YCHECK AND OCHECK ARRAYS TO CONTAIN THE CORRECT VARIABLE TYPES FOR THE INPUTS AND OUTPUTS
        DATA YCHECK/'TE1','MF1','CF1','TE1','TE1','CP1'/
        DATA OCHECK/'TE1','MF1','PW1','PW1','PW1','TE1'/
        !CALL THE INPUT-OUTPUT CHECK SUBROUTINE TO SET THE CORRECT INPUT AND OUTPUT UNITS
        CALL RCHECK(INFO,YCHECK,OCHECK)
        !RETURN TO THE CALLING PROGRAM
        RETURN 1
      ENDIF

C    PERFORM INITIAL TIMESTEP MANIPULATIONS
      IF(TIME.LT.(TIME0+DELT/2.)) THEN
        !set the UNIT number for future calls
        IUNIT=INFO(1)
        !read parameter values
        QMAX = PAR(1)
        UA = PAR(2)
        HTREFF = PAR(3)
        LHV_EFF = PAR(4)

        !check the parameters for problems and RETURN if any are found
         IF(QMAX.LT.0.) THEN !make sure the heater capacity is positive
	      CALL TYPECK(-4,INFO,0,1,0)
	      RETURN 1
	   ENDIF
         IF(CP.LT.0.) THEN !make sure the fluid specific heat is positive
	      CALL TYPECK(-4,INFO,0,2,0)
	      RETURN 1
	   ENDIF
         IF(UA.LT.0.) THEN !make sure the heater UA is positive
	      CALL TYPECK(-4,INFO,0,3,0)
	      RETURN 1
	   ENDIF
	   IF((HTREFF.GT.1.).OR.(HTREFF.LT.0.)) THEN !make sure heater efficiency is between 0 and 1
	      CALL TYPECK(-4,INFO,0,4,0)	
	      RETURN 1
	   ENDIF
         !perform any required calculations, set the outputs to the input initial values.
         OUT(1) = XIN(1) !outlet temperature = inlet temperature [C]
         OUT(2) = XIN(2) !mass flowrate out = mass flowrate in [kg/hr]
         OUT(3) = 0.     !required heating rate [kJ/hr]
         OUT(4) = 0.     !rate of losses to environment [kJ/hr]
         OUT(5) = 0.     !rate of energy delivered to stream [kJ/hr]
         OUT(6) = 0.     !The average temperature [C]
         RETURN 1        !the first timestep is for initialization - exit.
      ENDIF

C--------------------------------------------------------------------
C    THIS IS AN ITERATIVE CALL TO THIS COMPONENT ***
C--------------------------------------------------------------------

C    RE-READ THE PARAMETERS IF ANOTHER UNIT OF THIS TYPE HAS BEEN CALLED SINCE THE LAST 
C    TIME THEY WERE READ IN
      IF(INFO(1).NE.IUNIT) THEN
        !reset the unit number
	  IUNIT  = INFO(1)
	  ITYPE  = INFO(2)
	  !read the parameter values
        QMAX   = PAR(1)
        UA     = PAR(2)
        HTREFF = PAR(3)	
        LHV_EFF = PAR(4)					 
      ENDIF

C    RETRIEVE THE CURRENT VALUES OF THE INPUTS TO THIS MODEL FROM THE XIN ARRAY
      TIN  = XIN(1)
      FLOW = XIN(2)
      IGAM = JFIX(XIN(3)+0.1)
      TSET = XIN(4)
      TAMB = XIN(5)
      CP   = XIN(6)

C    PERFORM CALCULATIONS 
      IF (FLOW .GT. 0.) GO TO 10

C     NO FLOW
      OUT(1) = TIN
      OUT(2) = 0.
      OUT(3) = 0.
      OUT(4) = 0.
      OUT(5) = 0.
      OUT(6) = TIN
      RETURN 1

C     CHECK INLET TEMPERATURE AND CONTROL FUNCTION
10    IF (TIN .LT. TSET .AND. IGAM .EQ. 1) GO TO 20

C     HEATER OFF
      TOUT = TIN
      QAUX = 0.
      QLOSS = 0.
      QFLUID = 0.   
      GO TO 50

C    HEATER ON
20    TON=(QMAX*HTREFF+FLOW*CP*TIN+UA*TAMB-UA*TIN/2.d0)/(FLOW*CP+UA
     & /2.d0)
      TOUT = MIN(TSET,TON)
      TBAR = (TIN+TOUT)/2.d0
      QAUX = (FLOW*CP*(TOUT-TIN)+UA*(TBAR-TAMB))/HTREFF
      QLOSS = UA*(TBAR-TAMB) + (1.d0-HTREFF)*QAUX
      QFLUID = FLOW*CP*(TOUT-TIN)

C    SET OUTPUTS
50    OUT(1) = TOUT  
      OUT(2) = FLOW
      OUT(3) = QAUX/3.6e6 !Convert kJ/hr to MW
      OUT(4) = QLOSS/3.6e6 !Convert kJ/hr to MW
      OUT(5) = QFLUID/3.6e6/LHV_EFF !Convert kJ/hr to MW
      OUT(6) = (TIN+TOUT)/2.

	RETURN 1

      END
