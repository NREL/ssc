      SUBROUTINE TYPE230(TIME,XIN,OUT,T,DTDT,PAR,INFO,ICNTRL,*)

C**********************************************************************************************
C VARIABLE VOLUME TANK - THIS COMPONENT MODELS A FULLY-MIXED STORAGE TANK
C WITH A CONSTANT CROSS-SECTIONAL AREA THAT CONTAINS A VARIABLE QUANTITY
C OF FLUID.
C
C LAST MODIFIED - 05.00.1993 -- JWT
C               - 02.00.2004 -- TPM - CONVERSION TO TRNSYS16
C               - 03.00.2004 -- MKu - added units for outputs 10-13 and removed outputs 14-15 to 
C                               avoid warnings.
C               - 02.00.2005 -- DEB - changed from using outputs 14-15 to get/set storage. Also 
C                               corrected a bug in rereading parameters when there is more than
C                               one Type39 in a given simulation.
C               - 12.13.2005 -- DAA - Changed the order of recalling the Stored variables before
C                               updating the parameters at the beginning of each iteration.
C               - 02.02.2009 -- MJW Created under new type number, allowing HTF properties as
C                               inputs rather than parameters
C               - 05.01.2009 -- MJW added tank pairs calculation and tank heat trace system
C**********************************************************************************************
! Copyright © 2004 Solar Energy Laboratory, University of Wisconsin-Madison. All rights reserved.
! COPYRIGHT 2009 NATIONAL RENEWABLE ENERGY LABORATORY

! Doc. tables updated 7/1/2010 - MJW
!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Parameters
!    1| MODE                             | Tank operation mode                                               | none             | none             
!    2| VOL                              | Overall tank volume                                               | m3               | m3               
!    3| VMIN                             | Minimum fluid volume                                              | m3               | m3               
!    4| VMAX                             | Maximum fluid volume                                              | m3               | m3               
!    5| CIRC                             | Tank circumference                                                | m                | m                
!    6| AEND                             | Cross-sectional area                                              | m2               | m2               
!    7| UW                               | Wetted loss coefficient                                           | kW/m2.K          | kW/m2.K          
!    8| UD                               | Dry loss coefficient                                              | kW/m2.K          | kW/m2.K          
!    9| TZERO                            | Initial fluid temperature                                         | C                | C                
!   10| MZERO                            | Initial fluid volume                                              | m3               | m3               
!   11| tank_pairs                       | Number of equivalent tank pairs                                   | none             | none             
!   12| htr_set_point                    | Heater temperature set point                                      | C                | C                
!   13| max_heat_rate                    | Maximum heat rate of the heater                                   | MWt              | MWt              
!   14| eta_heater                       | Heater efficiency                                                 | none             | none             

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Inputs
!    1| TIN                              | Inlet fluid temperature                                           | C                | C                
!    2| MIN                              | Mass flow rate in                                                 | kg/hr            | kg/hr            
!    3| MOUT                             | Mass flow rate to load                                            | kg/hr            | kg/hr            
!    4| TAMB                             | Ambient temperature                                               | C                | C                
!    5| CP                               | Fluid specific heat                                               | kJ/kg.K          | kJ/kg.K          
!    6| DEN                              | Fluid density                                                     | kg/m3            | kg/m3            

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Outputs
!    1| TAV                              | Fluid outlet temperature                                          | C                | C                
!    2| -not named-                      | Load flow rate (MOUT-MRET)                                        | kg/hr            | kg/hr            
!    3| -not named-                      | Excess flow temperature (TAV or TIN)                              | C                | C                
!    4| MRET                             | Excess flow rate                                                  | kg/hr            | kg/hr            
!    5| VOLAV                            | Fluid volume                                                      | m3               | m3               
!    6| -not named-                      | Enthalpy difference                                               | kJ/hr            | kJ/hr            
!    7| QLOSS                            | Environment losses                                                | MWt              | MWt              
!    8| DE                               | Internal energy change                                            | kJ/hr            | kJ/hr            
!    9| LEVEL                            | Level indicator                                                   | none             | none             
!   10| TFIN                             | Tank temperature at end of timestep                               | C                | C                
!   11| VOLFIN                           | Tank volume at end of timestep                                    | m3               | m3               
!   12| MAV                              | Average mass flow rate over the timestep                          | kg/hr            | kg/hr            
!   13| -not named-                      | Average height of the heat transfer fluid in the tank             | m                | m                
!   14| Q_heater                         | The required thermal energy from the tank heater                  | MWt              | MWt              



C-----------------------------------------------------------------------------------------------------------------------
C    USE STATEMENTS
	USE TrnsysFunctions
C-----------------------------------------------------------------------------------------------------------------------

C-----------------------------------------------------------------------------------------------------------------------
C    REQUIRED BY THE MULTI-DLL VERSION OF TRNSYS
      !DEC$ATTRIBUTES DLLEXPORT :: TYPE230				!SET THE CORRECT TYPE NUMBER HERE
C-----------------------------------------------------------------------------------------------------------------------

C-----------------------------------------------------------------------------------------------------------------------
C    TRNSYS DECLARATIONS
      IMPLICIT NONE						!REQUIRES THE USER TO DEFINE ALL VARIABLES BEFORE USING THEM
	DOUBLE PRECISION XIN				!THE ARRAY FROM WHICH THE INPUTS TO THIS TYPE WILL BE RETRIEVED
	DOUBLE PRECISION OUT				!THE ARRAY WHICH WILL BE USED TO STORE THE OUTPUTS FROM THIS TYPE
	DOUBLE PRECISION TIME				!THE CURRENT SIMULATION TIME - YOU MAY USE THIS VARIABLE BUT DO NOT SET IT!
	DOUBLE PRECISION PAR				!THE ARRAY FROM WHICH THE PARAMETERS FOR THIS TYPE WILL BE RETRIEVED
	DOUBLE PRECISION STORED     		!THE STORAGE ARRAY FOR HOLDING VARIABLES FROM TIMESTEP TO TIMESTEP
	DOUBLE PRECISION T					!AN ARRAY CONTAINING THE RESULTS FROM THE DIFFERENTIAL EQUATION SOLVER
	DOUBLE PRECISION DTDT				!AN ARRAY CONTAINING THE DERIVATIVES TO BE PASSED TO THE DIFF.EQ. SOLVER
      INTEGER*4 INFO(15)					!THE INFO ARRAY STORES AND PASSES VALUABLE INFORMATION TO AND FROM THIS TYPE
	INTEGER*4 NP,NI,NO,ND				!VARIABLES FOR THE MAXIMUM NUMBER OF PARAMETERS,INPUTS,OUTPUTS AND DERIVATIVES
	INTEGER*4 NPAR,NIN,NDER				!VARIABLES FOR THE CORRECT NUMBER OF PARAMETERS,INPUTS,OUTPUTS AND DERIVATIVES
	INTEGER*4 IUNIT,ITYPE				!THE UNIT NUMBER AND TYPE NUMBER FOR THIS COMPONENT
	INTEGER*4 ICNTRL					!AN ARRAY FOR HOLDING VALUES OF CONTROL FUNCTIONS WITH THE NEW SOLVER
	INTEGER*4 NS    					!THE NUMBER OF VARIABLES THAT WILL BE PASSED INTO AND OUT OF STORAGE
      CHARACTER*3 OCHECK					!AN ARRAY TO BE FILLED WITH THE CORRECT VARIABLE TYPES FOR THE OUTPUTS
	CHARACTER*3 YCHECK					!AN ARRAY TO BE FILLED WITH THE CORRECT VARIABLE TYPES FOR THE INPUTS
C-----------------------------------------------------------------------------------------------------------------------

C-----------------------------------------------------------------------------------------------------------------------
C    USER DECLARATIONS - SET THE MAXIMUM NUMBER OF PARAMETERS (NP), INPUTS (NI),
C    OUTPUTS (NO), DERIVATIVES (ND), AND STORED VARIABLES (NS) THAT MAY BE SUPPLIED FOR THIS TYPE
      PARAMETER (NP=14,NI=6,NO=14,ND=0,NS=2)
C-----------------------------------------------------------------------------------------------------------------------

C-----------------------------------------------------------------------------------------------------------------------
C    REQUIRED TRNSYS DIMENSIONS
      DIMENSION XIN(NI),OUT(NO),PAR(NP),YCHECK(NI),OCHECK(NO),
	1   STORED(NS),T(ND),DTDT(ND)
C-----------------------------------------------------------------------------------------------------------------------

C-----------------------------------------------------------------------------------------------------------------------
C    DECLARATIONS AND DEFINITIONS FOR THE USER-VARIABLES
      DOUBLE PRECISION MIN,MOUT,LHTAV,MFIN,MAV,MFST,MZERO,MRET,TUNE,VOL,
     & VMIN,VMAX,CIRC,AEND,HEIGHT,UW,UD,CP,DEN,TZERO,TFST,VFST,TAV,VOLAV
     & ,TFIN,VOLFIN,TIN,TAMB,VFTEST,UAW,UAD,UA,CHECK,XXTEST,B,C,D,CC,DD,
     & AA,BB,G,H,A1,E,DE,HIN,HOUT,QLOSS,TIME0,TFINAL,DELT, tank_pairs,
     & htr_set_point, max_heat_rate, Q_heater, eta_heater, Q_flow, Q_vol
     & ,pi
	INTEGER MODE,LEVEL
      DATA IUNIT/0/
      DATA TUNE/10000.0/
      DATA PI/3.1415926/
C-----------------------------------------------------------------------------------------------------------------------
C    TRNSYS FUNCTIONS
      TIME0=getSimulationStartTime()
      TFINAL=getSimulationStopTime()
      DELT=getSimulationTimeStep()
C-----------------------------------------------------------------------------------------------------------------------
C    SET THE VERSION INFORMATION FOR TRNSYS
      IF(INFO(7).EQ.-2) THEN
	   INFO(12)=16
	   RETURN 1
	ENDIF
C-----------------------------------------------------------------------------------------------------------------------

C-----------------------------------------------------------------------------------------------------------------------
C    DO ALL THE VERY LAST CALL OF THE SIMULATION MANIPULATIONS HERE
      IF (INFO(8).EQ.-1) THEN
	   RETURN 1
	ENDIF
C-----------------------------------------------------------------------------------------------------------------------

C-----------------------------------------------------------------------------------------------------------------------
C    PERFORM ANY "AFTER-ITERATION" MANIPULATIONS THAT ARE REQUIRED
      IF(INFO(13).GT.0) THEN
         !SET T INITIAL AND M INITIAL FOR THE NEXT TIME STEP
         STORED(1) = OUT(10)
         STORED(2) = OUT(11)
	   CALL setStorageVars(STORED,NS,INFO)
	   RETURN 1
	END IF
C-----------------------------------------------------------------------------------------------------------------------

C-----------------------------------------------------------------------------------------------------------------------
C    DO ALL THE VERY FIRST CALL OF THE SIMULATION MANIPULATIONS HERE
      IF (INFO(7).EQ.-1) THEN

C       RETRIEVE THE UNIT NUMBER AND TYPE NUMBER FOR THIS COMPONENT FROM THE INFO ARRAY
         IUNIT=INFO(1)
	   ITYPE=INFO(2)

C       SET SOME INFO ARRAY VARIABLES TO TELL THE TRNSYS ENGINE HOW THIS TYPE IS TO WORK
         INFO(6)=NO				
         INFO(9)=1				
	   INFO(10)=0	!STORAGE FOR VERSION 16 HAS BEEN CHANGED

C       RESERVE SPACE IN STORAGE STRUCTURE
	   CALL setStorageSize(NS,INFO)				

C       CALL THE TYPE CHECK SUBROUTINE TO COMPARE WHAT THIS COMPONENT REQUIRES TO WHAT IS SUPPLIED IN 
C       THE TRNSYS INPUT FILE
	   CALL TYPECK(1,INFO,NI,NP,ND)

C       SET THE YCHECK AND OCHECK ARRAYS TO CONTAIN THE CORRECT VARIABLE TYPES FOR THE INPUTS AND OUTPUTS
C         DATA YCHECK/'TE1','MF1','MF1','TE1','CP1','DN1'/				
C         DATA OCHECK/'TE1','MF1','TE1','MF1','VL1','EN1','PW1','EN1',
C     &               'DM1','TE1','VL1','MA1','DM1'/				

C       CALL THE RCHECK SUBROUTINE TO SET THE CORRECT INPUT AND OUTPUT TYPES FOR THIS COMPONENT
C         CALL RCHECK(INFO,YCHECK,OCHECK)

C       RETURN TO THE CALLING PROGRAM
         RETURN 1

      ENDIF
C-----------------------------------------------------------------------------------------------------------------------

C-----------------------------------------------------------------------------------------------------------------------
C    DO ALL OF THE INITIAL TIMESTEP MANIPULATIONS HERE - THERE ARE NO ITERATIONS AT THE INTIAL TIME
      IF (TIME.LT.(TIME0+DELT/2.D0)) THEN

C       SET THE UNIT NUMBER FOR FUTURE CALLS
         IUNIT=INFO(1)
         ITYPE=INFO(2)

C       READ IN THE VALUES OF THE PARAMETERS IN SEQUENTIAL ORDER
         MODE=JFIX(PAR(1)+0.1)
         VOL = DMAX1(PAR(2),.001)  !MJW - Make the minimum volume non-zero.  This prevents NAN calculations when storage is zeroed out
         VMIN=DMAX1(PAR(3),.001*VOL)
         VMAX=DMIN1(PAR(4),.999*VOL)
         CIRC=DMAX1(PAR(5),.001)  !MJW
         AEND=DMAX1(PAR(6),.001)  !MJW
         HEIGHT=VOL/AEND
         UW = PAR(7)/3.6  !W/m^2-K --> kJ/hr-m^2-K
         UD = PAR(8)/3.6  !W/m^2-K --> kJ/hr-m^2-K
         TZERO = PAR(9)
         MZERO = PAR(10)
         tank_pairs = PAR(11)  !Apply losses for this tank based on this value
         htr_set_point = PAR(12) !The minimum fluid temperature at which the tank heater begins operation
         max_heat_rate = PAR(13) ![MWe]  The maximum rate at which the tank heater can operate
         eta_heater = PAR(14) !The efficiency in conversion from electric power to thermal power
         !Check to see if the tank has volume. if not, set values and return
         if(vol.le..01) goto 10

         if(tank_pairs.gt.1.)then
            VMIN=VMIN*tank_pairs
            VMAX=VMAX*tank_pairs
            AEND=(((4*VOL*CIRC/PI/(PI*HEIGHT))**(1./2.))**(2./3.))
     &           **2/4.*PI
            CIRC=PI*((4*VOL*CIRC/PI/(PI*HEIGHT))**(1./2.))**(2./3.)
            HEIGHT=VOL/AEND
         ENDIF

C       PERFORM ANY REQUIRED CALCULATIONS TO SET THE INITIAL VALUES OF THE OUTPUTS HERE
         STORED(1) = PAR(9) !OUT(14)=PAR(11)
	   STORED(2) = PAR(10) !OUT(15)=PAR(12)
	   CALL setStorageVars(STORED,NS,INFO)
	   
         TFST = STORED(1) !OUT(14)
         VFST = STORED(2) !OUT(15)
         MFST=VFST*DEN
         TAV=TFST
         VOLAV=VFST
         TFIN=TFST
         VOLFIN=VFST
         MFIN=VOLFIN*DEN

C    READ INPUTS
         TIN = XIN(1)
         MIN = XIN(2)
         MOUT = XIN(3)
         TAMB = XIN(4)
         CP = XIN(5)
         DEN = XIN(6)
         MZERO=MZERO*DEN
         MRET=0.
         LEVEL=0
         DE = CP*(MFIN*TFIN-MZERO*TZERO)
         HIN=CP*MIN*TIN
         HOUT=CP*MOUT*TAV
         QLOSS = UA*(TAV - TAMB)

C    SET OUTPUTS
10      continue
         OUT(1) = TAV
         OUT(2) = MOUT-MRET
         OUT(3) = TAV
         IF(MODE.EQ.2) OUT(3)=TIN
         OUT(4) = MRET
         OUT(5) = VOLAV
         OUT(6) = HIN-HOUT
         OUT(7) = QLOSS/3.6e6 !kJ/hr to MWt
         OUT(8) = DE
         OUT(9)  = LEVEL
         OUT(10) = TFIN
         OUT(11) = VOLFIN
         OUT(12) = MAV
         OUT(13) = LHTAV/HEIGHT
         OUT(14) = 0.
C       RETURN TO THE CALLING PROGRAM
         RETURN 1

      ENDIF
C-----------------------------------------------------------------------------------------------------------------------

C-----------------------------------------------------------------------------------------------------------------------
C    *** ITS AN ITERATIVE CALL TO THIS COMPONENT ***
C-----------------------------------------------------------------------------------------------------------------------

C-----------------------------------------------------------------------------------------------------------------------
C    RETRIEVE STORED VALUES
      CALL getStorageVars(STORED,NS,INFO)
      TFST = STORED(1) !TFST = OUT(14)
      VFST = STORED(2) !VFST = OUT(15)
      MFST=VFST*DEN

C-----------------------------------------------------------------------------------------------------------------------
C    RE-READ THE PARAMETERS IF ANOTHER UNIT OF THIS TYPE HAS BEEN CALLED SINCE THE LAST TIME THE PARAMETERS
C    WERE READ IN
      IF(INFO(1).NE.IUNIT) THEN

C       RESET THE UNIT NUMBER
	   IUNIT=INFO(1)
	   ITYPE=INFO(2)
	    
C       READ IN THE VALUES OF THE PARAMETERS IN SEQUENTIAL ORDER
         MODE=JFIX(PAR(1)+0.1)
         VOL = DMAX1(PAR(2),.001)  !MJW - Make the minimum volume non-zero.  This prevents NAN calculations when storage is zeroed out
         VMIN=DMAX1(PAR(3),.001*VOL)
         VMAX=DMIN1(PAR(4),.999*VOL)
         CIRC=DMAX1(PAR(5),.001)  !MJW
         AEND=DMAX1(PAR(6),.001)  !MJW
         HEIGHT=VOL/AEND
         UW = PAR(7)/3.6  !W/m^2-K --> kJ/hr-m^2-K
         UD = PAR(8)/3.6  !W/m^2-K --> kJ/hr-m^2-K
         TZERO = PAR(9)
         MZERO = PAR(10)*DEN
         tank_pairs = PAR(11)
         htr_set_point = PAR(12) !The minimum fluid temperature at which the tank heater begins operation
         max_heat_rate = PAR(13) ![MWe]  The maximum rate at which the tank heater can operate
         eta_heater = PAR(14) !The efficiency in conversion from electric power to thermal power
         
         !Modify inputs based on the number of tank pairs
         if(tank_pairs.gt.1.)then
            VMIN=VMIN*tank_pairs
            VMAX=VMAX*tank_pairs
            AEND=(((4*VOL*CIRC/PI/(PI*HEIGHT))**(1./2.))**(2./3.))
     &           **2/4.*PI
            CIRC=PI*((4*VOL*CIRC/PI/(PI*HEIGHT))**(1./2.))**(2./3.)
            HEIGHT=VOL/AEND
         ENDIF

         !TFST = OUT(14)
         !VFST = OUT(15)
         MFST=VFST*DEN
         TAV=TFST
         VOLAV=VFST
         TFIN=TFST
         VOLFIN=VFST
         MFIN=VOLFIN*DEN
      ENDIF

C-----------------------------------------------------------------------------------------------------------------------
      IF(INFO(7) .EQ. 0 ) THEN
C    SET NEW T AND M BASED ON CONVERGED VALUES FROM LAST TIME STEP.
         STORED(1) = OUT(10)
	   STORED(2) = OUT(11)
	   CALL setStorageVars(STORED,NS,INFO)
	   !OUT(14)=OUT(10)
         !OUT(15)=OUT(11)
	END IF

C-----------------------------------------------------------------------------------------------------------------------
      !Check to see if the tank has volume. if not, set values and return  MJW 5/26/09
      if(vol.le..01) goto 10


C    RETRIEVE THE CURRENT VALUES OF THE INPUTS TO THIS MODEL FROM THE XIN ARRAY IN SEQUENTIAL ORDER
      TIN = XIN(1)
      MIN = XIN(2)
      MOUT = XIN(3)
      TAMB = XIN(4)
      MRET = 0.
      LEVEL = 0

C-----------------------------------------------------------------------------------------------------------------------
C    PERFORM ALL THE CALCULATIONS HERE FOR THIS MODEL.
C-----------------------------------------------------------------------------------------------------------------------
C    CHECK FOR MAXIMUM OR MINIMUM LIQUID VOLUME
      VFTEST=VFST+DELT*(MIN-MOUT)/DEN
      IF((VFTEST-VMAX) .LT. -1.E-6) GO TO 19
      MRET=MIN-MOUT-DMAX1((VMAX*DEN-MFST)/DELT,0.)
      IF (MRET .LT. 1.E-6) MRET=0.
      LEVEL = 1
      GO TO (17,18) ,MODE

17    MOUT=MOUT+MRET
      GO TO 20

18    MIN=MIN-MRET
      GO TO 20
   
   19 IF((VFTEST-VMIN) .GT. 1.E-6) GO TO 20
      MOUT=DMAX1((MFST-VMIN*DEN)/DELT,0.)+MIN
      LEVEL = -1
   
   20 CONTINUE

C    CALCULATE FINAL AND AVERAGE FOR TANK FLUID MASS AND VOLUME
      MFIN = MFST +DELT*(MIN-MOUT)
      MAV = (MFST+MFIN)/2.0
      VOLFIN = MFIN/DEN
      VOLAV = MAV/DEN
      LHTAV = VOLAV/AEND

C    CALCULATE  AVERAGE TANK UA BASED ON DRY AREA AND WETTED AREA
      UAW = UW * (AEND+CIRC * LHTAV)
      UAD = UD * (AEND + CIRC * (HEIGHT - LHTAV))
      UA = UAW + UAD

C    CHECK FOR MIN = MOUT
      CHECK = DABS(MIN-MOUT)
      XXTEST = (MIN + UA/CP)/TUNE
      IF(CHECK .LT. XXTEST) GO TO 75

C    CHECK FOR NO FLOW
      IF(MIN .LT. 0.001 .AND. MOUT .LT. 0.001) GO TO 75

C    EQUATIONS FOR FLOW CONDITION
      CONTINUE
      B = MIN + UA/CP
      C = MIN - MOUT
      D = MIN * TIN + (UA/CP) * TAMB
      CC = TFST-D/B
      DD = (1+(C*DELT)/MFST)**(-B/C)
      TFIN = CC*DD+D/B
      AA = (TFST-D/B)/(C-B)
      BB = (1+(C*DELT)/MFST)**(1-B/C)
      TAV = AA*(MFST/DELT)*(BB-1.0) + D/B
      GO TO 150
   
   75 CONTINUE

C    EQUATIONS FOR MIN = MOUT CONDITION
      B = MIN + UA/CP
      D = MIN *TIN + (UA/CP) * TAMB
      G = -B/MFST
      H = 1.0/(DELT*(-B))
      A1 = D - B*TFST
      E = A1 * DEXP(DELT*G)
      TFIN = (E-D)/(-B)
      TAV =H*((E-A1)/G)+D/B
  150 CONTINUE
      !If the temperature of the fluid in the tank is less than the 
      !..aux heater set point, heat the fluid in - and passing through - 
      !..the tank. MJW
      if(TFIN.lt.htr_set_point) then
        Q_vol=CP*VOLFIN*DEN/(DELT*3600.)*(htr_set_point-TFIN)/1000.  !MW
        Q_flow=CP*MOUT/3600.*DELT/(DELT*3600.)*(htr_set_point-TFIN)/1000.  !MW
        Q_heater = dmin1(Q_flow+Q_vol,max_heat_rate) !MW
        TFIN=TFST+DELT*dmin1(Q_vol,max_heat_rate)/(CP*DEN*VOLFIN)
        TAV=(TFIN+TFST)/2.
      else
        Q_heater = 0.
      endif
      
      DE = CP*(MFIN*TFIN-MZERO*TZERO)
      HIN=CP*MIN*TIN
      HOUT=CP*MOUT*TAV
      QLOSS = UA*(TAV - TAMB)
      !Adjust the thermal losses based on the number or tank pairs.  MJW
      QLOSS = QLOSS*(2.*sqrt(3.141592/AEND)*tank_pairs**(1./3.)*VOL+
     &               AEND*tank_pairs**(-2./3.))/(CIRC*HEIGHT+AEND)  

C-----------------------------------------------------------------------------------------------------------------------
C    SET THE OUTPUTS FROM THIS MODEL IN SEQUENTIAL ORDER AND GET OUT
      OUT(1) = TAV  !Fluid temperature
      OUT(2) = MOUT-MRET  !Load flow rate
      OUT(3) = TAV  !Excess flow temperature
      IF(MODE.EQ.2) OUT(3) = TIN  !
      OUT(4) = MRET  !Excess flow rate
      OUT(5) = VOLAV  !Fluid volume
      OUT(6) = HIN-HOUT  !Enthalpy difference
      OUT(7) = QLOSS/3.6e6 !Environment losses || Convert from kJ/hr to MWt
      OUT(8) = DE  !Internal energy change
      OUT(9)  = LEVEL !Level indicator
      OUT(10) = TFIN
      OUT(11) = VOLFIN
      OUT(12) = MAV
      OUT(13) = LHTAV/HEIGHT
      OUT(14) = Q_heater  ![MW] The required thermal energy from the tank heater

C-----------------------------------------------------------------------------------------------------------------------
C    EVERYTHING IS DONE - RETURN FROM THIS SUBROUTINE AND MOVE ON
      RETURN 1
      END
