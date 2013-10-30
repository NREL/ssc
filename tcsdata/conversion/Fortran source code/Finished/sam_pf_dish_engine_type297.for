			SUBROUTINE TYPE297 (TIME,XIN,OUT,T,DTDT,PAR,INFO,ICNTRL,*) 
C************************************************************************
C  COPYRIGHT 2008 NATIONAL RENEWABLE ENERGY LABORATORY
C Object: 6_21_07_Stirling_Engine
C Simulation Studio Model: 10_18_07_Stirling_Engine
C 
C Author: 
C Editor: 
C Date:	 June 03, 2007 last modified: June 03, 2007
C 
C 
! Doc. tables updated 2010-08-05 - MJW
!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Parameters
!    1| manufacturer                     | Manufacturer (fixed as 5="other")                                 | none             | none             
!    2| T_heater_head_high               | Heater Head Set Temperature                                       | K                | K                
!    3| T_heater_head_low                | Header Head Lowest Temperature                                    | K                | K                
!    4| Beale_const_coef                 | Beale Constant Coefficient                                        | none             | none             
!    5| Beale_first_coef                 | Beale first-order coefficient                                     | 1/W              | 1/W              
!    6| Beale_square_coef                | Beale second-order coefficient                                    | 1/W2             | 1/W2             
!    7| Beale_third_coef                 | Beale third-order coefficient                                     | 1/W3             | 1/W3             
!    8| Beale_fourth_coef                | Beale fourth-order coefficient                                    | 1/W4             | 1/W4             
!    9| Pressure_coef                    | Pressure constant coefficient                                     | MPa              | MPa              
!   10| Pressure_first                   | Pressure first-order coefficient                                  | MPa/W            | MPa/W            
!   11| engine_speed                     | Engine operating speed                                            | rpm              | rpm              
!   12| V_displaced                      | Displaced engine volume                                           | cm3              | cm3              

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Inputs
!    1| P_SE_in                          | Receiver output power (equals engine inlet power)                 | kW               | kW               
!    2| T_amb_in                         | Ambient temperature in Kelvin                                     | K                | K                
!    3| Number_of_Collectors             | Number of collectors                                              | none             | none             
!    4| T_compression                    | Receiver efficiency                                               | K                | K                
!    5| rho_air                          | Air density                                                       | kg/m3            | kg/m3            
!    6| DNI                              | Direct normal radiation (not interpolated)                        | W/m2             | W/m2             
!    7| T_heater_head_operate            | Receiver head operating temperature                               | K                | K                
!    8| I_cut_in                         | Cut in DNI value used in the simulation                           | W/m2             | W/m2             
!    9| P_in_collector                   | Power incident on the collector                                   | kW               | kW               

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Outputs
!    1| P_out_SE                         | Stirling engine gross output                                      | kW               | kW               
!    2| T_amb_out                        | Ambient temperature in Kelvin                                     | K                | K                
!    3| P_SE_Losses                      | Stirling engine losses                                            | none             | none             
!    4| eta_SE                           | Stirling engine efficiency                                        | none             | none             
!    5| rho_air                          | Air density                                                       | kg/m3            | kg/m3            
!    6| DNI                              | Direct normal radiation (not interpolated)                        | W/m2             | W/m2             
!    7| T_heater_head_low                | Header Head Lowest Temperature                                    | K                | K                
!    8| T_heater_head_high               | Heater Head Set Temperature                                       | K                | K                
!    9| V_displaced                      | Displaced engine volume                                           | cm3              | cm3              
!   10| frequency                        | Engine frequency (= RPM/60s)                                      | 1/s              | 1/s              
!   11| engine_pressure                  | Engine pressure                                                   | Pa               | Pa               
!   12| Number_of_Collectors             | Number of collectors                                              | none             | none             
!   13| I_cut_in                         | Cut in DNI value used in the simulation                           | W/m2             | W/m2             
!   14| - no name -                      | Gross efficiency of the system                                    | none             | none             



C (Comments and routine interface generated by TRNSYS Studio)
C************************************************************************

C    TRNSYS acess functions (allow to acess TIME etc.) 
      USE TrnsysConstants
      USE TrnsysFunctions

C-----------------------------------------------------------------------------------------------------------------------
C    REQUIRED BY THE MULTI-DLL VERSION OF TRNSYS
      !DEC$ATTRIBUTES DLLEXPORT :: TYPE297				!SET THE CORRECT TYPE NUMBER HERE
C-----------------------------------------------------------------------------------------------------------------------
C-----------------------------------------------------------------------------------------------------------------------
C    TRNSYS DECLARATIONS
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
C-----------------------------------------------------------------------------------------------------------------------

C-----------------------------------------------------------------------------------------------------------------------
C    USER DECLARATIONS - SET THE MAXIMUM NUMBER OF PARAMETERS (NP), INPUTS (NI),
C    OUTPUTS (NOUT), AND DERIVATIVES (ND) THAT MAY BE SUPPLIED FOR THIS TYPE
      PARAMETER (NP=12,NI=9,NOUT=14,ND=0,NSTORED=0)
C-----------------------------------------------------------------------------------------------------------------------

C-----------------------------------------------------------------------------------------------------------------------
C    REQUIRED TRNSYS DIMENSIONS
      DIMENSION XIN(NI),OUT(NOUT),PAR(NP),YCHECK(NI),OCHECK(NOUT),
	1   STORED(NSTORED),T(ND),DTDT(ND)
      INTEGER NITEMS
C-----------------------------------------------------------------------------------------------------------------------
C-----------------------------------------------------------------------------------------------------------------------
C    ADD DECLARATIONS AND DEFINITIONS FOR THE USER-VARIABLES HERE


C    PARAMETERS
      DOUBLE PRECISION T_heater_head_high
      DOUBLE PRECISION T_heater_head_low
	DOUBLE PRECISION manufacturer
	DOUBLE PRECISION Beale_const_coef
	DOUBLE PRECISION Beale_first_coef
	DOUBLE PRECISION Beale_square_coef
	DOUBLE PRECISION Pressure_coef
	DOUBLE PRECISION Pressure_first

C    INPUTS
      DOUBLE PRECISION P_SE_in
      DOUBLE PRECISION T_amb_in
      DOUBLE PRECISION Number_of_Collectors
	DOUBLE PRECISION T_compression
	DOUBLE PRECISION rho_air
	DOUBLE PRECISION DNI
	DOUBLE PRECISION T_heater_head_operate

C    Equations Below
	DOUBLE PRECISION P_SE_out
	DOUBLE PRECISION engine_speed
	DOUBLE PRECISION V_displaced
	DOUBLE PRECISION frequency
	DOUBLE PRECISION Beale_max_fit
	DOUBLE PRECISION engine_pressure_fit
	DOUBLE PRECISION I_cut_in
	DOUBLE PRECISION Beale_third_coef
	DOUBLE PRECISION Beale_fourth_coef
	DOUBLE PRECISION P_in_collector
	

C-----------------------------------------------------------------------------------------------------------------------
C       READ IN THE VALUES OF THE PARAMETERS IN SEQUENTIAL ORDER
      manufacturer=PAR(1)
      T_heater_head_high=PAR(2)
      T_heater_head_low=PAR(3)
	Beale_const_coef=PAR(4)
	Beale_first_coef=PAR(5)
	Beale_square_coef=PAR(6)
	Beale_third_coef=PAR(7)
	Beale_fourth_coef=PAR(8)
	Pressure_coef=PAR(9)
	Pressure_first=PAR(10)
	engine_speed=PAR(11)
	V_displaced = PAR(12)


C-----------------------------------------------------------------------------------------------------------------------
C    RETRIEVE THE CURRENT VALUES OF THE INPUTS TO THIS MODEL FROM THE XIN ARRAY IN SEQUENTIAL ORDER

      P_SE_in=XIN(1)
      T_amb_in=XIN(2)
      Number_of_Collectors=XIN(3) 
	T_compression=XIN(4)
	rho_air=XIN(5)
	DNI=XIN(6)
	T_heater_head_operate=XIN(7)
	I_cut_in=XIN(8)
	P_in_collector = XIN(9)
	   IUNIT=INFO(1)
	   ITYPE=INFO(2)

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
C    PERFORM ANY 'AFTER-ITERATION' MANIPULATIONS THAT ARE REQUIRED HERE
C    e.g. save variables to storage array for the next timestep
      IF (INFO(13).GT.0) THEN
	   NITEMS=0
C	   STORED(1)=... (if NITEMS > 0)
C        CALL setStorageVars(STORED,NITEMS,INFO)
	   RETURN 1
	ENDIF
C
C-----------------------------------------------------------------------------------------------------------------------

C-----------------------------------------------------------------------------------------------------------------------
C    DO ALL THE VERY FIRST CALL OF THE SIMULATION MANIPULATIONS HERE
      IF (INFO(7).EQ.-1) THEN

C       SET SOME INFO ARRAY VARIABLES TO TELL THE TRNSYS ENGINE HOW THIS TYPE IS TO WORK
         INFO(6)=NOUT				
         INFO(9)=1				
	   INFO(10)=0	!STORAGE FOR VERSION 16 HAS BEEN CHANGED				

C       SET THE REQUIRED NUMBER OF INPUTS, PARAMETERS AND DERIVATIVES THAT THE USER SHOULD SUPPLY IN THE INPUT FILE
C       IN SOME CASES, THE NUMBER OF VARIABLES MAY DEPEND ON THE VALUE OF PARAMETERS TO THIS MODEL....
         NIN=NI
	   NPAR=NP
	   NDER=ND
	       
C       CALL THE TYPE CHECK SUBROUTINE TO COMPARE WHAT THIS COMPONENT REQUIRES TO WHAT IS SUPPLIED IN 
C       THE TRNSYS INPUT FILE
	   CALL TYPECK(1,INFO,NIN,NPAR,NDER)

C       SET THE NUMBER OF STORAGE SPOTS NEEDED FOR THIS COMPONENT
         NITEMS=0
C	   CALL setStorageSize(NITEMS,INFO)

C       RETURN TO THE CALLING PROGRAM
         RETURN 1

      ENDIF
C-----------------------------------------------------------------------------------------------------------------------

C-----------------------------------------------------------------------------------------------------------------------
C    DO ALL OF THE INITIAL TIMESTEP MANIPULATIONS HERE - THERE ARE NO ITERATIONS AT THE INTIAL TIME
      IF (TIME .LT. (getSimulationStartTime() +
     . getSimulationTimeStep()/2.D0)) THEN

C       SET THE UNIT NUMBER FOR FUTURE CALLS
         IUNIT=INFO(1)
         ITYPE=INFO(2)

C       CHECK THE PARAMETERS FOR PROBLEMS AND RETURN FROM THE SUBROUTINE IF AN ERROR IS FOUND
C         IF(...) CALL TYPECK(-4,INFO,0,"BAD PARAMETER #",0)

C       PERFORM ANY REQUIRED CALCULATIONS TO SET THE INITIAL VALUES OF THE OUTPUTS HERE
C		 P_out_SE
			OUT(1)=0
C		 T_amb_out
			OUT(2)=0
C		 P_SE_Losses
			OUT(3)=0
C		 eta_SE
			OUT(4)=0
C		rho_air  kg/m^3  [-Inf;+Inf]
			OUT(5)=0
C		DNI  W/m^2 [-Inf;+Inf]
			OUT(6)=0
C		T_heater_head_low  K  [-Inf;+Inf]
			OUT(7)=0
C		T_heater_head_high  K  [-Inf;+Inf]
			OUT(8)=0
C		V_displaced    cm^3  [-Inf;+Inf]
			OUT(9)=0
C		frequency  1/s   [-Inf;+Inf]
			OUT(10)=0
C		engine_pressure  Pa  [-Inf;+Inf]
			OUT(11)=0
C		Number_of_Collectors	- [-Inf;+Inf]
			OUT(12)=0
C		I_cut_in   W/m^2 [-Inf;+Inf]
			OUT(13)=0



C       PERFORM ANY REQUIRED CALCULATIONS TO SET THE INITIAL STORAGE VARIABLES HERE
         NITEMS=0
C	   STORED(1)=...

C       PUT THE STORED ARRAY IN THE GLOBAL STORED ARRAY
C         CALL setStorageVars(STORED,NITEMS,INFO)

C       RETURN TO THE CALLING PROGRAM
         RETURN 1

      ENDIF
C-----------------------------------------------------------------------------------------------------------------------

C-----------------------------------------------------------------------------------------------------------------------
C    *** ITS AN ITERATIVE CALL TO THIS COMPONENT ***
C-----------------------------------------------------------------------------------------------------------------------

	    
C-----------------------------------------------------------------------------------------------------------------------
C    RETRIEVE THE VALUES IN THE STORAGE ARRAY FOR THIS ITERATION
C      NITEMS=
C	CALL getStorageVars(STORED,NITEMS,INFO)
C      STORED(1)=
C-----------------------------------------------------------------------------------------------------------------------
C-----------------------------------------------------------------------------------------------------------------------
C    CHECK THE INPUTS FOR PROBLEMS
C      IF(...) CALL TYPECK(-3,INFO,'BAD INPUT #',0,0)
C	IF(IERROR.GT.0) RETURN 1
C-----------------------------------------------------------------------------------------------------------------------
C-----------------------------------------------------------------------------------------------------------------------
C    *** PERFORM ALL THE CALCULATION HERE FOR THIS MODEL. ***
C-----------------------------------------------------------------------------------------------------------------------

C		ADD YOUR COMPONENT EQUATIONS HERE; BASICALLY THE EQUATIONS THAT WILL
C		CALCULATE THE OUTPUTS BASED ON THE PARAMETERS AND THE INPUTS.	REFER TO
C		CHAPTER 3 OF THE TRNSYS VOLUME 1 MANUAL FOR DETAILED INFORMATION ON
C		WRITING TRNSYS COMPONENTS.




	!===============================================
	!Manufacturer specific engine details
	

	if(manufacturer .EQ. 1) then    !SES System = 1
	T_heater_head_high=993	
	T_heater_head_low=973	
	Beale_const_coef=0.04247
	Beale_first_coef=0.00001682
	Beale_square_coef=-5.105E-10 
	Beale_third_coef=7.07260E-15
	Beale_fourth_coef=-3.586E-20
	Pressure_coef=0.658769
	Pressure_first=0.000234963
	engine_speed = 1800  !rpm
	V_displaced = 380 * 0.000001     !convert(cm^3, m^3)


	elseif(manufacturer .EQ. 2) then    !WGA System = 2
	T_heater_head_high=903	
	T_heater_head_low=903	
	Beale_const_coef= 0.0850686         !0.103371  !-0.00182451  
	Beale_first_coef= 0.0000194116        !0.0000184703     !0.0000260289  
	Beale_square_coef=-3.18449E-10       !-3.07798e-10    !-4.68164E-10 
	Pressure_coef= -0.736342          ! -0.412058  !-0.0200284
	Pressure_first= 0.00036416          !0.000359699  !0.000352522
	engine_speed = 1800  !rpm
	V_displaced = 160 * 0.000001     !convert(cm^3, m^3)
	Beale_third_coef=0
	Beale_fourth_coef=0

     	elseif(manufacturer .EQ. 3) then    !SBP System = 3
	T_heater_head_high=903	
	T_heater_head_low=903	
	Beale_const_coef= -0.00182451
	Beale_first_coef=0.0000260289
	Beale_square_coef=-4.68164E-10
	Pressure_coef=-0.0200284
	Pressure_first=0.000352522
	engine_speed = 1800  !rpm  
!actually 1500rpm but would need new curve...data allows this curve to be accurate
!heat xfer will be worse based on 1500rpm
	V_displaced = 160 * 0.000001     !convert(cm^3, m^3)
	Beale_third_coef=0
	Beale_fourth_coef=0

	elseif(manufacturer .EQ. 4) then    !SAIC System = 4
	T_heater_head_high=993	
	T_heater_head_low=973
	Beale_const_coef=-0.016
	Beale_first_coef=0.000015
	Beale_square_coef=-3.50E-10
	Beale_third_coef=3.85E-15
	Beale_fourth_coef=-1.6E-20
	Pressure_coef=0.0000347944
	Pressure_first=5.26329E-9
	engine_speed = 2200  !rpm
!	V_displaced = 480 * 0.000001  This varies for SAIC.....see below


	elseif(manufacturer .EQ. 5) then    !User inputs values = 5
	T_heater_head_high=T_heater_head_high	
	T_heater_head_low=T_heater_head_low	
	Beale_const_coef=Beale_const_coef
	Beale_first_coef=Beale_first_coef
	Beale_square_coef=Beale_square_coef
	Pressure_coef=Pressure_coef
	Pressure_first=Pressure_first
	engine_speed = engine_speed  !rpm
	V_displaced = V_displaced      !input is in m^3!!!!
	Beale_third_coef=Beale_third_coef
	Beale_fourth_coef=Beale_fourth_coef


	endif  !end of if statement



	!===============================================
	!Curve fit of engine performance using Beale-Max method [Watts]
	!X-axis is input power to Stirling engine in Watts
	!Y-axis is the Beale number divided by 1-sqrt(TC/TE)

	frequency = MAX(0.001,engine_speed / 60)  !Hz

	Beale_max_fit=Beale_const_coef + Beale_first_coef*P_SE_in*1000 + 
     .              (Beale_square_coef*(P_SE_in*1000)**2)+ 
     .              (Beale_third_coef*(P_SE_in*1000)**3) + 
     .              (Beale_fourth_coef*(P_SE_in*1000)**4)  

	!Curve fit of the engine pressure vs. power in to the engine [MPa]
	if(manufacturer .EQ. 4) then   !SAIC varies the engine volume not pressure
	engine_pressure_fit = 12 !MPa
	V_displaced = MAX(0.00001,Pressure_coef+ !pressure_coef actually is volume
     .	Pressure_first*P_SE_in*1000)
	else   !all other systems vary the pressure not volume
	engine_pressure_fit =MAX(0.001,Pressure_coef + 
     .	Pressure_first*P_SE_in*1000)
	endif  !end of if statement
	
	!output gross power from engine
	P_SE_out = (Beale_max_fit * (engine_pressure_fit * 10**6 * 
     .	V_displaced*frequency)*(1-(T_compression/T_heater_head_operate)
     .         **0.5)) / 1000

C     =================================================
C	Make sure 


C-----------------------------------------------------------------------------------------------------------------------

C-----------------------------------------------------------------------------------------------------------------------
C-----------------------------------------------------------------------------------------------------------------------
C    SET THE STORAGE ARRAY AT THE END OF THIS ITERATION IF NECESSARY
C      NITEMS=
C      STORED(1)=
C	CALL setStorageVars(STORED,NITEMS,INFO)
C-----------------------------------------------------------------------------------------------------------------------
C-----------------------------------------------------------------------------------------------------------------------
C    REPORT ANY PROBLEMS THAT HAVE BEEN FOUND USING CALLS LIKE THIS:
C      CALL MESSAGES(-1,'put your message here','MESSAGE',IUNIT,ITYPE)
C      CALL MESSAGES(-1,'put your message here','WARNING',IUNIT,ITYPE)
C      CALL MESSAGES(-1,'put your message here','SEVERE',IUNIT,ITYPE)
C      CALL MESSAGES(-1,'put your message here','FATAL',IUNIT,ITYPE)
C-----------------------------------------------------------------------------------------------------------------------
C-----------------------------------------------------------------------------------------------------------------------
C    SET THE OUTPUTS FROM THIS MODEL IN SEQUENTIAL ORDER AND GET OUT

	!Gross output power from the Stirling engine
	!if the input power is less than 25W, then the output power will be 0
			if(P_SE_in .GE. 0.025)then 
				if (P_SE_out .GE. 0) then 
					if (P_SE_out .LT. P_SE_in) then   
						OUT(1) = P_SE_out
					else
						OUT(1) = 0
					endif
				else
					OUT(1) = 0
				endif
			else
				OUT(1) = 0
			endif
	!_________________________________________
C		 T_amb_out
			OUT(2)=T_amb_in
	!_________________________________________

C		Stirling Engine Losses
			if(P_SE_in .GE. 0.025)then 
				if (P_SE_out .GE. 0) then  
					if (P_SE_out .LT. P_SE_in) then  
					OUT(3) = P_SE_in - P_SE_out
					else
					OUT(3) = 0.001  !1 Watt so parasitic code is ok 
					endif
				else
					OUT(3) = 0.001  !1 Watt so parasitic code is ok 
				endif
			else
				OUT(3) = 0.001 !1 Watt so parasitic code is ok
			endif

	!_________________________________________

C		eta_SE

		OUT(4) = OUT(1) / ( OUT(1) + OUT(3) + 0.000000001)
	!_________________________________________

C		rho_air
		OUT(5) = rho_air
	!_________________________________________

C		DNI
		OUT(6) = DNI

	!_________________________________________

C		T_heater_head_low
		OUT(7) = T_heater_head_low

	!_________________________________________

C		T_heater_head_high
		OUT(8) = T_heater_head_high

	!_________________________________________

C		V_swept
		OUT(9) = V_displaced

	!_________________________________________

C		frequency
		OUT(10) = frequency

	!_________________________________________

C		engine_pressure
		OUT(11) = engine_pressure_fit*10**6
	!_________________________________________

C		Number_of_Collectors
		OUT(12) = Number_of_Collectors

	!_________________________________________
C		I_cut_in [W/m^2]
		OUT(13) = I_cut_in

	!_________________________________________
C		efficiency of the system (gross) 
		OUT(14) = OUT(1) / (P_in_collector+0.00000001)

C-----------------------------------------------------------------------------------------------------------------------
C    EVERYTHING IS DONE - RETURN FROM THIS SUBROUTINE AND MOVE ON
      RETURN 1
      END
C-----------------------------------------------------------------------------------------------------------------------
