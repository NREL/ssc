			SUBROUTINE TYPE298 (TIME,XIN,OUT,T,DTDT,PAR,INFO,ICNTRL,*) 
C************************************************************************
C  COPYRIGHT 2008 NATIONAL RENEWABLE ENERGY LABORATORY
C Object: Parasitic_power
C Simulation Studio Model: 10_18_07_Parasitic
C 
C Author: 
C Editor: 
C Date:	 June 05, 2007 last modified: June 05, 2007
C 
C 
! Doc. tables updated 7/1/2010 - MJW
!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Parameters
!    1| Cooling_Tower_ON                 | Option to use a cooling tower (set to 0=off)                      | none             | none             
!    2| Tower_mode                       | Cooling tower type (natural or forced draft)                      | none             | none             
!    3| d_pipe_tower                     | Runner pipe diameter to the cooling tower (set to 0.4m)           | m                | m                
!    4| Tower_m_dot_water                | Tower cooling water flow rate (set to 134,000 kg/hr)              | kg/hr            | kg/hr            
!    5| Tower_m_dot_water_TEST           | Test value for the cooling water flow rate (set to 134,000 kg/hr) | kg/hr            | kg/hr            
!    6| tower_pipe_material              | Tower pipe material (1=plastic, 2=new cast iron, 3=riveted steel) | none             | none             
!    7| eta_tower_pump                   | Tower pump efficiency (set to 0.6)                                | none             | none             
!    8| fan_control_signal               | Fan control signal (set to 1, not used in this model)             | none             | none             
!    9| EPSILON_tower_TEST               | Test value for cooling tower effectiveness (set to 0.7)           | none             | none             
!   10| system_availability              | System availability (set to 1.0)                                  | none             | none             
!   11| pump_speed                       | Reference Condition Pump Speed                                    | rpm              | rpm              
!   12| Fan_speed1                       | Cooling system fan speed 1                                        | rpm              | rpm              
!   13| Fan_speed2                       | Cooling system fan speed 2                                        | rpm              | rpm              
!   14| Fan_speed3                       | Cooling system fan speed 3                                        | rpm              | rpm              
!   15| T_cool_speed2                    | Cooling Fluid Temp. For Fan Speed 2 Cut-In                        | C                | C                
!   16| T_cool_speed3                    | Cooling Fluid Temp. For Fan Speed 3 Cut-In                        | C                | C                
!   17| epsilon_cooler_TEST              | Cooler effectiveness                                              | none             | none             
!   18| epsilon_radiator_TEST            | Radiator effectiveness                                            | none             | none             
!   19| Cooling_fluid                    | Reference Condition Cooling Fluid: 0=Water,1=V50%EG,2=V25%EG,3=V40%PG,4=V25%PG| none             | none             
!   20| manufacturer                     | Manufacturer (fixed as 5="other")                                 | none             | none             
!   21| P_controls                       | Control System Parasitic Power, Avg.                              | W                | W                
!   22| TEST_P_pump                      | Reference Condition Pump Parasitic Power                          | W                | W                
!   23| TEST_pump_speed                  | Reference Condition Pump Speed                                    | rpm              | rpm              
!   24| TEST_cooling_fluid               | Reference Condition Cooling Fluid                                 | none             | none             
!   25| TEST_T_fluid                     | Reference Condition Cooling Fluid Temperature                     | K                | K                
!   26| TEST_V_dot_fluid                 | Reference Condition Cooling Fluid Volumetric Flow Rate            | gpm              | gpm              
!   27| TEST_P_fan                       | Reference Condition Cooling System Fan Power                      | W                | W                
!   28| TEST_fan_speed                   | Reference Condition Cooling System Fan Speed                      | rpm              | rpm              
!   29| TEST_fan_diameter                | Reference Condition fan diameter                                  | m                | m                
!   30| TEST_fan_rho_air                 | Reference condition fan air density                               | kg/m3            | kg/m3            
!   31| TEST_fan_CFM                     | Reference condition van volumentric flow rate                     | cfm              | cfm              
!   32| b_radiator                       | b_radiator parameter                                              | none             | none             
!   33| b_cooler                         | b_cooler parameter                                                | none             | none             

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Inputs
!    1| gross_power                      | Stirling engine gross output                                      | kW               | kW               
!    2| T_amb                            | Ambient temperature in Kelvin                                     | K                | K                
!    3| Number_of_Collectors             | Number of collectors                                              | none             | none             
!    4| rho_air                          | Air density                                                       | kg/m3            | kg/m3            
!    5| DNI                              | Direct normal radiation (not interpolated)                        | W/m2             | W/m2             
!    6| T_heater_head_low                | Header Head Lowest Temperature                                    | K                | K                
!    7| V_swept                          | Displaced engine volume                                           | cm3              | cm3              
!    8| frequency                        | Engine frequency (= RPM/60s)                                      | 1/s              | 1/s              
!    9| engine_pressure                  | Engine pressure                                                   | Pa               | Pa               
!   10| I_cut_in                         | Cut in DNI value used in the simulation                           | W/m2             | W/m2             
!   11| Q_reject                         | Stirling engine losses                                            | W                | W                
!   12| Tower_water_outlet_temp          | Tower water outlet temperature (set to 20)                        | C                | C                
!   13| P_amb_Pa                         | Atmospheric pressure                                              | Pa               | Pa               
!   14| NS_dish_seperation               | North-South dish separation used in the simulation                | m                | m                
!   15| EW_dish_seperation               | East-West dish separation used in the simulation                  | m                | m                
!   16| P_tower_fan                      | Tower fan power (set to 0)                                        | kJ/hr            | kJ/hr            
!   17| Power_in_Collector               | Power incident on the collector                                   | kW               | kW               

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Outputs
!    1| net_power                        | Net system output power                                           | kW               | kW               
!    2| P_parasitic                      | Total parasitic power load                                        | W                | W                
!    3| T_compression                    | Cold sink temperature / compression temperature                   | K                | K                
!    4| P_fan                            | System fan power                                                  | W                | W                
!    5| P_pump                           | Pumping parasitic power                                           | W                | W                
!    6| Tower_water_inlet_temp           | Cooling water temperature into the cooling system                 | C                | C                
!    7| Tower_m_dot_water                | Cooling water mass flow rate in the cooling tower                 | kg/hr            | kg/hr            
!    8| T_amb_C                          | Ambient temperature                                               | C                | K                
!    9| fan_control_signal               | Fan control signal (set to 1, not used in this model)             | none             |                  
!   10| T_compression_C                  | Cold sink temperature / compression temperature in Celsius        | C                | K                
!   11| P_parasitic_tower                | Parasitic load associated with the heat rejection system          | W                | W                
!   12| no name                          | Cooling fluid temperature out of the cooling and into the tower   | C                | none             
!   13| no name                          | Cooling fluid temperature into the cooler and out of the tower    | C                | none             
!   14| no name                          | Net system efficiency                                             | none             | none             


C (Comments and routine interface generated by TRNSYS Studio)
C************************************************************************

C    TRNSYS acess functions (allow to acess TIME etc.) 
      USE TrnsysConstants
      USE TrnsysFunctions



C-----------------------------------------------------------------------------------------------------------------------
C    REQUIRED BY THE MULTI-DLL VERSION OF TRNSYS
      !DEC$ATTRIBUTES DLLEXPORT :: TYPE298				!SET THE CORRECT TYPE NUMBER HERE
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
      PARAMETER (NP=32,NI=17,NOUT=14,ND=0,NSTORED=0)
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
      DOUBLE PRECISION system_availability
      DOUBLE PRECISION Fan_speed1
      DOUBLE PRECISION Fan_speed2
      DOUBLE PRECISION Fan_speed3
      DOUBLE PRECISION DNI_speed2
      DOUBLE PRECISION DNI_speed3
      DOUBLE PRECISION epsilon_cooler_TEST
      DOUBLE PRECISION epsilon_radiator_TEST
      DOUBLE PRECISION manufacturer
      DOUBLE PRECISION P_controls
      DOUBLE PRECISION P_pump
      DOUBLE PRECISION TEST_P_fan
      DOUBLE PRECISION TEST_fan_speed
      DOUBLE PRECISION TEST_fan_diameter
      DOUBLE PRECISION TEST_fan_rho_air
      DOUBLE PRECISION TEST_fan_CFM
	DOUBLE PRECISION T_cool_speed2
	DOUBLE PRECISION T_cool_speed3

C    INPUTS
      DOUBLE PRECISION gross_power
      DOUBLE PRECISION T_amb
      DOUBLE PRECISION Number_of_Collectors
	DOUBLE PRECISION rho_air
	DOUBLE PRECISION DNI
	DOUBLE PRECISION T_heater_head_low
	DOUBLE PRECISION V_swept
	DOUBLE PRECISION engine_pressure
	DOUBLE PRECISION frequency
	DOUBLE PRECISION P_SE_losses

C	Parameters below
	DOUBLE PRECISION epsilon_cooler
	DOUBLE PRECISION epsilon_radiator
	DOUBLE PRECISION m_dot_cool_fluid 
	DOUBLE PRECISION Fan_speed
	DOUBLE PRECISION TEST_fan_speed_rad
	DOUBLE PRECISION Fan_speed_rad
	DOUBLE PRECISION C_W
	DOUBLE PRECISION P_fan
	DOUBLE PRECISION P_parasitic
	DOUBLE PRECISION R_gas
	DOUBLE PRECISION M_H2
	DOUBLE PRECISION T_H2_ave
	DOUBLE PRECISION V_total
	DOUBLE PRECISION mass_H2
	DOUBLE PRECISION m_dot_H2
	DOUBLE PRECISION Cp_H2
	DOUBLE PRECISION C_dot_H2
	DOUBLE PRECISION Cp_cool_fluid
	DOUBLE PRECISION rho_eglyc
	DOUBLE PRECISION Cp_air
	DOUBLE PRECISION C_dot_air
	DOUBLE PRECISION C_dot_cool_fluid
	DOUBLE PRECISION C_dot_min_radiator
	DOUBLE PRECISION T_cool_out
	DOUBLE PRECISION T_cool_in
	DOUBLE PRECISION T_cool_water_ave
	DOUBLE PRECISION C_dot_min_cooler
	DOUBLE PRECISION T_H2_in
	DOUBLE PRECISION m_dot_air
	DOUBLE PRECISION Q_losses
	DOUBLE PRECISION T_H2_out
	DOUBLE PRECISION T_compression
	DOUBLE PRECISION V_dot_test
	DOUBLE PRECISION C_V
	DOUBLE PRECISION V_dot_air
	DOUBLE PRECISION net_power_out
	DOUBLE PRECISION total_net_power
	DOUBLE PRECISION Fan_speed_use
	DOUBLE PRECISION I_cut_in
	DOUBLE PRECISION Cooling_fluid
	DOUBLE PRECISION pump_speed
	DOUBLE PRECISION pump_d_impeller
	DOUBLE PRECISION TEST_rho_fluid
	DOUBLE PRECISION TEST_V_dot_fluid
	DOUBLE PRECISION TEST_P_pump
	DOUBLE PRECISION TEST_pump_speed
	DOUBLE PRECISION TEST_cooling_fluid
	DOUBLE PRECISION TEST_T_fluid
	DOUBLE PRECISION TEST_pump_d_impeller
	DOUBLE PRECISION mu_water
	DOUBLE PRECISION T_K2
	DOUBLE PRECISION b_radiator
	DOUBLE PRECISION b_cooler
	DOUBLE PRECISION mu_fluid_TEST
	DOUBLE PRECISION rho_cool_fluid_TEST
	DOUBLE PRECISION Cp_fluid_TEST
	DOUBLE PRECISION V_dot_air_TEST
	DOUBLE PRECISION TEST_pump_speed_rad
	DOUBLE PRECISION pump_speed_rad
	DOUBLE PRECISION C_W_pump
	DOUBLE PRECISION rho_cool_fluid
	DOUBLE PRECISION C_V_pump
	DOUBLE PRECISION rho_fluid
	DOUBLE PRECISION Cp_fluid
	DOUBLE PRECISION mu_cool_fluid
	DOUBLE PRECISION V_dot_cool_fluid
	DOUBLE PRECISION engine_pressure_TEST
	DOUBLE PRECISION frequency_TEST
	DOUBLE PRECISION T_H2_ave_TEST
	DOUBLE PRECISION mass_H2_TEST
	DOUBLE PRECISION rho_H2
	DOUBLE PRECISION rho_H2_TEST 
	DOUBLE PRECISION m_dot_H2_TEST
	DOUBLE PRECISION V_dot_H2
	DOUBLE PRECISION V_dot_H2_TEST
	DOUBLE PRECISION C_dot_H2_TEST
	DOUBLE PRECISION Cp_H2_TEST
	DOUBLE PRECISION m_dot_cool_fluid_TEST
	DOUBLE PRECISION C_dot_cool_fluid_TEST
	DOUBLE PRECISION Cp_air_TEST
	DOUBLE PRECISION C_dot_air_TEST
	DOUBLE PRECISION C_dot_min_TEST_rad
	DOUBLE PRECISION T_K1
	DOUBLE PRECISION m_dot_air_TEST
	DOUBLE PRECISION C_dot_max_TEST_rad
	DOUBLE PRECISION C_dot_min_rad
	DOUBLE PRECISION C_dot_max_rad
	DOUBLE PRECISION V_dot_min_rad
	DOUBLE PRECISION V_dot_min_TEST_rad
	DOUBLE PRECISION C_dot_min_TEST_cooler
	DOUBLE PRECISION C_dot_max_TEST_cooler
	DOUBLE PRECISION C_dot_max_cooler
	DOUBLE PRECISION V_dot_min_cooler
	DOUBLE PRECISION V_dot_min_TEST_cooler
	DOUBLE PRECISION Cr_radiator_TEST
	DOUBLE PRECISION NTU_radiator_TEST
	DOUBLE PRECISION UA_radiator_TEST
	DOUBLE PRECISION UA_radiator
	DOUBLE PRECISION NTU_radiator
	DOUBLE PRECISION Cr_radiator
	DOUBLE PRECISION Cr_cooler_TEST
	DOUBLE PRECISION NTU_cooler_TEST
	DOUBLE PRECISION UA_cooler_TEST
	DOUBLE PRECISION UA_cooler
	DOUBLE PRECISION NTU_cooler
	DOUBLE PRECISION Cr_cooler
	DOUBLE PRECISION mu_div_mu_water
	DOUBLE PRECISION pump_multiplier
	DOUBLE PRECISION EPSILON_rad_TEST
	DOUBLE PRECISION T_tolerance
	DOUBLE PRECISION d_T
	DOUBLE PRECISION T_difference
	DOUBLE PRECISION T_res
	DOUBLE PRECISION Type
	DOUBLE PRECISION Tower_m_dot_water
	DOUBLE PRECISION Tower_fan_speed_fraction
	DOUBLE PRECISION relative_humidity
	DOUBLE PRECISION Q_reject
	DOUBLE PRECISION Tower_water_outlet_temp
	DOUBLE PRECISION T_amb_C
	DOUBLE PRECISION B17
	DOUBLE PRECISION D17
	DOUBLE PRECISION T_wet_bulb
	DOUBLE PRECISION Q_reject_total
	DOUBLE PRECISION Tower_water_inlet_temp
	DOUBLE PRECISION T_wet_bulb_K
	DOUBLE PRECISION P_amb_Pa
	DOUBLE PRECISION P_amb_atm
	DOUBLE PRECISION fan_control_signal
	DOUBLE PRECISION ambient_humidity_ratio
	DOUBLE PRECISION P_sat
	DOUBLE PRECISION Pv
	DOUBLE PRECISION Cooling_Tower_ON
	DOUBLE PRECISION Tower_m_dot_water_TEST
	DOUBLE PRECISION EPSILON_tower_TEST
	DOUBLE PRECISION b_tower
	DOUBLE PRECISION rho_tower
	DOUBLE PRECISION rho_tower_TEST
	DOUBLE PRECISION Cp_tower_water_TEST
	DOUBLE PRECISION C_dot_tower_TEST
	DOUBLE PRECISION C_dot_tower
	DOUBLE PRECISION V_dot_tower
	DOUBLE PRECISION V_dot_tower_TEST
	DOUBLE PRECISION C_dot_min_TEST_tower
	DOUBLE PRECISION C_dot_max_TEST_tower
	DOUBLE PRECISION C_dot_min_tower
	DOUBLE PRECISION C_dot_max_tower
	DOUBLE PRECISION V_dot_min_tower
	DOUBLE PRECISION V_dot_min_TEST_tower
	DOUBLE PRECISION Cr_tower_TEST
	DOUBLE PRECISION NTU_tower_TEST
	DOUBLE PRECISION UA_tower_TEST
	DOUBLE PRECISION UA_tower
	DOUBLE PRECISION Cp_tower_water
	DOUBLE PRECISION NTU_tower
	DOUBLE PRECISION Cr_tower
	DOUBLE PRECISION EPSILON_tower
	DOUBLE PRECISION T1
	DOUBLE PRECISION Tower_water_ave
	DOUBLE PRECISION NS_dish_separation
	DOUBLE PRECISION EW_dish_separation
	DOUBLE PRECISION pipe_length
	DOUBLE PRECISION d_pipe_tower
	DOUBLE PRECISION A_pipe
	DOUBLE PRECISION vel_tower
	DOUBLE PRECISION mu_tower
	DOUBLE PRECISION Re_tower
	DOUBLE PRECISION friction_factor
	DOUBLE PRECISION head_friction
	DOUBLE PRECISION K_total
	DOUBLE PRECISION head_minor
	DOUBLE PRECISION head_total
	DOUBLE PRECISION P_tower_pump
	DOUBLE PRECISION epsilon_wall
	DOUBLE PRECISION tower_pipe_material
	DOUBLE PRECISION Tower_mode
	DOUBLE PRECISION P_tower_fan
	DOUBLE PRECISION P_parasitic_tower
	DOUBLE PRECISION SUM
	DOUBLE PRECISION eta_tower_pump
	DOUBLE PRECISION P_in_collector


C-----------------------------------------------------------------------------------------------------------------------
C       READ IN THE VALUES OF THE PARAMETERS IN SEQUENTIAL ORDER

	Cooling_Tower_ON = PAR(1)
	Tower_mode = PAR(2)
	d_pipe_tower = PAR(3)	
	Tower_m_dot_water = PAR(4)
	Tower_m_dot_water_TEST = PAR(5)
	tower_pipe_material = PAR(6)
	eta_tower_pump = PAR(7)
	fan_control_signal = PAR(8)
	EPSILON_tower_TEST = PAR(9)
	system_availability=PAR(10)
	pump_speed=PAR(11)
      Fan_speed1=PAR(12)
      Fan_speed2=PAR(13)
      Fan_speed3=PAR(14)
      T_cool_speed2=PAR(15)
      T_cool_speed3=PAR(16)
      epsilon_cooler_TEST=PAR(17)
      epsilon_radiator_TEST=PAR(18)
	Cooling_fluid = PAR(19)
      manufacturer=PAR(20)
      P_controls=PAR(21)
      TEST_P_pump=PAR(22)
	TEST_pump_speed=PAR(23)
	TEST_cooling_fluid=PAR(24)
	TEST_T_fluid=PAR(25)
	TEST_V_dot_fluid=PAR(26)
      TEST_P_fan=PAR(27)
      TEST_fan_speed=PAR(28)
      TEST_fan_rho_air=PAR(29)
      TEST_fan_CFM=PAR(30)
	b_radiator = PAR(31)
	b_cooler = PAR(32)



C-----------------------------------------------------------------------------------------------------------------------
C    RETRIEVE THE CURRENT VALUES OF THE INPUTS TO THIS MODEL FROM THE XIN ARRAY IN SEQUENTIAL ORDER

      gross_power=XIN(1)
      T_amb=XIN(2)
      Number_of_Collectors=XIN(3)
	rho_air=XIN(4)
	DNI = XIN(5)
	T_heater_head_low=XIN(6)
	V_swept=XIN(7)
	frequency=XIN(8)
	engine_pressure=XIN(9)
	I_cut_in = XIN(10)
	Q_reject = XIN(11)
	Tower_water_outlet_temp = XIN(12) !+ 273.15
	P_amb_Pa = XIN(13)
	NS_dish_separation = XIN(14)
	EW_dish_separation = XIN(15)
	P_tower_fan = XIN(16)
	P_in_collector = XIN(17)

	   IUNIT=INFO(1)
	   ITYPE=INFO(2)
!=====================================================================


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
C		 net_power
			OUT(1)=0
C		 P_parasitic
			OUT(2)=0
C		 T_compression
			OUT(3)=0
C		total_net_power  kW  [-Inf;+Inf]
			OUT(4)=0
C		T_amb  K [-Inf;+Inf]
			OUT(5)=0
C		P_fan  W  [-Inf;+Inf]
			OUT(6)=0
C		P_pump  W  [-Inf;+Inf]
			OUT(7)=0
C		Tower_water_inlet_temp  C [-Inf;+Inf]
			OUT(8)=0
C		Tower_m_dot_water  kg/hr  [0;+Inf]
			OUT(9)=0
C		T_amb_C  C  [-Inf;+Inf]
			OUT(10)=0
C		P_amb_atm   atm    [-Inf;+Inf]
			OUT(11)=0
C		fan_control_signal  -  [-Inf;+Inf]
			OUT(12)=0
C		T_compression_C   C  [-Inf;+Inf]
			OUT(13)=0
C		P_parasitic_tower  W  [-Inf;+Inf]
			OUT(14)=0




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
	
	
	P_tower_fan = P_tower_fan *1000 / 3600   !convert kJ/hr to J/s
	P_SE_losses = Q_reject
	Q_losses = 1000*P_SE_losses  ![W] Heat rejected to the cooling system
	Q_reject_total=Number_of_Collectors*Q_reject*1000!W/kW
	pump_d_impeller = 0.15  !m  pump impeller diameter....won't change so need
	! any value for pump performane eq's
	TEST_fan_diameter = 0.63  ! does not change...arbitrary value chosen
	TEST_pump_d_impeller = 0.15   ! does not change...arbitrary value chosen
	Tower_m_dot_water = Tower_m_dot_water / 3600   !convert to kg/s
	Tower_m_dot_water_TEST = Tower_m_dot_water_TEST / 3600   !convert to kg/s

	!Air properties
	P_amb_atm = P_amb_Pa * 0.000009869
	T_amb_C = T_amb - 273.15


!===========================================================

	if(manufacturer .EQ. 1) then    !SES System = 1
	P_controls=150
      TEST_P_pump=100
	TEST_pump_speed=1800  !rpm
	TEST_cooling_fluid = 2
	TEST_T_fluid=288  !K
	TEST_V_dot_fluid= 9*0.003785/60  !convert 9 gpm --> m^3/s
	TEST_P_fan=1000      !W
      TEST_fan_speed=890   !rpm
      TEST_fan_rho_air=1.2
	TEST_fan_CFM=6000
	b_radiator = 0.7 !used for UA_act/UA_test = [V_dot_act/V_dot_test]^b
	b_cooler = 0.7   !used for UA_act/UA_test = [V_dot_act/V_dot_test]^b

	elseif(manufacturer .EQ. 2) then    !WGA System = 2
	P_controls= 100
      TEST_P_pump= 75
	TEST_pump_speed=1800  !rpm
	TEST_cooling_fluid = 2
	TEST_T_fluid=288  !K
	TEST_V_dot_fluid= 7.5*0.003785/60  !convert 7.5 gpm --> m^3/s
      TEST_P_fan=410
      TEST_fan_speed=890
      TEST_fan_rho_air=1.2
      TEST_fan_CFM=4000
	b_radiator = 0.7
	b_cooler = 0.7

     	elseif(manufacturer .EQ. 3) then    !SBP System = 3
	P_controls= 175
      TEST_P_pump= 100
	TEST_pump_speed=1800  !rpm
	TEST_cooling_fluid = 1
	TEST_T_fluid=288  !K
	TEST_V_dot_fluid= 7.5*0.003785/60
      TEST_P_fan=510
      TEST_fan_speed=890
      TEST_fan_rho_air=1.2
      TEST_fan_CFM=4500
	b_radiator = 0.7
	b_cooler = 0.7

	elseif(manufacturer .EQ. 4) then    !SAIC System = 4
	P_controls= 300
      TEST_P_pump= 200
	TEST_pump_speed=1800  !rpm
	TEST_cooling_fluid = 2
	TEST_T_fluid=288  !K
	TEST_V_dot_fluid= 12*0.003785/60
      TEST_P_fan=2500
      TEST_fan_speed=850
      TEST_fan_rho_air=1.2
      TEST_fan_CFM=10000
	b_radiator = 0.7
	b_cooler = 0.7

	elseif(manufacturer .EQ. 5) then    !User inputs values = 5
	P_controls=P_controls
      TEST_P_pump= TEST_P_pump
	TEST_pump_speed=TEST_pump_speed  !rpm
	TEST_cooling_fluid = TEST_cooling_fluid
	TEST_T_fluid=TEST_T_fluid  !K
	TEST_V_dot_fluid= TEST_V_dot_fluid*0.003785/60
      TEST_P_fan=TEST_P_fan
      TEST_fan_speed=TEST_fan_speed
      TEST_fan_rho_air=TEST_fan_rho_air
      TEST_fan_CFM=TEST_fan_CFM
	b_radiator = b_radiator
	b_cooler = b_cooler


	endif  

	!==================================================================
	!Determine properties of the cooling fluid during test conditions

	T_K2 = TEST_T_fluid
	T_K1 = TEST_T_fluid
	
	if(TEST_cooling_fluid .EQ. 1) then    !water
	mu_water=3.50542 - 0.0539638*T_K2 + 0.000333345*T_K2**2 - 
     .	0.0000010319*T_K2**3 + 1.59983E-09*T_K2**4 - 
     .     9.93386E-13*T_K2**5
	mu_fluid_TEST=3.50542 - 0.0539638*T_K2 + 0.000333345*T_K2**2 - 
     .	0.0000010319*T_K2**3 + 1.59983E-09*T_K2**4 - 
     .     9.93386E-13*T_K2**5
	rho_cool_fluid_TEST=692.604 + 2.2832*T_K2 - 0.00423412*T_K2**2
	Cp_fluid_TEST=2.14384E+06 - 34048.9*T_K2 + 216.467*T_K2**2 - 
     .	0.687249*T_K2**3 + 0.00108959*T_K2**4 - 6.90127E-07*T_K2**5

	elseif(TEST_cooling_fluid .EQ. 2) then  !50% Ethylene Glycol
	mu_fluid_TEST=18.3853 - 0.238994*T_K1 + 0.00116489*T_K1**2 - 
     .	0.00000252199*T_K1**3 + 2.04562E-09*T_K1**4
	rho_cool_fluid_TEST=1026.4 + 0.80163*T_K1 - 0.00227397*T_K1**2
	Cp_fluid_TEST=1899.32 + 4.19104*T_K1 + 0.00194702*T_K1**2

	elseif(TEST_cooling_fluid .EQ. 3) then  !25% Ethylene Glycol
	mu_fluid_TEST=5.33548 - 0.0686842*T_K1 + 0.000331949*T_K1**2 - 
     .	7.13336E-07*T_K1**3 + 5.74824E-10*T_K1**4
	rho_cool_fluid_TEST=823.536 + 1.76857*T_K1 - 0.00360812*T_K1**2
	Cp_fluid_TEST=3056.35 + 3.04401*T_K1 - 0.00126969*T_K1**2

	elseif(TEST_cooling_fluid .EQ. 4) then  !50% Propylene Glycol
	rho_cool_fluid_TEST=1032.38 + 0.658226*T_K1 - 0.00217079*T_K1**2
	Cp_fluid_TEST=3501.54 - 2.12091*T_K1 + 0.00807326*T_K1**2
	mu_fluid_TEST=29.2733 - 0.367285*T_K1 + 0.00172909*T_K1**2 - 
     .	0.00000361866*T_K1**3 + 2.83986E-09*T_K1**4

	elseif(TEST_cooling_fluid .EQ. 5) then  !25% Propylene Glycol
	rho_cool_fluid_TEST=814.76 + 1.75047*T_K1 - 0.00358803*T_K1**2
	Cp_fluid_TEST=10775.6 - 62.9957*T_K1 + 0.190454*T_K1**2 - 
     .	0.000186685*T_K1**3
	mu_fluid_TEST=8.22884 - 0.104386*T_K1 + 0.000496931*T_K1**2 - 
     .	0.00000105157*T_K1**3 + 8.34276E-10*T_K1**4

	else    !default....50% ethylene glycol
	mu_fluid_TEST=18.3853 - 0.238994*T_K1 + 0.00116489*T_K1**2 - 
     .	0.00000252199*T_K1**3 + 2.04562E-09*T_K1**4
	rho_cool_fluid_TEST=1026.4 + 0.80163*T_K1 - 0.00227397*T_K1**2
	Cp_fluid_TEST=1899.32 + 4.19104*T_K1 + 0.00194702*T_K1**2

	endif



!===========================================================================
	!fluid properties of cooling fluid

	T_tolerance = 1 !K   Difference between residual & cool fluid must 
	!						be less than the tolerance
	                 
	d_T = 1.0 !K    Increasing by larger value to be in middle of average
			!     cooling fluid temp
	T_difference = 200  !initialize
	T_cool_in = 300   !initialize
	T_cool_speed2 = T_cool_speed2+273.15  !convert C to K
	T_cool_speed3 = T_cool_speed3+273.15  !convert C to K

c	if (T_difference .ge. T_tolerance) then
		do 10  T_res = 260,600,d_T
		if (T_difference .ge. T_tolerance) then

!=============================
	!Determine the fan speed based on direct normal insolation

	if(T_cool_in .GE. T_cool_speed2) then    
		if(T_cool_in .LT. T_cool_speed3) then
		Fan_speed =  Fan_speed2
		else 
		Fan_speed =  Fan_speed3
		endif
	else
	Fan_speed =  Fan_speed1

	endif  !end of if statement

	!Set fan speed to zero if DNI < I_cut_in
	if(DNI .GE. I_cut_in) then    
		Fan_speed =  Fan_speed
		else 
		Fan_speed =  0
	endif


	!System does not converge with a fan speed below 50RPM so modify code
	!Output power is reduced in output below for fans speeds below 50RPM
	! to simulate natural convection of radiator
		Fan_speed_use = Fan_speed

		if(Fan_speed .LT. 50) then    
		Fan_speed = 50 
		endif


!=============================
	mu_water=3.50542 - 0.0539638*T_res + 0.000333345*T_res**2 - 
     .	0.0000010319*T_res**3 + 1.59983E-09*T_res**4 - 
     .     9.93386E-13*T_res**5

	if(Cooling_fluid .EQ. 1) then    !water
CNB changed this equation to match the one for water (not sure where this came from
C	mu_cool_fluid=0.0347763 - 0.000203407*T_res + 3.01332E-07*T_res**2
	mu_cool_fluid=3.50542 - 0.0539638*T_res + 0.000333345*T_res**2 - 
     .	0.0000010319*T_res**3 + 1.59983E-09*T_res**4 - 
     .     9.93386E-13*T_res**5

	rho_fluid=692.604 + 2.2832*T_res - 0.00423412*T_res**2
	Cp_fluid=2.14384E+06 - 34048.9*T_res + 216.467*T_res**2 - 
     .	0.687249*T_res**3 + 0.00108959*T_res**4 - 6.90127E-07*T_res**5

	elseif(Cooling_fluid .EQ. 2) then   !50% Ethylene Glycol
	mu_cool_fluid=18.3853 - 0.238994*T_res + 0.00116489*T_res**2 - 
     .	0.00000252199*T_res**3 + 2.04562E-09*T_res**4
	rho_fluid=1026.4 + 0.80163*T_res - 0.00227397*T_res**2
	Cp_fluid=1899.32 + 4.19104*T_res + 0.00194702*T_res**2

	elseif(Cooling_fluid .EQ. 3) then  !25% Ethylene Glycol
	mu_cool_fluid=5.33548 - 0.0686842*T_res + 0.000331949*T_res**2 - 
     .	7.13336E-07*T_res**3 + 5.74824E-10*T_res**4
	rho_fluid=823.536 + 1.76857*T_res - 0.00360812*T_res**2
	Cp_fluid=3056.35 + 3.04401*T_res - 0.00126969*T_res**2

	elseif(Cooling_fluid .EQ. 4) then  !50% Propylene Glycol
	rho_fluid=1032.38 + 0.658226*T_res - 0.00217079*T_res**2
	Cp_fluid=3501.54 - 2.12091*T_res + 0.00807326*T_res**2
	mu_cool_fluid=29.2733 - 0.367285*T_res + 0.00172909*T_res**2 - 
     .	0.00000361866*T_res**3 + 2.83986E-09*T_res**4

	elseif(Cooling_fluid .EQ. 5) then !25% Propylene Glycol
	rho_fluid=814.76 + 1.75047*T_res - 0.00358803*T_res**2
	Cp_fluid=10775.6 - 62.9957*T_res + 0.190454*T_res**2 - 
     .	0.000186685*T_res**3
	mu_cool_fluid=8.22884 - 0.104386*T_res + 0.000496931*T_res**2 - 
     .	0.00000105157*T_res**3 + 8.34276E-10*T_res**4

	else    !default....50% ethylene glycol
	mu_cool_fluid=18.3853 - 0.238994*T_res + 0.00116489*T_res**2 - 
     .	0.00000252199*T_res**3 + 2.04562E-09*T_res**4
	rho_fluid=1026.4 + 0.80163*T_res - 0.00227397*T_res**2
	Cp_fluid=1899.32 + 4.19104*T_res + 0.00194702*T_res**2

	endif



	!================================================================

	!Use fan laws to determine how fan parasitic power changes
	!& determine new air mass flow rate
	!C_W --> fan power coefficient
	
	TEST_fan_speed_rad = TEST_fan_speed*2*3.14159/60
	Fan_speed_rad = Fan_speed*2*3.14159/60 !rad/s

	C_W = TEST_P_fan / (TEST_fan_speed_rad**3*
     .	  TEST_fan_diameter**5*TEST_fan_rho_air+1E-8)

	!find fan power based on operating speed & air density

	P_fan = C_W*Fan_speed_rad**3*TEST_fan_diameter**5*rho_air
	
	!determine capacity coef C_V
	V_dot_air_TEST = TEST_fan_CFM*0.3048**3/60 !m^3/s !volume flow rate of air
	C_V=V_dot_air_TEST/(TEST_fan_speed_rad*TEST_fan_diameter**3+1E-8)
	!solve for new air volume flow rate
	V_dot_air=C_V*Fan_speed_rad*TEST_fan_diameter**3
	!determine new mass flow rate of air
	m_dot_air = V_dot_air*rho_air


	!=========================================================================
	!Determine pump (one-dish) power using dimentionless pump laws & viscosity conversion

	TEST_pump_speed_rad = TEST_pump_speed*2*3.14159/60
	pump_speed_rad = pump_speed*2*3.14159/60 !rad/s

	C_W_pump = TEST_P_pump / (TEST_pump_speed_rad**3*
     .	  TEST_pump_d_impeller**5*rho_cool_fluid_TEST+1E-8)

	!find pump power based on operating speed & cooling fluid density
	P_pump = C_W_pump*pump_speed_rad**3*
     .	    TEST_pump_d_impeller**5*rho_fluid

	!determine capacity coef C_V_pump

	C_V_pump=TEST_V_dot_fluid/(TEST_pump_speed_rad*
     .	     TEST_pump_d_impeller**3+1E-8)

	!solve for new cooling fluid volume flow rate
	V_dot_cool_fluid=C_V_pump*pump_speed_rad*TEST_pump_d_impeller**3

	!determine new mass flow rate of cooling fluid
	m_dot_cool_fluid = V_dot_cool_fluid*rho_fluid


!===============================================================================
	!capacitance, mass, and volumetric flow rates of H2, cooling fluid, & air

	engine_pressure_TEST = 15000000 !Pa
	frequency_TEST = 30 !1/s
	T_H2_ave_TEST = 600 !K
	Cp_H2_TEST = 14500

	!mass & capacitance rate of H2 in the engine
	R_gas = 8314 !J/kmol-K
	M_H2 = 2.016  !molar mass of H2
	T_H2_ave = (T_heater_head_low + T_amb)/2
	V_total = 2.5 * V_swept                            
	mass_H2  = engine_pressure * V_total*M_H2 /(R_gas*(T_H2_ave)+1E-8)
	mass_H2_TEST=engine_pressure_TEST * V_total*M_H2 /
     .	         (R_gas*(T_H2_ave_TEST)+1E-8)
	rho_H2 = engine_pressure *M_H2 /(R_gas*(T_H2_ave)+1E-8)
	rho_H2_TEST = engine_pressure_TEST /(R_gas*(T_H2_ave_TEST)+1E-8)  !at full load
	m_dot_H2_TEST = mass_H2_TEST * 2 * frequency_TEST	
	m_dot_H2 = mass_H2 * 2 * frequency	  
	Cp_H2 = 14500
	C_dot_H2 = m_dot_H2 * Cp_H2
	V_dot_H2 = m_dot_H2 / (rho_H2+1E-8)
	V_dot_H2_TEST = m_dot_H2_TEST / (rho_H2_TEST+1E-8)
	C_dot_H2_TEST = m_dot_H2_TEST * Cp_H2_TEST

	!capacitance rate of cooling fluid through radiator
	C_dot_cool_fluid = m_dot_cool_fluid * Cp_fluid  
	m_dot_cool_fluid_TEST = TEST_V_dot_fluid * rho_cool_fluid_TEST	
	C_dot_cool_fluid_TEST =  m_dot_cool_fluid_TEST * Cp_fluid_TEST

	!capacitance rate of air through radiator
	Cp_air_TEST = 1005   !mostly constant Cp for air
	m_dot_air_TEST = V_dot_air_TEST * TEST_fan_rho_air !mass flow rate of air 
	C_dot_air_TEST = m_dot_air_TEST * Cp_air_TEST
	Cp_air = 1005
	C_dot_air = m_dot_air * Cp_air	
c	m_dot_air_TEST = V_dot_air_TEST * TEST_fan_rho_air !mass flow rate of air 
 
	!Min and Max capacitance & volumetric flow rates
c	C_dot_min_radiator = MIN(C_dot_air, C_dot_cool_fluid)

	C_dot_min_TEST_rad = MIN(C_dot_air_TEST,C_dot_cool_fluid_TEST)
	C_dot_max_TEST_rad = MAX(C_dot_air_TEST,C_dot_cool_fluid_TEST)
	C_dot_min_rad =MIN(C_dot_air,C_dot_cool_fluid)
	C_dot_max_rad = MAX(C_dot_air,C_dot_cool_fluid)
	V_dot_min_rad = MIN(V_dot_air,V_dot_cool_fluid)  
	V_dot_min_TEST_rad = MIN(V_dot_air_TEST,TEST_V_dot_fluid)
	C_dot_min_TEST_cooler = MIN(C_dot_H2_TEST,C_dot_cool_fluid_TEST)
	C_dot_max_TEST_cooler = MAX(C_dot_H2_TEST,C_dot_cool_fluid_TEST)
	C_dot_min_cooler =MIN(C_dot_H2,C_dot_cool_fluid)
	C_dot_max_cooler = MAX(C_dot_H2,C_dot_cool_fluid)
	V_dot_min_cooler = MIN(V_dot_H2,V_dot_cool_fluid)
	V_dot_min_TEST_cooler = MIN(V_dot_H2_TEST,TEST_V_dot_fluid)


!===============================================================================
	!Determine effectiveness of the cooler and radiator for operating conditions


!==============================================================================
!Radiator effectiveness-NTU for determining radiator effectiveness with changing fan speed
!==============================================================================
	
	!log()  is the natural logarithm

	!Cross-flow NTU-effectiveness correlation
	Cr_radiator_TEST = C_dot_min_TEST_rad / C_dot_max_TEST_rad
	NTU_radiator_TEST = -log(1+(1/Cr_radiator_TEST)*
     .	(log(1-epsilon_radiator_TEST*Cr_radiator_TEST))) !solve for test NTU

	!determine overall heat xfer coef UA at test conditions
	UA_radiator_TEST = NTU_radiator_TEST * C_dot_min_TEST_rad  

	!determine overall heat xfer coef UA at operating conditions
	UA_radiator = UA_radiator_TEST * 
     .	         (V_dot_min_rad/V_dot_min_TEST_rad)**b_radiator

	!determine new NTU value based on new C_dot of air
	NTU_radiator = UA_radiator / (C_dot_min_rad+1E-8)  

	 !solve for new radiator effectiveness                           
	Cr_radiator=C_dot_min_rad / C_dot_max_rad
	EPSILON_radiator = (1/(Cr_radiator+1E-8))*(1-exp(-Cr_radiator*
     .	(1-exp(-NTU_radiator))))      

!=================================================================
	!Solve for cooling tower water temp into the tower
!=================================================================
	Tower_water_inlet_temp=Tower_water_outlet_temp + 
     .	Q_reject_total/(Cp_fluid * Tower_m_dot_water+1E-8)
	!average temp of cooling tower fluid
      Tower_water_ave=(Tower_water_inlet_temp+Tower_water_outlet_temp)/2

!===============================================================================
! Cooling Tower loop HX effectiveness-NTU for effectiveness with changing pump/tower flow
!===============================================================================
	T1 = Tower_water_outlet_temp+273.15	
	b_tower = 0.7
	rho_tower = 589.132 + 2.98577*T1 - 0.00542465*T1**2
	rho_tower_TEST = 1000	
	Cp_tower_water_TEST = 4185
	Cp_tower_water =5.64111E+06 - 93046.6*T1 + 614.276*T1**2 - 
     .     2.02742*T1**3 + 0.00334536*T1**4 - 0.00000220776*T1**5
	C_dot_tower_TEST = Tower_m_dot_water_TEST * Cp_tower_water_TEST
	C_dot_tower = Tower_m_dot_water * Cp_tower_water
	V_dot_tower = Tower_m_dot_water / (rho_tower+1E-8)
	V_dot_tower_TEST = Tower_m_dot_water_TEST/(rho_tower_TEST+1E-8)


	C_dot_min_TEST_tower = MIN(C_dot_tower_TEST,C_dot_cool_fluid_TEST)
	C_dot_max_TEST_tower = MAX(C_dot_tower_TEST,C_dot_cool_fluid_TEST)
	C_dot_min_tower =MIN(C_dot_tower,C_dot_cool_fluid)
	C_dot_max_tower = MAX(C_dot_tower,C_dot_cool_fluid)
	V_dot_min_tower = MIN(V_dot_tower,V_dot_cool_fluid)
	V_dot_min_TEST_tower = MIN(V_dot_tower_TEST,TEST_V_dot_fluid)


	!Counter-flow Concentric-Tube NTU-effectiveness correlation
	Cr_tower_TEST = C_dot_min_TEST_tower / C_dot_max_TEST_tower
	NTU_tower_TEST=1/(Cr_tower_TEST-1)*log((EPSILON_tower_TEST-1)/
     .	(EPSILON_tower_TEST*Cr_tower_TEST-1+1E-8))

	!determine overall heat xfer coef UA at test conditions
	UA_tower_TEST = NTU_tower_TEST * C_dot_min_TEST_tower 

	!determine overall heat xfer coef UA at operating conditions
	UA_tower = UA_tower_TEST *(V_dot_min_tower/ 
     .	         (V_dot_min_TEST_tower+1E-8))**b_tower

	!determine new NTU value based on new C_dot of the engine
	NTU_tower = UA_tower / (C_dot_min_tower+1E-8)	    
	
	!solve for new cooler effectiveness (counter-flow correlation)
	Cr_tower = C_dot_min_tower / C_dot_max_tower
	EPSILON_tower = (1-exp(-NTU_tower*(1-Cr_tower))) / 
     .	(1-Cr_tower*exp(-NTU_tower*(1-Cr_tower)))            


!===============================================================================
! Cooler effectiveness-NTU for determining cooler effectiveness with changing mass in engine
!===============================================================================

	!Counter-flow Concentric-Tube NTU-effectiveness correlation
	Cr_cooler_TEST = C_dot_min_TEST_cooler / C_dot_max_TEST_cooler
	NTU_cooler_TEST=1/(Cr_cooler_TEST-1)*log((EPSILON_Cooler_TEST-1)/
     .	(EPSILON_Cooler_TEST*Cr_cooler_TEST-1))

	!determine overall heat xfer coef UA at test conditions
	UA_cooler_TEST = NTU_cooler_TEST * C_dot_min_TEST_cooler  

	!determine overall heat xfer coef UA at operating conditions
	UA_cooler = UA_cooler_TEST * 
     .	     (V_dot_min_cooler/(V_dot_min_TEST_cooler+1E-8))**b_cooler

	!determine new NTU value based on new C_dot of the engine
	NTU_cooler = UA_cooler / (C_dot_min_cooler+1E-8)	    
	
	!solve for new cooler effectiveness (counter-flow correlation)
	Cr_cooler = C_dot_min_cooler / C_dot_max_cooler
	EPSILON_cooler = (1-exp(-NTU_cooler*(1-Cr_cooler))) / 
     .	(1-Cr_cooler*exp(-NTU_cooler*(1-Cr_cooler)))            


	!=================================================================
	!Determine compression space temperature

	if(Cooling_Tower_ON .EQ. 0) then  !No Tower
	!solve for cooling fluid temp out of cooler and into radiator
	T_cool_out = Q_losses/(EPSILON_radiator*C_dot_min_rad+1E-8)+T_amb
	!solve for cooling fluid temp in to cooler and out of radiator
	T_cool_in= -Q_losses/(C_dot_cool_fluid+1E-8)+T_cool_out !energy balance

	else  !Cooling Tower used
	!solve for cooling fluid temp out of cooler and into tower loop HX
	T_cool_out = Q_losses/(EPSILON_radiator*C_dot_min_rad+1E-8)+
     .	         (Tower_water_ave +273.15)
	!solve for cooling fluid temp in to cooler and out of tower loop HX
	T_cool_in= -Q_losses/(C_dot_cool_fluid+1E-8)+T_cool_out !energy balance
	endif

	T_cool_water_ave =  (T_cool_out + T_cool_in) / 2 
	C_dot_min_cooler = MIN(C_dot_H2, C_dot_cool_fluid)

	!solve for temp of hydrogen into the cooler"
	T_H2_in=Q_losses/(EPSILON_Cooler*C_dot_min_cooler)+T_cool_in

	!solve for temp of compression space"
	
	T_H2_out=T_H2_in-EPSILON_Cooler*C_dot_min_cooler/(C_dot_H2+1E-8)*
     .         (T_H2_in - T_cool_in)
	
	!solve for cold junction temp = compression space
	T_compression=T_H2_out

	!-------------------------
	
	T_difference = abs(T_res - T_cool_out)

		else 
			goto 20

		endif  !while loop solving for the cooling fluid temp

 10		continue   !while-loop is repeated

			
c	endif  !while loop solving for the cooling fluid temp

	!=================================================================
	!correct pump power based on viscosity of fluid
20	mu_div_mu_water = mu_cool_fluid / (mu_water+1E-8)
	pump_multiplier=1.01178 - 0.0117778*mu_div_mu_water

	P_pump = P_pump / (pump_multiplier+1E-8)

	!=================================================================
	! Set pump and control power to 0 when DNI < 1
	
	if(DNI .LT. 1) then    
		P_pump =  0
		P_controls =  0
	endif

	!=================================================================
	!Set individual system fan power to zero when cooling tower on
	if(Cooling_Tower_ON .EQ. 0) then
		P_fan = P_fan
	else
		P_fan = 0
	endif

	!=================================================================
	!Set cooling tower fan power to zero when DNI is low
	if(DNI .LT. 300) then
		P_tower_fan = 0
	else
		P_tower_fan = P_tower_fan
	endif


	!=========================================================================
	! Determine pump power for the cooling tower to distribute water to all dishes

	pipe_length = NS_dish_separation * Number_of_Collectors
	A_pipe = 3.14159*(d_pipe_tower/2)**2
	vel_tower = Tower_m_dot_water / (rho_tower * A_pipe+1E-8)
	mu_tower = 0.299062 - 0.00283786*T1 + 0.0000090396*T1**2 - 
     .	       9.64494E-09*T1**3
	Re_tower = rho_tower * vel_tower * d_pipe_tower / (mu_tower+1E-8)

	if(tower_pipe_material .EQ. 1) then    !plastic
		epsilon_wall = 0.0000015
	elseif(tower_pipe_material .EQ. 2) then  !cast iron
		epsilon_wall = 0.00026
	elseif(tower_pipe_material .EQ. 3) then  !riveted steel
		epsilon_wall = 0.003
	endif
	!Haaland eq......p366 White 2003
	friction_factor = 1/(-1.8*Log10(6.9/(Re_tower+1E-8)+(epsilon_wall/
     .	            (3.7*d_pipe_tower))**1.11))**2

	head_friction = friction_factor*pipe_length*vel_tower**2/
     .	            (d_pipe_tower*2*9.8)
	K_total = 0.001/39.37 !meters   p.387 White 2003 estimate for minor losses
	head_minor = K_total*(vel_tower**2/(2*9.8))
	head_total = head_friction + head_minor
	V_dot_tower = Tower_m_dot_water / (rho_tower+1E-8)
	P_tower_pump=rho_tower*9.8*V_dot_tower*(head_total)/
     .	(eta_tower_pump+1E-8)

	if(Cooling_Tower_ON .EQ. 0) then
	P_tower_pump = 0
	P_tower_fan = 0
	else
	P_tower_pump = P_tower_pump
	endif

	!=================================================================
	!Determine total parasitic power
	
	! Stirling dish system with a fan, pump and radiator
	P_parasitic = P_fan + P_pump + P_controls 

	!cooling tower with pump and fan
	if(Tower_mode .EQ. 1) then
	P_parasitic_tower = P_tower_pump  !natural convection
	else
	P_parasitic_tower = P_tower_pump + P_tower_fan   !forced draft
	endif



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

		
		net_power_out = gross_power - (P_parasitic/1000)

	SUM = Number_of_Collectors*net_power_out - P_parasitic_tower/1000  
	if(Tower_mode .EQ. 1) then
		if(SUM .LE. 0) then
		net_power_out = 0
		P_parasitic_tower = 0  
		else
		net_power_out = net_power_out 
		endif
	endif


	if(net_power_out .GE. 0) then    
		if(Fan_speed_use .GE. 50) then
		OUT(1)=net_power_out*system_availability
		else 
	!approx natural convection at very low fan speeds
		OUT(1)=net_power_out*system_availability   
     .		 * ((Fan_speed_use+0.5)/50)**0.5  
		endif
	else
	OUT(1) = 0
	endif
		
	!===========================
C		 P_parasitic
			OUT(2)= P_parasitic
	!===========================
C		 Cold sink temp...T_compression -->radiator or cooling Tower
		OUT(3)=T_compression 
	!===========================
C		 total_net_power including availability and # of collectors
!	if(net_power_out .GE. 0) then    
!		if(Fan_speed_use .GE. 50) then
!		OUT(4)=net_power_out*Number_of_Collectors*
 !    .		system_availability	- P_parasitic_tower /1000
!		else !approx natural convection in radiator
!		OUT(4)=net_power_out*((Fan_speed_use+0.5)/50)**0.5*
 !    .  Number_of_Collectors*system_availability-P_parasitic_tower /1000 
!		endif
!	else
!	OUT(4) = 0
!	endif

	!===========================
		OUT(4) = P_fan 
	!===========================
		OUT(5) = P_pump
	!===========================
		OUT(6) = Tower_water_inlet_temp
	!===========================
		Tower_m_dot_water = Tower_m_dot_water * 3600 !convert back to kg/hr
		OUT(7) = Tower_m_dot_water
	!===========================
		OUT(8) = T_amb_C
	!===========================	
c		OUT(9) = P_amb_atm
	!===========================
		OUT(9) = fan_control_signal
	!===========================
		OUT(10) = T_compression - 273.15   !Celcius
	!===========================
		OUT(11) = P_parasitic_tower
	!===========================
	! temp of radiator cooling fluid into radiator
		OUT(12) = T_cool_out - 273.15
	!===========================
	! temp of radiator cooling fluid out of radiator
		OUT(13) = T_cool_in - 273.15
	!===========================
	! Net system efficiency
		OUT(14) = OUT(1) / (P_in_collector+0.00000001)


C-----------------------------------------------------------------------------------------------------------------------
C    EVERYTHING IS DONE - RETURN FROM THIS SUBROUTINE AND MOVE ON
      RETURN 1
      END
C-----------------------------------------------------------------------------------------------------------------------
