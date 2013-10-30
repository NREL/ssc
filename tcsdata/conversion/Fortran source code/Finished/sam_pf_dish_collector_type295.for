			SUBROUTINE TYPE295 (TIME,XIN,OUT,T,DTDT,PAR,INFO,ICNTRL,*) 
C************************************************************************
C  COPYRIGHT 2008 NATIONAL RENEWABLE ENERGY LABORATORY
C Object: Concentrator Dish
C Simulation Studio Model: 11_1_07_Collector
C 
C Author: Paul R. Fraser
C Editor: Paul R. Fraser
C Date:	 May 31, 2007 last modified: May 13, 2008
C 
C 
! Doc. tables updated 7/1/2010 - MJW
!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Parameters
!    1| d_ap                             | Dish aperture diameter                                            | m                | m                
!    2| Reflectivity                     | Mirror surface reflectivity                                       | none             | none             
!    3| NNS                              | Number of collectors North-South                                  | none             | none             
!    4| NEW                              | Number of collectors East-West                                    | none             | none             
!    5| NS_dish_seperation               | Collector separation North-Sourth                                 | m                | m                
!    6| EW_dish_seperation               | Collector separation East-West                                    | m                | m                
!    7| slope_NS                         | North-South ground slope                                          | %                | m                
!    8| slope_EW                         | East-West ground slope                                            | %                | m                
!    9| width_slot_gap                   | Slot gap width                                                    | none             | none             
!   10| height_slot_gap                  | Slot gap height                                                   | none             | none             
!   11| manufacturer                     | Dish manufacturer (fixed as 5="other")                            | none             | none             
!   12| Wind_Stow_Speed                  | Wind stow speed                                                   | m/s              | m/s              
!   13| Projected_Area                   | Projected mirror area                                             | m2               | m2               
!   14| I_cut_in                         | Insolation cut in value                                           | W/m2             | W/m2             
!   15| d_ap_TEST                        | Receiver aperture diameter during test                            | m                | m                
!   16| TEST_intercept_factor            | Test intercept factor                                             | none             | none             
!   17| TEST_focal_length                | Focal length of mirror system                                     | m                | m                
!   18| Total_Area                       | Total surface area of the parabolic collector                     | m^2              | m^2              

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Inputs
!    1| I_beam_in                        | Direct normal radiation (not interpolated)                        | kJ/hr.m2         | kJ/hr.m2         
!    2| T_amb_in                         | Dry bulb temperature                                              | C                | C                
!    3| wind_speed_in                    | Wind velocity                                                     | m/s              | m/s              
!    4| sun_angle_in                     | Solar zenith angle                                                | deg              | rad              
!    5| P_atm_in                         | Atmospheric pressure                                              | Pa               | Pa               
!    6| solar_azimuth                    | Solar azimuth angle                                               | deg              | rad              

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Outputs
!    1| Power_out_col                    | Total power from the collector dish                               | kW               | kW               
!    2| T_amb_out                        | Ambient temperature in Kelvin                                     | K                | C                
!    3| P_atm_out                        | Atmospheric pressure                                              | Pa               | Pa               
!    4| wind_speed_out                   | Wind velocity                                                     | m/s              | m/s              
!    5| sun_angle_out                    | Solar altitude angle                                              | deg              | rad              
!    6| Collector_Losses                 | Total collector losses (Incident minus output power)              | kW               | kW               
!    7| eta_Collector                    | Collector efficiency                                              | none             | none             
!    8| Number_of_Collectors             | Total number of collectors (Num N-S x Num E-W)                    | none             | none             
!    9| DNI                              | Direct normal radiation (not interpolated)                        | W/m2             | W/m2             
!   10| I_cut_in                         | The cut-in DNI value used in the simulation                       | W/m2             | W/m2             
!   11| Power_in_Receiver                | Power entering the receiver from the collector                    | kW               | kW               
!   12| intercept_factor                 | The receiver intercept factor                                     | none             | none             
!   13| d_ap                             | The aperture diameter used in the simulation                      | m                | m                
!   14| Power_in_Collector               | Power incident on the collector                                   | kW               | kW               
!   15| NS_dish_seperation               | North-South dish separation used in the simulation                | m                | m                
!   16| EW_dish_seperation               | East-West dish separation used in the simulation                  | m                | m                
!   17| phi_shade                        | Dish-to-Dish shading performance factor                           | none             | none             


C (Comments and routine interface generated by TRNSYS Studio)
C************************************************************************

C    TRNSYS acess functions (allow to acess TIME etc.) 
      USE TrnsysConstants
      USE TrnsysFunctions

C-----------------------------------------------------------------------------------------------------------------------
C    REQUIRED BY THE MULTI-DLL VERSION OF TRNSYS
      !DEC$ATTRIBUTES DLLEXPORT :: TYPE295				!SET THE CORRECT TYPE NUMBER HERE
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
      PARAMETER (NP=18,NI=6,NOUT=17,ND=0,NSTORED=0)
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
      DOUBLE PRECISION Reflectivity
      DOUBLE PRECISION Number_of_Collectors
	DOUBLE PRECISION NS_dish_separation
	DOUBLE PRECISION EW_dish_separation
	DOUBLE PRECISION manufacturer
      DOUBLE PRECISION Projected_Area
      DOUBLE PRECISION Wind_Stow_Speed
      DOUBLE PRECISION I_cut_in
	DOUBLE PRECISION Total_Area  
	DOUBLE PRECISION d_ap		
	DOUBLE PRECISION d_ap_TEST	
	DOUBLE PRECISION TEST_intercept_factor  
	DOUBLE PRECISION TEST_focal_length


C    INPUTS
      DOUBLE PRECISION I_beam_in
      DOUBLE PRECISION T_amb_in
      DOUBLE PRECISION wind_speed_in
      DOUBLE PRECISION sun_angle_in
      DOUBLE PRECISION P_atm_in

C    Variables listed below

      DOUBLE PRECISION psi_rim
	DOUBLE PRECISION Power_tot
      DOUBLE PRECISION d_psi
      DOUBLE PRECISION Ib
      DOUBLE PRECISION intercept_factor_solve
      DOUBLE PRECISION sigma_tot_guess
	DOUBLE PRECISION h
      DOUBLE PRECISION d_collector
	DOUBLE PRECISION pi 
	DOUBLE PRECISION r 
	DOUBLE PRECISION b1 
	DOUBLE PRECISION b2 
	DOUBLE PRECISION b3 
	DOUBLE PRECISION b4 
	DOUBLE PRECISION b5 
	DOUBLE PRECISION x 
	DOUBLE PRECISION fx 
	DOUBLE PRECISION Q 
	DOUBLE PRECISION gamma 
	DOUBLE PRECISION A_total
	DOUBLE PRECISION psi
	DOUBLE PRECISION TEST_ap_diameter
	DOUBLE PRECISION omega_n_accurate
	DOUBLE PRECISION DELTAr_accurate
	DOUBLE PRECISION p
	DOUBLE PRECISION n_std_dev
	DOUBLE PRECISION d_Power
	DOUBLE PRECISION d_Power_intercept
	DOUBLE PRECISION Power_int_tot
	DOUBLE PRECISION sigma_tot
	DOUBLE PRECISION intercept_factor
	DOUBLE PRECISION t1
	DOUBLE PRECISION DNI	
	DOUBLE PRECISION Power_out_col
	DOUBLE PRECISION T_amb_out
	DOUBLE PRECISION P_atm_out
	DOUBLE PRECISION wind_speed_out
	DOUBLE PRECISION sun_angle_out
	DOUBLE PRECISION Collector_Losses
	DOUBLE PRECISION eta_Collector
	DOUBLE PRECISION NEW
	DOUBLE PRECISION NNS
	DOUBLE PRECISION A_collector
	DOUBLE PRECISION A_collector_tot
	DOUBLE PRECISION l_NS
	DOUBLE PRECISION l_EW
	DOUBLE PRECISION LNS
	DOUBLE PRECISION LEW
	DOUBLE PRECISION x_2_3 
	DOUBLE PRECISION azimuth_angle
	DOUBLE PRECISION y_2_3
	DOUBLE PRECISION x_prime_2_3
	DOUBLE PRECISION elevation_angle
	DOUBLE PRECISION d_2_3
	DOUBLE PRECISION theta_2
	DOUBLE PRECISION A_2_3
	DOUBLE PRECISION d_4_3
	DOUBLE PRECISION theta_1
	DOUBLE PRECISION A_4_3
	DOUBLE PRECISION l_d
	DOUBLE PRECISION alpha
	DOUBLE PRECISION BETA
	DOUBLE PRECISION x_1_3
	DOUBLE PRECISION solar_azimuth
	DOUBLE PRECISION y_1_3
	DOUBLE PRECISION x_prime_1_3
	DOUBLE PRECISION d_1_3
	DOUBLE PRECISION theta_3
	DOUBLE PRECISION A_1_3
	DOUBLE PRECISION d_1_2
	DOUBLE PRECISION xi_1
	DOUBLE PRECISION xi_2
	DOUBLE PRECISION xi_3
	DOUBLE PRECISION gamma_1
	DOUBLE PRECISION gamma_2
	DOUBLE PRECISION gamma_3
	DOUBLE PRECISION S1
	DOUBLE PRECISION S2
	DOUBLE PRECISION S3
	DOUBLE PRECISION B
	DOUBLE PRECISION EQ
	DOUBLE PRECISION A_tri
	DOUBLE PRECISION A_1_2_3
	DOUBLE PRECISION d_1_4
	DOUBLE PRECISION xi_1b
	DOUBLE PRECISION xi_2b
	DOUBLE PRECISION xi_3b
	DOUBLE PRECISION theta_1b
	DOUBLE PRECISION theta_2b
	DOUBLE PRECISION theta_3b
	DOUBLE PRECISION gamma_1b
	DOUBLE PRECISION gamma_2b
	DOUBLE PRECISION gamma_3b
	DOUBLE PRECISION S1b
	DOUBLE PRECISION S2b
	DOUBLE PRECISION S3b
	DOUBLE PRECISION b1b
	DOUBLE PRECISION b2b
	DOUBLE PRECISION b3b
	DOUBLE PRECISION Bb
	DOUBLE PRECISION EQb
	DOUBLE PRECISION A_tri_b
	DOUBLE PRECISION A_1_4_3
	DOUBLE PRECISION SP
	DOUBLE PRECISION A_shade_interior
	DOUBLE PRECISION A_shade_exterior
	DOUBLE PRECISION Shade_AVG
	DOUBLE PRECISION zero
	DOUBLE PRECISION phi_shade
	DOUBLE PRECISION N_rect
	DOUBLE PRECISION phi_A
	DOUBLE PRECISION phi_E
	DOUBLE PRECISION x_mirror_gap
	DOUBLE PRECISION H_mirror_gap
	DOUBLE PRECISION D_dish
	DOUBLE PRECISION r_dish
	DOUBLE PRECISION w_rect
	DOUBLE PRECISION x_A
	DOUBLE PRECISION y_B
	DOUBLE PRECISION H_sun
	DOUBLE PRECISION x_dish_2
	DOUBLE PRECISION x_A_EW
	DOUBLE PRECISION y_B_EW
	DOUBLE PRECISION H_sun_EW
	DOUBLE PRECISION x_dish_2_EW
	DOUBLE PRECISION N_loop
	DOUBLE PRECISION N_loop_1
	DOUBLE PRECISION N_loop_2
	DOUBLE PRECISION one
	DOUBLE PRECISION x_dish_1
	DOUBLE PRECISION NS_shade
	DOUBLE PRECISION x_shade
	DOUBLE PRECISION H_dish_1
	DOUBLE PRECISION H_dish_2
	DOUBLE PRECISION y_shade
	DOUBLE PRECISION H_dish_2_EW
	DOUBLE PRECISION y_shade_EW
	DOUBLE PRECISION EW_shade
	DOUBLE PRECISION A_shade_NS
	DOUBLE PRECISION A_shade_EW
	DOUBLE PRECISION A_shade_tot
	DOUBLE PRECISION A_shade_combined 
	DOUBLE PRECISION phi_diag
	DOUBLE PRECISION phi_diag_pt
	DOUBLE PRECISION x_A_diag
	DOUBLE PRECISION y_B_diag
	DOUBLE PRECISION H_sun_diag
	DOUBLE PRECISION A_shade_diag
	DOUBLE PRECISION diag_shade
	DOUBLE PRECISION x_dish_2_diag
	DOUBLE PRECISION H_dish_2_diag
	DOUBLE PRECISION y_shade_diag
	DOUBLE PRECISION slope_EW
	DOUBLE PRECISION slope_NS 
	DOUBLE PRECISION rise_EW
	DOUBLE PRECISION rise_NS
	DOUBLE PRECISION phi_slope_EW
	DOUBLE PRECISION phi_slope_NS
	DOUBLE PRECISION slope_diag
	DOUBLE PRECISION phi_E_EW
	DOUBLE PRECISION rise_diag
	DOUBLE PRECISION phi_slope_diag
	DOUBLE PRECISION phi_E_NS
	DOUBLE PRECISION phi_E_diag
	DOUBLE PRECISION width_slot_gap
	DOUBLE PRECISION height_slot_gap
	DOUBLE PRECISION aperture_diameter


C-----------------------------------------------------------------------------------------------------------------------
C       READ IN THE VALUES OF THE PARAMETERS IN SEQUENTIAL ORDER
 
	d_ap=PAR(1)
	Reflectivity=PAR(2)
	NNS= PAR(3)
	NEW = PAR(4)
	NS_dish_separation=PAR(5)
	EW_dish_separation=PAR(6)
c	manufacturer=PAR(7)
c	Wind_Stow_Speed=PAR(8)
c      Projected_Area=PAR(9)
c      I_cut_in=PAR(10)
c	d_ap_TEST=PAR(11)
c	TEST_intercept_factor=PAR(12)
c	TEST_focal_length=PAR(13)
c	Total_Area=PAR(14)
	slope_NS = PAR(7) !new deg?
	slope_EW = PAR(8) !new deg? 
	width_slot_gap = PAR(9) !new  m?
	height_slot_gap = PAR(10) ! new m?
	manufacturer=PAR(11) !formerly 7
	Wind_Stow_Speed=PAR(12) !formerly 8
      Projected_Area=PAR(13) !formerly 9
      I_cut_in=PAR(14) !formerly 10
	d_ap_TEST=PAR(15) !formerly 11
	TEST_intercept_factor=PAR(16) !formerly 12
	TEST_focal_length=PAR(17) !formerly 13
	Total_Area=PAR(18) !formerly 14


C-----------------------------------------------------------------------------------------------------------------------
C    RETRIEVE THE CURRENT VALUES OF THE INPUTS TO THIS MODEL FROM THE XIN ARRAY IN SEQUENTIAL ORDER

      I_beam_in=XIN(1)
      T_amb_in=XIN(2) 
      wind_speed_in=XIN(3)
      sun_angle_in=XIN(4)
      P_atm_in=XIN(5)
	solar_azimuth=XIN(6)
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



C-----------------------------------------------------------------
C	This code inputs the manufacturer specific design details
C		
C------------------------------------------------------------------

	! NEED to change values after the initial time step below also

	P_atm_in = P_atm_in*101325  !convert atmospheres to Pa

	if(manufacturer .EQ. 1) then    !SES System = 1
		Projected_Area=87.7  !Projected area in plane of collector aperture
		Wind_Stow_Speed=16   !wind speed when system stops producing power
		I_cut_in=200	     !value when system produces power
		d_ap_TEST=0.184 !receiver aperture diameter during test int factor
		TEST_intercept_factor=0.995  !test intercept factor 
		TEST_focal_length=7.45      !focal length of mirror system
		Total_Area=91.0  !total surface area of parabolic collector

	elseif(manufacturer .EQ. 2) then    !WGA System = 2
		Projected_Area=41.2
		Wind_Stow_Speed=16
		I_cut_in=275
		d_ap_TEST=0.14	
		TEST_intercept_factor=0.998  
		TEST_focal_length=5.45
		Total_Area=42.9  
			

     	elseif(manufacturer .EQ. 3) then    !SBP System = 3
		Projected_Area=56.7
		Wind_Stow_Speed=16
		I_cut_in=250
		d_ap_TEST=0.15	
		TEST_intercept_factor=0.93  
		TEST_focal_length=4.5
		Total_Area=60 

	elseif(manufacturer .EQ. 4) then    !SAIC System = 4
		Projected_Area=113.5
		Wind_Stow_Speed=16
		I_cut_in=375
		d_ap_TEST=0.38	
		TEST_intercept_factor=0.90  
		TEST_focal_length=12.0
		Total_Area=117.2  	

	elseif(manufacturer .EQ. 5) then    !User inputs values = 5
		Projected_Area=Projected_Area
		Wind_Stow_Speed=Wind_Stow_Speed
		I_cut_in=I_cut_in	
		d_ap_TEST=d_ap_TEST	
		TEST_intercept_factor=TEST_intercept_factor  
		TEST_focal_length=TEST_focal_length
		Total_Area=Total_Area  
	

	endif  !end of if statement





C--------------------------------------
C		Adding code to solve for intercept factor here to reduce time
C		The intercept factor only needs to be solved for the first time step
C		Theory comes from Stine and Harrigan (1985)
C--------------------------------------


	!Constants used to solve for gamma below
			pi = 3.14159
			r = 0.2316419
			b1 = 0.319381530
			b2 = -0.356563782
			b3 = 1.781477937
			b4 = -1.82125978
			b5 = 1.330274429


	! Collector diameter
		d_collector = 2*(Total_Area/(pi+0.0000001))**0.5
	! Height of parabola for a parabolic concentrator
		h = d_collector**2 / (16*TEST_focal_length+0.0000001)
	! Initial guess value for the collector tracking and manufacturer error [mrad]
		sigma_tot_guess = 0.001
	! Initial value for intercept_factor_solve
		intercept_factor_solve = 1.001
	! beam insolation value used for intercept factor....value doesn't matter
		Ib = 1000
	! step used for incrementing the rim angle (differential ring)
		d_psi = 0.001 !radians
	!initializing intial power from collector
		Power_tot = 0
	    Power_int_tot = 0


	! 1) solve for rim angle
		psi_rim = ATAN(1/(0.0000001+(d_collector/
     .		(8*h+ 0.0000001)-(2*h/(d_collector+0.0000001)))))

	! 2) Perform while loop to solve for collector error until appropriate intercept value is achieved
	
10		if(intercept_factor_solve .ge. TEST_intercept_factor) then

	! 3) Perform do (for) loop to solve for values for each rim angle and sum them

			sigma_tot_guess = sigma_tot_guess + 0.000015
			
			Power_tot = 0  !need to re-initialize to 0
			Power_int_tot = 0  !need to re-initialize to 0

			do 20 psi = 0,psi_rim,d_psi
			
			
	! 4) solve for n_std_dev (# of standard deviations) for a specific rim angle 

			omega_n_accurate = d_ap_TEST

		!p is the distance from point on parabolic reflector to focal point p.184 Stine"            	
			DELTAr_accurate = omega_n_accurate * cos(psi)
			p = 2*TEST_focal_length/(1+cos(psi)+ 0.0000001)
			n_std_dev=(2/sigma_tot_guess) * 
     .		        ATAN(DELTAr_accurate/(2*p+ 0.0000001))

	! 5) solve for gamma (intercept factor) for a specific rim angle
	!gamma is highest at the mirror vertex and decreases near the collector perimeter
			
			x = n_std_dev/2 
			t1 = 1/(1+r*x+0.0000001)
			fx = 1/(SQRT(2*pi)+0.0000001)*EXP(-(x**2/2)+ 0.0000001)
			Q = fx*(b1*t1 + b2*t1**2 + b3*t1**3 + b4*t1**4 + b5*t1**5)
			gamma = 1 - 2*Q  
	

	! 6) solve for d_Power (total power reflected from differential ring) for spec rim angle
	d_Power=(8*pi*Ib*TEST_focal_length**2*sin(psi)
     .	     *d_psi)/(1+cos(psi)+ 0.0000001)**2
	
	! 7) solve for d_Power_intercept (total power from diff ring intercepted by receiver) for spec rim angle
	!may be more accurate raising gamma to power of 2-->4
	d_Power_intercept = d_Power * gamma**4  
	
	! 8) sum up d_Power & d_Power_intercept until rim angle reached

	Power_tot = Power_tot + d_Power
	Power_int_tot = Power_int_tot + d_Power_intercept


20			continue   !do-loop is repeated


	! 9) Divide sum of d_Power_intercept by d_Power to obtain intercept_factor_solve

	intercept_factor_solve = Power_int_tot / (Power_tot+ 0.0000001)


	! 10) If intercept_factor_solve is greater than TEST_intercept_factor repeat using larger sigma_tot_guess


		goto 10    !loop back to intercept factor (while) loop
		endif  !end of intercept factor (while) loop


	!==============================================================================
	!solve for intercept factor when changing the aperture diameter
	!==============================================================================


	sigma_tot = sigma_tot_guess   !error in collector solved above in loops
	

	!re-initialize values
	Power_tot = 0
	Power_int_tot = 0


	if (d_ap .EQ. d_ap_TEST) then
		intercept_factor = TEST_intercept_factor
		else
		
		! 2) Perform while loop to solve for collector error until appropriate intercept value is achieved

	! 3) Perform do (for) loop to solve for values for each rim angle and sum them

			do 40 psi = 0,psi_rim,d_psi
			
	! 4) solve for n_std_dev (# of standard deviations) for a specific rim angle 

			omega_n_accurate = d_ap

		!p is the distance from point on parabolic reflector to focal point p.184 Stine            	
			DELTAr_accurate = omega_n_accurate * cos(psi)
			p = 2*TEST_focal_length/(1+cos(psi)+ 0.0000001)
			n_std_dev=(2/sigma_tot) * 
     .		        ATAN(DELTAr_accurate/(2*p+ 0.0000001))


	! 5) solve for gamma (intercept factor) for a specific rim angle
	!gamma is highest at the mirror vertex and decreases near the collector perimeter
		
			x = n_std_dev/2 
			t1 = 1/(1+r*x+0.0000001)
			fx = 1/(SQRT(2*pi)+0.0000001)*EXP(-(x**2/2)+ 0.0000001)
			Q = fx*(b1*t1 + b2*t1**2 + b3*t1**3 + b4*t1**4 + b5*t1**5)
			gamma = 1 - 2*Q  

	! 6) solve for d_Power (total power reflected from differential ring) for spec rim angle
			d_Power=(8*pi*Ib*TEST_focal_length**2*sin(psi)
     .	    *d_psi)/(1+cos(psi)+ 0.0000001)**2
	
	! 7) solve for d_Power_intercept (total power from diff ring intercepted by receiver) for spec rim angle
	!d_Power_intercept may be more accurate raising gamma to power of 2-->4
			d_Power_intercept = d_Power * gamma**4  
	
	! 8) sum up d_Power & d_Power_intercept until rim angle reached

			Power_tot = Power_tot + d_Power
			Power_int_tot = Power_int_tot + d_Power_intercept


40			continue   !do-loop is repeated


	! 9) Divide sum of d_Power_intercept by d_Power to obtain intercept_factor_solve

		intercept_factor = Power_int_tot / (Power_tot+ 0.0000001)

	endif 



C--------------------------------------

C       PERFORM ANY REQUIRED CALCULATIONS TO SET THE INITIAL VALUES OF THE OUTPUTS HERE
C		 Power_out_col
			OUT(1)=0    
C		 T_amb_out
			OUT(2)=0     
C		 P_atm_out
			OUT(3)= 0     
C		 wind_speed_out
			OUT(4)=0      
C		 sun_angle_out
			OUT(5)= 0        
C		 Collector_Losses
			OUT(6)=0        
C		 eta_Collector
			OUT(7)=0      
C		 Number_of_Collectors
			OUT(8)=0
C		 DNI
			OUT(9)=0
C		I_cut_in
			OUT(10)=0
C		Power_in_Receiver   kW [0;+Inf]
			OUT(11)=0
C		intercept_factor   - [-Inf;+Inf]
			OUT(12)=0
C		d_ap
			OUT(13) = d_ap
C		Power_in_Collector   kW [0;+Inf]
			OUT(14)=0
C		NS_dish_seperation     m   [0;+Inf]
			OUT(15)=0
C		EW_dish_seperation     m   [0;+Inf]
			OUT(16)=0



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



c	aperture_diameter = d_ap*1000

	!================================================================
c	intercept_factor=0.167333 + 0.0155*aperture_diameter - 
c     .	0.0000733333*aperture_diameter**2 + 0.00128571*wind_speed_in - 
c     . (0.00022449*wind_speed_in**2)*(95/aperture_diameter)**3


	!================================================================


	x_mirror_gap = width_slot_gap 
	H_mirror_gap = height_slot_gap  
	
c	if (intercept_factor .LT. 0.00001) then
c	intercept_factor = 0.98 !TEST_intercept_factor
c	endif


	DNI = I_beam_in*1000/3600   !Direct normal insolation [W/m^2]
	P_atm_in = P_atm_in*101325  !convert atmospheres to Pa

	if(manufacturer .EQ. 1) then    !SES System = 1
		Projected_Area=87.7
		Wind_Stow_Speed=16
		I_cut_in=200	
		d_ap_TEST=0.184	
		TEST_intercept_factor=0.995  
		TEST_focal_length=7.45
		Total_Area=91.0  !total surface area of parabolic collector

	elseif(manufacturer .EQ. 2) then    !WGA System = 2
		Projected_Area=41.2
		Wind_Stow_Speed=16
		I_cut_in=275
		d_ap_TEST=0.14	
		TEST_intercept_factor=0.998  
		TEST_focal_length=5.45
		Total_Area=42.9  		

     	elseif(manufacturer .EQ. 3) then    !SBP System = 3
		Projected_Area=56.7
		Wind_Stow_Speed=16
		I_cut_in=250
		d_ap_TEST=0.15	
		TEST_intercept_factor=0.93  
		TEST_focal_length=4.5
		Total_Area=60 

	elseif(manufacturer .EQ. 4) then    !SAIC System = 4
		Projected_Area=113.5
		Wind_Stow_Speed=16
		I_cut_in=375
		d_ap_TEST=0.38	
		TEST_intercept_factor=0.90  
		TEST_focal_length=12.0
		Total_Area=117.2  	

	elseif(manufacturer .EQ. 5) then    !User inputs values = 5
		Projected_Area=Projected_Area
		Wind_Stow_Speed=Wind_Stow_Speed
		I_cut_in=I_cut_in	
		d_ap_TEST=d_ap_TEST	
		TEST_intercept_factor=TEST_intercept_factor  
		TEST_focal_length=TEST_focal_length
		Total_Area=Total_Area  
	

	endif  !end of if statement



!==============================================================================
! Paul Fraser Collector Array Shading Code 2008
!==============================================================================

	!elevation angle referenced from the horizon
	elevation_angle = (90-sun_angle_in)*2*3.142/360 !convert to radians

C	azimuth_angle   referenced from the South
	azimuth_angle = ((solar_azimuth)*2*3.142/360) !convert to radians
           
	!d_collector:  above	
	!NNS  number dishes North-South
	!NEW  number of dishes East-West

	zero = 0
	Number_of_Collectors = NEW*NNS
	N_rect = 99 ![-]   number of rectangles dish is broken up into
	phi_A = azimuth_angle      ![rad]   } 	"azimuth angle"
	phi_E = elevation_angle         ![rad]}	"elevation angle"

	L_NS = NS_dish_separation        !	"dish separation distance N-S"
	L_EW = EW_dish_separation   !
	D_dish = d_collector            !diameter of dish"
	r_dish = D_dish / 2       	!"radius of dish"
	w_rect = D_dish / N_rect	!"width of differential rectangle for shading"

	rise_NS = 0.01*slope_NS*L_NS  !distance the dish moves vertically based on slope
	if (phi_A .LT. 0) then
	rise_EW = 0.01*slope_EW*L_EW
	else
	rise_EW = -0.01*slope_EW*L_EW
	endif


	if (phi_A .LE. 0) then
		slope_diag = (rise_NS + rise_EW) / (L_NS**2+L_EW**2)**0.5
		else
		slope_diag = (rise_NS - rise_EW) / (L_NS**2+L_EW**2)**0.5
	endif

	rise_diag = 0.01*slope_diag*(L_NS**2+L_EW**2)**0.5
	phi_slope_diag = atan(rise_diag/(L_NS**2+L_EW**2)**0.5)
	

	!"N-S shade"
	x_A = sin(phi_A) * L_NS	!"distance shading line from south dish is offset from center of north dish x-direction"
	y_B = (L_NS**2 - x_A**2)**0.5	!"distance shading line from south dish is offset from center of north dish y-direction"

	!"E-W shade"

	x_A_EW = sin(pi/2 - phi_A) * L_EW	!"distance shading line from south dish is offset from center of north dish x-direction"
	y_B_EW = (L_EW**2 - x_A_EW**2)**0.5	!"distance shading line from south dish is offset from center of north dish y-direction"

	!"Diagonal shade"
	
	phi_diag = atan(L_NS/L_EW)
	phi_diag_pt = pi/2 - ABS(phi_A) - phi_diag
	x_A_diag = sin(phi_diag_pt) * (L_EW**2+L_NS**2)**0.5
	y_B_diag = (((L_EW**2+L_NS**2)**0.5)**2 - x_A_diag**2)**0.5

c ==================="
	N_loop_1 = (-N_rect+1) / 2	!"# of rectangles to the left of center......loop starts at center of differential shading rectangle at the left of the first dish"
	N_loop_2 = (N_rect -1) / 2	!"# of rectangles to the right of center......loop ends at center of differential shading rectangle at the right of the first dish"
	one = 1
	A_shade_NS = 0    !initialize N-S shading
	A_shade_EW = 0     !initialize E-W shading
	A_shade_diag = 0
	NS_shade = 0       !initialize
	EW_shade = 0       !initialize
	diag_shade = 0 

		do 50 N_loop = N_loop_1,N_loop_2,one


	x_dish_1 = N_loop * w_rect	!position of differential rectangle on x-axis of dish 1 (south)
	x_dish_2 = N_loop * w_rect - x_A      !"position of differential rectangle on x-axis of dish 2 (north) that the center of dish 1 projects onto"
	x_dish_2_EW = N_loop * w_rect - x_A_EW    !point on dish 2 that the pt x_dish_1 on dish 1 projects to
	x_dish_2_diag = N_loop * w_rect - x_A_diag

c ========================="

	if (x_dish_2 .LT. -r_dish) then
	NS_shade = 0 ![m^2]
	endif

	if (x_dish_2 .GT. r_dish) then
	NS_shade = 0 ![m^2]
	endif

	if (x_dish_2_EW .LT. -r_dish) then
	EW_shade = 0 ![m^2]
	endif

	if (x_dish_2_EW .GT. r_dish) then
	EW_shade = 0 ![m^2]
	endif

	if (x_dish_2_diag .LT. -r_dish) then
	diag_shade = 0 ![m^2]
	endif

	if (x_dish_2_diag .GT. r_dish) then
	diag_shade = 0 ![m^2]
	endif

c========================="

	!Majority of dish outside of center"

	!N-S shade"
	if (x_dish_2 .GE. -r_dish) then		
		if (x_dish_2 .LE. r_dish) then
		x_shade = w_rect
		H_dish_1 = (r_dish**2 - x_dish_1**2)**0.5	!height (radius) between center of each differential rectangle on dish 1 and perimeter of dish 1" 
		H_dish_2 = (r_dish**2 - x_dish_2**2)**0.5	!"height (radius) between center of differential rectangle on dish 2 (point projected from dish 1) and perimeter of dish 2" 		
		
		y_shade = -(tan(phi_E)*y_B) + H_dish_1 + H_dish_2-rise_NS
		if (y_shade .LE. 0) then
		y_shade = 0
		endif
		if (y_shade .GT. 2*H_dish_2) then
		y_shade = 2*H_dish_2
		endif
		NS_shade = x_shade * y_shade
		endif
	endif



	!E-W shade
	if (x_dish_2_EW .GE. -r_dish) then
		if (x_dish_2_EW .LE. r_dish) then
	H_dish_2_EW = (r_dish**2 - x_dish_2_EW**2)**0.5  !height (radius) between center of differential rectangle on dish 2 (point projected from dish 1) and perimeter of dish 2"

	y_shade_EW = -(tan(phi_E)*y_B_EW) + H_dish_1 + H_dish_2_EW-rise_EW
	x_shade = w_rect	
		if (y_shade_EW .LE. 0) then
		y_shade_EW = 0
		endif
		if (y_shade_EW .GT. 2*H_dish_2_EW) then
		y_shade_EW = 2*H_dish_2_EW
		endif
	EW_shade = x_shade * y_shade_EW
		endif
	endif



	!Diagional shade
	if (x_dish_2_diag .GE. -r_dish) then
		if (x_dish_2_diag .LE. r_dish) then
	H_dish_2_diag = (r_dish**2 - x_dish_2_diag**2)**0.5  !height (radius) between center of differential rectangle on dish 2 (point projected from dish 1) and perimeter of dish 2"

	y_shade_diag=-(tan(phi_E)*y_B_diag)+H_dish_1+H_dish_2_diag-
     .	rise_diag
	x_shade = w_rect	
		if (y_shade_diag .LE. 0) then
		y_shade_diag = 0
		endif
		if (y_shade_diag .GT. 2*H_dish_2_diag) then
		y_shade_diag = 2*H_dish_2_diag
		endif
		diag_shade = x_shade * y_shade_diag
		endif
	endif




c ========================="
	!Dish center"

	!"N-S shade"
	if (x_dish_2 .GE. (-x_mirror_gap/2)) then
		if (x_dish_2 .LE. (x_mirror_gap/2)) then
	x_shade = w_rect
	H_dish_1 = (r_dish**2 - x_dish_1**2)**0.5	!height (radius) between center of each differential rectangle on dish 1 and perimeter of dish 1" 
	H_dish_2 = (r_dish**2 - x_dish_2**2)**0.5	!"height (radius) between center of differential rectangle on dish 2 (point projected from dish 1) and perimeter of dish 2" 
	y_shade=-(tan(phi_E)*y_B)+H_dish_1+H_dish_2-H_mirror_gap-rise_NS
		if (y_shade .LE. 0) then
		y_shade = 0
		endif
		if (y_shade .GT. 2*H_dish_2) then
		y_shade = 2*H_dish_2
		endif
		NS_shade = x_shade * y_shade
		endif
	endif

	!"E-W shade"

	if (x_dish_2_EW .GE. (-x_mirror_gap/2)) then
		if (x_dish_2_EW .LE. (x_mirror_gap/2)) then
	x_shade = w_rect
	H_dish_2_EW = (r_dish**2 - x_dish_2_EW**2)**0.5  !	"height (radius) between center of differential rectangle on dish 2 (point projected from dish 1) and perimeter of dish 2"
	y_shade_EW=-(tan(phi_E)*y_B_EW)+H_dish_1+H_dish_2_EW-
     .	        H_mirror_gap-rise_EW
		if (y_shade_EW .LE. 0) then
		y_shade_EW = 0
		endif
		if (y_shade_EW .GT. 2*H_dish_2_EW) then
		y_shade_EW = 2*H_dish_2_EW
		endif
		EW_shade = x_shade * y_shade_EW
		endif
	endif

	! Diagonal shading

	if (x_dish_2_diag .GE. (-x_mirror_gap/2)) then
		if (x_dish_2_diag .LE. (x_mirror_gap/2)) then
	x_shade = w_rect
	H_dish_2_diag = (r_dish**2 - x_dish_2_diag**2)**0.5  !	"height (radius) between center of differential rectangle on dish 2 (point projected from dish 1) and perimeter of dish 2"
	y_shade_diag = -(tan(phi_E)*y_B_diag) + H_dish_1 + 
     .                 H_dish_2_diag-H_mirror_gap - rise_diag
		if (y_shade_diag .LE. 0) then
		y_shade_diag = 0
		endif
		if (y_shade_diag .GT. 2*H_dish_2_diag) then
		y_shade_diag = 2*H_dish_2_diag
		endif
		diag_shade = x_shade * y_shade_diag
		endif
	endif


	A_shade_NS = A_shade_NS + NS_shade    !sum up total NS shading over dish
	A_shade_EW = A_shade_EW + EW_shade    !sum up total EW shading over dish
	A_shade_diag = A_shade_diag + diag_shade

50		continue   !do-loop is repeated




c ==========


	if (phi_E .LT. 0) then
		A_shade_NS= 0
		A_shade_EW=0
		A_shade_diag=0
	endif

	A_shade_combined = A_shade_NS + A_shade_EW + A_shade_diag
	A_shade_interior = (NNS-1)*(NEW-1) * A_shade_combined

	if (solar_azimuth .LE. -90) then  !Northeast quandrant
		A_shade_exterior = (NNS-1)*A_shade_NS + (NEW-1)*A_shade_EW
	endif

	if (solar_azimuth .GE. 0) then
		if (solar_azimuth .LE. 90) then  !Southwest quandrant
		A_shade_exterior = (NNS-1)*A_shade_NS + (NEW-1)*A_shade_EW
		endif
	endif

	if (solar_azimuth .GE. 90) then   !Northwest quandrant
		A_shade_exterior = (NNS-1)*A_shade_EW + (NEW-1)*A_shade_NS
	endif

	if (solar_azimuth .GE. 0) then
		if (solar_azimuth .LE. 90) then   !Southeast quandrant
		A_shade_exterior = (NNS-1)*A_shade_EW + (NEW-1)*A_shade_NS
		endif
	endif


	A_shade_tot = A_shade_interior + A_shade_exterior


	Shade_AVG=MAX(zero,(A_shade_tot)/(NNS*NEW))  


c ===================================================
	!Correct projected area of dish for shading

c	Projected_Area = MAX(zero,Projected_Area - Shade_AVG)
	! performance fraction reduced due to shading
	phi_shade = MAX(zero,(Projected_Area-Shade_AVG)/
     .	(Projected_Area+0.0000001))  

c ===================================================

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



C		 Output: Power_out_col  (total power from collector dish)
C			Turn off Stirling energy production if wind speed past allowable
C			Turn off SE if I_beam_in is less than the system cut in insolation
			if(wind_speed_in .LE. Wind_Stow_Speed) then
				if (I_beam_in*1000/3600 .GE. I_cut_in)  then
					OUT(1)=I_beam_in/3600*
     .					   Reflectivity*Projected_Area*phi_shade
				else
					OUT(1) = 0
				endif
			else
			OUT(1) = 0
			endif


C		 T_amb_out
			OUT(2)= T_amb_in + 273.15

C		 P_atm_out
			OUT(3)=P_atm_in

C		 wind_speed_out
			OUT(4)=wind_speed_in
C		 sun_angle_out
		!solar zenith is input from data file so convert to solar altitude angle
			OUT(5)= 90 - sun_angle_in   

C		 Collector_Losses
			if(wind_speed_in .LE. Wind_Stow_Speed) then
				if (I_beam_in*1000/3600 .GE. I_cut_in)  then
				OUT(6)=I_beam_in/3600*Projected_Area - OUT(1)
				else
					OUT(6) = 0
				endif
			else
			OUT(6) = 0
			endif
			
C		eta_Collector
		OUT(7) = OUT(1) / ( OUT(1) + OUT(6)+0.0000001 )    !A_shade_EW 

C	     Number_of_Collectors
		OUT(8) = Number_of_Collectors

C		DNI [W/m^2]
		OUT(9) = DNI

C		I_cut_in [W/m^2]
		OUT(10) = I_cut_in

C		P_in_Rec
		OUT(11)=OUT(1)*intercept_factor 

C		interecept_factor
		OUT(12) = intercept_factor   ! A_shade_diag

C		d_ap
		OUT(13) = d_ap
	
C		P_in_collector
C		OUT(14) = OUT(11) / (OUT(7) + 0.00000001)
			OUT(14) = I_beam_in/3600*Projected_Area

C		NS_dish_separation	
		OUT(15) = NS_dish_separation

C		EW_dish_separation
		OUT(16) = EW_dish_separation


C		Shading factor
		OUT(17) =  phi_shade   !A_shade_NS



C-----------------------------------------------------------------------------------------------------------------------
C    EVERYTHING IS DONE - RETURN FROM THIS SUBROUTINE AND MOVE ON
      RETURN 1
      END
C-----------------------------------------------------------------------------------------------------------------------
