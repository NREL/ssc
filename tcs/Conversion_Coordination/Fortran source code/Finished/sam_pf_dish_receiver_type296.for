			SUBROUTINE TYPE296 (TIME,XIN,OUT,T,DTDT,PAR,INFO,ICNTRL,*) 
C************************************************************************
C  COPYRIGHT 2008 NATIONAL RENEWABLE ENERGY LABORATORY
C Object: 7_10_07_Receiver
C Simulation Studio Model: 7_10_07_Receiver
C 
C Author: 
C Editor: 
C Date:	 May 30, 2007 last modified: July 10, 2007
C 
C 
! Doc. tables updated 7/1/2010 - MJW
!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Parameters
!    1| receiver_type                    | Receiver type (always =1)                                         | none             | none             
!    2| transmittance_cover              | Transmittance cover (always=1)                                    | none             | none             
!    3| manufacturer                     | Manufacturer (always=5)                                           | none             | none             
!    4| alpha_absorber                   | Absorber absorptance                                              | none             | none             
!    5| A_absorber                       | Absorber surface area                                             | m2               | m2               
!    6| alpha_wall                       | Cavity absorptance                                                | none             | none             
!    7| A_wall                           | Cavity surface area                                               | m2               | m2               
!    8| L_insulation                     | Insulation thickness                                              | m                | m                
!    9| k_insulation                     | Insulation thermal conductivity                                   | W/m.K            | W/m.K            
!   10| d_cav                            | Internal diameter of cavity perp. to aperture                     | m                | m                
!   11| P_cav                            | Internal cavity pressure with aperture covered                    | kPa              | kPa              
!   12| L_cav                            | Internal depth of cavity perp. to aperture                        | m                | m                
!   13| delta_T_DIR                      | Delta Temperature for DIR Receiver                                | K                | K                
!   14| delta_T_reflux                   | Delta Temperature for REFLUX Receiver (always = 40)               | K                | K                

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Inputs
!    1| Power_in_Rec                     | Power entering the receiver from the collector                    | kW               | kW               
!    2| T_amb_in                         | Ambient temperature in Kelvin                                     | K                | K                
!    3| P_atm_in                         | Atmospheric pressure                                              | Pa               | Pa               
!    4| wind_speed_in                    | Wind velocity                                                     | m/s              | m/s              
!    5| sun_angle_in                     | Solar altitude angle                                              | deg              | deg              
!    6| Number_of_Collectors             | Total number of collectors (Num N-S x Num E-W)                    | none             | none             
!    7| T_heater_head_high               | Heater Head Set Temperature                                       | K                | K                
!    8| T_heater_head_low                | Header Head Lowest Temperature                                    | K                | K                
!    9| DNI                              | Direct normal radiation (not interpolated)                        | W/m2             | W/m2             
!   10| I_cut_in                         | The cut-in DNI value used in the simulation                       | W/m2             | W/m2             
!   11| d_ap                             | The aperture diameter used in the simulation                      | m                | m                

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Outputs
!    1| Power_out_Rec                    | Receiver output power (equals engine inlet power)                 | kW               | kW               
!    2| T_amb_out                        | Ambient temperature in Kelvin                                     | K                | K                
!    3| q_rec_losses_kW                  | Receiver thermal losses                                           | kW               | kW               
!    4| eta_Receiver                     | Receiver efficiency                                               | none             | none             
!    5| T_heater_head_operate            | Receiver head operating temperature                               | K                | K                
!    6| Number_of_Collectors             | Number of collectors                                              | none             | none             
!    7| rho_air                          | Air density                                                       | kg/m3            | kg/m3            
!    8| DNI                              | Direct normal radiation (not interpolated)                        | W/m2             | W/m2             
!    9| I_cut_in                         | Cut in DNI value used in the simulation                           | W/m2             | W/m2             
!   10| q_rad_reflection                 | Reflected radiation                                               | kW               | kW               
!   11| q_rad_emission                   | Emitted radiation                                                 | kW               | kW               
!   12| q_conv                           | Total convection losses                                           | kW               | kW               
!   13| q_cond                           | Conduction losses                                                 | kW               | kW               


C (Comments and routine interface generated by TRNSYS Studio)
C************************************************************************

C    TRNSYS acess functions (allow to acess TIME etc.) 
      USE TrnsysConstants
      USE TrnsysFunctions

C-----------------------------------------------------------------------------------------------------------------------
C    REQUIRED BY THE MULTI-DLL VERSION OF TRNSYS
      !DEC$ATTRIBUTES DLLEXPORT :: TYPE296				!SET THE CORRECT TYPE NUMBER HERE
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
      PARAMETER (NP=14,NI=11,NOUT=13,ND=0,NSTORED=0)
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
      DOUBLE PRECISION d_ap
      DOUBLE PRECISION receiver_type
      DOUBLE PRECISION transmittance_cover
      DOUBLE PRECISION manufacturer
      DOUBLE PRECISION T_heater_head_high
      DOUBLE PRECISION T_heater_head_low
      DOUBLE PRECISION alpha_absorber
      DOUBLE PRECISION A_absorber
      DOUBLE PRECISION alpha_wall
      DOUBLE PRECISION A_wall
      DOUBLE PRECISION L_insulation
      DOUBLE PRECISION k_insulation
      DOUBLE PRECISION d_cav		
	DOUBLE PRECISION d_ap_TEST	
	DOUBLE PRECISION TEST_intercept_factor  
	DOUBLE PRECISION TEST_focal_length

C    INPUTS
      DOUBLE PRECISION Power_in_Rec
      DOUBLE PRECISION T_amb_in
      DOUBLE PRECISION P_atm_in
      DOUBLE PRECISION wind_speed_in
      DOUBLE PRECISION sun_angle_in
      DOUBLE PRECISION intercept_factor
	DOUBLE PRECISION Number_of_Collectors
	DOUBLE PRECISION DNI


C    Variables/Parameters called below
	DOUBLE PRECISION T_rec_ave
	DOUBLE PRECISION g
      DOUBLE PRECISION k_air
      DOUBLE PRECISION beta_air
      DOUBLE PRECISION mu_air
      DOUBLE PRECISION rho_air
      DOUBLE PRECISION nu_air
      DOUBLE PRECISION Pr_air
      DOUBLE PRECISION P_atm_standard
      DOUBLE PRECISION pi
      DOUBLE PRECISION A_ap
      DOUBLE PRECISION A_cav
	DOUBLE PRECISION h_out
      DOUBLE PRECISION R_cond_ins
      DOUBLE PRECISION R_conv_housing
      DOUBLE PRECISION q_cond_loss
      DOUBLE PRECISION Lc_3
	DOUBLE PRECISION S3
      DOUBLE PRECISION Gr3
      DOUBLE PRECISION Nu3
      DOUBLE PRECISION h_cav3
      DOUBLE PRECISION q_conv_loss
	DOUBLE PRECISION EPSILON_rad
      DOUBLE PRECISION sigma
	DOUBLE PRECISION q_rad_emission
      DOUBLE PRECISION alpha_cav_ave
      DOUBLE PRECISION alpha_eff
      DOUBLE PRECISION q_rad_reflection
      DOUBLE PRECISION q_rad_loss
	DOUBLE PRECISION q_rec_losses_kW
	DOUBLE PRECISION delta_T_DIR
	DOUBLE PRECISION delta_T_reflux
	DOUBLE PRECISION M_air
	DOUBLE PRECISION R_bar
	DOUBLE PRECISION R_air
	DOUBLE PRECISION T_heater_head_operate
	DOUBLE PRECISION I_cut_in
	DOUBLE PRECISION transmit_diffuse
	DOUBLE PRECISION h_forced_wind
	DOUBLE PRECISION q_conv_forced
      DOUBLE PRECISION psi_rim
	DOUBLE PRECISION Power_tot
      DOUBLE PRECISION d_psi
      DOUBLE PRECISION Ib
      DOUBLE PRECISION intercept_factor_solve
      DOUBLE PRECISION sigma_tot_guess
	DOUBLE PRECISION h
      DOUBLE PRECISION d_collector
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
	DOUBLE PRECISION t1	
	DOUBLE PRECISION Total_Area
	DOUBLE PRECISION L_glass
	DOUBLE PRECISION k_glass
	DOUBLE PRECISION P_cav
	DOUBLE PRECISION T_amb
	DOUBLE PRECISION theta_rad
	DOUBLE PRECISION T_film_in
	DOUBLE PRECISION T_glass_in
	DOUBLE PRECISION k_air_in
	DOUBLE PRECISION rho_air_in
	DOUBLE PRECISION Cp_air_in
	DOUBLE PRECISION mu_air_in
	DOUBLE PRECISION BETA_in
	DOUBLE PRECISION alpha_in
	DOUBLE PRECISION nu_in
	DOUBLE PRECISION Gr_in
	DOUBLE PRECISION Pr_in 
	DOUBLE PRECISION Ra_in
	DOUBLE PRECISION Nusselt_90
	DOUBLE PRECISION Nusselt_in
	DOUBLE PRECISION h_glass_in
	DOUBLE PRECISION T_film_out
	DOUBLE PRECISION k_air_out
	DOUBLE PRECISION L_cav
	DOUBLE PRECISION T_glass_out
	DOUBLE PRECISION rho_air_out
	DOUBLE PRECISION Cp_air_out
	DOUBLE PRECISION mu_air_out
	DOUBLE PRECISION BETA_out
	DOUBLE PRECISION alpha_out
	DOUBLE PRECISION nu_out
	DOUBLE PRECISION Gr_out
	DOUBLE PRECISION Pr_out
	DOUBLE PRECISION Ra_out
	DOUBLE PRECISION Nusselt_vertical
	DOUBLE PRECISION Nusselt_horiz
	DOUBLE PRECISION Nusselt_out_max
	DOUBLE PRECISION h_glass_out
	DOUBLE PRECISION Re_air_out
	DOUBLE PRECISION Pe_out
	DOUBLE PRECISION Nusselt_out_forced
	DOUBLE PRECISION h_out_forced
	DOUBLE PRECISION h_outside_total
	DOUBLE PRECISION R_rad_out
	DOUBLE PRECISION R_conv_out
	DOUBLE PRECISION R_rad_in
	DOUBLE PRECISION R_conv_in
	DOUBLE PRECISION R_cond_glass
	DOUBLE PRECISION R1
	DOUBLE PRECISION R2
	DOUBLE PRECISION R3
	DOUBLE PRECISION T_glass_in_TRN
	DOUBLE PRECISION T_glass_out_TRN
	DOUBLE PRECISION Q_rad_conv_glass
	DOUBLE PRECISION tolerance
	DOUBLE PRECISION residual
	DOUBLE PRECISION d_T
	DOUBLE PRECISION Q_losses1
	DOUBLE PRECISION Q_losses2
	DOUBLE PRECISION Q_losses3
	DOUBLE PRECISION T_glass
	DOUBLE PRECISION T_glass_res
	DOUBLE PRECISION Q_reject


C-----------------------------------------------------------------------------------------------------------------------
C       READ IN THE VALUES OF THE PARAMETERS IN SEQUENTIAL ORDER
      
	receiver_type=PAR(1)
      transmittance_cover=PAR(2)
      manufacturer=PAR(3)
      alpha_absorber=PAR(4)
      A_absorber=PAR(5)
      alpha_wall=PAR(6)
      A_wall=PAR(7)
      L_insulation=PAR(8)
      k_insulation=PAR(9)
      d_cav=PAR(10)
	  P_cav=PAR(11)
	  L_cav=PAR(12)
	  delta_T_DIR=PAR(13)        !Kelvin
	  delta_T_reflux=PAR(14)     !Kelvin

C-----------------------------------------------------------------------------------------------------------------------
C    RETRIEVE THE CURRENT VALUES OF THE INPUTS TO THIS MODEL FROM THE XIN ARRAY IN SEQUENTIAL ORDER

      Power_in_Rec=XIN(1)
      T_amb_in=XIN(2)
      P_atm_in=XIN(3)
      wind_speed_in=XIN(4)
      sun_angle_in=XIN(5)
	Number_of_Collectors=XIN(6)
	T_heater_head_high=XIN(7)
      T_heater_head_low=XIN(8)
	DNI=XIN(9)
	I_cut_in=XIN(10)
	d_ap = XIN(11)
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

C	============================================
c	d_ap_initial = 0.14  !initialize ap diameter so ap cover code works


C	============================================
C	Manufacturer system specs



C       PERFORM ANY REQUIRED CALCULATIONS TO SET THE INITIAL VALUES OF THE OUTPUTS HERE
C		 Power_out_Rec
			OUT(1)=0
C		 T_amb_out
			OUT(2)=280
C		 P_Rec_losses
			OUT(3)=0
C		 eta_Receiver
			OUT(4)=0
C		 T_heater_head_operate
			OUT(5)=0
C		Number_of_Collectors
			OUT(6)=0
C		rho_air  kg/m^3  [-Inf;+Inf]
			OUT(7)=0
C		DNI   W/m^2   [-Inf;+Inf]
			OUT(8)=0
C		I_cut_in   W/m^2   [-Inf;+Inf]
			OUT(9)=0
C		q_rad_reflection  W  [-Inf;+Inf]
			OUT(10)=0
C		q_rad_emission  W  [-Inf;+Inf]
			OUT(11)=0
C		q_conv  W  [-Inf;+Inf]
			OUT(12)=0
C		q_cond  W  [-Inf;+Inf]
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


	!_____________________________________


	if(manufacturer .EQ. 1) then    !SES System = 1	
			alpha_absorber= 0.90	
			A_absorber= 0.6	
			alpha_wall=	0.6
			A_wall= 0.6
			L_insulation= 0.075	
			k_insulation= 0.06	
			d_cav=0.46
			delta_T_DIR=90        !Kelvin
			delta_T_reflux=40     !Kelvin
			L_cav = d_cav

	elseif(manufacturer .EQ. 2) then    !WGA System = 2	
			alpha_absorber= 0.90	
			A_absorber= 0.15	
			alpha_wall=	0.6
			A_wall= 0.15
			L_insulation= 0.075	
			k_insulation= 0.06	
			d_cav=0.35
			delta_T_DIR=70        !Kelvin
			delta_T_reflux=30     !Kelvin
			L_cav = d_cav

     	elseif(manufacturer .EQ. 3) then    !SBP System = 3	
			alpha_absorber= 0.90	
			A_absorber= 0.15	
			alpha_wall=	0.6
			A_wall= 0.15
			L_insulation= 0.075	
			k_insulation= 0.06	
			d_cav=0.37
			delta_T_DIR=70        !Kelvin
			delta_T_reflux=30     !Kelvin
			L_cav = d_cav

	elseif(manufacturer .EQ. 4) then    !SAIC System = 4	
			alpha_absorber= 0.90	
			A_absorber= 0.8	
			alpha_wall=	0.6
			A_wall= 0.8
			L_insulation= 0.075	
			k_insulation= 0.06	
			d_cav=0.5
			delta_T_DIR=90        !Kelvin
			delta_T_reflux=40     !Kelvin
			L_cav = d_cav

	elseif(manufacturer .EQ. 5) then    !User inputs values = 5
			alpha_absorber= alpha_absorber	
			A_absorber= A_absorber	
			alpha_wall=	alpha_wall
			A_wall= A_wall
			L_insulation= L_insulation
			k_insulation= k_insulation
			d_cav=d_cav
			delta_T_DIR=delta_T_DIR        !Kelvin
			delta_T_reflux=delta_T_reflux     !Kelvin
			L_cav = L_cav 

	endif  !end of if statement


	!========================================================
	!Determine average receiver temperature for a DIR or reflux receiver
	!Reflux receivers should have a lower receiver temp and higher 
	!heater head operating temp.....ie....lower receiver losses 
	!and higher engine efficiency

	if(receiver_type .EQ. 1) then   !DIR is chosen
	T_rec_ave = T_heater_head_high + delta_T_DIR
	T_heater_head_operate = T_heater_head_low

	elseif(receiver_type .EQ. 2) then  !reflux receiver is chosen
	T_rec_ave = T_heater_head_high + 100 + delta_T_reflux
	T_heater_head_operate = T_heater_head_low + 100
	else
	T_rec_ave = T_heater_head_high + delta_T_DIR

	endif  !end of if statement


	!========================================================

	P_atm_standard = 101500  ![Pa]
	pi = 3.141593
	g = 9.8 !m/s^2   gravitational constant
	A_ap = pi*(d_ap/2)**2         !area of aperture
	A_cav = A_absorber + A_wall   !internal cavity area\
	T_amb=T_amb_in
	theta_rad = sun_angle_in*2*pi/360
	
	!_____________________________________
	!air properties  (curve fits generated in EES)
	k_air = 0.00169319 + 0.0000794814*T_amb_in
	beta_air = 0.00949962 - 0.0000297215*T_amb_in    
     .           + 3.06353*10E-08*T_amb_in**2  
	mu_air =0.00000499562 + 4.50917E-08*T_amb_in
	M_air =28.97 ![kg/kmol]  molar mass of air
	R_bar = 8314 ![J/kmol-K]  gas constant
	R_air = R_bar / M_air
	rho_air = P_atm_in/(R_air*T_amb_in)  !ideal gas law
	nu_air = mu_air / (rho_air+0.0000001)
	Pr_air=0.832636 - 0.000460708*T_amb_in + 3.67609E-07*(T_amb_in**2)

	!_____________________________________
	!Receiver conduction (losses)
	h_out = 20 ![W/K-m^2]	External housing convective estimate
	!resistance due to conduction
	R_cond_ins=L_insulation/(k_insulation*A_cav+0.0000001)
	!resistance due to convection
	R_conv_housing = 1 / (h_out *1.5*A_cav+0.0000001)	
	q_cond_loss= (T_rec_ave-T_amb_in)/
     .	        (R_cond_ins+R_conv_housing+0.0000001)/1000
                      
	!_____________________________________
	!Receiver natural convection (losses)...Stine and McDonald (1989) model
	Lc_3 = d_cav   !characteristic length 
	S3 = -0.982 * (d_ap / (Lc_3+0.0000001)) + 1.12
	!Grashof number
	Gr3 = (g * beta_air * (T_rec_ave-T_amb_in) * Lc_3**3) / 
     .	  (nu_air**2+0.0000001)
	!Nusselt number
	Nu3 = 0.088 * Gr3**0.3333 * (T_rec_ave / 
     .	(T_amb_in+0.0000001))**0.18*(cos(sun_angle_in*2*pi/360))**2.47
     .	   * (d_ap / (Lc_3+0.0000001))**S3   !Nusselt number
	h_cav3 = Nu3*k_air/(Lc_3+0.0000001) !convection heat transfer coefficient

	q_conv_loss=(h_cav3*A_cav*(T_rec_ave - T_amb_in)) / 1000   	               

	!_____________________________________
	!Receiver cavity forced convection losses due to wind speed

	h_forced_wind = 0.1967 * wind_speed_in**1.849
	q_conv_forced=(h_forced_wind*A_cav*(T_rec_ave-T_amb_in))/1000   

	!_____________________________________
	!total convection losses
	q_conv_loss = q_conv_loss + q_conv_forced

	!_____________________________________
	!Receiver radiation emitted (losses)

	EPSILON_rad = 1.0 !slight increase in losses from effective absorptance ok
	sigma=5.670E-08 ![W/m^2-K^4]
	q_rad_emission= EPSILON_rad*A_ap*sigma*
     .	           (T_rec_ave**4-T_amb_in**4) / 1000

	!_____________________________________
	!Receiver radiation reflected (losses)
	alpha_cav_ave=(alpha_absorber+alpha_wall)/2 !approx ave cavity apsorptance
	
	transmit_diffuse = 0.85*transmittance_cover !approximation

		if (transmittance_cover .LT. 1 ) then
			alpha_eff = transmittance_cover*alpha_cav_ave /  
     .		(alpha_cav_ave + (1-alpha_cav_ave)*
     .	     transmit_diffuse*(A_ap/(A_cav+0.0000001))+0.0000001)
			else
			alpha_eff = alpha_cav_ave / (alpha_cav_ave + 
     .	   (1-alpha_cav_ave)*(A_ap/(A_cav+0.0000001))+0.0000001)
		
		endif


	q_rad_reflection = (1 - alpha_eff) * Power_in_Rec

	!_____________________________________
	!Total receiver radiation (losses)
	
	if (transmittance_cover .LT. 1 ) then
	q_rad_loss = q_rad_reflection
	else
	q_rad_loss = q_rad_emission + q_rad_reflection
	endif
	!_____________________________________
	!Total receiver losses (kW)

		if (Power_in_Rec .GE. 0.001 ) then
			if (transmittance_cover .LT. 1 ) then
			q_rec_losses_kW = (q_cond_loss+q_rad_loss)
			else
			q_rec_losses_kW = (q_cond_loss+q_conv_loss+q_rad_loss)
			endif
		else
			q_rec_losses_kW = 0
		endif

!================================================================="
	! If receiver cover is used::::
!================================================================="
!Estimate of convection from inside of cavity to glass cover"

	if (transmittance_cover .LT. 1 ) then

	tolerance = 5.0  !2 eq's for glass temp must be within tolerance [K]
	residual = 100.0   !initialize residual to be greater than tolerance
	T_glass = T_rec_ave  !need to initialize
	d_T = 1    !increment T_glass by 1 degree

			do 10 T_glass = T_amb_in,1500,d_T
			if(residual .ge. tolerance) then

	!Use film temperature!
	T_film_in = (T_glass + T_rec_ave)/2
	k_air_in=0.00169319 + 0.0000794814*T_film_in         
	rho_air_in= P_cav /(R_air*T_film_in)  !cavity can be pressurized
	Cp_air_in=1017.7-0.136681*T_film_in+0.000311257*T_film_in**2
	mu_air_in= 0.00000499562 + 4.50917E-08*T_film_in   
	BETA_in=1/T_film_in

	alpha_in=k_air_in/(rho_air_in*Cp_air_in)   !thermal diffusivity
	nu_in=mu_air_in/rho_air_in     !kinematic viscosity
	Gr_in = g*BETA_in*(T_rec_ave-T_glass)*d_ap**3/nu_in**2
	Pr_in = nu_in/alpha_in
	Ra_in = Pr_in * Gr_in

	Nusselt_90 = 0.18*(Pr_in/(0.2+Pr_in)*Ra_in)**0.29   !p563 Incropera

	Nusselt_in = 1+ (Nusselt_90-1)*sin(1.5708+theta_rad) !"P.564 Incropera
	!Find tot convective heat xfer coef
	h_glass_in = max(0.000001,Nusselt_in/L_cav*k_air_in) 

!======================================================"
	! Estimate free (natural) convection from outside aperture plate
!-----------"
!Use film temperature!!!
	T_film_out = (T_amb + T_glass)/2
	k_air_out= 0.00169319 + 0.0000794814*T_film_out   
	rho_air_out= P_atm_in/(R_air*T_film_out)       
	Cp_air_out=1017.7-0.136681*T_film_out+0.000311257*T_film_out**2
	mu_air_out=0.00000499562 + 4.50917E-08*T_film_out     
	BETA_out=1/T_film_out
!--------------"
	alpha_out=k_air_out/(rho_air_out*Cp_air_out)   !thermal diffusivity
	nu_out=mu_air_out/rho_air_out     !kinematic viscosity
	g=9.8  ![m/s^2]
	Gr_out = g*BETA_out*(T_glass-T_amb)*d_ap**3/nu_out**2  !Grashof number
	Pr_out = nu_out/alpha_out	!Prandtl number"
	Ra_out = Pr_out*Gr_out    !Rayleigh number"

	Nusselt_vertical =0.68+(0.67*(Ra_out*cos(theta_rad))**0.25/
     .	(1+(0.492/Pr_out)**(9/16))**(4/9))  !for theta between 0-60 degrees
	Nusselt_horiz =0.27*Ra_out**0.25  !for theta between 60-90 degrees
	Nusselt_out_max = max(Nusselt_vertical,Nusselt_horiz)

	h_glass_out = Nusselt_out_max * k_air_out / d_ap

!======================================================"
!forced convection from outside plate

	Re_air_out = rho_air_out * wind_speed_in*d_ap/mu_air_out  !Reynolds number
	Pe_out = Re_air_out * Pr_out  	!Peclet number"

	if (Re_air_out .GE. 500000 ) then
		Nusselt_out_forced=(0.037*Re_air_out**0.8-871)*Pr_out**(1/3)!turb flow	
		else
		Nusselt_out_forced=0.664*Re_air_out**0.5*Pr_out**0.3333 !laminar flow
	endif

	h_out_forced = Nusselt_out_forced * k_air_out / d_ap
	h_outside_total=(h_glass_out**3+h_out_forced**3)**(1/3) !p924 Klein/Nellis

!===========================================

	R_rad_out=1/(A_ap*sigma*(T_glass**2+T_amb**2)*
     .	      (T_glass+T_amb))
	R_conv_out= 1 /( h_outside_total * A_ap)
	R_rad_in=1 / (A_ap *sigma*(T_rec_ave**2+T_glass**2)*
     .	    (T_rec_ave+T_glass) )
	R_conv_in= 0.001 + 1 /(h_glass_in * A_ap+0.0000001)

	R1 = (1/(R_rad_in+0.0000001)+1/(R_conv_in+0.0000001)+0.000001)
     .	  **(-1)
	R2 =(1/(R_rad_out+0.0000001)+1/(R_conv_out+0.0000001)+0.000001)
     .	  **(-1)

	T_glass_res = R2*(T_rec_ave-T_amb)/(R1+R2) + T_amb
	Q_reject = (T_glass_res - T_amb)/R2
	


!==================

	!determine error in Q_losses......they should all be equal

	residual = abs(T_glass_res-T_glass)

c		    goto 30    !loop back to (while) loop

		else 
			goto 20

		endif  !while loop solving for the cooling fluid temp
				
			
 10			continue   !T_glass_in do-loop is repeated
		

c 20		continue   !T_glass_out do-loop is repeated
		

	! 10) If residual is greater than tolerance repeat using lower T_glass_in temp
c		goto 30    !loop back to (while) loop
c		endif  !end of (while) loop
20		q_rec_losses_kW=q_rec_losses_kW+(Q_reject/1000)!convert to kW
		else
		q_rec_losses_kW = q_rec_losses_kW
	endif


!==============================================

	if (transmittance_cover .LT. 1 ) then
	q_conv_loss = Q_reject/1000
	q_rad_emission = 0
	endif

!==============================================

		if (Power_in_Rec .LT. 0.001 ) then
			q_rec_losses_kW = 0
			q_conv_loss = 0
			q_rad_emission = 0
			q_cond_loss = 0
			q_rad_reflection = 0
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


C		 Power_out_Rec......equivalent to power into the Stirling Engine
			if (q_rec_losses_kW .LE. Power_in_Rec) then
		OUT(1)= Power_in_Rec - q_rec_losses_kW      
			else
			OUT(1)= 0
			endif

C		 T_amb_out
			OUT(2)=T_amb_in
C		 P_Rec_losses
			OUT(3)=q_rec_losses_kW
C		 eta_Receiver
			OUT(4) = OUT(1) / ( Power_in_Rec + 0.0000001)
C		 T_heater_head_operate
			OUT(5)=T_heater_head_operate
C		 Number_of_Collectors
			OUT(6)=Number_of_Collectors
C		 rho_air
			OUT(7)= rho_air
C		DNI
			OUT(8) = DNI
C		I_cut_in [W/m^2]
			OUT(9) = I_cut_in
C		reflected radiation
			OUT(10) = q_rad_reflection
C		Emitted radiation
			OUT(11)=q_rad_emission
C		Total convection losses
			OUT(12)=q_conv_loss
C		conduction losses
			OUT(13)=q_cond_loss



C-----------------------------------------------------------------------------------------------------------------------
C    EVERYTHING IS DONE - RETURN FROM THIS SUBROUTINE AND MOVE ON
      RETURN 1
      END
C-----------------------------------------------------------------------------------------------------------------------
