SUBROUTINE TYPE232 (TIME,XIN,OUT,T,DTDT,PAR,INFO,ICNTRL,*)
!************************************************************************
! Object: Cavity Solar Central Receiver
! Simulation Studio Model: Type232
!
! Author: Lukas Feierabend
! Editor: Michael J. Wagner, Soenke Teichel
! Date: December 16, 2009
! Last modified: March 30, 2011 MJW
!                December 16, 2011 ST
!                September 25, 2012 TWN
!
! Doc. tables updated 2011-03-30 - MJW
!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Parameters
!    1| Rec_d_spec                       | The specified reciever aperture width (converted to R_rec in code)| m                | m                
!    2| H_rec                            | Height of a receiver panel                                        | m                | m                
!    3| H_lip                            | Height of the upper lip of the cavity                             | m                | m                
!    4| THT                              | Total height of the solar tower                                   | m                | m                
!    5| RecAngle                         | Section of the cavity circle which is covered with panels         | deg              | deg              
!    6| D_tube_out                       | Outer diameter of a single tube                                   | mm               | m                
!    7| th_tube                          | Wall thickness of a single tube                                   | mm               | m                
!    8| efficiency_pump                  | Efficiency of the pump for the working fluid                      | none             | none             
!    9| hel_stow_deploy                  | Heliostat field stow/deploy solar angle                           | deg              | deg              
!   10| FlowPattern                      | Flag indicating coolant flow scheme through the receiver panels   | none             | none             
!   11| HTF                              | Flag indicating the heat transfer fluid                           | none             | none             
!   12| Material                         | Receiver tube material                                            | none             | none             
!   13| LU_FL                            | Fluid property file logical unit                                  | none             | none             
!   14| LU_flux                          | Logical unit for the flux map file                                | none             | none             
!   15| hl_ffact                         | The heat loss factor (thermal loss fudge factor)                  | none             | none             
!   16| T_htf_hot_des                    | Hot HTF outlet temperature at design conditions                   | C                | K                
!   17| T_htf_cold_des                   | Cold HTF inlet temperature at design conditions                   | C                | K                
!   18| f_rec_min                        | Minimum receiver mass flow rate turn down fraction                | none             | none             
!   19| Q_rec_des                        | Design-point receiver thermal power output                        | MWt              | Wt               
!   20| rec_su_delay                     | Fixed startup delay time for the receiver                         | hr               | hr               
!   21| rec_qf_delay                     | Energy-based receiver startup delay (fraction of rated thermal power)| none             | none             
!   22| Convection_Model                  | The type of convection model {1=Clausing, 2=Siebers/Kraabel}      | none             | none             
!   23| m_dot_htf_max                    | Maximum receiver mass flow rate                                   | kg/hr            | kg/hr            
!ST 24| e_thermal                        | Thermal emissivity of the receiver surfaces                       | none             | none
!ST 25| e_solar                          | Solar emissivity of the receiver surfaces                         | none             | none
!ST 26| e_thermal_p                      | Thermal emissivity of the passive receiver surfaces               | none             | none
!ST 27| e_solar_p                        | Solar emissivity of the passive receiver surfaces                 | none             | none
!ST 28| Convection_Type                  | Sets coupled(1) or uncoupled(2) convection in the deck file       | none             | none
!ST 29| forced_convection                | Sets convection - FORCED/wind (1) or NATURAL(0)                   | none             | none 
!ST 30| h_wind_measurement               | Height at that wind measurements are given                        | m                | m  
!ST 31| lambda_step                      | emssivity step wavelength for two-band semi-gray surface          | miron            | micron
!ST 32| wind_direction_dependence        | wind direction dependent forced convection on (1) off(0)          | none             | none
!   33| LU_emissivity                    | emissivity data file logical unit                                 | none             | none             
!   34| N_band                           | Number of band used to approximate the receiver surface emissivity| none             | none             
!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Inputs
!    1| azimuth                          | Solar azimuth angle                                               | deg              | deg              
!    2| zenith                           | Solar zenith angle                                                | deg              | deg              
!    3| T_htf_hot                        | Desired hot outlet temperature of the working fluid               | C                | K                
!    4| T_htf_cold                       | Provided inlet temperature of the heat transfer fluid             | C                | K                
!    5| P_htf                            | Average coolant pressure                                          | bar              | Pa               
!    6| P_amb                            | Ambient atmospheric pressure                                      | atm              | Pa               
!    7| hour                             | Hour of the day                                                   | hr               | hr               
!    8| T_dp                             | Ambient dew point temperature                                     | C                | C                
!    9| I_bn                             | Direct (beam) normal irradiation                                  | kJ/m2.hr         | W/m2             
!   10| efficiency_field                 | Overall efficiency of the heliostat field                         | none             | none             
!   11| T_amb                            | Ambient atmospheric temperature                                   | C                | K    
!ST 12| u_wind                           | Wind velocity - TMY - data                                        | m/s              | m/s    
!ST 13| Deg_wind                         | Wind direction - TMY - data                                       | deg              | deg                

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Outputs
!    1| m_htf_total                      | Total mass flow rate of the working fluid                         | kg/hr            | kg/s             
!    2| efficiency_thermal               | Thermal efficiency of the receiver                                | none             | none             
!    3| W_pump                           | Estimated power for pumping the working fluid                     | MW               | W                
!    4| Q_convection_loss                | Thermal convection losses from the receiver                       | MW               | W                
!    5| Q_radiation_loss                 | radiation losses from the receiver                        | MW               | W                
!    6| Q_thermal                        | Thermal energy absorbed by the heat transfer fluid                | MW               | W                
!    7| T_htf_hot                        | Outlet temperature of the heat transfer fluid                     | C                | K                
!    8| -not named-                      | Receiver power prior to thermal losses                            | MW               | MW               
!    9| field_eff_adj                    | Adjusted heliostat field efficiency - includes overdesign adj.    | none             | none             
!   10| q_solar_tot                      | Total incident power on the receiver                              | MW               | W                
!   11| q_startup                        | Startup energy consumed during the current time step              | MW               | MW               
!ST 12| Availability                     | Availability of the solar tower (hours of operation)              | h                | h
!ST 13| Q_radiation_loss_solar           | solar radiation losses from the receiver                          | MW               | W     
!ST 14| Q_radiation_loss_therm           | Thermal radiation losses from the receiver                        | MW               | W   
!************************************************************************

!TRNSYS acess functions (allow to acess TIME etc.)
USE TrnsysConstants
USE TrnsysFunctions

!-----------------------------------------------------------------------------------------------------------------------
!REQUIRED BY THE MULTI-DLL VERSION OF TRNSYS
!DEC$ATTRIBUTES DLLEXPORT :: TYPE232 !SET THE CORRECT TYPE NUMBER HERE
!-----------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------
! TRNSYS DECLARATIONS
implicit none

real(8):: XIN, OUT, TIME, PAR, STORED, T, DTDT
integer:: INFO(15), NP, NI, NOUT, ND, NPAR, NIN, NDER, IUNIT, ITYPE, ICNTRL, ns

!Set array sizes
parameter (NP=34,NI=13,NOUT=14,ND=0,ns=4)

!Dimension the arrays
dimension XIN(NI),OUT(NOUT),PAR(NP),STORED(ns),T(ND),DTDT(ND)

!-----------------------------------------------------------------------------------------------------------------------
! ADD DECLARATIONS AND DEFINITIONS FOR THE USER-VARIABLES HERE
!outputs
real(8):: Q_thermal,Q_radiation_loss,Q_radiation_loss_therm,Q_radiation_loss_solar,Q_convection_loss,W_pump,m_htf_total,&
               Availability,efficiency_thermal,T_htf_hot_out,q_solar_total
!parameters
real(8):: R_rec,H_rec,H_lip,THT,RecAngle,D_tube_out,th_tube,Material,HTF,efficiency_pump, hl_ffact, T_htf_hot_des,&
               T_htf_cold_des, f_rec_min, Q_rec_des, rec_su_delay, rec_qf_delay, rec_d_spec, m_dot_htf_max,&
               e_solar, e_thermal, e_solar_p, e_thermal_p,h_wind_measurement, lambda_step, forced_convection,&
               wind_direction_dependence 
integer:: N_band

!INPUTS
real(8):: hour,azimuth,zenith,T_amb,T_dp,P_amb,P_htf,I_bn,efficiency_field,T_htf_cold,T_htf_hot,&
               hel_stow_deploy, u_wind, Deg_wind 
               
!local variables
    !The subroutine is able to take panel numbers of 2,4,6 and 12 as inputs.   !ST not anymore!
    !However, the trnsys receiver component is limited to 4 panels due to the viewfactor routines.
    !The number of vertical panel Nodes could be 1,2,5, or 10, The value of 5 was selected based on annual output change of <.02%. MJW
    !The number of tube coils per panel is arbitrarily set here. This number affects the number of fluid tubes per panel and the tube length.
    !The night recirculation mode does not work and is disabled.
integer,parameter::N_nodes = 5, N_panels = 4, N_coils = 6, NightRecirculation = 0 , N_band_fix=20,  Band_mode=1  !2 
    !maximum number of bands is set to N_band_fix=20, more bands are possible but generally not necessary!ST
    !Band_model=1 - two band model is used as described in "Teichel, Soenke H. -M.S.- 2011"
    !Band_model=2 - Multiband model is used
integer::ios,i,j,k, LU_flux, FlowPattern, LU_fl, check_htf, ii,dum1,qq,qq_max,N_tubes,N_45_bends,N_90_bends,N_rays, itermode,Convection_Type, ICT, LU_emissivity
real(8):: time_start,time_end,q_solar_critical,Convection_Model, c_htf_des, m_dot_htf_des, m_dot_htf_min, &
          mode, L_tube
real(8),parameter::pi=3.14159265,grav=9.81,sigma = 5.670373D-08
real(8)::alpha,D_tube_in,A_tube,Z,L_tube_node,LoverD,relRough,L_e_45,L_e_90,H_node,W_panel,W_aperture,A_node,&
        A_F,A_CE,A_L,A_O,Density,Conductivity,Viscosity,efficiency_thermalX,SpecHeat,&
        SkyTemp,deltaT_htfX,T_sky,T_htf(0:N_nodes,1:N_panels),T_htfX(0:N_nodes,1:N_panels),&
        fluxarray(10,12),rho_htf_cold,rho_htf,k_htf,c_htf,mu_htf,Pr_htf,errorsum_temp,errorsum_flow,&
        tolerance,T_O,T_FX,T_CEX,T_LX,T_htf_average,q_htf_total,q_convection,&
        deltaP_avg,deltaP_THT, Tsdum,flux_avg,&
        F_LF,F_OF,F_LCE,F_OCE,F_FCE, E_su, E_su0, t_su, t_su0, hour0, dt, est_load, eta_pump_adj, od_control, tol_od,&
        mode0, err_od, field_eff_adj, q_startup, error, gamma,gamma_count,T_bulk, q_conv_tot, q_radiation_loss_semi_sum,&
        q_convectionX, h_F, h_avg, h_stag, T_stag, s, corr_conv, A_stag, q_conv_stag, A_cavity,&
        q_convection_Clausing1983,q_convection_Clausing1987,q_convection_SiebersAndKraabel, h_CFP_floor_turb,h_CFP_ceiling_turb,&
        h_CFP_panels_turb,h_CFP_lip_turb,F_Solar, q_convection_PaitANDLove, q_convection_LeibAndOrth, q_convection_FCFP,&
	    q_convection_NCFP, T_F,T_CE,T_L, T_s_avg, h_SK_forced, u_wind_tower, h_PaitANDLove, h_LeibAndOrth,&
	    h_clausing1983,h_clausing1987,h_SiebersAndKraabel,h_CFP_x_turb,h_CFP_l_turb,h_CFP, h_conv_cavity,&
	    T_film_gonz, phi_gonz, beta_gonz, k_gonz, c_p_gonz, Pr_gonz, rho_gonz, Gr_gonz, Ra_gonz, Nu_avg_tot_gonz, mu_gonz, h_avg_tot_gonz, q_loss_gonz
logical::is_there
character::checkname*200, test*200
!Set up multidimensional variables
!N_panels    
real(8),dimension(N_panels)::T_htf_hotX,m_htf,m_htfX,q_solar_panel,error_temp,error_flow,u_htf,Re_htf,R_conv,f_htf,&
        Nu_htf,h_htf,q_htf_panel,deltaP_x_m_htf
!N_nodes
real(8),dimension(N_nodes)::F_AL,F_AO,F_BL,F_BO,F_AF, F_BF
!N_nodes,N_panels
real(8),dimension(N_nodes,N_panels)::T_sX,T_s,T_htf_avg,solarflux,q_solar,k_tube,R_cond,UA,&
        T_htf_avgX,q_htf,q_radiation,deltaP_node
!N_nodes,N_nodes
real(8),dimension(N_nodes,N_nodes)::F_AA,F_AB,F_AC,F_AD
!N_nodes,N_panels
real(8),dimension(N_nodes,N_panels)::F_L,F_O,F_F,T_HTFX_mid, rho_HTF_node, mu_HTF_node, Re_HTF_node,c_HTF_node, k_HTF_node,&
        Pr_HTF_node,Nu_HTF_node,f_HTF_node,h_HTF_node,R_conv_node
        
real(8),dimension(N_nodes,N_nodes,N_panels,N_panels)::F_nodes

!ST !N_nodes*N_panels+4
real(8),dimension(N_nodes*N_panels+4)::A_array, e_therm_array, e_solar_array, q_conv, h_conv, T_sX_mid_array,&
        flux_array,T_s_array,T_sX_array,UA_array, T_HTF_avg_array, q_rad_solar_net, q_htf_array, q_rad_therm_net,&
        q_rad_semi_gray_net,T_sX_array_new,q_rad_therm_out,q_rad_solar_out,F_thermal
        
!ST !N_nodes*N_panels+4,N_nodes*N_panels+4
real(8),dimension(N_nodes*N_panels+4,N_nodes*N_panels+4)::F_view, F_hat_therm,F_hat_thermX, F_hat_solar, F_hat_solarX,&
        error_array, h_rad_semi_gray_therm,h_rad_semi_gray_therm_multi, q_rad_solar,q_rad_therm,q_rad_semi_gray
        
real(8),dimension(7)::gamma_calc_array
!ST
real(8),dimension(N_band_fix)::e_band_sel, e_band_ref, f_solar_band
real(8),dimension(N_band_fix-1)::lambda_step_band
real(8),dimension(N_nodes*N_panels+4,N_band_fix)::e_band_array,f_temp_band
real(8),dimension(N_nodes*N_panels+4,N_nodes*N_panels+4,N_band_fix)::F_hat, F_hat_x
!-----------------------------------------------------------------------------------------------------------------------
!RETRIEVE THE CURRENT VALUES OF THE INPUTS TO THIS MODEL FROM THE XIN ARRAY IN SEQUENTIAL ORDER
azimuth = XIN(1) + 180.d0   !By TRNSYS convention, the azimuth angle is 0 at due south, negative to the east,
                    !and positive to the west. The range is then -180 to 180. By the convention used
                    !here, the azimuth is 0 at due north, and ranges clockwise from 0 to 360. This adjusts.
zenith = XIN(2)
T_htf_hot = XIN(3)+273.15   !Convert C to K
T_htf_cold = XIN(4)+273.15   !Convert C to K
P_htf = XIN(5)*1.e5         !Convert bar to Pa
P_amb = XIN(6)*101300.      !Convert atm to Pa
hour = XIN(7)
T_dp = XIN(8)+273.15   !Convert C to K
I_bn = XIN(9)/3.6 !UNIT CONVERSION OF THE DIRECT NORMAL RADIATION FROM kJ/m^2-hr TO W/m^2
efficiency_field = XIN(10)
T_amb = XIN(11)+273.15   !Convert C to K
u_wind = XIN(12)
Deg_wind = XIN(13)

    IUNIT=INFO(1)
    ITYPE=INFO(2)

!-----------------------------------------------------------------------------------------------------------------------
!SET THE VERSION INFORMATION FOR TRNSYS
if(INFO(7) == -2) then
    INFO(12)=16
    return 1
endif
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!do ALL THE VERY LAST call OF THE SIMULATION MANIPULATIONS HERE
if (INFO(8) == -1) then
    !close the flux map file
    inquire(unit=LU_flux,opened=is_there,name=checkname)
    if(is_there) close(LU_flux)
    
    !Call the fluxmap subroutine on final run to handle file closing and deallocation of arrays
    call fluxinterp2D(LU_flux,zenith,azimuth,fluxarray,info)
       
    return 1
endif
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!PERFORM ANY 'AFTER-ITERATION' MANIPULATIONS THAT ARE REQUIRED HERE
if (INFO(13) > 0) then
    
    !Call the property range check subroutine with the inlet and outlet HTF temps to make sure they're in the valid range
    i = check_htf(HTF,T_htf_hot_out)
    i = check_htf(HTF,T_htf_cold)
    
    
    !Set storage variables for the next time step
    if(mode==0.) then  
        !if the time step ends with the tower shut off, then reset the startup energy requirement for the next time step
        E_su = Q_rec_des * rec_qf_delay
        t_su = rec_su_delay
    endif

    stored(1) = mode
    stored(2) = E_su
    stored(3) = t_su
    stored(4) = 0.d0 !Q_solar_critical
    call SetStorageVars(stored,ns,info)
    
    itermode = 1
    od_control = 1.d0

    return 1
endif
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!do first call manipulations here. Don't return until after the receiver program is called
if (INFO(7) == -1) then
    !SET SOME INFO ARRAY VARIABLES TO TELL THE TRNSYS ENGINE HOW THIS TYPE IS TO WORK
    INFO(6)=NOUT
    INFO(9)=1
    INFO(10)=0 !STORAGE FOR VERSION 16 HAS BEEN CHANGED
    
    !SET THE REQUIRED NUMBER OF INPUTS, parameterS AND DERIVATIVES THAT THE USER SHOULD SUPPLY IN THE INPUT FILE
    !IN SOME caseS, THE NUMBER OF VARIABLES MAY DEPEND ON THE VALUE OF parameterS TO THIS MODEL....
    NIN=NI
    NPAR=NP
    NDER=ND
    
    !call THE TYPE CHECK SUBROUTINE TO COMPARE WHAT THIS COMPONENT REQUIRES TO WHAT IS SUPPLIED IN
    !THE TRNSYS INPUT FILE
    call TYPECK(1,INFO,NIN,NPAR,NDER)
    
    !READ IN THE VALUES OF THE parameterS IN SEQUENTIAL ORDER
    rec_d_spec = PAR(1) 
    R_rec = Rec_d_spec/2.d0   !The receiver radius is equal to half of the aperture width that is provided to the deck
    H_rec = PAR(2)
    H_lip = PAR(3)
    THT   = PAR(4)
    RecAngle = PAR(5)*pi/180.0
    D_tube_out = PAR(6)/1000.       !Convert mm to m
    th_tube = PAR(7)/1000.          !Convert mm to m
    efficiency_pump = PAR(8)
    hel_stow_deploy = PAR(9)
    FlowPattern = int(PAR(10))
    HTF = PAR(11)
    Material = PAR(12)
    LU_fl = int(PAR(13))
    LU_flux = int(PAR(14))
    hl_ffact=PAR(15)
    T_htf_hot_des=PAR(16)+273.15    !Convert C to K
    T_htf_cold_des=PAR(17)+273.15   !Convert C to K
    f_rec_min=PAR(18)
    Q_rec_des=PAR(19)*1.e6          !Convert MWt to Wt
    rec_su_delay=PAR(20)
    rec_qf_delay=PAR(21)
    Convection_Model=PAR(22)     !By default, this is 1 in the deck file. Can be changed by advanced users.
    m_dot_htf_max=PAR(23)/3600.d0 !kg/s mjw 3.30.11
    e_thermal=PAR(24)
    e_solar=PAR(25)
    e_thermal_p=PAR(26)
    e_solar_p=PAR(27)
    Convection_Type=PAR(28)
    forced_convection=PAR(29)
    h_wind_measurement=PAR(30)
    lambda_step=PAR(31)
    wind_direction_dependence=Par(32)
    LU_emissivity = int(PAR(33))
    N_band = PAR(34)

    !Initialize sensitive variables
    dt = getSimulationTimeStep()

    !MJW:: Call the fluid property file, initialize property arrays
    call readFluidPropFile(LU_FL)       
    
    mode = 0.d0  !0=requires startup, 1=starting up, 2=running
    itermode = 1  !1 solve for design temp, 2 solve to match mass flow restriction
    od_control = 1.d0 !additional defocusing control for over-design conditions
    tol_od = .001d0  !tolerance for over-design iteration
    
    !Calculate the reference HTF mass flow rate, and the max and min values
    c_htf_des = specheat(HTF,(T_htf_hot_des + T_htf_cold_des)/2.d0,1.d0)*1000.d0    ![J/kg-K] Specific heat at design conditions
    m_dot_htf_des = Q_rec_des/(c_htf_des * (T_htf_hot_des - T_htf_cold_des))    ![kg/s]
    m_dot_htf_min = m_dot_htf_des * f_rec_min   ![kg/s]
    
!ST  ??? WHAT IS THIS ABOUT?....T_amb=0 creates floating point error later in the program!
!    !PERFORM INITIAL CALCULATIONS TO ESTIMATE THE CRITICAL INCIDENT RADIATION FOR CONVERGENCE
!    !USE ARBITRARY INPUT VALUES TO GIVE A GOOD ESTIMATE OF THE SURFACE TEMPERATURE DISTRIBUTION
!    hour = 12.d0
!    azimuth = 0.d0
!    zenith = 30.d0
!    T_amb = 0.d0
!    T_dp = -10.d0
!    P_amb = 1.d0
!    P_htf = 1.d0
!    I_bn = 700.d0
!    efficiency_field = 0.6d0
!    efficiency_pump = 0.8d0
!    hel_stow_deploy = 5.d0
!    q_solar_critical = 0.0001d0
    
    !SET THE NUMBER OF STORAGE SPOTS NEEDED FOR THIS COMPONENT
    call setStorageSize(ns,INFO)
    !Set the storage variables since they're called right away
    stored(1) = mode  !mode / mode0
    stored(2) = Q_rec_des * rec_qf_delay ![W-hr]  Startup energy E_su/E_su0
    stored(3) = rec_su_delay  ![hr] Startup time requirement t_su/t_su0
    stored(4) = 0.d0   !This value is set after the initial performance estimate... q_solar_critical
    !call setstoragevars after initial performance calculation. See below for continuation of info(7)==-1 call

    !CALL THE VIEW FACTOR ROUTINES FOR CALCULATING VIEW FACTORS BETWEEN CAVITY SURFACES
    print *,"Numerical view factors..."
    
    N_rays = 300000
    call OuterPanel_Floor(N_rays, N_nodes,H_rec,H_lip,R_rec,RecAngle,F_AF)
    call InnerPanel_Floor(N_rays, N_nodes,H_rec,H_lip,R_rec,RecAngle,F_BF)
    call Lip_Ceiling(N_rays, N_nodes,H_rec,H_lip,R_rec,RecAngle,F_LCE)
    call Lip_Floor(N_rays, N_nodes,H_rec,H_lip,R_rec,RecAngle,F_LF)
    call Opening_Ceiling(N_rays, N_nodes,H_rec,H_lip,R_rec,RecAngle,F_OCE)
    call Opening_Floor(N_rays, N_nodes,H_rec,H_lip,R_rec,RecAngle,F_OF)

    print *,"Analytical view factors..."
    call panelviewfactors(N_nodes,RecAngle,R_rec,H_rec,H_lip,F_AB(:,1),F_AC(:,1),F_AD(:,1),F_AO(:),F_AL(:),F_BO(:),F_BL(:))
    
    !Cavity receiver dimensions in SI units
    alpha = RecAngle*0.25 !Divide RecAngle into the angles for the 4 panels [rad]
    H_node = H_rec/DBLE(N_nodes) !Height of a panel node
    W_panel = 2.0*R_rec*SIN(alpha/2.0) !Receiver panel width
    W_aperture = 2.0*R_rec*SIN(pi-2.0*alpha) !Aperture width
    Z = R_rec*COS(pi-2.0*alpha) !Distance between the aperture plane and the centerline of the panel cylinder
    A_node = W_panel*H_node !Area of a panel node
    A_F = 2.0*W_panel*R_rec*COS(alpha/2.0)+Z*W_aperture !Floor surface area
    A_CE = A_F !Ceiling surface area
    A_L = H_lip*W_aperture !Lip surface area
    A_O = (H_rec-H_lip)*W_aperture !Aperture surface area

    !dimensions of the HTF tubes
    !D_tube_out = D_tube_out/1000. !Outer tube diameter [m]
    !th_tube = th_tube/1000. !Thickness of the tube [m]
    N_tubes = FLOOR(H_rec/(2.0*DBLE(N_coils)*D_tube_out)) !Number of tubes in each panel
    D_tube_in = D_tube_out - 2.0*th_tube !Inner diameter of each receiver tube [m]
    A_tube = pi*D_tube_in**2*0.25 !Cross-sectional area of one tube [m2]
    L_tube = 2.0*N_coils*W_panel !Entire tube length over the whole panel height [m]
    L_tube_node = L_tube/DBLE(N_nodes) !Tube length in one panel node [m]
    LoverD = L_tube_node/D_tube_in !Ratio of tube length per node over the tube diameter for the pipe flow calculations
    relRough = (45.0e-6)/D_tube_in !(1.5e-6)/D_tube_in Relative roughness of the tubes. http:www.efunda.com/formulae/fluids/roughness.cfm
    N_45_bends = 0 !Number of 45 degree bends in one tube
    N_90_bends = 4*N_coils !Number of 90 degree bends in one tube
    L_e_45 = 16.0 !Equivalent length of a 45 degree bend in the tubing system
    L_e_90 = 30.0 !Equivalent length of a 90 degree bend in the tubing system
    efficiency_thermalX = 0.85 !Guess value for the thermal efficiency of the receiver
    
    
  

    !Arrange the view factors
    F_AA = 0.d0
    do j = 2,N_nodes
        do i = 1,j
            F_AB(i,j) = F_AB(j+1-i,1)
            F_AC(i,j) = F_AC(j+1-i,1)
            F_AD(i,j) = F_AD(j+1-i,1)
        enddo
    enddo
    do j = 2,N_nodes
        do i = j+1,N_nodes
            F_AB(i,j) = F_AB(i-j+1,1)
            F_AC(i,j) = F_AC(i-j+1,1)
            F_AD(i,j) = F_AD(i-j+1,1)
        enddo
    enddo

    F_FCE = 1. -((SUM(F_AF(1:N_nodes))+SUM(F_BF(1:N_nodes)))*2.*A_node + F_LF*A_L + F_OF*A_O)/A_F

    !Distribute the view factor arrays to a more general multidimensional view factor array
    F_nodes(:,:,1,1) = F_AA
    F_nodes(:,:,2,1) = F_AB
    F_nodes(:,:,3,1) = F_AC
    F_nodes(:,:,4,1) = F_AD
    F_nodes(:,:,1,2) = F_AB
    F_nodes(:,:,2,2) = F_AA
    F_nodes(:,:,3,2) = F_AB
    F_nodes(:,:,4,2) = F_AC
    F_nodes(:,:,1,3) = F_AC
    F_nodes(:,:,2,3) = F_AB
    F_nodes(:,:,3,3) = F_AA
    F_nodes(:,:,4,3) = F_AB
    F_nodes(:,:,1,4) = F_AD
    F_nodes(:,:,2,4) = F_AC
    F_nodes(:,:,3,4) = F_AB
    F_nodes(:,:,4,4) = F_AA

    F_L(:,1)=F_AL
    F_L(:,2)=F_BL
    F_L(:,3)=F_BL
    F_L(:,4)=F_AL
    F_O(:,1)=F_AO
    F_O(:,2)=F_BO
    F_O(:,3)=F_BO
    F_O(:,4)=F_AO
    F_F(:,1)=F_AF
    F_F(:,2)=F_BF
    F_F(:,3)=F_BF
    F_F(:,4)=F_AF
    
    
    !ST: F_hat parameters are calculated according to "Heat Transfer; Nellis,Klein; Sec:(10.5.4) Eqn:(10-100)  

    !For the calculation of the F_hat parameters, it is most convinient to organize the view factors in an two dimensional array.
    !Each row represents on of the isothermal surface segments of the cavity. 
    !(1,N_nodes) = Panel A ; (N_nodes+1,2*N_nodes) = Panel B ; (2*N_nodes+1,3*N_nodes) = Panel C ; (3*N_nodes+1,4*N_nodes) = Panel D ; 
    !(4*N_nodes+1) = Floor ; (4*N_nodes+2) = Ceiling ; (4*N_nodes+3) = Lip ; (4*N_nodes+4) = Opening


    do i=1,N_nodes
    	do j=1,N_nodes
	
	        !viewfactors surfaces of different panels
	        F_view(i,j+N_nodes)=F_nodes(i,j,1,2)    !F_AB[i,j]		
		    F_view(i,j+2*N_nodes)=F_nodes(i,j,1,3) 	!F_A_C[i,j]
		    F_view(i,j+3*N_nodes)=F_nodes(i,j,1,4) 	!F_A_D[i,j]
		
    		!realtions due to symmetry
	    	F_view(i+N_nodes,j+2*N_nodes)=F_view(i,j+N_nodes)   !F_B_C[i,j] = F_AB[i,j]		
		    F_view(i+2*N_nodes,j+3*N_nodes)=F_view(i,j+N_nodes) !F_C_D[i,j] = F_AB[i,j]
		    F_view(i+N_nodes,j+3*N_nodes)=F_view(i,j+2*N_nodes) !F_B_D[i,j] = F_A_C[i,j] 
    		
	    	!viewfactors surfaces of the same panel
    		F_view(i,j)=F_nodes(1,1,1,1)					 !F_A_A[1,1] 
	        F_view(i+N_nodes,j+N_nodes)=F_nodes(1,1,1,1)	 !F_A_A[1,1]	
	        F_view(i+2*N_nodes,j+2*N_nodes)=F_nodes(1,1,1,1) !F_A_A[1,1]
	        F_view(i+3*N_nodes,j+3*N_nodes)=F_nodes(1,1,1,1) !F_A_A[1,1]
		
	    enddo
    enddo

    !viewfactors surfaces of the same panel
    do i = 4*N_nodes+1,4*N_nodes+4				
        F_view(i,i)=F_nodes(1,1,1,1)	!F_A_A[1,1]
    enddo

    !viewfactors panel surfaces to passive surfaces(Gloor,Ceiling,Lip,Opening)
    do i=1,N_nodes
        F_view(i,N_panels*N_nodes+1)=F_AF(i)            !F_AF(i,1)
        F_view(i,N_panels*N_nodes+2)=F_AF(N_nodes+1-i)  !F_A_CE(i,1)
        F_view(i,N_panels*N_nodes+3)=F_L(i,1) 		     !F_A_L[i)		"viewfactors panels<->opening/lip"
        F_view(i,N_panels*N_nodes+4)=F_O(i,1)			 !F_A_O(i)

        F_view(i+N_nodes,N_panels*N_nodes+1)=F_BF(i)           !F_AF(i,1)
        F_view(i+N_nodes,N_panels*N_nodes+2)=F_BF(N_nodes+1-i) !F_A_CE(i,1)
        F_view(i+N_nodes,N_panels*N_nodes+3)=F_L(i,2)          !F_B_L(i)
        F_view(i+N_nodes,N_panels*N_nodes+4)=F_O(i,2)          !F_B_O(i)

        F_view(i+2*N_nodes,N_panels*N_nodes+1)=F_BF(i)           !F_AF(i,1)
        F_view(i+2*N_nodes,N_panels*N_nodes+2)=F_BF(N_nodes+1-i) !F_A_CE(i,1)
        F_view(i+2*N_nodes,N_panels*N_nodes+3)=F_L(i,3)          !F_C_L(i)
        F_view(i+2*N_nodes,N_panels*N_nodes+4)=F_O(i,3)          !F_C_O(i)

        F_view(i+3*N_nodes,N_panels*N_nodes+1)=F_AF(i)           !F_AF(i,1)
        F_view(i+3*N_nodes,N_panels*N_nodes+2)=F_AF(N_nodes+1-i) !F_A_CE(i,1)
        F_view(i+3*N_nodes,N_panels*N_nodes+3)=F_L(i,4)          !F_D_L[i)		
        F_view(i+3*N_nodes,N_panels*N_nodes+4)=F_O(i,4)          !F_D_O(i)

    enddo

    ! Reciprocity is used to find the corresponding view factors.

    do i = 1,4*N_nodes
        A_array(i)=A_node	!"panel node surface area"
    enddo

    A_array(N_panels*N_nodes+1)=A_F 	!"floor/ceiling surface area"
    A_array(N_panels*N_nodes+2)=A_CE

    A_array(N_panels*N_nodes+3)=A_L	!"lip surface area"
    A_array(N_panels*N_nodes+4)=A_O	!"opening surface area"

    do i = 1,N_panels*N_nodes+4
	    do j = 1,N_panels*N_nodes+4
 	        F_view(j,i)=(A_array(i)*F_view(i,j))/A_array(j)	!reciprocity   
	    enddo
    enddo

    
    F_view(4*N_nodes+3,4*N_nodes+1)=F_LF  !F_L_F
    F_view(4*N_nodes+1,4*N_nodes+3)=(A_array(4*N_nodes+3)*F_view(4*N_nodes+3,4*N_nodes+1))/A_array(4*N_nodes+1) 

    F_view(4*N_nodes+4,4*N_nodes+1)=F_OF  !F_O_F 
    F_view(4*N_nodes+1,4*N_nodes+4)=(A_array(4*N_nodes+4)*F_view(4*N_nodes+4,4*N_nodes+1))/A_array(4*N_nodes+1)

    F_view(4*N_nodes+3,4*N_nodes+2)=F_LCE !F_L_CE 
    F_view(4*N_nodes+2,4*N_nodes+3)=(A_array(4*N_nodes+3)*F_view(4*N_nodes+3,4*N_nodes+2))/A_array(4*N_nodes+2)

    F_view(4*N_nodes+4,4*N_nodes+2)=F_OCE !F_O_CE 
    F_view(4*N_nodes+2,4*N_nodes+4)=(A_array(4*N_nodes+4)*F_view(4*N_nodes+4,4*N_nodes+2))/A_array(4*N_nodes+2)

    F_view(4*N_nodes+1,4*N_nodes+2)=F_FCE !F_F_CE 
    F_view(4*N_nodes+2,4*N_nodes+1)=(A_array(4*N_nodes+1)*F_view(4*N_nodes+1,4*N_nodes+2))/A_array(4*N_nodes+2)

    F_view(4*N_nodes+3,4*N_nodes+4)=0.d0   !F_L_O
    F_view(4*N_nodes+4,4*N_nodes+3)=0.d0   !F_O_L 

    !************************
    !Multiband calcultation *
    !************************
    if (Band_mode == 2) then
         !ST does this work? Really?
        read (LU_emissivity,*) (e_band_sel(i),i=1,N_band)
        read (LU_emissivity,*) (e_band_ref(i),i=1,N_band)
        read (LU_emissivity,*) (lambda_step_band(i),i=1,N_band-1)

        do k = 1,N_band
        e_band_array(1:N_panels*N_nodes,k)=e_band_sel(k)
        e_band_array(N_panels*N_nodes+1:N_panels*N_nodes+3,k)=e_band_sel(k) !e_band_ref(k)
        e_band_array(N_panels*N_nodes+4,k)=1. !Cavity opening is not reflective
        enddo
     

        print *,"F_hat parameters..." !ST

        !!!Iteration F_hat
        !-----------------------------------------------------------------------------------
        do k = 1,N_band

            do i=1,N_panels*N_nodes+4	!Start value for the iteration of F_hat = view factor
                do j=1,N_panels*N_nodes+4
                    F_hat(i,j,k) = F_view(i,j)
                enddo
            enddo

         !Initial error
         error=9999.0
         ICT=0
    
        !Iteration to obtain result for F_hat
             do WHILE((error .GT. (1.0e-30)).and.(ict.lt.100))  

                 do i=1,N_panels*N_nodes+4	!Calculation of F_hat
                     do j=1,N_panels*N_nodes+4
                         F_hat_X(i,j,k) = F_view(i,j)+SUM((1-e_band_array(1:N_panels*N_nodes+4,k))*F_view(i,1:N_panels*N_nodes+4)*F_hat(1:N_panels*N_nodes+4,j,k))
                         error_array(i,j) = ABS(F_hat(i,j,k)-F_hat_X(i,j,k))
                         F_hat(i,j,k)=F_hat_X(i,j,k)                

                     enddo
                 enddo

             error=MAXVAL(error_array)
             ICT=ICT+1

            enddo
    
        enddo
    !----------------------------------------------
    endif


    !************************
    !Two band calcultation *
    !************************

    if (Band_mode == 1) then

        e_therm_array(1:N_panels*N_nodes)=e_thermal  ! Definition of the emissivity
        e_therm_array(N_panels*N_nodes+1:N_panels*N_nodes+3)=e_thermal_p  ! Definition of the emissivity
        e_therm_array(N_panels*N_nodes+4)=1            ! opening of the cavity is a black surfaces

        e_solar_array(1:N_panels*N_nodes)=e_solar ! Definition of the emissivity
        e_solar_array(N_panels*N_nodes+1:N_panels*N_nodes+3)=e_solar_p ! Definition of the emissivity
        e_solar_array(N_panels*N_nodes+4)=1      ! opening of the cavity is a black surfaces


        print *,"F_hat parameters..." !ST

        !!Iteration F_hat
        !--------------------------------------------------------------------------------------
        !F_hat_therm
        do i=1,N_panels*N_nodes+4	!Start value for the iteration of F_hat = view factor
            do j=1,N_panels*N_nodes+4
                F_hat_therm(i,j) = F_view(i,j)
            enddo
        enddo

        !Initial error
        error=9999.0
        ICT=0
    
        !Iteration to obtain result for F_hat
            do WHILE((error .GT. (1.0e-30)).and.(ict.lt.100))  
                do i=1,N_panels*N_nodes+4	!Calculation of F_hat
                    do j=1,N_panels*N_nodes+4
                         F_hat_thermX(i,j) = F_view(i,j)+SUM((1-e_therm_array(1:N_panels*N_nodes+4))*F_view(i,1:N_panels*N_nodes+4)*F_hat_therm(1:N_panels*N_nodes+4,j))

                         error_array(i,j) = ABS(F_hat_therm(i,j)-F_hat_thermX(i,j))
                         F_hat_therm(i,j)=F_hat_thermX(i,j)                

                    enddo
                enddo

                error=MAXVAL(error_array)
                ICT=ICT+1

            enddo
    
!----------------------------------------------
!F_hat_solar
        do i=1,N_panels*N_nodes+4	!Start value for the iteration of F_hat = view factor
            do j=1,N_panels*N_nodes+4  
                F_hat_solar(i,j) = F_view(i,j)
            enddo
        enddo

        !Initial error
        error=9999.0
        ICT=0
     
        !Iteration to obtain result for F_hat
            do WHILE((error .GT. (1.0e-30)).and.(ict.lt.100))

                do i=1,N_panels*N_nodes+4	!Calculation of F_hat
                    do j=1,N_panels*N_nodes+4
                        F_hat_solarX(i,j) = F_view(i,j)+SUM((1-e_solar_array(1:N_panels*N_nodes+4))*F_view(i,1:N_panels*N_nodes+4)*F_hat_solar(1:N_panels*N_nodes+4,j))
               
                        error_array(i,j) = ABS(F_hat_solar(i,j)-F_hat_solarX(i,j))
                        F_hat_solar(i,j)=F_hat_solarX(i,j)                

                    enddo
                enddo  

                error=MAXVAL(error_array)
                ICT=ICT+1

            enddo
         
    endif

    !Call the fluxmap subroutine on initial run to handle file opening and allocation of arrays
    call fluxinterp2D(LU_flux,zenith,azimuth,fluxarray,info)

    return 1

endif

!--------------------------------------------------------------------------------------




!-----------------------------------------------------------------------------------------------------------------------
!   Get storage variables and run the cavity model here. This allows calls during the first and last simulation calls
!-----------------------------------------------------------------------------------------------------------------------

call getStorageVars(stored,ns,info)
mode0 = stored(1)
E_su0 = stored(2)
t_su0 = stored(3)
q_solar_critical = STORED(4)

!-----------------------------------------------------------------------------------------------------------------------
! *** PERFORM ALL THE CALCULATION HERE FOR THIS MODEL. ***
!-----------------------------------------------------------------------------------------------------------------------

! **************************************************************************
! * This program is an implementation of the cavity-type central receiver model that
! * was developed originally in EES. This CRS model includes calculations of
! * the thermal performance of a cavity-type receiver given flux and ambient inputs.
! * Inputs:
! * hour - hour of the day, where solar noon is zero
! * azimuth - azimuth angle of the sun [deg]
! * zenith - zenith angle of the sun  [deg]
! * R_rec - radius of circle where the common panel edges are located: default 7 [m]
! * H_rec - height of the receiver panels: default 12 [m]
! * H_lip - height of the upper receiver lip: default 3 [m]
! * THT - height of the tower above the ground: default 150 [m]
! * RecAngle - angle of that circle which is covered by panels: default 180 [deg]
! * T_amb - ambient atmospheric temperature: default 293 [K]
! * T_dp - ambient dewpoint temperature [K]
! * P_htf - average pressure of the heat transfer fluid [Pa]
! * P_amb - ambient atmospheric pressure: default 101300 [Pa]
! * I_bn - direct (beam) normal radiation on the field [kJ/m^2-hr]
! * N_nodes - number of panel nodes in the vertical direction: default 5
! * N_panels - number of panels on the receiver: default 4
! * N_coils - number of tubes in each panel
! * D_tube_out - the outer diameter of the individual receiver tubes [m]
! * th_tube - the thickness of the tube wall [m]
! * efficiency_field - efficiency value input obtained from the heliostat field component
! * efficiency_pump - pump efficiency: default 0.8
! * hel_stow_deploy - heliostat field stow/deploy solar angle [deg]
! * LU_flux - logical unit number of the fluxmap.csv file, from TRNSYS
! * T_htf_hot - desired outlet temperature of the HTF: default 838 [K]
! * T_htf_cold - inlet temperature of the HTF: default 565 [K]
! * NightRecirculation - flag to indicate night recirculation through the receiver
! * q_solar_critical - critical solar radiation level that determines whether iterations should begin [W]
! * HTF - integer listing the HTF heat transfer fluid
! * Material - integer listing the receiver wall Material
! * Convection_Model - number listing the applied convection loss model
! * FlowPattern - flag that indicates the HTF flow scheme through the receiver: default 1
! * F_AF - view factor array containing view factors from outer panel to floor
! * F_BF - view factor array containing view factors from inner panel to floor
! * F_LCE - view factor from lip to ceiling
! * F_LF - view factor from lip to floor
! * F_OCE - view factor from opening to ceiling
! * F_OF - view factor from opening to floor
! * ------------------------------------------------------------------------
! * Outputs:
! * Q_thermal - total thermal output of the tower, [W]
! * Q_radiation_loss - thermal radiation losses from the receiver [W]
! * Q_convection_loss - thermal convection losses from the receiver [W]
! * W_pump - required pumping power for the coolant [W]
! * m_htf_total - total mass throughput of HTF [kg/s]
! * efficiency_thermal - thermal efficiency of the receiver
! * T_htf_hot_out - hot outlet temperature of HTF [K]
!
! **************************************************************************

!---------------------------------------------------------------------------

!do an initial check to make sure the solar position called is valid.
!if its not, return the output equal to zeros. Also check to make sure
!the solar flux is at a certain level, otherwise the correlations aren't valid

if(((zenith > (90.0-hel_stow_deploy)).OR.(I_bn <= 1.0)).OR.((zenith == 0.0).AND.(azimuth == 180.0))) then
    if (NightRecirculation == 1) then
        I_bn = 0.0
    else
        mode = 0.d0  !Set the startup mode
        goto 900
    endif
endif


!Evaluation of ambient sky temperature
T_sky = SkyTemp(T_amb,T_dp,hour) !The effective sky temperature [K]

err_od = 999.d0 !mjw 3.29.11 Always reset error to 999. before iteration
15 continue !mjw 3.29.11 return point for over-design iteration
field_eff_adj = efficiency_field*od_control  !mjw 3.29.11

!Get the values of the flux from the fluxmap and store them as flux_in(col,row)
if(I_bn > 1.0)then
    call fluxinterp2D(LU_flux,zenith,azimuth,fluxarray,info)
    if(ErrorFound()) return 1
        !The weather-adjusted, efficiency-adjusted flux values
        do i=1,10
            do j=1,12
                fluxarray(i,j)=fluxarray(i,j)/(950.0)*I_bn*field_eff_adj
            enddo
        enddo
    else
    fluxarray = 0.d0
endif

!Translate the original flux array to the number of vertical nodes and the number of panels, so
!each node has its own averaged flux value
call translateFluxArray(fluxarray,N_nodes,N_panels,solarflux)
if(ErrorFound()) return 1

!Calculate the absolute incident solar radiation on each panel node in [W]
do i=1,N_nodes
    do j=1,N_panels
        q_solar(i,j) = 1000.0*A_node*solarflux(i,j)
    enddo
enddo

!Total solar radiation incident onto each receiver panel [W]
do j=1,N_panels
    q_solar_panel(j) = SUM(q_solar(1:N_nodes,j))
enddo

!Total INCIDENT solar radiation on the cavity receiver [W] !ST
q_solar_total = SUM(q_solar_panel(1:N_panels))  
  
q_solar_critical = q_rec_des/efficiency_thermalX/e_solar* f_rec_min !*1.6  !ST
    
!Check if total incident solar radiation is sufficient for plant operation
if (q_solar_total < q_solar_critical) goto 900

!Aperture (Opening) surface is set to the sky temperature
T_O = T_sky
!Guess values for the receiver surface temperatures
T_sX = T_htf_hot
T_FX = T_htf_hot
T_CEX = T_htf_hot
T_LX = T_htf_hot

!Guess values for the surface and HTF temperature
if(NightRecirculation == 1) then
    T_htfX = (T_htf_hot+T_htf_cold)/2.0
    T_htf_avgX = (T_htf_hot+T_htf_cold)/2.0
else
    T_htfX = T_htf_cold
    T_htf_avgX = T_htf_cold
endif

!HT Fluid Properties at the average inlet and outlet temperature
T_htf_average = (T_htf_cold+T_htf_hot)/2.0
rho_htf = Density(HTF,T_htf_average,P_htf) ![kg/m3]
k_htf = Conductivity(HTF,T_htf_average,0.d0) ![W/m-K]
mu_htf = Viscosity(HTF,T_htf_average,1.d0) ![Pa-s]
c_htf = SpecHeat(HTF,T_htf_average,1.d0)*1000.0 ![J/kg-K]
Pr_htf = c_htf*mu_htf/k_htf ![-]


!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!ASSIGN GUESS VALUES FOR TEMPERATURES AND MASSFLOW RATES DEPENDING ON THE FLOW PATTERN
!>>>>><<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
if (I_bn > 1.0) then
    !Estimate the thermal power produced by the receiver
    !and calculate a HTF mass flow rate guess value, then assign guess values to the
    !average heat transfer fluid temperature for every node
    select case(FlowPattern)
        
    case(1)
        do j=1,N_panels
            m_htfX(j) = efficiency_thermalX*q_solar_panel(j)/(c_htf*(T_htf_hot-T_htf_cold))
            do i=1,N_nodes
                !The heat transferred to the fluid increases the HTF temperature
                T_htfX(i,j) = T_htfX(i-1,j)+ efficiency_thermalX*q_solar(i,j)/(m_htfX(j)*c_htf)
                !Average node heat transfer temperature [K]
                T_htf_avgX(i,j) = (T_htfX(i,j)+T_htfX(i-1,j))/2.0
            enddo
        enddo
        
    case(2)
        do j=1,N_panels
            m_htfX(j) = efficiency_thermalX*q_solar_panel(j)/(c_htf*(T_htf_hot-T_htf_cold))
            do i=1,N_nodes
                !The heat transferred to the fluid increases the HTF temperature
                T_htfX(N_nodes-i,j) = T_htfX(N_nodes+1-i,j) + efficiency_thermalX*q_solar(N_nodes+1-i,j)/(m_htfX(j)*c_htf)
                !Average node heat transfer temperature [K]
                T_htf_avgX(N_nodes+1-i,j) = (T_htfX(N_nodes+1-i,j)+T_htfX(N_nodes-i,j))/2.0
            enddo
        enddo
        
    case(3)
        !Guess value for the total mass flow rate through the receiver
        m_htfX = efficiency_thermalX*q_solar_total/(c_htf*(T_htf_hot-T_htf_cold))
        !Calculate the average HTF temperature for each node
        do j=1,N_panels
            do i=1,N_nodes
                if (j==1) then
                    !The heat transferred to the fluid increases the HTF temperature
                    T_htfX(i,j) = T_htfX(i-1,j) + efficiency_thermalX*q_solar(i,j)/(m_htfX(j)*c_htf)
                    !Average node heat transfer temperature [K]
                    T_htf_avgX(i,j) = (T_htfX(i,j)+ T_htfX(i-1,j))/2.0
                elseif(FLOOR(DBLE(j)/2.0) < (DBLE(j)/2.0))then
                    if (i == 1) then
                        !The heat transferred to the fluid increases the HTF temperature
                        T_htfX(i-1,j) = T_htfX(i-1,j-1)
                        T_htfX(i,j) = T_htfX(i-1,j)+ efficiency_thermalX*q_solar(i,j)/(m_htfX(j)*c_htf)
                        !Average node heat transfer temperature [K]
                        T_htf_avgX(i,j) = (T_htfX(i,j)+ T_htfX(i-1,j))/2.0
                    else
                        !The heat transferred to the fluid increases the HTF temperature
                        T_htfX(i,j) = T_htfX(i-1,j)+ efficiency_thermalX*q_solar(i,j)/(m_htfX(j)*c_htf)
                        !Average node heat transfer temperature [K]
                        T_htf_avgX(i,j) = (T_htfX(i,j)+ T_htfX(i-1,j))/2.0
                    endif
                else
                    if (i == 1) then
                        !The heat transferred to the fluid increases the HTF temperature
                        T_htfX(N_nodes+1-i,j) = T_htfX(N_nodes+1-i,j-1)
                        T_htfX(N_nodes-i,j) = T_htfX(N_nodes+1-i,j) + efficiency_thermalX*q_solar(N_nodes+1-i,j)/(m_htfX(j)*c_htf)
                        !Average node heat transfer temperature [K]
                        T_htf_avgX(N_nodes+1-i,j) = (T_htfX(N_nodes+1-i,j)+ T_htfX(N_nodes-i,j))/2.0
                    else
                        !The heat transferred to the fluid increases the HTF temperature
                        T_htfX(N_nodes-i,j) = T_htfX(N_nodes+1-i,j) + efficiency_thermalX*q_solar(N_nodes+1-i,j)/(m_htfX(j)*c_htf)
                        !Average node heat transfer temperature [K]
                        T_htf_avgX(N_nodes+1-i,j) = (T_htfX(N_nodes+1-i,j)+ T_htfX(N_nodes-i,j))/2.0
                    endif
                endif
            enddo
        enddo
        
    case(4)
        !Guess value for the total mass flow rate through the receiver
        m_htfX = efficiency_thermalX*q_solar_total/(c_htf*(T_htf_hot-T_htf_cold))
        !Calculate the average HTF temperature for each node
        do j=1,N_panels
            do i=1,N_nodes
                if (j==1) then
                    !The heat transferred to the fluid increases the HTF temperature
                    T_htfX(N_nodes-i,j) = T_htfX(N_nodes+1-i,j) + efficiency_thermalX*q_solar(N_nodes+1-i,j)/(m_htfX(j)*c_htf)
                    !Average node heat transfer temperature [K]
                    T_htf_avgX(N_nodes+1-i,j) = (T_htfX(N_nodes-i,j)+ T_htfX(N_nodes+1-i,j))/2.0
                elseif(FLOOR(DBLE(j)/2.0) < (DBLE(j)/2.0))then
                    if (i == 1) then
                        !The heat transferred to the fluid increases the HTF temperature
                        T_htfX(N_nodes+1-i,j) = T_htfX(N_nodes+1-i,j-1)
                        T_htfX(N_nodes-i,j) = T_htfX(N_nodes+1-i,j) + efficiency_thermalX*q_solar(N_nodes+1-i,j)/(m_htfX(j)*c_htf)
                        !Average node heat transfer temperature [K]
                        T_htf_avgX(N_nodes+1-i,j) = (T_htfX(N_nodes+1-i,j)+ T_htfX(N_nodes-i,j))/2.0
                    else
                        !The heat transferred to the fluid increases the HTF temperature
                        T_htfX(N_nodes-i,j) = T_htfX(N_nodes+1-i,j) + efficiency_thermalX*q_solar(N_nodes+1-i,j)/(m_htfX(j)*c_htf)
                        !Average node heat transfer temperature [K]
                        T_htf_avgX(N_nodes+1-i,j) = (T_htfX(N_nodes+1-i,j)+ T_htfX(N_nodes-i,j))/2.0
                    endif
                else
                    if (i == 1) then
                        !The heat transferred to the fluid increases the HTF temperature
                        T_htfX(i-1,j) = T_htfX(i-1,j-1)
                        T_htfX(i,j) = T_htfX(i-1,j)+ efficiency_thermalX*q_solar(i,j)/(m_htfX(j)*c_htf)
                        !Average node heat transfer temperature [K]
                        T_htf_avgX(i,j) = (T_htfX(i,j)+ T_htfX(i-1,j))/2.0
                    else
                        !The heat transferred to the fluid increases the HTF temperature
                        T_htfX(i,j) = T_htfX(i-1,j)+ efficiency_thermalX*q_solar(i,j)/(m_htfX(j)*c_htf)
                        !Average node heat transfer temperature [K]
                        T_htf_avgX(i,j) = (T_htfX(i,j)+ T_htfX(i-1,j))/2.0
                    endif
                endif
            enddo
        enddo
        
    case(5)
        !Guess value for the total mass flow rate through the receiver
        m_htfX = 0.0
        m_htfX(1) = efficiency_thermalX*SUM(q_solar_panel(1:N_panels/2))/(c_htf*(T_htf_hot-T_htf_cold))
        m_htfX(2) = efficiency_thermalX*SUM(q_solar_panel(N_panels/2+1:N_panels))/(c_htf*(T_htf_hot-T_htf_cold))
        !Average temperature rise in each node
        deltaT_htfX = (T_htf_hot-T_htf_cold)/DBLE(N_nodes*N_panels*0.5)
        do j=1,N_panels/2
            do i=1,N_nodes
                if (j==1) then
                    !The heat transferred to the fluid increases the HTF temperature
                    !Outer left panel
                    T_htfX(i,j) = T_htfX(i-1,j) + efficiency_thermalX*q_solar(i,j)/(m_htfX(1)*c_htf)
                    !Outer right panel
                    T_htfX(i,N_panels+1-j) = T_htfX(i-1,N_panels+1-j) + efficiency_thermalX*q_solar(i,N_panels+1-j)/(m_htfX(2)*c_htf)
                    !Average node heat transfer temperature [K]
                    T_htf_avgX(i,j) = (T_htfX(i,j)+ T_htfX(i-1,j))/2.0
                    T_htf_avgX(i,N_panels+1-j) = (T_htfX(i,N_panels+1-j)+ T_htfX(i-1,N_panels+1-j))/2.0
                elseif(FLOOR(DBLE(j)/2.0) < (DBLE(j)/2.0))then
                    if (i == 1) then
                        !The heat transferred to the fluid increases the HTF temperature
                        T_htfX(i-1,j) = T_htfX(i-1,j-1)
                        T_htfX(i-1,N_panels+1-j) = T_htfX(i-1,N_panels+2-j)
                        T_htfX(i,j) = T_htfX(i-1,j)+ efficiency_thermalX*q_solar(i,j)/(m_htfX(1)*c_htf)
                        T_htfX(i,N_panels+1-j) = T_htfX(i-1,N_panels+1-j)+ efficiency_thermalX*q_solar(i,N_panels+1-j)/(m_htfX(2)*c_htf)
                        !Average node heat transfer temperature [K]
                        T_htf_avgX(i,j) = (T_htfX(i,j)+ T_htfX(i-1,j))/2.0
                        T_htf_avgX(i,N_panels+1-j) = (T_htfX(i,N_panels+1-j)+ T_htfX(i-1,N_panels+1-j))/2.0
                    else
                        !The heat transferred to the fluid increases the HTF temperature
                        T_htfX(i,j) = T_htfX(i-1,j)+ efficiency_thermalX*q_solar(i,j)/(m_htfX(1)*c_htf)
                        T_htfX(i,N_panels+1-j) = T_htfX(i-1,N_panels+1-j)+ efficiency_thermalX*q_solar(i,N_panels+1-j)/(m_htfX(2)*c_htf)
                        !Average node heat transfer temperature [K]
                        T_htf_avgX(i,j) = (T_htfX(i,j)+ T_htfX(i-1,j))/2.0
                        T_htf_avgX(i,N_panels+1-j) = (T_htfX(i,N_panels+1-j)+ T_htfX(i-1,N_panels+1-j))/2.0
                    endif
                else
                    if (i == 1) then
                        !The heat transferred to the fluid increases the HTF temperature
                        T_htfX(N_nodes+1-i,j) = T_htfX(N_nodes+1-i,j-1)
                        T_htfX(N_nodes+1-i,N_panels+1-j) = T_htfX(N_nodes+1-i,N_panels+2-j)
                        T_htfX(N_nodes-i,j) = T_htfX(N_nodes+1-i,j) + efficiency_thermalX*q_solar(N_nodes+1-i,j)/(m_htfX(1)*c_htf)
                        T_htfX(N_nodes-i,N_panels+1-j) = T_htfX(N_nodes+1-i,N_panels+1-j)+ efficiency_thermalX*q_solar(N_nodes+1-i,N_panels+1-j)/(m_htfX(2)*c_htf)
                        !Average node heat transfer temperature [K]
                        T_htf_avgX(N_nodes+1-i,j) = (T_htfX(N_nodes+1-i,j)+ T_htfX(N_nodes-i,j))/2.0
                        T_htf_avgX(N_nodes+1-i,N_panels+1-j) = (T_htfX(N_nodes+1-i,N_panels+1-j)+ T_htfX(N_nodes-i,N_panels+1-j))/2.0
                    else
                        !The heat transferred to the fluid increases the HTF temperature
                        T_htfX(N_nodes-i,j) = T_htfX(N_nodes+1-i,j) + efficiency_thermalX*q_solar(N_nodes+1-i,j)/(m_htfX(1)*c_htf)
                        T_htfX(N_nodes-i,N_panels+1-j) = T_htfX(N_nodes+1-i,N_panels+1-j)+ efficiency_thermalX*q_solar(N_nodes+1-i,N_panels+1-j)/(m_htfX(2)*c_htf)
                        !Average node heat transfer temperature [K]
                        T_htf_avgX(N_nodes+1-i,j) = (T_htfX(N_nodes+1-i,j)+ T_htfX(N_nodes-i,j))/2.0
                        T_htf_avgX(N_nodes+1-i,N_panels+1-j) = (T_htfX(N_nodes+1-i,N_panels+1-j)+ T_htfX(N_nodes-i,N_panels+1-j))/2.0
                    endif
                endif
            enddo
        enddo
        
    case(6)
        !Guess value for the total mass flow rate through the receiver
        m_htfX = 0.0
        m_htfX(1) = efficiency_thermalX*SUM(q_solar_panel(1:N_panels/2))/(c_htf*(T_htf_hot-T_htf_cold))
        m_htfX(2) = efficiency_thermalX*SUM(q_solar_panel(N_panels/2+1:N_panels))/(c_htf*(T_htf_hot-T_htf_cold))
        !Average temperature rise in each node
        deltaT_htfX = (T_htf_hot-T_htf_cold)/DBLE(N_nodes*N_panels*0.5)
        do j=1,N_panels/2
            do i=1,N_nodes
                if (j == 1) then
                    !The heat transferred to the fluid increases the HTF temperature
                    !Outer left panel
                    T_htfX(N_nodes-i,j) = T_htfX(N_nodes+1-i,j) + efficiency_thermalX*q_solar(N_nodes+1-i,j)/(m_htfX(1)*c_htf)
                    !Outer right panel
                    T_htfX(N_nodes-i,N_panels+1-j) = T_htfX(N_nodes+1-i,N_panels+1-j)+ efficiency_thermalX*q_solar(N_nodes+1-i,N_panels+1-j)/(m_htfX(2)*c_htf)
                    !Average node heat transfer temperature [K]
                    T_htf_avgX(N_nodes+1-i,j) = (T_htfX(N_nodes+1-i,j)+ T_htfX(N_nodes-i,j))/2.0
                    T_htf_avgX(N_nodes+1-i,N_panels+1-j) = (T_htfX(N_nodes+1-i,N_panels+1-j)+ T_htfX(N_nodes-i,N_panels+1-j))/2.0
                elseif(FLOOR(DBLE(j)/2.0) < (DBLE(j)/2.0))then
                    if (i == 1) then
                        !The heat transferred to the fluid increases the HTF temperature
                        T_htfX(N_nodes+1-i,j) = T_htfX(N_nodes+1-i,j-1)
                        T_htfX(N_nodes+1-i,N_panels+1-j) = T_htfX(N_nodes+1-i,N_panels+2-j)
                        T_htfX(N_nodes-i,j) = T_htfX(N_nodes+1-i,j) + efficiency_thermalX*q_solar(N_nodes+1-i,j)/(m_htfX(1)*c_htf)
                        T_htfX(N_nodes-i,N_panels+1-j) = T_htfX(N_nodes+1-i,N_panels+1-j)+ efficiency_thermalX*q_solar(N_nodes+1-i,N_panels+1-j)/(m_htfX(2)*c_htf)
                        !Average node heat transfer temperature [K]
                        T_htf_avgX(N_nodes+1-i,j) = (T_htfX(N_nodes+1-i,j)+ T_htfX(N_nodes-i,j))/2.0
                        T_htf_avgX(N_nodes+1-i,N_panels+1-j) = (T_htfX(N_nodes+1-i,N_panels+1-j)+ T_htfX(N_nodes-i,N_panels+1-j))/2.0
                    else
                        !The heat transferred to the fluid increases the HTF temperature
                        T_htfX(N_nodes-i,j) = T_htfX(N_nodes+1-i,j) + efficiency_thermalX*q_solar(N_nodes+1-i,j)/(m_htfX(1)*c_htf)
                        T_htfX(N_nodes-i,N_panels+1-j) = T_htfX(N_nodes+1-i,N_panels+1-j)+ efficiency_thermalX*q_solar(N_nodes+1-i,N_panels+1-j)/(m_htfX(2)*c_htf)
                        !Average node heat transfer temperature [K]
                        T_htf_avgX(N_nodes+1-i,j) = (T_htfX(N_nodes+1-i,j)+ T_htfX(N_nodes-i,j))/2.0
                        T_htf_avgX(N_nodes+1-i,N_panels+1-j) = (T_htfX(N_nodes+1-i,N_panels+1-j)+ T_htfX(N_nodes-i,N_panels+1-j))/2.0
                    endif
                else
                    if (i == 1) then
                        !The heat transferred to the fluid increases the HTF temperature
                        T_htfX(i-1,j) = T_htfX(i-1,j-1)
                        T_htfX(i-1,N_panels+1-j) = T_htfX(i-1,N_panels+2-j)
                        T_htfX(i,j) = T_htfX(i-1,j)+ efficiency_thermalX*q_solar(i,j)/(m_htfX(1)*c_htf)
                        T_htfX(i,N_panels+1-j) = T_htfX(i-1,N_panels+1-j)+ efficiency_thermalX*q_solar(i,N_panels+1-j)/(m_htfX(2)*c_htf)
                        !Average node heat transfer temperature [K]
                        T_htf_avgX(i,j) = (T_htfX(i,j)+ T_htfX(i-1,j))/2.0
                        T_htf_avgX(i,N_panels+1-j) = (T_htfX(i,N_panels+1-j)+ T_htfX(i-1,N_panels+1-j))/2.0
                    else
                        !The heat transferred to the fluid increases the HTF temperature
                        T_htfX(i,j) = T_htfX(i-1,j)+ efficiency_thermalX*q_solar(i,j)/(m_htfX(1)*c_htf)
                        T_htfX(i,N_panels+1-j) = T_htfX(i-1,N_panels+1-j)+ efficiency_thermalX*q_solar(i,N_panels+1-j)/(m_htfX(2)*c_htf)
                        !Average node heat transfer temperature [K]
                        T_htf_avgX(i,j) = (T_htfX(i,j)+ T_htfX(i-1,j))/2.0
                        T_htf_avgX(i,N_panels+1-j) = (T_htfX(i,N_panels+1-j)+ T_htfX(i-1,N_panels+1-j))/2.0
                    endif
                endif
            enddo
        enddo
        
    case(7)
        !Guess value for the total mass flow rate through the receiver
        m_htfX = 0.0
        m_htfX(1) = efficiency_thermalX*SUM(q_solar_panel(1:N_panels/2))/(c_htf*(T_htf_hot-T_htf_cold))
        m_htfX(2) = efficiency_thermalX*SUM(q_solar_panel(N_panels/2+1:N_panels))/(c_htf*(T_htf_hot-T_htf_cold))
        !Average temperature rise in each node
        deltaT_htfX = (T_htf_hot-T_htf_cold)/DBLE(N_nodes*N_panels*0.5)
        do j=1,N_panels/2
            do i=1,N_nodes
                if (j == 1) then
                    !The heat transferred to the fluid increases the HTF temperature
                    !Inner left panel
                    T_htfX(i,N_panels/2+1-j) = T_htfX(i-1,N_panels/2+1-j) + efficiency_thermalX*q_solar(i,N_panels/2+1-j)/(m_htfX(1)*c_htf)
                    !Inner right panel
                    T_htfX(i,N_panels/2+j) = T_htfX(i-1,N_panels/2+j) + efficiency_thermalX*q_solar(i,N_panels/2+j)/(m_htfX(2)*c_htf)
                    !Average node heat transfer temperature [K]
                    T_htf_avgX(i,N_panels/2+1-j) = (T_htfX(i,N_panels/2+1-j)+ T_htfX(i-1,N_panels/2+1-j))/2.0
                    T_htf_avgX(i,N_panels/2+j) = (T_htfX(i,N_panels/2+j)+ T_htfX(i-1,N_panels/2+j))/2.0
                elseif(FLOOR(DBLE(j)/2.0) < (DBLE(j)/2.0))then
                    if (i == 1) then
                        !The heat transferred to the fluid increases the HTF temperature
                        T_htfX(i-1,N_panels/2+1-j) = T_htfX(i-1,N_panels/2+2-j)
                        T_htfX(i-1,N_panels/2+j) = T_htfX(i-1,N_panels/2-1+j)
                        T_htfX(i,N_panels/2+1-j) = T_htfX(i-1,N_panels/2+1-j)+ efficiency_thermalX*q_solar(i,N_panels/2+1-j)/(m_htfX(1)*c_htf)
                        T_htfX(i,N_panels/2+j) = T_htfX(i-1,N_panels/2+j)+ efficiency_thermalX*q_solar(i,N_panels/2+j)/(m_htfX(2)*c_htf)
                        !Average node heat transfer temperature [K]
                        T_htf_avgX(i,N_panels/2+1-j) = (T_htfX(i,N_panels/2+1-j)+ T_htfX(i-1,N_panels/2+1-j))/2.0
                        T_htf_avgX(i,N_panels/2+j) = (T_htfX(i,N_panels/2+j)+ T_htfX(i-1,N_panels/2+j))/2.0
                    else
                        !The heat transferred to the fluid increases the HTF temperature
                        T_htfX(i,N_panels/2+1-j) = T_htfX(i-1,N_panels/2+1-j)+ efficiency_thermalX*q_solar(i,N_panels/2+1-j)/(m_htfX(1)*c_htf)
                        T_htfX(i,N_panels/2+j) = T_htfX(i-1,N_panels/2+j)+ efficiency_thermalX*q_solar(i,N_panels/2+j)/(m_htfX(2)*c_htf)
                        !Average node heat transfer temperature [K]
                        T_htf_avgX(i,N_panels/2+1-j) = (T_htfX(i,N_panels/2+1-j)+ T_htfX(i-1,N_panels/2+1-j))/2.0
                        T_htf_avgX(i,N_panels/2+j) = (T_htfX(i,N_panels/2+j)+ T_htfX(i-1,N_panels/2+j))/2.0
                    endif
                else
                    if (i == 1) then
                        !The heat transferred to the fluid increases the HTF temperature
                        T_htfX(N_nodes+1-i,N_panels/2+1-j) = T_htfX(N_nodes+1-i,N_panels/2+2-j)
                        T_htfX(N_nodes+1-i,N_panels/2+j) = T_htfX(N_nodes+1-i,N_panels/2-1+j)
                        T_htfX(N_nodes-i,N_panels/2+1-j) = T_htfX(N_nodes+1-i,N_panels/2+1-j)+ efficiency_thermalX*q_solar(N_nodes+1-i,N_panels/2+1-j)/(m_htfX(1)*c_htf)
                        T_htfX(N_nodes-i,N_panels/2+j) = T_htfX(N_nodes+1-i,N_panels/2+j)+ efficiency_thermalX*q_solar(N_nodes+1-i,N_panels/2+j)/(m_htfX(2)*c_htf)
                        !Average node heat transfer temperature [K]
                        T_htf_avgX(N_nodes+1-i,N_panels/2+1-j) = (T_htfX(N_nodes-i,N_panels/2+1-j)+ T_htfX(N_nodes+1-i,N_panels/2+1-j))/2.0
                        T_htf_avgX(N_nodes+1-i,N_panels/2+j) = (T_htfX(N_nodes-i,N_panels/2+j)+ T_htfX(N_nodes+1-i,N_panels/2+j))/2.0
                    else
                        !The heat transferred to the fluid increases the HTF temperature
                        T_htfX(N_nodes-i,N_panels/2+1-j) = T_htfX(N_nodes+1-i,N_panels/2+1-j)+ efficiency_thermalX*q_solar(N_nodes+1-i,N_panels/2+1-j)/(m_htfX(1)*c_htf)
                        T_htfX(N_nodes-i,N_panels/2+j) = T_htfX(N_nodes+1-i,N_panels/2+j)+ efficiency_thermalX*q_solar(N_nodes+1-i,N_panels/2+j)/(m_htfX(2)*c_htf)
                        !Average node heat transfer temperature [K]
                        T_htf_avgX(N_nodes+1-i,N_panels/2+1-j) = (T_htfX(N_nodes-i,N_panels/2+1-j)+ T_htfX(N_nodes+1-i,N_panels/2+1-j))/2.0
                        T_htf_avgX(N_nodes+1-i,N_panels/2+j) = (T_htfX(N_nodes-i,N_panels/2+j)+ T_htfX(N_nodes+1-i,N_panels/2+j))/2.0
                    endif
                endif
            enddo
        enddo
        
    case(8)
        !Guess value for the total mass flow rate through the receiver
        m_htfX = 0.0
        m_htfX(1) = efficiency_thermalX*SUM(q_solar_panel(1:N_panels/2))/(c_htf*(T_htf_hot-T_htf_cold))
        m_htfX(2) = efficiency_thermalX*SUM(q_solar_panel(N_panels/2+1:N_panels))/(c_htf*(T_htf_hot-T_htf_cold))
        !Average temperature rise in each node
        deltaT_htfX = (T_htf_hot-T_htf_cold)/DBLE(N_nodes*N_panels*0.5)
        do j=1,N_panels/2
            do i=1,N_nodes
                if (j == 1) then
                    !The heat transferred to the fluid increases the HTF temperature
                    T_htfX(N_nodes-i,N_panels/2+1-j) = T_htfX(N_nodes+1-i,N_panels/2+1-j)+ efficiency_thermalX*q_solar(N_nodes+1-i,N_panels/2+1-j)/(m_htfX(1)*c_htf)
                    T_htfX(N_nodes-i,N_panels/2+j) = T_htfX(N_nodes+1-i,N_panels/2+j)+ efficiency_thermalX*q_solar(N_nodes+1-i,N_panels/2+j)/(m_htfX(2)*c_htf)
                    !Average node heat transfer temperature [K]
                    T_htf_avgX(N_nodes+1-i,N_panels/2+1-j) = (T_htfX(N_nodes-i,N_panels/2+1-j)+ T_htfX(N_nodes+1-i,N_panels/2+1-j))/2.0
                    T_htf_avgX(N_nodes+1-i,N_panels/2+j) = (T_htfX(N_nodes-i,N_panels/2+j)+ T_htfX(N_nodes+1-i,N_panels/2+j))/2.0
                elseif(FLOOR(DBLE(j)/2.0) < (DBLE(j)/2.0))then
                    if (i == 1) then
                        !The heat transferred to the fluid increases the HTF temperature
                        T_htfX(N_nodes+1-i,N_panels/2+1-j) = T_htfX(N_nodes+1-i,N_panels/2+2-j)
                        T_htfX(N_nodes+1-i,N_panels/2+j) = T_htfX(N_nodes+1-i,N_panels/2-1+j)
                        T_htfX(N_nodes-i,N_panels/2+1-j) = T_htfX(N_nodes+1-i,N_panels/2+1-j)+ efficiency_thermalX*q_solar(N_nodes+1-i,N_panels/2+1-j)/(m_htfX(1)*c_htf)
                        T_htfX(N_nodes-i,N_panels/2+j) = T_htfX(N_nodes+1-i,N_panels/2+j)+ efficiency_thermalX*q_solar(N_nodes+1-i,N_panels/2+j)/(m_htfX(2)*c_htf)
                        !Average node heat transfer temperature [K]
                        T_htf_avgX(N_nodes+1-i,N_panels/2+1-j) = (T_htfX(N_nodes-i,N_panels/2+1-j)+ T_htfX(N_nodes+1-i,N_panels/2+1-j))/2.0
                        T_htf_avgX(N_nodes+1-i,N_panels/2+j) = (T_htfX(N_nodes-i,N_panels/2+j)+ T_htfX(N_nodes+1-i,N_panels/2+j))/2.0
                    else
                        !The heat transferred to the fluid increases the HTF temperature
                        T_htfX(N_nodes-i,N_panels/2+1-j) = T_htfX(N_nodes+1-i,N_panels/2+1-j)+ efficiency_thermalX*q_solar(N_nodes+1-i,N_panels/2+1-j)/(m_htfX(1)*c_htf)
                        T_htfX(N_nodes-i,N_panels/2+j) = T_htfX(N_nodes+1-i,N_panels/2+j)+ efficiency_thermalX*q_solar(N_nodes+1-i,N_panels/2+j)/(m_htfX(2)*c_htf)
                        !Average node heat transfer temperature [K]
                        T_htf_avgX(N_nodes+1-i,N_panels/2+1-j) = (T_htfX(N_nodes-i,N_panels/2+1-j)+ T_htfX(N_nodes+1-i,N_panels/2+1-j))/2.0
                        T_htf_avgX(N_nodes+1-i,N_panels/2+j) = (T_htfX(N_nodes-i,N_panels/2+j)+ T_htfX(N_nodes+1-i,N_panels/2+j))/2.0
                    endif
                else
                    if (i == 1) then
                        !The heat transferred to the fluid increases the HTF temperature
                        T_htfX(i-1,N_panels/2+1-j) = T_htfX(i-1,N_panels/2+2-j)
                        T_htfX(i-1,N_panels/2+j) = T_htfX(i-1,N_panels/2-1+j)
                        T_htfX(i,N_panels/2+1-j) = T_htfX(i-1,N_panels/2+1-j)+ efficiency_thermalX*q_solar(i,N_panels/2+1-j)/(m_htfX(1)*c_htf)
                        T_htfX(i,N_panels/2+j) = T_htfX(i-1,N_panels/2+j)+ efficiency_thermalX*q_solar(i,N_panels/2+j)/(m_htfX(2)*c_htf)
                        !Average node heat transfer temperature [K]
                        T_htf_avgX(i,N_panels/2+1-j) = (T_htfX(i,N_panels/2+1-j)+ T_htfX(i-1,N_panels/2+1-j))/2.0
                        T_htf_avgX(i,N_panels/2+j) = (T_htfX(i,N_panels/2+j)+ T_htfX(i-1,N_panels/2+j))/2.0
                    else
                        !The heat transferred to the fluid increases the HTF temperature
                        T_htfX(i,N_panels/2+1-j) = T_htfX(i-1,N_panels/2+1-j)+ efficiency_thermalX*q_solar(i,N_panels/2+1-j)/(m_htfX(1)*c_htf)
                        T_htfX(i,N_panels/2+j) = T_htfX(i-1,N_panels/2+j)+ efficiency_thermalX*q_solar(i,N_panels/2+j)/(m_htfX(2)*c_htf)
                        !Average node heat transfer temperature [K]
                        T_htf_avgX(i,N_panels/2+1-j) = (T_htfX(i,N_panels/2+1-j)+ T_htfX(i-1,N_panels/2+1-j))/2.0
                        T_htf_avgX(i,N_panels/2+j) = (T_htfX(i,N_panels/2+j)+ T_htfX(i-1,N_panels/2+j))/2.0
                    endif
                endif
            enddo
        enddo
    end select
endif

!Initial value for error calculation
T_htf_hotX = 9999.0
m_htf = 9999.0
error=9999.0 !ST
q_convectionX=0 !ST
q_convection=1 !ST
ICT=0 !ST
h_conv=0 !ST
q_conv=0

! initial values for T_sX_mid
    do k=1,N_panels
       do j=1,N_nodes
            T_sX_mid_array(j+((k-1)*N_nodes))=T_sX(j,k)
        enddo 
    enddo
    T_sX_mid_array(N_panels*N_nodes+1)=T_FX
    T_sX_mid_array(N_panels*N_nodes+2)=T_CEX
    T_sX_mid_array(N_panels*N_nodes+3)=T_LX
    T_sX_mid_array(N_panels*N_nodes+4)=T_O

       do k=1,N_panels
       do j=1,N_nodes
            T_HTFX_mid(j,k)=T_htfX(j,k)
            enddo
            enddo



!Convection_Type - Implementation methods:
!
!COUPLED CONVECTION: Convection_Type=1.
!                    Convection losses ARE included in the energy balances to calculate the surface temperatures. The 
!                    Convection losses are updated after each iteration process of the surface temperatures. An outer
!                    iteration loop repeats the surface temperature calculations until the convection losses are constant.
!                    Using this method the surface temperatures are expected to be lower, resulting in lower receiever losses. 
!
!UNCOUPLED CONVECTION: Convection_Type=2.
!                      Convection is not included in the energy balances to calculate the surface temperatures.
!                      The surface temperatures are calculated without convection, after convergence is obtained,
!                      The convection loss is calculated and considered as a reduction of energy transfer to the HTF;
!                      the surface temperatures remain constant and are NOT influenced by the convection losses.
!
  
!
!-----------------
!Convection_Type=1. - from .dck file
!-----------------
gamma=1.
gamma_calc_array = (/ 1., 0.8, 0.6, 0.4, 0.2, 0.1, 0.05 /)
gamma_count=1.


!--------------------------------------------------------------------------
do WHILE((abs(error).GT.(1.0e-8)).and.ict.lt.100)
    ICT=ICT+1
!--------------------------------------------------------------------------

    222 continue
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    ! ITERATION STARTS HERE
    !>>>>><<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    qq_max = 200          !ST originally 50 1500
    do qq=1,qq_max+1
        
        !ST originally the convergence criterion was checked at the beginning of each loop, 
        !it is now moved to the end of the loop because of the additional iteration loop
        !using the coupled convection implementation method. After the convection loss was
        !updated, the surface temperatures have to be recalculted before convegence is checked.
    
        do k=1,N_panels
            do j=1,N_nodes
                T_htfX(j,k)=T_HTFX_mid(j,k)
            enddo
        enddo

    
    
        !------------------------------------------------------------------------
        !Flow and convective heat transfer characteristics
        !------------------------------------------------------------------------
        do j = 1,N_panels
            !Fluid velocity in the tubes [m/s]
            select case(FlowPattern)
                case(1:4)
                    u_htf(j) = m_htf(j)/(A_tube*rho_htf*DBLE(N_tubes))
                case(5:8)
                    if (j < N_panels/2+1) then
                        u_htf(j) = m_htf(1)/(A_tube*rho_htf*DBLE(N_tubes))
                    else
                        u_htf(j) = m_htf(2)/(A_tube*rho_htf*DBLE(N_tubes))
                    endif
            end select
        
            !Reynolds and Prandtl number that determine the flow conditions
            Re_htf(j) = u_htf(j)*rho_htf*D_tube_in/mu_htf
            call PipeFlowCavity(Re_htf(j),Pr_htf,LoverD,relRough,q_solar_panel(j),Nu_htf(j),f_htf(j))
            if (m_htf(j) == 0.0) then
                f_htf(j) = 0.0
            endif
        
            !The heat transfer coefficient for thermal energy transfer from the tube walls to the HTF
            h_htf(j) = Nu_htf(j)*k_htf/D_tube_in
            !Calculate the resistance to convection from the tube walls to the HTF [K/W]
            R_conv(j)= 2.0/(h_htf(j)*L_tube_node*D_tube_in*pi*DBLE(N_tubes))
        
                        
            do i=1,N_nodes
        
                !------------------------------------------------------------------------ !ST R_conv caclulated for every node
                rho_htf_node(i,j) = Density(HTF,T_htfX(i,j),P_htf) ![kg/m3]
                mu_htf_node(i,j) = Viscosity(HTF,T_htfX(i,j),1.d0) ![Pa-s]
                Re_htf_node(i,j) = u_htf(j)*rho_htf_node(i,j)*D_tube_in/mu_htf_node(i,j)
                c_htf_node(i,j) = SpecHeat(HTF,T_htfX(i,j),1.d0)*1000.0 ![J/kg-K]
                k_htf_node(i,j) = Conductivity(HTF,T_htfX(i,j),0.d0) ![W/m-K]
                Pr_htf_node(i,j) = c_htf_node(i,j)*mu_htf_node(i,j)/k_htf_node(i,j) ![-]
        
                call PipeFlowCavity(Re_htf_node(i,j),Pr_htf_node(i,j),LoverD,relRough,q_solar(i,j),Nu_htf_node(i,j),f_htf_node(i,j))
                
                !The heat transfer coefficient for thermal energy transfer from the tube walls to the HTF
                h_htf_node(i,j) = Nu_htf_node(i,j)*k_htf_node(i,j)/D_tube_in
                !Calculate the resistance to convection from the tube walls to the HTF [K/W]
                R_conv_node(i,j)= 2.0/(h_htf_node(i,j)*L_tube_node*D_tube_in*pi*DBLE(N_tubes)) 
        
        
                !------------------------------------------------------------------------
        
        
                !Wall conductivity [W/m-K]
                k_tube(i,j) = Conductivity(Material,(T_sX(i,j)+T_htf_average)/2.0,1.d0)
                !The resistance to conduction of thermal energy through the tube walls [K/W]
                R_cond(i,j) = LOG(D_tube_out/D_tube_in)/(pi*L_tube_node*k_tube(i,j)*N_tubes)     !Removed "2.0*" in denominator per discussion with Lukas 10.5.2010
                !Receiver panel conductance, the inverse of the sum of the resistances to conduction and convection [W/K]
                !UA(i,j) = 1.0/(R_cond(i,j)+(R_conv(j)/N_nodes))                                 !ST add /N_nodes for pipe_wall <-> flow convection resistance
                UA(i,j) = 1.0/(R_cond(i,j)+R_conv_node(i,j))                                     !ST add /N_nodes for pipe_wall <-> flow convection resistance
            enddo
            
        enddo
    
        !-----------------------------------------------------------------------------------------------
        !Calculate the radiation heat transfer coefficients based on the calculated surface temperatures
        !-----------------------------------------------------------------------------------------------
   
        do k=1,N_panels
            do j=1,N_nodes
                flux_array(j+((k-1)*N_nodes))=solarflux(j,k)*1000.
                T_s_array(j+((k-1)*N_nodes))=T_s(j,k)
                T_sX_array(j+((k-1)*N_nodes))=T_sX(j,k)
                UA_array(j+((k-1)*N_nodes))=UA(j,k)
                T_htf_avg_array(j+((k-1)*N_nodes))=T_htf_avg(j,k)
            enddo
        enddo
    
        do k=(N_panels*N_nodes+1),(N_panels*N_nodes+4)
            flux_array(k)=0.
            UA_array(k)=0.
            T_htf_avg_array(k)=0.
        enddo   
    
        T_s_array(N_panels*N_nodes+1)=T_F
        T_s_array(N_panels*N_nodes+2)=T_CE
        T_s_array(N_panels*N_nodes+3)=T_L
        T_s_array(N_panels*N_nodes+4)=T_O

        T_sX_array=T_sX_mid_array           !ST mid temperature - successive relaxation iteration method



        if (Band_mode==2)then
            CALL FractionFunction(N_nodes,N_panels,N_band,T_sX_array,lambda_step_band,f_temp_band,f_solar_band) 
        endif
    
        if (Band_mode==1)then
            CALL FractionFunction_two_band(N_nodes,N_panels,T_sX_array,lambda_step,f_Thermal,f_Solar ) 
        endif



        do i=1,N_nodes*N_panels+4
            do j=1,N_nodes*N_panels+4
       
                !h_rad_semi_gray_therm represents thermal (temperature driven) radiation heat transfer between all surfaces in the cavity
                
                If (T_sX_array(i)==T_sX_array(j))then
                    h_rad_semi_gray_therm(i,j)=1.E-6
                else
                
                    !Two band - model
                    if (Band_mode == 1) then
                        h_rad_semi_gray_therm(i,j)=((e_therm_array(i)*sigma*F_hat_therm(i,j)*e_therm_array(j)*((1.-f_Thermal(i))*T_sX_array(i)**4.-(1.-f_Thermal(j))*T_sX_array(j)**4.))/(T_sX_array(i)-T_sX_array(j))+(e_solar_array(i)*sigma*F_hat_solar(i,j)*e_solar_array(j)*((f_Thermal(i))*T_sX_array(i)**4.-(f_Thermal(j))*T_sX_array(j)**4.))/(T_sX_array(i)-T_sX_array(j)))
                    endif
                
                    !Multiband - model
                    if (Band_mode == 2) then
                        h_rad_semi_gray_therm(i,j)=sigma/(A_array(i)*(T_sX_array(i)-T_sX_array(j)))*(  sum( f_temp_band(i,1:N_band)*e_band_array(i,1:N_band)*A_array(i)*T_sX_array(i)**4.*F_hat(i,j,1:N_band)*e_band_array(j,1:N_band) -  f_temp_band(j,1:N_band)*e_band_array(j,1:N_band)*A_array(j)*T_sX_array(j)**4.*F_hat(j,i,1:N_band)*e_band_array(i,1:N_band)  )  )
                    endif
                       
                endif
                 
                 
                !q_rad_solar represents solar (flux and reflected flux) radiation heat transfer between all surfaces in the cavity  
                
                !Two band - model
                if (Band_mode == 1) then
                    q_rad_solar(i,j)=f_Solar*F_hat_solar(i,j)*A_array(i)*((e_solar_array(j)*flux_array(i)*(1.-e_solar_array(i)))-(e_solar_array(i)*flux_array(j)*(1.-e_solar_array(j))))+(1.-f_Solar)*F_hat_therm(i,j)*A_array(i)*((e_therm_array(j)*flux_array(i)*(1.-e_therm_array(i)))-(e_therm_array(i)*flux_array(j)*(1.-e_therm_array(j))))
                endif
                
                !Multiband - model
                if (Band_mode == 2) then
                    q_rad_solar(i,j)=sum(  f_solar_band(1:N_band)*F_hat(i,j,1:N_band)*A_array(i)*(  ( flux_array(i)*(1-e_band_array(i,1:N_band))*e_band_array(j,1:N_band) )-( flux_array(j)*(1-e_band_array(j,1:N_band))*e_band_array(i,1:N_band) )  )  )
                endif
                
            enddo
        enddo 
 
 
        !net solar radiation heat transfer from each surface (positive if more energy leaves the surface as energy that is absorbed) 
        do i=1,N_panels*N_nodes+4
            q_rad_solar_net(i)=sum(q_rad_solar(i,1:N_panels*N_nodes+4))
        enddo
 
    
        !Calculation active heat exchanger surfaces  - APPLICABLE FOR ALL SURFACE (i=1,N_panels*N_nodes+4) IF CONVECTION AT ALL SURFACES IS DEPENDENT ON THE SURFACE TEMPERATURE
         do i=1,N_panels*N_nodes+3 !ST 1  
            !energy balance active surfaces
            T_sX_array(i)= (flux_array(i)*A_array(i) + UA_array(i)*T_htf_avg_array(i) + A_array(i)*sum( h_rad_semi_gray_therm(i,1:N_nodes*N_panels+4)*T_sX_array(1:N_nodes*N_panels+4))-q_rad_solar_net(i)+ A_array(i)*h_conv(i)*T_bulk)/( UA_array(i)+A_array(i)*sum( h_rad_semi_gray_therm(i,1:N_nodes*N_panels+4) )+A_array(i)*h_conv(i) )
            !heat transfer to the HTF at each active surface
            q_htf_array(i)=UA_array(i)*(T_sX_array(i)-T_htf_avg_array(i))
        enddo
  
  
        q_htf_array(N_panels*N_nodes+1)=0.
        q_htf_array(N_panels*N_nodes+2)=0.
        q_htf_array(N_panels*N_nodes+3)=0.
        q_htf_array(N_panels*N_nodes+4)=0.
  

        !thermal radiation losses are calculated using the NEW surface temperatures
        do i=1,N_nodes*N_panels+4
            do j=1,N_nodes*N_panels+4
        
                !Two band - model
                if (Band_mode == 1) then
                    q_rad_therm(i,j)=A_array(i)*e_therm_array(i)*sigma*F_hat_therm(i,j)*e_therm_array(j)*((1.-f_Thermal(i))*T_sX_array(i)**4.-(1.-f_Thermal(j))*T_sX_array(j)**4.)+A_array(i)*e_solar_array(i)*sigma*F_hat_solar(i,j)*e_solar_array(j)*(f_Thermal(i)*T_sX_array(i)**4.-f_Thermal(j)*T_sX_array(j)**4.)
                endif
                
                !Multi band - model
                if (Band_mode == 2) then
                    q_rad_therm(i,j)=sigma*(  sum( f_temp_band(i,1:N_band)*e_band_array(i,1:N_band)*A_array(i)*T_sX_array(i)**4.*F_hat(i,j,1:N_band)*e_band_array(j,1:N_band) -  f_temp_band(j,1:N_band)*e_band_array(j,1:N_band)*A_array(j)*T_sX_array(j)**4.*F_hat(j,i,1:N_band)*e_band_array(i,1:N_band)  )  )
                endif   
              
                q_rad_semi_gray(i,j)=q_rad_therm(i,j)+q_rad_solar(i,j)
                 
            enddo
        enddo
    
        !net radiation from each surface (positive if more energy leaves the surface as energy that is absorbed)
        do i=1,N_panels*N_nodes+4
            q_rad_therm_net(i)=sum(q_rad_therm(i,1:N_panels*N_nodes+4))
            q_rad_semi_gray_net(i)=q_rad_therm_net(i)+q_rad_solar_net(i)
        enddo
 
        !convection loss from each surface
        do i=1,N_panels*N_nodes+3 
            q_conv(i)=A_array(i)*h_conv(i)*(T_sX_array(i)-T_bulk)
        enddo
        q_conv(N_panels*N_nodes+4)=0.
 
        !total convection loss
        q_conv_tot=sum(q_conv(1:N_panels*N_nodes+4))


        !calculation of relaxed temperature for next iteration step
        do i=1,N_nodes*N_panels+4
            T_sX_mid_array(i)=gamma*T_sX_array(i)+(1-gamma)*T_s_array(i)
        enddo 
    
    
    
        !Outputs are converted into two dimensional arrays used throughout the program
        do k=1,N_panels
            do j=1,N_nodes
                T_s(j,k)=T_s_array(j+((k-1)*N_nodes))
                T_sX(j,k)=T_sX_array(j+((k-1)*N_nodes))
                UA(j,k)=UA_array(j+((k-1)*N_nodes))
                T_htf_avg(j,k)=T_htf_avg_array(j+((k-1)*N_nodes))
                q_htf(j,k)=q_htf_array(j+((k-1)*N_nodes))
            enddo
        enddo

        T_FX=T_sX_array(N_panels*N_nodes+1)
        T_CEX=T_sX_array(N_panels*N_nodes+2)
        T_LX=T_sX_array(N_panels*N_nodes+3)

    
        !This must be edited for FLOWPATTERNS 1 and 2   !ST ??? I did not change anything about the flow patterns - should probably be reviewed
        !Absorbed thermal energy by the HTF [W]
        q_htf_total = SUM(q_htf(1:N_nodes,1:N_panels)) 
    
    
        !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        !Calculate the increase of temperature in the working fluid with regards to the different
        !flow patterns 1 through 8.
        !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        select case(FlowPattern)
            case(1)
                do j=1,N_panels
                    !Absorbed thermal energy by the HTF in each panel [W]
                    q_htf_panel(j) = SUM(q_htf(1:N_nodes,j))
                    !Calculate the mass flow rates [kg/s]
                    m_htfX(j) = q_htf_panel(j)/(c_htf*(T_htf_hot-T_htf_cold))
                    
                    do i=1,N_nodes
                        !The heat transferred to the fluid increases the HTF temperature
                        T_htfX(i,j) = T_htfX(i-1,j)+ q_htf(i,j)/(m_htfX(j)*c_htf)
                        !Average node heat transfer temperature [K]
                        T_htf_avgX(i,j) =(T_htfX(i,j)+T_htfX(i-1,j))/2.0
                    enddo
                    !Update the panel outlet temperature [K]
                    T_htf_hotX(j) = T_htfX(N_nodes,j)
                    !if the mass flow rate is negligibly low it is set to zero in that specific panel
                  enddo
        
            case(2)
                do j=1,N_panels
                    !Absorbed thermal energy by the HTF in each panel [W]
                    q_htf_panel(j) = SUM(q_htf(1:N_nodes,j))
                    !Calculate the mass flow rates [kg/s]
                    m_htfX(j) = q_htf_panel(j)/(c_htf*(T_htf_hot-T_htf_cold))
                    
                    !Calculate the average HTF temperature for each node
                    do i=1,N_nodes
                        !The heat transferred to the fluid increases the HTF temperature
                        T_htfX(N_nodes-i,j) = T_htfX(N_nodes+1-i,j)+ q_htf(N_nodes+1-i,j)/(m_htfX(j)*c_htf)
                        !Average node heat transfer temperature [K]
                        T_htf_avgX(N_nodes+1-i,j)=(T_htfX(N_nodes+1-i,j)+T_htfX(N_nodes-i,j))/2.0
                    enddo
                    !Update the panel outlet temperature [K]
                    T_htf_hotX(j) = T_htfX(0,j)
                    !if the mass flow rate is negligibly low it is set to zero in that specific panel
                enddo
        
            case(3)
                !Calculate the mass flow rates [kg/s]
                m_htfX = q_htf_total/(c_htf*(T_htf_hot-T_htf_cold))
                
                !Calculate the average HTF temperature for each node
                do j=1,N_panels
                    do i=1,N_nodes
                    
                        if (j == 1) then
                            !The heat transferred to the fluid increases the HTF temperature
                            T_htfX(i,j) = T_htfX(i-1,j)+ q_htf(i,j)/(m_htfX(j)*c_htf)
                            !Average node heat transfer temperature [K]
                            T_htf_avgX(i,j) = (T_htfX(i,j)+ T_htfX(i-1,j))/2.0
                            
                        elseif(FLOOR(DBLE(j)/2.0) < (DBLE(j)/2.0))then
                            if (i == 1) then
                                !The heat transferred to the fluid increases the HTF temperature
                                T_htfX(i-1,j) = T_htfX(i-1,j-1)
                                T_htfX(i,j) = T_htfX(i-1,j)+ q_htf(i,j)/(m_htfX(j)*c_htf)
                                !Average node heat transfer temperature [K]
                                T_htf_avgX(i,j) = (T_htfX(i,j)+ T_htfX(i-1,j))/2.0
                            else
                                !The heat transferred to the fluid increases the HTF temperature
                                T_htfX(i,j) = T_htfX(i-1,j) + q_htf(i,j)/(m_htfX(j)*c_htf)
                                !Average node heat transfer temperature [K]
                                T_htf_avgX(i,j) = (T_htfX(i,j)+ T_htfX(i-1,j))/2.0
                            endif
                            
                        else
                            if (i == 1) then
                                !The heat transferred to the fluid increases the HTF temperature
                                T_htfX(N_nodes+1-i,j) = T_htfX(N_nodes+1-i,j-1)
                                T_htfX(N_nodes-i,j) = T_htfX(N_nodes+1-i,j)+ q_htf(N_nodes+1-i,j)/(m_htfX(j)*c_htf)
                                !Average node heat transfer temperature [K]
                                T_htf_avgX(N_nodes+1-i,j) = (T_htfX(N_nodes+1-i,j)+ T_htfX(N_nodes-i,j))/2.0
                            else
                                !The heat transferred to the fluid increases the HTF temperature
                                T_htfX(N_nodes-i,j) = T_htfX(N_nodes+1-i,j)+ q_htf(N_nodes+1-i,j)/(m_htfX(j)*c_htf)
                                !Average node heat transfer temperature [K]
                                T_htf_avgX(N_nodes+1-i,j) = (T_htfX(N_nodes+1-i,j)+ T_htfX(N_nodes-i,j))/2.0
                            endif
                       
                        endif
                        
                    enddo
                 enddo
                 
                !Update the panel outlet temperature [K]
                T_htf_hotX = T_htfX(0,N_panels)
        
            case(4)
                !Calculate the mass flow rates [kg/s]
                m_htfX = q_htf_total/(c_htf*(T_htf_hot-T_htf_cold))
                
                !Calculate the average HTF temperature for each node
                do j=1,N_panels
                    do i=1,N_nodes
                        if (j == 1) then
                            !The heat transferred to the fluid increases the HTF temperature
                            T_htfX(N_nodes-i,j) = T_htfX(N_nodes+1-i,j)+ q_htf(N_nodes+1-i,j)/(m_htfX(j)*c_htf)
                            !Average node heat transfer temperature [K]
                            T_htf_avgX(N_nodes+1-i,j) = (T_htfX(N_nodes-i,j)+ T_htfX(N_nodes+1-i,j))/2.0
                            
                        elseif(FLOOR(DBLE(j)/2.0) < (DBLE(j)/2.0))then
                            if (i == 1) then
                               !The heat transferred to the fluid increases the HTF temperature
                                T_htfX(N_nodes+1-i,j) = T_htfX(N_nodes+1-i,j-1)
                                T_htfX(N_nodes-i,j) = T_htfX(N_nodes+1-i,j)+ q_htf(N_nodes+1-i,j)/(m_htfX(j)*c_htf)
                                !Average node heat transfer temperature [K]
                                T_htf_avgX(N_nodes+1-i,j) = (T_htfX(N_nodes+1-i,j)+ T_htfX(N_nodes-i,j))/2.0
                            else
                                !The heat transferred to the fluid increases the HTF temperature
                                T_htfX(N_nodes-i,j) = T_htfX(N_nodes+1-i,j)+ q_htf(N_nodes+1-i,j)/(m_htfX(j)*c_htf)
                                !Average node heat transfer temperature [K]
                                T_htf_avgX(N_nodes+1-i,j) = (T_htfX(N_nodes+1-i,j)+ T_htfX(N_nodes-i,j))/2.0
                            endif
                            
                        else
                            if (i == 1) then
                            !The heat transferred to the fluid increases the HTF temperature
                            T_htfX(i-1,j) = T_htfX(i-1,j-1)
                            T_htfX(i,j) = T_htfX(i-1,j)+ q_htf(i,j)/(m_htfX(j)*c_htf)
                            !Average node heat transfer temperature [K]
                            T_htf_avgX(i,j) = (T_htfX(i,j)+ T_htfX(i-1,j))/2.0
                            
                            else
                            !The heat transferred to the fluid increases the HTF temperature
                            T_htfX(i,j) = T_htfX(i-1,j) + q_htf(i,j)/(m_htfX(j)*c_htf)
                            !Average node heat transfer temperature [K]
                            T_htf_avgX(i,j) = (T_htfX(i,j)+ T_htfX(i-1,j))/2.0
                            
                            endif
                        endif
                    enddo
                enddo
                
                !Update the panel outlet temperature [K]
                T_htf_hotX = T_htfX(N_nodes,N_panels)
        
            case(5)
                do j=1,N_panels
                    !Absorbed thermal energy by the HTF in each panel [W]
                    q_htf_panel(j) = SUM(q_htf(1:N_nodes,j))
                enddo
                
                !Calculate the mass flow rates [kg/s]
                m_htfX(1) = SUM(q_htf_panel(1:N_panels/2))/(c_htf*(T_htf_hot-T_htf_cold))
                m_htfX(2) = SUM(q_htf_panel(N_panels/2+1:N_panels))/(c_htf*(T_htf_hot-T_htf_cold))
        
                !Calculate the average HTF temperature for each node
                do j=1,N_panels/2
                    do i=1,N_nodes
                        if (j == 1) then
                            !The heat transferred to the fluid increases the HTF temperature
                            !Outer left panel
                            T_htfX(i,j) = T_htfX(i-1,j)+ q_htf(i,j)/(m_htfX(1)*c_htf)
                            !Outer right panel
                            T_htfX(i,N_panels+1-j) = T_htfX(i-1,N_panels+1-j)+ q_htf(i,N_panels+1-j)/(m_htfX(2)*c_htf)
                            !Average node heat transfer temperature [K]
                            T_htf_avgX(i,j) = (T_htfX(i,j)+ T_htfX(i-1,j))/2.0
                            T_htf_avgX(i,N_panels+1-j) = (T_htfX(i,N_panels+1-j)+ T_htfX(i-1,N_panels+1-j))/2.0
                        elseif(FLOOR(DBLE(j)/2.0) < (DBLE(j)/2.0))then
                            if (i == 1) then
                                !The heat transferred to the fluid increases the HTF temperature
                                T_htfX(i-1,j) = T_htfX(i-1,j-1)
                                T_htfX(i-1,N_panels+1-j) = T_htfX(i-1,N_panels+2-j)
                                T_htfX(i,j) = T_htfX(i-1,j)+ q_htf(i,j)/(m_htfX(1)*c_htf)
                                T_htfX(i,N_panels+1-j) = T_htfX(i-1,N_panels+1-j)+ q_htf(i,N_panels+1-j)/(m_htfX(2)*c_htf)
                                !Average node heat transfer temperature [K]
                                T_htf_avgX(i,j) = (T_htfX(i,j)+ T_htfX(i-1,j))/2.0
                                T_htf_avgX(i,N_panels+1-j) = (T_htfX(i,N_panels+1-j)+ T_htfX(i-1,N_panels+1-j))/2.0
                            else
                                !The heat transferred to the fluid increases the HTF temperature
                                T_htfX(i,j) = T_htfX(i-1,j) + q_htf(i,j)/(m_htfX(1)*c_htf)
                                T_htfX(i,N_panels+1-j) = T_htfX(i-1,N_panels+1-j)+ q_htf(i,N_panels+1-j)/(m_htfX(2)*c_htf)
                                !Average node heat transfer temperature [K]
                                T_htf_avgX(i,j) = (T_htfX(i,j)+ T_htfX(i-1,j))/2.0
                                T_htf_avgX(i,N_panels+1-j) = (T_htfX(i,N_panels+1-j)+ T_htfX(i-1,N_panels+1-j))/2.0
                            endif
                            
                        else
                            if (i == 1) then
                                !The heat transferred to the fluid increases the HTF temperature
                                T_htfX(N_nodes+1-i,j) = T_htfX(N_nodes+1-i,j-1)
                                T_htfX(N_nodes+1-i,N_panels+1-j) =T_htfX(N_nodes+1-i,N_panels+2-j)
                                T_htfX(N_nodes-i,j) = T_htfX(N_nodes+1-i,j)+ q_htf(N_nodes+1-i,j)/(m_htfX(1)*c_htf)
                                T_htfX(N_nodes-i,N_panels+1-j) =T_htfX(N_nodes+1-i,N_panels+1-j)+ q_htf(N_nodes+1-i,N_panels+1-j)/(m_htfX(2)*c_htf)
                                !Average node heat transfer temperature [K]
                                T_htf_avgX(N_nodes+1-i,j) = (T_htfX(N_nodes+1-i,j)+ T_htfX(N_nodes-i,j))/2.0
                                T_htf_avgX(N_nodes+1-i,N_panels+1-j) =(T_htfX(N_nodes+1-i,N_panels+1-j)+ T_htfX(N_nodes-i,N_panels+1-j))/2.0
                            else
                                !The heat transferred to the fluid increases the HTF temperature
                                T_htfX(N_nodes-i,j) = T_htfX(N_nodes+1-i,j)+ q_htf(N_nodes+1-i,j)/(m_htfX(1)*c_htf)
                                T_htfX(N_nodes-i,N_panels+1-j) =T_htfX(N_nodes+1-i,N_panels+1-j)+ q_htf(N_nodes+1-i,N_panels+1-j)/(m_htfX(2)*c_htf)
                                !Average node heat transfer temperature [K]
                                T_htf_avgX(N_nodes+1-i,j) = (T_htfX(N_nodes+1-i,j)+ T_htfX(N_nodes-i,j))/2.0
                                T_htf_avgX(N_nodes+1-i,N_panels+1-j) =(T_htfX(N_nodes+1-i,N_panels+1-j)+ T_htfX(N_nodes-i,N_panels+1-j))/2.0
                            endif
                        
                        endif
                    enddo
                enddo   
                
                !Hot coolant outlet temperature
                if (FLOOR(DBLE(N_panels/2)/2.0) < (DBLE(N_panels/2)/2.0))then
                    T_htf_hotX(1) = T_htfX(N_nodes,N_panels/2)
                    T_htf_hotX(2) = T_htfX(N_nodes,N_panels/2+1)
                else
                    T_htf_hotX(1) = T_htfX(0,N_panels/2)
                    T_htf_hotX(2) = T_htfX(0,N_panels/2+1)
                endif
        
            case(6)
                do j=1,N_panels
                    !Absorbed thermal energy by the HTF in each panel [W]
                    q_htf_panel(j) = SUM(q_htf(1:N_nodes,j))
                enddo
                
                !Calculate the mass flow rates [kg/s]
                m_htfX(1) = SUM(q_htf_panel(1:N_panels/2))/(c_htf*(T_htf_hot-T_htf_cold))
                m_htfX(2) = SUM(q_htf_panel(N_panels/2+1:N_panels))/(c_htf*(T_htf_hot-T_htf_cold))
                
                !Calculate the average HTF temperature for each node
                do j=1,N_panels/2
                    do i=1,N_nodes
                        if (j == 1) then
                            !The heat transferred to the fluid increases the HTF temperature
                            !Outer left panel
                            T_htfX(N_nodes-i,j) = T_htfX(N_nodes+1-i,j)+ q_htf(N_nodes+1-i,j)/(m_htfX(1)*c_htf)
                            !Outer right panel
                            T_htfX(N_nodes-i,N_panels+1-j) =T_htfX(N_nodes+1-i,N_panels+1-j)+ q_htf(N_nodes+1-i,N_panels+1-j)/(m_htfX(2)*c_htf)
                            !Average node heat transfer temperature [K]
                            T_htf_avgX(N_nodes+1-i,j) = (T_htfX(N_nodes+1-i,j)+ T_htfX(N_nodes-i,j))/2.0
                            T_htf_avgX(N_nodes+1-i,N_panels+1-j) =(T_htfX(N_nodes+1-i,N_panels+1-j)+ T_htfX(N_nodes-i,N_panels+1-j))/2.0
                        elseif(FLOOR(DBLE(j)/2.0) < (DBLE(j)/2.0))then
                            if (i == 1) then
                                !The heat transferred to the fluid increases the HTF temperature
                                T_htfX(N_nodes+1-i,j) = T_htfX(N_nodes+1-i,j-1)
                                T_htfX(N_nodes+1-i,N_panels+1-j) =T_htfX(N_nodes+1-i,N_panels+2-j)
                                T_htfX(N_nodes-i,j) = T_htfX(N_nodes+1-i,j)+ q_htf(N_nodes+1-i,j)/(m_htfX(1)*c_htf)
                                T_htfX(N_nodes-i,N_panels+1-j) =T_htfX(N_nodes+1-i,N_panels+1-j)+ q_htf(N_nodes+1-i,N_panels+1-j)/(m_htfX(2)*c_htf)
                                !Average node heat transfer temperature [K]
                                T_htf_avgX(N_nodes+1-i,j) = (T_htfX(N_nodes+1-i,j)+ T_htfX(N_nodes-i,j))/2.0
                                T_htf_avgX(N_nodes+1-i,N_panels+1-j) =(T_htfX(N_nodes+1-i,N_panels+1-j)+ T_htfX(N_nodes-i,N_panels+1-j))/2.0
                            else
                                !The heat transferred to the fluid increases the HTF temperature
                                T_htfX(N_nodes-i,j) = T_htfX(N_nodes+1-i,j)+ q_htf(N_nodes+1-i,j)/(m_htfX(1)*c_htf)
                                T_htfX(N_nodes-i,N_panels+1-j) =T_htfX(N_nodes+1-i,N_panels+1-j)+ q_htf(N_nodes+1-i,N_panels+1-j)/(m_htfX(2)*c_htf)
                                !Average node heat transfer temperature [K]
                                T_htf_avgX(N_nodes+1-i,j) = (T_htfX(N_nodes+1-i,j)+ T_htfX(N_nodes-i,j))/2.0
                                T_htf_avgX(N_nodes+1-i,N_panels+1-j) =(T_htfX(N_nodes+1-i,N_panels+1-j)+ T_htfX(N_nodes-i,N_panels+1-j))/2.0
                            endif
                        else
                            if (i == 1) then
                                !The heat transferred to the fluid increases the HTF temperature
                                T_htfX(i-1,j) = T_htfX(i-1,j-1)
                                T_htfX(i-1,N_panels+1-j) = T_htfX(i-1,N_panels+2-j)
                                T_htfX(i,j) = T_htfX(i-1,j)+ q_htf(i,j)/(m_htfX(1)*c_htf)
                                T_htfX(i,N_panels+1-j) = T_htfX(i-1,N_panels+1-j)+ q_htf(i,N_panels+1-j)/(m_htfX(2)*c_htf)
                                !Average node heat transfer temperature [K]
                                T_htf_avgX(i,j) = (T_htfX(i,j)+ T_htfX(i-1,j))/2.0
                                T_htf_avgX(i,N_panels+1-j) = (T_htfX(i,N_panels+1-j)+ T_htfX(i-1,N_panels+1-j))/2.0
                            else
                                !The heat transferred to the fluid increases the HTF temperature
                                T_htfX(i,j) = T_htfX(i-1,j) + q_htf(i,j)/(m_htfX(1)*c_htf)
                                T_htfX(i,N_panels+1-j) = T_htfX(i-1,N_panels+1-j)+ q_htf(i,N_panels+1-j)/(m_htfX(2)*c_htf)
                                !Average node heat transfer temperature [K]
                                T_htf_avgX(i,j) = (T_htfX(i,j)+ T_htfX(i-1,j))/2.0
                                T_htf_avgX(i,N_panels+1-j) = (T_htfX(i,N_panels+1-j)+ T_htfX(i-1,N_panels+1-j))/2.0
                            endif
                        endif
                    enddo
                enddo
                !Hot coolant outlet temperature
                if (FLOOR(DBLE(N_panels/2)/2.0) < (DBLE(N_panels/2)/2.0))then
                    T_htf_hotX(1) = T_htfX(0,N_panels/2)
                    T_htf_hotX(2) = T_htfX(0,N_panels/2+1)
                else
                    T_htf_hotX(1) = T_htfX(N_nodes,N_panels/2)
                    T_htf_hotX(2) = T_htfX(N_nodes,N_panels/2+1)
                endif
                
            case(7)
                do j=1,N_panels
                    !Absorbed thermal energy by the HTF in each panel [W]
                    q_htf_panel(j) = SUM(q_htf(1:N_nodes,j))
                enddo
                !Calculate the mass flow rates [kg/s]
                m_htfX(1) = SUM(q_htf_panel(1:N_panels/2))/(c_htf*(T_htf_hot-T_htf_cold))
                m_htfX(2) = SUM(q_htf_panel(N_panels/2+1:N_panels))/(c_htf*(T_htf_hot-T_htf_cold))
                !Calculate the average HTF temperature for each node
                do j=1,N_panels/2
                    do i=1,N_nodes
                        if (j == 1) then
                            !The heat transferred to the fluid increases the HTF temperature
                            !Inner left panel
                            T_htfX(i,N_panels/2+1-j) =T_htfX(i-1,N_panels/2+1-j)+ q_htf(i,N_panels/2+1-j)/(m_htfX(1)*c_htf)
                            !Inner right panel
                            T_htfX(i,N_panels/2+j) =T_htfX(i-1,N_panels/2+j)+ q_htf(i,N_panels/2+j)/(m_htfX(2)*c_htf)
                            !Average node heat transfer temperature [K]
                            T_htf_avgX(i,N_panels/2+1-j) =(T_htfX(i,N_panels/2+1-j)+ T_htfX(i-1,N_panels/2+1-j))/2.0
                            T_htf_avgX(i,N_panels/2+j) =(T_htfX(i,N_panels/2+j)+ T_htfX(i-1,N_panels/2+j))/2.0
                        elseif(FLOOR(DBLE(j)/2.0) < (DBLE(j)/2.0))then
                            if (i == 1) then
                                !The heat transferred to the fluid increases the HTF temperature
                                T_htfX(i-1,N_panels/2+1-j) =T_htfX(i-1,N_panels/2+2-j)
                                T_htfX(i-1,N_panels/2+j) =T_htfX(i-1,N_panels/2-1+j)
                                T_htfX(i,N_panels/2+1-j) =T_htfX(i-1,N_panels/2+1-j)+ q_htf(i,N_panels/2+1-j)/(m_htfX(1)*c_htf)
                                T_htfX(i,N_panels/2+j) = T_htfX(i-1,N_panels/2+j)+ q_htf(i,N_panels/2+j)/(m_htfX(2)*c_htf)
                                !Average node heat transfer temperature [K]
                                T_htf_avgX(i,N_panels/2+1-j) =(T_htfX(i,N_panels/2+1-j)+ T_htfX(i-1,N_panels/2+1-j))/2.0
                                T_htf_avgX(i,N_panels/2+j) =(T_htfX(i,N_panels/2+j)+ T_htfX(i-1,N_panels/2+j))/2.0
                            else
                                !The heat transferred to the fluid increases the HTF temperature
                                T_htfX(i,N_panels/2+1-j) =T_htfX(i-1,N_panels/2+1-j)+ q_htf(i,N_panels/2+1-j)/(m_htfX(1)*c_htf)
                                T_htfX(i,N_panels/2+j) = T_htfX(i-1,N_panels/2+j)+ q_htf(i,N_panels/2+j)/(m_htfX(2)*c_htf)
                                !Average node heat transfer temperature [K]
                                T_htf_avgX(i,N_panels/2+1-j) =(T_htfX(i,N_panels/2+1-j)+ T_htfX(i-1,N_panels/2+1-j))/2.0
                                T_htf_avgX(i,N_panels/2+j) =(T_htfX(i,N_panels/2+j)+ T_htfX(i-1,N_panels/2+j))/2.0
                            endif
                        else
                            if (i == 1) then
                                !The heat transferred to the fluid increases the HTF temperature
                                T_htfX(N_nodes+1-i,N_panels/2+1-j) =T_htfX(N_nodes+1-i,N_panels/2+2-j)
                                T_htfX(N_nodes+1-i,N_panels/2+j) =T_htfX(N_nodes+1-i,N_panels/2-1+j)
                                T_htfX(N_nodes-i,N_panels/2+1-j) =T_htfX(N_nodes+1-i,N_panels/2+1-j)+ q_htf(N_nodes+1-i,N_panels/2+1-j)/(m_htfX(1)*c_htf)
                                T_htfX(N_nodes-i,N_panels/2+j) =T_htfX(N_nodes+1-i,N_panels/2+j)+ q_htf(N_nodes+1-i,N_panels/2+j)/(m_htfX(2)*c_htf)
                                !Average node heat transfer temperature [K]
                                T_htf_avgX(N_nodes+1-i,N_panels/2+1-j) =(T_htfX(N_nodes-i,N_panels/2+1-j)+ T_htfX(N_nodes+1-i,N_panels/2+1-j))/2.0
                                T_htf_avgX(N_nodes+1-i,N_panels/2+j) =(T_htfX(N_nodes-i,N_panels/2+j)+ T_htfX(N_nodes+1-i,N_panels/2+j))/2.0
                            else
                                !The heat transferred to the fluid increases the HTF temperature
                                T_htfX(N_nodes-i,N_panels/2+1-j) =T_htfX(N_nodes+1-i,N_panels/2+1-j)+ q_htf(N_nodes+1-i,N_panels/2+1-j)/(m_htfX(1)*c_htf)
                                T_htfX(N_nodes-i,N_panels/2+j) =T_htfX(N_nodes+1-i,N_panels/2+j)+ q_htf(N_nodes+1-i,N_panels/2+j)/(m_htfX(2)*c_htf)
                                !Average node heat transfer temperature [K]
                                T_htf_avgX(N_nodes+1-i,N_panels/2+1-j) =(T_htfX(N_nodes-i,N_panels/2+1-j)+ T_htfX(N_nodes+1-i,N_panels/2+1-j))/2.0
                                T_htf_avgX(N_nodes+1-i,N_panels/2+j) =(T_htfX(N_nodes-i,N_panels/2+j)+ T_htfX(N_nodes+1-i,N_panels/2+j))/2.0
                            endif
                        endif
                    enddo
                enddo
                !Hot coolant outlet temperature
                if (FLOOR(DBLE(N_panels/2)/2.0) < (DBLE(N_panels/2)/2.0))then
                    T_htf_hotX(1) = T_htfX(N_nodes,1)
                    T_htf_hotX(2) = T_htfX(N_nodes,N_panels)
                else
                    T_htf_hotX(1) = T_htfX(0,1)
                    T_htf_hotX(2) = T_htfX(0,N_panels)
                endif
            
            case(8)
                do j=1,N_panels
                    !Absorbed thermal energy by the HTF in each panel [W]
                    q_htf_panel(j) = SUM(q_htf(1:N_nodes,j))
                enddo
                !Calculate the mass flow rates [kg/s]
                m_htfX(1) = SUM(q_htf_panel(1:N_panels/2))/(c_htf*(T_htf_hot-T_htf_cold))
                m_htfX(2) = SUM(q_htf_panel(N_panels/2+1:N_panels))/(c_htf*(T_htf_hot-T_htf_cold))
                
                !Calculate the average HTF temperature for each node
                do j=1,N_panels/2
                    do i=1,N_nodes
                        if (j == 1) then
                            !The heat transferred to the fluid increases the HTF temperature
                            T_htfX(N_nodes-i,N_panels/2+1-j) =T_htfX(N_nodes+1-i,N_panels/2+1-j)+ q_htf(N_nodes+1-i,N_panels/2+1-j)/(m_htfX(1)*c_htf)
                            T_htfX(N_nodes-i,N_panels/2+j) =T_htfX(N_nodes+1-i,N_panels/2+j)+ q_htf(N_nodes+1-i,N_panels/2+j)/(m_htfX(2)*c_htf)
                            !Average node heat transfer temperature [K]
                            T_htf_avgX(N_nodes+1-i,N_panels/2+1-j) =(T_htfX(N_nodes-i,N_panels/2+1-j)+ T_htfX(N_nodes+1-i,N_panels/2+1-j))/2.0
                            T_htf_avgX(N_nodes+1-i,N_panels/2+j) =(T_htfX(N_nodes-i,N_panels/2+j)+ T_htfX(N_nodes+1-i,N_panels/2+j))/2.0
                        elseif(FLOOR(DBLE(j)/2.0) < (DBLE(j)/2.0))then
                            if (i == 1) then
                                !The heat transferred to the fluid increases the HTF temperature
                                T_htfX(N_nodes+1-i,N_panels/2+1-j) =T_htfX(N_nodes+1-i,N_panels/2+2-j)
                                T_htfX(N_nodes+1-i,N_panels/2+j) =T_htfX(N_nodes+1-i,N_panels/2-1+j)
                                T_htfX(N_nodes-i,N_panels/2+1-j) =T_htfX(N_nodes+1-i,N_panels/2+1-j)+ q_htf(N_nodes+1-i,N_panels/2+1-j)/(m_htfX(1)*c_htf)
                                T_htfX(N_nodes-i,N_panels/2+j) =T_htfX(N_nodes+1-i,N_panels/2+j)+ q_htf(N_nodes+1-i,N_panels/2+j)/(m_htfX(2)*c_htf)
                                !Average node heat transfer temperature [K]
                                T_htf_avgX(N_nodes+1-i,N_panels/2+1-j) =(T_htfX(N_nodes-i,N_panels/2+1-j)+ T_htfX(N_nodes+1-i,N_panels/2+1-j))/2.0
                                T_htf_avgX(N_nodes+1-i,N_panels/2+j) =(T_htfX(N_nodes-i,N_panels/2+j)+ T_htfX(N_nodes+1-i,N_panels/2+j))/2.0
                            else
                                !The heat transferred to the fluid increases the HTF temperature
                                T_htfX(N_nodes-i,N_panels/2+1-j) =T_htfX(N_nodes+1-i,N_panels/2+1-j)+ q_htf(N_nodes+1-i,N_panels/2+1-j)/(m_htfX(1)*c_htf)
                                T_htfX(N_nodes-i,N_panels/2+j) =T_htfX(N_nodes+1-i,N_panels/2+j)+ q_htf(N_nodes+1-i,N_panels/2+j)/(m_htfX(2)*c_htf)
                                !Average node heat transfer temperature [K]
                                T_htf_avgX(N_nodes+1-i,N_panels/2+1-j) =(T_htfX(N_nodes-i,N_panels/2+1-j)+ T_htfX(N_nodes+1-i,N_panels/2+1-j))/2.0
                                T_htf_avgX(N_nodes+1-i,N_panels/2+j) =(T_htfX(N_nodes-i,N_panels/2+j)+ T_htfX(N_nodes+1-i,N_panels/2+j))/2.0
                            endif
                        else
                            if (i == 1) then
                                !The heat transferred to the fluid increases the HTF temperature
                                T_htfX(i-1,N_panels/2+1-j) =T_htfX(i-1,N_panels/2+2-j)
                                T_htfX(i-1,N_panels/2+j) =T_htfX(i-1,N_panels/2-1+j)
                                T_htfX(i,N_panels/2+1-j) =T_htfX(i-1,N_panels/2+1-j)+ q_htf(i,N_panels/2+1-j)/(m_htfX(1)*c_htf)
                                T_htfX(i,N_panels/2+j) = T_htfX(i-1,N_panels/2+j)+ q_htf(i,N_panels/2+j)/(m_htfX(2)*c_htf)
                                !Average node heat transfer temperature [K]
                                T_htf_avgX(i,N_panels/2+1-j) =(T_htfX(i,N_panels/2+1-j)+ T_htfX(i-1,N_panels/2+1-j))/2.0
                                T_htf_avgX(i,N_panels/2+j) =(T_htfX(i,N_panels/2+j)+ T_htfX(i-1,N_panels/2+j))/2.0
                            else
                                !The heat transferred to the fluid increases the HTF temperature
                                T_htfX(i,N_panels/2+1-j) =T_htfX(i-1,N_panels/2+1-j)+ q_htf(i,N_panels/2+1-j)/(m_htfX(1)*c_htf)
                                T_htfX(i,N_panels/2+j) = T_htfX(i-1,N_panels/2+j)+ q_htf(i,N_panels/2+j)/(m_htfX(2)*c_htf)
                                !Average node heat transfer temperature [K]
                                T_htf_avgX(i,N_panels/2+1-j) =(T_htfX(i,N_panels/2+1-j)+ T_htfX(i-1,N_panels/2+1-j))/2.0
                                T_htf_avgX(i,N_panels/2+j) =(T_htfX(i,N_panels/2+j)+ T_htfX(i-1,N_panels/2+j))/2.0
                            endif
                        endif
                    enddo
                enddo
                !Hot coolant outlet temperature
                if (FLOOR(DBLE(N_panels/2)/2.0) < (DBLE(N_panels/2)/2.0))then
                    T_htf_hotX(1) = T_htfX(0,1)
                    T_htf_hotX(2) = T_htfX(0,N_panels)
                else
                    T_htf_hotX(1) = T_htfX(N_nodes,1)
                    T_htf_hotX(2) = T_htfX(N_nodes,N_panels)
                endif
        end select
                                
    
        do k=1,N_panels
            do j=1,N_nodes
                T_HTFX_mid(j,k)=gamma*T_htfX(j,k)+(1-gamma)*T_htf(j,k)
            enddo 
        enddo
    
        
        !Check convergence
        error_temp = 0.0
        error_flow = 0.0
        select case(FlowPattern)
            case(1:2)
                do j=1,N_panels
                    !Check if panel has a significant mass flow rate
                    if (m_htf(j) == 0.0) then
                        !don't include the inactive panels in the convergence calculation
                        error_temp(j) = 0.0
                        error_flow(j) = 0.0
                    else
                        !Temperature error for each flow path
                        error_temp(j) = ABS((T_htf_hotX(j)- T_htf_hot)/T_htf_hot)
                        error_flow(j)= ABS(ABS(m_htf(j)-m_htfX(j))/m_htfX(j))
                    endif
                enddo
                !Accumulated error
                errorsum_temp = maxval(error_temp)
                errorsum_flow = maxval(error_flow)
                
            case(3:4)
                errorsum_temp = ABS((T_htf_hotX(1)-T_htf_hot)/T_htf_hot)
                errorsum_flow = ABS(ABS(m_htf(1)-m_htfX(1))/m_htfX(1))
    
            case(5:8)
                do j=1,2
                    !Temperature error for each flow path
                    error_temp(j) = ABS((T_htf_hotX(j)-T_htf_hot)/T_htf_hot)
                    error_flow(j) = ABS(ABS(m_htf(j)-m_htfX(j))/m_htfX(j))
                enddo
                !Accumulated error
                errorsum_temp = maxval(error_temp)
                errorsum_flow = maxval(error_flow)
        end select
    
        !Set error tolerance
        if (NightRecirculation == 1.) then
            tolerance =  5.0e-4
        else                  
            tolerance =  1.0e-8    
        endif
    
    
    
        !Set the variables equal to their newly calculated guess values
        T_s_array=T_sX_array
        T_s = T_sX
        T_htf = T_htfX
        T_htf_avg = T_htf_avgX
        m_htf = m_htfX
        T_F = T_FX
        T_CE = T_CEX
        T_L = T_LX
    
    
   
        !if the problem fails to converge after the maximum number of iterations, 
        !the relaxation factor is reduced, the calculation restarted to obtain convergence
        
        !if the program fails to convege for the smallest convergence factor
        !then the total power on the receiver is likely to be negligible
        !and the mass flow rate and power outputs will be set to zero. The temperatures are set to the cold HTF temp

    
        if ((qq > qq_max).and.(gamma<0.1)) then
            mode = 0.d0
            goto 900
        endif
    
    
        if (qq > qq_max) then
            gamma_count=gamma_count+1  
            gamma=gamma_calc_array(gamma_count)
            goto 222    !restarts the qq-loop with a smaller relaxation factor
        endif   
    
     
        !Total convergence criterium
        if (errorsum_temp < tolerance.AND.errorsum_flow < tolerance) EXIT

    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    !Terminate the internal iteration (qq) loop - surface & HTF temperatures, radiation losses and Q_HTF are determined
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    enddo
    


    !Calculate the total radiation losses out of the aperture [W]

    Q_radiation_loss = ABS(q_rad_semi_gray_net(N_panels*N_nodes+4))

    Q_radiation_loss_solar=abs( q_rad_solar_net(N_panels*N_nodes+4) )
    Q_radiation_loss_therm=abs( q_rad_therm_net(N_panels*N_nodes+4) )
    
    Q_radiation_loss_semi_sum =Q_radiation_loss_therm+Q_radiation_loss_solar


    !ST area averaged surface temperature determined
    T_s_avg = (SUM(T_s(1:N_nodes,1:N_panels)*A_node)+T_F*A_F+T_CE*A_F+T_L*A_L)/(2*A_F+A_L+N_nodes*N_panels*A_node) 
    A_cavity=N_nodes*N_panels*A_node+2.*A_F+A_L

    q_convectionX=q_convection !ST Coupled convection


    !ST Clausing1983 is called to determine T_bulk
    CALL ConvectionClausing1983(N_nodes,N_panels,T_s,T_F,T_CE,T_L,T_amb,P_amb,H_rec,H_lip,R_rec,alpha,W_panel,A_node,A_F,A_O,Q_radiation_loss,q_convection_Clausing1983,h_F,h_avg,h_stag,T_stag,T_bulk,S)

    !Calculate the total natural convection heat losses out of the aperture [W]
    if (Convection_Model==1) then !Clausing 1983
        q_convection=q_convection_Clausing1983
        h_clausing1983=q_convection_Clausing1983/(A_cavity*(T_s_avg-T_amb))
    
    elseif (Convection_Model==2) then !Clausing 1987
        call ConvectionClausing1987(N_nodes,N_panels,T_s,T_F,T_amb,P_amb,H_rec,H_lip,W_panel,A_F,A_O,q_convection_Clausing1987)
        q_convection=q_convection_Clausing1987
        h_clausing1987=q_convection_Clausing1987/(A_cavity*(T_s_avg-T_amb))
    
    elseif (Convection_Model==3) then !SiebersAndKraabel
        CALL ConvectionSiebersAndKraabel(N_nodes,N_panels,W_panel,T_s,T_F,T_CE,T_L,T_amb,P_amb,H_rec,H_lip,A_F,A_L,A_O,q_convection_SiebersAndKraabel)    
        q_convection=q_convection_SiebersAndKraabel
        h_SiebersAndKraabel=q_convection_SiebersAndKraabel/(A_cavity*(T_s_avg-T_amb))
    
    elseif (Convection_Model==4) then !Forced_Convection_Flat_Plates (FCFP) - upper bound of natural convection loss
        call Forced_Combined_Flat_Plates(N_nodes,N_panels,T_s,T_F,T_CE,T_L,T_amb,T_bulk,P_amb,H_rec,H_lip,R_rec,W_panel,A_node,A_F,A_L,A_O,h_CFP_floor_turb,h_CFP_ceiling_turb,h_CFP_panels_turb,h_CFP_lip_turb,h_CFP,q_convection_FCFP)
        q_convection=q_convection_FCFP
   
    elseif (Convection_Model==5) then !Natural_Convection_Flat_Plates (NCFP)
        CALL Natural_Combined_Flat_Plates(N_nodes,N_panels,T_s,T_F,T_CE,T_L,T_amb,T_bulk,P_amb,H_rec,H_lip,R_rec,W_panel,W_aperture,A_node, A_F,A_L,A_O,h_CFP_x_turb,q_convection_NCFP )
        q_convection=q_convection_NCFP  
    
    elseif (Convection_Model==6) then !LeibAndOrth
        CALL LeibAndOrth(N_nodes,N_panels,T_s,T_F,T_CE,T_L,T_amb,P_amb,H_rec,R_rec,A_node,A_F,A_L,A_O, h_LeibAndOrth, q_convection_LeibAndOrth)
        q_convection=q_convection_LeibAndOrth   
   
    elseif (Convection_Model==7) then !Pait and Lovegrove
        CALL PaitANDLove(N_nodes,N_panels,T_s,T_F,T_CE,T_L,T_amb,T_bulk,P_amb,H_rec,H_lip,R_rec,h_PaitANDLove,q_convection_PaitANDLove)
        q_convection=q_convection_PaitANDLove  

    endif


    if (forced_convection==1)then
        call Forced_ConvectionSiebersAndKraabel(THT ,h_wind_measurement, u_wind, Deg_wind, T_s_array, T_amb, P_amb, H_rec, H_lip, W_panel, wind_direction_dependence, h_SK_forced, u_wind_tower)
    else
        h_SK_forced=0.
    endif


    !convection heat transfer coefficients for un-/ coupled convection method
    if (Convection_Type==1.) then
        !Coupled convection implementation - local convective heat transfer coefficients are determined

        error=(q_convection-q_convectionX)/q_convectionX   !convergence criterion coupled convection 

        Corr_conv=q_convection/q_convection_Clausing1983 !- same heat loss as updated clausing or kraabel  !ST



        do i=1,(N_nodes-S) ! - active surfaces not shaded by lip
            h_conv(i+0.*N_nodes)= Corr_conv*h_avg+h_SK_forced
            h_conv(i+1.*N_nodes)= Corr_conv*h_avg+h_SK_forced
            h_conv(i+2.*N_nodes)= Corr_conv*h_avg+h_SK_forced
            h_conv(i+3.*N_nodes)= Corr_conv*h_avg+h_SK_forced
        enddo

        A_stag=A_array(N_panels*N_nodes+2)+A_array(N_panels*N_nodes+3)+(4*W_panel*H_lip)
        q_conv_stag=Corr_conv*h_stag*A_F*0.3*(T_stag-T_bulk)

        do i=N_nodes-S+1,N_nodes      ! - active surfaces fully shaded by lip
            h_conv(i+0.*N_nodes)= h_SK_forced+(q_conv_stag *(A_array(i+0.*N_nodes)/A_stag) ) / (A_array(i+0.*N_nodes)*(T_s_array(i+0.*N_nodes)-T_bulk))
            h_conv(i+1.*N_nodes)= h_SK_forced+(q_conv_stag *(A_array(i+0.*N_nodes)/A_stag) ) / (A_array(i+1.*N_nodes)*(T_s_array(i+1.*N_nodes)-T_bulk))
            h_conv(i+2.*N_nodes)= h_SK_forced+(q_conv_stag *(A_array(i+0.*N_nodes)/A_stag) ) / (A_array(i+2.*N_nodes)*(T_s_array(i+2.*N_nodes)-T_bulk))
            h_conv(i+3.*N_nodes)= h_SK_forced+(q_conv_stag *(A_array(i+0.*N_nodes)/A_stag) ) / (A_array(i+3.*N_nodes)*(T_s_array(i+3.*N_nodes)-T_bulk))
        enddo

        i=N_nodes-S+1      ! - active surfaces partially shaded by lip
        h_conv(i+0.*N_nodes)= h_SK_forced+((Corr_conv*h_avg*(S*A_array(i+0.*N_nodes)-W_panel*H_lip)*(T_s_array(i+0.*N_nodes)-T_bulk)) + q_conv_stag *(abs((S-1)*A_node-(W_panel*H_lip))/A_stag) ) / ( A_array(i+0.*N_nodes)*(T_s_array(i+0.*N_nodes)-T_bulk))
        h_conv(i+1.*N_nodes)= h_SK_forced+((Corr_conv*h_avg*(S*A_array(i+1.*N_nodes)-W_panel*H_lip)*(T_s_array(i+1.*N_nodes)-T_bulk)) + q_conv_stag *(abs((S-1)*A_node-(W_panel*H_lip))/A_stag) ) / ( A_array(i+1.*N_nodes)*(T_s_array(i+1.*N_nodes)-T_bulk))
        h_conv(i+2.*N_nodes)= h_SK_forced+((Corr_conv*h_avg*(S*A_array(i+2.*N_nodes)-W_panel*H_lip)*(T_s_array(i+2.*N_nodes)-T_bulk)) + q_conv_stag *(abs((S-1)*A_node-(W_panel*H_lip))/A_stag) ) / ( A_array(i+2.*N_nodes)*(T_s_array(i+2.*N_nodes)-T_bulk))
        h_conv(i+3.*N_nodes)= h_SK_forced+((Corr_conv*h_avg*(S*A_array(i+3.*N_nodes)-W_panel*H_lip)*(T_s_array(i+3.*N_nodes)-T_bulk)) + q_conv_stag *(abs((S-1)*A_node-(W_panel*H_lip))/A_stag) ) / ( A_array(i+3.*N_nodes)*(T_s_array(i+3.*N_nodes)-T_bulk))

        h_conv(N_panels*N_nodes+1)= h_SK_forced+Corr_conv*h_F  !convection loss coefficient Floor
        h_conv(N_panels*N_nodes+2)= h_SK_forced+(q_conv_stag *(A_array(N_panels*N_nodes+2)/A_stag)) / (A_array(N_panels*N_nodes+2)*(T_s_array(N_panels*N_nodes+2)-T_bulk))    !*(T_s_array(N_panels*N_nodes+2)/(T_s_array(N_panels*N_nodes+2)+T_s_array(N_panels*N_nodes+3))) !*1/(T_s_array(N_panels*N_nodes+2)-T_bulk)   !convection loss coefficient Ceiling
        h_conv(N_panels*N_nodes+3)= h_SK_forced+(q_conv_stag *(A_array(N_panels*N_nodes+3)/A_stag)) / (A_array(N_panels*N_nodes+3)*(T_s_array(N_panels*N_nodes+3)-T_bulk))    !*(T_s_array(N_panels*N_nodes+3)/(T_s_array(N_panels*N_nodes+2)+T_s_array(N_panels*N_nodes+3))) !*1/(T_s_array(N_panels*N_nodes+3)-T_bulk)  !convection loss coefficient Lip      
  
        h_conv(N_panels*N_nodes+4)=0    !convection loss coefficient opening - no meaning just to complete the set    
                                                                                                           
    else    
        !uncoupled convection- no local convection heat transfer coefficients are determined; 
        !convection loss is considered as reduction of useful energy gain of the HTF
        
        h_conv = 0.

        !Sum of natural and forced convection is calculated
        q_convection=q_convection+( h_SK_forced*(H_rec-H_lip)*4*W_panel*(T_s_avg-T_amb) )

        !Substract the convection losses from the total energy gain in the HTF
        q_htf_total = q_htf_total - q_convection*hl_ffact      !MJW 9.8.2010:: Add convection heat loss multiplier into eqn. (hl_ffact)

        !!Total mass flow rate of the coolant [kg/s]
        !select case(FlowPattern)
        !case(1:2)
        !    m_htf_total = SUM(m_htf(1:N_panels))
        !case(3:4)
        !    m_htf_total = m_htf(1)
        !case(5:8)
        !    m_htf_total = SUM(m_htf(1:2))
        !end select
    
    endif

     
enddo !ST end of EXTERNAL (coupling) loop

!---------------------------------------------------------------------------------

!Total mass flow rate is adjusted accounting for the convection losses
m_htf_total = q_htf_total/(c_htf*(T_htf_hot-T_htf_cold))

!In case the code produced unrealistic negative numbers because of convergence issues, all outputs are set to 0.
if ((m_htf_total < 0.0).OR.(q_htf_total < 0.0)) then
    mode = 0.d0
    goto 900
endif

!---mjw 3.29.11 Limit the HTF mass flow rate to the maximum, if needed. 
if((m_htf_total > m_dot_htf_max).or.(itermode == 2)) then
    err_od = (m_htf_total - m_dot_htf_max)/m_dot_htf_max
    if(err_od < tol_od) then
        itermode = 1    
        od_control = 1.d0
    else
        od_control = od_control*(m_dot_htf_max/m_htf_total)**.8  !adjust the over-design defocus control by modifying the current value
        itermode = 2
        goto 15
    endif
endif
!---


!----Startup
if((E_su0 > 0.) .or. (t_su0 > 0.)) then  !mjw 3.10.11
    E_su = dmax1(0.d0, E_su0 - m_htf_total*c_htf*(T_htf_hot - T_htf_cold)*dt)
    t_su = dmax1(0.d0, t_su0 - dt)
    if(E_su + t_su > 0.d0) then
        mode = 1.d0 !If either are greater than 0, we're starting up but not finished
        q_startup = (E_su0 - E_su)/dt*1.e-6 !mjw 3.10.11
        goto 900  !mjw 3.10.11
    else    !Only part of the timestep/energy was needed to startup.  
        mode= 2.d0
        !Adjust the available mass flow to reflect startup
        m_htf_total = dmin1( (1.-t_su0/dt)*m_htf_total, m_htf_total - E_su0/(dt*c_htf*(T_htf_hot - T_htf_cold)) )
    endif
endif
q_startup = (E_su0 - E_su)/dt*1.e-6       !Convert W-hr to MW  mjw 3.10.11
!----

!Thermal receiver efficiency
efficiency_thermal = q_htf_total/q_solar_total

!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!Calculation of the required mechanical energy to move the working fluid
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!Pressure drop in the tubes in every node
do j=1,N_panels
    do i=1,N_nodes
        deltaP_node(i,j) = rho_htf*f_htf(j)* u_htf(j)**2/2.0*(L_tube_node/D_tube_in+ DBLE(N_45_bends)/DBLE(N_nodes)*L_e_45+ DBLE(N_90_bends)/DBLE(N_nodes)*L_e_90)
    enddo
enddo

!Mass flow rate weighted averaged pressure drop in a tube [Pa]
select case(FlowPattern)
    case(1:2)
        do j=1,N_panels
            deltaP_x_m_htf(j) = SUM(deltaP_node(1:N_nodes,j)*m_htf(j))
        enddo
        deltaP_avg = SUM(deltaP_x_m_htf(1:N_panels))/SUM(m_htf(1:N_panels))
    
    case(3:4)
        deltaP_avg = SUM(deltaP_node(1:N_nodes,1:N_panels))
    
    case(5:8)
        do j=1,N_panels/2
            deltaP_x_m_htf(j) = SUM(deltaP_node(1:N_nodes,j)*m_htf(1))
        enddo

        do j=N_panels/2+1,N_panels
            deltaP_x_m_htf(j) = SUM(deltaP_node(1:N_nodes,j)*m_htf(2))
        enddo
    
        deltaP_avg = SUM(deltaP_x_m_htf(1:N_panels))/(m_htf(1)+m_htf(2))
        !/(N_panels/2*(m_htf(1)+m_htf(2)))
end select

!Density of the cold working fluid [kg/m3]
rho_htf_cold = Density(HTF,T_htf_cold,P_htf)
!Pressure loss over the tower height [Pa]
deltaP_THT = rho_htf_cold*THT*grav
!Pump power calculation
est_load = dmax1(0.25d0, m_htf_total/m_dot_htf_des)*100.                 !MJW 8.26.2010. Calculate the relative pump load. Limit to 25%
eta_pump_adj = efficiency_pump*(-2.8825E-09*est_load**4 + 6.0231E-07*est_load**3 - 1.3867E-04*est_load**2 + 2.0683E-02*est_load)  !Calculate the adjusted pump efficiency
W_pump = m_htf_total*(deltaP_THT/rho_htf_cold+deltaP_avg/rho_htf)/eta_pump_adj !source: Fox et al, pp354

!Energy absorbed by the coolant [W]
Q_thermal = q_htf_total
!Convective heat losses from the receiver [W]
!Q_convection_loss = q_convection*hl_ffact      !MJW 9.8.2010:: Add convection heat loss multiplier into eqn. (hl_ffact)
Q_convection_loss = sum(q_conv(1:N_panels*N_nodes+4)) 
!Hot outlet temperature [K]
T_htf_hot_out = T_htf_hot


Flux_avg=(sum(flux_array(1:N_panels*N_nodes+4))/1000.)/(N_panels*N_nodes)

!After convergence, determine whether the mass flow rate falls below the lower limit
if(m_htf_total < m_dot_htf_min) goto 900


q_rad_therm_out = q_rad_therm(1:N_panels*N_nodes+4,N_panels*N_nodes+4)
q_rad_solar_out = q_rad_solar(1:N_panels*N_nodes+4,N_panels*N_nodes+4)


! overall convection heat transfer coefficient
h_conv_cavity=q_convection_loss/((T_s_avg-T_bulk)*(2*A_F+A_L+N_nodes*N_panels*A_node))



!*************************************************************************************************
!Gonzales et al. (2012) - cavity receiver - loss

T_film_gonz=(T_s_avg+T_amb)/2

phi_gonz=(T_s_avg-T_amb)/T_amb


!Volume expansion coefficient [1/K]
beta_gonz = 1./T_film_gonz 

!Conductivity [W/m-K]
k_gonz = Conductivity(1.d0,T_film_gonz,1.d0)
    
!Specific heat [J/kg-K]
c_p_gonz = SpecHeat(1.d0,T_film_gonz,1.d0)*1000.0

!Viscosity [Pa-s]
mu_gonz = Viscosity(1.d0,T_film_gonz,1.d0)

!Prandtl number
Pr_gonz = (c_p_gonz*mu_gonz)/k_gonz
    
!Density [kg/m3]
rho_gonz = Density(1.d0,T_film_gonz,P_amb)
     
!Grashof number
Gr_gonz=((grav*beta_gonz*(T_s_avg-T_amb)*(H_rec)**3)/((mu_gonz/rho_gonz)**2))
   
!Rayleigh number
Ra_gonz = abs(Gr_gonz*Pr_gonz)


Nu_avg_tot_gonz=exp(-1.26771+0.331391*log(Ra_gonz)+0.33537*phi_gonz)

h_avg_tot_gonz = (k_gonz/H_rec)*Nu_avg_tot_gonz
q_loss_gonz=h_avg_tot_gonz*(T_s_avg-T_amb)*SUM(A_array(1:N_panels*N_nodes+3))
!*************************************************************************************************


	
	
goto 999 !Normal operation, skip to the end

900 continue  !Receiver isn't producing usable energy
Q_thermal = 0.d0
Q_radiation_loss = 0.d0
Q_convection_loss = 0.d0
W_pump = 0.d0
m_htf_total = 0.d0
efficiency_thermal = 0.d0
T_htf_hot_out = T_htf_cold_des !mjw 3.30.11 Changed to design
q_solar_total = 0.d0 !mjw 5.26.11 
Q_radiation_loss_solar = 0.d0
Q_radiation_loss_therm = 0.d0
q_rad_semi_gray_net = 0.d0
q_rad_therm_out = 0.d0
q_rad_solar_out = 0.d0

999 continue  !Normal operation, return

!if(mode==2)then  !ST Availability
if(Q_thermal==0)then
    Availability=0
else
    Availability=1
endif

!-----------------------------------------------------------------------------------------------------------------------
!9-28-12, TN: This is not doing anything as no initial inputs are specified. Fine. Comment out.
! Continue with the rest of the initial call manipulations, and return
!if (INFO(7) == -1) then
!    !CHECK FOR SUITABILITY OF THE SPECifIED FLOWPATTERN
!    if (Q_thermal == 0.d0) call messages(-1,"The initial call calculations of the receiver component were not able to converge.","WARNING",INFO(1),INFO(2))
!    
!    !CRITICAL SOLAR RADIATION LEVEL THAT DETERMINES WHETHER SOLAR RADIATION IS HIGH ENOUGH TO START ITERATIONS.
!    q_solar_critical = 1.0*(Q_radiation_loss + Q_convection_loss)
!
!    !PUT THE STORED ARRAY IN THE GLOBAL STORED ARRAY
!    STORED(4) = q_solar_critical
!
!    call setStorageVars(STORED,ns,INFO)
!
!    !PERFORM ANY REQUIRED CALCULATIONS TO SET THE INITIAL VALUES OF THE OUTPUTS HERE
!    OUT(:) = 0.d0
!    OUT(7) = T_htf_hot_des-273.15   ![C]
!
!
!    !return TO THE callING PROGRAM
!    return 1
!     
!endif
!-----------------------------------------------------------------------------------------------------------------------


!SET THE OUTPUTS FROM THIS MODEL IN SEQUENTIAL ORDER AND GET OUT
OUT(1) = m_htf_total*3600.0 !Mass flow rate of the coolant[kg/hr]
OUT(2) = efficiency_thermal !Thermal efficiency of the receiver
OUT(3) = W_pump/1.0e6 !Pump power [MWe]
OUT(4) = Q_convection_loss/1.0e6 !Total convective losses [MWt]
OUT(5) = Q_radiation_loss/1.0e6 !Heat lost due to radiation [MWt]
OUT(6) = Q_thermal/1.0e6 !Heat absorbed by the coolant [MWt]
OUT(7) = T_htf_hot_out - 273.15 !Coolant outlet temperature [C]
OUT(8) = OUT(6)+abs(OUT(5)+OUT(4)) !Power before thermal losses
OUT(9) = field_eff_adj  !mjw 3.30.11 adjusted heliostat efficiency
OUT(10) = q_solar_total/1.e6 !MWt  !Total incident power on the receiver
OUT(11) = q_startup  !mjw 3.30.11 [MWt] Startup energy consumed during current time step
OUT(12) = Availability ! hours of operation of the solar tower
OUT(13) = Q_radiation_loss_solar/1.0e6 !Heat lost due to solar reflection [MWt]
OUT(14) = Q_radiation_loss_therm/1.0e6 !Heat lost due to thermal radiation [MWt]


!EVERYTHING IS DONE - RETURN FROM THIS SUBROUTINE AND MOVE ON
return 1


!-----------------------------------------------------------------------------------------------------------------------
    CONTAINS

        !-----------------------------------------------------------------------------------------
        subroutine fluxinterp2D(LU_flux,zen_in,azi_in,array,info)

        !************************************************************************
        ! This subroutine reads the 2-D fluxmap output by the PTGEN.exe         *
        ! program and locates the flux map closest to the requested azimuth/    *
        ! zenith angle, returning it in an array.  For justification of this    *
        ! selection method, see the PTGEN portion of the Plant Sizing and       *
        ! Optimization chapter.                                                 *
        ! Inputs and outputs for this model include:                            *
        ! INPUTS:                                                               *
        !   LU_flux :: The logical unit to be used for the fluxmap file         *
        !   zen_in  :: The solar zenith angle                                   *
        !   azi_in  :: The solar azimuth angle                                  *
        ! OUTPUTS:                                                              *
        !   array   :: a 10x12 fluxarray of the flux distribution on the        *
        !               receiver panels                                         *
        !************************************************************************

        implicit none
        integer(4),intent(in)::LU_flux,INFO(15)
        real(8),intent(in)::zen_in,azi_in
        real(8),intent(out)::array(10,12) 
        real(8)::xdist,hold
        real(8),allocatable,save::azimuth(:),zenith(:), day(:), time(:),array_all(:,:,:)
        integer(4)::mylen,i,j,ios,numrec,p1
        logical::is_there
        character::checkname*300, test*300
    
        !Save the variables that are used in future runs
        save::numrec

        !Initial timestep call. allocate arrays, open and read in the flux file
        if(info(7)==-1) then
            !Open the fluxmap file used for generating the flux distribution on the receiver
            inquire(unit=LU_flux,opened=is_there,name=checkname)
            if (.not.is_there) then
                open(unit=LU_flux,file=trim(checkname),iostat=ios,status="OLD",position="REWIND")
                !Check to see if the file was opened successfully
                    if(ios.ne.0) then
                        call messages(-1,"TRNSYS did not find the fluxmap file",'FATAL',INFO(1),INFO(2))
                        return  
                    endif
            endif


            !We need to check to make sure that the fluxmap file has been successfully
            !created by the PTGEN program.  Do this by counting the number of lines 
            !in the file to make sure that it is not zero or close to zero.
            !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            i=0 !initialize counting variable
            do 
                i=i+1
                if (i.gt.10) goto 400
                read(unit=LU_flux,fmt="(A)",err=400,eor=400,end=400, advance="YES") test
            enddo

            400 continue

            if(i <= 2) then
                call messages(-1,"TRNSYS did not find the fluxmap file",'FATAL',INFO(1),INFO(2))
                return 
            endif
            rewind(LU_flux)
            !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  

            ios=0 !Initialize
            !Load in the array of azimuth and zenith values from fluxmap.csv
            inquire(unit=LU_flux,opened=is_there,name=checkname)
            if(.not.is_there) then
                open (unit=LU_flux,file=trim(checkname),status="OLD",iostat=ios,position="rewind")
            endif

            !MJW 9.1.2010:: Start by figuring out how many flux maps are in the file
            rewind(LU_flux)
            do i=1,4
                read (LU_flux,*)
            enddo

            i=0
            do
                read (LU_flux,fmt="(A)") test
                if(test(2:15)=="Azimuth,Zenith") then
                    exit
                else
                    i=i+1
                endif
            enddo
            numrec=i !The number of flux arrays included

            if(.not.allocated(azimuth)) allocate(azimuth(numrec),zenith(numrec),day(numrec),time(numrec),&
                                             array_all(numrec,10,12))
            azimuth(:)=0.d0; zenith(:)=0.d0; day(:)=0.d0; time(:)=0.d0; array_all(:,:,:)=0.d0

            !The data begins on line #5 of the fluxmap.csv file
            rewind(LU_flux)
            do i=1,4
                read (LU_flux,*)
            enddo
            i=0
            
            do i=1,numrec
                read (LU_flux,fmt=500,iostat=ios) day(i),time(i),azimuth(i),zenith(i)
                500     format(3(F5.1,1X),F5.1)
            enddo
        
            !Read in all of the data into the array_all variable
            do i=1,3
                read(LU_flux,*)
            enddo
            do i=1,numrec
                do j=1,10
                    read(LU_flux,fmt='(12(F8.1,1X))') (array_all(i,j,k),k=1,12)  !array is always 12 wide
                enddo
            
                if(i/=numrec) then
                    do j=1,5
                        read(LU_flux,*)
                    enddo
                endif   
            
            enddo
        endif

        !Last call of the simulation. Deallocate, close and return
        if(info(8)==-1) then
            deallocate(azimuth,zenith,day,time,array_all)
            close(LU_flux)
        
            return
        endif

        !------------Calculations done every time-------------------

        !Find the closest neighbor to the inputted value
        hold = 1000  !set it to a high value so its sure to be reset in the loop
        do i=1,numrec
            xdist = sqrt((azimuth(i)-azi_in)**2 + (zenith(i) - zen_in)**2)
            if(xdist.le.hold) then
                hold=xdist
                p1=i
            endif
        enddo

        !Read in the flux map for the point p1
        do i=1,10
            array(i,:)=array_all(p1,i,:)
        enddo

    end subroutine



    SUBROUTINE translateFluxArray(fluxarray,N_nodes,N_panels,solarflux)

    !***********************************************************************
    ! This subroutine translates the 2-D (10x12) flux array and returns    *
    ! an array with averaged flux values for each panel node.    *
    ! Example: N_nodes was chosen to be 5 and the receiver has 4 panels,   *
    ! then the output array will be of the size 5x4.        *
    ! Inputs and outputs for this model include:       *
    ! INPUTS:            *
    !   fluxarray :: The initial flux array read from the fluxmap.csv file *
    !   N_nodes :: Number of vertical nodes on each panel   *
    ! OUTPUTS:           *
    !   solarflux :: The output array with the averaged solar flux values  *
    !***********************************************************************

    implicit none
    integer,intent(IN)::N_nodes,N_panels
    real(8),intent(IN)::fluxarray(10,12)
    real(8),intent(OUT)::solarflux(N_nodes,N_panels)

    !Define local variables
    integer::i,j
    real(8),dimension(10,N_panels)::fluxarray1

    select case(N_panels/2)
    case(1) !if (N_panels == 2) then
        do i=1,10
            fluxarray1(i,1) = (fluxarray(i,1)+fluxarray(i,2)+fluxarray(i,3)+fluxarray(i,4)+fluxarray(i,5)+fluxarray(i,6))/6.0
            fluxarray1(i,2) = (fluxarray(i,7)+fluxarray(i,8)+fluxarray(i,9)+fluxarray(i,10)+fluxarray(i,11)+fluxarray(i,12))/6.0
        enddo
    case(2) !elseif (N_panels == 4) then
        do i=1,10
            fluxarray1(i,1) = (fluxarray(i,1)+fluxarray(i,2)+fluxarray(i,3))/3.0
            fluxarray1(i,2) = (fluxarray(i,4)+fluxarray(i,5)+fluxarray(i,6))/3.0
            fluxarray1(i,3) = (fluxarray(i,7)+fluxarray(i,8)+fluxarray(i,9))/3.0
            fluxarray1(i,4) = (fluxarray(i,10)+fluxarray(i,11)+fluxarray(i,12))/3.0
        enddo
    case(3) !elseif (N_panels == 6) then
        do i=1,10
            fluxarray1(i,1) = (fluxarray(i,1)+fluxarray(i,2))/2.0
            fluxarray1(i,2) = (fluxarray(i,3)+fluxarray(i,4))/2.0
            fluxarray1(i,3) = (fluxarray(i,5)+fluxarray(i,6))/2.0
            fluxarray1(i,4) = (fluxarray(i,7)+fluxarray(i,8))/2.0
            fluxarray1(i,5) = (fluxarray(i,9)+fluxarray(i,10))/2.0
            fluxarray1(i,6) = (fluxarray(i,11)+fluxarray(i,12))/2.0
        enddo
    case(4:5)
        call messages(-1,"Number of receiver panels must be 2,4,6 or 12","FATAL",0,232)
        return 
    case(6) !elseif (N_panels == 12) then
        fluxarray1 = fluxarray
    case(7:)
        call messages(-1,"Number of receiver panels must be 2,4,6 or 12","FATAL",0,232)
        return 
    end select

    !The number of vertical panel nodes can be 1,2,5 or 10. The following loops average the temporary
    !fluxarray1 values in the vertical direction for the provided number of nodes.
    if(N_nodes == 1)then
        do j=1,N_panels
            !Initialize final flux array
            solarflux(1,j) = 0.0
            do i=1,10
                solarflux(1,j) = solarflux(1,j)+fluxarray1(i,j)
            enddo
            solarflux(1,j) = solarflux(1,j)/10.0
        enddo
    elseif(N_nodes == 2)then
        do j=1,N_panels
            !Initialize final flux array
            solarflux(1,j) = 0.0
            solarflux(2,j) = 0.0
            do i=1,5
                solarflux(1,j) = solarflux(1,j)+fluxarray1(i,j)
            enddo
            do i=6,10
                solarflux(2,j) = solarflux(2,j)+fluxarray1(i,j)
            enddo
            solarflux(1,j) = solarflux(1,j)/5.0
            solarflux(2,j) = solarflux(2,j)/5.0
        enddo
    elseif(N_nodes == 5)then
        do j=1,N_panels
            !Initialize final flux array
            solarflux(1,j) = 0.0
            solarflux(2,j) = 0.0
            solarflux(3,j) = 0.0
            solarflux(4,j) = 0.0
            solarflux(5,j) = 0.0
            do i=1,2
                solarflux(1,j) = solarflux(1,j)+fluxarray1(i,j)
            enddo
            do i=3,4
                solarflux(2,j) = solarflux(2,j)+fluxarray1(i,j)
            enddo
            do i=5,6
                solarflux(3,j) = solarflux(3,j)+fluxarray1(i,j)
            enddo
            do i=7,8
                solarflux(4,j) = solarflux(4,j)+fluxarray1(i,j)
            enddo
            do i=9,10
                solarflux(5,j) = solarflux(5,j)+fluxarray1(i,j)
            enddo
            solarflux(1,j) = solarflux(1,j)/2.0
            solarflux(2,j) = solarflux(2,j)/2.0
            solarflux(3,j) = solarflux(3,j)/2.0
            solarflux(4,j) = solarflux(4,j)/2.0
            solarflux(5,j) = solarflux(5,j)/2.0
        enddo
    elseif(N_nodes == 10)then
        solarflux = fluxarray1
    else
        call messages(-1,"Number of vertical nodes perpanel must be 1,2,5 or 10","FATAL",0,232)
        return
    endif

    END SUBROUTINE

    !-----------------------------------------------------------------------------------------

    !######################################################################################################################

    SUBROUTINE PipeFlowCavity(Re,Pr,LoverD,relRough,q_solar_total,Nusselt,f)

    implicit none

    !*********************************************************************
    !* PipeFlow_turbulent:              *
    !* This procedure calculates the average Nusselt number and friction *
    !* factor for turbulent flow in a pipe given Reynolds number (Re),   *
    !* Prandtl number (Pr), the pipe length diameter ratio (LoverD) and  *
    !* the relative roughness}             *
    !*********************************************************************

    real(8),intent(IN)::LoverD,relRough,q_solar_total
    real(8),intent(OUT)::Nusselt,f
    real(8)::f_fd,Nusselt_L,Gz,Gm,Nusselt_T,Nusselt_H,fR,X,Re,Pr

    !REGRESSION MODELS FOR ESTIMATING THE REYNOLDS NUMBERS BASED ON THE INCOMING SOLAR RADIATION
    !SERVES AS GUESS VALUES IN case A NEGATIVE REYNOLDS NUMBERS IS PROVIDED
    if (Re < 0.0) then
        if (q_solar_total > 2.0E+07)then
            Re = -5979.08 + 0.00266426*q_solar_total
        elseif (q_solar_total > 3.69E+06) then
            Re = -14267.6+0.00410787*q_solar_total-6.40334E-11*q_solar_total**2
        else
            Re = 0.001174*q_solar_total
        endif
    endif

    !GUESS VALUE FOR THE PRANDTL NUMBER IN case NEGATIVE NUMBERS IS PROVIDED
    if (Pr < 0.0) then
        Pr = 5.0
    endif

    !Correlation for laminar flow.. Note that no transitional effects are considered
    if (Re < 2300.) then
        !This procedure calculates the average Nusselt number and friction factor for laminar flow in a pipe
        !..given Reynolds number (Re), Prandtl number (Pr), the pipe length diameter ratio (LoverD)
        !..and the relative roughness}
        Gz=Re*Pr/LoverD
        x=LoverD/Re
        fR=3.44/SQRT(x)+(1.25/(4*x)+16-3.44/SQRT(x))/(1+0.00021*x**(-2))
        f=4.*fR/Re
        !{f$='Shah' {Shah, R.K.  and London, A.L. "Laminar Flow Forced Convection in Ducts",
        !..Academic Press, 1978 ,Eqn 192, p98}}
        Gm=Gz**(1./3.)
        Nusselt_T=3.66+((0.049+0.02/Pr)*Gz**1.12)/(1+0.065*Gz**0.7)
        Nusselt_H=4.36+((0.1156 +0.08569 /Pr**0.4)*Gz)/(1+0.1158*Gz**0.6)
        !{Nusselt$='Nellis and Klein fit to Hornbeck'  {Shah, R.K.  and London, A.L. "Laminar Flow Forced Convection in Ducts",
        !..Academic Press, 1978 ,Tables  20 and 22}}
        Nusselt = Nusselt_T  !Constant temperature Nu is better approximation
        
    else
        !Correlation for turbulent flow
        !Petukhov, B.S., in Advances in Heat Transfer, Vol. 6, Irvine and Hartnett, Academic Press, 1970
        f_fd = (0.79*LOG(Re)-1.64)**(-2)
        !Gnielinski, V.,, Int. Chem. Eng., 16, 359, 1976
        Nusselt_L= ((f_fd/8.)*(Re-1000)*Pr)/(1.+12.7*SQRT(f_fd/8.)*(Pr **(2./3.)-1.))
        
        if (relRough > 1e-5) then
            
            f=8.*((8./Re)**12+((2.457*LOG(1./((7./Re)**0.9+0.27*(relRough))))**16+(37530./Re)**16)**(-1.5))**(1./12.)
            
            f_fd=(-2.*LOG10(2*relRough/7.4-5.02*LOG10(2*relRough/7.4+13/Re)/Re))**(-2)
            !Gnielinski, V.,, Int. Chem. Eng., 16, 359, 1976}
            Nusselt_L= ((f_fd/8.)*(Re-1000.)*Pr)/(1.+12.7*SQRT(f_fd/8.)*(Pr **(2./3.)-1.))
        endif
        f=f_fd*(1.+(1./LoverD)**0.7) !account for developing flow
        Nusselt= Nusselt_L*(1.+(1./LoverD)**0.7)  !account for developing flow
    endif
    9 CONTINUE
    END SUBROUTINE
    
!---------------------------------------------------------------------------------------------------------------------------    
SUBROUTINE FractionFunction(N_nodes,N_panels,N_band,T_sX_array,lambda_step_band,f_temp_band,f_solar_band )

    implicit none
    !**********************************************************************************
    !This subroutine calculates the total convective heat losses from the receiver
    !with the correlation presented in Petukhov and Popov (1963)  !ST ( Heat Exchanger Design Handbook 2008 G.F. Hewitt Section 2.5 II b) )
    !  The inputs are:
    !    - lambda_step - emissivity step wavelength [micron]
    !    - T_sX_array - surface temperatures
    !  The outputs are:
    !    - F_Thermal - fraction of blackbody radiaiton at 800K in the wavelength band from 0-lambda_step
    !    - f_solar_band - fraction of blackbody radiaiton at 5800K in the wavelength band from 0-lambda_step
    !**********************************************************************************
    !integer,parameter,intent(IN)::N_nodes,N_panels
    integer::N_nodes,N_panels,N_band
    real(8),dimension(N_band-1),intent(IN)::lambda_step_band
    real(8),dimension(N_nodes*N_panels+4),intent(IN)::T_sX_array
    
    !real(8),dimension(N_nodes*N_panels+4)::
    real(8),dimension(N_nodes*N_panels+5,N_band)::f_uni,gamma_array
    real(8),dimension(10)::n
    
    real(8),dimension(N_nodes*N_panels+4,N_band),intent(OUT):: f_temp_band
    real(8),dimension(N_band),intent(OUT):: f_solar_band
    
    real(8)::i,k,T_sun,l
    
    real(8),parameter::pi=3.14159265, C_2=14387.69
    
	T_sun=5800
	
	do i=1,10
    	n(i)=i
	enddo
	
	
    do l=1,(N_band-1)

        !gamma value for temperature dependent fraction
        do k=1,N_nodes*N_panels+4
            gamma_array(k,l)=C_2/(lambda_step_band(l)*T_sX_array(k))
        enddo
      
        !opening has fraction of UNITY 
        !gamma_array(N_nodes*N_panels+4,l)=0
        gamma_array(N_nodes*N_panels+5,l)=C_2/(lambda_step_band(l)*T_sun)
      
      
	  
        do k=1,N_nodes*N_panels+5
         	f_uni(k,l)=  15/pi**4 * sum( exp(-n(1:10)*gamma_array(k,l))/n(1:10)*(  gamma_array(k,l)**3+ (3*gamma_array(k,l)**2)/n(1:10) + (6*gamma_array(k,l))/n(1:10)**2 + 6./n(1:10)**3  )   )
		enddo
    enddo


    do l=1,(N_band)
        if (l<=1) then
    
	        do k=1,N_nodes*N_panels+4
	            f_temp_band(k,l)=f_uni(k,l)
	        enddo   

	        f_solar_band(l)=f_uni(N_nodes*N_panels+5,l)
	    
	    elseif(l==N_band)then
	
	        do k=1,N_nodes*N_panels+4
	            f_temp_band(k,l)=1-f_uni(k,l-1)
	        enddo
	    
	        f_solar_band(l)=1-f_uni(N_nodes*N_panels+5,l-1)
	    
        else

	        do k=1,N_nodes*N_panels+4
	            f_temp_band(k,l)=f_uni(k,l)-f_uni(k,l-1)
	        enddo

	        f_solar_band(l)=f_uni(N_nodes*N_panels+5,l)-f_uni(N_nodes*N_panels+5,l-1)
	
        endif
	
    enddo
	

	END SUBROUTINE

	!---------------------------------------------------------------------------------------------------------------------------    

	SUBROUTINE FractionFunction_two_band(N_nodes,N_panels,T_sX_array,lambda_step,f_Thermal,f_Solar )

    implicit none
    !**********************************************************************************
    !This subroutine calculates the total convective heat losses from the receiver
    !with the correlation presented in Petukhov and Popov (1963)  !ST ( Heat Exchanger Design Handbook 2008 G.F. Hewitt Section 2.5 II b) )
    !  The inputs are:
    !    - lambda_step - emissivity step wavelength [micron]
    !    - T_sX_array - surface temperatures
    !  The outputs are:
    !    - F_Thermal - fraction of blackbody radiaiton at 800K in the wavelength band from 0-lambda_step
    !    - F_Solar - fraction of blackbody radiaiton at 5800K in the wavelength band from 0-lambda_step
    !**********************************************************************************
    !integer,parameter,intent(IN)::N_nodes,N_panels
    integer::N_nodes,N_panels
    real(8),intent(IN)::lambda_step
    real(8),dimension(N_nodes*N_panels+4),intent(IN)::T_sX_array
    
    !real(8),dimension(N_nodes*N_panels+4)::
    real(8),dimension(N_nodes*N_panels+5)::f_uni,gamma_array
    real(8),dimension(10)::n
    
    real(8),dimension(N_nodes*N_panels+4),intent(OUT):: f_Thermal
    real(8),intent(OUT):: f_Solar
    
    real(8)::i,k,T_sun
    
    real(8),parameter::pi=3.14159265, C_2=14387.69
    
	T_sun=5800
	
	do i=1,10
    	n(i)=i
	enddo

    
    do k=1,N_nodes*N_panels+3
        gamma_array(k)=C_2/(lambda_step*T_sX_array(k))
    enddo
      
    gamma_array(N_nodes*N_panels+4)=0
    gamma_array(N_nodes*N_panels+5)=C_2/(lambda_step*T_sun)
      
      
	do k=1,N_nodes*N_panels+5
        f_uni(k)=  15/pi**4 * sum( exp(-n(1:10)*gamma_array(k))/n(1:10)*(  gamma_array(k)**3+ (3*gamma_array(k)**2)/n(1:10) + (6*gamma_array(k))/n(1:10)**2 + 6./n(1:10)**3  )   )
	enddo

	do k=1,N_nodes*N_panels+4
	    f_Thermal(k)=f_uni(k)
	enddo

	f_solar=f_uni(N_nodes*N_panels+5)

	END SUBROUTINE



    !-----------------------------------------------------------------------------------------
    SUBROUTINE ConvectionClausing1983(N_nodes,N_panels,T_s,T_F,T_CE,T_L,T_amb,P_amb,H_rec,H_lip,R_rec,alpha,W_panel,A_node,A_F,A_O,Q_radiation_loss,q_convection_Clausing1983,h_F,h_avg,h_stag,T_stag,T_bulk,S)
   
    implicit none
    !**********************************************************************************
    !This subroutine calculates the total convective heat losses from the receiver
    !with the correlations presented in Clausing (1983).
    !  The inputs are:
    !    - N_nodes -> number of vertical nodes per receiver panel [-]
    !    - N_panels -> number of receiver panels [-]
    !    - T_F -> the temperature of the receiver FLOOR [K]
    !    - T_amb -> ambient temperature [K]
    !    - P_amb -> ambient pressure [Pa]
    !    - H_rec -> internal receiver height [m]
    !    - H_lip -> height of the upper lip [m]
    !    - R_rec -> internal receiver radius [m]
    !    - alpha -> segment angle [rad]
    !    - W_panel -> width of one receiver panel [m]
    !    - A_node -> area of the active receiver surfaces [m2]
    !    - A_F -> area of the FLOOR surface [m2]
    !    - A_O -> area of the aperture [m2]
    !  The outputs are:
    !    - q_convection_Clausing1983 -> the total convective heat losses through the aperture [W]
    !**********************************************************************************
    integer::N_nodes,N_panels,CE,FL,IICT
    real(8),intent(IN)::T_s(N_nodes,N_panels),T_F,T_CE,T_L,T_amb,P_amb,H_rec,H_lip,R_rec,alpha,W_panel,A_node,A_F,A_O,Q_radiation_loss
    real(8),intent(OUT)::q_convection_Clausing1983,h_F,h_avg,h_stag,T_bulk,T_stag,S
    real(8),parameter::grav=9.81,pi=3.14159265
    real(8)::W,c,T_avg, T_film_F, T_film_stag, T_film_avg, T_c, c_p_amb, c_p_F, c_p_stag, c_p_avg, beta_amb, beta_F, beta_stag, beta_avg,&
        k_F, k_stag, k_avg, mu_F, mu_stag, mu_avg, rho_amb, rho_F, rho_stag, rho_avg, Pr_F, Pr_stag, Pr_avg, Ra_F, Ra_stag, Ra_avg, SpecHeat,&
        Viscosity, Density, Conductivity, q_conv_1, q_conv_2, q_conv_3, q_conv_4, q_convection_Clausing1983X,&
        v_b, v_a, v,error, Gr_F, GR_stag, GR_avg, Nusselt_F, Nusselt_stag, Nusselt_avg, T_F_calc
    !-----------------------------------------------------------------------------------------
    !ST - from EES
    W = 2.*R_rec*SIN(alpha/2.)		!panel width if panels have equal size
    c = 2.*R_rec*SIN(PI-2.*alpha)    !distance between the vertical aperture edges if the aperature is considered to be at the outer edges of the outer panels

    S=ceiling(H_lip/(H_rec/N_nodes)) !number of panels that are influenced by the stagnant zone
   
    T_F_calc=T_F !IN - variable can't be changed in case it violated the correlation limits
    T_avg=sum(T_s(1:N_nodes-S,1:N_panels))/(N_panels*(N_nodes-S))
    T_stag=(sum(T_s(N_nodes-S+1:N_nodes,1:N_panels))+T_CE+T_L)/(S*N_panels+2)
   
   
    if (T_F_calc/T_amb>2.6) then
        T_F_calc=2.6*T_amb
    endif   
   
    if (T_stag/T_amb>2.6) then
        T_stag=2.6*T_amb
    endif   
   
    if (T_avg/T_amb>2.6) then
        T_avg=2.6*T_amb
    endif   
   
    !Ambient properties:
    beta_amb = 1./T_amb 
    rho_amb = Density(1.d0,T_amb,P_amb)
    c_p_amb = SpecHeat(1.d0,T_amb,1.d0)*1000.0

    v=0                                                                     !Free stream, velocity - clasuing 1983 - forced convection?
   
    error=9999                                                              !IICT and error have to be defined!
    IICT=0
    T_c=T_avg
    q_convection_Clausing1983X=Q_radiation_loss
    q_convection_Clausing1983=5.
    
    do WHILE((error .GT. (1.0e-12)).and.(iict.lt.50))
        IICT=IICT+1
        error=abs((q_convection_Clausing1983X-q_convection_Clausing1983)/q_convection_Clausing1983)
        q_convection_Clausing1983=q_convection_Clausing1983X
    
        T_bulk=(T_c+T_amb)/2.0

        !Film temperature for the property evaluation
        T_film_F = (T_F_calc+T_bulk)/2.0         !"Film temperature at the floor"          
        T_film_stag = (T_stag+T_bulk)/2.0         !"Film temperature in the stagnant zone"
        T_film_avg = (T_avg+T_bulk)/2.0     !"Average film temperature"
        !Property evaluation at different locations in the cavity - Floor(F); Stagnant zone(s); Average(avg)
   
       !Volume expansion coefficient [1/K]
        beta_F = 1./T_film_F 
        beta_stag = 1./T_film_stag
        beta_avg = 1./T_film_avg 

        !Conductivity [W/m-K]
        k_F = Conductivity(1.d0,T_film_F,1.d0)
        k_stag = Conductivity(1.d0,T_film_stag,1.d0)
        k_avg = Conductivity(1.d0,T_film_avg,1.d0)
    
        !Specific heat [J/kg-K]
        c_p_F = SpecHeat(1.d0,T_film_F,1.d0)*1000.0
        c_p_stag = SpecHeat(1.d0,T_film_stag,1.d0)*1000.0
        c_p_avg = SpecHeat(1.d0,T_film_avg,1.d0)*1000.0

       !Viscosity [Pa-s]
        mu_F = Viscosity(1.d0,T_film_F,1.d0)
        mu_stag = Viscosity(1.d0,T_film_stag,1.d0)
        mu_avg = Viscosity(1.d0,T_film_avg,1.d0)

       !Prandtl number
        Pr_F = (c_p_F*mu_F)/k_F
        Pr_stag = (c_p_stag*mu_stag)/k_stag
        Pr_avg = (c_p_avg*mu_avg)/k_avg
    
        !Density [kg/m3]
        rho_F = Density(1.d0,T_film_F,P_amb)
        rho_stag = Density(1.d0,T_film_stag,P_amb)
        rho_avg = Density(1.d0,T_film_avg,P_amb)
     
        !Grashof number
        Gr_F=((grav*beta_F*(T_F_calc-T_bulk)*(A_F/(4*W+c))**3)/((mu_F/rho_F)**2))
        Gr_stag=((grav*beta_stag*(T_stag-T_bulk)*(A_F/(4*W+c))**3)/((mu_stag/rho_stag)**2))
        Gr_avg=((grav*beta_avg*(T_avg-T_bulk)*(H_rec-H_lip)**3)/((mu_avg/rho_avg)**2))
   
        !Rayleigh number
        Ra_F = abs(Gr_F*Pr_F)
        Ra_stag = abs(Gr_stag*Pr_stag)
        Ra_avg = abs(Gr_avg*Pr_avg)
  
        !Nusselt number
        Nusselt_F=(0.082*Ra_F**(1./3.)*(-0.9+2.4*(T_F_calc/T_amb)-0.5*(T_F_calc/T_amb)**2))
        Nusselt_stag=(2./3.*0.082*Ra_stag**(1./3.)*(-0.9+2.4*(T_stag/T_amb)-0.5*(T_stag/T_amb)**2))
        Nusselt_avg=(0.082*Ra_avg**(1./3.)*(-0.9+2.4*(T_avg/T_amb)-0.5*(T_avg/T_amb)**2))
   
   
   
        h_F=(((4.*W+c)*k_f)/(A_F))*Nusselt_F
        h_stag=(((4.*W+c)*k_stag)/(A_F))*Nusselt_stag
        h_avg=(k_avg/(H_rec-H_lip))*Nusselt_avg
   
        q_conv_1=sum(h_avg*A_node*(T_s(1:N_nodes-S,1:N_panels)-T_bulk))                  !convection loss unshaded absorber surfaces
        q_conv_2=sum(h_avg*(S*A_node-W*H_lip)*(T_s(N_nodes-S+1,1:N_panels)-T_bulk))      !convection loss absorber surfaces partially in stagnant zone
        q_conv_3=0                                                                       !convection loss absorber surfaces in stagnant zone
        q_conv_4=h_F*A_F*(T_F_calc-T_bulk)+h_stag*0.3*A_F*(T_stag-T_bulk)                !convection loss from the floor and stagnant zone interface area
        q_convection_Clausing1983X=q_conv_1+q_conv_2+q_conv_3+q_conv_4

        !Velocity due to bouyant forces
        v_b=sqrt(grav*beta_amb*(T_c-T_amb)*(H_rec-H_lip))
        v_a=0.5*sqrt(v_b**2.+(v/2.)**2.)

        T_c=q_convection_Clausing1983X/(rho_amb*v_a*A_O*0.5*c_p_amb)+T_amb
   enddo
   
   END SUBROUTINE
!-----------------------------------------------------------------------------------------

    
    
    SUBROUTINE ConvectionClausing1987(N_nodes,N_panels,T_s,T_F,T_amb,P_amb,H_rec,H_lip,W_panel,A_F,A_O,q_convection)
    implicit none
    !**********************************************************************************
    !This subroutine calculates the total convective heat losses from the receiver
    !with the correlations presented in Clausing (1987).
    !  The inputs are:
    !    - N_nodes -> number of vertical nodes per receiver panel [-]
    !    - N_panels -> number of receiver panels [-]
    !    - T_s -> the array of surface temperature for every active surface node [K]
    !    - T_F -> the temperature of the receiver FLOOR [K]
    !    - T_amb -> ambient temperature [K]
    !    - P_amb -> ambient pressure [Pa]
    !    - H_rec -> internal receiver height [m]
    !    - H_lip -> height of the upper lip [m]
    !    - W_panel -> width of one receiver panel [m]
    !    - A_F -> area of the FLOOR surface [m2]
    !    - A_O -> area of the aperture [m2]
    !  The outputs are:
    !    - q_convection -> the total convective heat losses through the aperture [W]
    !**********************************************************************************
    integer::N_nodes,N_panels,CE,FL
    real(8),intent(IN)::T_s(N_nodes,N_panels),T_F,T_amb,P_amb,H_rec,H_lip,W_panel,A_F,A_O
    real(8),intent(OUT)::q_convection
    real(8),parameter::grav=9.81,pi=3.14159265
    real(8)::T_w_avg, T_film, L_a, L_c, A_cz, c_p_amb, c_p_film, beta_amb, beta_film, k_amb, k_film, mu_amb, mu_film, rho_amb, rho_film, &
        Pr_amb, Pr_film, Ra_amb, Ra_film, SpecHeat, Viscosity, Density, Conductivity, MO, ratio_H, g, f, b, bX, error, H_node, A_node !,&
        

    !Ratio between node height and lip height
    H_node = H_rec/DBLE(N_nodes)
    ratio_H = H_lip/H_node
    CE = CEILING(ratio_H)
    FL = FLOOR(ratio_H)
    MO = ratio_H-DBLE(FL)
    A_node = H_node*W_panel

    !Average wall temperature of the surfaces below the horizontal plane passing through the upper lip
    T_w_avg = (SUM(T_s(1:N_nodes-CE,1:N_panels))*A_node+(1.0-MO)*SUM(T_s(N_nodes-CE+1,1:N_panels))*A_node+2./3.*A_F*T_F)/((DBLE(N_nodes-CE)+1.0-MO)*DBLE(N_panels)*A_node+2./3.*A_F)

    !In case unrealistic wall temperature values are provided, the convection loss rate is set to 0.
    if (T_w_avg < 250.0) then
        q_convection = 0.0
        return
    endif

    !Film temperature for the property evaluation
    T_film = (T_w_avg+T_amb)/2.0
    !Aperture length
    L_a = H_rec-H_lip
    !characteristic length for the dimensionless numbers (reference: Clausing, 1987)
    L_c = L_a+0.5*H_rec !1.5*H_rec-H_lip       !ST is that what Clausing did? L_c=Height_Apperature+0.5*Sidelength_of_the_cube_that_resembles_the_cavity
    !Convective area definition for the wall area below the shear layer (reference: Clausing, 1987)
    A_cz = A_F + A_O + N_panels*W_panel*(H_rec-H_lip)*pi/2.0

    !Calculate the air properties
    !Specific heat [J/kg-K]
    c_p_amb = SpecHeat(1.d0,T_amb,1.d0)*1000.0
    c_p_film = SpecHeat(1.d0,T_film,1.d0)*1000.0
    !Volume expansion coefficient [1/K]
    beta_amb = 1./T_amb 
    beta_film = 1./T_film 
    !Conductivity [W/m-K]
    k_amb = Conductivity(1.d0,T_amb,1.d0)
    k_film = Conductivity(1.d0,T_film,1.d0)
    !Viscosity [Pa-s]
    mu_amb = Viscosity(1.d0,T_amb,1.d0)
    mu_film = Viscosity(1.d0,T_film,1.d0)
    !Density [kg/m3]
    rho_amb = Density(1.d0,T_amb,P_amb)
    rho_film = Density(1.d0,T_film,P_amb)
    !Prandtl number
    Pr_amb = c_p_amb*mu_amb/k_amb
    Pr_film = c_p_film*mu_film/k_film
    !Rayleigh number
    Ra_amb = grav*beta_amb*(T_w_avg-T_amb)*L_c**3*(rho_amb/mu_amb)**2*Pr_amb
    Ra_film = grav*beta_film*(T_w_avg-T_amb)*L_c**3*(rho_film/mu_film)**2*Pr_film

    !Factor g and f for the Nusselt number correlation
    if (Ra_film < (3.8E+08))then
        g = 0.63*Ra_film**0.25
        f = 1.0
    elseif (Ra_film < (1.6E+09))then
        g = 0.63*Ra_film**0.25
        f = (-0.7476+0.9163*(T_w_avg/T_amb)-0.1663*(T_w_avg/T_amb)**2)*(Ra_film**(1.0/3.0)-3.8E+08**(1.0/3.0))/(1.6E+09**(1.0/3.0)-3.8E+08**(1.0/3.0))+1
    else
        g = 0.108*Ra_film**(1.0/3.0)
        f = 0.2524+0.9163*(T_w_avg/T_amb)-0.1663*(T_w_avg/T_amb)**2
    endif

    !Start value for the iteration of b
    b = 1.0
    !Initial high error
    error = 9999.0
    !iteration to obtain result for factor b
    do WHILE (error > (1.0E-06))
        bX = 1-1.57*((g*f*b*k_film/k_amb)/((Ra_amb*Pr_amb*L_a/L_c)**0.5*A_O/A_cz))**(2.0/3.0)
        error = ABS(b-bX)/b
        b = bX
    enddo

    !Total convective heat losses throughout the aperture
    q_convection = g*f*b*k_film*A_cz*(T_w_avg-T_amb)/L_c

    END SUBROUTINE

    !-----------------------------------------------------------------------------------------

    SUBROUTINE ConvectionSiebersAndKraabel(N_nodes,N_panels,W_panel,T_s,T_F,T_CE,T_L,T_amb,P_amb,H_rec,H_lip,A_F,A_L,A_O,q_convection)
    implicit none
    !**********************************************************************************
    !This subroutine calculates the total convective heat losses from the receiver
    !with the correlations presented in Siebers and Kraabel (1984) !ST
    !  The inputs are:
    !    - N_nodes -> number of vertical nodes per receiver panel [-]
    !    - N_panels -> number of receiver panels [-]
    !    - T_s -> the array of surface temperature for every active surface node [K]
    !    - T_F -> the temperature of the receiver FLOOR [K]
    !    - T_amb -> ambient temperature [K]
    !    - P_amb -> ambient pressure [Pa]
    !    - H_rec -> internal receiver height [m]
    !    - H_lip -> height of the upper lip [m]
    !    - W_panel -> width of one receiver panel [m]
    !    - A_F -> area of the FLOOR surface [m2]
    !    - A_O -> area of the aperture [m2]
    !  The outputs are:
    !    - q_convection -> the total conective heat losses through the aperture [W]
    !**********************************************************************************
    integer::N_nodes,N_panels
    real(8),intent(IN)::T_s(N_nodes,N_panels),T_F,T_CE,T_L,T_amb,P_amb,H_rec,H_lip,A_F,A_O,A_L,W_panel
    real(8),intent(OUT)::q_convection
    real(8),parameter::grav=9.81,pi=3.14159265
    real(8)::T_w_avg,A_1,A_2,beta_film,k_film,mu_film,rho_film,Gr,Viscosity,Density,Conductivity,h_convection,T_film


	!T_w_avg = (SUM(T_s(1:N_nodes,1:N_panels))+T_F+T_CE+T_L)/ DBLE((N_nodes*N_panels)+3)
	T_w_avg = (SUM(T_s(1:N_nodes,1:N_panels)*A_node)+T_F*A_F+T_CE*A_F+T_L*A_L)/(2*A_F+A_L+N_nodes*N_panels*A_node) 
    T_film=(T_w_avg+T_amb)/2
    !Cavity surface area
    A_1 = DBLE(N_panels)*W_panel*H_rec*pi/2.0+2.0*A_F+A_L
    !Cavity surface area under the lip
    A_2 = A_F + DBLE(N_panels)*W_panel*(H_rec-H_lip)*pi/2.0

    beta_film = 1./T_film 
    mu_film = Viscosity(1.d0,T_film,1.d0)
    k_film = Conductivity(1.d0,T_film,1.d0)
    rho_film = Density(1.d0,T_film,P_amb)

    !Grashof number
    Gr = grav*beta_film*(T_w_avg-T_amb)*H_rec**3*rho_film**2/mu_film**2
    !Heat transfer coefficient
    h_convection = (k_film/H_rec)*(0.088*Gr**(1.0/3.0)*(T_w_avg/T_amb)**(0.18))
    !Influence of the lip
    h_convection = h_convection * (A_2/A_1)**(0.63) 
    !Total convective losses
    q_convection = h_convection*A_1*(T_w_avg-T_amb)

    END SUBROUTINE



    SUBROUTINE Forced_ConvectionSiebersAndKraabel(THT ,h_wind_measurement, u_wind, Deg_wind, T_s_array, T_amb, P_amb, H_rec, H_lip, W_panel, wind_direction_dependence, h_SK_forced, u_wind_tower)
    implicit none
    !**********************************************************************************
    !This subroutine calculates the forced convection loss of the cavity receiver, according to 
    !Siebers&Kraabel - vertical flat plate - aperture - SAND87-2290 - 6-69
    !  The inputs are:
    !        - THT -> tower height [m]
    !        - h_wind_measurement -> height at that the wind was measured [m]
    !        - u_wind -> wind velocity [m/s] 
    !        - Deg_wind -> wind direction [deg]
    !        - T_s -> the array of surface temperature for every surface node [K]
    !        - T_amb -> ambient temperature [K]
    !        - P_amb -> ambient pressure [Pa]
    !        - H_rec -> internal receiver height [m]
    !        - H_lip -> height of the upper lip [m]
    !        - W_panel -> width of one receiver panel [m]
    !  The outputs are:
    !        - h_SK_forced
    !**********************************************************************************
    real(8),intent(IN)::THT ,h_wind_measurement,u_wind, Deg_wind, T_s_array(N_panels*N_nodes+4),T_amb,P_amb,H_rec,H_lip,W_panel,wind_direction_dependence
    real(8),intent(OUT)::h_SK_forced, u_wind_tower
    real(8):: T_cav_avg, T_film, c_p_film, k_film, mu_film, rho_film, wind_direction_factor, Nusselt_SK_f, Re_SK_f, Pr_film
   
    u_wind_tower=u_wind/((h_wind_measurement/THT)**0.14)    ! Wind velocity adjustment - Duffie&Beckman "Solar Engineering of Thermal Processes" 3rd Ed. p784 Eqn. (24.2.4)
   
   
    T_cav_avg= SUM(T_s_array(1:N_nodes*N_panels+3))/(N_nodes*N_panels+3)
    T_film=(T_cav_avg+T_amb)/2
   
    !Specific heat [J/kg-K]
    c_p_film = SpecHeat(1.d0,T_film,1.d0)*1000.0
    !Conductivity [W/m-K]
    k_film = Conductivity(1.d0,T_film,1.d0)
    !Viscosity [Pa-s]
    mu_film = Viscosity(1.d0,T_film,1.d0)
    !Density [kg/m3]
    rho_film = Density(1.d0,T_film,P_amb)
    
    !Prandtl number
    Pr_film = c_p_film*mu_film/k_film
    !Reynolds number - wind
    Re_SK_f=(u_wind_tower*(H_rec-H_lip)*rho_film)/mu_film
   
    if (wind_direction_dependence==1.)then
        wind_direction_factor=( 2.+COSD(Deg_wind) )/3.     !Considers influence wind direction - Made up by ST - SEL 7/8/2011
    else
        wind_direction_factor=1. 
    endif
   
    Nusselt_SK_f=0.0287*(Re_SK_f**0.8)*Pr_film**(1./3.)* wind_direction_factor
   
    h_SK_forced=(Nusselt_SK_f*k_film)/(H_rec-H_lip)
   

    END SUBROUTINE


 
    SUBROUTINE PaitANDLove(N_nodes,N_panels,T_s,T_F,T_CE,T_L,T_amb,T_bulk,P_amb,H_rec,H_lip,R_rec,h_PaitANDLove,q_convection)
    implicit none
    !**********************************************************************************
    !This subroutine calculates the total convective heat losses from the receiver
    !with the correlations presented in Paitoonsurikarn and Lovegrove (2006) !ST
    !  The inputs are:
    !    - N_nodes -> number of vertical nodes per receiver panel [-]
    !    - N_panels -> number of receiver panels [-]
    !    - T_s -> the array of surface temperature for every active surface node [K]
    !    - T_F -> the temperature of the receiver FLOOR [K]
    !    - T_amb -> ambient temperature [K]
    !    - T_bulk -> bulk temperature from Clausing1983
    !    - P_amb -> ambient pressure [Pa]
    !    - H_rec -> internal receiver height [m]
    !    - H_lip -> height of the upper lip [m]
    !    - R_rec -> radius of the receiver [m]
    !  The outputs are:
    !    - h_PaitANDLove -> conective heat loss coefficient [W/m^2-K]
    !**********************************************************************************
    integer::N_nodes,N_panels
    real(8),intent(IN)::T_s(N_nodes,N_panels),T_F,T_CE,T_L,T_amb,T_bulk,P_amb,H_rec,H_lip,R_rec
    real(8),intent(OUT)::h_PaitANDLove, q_convection
    real(8)::T_w_avg, T_film_w_avg, L_s_PL, theta_PL, beta_PL_film, mu_PL_film, rho_PL_film, nu_PL_film, c_p_PL_film, k_PL_film, alpha_PL_film, Pr_PL, Ra_L_PL, Nu_L_PL
    real(8),dimension(3)::a_PL,phi_PL,b_PL,L_PL
    real(8),parameter::grav=9.81

    !Average wall temperature of the cavity surfaces
	!T_w_avg = (SUM(T_s(1:N_nodes,1:N_panels))+T_F+T_CE+T_L)/ DBLE((N_nodes*N_panels)+3)
	T_w_avg = (SUM(T_s(1:N_nodes,1:N_panels)*A_node)+T_F*A_F+T_CE*A_F+T_L*A_L)/(2*A_F+A_L+N_nodes*N_panels*A_node) 
    T_film_w_avg = (T_w_avg+T_amb)/2.

    theta_PL=0 ! - cavity inclination in radian
    
    L_PL = (/ H_rec, R_rec, (H_rec-H_lip) /)
    a_PL = (/ 4.08, -1.17, 0.07 /)
    b_PL = (/ 5.41, 7.17, 1.99 /)
    phi_PL = (/ -0.11, -0.30, -0.08 /)
    
    !Characteristic Length according to Paitoonsurikarn and Lovegrove (2006)
    L_s_PL=abs(SUM(a_PL(1:3)*cos(theta_PL+phi_PL(1:3))**b_PL(1:3)*L_PL(1:3)))

    !expansion factor
    beta_PL_film=1./T_film_w_avg
    
    !dynamic Viscosity [Pa-s]
    mu_PL_film = Viscosity(1.d0,T_film_w_avg,1.d0)
    
    !Density [kg/m3]
    rho_PL_film = Density(1.d0,T_film_w_avg,P_amb)
    
    !kinematic viscosity [m^2/s]
    nu_PL_film=mu_PL_film/rho_PL_film
    
    !Specific heat [J/kg-K]
    c_p_PL_film = SpecHeat(1.d0,T_film_w_avg,1.d0)*1000.0
    
    !Conductivity [W/m-K]
    k_PL_film = Conductivity(1.d0,T_film_w_avg,1.d0)
    
    !thermal diffusivity [m^2/s]
    alpha_PL_film=k_PL_film/(rho_PL_film*c_p_PL_film)
    
    !Prandtl number
    Pr_PL=(nu_PL_film)/(alpha_PL_film)
    
    !Rayleigh number
    Ra_L_PL=(grav*beta_PL_film*(T_w_avg-T_amb)*L_s_PL**3)/(nu_PL_film * alpha_PL_film)

    !Nusselt number
    Nu_L_PL=0.0196*Ra_L_PL**0.41*PR_PL**0.13
    
    !Heat transfer coefficient
    h_PaitANDLove = (k_PL_film/L_s_PL)*Nu_L_PL
    
    q_convection=h_PaitANDLove*((T_w_avg-T_bulk)*(2*A_F+A_L+N_nodes*N_panels*A_node))


    END SUBROUTINE
    
    
    
    
    SUBROUTINE LeibAndOrth(N_nodes,N_panels,T_s,T_F,T_CE,T_L,T_amb,P_amb,H_rec,R_rec,A_node,A_F,A_L,A_O, h_LeibAndOrth, q_convection)

    implicit none
    !**********************************************************************************
    !This subroutine calculates the total convective heat losses from the receiver
    !with the correlations presented in Leibfried and Ortjohan (1995)  !ST
    !  The inputs are:
    !    - N_nodes -> number of vertical nodes per receiver panel [-]
    !    - N_panels -> number of receiver panels [-]
    !    - T_s -> the array of surface temperature for every active surface node [K]
    !    - T_F -> the temperature of the receiver FLOOR [K]
    !    - T_CE -> the temperature of the receiver CEILING [K]
    !    - T_L -> the temperature of the receiver LIP [K]
    !    - T_amb -> ambient temperature [K]
    !    - P_amb -> ambient pressure [Pa]
    !    - H_rec -> internal receiver height [m]
    !    - R_rec -> radius of the receiver [m]
	!	 - A_node -> area of active receiver surface segment [m^2]
	!	 - A_F -> area of receiver floor [m^2]
	!    - A_L -> area of receiver lip [m^2]
	!	 - A_O -> area of receiver opening [m^2]	
    !  The outputs are:
    !    - h_LeibAndOrth -> conective heat loss coefficient [W/m^2-K]
    !**********************************************************************************
    integer::N_nodes,N_panels
    real(8),intent(IN)::T_s(N_nodes,N_panels),T_F,T_CE,T_L,T_amb,P_amb,H_rec,R_rec,A_node,A_F,A_L,A_O
    real(8)::T_w_avg, A_aperture, A_cavity, s_LO, h_char_LO, L_char, beta_amb, mu_amb, k_amb, rho_amb, theta_max_LO, theta_stag_LO, theta_LO, Gr_amb, &
              theta_stag_eff_LO, theta_bar_LO, h_0_LO, Nu_LeibAndOrth
    real(8),parameter::pi=3.14159265
    real(8),intent(OUT)::h_LeibAndOrth, q_convection


    L_char=max(H_rec,2*R_rec)

    !Properties at ambient temperature
    beta_amb = 1./T_amb 
    mu_amb = Viscosity(1.d0,T_amb,1.d0)
    k_amb = Conductivity(1.d0,T_amb,1.d0)
    rho_amb = Density(1.d0,T_amb,P_amb)
    
    !Average wall temperature of the cavity surfaces
	T_w_avg = (SUM(T_s(1:N_nodes,1:N_panels)*A_node)+T_F*A_F+T_CE*A_F+T_L*A_L)/(2*A_F+A_L+N_nodes*N_panels*A_node) 

    !Grashof number
    Gr_amb = grav*beta_amb*(T_w_avg-T_amb)*L_char**3.*rho_amb**2./mu_amb**2.
        
    A_aperture=A_O
    A_cavity=N_nodes*N_panels*A_node+2.*A_F+A_L
    s_LO=0.56-1.01*(A_aperture/A_cavity)**0.5
    
    
    !angles in degree
    theta_max_LO=-23.-260.*(A_aperture/A_cavity)  !VALID FOR (A_aperture/A_cavity)<=0.2
    theta_stag_LO=90. 
    theta_LO=0.
    !In case the cavity is inclined(theta_LO not equal 0)
    !theta_stag_eff_LO=theta_stag_LO+(90.-theta_stag_LO)*(theta_stag_LO-theta_LO)/theta_stag_LO
    theta_stag_eff_LO=theta_stag_LO
    
    theta_bar_LO=(theta_LO-theta_stag_eff_LO)/(theta_max_LO-theta_stag_eff_LO)
      
    h_0_LO=1.-COSD(((0.-theta_stag_eff_LO)/(theta_max_LO-theta_stag_eff_LO))**0.85*pi)!    
    
    h_char_LO=1./h_0_LO*(1-COSD(theta_bar_LO**0.85*pi))
   
    !Nusselt number 
    Nu_LeibAndOrth=0.106*Gr_amb**(1./3.)*(T_w_avg/T_amb)**0.18*(4.256*(A_aperture)/(A_cavity))**s_LO*h_char_LO
    
    !Heat transfer coefficient
    h_LeibAndOrth = (k_amb/L_char)*Nu_LeibAndOrth
    
    
    q_convection=h_LeibAndOrth*((T_w_avg-T_bulk)*(2*A_F+A_L+N_nodes*N_panels*A_node))

  
   END SUBROUTINE
   
   
   

    SUBROUTINE Forced_Combined_Flat_Plates(N_nodes,N_panels,T_s,T_F,T_CE,T_L,T_amb,T_bulk,P_amb,H_rec,H_lip,R_rec,W_panel,A_node,A_F,A_L,A_O,h_CFP_floor_turb,h_CFP_ceiling_turb,h_CFP_panels_turb,h_CFP_lip_turb,h_CFP,q_convection)
    
    implicit none
    !**********************************************************************************
    !This subroutine calculates the total convective heat losses from the receiver
    !with the correlation presented in Petukhov and Popov (1963)  !ST ( Heat Exchanger Design Handbook 2008 G.F. Hewitt Section 2.5 II b) )
    !  The inputs are:
    !    - N_nodes -> number of vertical nodes per receiver panel [-]
    !    - N_panels -> number of receiver panels [-]
    !    - T_s -> the array of surface temperature for every active surface node [K]
    !    - T_F -> the temperature of the receiver FLOOR [K]
    !    - T_CE -> the temperature of the receiver CEILING [K]
    !    - T_L -> the temperature of the receiver LIP [K]
    !    - T_amb -> ambient temperature [K]
    !    - T_bulk -> bulk temperature from Clausing1983
    !    - H_rec -> internal receiver height [m]
    !    - H_lip -> height of the upper lip [m]
    !    - W_panel -> width of one receiver panel [m]
    !    - R_rec -> Receiver radius
	!	 - A_node -> area of active receiver surface segment [m^2]
	!	 - A_F -> area of receiver floor [m^2]
	!    - A_L -> area of receiver lip [m^2]
	!	 - A_O -> area of receiver opening [m^2]	
    !    - P_amb -> ambient pressure [Pa]
    !
    !  The outputs are:
    !    - h_VlietANDLiu_sub -> conective heat loss coefficient [W/m^2-K]3
    !    - v_b
    !    - h_CFP_x_turb,h_CFP_floor_turb,h_CFP_ceiling_turb,h_CFP_panels_turb,h_CFP_lip_turb,h_CFP,q_convection
    !**********************************************************************************
    integer::N_nodes,N_panels
    real(8),intent(IN)::T_s(N_nodes,N_panels),T_F,T_CE,T_L,T_amb,T_bulk,P_amb,&
                        H_rec,H_lip,R_rec,W_panel,A_node,A_F,A_L,A_O
    real(8)::T_panels_avg, T_w_avg, T_film, beta_CFP, c_p_CFP, rho_CFP, k_CFP, mu_CFP,&
             Pr_CFP,L_floor, Re_CFP_floor, Re_CFP_CEILiNG, Re_CFP_PANELS,&
             Re_CFP_LIP, NU_CFP_FLOOR_turb,NU_CFP_CEILING_turb,v_b,&
             NU_CFP_Panels_turb,NU_CFP_LIP_turb,q_conv_CFP_floor,&
             q_conv_CFP_ceiling,q_conv_CFP_Panels,q_conv_CFP_LIP,q_conv_CFP
    real(8),intent(OUT):: h_CFP_floor_turb,h_CFP_ceiling_turb,h_CFP_panels_turb,h_CFP_lip_turb,h_CFP,q_convection
	real(8),parameter::grav=9.81

	
	T_panels_avg = (SUM(T_s(1:N_nodes,1:N_panels)))/ DBLE((N_nodes*N_panels))
	T_w_avg = (SUM(T_s(1:N_nodes,1:N_panels)*A_node)+T_F*A_F+T_CE*A_F+T_L*A_L)/(2*A_F+A_L+N_nodes*N_panels*A_node) 
    !T_film=(T_w_avg+T_amb)/2
    
    !!!PROPERTIES DETERMINED AT BULK TEMPERATURE - clausing 1983
     
    !Volume expansion coefficient [1/K]
    beta_CFP = 1./T_bulk

    !Specific heat [J/kg-K]
    c_p_CFP = SpecHeat(1.d0,T_bulk,1.d0)*1000.0

    !Density [kg/m3]
    rho_CFP = Density(1.d0,T_bulk,P_amb)  
 
    !Conductivity [W/m-K]
    k_CFP = Conductivity(1.d0,T_bulk,1.d0)

    !Viscosity [Pa-s]
    mu_CFP = Viscosity(1.d0,T_bulk,1.d0)

  
    ! Correlation is modeling fully developed turbulent forced convection on a flat plate for 5x10^5 < Re_l <  10^7 AND 0.5 < Pr < 2000
    ! Re_l can have values up to 3*10^7 !!!!
    ! The forced convection model uses the buoyancy induced flow velocity to estimate an upper bound for natural convection
   
    !Velocity due to bouyant forces
     v_b=sqrt(grav*beta_CFP*(T_w_avg-T_amb)*(H_rec-H_lip))   

    
    !Prandtl number
    Pr_CFP = c_p_CFP*mu_CFP/k_CFP
    
 
    !Floor
    L_floor=A_F/(2*R_rec+4*W_panel)
    
    Re_CFP_floor=(v_b*(L_floor)*rho_CFP)/mu_CFP
    NU_CFP_FLOOR_turb=( 0.0376*Re_CFP_floor**0.8*Pr_CFP )/( 1+2.443*Re_CFP_floor**(-0.1)*(Pr_CFP**(2./3.)-1) )
    h_CFP_floor_turb = (k_CFP/(L_floor))*NU_CFP_floor_turb
    q_conv_CFP_floor=A_F*h_CFP_floor_turb*(T_F-T_bulk)
    
    
    !Ceiling
    Re_CFP_CEILING=Re_CFP_floor!(v_b*(L_floor)*rho_CFP)/mu_CFP
    NU_CFP_CEILING_turb=NU_CFP_FLOOR_turb!( 0.0376*Re_CFP_floor**0.8*Pr_CFP )/( 1+2.443*Re_CFP_floor**(-0.1)*(Pr_CFP**(2./3.)-1) )
    h_CFP_ceiling_turb=h_CFP_floor_turb !(k_CFP/(H_L_floor))*NU_CFP_floor_turb
    q_conv_CFP_ceiling=A_F*h_CFP_ceiling_turb*(T_CE-T_bulk)
   
 
    !Panels
    Re_CFP_PANELS=(v_b*(H_rec)*rho_CFP)/mu_CFP
    NU_CFP_PANELS_turb=( 0.0376*Re_CFP_panels**0.8*Pr_CFP )/( 1+2.443*Re_CFP_panels**(-0.1)*(Pr_CFP**(2./3.)-1) )
    h_CFP_panels_turb = (k_CFP/(H_rec))*NU_CFP_panels_turb
    q_conv_CFP_Panels=N_nodes*N_panels*A_node*h_CFP_panels_turb*(T_panels_avg-T_bulk)

       
    !Lip
    Re_CFP_LIP=(v_b*(H_LIP)*rho_CFP)/mu_CFP
    NU_CFP_LIP_turb=( 0.0376*Re_CFP_LIP**0.8*Pr_CFP )/( 1+2.443*Re_CFP_LIP**(-0.1)*(Pr_CFP**(2./3.)-1) )
    h_CFP_lip_turb = (k_CFP/(H_lip))*NU_CFP_LIP_turb
    q_conv_CFP_LIP=A_L*h_CFP_LIP_turb*(T_L-T_bulk)
     

    q_conv_CFP=q_conv_CFP_floor+q_conv_CFP_ceiling+q_conv_CFP_Panels+q_conv_CFP_LIP
    h_CFP=q_conv_CFP/((T_w_avg-T_bulk)*(2*A_F+A_L+N_nodes*N_panels*A_node))
    
    q_convection=q_conv_CFP
    
   
	END SUBROUTINE
	
	
	
	
	SUBROUTINE Natural_Combined_Flat_Plates(N_nodes,N_panels,T_s,T_F,T_CE,T_L,T_amb,T_bulk,P_amb,H_rec,H_lip,R_rec,W_panel,W_aperture,A_node, A_F,A_L,A_O,h_conv_tot,q_convection )

    implicit none
    !**********************************************************************************
    !This subroutine calculates the total convective heat losses from the receiver
    !with the correlation presented in Petukhov and Popov (1963)  !ST ( Heat Exchanger Design Handbook 2008 G.F. Hewitt Section 2.5 II b) )
    !  The inputs are:
    !    - N_nodes -> number of vertical nodes per receiver panel [-]
    !    - N_panels -> number of receiver panels [-]
    !    - T_s -> the array of surface temperature for every active surface node [K]
    !    - T_F -> the temperature of the receiver FLOOR [K]
    !    - T_amb -> ambient temperature [K]
    !    - P_amb -> ambient pressure [Pa]
    !    - H_rec -> internal receiver height [m]	
	!    - Flux_avg -> average flux on cavity [W-m^2]
    !  The outputs are:
    !    - h_VlietANDLiu_sub -> conective heat loss coefficient [W/m^2-K]
    !**********************************************************************************
    integer::N_nodes,N_panels
    real(8),intent(IN)::T_s(N_nodes,N_panels),T_F,T_CE,T_L,T_amb,T_bulk,P_amb,&
                        H_rec,H_lip,R_rec,W_panel,W_aperture,A_node,A_F,A_L,A_O
                        
    real(8)::T_panels_avg, T_w_avg, T_film, T_film_floor, T_film_ceiling, T_film_panels, T_film_lip, beta_floor, beta_ceiling, beta_panels, beta_lip,&
             K_floor, K_ceiling, K_panels, K_lip, c_p_floor, c_p_ceiling, c_p_panels, c_p_lip, mu_floor, mu_ceiling,&
             mu_panels, mu_lip, Pr_floor, Pr_ceiling, Pr_panels, Pr_lip, rho_floor, rho_ceiling, rho_panels, rho_lip,&
             Gr_floor, Gr_ceiling, Gr_panels, Gr_lip, Ra_floor, Ra_ceiling, Ra_panels, Ra_lip, Nu_floor, Nu_ceiling,&
              Nu_panels, Nu_lip, L_floor, h_conv_floor, h_conv_ceiling, h_conv_panels, h_conv_lip, q_conv_floor, q_conv_ceiling, q_conv_panels, q_conv_LIP, q_conv_sum
             
    real(8),intent(OUT):: h_conv_tot,q_convection
    
	real(8),parameter::grav=9.81
	
	T_panels_avg = (SUM(T_s(1:N_nodes,1:N_panels)))/ DBLE((N_nodes*N_panels))
	!T_w_avg = (SUM(T_s(1:N_nodes,1:N_panels))+T_F+T_CE+T_L)/ DBLE((N_nodes*N_panels)+3)
	T_w_avg = (SUM(T_s(1:N_nodes,1:N_panels)*A_node)+T_F*A_F+T_CE*A_F+T_L*A_L)/(2*A_F+A_L+N_nodes*N_panels*A_node) 
	T_film=(T_w_avg+T_amb)/2
    
    !Film temperature for the property evaluation
    T_film_floor = (T_F+T_bulk)/2.0                   !"Film temperature at the floor"
    T_film_ceiling = (T_CE+T_bulk)/2.0                !"Film temperature at the ceiling"          
    T_film_panels = (T_panels_avg+T_bulk)/2.0         !"Film temperature of the panels"
    T_film_lip = (T_L+T_bulk)/2.0                     !"Film temperature of the lip"
   

   
    !Volume expansion coefficient [1/K]
    beta_floor = 1./T_film_floor 
    beta_ceiling = 1./T_film_ceiling 
    beta_panels = 1./T_film_panels 
    beta_lip = 1./T_film_lip

    !Conductivity [W/m-K]
    K_floor = Conductivity(1.d0,T_film_floor,1.d0)
    K_ceiling = Conductivity(1.d0,T_film_ceiling,1.d0) 
    K_panels = Conductivity(1.d0,T_film_panels,1.d0) 
    K_lip = Conductivity(1.d0,T_film_lip,1.d0)

    !Specific heat [J/kg-K]
    c_p_floor = SpecHeat(1.d0,T_film_floor,1.d0)*1000.0
    c_p_ceiling = SpecHeat(1.d0,T_film_ceiling,1.d0) *1000.0
    c_p_panels = SpecHeat(1.d0,T_film_panels,1.d0)*1000.0 
    c_p_lip = SpecHeat(1.d0,T_film_lip,1.d0)*1000.0

    !Viscosity [Pa-s]
    mu_floor = Viscosity(1.d0,T_film_floor,1.d0)
    mu_ceiling = Viscosity(1.d0,T_film_ceiling,1.d0) 
    mu_panels = Viscosity(1.d0,T_film_panels,1.d0) 
    mu_lip = Viscosity(1.d0,T_film_lip,1.d0)

    !Prandtl number
    Pr_floor = (c_p_floor*mu_floor)/k_floor
    Pr_ceiling = (c_p_ceiling*mu_ceiling)/k_ceiling
    Pr_panels = (c_p_panels*mu_panels)/k_panels
    Pr_lip = (c_p_lip*mu_lip)/k_lip


    !Density [kg/m3]
    rho_floor = Density(1.d0,T_film_floor,P_amb)
    rho_ceiling = Density(1.d0,T_film_ceiling,P_amb)
    rho_panels = Density(1.d0,T_film_panels,P_amb)
    rho_lip = Density(1.d0,T_film_lip,P_amb) 


    !Grashof number
    Gr_floor=((grav*beta_floor*(T_F-T_bulk)*(A_F/(4*W_panel+W_aperture))**3)/((mu_floor/rho_floor)**2))
    Gr_ceiling=((grav*beta_ceiling*(T_CE-T_bulk)*(A_F/(4*W_panel+W_aperture))**3)/((mu_ceiling/rho_ceiling)**2))
    Gr_panels=((grav*beta_panels*(T_panels_avg-T_bulk)*((20.*A_node)/(4*W_panel+W_aperture))**3)/((mu_panels/rho_panels)**2))
    Gr_lip=((grav*beta_lip*(T_L-T_bulk)*(A_L/(4*W_panel+W_aperture))**3)/((mu_lip/rho_lip)**2)) 

    !Rayleigh number
    Ra_floor = abs(Gr_floor*Pr_floor)
    Ra_ceiling = abs(Gr_ceiling*Pr_ceiling)
    Ra_panels = abs(Gr_panels*Pr_panels)
    Ra_lip = abs(Gr_lip*Pr_lip)
    
   
   
    !VARIABLES
   
    Nu_floor=( 0.766*Ra_floor**(1./5.) )/( 1+(0.322/Pr_floor)**(11./20.) )**(4./11.)
    Nu_ceiling=0.6*( Ra_ceiling*( 1+(0.492/Pr_ceiling)**(9./15.) )**(-16./9.) )**(1./5.)
    Nu_panels=0.15*( Ra_panels*( 1+(0.492/Pr_panels)**(9./15.) )**(-16./9.)  )**(1./3.)
    Nu_lip=0.15*( Ra_lip*( 1+(0.492/Pr_lip)**(9./15.) )**(-16./9.)  )**(1./3.)
    
    L_floor=A_F/(2*R_rec+4*W_panel)
   
    h_conv_floor = (k_floor/(L_floor))*NU_floor
    h_conv_ceiling = (k_ceiling/(L_floor))*NU_ceiling
    h_conv_panels = (k_panels/(H_rec))*NU_panels
    h_conv_lip = (k_lip/(H_lip))*NU_lip
   
   
    q_conv_floor=A_F*h_conv_floor*(T_F-T_bulk)
    q_conv_ceiling=A_Ce*h_conv_ceiling*(T_CE-T_bulk)
    q_conv_panels=A_node*N_nodes*N_panels*h_conv_panels*(T_panels_avg-T_bulk)
    q_conv_LIP=A_L*h_conv_LIP*(T_L-T_bulk)
   
    q_conv_sum=q_conv_floor+q_conv_ceiling+q_conv_panels+q_conv_LIP
    h_conv_tot=q_conv_sum/((T_w_avg-T_bulk)*(2*A_F+A_L+N_nodes*N_panels*A_node))
    
    q_convection=q_conv_sum
	END SUBROUTINE
	
	
END SUBROUTINE
!-----------------------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------
