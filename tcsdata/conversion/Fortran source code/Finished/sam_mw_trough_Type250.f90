SUBROUTINE TYPE250 (TIME,XIN,OUT,T,DTDT,PAR,INFO,ICNTRL,*)
!************************************************************************
! Object: Parabolic trough field model
! Simulation Studio Model: Type250
! 
! Author: Michael J. Wagner
! Editor: Ty Neises
! Date:	 December 3, 2009
! Modified: September 7, 2011
 
! COPYRIGHT 2010 NATIONAL RENEWABLE ENERGY LABORATORY

! Doc. tables updated 2011-03-29 - MJW
!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Parameters
!    1| nSCA                             | Number of SCA's in a loop                                         | none             | none             
!    2| nHCEt                            | Number of HCE types                                               | none             | none             
!    3| nColt                            | Number of collector types                                         | none             | none             
!    4| nHCEVar                          | Number of HCE variants per type                                   | none             | none             
!    5| nLoops                           | Number of loops in the field                                      | none             | none             
!    6| dt                               | Timestep                                                          | hr               | hr               
!    7| eta_pump                         | HTF pump efficiency                                               | none             | none             
!    8| HDR_rough                        | Header pipe roughness                                             | m                | m                
!    9| lattitude                        | Site lattitude read from weather file                             | deg              | rad              
!   10| theta_stow                       | stow angle                                                        | deg              | rad              
!   11| theta_dep                        | deploy angle                                                      | deg              | rad              
!   12| Row_Distance                     | Spacing between rows (centerline to centerline)                   | m                | m                
!   13| FieldConfig                      | Number of subfield headers                                        | none             | none             
!   14| T_startup                        | The required temperature of the system before the power block can be switched on| C                | K                
!   15| pb_rated_cap                     | Rated plant capacity                                              | MWe              | MWe              
!   16| m_dot_htfmin                     | Minimum loop HTF flow rate                                        | kg/s             | kg/s             
!   17| m_dot_htfmax                     | Maximum loop HTF flow rate                                        | kg/s             | kg/s             
!   18| T_loop_in_des                    | Design loop inlet temperature                                     | C                | K                
!   19| T_loop_out                       | Target loop outlet temperature                                    | C                | K                
!   20| Fluid                            | Field HTF fluid number                                            | none             | none             
!   21| T_field_ini                      | Initial field temperature                                         | C                | K                
!   22| LU_dat                           | Data file logical unit                                            | none             | none             
!   23| LU_FL                            | Fluid property file logical unit                                  | none             | none             
!   24| T_fp                             | Freeze protection temperature (heat trace activation temperature) | C                | K                
!   25| I_bn_des                         | Solar irradiation at design                                       | W/m2             | W/m2             
!   26| V_hdr_max                        | Maximum HTF velocity in the header at design                      | m/s              | m/s              
!   27| V_hdr_min                        | Minimum HTF velocity in the header at design                      | m/s              | m/s              
!   28| Pipe_hl_coef                     | Loss coefficient from the header, runner pipe, and non-HCE piping | W/m2-K           | W/m2-K           
!   29| SCA_drives_elec                  | Tracking power, in Watts per SCA drive                            | W/SCA            | W/SCA            
!   30| LU_diam                          | Logical unit of the file containing calculated header diameters   | none             | none             
!   31| fthrok                           | Flag to allow partial defocusing of the collectors                | none             | none             
!   32| fthrctrl                         | Defocusing strategy                                               | none             | none             
!   33| ColTilt                          | Collector tilt angle (0 is horizontal, 90deg is vertical)         | deg              | rad              
!   34| ColAz                            | Collector azimuth angle                                           | deg              | rad              
!   35| accept_mode                      | Acceptance testing mode? (1=yes, 0=no)                            | none             | none             
!   36| accept_init                      | In acceptance testing mode - require steady-state startup         | none             | none             
!   37| accept_loc                       | In acceptance testing mode - temperature sensor location (1=hx,2=loop)| none             | none             
!   38| solar_mult                       | Solar multiple                                                    | none             | none             
!   39| mc_bal_hot                       | The heat capacity of the balance of plant on the hot side         | kWht/K-MWt       | J/K-MWt          
!   40| mc_bal_cold                      | The heat capacity of the balance of plant on the cold side        | kWht/K-MWt       | J/K-MWt          
!   41| mc_bal_sca                       | Non-HTF heat capacity associated with each SCA - per meter basis  | Wht/K-m          | J/K-m            

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!This component requires a geometry file that contains the remainder of the solar field parameter inputs. These are defined
!as follows, and must appear in the order specified. Input loops should be entered sequentially in the file.
! ...... Loop for 1..(Number of collector types) ......
!    1| OptCharType                      | The optical characterization method                               | none             | none             
!    2| CollectorType                    | {1=user defined, 2=LS-2, 3=LS-3, 4=IST}                           | none             | none             
!    3| W_aperture                       | The collector aperture width (Total structural area.. used for shadowing)| m                | m                
!    4| A_aperture                       | Reflective aperture area of the collector                         | m2               | m2               
!    5| IamF0                            | Incident angle modifier 0th order term                            | none             | none             
!    6| IamF1                            | Incident angle modifier 1st order term                            | none             | none             
!    7| IamF2                            | Incident angle modifier 2nd order term                            | none             | none             
!    8| reflectivity                     | Base solar-weighted mirror reflectivity value                     | none             | none             
!    9| TrackingError                    | User-defined tracking error derate                                | none             | none             
!   10| GeomEffects                      | User-defined geometry effects derate                              | none             | none             
!   11| Rho_mirror_clean                 | User-defined clean mirror reflectivity                            | none             | none             
!   12| Dirt_mirror                      | User-defined dirt on mirror derate                                | none             | none             
!   13| Error                            | User-defined general optical error derate                         | none             | none             
!   14| Ave_Focal_Length                 | The average focal length of the collector                         | m                | m                
!   15| L_SCA                            | The length of the SCA                                             | m                | m                
!   16| L_aperture                       | The length of a single mirror/HCE unit                            | m                | m                
!   17| ColperSCA                        | The number of individual collector sections in an SCA             | none             | none             
!   18| Distance_SCA                     |  piping distance between SCA's in the field                       | m                | m                
! ......//
! ...... Loop for 1..(Number of receiver types) .......
! ............ Loop for 1..(Number of receiver variants) ............
!   19| HCE_FieldFrac                    | The fraction of the field occupied by this HCE type               | none             | none             
!   20| D_2                              | The inner absorber tube diameter                                  | m                | m                
!   21| D_3                              | The outer absorber tube diameter                                  | m                | m                
!   22| D_4                              | The inner glass envelope diameter                                 | m                | m                
!   23| D_5                              | The outer glass envelope diameter                                 | m                | m                
!   24| D_p                              | The diameter of the absorber flow plug (optional)                 | m                | m                
!   25| Flow_type                        | The flow type through the absorber                                | none             | none             
!   26| Rough                            | Roughness of the internal surface                                 | m                | m                
!   27| alpha_env                        | Envelope absorptance                                              | none             | none             
!   28| epsilon_3                        | Absorber emittance                                                | none             | none             
!   29| alpha_abs                        | Absorber absorptance                                              | none             | none             
!   30| Tau_envelope                     | Envelope transmittance                                            | none             | none             
!   31| EPSILON_4                        | Inner glass envelope emissivities (Pyrex)                         | none             | none             
!   32| EPSILON_5                        | Outer glass envelope emissivities (Pyrex)                         | none             | none             
!   33| GlazingIntactIn                  | The glazing intact flag {1=true, else=false}                      | none             | none             
!   34| P_a                              | Annulus gas pressure                                              | torr             | Pa               
!   35| AnnulusGas                       | Annulus gas type (1=air, 26=Ar, 27=H2)                            | none             | none             
!   36| AbsorberMaterial                 | Absorber material type                                            | none             | none             
!   37| Shadowing                        | Receiver bellows shadowing loss factor                            | none             | none             
!   38| Dirt_HCE                         | Loss due to dirt on the receiver envelope                         | none             | none             
!   39| Design_loss                      | Receiver heat loss at design                                      | W/m              | W/m              
! ............//
! ......//
! ...... Loop for 1..(Number of SCAs) .......
!   40| SCAInfoArray(:,1)                | HCE type for each SCA in the loop                                 | none             | none             
!   41| SCAInfoArray(:,2)                | Collector type for each SCA in the loop                           | none             | none             
!   42| SCADefocusArray(:)               | Order in which the SCA's should be defocused                      | none             | none             
! ......//

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Inputs
!    1| I_b                              | Direct normal incident solar irradiation                          | kJ/m2-hr         | W/m2             
!    2| T_db                             | Dry bulb air temperature                                          | C                | K                
!    3| V_wind                           | Ambient windspeed                                                 | m/s              | m/s              
!    4| P_amb                            | Ambient pressure                                                  | atm              | Pa               
!    5| T_dp                             | The dewpoint temperature                                          | C                | K                
!    6| shift                            | Shift in longitude from local standard meridian                   | deg              | rad              
!    7| T_cold_in                        | HTF return temperature                                            | C                | K                
!    8| m_dot_in                         | HTF mass flow rate at the inlet                                   | kg/hr            | kg/s             
!    9| defocus                          | Defocus control                                                   | none             | none             
!   10| SolarAz                          | Solar azimuth angle reported by the Type15 weather file           | deg              | rad              

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Outputs
!    1| T_sys_h                          | Solar field HTF outlet temperature                                | C                | K                
!    2| m_dot_avail                      | HTF mass flow rate from the field                                 | kg/hr            | kg/s             
!    3| q_avail                          | Thermal power produced by the field                               | MWt              | MWt              
!    4| DP_tot                           | Total HTF pressure drop                                           | bar              | bar              
!    5| W_dot_pump                       | Required solar field pumping power                                | MWe              | kWe              
!    6| E_fp_tot                         | Freeze protection energy                                          | J                | W                
!    7| qq                               | Number of iterations required to solve                            | none             | none             
!    8| T_sys_c                          | Collector inlet temperature                                       | C                | K                
!    9| EqOpteff                         | Collector equivalent optical efficiency                           | none             | none             
!   10| SCAs_def                         | The fraction of focused SCA's                                     | none             | none             
!   11| m_dot_htf_tot                    | The actual flow rate through the field..                          | kg/hr            | kg/s             
!   12| E_bal_startup                    | Startup energy consumed                                           | MWt              | W                
!   13| - no name -                      | Total power incident on the field                                 | MWt              | MWt              
!   14| q_abs_SCAtot                     | Total absorbed energy                                             | MWt              | Wt               
!   15| q_loss_SCAtot                    | Total HCE thermal losses                                          | MWt              | Wt               
!   16| m_dot_htf                        | Flow rate in a single loop                                        | kg/s             | kg/s             
!   17| q_1abs_tot                       | Field average receiver thermal losses                             | W/m              | W/m              
!   18| SCA_par_tot                      | Parasitic electric power consumed by the SC                       | MWe              | We               
!   19| Pipe_hl                          | Pipe heat loss in the hot header and the hot runner               | MWt              | MWt              
!   20| q_dump                           | Dumped thermal energy                                             | MWt              | MWt              
!   21| Theta_ave                        | Field average theta value                                         | deg              | rad              
!   22| CosTh_ave                        | Field average costheta value                                      | none             | none             
!   23| IAM_ave                          | Field average incidence angle modifier                            | none             | none             
!   24| RowShadow_ave                    | Field average row shadowing loss                                  | none             | none             
!   25| EndLoss_ave                      | Field average end loss                                            | none             | none             
!   26| - no name -                      | DNI_x_CosTh                                                       | W/m2             | W/m2             
!   27| - no name -                      | Q_inc_x_CosTh                                                     | MWt              | MWt              
!   28| SolarAlt                         | Solar altitude used in optical calculations                       | deg              | rad              
!   29| SolarAz                          | Solar azimuth used in optical calculations                        | deg              | rad              
!   30| - no name -                      | T_loop_out                                                        | C                | C                
!   31| c_htf_ave                        | Average solar field specific heat                                 | J/kg-K           | J/kg-K           
!   32| q_field_delivered                | Total solar field thermal power delivered                         | MWt              | MWt              
!   33| eta_thermal                      | Solar field thermal efficiency (power out/ANI)                    | none             | none             
!   34| E_loop_accum                     | Accumulated internal energy change rate in the loops ONLY         | MWht             | MWht             
!   35| E_hdr_accum                      | Accumulated internal energy change rate in the headers/SGS        | MWht             | MWht             
!   36| E_tot_accum                      | Total accumulated internal energy change rate                     | MWht             | MWht             
!   37| E_field                          | Accumulated internal energy in the entire solar field             | MWht             | MWht             

!-----------------------------------------------------------------------------------------------------------------------
!    TRNSYS acess functions (allow to acess TIME etc.) 
USE TrnsysConstants
USE TrnsysFunctions
use global_props

!-----------------------------------------------------------------------------------------------------------------------
! required by the multi-dll version of TRNSYS
!DEC$ATTRIBUTES DLLEXPORT :: TYPE250

implicit none 

!TRNSYS declarations
real(8):: time
integer*4:: info(15), iunit, itype, icntrl
integer*4,parameter::np=41,ni=10,nout=37,nd=0,ns=100, nea=15 !ns must be 2 + twice as many as the number of SCA's in the loop

!Dimension the TRNSYS variables
real(8)::xin(ni),out(nout),par(np),stored(ns),T(nd),dtdt(nd) 

!-----------------------------------------------------------------------------------------------------------------------


!---- User variable declarations----------------------------------------------------------------------------------------

!Inputs and parameters
real(8):: V_wind, SolarAlt, pi, skytemp, hour, P_amb, T_db, T_air, T_dp, T_sky,I_b, SolarAz, fluid, m_dot_htf, m_dot_htfX,& 
          m_dot_htfmax, m_dot_htfmin, T_loop_in, T_loop_out,T_loop_outX,err, c_htf_ave, q_abs_maxOT, &
          q_check, theta_stow, theta_dep,dt, B, EOT, Dec, SolarNoon, shift, solartime, omega, lattitude, T_sys_c, &
          T_sys_c_last, T_sys_h, T_sys_h_last, Row_distance, density, specheat, FieldConfig, DP_hdr_hot, DP_hdr_cold, &
          eta_pump, rho_hdr_hot, rho_hdr_cold, rho_ave, W_dot_pump, m_dot_htf_tot, DP_loop, DP_tot,x1,x2,x3,&
          HDR_rough,DP_IOCOP,PressureDrop, DP_toField,DP_fromField, m_dot_header,E_avail_tot, E_accum_loop, E_accum_hdr, q_avail,v_tot, &
          v_header, v_loop_tot, v_tofrom_sgs, c_hdr_cold, E_bal_startup, T_startup, mc_bal_hot, mc_bal_cold, mc_bal_sca, pb_rated_cap, &
          tsnow, dephr1, dephr2, dephr3, deptime, stwhr1, stwhr2, stwhr3, stwtime, hrA, hrB, ftrack, midtrack,&
          stdtime, T_loop_in_des,T_field_ini,T_fp, E_fp_tot, m_node, T_cold_in, m_dot_in, v_hot, v_cold, &
          c_hdr_hot,m_dot_avail,defocus, SCAs_def,q_SCA_tot, V_hdr_min, V_hdr_max, m_dot_design, &
          el_des, opteff_des, I_bn_des, Ap_tot, L_tot, loss_tot, Pipe_hl_coef, SCA_drives_elec,LU_diam,&
          Pipe_hl_cold, pipe_hl_hot, Pipe_hl_tot, SCA_par_tot, L_int, q_dump, EqOpteff, Theta_ave, CosTh_ave, &
          IAM_ave, RowShadow_ave, EndLoss_ave,fthrctrl, eps_3, flag, xx(nea), yy(nea), ColTilt, ColAz, e1, e2, &
          q_field_delivered, eta_thermal, m_dot_temp, solar_mult, v_sgs, pump_sgs, q_design,& 
          accept_loc, E_field, TrackAngle, CosTh, Theta, c_htf_j, rho_htf_j, m_dot_lower, m_dot_upper,&
          y_upper,y_lower,upmult,t_tol,m_dot_run_in,m_dot_header_in,&
          T_cold_in_1,T_in_lower,y_fp_lower,T_in_upper,y_fp_upper,c_hdr_cold_last,&
          N_run_mult,Runner_hl_hot,Header_hl_hot,Header_hl_cold,Runner_hl_cold,&
          defocus_new, defocus_old
real(8),allocatable::D_2(:,:), D_3(:,:), D_4(:,:), D_5(:,:), D_p(:,:), D_h(:,:), A_cs(:,:), &
                     alpha_env(:,:), epsilon_3t(:,:,:), epsilon_3(:,:,:), alpha_abs(:,:), Tau_envelope(:,:), &
                     epsilon_4(:,:), epsilon_5(:,:), GlazingIntactIn(:,:), Flow_type(:,:),&
                     HCE_FieldFrac(:,:), AbsorberMaterial(:,:), P_a(:,:), annulusGas(:,:), &
                     q_loss_SCAtot(:), q_abs_SCAtot(:), q_SCA(:), ColperSCA(:), &
                     L_SCA(:), L_actSCA(:), L_aperture(:), Distance_SCA(:), Rough(:,:), Shadowing(:,:), &
                     Dirt_HCE(:,:), design_loss(:,:)
real(8),allocatable::A_aperture(:), IamF0(:), IamF1(:),&
                     IamF2(:), IAM(:), reflectivity(:), TrackingError(:), &
                     GeomEffects(:), Rho_mirror_clean(:), Dirt_mirror(:), Error(:),&
                     ColOptEff(:,:), q_i(:), Ave_focal_length(:),&
                     W_aperture(:), EndGain(:,:), EndLoss(:,:), RowShadow(:)
real(8),allocatable:: T_htf_in(:), T_htf_out(:), T_htf_ave(:), q_loss(:), q_abs(:), c_htf(:),rho_htf(:), DP_tube(:),&
                      T_htf_in0(:), T_htf_out0(:), T_htf_ave0(:),E_abs_field(:), E_avail(:), E_int_loop(:), E_accum(:), E_abs_max(:), v_1(:),D_hdr(:),&
                      xpar(:), xarr(:,:),E_fp(:), q_1abs_tot(:), q_1abs(:), D_runner(:), L_runner(:)
integer,allocatable::OptCharType(:),CollectorType(:),SCAInfoArray(:,:), SCADefocusArray(:), epsilon_3l(:,:)
logical,allocatable::GlazingIntact(:,:)
         
integer:: i, j, k, m, n, HT, CT, nSCA, nHCEt, nColt, nHCEVar, nLoops, qq, dfcount, day_of_year, SolveMode,up1,LU_dat,&
          filelen,LU_fl, nhdrsec, nfsec, li, trkflag, eps3sav, lxarr(nea), nrunsec, open_stat12
logical:: fthrok, accept_mode, accept_init, upflag, lowflag, no_fp, fp_lowflag, fp_upflag

! --- Messages for this Type -------------------------------------------------------------------------------------------
character:: test*300
character(len=maxMessageLength) :: aString, msg(10)

save::eps3sav
msg(1) = ""

!Set parameters
pi=3.1415926 ; dfcount = 0; flag=8675309.d0

! --- Initial call to detect the TRNSYS version for which this Type is written -----------------------------------------
if (info(7) .eq. -2) then
    info(12) = 16   ! This component is a TRNSYS 16 Type
    return 1
endif


! --- Very last call in simulation -------------------------------------------------------------------------------------

if (info(8) == -1) then
    if(allocated(D_2)) then
    !deallocate space for all of the HCE arrays
    deallocate(D_2, D_3, D_4, D_5, D_p, D_h, A_cs, alpha_env, epsilon_3t, epsilon_3l, epsilon_3, alpha_abs, Tau_envelope, epsilon_4, &
               epsilon_5, GlazingIntactIn, Flow_type, GlazingIntact,HCE_FieldFrac,AbsorberMaterial,P_a,annulusGas,&
               Rough, design_loss)
    !deallocate space for all the collector arrays
    deallocate(A_aperture, IamF0, IamF1,IamF2, IAM, reflectivity, Shadowing, &
               TrackingError, GeomEffects, Rho_mirror_clean, Dirt_mirror, Dirt_HCE, Error,ColOptEff, OptCharType, &
               CollectorType, q_i, Ave_focal_length, W_aperture, EndGain, EndLoss, RowShadow,L_SCA,&
               L_actSCA,L_aperture,ColperSCA,Distance_SCA)
    !deallocate space for the SCA info array. For each SCA, contains (HCE type, Collector type)
    deallocate(SCAInfoArray,T_htf_in, T_htf_out, T_htf_ave, q_loss, q_abs, c_htf,rho_htf,DP_tube,SCADefocusArray, &
               E_abs_field, E_avail, E_int_loop, E_accum, E_abs_max,v_1,q_loss_SCAtot, q_abs_SCAtot, q_SCA, T_htf_in0, T_htf_out0, &
               T_htf_ave0)
    endif
    
    if(allocated(D_hdr)) deallocate(D_hdr, D_runner, L_runner, E_fp, q_1abs_tot, q_1abs)    
    

    return 1    ! Exit 
    
endif



! --- Post-convergence call --------------------------------------------------------------------------------------------
if (info(13) == 1) then

    !******************************************************************
    ! Set the system state values for the next timestep 
    !******************************************************************
    stored(1) = T_sys_c       !Set T_sys_c_last
    stored(2) = T_sys_h       !Set T_sys_h_last
    stored(3) = 0.            !Reset iteration every timestep
    j=3
    do i=1,nSCA
        stored(i + j) = T_htf_in(i)  !Set T_htf_in0
    enddo    
    do i=1,nSCA
        stored(i + j + nSCA) = T_htf_out(i)  !Set T_htf_out0
    enddo

    call setStorageVars(stored,nS,INFO)
    
    !MJW 9.8.2010 :: Call the property range check subroutine with the inlet and outlet HTF temps to make sure they're in the valid range
    call check_htf(Fluid,maxval(T_htf_out(:)))
    call check_htf(Fluid,T_sys_h)
    call check_htf(Fluid,T_sys_c)

    return 1    ! Exit - End of the routine for post-convergence calls
    
endif


! --- Second call in simulation: initialization call (not a simulation call) -------------------------------------------
!---- read static parameters---
if (info(7) .eq. -1) then
    !******************************************************************************************************************************
    !               Plant configuration inputs
    !******************************************************************************************************************************
    nSCA = par(1)               !The number of SCA's in a loop (also equal to the number of simulation nodes
    nHCEt = par(2)              !The number of HCE types in the field (excluding variations of the same HCE)
    nColt = par(3)              !The number of Collector types in teh field
    nHCEVar = par(4)            !The number of variations of a single HCE (due to H2, lost vaccum, broken glass)
    nLoops = par(5)             !The number of SCA loops in the field
    dt = par(6)                 ![hr] Time step
    

    !Allocate space for all of the HCE arrays
    allocate(D_2(nHCEt,nHCEVar), D_3(nHCEt,nHCEVar), D_4(nHCEt,nHCEVar), D_5(nHCEt,nHCEVar), D_p(nHCEt,nHCEVar), D_h(nHCEt,nHCEVar), &
             A_cs(nHCEt,nHCEVar), alpha_env(nHCEt,nHCEVar), epsilon_3t(nHCEt,nHCEVar,nea), epsilon_3l(nHCEt,nHCEVar),epsilon_3(nHCEt,nHCEVar,nea), &
             alpha_abs(nHCEt,nHCEVar), Tau_envelope(nHCEt,nHCEVar), epsilon_4(nHCEt,nHCEVar), epsilon_5(nHCEt,nHCEVar), GlazingIntactIn(nHCEt,nHCEVar), &
             Flow_type(nHCEt,nHCEVar), GlazingIntact(nHCEt,nHCEVar),HCE_FieldFrac(nHCEt,nHCEVar),AbsorberMaterial(nHCEt,nHCEVar),&
             P_a(nHCEt,nHCEVar),annulusGas(nHCEt,nHCEVar),&
             Rough(nHCEt,nHCEVar),Shadowing(nHCEt,nHCEVar),Dirt_HCE(nHCEt,nHCEVar), design_loss(nHCEt,nHCEVar))
    !Allocate space for all the collector arrays
    allocate(A_aperture(nColt), IamF0(nColt), IamF1(nColt),&
             IamF2(nColt), IAM(nColt), reflectivity(nColt), TrackingError(nColt), &
             GeomEffects(nColt), Rho_mirror_clean(nColt), Dirt_mirror(nColt), Error(nColt),&
             ColOptEff(nColt,nSCA), OptCharType(nColt), CollectorType(nColt), q_i(nColt), Ave_focal_length(nColt),&
             W_aperture(nColt), EndGain(nColt,nSCA), EndLoss(nColt,nSCA), RowShadow(nColt),L_SCA(nColt),&
             L_actSCA(nColt),L_aperture(nColt),ColperSCA(nColt),Distance_SCA(nColt))
    !Allocate space for the SCA info array. For each SCA, contains (HCE type, Collector type)
    allocate(SCAInfoArray(nSCA,2),T_htf_in(nSCA), T_htf_out(nSCA), T_htf_ave(nSCA), q_loss(nHCEVar), q_abs(nHCEVar), c_htf(nSCA),&
             rho_htf(nSCA),DP_tube(nSCA),SCADefocusArray(nSCA), E_abs_field(nSCA), E_int_loop(nSCA), E_accum(nSCA), E_avail(nSCA), E_abs_max(nSCA),v_1(nSCA),&
             q_loss_SCAtot(nSCA), q_abs_SCAtot(nSCA), q_SCA(nSCA), T_htf_in0(nSCA), T_htf_out0(nSCA), T_htf_ave0(nSCA), &
             E_fp(nSCA), q_1abs_tot(nSCA), q_1abs(nHCEVar))
    
    
    !initialize important variables
    E_fp = 0.d0
    E_fp_tot = 0.d0
    epsilon_3 = 0.d0
    epsilon_3t = 0.d0
    trkflag=1

    !---- Initialize outputs
    out(1:nout) = 0.d0
    info(6)=nout
    call typeck(1,info,nI,nP,nD)


    !---- Set storage array size 
    CALL setStorageSize(nS,INFO)


    !******************************************************************************************************************************
    !                   Collector inputs
    !******************************************************************************************************************************
    !Read in other non-time-dependent parameters
    eta_pump = par(7)                       ![-] HTF pump efficiency
    HDR_rough = par(8)                      ![m] Header pipe roughness
    lattitude = par(9) * 0.0174532925       ![rad] Site lattitude, read from weather file
    theta_stow = par(10) * 0.0174532925     ![rad] stow angle
    theta_dep = par(11)  * 0.0174532925     ![rad] deploy angle
    Row_Distance = par(12)                  ![m] Spacing between rows (centerline to centerline)
    FieldConfig = par(13)                   ![-] Number of subfields. an "I" configuration has 2, "H" has 4, double-H has 8. Must be an even number
    T_startup = par(14)+273.15              ![K] The required temperature of the system before the power block can be switched on
    pb_rated_cap = par(15)                  ![MWe]  Rated plant capacity
    m_dot_htfmin = par(16)                  ![kg/s] Minimum HTF flow rate
    m_dot_htfmax = par(17)                  ![kg/s] Maximum HTF flow rate
    T_loop_in_des = par(18)+273.15          ![K] Design loop inlet temperature
    T_loop_out = par(19)+273.15             ![K] Target loop outlet temperature
    Fluid = par(20)                         !Field HTF
    T_field_ini = par(21)+273.15            ![K] initial field temperature
    LU_dat = par(22)
    LU_FL = par(23)                         !Fluid property file logical unit
    T_fp = par(24)+273.15                   ![K] Freeze protection temperature (heat trace activation temperature)
    I_bn_des = par(25)                      ![W/m2] DNI at design
    V_hdr_max = par(26)                     ![m/s] maximum header velocity at design
    V_hdr_min = par(27)                     ![m/s] minimum header velocity at design
    Pipe_hl_coef = par(28)                  ![W/m2-K] Piping heat loss coefficient
    SCA_drives_elec = par(29)               ![W/SCA] parasitic drive power, per SCA
    LU_diam = par(30)                       !logical unit to write the header diameter file to
    !Allow "feathering" or partial defocusing of an SCA as part of the control scheme
    if(par(31)==1.) then; fthrok = .true.; else; fthrok = .false.; endif    
    !Feathering defocus strategy {0=fthrok->false, 1=SCA sequenced defocus, 2=simultaneous defocusing}
    if(.not.fthrok) then; fthrctrl = 0.; else; fthrctrl = par(32); endif    
    ColTilt = par(33) * 0.0174532925        ![rad] Collector tilt, 0 is horizontal, pi is vertical
    ColAz = par(34) * 0.0174532925          ![rad] Collector azimuth angle
    if(par(35)==1.) then; accept_mode = .true.; else; accept_mode=.false.; endif  !MJW 1.3.2011 Acceptance test mode flag
    if(par(36)==1.) then; accept_init = .true.; else; accept_init=.false.; endif  !MJW 1.5.2011 Acceptance test initialization required
    accept_loc = par(37)                    !MJW 1.17.2011 In acceptance testing mode, where is the temperature sensor (1=heat exchanger, 2=loop inlet)
    solar_mult = par(38)                    ![-] solar multiple
    !The balance of plant heat capacities capture the thermal inertia of the solar field not associated with HTF volume. These terms
    !can include piping thermal inertia, insulation, supports, or other equipment that cycles thermally with the field. The units
    !are the total energy in kWht required to raise the temperature of the term 1 deg K, normalized by the total solar field thermal capacity.
    mc_bal_hot = par(39)                    ![kWht/K-MWe(cap)] -> [J/K] the heat capacity of the balance of plant on the hot side. 
    mc_bal_cold = par(40)                   ![kWht/K-MWe(cap)] -> [J/K] the heat capacity of the balance of plant - cold side. 
    mc_bal_SCA = par(41) * 3.6e3            ![Wht/K-m] -> [J/K-m] the heat capacity associated with each SCA (per-meter basis) 
    
    !On the initial timestep call, read in the fluid property file
    call readFluidPropFile(LU_FL)    

    !The other system data needs to be read in from the data file
    rewind(LU_dat)
    filelen = 0
    do 
        filelen=filelen+1
        read(unit=LU_dat,fmt="(A)",err=400,eor=400,end=400, advance="YES") test
        if(test(1:1)=="&") then
            read(test(2:4),"(I2)") li
            do i=1,li
                read(unit=LU_dat,fmt="(A)",err=400,eor=400,end=400, advance="YES") test
            enddo
        endif
    enddo

    400   filelen=filelen-1
    
    if(filelen /= (nColt*18 + 21*nHCEt*nHCEvar + 3*nSCA )) then  !+ nLoops
        call messages(-1,"The number of inputs in the TYPE250 system geometry data file did not match requirements",'FATAL',INFO(1),INFO(2))
        return  !Quit the program here
    endif
    rewind(LU_dat)

    !read in all the information from the file to a temporary arrays
    allocate(xpar(filelen),xarr(nHCEt*nHCEvar*2,nea))  
    !Need room for emittance tables for all HCE combos plus corresponding temperature specs.
    !First array will be temp spec, second will be emittance value
    xpar(:)=0.d0; xarr(:,:)=0.d0; j=0; n=1
    do i=1,filelen
        read(LU_dat,"(A)") test
        if(test(1:1)=="&") then
            xpar(i)=flag
            j=j+1
            read(test(2:4),"(I2)") li
            lxarr(j)=li  !track record length
            do k=1,li
                read(LU_dat,*) xarr(2*j-1,k),xarr(2*j,k)
            enddo
        else
            k=len(trim(test))
            n=1; m=1
            call STRNUM(test(1:k),xpar(i),n,m)
        endif
    enddo
    rewind(LU_dat)
    close(LU_dat)    
    
    
    i=up1(0) !incremental counter that increases with each parameter that is read in
    
    do i=1,nColt
        OptCharType(i) = xpar(up1(1))              !The optical characterization method  (1 for traditional SAM method)
        CollectorType(i) = xpar(up1(1))            !{1=user defined, 2=LS-2, 3=LS-3, 4=IST}
        W_aperture(i) = xpar(up1(1))               ![m] The collector aperture width (total structure area.. used for shadowing)
        A_aperture(i) = xpar(up1(1))               ![m2] Reflective aperture area of the collector
        
        !Calculation of incidence angle modifier
        IamF0(i) = xpar(up1(1)) 
        IamF1(i) = xpar(up1(1)) 
        IamF2(i) = xpar(up1(1)) 

        !The following optical inputs are optional, and could be determined from a library
        reflectivity(i) = xpar(up1(1))             !Base solar-weighted mirror reflectivity value
        TrackingError(i) = xpar(up1(1))            !User-defined tracking error derate
        GeomEffects(i) = xpar(up1(1))              !User-defined geometry effects derate
        Rho_mirror_clean(i) = xpar(up1(1))         !User-defined clean mirror reflectivity
        Dirt_mirror(i) = xpar(up1(1))              !User-defined dirt on mirror derate
        Error(i) = xpar(up1(1))                    !User-defined general optical error derate

        Ave_Focal_Length(i) = xpar(up1(1))         ![m] The average focal length of the collector
        L_SCA(i) = xpar(up1(1))                    ![m] The length of the SCA
        L_aperture(i) = xpar(up1(1))               ![m] The length of a single mirror/HCE unit
        ColperSCA(i) = xpar(up1(1))                !The number of individual collector sections in an SCA
        L_actSCA(i) = L_aperture(i)*ColperSCA(i)        ![m] The active length of the SCA
        Distance_SCA(i) = xpar(up1(1))             ![m] piping distance between SCA's in the field
    enddo


    !******************************************************************************************************************************
    !               HCE Inputs
    !******************************************************************************************************************************
    do i=1,nHCEt
        do j=1,nHCEVar
            HCE_FieldFrac(i,j) = xpar(up1(1))      !The fraction of the field occupied by this HCE type
            D_2(i,j) = xpar(up1(1))                ![m] The inner absorber tube diameter
            D_3(i,j) = xpar(up1(1))                ![m] The outer absorber tube diameter
            D_4(i,j) = xpar(up1(1))                ![m] The inner glass envelope diameter
            D_5(i,j) = xpar(up1(1))                ![m] The outer glass envelope diameter
            D_p(i,j) = xpar(up1(1))                ![m] The diameter of the absorber flow plug (optional)
            Flow_type(i,j) = xpar(up1(1))          ! The flow type through the absorber {1="Tube flow", 2="Annular Flow"}
            if(Flow_type(i,j) == 2.d0) then
                D_h(i,j) = D_2(i,j) - D_p(i,j)          ![m] The hydraulic diameter for plug flow
            else
                D_h(i,j) = D_2(i,j)                     ![m] The hydraulic diameter for tube flow
                D_p(i,j) = 0.d0
            endif
            A_cs(i,j) = pi * (D_2(i,j)*D_2(i,j) - D_p(i,j)*D_p(i,j)) / 4.  ![m2] The cross-sectional flow area
            Rough(i,j) = xpar(up1(1))              ![m] Roughness of the internal surface

            alpha_env(i,j) = xpar(up1(1))          ![-] Envelope absorptivity
            epsilon_3(i,j,1) = xpar(up1(1))          ![-] Absorber emissivity
            if(epsilon_3(i,j,1)==flag) then
                epsilon_3l(i,j) = lxarr(trkflag)
                epsilon_3t(i,j,1:size(xarr(trkflag,:)))=xarr(2*trkflag-1,:)
                epsilon_3(i,j,1:size(xarr(trkflag+1,:)))=xarr(2*trkflag,:)
                trkflag=trkflag+1
            else
                epsilon_3l(i,j)=1
            endif
            
            alpha_abs(i,j) = xpar(up1(1))          ![-] Absorber absorptivity
            Tau_envelope(i,j) = xpar(up1(1))       ![-] Envelope transmittance 
            ! Inner and outer glass envelope emissivities (Pyrex)
            EPSILON_4(i,j) = xpar(up1(1)) 
            EPSILON_5(i,j) = xpar(up1(1)) 

            GlazingIntactIn(i,j) = xpar(up1(1))    !The glazing intact flag {1=true, else=false}
            if(GlazingIntactIn(i,j) == 1.) then
                GlazingIntact(i,j) = .true.             !Is the glass envelope intact?
            else
                GlazingIntact(i,j) = .false.
            endif
            P_a(i,j) = xpar(up1(1))                    ![torr]
            AnnulusGas(i,j) = dmax1(xpar(up1(1)),1.d0) !Annulus gas type (1=air, 26=Ar, 27=H2)
            
            !Absorber materials:
            ! (1)   304L
            ! (2)   216L
            ! (3)   321H
            ! (4)   B42 Copper Pipe
            AbsorberMaterial(i,j) = xpar(up1(1)) 
            Shadowing(i,j) = xpar(up1(1))               !User-defined shadowing value derate 
            Dirt_HCE(i,j) = xpar(up1(1))                !User-defined dirt on HCE derate
            design_loss(i,j) = xpar(up1(1))             ![W/m] User-defined thermal loss from absorber tube 

        enddo
    enddo

    !read in the array specifying the arrangement of the HCE types and Collector types
    do i=1,nSCA
        SCAInfoArray(i,1) = xpar(up1(1))           !HCE type
        SCAInfoArray(i,2) = xpar(up1(1))           !Collector type
        !read in the SCA defocusing process array.. this array specifies the order in which
        !the SCA's will be defocused in the case of over-temping
        SCADefocusArray(i) = xpar(up1(1))
    enddo
    
    !Calculate the total field aperture area
    Ap_tot = 0.d0
    do i=1,nSCA
        CT = SCAInfoArray(i,2)
        Ap_tot = Ap_tot + A_aperture(CT)*dble(nLoops)
    enddo
    
    !Calculate header diameters here based on min/max velocities
    !output file with calculated header diameter "header_diam.out"
    nfsec = FieldConfig  !MJW 1.12.11 allow the user to specify the number of field sections
    !Check to make sure the number of field sections is an even number
    if(mod(nfsec,2)/=0) call messages(-1,"Number of field subsections must equal an even number","FATAL",INFO(1),INFO(2))
    
    !The number of header sections per field section is equal to the total number of loops divided
    !by the number of distinct headers. Since two loops are connected to the same header section,
    !the total number of header sections is then divided by 2.
    nhdrsec = ceiling(dble(nLoops)/dble(nfsec*2))

    !Allocate space for the D_hdr array
    allocate(D_hdr(nhdrsec))
    D_hdr(:)=0.d0
    !We need to determine design information about the field for purposes of header sizing ONLY
    c_htf_ave = specheat(fluid,((T_loop_out+T_loop_in_des)/2.d0),1.d0)*1000.    !Specific heat
    
    !Need to loop through to calculate the weighted average optical efficiency at design
    !Start by initializing sensitive variables
    x1=0.d0; x2=0.d0; el_des=0.d0; opteff_des=0.d0; m_dot_design=0.d0; L_tot=0.d0; loss_tot=0.d0
    do i=1,nSCA
        CT = SCAInfoArray(i,2)
        L_tot = L_tot + L_actSCA(CT)
    enddo
    
    do i=1,nSCA
        CT = SCAInfoArray(i,2)    !Collector type    
        !Calculate the CosTheta value at summer solstice noon
        x1 = SQRT(1. - (COS((lattitude - 23.5/180.*pi)-ColTilt) - COS(ColTilt) * COS((lattitude - 23.5/180.*pi)) * (1. - COS(0. -ColAz))) ** 2) !costheta
        !Calculate end gain factor
        x2 = dmax1((Ave_focal_length(CT)*tan(acos(x1))-Distance_SCA(CT)),0.d0)  !end gain
        !calculate end loss
        el_des =  1. - (Ave_focal_length(CT) * TAN(acos(x1)) - (dble(nSCA) - 1.) /dble(nSCA)* x2) / L_actSCA(CT)
        
        do j=1,nHCEvar
            HT = SCAInfoArray(i,1)    !HCE type
            !Calculate optical efficiency approximating use of the first collector only
            opteff_des = opteff_des+ Shadowing(HT,j)*TrackingError(CT)*GeomEffects(CT)*Rho_mirror_clean(CT)*Dirt_mirror(CT)*&
                                     Dirt_HCE(HT,j)*Error(CT)*el_des*(L_actSCA(CT)/L_tot)*HCE_FieldFrac(HT,j)
            Loss_tot = Loss_tot + design_loss(HT,j)*L_actSCA(CT)*HCE_FieldFrac(HT,j)
        enddo
    enddo
    !the estimated mass flow rate at design
    m_dot_design = (Ap_tot*I_bn_des*opteff_des - Loss_tot*dble(nLoops))/(c_htf_ave*(T_loop_out - T_loop_in_des))  !tn 4.25.11 using Ap_tot instead of A_loop. Change location of opteff_des
    !mjw 1.16.2011 Design field thermal power 
    q_design = m_dot_design * c_htf_ave * (T_loop_out - T_loop_in_des) ![Wt]
    !mjw 1.16.2011 Convert the thermal inertia terms here
    mc_bal_hot = mc_bal_hot * 3.6 * q_design    ![J/K]
    mc_bal_cold = mc_bal_cold * 3.6 * q_design  ![J/K]
    
    !need to provide fluid density
    rho_ave = density(fluid,((T_loop_out+T_loop_in_des)/2.d0),0.d0) !kg/m3
    !Calculate the header design
    nrunsec = floor(dble(nfsec)/4.d0)+1  !The number of unique runner diameters
    if(.not.allocated(D_runner)) allocate(D_runner(nrunsec), L_runner(nrunsec))
    call header_design(LU_diam,nhdrsec,nfsec,nrunsec,rho_ave,V_hdr_max,V_hdr_min,m_dot_design,D_hdr,D_runner)
    if(ErrorFound()) return
    
    deallocate(xpar,xarr)  !tn 4.25.11 also deallocate xarr
    
    !Set initial storage values
    stored(1:ns) = T_field_ini
    stored(3) = 0.  !Iter
    call setStorageVars(stored,nS,info)

    !Do one-time calculations for system geometry. Calculate all HTF volume, set runner piping length
    !Assume there are two field subsections per span, then if there's an even number of spans in the field, 
    !we count the first header section as half-length. I.e., if a field looks like this:
    !   (1)        (2)
    ! |||||||   |||||||
    ! -----------------
    ! ||||||| : |||||||
    !         :
    !        [P]
    !         :
    ! ||||||| : |||||||
    ! -----------------
    ! |||||||   |||||||
    !   (3)        (4)
    !Then the field has 4 subfields and two spans. The runner pipe (:) is half the distance between the two spans. 
    !If the number of subfields were 6 (3 spans), the two runner pipe segments would both be equal to the full
    !distance between spans.
    if(mod(nfsec/2,2)==1) then
        x1 = 2.     !the first runners are normal
    else
        x1 = 1.     !the first runners are short
    endif
    L_runner(1) = 50.  !Assume 50 [m] of runner piping in and around the power block before it heads out to the field in the main runners
    if(nrunsec > 1) then
        do i=2,nrunsec
            j = SCAInfoArray(1,2)
            L_runner(i) = x1 * (2*Row_distance + (L_SCA(j) + Distance_SCA(j))*dble(nSCA)/2.)
            x1 = 2.   !tn 4.25.11 Default to 2 for subsequent runners
        enddo
    endif
    v_tofrom_sgs = 0.d0
    do i=1,nrunsec
        v_tofrom_sgs = v_tofrom_sgs + 2.*L_runner(i)*pi*D_runner(i)**2/4.  !This is the volume of the runner in 1 direction.
    enddo
    
    !6/14/12, TN: Multiplier for runner heat loss. In main section of code, are only calculating loss for one path.
    !Since there will be two symmetric paths (when nrunsec > 1), need to calculate multiplier for heat loss, considering
    !that the first 50 meters of runner is assumed shared.
    N_run_mult = 1.d0 + (1.d0 - 50.d0/sum(L_runner(:)))
    
    !-------piping from header into and out of the HCE's
    v_loop_tot = 0.d0
    do j=1,nHCEVar
        do i=1,nSCA
            CT = SCAInfoArray(i,2)    !Collector type    
            HT = SCAInfoArray(i,1)    !HCE type
            !v_loop_bal = v_loop_bal + Distance_SCA(CT)*A_cs(HT,j)*HCE_FieldFrac(HT,j)*dble(nLoops)
            v_loop_tot = v_loop_tot + (L_SCA(CT) + Distance_SCA(CT))*A_cs(HT,j)*HCE_FieldFrac(HT,j)*dble(nloops)
        enddo
    enddo
    !mjw 1.13.2011 Add on volume for the crossover piping 
    !v_loop_tot = v_loop_tot + Row_distance*A_cs(SCAInfoArray(nSCA/2,1),1)*dble(nloops)
    v_loop_tot = v_loop_tot + Row_distance*A_cs(SCAInfoArray(max(2,nSCA)/2,1),1)*dble(nloops)      !TN 6/20: need to solve for nSCA = 1
    
    
    !-------field header loop
    v_header = 0.d0
    do i=1,nhdrsec
        !Also calculate the hot and cold header volume for later use. 4.25 is for header expansion bends
        v_header = v_header + D_hdr(i)*D_hdr(i)/4.*pi*(Row_distance+4.275)*dble(nfsec)*2.d0  !tn 4.25.11 The header distance should be multiplied by 2 row spacings
    enddo
    !Add on inlet/outlet from the header to the loop. Assume header to loop inlet ~= 10 [m] (Kelley/Kearney)
    v_header = v_header + 20.*A_cs(1,1)*dble(nloops)
    
    !Calculate the HTF volume associated with pumps and the SGS
    v_sgs = Pump_SGS(rho_ave,m_dot_design,solar_mult)
    
    !Calculate the hot and cold balance-of-plant volumes
    v_hot = v_header + v_tofrom_sgs 
    v_cold = v_hot
    
    !Write the volume totals to the piping diameter file
    write(LU_diam,fmt=201) v_cold, v_hot, v_loop_tot/dble(nloops), v_loop_tot, (v_hot*2. + v_loop_tot), v_sgs, (v_hot*2. + v_loop_tot + v_sgs)
    201 format(//&
               "----------------------------------------------"/&
               "Plant HTF volume information:"/&
               "----------------------------------------------"/&
               "Cold header pipe volume:   ",E10.4," m3"/&
               "Hot header pipe volume:    ",E10.4," m3"/&
               "Volume per loop:           ",E10.4," m3"/&
               "Total volume in all loops: ",E10.4," m3"/&
               "Total solar field volume:  ",E10.4," m3"/&
               "Pump / SGS system volume:  ",E10.4," m3"/&
               "---------------------------"/&
               "Total plant HTF volume:    ",E10.4," m3")
    !Include the pump/SGS volume with the header
    v_hot = v_hot + v_sgs/2.
    v_cold = v_cold + v_sgs/2.
    
    return 1
endif

!******************************************************************************************************************************
!               Time-dependent conditions
!******************************************************************************************************************************
I_b = xin(1)/3.6                    ![W/m2] Direct normal incident solar irradiation
T_db = xin(2)+273.15                ![K] dry bulb air temperature 
V_wind = xin(3)                     ![m/s]  Ambient windspeed
P_amb = xin(4) * 101325.d0          ![Pa] Ambient pressure, convert from ATM
T_dp = xin(5)+273.15                ![K] The dewpoint temperature
shift = xin(6) * 0.0174532925       ![rad] input from weather file.. shift in longitude from local standard meridian
T_cold_in = xin(7)+273.15           ![K] HTF return temperature.. used to calculate field inlet temperature
m_dot_in = 0.d0 !xin(8)/3600.             ![kg/s]
defocus_new = xin(9)                    ![-] Defocus control
SolarAz = xin(10) * 0.0174532925    ![rad] solar azimuth
!*********** End of input section *********************************************************************************************

!If no change in inputs between calls, then no need to go through calculations.  Sometimes an extra call was run.....
IF (INFO(7)>0) THEN
    
    IF( (.not.(no_fp)) .and. (T_cold_in_1 > T_cold_in) )THEN
        E_fp_tot = m_dot_htf_tot * c_hdr_cold * (T_cold_in_1 - T_cold_in)       ![kg/s]*[J/kg-K]*[K] = [W]  Freeze protection energy required
        GOTO 12
    ENDIF
    
ENDIF
!**********************************************************************************************************************

!******* Read in stored variables every timestep*******
if(accept_init) then
    if((time == getSimulationStartTime()).and.(info(7)==0)) then
        print *,"Initializing..."
    endif
endif
5 continue !mjw 1.5.2011 Acceptance test initialization entry point
call getStorageVars(stored,nS,info)

T_sys_c_last = stored(1)  !Get T_sys from the last timestep
T_sys_h_last = stored(2)
j=3
do i=1,nSCA
    T_htf_in0(i) = stored(i + j)
    T_htf_out0(i) = stored(i + j + nSCA)
    T_htf_ave0(i) = (T_htf_in0(i) + T_htf_out0(i))/2.d0
enddo

!9-27-12, TWN: This model uses relative defocus. Changed controller to provide absolute defocus, so now convert to relative here
defocus = defocus_new / defocus_old       
defocus_old = defocus_new

if(info(7) == 0)then        !Always reset the defocus control at the begining of the timestep
    defocus_new = 1.
    defocus_old = 1.
    defocus = 1.0
endif

T_sky = skytemp(T_db,T_dp,hour)     ![K] Effective sky temperature 
T_air = T_db                        ![K] Set T_air equal to air temperature

!*******Recalculate values that are time-dependent***********

!First calculate the cold header temperature, which will serve as the loop inlet temperature
rho_hdr_cold = density(Fluid,T_sys_c_last,1.d0)
rho_hdr_hot = density(Fluid,T_sys_h_last,1.d0)
c_hdr_cold_last = specheat(Fluid,T_sys_c_last,1.d0)*1000.d0 !mjw 1.6.2011 Adding mc_bal to the cold header inertia

!MJW 1.17.2011 In acceptance test mode, select whether to enforce the cold inlet temperature at the heat exchanger
!outlet (==1) or at the loop inlet (else).
!if((accept_loc/=2.d0).or.(time==getSimulationStartTime())) then
!    T_sys_c = (T_sys_c_last - T_cold_in)*exp(-m_dot_in/(v_cold*rho_hdr_cold+mc_bal_cold/c_hdr_cold)*dt*3600.) + T_cold_in
!else
!    T_sys_c = T_cold_in
!endif
!T_loop_in = T_sys_c

if (info(7)==0) then !mjw 3.5.11 We only need to calculate these values once per timestep..
    !Time calculations
    hour = modulo(time,24.)       !hour of the day (1..24)  !tn 4.25.11 mod returns a natural number. This messes with the ftrack HrA/HrB calculations
    day_of_year = ceiling(time/24.)  !Day of the year
    ! Duffie & Beckman 1.5.3b
    B = (day_of_year-1)*360.0/365.0*pi/180.0
    ! Eqn of time in minutes
    EOT = 229.2 * (0.000075 + 0.001868 * COS(B) - 0.032077 * SIN(B)	- 0.014615 * COS(B*2.0) - 0.04089 * SIN(B*2.0))
    ! Declination in radians (Duffie & Beckman 1.6.1)
    Dec = 23.45 * SIN(360.0*(284.0+day_of_year)/365.0*pi/180.0) * pi/180.0
    ! Solar Noon and time in hours
    SolarNoon = 12. - ((shift)*180.0/pi) / 15.0 - EOT / 60.0

    ! Deploy & stow times in hours
    ! Calculations modified by MJW 11/13/2009 to correct bug
    theta_dep = dmax1(theta_dep,1.e-6)
    DepHr1 = COS(lattitude) / TAN(theta_dep)
    DepHr2 = -TAN(Dec) * SIN(lattitude) / TAN(theta_dep)
    DepHr3 = sign(1.d0,tan(pi-theta_dep))*ACOS((DepHr1*DepHr2 + Sqrt(DepHr1*DepHr1-DepHr2*DepHr2+1.0)) / (DepHr1 * DepHr1 + 1.0)) * 180.0 / pi / 15.0
    DepTime = SolarNoon + DepHr3

    theta_stow = dmax1(theta_stow,1.e-6)
    StwHr1 = COS(lattitude) / TAN(theta_stow)
    StwHr2 = -TAN(Dec) * SIN(lattitude) / TAN(theta_stow)
    StwHr3 = sign(1.d0,tan(pi-theta_stow))*ACOS((StwHr1*StwHr2 + Sqrt(StwHr1*StwHr1-StwHr2*StwHr2+1.0)) / (StwHr1 * StwHr1 + 1.0)) * 180.0 / pi / 15.0
    StwTime = SolarNoon + StwHr3

    ! Ftrack is the fraction of the time period that the field is tracking. MidTrack is time at midpoint of operation
    HrA = hour-dt
    HrB = hour

    ! Solar field operates
    if ((HrB > DepTime) .AND. (HrA < StwTime)) then
        ! solar field deploys during time period
        if (HrA < DepTime) then
            Ftrack = (HrB - DepTime) / dt
            MidTrack = HrB - Ftrack * 0.5 *dt
        ! Solar field stows during time period
        elseif (HrB > StwTime) then
            Ftrack = (StwTime - HrA) / dt
            MidTrack = HrA + Ftrack * 0.5 *dt
        ! solar field operates during entire period
        else
            Ftrack = 1.0
            MidTrack = HrA + 0.5 *dt
        endif
    ! solar field doesn't operate
    else
        Ftrack = 0.0
        MidTrack = HrA + 0.5 *dt
    endif

    StdTime = MidTrack
    SolarTime = StdTime+((Shift)*180.0/pi)/15.0+ EOT/60.0
    ! hour angle (arc of sun) in radians
    omega = (SolarTime - 12.0)*15.0*pi/180.0
    ! B. Stine equation for Solar Altitude angle in radians
    SolarAlt = asin(sin(dec)*sin(lattitude)+cos(lattitude)*cos(dec)*cos(omega))
    if(accept_init .and. time==getSimulationStartTime()) then  !MJW 1.14.2011 
        SolarAz = sign(1.d0, omega)*abs(acos(dmin1(1.d0,(cos(pi/2.-SolarAlt)*sin(lattitude)-sin(dec))/(sin(pi/2.-SolarAlt)*cos(lattitude)))))
    endif
    
    ! Calculation of Tracking Angle for Trough. Stine Reference
    TrackAngle = Atan ( Cos(SolarAlt) * Sin(SolarAz-ColAz) / &
                 (Sin(SolarAlt-ColTilt)+Sin(ColTilt)*Cos(SolarAlt)*(1-Cos(SolarAz-ColAz))) )
    ! Calculation of solar incidence angle for trough.. Stine reference
    if(ftrack==0.d0) then
        CosTh=1.d0
    else
        CosTh = SQRT(1.d0 - (COS(SolarAlt-ColTilt) - COS(ColTilt) * COS(SolarAlt) * (1.d0 - COS(SolarAz -ColAz))) ** 2)
    endif
    ! Theta in radians
    Theta = ACOS(CosTh)
        
    do i=1,nColt
        q_i(i) = I_b*A_aperture(i)/L_actSCA(i) ![W/m] The incoming solar irradiation per aperture length
        !incidence angle modifier (radians)
        IAM(i) = IamF0(i) + IamF1(i) * Theta / CosTh + IamF2(i) * Theta*Theta/ CosTh
        !Calculate the Optical efficiency of the collector
        ColOptEff(i,:) = TrackingError(i) * GeomEffects(i) * Rho_mirror_clean(i) * Dirt_mirror(i) * Error(i) * IAM(i)

        !Account for light reflecting off the collector and missing the receiver, also light from other 
        !collectors hitting a different receiver
        !mjw 4.21.11 - rescope this to be for each specific collector j=1,nSCA
        do j=1,nSCA
            if(abs(SolarAz) <= 90.d0) then  !mjw 5.1.11 The sun is in the southern sky (towards equator)
                if((j==1).or.(j==nSCA)) then
                    EndGain(i,j) =  0.d0 !No gain for the first or last collector
                else
                    EndGain(i,j) = dmax1(Ave_focal_length(i) * tan(theta) - Distance_SCA(i), 0.d0)/ L_actSCA(i)
                endif
            else  !mjw 5.1.11 The sun is in the northern sky (away from equator)
                if((j==floor(dble(nSCA)/2.)).or.(j==floor(dble(nSCA)/2.)+1)) then
                    EndGain(i,j) = 0.d0 !No gain for the loops at the ends of the rows
                else
                    EndGain(i,j) = dmax1(Ave_focal_length(i) * tan(theta) - Distance_SCA(i), 0.d0)/ L_actSCA(i)
                endif
            endif
            EndLoss(i,j) = 1.d0 - Ave_focal_length(i) * tan(theta) / L_actSCA(i) + EndGain(i,j)
        enddo
                
        ! Row to Row Shadowing Lossess
        !PH = pi / 2.0 - TrackAngle(i)
        RowShadow(i) = Abs(cos(TrackAngle)) * Row_Distance / W_aperture(i)
        If ((RowShadow(i) < 0.5) .OR. (SolarAlt < 0.)) Then
            RowShadow(i) = 0.
        Else If (RowShadow(i) > 1.) Then
            RowShadow(i) = 1.
        End If
        
        !Finally correct for these losses on the collector optical efficiency value
        ColOptEff(i,:) = ColOptEff(i,:)*RowShadow(i)*EndLoss(i,:)*Ftrack  !mjw 4.21.11 now a separate value for each SCA

    enddo
    !Calculate the flux level associated with each SCA
    !but only calculate for the first call of the timestep<----[NO! messes with defocusing control: mjw 11.4.2010]
    Theta_ave = 0.d0; CosTh_ave = 0.d0; IAM_ave = 0.d0; RowShadow_ave = 0.d0; EndLoss_ave = 0.d0
    do i=1,nSCA
        CT = SCAInfoArray(i,2)    !Collector type
        q_SCA(i) = q_i(CT)*CosTh        !The flux corresponding with the collector type
        !Also use this chance to calculate average optical values
        Theta_ave = Theta_ave + theta*L_actSCA(CT)/L_tot
        CosTh_ave = CosTh_ave + CosTh*L_actSCA(CT)/L_tot
        IAM_ave = IAM_ave + IAM(CT)*L_actSCA(CT)/L_tot
        RowShadow_ave = RowShadow_ave + RowShadow(CT)*L_actSCA(CT)/L_tot
        EndLoss_ave = EndLoss_ave + EndLoss(CT,i)*L_actSCA(CT)/L_tot
    enddo
else !mjw 3.5.11
    do i=1,nSCA
        CT = SCAInfoArray(i,2)
        q_SCA(i) = q_i(CT)*CosTh
    enddo
endif  !mjw 3.5.11 ---------- end of the items that only need to be calculated once per time step
q_SCA_tot = sum(q_SCA(1:nSCA)) !W/m

!mode for solving the field 
!1=constrain temperature
!2=defocusing array
!3=constrain mass flow above min
SolveMode = 1   
!MJW 12.14.2010 Only calculate the estimate on the first call of the timestep
if(info(7)==0) then
    SCAs_def = 1.
    if(I_b > 25.) then
            m_dot_HTFX = dmax1(dmin1(10./950.*I_b,m_dot_htfmax),m_dot_htfmin)   !*defocus[kg/s] guess Heat transfer fluid mass flow rate through one loop
    else
        m_dot_HTFX = m_dot_htfmin
    endif
else
    m_dot_htfx = dmax1(dmin1(m_dot_htfmax, m_dot_htfX),m_dot_htfmin)
endif

!TWN 6/14/11  If defous is < 1 then calculate defocus from previous call's q_abs and then come back to temperature loop.
if(defocus<1.) then
    dfcount = dfcount +1
    GOTO 11
endif

10 continue     !Return loop for over-temp conditions
dfcount = dfcount + 1   !The defocus iteration counter

!******* Initial values *******
!if(T_loop_in == T_loop_out) T_loop_in = T_loop_out - 1. !Don't allow equal temperatures
!T_htf_in(1) = T_loop_in

qq = 0                  !Set iteration counter

!Set ends of iteration bracket.  Make bracket greater than bracket defined on system limits so a value less than system limit can be tried earlier in iteration
m_dot_lower = 0.7*m_dot_htfmin  
m_dot_upper = 1.3*m_dot_htfmax

upflag = .false.        !Set logic to switch from bisection to false position mode
lowflag = .false.       !Set logic to switch from bisection to false position mode
fp_lowflag  = .false.   !Set logic to switch from bisection to false position mode
fp_upflag   = .false.   !Set logic to switch from bisection to false position mode
no_fp = .true.          !Set logic that specify that SCA loop is not solving for freeze protection temperature
T_cold_in_1 = T_cold_in !Inlet temperature may change due to new freeze protection model

t_tol = 1.5e-3          !Tolerance for T_loop_out

!Decreasing the tolerance helps get out of repeating defocus iterations
IF(info(7)>8)THEN
    t_tol = 1.5e-4
ENDIF

err = t_tol * 10.       !Set beginning error > tolerance
!******************************************************************************************************************************
!                   Iterative section
!******************************************************************************************************************************
DO WHILE((abs(err) > t_tol).and.(qq < 30))
!do      !Main convergence loop
    
    qq=qq+1 !Iteration counter
    q_loss_SCAtot(1:nSCA) = 0.d0
    q_abs_SCAtot(1:nSCA) = 0.d0
    q_1abs_tot(:) = 0.d0
    E_avail(1:nSCA) = 0.d0
    E_accum(1:nSCA) = 0.d0
    E_int_loop(1:nSCA) = 0.d0
    EqOpteff = 0.d0
    c_htf(:) = 0.d0
    rho_htf(:) = 0.d0
   
    if(accept_mode) then  !mjw 1.3.2011 accommodate acceptance testing
        m_dot_htf = m_dot_in/dble(nloops)
        m_dot_htfX = m_dot_htf
    else
        m_dot_htf = m_dot_htfX
    endif
    
    m_dot_htf_tot = m_dot_htf*dble(nLoops)
    
    if((accept_loc/=2.d0).or.(time==getSimulationStartTime())) then
        T_sys_c  = (T_sys_c_last - T_cold_in_1)*exp(-(m_dot_htf*dble(nloops))/(v_cold*rho_hdr_cold+mc_bal_cold/c_hdr_cold_last)*dt*3600.) + T_cold_in_1
        c_hdr_cold = specheat(Fluid,T_sys_c,1.d0)*1000.d0 !mjw 1.6.2011 Adding mc_bal to the cold header inertia
        !Consider heat loss from cold piping
        !Pipe_hl_cold = 0.d0
        Header_hl_cold = 0.d0
        Runner_hl_cold = 0.d0        
        !Header
        do i=1,size(D_hdr)
            !Pipe_hl_cold = Pipe_hl_cold + Row_distance*D_hdr(i)*pi*Pipe_hl_coef*(T_sys_c - T_db)  ![W]
            Header_hl_cold = Header_hl_cold + Row_distance*D_hdr(i)*pi*Pipe_hl_coef*(T_sys_c - T_db)  ![W]
        enddo
        !Runner
        do i=1,nrunsec
            !Pipe_hl_cold = Pipe_hl_cold + L_runner(i)*pi*D_runner(i)*Pipe_hl_coef*(T_sys_c - T_db)  ![W]
            Runner_hl_cold = Runner_hl_cold + L_runner(i)*pi*D_runner(i)*Pipe_hl_coef*(T_sys_c - T_db)  ![W]
        enddo
        Pipe_hl_cold = Header_hl_cold + Runner_hl_cold
        
        T_loop_in   = T_sys_c - Pipe_hl_cold/(m_dot_htf*dble(nloops)*c_hdr_cold)
        T_htf_in(1) = T_loop_in
    else
        T_htf_in(1) = T_loop_in
    endif    
    
    !---------------------
    do i=1,nSCA
        q_loss(:) = 0.d0
        q_abs(:) = 0.d0
        q_1abs(:) = 0.d0
        
        do j=1,nHCEVar

            HT = SCAInfoArray(i,1)    !HCE type
            CT = SCAInfoArray(i,2)    !Collector type
            
            !Check to see if the field fraction for this HCE is zero.  If so, don't bother calculating for this variation
            if(HCE_FieldFrac(HT,j)==0.d0) cycle
            
            if(epsilon_3l(HT,j)>1) then
                xx(:)=epsilon_3t(HT,j,:)
                yy(:)=epsilon_3(HT,j,:)
                !call interp((T_htf_ave(i)-273.15),epsilon_3l(HT,j),xx,yy,eps3sav,eps_3): Now calculate inside of receiver subroutine such that correct temp is used
            else
                eps_3 = epsilon_3(HT,j,1)
            endif
            
            call EvacReceiver(&  !Inputs
                      T_htf_in(i), m_dot_htf, T_db, T_sky, v_wind, P_amb, q_SCA(i),A_cs(HT,j), D_2(HT,j), D_3(HT,j),D_4(HT,j), &
                      D_5(HT,j), D_p(HT,j), D_h(HT,j),&
                      epsilon_3l(HT,j),xx,yy,nea,L_actSCA,.false.,& !Inputs added to calculate transient
                      eps_3, Epsilon_4(HT,j), Epsilon_5(HT,j),& 
                      Alpha_abs(HT,j), alpha_env(HT,j), (ColOptEff(CT,i)*Shadowing(HT,j)*Dirt_HCE(HT,j)), Tau_envelope(HT,j), P_a(HT,j), &
                      Flow_type(HT,j), AbsorberMaterial(HT,j), annulusGas(HT,j), glazingIntact(HT,j), Fluid, info,time,&
                        !Outputs
                      q_loss(j),q_abs(j),q_1abs(j),c_htf_j,rho_htf_j)
            
            if(isnan(q_abs(j))) then
                m_dot_htfX = m_dot_htfmax
                if(dfcount > 20) then
                    call messages(-1,"The solution encountered an unresolvable NaN error in the heat loss calculations.",'WARNING',INFO(1),INFO(2))
                    return 1
                endif
                goto 10
            endif

            !Keep a running sum of all of the absorbed/lost heat for each SCA in the loop
            q_abs_SCAtot(i) = q_abs_SCAtot(i) + q_abs(j)*L_actSCA(CT)*HCE_FieldFrac(HT,j)
            q_loss_SCAtot(i) = q_loss_SCAtot(i) + q_loss(j)*L_actSCA(CT)*HCE_FieldFrac(HT,j)
            q_1abs_tot(i) = q_1abs_tot(i) + q_1abs(j)*HCE_FieldFrac(HT,j)  !losses in W/m from the absorber surface
            c_htf(i) = c_htf(i) + c_htf_j*HCE_FieldFrac(HT,j)
            rho_htf(i) = rho_htf(i) + rho_htf_j*HCE_FieldFrac(HT,j)

            !keep track of the total equivalent optical efficiency
            EqOpteff = EqOptEff + ColOptEff(CT,i)*Shadowing(HT,j)*Dirt_HCE(HT,j)*Alpha_abs(HT,j)*Tau_envelope(HT,j)*(L_actSCA(CT)/L_tot)*HCE_FieldFrac(HT,j)
        enddo  !nHCEVar loop

        !Calculate the specific heat for the node
        c_htf(i) = c_htf(i)*1000.d0 
        !Calculate the average node outlet temperature, including transient effects
        m_node = rho_htf(i)*A_cs(HT,1)*L_actSCA(CT)
        
        !MJW 12.14.2010 The first term should represent the difference between the previous average temperature and the new 
        !average temperature. Thus, the heat addition in the first term should be divided by 2 rather than include the whole magnitude
        !of the heat addition.
        !mjw & tn 5.1.11: There was an error in the assumption about average and outlet temperature      
        T_htf_out(i) = q_abs_SCAtot(i)/(m_dot_htf*c_htf(i)) + T_htf_in(i) + &
                       2.d0 * (T_htf_ave0(i) - T_htf_in(i) - q_abs_SCAtot(i)/(2.d0 * m_dot_htf * c_htf(i))) * &
                       exp(-2. * m_dot_htf * c_htf(i) * dt*3600. / (m_node * c_htf(i) + mc_bal_sca * L_actSCA(CT)))
        !Recalculate the average temperature for the SCA
        T_htf_ave(i) = (T_htf_in(i) + T_htf_out(i))/2.d0
        
        
        !Calculate the actual amount of energy absorbed by the field that doesn't go into changing the SCA's average temperature
        !MJW 1.16.2011 Include the thermal inertia term
        if(q_abs_SCAtot(i) > 0.d0) then
            !E_avail(i) = dmax1(q_abs_SCAtot(i)*dt*3600. - A_cs(HT,1)*L_actSCA(CT)*rho_htf(i)*c_htf(i)*(T_htf_ave(i)- T_htf_ave0(i)),0.d0)
            x1 = (A_cs(HT,1)*L_actSCA(CT)*rho_htf(i)*c_htf(i) + L_actSCA(CT)*mc_bal_sca)  !mjw 4.29.11 removed c_htf(i) -> it doesn't make sense on the mc_bal_sca term
            E_accum(i) = x1*(T_htf_ave(i)- T_htf_ave0(i))
            E_int_loop(i) = x1*(T_htf_ave(i) - 298.15d0)  !mjw 1.18.2011 energy relative to ambient 
            E_avail(i) = dmax1(q_abs_SCAtot(i)*dt*3600. - E_accum(i),0.d0)      ![J/s]*[hr]*[s/hr]: [J]
            
            !Equation: m_dot_avail*c_htf(i)*(T_hft_out - T_htf_in) = E_avail/(dt*3600)
            !m_dot_avail = (E_avail(i)/(dt*3600.))/(c_htf(i)*(T_htf_out(i) - T_htf_in(i)))   ![J/s]*[kg-K/J]*[K]: 
        endif

        
        !Set the inlet temperature of the next SCA equal to the outlet temperature of the current SCA
        !minus the heat losses in intermediate piping
        if(i < nSCA) then
            !Determine the length between SCA's to use.  If halfway down the loop, use the row distance.
            if(i==nSCA/2) then 
                L_int = 2.+ Row_distance
            else
                L_int = Distance_SCA(CT)
            endif
            
            !Calculate inlet temperature of the next SCA
            T_htf_in(i+1) = T_htf_out(i) - pipe_hl_coef*D_3(HT,1)*pi*L_int*(T_htf_out(i) - T_db)/(m_dot_htf*c_htf(i))
            !mjw 1.18.2011 Add the internal energy of the crossover piping
            E_int_loop(i) = E_int_loop(i) + L_int*(D_3(HT,1)**2/4.*pi + mc_bal_sca/c_htf(i))*(T_htf_out(i) - 298.15d0)
        endif

        continue
    enddo

    !Set the loop outlet temperature
    T_loop_outX = T_htf_out(nSCA)

    !Calculation for heat losses from hot header and runner pipe
    !Pipe_hl_hot = 0.d0 !initialize
    Runner_hl_hot = 0.d0    !initialize
    Header_hl_hot = 0.d0   !initialize
    do i=1,size(D_hdr)
        !Pipe_hl_hot = Pipe_hl_hot + Row_distance*D_hdr(i)*pi*Pipe_hl_coef*(T_loop_outX - T_db)
        Header_hl_hot = Header_hl_hot + Row_distance*D_hdr(i)*pi*Pipe_hl_coef*(T_loop_outX - T_db)
    enddo
    !Add the runner length
    do i=1,nrunsec
        !Pipe_hl_hot = Pipe_hl_hot + L_runner(i)*pi*D_runner(i)*Pipe_hl_coef*(T_loop_outX - T_db)  !Wt
        Runner_hl_hot = Runner_hl_hot + L_runner(i)*pi*D_runner(i)*Pipe_hl_coef*(T_loop_outX - T_db)  !Wt
    enddo
    Pipe_hl_hot = Header_hl_hot + Runner_hl_hot

    c_hdr_hot = specheat(Fluid,T_loop_outX,1.d0)* 1000.    

    !Adjust the loop outlet temperature to account for thermal losses incurred in the hot header and the runner pipe
    T_sys_h = T_loop_outX - Pipe_hl_hot/(m_dot_htf_tot*c_hdr_hot)

    !Calculate the system temperature of the hot portion of the collector field. 
    !This will serve as the fluid outlet temperature
    T_sys_h = (T_sys_h_last - T_sys_h)*exp(-m_dot_htf_tot/(v_hot*rho_hdr_hot+mc_bal_hot/c_hdr_hot)*dt*3600.) + T_sys_h

    if(accept_mode) exit    !mjw 1.4.2011
    
7   continue    
    
    IF(solveMode==3) THEN  !Solve to find (increased) inlet temperature that keeps outlet temperature above freeze protection temp
    
        IF((no_fp).and.(T_sys_h > T_fp)) GOTO 9
                
        no_fp   = .false.
        
        err = (T_fp - T_sys_h)/T_fp
        
        IF(abs(err) <= t_tol) GOTO 9

        IF((fp_lowflag).and.(fp_upflag))THEN
            IF(err > 0.d0)THEN      !Outlet too low, set lower limit of inlet temperature
                T_in_lower  = T_cold_in_1
                y_fp_lower  = err
            ELSE                    !Outlet too high, set upper limit of inlet temperature
                T_in_upper  = T_cold_in_1
                y_fp_upper  = err
            ENDIF
                T_cold_in_1 = (y_fp_upper)/(y_fp_upper-y_fp_lower)*(T_in_lower - T_in_upper) + T_in_upper
        ELSE
        
            IF(err > 0.d0)THEN      !Outlet too low, set lower limit of inlet temperature
                T_in_lower  = T_cold_in_1
                y_fp_lower  = err
                fp_lowflag  = .true.
                upmult      = 0.5d0
            ELSE                    !Outlet too high, set upper limit of inlet temperature
                T_in_upper  = T_cold_in_1
                y_fp_upper  = err
                fp_upflag   = .true.
                upmult      = 0.5d0
            ENDIF
            
            IF((fp_lowflag).and.(fp_upflag))THEN    !If results of bracket are defined, use false position
                T_cold_in_1 = (y_fp_upper)/(y_fp_upper-y_fp_lower)*(T_in_lower - T_in_upper) + T_in_upper
                
            ELSE                            !If not, recalculate value based on approximate energy balance
                T_cold_in_1 = T_cold_in_1 + 40.d0   !Will always start low, so fp_lowflag = .true. so until fp_upflag = .true. then increase inlet temperature
                
            ENDIF
        ENDIF    
    
    ELSE    !Solve to find mass flow that results in target outlet temperature
                 
        err = (T_loop_out - T_loop_outX)/T_loop_out
        
        IF(m_dot_htf==m_dot_htfmin)THEN
            IF(T_loop_outX < T_fp)THEN         !freeze protection mode
                solveMode = 3       
                GOTO 7
            ELSEIF(err>0.d0)THEN
                GOTO 9      !M_dot may already equal m_dot_min while the temperature is still too low, this saves 1 more iteration    
            ENDIF
        ENDIF
        
        !IF((m_dot_htf==m_dot_htfmin).and.(err>0.d0))    GOTO 9      !M_dot may already equal m_dot_min while the temperature is still too low, this saves 1 more iteration    

        !****************************************************   
        !***** Hybrid False Position Iteration Method********  
        !****************************************************
        !Determine next guess for mass flow rate.  Want to use false position method, but it requires that the *results* (err in this case) at both ends of the bracket are known.  We have
        !defined a bracket but not the results.  Use the initial mass flow rate guess to get the results at one end of a new bracket.  Then calculate a new mass flow rate based on the appoximation 
        !We eventually want a result for undefined result in the bracket.  After 2 iterations using approximation without defining bracket, switch to bisection.
        !Once results for both sides are then defined, "lowflag" and "upflag" will be true, and false position method will be applied.
        IF((lowflag).and.(upflag))THEN
            IF(err > 0.d0)THEN
                m_dot_upper = m_dot_htf
                y_upper     = err
            ELSE
                m_dot_lower = m_dot_htf
                y_lower     = err
            ENDIF
                m_dot_htfX = (y_upper)/(y_upper-y_lower)*(m_dot_lower - m_dot_upper) + m_dot_upper
        ELSE
        
            IF(err > 0.d0)THEN      !Prescribed is greater than calculated, so decrease mass flow, so set upper limit
                m_dot_upper = m_dot_htf
                y_upper     = err
                upflag      = .true.
                upmult      = 0.5d0
            ELSE                    !Presribed is less than calculated, so increase mass flow, so set lower limit
                m_dot_lower = m_dot_htf
                y_lower     = err
                lowflag     = .true.
                upmult      = 0.5d0
            ENDIF
            
            IF((lowflag).and.(upflag))THEN  !If results of bracket are defined, use false position
                m_dot_htfX = (y_upper)/(y_upper-y_lower)*(m_dot_lower - m_dot_upper) + m_dot_upper
            ELSE                            !If not, recalculate value based on approximate energy balance
                IF(qq<3)THEN
                    c_htf_ave = specheat(fluid,((T_loop_out+T_loop_in)/2.d0),1.d0)*1000.    !Specific heat
                    m_dot_htfX = sum(q_abs_SCAtot(1:nSCA))/(c_htf_ave*(T_loop_out - T_loop_in))
                    m_dot_htfX = max( m_dot_htfmin, min( m_dot_htfX, m_dot_htfmax))
                ELSE
                    m_dot_htfX = 0.5*m_dot_upper + 0.5*m_dot_lower
                ENDIF
            ENDIF
        ENDIF

        !***************************************************************************
        !****** End Hyrbid False Position Iteration Method *************************
        !***************************************************************************

        IF(m_dot_lower > m_dot_htfmax)THEN      !Once the minimum possible m_dot to solve energy balance is greater than maximum allowable, exit loop and go to defocus
            EXIT
        ENDIF

        IF(m_dot_upper < m_dot_htfmin)THEN      !Once the maximum possible m_dot to solve energy balance is less than minimum allowable, set to min value and get final T_out
            m_dot_htfX = m_dot_htfmin
            solveMode = 3
        ENDIF
    
    ENDIF
    
!    if(info(7) > 10)then
!        exit
!    endif
    
    continue
enddo

9 continue

E_fp_tot = 0.d0
IF(.not.(no_fp))THEN
    E_fp_tot = m_dot_htf_tot * c_hdr_cold * (T_cold_in_1 - T_cold_in)       ![kg/s]*[J/kg-K]*[K] = [W]  Freeze protection energy required
ENDIF

IF(qq.GT.29) THEN
    call messages(-1,"Mass Flow Rate Convergence Error",'WARNING',INFO(1),INFO(2))
    return
ENDIF

m_dot_htfX = m_dot_htf              !for calls back to the temperature loop during the same run, this ensures that the first mass flow rate used will be the last mass flow rate

!******************************************************************
! Special condition checks
!******************************************************************

!After convergence, check to see if the HTF flow rate is over the maximum level

11 continue
if(((m_dot_htf > m_dot_htfmax).and.(dfcount<5)).or.((defocus<1.d0).and.(dfcount==1)))then

    !If so, the field defocus control must be engaged. Do this by calculating the maximum
    !amount of absorbed energy that corresponds to the maximum mass flow rate
    
    !Now determine the number of SCA's that must be defocused to meet this absorption level
    q_check = dmax1(sum(q_abs_SCAtot(1:nSCA)),0.d0) !limit to positive
    !mjw 11.4.2010: the max Q amount is based on the defocus control    
    if(dfcount==1) q_abs_maxOT = dmin1(q_check*defocus,c_htf_ave*m_dot_htfmax*(T_loop_out - T_loop_in_des))  !mjw 11.30.2010
    
    !Select the method of defocusing used for this system
    select case(int(fthrctrl))
    case(0)     
    !Standard defocusing.. 1 SCA at a time completely defocuses
        j=0
        do 
            j=j+1
            !Incrementally subtract the losses, but limit to positive absorption values to avoid 
            !accounting for losses from previously defocused SCA's
            !q_check = q_check - dmax1(q_abs_SCAtot(SCADefocusArray(j))-q_loss_SCAtot(SCADefocusArray(j)), 0.d0)
            !4/9/11, TN: Don't need to subtract losses: see equation for q_check above
            q_check = q_check - dmax1(q_abs_SCAtot(SCADefocusArray(j)), 0.d0)
            
            if((j==nSCA) .or. (q_check <= q_abs_maxOT)) exit
        enddo
        
        !Reassign the flux on each SCA
        do i=1,j
            q_sca(SCADefocusArray(i)) = 0.d0
        enddo
    case(1)
    !Partial defocusing in the sequence specified by the SCADefocusArray
        j=0
        do 
            j=j+1
            !Incrementally subtract the losses, but limit to positive absorption values to avoid 
            !accounting for losses from previously defocused SCA's
            !q_check = q_check - dmin1(dmax1(q_abs_SCAtot(SCADefocusArray(j))-q_loss_SCAtot(SCADefocusArray(j)), 0.d0),q_check-q_abs_maxOT)
            !4/9/11, TN: Don't need to subtract losses: see equation for q_check above
            q_check = q_check - dmax1(q_abs_SCAtot(SCADefocusArray(j)), 0.d0)
            if((j==nSCA) .or. (q_check <= q_abs_maxOT)) exit
        enddo
        
        !Reassign the flux on each SCA
        do i=1,j-1
            q_sca(SCADefocusArray(i)) = 0.d0
        enddo
        q_sca(SCADefocusArray(j)) = q_sca(SCADefocusArray(j))*(q_check-sum(q_abs_SCAtot(SCADefocusArray(j+1:nSCA))))/q_abs_SCAtot(SCADefocusArray(j))
   
    case(2)
    !Partial defocusing of each SCA in the loop simultaneously. Specified defocus order is disregarded.
        q_sca(:) = q_sca(:)*dmin1(q_abs_maxOT/dmax1(q_check,1.e-6),1.d0)  !MJW 7.23.2010::Limit fraction to 1
    end select

    if(q_SCA_tot>0.) then
        SCAs_def = dmin1(sum(q_SCA(1:nSCA))/q_SCA_tot,1.d0)  !MJW 7.23.2010::Limit fraction to 1
    else
        SCAs_def = 1.
    endif
    
    solvemode=2  !Mode 2 -> defocusing
    !mjw 11.4.2010 added conditional statement
    if(dfcount<5) goto 10 !Return to recalculate with new defocus arrangement
endif
!Calculate the amount of defocusing
if(q_SCA_tot>0.) then
    SCAs_def = dmin1(sum(q_SCA(1:nSCA))/q_SCA_tot,1.d0)  !MJW 7.23.2010::Limit fraction to 1
else
    SCAs_def = 1.
endif

!******************************************************************
! Determine whether freeze protection is needed for the SCA's
!******************************************************************
!E_fp(:) = 0.d0
!do i=1,nSCA
!    if(T_htf_ave(i) < T_fp) then
!        CT = SCAInfoArray(i,2)    !Collector type    
!        HT = SCAInfoArray(i,1)    !HCE type
!        do j=1,nHCEVar
!            !E_fp(i) = E_fp(i) +  ((T_fp - T_htf_ave(i))*A_cs(HT,j)*L_actSCA(CT)*rho_htf(i)*c_htf(i) + q_loss_SCAtot(i))*HCE_FieldFrac(HT,j)*dble(nLoops)
!            E_fp(i) = E_fp(i) +  q_loss_SCAtot(i)*HCE_FieldFrac(HT,j)*dble(nLoops)*3600.d0*dt      ![J]
!        enddo
!        T_htf_ave(i) = T_fp
!        T_htf_in(i) = T_fp
!        T_htf_out(i) = T_fp
!    else
!        E_fp(i) = 0.d0
!    endif
!enddo
!E_fp_tot = sum(E_fp(1:nSCA))
!!Reset the loop outlet temperature if it's different
!T_loop_outX = T_htf_out(nSCA)


!******************************************************************************************************************************
! Calculate the pressure drop across the piping system
!******************************************************************************************************************************
!m_dot_htf = 8.673
!------Inlet, Outlet, and COP
DP_IOCOP = PressureDrop(Fluid,m_dot_htf,(T_loop_in + T_loop_outX)/2.d0,1.d0,D_h(SCAInfoArray(1,1),1),&
                HDR_rough,(40.+Row_Distance),0.d0,0.d0,2.d0,0.d0,0.d0,2.d0,0.d0,0.d0,2.d0,1.d0,0.d0)
                if(ErrorFound()) return 1
!-------HCE's
DP_tube = 0.d0
do j=1,nHCEVar
    do i=1,nSCA
        CT = SCAInfoArray(i,2)    !Collector type    
        HT = SCAInfoArray(i,1)    !HCE type
        
        !Account for extra fittings on the first HCE
        if(i==1) then 
            x1 = 10.d0
            x2 = 3.d0
        else
            x1 = 0.d0
            x2 = 1.d0
        endif
        DP_tube(i) = DP_tube(i) + PressureDrop(Fluid,m_dot_htf,T_htf_ave(i),1.d0,D_h(HT,j),(Rough(HT,j)*D_h(HT,j)),&
                     (L_SCA(CT)+Distance_SCA(CT)),0.d0,0.d0,x1,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,x2)*HCE_FieldFrac(HT,j)  
        if(ErrorFound()) return 1
    enddo
enddo
!The pressure drop only across the loop
DP_loop = sum(DP_tube(1:nSCA))

!-------SGS to field section
!if(FieldConfig==1.) then !"H" type
!    x1 = 1.  
!    !MJW changed length calculation 6-8-2010
!    x2 = (40.+Row_distance+(L_SCA(SCAInfoArray(1,2))+Distance_SCA(SCAInfoArray(1,2)))*dble(nSCA))  !x2 is calculation for piping length
!    x3 = 1. !number of expansions
!else !"I" type
!    x1 = 2.
!    x2 = 50.
!    x3 = 0.
!endif

m_dot_htf_tot = m_dot_htf*dble(nLoops)
if(nfsec>2) then  !mjw 5.4.11 Correct the mass flow for situations where nfsec/2==odd
    m_dot_run_in = m_dot_htf_tot/2.d0 * (1. - dble(mod(nfsec,4))/dble(nfsec))
else
    m_dot_run_in = m_dot_htf_tot/2.d0 
endif
x3 = dble(nrunsec) - 1.d0  !Number of contractions/expansions
m_dot_temp = m_dot_run_in
DP_toField = 0.d0; DP_fromField = 0.d0
do i=1,nrunsec
    DP_toField = DP_toField + PressureDrop(Fluid,m_dot_temp,T_loop_in,1.d0,D_runner(i),HDR_rough,L_runner(i),0.d0,x3,0.d0,0.d0,&
                              dmax1(dble(nint(L_runner(i)/70.))*4.,8.),1.d0,0.d0,1.d0,0.d0,0.d0,0.d0)   !*m_dot_temp/m_dot_run_in  !mjw 5.11.11 Correct for less than all mass flow passing through each section
    if(ErrorFound()) return 1                  
    !-------SGS from field section
    DP_fromField = DP_fromField + PressureDrop(Fluid,m_dot_temp,T_loop_outX,1.d0,D_runner(i),HDR_rough,L_runner(i),x3,0.d0,0.d0,0.d0,&
                                  dmax1(dble(nint(L_runner(i)/70.))*4.,8.),1.d0,0.d0,0.d0,0.d0,0.d0,0.d0)   !*m_dot_temp/m_dot_run_in  !mjw 5.11.11 Correct for less than all mass flow passing through each section
    if(ErrorFound()) return 1
    if(i>1) m_dot_temp = dmax1(m_dot_temp - 2.*m_dot_htf_tot/dble(nfsec),0.d0)
enddo
!Calculation for heat losses from hot header and runner pipe
!Pipe_hl_cold = 0.d0; Pipe_hl_hot = 0.d0 !initialize
!do i=1,size(D_hdr)
    !Pipe_hl_hot = Pipe_hl_hot + Row_distance*D_hdr(i)*pi*Pipe_hl_coef*(T_loop_outX - T_db)
    !Pipe_hl_cold = Pipe_hl_cold + Row_distance*D_hdr(i)*pi*Pipe_hl_coef*(T_sys_c - T_db)
!enddo
!Add the runner length
!do i=1,nrunsec
    !Pipe_hl_hot = Pipe_hl_hot + L_runner(i)*pi*D_runner(i)*Pipe_hl_coef*(T_loop_outX - T_db)  !Wt
    !Pipe_hl_cold = Pipe_hl_cold + L_runner(i)*pi*D_runner(i)*Pipe_hl_coef*(T_sys_c - T_db)
!enddo

!-------field header loop
!if(FieldConfig==1.) then    !"H" type
!    x1 = 1.
!else                        !"I" type
!    x1 = 2.
!endif

m_dot_header_in = m_dot_htf_tot/dble(nfsec)
m_dot_header = m_dot_header_in
DP_hdr_cold = 0.d0
DP_hdr_hot = 0.d0
do i=1,nhdrsec
    !Determine whether the particular section has an expansion valve
    x2=0.d0
    if(i>1) then
        if(D_hdr(i)/=D_hdr(i-1)) x2=1.
    endif
    
    !Calculate pressure drop in cold header and hot header sections.. both use similar information
    DP_hdr_cold = DP_hdr_cold + PressureDrop(Fluid,m_dot_header,T_loop_in,1.d0,D_hdr(i),HDR_rough,&
                    (Row_distance+4.275)*2.,0.d0,x2,0.d0,0.d0,1.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0) !*m_dot_header/m_dot_header_in  !mjw/tn 1.25.12 already account for m_dot_header in function call !mjw 5.11.11 scale by mass flow passing though
    if(ErrorFound()) return 1
    DP_hdr_hot =  DP_hdr_hot + PressureDrop(Fluid,m_dot_header,T_loop_outX,1.d0,D_hdr(i),HDR_rough,&
                    (Row_distance+4.275)*2.,x2,0.d0,0.d0,0.d0,1.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0) !*m_dot_header/m_dot_header_in  !mjw 5.11.11
    if(ErrorFound()) return 1
    !Siphon off header mass flow rate at each loop.  Multiply by 2 because there are 2 loops per hdr section
    m_dot_header = dmax1(m_dot_header - 2.*m_dot_htf, 0.d0)
    continue
enddo


!The total pressure drop in all of the piping
DP_tot = (DP_loop + DP_hdr_cold + DP_hdr_hot + DP_fromField + DP_toField + DP_IOCOP)

!The total pumping power consumption
W_dot_pump = DP_tot*m_dot_htf_tot/(rho_hdr_cold*eta_pump)/ 1000.  ![kW]

!The parasitic power consumed by electronics and SCA drives
if(EqOpteff>0.d0) then
    SCA_par_tot = SCA_drives_elec*SCAs_def*dble(nSCA*nLoops)
else
    SCA_par_tot = 0.d0
endif


!******************************************************************************************************************************



!******************************************************************
! Calculate the system transient temperature for the next time step
!******************************************************************
!First, calculate the amount of energy absorbed during the time step that didn't contribute to 
!heating up the solar field
E_avail_tot = sum(E_avail(1:nSCA))*dble(nLoops) - E_fp_tot  ![J]
E_accum_loop = sum(E_accum(1:nSCA))*dble(nLoops) ![J]

!Calculate the HTF mass in the header, balance of field piping, piping to&from the steam generator (SGS)
!The mass of HTF in the system will be calculated based on the design loop inlet temperature
v_tot = v_hot + v_cold
c_hdr_cold = specheat(Fluid,T_loop_in,1.d0)* 1000.
c_hdr_hot = specheat(Fluid,T_loop_outX,1.d0)* 1000.

!Adjust the loop outlet temperature to account for thermal losses incurred in the hot header and the runner pipe
!T_loop_outX = T_loop_outX - Pipe_hl_hot/(m_dot_htf_tot*c_hdr_hot)
!
!!Calculate the system temperature of the hot portion of the collector field. 
!!This will serve as the fluid outlet temperature
!T_sys_h = (T_sys_h_last - T_loop_outX)*exp(-m_dot_htf_tot/(v_hot*rho_hdr_hot+mc_bal_hot/c_hdr_hot)*dt*3600.) + T_loop_outX
!
!!Enforce freeze protection on the system temperatures
!if(T_sys_h < T_fp) then
!    !E_fp_tot = E_fp_tot + dmax1((T_fp - T_sys_h)*(v_header + v_tofrom_sgs)*rho_hdr_hot*c_hdr_hot + pipe_hl_hot , 0.d0)
!    E_fp_tot = E_fp_tot + Pipe_hl_hot*dt*3600.d0 ![J]
!    T_sys_h = T_fp
!endif
!if(T_sys_c < T_fp) then
!    !E_fp_tot = E_fp_tot + dmax1((T_fp - T_sys_c)*(v_header + v_tofrom_sgs)*rho_hdr_cold*c_hdr_cold + pipe_hl_cold , 0.d0)
!    E_fp_tot = E_fp_tot + Pipe_hl_cold*dt*3600.d0 ![J]
!    T_sys_c = T_fp
!endif

!Half of the plant thermal mass must be heated up to the startup temperature (think hot header, hot half of heat
!exchangers) and the other half must be heated up to the design loop inlet temperature
!MJW 12.8.2010 modified startup temp calc to be based on previous system temperature
!MJW 12.14.2010 Limit to positive to avoid step-to-step oscillation introduced by using previous step. 
!.. This may cause a minor underestimation of annual energy output (<<.5%).
E_accum_hdr = (v_hot*rho_hdr_hot*c_hdr_hot + mc_bal_hot)*(T_sys_h - T_sys_h_last) + & !Hot half
              (v_cold*rho_hdr_cold*c_hdr_cold + mc_bal_cold)*(T_sys_c - T_sys_c_last)   !cold half
E_bal_startup = dmax1(E_accum_hdr,0.d0) !cold half

!mjw 1.17.2011 Calculate the total energy content of the solar field relative to a standard ambient temp. of 25[C]
rho_ave = density(fluid,((T_loop_outX+T_sys_c)/2.d0),0.d0) !kg/m3
c_htf_ave = specheat(fluid, (T_sys_h + T_cold_in_1)/2.d0, 1.d0)*1000.d0  !MJW 12.7.2010
E_field = ((v_hot*rho_hdr_hot*c_hdr_hot + mc_bal_hot)*(T_sys_h - 298.15d0) + &      !hot header and piping
           (v_cold*rho_hdr_cold*c_hdr_cold + mc_bal_cold)*(T_sys_c - 298.15d0) + &  !cold header and piping
           sum(E_int_loop)*dble(nloops))  !Field loops

!6/14/12, TN: Redefine pipe heat losses with header and runner components to get total system losses
Pipe_hl_hot = N_run_mult*Runner_hl_hot + dble(Nfsec)*Header_hl_hot
Pipe_hl_cold = N_run_mult*Runner_hl_cold + dble(Nfsec)*Header_hl_cold

pipe_hl_tot = pipe_hl_hot + pipe_hl_cold

E_avail_tot = dmax1(E_avail_tot - (Pipe_hl_hot+Pipe_hl_cold)*dt*3600., 0.d0)    ![J] 11/1/11 TN: Include hot and cold piping losses in available energy calculation

E_avail_tot = dmax1(E_avail_tot - E_bal_startup, 0.d0)  ![J]

!******************************************************************
! Calculate final output values
!******************************************************************
DP_tot = DP_tot * 1.e-5 ![bar]
!Calculate the thermal power produced by the field
if(T_sys_h >= T_startup) then  !MJW 12.14.2010 Limit field production to above startup temps. Otherwise we get strange results during startup. Does this affect turbine startup?
    q_avail = E_avail_tot/(dt*3600.)*1.e-6  ![MW]
    !Calculate the available mass flow of HTF
    m_dot_avail = dmax1(q_avail*1.e6/(c_htf_ave*(T_sys_h - T_cold_in_1)),0.d0) ![kg/s] 
    
else
    q_avail = 0.d0
    m_dot_avail = 0.d0
endif

!Dumped energy
q_dump = Ap_tot*I_b*EqOptEff*(1.-SCAs_def)/1.e6  !MW

!Total field performance
q_field_delivered = m_dot_htf_tot * c_htf_ave * (T_sys_h - T_cold_in_1) / 1.e6 !MJW 1.11.11 [MWt]
eta_thermal = q_field_delivered / (I_b*CosTh_ave*Ap_tot / 1.e6)  !MJW 1.11.11


!------mjw 1.5.2011 For initialization in acceptance testing mode, check to see if the solar field is in equilibrium. 
!if not (and if it's the first call), go back and keep warming up.
if((time == getSimulationStartTime()).and.(info(7)==0)) then
    if(accept_init) then
        if(E_bal_startup > 0.d0) then
            ! Set the system state values for the next calculation as if we were proceeding to the next timestep
            stored(1) = T_sys_c       !Set T_sys_c_last
            stored(2) = T_sys_h       !Set T_sys_h_last
            stored(3) = 0.            !Reset iteration every timestep
            j=3
            do i=1,nSCA
                stored(i + j) = T_htf_in(i)  !Set T_htf_in0
            enddo    
            do i=1,nSCA
                stored(i + j + nSCA) = T_htf_out(i)  !Set T_htf_out0
            enddo

            call setStorageVars(stored,nS,INFO)
            
            goto 5  !Loop back
        endif
    endif
endif
!----------------------------

!!----set iteration-dependent storage variables
!stored(3) = 1.
!call setStorageVars(stored,nS,INFO)

!-------------Set the outputs and return------------------------------------------------------------------------------------
12 continue
OUT(1) = T_sys_h-273.15             ![C] outlet temperature
OUT(2) = m_dot_avail*3600.          ![kg/hr] mass flow rate
OUT(3) = q_avail                    ![MWth] thermal power produced by the field
OUT(4) = DP_tot                     ![bar] total HTF pressure drop
OUT(5) = W_dot_pump/1000.           ![MWe] required pumping power
!OUT(6) = E_fp_tot/(dt*3600.)*1.e-6  ![MW] Freeze protection energy
OUT(6) = E_fp_tot*1.e-6             ![MW] Freeze protection rate
OUT(7) = qq                         !Number of iterations required to solve
OUT(8) = T_sys_c-273.15             ![C] Collector inlet temperature
!4/16/12: Added cosine effect and accounting for defocusing
OUT(9) = EqOpteff * CosTh_ave*ftrack                   !Collector equivalent optical efficiency, includes envelope losses
OUT(10)= SCAs_def                   !The fraction of focused SCA's
OUT(11)= m_dot_htf_tot*3600.        ![kg/hr] The actual flow rate through the field.. NOTE this is NOT the available flow rate for use in 
                                    !the power block or in storage! This is only to be reconnected as an input to this type.
OUT(12)= E_bal_startup/(dt*3600.)*1.e-6 ![MWt] startup energy consumed
OUT(13)= Ap_tot*I_b/1.e6            ![MWt] Total power incident on the field
OUT(14)= sum(q_abs_SCAtot(1:nSCA))/1.e6*dble(nLoops) ![MWt] Total absorbed energy
OUT(15)= sum(q_loss_SCAtot(1:nSCA))/1.e6*dble(nLoops) ![MWt] Total HCE thermal losses 
OUT(16)= m_dot_htf                  ![kg/s] Flow rate in a single loop
OUT(17)= sum(q_1abs_tot(1:nSCA))/dble(nSCA)
OUT(18)= SCA_par_tot/1.e6           ![MWe] Parasitic electric power consumed by the SCA drives and electronics
OUT(19)= Pipe_hl_tot/1.e6           ![MWt] Pipe heat loss in the hot header and the hot runner pipe. Does not include piping losses in the field
OUT(20)= q_dump                     ![MWt] Dumped thermal energy
OUT(21)= Theta_ave*180./pi          ![deg] Field average theta value
OUT(22)= CosTh_ave*ftrack           ![none] Field average costheta value  MJW 11.10.2010 added ftrack
OUT(23)= IAM_ave                    ![none] Field average incidence angle modifier
OUT(24)= RowShadow_ave              ![none] Field average row shadowing loss
OUT(25)= EndLoss_ave                ![none] Field average end loss
OUT(26)= I_b*CosTh_ave              ![W/m2] DNI_x_CosTh
OUT(27)= OUT(26)*Ap_tot/1.e6        ![MW] Q_inc_x_CosTh
OUT(28)= SolarAlt*180./pi           ![deg] solar altitude used in optical calculations
OUT(29)= SolarAz*180./pi            ![deg] solar azimuth used in optical calculations
OUT(30)= T_loop_outX - 273.15d0     ![C] HTF temperature at the loop outlet
OUT(31)= c_htf_ave                  ![J/kg-K] Average solar field specific heat 
OUT(32)= q_field_delivered          ![MWt] Solar field power delivered to the power block heat exchanger
OUT(33)= eta_thermal                ![none] The solar field thermal efficiency, defined as q_field_delivered/(ANI x Aperture area)
OUT(34)= E_accum_loop*3.6e-9        ![MWht] Total internal energy change in the loop ONLY
OUT(35)= E_accum_hdr*3.6e-9         ![MWht] Total internal energy change in the headers
OUT(36)= sum(OUT(34:35))            ![MWht] Sum of internal energy change rates in the loop and headers
OUT(37)= E_field*3.6e-9             ![MWht] Internal energy of the solar field, relative to an ambient temperature of 25 C

return 1

contains
    real(8) function mlimit(val, hi, low) 
    implicit real(8) (a-z)
        mlimit=val
        if(val<low) mlimit = low
        if(val>hi) mlimit = hi
        
    end function

end subroutine

!This subroutine contains the trough detailed plant model.  The collector field is modeled
!using an iterative solver.
!This code was written for the National Renewable Energy Laboratory
!Copyright 2009-2010
!Author: Mike Wagner

!Subroutine Inputs (and parameters)
! ----------------------------------------------------------------------------------------------------------------------
! Nb | Variable             | Description                                             | Input  Units   | Internal Units 
! ---|----------------------|---------------------------------------------------------|----------------|----------------
! 1  | T_1_in               | Receiver inlet temperature                              |                |
! 2  | m_dot                | Heat transfer fluid mass flow rate                      |                |
! 3  | T_amb                | Ambient dry-bulb temperature                            |                |
! 4  | T_sky                | Sky temperature                                         |                |
! 5  | v_6                  | Ambient wind velocity                                   |                |
! 6  | P_6                  | Ambient atmospheric pressure                            |                |
! 7  | q_i                  | Total incident irradiation on the receiver              |                |
! 8  | A_cs                 | Internal absorber tube cross-sectional area             |                |
! 9  | D_2                  | Internal absorber tube diameter                         |                |
! 10 | D_3                  | External absorber tube diameter                         |                |
! 11 | D_4                  | Internal glass envelope diameter                        |                |
! 12 | D_5                  | External glass envelope diameter                        |                |
! 13 | D_p                  | (optional) Plug diameter                                |                |
! 14 | D_h                  | Absorber tube hydraulic diameter                        |                |
! 15 | eps_mode             | Interpolation mode for the emissivity (1=table,2=fixed) |                |
! 16 | xx                   | Array of temperature values for emissivity table        |                |
! 17 | yy                   | Array of emissivity values for table                    |                |
! 18 | nea                  | Number of entries in the emissivity table               |                |
! 19 | L_actSCA             | Length of the active receiver surface                   |                |
! 20 | single_point         | Logical flag - is the calculation for a single point?   |                |
! 21 | Epsilon_32           | Constant value for emissivity if table isn't used       |                |
! 22 | Epsilon_4            | Envelope inner surface emissivity                       |                |
! 23 | Epsilon_5            | Envelope outer surface emissivity                       |                |
! 24 | Alpha_abs            | Absorber tube absorptance                               |                |
! 25 | alpha_env            | Envelope absorptance                                    |                |
! 26 | ColOptEff            | Collector optical efficiency                            |                |
! 27 | Tau_envelope         | Total envelope transmittance                            |                |
! 28 | P_a                  | Annulus gas pressure                                    | torr           |
! 29 | Flow_type            | Flag indicating the presence of an internal plug        |                |
! 30 | AnnulusGas           | Annulus gas type                                        |                |
! 31 | Fluid                | Heat transfer fluid type                                |                |
! 32 | AbsorberMaterial     | Absorber material type                                  |                |
! 33 | time                 | Simulation time                                         |                |

!Subroutine outputs 
! ----------------------------------------------------------------------------------------------------------------------
! Nb | Variable             | Description                                             | Input  Units   | Internal Units 
! ---|----------------------|---------------------------------------------------------|----------------|----------------
! 1  | q_heatloss           | Total heat loss from the receiver                       | W/m            |
! 2  | q_12conv             | Total heat absorption into the HTF                      | W/m            |
! 3  | q_34tot              | Convective and radiative heat loss                      |                |
! 4  | c_1ave               | Specific heat of the HTF across the receiver            | kJ/kg-K        |
! 5  | rho_1ave             | Density of the HTF across the receiver                  |                |

! ----------------------------------------------------------------------------------------------------------------------
!Forristall Temperature distribution diagram
!*****************************************************
!	Fluid (1) ----------->(2)<--Absorber-->(3)<-- Annulus -->(4)<--- Glass  --->(5)<-- Air (6)/Sky (7)
!
!
!	T_1 = Bulk heat transfer fluid (HTF) temperature 
!	T_2 = Absorber Inside surface temperature
!	T_3 = Absorber outside surface temperature 
!	T_4 = Glass envelope inside surface temperature
!	T_5 = Glass envelope outside surface temperature
!	T_6 = Ambient temperature
!	T_7 = Effective Sky Temperature
!
!	q_12conv = Convection heat transfer rate per unit length between the HTF and the inside of the receiver tube
!	q_23cond = Conduction heat transfer rate per unit length through the absorber
!	q_34conv = Convection heat transfer rate per unit length between the absorber outer surface and the glazing inner surface
!	q_34rad = Radiation heat transfer rate per unit length between the absorber outer surface and the glazing inner surface
!	q_45cond = Conduction heat transfer rate per unit length through the glazing
!	q_56conv = Convection heat transfer rate per unit length between the glazing outer surface and the ambient air
!	q_57rad = Radiation heat transfer rate per unit length between the glazing outer surface and the sky
! ----------------------------------------------------------------------------------------------------------------------
subroutine EvacReceiver(T_1_in, m_dot, T_amb, T_sky, v_6, P_6, q_i,A_cs, D_2, D_3, D_4, D_5, D_p, D_h, &
                eps_mode,xx,yy,nea,L_actSCA,single_point,&            !Inputs added TN 6/15/11
                Epsilon_32,Epsilon_4, Epsilon_5, Alpha_abs, alpha_env, ColOptEff, Tau_envelope, &
                P_a, Flow_type, AbsorberMaterial, annulusGas, glazingIntact, Fluid,info,time,&
                !outputs
                q_heatloss, q_12conv, q_34tot, c_1ave, rho_1ave)

implicit none 

!---Variable declarations------
real(8),intent(in):: T_1_in, m_dot, T_amb, T_sky, v_6, P_6, q_i,&
                     A_cs, D_2, D_3, D_4, D_5, D_p, D_h,&
                     L_actSCA,& !Inputs added to calculate transient
                     Epsilon_32,Epsilon_4, Epsilon_5, Alpha_abs, alpha_env, ColOptEff, Tau_envelope, &
                     P_a, Flow_type, annulusGas, Fluid, AbsorberMaterial, time
logical,intent(in)::glazingIntact, single_point
logical:: wasIntact, reguess
real(8)::T_2, T_3, T_4, T_5, T_6, T_7, rho_1ave, c_1ave, density, specheat, &
         pi, v_1, FQ_12CONV, Q_12CONV, Fk_23, k_23, Q_34conv, q_34rad, h_34conv, h_34rad, &
         q_23cond, k_45, q_45cond, q_56conv, h_56conv, q_57rad, q_3SolAbs, q_5solabs, &
         q_cond_bracket, Fq_cond_bracket, q_heatloss, fT_2,&
         R_45cond, h_57rad, R_57rad, R_37eff, q_34tot, T_save(5),&
         T_1last, T_2g, cp_1, T3_tol, q5_tol,&
         T1_tol, T2_tol, Diff_T3,diff_q5,T_lower,T_upper,q_5out,T_1_out,diff_T1,&
         T_1_ave,T_1_out1,diff_T2,eps_3,epsilon_3,eps3old,q_in_W,Pa_old,T_upper_max,&
         y_upper,y_lower,upmult,q5_tol_1,T3_upper,T3_lower,y_T3_upper,y_T3_lower,abs_diffT3
integer,intent(in)::nea,eps_mode
LOGICAL::UPFLAG,LOWFLAG,T3upflag,T3lowflag
integer::qq,q5_iter,T1_iter,q_conv_iter
real(8),intent(in)::xx(nea),yy(nea)
integer*4,intent(in)::info(15)
save T_save, wasIntact, T_1last, eps3old,Pa_old
pi=3.1415926
!-------

!---Re-guess criteria:---
if(time<=2) goto 10
if(wasIntact /= glazingIntact) goto 10
if(P_a /= Pa_old) goto 10                   !Add logic to reguess for different annulus pressures
if(abs(T_1last-T_1_in) > 50.) goto 10
if(minval(T_save(:)) < T_sky - 1.) goto 10
if(isnan(sum(T_save(:)))) goto 10
    reguess=.false.
    goto 20
10 continue
    reguess=.true.
20 continue
!------------------------

if(reguess) then
    if(glazingIntact) then
        T_save(1) = T_1_in
        T_save(2) = T_1_in+2.
        T_save(3) = T_save(2) + 5.
        if(P_a > 1.d0)then          !Set guess values for different annulus pressures
            T_save(4) = T_save(3) - 0.5*(T_save(3)-T_amb)       !If higher pressure, guess higher T4   
            T_upper_max = T_save(3) - 0.2*(T_save(3)-T_amb)     !Also, high upper limit for T4
        else
            T_save(4) = T_save(3) - 0.9*(T_save(3)-T_amb)       !If lower pressure, guess lower T4
            T_upper_max = T_save(3) - 0.5*(T_save(3)-T_amb)     !Also, low upper limit for T4
        endif                      
        Pa_old  = P_a               !Reset previous pressure
        T_save(5) = T_save(4) - 2.
        wasIntact = glazingIntact   !Reset previous glazing logic
        T_1last = T_1_in            !Reset previous T_1_in
    else
        T_save(1) = T_1_in
        T_save(2) = T_1_in+2.
        T_save(3) = T_save(2) + 5.
        T_save(4:5) = T_amb
        wasIntact = glazingIntact   !Reset previous glazing logic
        T_1last = T_1_in            !Reset previous T_1_in
    endif
endif

!Set intial guess values
T_2 = T_save(2)
T_3 = T_save(3)
T_4 = T_save(4)
T_5 = T_save(5)
!Set constant temps
T_6 = T_amb
T_7 = T_sky          

qq = 0                  !Set iteration counter for T3 loop

T_2g = T_2              !Initial guess value for T_2 (only used in property lookup)        
cp_1 = 1950.            !Initial guess value for cp of WF

!Tolerances for iteration
T3_tol = 1.5e-3           
q5_tol = 1.0e-3         !Since iterations are nested inside T3, make tolerances a bit tighter        
T1_tol = 1.0e-3        
T2_tol = 1.0e-3        

!Decreasing the tolerance helps get out of repeating defocus iterations
IF(info(7)>8)THEN
    T3_tol = 1.5e-4        !1.d0   
    q5_tol = 1.0e-4        !max(1.d0, 0.001*q_i)
    T1_tol = 1.0e-4        !1.d0
    T2_tol = 1.0e-4        !1.d0
ENDIF

diff_T3 = 10.d0 + T3_tol    !Set difference > tolerance

!Constants
k_45 = 1.04                             ![W/m-K]  Conductivity of glass
R_45cond = log(D_5/D_4)/(2.*pi*k_45)    ![K-m/W]Equation of thermal resistance for conduction through a cylinder

IF(glazingIntact)THEN   !These calculations (q_3SolAbs,q_5solAbs) are not dependent on temperature, so only need to be computed once per call to subroutine
    
    q_3SolAbs = q_i * ColOptEff * tau_envelope * Alpha_abs  ![W/m]  
    !We must account for the radiation absorbed as it passes through the envelope
    q_5solabs = q_i * ColOptEff * alpha_env   ![W/m]  
    
ELSE
    
    !Calculate the absorbed energy 
    q_3SolAbs = q_i * ColOptEff * Alpha_abs  ![W/m]  
    !No envelope
    q_5solabs = 0.d0                            ![W/m]

ENDIF

eps_3 = epsilon_32          !Set epsilon value for case that eps_mode = 1.  Will reset inside temp loop if eps_mode > 1.

T3upflag = .false.
T3lowflag = .false.

DO WHILE(((abs(diff_T3)>T3_tol).and.(qq<100)).or.(qq<2))    !Outer loop: Find T_3 such than energy balance is satisfied
    qq=qq+1 !loop counter

    IF(qq>1)THEN
        IF((T3upflag).and.(T3lowflag))THEN
            IF(diff_T3 > 0.)THEN
                T3_upper = T_3
                y_T3_upper = diff_T3
            ELSE    
                T3_lower = T_3
                y_T3_lower = diff_T3
            ENDIF
            T_3 = (y_T3_upper)/(y_T3_upper-y_T3_lower)*(T3_lower - T3_upper) + T3_upper

        ELSE
            IF(diff_T3 > 0.)THEN
                T3_upper = T_3
                y_T3_upper = diff_T3
                T3upflag = .true.
            ELSE    
                T3_lower = T_3
                y_T3_lower = diff_T3
                T3lowflag = .true.
            ENDIF
            
            IF((T3upflag).and.(T3lowflag))THEN  
                T_3 = (y_T3_upper)/(y_T3_upper-y_T3_lower)*(T3_lower - T3_upper) + T3_upper
            ELSE
                T_3 = T_3 - abs_diffT3         !Note that recalculating T_3 using this exact equation, rather than T_3 = T_3 - frac*diff_T3 was found to solve in fewer iterations
            ENDIF
        ENDIF
    ENDIF
            

    !Calculate temperature sensitive emissivity using T_3, if required
    if(eps_mode>1) call interp((T_3-273.15),eps_mode,xx,yy,eps3old,eps_3)

    !Separate GlazingIntact = .true. and GlazingIntact = .false.  If true, T4 must be solved, if false then T4 is explicitly known (or doesn't exist, depending on how you want to look at it)
    !Solving for correct T4 as it relates to current T3 value
    IF (glazingIntact) THEN
    
        !**********************************************
        !************* SET UP T_4 ITERATION **********************
        !**********************************************
        
        IF(qq==1)THEN               !If first iteration, set T_4 bounds to phyiscal limits defined by T_3 and T_sky
            T_lower = T_sky         !Lowest possible temperature of T_4 is sky temp        
            T_upper = max(T_upper_max,T_amb)    !Highest possible temperature is the highest temperature on either side of T_4: either T_3 or ambient
            q5_tol_1= 0.001           !Just get T4 in the ball park.  '20' may not be the optimum value.....
        ELSE                                            !For additional iterations:
            T_lower = T_lower - max(abs_diffT3,0.d0)       !If diff_T3 is + then new T3 < old T3 so adjust lower limit
            T_upper = T_upper + abs(min(abs_diffT3,0.d0))  !If diff_T3 is (-) then new T3 > old T3 so adjust upper limit
            q5_tol_1= q5_tol        !For remaining T3 iterations, use specified tolerance (note that 2 iterations for T3 are gauranteed)                   
        ENDIF
        
        diff_q5 = q5_tol_1 + 1.d0       !Set diff > tolerance
        q5_iter = 0                     !Set iteration counter
        
        UPFLAG      = .FALSE.           !Set logic to switch from bisection to false position mode
        LOWFLAG     = .FALSE.           !Set logic to switch from bisection to false position mode   
        !***********************************************************************************
        !************* Begin Bisection/False Position Iteration method *********************
        !***********************************************************************************
        DO WHILE((abs(diff_q5)>q5_tol_1).and.(q5_iter<100))       !Determine T_4 such that energy balance from T_3 to surroundings is satisfied
            
            q5_iter = q5_iter + 1                       !Increase iteration counter

            !The convective heat exchange between the absorber and the envelope
            !      UNITS   ( K , K ,  m ,  m , m,torr, Pa , m/s,  K , real{1,26,27}, logical  ,   W/m   , W/m2-K)
            call FQ_34CONV(T_3,T_4, D_3, D_4, P_a, P_6, v_6, T_6, annulusGas, glazingIntact, q_34conv, h_34conv)   
            
            !The radiative heat exchange between the absorber and the envelope
            !    Units         ( K ,  K ,  m ,  m , K  ,    -     ,    -     ,   logical    ,  W/m   , W/m2-K)
            call FQ_34RAD(T_3, T_4, D_3, D_4, T_7, eps_3, EPSILON_4, glazingIntact, q_34rad, h_34rad)       
            !The total heat exchange between absorber and envelope
            q_34tot = q_34conv + q_34rad            ![W/m]
    
            !**********************************************
            !************* Calculate T_5 *************
            !**********************************************
            !The thermal energy flow across 45 is equal to the energy from the absorber plus
            !the thermal energy that is generated by direct heating of the glass envelope
            q_45cond = q_34tot + q_5solabs          ![W/m]
            
            !Knowing heat flow and properties, T_5 can be calculated
            T_5 = T_4 - q_45cond*R_45cond           ![K]
   
            !*************************************************************************
            !************* Calculate HT from exterior surface to ambient *************
            !*************************************************************************
            !With T_5 and T_6 (amb T) calculate convective and radiative loss from the glass envelope
            !           units   ( K ,  K ,  m , torr, m/s,  logical     ,  W/m    , W/m2-K)
            call fq_56conv(T_5, T_6, D_5, P_6, v_6, GlazingIntact, q_56conv, h_56conv) ![W/m]    
            q_57rad = epsilon_5 * 5.67e-8 * (T_5**4 - T_7**4)
            q_5out = q_57rad + q_56conv     ![W/m]

            !***************************************************************************
            !********** Compare q_5out with q_45 cond***********************************
            !***************************************************************************
            diff_q5 = (q_5out - q_45cond)/q_45cond     ![W/m]
            
            !Determine next guess for T_4.  Want to use false position method, but it requires that the *results* at both ends of the bracket are known.  We have
            !defined a bracket but not the results.  Use the guess T_4 to get the results at one end of a new bracket.  Then calculate a new T_4 that is highly weighted 
            !towards the side of the original bracket that the 1st T_4 did not replace.  In most cases, this new T_4 will result in the opposite diff_q5, which 
            !defines both sides of the bracket.  If results for both sides are then defined, "lowflag" and "upflag" will be true, and false position method will be applied.

            IF((lowflag).and.(upflag))THEN          !False position method     
                IF(diff_q5>0.d0)THEN
                    T_upper = T_4       !If energy leaving T_5 is greater than energy entering T_5, then T_4 guess is too high
                    y_upper = diff_q5   !so set new upper limit to T_4
                ELSE                    !If energy leaving T_5 is less than energy entering T_5, then T_4 guess is too low
                    T_lower = T_4       !so set new lower limit to T_4
                    y_lower = diff_q5   !also, set result to go along with lower limit
                ENDIF    
                T_4 = (y_upper)/(y_upper-y_lower)*(T_lower - T_upper) + T_upper             
            
            ELSE                        !For bisection method...
                 
                IF(diff_q5>0.d0)THEN    !If energy leaving T_5 is greater than energy entering T_5, then T_4 guess is too high
                    T_upper = T_4       !so set new upper limit to T_4
                    y_upper = diff_q5   !also, set result to go along with upper limit
                    upflag  = .true.    !Upper result is now known
                    IF(qq==1)THEN
                    upmult  = 0.1       !Just want to get in ballpark for first iteration of receiver
                    ELSE
                    upmult  = 0.1       !Weight such that next calculated T_4 (if using bisection method) results in negative diff_q5
                    ENDIF
                    
                ELSE                    !If energy leaving T_5 is less than energy entering T_5, then T_4 guess is too low          
                    T_lower = T_4       !so set new lower limit to T_4
                    y_lower = diff_q5   !also, set result to go along with lower limit
                    lowflag = .true.    !Lower result is now known
                    IF(qq==1)THEN
                    upmult  = 0.1       !Just want to get in ballpark for first iteration of receiver
                    ELSE
                    upmult  = 0.9       !Weight such that next calculated T_4 (if using bisection method) results in positive diff_q5
                    ENDIF
                    
                ENDIF
                
                IF((lowflag).and.(upflag))THEN  !If results of bracket are defined, use false position
                    T_4 = (y_upper)/(y_upper-y_lower)*(T_lower - T_upper) + T_upper
                ELSE                            !If not, keep bisection
                    T_4 = (1.-upmult)*T_lower + upmult*T_upper
                ENDIF

            ENDIF  
  
        !*********************************************************************************************
        !********** END Bisection/False Position Iteration Loop on T_4 *******************************
        !*********************************************************************************************       
        ENDDO
            
    ELSE      !Glazing is not intact

        !Know convection and radiation forcing temps
        !----Having guessed the system temperatures, calculate the thermal losses starting from
        !----the absorber surface (3)
        !The convective heat exchange between the absorber and the envelope
        call FQ_34CONV(T_3,T_4, D_3, D_4, P_a, P_6, v_6, T_6, annulusGas, glazingIntact, q_34conv, h_34conv)   
        !The radiative heat exchange between the absorber and the envelope
        call FQ_34RAD(T_3, T_4, D_3, D_4, T_7, eps_3, EPSILON_4, glazingIntact, q_34rad, h_34rad)       
        !The total heat exchange between absorber and envelope
        q_34tot = q_34conv + q_34rad    ![W/m]
        
    ENDIF      !Know heat transfer from outer surface of receiver tube to ambient
    
    !Bracket Losses
    !Bracket conduction losses apply 
    q_cond_bracket = Fq_cond_bracket(T_3, T_6, P_6, v_6)    ![W/m] 

    q_12conv = q_3SolAbs - (q_34tot+q_cond_bracket)         ![W/m] Energy transfer to/from fluid based on energy balance at T_3
    
    q_in_W  = q_12conv * L_actSCA                           !Convert [W/m] to [W] for some calculations
    
    if(.not.single_point) then
        T_1_out = max( T_sky, q_in_W/(m_dot*cp_1) + T_1_in )    !Estimate outlet temperature with previous cp
        
        diff_T1 = T1_tol + 1.d0                                 !Set diff > tolerance
        T1_iter = 0                                             !Set iteration counter    
        
        DO WHILE((abs(diff_T1)>T1_tol).and.(T1_iter<100))       !Find correct cp& rho and solve for T_1_ave
        
            T1_iter = T1_iter + 1                   !Increase iteration counter
            T_1_ave = (T_1_out + T_1_in) / 2.d0     !Average fluid temperature
            cp_1    = specheat(fluid,T_1_ave,1.d0)*1000.
            T_1_out1= max( T_sky, q_in_W/(m_dot*cp_1) + T_1_in )  !Estimate outlet temperature with previous cp 
            diff_T1 = (T_1_out - T_1_out1)/T_1_out  !Difference between T_1_out used to calc T_ave, and T_1_out calculated with new cp
            T_1_out = T_1_out1                      !Calculate new T_1_out
        
        ENDDO
    else
        !If we're only calculating performance for a single point, set the receiver ave/outlet temperature to the inlet.
        T_1_out = T_1_in
        T_1_ave = T_1_in
    endif
    
    rho_1ave    = density(fluid,T_1_ave,0.d0)       ![kg/m^3] Density
    v_1         = m_dot/(rho_1ave*A_cs)             !HTF bulk velocity
    
    q_conv_iter = 0                 !Set iteration counter
    diff_T2 = 1.d0 + T2_tol         !Set diff > tolerance
    
    !Ensure convective calculations are correct (converge on T_2)
    DO WHILE((abs(diff_T2)>T2_tol).and.(q_conv_iter<100))
 
        q_conv_iter = q_conv_iter + 1       !Increase iteration counter

        T_2 =  fT_2(q_12conv, T_1_ave, T_2g, Fluid, D_2, D_p, D_h, v_1, Flow_type, .true.)  !Calculate T_2 (with previous T_2 as input)
        diff_T2 = (T_2 - T_2g)/T_2          !T_2 difference
        T_2g = T_2 - 0.5*(T_2-T_2g)         !Reset T_2
        
        IF(qq<2)THEN        !For first T3 iteration, do not iterate on T_2 (again, this control is based on observation of solve time and may not be optimal for all simulations)
            exit
        ENDIF
 
    ENDDO
    
    !The conductive heat transfer equals the convective heat transfer (energy balance)
    q_23cond = q_12conv         ![W/m]
    
    !Calculate tube conductivity 
    k_23 = Fk_23(T_2, T_3, AbsorberMaterial)      ![W/m-K]  

    !Update the absorber surface temperature (T_3) according to new heat transfer rate
    abs_diffT3 = T_3 - (T_2 + q_23cond*log(D_3/D_2)/(2.*pi*k_23))
    diff_T3 = abs_diffT3/T_3
   
    
ENDDO

!Warning of convergence failure
!IF(qq.GT.99) THEN                           !End simulation if loop does not converge
!    call messages(-1,"Trough Energy Balance Convergence Error 1",'WARNING',INFO(1),INFO(2))
!    return
!ENDIF
!
!IF(T1_iter.GT.99) THEN
!    call messages(-1,"Trough Energy Balance Convergence Error 2",'WARNING',INFO(1),INFO(2))
!    return
!ENDIF
!
!IF(q_conv_iter.GT.99) THEN
!    call messages(-1,"Trough Energy Balance Convergence Error 3",'WARNING',INFO(1),INFO(2))
!    return
!ENDIF
!
!IF(q5_iter.GT.99) THEN
!    call messages(-1,"Trough Energy Balance Convergence Error 4",'WARNING',INFO(1),INFO(2))
!    return
!ENDIF
  
!Calculate specific heat in kJ/kg
c_1ave = cp_1/1000.
 
q_heatloss = q_34tot + q_cond_bracket + q_5solabs   ![W/m]

!Save temperatures
T_save(2) = T_2
T_save(3) = T_3 
T_save(4) = T_4
T_save(5) = T_5

continue
end subroutine


!#################################################################################################################
!#################################################################################################################
!#################################################################################################################


!"******************************************************************************************************************************
! 	FUNCTION Fq_12conv :  Convective heat transfer rate from the HTF to the inside of the receiver tube
!******************************************************************************************************************************"
!  Author: R.E. Forristall (2003, EES)
!  Implemented and revised:  M.J. Wagner (10/2009)
!                Copyright:  National Renewable Energy Lab (Golden, CO) 2009
!                     note:  This function was programmed and tested against the EES original.
!                            Small variations in output are due to slightly different fluid 
!                            properties used in the two models.
!
!{	Newton's Law of Cooling.
!
!		q' = h * D_i *  PI * (T_m - T_s)
!
!		h = Nu_Di * k / D_i
!
!	Where
!
!		q' = convection heat transfer rate per unit length [W/m]
!		h = convection heat transfer coefficient [W/m^2-k]
!		D_i = inside diameter of absorber pipe [m]
!		T_m = mean (bulk) temperature of HTF [C]
!		T_s = inside surface temperature of absorber pipe [C]
!		Nu_Di = Nusselt number based on inside diameter
!		k = conduction heat transfer coefficient of HTF [W/m-K]
!
!	The Nusselt number is estimated with the correlation developed by Gnielinski. 
!
! 		Nu# = (f / 8) * (Re_Di - 1000) * Pr / (1 + 12.7 * (f / 8)^(1/2) * (Pr^(2/3) -1))  * (Pr / Pr_w)^0.11
!		f = (1.82 * log10(Re_Di) - 1.64)^(-2)
!		Re_Di = Rho * v_m * Di / u
!		Pr  = Cp * u / k
!
!	Where
!
!		Nu# = Nusselt number
!		Re_Di = Reynolds number for internal pipe flow
!		Pr = Prandtl number
!		Pr_w = Prandtl number evaluated at the wall temperature
!		u = fluid absolute viscosity [kg/m-s]
!		Di = inside diameter [m]
!		Cp = fluid specific heat [J/kg-K]
!		k = fluid thermal conductivity [W/m-K]
!		Rho = fluid density [kg/m^3]
!		v_m = mean fluid velocity [m/s]
!
!The above correlation is valid for 0.5 < Pr < 2000 and 2300< Re_Di < 5 * 10^6 and can be used for both uniform heat flux and uniform wall temperature cases. With the exception of Pr_w, all properties are evaluated at the mean fluid temperature.
!
!If Re_D <= 2300 and the choice was made from the diagram window  to use the laminar flow model, one of  the following correlations is used.
!
!		for inner tube flow (uniform flux condition)
!		Nu# = 4.36
!
!		for inner annulus flow (uniform flux condition -- estimated from table for Nu# with heat fluxes at both surfaces)
!		D_p/D_2	Nu#
!		0		4.364
!		0.05		4.792
!		0.10		4.834
!		0.20		4.833
!		0.40		4.979
!		0.60		5.099
!		0.80		5.24
!		1.00		5.385		
!
!
!For the "SNL test platform" case the inside diameter in the above correlations is replaced with the following hydraulic diameter definition.
!
!		D_h = 4 * A_c / P = D_ao - D_ai
!
!	Where
!
!		D_h = hydraulic diameter [m]
!		A_c = flow cross sectional area [m^2]
!		P = wetted perimeter [m]
!		D_ai = inner annulus diameter [m]
!		D_ao = outer annulus diameter [m]
!
!(Sources: Incropera, F., DeWitt, D., Fundamentals of Heat and Mass Transfer, Third Edition; John Wiley and Sons, New York, 1981, pp. 489-491, 502-503. Gnielinski, V., "New Equations for Heat and Mass Transfer in Turbulent Pipe and Channel Flow," International Chemical Engineering, Vol. 16, No. 2, April 1976.)
!}

double precision function fT_2(q_12conv, T_1, T_2g, Fluid, D_2, D_p, D_h, v_1, Flow_type, IncludeLaminar)
!     Input units (  K   ,  K ,  real,  m,  m ,  m , m, m/s,  string  ,   logical     )
implicit none
real(8),intent(in):: T_1, T_2g, Fluid, v_1, D_2, D_p, D_h
real(8):: Viscosity, Specheat, Conductivity, Density, Cp_1, Cp_2, f, h_1, k_1,&
          k_2, mu_1, mu_2, Nu_D2, Pr_1, Pr_2, q_12conv, Re_D2, rho_1, DRatio, pi, Flow_type
Logical::IncludeLaminar
pi = 3.1415926
	! Thermophysical properties for HTF 
	MU_1 = Viscosity(Fluid,T_1, 0.d0)  ![kg/m-s]
	MU_2 = Viscosity(Fluid,T_2g, 0.d0)  ![kg/m-s]
	Cp_1 = Specheat(Fluid,T_1, 1.d0)*1000.  ![J/kg-K]
	Cp_2 = Specheat(Fluid,T_2g, 1.d0)*1000.  ![J/kg-K]
	k_1 = dmax1(Conductivity(Fluid,T_1, 0.d0),1.e-4)  ![W/m-K]
	k_2 = dmax1(Conductivity(Fluid,T_2g, 0.d0),1.e-4)  ![W/m-K]
	RHO_1 = Density(Fluid,T_1, 0.d0)  ![kg/m^3]

	Pr_2 = (Cp_2 * MU_2) / k_2
	Pr_1 = (Cp_1 * MU_1) / k_1

    if(v_1 > 0.1) then

	    Re_D2 = (RHO_1 * D_H * V_1) / (MU_1)

	    ! Nusselt Number for laminar flow case if option to include laminar flow model is chosen 
	    IF ((INCLUDELAMINAR == .TRUE.) .and. (Re_D2 <= 2300.)) THEN
		    If (FLOW_TYPE == 2.d0) Then
			    DRatio = D_P/D_2
			    !Estimate for uniform heat flux case (poly. regression based on lookup table in Forristall EES model)
			    !---Note that this regression is based on an 8-point table, and is highly non-practical outside of DRatio bounds
			    !---0 and 1
			    if(DRatio > 1.) then
			        Nu_D2 = 5.385
			    elseif(DRatio < 0.) then
			        Nu_D2 = 4.364
			    else
			        Nu_D2 = 41.402*DRatio*DRatio*DRatio*DRatio*DRatio - 109.702*DRatio*DRatio*DRatio*DRatio + 104.570*DRatio*DRatio*DRatio - 42.979*DRatio*DRatio + 7.686*DRatio + 4.411
			    endif
		    Else
			    Nu_D2 = 4.36				!uniform heat flux
		    EndIf
	    Else

		    ! Warning statements if turbulent/transitional flow Nusselt Number correlation is used out of recommended range 
    !		IF (Pr_1 <= 0.5) or (2000 <= Pr_1) THEN CALL WARNING('The result may not be accurate, since 0.5 < Pr_1 < 2000 does not hold. See PROCEDURE Pq_12conv. Pr_1 = XXXA1', Pr_1)
    !		IF (Pr_2 <= 0.5) or (2000 <= Pr_2) THEN CALL WARNING('The result may not be accurate, since 0.5 < Pr_2 < 2000 does not hold. See PROCEDURE Pq_12conv. Pr_2 = XXXA1', Pr_2)
    !		If ( Re_D2 <= (2300) ) or (5*10**6 <= Re_D2 ) Then CALL WARNING('The result may not be accurate, since 2300 < Re_D2 < (5 * 10**6) does not hold. See PROCEDURE Pq_12conv. Re_D2 = XXXA1', Re_D2)

		    ! Turbulent/transitional flow Nusselt Number correlation (modified Gnielinski correlation) 	
		    f = (1.82 * LOG10(Re_D2) - 1.64)**(-2)	
		    Nu_D2 = (f / 8.) * (Re_D2 - 1000.) * Pr_1 / (1. + 12.7 * (f / 8.)**(0.5) * (Pr_1**(0.6667) -1.)) * (Pr_1 / Pr_2)**0.11
	    EndIf

	    h_1 = Nu_D2 * k_1 / D_H  ![W/m**2-K]!
	    fT_2 = T_1 + q_12conv/(h_1*D_2*pi)
	    !q_12conv = h_1 * D_2 * PI  * (T_2 - T_1ave)  ![W/m]!
	
	else
	
	    h_1 = 0.0001d0
	    fT_2 = T_1
	
	endif

End



!"******************************************************************************************************************************
! 	FUNCTION fq_34conv :	Convective heat transfer rate between the absorber outer surface and the glazing inner surface
!******************************************************************************************************************************"
!  NOTE: Temperatures input in terms of degrees K
!
!  Author: R.E. Forristall (2003, EES)
!  Implemented and revised:  M.J. Wagner (10/2009)
!                Copyright:  National Renewable Energy Lab (Golden, CO) 2009
!
!{ Four cases:
!
!	1. Vacuum in annulus: free-molecular heat transfer model for an annulus.
!	2. Low or lost vacuum: natural convection heat transfer model for an annulus.
!	3. No glazing, no wind: natural convection heat transfer model for a horizontal cylinder.
!	4. No glazing, with wind: forced convection heat transfer model for a horizontal cylinder.
!
!
!Case 1:
!	
!	Free-molecular heat transfer for an annular space between horizontal cylinders.
!
!		q' = D_i * PI * h * (T_i - T_o)		
!		h = k_gas / (D_i / 2 * ln(D_o / D_i) + b * Lambda * (D_i / D_o + 1))
!		b = (2 - a) / a * (9 * Gamma - 5) / (2 * (Gamma + 1))
!		Lambda = 2.331 * 10^(-20) * T_avg / (P * Delta^2)
!
!	Where
!
!		q' = convection heat transfer rate per unit length [W/m]
!		D_i = outer absorber diameter [m]
!		D_o = inner glazing diameter [m]
!		h = convection heat transfer coefficient for annulus gas [W/m^2-K]
!		T_i = outer absorber surface temperature [C]
!		T_o = inner glazing surface temperature [C]
!		k_gas = thermal conductivity of the annulus fluid at standard temperature and pressure [W/m^2-K]
!		b = interaction coefficient [dimensionless]
!		Lambda = mean-free-path between collisions of a molecule [cm]
!		a = accommodation coefficient [dimensionless]
!		Gamma = ratio of specific heats for the annulus fluid [dimensionless]
!		T_avg = average temperature of the annulus fluid [K]
!		P = pressure of the annulus gas [mm of Hg]
!		Delta = molecular diameter of the annulus gas [cm]
!
!	The above correlation is valid for Ra_Do < (D_o / (D_o -D_i))^4, but may over estimate q' slightly for large vacuums.
!
!(Source: Ratzel, A., Hickox, C., Gartling, D., "Techniques for Reducing Thermal Conduction and Natural Convection Heat Losses 
! in Annular Receiver Geometries," Journal of Heat Transfer, Vol. 101, No. 1, February 1979; pp. 108-113)
!
!
!Case 2:
!	
!	Modified Raithby and Hollands correlation for natural convection in an annular space between horizontal cylinders.
!
!		q' = 2.425 * k * (T_i - T_o) / (1 + (D_i / D_o)^(3/5))^(5/4) * (Pr * Ra_Di / (0.861 + Pr))^(1/4)
!		Pr = NU / Alpha
!		Ra_Di = g * Beta * (T_i - T_o) * (D_i)^3 / (Alpha * NU)
!		Beta = 1 / T_avg		"Ideal Gas"
!
!	Where
!
!		k = conduction heat transfer coefficient for the annulus gas [W/m-K]
!		Pr = Prandtl number 
!		NU = kinematic viscosity [m^2/s]
!		Alpha = thermal diffusivity [m^2/s]
!		Ra_Di = Rayleigh number based on the annulus inner diameter
!		g = local acceleration due to gravity [m/s^2]
!		Beta = volumetric thermal expansion coefficient [1/K]
!		Rho_o = annulus gas density at the outer surface [kg/m^3]
!		Rho_i = annulus gas density at the inner surface [kg/m^3]
!		T_avg = average temperature, (T_i + T_o) / 2 [K]
!
!	Above correlation is valid for Ra_Do > (D_o / (D_o -D_i))^4. All physical properties are evaluated at the average temperature, (T_i + T_o)/2.
!
!(Source: Bejan, A., Convection Heat Transfer, Second Edition; John Wiley & Son's, New York, 1995, pp. 257-259.)
!
!
!Case 3:
!
!	Churchill and Chu correlation for natural convection from a long isothermal horizontal cylinder.
!
!		Nu_bar = (0.60 + (0.387 * Ra_D^(1/6)) / (1 + (0.559 / Pr)^(9/16))^(8/27) )^2
!		Ra_D = g * Beta * (T_s - T_inf) * D^3 / (Alpha * NU)
!		Beta =  1 / T_f	"Ideal Gas"
!		Alpha = k / (Cp * Rho)
!		Pr = NU / Alpha
!
!		h = Nu_bar * k / D
!
!		q' = h * PI * D * (T_s - T_inf)
!
!	Where
!
!		Nu_bar = average Nusselt number
!		Ra_D = Rayleigh number based on diameter
!		Rho = fluid density  [kg/m^3]
!		Cp = specific heat at constant pressure [kJ / kg-K]
!		T_inf = fluid temperature in the free stream [C]
!		T_s = surface temperature [C]
!		T_f = film temperature, (T_s + T_inf) / 2 [K]
!		T_inf = ambient air temperature [C]
!
!	Above correlation is valid for  10^(-5) < Ra_D < 10^12. All physical properties are evaluated at the film temperature, (T_s + T_inf) / 2.
!
!(Source: Incropera, F., DeWitt, D., Fundamentals of Heat and Mass Transfer, Third Edition; John Wiley and Sons, New York, 1981, pp. 550-552.)
!
!
!Case 4:
!
!	Zhukauskas's correlation for external forced convection flow normal to an isothermal cylinder.
!
! 		Nu_bar = C * Re_D^m * Pr^n * (Pr / Pr_s)^(1/4)
!
!		Re_D		C			m
!		1-40		0.75			0.4
!		40-1000		0.51			0.5
!		1e3- 2e5	0.26			0.6
!		2e5-1e6	0.076			0.7
!
!		n = 0.37, Pr <=10
!		n = 0.36, Pr >10
!
! 		Re_D =  U_inf * D / NU
!		Pr  = NU / Alpha
!		Alpha = k / (Cp * Rho)
!
!		Q =  h * D * PI * (T_s - T_inf) * L
!
!	Where,
!
!		Re_D = Reynolds number evaluated at the diameter
!		Cp = specific heat at constant pressure of air [W/m-K]
!		Rho = density of air [kg/m^3]
!		C, m, n = constants
!
!	Above correlation is valid for  0.7 < Pr < 500, and 1 < Re_D < 10^6. All physical properties evaluated 
!   at the free stream temperature, T_inf,  except Pr_s.
!
!(Source: Incropera, F., DeWitt, D., Fundamentals of Heat and Mass Transfer, Third Edition; John Wiley and 
! Sons, New York, 1981, p. 413.)
!}
subroutine FQ_34CONV(T_3,T_4, D_3, D_4, P_a, P_6, v_6, T_6, annulusGas, glazingIntact, q_34conv, h_34)
   !      UNITS   ( K , K ,  m ,  m , m,torr, Pa , m/s,  K , real{1,26,27}, logical  ,   W/m   , W/m2-K)
implicit none

real(8),intent(in)::T_3, T_4, P_a, P_6, v_6, T_6, annulusGas
real(8)::D_3, D_4
real(8):: a, alpha_34, b, beta_34, c, C1, Cp_34, Cv_34, delta, gamma, Kineticq_34_conv, k_34, lambda, &
          m, mu_34, n, Natq_34_conv, nu_34, P, Pr_34, P_A1, Ra_D3, Ra_D4, rho_34, T_34, T_36, &
          density, viscosity, cp, conductivity, grav, Nu_bar,pi, rho_3, rho_6, mu_36, rho_36, cp_36,&
          specheat, k_36, nu_36, alpha_36, beta_36, Pr_36, H_36, mu_3, mu_6, k_3, k_6, cp_3, cp_6, nu_6, nu_3,&
          alpha_3, alpha_6, Re_d3, pr_3, pr_6, Cv, Natq_34CONV, KineticQ_34CONV
real(8),intent(out)::q_34conv,h_34          
Logical:: glazingIntact

grav = 9.81 !m/s2  gravitation constant
pi = 3.1415926

P_A1 = P_A * 133.322368  !convert("torr", "Pa")  ![Pa]

	T_34 = (T_3 + T_4) / 2.  ![C]
	T_36 = (T_3 + T_6) / 2.  ![C]

	If (GLAZINGINTACT .eq. .FALSE.) Then
		
		! Thermophysical Properties for air 
		Rho_3 = DENSITY(1.d0, T_3, P_6)  ![kg/m**3], air is fluid 1.
		Rho_6 = DENSITY(1.d0, T_6, P_6)  ![kg/m**3], air is fluid 1.

		If (V_6 <= 0.1) Then
			MU_36 = VISCOSITY(1.d0, T_36, 0.d0)  ![N-s/m**2], AIR
			Rho_36 = DENSITY(1.d0, T_36, P_6)  ![kg/m**3], AIR
			Cp_36 = specheat(1.d0, T_36, 1.d0)*1000.  ![J/kg-K], AIR
			k_36 = CONDUCTIVITY(1.d0, T_36, 0.d0)  ![W/m-K], AIR
			NU_36 = MU_36 / Rho_36  ![m**2/s] kinematic viscosity, AIR
			Alpha_36 = k_36 / (Cp_36 * Rho_36)  ![m**2/s], thermal diffusivity, AIR
			Beta_36 =  1.d0 / T_36  ![1/K]
			Ra_D3 = grav * Beta_36 * ABS(T_3 - T_6) * D_3*D_3*D_3 / (Alpha_36 * NU_36)

			! Warning Statement if following Nusselt Number correlation is used out of recommended range !
			!If ((Ra_D3 <= 1.e-5) .or. (Ra_D3 >= 1.e12)) continue
			    !CALL WARNING('The result may not be accurate, since 10**(-5) < Ra_D3 < 10**12 does not hold. See Function fq_34conv. Ra_D3 = XXXA1', Ra_D3)

			! Churchill and Chu correlation for natural convection from a long isothermal horizontal cylinder !
			Pr_36 = NU_36 / Alpha_36
			Nu_bar = (0.60 + (0.387 * Ra_D3**(0.1667)) / (1. + (0.559 / Pr_36)**(0.5625))**(0.2963) )**2
			h_36 = Nu_bar * k_36 / D_3  ![W/m**2-K]!
			Q_34CONV = h_36 * pi * D_3 * (T_3 - T_6)  ![W/m]!
			h_34 = h_36  !Set output coefficient

		Else

			! Thermophysical Properties for air 
			MU_3 = VISCOSITY(1.d0, T_3, 0.d0)  ![N-s/m**2]
			MU_6 = VISCOSITY(1.d0, T_6, 0.d0)  ![N-s/m**2]
			k_3 = CONDUCTIVITY(1.d0, T_3, 0.d0)  ![W/m-K]
			k_6 = CONDUCTIVITY(1.d0, T_6, 0.d0)  ![W/m-K]
			Cp_3 = specheat(1.d0, T_3, 1.d0)*1000.  ![J/kg-K]
			Cp_6 = specheat(1.d0, T_6, 1.d0)*1000.  ![J/kg-K]
			NU_6 = MU_6 / Rho_6  ![m**2/s]
			NU_3 = MU_3 / Rho_3  ![m**2/s]
			Alpha_3 = k_3 / (Cp_3 * Rho_3)  ![m**2/s]
			Alpha_6 = k_6 / (Cp_6 * Rho_6)  ![m**2/s]
			Re_D3 = V_6 * D_3 / NU_6
			Pr_3 = NU_3 / Alpha_3
			Pr_6 = NU_6 / Alpha_6

			! Warning Statements if following Nusselt Number correlation is used out of range !
			!IF (Re_D3 <= 1) or (Re_D3 >= 10**6) THEN CALL WARNING('The result may not be accurate, since 1 < Re_D3 < 10**6 does not hold. See Function fq_34conv. Re_D3 = XXXA1', Re_D3)
			!If (Pr_6 <= 0.7) or (Pr_6 >= 500) Then CALL WARNING('The result may not be accurate, since 0.7 < Pr_6 < 500 does not hold. See Function fq_34conv. Pr_6 = XXXA1', Pr_6)

			! Coefficients for external forced convection Nusselt Number correlation (Zhukauskas's correlation) !
			If (Pr_6 <= 10) Then
				n = 0.37
			Else
				n = 0.36
			EndIf

			If (Re_D3 < 40) Then
				C = 0.75
				m = 0.4	
			Else

				If ((40 <= Re_D3) .and. (Re_D3 < 1000.)) Then
					C = 0.51
					m = 0.5
				Else
					If ((1.e3 <= Re_D3) .and. (Re_D3 < 2.e5)) Then
						C = 0.26
						m = 0.6
					Else
						If ((2.e5 <= Re_D3) .and. (Re_D3 < 1.e6)) Then
							C = 0.076
							m = 0.7
						EndIf
					EndIf
				EndIf
			EndIf

			! Zhukauskas's correlation for external forced convection flow normal to an isothermal cylinder 
			Nu_bar = C * (Re_D3)**m  * (Pr_6)**n * (Pr_6 / Pr_3)**(0.25)
			h_36 = Nu_bar  *  k_6  /  D_3  ![W/m**2-K]
			Q_34CONV =  h_36  *  D_3  *  PI  *  (T_3 - T_6)  ![W/m]	
			h_34 = h_36  !set output coefficient
		EndIf
	Else

		! Thermophysical Properties for gas in annulus space 
		MU_34 = Viscosity(annulusGas, T_34, 0.d0)  ![kg/m-s] 
		Cp_34 = Specheat(annulusGas, T_34, 1.d0)*1000.  ![J/kg-K]
		Cv_34 = Cv(annulusGas, T_34)*1000.  ![J/kg-K]
		Rho_34 = density(annulusGas, T_34, P_A1)  ![kg/m**3]
		k_34 = conductivity(annulusGas, T_34, 0.d0)  ![W/m-K]

		! Modified Raithby and Hollands correlation for natural convection in an annular space between horizontal cylinders 
		Alpha_34 = k_34 /(Cp_34 * Rho_34)  ![m**2/s]!
		NU_34 = MU_34 / Rho_34  ![m**2/s]!
		Beta_34 = 1. / dmax1(T_34,1.d0)  ![1/K]!
		Ra_D3 = grav * Beta_34 * ABS(T_3 - T_4) * D_3*D_3*D_3 / (Alpha_34 * NU_34)
		Ra_D4 = grav * Beta_34 * ABS(T_3 - T_4) * D_4*D_4*D_4 / (Alpha_34 * NU_34)
		Pr_34 = NU_34 / Alpha_34
		Natq_34conv = 2.425 * k_34 * (T_3 - T_4) / (1 + (D_3/ D_4)**(0.6))**(1.25) * (Pr_34 * Ra_D3 / (0.861 + Pr_34))**(0.25)  ![W/m]!	
		P = P_A  ![mmHg] (note that 1 torr = 1 mmHg by definition)
		C1 = 2.331e-20  ![mmHg-cm**3/K]!

		! Free-molecular heat transfer for an annular space between horizontal cylinders 
		If (ANNULUSGAS == 1.d0) Then  !AIR
			Delta = 3.53e-8  ![cm]
		EndIf

		If (ANNULUSGAS == 27.d0) Then  !H2
			Delta = 2.4e-8  ![cm]
		EndIf

		If (ANNULUSGAS == 26.d0) Then  !Argon
			Delta = 3.8e-8  ![cm]
		EndIf

		Lambda = C1 * T_34 / (P * Delta*Delta)  ![cm]
		Gamma = Cp_34 / Cv_34
		a = 1.
		b = (2. - a) / a * (9. * Gamma - 5.) / (2. * (Gamma + 1.))
		h_34 = k_34 / (D_3 / 2. * log(D_4 / D_3) + b * Lambda /100.* (D_3 / D_4 + 1.))  ![W/m**2-K]
		Kineticq_34conv  = D_3 * PI * h_34 * (T_3 - T_4)  ![W/m]

		! Following compares free-molecular heat transfer with natural convection heat transfer and uses the largest value for heat transfer in annulus 
		If (Kineticq_34conv > Natq_34conv) Then
			Q_34CONV = Kineticq_34conv  ![W/m]
		Else
			Q_34CONV = Natq_34conv  ![W/m]
			h_34 = Q_34CONV/(D_3*PI*(T_3-T_4))  !Recalculate the convection coefficient for natural convection
		EndIf
	EndIf

 End subroutine



!"******************************************************************************************************************************
! 	FUNCTION fq_34rad :	Radiation heat transfer rate between the absorber surface and glazing inner surface
!******************************************************************************************************************************"
!  NOTE: Temperatures input in terms of degrees K
!
!  Author: R.E. Forristall (2003, EES)
!  Implemented and revised:  M.J. Wagner (10/2009)
!                Copyright:  National Renewable Energy Lab (Golden, CO) 2009
!                   note  :  Tested against original EES version
!
!{ 	Radiation heat transfer for a two-surface enclosure.
!
!		Two cases, one if the glazing envelope is intact and one if the glazing is missing or damaged.
!
!		Case 1: Long (infinite) concentric cylinders.
!
!			q' = sigma * PI * D_1 * (T_1^4 - T_2^4) / (1 / EPSILON_1 + (1 - EPSILON_2) / EPSILON_2 * (D_1 / D_2))
!
!			Where,
!
!				q' = radiation heat transfer per unit length [W/m]
!				sigma = Stephan-Boltzmann constant [W/m^2-K^4]
!				T_1 = absorber outer surface temperature [K]
!				T_2 = glazing inner surface temperature [K]
!				D_1 = outer absorber diameter [m]
!				D_2 = inner glazing diameter [m]
!				EPSILON_1 = emissivity of inner surface
!				EPSILON_2 = emissivity of outer surface
!
!		Case 2: Small convex object in a large cavity.
!
!			q' = sigma * PI * D_1 * EPSILON_1 * (T_1^4 - T_2^4)
!}

subroutine FQ_34RAD(T_3, T_4, D_3, D_4, T_7, EPSILON_3, EPSILON_4, GlazingIntact, q_34rad, h_34)
!    Units         ( K ,  K ,  m ,  m , K  ,    -     ,    -     ,   logical    ,  W/m   , W/m2-K)
implicit none
real(8),intent(in):: D_3, D_4, T_7, EPSILON_3, EPSILON_4
real(8):: sigma, pi, T_3, T_4, T_ave
real(8),intent(out)::q_34rad, h_34
logical:: GlazingIntact
sigma=5.67e-8; pi=3.1415926
T_ave = (T_3 + T_4)/2.
	If (GLAZINGINTACT == .false.) Then
		Q_34RAD =EPSILON_3 * PI * D_3  * sigma * (T_3*T_3*T_3*T_3 - T_7*T_7*T_7*T_7)  ![W/m]
		h_34 = q_34rad/(pi*D_3*(T_3 - T_7))
	Else
		h_34 = sigma*(T_3*T_3 + T_4*T_4)*(T_3 + T_4)/ (1.d0 / EPSILON_3 + D_3 / D_4 * ( 1.d0 / EPSILON_4 - 1.d0)) 
		Q_34rad = pi* D_3 * h_34 * (T_3 - T_4)
	EndIf

END


!"******************************************************************************************************************************
! 	FUNCTION fq_56conv :	Convective heat transfer rate between the glazing outer surface and the ambient air
!******************************************************************************************************************************"
!  Author: R.E. Forristall (2003, EES)
!  Implemented and revised:  M.J. Wagner (10/2009)
!                Copyright:  National Renewable Energy Lab (Golden, CO) 2009
!                   note  :  Tested against original EES version
!
!{ 	h6	Heat Transfer Coefficient
!
!	If no wind, then the Churchill and Chu correlation is used. If wind, then the Zhukauskas's correlation is used. These correlations are described above for q_34conv.
!}

subroutine FQ_56CONV(T_5, T_6, D_5,  P_6, v_6, GlazingIntact, q_56conv, h_6)
!           units   ( K ,  K ,  m , torr, m/s,  logical     ,  W/m    , W/m2-K)

implicit none

real(8),intent(in):: T_5, T_6, D_5,  P_6, v_6
logical,intent(in):: GlazingIntact
real(8),intent(out):: q_56conv, h_6
real(8):: alpha_5, alpha_6, C, Cp_5, Cp_56, Cp_6, k_5, k_56, k_6, m, mu_5, mu_56, mu_6, n, Nus_6,&
          nu_5, nu_6, Pr_5, Pr_6, Re_D5, rho_5, rho_56, rho_6, T_56, viscosity, conductivity, density, Nu_bar,&
          nu_56, alpha_56, beta_56, Ra_D5, Pr_56, specheat
real(8),parameter:: pi=3.1415926, g = 9.81

	T_56 = (T_5 + T_6)/2.d0  ![K]

	! Thermophysical Properties for air 
	MU_5 = VISCOSITY(1.d0,T_5, 0.d0)  ![kg/m-s]
	MU_6 = VISCOSITY(1.d0,T_6, 0.d0)  ![kg/m-s]
	MU_56 = VISCOSITY(1.d0, T_56, 0.d0)  ![kg/m-s]
	k_5 = CONDUCTIVITY(1.d0,T_5, 0.d0)  ![W/m-K]
	k_6 = CONDUCTIVITY(1.d0,T_6, 0.d0)  ![W/m-K]
	k_56 = CONDUCTIVITY(1.d0, T_56, 0.d0)  ![W/m-K]
	Cp_5 = Specheat(1.d0,T_5, 1.d0)*1000.  ![J/kg-K]
	Cp_6 = Specheat(1.d0,T_6, 1.d0)*1000.  ![J/kg-K]
	Cp_56 = Specheat(1.d0, T_56, 1.d0)*1000.  ![J/kg-K]
	Rho_5 = DENSITY(1.d0,T_5, P_6)  ![kg/m^3]
	Rho_6 = DENSITY(1.d0,T_6, P_6)  ![kg/m^3]
	Rho_56 = DENSITY(1.d0, T_56, P_6)  ![kg/m^3]

	! IF the glass envelope is missing then the convection heat transfer from the glass 
	!envelope is forced to zero by T_5 = T_6 
	If (GLAZINGINTACT == .FALSE.) Then
		Q_56CONV = (T_5 - T_6)  ![W/m]
	Else
		If (V_6 <= 0.1) Then

			! Coefficients for Churchill and Chu natural convection correlation !
			NU_56 = MU_56 / Rho_56  ![m^2/s]
			Alpha_56 = k_56 / (Cp_56 * Rho_56 )  ![m^2/s]
			Beta_56 =  1.d0 / T_56  ![1/K]
			Ra_D5 = g *Beta_56 * ABS(T_5 - T_6) * D_5*D_5*D_5 / (Alpha_56 * NU_56)

			! Warning Statement if following Nusselt Number correlation is used out of range !
			!If (Ra_D5 <= 10**(-5)) or (Ra_D5 >= 10**12) Then CALL WARNING('The result may not be accurate, 
			!since 10**(-5) < Ra_D5 < 10**12 does not hold. See Function fq_56conv. Ra_D5 = XXXA1', Ra_D5)

			! Churchill and Chu correlation for natural convection for a horizontal cylinder !
			Pr_56 = NU_56 / Alpha_56
			Nu_bar = (0.60 + (0.387 * Ra_D5**(0.1667)) / (1.d0 + (0.559 / Pr_56)**(0.5625))**(0.2963) )**2
			h_6 = Nu_bar * k_56 / D_5  ![W/m**2-K]
			Q_56CONV = h_6 * PI * D_5 * (T_5 - T_6)  ![W/m]

		Else

			! Coefficients for Zhukauskas's correlation !
			Alpha_5 = k_5 / (Cp_5 * Rho_5)  ![m**2/s]
			Alpha_6 = k_6 / (Cp_6 * Rho_6)  ![m**2/s]
			NU_5 = MU_5 / Rho_5  ![m**2/s]
			NU_6 = MU_6 / Rho_6  ![m**2/s]
			Pr_5 = NU_5 / Alpha_5
			Pr_6 = NU_6 / Alpha_6
			Re_D5 = V_6 * D_5 * Rho_6 / MU_6

			! Warning Statement if following Nusselt Number correlation is used out of range !
!			IF (Pr_6 <= 0.7) or (Pr_6 >= 500) THEN CALL WARNING('The result may not be accurate, since 0.7 < Pr_6 < 500 does not hold. See Function fq_56conv. Pr_6 = XXXA1', Pr_6)
!			If (Re_D5 <= 1) or (Re_D5 >= 10**6) Then CALL WARNING('The result may not be accurate, since 1 < Re_D5 < 10**6 does not hold. See Function fq_56conv. Re_D5 = XXXA1 ', Re_D5)

			! Zhukauskas's correlation for forced convection over a long horizontal cylinder !
			If (Pr_6 <= 10) Then
				n = 0.37
			Else
				n = 0.36
			EndIf

			If (Re_D5 < 40.d0) Then
				C = 0.75
				m = 0.4	
			Else
				If ((40.d0 <= Re_D5) .and. (Re_D5 < 1.e3)) Then
					C = 0.51
					m = 0.5
				Else
					If ((1.e3 <= Re_D5) .and. (Re_D5 < 2.e5)) Then
						C = 0.26
						m = 0.6	
					Else
						If ((2.e5 <= Re_D5) .and. (Re_D5 < 1.e6)) Then
							C = 0.076
							m = 0.7
						EndIf
					EndIf
				EndIf
			EndIf

			Nus_6 = C * Re_D5**m *  Pr_6**n  *(Pr_6/Pr_5)**0.25
			h_6 = Nus_6 * k_6 / D_5  ![W/m**2-K]
			Q_56CONV = h_6 * PI * D_5 * (T_5 - T_6)  ![W/m]
		EndIf
	ENDIf
End




!"******************************************************************************************************************************
!	FUNCTION fq_cond_bracket:	Heat loss estimate through HCE support bracket
!******************************************************************************************************************************"
!  Author: R.E. Forristall (2003, EES)
!  Implemented and revised:  M.J. Wagner (10/2009)
!                Copyright:  National Renewable Energy Lab (Golden, CO) 2009
!                   note  :  Tested against original EES version
!
double precision FUNCTION FQ_COND_BRACKET(T_3, T_6, P_6, v_6)
    !           units                    ( K ,  K , bar, m/s)
implicit none

real(8),intent(in):: T_3, T_6, P_6, v_6
real(8),parameter::g=9.81, pi = 3.1415926
real(8):: P_brac, D_brac, A_CS_brac, k_brac, T_base, T_brac, T_brac6, mu_brac6, viscosity, rho_brac6, &
          Cp_brac6, density, k_brac6, nu_brac6, alpha_brac6, beta_brac6, Ra_dbrac, Pr_brac6, Nu_bar, H_brac6,&
          mu_brac, conductivity, mu_6, rho_6, rho_brac, k_6, cp_brac, specheat, nu_6, cp_6, nu_brac, alpha_brac,&
          Re_dbrac, Pr_brac, Pr_6, N, C, M, L_HCE, alpha_6


	! effective bracket perimeter for convection heat transfer
	P_brac = 0.2032  ![m]

	! effective bracket diameter (2 x 1in) 
	D_brac = 0.0508  ![m]

	! minimum bracket cross-sectional area for conduction heat transfer
	A_cs_brac = 0.00016129  ![m**2]

	! conduction coefficient for carbon steel at 600 K
	k_brac = 48.d0  ![W/m-K]

	! effective bracket base temperature
	T_base = T_3 - 10.d0  ![C]

	! estimate average bracket temperature 
	T_brac = (T_base + T_6) / 2.d0  ![C]  !NOTE: MJW modified from /3 to /2.. believed to be an error

	! estimate film temperature for support bracket 
	T_brac6 = (T_brac + T_6) /2.d0  ![C]

	! convection coefficient with and without wind
	If (V_6 <= 0.1) Then
		
		MU_brac6 = VISCOSITY(1.d0, T_brac6, 0.d0)  ![N-s/m**2]
		Rho_brac6 = DENSITY(1.d0, T_brac6, P_6)  ![kg/m**3]
		Cp_brac6 = specheat(1.d0, T_brac6, 1.d0)*1000.  ![J/kg-K]
		k_brac6 = CONDUCTIVITY(1.d0, T_brac6, 0.d0)  ![W/m-K]
		NU_brac6 = MU_brac6 / Rho_brac6  ![m**2/s]
		Alpha_brac6 = k_brac6 / (Cp_brac6 * Rho_brac6)  ![m**2/s]
		Beta_brac6 =  1.d0 / T_brac6  ![1/K]
		Ra_Dbrac = g * Beta_brac6 * ABS(T_brac - T_6) * D_brac*D_brac*D_brac / (Alpha_brac6 * NU_brac6)

		! Warning Statement if following Nusselt Number correlation is used out of recommended range 
		!If ((Ra_Dbrac <= 1.e-5)) .or. (Ra_Dbrac >= 1.e12) Then CALL WARNING('The result may not be accurate, 
		!since 10**(-5) < Ra_Dbrac < 10**12 does not hold. See Function fq_cond_bracket. Ra_Dbrac = XXXA1', Ra_Dbrac)

		! Churchill and Chu correlation for natural convection from a long isothermal horizontal cylinder 
		Pr_brac6 = NU_brac6 / Alpha_brac6
		Nu_bar = (0.60 + (0.387 * Ra_Dbrac**(0.1667)) / (1.d0 + (0.559 / Pr_brac6)**(0.5625))**(0.2963) )**2
		h_brac6 = Nu_bar * k_brac6 / D_brac  ![W/m**2-K]
			
	Else
		
		! Thermophysical Properties for air 
		MU_brac = VISCOSITY(1.d0, T_brac, 0.d0)  ![N-s/m**2]
		MU_6 = VISCOSITY(1.d0, T_6, 0.d0)  ![N-s/m**2]
		Rho_6 = DENSITY(1.d0, T_6, P_6)  ![kg/m**3]
		Rho_brac = DENSITY(1.d0, T_brac, P_6)  ![kg/m**3]
		k_brac = CONDUCTIVITY(1.d0, T_brac, 0.d0)  ![W/m-K]
		k_6 = CONDUCTIVITY(1.d0, T_6, 0.d0)  ![W/m-K]
		k_brac6 = CONDUCTIVITY(1.d0, T_brac6, 0.d0)  ![W/m-K]
		Cp_brac = specheat(1.d0, T_brac, 1.d0)*1000.  ![J/kg-K]
		Cp_6 = specheat(1.d0, T_6, 1.d0)*1000.  ![J/kg-K]
		NU_6 = MU_6 / Rho_6  ![m**2/s]
		NU_brac = MU_brac / Rho_brac  ![m**2/s]

		Alpha_brac = k_brac / (Cp_brac * Rho_brac * 1000.d0)  ![m**2/s]
		Alpha_6 = k_6 / (Cp_6 * Rho_6 * 1000.d0)  ![m**2/s]
		Re_Dbrac = V_6 * D_brac / NU_6
		Pr_brac = NU_brac / Alpha_brac
		Pr_6 = NU_6 / Alpha_6

		! Warning Statements if following Nusselt Correlation is used out of range 
!		IF (Re_Dbrac <= 1) or (Re_Dbrac >= 10**6) THEN CALL WARNING('The result may not be accurate, since 1 < Re_Dbrac < 10**6 does not hold. See Function fq_cond_bracket. Re_Dbrac = XXXA1', Re_Dbrac)
!		If (Pr_6 <= 0.7) or (Pr_6 >= 500) Then CALL WARNING('The result may not be accurate, since 0.7 < Pr_6 < 500 does not hold. See Function fq_cond_bracket. Pr_6 = XXXA1', Pr_6)

		! Coefficients for external forced convection Nusselt Number correlation (Zhukauskas's correlation) 
		If (Pr_6 <= 10.) Then
			n = 0.37
		Else
			n = 0.36
		EndIf

		If (Re_Dbrac < 40.) Then
			C = 0.75
			m = 0.4	
		Else

			If ((40. <= Re_Dbrac) .and. (Re_Dbrac< 1.e3)) Then
				C = 0.51
				m = 0.5
			Else
				If ((1.e3 <= Re_Dbrac) .and. (Re_Dbrac < 2.e5)) Then
					C = 0.26
					m = 0.6
				Else
					If ((2.e5 <= Re_Dbrac) .and. (Re_Dbrac < 1.e6)) Then
						C = 0.076
						m = 0.7
					EndIf
				EndIf
			EndIf
		EndIf

		! Zhukauskas's correlation for external forced convection flow normal to an isothermal cylinder 
		Nu_bar = C * (Re_Dbrac)**m  * (Pr_6)**n * (Pr_6 / Pr_brac)**(0.25)
		h_brac6 = Nu_bar  *  k_brac6  /  D_brac  ![W/m**2-K]
	
	EndIf

	! estimated conduction heat loss through HCE support brackets / HCE length 
	L_HCE = 4.06  ![m]
	FQ_COND_BRACKET = SQRT(h_brac6 * P_brac * k_brac * A_cs_brac) * (T_base - T_6)/L_HCE  ![W/m]

END



!"******************************************************************************************************************************
!	Subroutine pOpticalEfficiency:  Optical Efficiencies based on HCE type
!******************************************************************************************************************************"
!  Author: R.E. Forristall (2003, EES)
!  Implemented and revised:  M.J. Wagner (10/2009)
!                Copyright:  National Renewable Energy Lab (Golden, CO) 2009
!                   note  :  Tested against original EES version
!
subroutine OpticalEfficiency(OptCharType,CollectorType, reflectivity, K, Shadowing, &
                             TrackingError, GeomEffects, Rho_mirror_clean, Dirt_mirror,Dirt_HCE, Error, ColOptEff)

implicit none

integer,intent(in):: OptCharType, CollectorType
real(8),intent(inout):: reflectivity, shadowing, trackingError, GeomEffects, Rho_mirror_clean, &
                        Dirt_mirror, Dirt_HCE, Error
real(8),intent(out):: ColOptEff
real(8):: K


!Various methods for characterizing the optical performance of collectors are anticipated.  Among these will be
!the traditional SAM method of specifying the various "derate" factors or selecting these from a library.  The 
!other methods are likely going to involve calculation of an intercept factor based on input of surface character-
!istics. 

select case(OptCharType)
case(1)  !The traditional method of entering derate factors that roll up into a single value:
	If (CollectorType == 1) then   ! 'User-Defined' 
		Shadowing = dmin1(1.d0,Shadowing)
		TrackingError = dmin1(1.d0,TrackingError)
		GeomEffects = dmin1(1.d0,GeomEffects)
		Rho_mirror_clean = dmin1(1.d0,Rho_mirror_clean)
		Dirt_mirror = dmin1(1.d0,Dirt_mirror)
		Dirt_HCE = dmin1(1.d0,Dirt_HCE)
		Error = dmin1(1.d0,Error)
	EndIf

	If (CollectorType == 2) then    !'LS-2'
		Shadowing = 0.974
		TrackingError = 0.994
		GeomEffects = 0.98
		Rho_mirror_clean = 0.935
		Dirt_mirror = dmin1(1.d0,reflectivity/Rho_mirror_clean)
		Dirt_HCE = dmin1(1.d0,(1.d0+ Dirt_mirror)/2.d0)
		Error = 0.96
	EndIf

 	If ((CollectorType == 3) .or. (CollectorType == 4)) then    !'LS-3' or 'IST'
		Shadowing = 0.974
		TrackingError = 0.994
		GeomEffects = 0.98
		Rho_mirror_clean = 0.935
		Dirt_mirror = dmin1(1.d0,reflectivity/Rho_mirror_clean)
		Dirt_HCE = dmin1(1.d0,(1.d0+ Dirt_mirror)/2.d0)
		Error = 0.96
	EndIf
	
    ColOptEff = Shadowing * TrackingError * GeomEffects * Rho_mirror_clean * Dirt_mirror * Dirt_HCE * Error * K

case(2:)
    continue !reserve space for additional characterization methods here...
end select


end subroutine


!"******************************************************************************************************************************
!	FUNCTION fk_23:	Absorber conductance
!******************************************************************************************************************************"
!{ Based on linear fit of data from "Alloy Digest, Sourcebook, Stainless Steels"; ASM International, 2000.}
!
real(8) FUNCTION FK_23(T_2, T_3, AbsorberMaterial)
implicit none
real(8):: T_2, T_3, T_23, AbsorberMaterial

!Absorber materials:
! (1)   304L
! (2)   216L
! (3)   321H
! (4)   B42 Copper Pipe

	T_23 = (T_2 + T_3) / 2. - 273.15  ![C]

	If ((AbsorberMaterial == 1.) .or. (AbsorberMaterial == 2.))  Then
		FK_23 = 0.013 * T_23 + 15.2  ![W/m-K]
	EndIf

	If (AbsorberMaterial == 3.) Then
		FK_23 = 0.0153 * T_23 + 14.775  ![W/m-K]
	EndIf

	If (AbsorberMaterial == 4.) Then
		FK_23 = 400.  ![W/m-K]
	EndIf

END

!***************************************************************************************************

subroutine PipeFlow(Re, Pr, LoverD,relRough, Nusselt, f)
implicit none

!*********************************************************************
!* PipeFlow_turbulent:                                               *
!* This procedure calculates the average Nusselt number and friction *
!* factor for turbulent flow in a pipe given Reynolds number (Re),   *
!* Prandtl number (Pr), the pipe length diameter ratio (LoverD) and  *
!* the relative roughness}                                           *
!*********************************************************************

double precision,intent(in)::Re,Pr,LoverD,relRough
double precision,intent(out)::Nusselt,f
double precision::f_fd,Nusselt_L, Gz, Gm, Nusselt_T, Nusselt_H,fR,X

  !Correlation for laminar flow.. Note that no transitional effects are considered
if (Re.lt.2300.) then 
    !This procedure calculates the average Nusselt number and friction factor for laminar flow in a pipe 
    !..given Reynolds number (Re), Prandtl number (Pr), the pipe length diameter ratio (LoverD) 
    !..and the relative roughness}
    Gz=Re*Pr/LoverD	
    x=LoverD/Re
    fR=3.44/sqrt(x)+(1.25/(4*x)+16-3.44/sqrt(x))/(1+0.00021*x**(-2))
    f=4.*fR/Re
    !{f$='Shah' {Shah, R.K.  and London, A.L. "Laminar Flow Forced Convection in Ducts", 
    !..Academic Press, 1978 ,Eqn 192, p98}}
    Gm=Gz**(1./3.)
    Nusselt_T=3.66+((0.049+0.02/Pr)*Gz**1.12)/(1+0.065*Gz**0.7)
    Nusselt_H=4.36+((0.1156 +0.08569 /Pr**0.4)*Gz)/(1+0.1158*Gz**0.6) 
    !{Nusselt$='Nellis and Klein fit to Hornbeck'  {Shah, R.K.  and London, A.L. "Laminar Flow Forced Convection in Ducts",
    !..Academic Press, 1978 ,Tables  20 and 22}}
    Nusselt = Nusselt_T  !Constant temperature Nu is better approximation
    
else  !Correlation for turbulent flow
 	f_fd = (0.79*log(Re)-1.64)**(-2) !Petukhov, B.S., in Advances in Heat Transfer, Vol. 6, Irvine and Hartnett, Academic Press, 1970
    Nusselt_L= ((f_fd/8.)*(Re-1000)*Pr)/(1.+12.7*sqrt(f_fd/8.)*(Pr **(2/3.)-1.)) !Gnielinski, V.,, Int. Chem. Eng., 16, 359, 1976

    if (relRough.gt.1e-5) then

      !f=8.*((8./Re)**12+((2.457*log(1./((7./Re)**0.9+0.27*(RelRough))))**16+(37530./Re)**16)**(-1.5))**(1./12.)
      !mjw 8.30.2010 :: not used  
        
      f_fd=(-2.*log10(2*relrough/7.4-5.02*log10(2*relrough/7.4+13/Re)/Re))**(-2)
	
      Nusselt_L= ((f_fd/8.)*(Re-1000.)*Pr)/(1.+12.7*sqrt(f_fd/8.)*(Pr **(2./3.)-1.)) !Gnielinski, V.,, Int. Chem. Eng., 16, 359, 1976}
    endif
    f=f_fd*(1.+(1./LoverD)**0.7) !account for developing flow
    Nusselt= Nusselt_L*(1.+(1./LoverD)**0.7)  !account for developing flow
endif
  
99	continue

end

!***************************************************************************************************
! Trough system piping loss model
!***************************************************************************************************
!
! This piping loss model is derived from the pressure drop calculations presented in the 
! following document:
!
!   Parabolic Trough Solar System Piping Model
!   Final Report May 13, 2002 — December 31, 2004
!
!   B. Kelly
!   Nexant, Inc. San Francisco, California
!
!   D. Kearney
!   Kearney & Associates
!   Vashon, Washington
!
!   Subcontract Report
!   NREL/SR-550-40165
!   July 2006
!
! ----------------------------
! Note on use of this function
! ----------------------------
! The function returns the pressure drop across a given length of pipe, and also accounts for 
! a variety of possible pressure-loss components. This function should be called multiple times -
! once for each section under consideration.  For example, separate calls should be made for the
! HCE pressure drop, the pressure drop in each section of the header in which flow/geometrical 
! conditions vary, the section of pipe leading to the header, and so on.
!
! ----------------------------
! Inputs
! ----------------------------
! No | Name         | Description                           | Units     |  Type
!===================================================================================
!  1 | Fluid        | Number associated with fluid type     | none      | dble
!  2 | m_dot        | Mass flow rate of the fluid           | kg/s      | dble
!  3 | T            | Fluid temperature                     | K         | dble
!  4 | P            | Fluid pressure                        | Pa        | dble
!  5 | D            | Diameter of the contact surface       | m         | dble
!  6 | Rough        | Pipe roughness                        | m         | dble
!  7 | L_pipe       | Length of pipe for pressure drop      | m         | dble
!  8 | Nexp         | Number of expansions                  | none      | dble
!  9 | Ncon         | Number of contractions                | none      | dble
! 10 | Nels         | Number of standard elbows             | none      | dble
! 11 | Nelm         | Number of medium elbows               | none      | dble
! 12 | Nell         | Number of long elbows                 | none      | dble
! 13 | Ngav         | Number of gate valves                 | none      | dble
! 14 | Nglv         | Number of globe valves                | none      | dble
! 15 | Nchv         | Number of check valves                | none      | dble
! 16 | Nlw          | Number of loop weldolets              | none      | dble
! 17 | Nlcv         | Number of loop control valves         | none      | dble
! 18 | Nbja         | Number of ball joint assemblies       | none      | dble
!===================================================================================
! ----------------------------
! Outputs
! ----------------------------
! 1. PressureDrop  (Pa)

double precision function PressureDrop(Fluid,m_dot,T,P,D,Rough,L_pipe,&
                                       Nexp,Ncon,Nels,Nelm,Nell,Ngav,Nglv,Nchv,Nlw,Nlcv,Nbja)

use TrnsysFunctions

implicit none                                       
                                       
real(8),intent(in):: Fluid,m_dot,T,P,D,Rough,L_pipe,Nexp,Ncon,Nels,Nelm,Nell,Ngav,Nglv,Nchv,&
                     Nlw,Nlcv,Nbja
real(8):: rho, v_dot, mu, nu, u_fluid, Re, f, DP_pipe, DP_exp,DP_con,DP_els,DP_elm,DP_ell,DP_gav,&
          DP_glv,DP_chv,DP_lw,DP_lcv,DP_bja, density, viscosity, FricFactor,&
          HL_pm,pi,g
pi=3.1415928; g=9.80665

!Calculate fluid properties and characteristics
rho = density(fluid,T,P)
mu = viscosity(fluid,T,P)
nu = mu/rho
v_dot = m_dot/rho   !fluid volumetric flow rate
u_fluid = v_dot/(pi*(D/2.)*(D/2.))  !Fluid mean velocity

!Dimensionless numbers
Re = u_fluid*D/nu
!if(Re<2300.) then
!    f = 64./dmax1(Re,1.d0)
!else
    f = FricFactor(Rough/D,Re)
    if(ErrorFound()) return
!endif

!Calculation of pressure loss from pipe length
HL_pm = f*u_fluid*u_fluid/(2.*D*g)
DP_pipe = HL_pm*rho*g*L_pipe

!Calculation of pressure loss from Fittings
DP_exp = 0.25*rho*u_fluid*u_fluid*Nexp
DP_con = 0.25*rho*u_fluid*u_fluid*Ncon
DP_els = 0.9 * D / f * HL_pm * rho * g * Nels
DP_elm = 0.75 * D / f * HL_pm * rho * g * Nelm
DP_ell = 0.6 * D / f * HL_pm * rho * g * Nell
DP_gav = 0.19 * D / f * HL_pm * rho * g * Ngav
DP_glv = 10.0 * D / f * HL_pm * rho * g * Nglv
DP_chv = 2.5 * D / f * HL_pm * rho * g * Nchv
DP_lw = 1.8 * D / f * HL_pm * rho * g * Nlw
DP_lcv = 10.0 * D / f * HL_pm * rho * g * Nlcv
DP_bja = 8.69 * D / f * HL_pm * rho * g * Nbja

PressureDrop = sum((/DP_pipe, DP_exp,DP_con,DP_els,DP_elm,DP_ell,DP_gav,&
                   DP_glv,DP_chv,DP_lw,DP_lcv,DP_bja/))

end 


!***************************************************************************************************
! Friction factor (taken from Piping loss model)
!***************************************************************************************************
! Uses an iterative method to solve the implicit friction factor function.
! For more on this method, refer to Fox, et al., 2006 Introduction to Fluid Mechanics.
double precision function FricFactor(Rough, Reynold)

use TrnsysFunctions

implicit none

real(8),intent(in)::Rough,Reynold
real(8):: Test, TestOld, X, Xold, Slope
real(8),parameter:: Acc = .01 !0.0001
integer:: NumTries

if(Reynold < 2750.) then
    FricFactor = 64./dmax1(Reynold,1.d0)
    goto 10
endif

X = 33.33333  !1. / 0.03
TestOld = X + 2. * Log10(Rough / 3.7 + 2.51 * X / Reynold)
Xold = X
X = 28.5714  !1. / (0.03 + 0.005)
NumTries = 0

do
    NumTries = NumTries + 1
    Test = X + 2 * Log10(Rough / 3.7 + 2.51 * X / Reynold)
    If (Abs(Test - TestOld) <= Acc) Then
        FricFactor = 1. / (X * X)
        exit 
    End If

    If (NumTries > 20) Then
        call Messages(-1," Could not find friction factor solution",'Warning',0,250) 
        return
    End If

    Slope = (Test - TestOld) / (X - Xold)
    Xold = X
    TestOld = Test
    X = dmax1((Slope * X - Test) / Slope,1.e-5)
enddo

10 continue
End Function


!***************************************************************************************************

subroutine header_design(unin,nhsec,nfsec,nrunsec,rho,V_max,V_min,m_dot,D_hdr,D_runner)
use trnsysfunctions
implicit none
!---------------------------------------------------------------------------------
!--Inputs
!   * nhsec - [-] number of header sections
!   * nfsec - [-] number of field section
!   * nrunsec- [-] number of unique runner diameter sections
!   * rho   - [kg/m3] Fluid density
!   * V_max - [m/s] Maximum fluid velocity at design
!   * V_min - [m/s] Minimum fluid velocity at design
!   * m_dot - [kg/s] Mass flow rate at design
!--Outputs
!   * D_hdr - [m] An ARRAY containing the header diameter for each loop section
!   * D_runner - [m] An ARRAY containing the diameter of the runner pipe sections
!---------------------------------------------------------------------------------
integer,intent(in):: nhsec,nfsec
real(8),intent(in):: V_max, V_min, m_dot, rho, unin
real(8),intent(out):: D_hdr(nhsec), D_runner(nrunsec)
!----
integer::nst,nend, i, nd, un, nrunsec
real(8)::m_dot_max, m_dot_min, m_dot_ts, m_dot_hdr, m_dot_2loops, pi, inch, m_dot_temp,pipe_sched
logical::used
pi=3.1415926; inch=39.3700787; D_hdr(:)=0.d0

!mass flow to section is always half of total
m_dot_ts = m_dot/2.
!Mass flow into 1 header
m_dot_hdr = 2.*m_dot_ts/(dble(nfsec))
!Mass flow into the 2 loops attached to a single header section
m_dot_2loops = m_dot_hdr/dble(nhsec)

!Runner diameters
!runner pipe needs some length to go from the power block to the headers
D_runner(1) = pipe_sched(sqrt(4.*m_dot_ts/(rho*V_max*pi)))
!other runner diameters
m_dot_temp = m_dot_ts*(1.-dble(mod(nfsec,4))/dble(nfsec))  !mjw 5.4.11 Fix mass flow rate for nfsec/2==odd 
if(nrunsec>1) then
    do i=2,nrunsec
        D_runner(i) = pipe_sched(SQRT(4.*m_dot_temp/(rho*V_max*pi)))
        m_dot_temp = dmax1(m_dot_temp - m_dot_hdr*2, 0.d0)
    enddo
endif

!Calculate each section in the header
nst=1; nend = 0; nd = 0
m_dot_max = m_dot_hdr
do i=1,nhsec
    if((i==nst).and.(nd <= 10)) then 
        !If we've reached the point where a diameter adjustment must be made...
        !Also, limit the number of diameter reductions to 10
        
        nd=nd+1 !keep track of the total number of diameter sections
        !Calculate header diameter based on max velocity
        D_hdr(i)=pipe_sched(sqrt(4.*m_dot_max/(rho*V_max*pi)))
        !Determine the mass flow corresponding to the minimum velocity at design
        m_dot_min = rho*V_min*pi*D_hdr(i)*D_hdr(i)/4.
        !Determine the loop after which the current diameter calculation will no longer apply
        nend = floor((m_dot_hdr-m_dot_min)/(m_dot_2loops))  !tn 4.12.11 ceiling->floor
        !The starting loop for the next diameter section starts after the calculated ending loop
        nst = nend + 1
        !Adjust the maximum required flow rate for the next diameter section based on the previous 
        !section's outlet conditions
        m_dot_max = dmax1(m_dot_hdr - m_dot_2loops*dble(nend),0.d0)
    else
        !If we haven't yet reached the point where the minimum flow condition is acheived, just
        !set the header diameter for this loop to be equal to the last diameter
        D_hdr(i) = D_hdr(i-1)
    endif
enddo
!check for errors in pipe schedule function
!if(ErrorFound()) return
!Print the results to a file
un=int(unin)

!Write runner diam
write(un,fmt=100) V_max,V_min
100 format("Piping geometry file"//"Maximum fluid velocity: ",F5.2/"Minimum fluid velocity: ",F5.2//)
do i=1,nrunsec
    write(un,fmt=105) i, D_runner(i), D_runner(i)*inch
enddo
105 format("To section ",I2," header pipe diameter: ",F6.4,"m  (",F6.2," in)")
!Write header diams
write(un,fmt=109) 
109 format(//"Header pipe diameters for all subfields:"/&
           "Loop No. | Diameter [m] | Diameter [in] | Diam. ID"/&
           "--------------------------------------------------")
nd=1
do i=1,nhsec
    if(i>1) then
        if(D_hdr(i)/=D_hdr(i-1)) nd=nd+1
    endif
    write(un,fmt=110) i,D_hdr(i),D_hdr(i)*inch, nd
enddo
110 format(2X,I4,3X,"|",4X,F6.4,4X,"|",4X,F6.3,5X,"|",1X,I3)


end subroutine

!***************************************************************************************************

real(8) function pipe_sched(De) 

!This function takes a piping diameter "De" [m] and locates the appropriate pipe schedule
!from a list of common pipe sizes. The function always returns the pipe schedule equal to or
!immediately larger than the ideal diameter De.
!The pipe sizes are selected based on the assumption of a maximum hoop stress of 105 MPa and a total
!solar field pressure drop of 20 Bar. The sizes correspond to the pipe schedule with a wall thickness
!sufficient to match these conditions. For very large pipe diameters (above 42in), no suitable schedule 
!was found, so the largest available schedule is applied.
!Data and stress calculations were obtained from Kelly & Kearney piping model, rev. 1/2011.

use trnsysfunctions
implicit none

real(8),intent(in):: De
integer,parameter::np=25
real(8):: D_m(np), mtoinch, j
integer:: i
character:: txt*300
mtoinch = 39.3700787d0

!D_inch = (/2.5d0, 3.d0, 4.d0, 6.d0, 8.d0, 10.d0, 12.d0, 14.d0, 16.d0, 18.d0, 20.d0, 22.d0, 24.d0, 26.d0, &
!           28.d0, 30.d0, 32.d0, 34.d0, 36.d0, 42.d0, 48.d0, 54.d0, 60.d0, 66.d0, 72.d0/)

D_m = (/0.0688086d0, 0.0846836d0, 0.108204d0, 0.1614678d0, 0.206375d0, 0.26035d0, 0.31115d0, 0.3397504d0, &
        0.3905504d0, 0.43815d0, 0.48895d0, 0.5334d0, 0.5842d0, 0.635d0, 0.67945d0, 0.73025d0, 0.78105d0, &
        0.8286496d0, 0.8763d0, 1.0287d0, 1.1684d0, 1.3208d0, 1.4732d0, 1.6256d0, 1.778d0/)

!Select the smallest pipe schedule above the diameter provided
i=1
do 
    pipe_sched = D_m(i)
    if(D_m(i) >= De) exit
    if(i==np) then
        txt=''
        write(txt,fmt=100) De*mtoinch, D_m(size(D_m))*mtoinch
        100 format("No suitable pipe schedule found for this plant design. Looking for a schedule above ",F6.2,&
                    " in. Maximum schedule is ",F6.2,"in. Using the exact pipe diameter instead.")
        call messages(-1,trim(txt),"WARNING",0,250)
        exit
    endif
    i=i+1
enddo

end function

!***************************************************************************************************
real(8) function Pump_SGS(rho,m_dotsf,sm)
use trnsysfunctions
implicit none

real(8),intent(in):: rho, m_dotsf, sm
integer,parameter:: nl = 8
real(8)::L_line(nl), v_dotpb, v_dotsf, v_dot(nl), m_dotpb, D(nl), pi, pipe_sched, V(nl), vel_max, mtoinch
integer::i
pi=3.1415926
mtoinch = 39.3700787d0
!Line no.	
!1	Expansion vessel or thermal storage tank to pump suction header
!2	Individual pump suction line, from suction header to pump inlet
!3	Individual pump discharge line, from pump discharge to discharge header
!4	Pump discharge header
!5	Collector field outlet header to expansion vessel or thermal storage tank
!6	Steam generator supply header
!7	Inter steam generator piping
!8	Steam generator exit header to expansion vessel or thermal storage
!Assume standard lengths for each line [m] (Kelly & Kearney)
!Assume 3 pumps at 50% each. #3) 3*30. 
L_line = (/0.d0, 0.d0, 90.d0, 100.d0, 120.d0, 80.d0, 120.d0, 80.d0/)

!Assume a maximum HTF velocity of 1.85 m/s (based on average from Kelly & Kearney model
vel_max = 1.85d0

!design-point vol. flow rate m3/s
m_dotpb = m_dotsf/sm
v_dotpb = m_dotpb/rho
v_dotsf = m_dotsf/rho

!Set the volumetric flow rate for each line.
V_dot(1) = v_dotsf
V_dot(2:3) = v_dotsf/2.d0
V_dot(4:5) = v_dotsf
V_dot(6:8) = v_dotpb

!for each line..
pump_sgs = 0.d0
do i=1,nl
    !Calculate the pipe diameter
    D(i) = pipe_sched(sqrt(4.d0*V_dot(i)/(vel_max*pi)))
    !Calculate the total volume
    V(i) = D(i)**2/4.*pi*L_line(i)
enddo

pump_sgs = sum(V)


end function

!***************************************************************************************************

double precision function myconvert(units1,units2)
implicit none

!NOTE: THIS FUNCTION IS NOT OPTIMIZED AND CAUSES PROGRAMS TO RUN SLOWLY

!This function takes as an input two text strings and outputs the 
!factor by which the two units differ (a real number)
character(*),intent(in)::units1,units2
character(len=10)::units(2)
integer::i
double precision::factor(2),diff(2)
factor=1 ; diff=0  !initialize variables to default values
units(1)=units1
units(2)=units2

!get unit in terms of its base units
do i=1,2
!Length --> m
  if(trim(units(i)).eq.'m') then
    factor(i)=1 ; diff(i)=0
  elseif(trim(units(i)).eq.'cm') then
    factor(i)=1./100. ; diff(i)=0
  elseif(trim(units(i)).eq.'mm') then
    factor(i)=1./1000. ;diff(i)=0
  elseif(trim(units(i)).eq.'in') then
    factor(i)=.0254 ; diff(i)=0
  endif
!velocity --> m/s
  if(trim(units(i)).eq.'mph') then
    factor(i)=.44704 ; diff(i)=0
  elseif(trim(units(i)).eq.'m/s') then
    factor(i)=1 ; diff(i)=0
  elseif(trim(units(i)).eq.'km/s') then
    factor(i)=1./1000. ; diff(i)=0
  elseif(trim(units(i)).eq.'km/hr') then
    factor(i)=.27777777778 ; diff(i)=0
  endif
!time --> s
  if(trim(units(i)).eq.'hr') then
    factor(i)=3600. ; diff(i)=0
  elseif(trim(units(i)).eq.'min') then
    factor(i)=60. ; diff(i)=0
  elseif(trim(units(i)).eq.'day') then
    factor(i)= 86400. ; diff(i)=0
  elseif(trim(units(i)).eq.'year') then
    factor(i)=31556926. ; diff(i)=0
  elseif(trim(units(i)).eq.'s') then
    factor(i)=1. ; diff(i)=0
  endif
!energy --> J
  if(trim(units(i)).eq.'kJ') then
    factor(i)=1000. ; diff(i)=0
  elseif(trim(units(i)).eq.'J') then
    factor(i)=1. ; diff(i)=0
  elseif(trim(units(i)).eq.'MJ') then
    factor(i)=1.0e6 ; diff(i)=0
  elseif(trim(units(i)).eq.'GJ') then
    factor(i)=1.0e9 ; diff(i)=0
  elseif(trim(units(i)).eq.'MW-hr') then
    factor(i)=3.6e9 ; diff(i)=0
  elseif(trim(units(i)).eq.'W-hr') then
    factor(i)=3600. ; diff(i)=0
  elseif(trim(units(i)).eq.'kW-hr') then
    factor(i)=3.6e6 ; diff(i)=0
  endif
!energy rate --> W (J/s)
  if(trim(units(i)).eq.'kW') then
    factor(i)=1000. ; diff(i)=0
  elseif(trim(units(i)).eq.'W') then
    factor(i)=1. ; diff(i)=0
  elseif(trim(units(i)).eq.'MW') then
    factor(i)=1.0e6 ; diff(i)=0
  elseif(trim(units(i)).eq.'GW') then
    factor(i)=1.0e9 ; diff(i)=0
  elseif(trim(units(i)).eq.'hp') then     !horsepower
    factor(i)=745.699872 ; diff(i)=0
  endif
!pressure --> Pa
  if(trim(units(i)).eq.'kPa') then
    factor(i)=1000. ; diff(i)=0
  elseif(trim(units(i)).eq.'Pa') then
    factor(i)=1. ; diff(i)=0
  elseif(trim(units(i)).eq.'MPa') then
    factor(i)=1.0e6 ; diff(i)=0
  elseif(trim(units(i)).eq.'GPa') then
    factor(i)=1.0e9 ; diff(i)=0
  elseif(trim(units(i)).eq.'psi') then
    factor(i)=6894.75729 ; diff(i)=0
  elseif(trim(units(i)).eq.'bar') then
    factor(i)=1.e5 ; diff(i)=0
  elseif(trim(units(i)).eq.'millibar') then
    factor(i)=100. ; diff(i)=0
  elseif(trim(units(i)).eq.'mbar') then     !also millibar
    factor(i)=100. ; diff(i)=0
  elseif(trim(units(i)).eq.'atm') then
    factor(i)=101325. ; diff(i)=0
  elseif(trim(units(i)).eq.'torr') then
    factor(i)=133.322368 ; diff(i)=0
  elseif(trim(units(i)).eq.'mmHg') then
    factor(i)=133.322368 ; diff(i)=0
  endif
!viscosity --> Pa-s (kg/m-s)
  if(trim(units(i)).eq.'kPa-s') then
    factor(i)=1000. ; diff(i)=0
  elseif(trim(units(i)).eq.'Pa-s') then
    factor(i)=1. ; diff(i)=0
  elseif(trim(units(i)).eq.'cP') then
    factor(i)=1./1000. ; diff(i)=0
  endif
 !volumetric flow rate --> m3/s
  if(trim(units(i)).eq.'m3/s') then
    factor(i)=1. ; diff(i)=0.
  elseif(trim(units(i)).eq.'m^3/s') then
    factor(i)=1. ; diff(i)=0.
  elseif(trim(units(i)).eq.'gpm') then
    factor(i)= 63.09020E-6 ; diff(i)=0
  elseif(trim(units(i)).eq.'gal/min') then
    factor(i)= 63.09020e-6 ; diff(i)=0
  elseif(trim(units(i)).eq.'gph') then
    factor(i)=1.05150E-06 ; diff(i)=0
  elseif(trim(units(i)).eq.'gal/hr') then
    factor(i)=1.05150e-06 ; diff(i)=0
  elseif(trim(units(i)).eq.'m3/hr') then
    factor(i)=277.77800E-6 ; diff(i)=0
  elseif(trim(units(i)).eq.'m^3/hr') then
    factor(i)=277.77800E-6 ; diff(i)=0
  endif
 !angle -> rad
  if(trim(units(i)).eq.'rad') then
    factor(i) = 1. ; diff(i) = 0
  elseif(trim(units(i)).eq.'deg') then
    factor(i) = 0.0174532925 ; diff(i) = 0
  elseif(trim(units(i)).eq.'mrad') then
    factor(i) = 0.001 ; diff(i) = 0
  endif
enddo

myconvert=factor(1)/factor(2)
end function

!**********************************************************************

double precision function converttemp(units1,units2,value)
character(len=*),intent(in)::units1,units2
character(len=10)::units(2)
double precision,intent(in)::value
double precision::factor(2),diff(2)
integer::i
factor=1 ; diff=0
units(1)=units1
units(2)=units2


!temperature --> C
do i=1,2
  if(trim(units(i)).eq.'K') then
    factor(i)=1 ; diff(i)=-273.15
  elseif(trim(units(i)).eq.'C') then
    factor(i)=1 ; diff(i)=0
  elseif(trim(units(i)).eq.'F') then
    factor(i)=5./9. ; diff(i)=-32.
  elseif(trim(units(i)).eq.'R') then
    factor(i)=5./9. ; diff(i)=-491.69
  endif
enddo
converttemp= (value + diff(1))*factor(1)/factor(2) - diff(2)

end function
!**********************************************************************

double precision function skytemp(T_amb,T_dp,hour)
implicit none

!**********************************************************************
!  This function uses the correlation for Sky Temperature             *
!  that was provided in Duffie & Beckman (2006), and was              *
!  also implemented in EES.                                           *
!                                                                     *
!  This function takes as inputs:                                     *
!    - T_amb -> ambient air temperature, dry bulb [K]                 *
!    - T_dp  -> the ambient dewpoint temperature [K]                  *
!    - hour  -> the hour, in solar time, with noon as 0               *
!  The function outputs:                                              *
!    - skytemp -> the effective temperature of the sky, in degrees [K]*
!                                                                     *
!**********************************************************************

double precision,intent(in)::T_amb,T_dp,hour
double precision::T_sky,T_ambC,T_dpC,T_skyC,time
double precision,parameter::pi=3.1415

!express "time" in terms of an angle  [rad]
time = (180.-hour*15.)*pi/180.

!The inputs are in terms of degrees K, but the dewpoint temperature is in terms of degrees C
T_dpC = T_dp-273.15

!The sky temperature relationship
skytemp = (T_amb*(.711+.0056*T_dpC+.000073*T_dpC*T_dpC+.013*cos(time))**.25)


end function


!#################################################################################################################
!#################################################################################################################
!#################################################################################################################