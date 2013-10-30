SUBROUTINE TYPE265 (TIME,XIN,OUT,T,DTDT,PAR,INFO,ICNTRL,*)

!************************************************************************
! Object: External Receiver/Tower w/ STEAM HTF
! Simulation Studio Model: Type261 
! 
! Author: Ty Neises
! Date:	 August, 2011 
! Last modified: ----
! COPYRIGHT 2011 NATIONAL RENEWABLE ENERGY LABORATORY
 
! Doc. tables updated 2011-10-27 - TN
!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Parameters
!    1| NUMTOU                           | Number of time-of-use periods                                     | none             | none             
!    2| fossil_mode                      | The fossil fill operation strategy mode                           | none             | none             
!    3| q_pb_design                      | Heat rate into powerblock at design                               | MW               | W                
!    4| q_aux_max                        | Maximum heat rate of auxiliary heater                             | MW               | W                
!    5| LHV_eff                          | Aux Heater lower heating value efficiency                         | none             | none             
!    6| THT                              | Tower Height                                                      | m                | m                
!    7| N_panels                         | Number of panels                                                  | none             | none             
!    8| D_rec                            | Diameter of Receiver                                              | m                | m                
!    9| q_rec_des                        | Design-point thermal power                                        | MW               | W                
!   10| f_rec_min                        | Minimum receiver absorbed power fraction                          | none             | none             
!   11| rec_qf_delay                     | Receiver start-up delay fraction of thermal energy of receiver running at design for 1 hour| none             | none             
!   12| rec_su_delay                     | Receiver start-up delay time                                      | hr               | hr               
!   13| f_pb_cutoff                      | Cycle cut-off fraction                                            | none             | none             
!   14| f_pb_sb                          | Cycle minimum standby fraction                                    | none             | none             
!   15| T_standby_ini                    | Power block standby time                                          | hr               | hr               
!   16| x_b_target                       | Target boiler outlet quality                                      | none             | none             
!   17| eta_rec_pump                     | Feedwater pump efficiency                                         | none             | none             
!   18| P_HP_in_des                      | Design HP Turbine Inlet Pressure                                  | bar              | kPa              
!   19| P_HP_out_des                     | Design HP Turbine Outlet Pressure                                 | bar              | kPa              
!   20| f_mdotrh_des                     | Design reheat mass flow rate fraction                             | none             | none             
!   21| P_cycle_design                   | Design Cycle Power                                                | MW               | kW               
!   22| CT                               | Cooling Type                                                      | none             | none             
!   23| T_amb_des                        | Design ambient temperature (power cycle)                          | C                | K                
!   24| dT_cw_ref                        | Reference condenser water dT                                      | C/K              | C/K              
!   25| T_approach                       | Approach temperature for wet cooling                              | C/K              | C/K              
!   26| T_ITD_des                        | Approach temperature for dry cooling                              | C/K              | C/K              
!   27| hl_ffact                         | Heat Loss Fudge FACTor                                            | none             | none             
!   28| PAR_b(4)                         | Height of boiler                                                  | m                | m                
!   29| PAR_b(5)                         | O.D. of boiler tubes                                              | m                | m                
!   30| PAR_b(6)                         | Thickness of boiler tubes                                         | m                | m                
!   31| PAR_b(7)                         | Emissivity of boiler tubes                                        | none             | none             
!   32| PAR_b(8)                         | Absorptance of boiler tubes                                       | none             | none             
!   33| PAR_b(9)                         | Numerical code for tube material                                  | none             | none             
!   34| PAR_b(10)                        | Thickness of "fin"                                                | m                | m                
!   35| PAR_b(11)                        | Length of "fin" (distance between boiler tubes)                   | m                | m                
!   36| PAR_b(12)                        | Emissivity of "fin"                                               | none             | none             
!   37| PAR_b(13)                        | Absorptance of "fin"                                              | none             | none             
!   38| PAR_b(14)                        | Numerical code for "fin" material                                 | none             | none             
!   39| PAR_sh(4)                        | Height of superheater                                             | m                | m                
!   40| PAR_sh(5)                        | O.D. of superheater tubes                                         | m                | m                
!   41| PAR_sh(6)                        | Thickness of superheater tubes                                    | m                | m                
!   42| PAR_sh(7)                        | Emissivity of superheater tubes                                   | none             | none             
!   43| PAR_sh(8)                        | Absorptance of superheater tubes                                  | none             | none             
!   44| PAR_sh(9)                        | Numerical code for superheater material                           | none             | none             
!   45| PAR_sh(10)                       | Target superheater outlet temperature                             | C                | K                
!   46| PAR_rh(4)                        | Height of reheater                                                | m                | m                
!   47| PAR_rh(5)                        | O.D. of reheater tubes                                            | m                | m                
!   48| PAR_rh(6)                        | Thickness of reheater tubes                                       | m                | m                
!   49| PAR_rh(7)                        | Emissivity of reheater tubes                                      | none             | none             
!   50| PAR_rh(8)                        | Absorptance of reheater tubes                                     | none             | none             
!   51| PAR_rh(9)                        | Numerical code for reheater material                              | none             | none             
!   52| PAR_rh(10)                       | Target reheater outlet temperature                                | C                | K                
!   53| LU_Flux                          | Logical unit for input flux file                                  | none             | none             
!   54| tfrac                            | Dispatch logic - Turbine load fraction                            | none             | none             
! ...... Loop for 1..(Number of TOU periods) .......
!   55| ffrac                            | Fossil dispatch logic                                             | none             | none             
! ......//

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Inputs
!    1| azimuth                          | Solar azimuth angle                                               | deg              | deg              
!    2| zenith                           | Solar zenith angle                                                | deg              | deg              
!    3| I_bn                             | Beam normal irradiance                                            | kJ/m^2-hr        | W/m^2            
!    4| T_amb                            | Dry bulb temperature                                              | C                | K                
!    5| v_wind_g                         | Wind speed                                                        | m/s              | m/s              
!    6| P_atm                            | Ambient pressure                                                  | atm              | Pa               
!    7| hour                             | Hour of the day                                                   | none             | none             
!    8| T_dp                             | Dew point temperature                                             | C                | K                
!    9| TOUPeriod                        | Time of Use Period                                                | none             | none             
!   10| field_eff                        | Heliostat field efficiency                                        | none             | none             
!   11| P_b_in                           | Boiler inlet pressure                                             | bar              | kPa              
!   12| f_mdot_rh                        | Reheat mass flow rate fraction                                    | none             | none             
!   13| P_HP_out                         | HP turbine outlet pressure                                        | bar              | kPa              
!   14| T_HP_out                         | HP turbine outlet temperature                                     | C                | K                
!   15| T_rh_target                      | Target reheater outlet temperature                                | C                | K                
!   16| T_fw                             | Feedwater outlet temperature                                      | C                | K                
!   17| P_cond                           | Condenser pressure                                                | Pa               | Pa               

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Outputs
!    1| T_fw                             | Feedwater outlet temperature                                      | C                | K                
!    2| XOUT_b(14)                       | Boiler inlet temperature                                          | C                | K                
!    3| T_boil                           | Boiler temperature (= recirc temp, steam drum temp)               | C                | K                
!    4| P_b_in                           | Boiler inlet pressure                                             | kPa              | kPa              
!    5| XOUT_b(13)                       | Boiler outlet pressure                                            | kPa              | kPa              
!    6| dp_b                             | Pressure drop through boiler                                      | Pa               | Pa               
!    7| XOUT_b(1)                        | Mass flow rate through boiler                                     | kg/hr            | kg/s             
!    8| XOUT_b(11)                       | Maximum outlet quality of boiler flow paths                       | none             | none             
!    9| XOUT_b(12)                       | Minimum outlet quality of boiler flow paths                       | none             | none             
!   10| eta_b                            | Efficiency of boiler                                              | none             | none             
!   11| XOUT_b(19)                       | Boiler convective losses                                          | MW               | MW               
!   12| XOUT_b(20)                       | Boiler radiative losses                                           | MW               | MW               
!   13| XOUT_b(21)                       | Energy rate absorbed by boiler                                    | MW               | MW               
!   14| XOUT_b(9)                        | Efficiency of fin (if applicable)                                 | none             | none             
!   15| XOUT_b(6)                        | Max fin temperature (if applicable)                               | C                | K                
!   16| XOUT_b(5)                        | Max boiler surface temperature                                    | C                | K                
!   17| m_dot_sh                         | Mass flow rate through superheater                                | kg/hr            | kg/s             
!   18| XOUT_sh(3)                       | Outlet pressure of superheater                                    | kPa              | Pa               
!   19| DPSH                             | Pressure drop through superheater                                 | Pa               | Pa               
!   20| XOUT_sh(1)                       | Efficiency of superheater                                         | none             | none             
!   21| XOUT_sh(13)                      | Superheater convective losses                                     | MW               | MW               
!   22| XOUT_sh(14)                      | Superheater radiative losses                                      | MW               | MW               
!   23| XOUT_sh(15)                      | Energy rate absorbed by superheater                               | MW               | MW               
!   24| XOUT_sh(7)                       | Max superheater surface temperature                               | C                | K                
!   25| XOUT_sh(9)                       | Exit velocity through superheater                                 | m/s              | m/s              
!   26| f_mdot_rh                        | Reheater mass flow rate fraction                                  | none             | none             
!   27| P_rh_in                          | Reheater inlet pressure                                           | kPa              | kPa              
!   28| T_rh_in                          | Reheater inlet temperature                                        | C                | K                
!   29| XOUT_rh(3)                       | Reheater outlet pressure                                          | kPa              | Pa               
!   30| T_rh_target                      | Reheater outlet temperature                                       | C                | K                
!   31| DPRH                             | Pressure drop through reheater                                    | Pa               | Pa               
!   32| XOUT_rh(1)                       | Efficiency of reheater                                            | none             | none             
!   33| XOUT_rh(7)                       | Max reheater surface temperature                                  | C                | K                
!   34| XOUT_rh(15)                      | Exit velocity through reheater                                    | m/s              | m/s              
!   35| XOUT_rh(12)                      | Reheater convective losses                                        | MW               | MW               
!   36| XOUT_rh(13)                      | Reheater radiative losses                                         | MW               | MW               
!   37| XOUT_rh(14)                      | Energy rate absorbed by reheater                                  | MW               | MW               
!   38| Q_total                          | Total pre-defocus incident radiation on receiver                  | MW               | W                
!   39| EnergyInComb                     | Total post-defocus incident radiation on receiver                 | MW               | W                
!   40| defocus                          | Defocus fraction                                                  | none             | none             
!   41| field_eff_adj                    | Adjusted field efficiency                                         | none             | none             
!   42| Q_abs_rec                        | Energy rate absorbed by receiver                                  | MW               | MW               
!   43| Q_conv_rec                       | Receiver convective losses                                        | MW               | MW               
!   44| Q_rad_rec                        | Receiver radiative losses                                         | MW               | MW               
!   45| Q_therm_in_rec                   | Rate of energy into steam/water                                   | MW               | W                
!   46| W_dot_boost                      | Feedwater booster pump power                                      | MW               | W                
!   47| m_dot_aux                        | Auxiliarly mass flow rate (steam/water)                           | kg/hr            | kg/s             
!   48| Q_aux                            | Auxiliarly heat rate delivered to cycle                           | MW               | W                
!   49| Q_aux_fuel                       | Fuel energy rate delivered to aux heater                          | MMBTU            | MMBTU            
!   50| standby_control                  | 1: normal turbine operation, 2: Standby, 3: Off                   | none             | none             
!   51| f_timestep                       | Fraction of timestep turbine can operate                          | none             | none             
!   52| m_dot_toPB                       | Mass flow rate to power block (rec + aux)                         | kg/hr            | kg/s             
            

!-----------------------------------------------------------------------------------------------------------------------
!    TRNSYS acess functions (allow to acess TIME etc.) 
USE TrnsysConstants
USE TrnsysFunctions
use global_props
!USE FluidProperties
USE Water_properties

!-----------------------------------------------------------------------------------------------------------------------
! required by the multi-dll version of TRNSYS
!DEC$ATTRIBUTES DLLEXPORT :: TYPE265

implicit none 

!TRNSYS declarations
real(8):: time
integer*4:: info(15), iunit, itype, icntrl
integer*4,parameter::np=65,ni=17,nout=61,nd=0,ns=4      
integer*4,parameter::ni_b=10,np_b=15,nout_b=21, ni_sh=7,np_sh=11,nout_sh=15, ni_rh=8,np_rh=11,nout_rh=15  !Number of parameters for boiler, sh, and rh models

!Dimension the TRNSYS variables
real(8)::xin(ni),out(nout),par(np),stored(ns),T(nd),dtdt(nd) 

!*******************************

REAL(8)::PAR_b(np_b),PAR_sh(np_sh),PAR_rh(np_rh),XIN_b(ni_b),XIN_sh(ni_sh),XIN_rh(ni_rh),XOUT_b(nout_b),XOUT_sh(nout_sh),XOUT_rh(nout_rh)
REAL(8)::array(12),flux_in(12) 
REAL(8)::T_amb,T_sky,v_wind,P_atm,m_dot_sh,eta_rec,T_boil,& 
            T_sh_out,f_b,diff_T_sh,T_sh_target,f_upper,f_lower,y_upper,y_lower,f_adjust,&
            P_rh_in,T_rh_target,T_rh_in,m_dot_rh,T_rh_out,eta_b,f_rh,diff_T_rh,&
            f_rh_upper,f_rh_lower,y_rh_upper,y_rh_lower,f_rh_adjust,f_mdot_rh,diff_f_bracket,diff_frh_br,defocus,&
            h_sh_target,h_rh_target,tol_t_rh,tol_t_sh,bracket_tol,P_sh_out_min,P_rh_out_min,&
            L_rec,W_rec,D_rec,Per_rec,zenith,azimuth,I_bn,field_eff,hour,T_dp,skytemp,THT,hl_ffact,&
            tol_t_sh_base,tol_t_sh_high,v_wind_g,&
            q_rec_des,rec_qf_delay,rec_su_delay,Q_therm_in_b,Q_therm_in_sh,Q_therm_in_rh,dt,E_su,t_su,E_su0,t_su0,q_startup,f_timestep,&
            m_dot_aux,m_dot_toPB,Q_aux,h_HP_in,h_LP_in,h_rh_in,q_aux_max,E_aux,LHV_eff,&
            h_b,h_sh,h_rh,h_total,&    !heights of receivers
            P_b_in,T_HP_out,T_fw,P_cond,& !Inputs from PB model
            EnergyInComb,dpSH,dpRH,x_b_target,h_fw,h_fw_Jkg,&            
            f_rec_min,q_rec_min,area_rec,q_total,f_pb_cutoff,q_pb_design,q_pb_min,f_pb_sb,q_sb_min,Q_therm_in_rec,q_aux_rec,&
            rho_fw,W_dot_fw,deltaP1,tfrac,A_panel,eta_b_ref,eta_sh_ref,eta_rh_ref,h_rh_out_ref,h_sh_in_ref,m_dot_ref,&
            q_b_des,q_sh_des,q_rh_des,h_sh_out_ref,h_rh_in_ref,m_dot_guess,dp_b,q_pb_max,&
            diff_m_dot_old,diff_m_dot_out,m_dot_prev,Q_inc_abs,Q_aux_fuel,W_dot_sd,W_dot_boost,eta_fw_pump,f_time_prev,dp_b_prev,DPSH_prev,DPRH_prev,&
            P_HP_in_des,P_HP_out_des,f_mdotrh_des,P_b_in_min,P_HP_out_min,P_HP_out,rho_HP_out,h_HP_out,dp_rh_up,P_rh_out,&
            rho_rh_out,h_rh_out,dp_rh_down,P_LP_in,P_sh_out,rho_sh_out,h_sh_out,dp_sh_down,P_HP_in,&
            h_HP_in_des,s_hp_in_des,h_HP_out_isen,h_HP_out_des,T_rh_in_des,h_rh_out_des,h_sh_in_des,h_fw_out_des,m_dot_des,T_fw_out_des,T_rh_target_des,&
            diff_dp_b, diff_dpsh, diff_dprh,&
            T_amb_des,dt_cw_ref,T_approach,T_itd_des,m_dot_ND,Psat_des,q_b_pred_sp,q_sh_pred_sp,q_rh_pred_sp,q_b_pred,q_sh_pred,q_rh_pred,&
            s_HP_out_des,h_LP_out_isen,h_LP_out_des,P_cycle_design,T_boil_des,deltaT_fw_des,T_boil_pred,s_sh_out_ref,h_LP_isen_ref,&
            q_sh_des_sp,q_rh_des_sp,q_b_des_sp,&
            rh_count_out,sh_count_out,boiler_count_out,q_total_low,eta_rh_low,eta_sh_low,eta_b_low,q_total_high,eta_rh_high,eta_sh_high,eta_b_high,&
            m_dot_lower,m_dot_upper,&
            q_inc_b_sh,eta_sh,eta_rh,W_dot_aux,q_abs_rec,q_conv_rec,q_rad_rec,eta_therm_rec, field_eff_adj,Q_therm_in_diff,&
            Interpolate,eta_adj,q_rec_pred_tot,q_total_df,mguessmult,PB_on,PB_on0,T_standby,T_standby0,T_standby_ini,&
            eta_des, cyclemap_dsg,m_dot_aux1,df_upper,y_df_upper,df_lower,y_df_lower,A_cs_b,A_cs_sh,A_cs_rh,rho_HP_in_des,rho_LP_in_des,rho_fw_out_des,&
            v_boiler_in,v_sh_out,v_rh_out,d_b,d_sh,d_rh,m_dot_tube_b,m_dot_tube_sh,m_dot_tube_rh,A_sf

integer*4::i,j,indx,TOUPeriod,NUMTOU,fossil_mode,N_panels,T_sh_iter,sh_exit,T_rh_iter,df_count,boiler_exit,checkflux,rh_exit,success,CT,&
                br_lower,br_upper,fb_stuck,rh_br_upper,rh_br_lower,dummy,up1,LU_flux,par_end,par_b_end,par_sh_end,&
                par_rh_end,standby_control,open_stat12,rh_count,sh_count,boiler_count,high_pres_count,flowtype,df_pred_ct
character::error_message*255
logical::upflag,lowflag,RHupflag,RHlowflag,RHlowguess,upguess,df_flag,lowguess,RHupguess,checkHXs,eta_lin_approx,q_low_set,q_high_set,High_Tol,df_upflag,df_lowflag
           
integer*4,allocatable::Panel(:),flo(:),ceil(:)
real(8),allocatable::FFRAC(:),ppos(:),ind(:),Q_inc(:),Q_inc_base(:),Q_inc_b(:),Q_inc_sh(:),Q_inc_rh(:)

! --- Initial call to detect the TRNSYS version for which this Type is written -----------------------------------------
if (info(7) .eq. -2) then
    info(12) = 16   ! This component is a TRNSYS 16 Type
    return 1
endif

!---PERFORM ANY POST-CONVERGENCE MANIPULATIONS THAT ARE REQUIRED HERE
IF (INFO(13).GT.0) THEN
        
        stored(1) = pb_on           ![-] is the power block running? 1: yes, 2: no
        stored(2) = E_su            ![W-hr] Startup energy remaining
        stored(3) = t_su            ![hr] Startup time remaining
        stored(4) = T_standby       ![hr] Remaining standby operation time
        call setStorageVars(stored,ns,info) 
        
        IF(df_flag)THEN
            call messages(-1,"The outlet pressure of either the superheater or reheater was less than the condenser pressure, requiring defocus",'WARNING',INFO(1),INFO(2))
        ENDIF       
        
        IF(XIN(11)>190)THEN
            high_pres_count = high_pres_count+1
        ENDIF      
        
        IF( (TIME>8758).and.(high_pres_count>0) )THEN
            call messages(-1,"The boiler inlet pressure is greater than 190 bar (steam property limit) during some timesteps.  It is limited to 190 in the boiler model which may affect the accuracy of some calculations", 'WARNING', INFO(1), INFO(2))
        ENDIF      
                        
	    RETURN 1
ENDIF

!*******************************************************
!For self-contained: set input and parameter arrays here
!*******************************************************
    !Inputs
!dummy   = up1(0)
!XIN(up1(1)) = 0.d0 +180.d0      ![deg] Solar azimuth
!!By TRNSYS convention, the azimuth angle is 0 at due south, negative to the east,
!! and positive to the west. The range is then -180 to 180. By the convention used
!! here, the azimuth is 0 at due north, and ranges clockwise from 0 to 360. This adjusts.
!XIN(up1(1)) = 10.d0             ![deg] Solar zenith
!XIN(up1(1)) = 3420.d0           ![kJ/m^2-hr] Beam normal irradiance
!XIN(up1(1)) = 283.15d0 - 273.15d0 ![C] Ambient Temperature
!XIN(up1(1)) = 5.25d0            ![m/s] Wind Speed 
!XIN(up1(1)) = 1.d0              ![Pa] Ambient Pressure
!XIN(up1(1)) = 11.d0             ![-] Hour of the day (used in sky temp model)
!XIN(up1(1)) = T_amb - 4.d0      ![C] Dew Point
!XIN(up1(1)) = 1                 ![-] Time of Use Period
!XIN(up1(1)) = 170               ![bar] Turbine Inlet Pressure from PB model
!XIN(up1(1)) = 0.85              ![-] Reheat mass flow fraction from PB model
!XIN(up1(1)) = 38.9              ![bar] Reheat inlet pressure from PB model
!XIN(up1(1)) = 300               ![C] Reheat inlet temp from PB model
!XIN(up1(1)) = 485               ![C] Reheat outlet temp from PB model

!*******************************************************
!First Call: Get Parameters and Perform Calculations That Are Constant
!*******************************************************
IF (INFO(7) == -1) THEN
    !*******************************************************
    !Get All Parameters That Will Be Delivered By TRNSYS
    !*******************************************************    
    dt          = getSimulationTimeStep()   !Get simulation timestep  
    
    !Use Northland Numerics Routine for water properties (false)
    use_refprop = .false.                    
           
    dummy       = up1(0)
            !Time Of Use
    NUMTOU      = int(PAR(up1(1)))          ![-] Number of time-of-use periods
    fossil_mode = int(PAR(up1(1))) 	        ![-] The fossil fill operation strategy mode
    q_pb_design = PAR(up1(1))*1.e6          ![W] Heat rate into powerblock at design
    q_aux_max   = PAR(up1(1))*1.e6          ![W] Maximum heat rate of auxiliary heater
    LHV_eff     = PAR(up1(1))               ![-] Aux Heater lower heating value efficiency
    
            !General Geometry
    THT         = PAR(up1(1))               ![m] Tower height
    
    N_panels    = PAR(up1(1))               ![-] Number of vertical panels on receiver 
    flowtype    = PAR(up1(1))               ![-] Flow Pattern
    
    if( (flowtype<=4) .and. (mod(N_panels,2)>0)) then
        call messages(-1,'The number of panels for this flow configuration must be divisible by 2',"Fatal",INFO(1),INFO(2))
    endif
    
    D_rec       = PAR(up1(1))               ![m] Diameter of receiver for external convection correlation        
    Per_rec     = 3.14159 * D_rec           ![m] Perimeter of receiver
    
    !Allocate important arrays
    ALLOCATE(Panel(N_panels),flo(N_panels),ceil(N_panels),ppos(N_panels),ind(N_panels),Q_inc(N_panels),&
                Q_inc_base(N_panels),Q_inc_b(N_panels),Q_inc_sh(N_panels),Q_inc_rh(N_panels))
    !*********************************************************************************************************************************
    
    par_end     = up1(1) - 1
    
    dummy       = up1(0)
    q_rec_des   = PAR(par_end + up1(1))*1.e6    ![MW] Design-point thermal power
    f_rec_min   = PAR(par_end + up1(1))     ![-] Minimum receiver absorbed power fraction
    q_rec_min   = q_rec_des * f_rec_min     ![MW] Minimum receiver absorbed power
    rec_qf_delay = PAR(par_end + up1(1))    ![-] Receiver start-up delay fraction of thermal energy of receiver running at design for 1 hour
    rec_su_delay = PAR(par_end + up1(1))    ![hr] Receiver start-up delay time   
    f_pb_cutoff = PAR(par_end + up1(1))     ![-] Cycle cut-off fraction 
    q_pb_min    = q_pb_design * f_pb_cutoff ![W] Cycle cut-off heat rate 
    f_pb_sb     = PAR(par_end + up1(1))     ![-] Cycle minimum standby fraction
    q_sb_min    = q_pb_design * f_pb_sb     ![W] Cycle minimum standby heat rate
    T_standby_ini = PAR(par_end + up1(1))   ![hr] Time that power block can remain in standby
    
    x_b_target  = PAR(par_end + up1(1))     ![-] Set target outlet quality of boiler
    eta_fw_pump = PAR(par_end + up1(1))     ![-] Efficiency of feedwater pump(s)
    
    P_HP_in_des  = PAR(par_end + up1(1))*1.E2   ![kPa] Design High Pressure Turbine Inlet Pressure (convert from bar)
    P_HP_out_des = PAR(par_end + up1(1))*1.E2   ![kPa] Design High Pressure Turbine Outlet Pressure (convert from bar)
    
    IF( (P_HP_in_des>18000.d0).or.(P_HP_out_des>18000.d0) )THEN
        call messages(-1,'The specified pressure(s) is greater than 180 bar (limit for SAM property routine)',"Fatal",INFO(1),INFO(2))
    ENDIF
    
    f_mdotrh_des = PAR(par_end + up1(1))        ![-] Design reheat mass flow rate fraction
    P_cycle_design = PAR(par_end + up1(1))*1.E3         ![kW] Design cycle power (convert from MW)
    eta_des     = P_cycle_design*1000.d0/q_pb_design    ![-] Design cycle efficiency
    
    CT          = int(PAR(par_end + up1(1)))    ![-] Cooling Type
    T_amb_des   = PAR(par_end + up1(1)) + 273.15d0  ![K] Design ambient temperature for design power cycle values, convert from C
    dT_cw_ref   = PAR(par_end + up1(1))         ![C] Reference condenser cooling water inlet/outlet T diff
    T_approach  = PAR(par_end + up1(1))         ![K] Cooling tower approach temperature, convert from C
    T_ITD_des   = PAR(par_end + up1(1))         ![C] ITD at design for dry system
    
    P_b_in_min  = 0.5 * P_HP_in_des         ![kPa] Corresponds to limit set in Type 234
    !P_b_in_min  = 5.E3                      ![kPa] Once limit is set in Type 234 we can delete this line and use above
    
    P_HP_out_min = 0.5 * P_HP_out_des      ![kPa] Corresponds to limit set in Type 234
    !P_HP_out_min = 5.E2                    ![kPa] Once limit is set in Type 234 we can delete this line and use above

    hl_ffact    = PAR(par_end + up1(1))     ![-] Heat Loss Fudge FACTor
    par_end     = par_end + up1(1) - 1    
    
    PAR_b(1)        = D_rec             ![m] "Diameter" of receiver for external convection correlation            
    PAR_b(2)        = Per_rec           ![m] Perimeter of receiver
    PAR_b(3)        = hl_ffact          ![-] Heat Loss Fudge FACTor
    PAR_b(4)        = flowtype          ![-] Flow Pattern
    par_b_end       = 4
    DO j=1,(np_b-par_b_end)
        IF(j==1) dummy = up1(0)
        indx   = up1(1)
        PAR_b(par_b_end + indx)   = PAR(par_end + indx)         ![m] Height of receiver
    ENDDO
    par_end         = par_end + up1(1) - 1
    h_b             = PAR_b(par_b_end + 1)
    d_b             = par_b(6)
    A_cs_b          = 3.14d0 * 0.25d0 * par_b(6)**2             ![m^2] Cross-sectional area of boiler tube
    
!    PAR_b(5)       ![m] Height of boiler   
!    PAR_b(6)       ![m] Outer diameter of boiler tubes
!    PAR_b(7)       ![m] Thickness of boiler tubes 
!    PAR_b(8)       ![-] Emissivity of boiler tubes 
!    PAR_b(9)       ![-] Absorptivity of boiler tubes 
!    PAR_b(10)       ![-] Numerical code for tube material (2: Stainless_AISI316, 28: T-91 Steel) 
!        !Possible Fin Dimensions & Properties
!    PAR_b(11)       ![m] Thickness of fin 
!    PAR_b(12)      ![m] Length of fin (distance between boiler tubes)
!    PAR_b(13)      ![-] Emissivity of fin
!    PAR_b(14)      ![-] Absorptance of fin 
!    PAR_b(15)      ![-] Numerical code for fin material (2: Stainless_AISI316, 28: T-91 Steel)

    call Boiler(TIME,XIN_b,XOUT_b,PAR_b,INFO,N_panels,Q_inc_b,boiler_exit,checkflux)
        !Superheater Geometry and Setpoints
    PAR_sh(1)  = D_rec             ![m] "Diameter" of receiver for external convection correlation
    PAR_sh(2)  = Per_rec           ![m] Perimeter of receiver
    PAR_sh(3)  = hl_ffact          ![-] Heat Loss Fudge FACTor  
    PAR_sh(4)  = flowtype          ![-] Flow Pattern
    par_sh_end = 4
    DO j=1,(np_sh-par_sh_end)
        IF(j==1) dummy = up1(0)
        indx   = up1(1)
        PAR_sh(par_sh_end + indx)   = PAR(par_end + indx)         ![m] Height of receiver
    ENDDO
    T_sh_target     = PAR_sh(np_sh) + 273.15d0  ![K] Target outlet temperature of superheater
    par_end         = par_end + up1(1) - 1
    h_sh            = PAR_sh(par_sh_end + 1)
    d_sh            = par_sh(6)
    A_cs_sh         = 3.14d0 * 0.25d0 * par_sh(6)**2            ![m^2] Cross-sectional area of superheater tube
    
            !Superheater Dimensions & Properties
    !PAR_sh(5)      = ![m] Height of superheater
    !PAR_sh(6)      = ![m] Outer diameter of superheater tubes
    !PAR_sh(7)      = ![m] Thickness of superheater tubes
    !PAR_sh(8)      = ![-] Emissivity of superheater tubes
    !PAR_sh(9)      = ![-] Absorptivity of superheater tubes
    !PAR_sh(10)      = ![-] Superheater material
    !PAR_sh(11)     = ![C] Target superheater outlet temperature - convert to K

    call Superheater(INFO,XIN_sh,XOUT_sh,N_panels,PAR_sh,Q_inc_sh,h_sh_target,sh_exit,checkflux)    
  
            !Reheater Geometry and Setpoints
    PAR_rh(1)  = D_rec             ![m] "Diameter" of receiver for external convection correlation
    PAR_rh(2)  = Per_rec           ![m] Perimeter of receiver
    PAR_rh(3)  = hl_ffact          ![-] Heat Loss Fudge FACTor
    PAR_rh(4)  = flowtype          ![-] Flow Pattern
    par_rh_end = 4
    DO j=1,(np_rh-par_rh_end)
        IF(j==1) dummy = up1(0)
        indx   = up1(1)
        PAR_rh(par_rh_end + indx)   = PAR(par_end + indx)         ![m] Height of receiver
    ENDDO
    T_rh_target_des = PAR_rh(np_rh) + 273.15d0  ![K] Target outlet temperature of reheater
    par_end         = par_end + up1(1) - 1
    h_rh            = PAR_rh(par_rh_end + 1)
    d_rh            = par_rh(6)                                 ![m] Diameter of reheater tube
    A_cs_rh         = 3.14d0 * 0.25d0 * par_rh(6)**2             ![m^2] Cross-sectional area of superheater tube
    
    h_total         = h_b + h_sh + h_rh
    Area_rec        = h_total * Per_rec         ![m^2] Total surface area of receiver
    A_panel         = h_total * Per_rec / dble(N_panels) ![m^2]
     
        !Reheater Dimensions & Properties  
    !PAR_rh(5)      = ![m] Height of reheater
    !PAR_rh(6)      = ![m] Outer diameter of reheater tubes
    !PAR_rh(7)      = ![m] Thickness of reheater tubes
    !PAR_rh(8)      = ![-] Emissivity of reheater tubes
    !PAR_rh(9)      = ![-] Absorptivity of reheater tubes
    !PAR_rh(10)      = ![-] Reheater material (2: Stainless_AISI316, 28: T-91 Steel)
    !PAR_rh(11)     = ![C] Target reheater outlet temperature

    CALL Reheater(INFO,XIN_rh,XOUT_rh,N_panels,PAR_rh,Q_inc_rh,h_rh_target,rh_exit,checkflux)

    dummy           = up1(0)
    !Get Logical Unit for Input Flux File
    LU_Flux         = Par(par_end + up1(1))
    tfrac           = Par(par_end + up1(1))
    A_sf            = Par(par_end + up1(1))     ![m^2] Total reflective of heliostat field
    q_pb_max        = tfrac * q_pb_design       ![W] Maximum heat rate to power block.  Defocus if > 

    !Set fossil fraction for TOUPeriods
    ALLOCATE(ffrac(NUMTOU))
    DO i=1,NUMTOU
        ffrac(i)=PAR(par_end + up1(1))                ![-] Fossil fill fraction control array
    ENDDO   
    
    call water_TP(T_sh_target - 273.15d0,P_HP_in_des, enth=h_HP_in_des,entr=s_HP_in_des,dens=rho_HP_in_des)   !Design high pressure turbine inlet enthalpy(kJ/kg) and entropy(kJ/kg-K)    
    call water_PS(P_HP_out_des, s_HP_in_des, enth=h_HP_out_isen)                    !Design reheat extraction enthalpy(kJ/kg) assuming isentropic expansion
    h_HP_out_des = h_HP_in_des - (h_HP_in_des - h_HP_out_isen)*0.88                 !Design reheat inlet enthalpy(kJ/kg) (isentropic efficiency = 88%)
    call water_PH(P_HP_out_des, h_HP_out_des, temp=T_rh_in_des)                     !Design reheat inlet temperature(C) 
    T_rh_in_des     = T_rh_in_des + 273.15d0                                        ![K] Convert from C   
    call water_TP(T_rh_target_des - 273.15d0, P_HP_out_des, enth=h_rh_out_des, entr = s_HP_out_des,dens=rho_LP_in_des)              !Reheat outlet enthalpy(kJ/kg) and entropy(kJ/kg-K)
    !************ Design Condenser Pressure [kPa] *********************
    IF(CT==1)THEN
        CALL water_TQ((dT_cw_ref + 3.d0 + T_approach + T_amb_des - 273.15d0),0.d0, pres = Psat_des)
        eta_adj = eta_des/(CycleMap_DSG(5,12,2,Psat_des*1000.d0)/CycleMap_DSG(5,22,2,Psat_des*1000.d0))
    ELSEIF((CT==2).or.(CT==3))THEN
        CALL water_TQ((T_ITD_des + T_amb_des - 273.15d0),0.d0, pres = Psat_des)
        eta_adj = eta_des/(CycleMap_DSG(5,12,2,Psat_des*1000.d0)/CycleMap_DSG(5,22,2,Psat_des*1000.d0))
    ENDIF
    !**********************************************************
    Q_pb_design = P_cycle_design/eta_adj*1000.d0                            ![W] Now reset cycle efficiency based on design conditions
    call Water_PS(Psat_des, s_HP_out_des, enth = h_LP_out_isen)                      !Design low pressure outlet enthalpy (kJ/kg) assuming isentropic expansion
    h_LP_out_des = h_rh_out_des - (h_rh_out_des - h_LP_out_isen)*0.88               !Design low pressure outlet enthalpy (kJ/kg)

    call water_PQ(P_HP_in_des,1.d0, enth=h_sh_in_des, temp=T_boil_des)                               !Design SH inlet enthalpy [kJ/kg]     
    
    !Calculate design mass flow rate based on design setpoints
    !Equation: P_cycle_design = m_dot*(h_HP_in_des - h_HP_out_des) + m_dot*f_mdotrh_des*(h_rh_out_des - h_LP_out_des)
    m_dot_des = P_cycle_design/( (h_HP_in_des - h_HP_out_des) + f_mdotrh_des*(h_rh_out_des - h_LP_out_des) )    ![kW/(kJ/kg)] = kg/s

    !Now, find feedwater outlet enthalpy using design cycle efficiency
    q_sh_des = (h_HP_in_des - h_sh_in_des)*m_dot_des        ![kW] Design rate of energy input to superheater
    q_rh_des = (h_rh_out_des - h_HP_out_des)*m_dot_des*f_mdotrh_des      ![kW] Design rate of energy input to reheater
    
    !Equation: Q_pb_design = q_sh_des + q_rh_des + q_b_des
    q_b_des  = (Q_pb_design/1.E3) - q_sh_des - q_rh_des            ![kW] Design rate of energy input to boiler
    
    !Equation: q_b_des = (h_sh_in_des - h_fw_out_des)*m_dot_des
    h_fw_out_des = h_sh_in_des - q_b_des/m_dot_des                  ![kJ/kg] Design feedwater outlet enthalpy
    call Water_PH(P_HP_in_des, h_fw_out_des, temp = T_fw_out_des,dens=rho_fw_out_des)   ![C] Design feedwater outlet temperature
    T_fw_out_des = T_fw_out_des + 273.15d0                          ![K] Convert from C
          
    m_dot_tube_b = (m_dot_des/x_b_target)/2.d0/(Per_rec/dble(N_panels)/d_b) ![kg/s] Design mass flow rate through one boiler tube
    m_dot_tube_sh = (m_dot_des/2.d0)/(Per_rec/dble(N_panels)/d_sh)          ![kg/s] Design mass flow rate through one SH tube
    m_dot_tube_rh = (m_dot_des/2.d0)/(Per_rec/dble(N_panels)/d_rh)          ![kg/s] Design mass flow rate through one RH tube
    
    v_boiler_in = (m_dot_tube_b/x_b_target)/(rho_fw_out_des*A_cs_b)         ![m/s] Design fluid entrance velocity in boiler tube
    v_sh_out = m_dot_tube_sh / (rho_HP_in_des*A_cs_sh)                      ![m/s] Design fluid exit velocity in superheater tube
    v_rh_out = m_dot_tube_rh / (rho_LP_in_des*A_cs_rh)                      ![m/s] Design fluid exit velocity in reheater tube
    
    !Specific heat rate of receivers
    q_sh_des_sp = (h_HP_in_des - h_sh_in_des)       ![kJ/kg]
    q_rh_des_sp = (h_rh_out_des - h_HP_out_des)     ![kJ/kg]
    q_b_des_sp  = (h_sh_in_des - h_fw_out_des)      ![kJ/kg]
    
    deltaT_fw_des = T_boil_des - T_fw_out_des                       ![K] Design difference between boiling temperature and feedwater outlet temperature     
         
    Psat_des    = Psat_des * 1.E3   ![Pa] Convert from kPa for calculations later in code
    
    Call fluxinterp(LU_flux,zenith,azimuth,array,info)
    
    if(.not.allocated(fprop)) allocate(fprop(1,1))
    
    call setStorageSize(ns,info)
    stored(1) = 0.d0                        ![-] Powerblock is not running at first timestep                  
    stored(2) = Q_rec_des * rec_qf_delay    ![W-hr]  Set startup energy requirement for mode = 0
    stored(3) = rec_su_delay                ![hr] Set startup time requirement for mode = 0
    stored(4) = T_standby_ini               ![hr] Powerblock standby time remaining
    call setStorageVars(stored,ns,info)

    !Solving (absolute) tolerances for reheater and superheater exit temperatures
!    tol_T_rh = 1.d0         ![K]
!    tol_T_sh_base = 1.d0    ![K]
!    tol_T_sh_high = 10.d0   ![K]
    tol_T_rh = 0.005    ![-]
    tol_T_sh_base = 0.005   ![-]
    tol_T_sh_high = 0.03    ![-]
    !tol_T_sh_high = 1.d0
    bracket_tol = 0.001     ![-] Boiler fraction bracket
    
    high_pres_count = 0
    eta_lin_approx = .true.
    q_low_set = .false.
    q_high_set = .false.

    !**** Turns out it is very important to include this ****
    info(6)=nout
    
    RETURN 1
endif

!************************************
!Very last call of the simulation: close files, etc
!************************************
IF (INFO(8)== -1) THEN

    call Boiler(TIME,XIN_b,XOUT_b,PAR_b,INFO,N_panels,Q_inc_b,boiler_exit,checkflux)
    call Superheater(INFO,XIN_sh,XOUT_sh,N_panels,PAR_sh,Q_inc_sh,h_sh_target,sh_exit,checkflux)
    CALL Reheater(INFO,XIN_rh,XOUT_rh,N_panels,PAR_rh,Q_inc_rh,h_rh_target,rh_exit,checkflux)
    Call fluxinterp(LU_flux,zenith,azimuth,array,info)
    
    !Close Files
    close(LU_Flux)

    !Deallocate
    if(allocated(ffrac)) deallocate(FFRAC,Panel,flo,ceil,ppos,ind,Q_inc,Q_inc_base,Q_inc_b,Q_inc_sh,Q_inc_rh)

    RETURN 1
    
ENDIF

IF( (standby_control>1).and.(info(7)>0) )  RETURN 1

!***********************************
!Time-dependent Calculations
!***********************************
!Inputs
dummy   = up1(0)

P_b_in    = XIN(10+up1(1))*1.E2     ![kPa] Boiler Inlet Pressure-> HP turbine inlet (from regression) + DP_SH + DP_B
f_mdot_rh = XIN(10+up1(1))          ![-] Reheat mass flow fraction from PB model
P_HP_out = XIN(10+up1(1))*1.E2      ![kPa] Reheat inlet pressure from PB model
T_HP_out = XIN(10+up1(1))+273.15    ![K] Reheat inlet temp from PB model

T_rh_target= XIN(10+up1(1))+273.15  ![K] Reheat outlet temp from PB model
T_fw = XIN(10+up1(1))+273.15        ![K] Feedwater outlet temp from PB model
P_cond  = XIN(10+up1(1))            ![Pa] Condenser pressure from PB model      

!Need to set this here because if using fossil fuel in low irradiance cases, it will skip part of code that sets T_rh_in
T_rh_in = T_HP_out

!May be using aux with receiver off, so need logic to skip receiver calcs 
IF( (success==0) .and.(info(7)>0) ) GOTO 375    

df_upflag = .false.
df_lowflag = .false.

!These values only need to be calculated once every timestep
IF(info(7)==0)THEN

    dummy   = up1(0)    !Reset counter
    
    azimuth = XIN(up1(1))           ![deg] Solar azimuth angle       
    zenith  = XIN(up1(1))           ![deg] Solar zenith
    I_bn    = XIN(up1(1))/3.6d0     ![W/m^2] Beam normal irradiance: convert from [kJ/m^2-hr]
    T_amb   = XIN(up1(1))+273.15d0  ![K] Ambient Temperature
    v_wind_g = XIN(up1(1))          ![m/s] Wind Speed (at 10m, not receiver) 
    P_atm   = XIN(up1(1))*101325    ![Pa] Ambient Pressure
    hour    = XIN(up1(1))           ![-] Hour of the day (used in sky temp model)
    T_dp    = XIN(up1(1)) + 273.15  ![K] Dew point temperature
    TOUPeriod = int(XIN(up1(1)))    ![-] Time of Use PeriodB2    
    field_eff = XIN(up1(1))         ![-] field efficiency
        
    !Calculate sky temp [K]
    T_sky   = skytemp(T_amb,T_dp,hour)
    !Correct the windspeed to receiver height using the logarithmic profile law (HOMER documentation, Wind Shear Inputs section)
    V_wind = log((THT+(h_total/2.)/2.)/.003)/log(10./.003)*V_wind_g

    call getStorageVars(stored,ns,info)
    pb_on0  = stored(1)     ![-] is the power block running? 1: yes, 2: no
    E_su0   = stored(2)     !Energy required for startup
    t_su0   = stored(3)     ![hr] Time required for startup
    T_standby0 = stored(4)  ![hr] Remaining standby operation time

    !Reset Values
    Q_aux = 0.d0
    m_dot_aux = 0.d0

    !Defocus: this needs to be before low flux checks!!
    defocus     = 1.            ![-] Defocus control
    df_flag     = .false.       ![-] Defocus flag: true = defocus < 1

    !Calculate Flux
    IF(I_bn > 150.d0)THEN
        Call fluxinterp(LU_flux,zenith,azimuth+180.d0,array,info)
        !By TRNSYS convention, the azimuth angle is 0 at due south, negative to the east,
        ! and positive to the west. The range is then -180 to 180. By the convention used
        ! here, the azimuth is 0 at due north, and ranges clockwise from 0 to 360. This adjusts.
        !flux_in(:)=array(:)/(950.)*I_bn*field_eff   ![kW/m^2] The weather-adjusted, efficiency-adjusted flux values
        !mjw 6.27.12 Modified the PTGEN code to give a normalized 1x12 array of [flux absorbed at node j/flux incident on receiver]
        !Get the flux_in() array in terms of kW/m2.
        !(pi*D_rec*H_rec/12.) = Area of panel
        flux_in(:)=array(:)*I_bn*field_eff*A_sf/1000./(h_total * Per_rec / 12.)  !kw/m2
    ELSE
        success = 0
        Q_total = 0.d0
        GOTO 375
    ENDIF

    !Translate to the number of panels, so each panel has its own linearly interpolated flux value
    !Taken from Type 222
    do j=1,N_panels
        Panel(j) = j	                       !The position of each panel
        ppos(j)=(12./dble(N_panels)*(j-1.)+6./dble(N_panels))+1.
        flo(j)=floor(ppos(j))
        ceil(j)=ceiling(ppos(j))
        ind(j)= (ppos(j)-flo(j))/max(dble(ceil(j)-flo(j)),1.e-6)
        if(ceil(j).gt.12.) ceil(j)=1
        Q_inc_base(j)=ind(j)*(flux_in(ceil(j))-flux_in(flo(j)))+flux_in(flo(j))*1000.d0  ![W/m^2] Average area-specific power for each node
    enddo
    
    Q_total     = sum(Q_inc_base)* A_panel     ![W] Available Thermal Power

    !If available thermal power is less specified receiver minimum, then receiver is shut down, go to post-receiver calcs
    IF(Q_total < q_rec_min)THEN
        success = 0
        GOTO 375
    ENDIF                     
    
    Q_total_df  = Q_total
    mguessmult  = 1.d0
    
    !Predict efficiencies
    IF(eta_lin_approx)THEN
        eta_b_ref = 0.86        ![-] Guess boiler efficiency
        eta_sh_ref = 0.78       ![-] Guess superheater efficiency
        eta_rh_ref = 0.55        ![-] Guess reheater efficiency                
    ELSE
        eta_b_ref = (eta_b_high - eta_b_low)/(q_total_high - q_total_low)*(Q_total-q_total_low) + eta_b_low
        eta_sh_ref = (eta_sh_high - eta_sh_low)/(q_total_high - q_total_low)*(Q_total-q_total_low) + eta_sh_low
        eta_rh_ref = (eta_rh_high - eta_rh_low)/(q_total_high - q_total_low)*(Q_total-q_total_low) + eta_rh_low
    ENDIF
    !Predict mass flow rates using design conditions
    m_dot_ref = (Q_total/1.E3)/(q_b_des_sp/eta_b_ref + q_sh_des_sp/eta_sh_ref + q_rh_des_sp*f_mdotrh_des/eta_rh_ref)   ![kg/s] Estimate mass flow rate using design conditions
    
    !Use max turbine fraction and a multiplier to limit possible values in over-design conditions
    !m_dot_ND = min(tfrac*1.1, m_dot_ref / m_dot_des)      ![-] Nondimensional mass fow rate
    m_dot_ND    = min(tfrac, m_dot_ref/m_dot_des)           ![-] Nondimensional mass flow rate

    P_HP_out    = (Psat_des**2 + m_dot_ND**2*((P_HP_out_des*1.E3)**2 - Psat_des**2))**0.5       
    P_b_in      = (P_HP_out**2 + m_dot_ND**2*((P_HP_in_des*1.E3)**2 - (P_HP_out_des*1.E3)**2))**0.5
    P_HP_out    = P_HP_out / 1.E3
    P_b_in      = P_b_in / 1.E3

    !From Type 234
    !if(is_rh) then
    !    P_rh_in = (Psat_des**2. + m_dot_ND**2.*(P_rh_ref**2. - Psat_des**2.))**0.5  !Patnode thesis, p. 69
    !endif
    !P_turb_in = (P_rh_in**2. + m_dot_ND**2.*(P_boil_ref**2. - P_rh_ref**2.))**0.5
    
    !************************************************************************************
    !*** Inputs from PB model were 0, so we need supply useful values to get started ****
    !************************************************************************************
    !Ensure the boiler and reheat inlet pressures are within the bounds of the steam property code (and are not 0)
    P_b_in  = min(19.e3, max(P_b_in_min,P_b_in))
    P_HP_out = min(19.e3, max(P_HP_out_min,P_HP_out)) 

    !If reheater target temperature is = 0
    IF(T_rh_target==273.15d0)   T_rh_target = T_rh_target_des

    !If mass flow rate fraction is >1 or <0
    IF(f_mdot_rh==0.d0)         f_mdot_rh   = f_mdotrh_des
    f_mdot_rh     = min(1.d0, f_mdot_rh)

    !Always recalculate feedwater temperature
    call water_PQ(P_b_in,1.d0, enth=h_sh_in_ref, temp = T_boil_pred)
    T_fw    = T_boil_pred - deltaT_fw_des
        
    !***Calculate reheat inlet temperature***
    call water_TP((T_sh_target - 273.15),P_b_in, enth=h_sh_out_ref, entr=s_sh_out_ref)  !Predict high pressure turbine inlet enthalpy(kJ/kg) and entropy(kJ/kg-K) 
    call water_PS(P_HP_out,s_sh_out_ref, enth=h_LP_isen_ref)                            ![kJ/kg] Predict isentropic outlet enthalpy at tower base
    h_rh_in_ref = h_sh_out_ref - (h_sh_out_ref - h_LP_isen_ref)*0.88                    ![kJ/kg] Predict outlet enthalpy at tower base
    
    call water_PH(P_rh_in, h_rh_in_ref, dens = rho_HP_out)                              ![kg/m^3] Predict density at tower base
    dp_rh_up    = rho_HP_out * 9.81 * THT                                               ![Pa] Pressure loss due to elevation rise
    P_rh_in     = P_HP_out - dp_rh_up/1000.d0                                           ![kPa] Reheater inlet pressure at receiver                                            
    
    call water_PH(P_rh_in, h_rh_in_ref, temp = T_rh_in)                                 ![C] Predict reheat inlet temperature
    T_rh_in     = T_rh_in + 273.15d0                                                    ![K] Convert from C
        
    call water_TP((T_rh_target - 273.15),P_rh_in, enth=h_rh_out_ref)                    !Reheat outlet enthalpy(kJ/kg)
    CALL Water_TP(T_fw - 273.15, P_b_in, enth=h_fw)                                     ![kJ/kg] Feedwater enthalpy

    df_pred_ct  = 1

681 q_b_pred_sp = (h_sh_in_ref - h_fw)*1000.d0/eta_b_ref            ![J/kg] Predict specific energy input to boiler
    q_sh_pred_sp = (h_sh_out_ref - h_sh_in_ref)*1000.d0/eta_sh_ref  ![J/kg] Predict specific energy input to superheater
    q_rh_pred_sp = (h_rh_out_ref - h_rh_in_ref)*1000.d0/eta_rh_ref  ![J/kg] Predict specific energy input to reheater
 
    m_dot_ref    = Q_total_df/(q_b_pred_sp + q_sh_pred_sp + q_rh_pred_sp*f_mdot_rh)   ![kg/s] Predict cycle mass flow rate
    
    q_b_pred     = q_b_pred_sp * m_dot_ref                          ![W] Predicted rate of energy transferred to boiler
    q_sh_pred    = q_sh_pred_sp * m_dot_ref                         ![W] Predicted rate of energy transferred to superheater
    q_rh_pred    = q_rh_pred_sp * m_dot_ref * f_mdot_rh             ![W] Predicted rate of energy transferred to reheater

    q_rec_pred_tot = q_b_pred + q_sh_pred + q_rh_pred               ![W] Total predicted rate of energy transferred to receiver

    Q_therm_in_diff = (Q_rec_pred_tot - q_pb_max)/q_pb_max
    IF( (Q_therm_in_diff > 0.10) .and. (df_pred_ct==1) )THEN
        defocus = min(1.d0,(defocus*(q_pb_max/Q_rec_pred_tot)))
        Q_total_df   = Q_total * defocus               
        IF(.not.(eta_lin_approx))THEN
            eta_b_ref = (eta_b_high - eta_b_low)/(q_total_high - q_total_low)*(Q_total_df-q_total_low) + eta_b_low
            eta_sh_ref = (eta_sh_high - eta_sh_low)/(q_total_high - q_total_low)*(Q_total_df-q_total_low) + eta_sh_low
            eta_rh_ref = (eta_rh_high - eta_rh_low)/(q_total_high - q_total_low)*(Q_total_df-q_total_low) + eta_rh_low
        ENDIF        
        df_pred_ct = 0
        IF(defocus<0.70d0)THEN
            mguessmult = 1.4d0
        ENDIF
        GOTO 681
    ENDIF

    m_dot_guess = m_dot_ref/x_b_target                              ![kg/s] Mass flow rate through the boiler is related to target outlet quality
    f_rh        = q_rh_pred / (q_b_pred + q_sh_pred + q_rh_pred)    ![-] Predicted reheater fraction
    f_b         = q_b_pred / (q_b_pred + q_sh_pred)                 ![-] Predicted boiler fraction    

    P_cond      = Psat_des          ![Pa] Use reference condenser pressure at first timestep
    !Mass flow rate dependent
    P_sh_out_min = max(1e5,P_cond)  ![Pa] Specify minimum allowable outlet pressure of reheater
    P_rh_out_min = max(1e4,P_cond)  ![Pa] Specify minimum allowable outlet pressure of reheater   
    
    B_EB_count = 0
    
ENDIF   !INFO(7) == 0

IF(INFO(7)>0)THEN
    
    P_b_in = min(19000.d0, P_b_in)
    call water_TP((T_sh_target - 273.15),P_b_in, enth=h_sh_out_ref)  ![kJ/kg] Predict superheater outlet enthalpy
    
    !Calculate temperature and pressure at reheater inlet
    CALL Water_TP(T_HP_out - 273.15d0, P_HP_out, dens = rho_HP_out, enth = h_HP_out)    !Density [kg/m^3] and enthalpy [kJ/kg] at HP outlet
    dp_rh_up    = rho_HP_out * 9.81 * THT               ![Pa] Pressure loss due to elevation rise
    P_rh_in     = P_HP_out - dp_rh_up/1000.d0           ![kPa] Reheater inlet pressure
    CALL Water_PH(P_rh_in, h_HP_out, Temp = T_rh_in)    ![C] Inlet temperature to reheater (receiver)
    T_rh_in = T_rh_in + 273.15d0                        ![K] Convert from C
    !****************************************************
    
    h_rh_in_ref = h_HP_out                                                      ![kJ/kg] Predict reheater inlet enthalpy
    call water_TP((T_rh_target - 273.15),P_rh_in, enth=h_rh_out_ref)            !Predict reheat outlet enthalpy(kJ/kg)
    
    !***********************************
    !11/14/11:
    !***********************************
!    call Water_PQ(P_b_in,0.d0, enth = h_sh_in_ref)
!    CALL Water_TP(T_fw - 273.15, P_b_in, enth=h_fw)                     ![kJ/kg] Feedwater enthalpy
!    q_b_pred_sp     = (h_sh_in_ref - h_fw)*1000.d0/eta_b                ![J/kg] Predict specific energy input to boiler
!    q_sh_pred_sp    = (h_sh_out_ref - h_sh_in_ref)*1000.d0/eta_sh       ![J/kg] Predict specific energy input to superheater
!    q_rh_pred_sp    = (h_rh_out_ref - h_rh_in_ref)*1000.d0/eta_rh       ![J/kg] Predict specific energy input to reheater
!    m_dot_ref       = Q_total_df/(q_b_pred_sp + q_sh_pred_sp + q_rh_pred_sp*f_mdot_rh)   ![kg/s] Predict cycle mass flow rate
!    m_dot_guess = m_dot_ref/x_b_target                              ![kg/s] Mass flow rate through the boiler is related to target outlet quality
!    q_b_pred     = q_b_pred_sp * m_dot_ref                          ![W] Predicted rate of energy transferred to boiler
!    q_sh_pred    = q_sh_pred_sp * m_dot_ref                         ![W] Predicted rate of energy transferred to superheater
!    q_rh_pred    = q_rh_pred_sp * m_dot_ref * f_mdot_rh             ![W] Predicted rate of energy transferred to reheater
!    f_rh        = q_rh_pred / (q_b_pred + q_sh_pred + q_rh_pred)    ![-] Predicted reheater fraction
!    f_b         = q_b_pred / (q_b_pred + q_sh_pred)                 ![-] Predicted boiler fraction 
    
ENDIF 

rh_count = 0
sh_count = 0
boiler_count = 0

df_count    = -1            ![-] Defocus counter

589 continue
df_count    = df_count + 1          !Increase iteration counter
Q_inc       = defocus*Q_inc_base    ![W/m^2] New incident radiation (for defocus calcs)
Q_total_df  = Q_total*defocus       ![W]

!Need to recalculate mass flow rate and flux fraction guesses when defocus changes
IF((df_count > 0).and.(.not.(df_flag)))THEN
    IF(df_count > 10) GOTO 1529
    q_b_pred_sp     = (h_sh_in_ref - h_fw)*1000.d0/eta_b                ![J/kg] Predict specific energy input to boiler
    q_sh_pred_sp    = (h_sh_out_ref - h_sh_in_ref)*1000.d0/eta_sh       ![J/kg] Predict specific energy input to superheater
    q_rh_pred_sp    = (h_rh_out_ref - h_rh_in_ref)*1000.d0/eta_rh       ![J/kg] Predict specific energy input to reheater
    m_dot_ref       = Q_total_df/(q_b_pred_sp + q_sh_pred_sp + q_rh_pred_sp*f_mdot_rh)   ![kg/s] Predict cycle mass flow rate
    m_dot_guess = m_dot_ref/x_b_target                              ![kg/s] Mass flow rate through the boiler is related to target outlet quality
    q_b_pred     = q_b_pred_sp * m_dot_ref                          ![W] Predicted rate of energy transferred to boiler
    q_sh_pred    = q_sh_pred_sp * m_dot_ref                         ![W] Predicted rate of energy transferred to superheater
    q_rh_pred    = q_rh_pred_sp * m_dot_ref * f_mdot_rh             ![W] Predicted rate of energy transferred to reheater
    f_rh        = q_rh_pred / (q_b_pred + q_sh_pred + q_rh_pred)    ![-] Predicted reheater fraction
    f_b         = q_b_pred / (q_b_pred + q_sh_pred)                 ![-] Predicted boiler fraction 
ENDIF

!Reset differences and iteration counter for next nested loop
diff_frh_br = 999.d0                !Difference between upper and lower bounds of reheater fraction iteration     
diff_T_rh   = 999.d0                !Difference between prescribed and calculated reheater temperature
T_rh_iter   = 0                     !Number of iterations on reheater fraction

!Reset logic flags for reheater fraction iteration
RHlowguess  = .false.               !Has a lower BOUND been established?
RHlowflag   = .false.               !Has a lower RESULT been established?
RHupguess   = .false.               !Has an upper BOUND been established? 
RHupflag    = .false.               !Has an upper RESULT been established?

!Reset value of flags for reheater flux fraction iteration
f_rh_upper  = 1.
f_rh_lower  = 0.
rh_br_upper = 0
rh_br_lower = 0
success     = 1

checkHXs    = .false.
High_Tol    = .true.        !True = use high tolerance to quickly get ballpark results.  False = use tight tolerance for final answer
            
!********** Reheater Loop *************
!While outlet temp diff is high and iteration count is small
!Iterate on reheater flux fraction WHILE:
!   - Difference between calculated and target reheater temperature is > tolerance
!   - or the Superheater model is still being solved with a larger tolerance (because reheater fraction bracket is not set)
!   -              AND
!   - Number of reheater iterations is "small"  
93  DO WHILE( ((abs(diff_T_rh)>tol_T_rh).or.(High_Tol)) .and. (T_rh_iter<20) )    
    T_rh_iter   = T_rh_iter + 1     !Increase iteration counter
    
    diff_frh_br = f_rh_upper - f_rh_lower       !Calculate difference between brackets

    !IF(T_rh_iter == 20) CONTINUE   "Exceeds Reheater Fraction Iteration Limit"     
    

    !***************************************************************************************************
    !***** Convergence logic to zero in on correct value of fraction of incident radiation to reheater 
    !**************************************************************************************************           
    IF(T_rh_iter > 1)THEN           !After first run-through of calculations
        
        IF((fb_stuck == 0) .and. (rh_exit==0))THEN       !If reheater results are available, then we can try false position mode
        
            !Use relaxed tolerance until reheater solves once.  This will give updated an updated reheater efficiency and outlet enthalpy.
            !Use these updated values to predict reheat fraction and boiler fraction
            IF(High_Tol)THEN
                !**** New 10/4/11****
                High_Tol = .false.
                q_b_pred_sp     = (h_sh_in_ref - h_fw)*1000.d0/eta_b                ![J/kg] Predict specific energy input to boiler
                q_sh_pred_sp    = (h_sh_out_ref - h_sh_in_ref)*1000.d0/eta_sh       ![J/kg] Predict specific energy input to superheater
                q_rh_pred_sp    = (h_rh_out_ref - h_rh_in_ref)*1000.d0/eta_rh       ![J/kg] Predict specific energy input to reheater
                m_dot_ref   = Q_total_df/(q_b_pred_sp + q_sh_pred_sp + q_rh_pred_sp*f_mdot_rh)   ![kg/s] Predict cycle mass flow rate
                m_dot_guess = m_dot_ref/x_b_target                                  ![kg/s] Mass flow rate through the boiler is related to target outlet quality

                q_b_pred     = q_b_pred_sp * m_dot_ref                          ![W] Predicted rate of energy transferred to boiler
                q_sh_pred    = q_sh_pred_sp * m_dot_ref                         ![W] Predicted rate of energy transferred to superheater
                q_rh_pred    = q_rh_pred_sp * m_dot_ref * f_mdot_rh             ![W] Predicted rate of energy transferred to reheater

                f_rh_adjust    = q_rh_pred / (q_b_pred + q_sh_pred + q_rh_pred) ![-] Predicted reheater fraction
                f_b         = q_b_pred / (q_b_pred + q_sh_pred)                 ![-] Predicted boiler fraction 
        
            ELSEIF((RHupflag).and.(RHlowflag))THEN      !If upper and lower results are saved, use false position
                IF(diff_T_rh < 0.d0)THEN            !Prescribed is less than calculated, need to decrease flux on reheater/increase flux on other HXs 
                    rh_br_upper = 3
                    f_rh_upper  = f_rh
                    y_rh_upper  = diff_T_rh
                ELSE                                !Prescribed is greater than calculated, need to increase flux on reheater/decrease flux on other HXs
                    rh_br_lower = 4
                    f_rh_lower  = f_rh
                    y_rh_lower  = diff_T_rh
                ENDIF
                f_rh = (y_rh_upper)/(y_rh_upper-y_rh_lower)*(f_rh_lower - f_rh_upper) + f_rh_upper   !False position method
                    
            ELSE                                    !Try to set upper and lower results
                IF(diff_T_rh < 0.d0)THEN            !Prescribed is less than calculated, need to decrease flux on reheater/increase flux on other HXs
                    rh_br_upper = 3
                    f_rh_upper  = f_rh              !so set upper bound on reheater fraction
                    y_rh_upper  = diff_T_rh         !set upper result
                    RHupflag    = .true.            !set flag showing result is saved
                    !f_rh_adjust = 0.75 * f_rh       !may need a guess if lower bound is unknown
                    !**** New 10/4/11****
                    q_b_pred_sp     = (h_sh_in_ref - h_fw)*1000.d0/eta_b                ![J/kg] Predict specific energy input to boiler
                    q_sh_pred_sp    = (h_sh_out_ref - h_sh_in_ref)*1000.d0/eta_sh       ![J/kg] Predict specific energy input to superheater
                    q_rh_pred_sp    = (h_rh_out_ref - h_rh_in_ref)*1000.d0/eta_rh       ![J/kg] Predict specific energy input to reheater
                    m_dot_ref   = Q_total_df/(q_b_pred_sp + q_sh_pred_sp + q_rh_pred_sp*f_mdot_rh)   ![kg/s] Predict cycle mass flow rate
                    m_dot_guess = m_dot_ref/x_b_target                                  ![kg/s] Mass flow rate through the boiler is related to target outlet quality

                    q_b_pred     = q_b_pred_sp * m_dot_ref                          ![W] Predicted rate of energy transferred to boiler
                    q_sh_pred    = q_sh_pred_sp * m_dot_ref                         ![W] Predicted rate of energy transferred to superheater
                    q_rh_pred    = q_rh_pred_sp * m_dot_ref * f_mdot_rh             ![W] Predicted rate of energy transferred to reheater

                    f_rh_adjust    = q_rh_pred / (q_b_pred + q_sh_pred + q_rh_pred) ![-] Predicted reheater fraction
                    f_b         = q_b_pred / (q_b_pred + q_sh_pred)                 ![-] Predicted boiler fraction 
                        !Sometimes this predictive method gets "stuck", so need more brute force approach
                    IF( (f_rh - f_rh_adjust) < 0.0005 )       f_rh_adjust = f_rh - 0.001
                    
                ELSE                                !Prescribed is greater than calculated, need to increase flux on reheater/decrease flux on other HXs
                    rh_br_lower = 4
                    f_rh_lower  = f_rh              
                    y_rh_lower  = diff_T_rh
                    RHlowflag   = .true.
                    !f_rh_adjust = 1.25 * f_rh
                    !**** New 10/4/11****
                    q_b_pred_sp     = (h_sh_in_ref - h_fw)*1000.d0/eta_b                ![J/kg] Predict specific energy input to boiler
                    q_sh_pred_sp    = (h_sh_out_ref - h_sh_in_ref)*1000.d0/eta_sh       ![J/kg] Predict specific energy input to superheater
                    q_rh_pred_sp    = (h_rh_out_ref - h_rh_in_ref)*1000.d0/eta_rh       ![J/kg] Predict specific energy input to reheater
                    m_dot_ref   = Q_total_df/(q_b_pred_sp + q_sh_pred_sp + q_rh_pred_sp*f_mdot_rh)   ![kg/s] Predict cycle mass flow rate
                    m_dot_guess = m_dot_ref/x_b_target                                  ![kg/s] Mass flow rate through the boiler is related to target outlet quality

                    q_b_pred     = q_b_pred_sp * m_dot_ref                          ![W] Predicted rate of energy transferred to boiler
                    q_sh_pred    = q_sh_pred_sp * m_dot_ref                         ![W] Predicted rate of energy transferred to superheater
                    q_rh_pred    = q_rh_pred_sp * m_dot_ref * f_mdot_rh             ![W] Predicted rate of energy transferred to reheater

                    f_rh_adjust    = q_rh_pred / (q_b_pred + q_sh_pred + q_rh_pred)    ![-] Predicted reheater fraction
                    f_b         = q_b_pred / (q_b_pred + q_sh_pred)                 ![-] Predicted boiler fraction
                        !Sometimes this predictive method gets "stuck", so need more brute force approach
                    IF( (f_rh_adjust - f_rh) < 0.0005 )       f_rh_adjust = f_rh + 0.001
                ENDIF
                
                IF((RHupflag).and.(RHlowflag))THEN  !If current call set the final result, use false position
                    f_rh = (y_rh_upper)/(y_rh_upper-y_rh_lower)*(f_rh_lower - f_rh_upper) + f_rh_upper   !False position method
                ELSE
                    IF(((RHlowguess).or.(RHlowflag)).and.((RHupguess).or.(RHupflag)))THEN   !if each BOUND (but not result) is known, use bisection
                        !f_rh    = 0.5*f_rh_lower + 0.5*f_rh_upper   !Use bisection method if GUESSES for each end of bracket are set
                        !*** New 10/4/11
                        f_rh = f_rh_adjust
                    ELSE
                        f_rh = f_rh_adjust              !If only know one end, use guess
                    ENDIF                            
                ENDIF
            
            ENDIF

        ELSEIF(  (fb_stuck==1) .or. (rh_exit==1) .or. (rh_exit==2))THEN
                IF(fb_stuck==1)     rh_br_lower = 1
                IF(rh_exit==1)      rh_br_lower = 2
                IF(rh_exit==2)      rh_br_lower = 3
                rh_exit  = 0 
                f_rh_lower  = f_rh      !so increase reheater flux
                RHlowguess  = .true.
                RHlowflag   = .false.    
                IF((RHupflag).or.(RHupguess))THEN        !if upper bound is known, use bisection   
                    f_rh    = 0.5*f_rh_lower + 0.5*f_rh_upper
                ELSE                                    !otherwise, guess
                    f_rh    = 1.25*f_rh
                ENDIF

        ELSEIF( (fb_stuck==2) .or. (rh_exit==3) )THEN
                IF(fb_stuck==2)THEN
                    rh_br_upper = 1
                ELSEIF(rh_exit==3)THEN
                    rh_br_upper = 2
                ENDIF
                rh_exit  = 0
                f_rh_upper  = f_rh      !so decrease reheater flux
                RHupguess   = .true.    
                RHupflag    = .false.
                IF((RHlowflag).or.(RHlowguess))THEN     !if lower bound is known, use bisection
                    f_rh    = 0.5*f_rh_lower + 0.5*f_rh_upper
                ELSE
                    f_rh    = 0.75*f_rh
                ENDIF
        ENDIF                
         
    ENDIF               
    
    IF(  (abs(diff_frh_br)<0.0051).or.(T_rh_iter==20) ) THEN
    
        IF( (f_rh_upper < 0.01) .and. (rh_br_lower==0) ) rh_br_lower = 5
        
        !Set lower limit on reheat fraction
        !rh_br_lower==1: Boiler fraction is stuck: boiler/superheat needs less available flux
        !rh_br_lower==2: High mass flow through RH led to pressure drop exit
        !rh_br_lower==3: Flux is too low for RH model to solve
        !rh_br_lower==4: RH Target temperature is higher than calculated temperature
        !rh_br_lower==5: No lower limit established
        
        !Set upper limit on reheat fraction
        !rh_br_upper==1: Boiler fractionis stuck: boiler/superheat needs more available flux
        !rh_br_upper==2: The RH model coudl not solve due to low mass flow rate
        !rh_br_upper==3: RH Target temperature is lower than calculated temperature
        
        !***** NEW 8/26/11********************
        !Pairing should not occur:
        !IF( ((rh_br_lower==1).and.(rh_br_upper==1)) .or. ((rh_br_lower==3).and.(rh_br_upper==3)) .or. &
        !    ((rh_br_lower==4).and.(rh_br_upper==3)) .or. ((rh_br_lower==5).and.(rh_br_upper==2,3))
        
        !Low flux: exit receiver modeling and determine if aux heat is used to power cycle
        IF( ((rh_br_lower==2).and.(rh_br_upper==1)) .or.  &
            ((rh_br_lower==3).and.(rh_br_upper==1)) .or. ((rh_br_lower==3).and.(rh_br_upper==2)) .or. ((rh_br_lower==3).and.(rh_br_upper==3)) .or. &
            ((rh_br_lower==4).and.(rh_br_upper==1)) .or. ((rh_br_lower==4).and.(rh_br_upper==2)) .or. &
            ((rh_br_lower==5).and.(rh_br_upper==1)) ) THEN
            
            success = 0
            EXIT
        
        !Defocus
        ELSEIF( ((rh_br_lower==1).and.(rh_br_upper==2)) .or. ((rh_br_lower==1).and.(rh_br_upper==3)) .or. &
            ((rh_br_lower==2).and.(rh_br_upper==2)) .or. ((rh_br_lower==2).and.(rh_br_upper==3)) )THEN
            
            df_flag     = .true.        !Defocus is required
            defocus     = max(0.5*defocus, defocus - 0.1)   !Since defocus is required due to huge pressure drops, the system will not be operating efficiently.  Therefore,
                                                            !defocus in large increments until receiver solves.  Don't attempt to then increase defocus to optimize.
            EXIT
        
        !Need to continue iterating, may have to slightly adjust limits due to relaxed sh exit tolerance at early f_rh iterations
        ELSEIF( ((rh_br_lower==4).and.(rh_br_upper==3)).and.(abs(diff_frh_br)<0.0005) )THEN
            IF(diff_T_rh<0.d0)THEN
                    f_rh    = f_rh - 0.01
                    RHlowflag = .false.
                    f_rh_lower = f_rh
                    GOTO 778
            ELSE
                    f_rh    = f_rh + 0.01
                    RHupflag    = .false.
                    f_rh_upper = f_rh
                    GOTO 778
            ENDIF
        ENDIF
        !****************************************************
      
    ENDIF 
    
778     IF(f_rh > 1.)THEN
        f_rh_lower  = 1.
        f_rh        = 1.
    ENDIF
    
    !IF(T_rh_iter == 20) Pause   "Exceeds Reheater Fraction Iteration Limit"
    
    rh_exit = 0     !Reset reheater exit flag
                
    !***************************************************************************************************
    !***** End of reheater convergence logic 
    !************************************************************************************************** 
          
    !diff_frh_br = f_rh_upper - f_rh_lower       !Calculate difference between brackets

    Q_inc_rh    = f_rh * Q_inc * h_total/h_rh   ![W/m^2] Incident radiation: reheater  
    Q_inc_b_sh  = Q_total_df - sum(Q_inc_rh)*(h_rh/h_total)*A_panel     ![W] Incident energy on boiler and superheater                 
    
    diff_T_rh   = 999.9

    !Reset iteration variables for inner loop
    diff_T_sh   = 999.d0                        !Difference between prescribed and calculated superheater outlet temperature
    diff_f_bracket = 999.d0                     !Difference between upper and lower bounds for boiler fraction iteration
    T_sh_iter   = 0
    
    !Set limits on boiler fraction
    f_upper     = 1.
    f_lower     = 0.
    
    !Reset logic flags for boiler fraction iteration
    upflag      = .false.                       !Has an upper RESULT been established?
    lowflag     = .false.                       !Has a lower RESULT been established?
    upguess     = .false.                       !Has an upper BOUND been established?
    lowguess    = .false.                       !Has a lower BOUND been established?
    br_lower    = 0
    br_upper    = 0
    fb_stuck    = 0

    !After reheat bracket has been established, switch to tighter tolerance on superheater outlet
!        IF(((rh_br_upper > 0).and.(rh_br_lower > 0)))THEN
!            tol_T_sh = tol_T_sh_base
!        ELSE
!            tol_T_sh = tol_T_sh_high
!        ENDIF

    IF(High_Tol)THEN
        tol_T_sh = tol_T_sh_high
    ELSE
        tol_T_sh = tol_T_sh_base
    ENDIF
    
    !*********************************************************************************************
    !***** Loop to determine the fraction of reheat on boiler, given some fixed amount on reheater
    !*********************************************************************************************
184     DO WHILE( ((abs(diff_T_sh)>tol_T_sh).and.(T_sh_iter<20)) )
    
        T_sh_iter   = T_sh_iter + 1             !Add to iteration counter
        diff_f_bracket  = f_upper - f_lower     !Difference between upper and lower bracket guesses
        
        !IF(T_sh_iter == 20) CONTINUE "Exceeds Boiler Fraction Iteration Limit"
        !IF(T_sh_iter == 20) Pause "Exceeds Boiler Fraction Iteration Limit"

        !***********************************************************************************************************************************************
        !***** Convergence logic to zero in on correct value of fraction of incident radiation to boiler given a total available to boiler and superheater
        !***********************************************************************************************************************************************
        IF(T_sh_iter > 1)THEN                   !After first run, need to recalculate 'boiler fraction'
        
            IF((sh_exit == 0).and.(boiler_exit==0))THEN                !If low pressure flag is not tripped (i.e. there is a "result") then begin/continue setting values for false interpolation
        
                IF((upflag).and.(lowflag))THEN      !If results are available for both ends of bracket, use false interpolation
                    
                    IF(diff_T_sh > 0.d0)THEN        !Prescribed is greater than calculated, need to decrease mass flow/increase flux on SH, so reduce flux on boiler, set upper limit
                        br_upper    = 3
                        f_upper     = f_b
                        y_upper     = diff_T_sh
                    ELSE                            !Prescribed is less than calculated, need to increase mass flow/decrease flux on SH, so increase flux on boiler, set lower limit
                        br_lower    = 3
                        f_lower     = f_b
                        y_lower     = diff_T_sh
                    ENDIF
                    f_b = (y_upper)/(y_upper-y_lower)*(f_lower - f_upper) + f_upper   !False position method
                ELSE

                    IF(diff_T_sh > 0.d0)THEN        !Prescribed is greater than calculated, need to decrease mass flow/increase flux on SH, so reduce flux on boiler, set upper limit
                        br_upper    = 3
                        f_upper     = f_b
                        y_upper     = diff_T_sh
                        upflag      = .true.
                        !f_adjust = f_b - 0.01
                        !******New 10/4*********
                        q_b_pred_sp = (h_sh_in_ref - h_fw)*1000.d0/eta_b            ![J/kg] Predict specific energy input to boiler
                        q_sh_pred_sp = (h_sh_out_ref - h_sh_in_ref)*1000.d0/eta_sh  ![J/kg] Predict specific energy input to superheater
                        m_dot_ref   = (q_inc_b_sh)/(q_b_pred_sp + q_sh_pred_sp)     ![kg/s] Predict mass flow rate
                        m_dot_guess = m_dot_ref/x_b_target
                        f_adjust    = q_b_pred_sp*m_dot_ref/q_inc_b_sh              ![-] Predict boiler fraction   
                        !Know that boiler fraction needs to be decreased, so make sure that happens
                        IF( (f_b - f_adjust)<0.0005)        f_adjust = f_b - 0.001   
                        !Also, if we know lower bound, then make sure we stay within 10/16
                        IF( (f_adjust < f_lower) )          f_adjust = 0.8*f_lower + 0.2*f_upper
                    ELSE
                        br_lower    = 3
                        f_lower     = f_b
                        y_lower     = diff_T_sh
                        lowflag     = .true.
                        !f_adjust = f_b + 0.01
                        !******New 10/4*********
                        q_b_pred_sp = (h_sh_in_ref - h_fw)*1000.d0/eta_b            ![J/kg] Predict specific energy input to boiler
                        q_sh_pred_sp = (h_sh_out_ref - h_sh_in_ref)*1000.d0/eta_sh  ![J/kg] Predict specific energy input to superheater
                        m_dot_ref   = (q_inc_b_sh)/(q_b_pred_sp + q_sh_pred_sp)     ![kg/s] Predict mass flow rate
                        m_dot_guess = m_dot_ref/x_b_target
                        f_adjust    = q_b_pred_sp*m_dot_ref/q_inc_b_sh              ![-] Predict boiler fraction 
                        !Know that boiler fraction need to be increased, so make sure that happens
                        IF( (f_adjust - f_b)<0.0005)        f_adjust = f_b + 0.001
                        !Also, if we know upper bound, then make sure we stay within 10/16
                        IF( (f_adjust > f_upper) )          f_adjust = 0.8*f_upper + 0.2*f_lower
                    ENDIF
                
                    IF((upflag).and.(lowflag))THEN
                        f_b = (y_upper)/(y_upper-y_lower)*(f_lower - f_upper) + f_upper   !False position method
                    ELSEIF(((upguess).and.(lowflag)).or.((lowguess).and.(upflag)))THEN
                        !f_b     = 0.5*f_upper + 0.5*f_lower
                        !*****New 10/4 ******
                        f_b     = f_adjust
                    ELSE    
                        f_b     = f_adjust
                    ENDIF        

                ENDIF
            
            ELSEIF((boiler_exit==2).or.(sh_exit==3))THEN            !Boiler model did not solve: requires higher flux to solve OR superheater did not solve, needs less flux/mass flow
                IF(boiler_exit==2)THEN
                    br_lower    = 1
                ELSE
                    br_lower    = 2
                ENDIF
                f_lower     = f_b
                lowguess    = .true.
                IF((upflag).or.(upguess))THEN
                    f_b     = 0.5*f_upper + 0.5*f_lower 
                ELSE
                    f_b     = f_b + 0.05
                ENDIF
                
            ELSEIF((sh_exit==1).or.(sh_exit==2).or.(boiler_exit==3))THEN    !Superheater model did not solve or boiler could not due to high flux: requires that boiler has less flux (sh_exit = 1 or 2)
                IF(sh_exit==1)      br_upper    = 1
                IF(sh_exit==2)      br_upper    = 2
                IF(boiler_exit==3)  br_upper    = 5

                f_upper     = f_b
                upguess     = .true.
                IF((lowguess).or.(lowflag))THEN
                    f_b     = 0.5*f_upper + 0.5*f_lower
                ELSE
                    f_b     = f_b - 0.05         !Decrease boiler fraction: if flag is tripped, then logically upflag should be false
                ENDIF
            ENDIF
        
        ENDIF
        
        IF(f_b > 1.)THEN
            f_b         = 1.
        ENDIF
        
        boiler_exit = 0
        sh_exit     = 0
        
        !***********************************************************************************************************************************************
        !***** End of convergence (boiler)
        !***********************************************************************************************************************************************

        IF( (abs(diff_f_bracket)<bracket_tol) .or. (T_sh_iter==20) )THEN
            
            IF( (f_lower > 0.99) .and. (br_upper==0) )  br_upper = 4
            
            !******* NEW **************************************
            !**** Set lower limit on boiler fraction *********
            !br_lower==1: Boiler exit = 2 -> Boiler cannot solve due to low flux
            !br_lower==2: SH exit = 3     -> Superheater requires an increase in mass flow to solve
            !br_lower==3: The calculated SH outlet temperature is higher than the target temperature
            
            !**** Set upper limit on boiler fraction **********
            !br_upper==1: SH exit = 1     -> Superheater requires a decrease in mass flow to solve
            !br_upper==2: SH exit = 2     -> Superheater cannot solve due to low flux
            !br_upper==3: The calculated SH outlet temperature is lower than the target temperature
            !br_upper==4: No upper limit has been set
            !br_upper==5: Boiler exit = 3 -> Boiler cannot solve due to high flux
            
            IF( ((br_lower==1).and.(br_upper==2)) .or. ((br_lower==1).and.(br_upper==3)) .or. ((br_lower==1).and.(br_upper==4)) .or.&
                ((br_lower==2).and.(br_upper==2)) .or. ((br_lower==2).and.(br_upper==3)) .or. ((br_lower==2).and.(br_upper==4)) ) THEN
                
                fb_stuck = 2
                GOTO 93
                
            ELSEIF(  ((br_lower==2).and.(br_upper==1)) .or. ((br_lower==2).and.(br_upper==5)) .or. &
                        ((br_lower==3).and.(br_upper==1)) .or. ((br_lower==3).and.(br_upper==5)) )THEN

                fb_stuck = 1
                GOTO 93

            ELSEIF( ((br_lower==1).and.(br_upper==1)) .or. ((br_lower==1).and.(br_upper==5)) .or. &
                    ((br_lower==3).and.(br_upper==2)) .or. ((br_lower==3).and.(br_upper==4)) )THEN
                CONTINUE

            ELSEIF(  ((br_lower==3).and.(br_upper==3))  )  THEN
                GOTO 1075
            ENDIF                
            
            !**************************************************

        ENDIF 
        
        Q_inc_b = f_b*(1.-f_rh)*Q_inc*h_total/h_b               ![W/m^2] Portion of total allocatable flux to boiler 
                                                                                                                                                                                                    
        Q_inc_sh = (1.-f_b)*(1.-f_rh)*Q_inc*h_total/h_sh        ![W/m^2] Portion of total allocatable flux to superheater                                              
        
        m_dot_lower = 0.75*m_dot_guess/mguessmult
        m_dot_upper = 1.35*m_dot_guess*mguessmult          
        
        !Set boiler inputs
        XIN_b(1)  = T_amb             ![K]
        XIN_b(2)  = T_sky             ![K]
        XIN_b(3)  = v_wind            ![m/s]
        XIN_b(4)  = P_atm             ![Pa]
        XIN_b(5)  = T_fw              ![K] Feedwater outlet temperature (@ HP inlet pressure)
        XIN_b(6)  = P_b_in            ![kPa] Boiler Inlet Pressure
        XIN_b(7)  = x_b_target        ![-] Maximum allowable outlet quality
        XIN_b(8)  = m_dot_guess       ![kg/s] Guessed mass flow rate through boiler
        XIN_b(9)  = m_dot_lower       ![kg/s] Lower bound on possible mass flow rate
        XIN_b(10) = m_dot_upper       ![kg/s] Upper bound on possible mass flow rate  
        
        IF(checkHXs)THEN        !increased flux is required to run boiler, so want to make sure any flux distribution minimally works for each HX
            checkflux = 1
            boiler_exit = 0
            !First, check boiler. 
            call Boiler(TIME,XIN_b,XOUT_b,PAR_b,INFO,N_panels,Q_inc_b,boiler_exit,checkflux)
            IF(boiler_exit==2) GOTO 184
            
            !If code reaches this point, check superheater
            !Set superheater inputs
            XIN_sh(1) = T_amb             ![K]
            XIN_sh(2) = T_sky             ![K]
            XIN_sh(3) = v_wind            ![m/s]
            XIN_sh(4) = P_atm             ![Pa]
            XIN_sh(5) = T_boil            ![K]
            XIN_sh(6) = 1.0               ![kg/s] Just need a dummy variable to pass
            XIN_sh(7) = 1.0
            
            
            sh_exit = 0
            call Superheater(INFO,XIN_sh,XOUT_sh,N_panels,PAR_sh,Q_inc_sh,h_sh_target,sh_exit,checkflux)
            IF(sh_exit==2) GOTO 184
            
            !Set reheater inputs
            XIN_rh(1) = T_amb           ![K]
            XIN_rh(2) = T_sky           ![K]
            XIN_rh(3) = v_wind          ![m/s]
            XIN_rh(4) = P_atm           ![Pa]
            XIN_rh(5) = T_HP_out        ![K] Reheat Inlet Temp
            XIN_rh(6) = 1.0             ![kg/s] Value not used in flux check mode
            XIN_rh(7) = 1.0             ![kPa] Value not used in flux check mode
            XIN_rh(8) = 1.0             ![Pa] Value not used in flux check mode    

            !Solve for reheater outlet conditions.
            rh_exit = 0
            CALL Reheater(INFO,XIN_rh,XOUT_rh,N_panels,PAR_rh,Q_inc_rh,h_rh_target,rh_exit,checkflux)
            IF(rh_exit == 2)THEN
                GOTO 93
            ENDIF
            
        ENDIF
                        
        !In order to carry values from previous call, need to change compiler settings (if starting new project).  This can be accomplished by
        !Project->Properties->Configuration Properties->FORTRAN->Data.  Then, change Local Variable Storage to: "All Variables SAVE (/Qsave)"
        !and Initialize Local Saved Scalars to Zero to: Yes(/QZero).  This change must be made for both Debug and Release modes.
        checkflux = 0
        boiler_exit = 0
        boiler_count = boiler_count + 1
        !write(unit=12,FMT='(I2,TR2,I2,TR2,I2,TR2,F6.4,TR2,F6.4,TR2,F12.0)') df_count, T_rh_iter, T_sh_iter, f_rh, f_b, Q_inc_b(1)
        call Boiler(TIME,XIN_b,XOUT_b,PAR_b,INFO,N_panels,Q_inc_b,boiler_exit,checkflux)
        IF(boiler_exit > 0)THEN
!                m_dot_lower = 0.5*m_dot_guess
!                m_dot_upper = 1.5*m_dot_guess
            IF(boiler_exit==1) THEN
                success     = 0
                GOTO 375
            ENDIF
            IF(boiler_exit==2) THEN
                checkHXs    = .true.
                GOTO 184 
            ENDIF 
            IF(boiler_exit==3) THEN
                GOTO 184
            ENDIF
        ENDIF                                     
              
        eta_b   = XOUT_b(4)         ![-] Boiler efficiency
        T_boil  = XOUT_b(7)         ![K] Boiling / Steam drum temperature
        m_dot_sh = XOUT_b(2)        ![kg/s] Mass flow rate to superheater (mass flow rate through boiler * outlet quality)
        h_fw    = XOUT_b(16)        ![kJ/kg] Feedwater enthalpy
        h_sh_in_ref = XOUT_b(18)    ![kJ/kg] Superheater inlet enthalpy
        m_dot_guess = m_dot_sh/x_b_target ![kg/s]  
        dp_b    = (P_b_in - XOUT_b(13))*1.e3    ![Pa] Pressure drop through boiler                          
                     
        !Set superheater inputs
        XIN_sh(1) = T_amb             ![K]
        XIN_sh(2) = T_sky             ![K]
        XIN_sh(3) = v_wind            ![m/s]
        XIN_sh(4) = P_atm             ![Pa]
        XIN_sh(5) = T_boil            ![K]  Saturation temperature of boiler outlet pressure
        XIN_sh(6) = m_dot_sh          ![kg/s]   Mass flow rate through superheater
        XIN_sh(7) = P_sh_out_min      ![Pa]     Minimum allowable outlet pressure

        !Call steam props, get superheater target outlet enthalpy      
        !P(kPa),T(C),enth(kJ/kg),dens(kg/m3),inte(kJ/kg),entr(kJ/kg-K),cp(kJ/kg-K),cond(W/m-K),visc(kg/m-s)
        CALL Water_TP(T_sh_target-273.15d0,XOUT_b(13),enth=h_sh_target)
        h_sh_target = h_sh_target*1000.d0

        sh_count = sh_count + 1
        !Solve for superheater outlet conditions. 'sh_exit' is a flag representing how the code ended
        call Superheater(INFO,XIN_sh,XOUT_sh,N_panels,PAR_sh,Q_inc_sh,h_sh_target,sh_exit,checkflux)
        IF(sh_exit>0)THEN                
            GOTO 184      !If SH did not solve, don't need following calculations
        ENDIF
        
        !Get important outputs from call to superheater
        !T_sh_out   = XOUT_sh(2)          ![K]  
        
        P_sh_out    = XOUT_sh(11)/1000.d0   ![kPa] Superheater outlet pressure
        
        eta_sh      = XOUT_sh(1)            ![-] Superheater efficiency
        rho_sh_out  = XOUT_sh(12)           ![kg/m^3] Superheater outlet density
        h_sh_out    = XOUT_sh(5)/1000.d0    ![kJ/kg] Superheater outlet enthalpy
        
        CALL water_TP(T_sh_target - 273.15d0, P_sh_out, enth = h_sh_out_ref)
        
        dp_sh_down  = rho_sh_out * 9.81 * THT   ![Pa] Pressure due to tower elevation
        
        P_HP_in     = P_sh_out + dp_sh_down/1000.d0     ![kPa] Pressure at HP turbine inlet (bottom of tower)
        CALL water_PH(P_HP_in, h_sh_out, temp = T_sh_out)  ![C] Outlet temperature at bottom of tower assuming no heat transfer
        T_sh_out       = T_sh_out + 273.15d0      ![K] Convert from C to K                        
                    
        diff_T_sh       = (T_sh_target - T_sh_out)/T_sh_target   ![K]
        
    ENDDO
    
    IF((T_sh_iter==20).and.(abs(diff_T_sh)>tol_T_sh))THEN
        success=0
        call messages(-1,"The receiver model did converge at this timestep (SH)",'WARNING',INFO(1),INFO(2))
        GOTO 375
    ENDIF
    
1075    CONTINUE
           
    !Call steam props, get reheater target outlet enthalpy      
    !P(kPa),T(C),enth(kJ/kg),dens(kg/m3),inte(kJ/kg),entr(kJ/kg-K),cp(kJ/kg-K),cond(W/m-K),visc(kg/m-s)
    CALL Water_TP(T_rh_target-273.15d0,P_rh_in,enth=h_rh_target)
    h_rh_target = h_rh_target*1000.d0
    
    m_dot_rh = f_mdot_rh * m_dot_sh     !Caculate mass flow rate through reheater based on specified fraction
    
    !Call reheat model, adjust flux as necessary to achieve outlets
    !Set superheater inputs
    XIN_rh(1) = T_amb             ![K]
    XIN_rh(2) = T_sky             ![K]
    XIN_rh(3) = v_wind            ![m/s]
    XIN_rh(4) = P_atm             ![Pa]
    XIN_rh(5) = T_rh_in           ![K]      Reheater inlet temperature
    XIN_rh(6) = m_dot_rh          ![kg/s]   
    XIN_rh(7) = P_rh_in           ![kPa]    Reheater inlet pressure
    XIN_rh(8) = P_rh_out_min      ![Pa]     Minimum allowable outlet pressure

    !Solve for reheater outlet conditions. 'rh_exit' is a flag representing how the code ended
    rh_exit = 0
    rh_count = rh_count + 1
    CALL Reheater(INFO,XIN_rh,XOUT_rh,N_panels,PAR_rh,Q_inc_rh,h_rh_target,rh_exit,checkflux)
    IF(rh_exit > 0) GOTO 93             
          
    !Reheater Outputs
    !T_rh_out    = XOUT_rh(2)      ![K]
    
    eta_rh      = XOUT_rh(1)            ![-] Reheater efficiency
    P_rh_out    = XOUT_rh(10)/1000.d0   ![kPa] Reheater outlet pressure
    rho_rh_out  = XOUT_rh(11)           ![kg/m^2] Reheater outlet density
    h_rh_out    = XOUT_rh(5)            ![kJ/kg] Reheater outlet enthalpy
    
    call Water_TP(T_rh_target-273.15d0,P_rh_out, enth = h_rh_out_ref)
    
    dp_rh_down  = rho_rh_out * 9.81 * THT   ![Pa] Pressure due to tower elevation
    
    P_LP_in     = P_rh_out + dp_rh_down/1000.d0         ![kPa] Pressure at LP turbine inlet (bottom of tower)
    CALL water_PH(P_LP_in, h_rh_out, temp = T_rh_out)   ![C] Outlet temperature at bottom of tower assuming no heat transfer        
    T_rh_out    = T_rh_out + 273.15d0                   ![K] Convert from C to K 
           
    diff_T_rh   = (T_rh_target - T_rh_out)/T_rh_target  ![K]
    
    !Logic that allows code to solve on first iteration if SH and RH are both within final tolerance
    IF( (High_Tol) .and. (abs(diff_T_sh)<tol_T_sh_base) .and. (abs(diff_T_rh)>tol_T_rh) )THEN
        High_Tol = .false.
    ENDIF

ENDDO       !Reheat loop

IF((T_rh_iter==20).and.(abs(diff_T_rh)>tol_T_rh))THEN
    success=0
    call messages(-1,"The receiver model did converge at this timestep (RH)",'WARNING',INFO(1),INFO(2))
ENDIF

375 CONTINUE

IF(success==0) THEN
    m_dot_sh = 0
    m_dot_rh = 0
    E_su = Q_rec_des * rec_qf_delay
    t_su = rec_su_delay
    dpSH = 0.d0 ![Pa] Pressure drop through superheater
    dpRH = 0.d0  
    Q_therm_in_b = 0.d0
    Q_therm_in_sh = 0.d0
    Q_therm_in_rh = 0.d0
    Q_therm_in_rec = 0.d0  
    P_cond = 0.d0     
    deltaP1 = 10.e6     !Set high so that difference in pressures trends towards 0
    W_dot_fw = 0.d0 
    W_dot_boost = 0.d0
ELSE
    IF((eta_lin_approx).and.(defocus>0.999d0))THEN
        IF( (Q_total < (0.75*q_rec_min+0.25*q_pb_design)) .and. (.not.(q_low_set)) )THEN
            Q_total_low = Q_total
            eta_rh_low = XOUT_rh(1)
            eta_sh_low = XOUT_sh(1)
            eta_b_low = eta_b
            q_low_set = .true.
        ENDIF
        IF( (Q_total > 0.85*q_pb_design) .and. (.not.(q_high_set)) )THEN
            Q_total_high = Q_total
            eta_rh_high = XOUT_rh(1)
            eta_sh_high = XOUT_sh(1)
            eta_b_high  = eta_b
            q_high_set  = .true.
        ENDIF             
        IF((q_low_set).and.(q_high_set))THEN
            eta_lin_approx = .false.
        ENDIF
    ENDIF
      
    h_fw_Jkg    = XOUT_b(16)*1000.d0    ![J/kg] Feedwater enthalpy 
    rho_fw      = XOUT_b(17)            ![kg/m^3] Feedwater density
    !dpSH        = XOUT_sh(8)    ![Pa] Pressure drop through superheater
    dpSH        = (XOUT_b(13) - P_HP_in)*1000.d0    ![Pa] Pressure drop through superheater (accounting for down tower)
    !dpRH        = XOUT_rh(9)   ![Pa] Pressure drop through reheater (only receiver, not vertical piping)
    dpRH        = (P_HP_out - P_LP_in)*1000.d0  ![Pa] Pressure drop through reheater (accounting for up and down tower)
    Q_therm_in_b     = XOUT_b(8)            ![W] Rate of energy transferred to boiler 
    Q_therm_in_sh    = XOUT_sh(4)           ![W] Rate of energy transferred to SH
    Q_therm_in_rh    = XOUT_rh(4)           ![W] Rate of energy transferred to RH
    Q_therm_in_rec   = Q_therm_in_b + Q_therm_in_sh + Q_therm_in_rh ![W] Rate of energy transferred to receiver
    deltaP1     = rho_fw*9.81d0*THT         ![Pa] Pressure drop due to pumping feedwater up tower
    W_dot_fw    = (deltaP1 + max(0.d0,dpSH))*m_dot_sh/rho_fw        ![W] Power required to pump feedwater up tower AND increase pressure from HP turbine inlet to steam drum pressure
    W_dot_sd    = max(0.d0,dp_b)*(m_dot_sh/x_b_target)/XOUT_b(15)        ![W] Power required to pump boiler flow from steam drum pressure to boiler inlet pressure
    W_dot_boost = (W_dot_fw + W_dot_sd)/eta_fw_pump         ![W] Total pumping power required (that is not already accounted for in regression model) 
    !11/3/11, TN: calculate non-dimensional mass flow rate
    m_dot_ND    = m_dot_sh/m_dot_des        ![-] 
       
ENDIF

!Q_therm_in_diff = (Q_therm_in_rec - q_pb_max)/q_pb_max
!IF( (Q_therm_in_diff > 0.005) .or. ((Q_therm_in_diff < -0.005).and.(defocus<1).and.(.not.(df_flag))) )THEN
!    !Q_df_flag = .true.          !11/4/11, TN: Add flag specifying we are in "energy defocus mode"
!    defocus = min(1.d0,(defocus*(q_pb_max/Q_therm_in_rec)))
!    GOTO 589
!ENDIF

IF( ((m_dot_ND-tfrac)/tfrac>0.005) .or. (((m_dot_ND-tfrac)/tfrac < -0.005).and.(defocus<1).and.(.not.(df_flag))) )THEN
    
    IF(df_upflag)THEN
        IF((m_dot_ND-tfrac)>0.d0)THEN
            df_upper = defocus
            y_df_upper = m_dot_ND-tfrac
        ELSE
            df_lower = defocus
            y_df_lower = m_dot_ND-tfrac
        ENDIF
        defocus = (y_df_upper)/(y_df_upper-y_df_lower)*(df_lower - df_upper) + df_upper
    ELSE
        IF((m_dot_ND-tfrac)>0.d0)THEN       
            df_upflag = .true.
            df_upper = defocus
            y_df_upper = m_dot_ND-tfrac
        ELSE
            df_lowflag = .true.
            df_lower   = defocus
            y_df_lower = m_dot_ND - tfrac
        ENDIF
        IF((df_upflag).and.(df_lowflag))THEN
            defocus = (y_df_upper)/(y_df_upper-y_df_lower)*(df_lower - df_upper) + df_upper
        ELSE
            defocus = min(1.d0,(defocus*(tfrac/m_dot_ND)))
        ENDIF
        
    ENDIF            
        
    GOTO 589
ENDIF


!********************************** 
!Auxiliary Heating Contribution
!**********************************
!Is aux heating available, and if so, which mode?
1529 continue
m_dot_aux   = 0.d0
Q_aux       = 0.d0
Q_aux_fuel  = 0.d0
f_timestep  = 1.d0 

!Energy based controls
!10/18/11, TN: Allow fossil dispatch when receiver is not operating
!IF((ffrac(TOUPeriod)>0))THEN
!
!    !If receiver solves, then define fossil cycle from receiver model
!     IF(success==1)THEN
!        h_HP_in = XOUT_sh(5)    ![J/kg] Inlet enthalpy to high pressure turbine
!        h_LP_in = XOUT_rh(5)*1000.d0    ![J/kg] Inlet enthalpy to low pressure turbine
!        h_rh_in = XOUT_rh(6)    ![J/kg] Inlet enthalpy to reheater   
!        
!    !If receiver does not solve, use type inputs to define a fossil cycle
!    ELSEIF(success==0)THEN
!        CALL water_TP(T_sh_target-273.15d0,P_b_in, enth = h_HP_in)
!        h_HP_in = h_HP_in * 1000.d0     ![J/kg] Convert from kJ/kg
!        CALL water_TP(T_fw,P_b_in, enth = h_fw,dens = rho_fw)
!        h_fw_Jkg = h_fw * 1000.d0       ![J/kg] Convert from kJ/kg
!        CALL water_TP(T_rh_target,P_HP_out, enth = h_LP_in)
!        h_LP_in = h_LP_in * 1000.d0     ![J/kg] Convert from kJ/kg
!        CALL water_TP(T_rh_in,P_HP_out,  enth = h_rh_in)
!        h_rh_in = h_rh_in * 1000.d0     ![J/kg] Convert from kJ/kg
!    ENDIF
!
!    IF(fossil_mode==1)THEN
!        !If fossil fraction is less than minimum turbine fraction (poor specification in fossil mode 1, but it could happen..)
!        !or
!        !If the heat rate corresponding to the fossil fraction is less than the heat rate absorbed by the receiver
!        IF( (ffrac(TOUPeriod)<f_pb_cutoff) .or. ((ffrac(TOUPeriod)*q_pb_design) < Q_therm_in_rec) )THEN    
!            Q_aux   = 0.d0                                      ![W]
!        ELSE    !Calculate auxiliary heat rate
!            Q_aux = ffrac(TOUPeriod)*q_pb_design - Q_therm_in_rec    ![W]
!        ENDIF
!    ELSEIF(fossil_mode==2)THEN
!        Q_aux   = ffrac(TOUPeriod)*q_pb_design                  ![W]
!        IF((Q_aux + Q_therm_in_rec)<q_pb_min)THEN
!            Q_aux = 0.d0                    ![W] Aux contribution must be enough for normal turbine operation
!        ELSEIF((Q_aux + Q_therm_in_rec) > q_pb_max)THEN
!            Q_aux = q_pb_max - Q_therm_in_rec    ![W] Don't let total heat rate be greater than allowed
!        ENDIF
!    ENDIF         
!    
!    IF( (ISNAN(Q_aux)) .or. (Q_aux<0.d0) )THEN
!        continue
!    ENDIF
!    
!    m_dot_aux   = Q_aux / ((h_HP_in - h_fw_Jkg) + f_mdot_rh*(h_LP_in - h_rh_in))    ![kg/s] mass flow rate of steam through parallel aux. path
!    IF( (((m_dot_aux+m_dot_sh)/m_dot_des)-tfrac)/tfrac > 0.005 )THEN    !If combined mass flow rate to cycle is greater than allowed, reduce aux contribution
!        m_dot_aux1 = tfrac*m_dot_des - m_dot_sh     ![kg/s] Adjusted aux mass flow rate
!        Q_aux   = Q_aux * (m_dot_aux1/m_dot_aux)    ![W] Set auxiliary to adjusted auxiliary heat rate
!        m_dot_aux = m_dot_aux1                      ![kg/s] Set mass flow to adjusted
!    ENDIF 
!    q_aux_fuel  = Q_aux / lhv_eff *dt * 3.41214116E-6                           ![MMBTU] Fuel energy usage (convert W-hr to MMBTU)      
!    W_dot_aux   = max(0.d0,dpSH)*m_dot_aux/rho_fw                               ![W] Power required to increase pressure from HP turbine inlet to steam drum pressure
!    W_dot_boost = W_dot_boost + W_dot_aux                                       ![W] Final pumping power must include aux pump power
!    
!ENDIF

!Mass flow rate controls
!10/18/11, TN: Allow fossil dispatch when receiver is not operating
IF((ffrac(TOUPeriod)>0))THEN

    !If receiver solves, then define fossil cycle from receiver model
     IF(success==1)THEN
        h_HP_in = XOUT_sh(5)    ![J/kg] Inlet enthalpy to high pressure turbine
        h_LP_in = XOUT_rh(5)*1000.d0    ![J/kg] Inlet enthalpy to low pressure turbine
        h_rh_in = XOUT_rh(6)    ![J/kg] Inlet enthalpy to reheater   
        
    !If receiver does not solve, use type inputs to define a fossil cycle
    ELSEIF(success==0)THEN
        CALL water_TP(T_sh_target-273.15d0,P_b_in, enth = h_HP_in)
        h_HP_in = h_HP_in * 1000.d0     ![J/kg] Convert from kJ/kg
        CALL water_TP(T_fw-273.15d0,P_b_in, enth = h_fw,dens = rho_fw)
        h_fw_Jkg = h_fw * 1000.d0       ![J/kg] Convert from kJ/kg
        CALL water_TP(T_rh_target-273.15d0,P_HP_out, enth = h_LP_in)
        h_LP_in = h_LP_in * 1000.d0     ![J/kg] Convert from kJ/kg
        CALL water_TP(T_rh_in-273.15d0,P_HP_out,  enth = h_rh_in)
        h_rh_in = h_rh_in * 1000.d0     ![J/kg] Convert from kJ/kg
    ENDIF

    IF(fossil_mode==1)THEN
        !If fossil fraction is less than minimum turbine fraction (poor specification in fossil mode 1, but it could happen..)
        !or
        !If the heat rate corresponding to the fossil fraction is less than the heat rate absorbed by the receiver
        IF( (ffrac(TOUPeriod)<f_pb_cutoff) .or. ((ffrac(TOUPeriod)*m_dot_des) < m_dot_sh) )THEN    
            m_dot_aux   = 0.d0                                      ![kg/s]
        ELSE    !Calculate auxiliary heat rate
            m_dot_aux   = ffrac(TOUPeriod)*m_dot_des - m_dot_sh     ![kg/s]
        ENDIF
    ELSEIF(fossil_mode==2)THEN
        m_dot_aux   = ffrac(TOUPeriod)*m_dot_des
        IF((m_dot_aux + m_dot_sh)<(f_pb_cutoff*m_dot_des))THEN
            m_dot_aux   = 0.d0
        ELSEIF( (((m_dot_aux+m_dot_sh)/m_dot_des)-tfrac)/tfrac > 0.005 )THEN
            m_dot_aux   = tfrac*m_dot_des - m_dot_sh                        
        ENDIF
    ENDIF         
    
    IF(INFO(8)==3)THEN
        m_dot_aux = 0.d0        !Time = Start time, no iteration
    ENDIF
    
    Q_aux   = m_dot_aux*((h_HP_in - h_fw_Jkg) + f_mdot_rh*(h_LP_in - h_rh_in))      ![W]

    q_aux_fuel  = Q_aux / lhv_eff *dt * 3.41214116E-6                           ![MMBTU] Fuel energy usage (convert W-hr to MMBTU)      
    W_dot_aux   = max(0.d0,dpSH)*m_dot_aux/rho_fw                               ![W] Power required to increase pressure from HP turbine inlet to steam drum pressure
    W_dot_boost = W_dot_boost + W_dot_aux                                       ![W] Final pumping power must include aux pump power
    
ENDIF

Q_aux_rec   = Q_aux + Q_therm_in_rec     ![W] Total (auxiliary + incident absorbed) energy to power block
m_dot_toPB  = m_dot_aux + m_dot_sh  ![kg/s] Total mass flow through HP turbine is sum of aux and sh mass flow since they operate in parallel 
E_aux       = Q_aux * dt            ![W-hr] Energy added by aux heater in current timestep

!Energy-based controls
!IF(Q_aux_rec > q_pb_min)THEN
!    standby_control = 1                 ![-] Power cycle is in normal operation
!    T_standby       = T_standby_ini     ![hr] If cycle is in normal operation, then full standby time is available
!ELSEIF( (Q_aux_rec>q_sb_min) .and. ((T_standby0 - dt)>0.d0) )THEN
!    standby_control = 2                 ![-] Power cycle is in standby operation
!    T_standby       = T_standby0 - dt   ![hr] Subtract timestep from available standby time
!ELSE
!    standby_control = 3                 ![-] Power cycle is off
!    T_standby       = 0.d0              ![hr] No standby time remaining
!ENDIF

!Mass flow rate based controls
IF(m_dot_toPB > f_pb_cutoff*m_dot_des)THEN
    standby_control = 1                 ![-] Power cycle is in normal operation
    T_standby       = T_standby_ini     ![hr] If cycle is in normal operation, then full standby time is available
ELSEIF( (m_dot_toPB > f_pb_sb*m_dot_des) .and. ((T_standby0 - dt)>0.d0) )THEN
    standby_control = 2                 ![-] Power cycle is in standby operation
    T_standby       = T_standby0 - dt   ![hr] Subtract timestep from available standby time
ELSE
    standby_control = 3                 ![-] Power cycle is off
    T_standby       = 0.d0              ![hr] No standby time remaining
ENDIF

!Has receiver completed startup?
!IF( ((E_su0 > 0.d0).or.(t_su0 > 0.d0)) .and. (success > 0))THEN
!    q_startup   = dmin1(E_su0, (Q_therm_in_rec)*dt) ![W-hr] Startup energy during current timestep
!    E_su        = dmax1(0.d0, E_su0 - (Q_therm_in_rec)*dt)     ![W-hr] Subtract energy added in current timestep to find remaining required energy
!    t_su        = dmax1(0.d0, t_su0 - dt)
!    IF(E_su + t_su > 0.d0)THEN
!        f_timestep = 0.d0       !Receiver has not completed start-up at end of timestep
!    ELSE
!        f_timestep = dmin1( (1.d0 - t_su0/dt), E_su0/(Q_therm_in_b + Q_therm_in_sh + Q_therm_in_rh)*dt )    !Receiver has completed startup during timestep
!    ENDIF
!ENDIF    

!11/17/11, TN: Try considering aux heat in receiver startup
!Has receiver completed startup?
IF( ((E_su0 > 0.d0).or.(t_su0 > 0.d0)) .and. (success > 0))THEN
    q_startup   = dmin1(E_su0, (Q_therm_in_rec)*dt) ![W-hr] Startup energy during current timestep
    E_su        = dmax1(0.d0, E_su0 - (Q_therm_in_rec)*dt)     ![W-hr] Subtract energy added in current timestep to find remaining required energy
    t_su        = dmax1(0.d0, t_su0 - dt)
    IF(E_su + t_su > 0.d0)THEN
        f_timestep = 0.d0       !Receiver has not completed start-up at end of timestep
    ELSE
        f_timestep = dmin1( (1.d0 - t_su0/dt), E_su0/(Q_therm_in_b + Q_therm_in_sh + Q_therm_in_rh)*dt )    !Receiver has completed startup during timestep
    ENDIF
    IF(m_dot_aux > 0.d0)THEN
        f_timestep = m_dot_sh/m_dot_toPB*f_timestep + m_dot_aux/m_dot_toPB*1.d0     !Adjust f_timestep to allow aux mass flow rate to be used for power if receiver is starting up
        f_timestep = min(1.d0, f_timestep)
    ENDIF
ENDIF

EnergyInComb    = XOUT_b(10) + XOUT_sh(10) + XOUT_rh(8)        ![W] Flux * receiverArea

!If mass flow rate and pressure drops do not change within tolerance (0.005) then we can assume that
!return conditions from the power block will be the same, so end loop (rather than rely on TRNSYS internal tolerance)
!Specify relative tolerance for pressure drops based on turbine pressures, since that is the useful value in the power block model
diff_m_dot_old = diff_m_dot_out
diff_m_dot_out = m_dot_toPB - m_dot_prev
!diff_dp_b = (dp_b - dp_b_prev)/max(max(dp_b, dp_b_prev),1.d0)
!diff_dp_b = (dp_b - dp_b_prev)/(deltaP1 + dp_b)
diff_dp_b = (dp_b - dp_b_prev)/(P_b_in)
!diff_dpsh = (dpsh - dpsh_prev)/max(max(dpsh, dpsh_prev),1.d0)
!diff_dpsh = (dpsh - dpsh_prev)/(deltaP1 + dpsh)
diff_dpsh = (dpsh - dpsh_prev)/(P_b_in)
!diff_dprh = (dprh - dprh_prev)/max(max(dprh, dprh_prev),1.d0)
diff_dprh = (dprh - dprh_prev)/(P_rh_in*1.E3)

IF(info(7)>2)THEN
    IF(diff_m_dot_old > diff_m_dot_out)THEN
        !IF( (abs(diff_m_dot_old+diff_m_dot_out)<0.01) .and. (abs(diff_m_dot_out/m_dot_toPB)<0.01) )THEN
        IF((abs(diff_m_dot_old+diff_m_dot_out)<0.01))THEN             
            m_dot_toPB = 0.5*m_dot_prev + 0.5*m_dot_toPB
            diff_m_dot_out = m_dot_toPB - m_dot_prev
        ENDIF
    ELSEIF(diff_m_dot_out > diff_m_dot_old)THEN
        !IF( (abs(diff_m_dot_out+diff_m_dot_old)<0.01) .and. (abs(diff_m_dot_out/m_dot_toPB)<0.01) )THEN
        IF((abs(diff_m_dot_out+diff_m_dot_old)<0.01))THEN
            m_dot_toPB = 0.5*m_dot_prev + 0.5*m_dot_toPB
            diff_m_dot_out = m_dot_toPB - m_dot_prev
        ENDIF
    ENDIF
ENDIF

!10/19/11, TN: Include tolerances for all variables passed to power block model
IF((info(7)>0).and.(abs(diff_m_dot_out/m_dot_toPB)<0.005).and.(abs(diff_dp_b)<0.005).and.(abs(diff_dpsh)<0.005).and.(abs(diff_dprh)<0.005))THEN
!IF((info(7)>0).and.(abs(diff_m_dot_out/m_dot_toPB)<0.005))THEN
    m_dot_toPB    = m_dot_prev
    f_timestep  = f_time_prev
    dp_b    = dp_b_prev
    DPSH    = DPSH_prev
    DPRH    = DPRH_prev
ENDIF

m_dot_prev = m_dot_toPB    
f_time_prev = f_timestep
dp_b_prev = dp_b
DPSH_prev = DPSH
DPRH_prev = DPRH

IF(info(7)==0)THEN
    rh_count_out = dble(rh_count)*success
    sh_count_out = dble(sh_count)*success
    boiler_count_out = dble(boiler_count)*success
ENDIF

IF((success==0).and.(m_dot_toPB==0.d0))THEN
    PB_on = 0.d0
ELSE
    PB_on = 1.d0
ENDIF

!Set Outputs
    !Boiler
OUT(1)  = PB_on*(T_fw - 273.15)                 ![C] Feedwater Outlet Temp
OUT(2)  = dble(success)*(XOUT_b(14)-273.15d0)   ![C] Boiler Inlet Temperature
OUT(3) = dble(success)*(T_boil - 273.15)        ![C] Boiler Temperature (= recirc temp, steam drum temp)
OUT(4) = PB_on*P_b_in                           ![kPa] Boiler Inlet Pressure
OUT(5) = dble(success)*XOUT_b(13)               ![kPa] Boiler Outlet Pressure (= recirc pressure, steam drum pressure) 
OUT(6) = dble(success)*dp_b                     ![Pa] Pressure drop through boiler (may be negative due to hydrostatic pressure of elevated steam drum)
OUT(7) = dble(success)*XOUT_b(1)*3600.d0        ![kg/hr] Mass flow rate through boiler
OUT(8) = dble(success)*XOUT_b(11)               ![-] Maximum quality: boiler flow paths
OUT(9) = dble(success)*XOUT_b(12)               ![-] Minimum quality: boiler flow paths
OUT(10) = dble(success)*eta_b                   ![-] Efficiency of boiler
OUT(11) = dble(success)*XOUT_b(19)              ![MW] Convective losses
OUT(12) = dble(success)*XOUT_b(20)              ![MW] Radiative losses
OUT(13) = dble(success)*XOUT_b(21)              ![MW] Energy rate absorbed
OUT(14) = dble(success)*XOUT_b(9)               ![-] Efficiency of fin (if applicable)
OUT(15) = dble(success)*(XOUT_b(6) - 273.15d0)  ![C] Maximum fin temperature
OUT(16) = dble(success)*(XOUT_b(5) - 273.15d0)  ![C] Maximum boiler tube surface temperature

    !Superheater
OUT(17) = dble(success)*m_dot_sh*3600.d0        ![kg/hr] Mass flow rate through superheater
OUT(18) = dble(success)*XOUT_sh(3)/(1.e3)       ![kPa] Outlet pressure of superheater
OUT(19) = dble(success)*DPSH                    ![Pa] SH Pressure drop
OUT(20) = dble(success)*XOUT_sh(1)              ![-] Efficiency of superheater
OUT(21) = dble(success)*XOUT_sh(13)             ![MW] Convective losses
OUT(22) = dble(success)*XOUT_sh(14)             ![MW] Radiative losses
OUT(23) = dble(success)*XOUT_sh(15)             ![MW] Energy rate absorbed
OUT(24) = dble(success)*(XOUT_sh(7) - 273.15d0) ![C] Maximum superheater surface temperature
OUT(25) = dble(success)*XOUT_sh(9)              ![m/s] exit velocity through SH

    !Reheater
OUT(26) = PB_on*f_mdot_rh            ![-] Reheater mass flow rate fraction    
OUT(27) = PB_on*P_rh_in              ![kPa]  Reheater inlet pressure    
OUT(28) = PB_on*(T_rh_in - 273.15)   ![C] Reheater Inlet Temp
OUT(29) = dble(success)*XOUT_rh(3)/(1.e3)       ![kPa] Outlet pressure of reheater  
OUT(30) = PB_on*(T_rh_target - 273.15)  ![C] Reheater Outlet Temp  
OUT(31) = dble(success)*DPRH                    ![Pa] RH pressure drop
OUT(32) = dble(success)*XOUT_rh(1)              ![-] Efficiency of reheater
OUT(33) = dble(success)*(XOUT_rh(7) - 273.15d0) ![C] Maximum reheater surface temperature
OUT(34) = dble(success)*XOUT_rh(15)             ![m/s] exit velocity through RH
OUT(35) = dble(success)*XOUT_rh(12)             ![MW] Convective losses
OUT(36) = dble(success)*XOUT_rh(13)             ![MW] Radiative losses
OUT(37) = dble(success)*XOUT_rh(14)             ![MW] Energy rate absorbed

field_eff_adj   = field_eff * defocus
Q_abs_rec = OUT(13)+OUT(23)+OUT(37)             ![MW] Energy rate absorbed by receiver
Q_conv_rec = OUT(11)+OUT(21)+OUT(35)            ![MW] Receiver convective losses
Q_rad_rec = OUT(12)+OUT(22)+OUT(36)             ![MW] Receiver radiative losses
eta_therm_rec = Q_therm_in_rec/max(0.001,EnergyInComb)  ![-] Receiver thermal efficiency

    !Receiver Combined
OUT(38) = Q_total/1.e6                                  ![MW] Total incident radiation on receiver: Defocus doesn't affect this value
OUT(39) = max(1.d0, dble(success)*EnergyInComb/(1.e6))  ![MW] Total Energy Incident on Receiver
OUT(40) = defocus                                       ![-] Defocus fraction
OUT(41) = field_eff_adj                                 ![-] adjusted field efficiency
OUT(42) = dble(success)*(Q_abs_rec)                     ![MW] Energy rate absorbed by receiver
OUT(43) = dble(success)*(Q_conv_rec)                    ![MW] Receiver convective losses
OUT(44) = dble(success)*(Q_rad_rec)                     ![MW] Receiver radiative losses
OUT(45) = dble(success)*Q_therm_in_rec/1.E6             ![MW] Rate of energy out
OUT(46) = dble(success)*(eta_therm_rec)                 ![-] Receiver thermal efficiency
OUT(47) = W_dot_boost/(1.e6)                            ![MW] Feedwater booster pump power

    !Auxiliary
OUT(48) = m_dot_aux*3600.d0                     ![kg/hr] Auxiliary mass flow rate
OUT(49) = Q_aux/(1.e6)                          ![MW] Auxiliary heat rate delivered cycle
OUT(50) = Q_aux_fuel                            ![MMBTU] Fuel energy rate delivered to aux heater

IF(ISNAN(Q_aux))THEN
    continue
ENDIF

    !Controls
OUT(51) = dble(standby_control)                 ![-] 1: Turbine can operate, 2: Turbine can be in standby, 3: Turbine is off
!OUT(52) = dble(success)*f_timestep              ![-] Fraction of timestep turbine can operate due to receiver start-up
OUT(52) = f_timestep                            ![-] Fraction of timestep turbine can operate due to receiver start-up
OUT(53) = m_dot_toPB*3600.d0                    ![kg/hr] Mass flow rate to power block.  m_dot_sh + m_dot_aux

    !Other
OUT(54) = dble(success)                         ![-] Did model solve? (1 - yes, 0 - no)
OUT(55) = INFO(7)                               ![-] Number of times unit has been called in current timestep
OUT(56) = dble(success)*f_rh                    ![-] Fraction of total energy allocated to reheater
OUT(57) = dble(success)*f_b                     ![-] Fraction of remaining (total - rh) energy allocated to boiler 
    
    !Convergence
OUT(58) = rh_count_out
OUT(59) = sh_count_out
OUT(60) = boiler_count_out
OUT(61) = B_EB_count

RETURN 1
end subroutine

!**************************************************************************************************

!#################################################################################################################

!#######################################################################################################################

subroutine flowPatternsDSR(N_panels,flowtype,Flow_pattern)
implicit none

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! This subroutine takes the number of panels, the requested flow type, and 
! returns the corresponding flow pattern (the order of panels through which the
! WF passes, this code is modified from the version in Type222
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
integer,intent(in)::N_panels
integer,dimension(N_panels)::Flow_pattern(2,N_panels/2),Z_all(N_panels)
integer::nlines,flowtype,i

select case(flowtype)
case(1)
    !This flow pattern begins at the northmost 2 panels, splits into 2 flows, and crosses over
    !  at the quarter position, exiting in 2 flows on the southmost 2 panels. This is the flow
    !  configuration that was used for SOLAR II
    nlines = 2

    !New: Accounts for receiver with numbers of panels divisible by 2 but not 4
    Flow_pattern(1,:) = (/ (N_panels/2-i+1,i=1,N_panels/4),((N_panels/2+N_panels/4+i),i=1,(N_panels-N_panels/2-N_panels/4)) /)
    Flow_pattern(2,:) = (/ (N_panels/2+i,i=1,N_panels/4),(N_panels/2-N_panels/4-i+1,i=1,N_panels/2-N_panels/4) /)
    !Define panels in flow direction: Number of panels divisible by 4?
    !Z_all = [13,14,15,16,17,18,6,5,4,3,2,1,12,11,10,9,8,7,19,20,21,22,23,24]
    !Old: Only accounts for receiver with numbers of panels divisible by 4
!    Flow_pattern(2,:) = (/ (i,i=N_panels/2+1,3*N_panels/4),((N_panels/4-(i-1)),i=1,N_panels/4) /)
!    Flow_pattern(1,:) = (/ ((N_panels/2-(i-1)),i=1,N_panels/4),(i,i=3*N_panels/4+1,N_panels) /)
                 
case(2)
    !This flow pattern is the same as flow pattern #1, but in reverse. The salt enters
    !  on the 2 southmost panels, crosses over, and exits on the 2 northmost panels.
    nlines = 2
                
    !New: Accounts for receiver with numbers of panels divisible by 2 but not 4      
    Flow_pattern(1,:) = (/ (i,i=1,N_panels/4),(N_panels-N_panels/4-i+1,i=1,N_panels-N_panels/4-N_panels/2) /)
    Flow_pattern(2,:) = (/ (N_panels-i+1,i=1,N_panels/4),(N_panels/4+i,i=1,N_panels/2-N_panels/4) /)
    ![1,2,3,4,5,6,17,16,15,14,13,12,24,23,22,21,20,19,18,7,8,9,10,11,12]
    !Old: Only accounts for receiver with numbers of panels divisible by 4
!    Flow_pattern(1,:) = (/ (i,i=1,N_panels/4),((3*N_panels/4-(i-1)),i=1,N_panels/4) /)
!    Flow_pattern(2,:) = (/ ((N_panels-(i-1)),i=1,N_panels/4),(i,i=N_panels/4+1,N_panels/2) /)
                    
case(3)
!This flow pattern has 2 separate flows that enter in 2 of the northmost panels
!  and flow around (without crossing over), exiting at the 2 southmost panels
    nlines=2
    
    ![13,14,15,16,17,18,19,20,21,22,23,24,12,11,10,9,8,7,6,5,4,3,2,1]
    Flow_pattern(1,:) = (/ (i,i=N_panels/2+1,N_panels) /)
    Flow_pattern(2,:) = (/ (N_panels/2-(i-1),i=1,N_panels/2) /)
    
case(4)
!This flow pattern has 2 separate flows that enter in 2 of the southmost panels
!  and flow around (without crossing over), exiting at the 2 northmost panels
    nlines=2
    
    ![1,2,3,4,5,6,7,8,9,10,11,12,24,23,22,21,20,19,18,17,16,15,14,13]
    Flow_pattern(1,:) = (/ (i,i=1,N_panels/2) /) 
    Flow_pattern(2,:) = (/ (N_panels-(i-1),i=1,N_panels/2) /)
    
case(5)
    !This flow type enters on a panel at the southmost side of the receiver,
    !  travels completely around the receiver in a clockwise direction,
    !  and exits again on the south side
    nlines=1
    
    ![1..24]
    Flow_pattern(1,:) = (/ (i,i=1,N_panels) /)
    
case(6)
    !This flow type enters on a panel at the southmost side of the receiver,
    !  travels completely around the receiver in a counter-clockwise direction,
    !  and exits again on the south side
    nlines=1
    
    ![24..1]
    Flow_pattern(1,:) = (/ (N_panels-(i-1),i=1,N_panels) /)
    
case(7)
    !This flow type enters on a panel at the northmost side of the receiver,
    !  travels completely around the receiver in a clockwise direction,
    !  and exits again on the north side
    nlines=1
    
    ![13..24,1..12]
    Flow_pattern(1,:) = (/ (i,i=N_panels/2+1,N_panels),(i,i=1,N_panels/2) /)
    
case(8)
    !This flow type enters on a panel at the northmost side of the receiver,
    !  travels completely around the receiver in a counter-clockwise direction,
    !  and exits again on the north side
    nlines=1
    
    ![12..1,24..13]
    Flow_pattern(1,:) = (/ (N_panels/2-(i-1),i=1,N_panels/2),(N_panels-(i-1),i=1,N_panels/2) /)
    
end select

end subroutine

!#######################################################################################################################