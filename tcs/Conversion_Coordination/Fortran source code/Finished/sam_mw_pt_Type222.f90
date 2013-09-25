SUBROUTINE TYPE222 (TIME,XIN,OUT,T,DTDT,PAR,INFO,ICNTRL,*)

!************************************************************************
! Object: External Receiver/Tower
! Simulation Studio Model: Type222
! 
! Author: Michael J. Wagner
! Date:	 March 14, 2008 
! Last modified: October 28, 2011
! COPYRIGHT 2010 NATIONAL RENEWABLE ENERGY LABORATORY
!
! Doc. tables updated 2011-10-28 - MJW
!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Parameters
!    1| N_panels                         | Number of individual panels on the receiver                       | none             | none             
!    2| D_rec                            | The overall outer diameter of the receiver                        | m                | m                
!    3| H_rec                            | The height of the receiver                                        | m                | m                
!    4| THT                              | The height of the tower (hel. pivot pt to rec equator)            | m                | m                
!    5| D_out                            | The outer diameter of an individual receiver tube                 | mm               | m                
!    6| th_tu                            | The wall thickness of a single receiver tube                      | mm               | m                
!    7| Material                         | The material name of the receiver tubes                           | none             | none             
!    8| Coolant                          | The name of the HTF used in the receiver                          | none             | none             
!    9| Flow_type                        | A flag indicating which flow pattern is used                      | none             | none             
!   10| PLAT                             | The lattitude of the power plant                                  | deg              | deg              
!   11| LU_flux                          | The logical unit of the flux map file                             | none             | none             
!   12| epsilon                          | The emissivity of the receiver surface coating                    | none             | none             
!   13| hl_ffact                         | The heat loss factor (thermal loss fudge factor)                  | none             | none             
!   14| T_htf_hot_des                    | Hot HTF outlet temperature at design conditions                   | C                | K                
!   15| T_htf_cold_des                   | Cold HTF inlet temperature at design conditions                   | C                | K                
!   16| f_rec_min                        | Minimum receiver mass flow rate turn down fraction                | none             | none             
!   17| Q_rec_des                        | Design-point receiver thermal power output                        | MWt              | Wt               
!   18| rec_su_delay                     | Fixed startup delay time for the receiver                         | hr               | hr               
!   19| rec_qf_delay                     | Energy-based receiver startup delay (fraction of rated thermal power)| none             | none             
!   20| LU_fl                            | Fluid property file logical unit                                  | none             | none             
!   21| m_dot_htf_max                    | Maximum receiver mass flow rate                                   | kg/hr            | kg/s             

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Inputs
!    1| azimuth                          | Solar azimuth angle                                               | deg              | deg              
!    2| zenith                           | Solar zenith angle                                                | deg              | deg              
!    3| T_salt_hot                       | Desired HTF outlet temperature                                    | C                | K                
!    4| T_salt_cold                      | Desired HTF inlet temperature                                     | C                | K                
!    5| V_wind                           | Ambient wind velocity, ground level                               | m/s              | m/s              
!    6| P_amb                            | Ambient atmospheric pressure                                      | atm              | Pa               
!    7| eta_pump                         | Receiver HTF pump efficiency                                      | none             | none             
!    8| hour                             | Hour of the day                                                   | hr               | hr               
!    9| T_dp                             | Ambient dew point temperature                                     | C                | C                
!   10| I_bn                             | Direct (beam) normal radiation                                    | kJ/m^2.hr        | kW/m^2           
!   11| field_eff                        | Heliostat field efficiency                                        | none             | none             
!   12| T_db                             | Ambient dry bulb temperature                                      | C                | K                
!   13| night_recirc                     | Flag to indicate night recirculation through the rec.             | none             | none             
!   14| hel_stow_deploy                  | Heliostat field stow/deploy solar angle                           | deg              | deg              

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Outputs
!    1| m_dot_salt_tot                   | Total HTF flow rate through the receiver                          | kg/hr            | kg/s             
!    2| eta_therm                        | Receiver thermal efficiency                                       | none             | none             
!    3| W_dot_pump                       | Receiver pump power                                               | MWe              | We               
!    4| q_conv_sum                       | Receiver convective losses                                        | MWt              | Wt               
!    5| q_rad_sum                        | Receiver radiative losses                                         | MWt              | Wt               
!    6| Q_thermal                        | Receiver total thermal losses for timestep                        | MWt              | Wt               
!    7| T_salt_hot                       | HTF outlet temperature                                            | C                | K                
!    8| -not named-                      | Receiver power prior to thermal losses                            | MWt              | NA               
!    9| field_eff_adj                    | Adjusted heliostat field efficiency - includes overdesign adjustment| none             | none             
!   10| q_inc_sum                        | Total incident power on the receiver                              | MWt              | Wt               
!   11| q_startup                        | Startup energy consumed during the current time step              | MWt              | MWt              
!   12| dP_receiver                      | Receiver HTF pressure drop                                        | bar              | bar              
!   13| dP_total                         | Total receiver and tower pressure drop                            | bar              | MPa              
!   14| vel_htf                          | Heat transfer fluid average velocity                              | m/s              | m/s              

!TRNSYS acess functions (allow to acess TIME etc.) 
USE TrnsysConstants
USE TrnsysFunctions

!DEC$ATTRIBUTES DLLEXPORT :: TYPE222				!SET THE CORRECT TYPE NUMBER HERE

!-----------------------------------------------------------------------------------------------------------------------
!    TRNSYS DECLARATIONS
IMPLICIT NONE			!REQUIRES THE USER TO DEFINE ALL VARIABLES BEFORE USING THEM

real(8):: XIN, OUT, TIME, PAR, STORED, T, DTDT 	
integer(4):: INFO(15),IUNIT,ITYPE,ICNTRL,NITEMS
integer,parameter:: NP=22,NI=14,NOUT=14,ND=0,ns=3

DIMENSION:: XIN(NI),OUT(NOUT),PAR(NP),STORED(ns),T(ND),DTDT(ND)
!-----------------------------------------------------------------------------------------------------------------------

!ADD DECLARATIONS AND DEFINITIONS FOR THE USER-VARIABLES HERE
real(8)::m_dot_salt_tot,eta_therm,W_dot_pump,q_conv_sum,q_rad_sum,Q_thermal, P_amb_kPa, q_inc_sum, q_abs_sum, E_su, q_startup
real(8),allocatable,save:: T_s(:)

!PARAMETERS
real(8):: D_rec, H_rec, THT, D_out, th_tu,Material, Coolant, PLAT, epsilon, hl_ffact,&
          T_htf_hot_des, T_htf_cold_des, f_rec_min, Q_rec_des, rec_su_delay, rec_qf_delay,A_sf
integer:: LU_fl, N_panels, flowtype, LU_flux

!INPUTS
real(8)::azimuth, zenith, T_salt_hot, T_salt_cold,V_wind, eta_pump, hour, T_dp, I_bn, field_eff, &
         night_recirc, hel_stow_deploy
!Locals         
integer(4):: ios, i, qq, j,nlines,qq_max,itermode
integer(4),allocatable::Panel(:),ceil(:),flo(:)
integer(4),allocatable,save::Flow_pattern(:)
integer,dimension(2)::salt_out
real(8),parameter::pi=3.14159265,grav=9.81, sigma=5.670e-8
real(8)::D_tube,th_tube,D_inner,D_in,A_tube,n_t,N_tube,q_guess,c_guess,A_rec_proj,A_node,P_atm,&
         flux_in(12),m_dot_salt,array(12),err,T_salt_hotX, m_dot_saltX,T_coolant_prop,T_s_ave,T_film_ave,&
         k_film,mu_film,rho_film,c_p_film,Re_for,ksD,Nusselt_for,h_for,conductivity,Viscosity,Density,Nusselt_FC,beta,nu_amb,&
         volexpcoef,m,T_sky,C_p_coolant,RelRough,mu_coolant,k_coolant,rho_coolant,u_coolant,&
         Re_inner,Pr_inner,h_inner,R_conv_inner,Nusselt_t,f,LoverD,m_dot_tube,L_e_45,L_e_90,DELTAP_tube,DELTAP_45,DELTAP_90,DELTAP,&
         DELTAP_THT,DELTAP_net,Pres_D,W_dot_pump_hp,T_amb, skytemp,tol, est_load, eta_pump_adj, E_su0, t_su0, field_eff_adj,&
         t_su, dt, specheat, m_dot_htf_des, m_dot_htf_min, c_htf_des, mode, mode0, q_rec_min, m_dot_htf_max, od_control, err_od, tol_od
!N_panels -sized variables         
real(8),allocatable::ind(:),Psp_field(:),P_field(:),T_panel_out(:),T_panel_in(:),T_sX(:),T_panel_outX(:),T_panel_inX(:),T_film(:),&
                     h_rad_amb(:),h_rad_sky(:),q_dot_amb(:),q_dot_sky(:),q_dot_rad(:),q_dot_conv(:),q_dot_loss(:),q_loss_sum(:),&
                     q_dot_inc(:),q_dot_abs(:),T_wall(:),k_tube(:),R_tube_wall(:),ppos(:),T_panel_ave(:),Gr_nat(:),Nusselt_nat(:),&
                     h_nat(:),h_mixed(:)
Logical::is_there
character::checkname*300, test*300
save:: salt_out,nlines

!-----------------------------------------------------------------------------------------------------------------------
!    SET THE VERSION INFORMATION FOR TRNSYS
IF(INFO(7).EQ.-2) THEN
    INFO(12)=16
    RETURN 1
ENDIF


!    post-convergence call
IF (INFO(13).GT.0) THEN

    !MJW 9.8.2010 :: Call the property range check subroutine with the inlet and outlet HTF temps to make sure they're in the valid range
    call check_htf(Coolant,T_salt_hot)
    call check_htf(Coolant,T_salt_cold)
    
    !Set storage variables for the next time step
    if(mode==0.) then  
        !if the time step ends with the tower shut off, then reset the startup energy requirement for the next time step
        E_su = Q_rec_des * rec_qf_delay
        t_su = rec_su_delay
    endif
    stored(1) = mode
    stored(2) = E_su
    stored(3) = t_su
    call setStorageVars(stored,ns,info)
    
    itermode = 1
    od_control = 1.d0

    RETURN 1
ENDIF
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Last call manipulations here
IF (INFO(8).EQ.-1) THEN
    
    !mjw 3.29.11 Call the fluxmap array to handle variable allocation
    call fluxinterp(LU_flux,zenith,azimuth,array,info)
    if(allocated(flow_pattern)) deallocate(flow_pattern)

    !Deallocate the surface temperature array
    if(allocated(T_s)) deallocate(T_s)
    
    !Deallocate other variables    
    if(allocated(ind)) deallocate(ind,Psp_field,P_field,T_panel_out,T_panel_in,T_sX,T_panel_outX,T_panel_inX,T_film,&
        h_rad_amb,h_rad_sky,q_dot_amb,q_dot_sky,q_dot_rad,q_dot_conv,q_dot_loss,q_loss_sum,q_dot_inc,q_dot_abs,&
        T_wall,k_tube,R_tube_wall,ppos,T_panel_ave,Gr_nat,Nusselt_nat,h_nat,h_mixed)
    if(allocated(Panel)) deallocate(Panel,ceil,flo)
    
    
    !Close the flux map file
    inquire(unit=LU_flux,opened=is_there,name=checkname)
    if (is_there) close(LU_flux)

    RETURN 1
ENDIF

!-----------------------------------------------------------------------------------------------------------------------

!Read in parameters here and do initial calculations, but continue on and don't return until after the CRS subroutine is called
IF (INFO(7).EQ.-1) THEN
!-----------------------------------------------------------------------------------------------------------------------
    !Parameters
    N_panels=int(PAR(1))
    D_rec=PAR(2)
    H_rec=PAR(3)
    THT=PAR(4)
    D_out=PAR(5)
    th_tu=PAR(6)
    Material=PAR(7)
    Coolant=PAR(8)
    flowtype=int(PAR(9))
    PLAT=PAR(10)
    LU_flux=int(PAR(11))
    epsilon=PAR(12)
    hl_ffact=PAR(13)
    T_htf_hot_des=PAR(14)+273.15    !Convert C to K
    T_htf_cold_des=PAR(15)+273.15   !Convert C to K
    f_rec_min=PAR(16)
    Q_rec_des=PAR(17)*1.e6          !Convert MWt to Wt
    rec_su_delay=PAR(18)
    rec_qf_delay=PAR(19)
    LU_fl = int(PAR(20))
    m_dot_htf_max = PAR(21) / 3600.  !mjw 3.29.11
    A_sf = PAR(22)      ![m2] mjw 6.26.12 solar field area

    dt = getSimulationTimeStep()

    !Receiver dimensions, parameters
    D_tube = D_out/1000.                    !Value in meters
    th_tube = th_tu/1000.                   !Thickness of the tube
    D_inner = D_tube - 2.*th_tube	        !Diameter of each receiver tube
    D_in = D_inner*1000.                    !Inner diameter of the tube, D_in set from D_out and th_tube
    A_tube = pi*D_tube/2.*H_rec             !Outer area of each tube
    n_t = Nint((pi*D_rec)/(D_tube*N_panels))!The number of tubes per panel, as a function of the number
									        !  of panels and the desired diameter of the receiver
    N_tube =n_t*N_panels                    !Number of tubes in the system
    A_rec_proj = D_tube*H_rec*N_tube        !The projected area of the tubes on a plane parallel to the center lines of the tubes
    A_node = pi*D_rec/N_panels*H_rec        !The area associated with each node


    !-----------------------------------------------------------------------------------------------------------------------
    !allocate space for the arrays
    if(.not.allocated(T_s)) allocate(T_s(N_panels))

    if(.not.allocated(ind)) allocate(ind(N_panels),Psp_field(N_panels),P_field(N_panels),T_panel_out(N_panels),&
        T_panel_in(N_panels),T_sX(N_panels),T_panel_outX(N_panels),T_panel_inX(N_panels),T_film(N_panels),&
        h_rad_amb(N_panels),h_rad_sky(N_panels),q_dot_amb(N_panels),q_dot_sky(N_panels),q_dot_rad(N_panels),&
        q_dot_conv(N_panels),q_dot_loss(N_panels),q_loss_sum(N_panels),q_dot_inc(N_panels),q_dot_abs(N_panels),&
        T_wall(N_panels),k_tube(N_panels),R_tube_wall(N_panels),ppos(N_panels),T_panel_ave(N_panels),&
        Gr_nat(N_panels),Nusselt_nat(N_panels),h_nat(N_panels),h_mixed(N_panels))
    if(.not.allocated(Panel)) allocate(Panel(N_panels),ceil(N_panels),flo(N_panels))

    !SET SOME INFO ARRAY VARIABLES TO TELL THE TRNSYS ENGINE HOW THIS TYPE IS TO WORK
    INFO(6)=NOUT				
    INFO(9)=1				
    INFO(10)=0	

    
    !8.2.2010 MJW:: Call the fluid property file, initialize property arrays
    call readFluidPropFile(LU_FL)       
    
    !Get flow pattern
    if(.not.allocated(Flow_pattern)) allocate(Flow_pattern(N_panels))
    call flowPatterns(N_panels,flowtype,Flow_pattern,salt_out,nlines)
    
    
    !initialize the startup mode
    mode = 0.d0  !0=requires startup, 1=starting up, 2=running
    itermode = 1  !1 solve for design temp, 2 solve to match mass flow restriction
    od_control = 1.d0 !additional defocusing control for over-design conditions
    tol_od = .001d0  !tolerance for over-design iteration
    
    !7.30.2010 MJW:: Calculate the reference HTF mass flow rate, and the max and min values
    c_htf_des = specheat(Coolant,(T_htf_hot_des + T_htf_cold_des)/2.d0,1.d0)*1000.d0    ![J/kg-K] Specific heat at design conditions
    m_dot_htf_des = Q_rec_des/(c_htf_des * (T_htf_hot_des - T_htf_cold_des))    ![kg/s]
    !m_dot_htf_min = m_dot_htf_des * f_rec_min   ![kg/s]
    q_rec_min = q_rec_des * f_rec_min !mjw 3.10.11 [W] Minimum receiver thermal power 

    call setStorageSize(ns,info)
    stored(1) = mode  !mode / mode0
    stored(2) = Q_rec_des * rec_qf_delay ![W-hr]  Startup energy E_su/E_su0
    stored(3) = rec_su_delay  ![hr] Startup time requirement t_su/t_su0
    call setStorageVars(stored,ns,info)

    !mjw 3.29.11 Call the fluxmap array to handle variable allocation
    call fluxinterp(LU_flux,zenith,azimuth,array,info)
    
    !Set outputs
    OUT = 0.d0
    OUT(7)=T_htf_cold_des  !mjw 3.29.11   565.0
    
    RETURN 1

endif

!-----------------------------------------------------------------------------------------------------------------------
! Read inputs and call the CRS subroutine here. This allows calls after all iterations and at the initial timestep
!-----------------------------------------------------------------------------------------------------------------------

azimuth=XIN(1)+180.d0              !Solar azimuth
!By TRNSYS convention, the azimuth angle is 0 at due south, negative to the east,
! and positive to the west. The range is then -180 to 180. By the convention used
! here, the azimuth is 0 at due north, and ranges clockwise from 0 to 360. This adjusts.
zenith=XIN(2)               !Solar zenith
T_salt_hot=XIN(3)+273.15    !Desired hot temp, Convert from deg C to deg K
T_salt_cold=XIN(4)+273.15   !Specified inlet temp, Convert from C to K
V_wind=XIN(5)               !Wind velocity
P_atm=XIN(6)*101325.d0      !ambient pressure
eta_pump=XIN(7)             !Receiver HTF pump efficiency
hour=XIN(8)-12.d0           !Hour of the day
T_dp=XIN(9)                 !Dewpoint temp
I_bn=XIN(10)/3.6d0          !Beam normal radiation
field_eff=XIN(11)           !Field efficiency value
T_amb=XIN(12)+273.15d0                !Dry bulb temp
night_recirc=XIN(13)        !Night recirculation control 0=empty receiver, 1=recirculate
hel_stow_deploy=XIN(14)     !Solar elevation angle at which heliostats are stowed
    IUNIT=INFO(1)
    ITYPE=INFO(2)

T_sky = skytemp(T_amb,(T_dp+273.15),hour)     !The effective sky temperature [K]

call getStorageVars(stored,ns,info)
mode0 = stored(1)
E_su0 = stored(2)
t_su0 = stored(3)


!#######################################################################################################################
!Initialize sensitive variables
ppos=0


!Correct the windspeed using the logarithmic profile law (HOMER documentation, Wind Shear Inputs section)
V_wind = log((THT+H_rec/2.)/.003)/log(10./.003)*V_wind

!Do an initial check to make sure the solar position called is valid.
!  If its not, return the output equal to zeros. Also check to make sure 
!  the solar flux is at a certain level, otherwise the correlations
!  aren't valid
if(((zenith.gt.(90.-hel_stow_deploy)).or.(I_bn.le.1.e-6)).or.((zenith.eq.0.d0).and.(azimuth.eq.180.d0))) then
    if(night_recirc.eq.1.)then
        I_bn=0.
    else
        mode = 0.d0  !8.2.2010 :: Set the startup mode
        goto 900 !return zeros
    endif
endif

err_od = 999.d0 !mjw 3.29.11 Always reset error to 999. before iteration
15 continue !mjw 3.29.11 return point for over-design iteration
field_eff_adj = field_eff*od_control  !mjw 3.29.11

!Get the values of the flux from the fluxmap and store them as flux_in(col,row)
if(I_bn.gt.1.) then
    call fluxinterp(LU_flux,zenith,azimuth,array,info)
    !mjw 6.27.12 Modified the PTGEN code to give a normalized 1x12 array of [flux absorbed at node j/flux incident on receiver]
    !Get the flux_in() array in terms of kW/m2.
    flux_in(:)=array(:)*I_bn*field_eff_adj*A_sf/1000./(pi*D_rec*H_rec/12.)  !kw/m2
else
    flux_in=0.d0
endif

!Translate to the number of panels, so each panel has its own linearly interpolated flux value
do j=1,N_panels
    Panel(j) = j	                       !The position of each panel
    ppos(j)=(12./dble(N_panels)*(j-1.)+6./dble(N_panels))+1.
    flo(j)=floor(ppos(j))
    ceil(j)=ceiling(ppos(j))
    ind(j)= (ppos(j)-flo(j))/max(dble(ceil(j)-flo(j)),1.e-6)
    if(ceil(j).gt.12.) ceil(j)=1
    Psp_field(j)=ind(j)*(flux_in(ceil(j))-flux_in(flo(j)))+flux_in(flo(j))  !Average area-specific power for each node
    P_field(j)=A_node*Psp_field(j)	        !The power incident on each node
enddo

!Guess values -------------------------------------------------------------
if(night_recirc.eq.1.) then
    T_sX = T_salt_hot                !Guess temperature for the surface nodes
    T_panel_outX = (T_salt_hot+T_salt_cold)/2.       !Guess values for the fluid temp coming out of the control volume
    T_panel_inX = (T_salt_hot+T_salt_cold)/2.    !Guess values for the fluid temperature coming into the control volume
else
    T_sX = T_salt_hot                !Guess temperature for the surface nodes
    T_panel_outX = T_salt_cold       !Guess values for the fluid temp coming out of the control volume
    T_panel_inX = T_salt_cold        !Guess values for the fluid temperature coming into the control volume
endif
      
if(I_bn.gt.1.e-6) then
    q_guess = 0.5*sum(P_field(1:N_panels)) !Estimate the thermal power produced by the receiver [kWt].
                                         !0.5 coefficient is optimized based on run-time 
    c_guess = specheat(Coolant, (T_salt_hot+T_salt_cold)/2.,1.d0) !Estimate the specific heat of the fluid in the receiver [kJ/kg-K]
    m_dot_saltX = q_guess/(c_guess*(T_salt_hot-T_salt_cold)*dble(nlines))     !coolant mass flow rate guess value
else !The tower recirculates at night (based on earlier condition) 
    !Enter recirculation mode, where inlet/outlet temps switch
    T_salt_hot = T_salt_cold
    T_salt_cold = T_sX(1) !T_sX is set to T_salt_hot before.. just a way of switching
    c_guess = specheat(Coolant, (T_salt_hot+T_salt_cold)/2.,1.d0) !Estimate the specific heat of the fluid in the receiver [kJ/kg-K]
    m_dot_saltX = -3500./(c_guess*(T_salt_hot-T_salt_cold)/2.) !Estimate the flow rate to be some low number
endif
T_salt_hotX = 9999.                   !Initial value for error calculation
!--------------------------------------------------------------------------

!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!                            ITERATION STARTS HERE
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
qq_max=50
do qq=1,qq_max+1
    !if the problem fails to converge after 50 iterations, then the power is likely negligible and
    !..the zero set can be returned
    if(qq > qq_max) then
        mode = 0.d0  !8.2.2010 :: Set the startup mode
        goto 900
    endif
    
    !Set the variables equal to their newly calculated guess values----------
    T_s(:) = T_sX(:)
    T_panel_out(:) = T_panel_outX(:)
    T_panel_in(:) = T_panel_inX(:)

    m_dot_salt = m_dot_saltX
    !------------------------------------------------------------------------

    !Now do the actual calculations
    T_panel_ave(:) = (T_panel_in(:)+T_panel_out(:))/2.  !The average coolant temperature in each control volume
    T_film(:) = (T_s(:)+T_amb)/2.
    T_coolant_prop = (T_salt_hot + T_salt_cold)/2.             !The temperature at which the coolant properties are evaluated. Validated as constant
    T_salt_hotX = sum(T_panel_out(salt_out(1:nlines)))/dble(nlines)!Calculates the mixed outlet temperature of the salt

    !Calculate the average surface temperature so that the forced convection coefficient can be determined
    T_s_ave = sum(T_s(1:N_panels))/(dble(N_panels))
    T_film_ave = (T_amb+T_salt_hot)/2.

    !Convection coefficient for external forced convection using Siebers & Kraabel
    k_film = Conductivity(1.d0,T_film_ave,1.d0)                    !The conductivity of the air
    mu_film = Viscosity(1.d0,T_film_ave, 1.d0)                      !Dynamic viscosity of the air
    rho_film = Density(1.d0, T_film_ave, P_atm)               !Density of the air
    c_p_film = specheat(1.d0,T_film_ave,1.d0)*1000.    !Specific heat of the air
    Re_for = rho_film*V_wind*D_rec/mu_film                     !Reynolds number
    ksD = (D_tube/2.)/D_rec                                    !The effective roughness of the cylinder [Siebers, Kraabel 1984]
    Nusselt_for=Nusselt_FC(ksD,Re_for)                         ![S&K]
    h_for = Nusselt_for*k_film/D_rec*hl_ffact                  !The forced convection heat transfer coefficient

    !Convection coefficient for external natural convection using Siebers & Kraabel"
    !Note: This relationship applies when the surrounding properties are evaluated at ambient conditions [S&K]"
    beta = 1./T_amb                                          !Volumetric expansion coefficient
    nu_amb = Viscosity(1.d0,T_amb, 1.d0)/Density(1.d0,T_amb,P_atm) !Kinimatic viscosity
    Gr_nat(:) = dmax1(0.d0, grav*beta*(T_s(:)-T_amb)*H_rec**3/(nu_amb**2))      !Grashof Number at ambient conditions   MJW 8.4.2010 :: Hard limit of 0 on Gr #
    Nusselt_nat(:) = .098*Gr_nat(:)**(1./3.)*(T_s(:)/T_amb)**(-.14) !Nusselt number
    h_nat(:) = Nusselt_nat(:)*Conductivity(1.d0,T_amb,1.d0)/H_rec*hl_ffact    !TN 3.29.11 ; The natural convection cofficient ; conductivity calculation corrected
    
    !Mixed convection
    m=3.2
    h_mixed(:) = (h_for**m+h_nat(:)**m)**(1./m)*4.0              !MJW 7.30.2010:: (4.0) is a correction factor to match convection losses at Solar II (correspondance with G. Kolb, SNL)
    q_dot_conv(:) = h_mixed(:)*A_node*(T_s(:) - T_film(:))   !Convection losses per node

    !Radiation from the receiver
    !Calculate the radiation node by node
    h_rad_amb(:) = sigma*epsilon*(T_s(:)**2+T_amb**2)*(T_s(:)+T_amb)*hl_ffact  !The radiation coefficient for amb
    h_rad_sky(:) = sigma*epsilon*(T_s(:)**2+T_sky**2)*(T_s(:)+T_sky)*hl_ffact  !The radiation coef. for sky
    q_dot_amb(:) = .5*h_rad_amb(:)*A_node*(T_s(:)-T_amb)  !amb losses per node
    q_dot_sky(:) = .5*h_rad_sky(:)*A_node*(T_s(:) - T_sky)  !sky losses per node

    !Calculate the losses from the surface"
    q_dot_rad(:) = q_dot_amb(:)+q_dot_sky(:)		     !Total radiation losses per node
    q_dot_loss(:) = q_dot_rad(:)+q_dot_conv(:)		     !Total overall losses per node

    q_conv_sum = sum(q_dot_conv(1:N_panels))                   !Receiver convective losses
    q_rad_sum = sum(q_dot_rad(1:N_panels))                     !Receiver radiation losses
  
    !Calculate the flux incident on the surface
    q_dot_inc(:)=P_field(:)*1000.     ![kW]
    q_dot_abs(:) = q_dot_inc(:) - q_dot_loss(:)     !The absorbed flux at each node
    q_inc_sum = sum(q_dot_inc(1:N_panels))	             !The total power incident on the surface
    q_abs_sum = sum(q_dot_abs(1:N_panels))	             !The total power absorbed by the receiver"

    !Calculate the temperature drop across the receiver tube wall.. assume a cylindrical thermal resistance
    T_wall(:) = (T_s(:) + T_panel_ave(:))/2.	             !The temperature at which the conductivity of the wall is evaluated
    k_tube(:) = Conductivity(Material,T_wall(:),1.d0)	     !The conductivity of the wall
    R_tube_wall(:) = th_tube/(k_tube(:)*H_rec*D_rec*pi**2/2./dble(N_panels)) !The thermal resistance of the wall   TN 2.9.11 Need to divide by the number of panels
 
    !Calculations for the inside of the tube
    C_p_coolant = specheat(Coolant, T_coolant_prop,1.d0)*1000.    !Specific heat of the coolant
    LoverD = H_rec/D_inner
    RelRough = (1.5e-6)/D_inner                                !Relative roughness of the tubes. http:www.efunda.com/formulae/fluids/roughness.cfm
    mu_coolant = viscosity(Coolant, T_coolant_prop, 1.d0)            !Absolute viscosity of the coolant
    k_coolant = conductivity(Coolant,  T_coolant_prop,1.d0)         !Conductivity of the coolant fluid
    rho_coolant = density(Coolant, T_coolant_prop, 1.d0)         !Density of the coolant

    u_coolant = m_dot_salt/(n_t*rho_coolant*(D_inner/2.)**2*pi)!Average velocity of the coolant through the receiver tubes.
    Re_inner = rho_coolant*u_coolant*D_inner/mu_coolant        !Reynolds number for internal flow
    Pr_inner = C_p_coolant*mu_coolant/k_coolant                !Prandtl number for internal flow
    call PipeFlow(Re_inner,Pr_inner,LoverD,relRough,Nusselt_t,f)  !The internal convection correlation. Petukhov, Gneilinski
    if(Nusselt_t.le.0) then
        mode = 0.d0  !8.2.2010 :: Set the startup mode
        goto 900
    endif
    h_inner = Nusselt_t*k_coolant/D_inner                      !Convection coefficient between the inner tube wall and the coolant
    R_conv_inner = 1./(h_inner*pi*D_inner/2.*H_rec*n_t)        !The thermal resistance associated with this value

    !Set up numerical flow grid
    !tn 3.21.2011 Set the inlet panel temperature using T_panel_inX instead of T_panel_in
    do j=1,N_panels
        if(Flow_pattern(j).lt.1) then    !Declare equation duplicities
            T_panel_inX(j) = T_salt_cold
        else
            T_panel_inX(j) = T_panel_out(Flow_pattern(j))       !The panel inlet temp is equal to the panel outlet temp from the previous panel, according to the flow diagram
        endif
    enddo
    T_panel_outX(:) = T_panel_inX(:) + q_dot_abs(:)/(m_dot_salt*c_p_coolant) !The energy balance for each node
    T_panel_ave(:) = (T_panel_inX(:) + T_panel_outX(:))/2.d0          !tn 3.21.11 Update the panel average temperature
    T_salt_hotX = sum(T_panel_outX(salt_out(1:nlines)))/dble(nlines)  !mjw 3.29.11 Update the calculated hot salt outlet temperature
    T_sX(:)=T_panel_ave(:)+q_dot_abs(:)*(R_conv_inner+R_tube_wall(:)) !Calculate the surface temperature based on the absorbed heat
    if(q_inc_sum.gt.0.) then
        eta_therm = q_abs_sum/q_inc_sum
    else
        eta_therm = 0.
    endif
        
    err=(T_salt_hotX - T_salt_hot)/T_salt_hot  !The basis on which convergence is determined
    if(night_recirc.eq.1.) then
        tol= 0.0057
    else
        tol= 0.001 
    endif
    
    if(abs(err).lt.tol) exit        !Check to see if the problem has converged
    
    !Final calculations
    m_dot_saltX = q_abs_sum/(nlines*C_p_coolant*(T_salt_hot - T_salt_cold))

    !Do a check to make sure the mass flow rate is reasonable
    if(m_dot_saltX.lt.1.e-5) then
        mode = 0.d0  !8.2.2010 :: Set the startup mode
        goto 900  !Modified 3-30-09, MJW
    endif
    
enddo

!Now we can calculate some of the parasitics associated with pumping the coolant fluid
!Calculating the pressure drop across the receiver panels
!mjw 3.29.11 changed m_dot_salt to m_dot_saltX
m_dot_salt_tot = m_dot_saltX*nlines
m_dot_tube = m_dot_saltX/dble(n_t)                  !The mass flow through each individual tube

!---mjw 3.29.11 Limit the HTF mass flow rate to the maximum, if needed. 
if((m_dot_salt_tot > m_dot_htf_max).or.(itermode == 2)) then
    err_od = (m_dot_salt_tot - m_dot_htf_max)/m_dot_htf_max
    if(err_od < tol_od) then
        itermode = 1    
        od_control = 1.d0
    else
        od_control = od_control*(m_dot_htf_max/m_dot_salt_tot)**.8  !adjust the over-design defocus control by modifying the current value
        itermode = 2
        goto 15
    endif
endif
!---


!----MJW 8.2.2010 :: Startup
!if(mode/=2.d0) then  !If the plant hasn't already been running
if((E_su0 > 0.) .or. (t_su0 > 0.)) then  !mjw 3.10.11
    E_su = dmax1(0.d0, E_su0 - m_dot_salt_tot*c_p_coolant*(T_salt_hotX - T_salt_cold)*dt)
    t_su = dmax1(0.d0, t_su0 - dt)
    if(E_su + t_su > 0.d0) then
        mode = 1.d0 !If either are greater than 0, we're starting up but not finished
        q_startup = (E_su0 - E_su)/dt*1.e-6 !mjw 3.10.11
        goto 900  !mjw 3.10.11
    else    !Only part of the timestep/energy was needed to startup.  
        mode= 2.d0
        !Adjust the available mass flow to reflect startup
        m_dot_salt_tot = dmin1( (1.-t_su0/dt)*m_dot_salt_tot, m_dot_salt_tot - E_su0/(dt*c_p_coolant*(T_salt_hotX - T_salt_cold)) )
    endif
endif
q_startup = (E_su0 - E_su)/dt*1.e-6       !Convert W-hr to MW  mjw 3.10.11
!----

!----Pressure drop calculations
L_e_45 = 16.                                       !The equivalent length produced by the bends in the tubes.
L_e_90 = 30.											 !'Intro to Fluid Mechanics, Fox, et al.'
DELTAP_tube = rho_coolant*(f*H_rec/D_inner*u_coolant**2/2.)  !Pressure drop across the tube, straight length
DELTAP_45 = rho_coolant*(f*L_e_45*u_coolant**2/2.) !Pressure drop across 45 degree bends
DELTAP_90 = rho_coolant*(f*L_e_90*u_coolant**2/2.) !Pressure drop across 90 degree bends
DELTAP = DELTAP_tube + 2*DELTAP_45 + 4*DELTAP_90   !Total pressure drop across the tube with (4) 90-deg bends, (2) 45-deg bends
DELTAP_THT = rho_coolant*THT*grav                  !The pressure drop from pumping up to the receiver
DELTAP_net = DELTAP*N_panels/dble(nlines)+DELTAP_THT !The net pressure drop across the receiver panels
Pres_D = DELTAP_net*1.e-6 ![MPa]
est_load = dmax1(0.25d0, m_dot_salt_tot/m_dot_htf_des)*100.                 !MJW 8.26.2010. Calculate the relative pump load. Limit to 25%
eta_pump_adj = eta_pump*(-2.8825E-09*est_load**4 + 6.0231E-07*est_load**3 - 1.3867E-04*est_load**2 + 2.0683E-02*est_load)  !MJW 7.16.2010 :: Calculate the adjusted pump efficiency
!W_dot_pump = DELTAP_net*u_coolant*pi*D_inner**2/4.*n_t/eta_pump !The work done by the pump to move the coolant through the receiver  7.30 MJW NOTE: THIS EQUATION IS WRONG!!
W_dot_pump = DELTAP_net*m_dot_salt_tot/rho_coolant/eta_pump_adj !MJW 7.30.2010 (source: Fox et al, pp354)
!Calculate the thermal output of the tower. This includes all thermal losses.
Q_thermal = m_dot_salt_tot*C_p_coolant*(T_salt_hotX - T_salt_cold)
!Allow a thermal loss factor, supplied by the user, to tweak the thermal losses
Q_thermal = q_inc_sum-(q_inc_sum-Q_thermal) 
q_conv_sum = q_conv_sum 
q_rad_sum = q_rad_sum 

!MJW 8.2.2010 :: After convergence, determine whether the mass flow rate falls below the lower limit
!if(m_dot_salt_tot < m_dot_htf_min) goto 900
if(Q_thermal < q_rec_min) goto 900  !mjw 3.10.11


goto 999 !Normal operation, skip to the end

900 continue  !Receiver isn't producing usable energy
m_dot_salt_tot=0.d0 ; eta_therm=0.d0 ; W_dot_pump=0.d0  
q_conv_sum=0.d0 ; q_rad_sum=0.d0 ; T_s=0.d0 ;  Q_thermal=0.d0
!set the receiver outlet temperature equal to the inlet design temperature
T_salt_hotX = T_htf_cold_des !mjw 3.29.11
q_inc_sum = 0.d0 !mjw 3.29.11
!pressure drops
deltap = 0.d0; pres_d = 0.d0; u_coolant = 0.d0
999 continue  !Normal operation, 

!#######################################################################################################################

!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------



! SET THE OUTPUTS FROM THIS MODEL IN SEQUENTIAL ORDER AND GET OUT

OUT(1)=(m_dot_salt_tot)*3600 !Convert to kg/hr
OUT(2)=eta_therm
OUT(3)=W_dot_pump/1.e6 !Convert We to MWe
OUT(4)=q_conv_sum/1.e6 !Convert Wt to MWt
OUT(5)=q_rad_sum/1.e6  !Convert Wt to MWt
OUT(6)=Q_thermal/1.e6  !Convert Wt to MWt
OUT(7)=	T_salt_hotX - 273.15 !Convert from K back to C
OUT(8)=OUT(6)+abs(OUT(5)+OUT(4)) !Power before thermal losses
out(9)= field_eff_adj !mjw 3.29.11 adjusted field efficiency
OUT(10) = q_inc_sum/1.e6 !MWt  !Total incident power on the receiver
OUT(11) = q_startup  !MWt  Startup energy consumed during the current time step
OUT(12) = DeltaP*N_panels/nlines/1.e5 ![bar] receiver dP only
OUT(13) = pres_d*10.d0 ![bar] total dP
OUT(14) = u_coolant ![m/s] 

RETURN 1
END
!-----------------------------------------------------------------------------------------------------------------------

!#######################################################################################################################
!	THE FOLLOWING SECTION CONTAINS THE SUBROUTINES CALLED BY TYPE222
!#######################################################################################################################

subroutine flowPatterns(N_panels,flowtype,Flow_pattern,salt_out,nlines)
implicit none

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! This subroutine takes the number of panels, the requested flow type, and 
! returns the corresponding flow pattern and panel numbers where the salt 
! exits.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
integer,intent(in)::N_panels
integer,dimension(N_panels)::Flow_pattern
integer,dimension(2)::salt_out
integer::nlines,flowtype,i

if((flowtype<=4).and.(dble(floor(N_panels/2.)).lt.(dble(N_panels)/2.))) then
    call messages(-1,'The number of panels for this flow configuration must be divisible by 2',"Fatal",0,222)
endif

select case(flowtype)
case(1)
    !This flow pattern begins at the northmost 2 panels, splits into 2 flows, and crosses over
    !  at the quarter position, exiting in 2 flows on the southmost 2 panels. This is the flow
    !  configuration that was used for SOLAR II
    nlines = 2
    salt_out(1)= 1
    salt_out(2)= N_panels
    Flow_Pattern = (/(i+1,i=1,(floor(N_panels/4.)-1)),(N_panels-floor(N_panels/4.)),(i+1,i=(floor(N_panels/4.)+1),&
                     (N_panels/2)-1),0,0,(i,i=N_panels/2+1,(N_panels-floor(N_panels/4.)-1)),(floor(N_panels/4.)+1),&
                     (i-1,i=(N_panels-floor(N_panels/4.)+2),N_panels),(0,i=1,100-N_panels)/)
case(2)
    !This flow pattern is the same as flow pattern #1, but in reverse. The salt enters
    !  on the 2 southmost panels, crosses over, and exits on the 2 northmost panels.
    nlines = 2
    salt_out(1)=N_panels/2
    salt_out(2)=N_panels/2+1
    Flow_Pattern = (/0,(i-1,i=2,floor(N_panels/4.)),(N_panels+1-floor(N_panels/4.)),(i-1,i=floor(N_panels/4.)+2,N_panels/2),&
                    (i+1,i=(N_panels/2+1),(N_panels-floor(N_panels/4.)-1)),(floor(N_panels/4.)),(i+1,i=(N_panels-floor(N_panels/4.)+1),&
                    (N_panels-1)),0,(0,i=1,100-N_panels)/)
case(3)
!This flow pattern has 2 separate flows that enter in 2 of the northmost panels
!  and flow around (without crossing over), exiting at the 2 southmost panels
    nlines=2
    salt_out(1)=1
    salt_out(2)=N_panels
    Flow_Pattern = (/(i+1,i=1,N_panels/2-1),0,0,(i-1,i=(N_panels/2+2),N_panels),(0,i=1,100-N_panels)/)
case(4)
!This flow pattern has 2 separate flows that enter in 2 of the southmost panels
!  and flow around (without crossing over), exiting at the 2 northmost panels
    nlines=2
    salt_out(1)=N_panels/2
    salt_out(2)=N_panels/2+1
    Flow_Pattern = (/0,(i-1,i=2,N_panels/2),(i+1,i=N_panels/2+1,N_panels-1),0,(0,i=1,100-N_panels)/)
case(5)
    !This flow type enters on a panel at the southmost side of the receiver,
    !  travels completely around the receiver in a clockwise direction,
    !  and exits again on the south side
    nlines=1
    salt_out(1)=N_panels
    Flow_Pattern = (/0,(i-1,i=2,N_panels),(0,i=1,100-N_panels)/)
case(6)
    !This flow type enters on a panel at the southmost side of the receiver,
    !  travels completely around the receiver in a counter-clockwise direction,
    !  and exits again on the south side
    nlines=1
    salt_out(1)=1
    Flow_Pattern = (/(i+1,i=1,N_panels-1),0,(0,i=1,100-N_panels)/)
case(7)
    !This flow type enters on a panel at the northmost side of the receiver,
    !  travels completely around the receiver in a clockwise direction,
    !  and exits again on the north side
    nlines=1
    salt_out(1)=floor(N_panels/2.)
    Flow_Pattern = (/N_panels,(i-1,i=2,floor(N_panels/2.)),0,(i-1,i=floor(N_panels/2.)+2,N_panels),(0,i=1,100-N_panels)/)
case(8)
    !This flow type enters on a panel at the northmost side of the receiver,
    !  travels completely around the receiver in a counter-clockwise direction,
    !  and exits again on the north side
    nlines=1
    salt_out(1)=floor(N_panels/2.)+1
    Flow_Pattern = (/(i+1,i=1,floor(N_panels/2.)-1),0,(i+1,i=floor(N_panels/2.)+1,N_panels-1),1,(0,i=1,100-N_panels)/)
end select

end subroutine

!#######################################################################################################################

subroutine fluxinterp(LU_flux,zen_in,azi_in,array,info)

!**********************************************************************
! This subroutine reads the 1-D fluxmap output by the PTGEN.exe       *
! program and locates the flux map closest to the requested azimuth/  *
! zenith angle, returning it in an array.  For justification of this  *
! selection method, see the PTGEN portion of the Plant Sizing and     *
! Optimization chapter.                                               *
! Inputs and outputs for this model include:                          *
! INPUTS:                                                             *
!   LU_flux :: The logical unit to be used for the fluxmap file       *
!   zen_in  :: The solar zenith angle                                 *
!   azi_in  :: The solar azimuth angle                                *
! OUTPUTS:                                                            *
!   array   :: a 12x1 array of the flux distrubution around the       *
!              circumference of the receiver                          *  
!**********************************************************************

implicit none
integer(4),intent(in)::LU_flux,INFO(15)
real(8),intent(in)::zen_in,azi_in
real(8),intent(out)::array(12) 
real(8)::xdist,hold
real(8),allocatable,save::azimuth(:),zenith(:), day(:), time(:), array_all(:,:)
integer(4)::mylen,i,j,ios,numrec,p1
logical::is_there
character::checkname*300, test*300

!Save the variables that are used in future runs
save::numrec

!MJW 8.17.2010 :: Initial timestep call. allocate arrays, open and read in the flux file
if(info(7)==-1) then
    !Open the fluxmap file used for generating the flux distribution on the receiver
    inquire(unit=LU_flux,opened=is_there,name=checkname)
    if (.not.is_there) then
        open(unit=LU_flux,file=trim(checkname),iostat=ios,status="OLD",position="REWIND")
        !Check to see if the file was opened successfully
        if(ios.ne.0) then
            call messages(-1,"TRNSYS did not find the fluxmap file",'FATAL',INFO(1),INFO(2))
            return   !MJW 8.12.2010
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
    
    !Allocate the arrays
    if(.not.allocated(azimuth)) allocate(azimuth(numrec),zenith(numrec),day(numrec),time(numrec),array_all(numrec,12))
    azimuth(:)=0.d0; zenith(:)=0.d0; day(:)=0.d0; time(:)=0.d0; array_all(:,:)=0.d0

    !The data begins on line #5 of the fluxmap.csv file
    rewind(LU_flux)
    do i=1,4
        read (LU_flux,*)
    enddo
    i=0
    do i=1,numrec !while(ios.eq.0)
        read (LU_flux,fmt=500,iostat=ios) day(i),time(i),azimuth(i),zenith(i)
        500     format(3(F5.1,1X),F5.1)
    enddo
    
    
    !Read in all of the data into the array_all variable
    do i=1,3
        read(LU_flux,*)
    enddo
    do i=1,numrec
        read(LU_flux,fmt='(12(F12.8,1X))') (array_all(i,j),j=1,12)  !array is always 12 wide
        if(i>=numrec) exit
        do j=1,5
            read(LU_flux,*)
        enddo
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
array(:)=array_all(p1,:)

end subroutine


!#######################################################################################################################


double precision function Nusselt_FC(ksDin,Re)
implicit none

double precision,intent(in)::ksDin,Re
double precision::Nomval,ValHi,ValLo,ValHi2,ValLo2, Nu_Hi,Nu_Lo,chi,ksD
integer::rerun

!This is the forced convection correlation presented in [Siebers & Kraabel, 1984]
!The value of ks\D determines the interpolation that takes place between these 4 data points.
ksD = ksDin
Nomval = ksD
rerun=0
!Select the bounding conditions

10	continue
!Point 1: ksD=0.0
if(ksD.lt.75e-5) then
    Nusselt_FC=.3+.488*Re**.5*(1.+(Re/282000.)**.625)**.8
    ValHi=75.e-5
    ValLo=0.0
else
    !Point 2: ksD= 75 x 10**(-5)
    if((ksD>=75.e-5).and.(ksD<300.e-5)) then
        ValHi=300.e-5
        ValLo=75.e-5
        if(Re<=7e5) then
            Nusselt_FC=.3+.488*Re**.5*(1.+(Re/282000.)**.625)**.8
        else
            if((Re>7.0e5).and.(Re<2.2e7)) then
                Nusselt_FC=2.57e-3*Re**.98
            else
                Nusselt_FC=.0455*Re**.81
            endif
        endif
    else
        !Point 3: ksD= 300 x 10**(-5)
        if((ksD>=300.0e-5).and.(ksD<900.0e-5)) then
            ValHi=900.0e-5
            ValLo=300.0e-5
            if(Re<=1.8e5) then
                Nusselt_FC=.3+.488*Re**.5*(1.+(Re/282000.)**.625)**.8
            else
                if((Re>1.8e5).and.(Re<4.0e6)) then
                    Nusselt_FC=.0135*Re**.89
                else
                    Nusselt_FC=.0455*Re**.81
                endif
            endif
        else
            !Point 4: ksD = 900 x 10**(-5)
            if(ksD>=900.0e-5) then
                ValHi=900.0e-5
                ValLo=900.0e-5
                if(Re<=1e5) then
                    Nusselt_FC=.3+.488*Re**.5*(1.+(Re/282000.0)**.625)**.8
                else
                    Nusselt_FC=.0455*Re**.81
                endif
            endif
        endif
    endif
endif

if (rerun.eq.1) then
    goto 20
else
    rerun=1
    Nu_Lo=Nusselt_FC
    ksD=ValHi
    ValLo2=ValLo
    ValHi2=ValHi
    goto 10
endif

20 continue
Nu_hi=Nusselt_FC

if (Nomval.ge.900.0e-5) then
    chi=0
else
    chi=(Nomval-ValLo2)/(ValHi2-ValLo2)
endif

Nusselt_FC=Nu_Lo+(Nu_hi-Nu_Lo)*chi

end function

!#######################################################################################################################