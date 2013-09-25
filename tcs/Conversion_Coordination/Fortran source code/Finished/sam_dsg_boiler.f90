subroutine Boiler(TIME,XIN,XOUT,PAR,INFO,N_panels,Q_inc_b,boiler_exit,checkflux)

! Doc. tables updated 2011-10-12 - TN
!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Parameters
!    1| D_rec                            | Diameter of receiver                                              | m                | m                
!    2| Per_rec                          | Perimeter of receiver                                             | m                | m                
!    3| hl_ffact                         | Heat Loss Fudge FACTor                                            | none             | none             
!    4| H_rec                            | Height of boiler                                                  | m                | m                
!    5| D_tube                           | O.D. of boiler tubes                                              | m                | m                
!    6| th_tube                          | Thickness of boiler tubes                                         | m                | m                
!    7| eps_tube                         | Emissivity of boiler tubes                                        | none             | none             
!    8| abs_tube                         | Absorptivity of boiler tubes                                      | none             | none             
!    9| Mat_tube                         | Numerical code for tube material (2: stainless, 29: T-91)         | none             | none             
!   10| th_fin                           | Thickness of fin                                                  | m                | m                
!   11| L_fin                            | Length of fin (distance between boiler tubes)                     | m                | m                
!   12| eps_fin                          | Emissivity of fin material                                        | none             | none             
!   13| abs_fin                          | Absorptivity of fin material                                      | none             | none             
!   14| Mat_fin                          | Numerical code for fin material                                   |                  |                  

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Inputs
!    1| T_amb                            | Ambient temperature                                               | K                | K                
!    2| T_sky                            | Sky temperature                                                   | K                | K                
!    3| v_wind                           | Wind speed                                                        | m/s              | m/s              
!    4| P_atm                            | Ambient pressure                                                  | Pa               | Pa               
!    5| T_fw                             | Feedwater temperature                                             | K                | K                
!    6| P_in_PB                          | Inlet pressure                                                    | kPa              | kPa              
!    7| x_out_target                     | Outlet quality                                                    | none             | none             
!    8| m_dot_in                         | Guessed mass flow rate                                            | kg/s             | kg/s             
!    9| m_dot_lower                      | Lower limit on possible mass flow rates                           | kg/s             | kg/s             
!   10| m_dot_upper                      | Upper limit on possible mass flow rates                           | kg/s             | kg/s             

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Outputs
!    1| m_dot_in                         | Solved mass flow rate through boiler                              | kg/s             | kg/s             
!    2| m_dot_v_total                    | Mass flow rate of VAPOR through boiler                            | kg/s             | kg/s             
!    3| m_dot_l_total                    | Mass flow rate of LIQUID through boiler                           | kg/s             | kg/s             
!    4| eta_rec                          | Efficiency of boiler                                              | none             | none             
!    5| T1_max                           | Max temp of boiler surface                                        | K                | K                
!    6| Tfin_max_out                     | Max temp of fin                                                   | K                | K                
!    7| T_n_in                           | Exit (boiling) temperature                                        | K                | K                
!    8| Energy_out                       | Energy transferred to steam                                       | W                | W                
!    9| eta_fin                          | Efficiency of fin                                                 | none             | none             
!   10| Energy_in                        | Flux * ReceiverArea                                               | W                | W                
!   11| x_path_out                       | Maximum outlet quality                                            | none             | none             
!   12| x_path_out                       | Minimum outlet quality                                            | none             | none             
!   13| P_out_avg                        | Average outlet pressure                                           | kPa              | kPa              
!   14| T_in                             | Boiler Inlet Temperature                                          | K                | K                
!   15| rho_n_out                        | Outlet density                                                    | kg/m^3           | kg/m^3           
!   16| h_fw                             | Feedwater enthalpy                                                | kJ/kg            | kJ/kg            
!   17| rho_fw                           | Feedwater density                                                 | kg/m^3           | kg/m^3           
!   18| h_x1                             | Superheater inlet enthalpy                                        | kJ/kg            | kJ/kg            
!   19| Q_conv_boiler                    | Total convective loss from boiler                                 | MW               | MW               
!   20| Q_rad_boiler                     | Total radiative loss from boiler                                  | MW               | MW               
!   21| Q_abs_boiler                     | Total energy rate absorbed by boiler                              | MW               | MW               


!USE FluidProperties
USE Water_properties
use global_props

implicit none

!declare other variables here  
real(8),intent(in)::PAR(15),XIN(10),TIME
integer*4,intent(in)::info(15),checkflux,N_panels
integer*4,intent(inout)::boiler_exit
real(8):: XOUT(21),xx(N_panels)
real(8):: T_amb,T_sky,u_wind,P_atm,v_wind,T_drum,H_rec,D_tube,th_tube,eps_tube,abs_tube,th_fin,L_fin,&
            eps_fin,abs_fin,D_rec,Per_rec,Per_panel,sigma,grav,pi,RelRough,W_assem,L_fr,L,D_in,A_t_cs,A_n_proj,A_n_in_act,A_fin,Beta,FNodes,Nusselt_FC,&
            hl_ffact,m,ksD,q_fin,eta_fin,h_c,h_mixed,q_wf,h_n_in,G,h_n_ave,x_n_ave,rho_n_ave,T_2,k_n,conductivity,r_n,&
            q_t_flux,rho_l,h_v,h_diff,mu_l,k_l,c_l,alpha_l,pr_l,h_fluid,h_l,rho_v,mu_v,h_n_out_total,h_by_m,m_dot_total,&
            rho_in,rho_n_in,rho_n_out,mu_f,Re_LO,Re_TP,f_wLO,f_wTP,phi_LOsq,rho_f,dpdz_fLO,dpdx_grav,dpdx_fTP,dp_acc,deltaP_tube,L_eff_90,L_eff_45,deltaP_in,&
            dp,r_conv,diff_T_1,mu_n_ave,k_n_ave,u_n,Re,f_fd,alpha_n,c_n_ave,Pr,Nusselt,k_v,c_v,T_1_upper,T_1_lower,y_T_upper,y_T_lower,m_dot_min,&
            Mat_tube,Mat_fin,P_in,T_in,T_in1,x_n_out,m_dot_v_total,m_dot_l_total,energy_in,energy_out,eta_rec,T1_max,Tfin_max,Q_inc_b(N_panels),&
            diff_T1_g,diff_m_dot,T_1_max,dx,L_eff,&
            u_n_max,Q_sum,x_out_target,m_dot_in,diff_P_path,T_n_in,P_n_in,diff_T_HT,diff_P_ave,P_ave,&
            P_upper,P_lower,diff_P_bracket,P_out,T_1,h_n_out,m_dot,h_b_max,T_n_ave,u_n_exit,grav_mult,diff_x_out,&
            m_dot_lower,m_dot_upper,P_out_avg,y_m_upper,y_m_lower,P_b_min,m_dot_max,&
            h_fw,P_out_guess,h_x0,diff_Pout,h_in,P_in_PB,uplast_mult,T_1_min,T_fw,rho_fw,h_x1,Tfin_max_out,&
            Q_conv_boiler,Q_rad_boiler,Q_abs_boiler
real(8)::eta_finX(N_panels),q_fin1X(N_panels),eta_fin_out(N_panels),T_max_out(N_panels),T_base_prev(N_panels)
real(8),allocatable,save::Q_inc(:),Flow_Pattern(:,:),m_dot_v(:),&
                            Q_adj(:),q_wf1(:),q_wf_total(:),m_dot_path(:),h_path_out(:),P_path_out(:),x_path_out(:),&
                            Q_conv(:),Q_rad(:),Q_abs(:)
                            
integer*4,allocatable::Flow_Pat(:,:),Flow_Pat_adj(:,:)
integer*4:: N_par,N_fr,Nodes,T1_br_upper,T1_br_lower,Pout_iter,x_br_upper,x_br_lower
integer*4:: i,eff_set(N_panels),dummy,up1,k,j,N_comb,CombNodes,flowtype,diff_path_iter,P_ave_iter,P_br_low,Pave_flag,T_1_iter,mdot_flag,x_iter,open_status12
character:: error_message*255
logical:: Tupflag,Tlowflag,HT_flag,enth_flag,modelfin,P_upguess,P_lowguess,lowpres_flag,mupflag,mlowflag

!**************************************************
!********* Read Inputs and Parameters and Perform Initial Calculations **************
!**************************************************
!####################################
!$$$Parameters
!####################################
    !Boiler Dimensions & Properties
IF(info(7)==-1)THEN
    dummy       = up1(0)
           
    D_rec       = PAR(up1(1))           ![m] "Diameter" of boiler for external convection correlation
    Per_rec     = PAR(up1(1))           ![m] Perimeter of boiler
    hl_ffact    = PAR(up1(1))           ![-] Heat Loss Fudge FACTor
    flowtype    = int(PAR(up1(1)))      ![-] Specifies flow path through receiver
    H_rec       = PAR(up1(1))           ![m] Height of receiver
    D_tube      = PAR(up1(1))           ![m] Outer diameter of boiler tubes
    th_tube     = PAR(up1(1))           ![m] Thickness of boiler tubes 
    eps_tube    = PAR(up1(1))           ![-] Emissivity of boiler tubes 
    abs_tube    = PAR(up1(1))           ![-] Absorptivity of boiler tubes 
    Mat_tube    = PAR(up1(1))           ![-] Numerical code for tube material (2: Stainless_AISI316, 28: T-91 Steel) 
    th_fin      = PAR(up1(1))           ![m] Thickness of fin 
    L_fin       = PAR(up1(1))           ![m] Length of fin (distance between boiler tubes)
    eps_fin     = PAR(up1(1))           ![-] Emissivity of fin
    abs_fin     = PAR(up1(1))           ![-] Absorptance of fin 
    Mat_fin     = PAR(up1(1))           ![-] Numerical code for fin material (2: Stainless_AISI316, 28: T-91 Steel)

    !Absorptivity (and reflectivity) are accounted for in DELSOL efficiency files, so set to 1 here
    abs_tube    = 1.d0
    abs_fin     = 1.d0

!    H_rec       = 10.d0             ![m] Height of boiler 
!    L_rec       = 16.5d0            ![m] Length of square boiler
!    W_rec       = 16.5d0            ![m] Width of square boiler
!    D_tube      = 0.0254            ![m] Outer diameter of boiler tubes
!    th_tube     = 0.003175          ![m] Thickness of boiler tubes 
!    eps_tube    = 1.d0              ![-] Emissivity of boiler tubes 
!    abs_tube    = 0.95d0            ![-] Absorptivity of boiler tubes 
!    Mat_tube    = 28.d0             ![-] Numerical code for tube material (2: Stainless_AISI316, 28: T-91 Steel) 
!        !Possible Fin Dimensions & Properties
!    th_fin      = 0.0015875         ![m] Thickness of fin 
!    L_fin       = 0.0254            ![m] Length of fin (distance between boiler tubes)
!    eps_fin     = 1.d0              ![-] Emissivity of fin
!    abs_fin     = 0.95d0            ![-] Absorptance of fin 
!    Mat_fin     = 28.d0             ![-] Numerical code for fin material (2: Stainless_AISI316, 28: T-91 Steel)

        !Boiler Configuration
!    D_rec       = (L_rec**2 + W_rec**2)**0.5    ![m] "Diameter" of boiler for external convection correlation
!    Per_rec     = 2.*L_rec + 2.*W_rec           ![m] Perimeter of boiler

!    !*********** Only consider this arrangement **************************************
!    N_fr        = N_panels          ![-] Assume only upward flow through only ONE panel before returning to steam drum
!    !**********************************************************************************
    !*** Hardcode receiver flow information ***
    N_fr        = 2                 ![-] Number of flow paths: Hardcode to 2 for steam receiver 
    N_comb      = 1                 ![-] Number of parallel panels in flow path. Hardcode to 1. Effectively reduces the number of receiver panels.
                                        ! Receiver panels should not be decreases below 12 due to flux interpolation
!    flowtype    = 2                 ![-] Specifies flow path through receiver: south to north
!    flowtype    = 1

        !Heat Transfer Info
    hl_ffact    = 1.d0              ![-] Heat Loss Fudge FACTor
    m           = 3.2               ![-] Exponential for calculating mixed convection
    !####################################
    !$$$End of Parameters
    !####################################

    !#####################################
    !$$$Initial Calculations: Once per simulation
    !#####################################

    !Internal Modeling Settings
    FNodes      = 10                            ![-] Model fin with 10 nodes

    Per_panel   = Per_rec/dble(N_panels)        ![m] "Perimeter" of individual panel
    Nodes       = N_panels/N_fr                 ![-] Nodes per flow path: INTEGER

    sigma       = 5.67D-8                       ![W/m2K4] stefan boltzmann constant
    grav        = 9.81                          ![m/s^2] Gravitional constant
    pi          = 3.1415926
    RelRough    = (1.5D-6)/D_tube               ![-] Relative roughness of the tubes: http.www.efunda/formulae/fluids/roughness.cfm

    W_assem     = D_tube + L_fin                ![m] Total width of one tube/fin assembly
    N_par       = floor(Per_panel / W_assem)    ![-] Number of parallel assemblies per panel
    L_fr        = H_rec * Nodes                 ![m] Length of one path through boiler
    L           = H_rec                         ![m] Distance through one node

    D_in        = D_tube - 2.*th_tube           ![m] Inner diameter of tube
    A_t_cs      = pi*d_in**2/4.d0               ![m^2] Cross-sectional area of tubing
    A_n_proj    = d_tube * L                    ![m^2] Projected Area ** Node **
    A_n_in_act  = pi * d_in * 0.5d0 * L         ![m^2] ACTIVE inside surface area - nodal
    A_fin       = L_fin*0.5*L                   ![m^2] Area of 1/2 of fin
    ksD         = (D_tube/2.)/D_rec             ![-] The effective roughness of the cylinder [Siebers, Kraabel 1984]

    L_eff_90    = 30.                           ![m] Effective length for pressure drop for 90 degree bend
    L_eff_45    = 16.                           ![m] Effective length for pressure drop for 45 degree bend
    
    IF(L_fin < 0.0001)THEN
        modelfin    = .false.
        q_fin       = 0.d0
        eta_fin     = -999.d0
        Tfin_max    = -999.d0
    ELSE
        modelfin    = .true.
        dx          = (L_fin/2.d0)/(dble(FNodes)-1.)    !Distance between nodes in numerical fin model
        L_eff       = 0.5*L_fin
    ENDIF
    
    CombNodes = (Nodes/N_comb)
    
        !Allocate Arrays
    IF(.not.allocated(Q_inc))THEN
        allocate(Q_inc(N_panels))
        allocate(Flow_Pat(N_fr,Nodes),m_dot_path(N_fr),m_dot_v(N_fr))
        allocate(Flow_Pattern(N_fr,Nodes))
        allocate(Flow_Pat_adj(N_fr,CombNodes),Q_adj(N_fr*CombNodes),q_wf1(CombNodes),q_wf_total(N_fr),h_path_out(N_fr),P_path_out(N_fr),x_path_out(N_fr),Q_conv(N_fr*CombNodes),Q_rad(N_fr*CombNodes),Q_abs(N_fr*CombNodes))
    ENDIF
    
    !Panels are numbered clockwise starting at the panel clockwise-adjacent to due south.
    !For a 12-panel receiver and flowtype = 2:
        !Flow Path 1 = {1, 2, 3, 9, 8, 7}
        !Flow Path 2 = {12, 11, 10, 4, 5, 6}
    CALL flowPatternsDSR(N_panels,flowtype,Flow_Pat)

    !Sorted number of independent panels in each flow path. For 12 panel receiver with 2 parallel flow panels:
        !Flow Path 1 = {1, 2, 3}
        !Flow Path 2 = {4, 5, 6}
    DO j=1,N_fr
        DO i=1,(CombNodes)
            Flow_Pat_adj(j,i) = i + (Nodes/N_comb)*(j-1)
        ENDDO
    ENDDO      
      
    RETURN
      
ENDIF

if(info(8)==-1) then
    deallocate(Q_inc)
    deallocate(m_dot_path,m_dot_v,h_path_out,P_path_out,Flow_pat)
    deallocate(Flow_Pattern)
    deallocate(Flow_Pat_adj,Q_adj,q_wf1,q_wf_total,x_path_out,Q_rad,Q_conv,Q_abs)
    
    return
    
endif

!######################################    
!$$$Inputs
!######################################
    !Ambient Conditions
    !XIN(T_amb,T_sky,v_wind,P_atm,h_in,P_in,x_out)
    
T_amb   = XIN(1)            ![K] Ambient Temperature
T_sky   = XIN(2)            ![K] Sky Temperature
v_wind  = XIN(3)            ![K] Wind Speed
P_atm   = XIN(4)            ![Pa] Ambient Pressure
T_fw    = XIN(5)            ![K] Feedwater temperature
P_in_PB = XIN(6)            ![kPa] Inlet Pressure
x_out_target = XIN(7)          ![-] Outlet Quality
m_dot_in = XIN(8)           ![kg/s] Guessed Mass Flow Rate
m_dot_lower = XIN(9)        ![kg/s] Lower limit on possible mass flow rates
m_dot_upper = XIN(10)       ![kg/s] Upper limit on possible mass flow rates 
    
DO i=1,N_panels
    Q_inc(i)    = Q_inc_b(i) !150000.d0 !- 5000.*(i-1)              ![W/m^2] Incident Radation
ENDDO

Energy_In   = sum(Per_panel * H_rec * Q_inc(:))     ![W] Total energy incident on receiver

!Create new flux arrays that allow for multiple panels in parallel
!Q_adj sorts flux into flow path order, and is indexed with Flow_Pat_Adj
Q_sum = 0.d0
k = 1
DO j=1,N_fr
    DO i=1,Nodes
        Q_sum = Q_sum + Q_inc(Flow_Pat(j,i))
        IF(mod(i,N_comb)==0)THEN
            Q_adj(k)    = Q_sum/dble(N_comb)
            Q_sum       = 0.d0
            k = k + 1
        ENDIF
    ENDDO
ENDDO

!Check if enough flux is available to produce positive net energy through each flow path
DO j=1,N_fr         
    DO i=1,CombNodes                                                         !Choose the panel that we are modeling 
        h_c         = h_mixed(T_fw,T_amb,v_wind,ksD,hl_ffact,P_atm,grav,beta,H_rec,D_rec,m)     ![W/m^2-K] Function calculates combined free and forced convection coefficient, same as used in fin HT model
        q_conv(Flow_Pat_Adj(j,i))      = h_c*(A_n_proj + 2.d0*A_fin)*(T_fw - T_amb)                                ![W] Convective heat transfer                                                     
        q_rad(Flow_Pat_Adj(j,i))       = eps_tube*sigma*(A_n_proj + 2.d0*A_fin)*(0.5*(T_fw**4 - T_sky**4) + 0.5*(T_fw**4 - T_amb**4))    ![W] Radiative heat transfer: 8/10/11, view factor to ground ~ ambient (1/2) 
        q_abs(Flow_Pat_Adj(j,i))       = Q_adj(Flow_Pat_Adj(j,i))*abs_tube*(A_n_proj + 2.d0*A_fin)                 ![W] Irradiance absorbed by panel
        q_wf1(i)    = q_abs(Flow_Pat_Adj(j,i)) - q_conv(Flow_Pat_Adj(j,i)) - q_rad(Flow_Pat_Adj(j,i))                                                    ![W] Heat transfer to working fluid solved by energy balance
    ENDDO
    q_wf_total(j)   = sum(q_wf1)
    IF(q_wf_total(j)<0.d0)THEN
        boiler_exit    = 2         !Set flag for calling code
        RETURN
    ENDIF
ENDDO

IF(checkflux == 1) RETURN

eff_set = 0.d0              ![-] Need to set fin efficiency in subroutine
u_n_max = 0.d0              ![m/s] Maximum velocity through boiler

Beta    = 1.d0 / T_amb      ![1/K] Volumetric expansion coefficient

!Set bounds on possible mass flow rate guesses
!m_dot_lower   = 0.5*m_dot_in
!m_dot_upper   = 1.5*m_dot_in
m_dot_min     = m_dot_lower
m_dot_max     = m_dot_upper

!********** Guess Average (w/r/t flow paths) Outlet Pressure ***********
P_out_guess = 0.99*P_in_PB     ![kPa]
diff_Pout = 999.d0
Pout_iter = 0

!DO WHILE( (abs(diff_Pout)>0.01).and.(Pout_iter<20).and.(P_in < 19000.d0) )
!Adjust steam drum pressure to equal calculated boiler outlet pressure
DO WHILE( (abs(diff_Pout)>0.01).and.(Pout_iter<20) )

    Pout_iter = Pout_iter + 1
    
    !IF(Pout_iter == 20)  Pause "Exceeds Boiler Outlet Pressure Iteration"
    
    !Guess a new boiler outlet pressure based on previous results
    IF(Pout_iter > 1)THEN
        P_out_guess = 0.5*P_out_guess + 0.5*P_out_avg
        !Can also reset mass flow rate guess and limits
            !If new pressure is lower then, by convention, feedwater enthalpy will be lower, requiring more specific energy input into the flow
            !to reach a specified quality, therefore, mass flow rate will decrease.
        IF(P_out_avg < P_out_guess)THEN
            !m_dot_lower = m_dot_in
            !m_dot_upper = 0.95*m_dot_in
            m_dot_lower = 0.95*m_dot_in
            m_dot_upper = m_dot_in
            m_dot_min   = m_dot_lower
            m_dot_max   = m_dot_upper
        ELSE
            !m_dot_lower = 1.05*m_dot_in
            !m_dot_upper = m_dot_in
            m_dot_lower = m_dot_in
            m_dot_upper = 1.05*m_dot_in
            m_dot_min   = m_dot_lower
            m_dot_max   = m_dot_upper
        ENDIF
    ENDIF     

    call Water_PQ(P_out_guess,0.d0,enth = h_x0)     ![kJ/kg] Enthalpy of saturated liquid recirculating to steam drum
    call Water_TP(T_fw-273.15d0,P_out_guess,enth = h_fw, dens=rho_fw)     ![kJ/kg] Enthalpy of feedwater entering steam drum
    
    h_in = x_out_target*h_fw + (1.d0 - x_out_target)*h_x0   ![kJ/kg] Energy balance to find enthalpy of feedwater/recirc mixture    

    !P(kPa),T(C),enth(kJ/kg),dens(kg/m3),inte(kJ/kg),entr(kJ/kg-K),cp(kJ/kg-K),cond(W/m-K),visc(kg/m-s)
    CALL Water_PH(P_in_PB,h_in,dens=rho_in)         ![kg/m^3] Find density of mixture
    deltaP_in   = rho_in*grav*L                     ![Pa] Hydrostatic pressure assuming water level is at top of tubes
    
    !8/20/11, Need to account for the gravity head so we don't observe large pressure drops while not exceeding Dyreby props pressure limitations
    P_in = (P_in_PB + deltaP_in/1000.d0)            ![kPa] Inlet pressure adding gravity head from steam drum
    call Water_PH(P_in,h_in,temp = T_in)            ![C] Temperature at first panel (NOT steam drum) inlet
    T_in = T_in + 273.15d0              ![K] Convert C to K
    h_in = h_in*1000.                   ![J/kg] convert from kJ/kg
     
    h_n_in      = h_in                  ![J/kg-K] Inlet enthalpy
    m_dot_path(:) = m_dot_total/dble(N_fr)      ![kg/s] Mass flow rate through each flow route (divide evenly for now)
    diff_P_path = 999999.d0             !Set difference > tolerance
    diff_path_iter = 0                  !Set iteration counter

    x_iter      = 0
    diff_x_out  = 999.d0
    Pave_flag   = 0
    mdot_flag   = 0
    x_br_upper  = 0
    x_br_lower  = 0
    mupflag     = .false.
    mlowflag    = .false.

    !Adjust mass flow rate to reach target quality
    297 DO WHILE( (abs(diff_x_out)> 0.0035) .and. (x_iter < 20) )

        B_EB_count = B_EB_count + 1
        x_iter = x_iter + 1
        T1_max      = 0.            ![K] Set T1_max
        Tfin_max_out = 0.d0         ![K] Set fin max
        
        !IF(x_iter == 50)  Pause "Exceeds Boiler Mass Flow Rate Iterations"
        
        diff_m_dot = (m_dot_upper - m_dot_lower)/m_dot_upper
        
        IF(x_iter > 1)THEN
            IF((Pave_flag==0).and.(mdot_flag==0))THEN
                IF((mupflag).and.(mlowflag))THEN
                    IF(diff_x_out > 0.d0)THEN
                        x_br_upper  = 1
                        m_dot_upper = m_dot_in
                        y_m_upper   = diff_x_out
                    ELSE
                        x_br_lower  = 1
                        m_dot_lower = m_dot_in
                        y_m_lower  = diff_x_out
                    ENDIF    
                    m_dot_in = (y_m_upper)/(y_m_upper-y_m_lower)*(m_dot_lower - m_dot_upper) + m_dot_upper
                ELSE        
                    IF(diff_x_out > 0.d0)THEN
                        x_br_upper  = 1
                        m_dot_upper = m_dot_in
                        mupflag  = .true.
                        y_m_upper   = diff_x_out
                    ELSE
                        x_br_lower  = 1
                        m_dot_lower = m_dot_in
                        mlowflag = .true.
                        y_m_lower  = diff_x_out
                    ENDIF
                    m_dot_in = 0.5*m_dot_upper + 0.5*m_dot_lower
                ENDIF 
            ELSEIF(Pave_flag==1)THEN
                x_br_upper  = 2
                m_dot_upper = m_dot_in
                m_dot_in = 0.5*m_dot_upper + 0.5*m_dot_lower
            ELSEIF(mdot_flag==1)THEN
                x_br_lower  = 2
                m_dot_lower = m_dot_in
                m_dot_in = 0.5*m_dot_upper + 0.5*m_dot_lower
            ENDIF       
        ENDIF                
        
        IF(diff_m_dot < 0.01)THEN
            
            IF( (m_dot_upper < 1.01*m_dot_min) .and. (x_br_lower==0) )  x_br_lower = 3
            IF( (m_dot_lower > 0.99*m_dot_max) .and. (x_br_upper==0) )  x_br_upper = 3
            
            !Should not occur with enough iterations
            !IF((x_br_lower==1).and.(x_br_upper==1))THEN
            
            ! Exit quality too high / Pressure drop too high
            IF((x_br_lower==1).and.(x_br_upper==2))THEN
                boiler_exit = 3 !too much flux
                RETURN
                
            ! Exit quality too high / No upper limit on mass flow rate
            ELSEIF((x_br_lower==1).and.(x_br_upper==3))THEN
                boiler_exit = 3 ! too much flux
                RETURN
            
            ! Mass flow rate too low for energy balance / Exit quality too low
            ELSEIF((x_br_lower==2).and.(x_br_upper==1))THEN
                boiler_exit = 2 !not enough flux
                RETURN
                
            ! Mass flow rate too low for energy balance / Pressure drop too high
            ELSEIF((x_br_lower==2).and.(x_br_upper==2))THEN 
                boiler_exit = 3 !too much flux
                RETURN

            ! Mass flow rate too low for energy balance / No upper limit on mass flow rate
            ELSEIF((x_br_lower==2).and.(x_br_upper==3))THEN
                boiler_exit = 3 !too much flux
                RETURN
            
            ! No lower limit on mass flow rate / Exit quality too low
            ELSEIF((x_br_lower==3).and.(x_br_upper==1))THEN
                boiler_exit = 2 !not enough flux
                RETURN
                
            ! No lower limit on mass flow rate / Pressure drop too high
            ELSEIF((x_br_lower==3).and.(x_br_upper==2))THEN
                boiler_exit = 3 !too much flux
                RETURN
                
            ENDIF
            
        ENDIF
        
        !Mass flow rate in one tube - ASSUMING ONE FLOW PATH
        m_dot_total     = m_dot_in / (dble(N_par)*dble(N_comb))     ![kg/s]
        m_dot_path = m_dot_total / 2.d0         ![kg/s] Divide flow evenly between flow paths

        !Solve for outlet conditions of each flow path
        DO j=1,N_fr                             

            h_n_in      = h_in                  ![J/kg-K] Inlet enthalpy of first node
            T_n_in      = T_in                  ![K] Inlet temperature of first node
            rho_n_in    = rho_in                ![kg/m^3] Inlet density of first node
            P_n_in      = P_in*1000.d0          ![Pa] Inlet pressure of first node
            dp          = 2.e4                  ![Pa] Guess small pressure drop
            diff_T_HT   = 45.d0                 ![K] Estimate of difference between surface temp and inlet temp
            m_dot       = m_dot_path(j)         ![kg/s] Use m_dot so we don't have to carry array through
            
            DO i=1,CombNodes            !Now, solve energy balance for each node (or panel) in flow path.  Output of i is input to i+1

                IF(i==1)THEN
                    T_1        = T_n_in + 45.d0
                ELSE
                    T_1        = T_n_in + 1.5*diff_T_HT
                ENDIF
                
                IF(mod(i,2)==0)THEN
                    grav_mult = -1.d0   !For even numbered panels, flow is downwards
                ELSE    
                    grav_mult = 1.d0    !For odd numbered panels, flow is upwards
                ENDIF
                       
                diff_P_ave = 9999999.d0                 ![Pa] Set diff > tolerance
                P_ave_iter = 0                          ![-] Set iteration counter
                P_out      = max(1.e6, P_n_in - dp)     ![Pa] Guess outlet pressure of flow path
                P_ave      = 0.5*P_n_in + 0.5*P_out     ![Pa] Guess average pressure through flow path
                P_upguess  = .false.                    ![-] Has upper guess on pressure been set?
                P_lowguess = .false.                    ![-] Has lower guess on pressure been set?
                P_upper    = P_n_in                     ![Pa] Initial upper pressure is equal to inlet pressure
                P_lower    = 500000.d0                  ![Pa] Some minimum outlet pressure
                diff_P_bracket = 999.d0                 ![Pa] Difference between upper and lower guesses
                lowpres_flag = .false.                  ![-] Did model trip due to low pressure?
                P_br_low    = 0                         ![-] Flag to show how energy balance solved
                Pave_flag   = 0   
                mdot_flag   = 0
                        
                    !An average pressure of the node is used as one independent variable to calculate additional properties.
                    !After pressure drop through node is calculated, want to make sure calculated average pressure is within some % of average used for props.
                DO WHILE((abs(diff_P_ave)>0.001d0).and.(P_ave_iter < 125))
                    
        348         P_ave_iter  = P_ave_iter + 1        !Increase iteration counter
                    !IF(P_ave_iter == 125)  CONTINUE !Pause "Exceeds SH average pressure iteration"
                    !IF(P_ave_iter == 125)  Pause "Exceeds Boiler average pressure iteration"

                    IF(P_ave_iter > 1)THEN
                        IF((P_upguess).and.(P_lowguess))THEN
                            IF(diff_P_ave<0)THEN
                                P_br_low    = 1
                                P_lower     = P_ave
                            ELSE
                                P_upper     = P_ave
                            ENDIF
                            P_ave   = 0.5*P_lower + 0.5*P_upper
                        ELSE                
                            IF(diff_P_ave<0)THEN
                                P_br_low    = 1
                                P_lower     = P_ave
                                P_lowguess  = .true.
                            ELSE
                                P_upper     = P_ave
                                P_upguess   = .true.
                            ENDIF
                            P_ave   = (P_n_in + P_out) / 2.d0                   ![Pa] New average pressure
                        ENDIF
                        P_b_min = 1.E6              ![Pa] Minimum boiler outlet pressure
                        P_ave   = max(1.E6, P_ave)
                        P_out   = 2.d0*(P_ave - 0.5*P_n_in)
                        diff_P_bracket  = (P_upper - P_lower)/P_lower
                    ENDIF

                    !If a pressure near the minimum pressure has caused the code to guess a lower pressure, then flag and guess lower mass flow
                    IF(P_upper < 1.01*P_b_min)THEN 
                    
                        Pave_flag = 1
                        GOTO 297
                              
                    ENDIF

                    lowpres_flag    = .false.           ![-] Reset flag marking an outlet pressure below allowed minimum

                    diff_T_1    = 999.                  ![K] Set diff > tolerance
                    T_1_iter    = 0                     ![-] Set iteration counter 
                    
                    T_1_max     = 800.d0               ![K] Set some maximum allowable surface temperature to control iteration
                    G           = m_dot / A_t_cs                         ![kg/m^2-s] Quantity in boiling correlation
                    
                    !Set limits on surface temperature iteration
                    T_1_upper   = T_1_max               ![K]
                    T_1_lower   = T_n_in - 15.d0        ![K]
                    T_1_min     = T_1_lower             ![K]
                    T_1         = min( max(T_1_lower, T_1), T_1_upper)
                    
                    diff_T1_g   = 999.                  ![K] Reset difference between upper and lower iteration limits

                    !Reset iteration logic and integer flags
                    Tupflag     = .false.               ![-] Has a temperature difference at the upper bound been established
                    Tlowflag    = .false.               ![-] Has a temperature difference at the lower bound been established
                    HT_flag     = .false.               ![-] Did the energy balance encounter low heat transfer rates
                    enth_flag   = .false.               ![-] Did the energy balance encounter high heat transfer rates
                    T1_br_lower = 0                     ![-] Flag to show how energy balance solved
                    T1_br_upper = 0                     ![-] Flag to show how energy balance solved
                    !Reset temperature calculation errors at high and low guesses
                    y_T_upper   = 0.d0                  ![K] Temperature difference at upper bound
                    y_T_lower   = 0.d0                  ![K] Temperature difference at lower bound
                    
                    !Need properties at saturated vapor in case some guess in energy balance results in x<1
                    !P(kPa),T(C),enth(kJ/kg),dens(kg/m3),inte(kJ/kg),entr(kJ/kg-K),cp(kJ/kg-K),cond(W/m-K),visc(kg/m-s)
                    !CALL Water_PQ(P_ave/1000.d0,1.d0,enth = h_b_max)
                    CALL Water_PQ(min((P_ave/1000.d0),19000.d0),1.d0,enth = h_b_max)        !Set max value when calling, not in rest of code
                    
                    !This loop ensures that the outlet flow conditions for each panel are solved correctly by find the correct T1 (outer surface temp)
        272         DO WHILE((abs(diff_T_1/T_1)>0.0025d0).AND.(T_1_iter < 50)) 
        !272         DO WHILE((abs(diff_T_1)>1.d0).AND.(T_1_iter < 50)) 
        
                        T_1_iter    = T_1_iter + 1      ![-] Add to iteration counter
                        !IF(T_1_iter == 50)  CONTINUE !Pause "Exceeds Superheater Energy Balance Iterations"
                        !IF(T_1_iter == 50)  Pause "Exceeds Boiler Energy Balance Iterations"

                        IF(T_1_iter > 1) THEN      !If first iteration, then use initial guess
                            IF(((.not.(HT_flag)).and.(.not.(enth_flag)))) THEN
                                IF(abs(diff_T_1)>50.d0)THEN         !If error is very large, use bisection rather than false interpolation method
                                    IF(diff_T_1>0.)THEN             !If old temp is higher than new temp, then old temp is too high, so:
                                        T1_br_upper = 2
                                        T_1_upper   = T_1           ![K] Set upper limit
                                        Tupflag     = .false.
                                    ELSE
                                        T1_br_lower = 2
                                        T_1_lower   = T_1           ![K] Set lower limit
                                        Tlowflag    = .false.
                                    ENDIF
                                    T_1         = 0.5*T_1_lower + 0.5*T_1_upper ![K] Bisection method to calculate next T_1 guess
                                    GOTO 474
                                ENDIF
                                IF((Tupflag).and.(Tlowflag))THEN    !If bracket results are set, use false position
                                    IF(diff_T_1>0.)THEN             !If old temp is higher than new temp, then old temp is too high, so:
                                        T1_br_upper = 2
                                        T_1_upper   = T_1           ![K] Set upper limit
                                        y_T_upper   = diff_T_1      ![K] Set upper bracket result
                                    ELSE                            !If old temp is lower than new temp, then old temp is too low, so:
                                        T1_br_lower = 2
                                        T_1_lower   = T_1           ![K] Set lower limit
                                        y_T_lower   = diff_T_1      ![K] Set lower bracket result
                                    ENDIF
                                    T_1 = (y_T_upper)/(y_T_upper-y_T_lower)*(T_1_lower - T_1_upper) + T_1_upper          ![K] False position method
                                ELSE                                !If bracket results are not set:
                                    IF(diff_T_1>0.)THEN             !If old temp is higher than new temp, then old temp is too high, so:
                                        T1_br_upper = 2
                                        T_1_upper   = T_1           ![K] Set upper limit
                                        y_T_upper   = diff_T_1      ![K] Set upper bracket result
                                        Tupflag     = .true.        !Set logic to show that upper bracket is set
                                    ELSE
                                        T1_br_lower = 2
                                        T_1_lower   = T_1           ![K] Set lower limit
                                        y_T_lower   = diff_T_1      ![k] Set lower bracket result
                                        Tlowflag    = .true.        !Set logic to show that lower bracket is set
                                    ENDIF
                                    IF((Tupflag).and.(Tlowflag))THEN 
                                        T_1     = (y_T_upper)/(y_T_upper-y_T_lower)*(T_1_lower - T_1_upper) + T_1_upper     ![K] False position method
                                    ELSE
                                        T_1     = T_1 - diff_T_1                                ![K] Calculate new T_1 based on difference
                                        IF((T_1 < T_1_lower).or.(T_1 > T_1_max)) THEN
                                            T_1         = 0.5*T_1_lower + 0.5*T_1_upper         ![K] Bisection method
                                        ELSE
                                            T_1     = max(T_1_lower, min(T_1_max, T_1))
                                        ENDIF
                                    ENDIF    
                                ENDIF
                            ELSEIF(enth_flag)THEN
                                T1_br_lower = 1
                                T_1_lower   = T_1
                                T_1         = 0.5*T_1_lower + 0.5*T_1_upper     ![K] Bisection method
                                enth_flag   = .false.
                            ELSEIF(HT_flag)THEN
                                T1_br_upper = 1
                                T_1_upper   = T_1
                                T_1         = 0.5*T_1_lower + 0.5*T_1_upper     ![K] Bisection method
                                HT_flag     = .false.
                            ENDIF
                        ENDIF
                           
        474             diff_T1_g   = (T_1_upper - T_1_lower)       ![K] If iteration window is small, look for way out
                                
                        !If the bracket from which to guess T_1 is small, then try to exit loop                
                        IF(diff_T1_g < 0.01)THEN
                        
                            IF( (T_1_lower > T_1_max - 0.02) .and. (T1_br_upper==0) )   T1_br_upper = 3
                            IF( (T_1_upper < 0.02 + T_1_min) .and. (T1_br_lower==0) )    T1_br_lower = 3 
                            
                            !T1_br_lower==1: Enth_flag, the resulting enthalpy from the heat addition is too large for steam property look-up routine
                            !T1_br_lower==2: Calculated surface temperature is higher than guessed
                            !T1_br_lower==3: Lower limit has not been set during iteration and is equal to inlet temperature
                            
                            !T1_br_upper==1: HT Flag, negative (or nearly negative) energy input to boiler
                            !T1_br_upper==2: Calculated surface temperature is lower than guessed
                            !T1_br_upper==3: Upper limit has not been set during iteration and is equal to maximum value
                            
                            !*********** New 8/26/11 ***********************************
                            ! Basically means any energy gain to fluid will result in fluid reaching an enthalpy too high for fluid props.  This should
                            ! not happen since code is stopping if outlet on previous panel is greater than target temp
                            !IF( ((T1_br_lower==1).and.(T1_br_upper==1)) )THEN
                            
                            ! Need to increase mass flow rate in hopes of eliminating enth_flag                                     
                            IF( ((T1_br_lower==1).and.(T1_br_upper==2)) )THEN  
                                mdot_flag = 1
                                GOTO 297
                                                    
                            ! Need to increase mass flow rate in hopes of eliminating enth_flag
                            ELSEIF( ((T1_br_lower==1).and.(T1_br_upper==3)) )THEN
                                mdot_flag = 1
                                GOTO 297
                            
                            ! Increase mass flow rate to bring down average fluid temp
                            ELSEIF( ((T1_br_lower==2).and.(T1_br_upper==1)) )THEN
                                mdot_flag = 1
                                GOTO 297
                                                                                                      
                            !This should not happen given enough iterations
                            !ELSEIF( ((T1_br_lower==2).and.(T1_br_upper==2)) )THEN
                            
                            !Increase mass flow rate
                            ELSEIF( ((T1_br_lower==2).and.(T1_br_upper==3)) )THEN
                                mdot_flag = 1
                                GOTO 297
                            
                            !Increase mass flow rate
                            ELSEIF( ((T1_br_lower==3).and.(T1_br_upper==1)) )THEN
                                mdot_flag = 1
                                GOTO 297                            
                            
                            !Increase mass flow rate
                            ELSEIF( ((T1_br_lower==3).and.(T1_br_upper==2)) )THEN
                                mdot_flag = 1
                                GOTO 297
                            ENDIF
                            
                            !Should not happen
                            !ELSEIF( ((T1_br_lower==3).and.(T1_br_upper==3)) )THEN                        
                            !*********************************************************************************
                        ENDIF                   
                           
                        !!Calculate Energy Inputs and Outputs on Surface (with guessed T_1)
                        !Subroutine to calculate energy contribution to tube from fin as well as "fin efficiency", based on a fin with infinite thermal conductance
                        !Call FinIn(Q_adj(i),Beta,abs_fin,L_fin,Mat_Fin,FNodes,T_1(i),T_amb,P_atm,v_wind,ksD,D_rec,hl_ffact,H_rec,grav,m,eps_fin,sigma,T_sky,th_fin,q_fin,eta_fin,Tfin_max)
                        IF(modelfin)   Call FinIn(TIME,N_panels,eff_set,i,Q_adj(Flow_Pat_Adj(j,i)),Beta,abs_fin,Mat_Fin,FNodes,T_1,T_amb,P_atm,v_wind,ksD,D_rec,hl_ffact,H_rec,grav,m,eps_fin,sigma,T_sky,&
                                        th_fin,L_eff,dx,q_fin,eta_fin,Tfin_max,eta_finX,q_fin1X,eta_fin_out,T_max_out,T_base_prev)
                        h_c         = h_mixed(T_1,T_amb,v_wind,ksD,hl_ffact,P_atm,grav,beta,H_rec,D_rec,m)       ![W/m^2-K] Function calculates combined free and forced convection coefficient, same as used in fin HT model
                        q_conv(Flow_Pat_Adj(j,i))      = h_c*A_n_proj*(T_1 - T_amb)                                                 ![W] Convective heat transfer                                                 
                        q_rad(Flow_Pat_Adj(j,i))       = eps_tube*sigma*A_n_proj*(0.5*(T_1**4 - T_sky**4) + 0.5*(T_1**4 - T_amb**4)) ![W] Radiative heat transfer: 8/10/11, view factor to ground ~ ambient (1/2)
                        q_abs(Flow_Pat_Adj(j,i))       = Q_adj(Flow_Pat_Adj(j,i))*abs_tube*A_n_proj + 2*q_fin*L                                        ![W] Absorbed radiation + fin contributions(2: 1 on each side)
                        q_wf        = q_abs(Flow_Pat_Adj(j,i)) - q_conv(Flow_Pat_Adj(j,i)) - q_rad(Flow_Pat_Adj(j,i))                                                        ![W] Heat transfer to working fluid solved by energy balance                                                     
                        
                        !If "not much" energy is available, flag and reguess T_1
!                        IF(q_wf < 10.)THEN
!                            HT_flag = .true.
!                            GOTO 272
!                        ENDIF                                                   

                        !Equation: m_dot*(h_out-h_in) = q_wf
                        h_n_out     = q_wf/m_dot + h_n_in           ![J/kg] Calculate outlet enthalpy according to calculated energy addition to working fluid
                        h_n_ave     = (h_n_in + h_n_out)/2.d0       ![J/kg] Determine average enthalpy in node
                                 
                        !If the outlet enthalpy is greater than property routine can handle, flag and reguess T_1
                        IF(h_n_out > (1.5*h_b_max*1000.d0))THEN
                            enth_flag = .true.
                            GOTO 272
                        ENDIF
                        
                        !*********  REFPROP **********************************************************************
                        !P(kPa),T(C),enth(kJ/kg),dens(kg/m3),inte(kJ/kg),entr(kJ/kg-K),cp(kJ/kg-K),cond(W/m-K),visc(kg/m-s),qual(-)
627                     CALL Water_PH(min((P_ave/1000.d0),19000.d0),h_n_ave/1000.d0,dens=rho_n_ave,qual=x_n_ave,visc=mu_n_ave,cond=k_n_ave,cp=c_n_ave,temp=T_in1)!,qual=x_n_ave,dens=rho_n_ave)
                        !CALL Water_PH(P_n_in/1000.d0,h_n_ave/1000.d0,dens=rho_n_ave,qual=x_n_ave,visc=mu_n_ave,cond=k_n_ave,cp=c_n_ave,temp=T_in1)!,qual=x_n_ave,dens=rho_n_ave)
                        !*****************************************************************************************
                        
                        !Check for Dyreby props failing near vapor dome
                        IF((c_n_ave < 0.d0).or.(k_n_ave < 0.d0))THEN
                            h_n_ave = 0.99 * h_n_ave
                            GOTO 627
                        ENDIF
                        
                        T_in1       = T_in1 + 273.15d0              ![K] Convert C to K
                        
                        q_t_flux    = q_wf / (A_n_in_act)           ![W/m^2] Heat FLUX transferred to working fluid: required for boiling heat transfer correlation
                        u_n         = m_dot / (rho_n_ave*A_t_cs)    ![m/s] Velocity through tube
                        
                        IF((x_n_ave < 1.).and.(x_n_ave > 0.))THEN   !If average flow quality shows fluid is two phase, then call 'boiling flow' correlation
                                            
                            !Need props at saturated liquid for boiling correlations
                            !P(kPa),T(C),enth(kJ/kg),dens(kg/m3),inte(kJ/kg),entr(kJ/kg-K),cp(kJ/kg-K),cond(W/m-K),visc(kg/m-s)
                            !CALL Water_PQ(P_n_in/1000.d0,0.d0,enth=h_l,dens=rho_l,visc=mu_l,cond=k_l,cp=c_l) 
                            CALL Water_PQ(min((P_ave/1000.d0),19000.d0),0.d0,enth=h_l,dens=rho_l,visc=mu_l,cond=k_l,cp=c_l) 
                            h_l     = h_l*1000.             ![J/kg] Convert kJ/kg to J/kg
                            c_l     = c_l*1000.             ![J/kg-K] Specific heat at x = 0 
                            !*****************************************************************************************

                            !*********  REFPROP **********************************************************************
                            !Need props at saturated vapor for boiling correlations
                            !P(kPa),T(C),enth(kJ/kg),dens(kg/m3),inte(kJ/kg),entr(kJ/kg-K),cp(kJ/kg-K),cond(W/m-K),visc(kg/m-s)
                            !CALL Water_PQ(P_n_in/1000.d0,1.D0,enth=h_v,dens=rho_v,visc=mu_v,cond=k_v,cp=c_v)
                            CALL Water_PQ(min((P_ave/1000.d0),19000.d0),1.D0,enth=h_v,dens=rho_v,visc=mu_v,cond=k_v,cp=c_v)
                            h_v     = h_v*1000.             ![J/kg] Enthalpy at x = 1
                            c_v     = c_v*1000.             ![J/kg-K] Specific heat at x = 1
                            !*****************************************************************************************

                            h_diff  = h_v - h_l             ![J/kg] Heat of Vaporization
                            alpha_l = k_l / (rho_l*c_l)     ![m^2/s] Thermal diffusivity of saturated liquid
                            Pr_l    = (mu_l/rho_l)/alpha_l  ![-] Prandtl number of saturated liquid
                                !Determine heat transfer coefficient for boiling flow
                            Call Flow_Boiling(T_in1,T_2,G,d_in,x_n_ave,q_t_flux,rho_l,rho_v,k_l,mu_l,Pr_l,h_l,h_diff,grav,mu_v,c_v,k_v,RelRough,h_fluid)
                                !Flow_Boiling(T_fluid,G,d,x_in,q_t_flux,rho_l,k_l,mu_l,Pr_l,enth_l,h_diff,grav,h_fluid)
                                
                        ELSE                                        !Otherwise, use single phase flow correlations
                            !***** Need correlation for single phase flow as well ************************************
                            Re          = rho_n_ave * u_n * d_in / mu_n_ave     ![-] Reynolds number
                            f_fd        = (-2.0*log10(2.*RelRough/7.54 - 5.02*log10( 2.*RelRough/7.54 + 13./Re)/Re))**(-2)  ![-](Moody) friction factor (Zigrang and Sylvester)

                            f_fd        = (-2.0*log10(2.*relrough/7.54 - 5.02*log10( 2.*relrough/7.54 + 13./Re)/Re))**(-2)

                            c_n_ave     = c_n_ave * 1000.                       ![J/kg-K] convert from kJ/kg-K
                            alpha_n     = k_n_ave/(rho_n_ave*c_n_ave)           ![m^2/s] Thermal diffusivity of fluid
                            Pr          = mu_n_ave/(alpha_n*rho_n_ave)          ![-] Prandtl number      
                            Nusselt     = ((f_fd/8.)*(Re-1000.)*Pr)/(1.+12.7*sqrt(f_fd/8.)*(Pr**(2./3.)-1.)) ![-] Turbulent Nusselt Number (Gnielinksi)
                            h_fluid     = Nusselt*k_n_ave/d_in                  ![W/m^2-K] Convective heat transfer coefficient
                            !******************************************************************************************
                        ENDIF
                        !h_fluid     = max(0.1, h_fluid)                         !Don't let h = 0
                        !6/5/12, TN: Minimum convective heat transfer coefficient should be conduction
                        h_fluid     = max(k_n_ave/d_in, h_fluid)
                        
                        R_conv      = 1.d0 / (h_fluid * A_n_in_act)                     ![K/W] Thermal resistance to convection
                        T_2         = q_wf*R_conv + T_in1                               ![K] Calculate T_2 
                        k_n         = Conductivity(Mat_tube,((T_1+T_2)/2.d0),1.d0)   ![W/m-K] Conductivity of tube using average temperature
                        R_n         = log(d_tube/d_in)/(k_n*2.d0*pi*L/2.d0)             ![K/W] Thermal resistance of ACTIVE tube
                        diff_T_1    = T_1 - (T_2 + q_wf*R_n)                         ![K] Calculate difference between assumed T_1 and calculated T_1                                
                        !diff_T_1    = (T_1 - (T_2 + q_wf*R_n))/T_1                     ![K] Calculate difference between assumed T_1 and calculated T_1                                
                        
                    ENDDO       !End Energy Balance Iteration
                    
                    IF(x_n_ave<-1.)  x_n_out = -10.d0
                    IF(x_n_ave>1.)  x_n_out = 10.d0
                    
                    !Calculate the pressure drop through the panel
                    IF((x_n_ave < 1.).and.(x_n_ave > 0.))THEN               !2 Phase Pressure Drop
                        !Pressure drop calcs: get props at outlet
                        !*********  REFPROP **********************************************************************
                        !P(kPa),T(C),enth(kJ/kg),dens(kg/m3),inte(kJ/kg),entr(kJ/kg-K),cp(kJ/kg),cond(W/m-K),visc(kg/m-s),qual(-)
                        !CALL Water_PH(P_n_in/1000.d0,h_n_out/1000.d0,dens=rho_n_out,qual=x_n_out)
                        CALL Water_PH(min((P_out/1000.d0),19000.d0),h_n_out/1000.d0,dens=rho_n_out,qual=x_n_out)
                        !*****************************************************************************************

                        !Frictional pressure drop equations taken from Chexal et al. pages 7-6, 7-7, 7-9
                        mu_f    = (1.d0 - x_n_ave)*mu_l + x_n_ave*mu_v      ![kg/m-s] Mixture viscosity
                        Re_LO   = G * d_in / mu_l                           ![-] Liquid only Reynolds number
                        Re_TP   = G * d_in / mu_f                           ![-] Mixture Reynolds number

                        f_wLO   = 0.0791 / (Re_LO**0.25)                    ![-] Liquid only friction factor: Blasius single-phase Fanning friction factor
                        f_wTP   = f_wLO                                     ![-] Two-phase friction factor defined as liquid

                        phi_LOsq    = f_wTP * rho_l / (f_wLO*rho_n_ave)     ![-] Two-phase friction multiplier
                        dpdz_fLO    = 4.d0/d_in*0.5*f_wLO*G*G/rho_l         ![-] Liquid-only pressure drop
                        dpdx_fTP    = dpdz_fLO * phi_LOsq                   ![-] Two-phase friction factor

                        dpdx_grav   = grav_mult * rho_n_ave * grav          ![Pa/m] Pressure due to elevation

                        dp_acc      = G*G*(1.d0/rho_n_out - 1.d0/rho_n_in)  ![Pa/m] Pressure loss due to acceleration of fluid

                        deltaP_tube = (dpdx_fTP + dpdx_grav)*L + dp_acc + (dpdx_fTP*L_eff_90*d_in*4.d0 + dpdx_fTP*L_eff_45*d_in*2.d0) ![Pa] Pressure loss through tube
                        !**********************************************************************************************************************
                    ELSE                                                    !Single Phase Pressure Drop
                        deltaP_tube = (f_fd * L * rho_n_ave * u_n**2 / (2.d0 * d_in)) + 2.*(f_fd * L_eff_45 * rho_n_ave * u_n**2 / 2.d0) + 4.*(f_fd * L_eff_90 * rho_n_ave * u_n**2 / 2.d0) ![Pa] Pressure loss through tube              
                        deltaP_tube = deltaP_tube + grav_mult*rho_n_ave*grav*L        !Add pressure due to elevation!!!!!!!!!
                    ENDIF                                             
                     
                    P_out       = P_n_in - deltaP_tube                      ![Pa] Outlet pressure 
                    diff_P_ave  = (P_ave - (P_n_in + P_out)/2.d0)/P_n_in    ![Pa] Difference between ave. pressure guessed and ave. pressure calculated                
                
                ENDDO       !Average pressure iteration

                !Don't let boiler approach superheater: it affects the temperature limits on the energy balance
                IF((x_n_out > 0.85).and.(i<CombNodes))THEN
                    mdot_flag = 1
                    GOTO 297
                ENDIF

                P_n_in  = P_out                 ![Pa] Calculate inlet pressure of next node
                h_n_in  = h_n_out               ![J/kg] Set inlet enthalpy for next node
                rho_n_in = rho_n_out            ![kg/m^3] Set inlet density for next node

                !P(kPa),T(C),enth(kJ/kg),dens(kg/m3),inte(kJ/kg),entr(kJ/kg-K),cp(kJ/kg-K),cond(W/m-K),visc(kg/m-s)
                CALL Water_PH(min((P_out/1000.),19000.d0),h_n_in/1000.,Temp=T_n_in)      !calculate temperature corresponding to outlet enthalpy and pressure
                
                T_n_in  = T_n_in + 273.15       ![K]
                
                diff_T_HT = max(0.d0, (T_1 - T_n_in))          ![K] Difference between surface temp and inlet temp: used to estimate T_1 of next node
                !T1_max  = max(T1_max, T_1)      ![K] Calculate highest tube temperature
                !11/15/11, TN: Change max temperature to estimate surface temp at hot end, not center (where energy balance is calculated)
                T1_max  = max(T1_max, T_n_in + (T_1 - T_in1))
                Tfin_max_out = max(Tfin_max_out,Tfin_max)
            ENDDO               !End evaluation of 1 flow path

            IF(mod(CombNodes,2)==0)THEN
                uplast_mult = 1.d0
            ELSE
                uplast_mult = 0.d0
            ENDIF

            x_path_out(j)   = x_n_out           ![-] Outlet quality of path
            h_path_out(j)   = h_n_out           ![J/kg] Keep track of enthalpy outlet of flow path
            P_path_out(j)   = P_out -  uplast_mult*rho_n_ave*grav*L            ![Pa] Keep track of pressure outlet of flow path
            u_n_exit        = max(u_n_exit, u_n)![m/s] Maximum outlet velocity

        ENDDO       !End of evaluation of both flow paths

        P_out_avg       = min( (sum(P_path_out)/dble(N_fr)/1.e3), 19000.d0)            ![kPa] Average (flow paths) outlet pressure

        !All flow paths have been solved (with equal pressure drops), so complete cumulative calculations to determine final mixed enthalpy
        h_by_m      = 0.d0              ![W] Enthalpy-mass flow rate product
        DO i=1,N_fr
            h_by_m      = h_by_m + h_path_out(i)*m_dot_path(i)
        ENDDO

        h_n_out_total = h_by_m / m_dot_total                    ![J/kg] Total mass flow rate time mixed enthalpy = sum(h*m_dot)
        CALL Water_PH(P_out_avg,h_n_out_total/1000.d0,qual=x_n_out,dens=rho_n_out)  !Calculate final combined outlet quality

        diff_x_out = x_out_target - x_n_out
        
        IF(x_iter==20)THEN
            continue    
        ENDIF

    ENDDO           !Outlet quality iteration

    diff_Pout = (P_out_guess - P_out_avg)/P_out_guess
    continue

ENDDO               !(Total Boiler) outlet pressure iteration

IF( (x_iter==20) .and. (abs(diff_x_out)> 0.0035) )THEN
    call messages(-1,"The boiler model did not converge on the correct quality",'WARNING',INFO(1),INFO(2))
ENDIF

Energy_out      = m_dot_in * (h_n_out_total - h_in)     ![W] Energy transferred to steam
m_dot_v_total   = x_n_out * m_dot_in                    ![kg/s] Total mass flow rate of vapor
m_dot_l_total   = m_dot_in - m_dot_v_total              ![kg/s] Total mass flow rate of liquid
Q_conv_boiler   = sum(Q_conv)*dble(N_par)*dble(N_comb)/1.E6             ![MW] Total convective loss from boiler
Q_rad_boiler    = sum(Q_rad)*dble(N_par)*dble(N_comb)/1.E6              ![MW] Total radiative loss from boiler
Q_abs_boiler    = sum(Q_abs)*dble(N_par)*dble(N_comb)/1.E6              ![MW] Total energy rate absorbed by boiler

Eta_rec         = Energy_out / Energy_in                ![-] Efficiency of receiver
CALL Water_PQ(P_out_avg,1.d0, enth = h_x1)              ![kJ/kg] Superheater inlet enthalpy

!!Outputs
XOUT(1)         = m_dot_in                              ![kg/s] Total mass flow rate through boiler (liquid and vapor)
XOUT(2)         = m_dot_v_total                         ![kg/s] Total mass flow rate of vapor exiting boiler
XOUT(3)         = m_dot_l_total                         ![kg/s] Total mass flow rate of liquid exiting boiler
XOUT(4)         = eta_rec                               ![-] Efficiency of receiver
XOUT(5)         = T1_max                                ![K] Maximum calculated boiler tube outer surface temperature
XOUT(6)         = Tfin_max_out                          ![K] Maximum fin (or rib) temperature (if applicable)
XOUT(7)         = T_n_in                                ![K] Saturation Temperature
XOUT(8)         = Energy_out                            ![W] Energy transferred to steam
XOUT(9)         = eta_fin                               ![-] Efficiency of fin between tubes (if applicable)
XOUT(10)        = Energy_in                             ![W] Flux * receiverArea
XOUT(11)        = maxval(x_path_out)                    ![-] Maximum outlet quality
XOUT(12)        = minval(x_path_out)                    ![-] Minimum outlet quality
XOUT(13)        = P_out_avg                             ![kPa] Average outlet pressure
XOUT(14)        = T_in                                  ![K] Boiler Inlet Temperature
XOUT(15)        = rho_n_out                             ![kg/m^3] Outlet Density
XOUT(16)        = h_fw                                  ![kJ/kg] Feedwater enthalpy
XOUT(17)        = rho_fw                                ![kg/m^3] Feedwater density
XOUT(18)        = h_x1                                  ![kJ/kg] Superheater inlet enthalpy
XOUT(19)        = Q_conv_boiler                         ![MW] Total convective loss from boiler
XOUT(20)        = Q_rad_boiler                          ![MW] Total radiative loss from boiler
XOUT(21)        = Q_abs_boiler                          ![MW] Total energy rate absorbed by boiler

END subroutine Boiler !PROGRAM

!******************************************************************
!***** Calculate Fin Input to Tubing ******************************
!******************************************************************

subroutine FinIn(TIME,N_panels,eff_set,panelnum,Q_inc,Beta,abs_fin,Mat_Fin,FNodes,T_base,T_amb,P_atm,v_wind,ksD,D_rec,hl_ffact,H_rec,grav,m,eps_fin,sigma,T_sky,&
                th_fin,L_eff,dx,q_fin1,eta_fin,T_max,eta_finX,q_fin1X,eta_fin_out,T_max_out,T_base_prev)

implicit none

real(8),intent(in)::TIME,Q_inc,Beta,abs_fin,Mat_Fin,FNodes,T_base,T_amb,P_atm,v_wind,ksD,D_rec,hl_ffact,H_rec,grav,m,eps_fin,sigma,T_sky,th_fin,dx
integer*4,intent(inout)::N_panels,eff_set(N_panels)
real(8)::eta_finX(N_panels),q_fin1X(N_panels),eta_fin_out(N_panels),T_max_out(N_panels),T_base_prev(N_panels)
real(8)::S_fin,L_eff,q_cond,q_lower,q_upper,q_fin1,T_node,T_next,q_cond_prev,denom1,T_film,k_film,mu_film,rho_film,c_p_film,&
            conductivity,viscosity,density,specheat,Re_for,Nusselt_for,h_for,Nusselt_FC,nu_amb,Gr_nat,Nusselt_nat,h_nat,h_c,h_c2,h_mixed,T_diff,k_node,q_in,q_conv_out,&
            q_rad_out,q_ideal,y_lower,y_upper,diff_T_input,q_fin_max,eta_fin,T_max,T_cond
integer*4::q_iter,i,k_iter,panelnum
logical::upflag,lowflag,lowguess

upflag      = .false.           !Set logic to switch from bisection to false position mode
lowflag     = .false.           !Set logic to switch from bisection to false position mode
diff_T_input = T_base - T_base_prev(panelnum)
lowguess    = .false.

S_fin       = Q_inc * abs_fin   ![W/m^2] Irradiance absorbed by fin [W/m^2]
q_fin_max   = S_fin*L_eff


IF(eff_set(panelnum)==1)THEN
    IF(abs(diff_T_input)<0.1d0)THEN
    !IF(abs(diff_T_input/T_base)<0.005d0)THEN
        q_fin1 = q_fin1X(panelnum)
        eta_fin = eta_fin_out(panelnum)
        T_max = T_max_out(panelnum)
        RETURN
    ENDIF
    q_fin1  = eta_finX(panelnum)
    IF(diff_T_input > 0.d0)THEN !Temperature increases, so efficiency will decrease
        q_upper  = q_fin1X(panelnum)
        IF(diff_T_input > 30.d0)THEN
            q_fin1  = eta_finX(panelnum)*max(0.1d0,(1 - diff_T_input/600.d0))*q_fin_max
        ELSE
            q_fin1  = (1.d0 - 0.25*(diff_T_input/30.d0))*eta_finX(panelnum)*q_fin_max
        ENDIF
    ELSE
        q_lower  = q_fin1X(panelnum)
        q_upper  = q_fin_max
        lowguess = .true.
        IF(diff_T_input < 30.d0)THEN  !Temperature decrease, so efficiency will increase
            q_fin1  = eta_finX(panelnum)*min(1.d0,(1 - 0.1*diff_T_input/600.d0))*q_fin_max
        ELSE
            q_fin1  = min(1.d0,(1 - 0.1*diff_T_input/30.d0))*eta_finX(panelnum)*q_fin_max
        ENDIF
    ENDIF   
ELSE
    q_fin1  = 0.70*q_fin_max
    q_upper = q_fin_max
ENDIF

q_iter      = 0                 ![-] Set iteration counter for outside loop
q_cond      = 10000.d0              ![W/m] Set conductive HT to value > tolerance

!Begin outside loop: vary q_fin1 to determine when adiabatic end condition is satisfied
DO WHILE((dabs(q_cond/q_fin1)>0.01).AND.(q_iter<200))

    q_iter      = q_iter + 1    ![-] Iteration Counter  

    IF(q_iter > 1) THEN         !Any iteration after 1st needs to recalculate q_fin1

        IF((upflag).and.(lowflag))THEN  !If bracketed results are available, use false position
            IF(q_cond > 0.)THEN         !If adiabatic edge is rejecting energy, too much energy is being put into system, so
                q_lower = q_fin1        ![W/m] set lower limit of q_fin1
                y_lower = q_cond        ![W/m] Set lower bracket result
            ELSE                        !Adiabatic edge is "intaking" energy, so not enough energy is being put into system, so
                q_upper = q_fin1        ![W/m] set upper limit of q_fin1
                y_upper = q_cond        ![W/m] set upper bracket result
            ENDIF
                q_fin1 = (y_upper)/(y_upper-y_lower)*(q_lower - q_upper) + q_upper      !False Position Method
        ELSE
            IF(q_cond > 0.)THEN         !If adiabatic edge is rejecting energy, too much energy is being put into system, so
                q_lower = q_fin1        ![W/m] set lower limit of q_fin1
                y_lower = q_cond        ![W/m] Set lower bracket result
                lowflag = .true.        !Set logic to show that upper bracket is set
                lowguess= .true.
            ELSE
                q_upper = q_fin1        ![W/m] set upper limit of q_fin1
                y_upper = q_cond        ![W/m] set upper bracket result
                upflag  = .true.        !Set logic to show that lower bracket is set
            ENDIF
            
            IF(.not.(lowguess))THEN
                IF(q_fin1 > 500.d0)THEN
                    q_lower = 0.75*q_fin1
                ELSE
                    q_lower = q_fin1 - 1000.d0
                ENDIF
            ENDIF                
             
            IF((upflag).and.(lowflag))THEN  !If current iteration set the last needed bracket, then we can use false position
                q_fin1 = (y_upper)/(y_upper-y_lower)*(q_lower - q_upper) + q_upper      ![W/m]      
            ELSE                        !Otherwise, use bisection method
                q_fin1 = 0.5*q_upper + 0.5*q_lower                                      ![W/m]
            ENDIF
            
        ENDIF  
        
    ENDIF

    T_node      = T_base        ![K] Guess temperature for first node (assumed = outer tube temp)
    T_next      = T_base        ![K] Guess temperature for next node     
    q_cond_prev = q_fin1        ![W/m] Conductive HT from first node to tube
    
        !Solve for fin nodal temps from tube to adaibatic end
    DO i=1,FNodes
        
        !If on either end, then only 1/2 surface area.  This is due to assumed nodal structure.  See documentation (does not yet exist)
        IF((i==1).or.(i==FNodes)) THEN
            denom1  = 2.d0      ![-]
        ELSE
            denom1  = 1.d0      ![-]
        ENDIF

        !Call Convective Heat Transfer Correlations (Originally in Type 222)
        h_c         = h_mixed(T_node,T_amb,v_wind,ksD,hl_ffact,P_atm,grav,beta,H_rec,D_rec,m)     ![W/m^2-K]
        k_iter      = 0         ![-] Set iteration counter for conductance loop
        T_diff      = 1.d0      ![K] Set temp difference > tolerance
        
        !Energy balance to find temperature of next node.  Since a significant temperature gradient exists, need to iterate on proper conduction
        DO WHILE((abs(T_diff)>0.01).AND.(k_iter<131))        
            k_iter      = k_iter + 1                                                ![-] Add to iteration counter
            T_cond      = min( 0.5*T_node + 0.5*T_next, 1100.d0)                    ![K] Limit to avoid drifting to very low conductivities at high temps
            k_node      = Conductivity(Mat_fin,T_cond,1.d0)      ![W/m-K] Conductivity
            q_in        = S_fin*dx/denom1                                           ![W/m] Radiation absorbed by node [W/m]
            q_conv_out  = h_c*dx/denom1*(T_node-T_amb)                              ![W/m] Energy lost due to convection [W/m]
            q_rad_out   = eps_fin*sigma*dx/denom1*(0.5*(T_node**4 - T_sky**4) + 0.5*(T_node**4 - T_amb**4))            ![W/m] Energy lost due to radiation [W/m] 8/10/11: add view factor to ground (ambient)
            q_cond      = q_in - q_conv_out - q_rad_out - q_cond_prev               ![W/m] Energy balance solved for energy transferred to next node
            T_diff      = T_next - (T_node - q_cond*dx/(k_node*th_fin))             ![K] Difference between T_next used for cond. calcs and calculated T_next
            T_next      = T_next - T_diff                                           ![K] Reset T_next
        ENDDO
        
        ![W/m] If on the node adjacent to tubing (first node), calculate the heat transfer to tube if entire fin were at this node's temperature
        IF(i==1) q_ideal = q_fin_max - h_c*L_eff*(T_node-T_amb) - eps_fin*sigma*L_eff*(T_node**4 - T_sky**4)
        
        
        
        !Update for next node
        q_cond_prev = -q_cond       ![W/m] For remaining nodes, conduction to right is old conduction to left
        T_node      = T_next        ![K]                                                
        
    ENDDO
    
    !write(unit=16,FMT='(5(F12.4,A),2(I3,A))')  TIME, ",", Q_inc, ",", T_base, ",", Q_fin1, ",", q_cond, ",", panelnum, ",", q_iter, ","       

ENDDO


eta_fin_out(panelnum) = q_fin1/q_ideal        ![-] Ratio of energy transferred from "real" fin with temp gradient to "ideal" fin with no gradient
eta_fin = eta_fin_out(panelnum)
T_max_out(panelnum) = T_node
T_max = T_max_out(panelnum)
    
eff_set(panelnum) = 1

eta_finX(panelnum)= q_fin1/q_fin_max
q_fin1X(panelnum) = q_fin1
T_base_prev(panelnum) = T_base

end subroutine

!******************************************************************
!***** END Calculation of Fin Input to Tubing *********************
!******************************************************************

!#######################################################################################
!************ Flow Boiling Model: local HT coefficient: Nellis and Klein (2008)
!#######################################################################################
Subroutine Flow_Boiling(T_sat,T_surf,G,d,x_in,q_t_flux,rho_l,rho_v,k_l,mu_l,Pr_l,enth_l,h_diff,grav,mu_v,c_v,k_v,RelRough,h_fluid)

implicit none

double precision,intent(in)::T_sat,T_surf,G,d,x_in,q_t_flux,rho_l,rho_v,k_l,mu_l,Pr_l,enth_l,h_diff,grav,mu_v,c_v,k_v,RelRough
double precision::h_fluid,Re_l,f_l,h_l,Co,Bo,Fr,N,h_cb,h_nb,h_bs1,h_bs2,h_dim,x,Re_v,f_fd,alpha_v,Pr_v,Nusselt,h_fluid_v,u_n_v,&
                    X_tt,Ga,Fr_mod,A,Nu_fc,c_l,vf,Fr_1,C_1,C_2,T_s
logical::interp

x       = x_in                              !Set quality
Re_l    = G*d*(1.d0-x)/mu_l                 ![-] Eq. 7-10: Reynolds number of saturated liquid flow

IF(q_t_flux < 0.d0)THEN     !Flow Condensation correlations: Section 7.5.2 in Nellis and Klein 
    
    X_tt    = (rho_v/rho_l)**(0.5) * (mu_l/mu_v)**(0.1) * ( (1.d0 - x) / x)**(0.9)          !Eq. 7-105: Lockhart Martinelli parameter
    
    IF(G > 500.d0)THEN            
        h_fluid = k_l/d * 0.023 * Re_l**(0.8) * Pr_l**(0.4) * (1.d0 + 2.22d0/X_tt**(0.89))      !Eq. 7-104
    ELSE
        Ga  = 9.81d0 * rho_l * (rho_l - rho_v) * d**(3.) / mu_l**2.                             !Eq. 7-109
        IF(Re_l <= 1250.d0)THEN
            Fr_mod  = 0.025*Re_l**(1.59)/Ga**(0.5)*( (1.d0+1.09*X_tt**(0.39)) / X_tt)**(1.5)    !Eq. 7-107
        ELSE
            Fr_mod  = 1.26*Re_l**(1.04)/Ga**(0.5)*( (1.d0+1.09*X_tt**(0.39)) / X_tt)**(1.5)     !Eq. 7-108
        ENDIF
        
        IF(Fr_mod > 20.d0)THEN
            h_fluid = k_l / d * 0.023 * Re_l**(0.8) * Pr_l**(0.4) * (1.d0 + 2.22d0/X_tt**(0.89)) !Eq. 7-110
        ELSE
            IF(T_surf > T_sat)THEN
                T_s     = T_sat - 1.d0
            ELSE
                T_s     = T_surf
            ENDIF
            c_l     = Pr_l * k_l / mu_l
            vf      = (1.d0 + (1.d0-x)/x*(rho_v/rho_l)**(2./3.))**(-1.d0)                       !Eq. 7-113
            A       = acos(2.d0*vf - 1.)/3.141                                                  !Eq. 7-112
            Fr_1    = G**2.0 / (rho_l**2.0*9.81d0*d)                                            !Eq. 7-115
            IF(FR_1 > 0.7)THEN          !Eq. 7-116
                C_1 = 7.242
                C_2 = 1.655
            ELSE                        !Eq. 7-117
                C_1 = 4.172 + 5.48*Fr_1 - 1.564*Fr_1**2.
                C_2 = 1.773 - 0.169*Fr_1
            ENDIF
            Nu_fc   = 0.0195*Re_l**(0.8)*Pr_l**(0.4)*(1.376d0 + C_1/X_tt**(C_2))                !Eq. 7-114
            h_fluid = (k_l/d)*( (0.23/(1.d0+1.11d0*X_tt**(0.58d0))) * (G*d/mu_l)**(0.12) * (h_diff/(c_l*(T_sat-T_s)))**(0.25)*Ga**(0.25)*Pr_l**(0.25) + A*Nu_fc)    !Eq. 7-111
        ENDIF
            
    ENDIF                 

ELSE    
    interp  = .false.                               
    IF(Re_l < 2300.)THEN                        !If Re < 2300 (N, then:
        Re_l    = 2300.                         !Set Re to 2300
        x       = 1.d0 - Re_l*mu_l/(G*d)        !Calculate x corresponding to  Re=2300
        interp  = .true.                        !Now need to interpolate to find final heat transfer coefficient    
        
        !G           = m_dot(i) / A_t_cs                         ![kg/m^2-s] Quantity in boiling correlation
        !u_n_v       = m_dot / (rho_v*A_t_cs) ![m/s] Velocity through tube
        u_n_v       = G / rho_v    
        
        !***** Heat transfer coefficient for x=1 ************************************
        Re_v        = rho_v * u_n_v * d / mu_v        ![-] Reynolds number of single phase (x=l) flow
        f_fd        = (-2.0*log10(2.*RelRough/7.54 - 5.02*log10( 2.*RelRough/7.54 + 13./Re_v)/Re_v))**(-2)  ![-](Moody) friction factor (Zigrang and Sylvester)

        alpha_v     = k_v/(rho_v*c_v)               ![m^2/s] Thermal diffusivity of fluid
        Pr_v        = mu_v/(alpha_v*rho_v)          ![-] Prandtl number      
        Nusselt     = ((f_fd/8.)*(Re_v-1000.)*Pr_v)/(1.+12.7*sqrt(f_fd/8.)*(Pr_v**(2./3.)-1.))  ![-] Turbulent Nusselt Number (Gnielinksi)
        h_fluid_v   = Nusselt*k_v/d                 ![W/m^2-K] Convective heat transfer coefficient
        !******************************************************************************************
        
    ENDIF
    f_l     = (0.79*log(Re_l)-1.64)**(-2)       ![-] Eq. 7-12: Friction factor for saturated liquid flow
    h_l     = ((f_l/8.d0)*(Re_l - 1000.d0)*Pr_l)/(1.d0+12.7*(Pr_l**(2./3.)-1.d0)*(f_l/8.d0)**0.5)*(k_l/d)    ![W/m^2-K] Eq. 7-9: Heat transfer correlation for saturated liquid flow
    Co      = (1.d0/x - 1.d0)**0.8*(rho_v/rho_l)**0.5           ![-] Eq. 7-13: Dimensionless parameter
    Bo      = q_t_flux/(G*h_diff)                               ![-] Eq. 7-14: Dimensionless parameter: Boiling number
    Fr      = G**2/(grav*d*rho_l**2)                            ![-] Eq. 7-15: Dimensionless parameter: Froude number
    N       = Co                                                ![-] Eq. 7-16: For vertical tubes

    h_cb    = 1.8*N**-0.8                       ![-] Eq. 7-17

    IF(Bo>=0.3E-4)THEN                          ![-] Eq. 7-18
        h_nb    = 230.d0*Bo**0.5
    ELSE
        h_nb    = 1.d0 + 46.d0*Bo**0.5
    ENDIF

    IF(Bo>=11E-4)THEN                           
        h_bs1   = 14.7*Bo**0.5*exp(2.74*N**-0.1)    ![-] Eq. 7-19
        h_bs2   = 14.7*Bo**0.5*exp(2.47*N**-0.15)   ![-] Eq. 7-20
    ELSE
        h_bs1   = 15.43*Bo**0.5*exp(2.74*N**-0.1)   ![-] Eq. 7-19
        h_bs2   = 15.43*Bo**0.5*exp(2.47*N**-0.15)  ![-] Eq. 7-20
    ENDIF

    If(N<=0.1d0)THEN                            ![-] Eq. 7-21
        h_dim   = MAX(h_cb,h_bs2)
    ELSEIF(N<=1.d0)THEN
        h_dim   = MAX(h_cb,h_bs1)
    ELSE
        h_dim   = MAX(h_cb,h_nb)
    ENDIF

    h_fluid     = h_dim * h_l                   ![W/m^2-K] Eq. 7-8

    IF(interp) h_fluid = (h_fluid_v - h_fluid)/(1.d0 - x) * (x_in - x) + h_fluid

ENDIF
    
end subroutine
!#######################################################################################
!#######################################################################################

double precision function h_mixed(T_node,T_amb,v_wind,ksD,hl_ffact,P_atm,grav,beta,H_rec,D_rec,m)
double precision,intent(in)::T_node,T_amb,v_wind,ksD,hl_ffact,P_atm,grav,beta,H_rec,D_rec,m
real(8)::T_film,k_film,Conductivity,mu_film,Viscosity,rho_film,Density,c_p_film,specheat,Re_for,Nusselt_for,Nusselt_FC,h_for,nu_amb,Gr_nat,Nusselt_nat,h_nat,h_c

!Calculate Convective Heat Transfer Coefficient (From Type 222)
            !Fluid Props: FILM
        T_film      = (T_node + T_amb)/2.d0                             ![K] Film Temperature
        k_film      = Conductivity(1.d0,T_film,1.d0)                    ![W/m-K] The conductivity of the air
        mu_film     = Viscosity(1.d0,T_film, 1.d0)                      ![kg/m-s] Dynamic viscosity of the air
        rho_film    = Density(1.d0, T_film, P_atm)                      ![kg/m^3] Density of the air
        c_p_film    = specheat(1.d0,T_film,1.d0)*1000.d0                ![J/kg] Specific heat of the air
        
            !Forced Convection
        Re_for      = rho_film*v_wind*D_rec/mu_film                     ![-] Reynolds number
        Nusselt_for = Nusselt_FC(ksD,Re_for)                            ![-] S&K
        h_for       = Nusselt_for*k_film/D_rec*hl_ffact                 ![W/m^2-K] The forced convection heat transfer coefficient

            !Free convection: use AMBIENT Basis for fluid props                                       
        nu_amb      = Viscosity(1.d0,T_amb,1.d0)/Density(1.d0,T_amb,P_atm)          ![m^2/s] Kinimatic viscosity
        Gr_nat      = dmax1(0.d0, grav*beta*(T_node-T_amb)*H_rec**3/(nu_amb**2))    ![-] Grashof Number at ambient conditions   MJW 8.4.2010 :: Hard limit of 0 on Gr #
        Nusselt_nat = .098*Gr_nat**(1./3.)*(T_node/T_amb)**(-.14)                   ![-] Nusselt number
        h_nat       = Nusselt_nat*Conductivity(1.d0,T_amb,1.d0)/H_rec*hl_ffact      ![W/m^2-K] TN 3.29.11 ; The natural convection cofficient ; conductivity calculation corrected

        h_mixed     = (h_for**m+h_nat**m)**(1./m)*4.0                   ![W/m^2-K] MJW 7.30.2010:: (4.0) is a correction factor to match convection losses at Solar II (correspondance with G. Kolb, SNL)

end function

!#######################################################################################
!#######################################################################################

!**************************************************
!******** Conductivity Lookup **********
!**************************************************

!double precision function Conductivity(fnumd,T,P)
!double precision,intent(in):: T,P,fnumd
!real(8)::T_C
!integer::fnum
!fnum=nint(fnumd)
!
!select case(fnum)
!    case(1)   !    1.) Air
!    Conductivity = dmax1(0.00145453 + 0.0000872152*T - 2.20614E-08*T*T,1.e-4)
!    
!    case(28)        !   28.) T-91 Steel
!    T_C     = T - 273.15
!    Conductivity = -2E-5*T_C*T_C + 0.017*T_C + 25.535
!
!    case(2)         !   2.) Stainless_AISI316
!    Conductivity = 3E-09*T*T*T - 8E-06*T*T + 0.0177*T + 7.7765
!end select
!
!end function


!**************************************************
!******** Viscosity Lookup **********
!**************************************************

!double precision function Viscosity(fnumd,T,P)
!implicit none
!!This function accepts as inputs temperature [K] and pressure [Pa]
!!This function outputs in units of [Pa-s]
!double precision,intent(in)::T,P,fnumd
!integer::fnum
!viscosity=1.
!fnum=nint(fnumd)
!
!
!select case(fnum)
!case(1)   !    1.) Air
!Viscosity = dmax1(0.0000010765 + 7.15173E-08*T - 5.03525E-11*T*T + 2.02799E-14*T*T*T,1.e-6)
!end select
!
!end function

!**************************************************
!******** Density Lookup **********
!**************************************************

!double precision function Density(fnumd,T,P)
!
!implicit none
!!This function accepts as inputs temperature [K] and pressure [Pa]
!!This function outputs in units of [kg/m^3]
!double precision::T,P,fnumd
!integer::fnum
!!Density=1.
!fnum=nint(fnumd)
!
!select case(fnum)
!case(1)   !    1.) Air
!Density = P/(287.*T)
!end select
!
!end function

!**************************************************
!******** SpecHeat Lookup **********
!**************************************************
!double precision function specheat(fnumd,T,P)
!
!implicit none
!!This function accepts as inputs temperature [K] and pressure [Pa]
!!This function outputs in units of [kJ/kg-K]
!
!double precision,intent(in)::T,P,fnumd
!integer::fnum
!specheat=1.
!fnum=nint(fnumd)
!
!select case(fnum)
!case(1)   !    1.) Air
!specheat = 1.03749 - 0.000305497*T + 7.49335E-07*T*T - 3.39363E-10*T*T*T
!end select
!
!end function

!**************************************************
!******** Forced Convection Recipe ****************
!**************************************************
!double precision function Nusselt_FC(ksDin,Re)
!implicit none
!
!double precision,intent(in)::ksDin,Re
!double precision::Nomval,ValHi,ValLo,ValHi2,ValLo2, Nu_Hi,Nu_Lo,chi,ksD
!integer::rerun
!
!!This is the forced convection correlation presented in [Siebers & Kraabel, 1984]
!!The value of ks\D determines the interpolation that takes place between these 4 data points.
!ksD = ksDin
!Nomval = ksD
!rerun=0
!!Select the bounding conditions
!
!10	continue
!!Point 1: ksD=0.0
!if(ksD.lt.75e-5) then
!    Nusselt_FC=.3+.488*Re**.5*(1.+(Re/282000.)**.625)**.8
!    ValHi=75.e-5
!    ValLo=0.0
!else
!    !Point 2: ksD= 75 x 10**(-5)
!    if((ksD>=75.e-5).and.(ksD<300.e-5)) then
!        ValHi=300.e-5
!        ValLo=75.e-5
!        if(Re<=7e5) then
!            Nusselt_FC=.3+.488*Re**.5*(1.+(Re/282000.)**.625)**.8
!        else
!            if((Re>7.0e5).and.(Re<2.2e7)) then
!                Nusselt_FC=2.57e-3*Re**.98
!            else
!                Nusselt_FC=.0455*Re**.81
!            endif
!        endif
!    else
!        !Point 3: ksD= 300 x 10**(-5)
!        if((ksD>=300.0e-5).and.(ksD<900.0e-5)) then
!            ValHi=900.0e-5
!            ValLo=300.0e-5
!            if(Re<=1.8e5) then
!                Nusselt_FC=.3+.488*Re**.5*(1.+(Re/282000.)**.625)**.8
!            else
!                if((Re>1.8e5).and.(Re<4.0e6)) then
!                    Nusselt_FC=.0135*Re**.89
!                else
!                    Nusselt_FC=.0455*Re**.81
!                endif
!            endif
!        else
!            !Point 4: ksD = 900 x 10**(-5)
!            if(ksD>=900.0e-5) then
!                ValHi=900.0e-5
!                ValLo=900.0e-5
!                if(Re<=1e5) then
!                    Nusselt_FC=.3+.488*Re**.5*(1.+(Re/282000.0)**.625)**.8
!                else
!                    Nusselt_FC=.0455*Re**.81
!                endif
!            endif
!        endif
!    endif
!endif
!
!if (rerun.eq.1) then
!    goto 20
!else
!    rerun=1
!    Nu_Lo=Nusselt_FC
!    ksD=ValHi
!    ValLo2=ValLo
!    ValHi2=ValHi
!    goto 10
!endif
!
!20 continue
!Nu_hi=Nusselt_FC
!
!if (Nomval.ge.900.0e-5) then
!    chi=0
!else
!    chi=(Nomval-ValLo2)/(ValHi2-ValLo2)
!endif
!
!Nusselt_FC=Nu_Lo+(Nu_hi-Nu_Lo)*chi
!
!END FUNCTION

!#######################################################################################################################

!***************************************************************************************************

