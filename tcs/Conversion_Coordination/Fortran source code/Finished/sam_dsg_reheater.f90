SUBROUTINE Reheater(info,XIN,XOUT,N_panels,PAR,Q_inc,h_out_target,rh_exit,checkflux)

! Doc. tables updated 2011-10-12 - TN
!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Parameters
!    1| D_rec                            | Diameter of receiver                                              | m                | m                
!    2| Per_rec                          | Perimeter of receiver                                             | m                | m                
!    3| hl_ffact                         | Heat Loss Fudge FACTor                                            | none             | none             
!    4| H_rec                            | Height of reheater                                                | m                | m                
!    5| D_tube                           | O.D. of RH tubes                                                  | m                | m                
!    6| th_tube                          | Thickness of RH tubes                                             | m                | m                
!    7| eps                              | Emissivity of RH tubes                                            | none             | none             
!    8| absorp                           | Absorptivity of RH tubes                                          | none             | none             
!    9| Mat_tube                         | Numerical code for tube material                                  | none             | none             
!   10| T_out_target                     | Target outlet temperature                                         | C                | K                

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Inputs
!    1| T_amb                            | Ambient temperature                                               | K                | K                
!    2| T_sky                            | Sky temperature                                                   | K                | K                
!    3| v_wind                           | Wind speed                                                        | m/s              | m/s              
!    4| P_atm                            | Ambient pressure                                                  | Pa               | Pa               
!    5| T_in                             | reheater inlet temperature                                        | K                | K                
!    6| m_dot_in                         | Inlet steam mass flow rate to reheater                            | kg/s             | kg/s             
!    7| P_RH_out_min                     | Minimum allowable outlet pressure of reheater                     | Pa               | Pa               

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Outputs
!    1| eta_rec                          | Thermal efficiency of receiver                                    | none             | none             
!    2| T_out                            | Exit temperature of reheater                                      | K                | K                
!    3| P_out                            | Exit pressure of reheater                                         | Pa               | Pa               
!    4| Energy_out                       | Energy rate transferred to steam                                  | W                | W                
!    5| h_out_comb                       | Outlet enthalpy                                                   | J/kg             | J/kg             
!    6| h_in                             | Inlet enthalpy                                                    | J/kg             | J/kg             
!    7| T1_max                           | Max RH surface temperature                                        | K                | K                
!    8| Energy_in                        | Flux * receiverArea                                               | W                | W                
!    9| P_in - P_Path_out(1)             | Pressure drop through reheater                                    | Pa               | Pa               
!   10| P_path_out(1)                    | RH outlet pressure                                                | Pa               | Pa               
!   11| h_out_comb                       | Outlet enthalpy                                                   | J/kg             | kJ/kg            
!   12| rho_out                          | RH outlet density                                                 | kg/m^3           | kg/m^3           
!   13| Q_conv_RH                        | Total convective loss from reheater                               | MW               | MW               
!   14| Q_rad_RH                         | Total radiative loss from reheater                                | MW               | MW               
!   15| Q_abs_RH                         | Total energy rate absorbed by reheater                            | MW               | MW               
!   16| u_n_exit                         | Exit velocity of steam                                            | m/s              | m/s              


!USE FluidProperties
USE Water_properties

implicit none

!declare other variables here
real(8),intent(in)::XIN(8),PAR(11),h_out_target
integer*4,intent(in)::info(15),checkflux,N_panels
integer*4,intent(inout)::rh_exit
real(8):: XOUT(15)
real(8):: T_amb,H_rec,L_rec,W_rec,D_tube,th_tube,eps,absorp,Mat_tube,d_in,hl_ffact,m,L_eff_90,L_eff_45,sigma,grav,pi,&
            T_sky,v_wind,P_atm,T_in,P_in,T_out,&
            D_rec,Per_rec,Per_panel,RelRough,L_fr,L,A_t_cs,A_n_proj,A_n_in_act,KsD,&
            h_in,h_n_in,Beta,m_dot,T_1,h_c,h_mixed,q_wf,&
            h_n_out,h_n_ave,P_ave,rho_n_ave,T_n_ave,mu_n_ave,u_n,Re,f_fd,&
            c_n_ave,k_n_ave,alpha_n,Pr,Nusselt,h_fluid,T_2,k_n,Conductivity,R_n,diff_T_1,&
            DELTAP_TUBE,DELTAP_45,DELTAP_90,P_out,diff_P_ave,T_n_out,m_dot_total,&
            diff_P_path,x_n_ave,&
            mh_sum,h_out_comb,energy_in,energy_out,eta_rec,T1_max,P_n_in,dp,diff_T1_g,&
            P_lower,P_upper,diff_P_bracket,P_rh_out_min,diff_T_HT,h_rh_max,T_out_target,&
            T_1_upper,y_t_upper,T_1_lower,y_t_lower,Q_inc(N_panels),q_wf_min,T_1_max,T_n_in,&
            cp_x1,rho_x1,mu_x1,k_x1,&
            u_n_exit,rho_x2,u_n_base,Q_sum,m_dot_in,T_1_min,rho_out,q_rad_rh,q_conv_rh,q_abs_rh,P_out_avg
real(8),allocatable::h_path_out(:),P_path_out(:),m_dot_path(:),q_wf1(:),q_wf_total(:),Q_adj(:),q_conv(:),q_rad(:),q_abs(:)
integer*4,allocatable::Flow_Pat(:,:),Flow_Pat_Adj(:,:)
    
integer*4:: N_fr,Nodes,N_par,T_1_iter,P_ave_iter,diff_path_iter,T1_br_lower,T1_br_upper,P_br_low,Pave_flag 
integer*4:: i,j,dummy,up1,flowtype,CombNodes,N_comb,k
logical::Tupflag,Tlowflag,enth_flag,HT_flag,P_upguess,P_lowguess,lowpres_flag

!**************************************************
!********* Read Inputs and Parameters and Perform Initial Calculations **************
!**************************************************
!####################################
!$$$Parameters
!####################################
IF(info(7)==-1)THEN

    h_rh_max    = 4658519.d0

    dummy       = up1(0)
        !Superheater Dimensions & Properties
    D_rec       = PAR(up1(1))    ![m] "Diameter" of receiver for external convection correlation
    Per_rec     = PAR(up1(1))    ![m] Perimeter of receiver
    hl_ffact    = PAR(up1(1))    ![-] Heat Loss Fudge FACTor
    flowtype    = INT(PAR(up1(1))) ![-] Flow Pattern
    H_rec       = PAR(up1(1))    ![m] Height of superheater
    D_tube      = PAR(up1(1))    ![m] Outer diameter of superheater tubes
    th_tube     = PAR(up1(1))    ![m] Thickness of superheater tubes
    eps         = PAR(up1(1))    ![-] Emissivity of superheater tubes
    absorp      = PAR(up1(1))    ![-] Absorptivity of superheater tubes
    Mat_tube    = PAR(up1(1))    ![-] Superheater material (2: Stainless_AISI316, 28: T-91 Steel)
    T_out_target= PAR(up1(1))+273.15d0  ![K] Target outlet temperature
    
    !Absorptivity (and reflectivity) are accounted for in DELSOL efficiency files, so set to 1 here
    absorp      = 1.d0

        !Superheater Dimensions & Properties
!    D_rec       = (L_rec**2 + W_rec**2)**0.5    ![m] "Diameter" of boiler for external convection correlation
!    Per_rec     = 2.*L_rec + 2.*W_rec           ![m] Perimeter of boiler
!    H_rec       = 10.d0             ![m] Height of superheater
!    D_tube      = 0.0381            ![m] Outer diameter of superheater tubes
!    th_tube     = 0.003175          ![m] Thickness of superheater tubes
!    eps         = 1.d0              ![-] Emissivity of superheater tubes
!    absorp      = 0.95d0            ![-] Absorptivity of superheater tubes
!    Mat_tube    = 28.d0             ![-] Superheater material

    !Set as a constant in this subroutine
    N_fr        = 2                 ![-] Number of unique flow routes through superheater
    N_comb      = 1                 ![-] Number of panels 
!    flowtype    = 2                 ![-] Specifies flow path through receiver --- include as user defined or set constant?
!    flowtype    = 1

        !Heat Transfer & Pressure Drop 
    hl_ffact    = 1.d0          ![-] Heat Loss Fudge FACTor
    m           = 3.2           ![-] Exponential for calculating mixed convection
    L_eff_90    = 30.d0         ![-] Effective length for pressure drop for 90 degree bend
    L_eff_45    = 16.d0         ![-] Effective length for pressure drop for 45 degree bend

        !Constants
    sigma       = 5.67D-8       ![W/m2K4] stefan boltzmann constant
    grav        = 9.81          ![m/s^2] Gravitional constant
    pi          = 3.1415926                    

    !####################################
    !$$$End of Parameters
    !####################################

    !#####################################
    !$$$Initial Calculations: Once per simulation
    !#####################################

    !Check that panels divided by flow routes is a whole number
    !Maybe just specify that # flow routes is either '1' or '2'??
!    IF (mod(N_panels,N_fr) > 0)  STOP

!    if( (flowtype<=4) .and. (mod(N_panels,4)>0)) then
!        call messages(-1,'The number of panels for this flow configuration must be divisible by 4',"Fatal",0,222)
!    endif

        !Superheater Configuration

    Per_panel   = Per_rec/dble(N_panels)        ![m] "Perimeter" of individual panel
    Nodes       = N_panels/N_fr                 ![-] Nodes per flow path: INTEGER
    D_in        = D_tube - 2.*th_tube           ![m] Inner diameter of tube
    RelRough    = (1.5D-6)/d_in                 ![-] Relative roughness of the tubes: http.www.efunda/formulae/fluids/roughness.cfm
    N_par       = floor(Per_panel / d_tube)     ![-] Number of parallel assemblies per panel
    L_fr        = H_rec * Nodes                 ![m] Length of one path through boiler
    L           = H_rec                         ![-] Distance through one node (node = panel for this code)

    A_t_cs      = pi*d_in**2/4.d0               ![m^2] Cross-sectional area of tubing
    A_n_proj    = d_tube * L                    ![m^2] Projected Area ** Node **
    A_n_in_act  = pi * d_in * 0.5d0 * L         ![m^2] ACTIVE inside surface area - nodal
    ksD         = (D_tube/2.)/D_rec             ![-] The effective roughness of the cylinder [Siebers, Kraabel 1984]
    
    CombNodes = (Nodes/N_comb)
    
        !Allocate Arrays
    IF(.not.allocated(Flow_Pat))THEN
        allocate(Flow_Pat(N_fr,Nodes),h_path_out(N_fr),P_path_out(N_fr),m_dot_path(N_fr),q_wf1(CombNodes),q_wf_total(N_fr),Flow_Pat_adj(N_fr,(Nodes/N_comb)),Q_adj(N_fr*CombNodes),q_conv(N_fr*CombNodes),q_rad(N_fr*CombNodes),q_abs(N_fr*CombNodes))
    ENDIF
    
    CALL flowPatternsDSR(N_panels,flowtype,Flow_Pat)

    DO j=1,N_fr
        DO i=1,(CombNodes)
            Flow_Pat_adj(j,i) = i + (Nodes/N_comb)*(j-1)
        ENDDO
    ENDDO

    Return    
    
ENDIF

IF(info(8)==-1)THEN
    DEALLOCATE(Flow_Pat,h_path_out,P_path_out,m_dot_path,q_wf1,q_wf_total,Flow_Pat_adj,Q_adj,q_conv,q_rad,q_abs)
    
    RETURN
    
ENDIF


!######################################    
!$$$Inputs
!######################################

    !Ambient Conditions
T_amb   = XIN(1)            ![K] Ambient Temperature
T_sky   = XIN(2)            ![K] Sky Temperature
v_wind  = XIN(3)            ![m/s] Wind Speed
P_atm   = XIN(4)            ![Pa] Ambient Pressure

    !Superheater Inlet Conditions (from boiler)
T_in    = XIN(5)            ![K] Temperature entering superheater
m_dot_in = XIN(6)        ![kg/s] Total mass flow rate through system
P_in    = XIN(7)            ![kPa] Inlet pressure to reheater
P_rh_out_min = XIN(8)       ![Pa] Minimum allowable outlet pressure of reheater

!######################################    
!$$$End of Inputs
!######################################

XOUT    = 0.                !Set all outputs to 0 so it is simple to tell if they have been set

!#################################################################
!*********** Main Program **************************************
!#################################################################

!Mass flow rate in one tube - ASSUMING ONE FLOW PATH
m_dot_total     = m_dot_in / (dble(N_par)*dble(N_comb))     ![kg/s]

!*********  REFPROP **********************************************************************
!P(kPa),T(C),enth(kJ/kg),dens(kg/m3),inte(kJ/kg),entr(kJ/kg-K),cp(kJ/kg-K),cond(W/m-K),visc(kg/m-s)
CALL WATER_TP(T_in-273.15,P_in,enth=h_in)
P_in = P_in * 1000.     !Convert kPa to Pa
h_in = h_in * 1000.     !Convert kJ/kg to J/kg

!*********  REFPROP **********************************************************************
!P(kPa),T(C),enth(kJ/kg),dens(kg/m3),inte(kJ/kg),entr(kJ/kg-K),cp(kJ/kg-K),cond(W/m-K),visc(kg/m-s)
CALL Water_TQ(T_in-273.15,1.d0,dens=rho_x1)     
!*****************************************************************************************

!*********  REFPROP **********************************************************************
!P(kPa),T(C),enth(kJ/kg),dens(kg/m3),inte(kJ/kg),entr(kJ/kg-K),cp(kJ/kg-K),cond(W/m-K),visc(kg/m-s)
!CALL Water_TQ(T_in-273.15,1.d0,enth=h_in,pres=P_in,cp=cp_in,dens=rho_in,visc=mu_in,cond=k_in)
!cp_in = cp_in * 1000.       !Convert kJ/kg-K to J/kg-K
!h_in = 1000.*h_in           !Convert kJ/kg to J/kg
!*****************************************************************************************

u_n_base    = (m_dot_total/2.d0) / (rho_x1*A_t_cs)        ![m/s] Velocity through tube at inlet conditions
u_n_exit    = 0.d0

Energy_in   = sum(Q_inc(:)*Per_panel*H_rec)     ![W] Total energy incident on receiver: Used at end for efficiency calcs

!Create new flux arrays that allow for multiple panels in parallel
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
        
Beta        = 1.d0 / T_amb          ![1/K] Volumetric expansion coefficient 
h_n_in      = h_in                  ![J/kg-K] Inlet enthalpy
T_1         = T_in + 30.d0          ![K] Guess T_1
m_dot_path(:) = m_dot_total/dble(N_fr)      ![kg/s] Mass flow rate through each flow route (divide evenly for now)
diff_P_path = 999999.d0             !Set difference > tolerance
diff_path_iter = 0                  !Set iteration counter

!Check if enough flux is available to produce positive net energy through each flow path
DO j=1,N_fr         
    DO i=1,CombNodes                                                         !Choose the panel that we are modeling 
        h_c         = h_mixed(T_in,T_amb,v_wind,ksD,hl_ffact,P_atm,grav,beta,H_rec,D_rec,m)     ![W/m^2-K] Function calculates combined free and forced convection coefficient, same as used in fin HT model
        q_conv(Flow_Pat_Adj(j,i))      = h_c*A_n_proj*(T_in - T_amb)                                               ![W] Convective heat transfer                                                     
        q_rad(Flow_Pat_Adj(j,i))       = eps*sigma*A_n_proj*(0.5*(T_in**4 - T_sky**4) + 0.5*(T_in**4 - T_amb**4))    ![W] Radiative heat transfer: 8/10/11, view factor to ground ~ ambient (1/2)        
        q_abs(Flow_Pat_Adj(j,i))       = Q_adj(Flow_Pat_Adj(j,i))*A_n_proj*absorp                                              ![W] Irradiance absorbed by panel
        q_wf1(i)    = q_abs(Flow_Pat_Adj(j,i)) - q_conv(Flow_Pat_Adj(j,i)) - q_rad(Flow_Pat_Adj(j,i))                                                    ![W] Heat transfer to working fluid solved by energy balance
    ENDDO
    q_wf_total(j)   = sum(q_wf1)
    IF(q_wf_total(j)<0.d0)THEN
        rh_exit    = 2         !Set flag for calling code
        RETURN
    ENDIF
ENDDO

IF(checkflux == 1) RETURN

!In Writing: Required rate of energy addition [W] = mass flow rate [kg/s] * (enthalpy rise [J/kg])
!Equation: q_wf_min = m_dot_total*(h_out_target - h_in)
q_wf_min    = m_dot_total*(h_out_target - h_in) ![W]
    
!Also check that maximum total available energy is at least 0.8% (conservative) total needed to reach superheat target
!IF(sum(q_wf_total) < 0.8*q_wf_min)THEN
IF(sum(q_wf_total) < q_wf_min)THEN
    rh_exit   = 2             !Set flag for calling code
    RETURN
ENDIF

T1_max      = 0.                    ![K] Set T1_max    

!Solve for outlet conditions of each flow path
DO j=1,N_fr                             

    h_n_in      = h_in                  ![J/kg-K] Inlet enthalpy of first node
    T_n_in      = T_in                  ![K] Inlet temperature of first node
    P_n_in      = P_in                  ![Pa] Inlet pressure of first node
    dp          = 2.e4                  ![Pa] Guess small pressure drop
    diff_T_HT   = 45.d0                 ![K] Estimate of difference between surface temp and inlet temp
    m_dot      = m_dot_path(j)          !Use m_dot so we don't have to carry array through
    
    DO i=1,CombNodes            !Now, solve energy balance for each node (or panel) in flow path.  Output of i is input to i+1

        IF(i==1)THEN
            T_1        = T_n_in + 45.d0
        ELSE
            T_1        = T_n_in + 1.5*diff_T_HT
        ENDIF
                    
        diff_P_ave = 9999999.d0             !Set diff > tolerance
        P_ave_iter = 0                      !Set iteration counter
        P_ave      = max(1.e6, P_n_in - dp/2.)         ![Pa] Guess average pressure through flow path
        P_upguess  = .false.
        P_lowguess = .false.
        P_upper    = P_n_in
        !P_lower    = 1.e6
        P_lower    = P_rh_out_min       !8/29/11
        diff_P_bracket = 999.d0
        lowpres_flag = .false.
        P_br_low    = 0
        Pave_flag   = 0   
            !An average pressure of the node is used as one independent variable to calculate additional properties.
            !After pressure drop through node is calculated, want to make sure calculated average pressure is within some % of average used for props.
        DO WHILE((abs(diff_P_ave)>0.001d0).and.(P_ave_iter < 125))

348             P_ave_iter  = P_ave_iter + 1        !Increase iteration counter
            !IF(P_ave_iter == 125)  CONTINUE !Pause "Exceeds RH average pressure iteration"
            !IF(P_ave_iter == 125)  Pause "Exceeds RH average pressure iteration"

            IF(P_ave_iter > 1)THEN
                IF(.not.(lowpres_flag))THEN
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
                ELSEIF(lowpres_flag)THEN
                    P_br_low = 2
                    P_lower = P_ave
                    P_lowguess = .true.
                    P_ave   = 0.5*P_lower + 0.5*P_upper
                ENDIF
                diff_P_bracket  = (P_upper - P_lower)/P_lower
            ENDIF

            !If bracket from which to choose average pressure is small, then try to get out of loop
            IF(diff_P_bracket < 0.0005)THEN
            
                !Set lower pressure limit
                !P_br_low==1: Calculated average pressure is less than guessed
                !P_br_low==2: Calculated outlet pressure is less than specified minimum
                
                !Set upper pressure limit
                !P_br_up==1: Calculated average pressure is greater than guessed
             
                !If pressure flag, then decrease mass flow of problem flow path
!                IF(P_br_low==2)THEN
!                    IF(j==1) Pave_flag = 1
!                    IF(j==2) Pave_flag = 2
!                    GOTO 204
!                ENDIF

                IF(P_br_low==2)THEN
                    rh_exit = 1
                    RETURN
                ENDIF
                
                !Should be able to iterate out of this
                !IF(P_br_low==1)THEN
                                                    
            ENDIF

            lowpres_flag    = .false.           ![-] Reset flag marking an outlet pressure below allowed minimum

            diff_T_1    = 999.                  ![K] Set diff > tolerance
            T_1_iter    = 0                     ![-] Set iteration counter          
            
            T_1_max     = 1000.d0               ![K] Set some maximum allowable surface temperature to control iteration
            
            !Set limits on surface temperature iteration
            T_1_upper   = T_1_max               ![K]
            !T_1_lower   = T_n_in                ![K]
            T_1_lower   = T_n_in - 15.d0         ![K]        !8/29/11
            T_1_min     = T_1_lower             ![K]
            T_1         = min( max(T_1_lower, T_1), T_1_upper)
            
            diff_T1_g   = 999.                  ![K] Reset difference between upper and lower iteration limits

            !Reset iteration logic and integer flags
            Tupflag     = .false.               ![-]
            Tlowflag    = .false.               ![-]  
            HT_flag     = .false.               ![-]
            enth_flag   = .false.               ![-]
            T1_br_lower = 0                     ![-]
            T1_br_upper = 0                     ![-]
            !Reset temperature calculation errors at high and low guesses
            y_T_upper   = 0.d0                  ![K]
            y_T_lower   = 0.d0                  ![K]
            
            !Need properties at saturated vapor in case some guess in energy balance results in x<1
            !P(kPa),T(C),enth(kJ/kg),dens(kg/m3),inte(kJ/kg),entr(kJ/kg-K),cp(kJ/kg-K),cond(W/m-K),visc(kg/m-s)
            CALL Water_PQ(P_ave/1000.d0,1.d0,cp=cp_x1,dens=rho_x1,visc=mu_x1,cond=k_x1)
            cp_x1 = cp_x1 * 1000.       !Convert kJ/kg-K to J/kg-K 
            
            !This loop ensures that the outlet flow conditions for each panel are solved correctly by find the correct T1 (outer surface temp)
272         DO WHILE((abs(diff_T_1/T_1)>0.0025d0).AND.(T_1_iter < 50)) 
!272         DO WHILE((abs(diff_T_1)>1.d0).AND.(T_1_iter < 50)) 
                
                T_1_iter    = T_1_iter + 1              ![-] Add to iteration counter
                !IF(T_1_iter == 50) CONTINUE  !Pause "Exceeds RH Energy Balance Iterations"
                !IF(T_1_iter == 50) Pause "Exceeds RH Energy Balance Iterations"

                IF(T_1_iter > 1) THEN      !If first iteration, then use initial guess
                    
                    IF(((.not.(HT_flag)).and.(.not.(enth_flag)))) THEN
                        IF(abs(diff_T_1)>50.d0)THEN        !If error is very large, use bisection rather than false interpolation method
                            IF(diff_T_1>0.)THEN              !If old temp is higher than new temp, then old temp is too high, so:
                                T1_br_upper = 2
                                T_1_upper   = T_1           !Set upper limit
                                Tupflag     = .false.
                            ELSE
                                T1_br_lower = 2
                                T_1_lower   = T_1           !Set lower limit
                                Tlowflag    = .false.
                            ENDIF
                            T_1         = 0.5*T_1_lower + 0.5*T_1_upper
                            GOTO 474
                        ENDIF
                        IF((Tupflag).and.(Tlowflag))THEN    !If bracket results are set, use false position
                            IF(diff_T_1>0.)THEN              !If old temp is higher than new temp, then old temp is too high, so:
                                T1_br_upper = 2
                                T_1_upper   = T_1           !Set upper limit
                                y_T_upper   = diff_T_1      !Set upper bracket result
                            ELSE                            !If old temp is lower than new temp, then old temp is too low, so:
                                T1_br_lower = 2
                                T_1_lower   = T_1           !Set lower limit
                                y_T_lower   = diff_T_1      !Set lower bracket result
                            ENDIF
                            T_1 = (y_T_upper)/(y_T_upper-y_T_lower)*(T_1_lower - T_1_upper) + T_1_upper          !False position method
                        ELSE                                !If bracket results are not set:
                            IF(diff_T_1>0.)THEN              !If old temp is higher than new temp, then old temp is too high, so:
                                T1_br_upper = 2
                                T_1_upper   = T_1           !Set upper limit
                                y_T_upper   = diff_T_1      !Set upper bracket result
                                Tupflag     = .true.        !Set logic to show that upper bracket is set
                            ELSE
                                T1_br_lower = 2
                                T_1_lower   = T_1           !Set lower limit
                                y_T_lower   = diff_T_1      !Set lower bracket result
                                Tlowflag    = .true.        !Set logic to show that lower bracket is set
                            ENDIF
                            
                            IF((Tupflag).and.(Tlowflag))THEN 
                                T_1     = (y_T_upper)/(y_T_upper-y_T_lower)*(T_1_lower - T_1_upper) + T_1_upper          !False position method
                            ELSE
                                T_1     = T_1 - diff_T_1                                 ![K] Calculate new T_1 based on difference
                                !IF(T_1 < T_n_in) THEN      
                                IF(T_1 < T_1_lower) THEN        !8/29/11
                                    T_1         = 0.5*T_1_lower + 0.5*T_1_upper
                                ELSE
                                    !T_1         = max(T_n_in, min(T_1_max, T_1))
                                    T_1         = max(T_1_lower, min(T_1_max, T_1))     !8/29/11
                                ENDIF
                            ENDIF   
                        ENDIF                            
                    ELSEIF(enth_flag)THEN
                        T1_br_lower = 1
                        T_1_lower   = T_1
                        T_1         = 0.5*T_1_lower + 0.5*T_1_upper
                        enth_flag   = .false.
                    ELSEIF(HT_flag)THEN
                        T1_br_upper = 1
                        T_1_upper   = T_1
                        T_1         = 0.5*T_1_lower + 0.5*T_1_upper
                        HT_flag     = .false.
                    ENDIF                                          
                ENDIF
                  
474                 diff_T1_g   = (T_1_upper - T_1_lower)       !If iteration window is small, look for way out
                        
                !If the bracket from which to guess T_1 is small, then try to exit loop                
                IF(diff_T1_g < 0.01)THEN
                
                    IF( (T_1_lower > T_1_max - 0.02) .and. (T1_br_upper==0) ) T1_br_upper = 3
                    !IF( (T_1_upper < 0.02 + T_n_in) .and. (T1_br_lower==0) ) T1_br_lower = 3 
                    IF( (T_1_upper < 0.02 + T_1_min) .and. (T1_br_lower==0) ) T1_br_lower = 3     !8/29/11
                    
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
                    
                    !Need to increase mass flow rate                                  
                    IF( ((T1_br_lower==1).and.(T1_br_upper==2)) )THEN  
                        rh_exit = 3
                        RETURN

                    !Need to increase mass flow rate
                    ELSEIF( ((T1_br_lower==1).and.(T1_br_upper==3)) )THEN
                        rh_exit = 3
                        RETURN
                    
                    !Flux is too low: 10/16, no longer using HT_flag
                    !ELSEIF( ((T1_br_lower==2).and.(T1_br_upper==1)) )THEN
                    !    rh_exit = 2         !9/20/11 TN
                    !    RETURN                    
                                                                  
                    !This should not happen given enough iterations
                    !ELSEIF( ((T1_br_lower==2).and.(T1_br_upper==2)) )THEN
                    
                    !Need to increase mass flow rate
                    ELSEIF( ((T1_br_lower==2).and.(T1_br_upper==3)) )THEN
                        rh_exit = 3
                        RETURN  

                    !Flux is too low, no longer using HT_flag
                    !ELSEIF( ((T1_br_lower==3).and.(T1_br_upper==1)) )THEN
                    !    rh_exit = 2     !9/20/11 TN
                    !    RETURN
                    
                    !Flux is too low
                    ELSEIF( ((T1_br_lower==3).and.(T1_br_upper==2)) )THEN
                        !IF(T_1 >= T_out_target)THEN
                            rh_exit = 3
                        !ELSE                    
                        !    rh_exit = 2 
                        !ENDIF
                        RETURN
                        
                    ENDIF
                    
                    !Should not happen
                    !ELSEIF( ((T1_br_lower==3).and.(T1_br_upper==3)) )THEN
                    
                    !*********************************************************************************
                ENDIF                   
                                                     
                h_c         = h_mixed(T_1,T_amb,v_wind,ksD,hl_ffact,P_atm,grav,beta,H_rec,D_rec,m)      ![W/m^2-K] Function calculates combined free and forced convection coefficient, same as used in fin HT model
                q_conv(Flow_Pat_Adj(j,i))      = h_c*A_n_proj*(T_1 - T_amb)                                                ![W] Convective heat transfer                                                 
                q_rad(Flow_Pat_Adj(j,i))       = eps*sigma*A_n_proj*(0.5*(T_1**4 - T_sky**4) + 0.5*(T_1**4 - T_amb**4)) ![W] Radiative heat transfer: 8/10/11, view factor to ground ~ ambient (1/2)
                q_abs(Flow_Pat_Adj(j,i))       = Q_adj(Flow_Pat_Adj(j,i))*A_n_proj*absorp                                  ![W] Irradiance absorbed by panel
                q_wf        = q_abs(Flow_Pat_Adj(j,i)) - q_conv(Flow_Pat_Adj(j,i)) - q_rad(Flow_Pat_Adj(j,i))                                                    ![W] Heat transfer to working fluid solved by energy balance

                !If "not much" energy is available, flag and reguess T_1
!                IF(q_wf < 10.)THEN
!                    HT_flag = .true.
!                    GOTO 272
!                ENDIF

                !Equation: m_dot*(h_out-h_in) = q_wf
                h_n_out     = q_wf/m_dot + h_n_in           ![J/kg] Calculate outlet enthalpy according to calculated energy addition to working fluid
                h_n_ave     = (h_n_in + h_n_out)/2.d0       ![J/kg] Determine average enthalpy in node
                
                !If the outlet enthalpy is greater than property routine can handle, flag and reguess T_1
                IF(h_n_out > h_rh_max)THEN
                    enth_flag = .true.
                    GOTO 272
                ENDIF
                                 
                !*********  REFPROP **********************************************************************
                !P(kPa),T(C),enth(kJ/kg),dens(kg/m3),inte(kJ/kg),entr(kJ/kg-K),cp(kJ/kg-K),cond(W/m-K),visc(kg/m-s)
709               CALL Water_PH(P_ave/1000.,h_n_ave/1000.,dens=rho_n_ave,Temp=T_n_ave,visc=mu_n_ave,cond=k_n_ave,cp=c_n_ave,qual=x_n_ave)
                
                !Check for Dyreby props failing near vapor dome
                IF((c_n_ave < 0.d0).or.(k_n_ave < 0.d0))THEN
                    h_n_ave = 0.99 * h_n_ave
                    GOTO 709
                ENDIF
                
                T_n_ave     = T_n_ave + 273.15              !Convert C to K
                c_n_ave     = c_n_ave * 1000.               !Convert kJ/kg-K to J/kg-K
                !If quality is < 1 then REFPROP gives negative cp, so just set to value at x=1.
                IF(x_n_ave < 1.d0)THEN
                    c_n_ave = cp_x1
                    rho_n_ave = rho_x1
                    k_n_ave = k_x1
                    mu_n_ave = mu_x1
                ENDIF
                !*****************************************************************************************
                
                u_n         = m_dot / (rho_n_ave*A_t_cs)        ![m/s] Velocity through tube
                Re          = rho_n_ave * u_n * d_in / mu_n_ave ![-] Reynolds number
                f_fd        = (-2.0*log10(2.*RelRough/7.54 - 5.02*log10( 2.*RelRough/7.54 + 13./Re)/Re))**(-2)  ![-](Moody) friction factor (Zigrang and Sylvester)

                alpha_n     = k_n_ave/(rho_n_ave*c_n_ave)   ![m^2/s] Thermal diffusivity of fluid
                Pr          = mu_n_ave/(alpha_n*rho_n_ave)  ![-] Prandtl number      
                Nusselt     = ((f_fd/8.)*(Re-1000.)*Pr)/(1.+12.7*sqrt(f_fd/8.)*(Pr**(2./3.)-1.)) ![-] Turbulent Nusselt Number (Gnielinksi)
                !h_fluid     = max(0.1, Nusselt*k_n_ave/d_in)          ![W/m^2-K] Convective heat transfer coefficient
                !6/5/12, TN: Minimum heat transfer coefficient should be set to conductive heat transfer
                h_fluid     = max(k_n_ave/d_in, Nusselt*k_n_ave/d_in)

                !Equation: q_wf = h_fluid*Area*(T_2 - T_n_ave)
                T_2         = q_wf/(h_fluid*A_n_in_act) + T_n_ave               ![K] Temperature of inner tube surface
                k_n         = Conductivity(Mat_tube,((T_1+T_2)/2.d0),1.d0)      ![W/m-K] Conductivity of tube using average temperature
                R_n         = log(d_tube/d_in)/(k_n*2.d0*pi*L/2.d0)             ![K/W] Thermal resistance of ACTIVE tube
                diff_T_1    = T_1 - (T_2 + q_wf*R_n)                            ![K] Calculate difference between assumed T_1 and calculated T_1
                !diff_T_1    = (T_1 - (T_2 + q_wf*R_n))/T_1                      ![-] Calculate difference between assumed T_1 and calculated T_1
                
            ENDDO       !End Energy Balance Iteration
            
            !Include pressure drop/gain from density difference in closed system? Unlikely to be important............
            !DELTAP [Pa] =                DELTAP_tube                                     DELTAP_45                                   DELTAP_90
            DP      = (f_fd * L * rho_n_ave * u_n**2 / (2.d0 * d_in)) + 2.*(f_fd * L_eff_45 * rho_n_ave * u_n**2 / 2.d0) + 4.*(f_fd * L_eff_90 * rho_n_ave * u_n**2 / 2.d0)
            P_out   = P_n_in - DP                               ![Pa] Calculate Node Outlet Pressure
            diff_P_ave = (P_ave - (P_n_in + P_out)/2.d0)/P_in   ![Pa] Difference between ave. pressure guessed and ave. pressure calculated
            
            IF(P_out < P_rh_out_min)THEN
            !IF(P_out < 0.85*P_in)THEN
                lowpres_flag = .true.
                GOTO 348
            ENDIF

        ENDDO           !End Average Pressure Iteration for 1 Node of 1 Flow Path

        P_n_in  = P_out                 ![Pa] Calculate inlet pressure of next node
        h_n_in  = h_n_out               ![J/kg] Set inlet enthalpy for next node

        !Want T as a function of H,P
        !P(kPa),T(C),enth(kJ/kg),dens(kg/m3),inte(kJ/kg),entr(kJ/kg-K),cp(kJ/kg-K),cond(W/m-K),visc(kg/m-s)
        CALL Water_PH(P_out/1000.,h_n_in/1000.,Temp=T_n_in)      !calculate temperature corresponding to outlet enthalpy and pressure
        
        T_n_in  = T_n_in + 273.15       ![K]
        
        IF((T_n_in > (T_out_target+50.d0)).and.(i<CombNodes))THEN
            rh_exit = 3
            RETURN
        ENDIF
        
        diff_T_HT = max(0.d0, (T_1 - T_n_in))          ![K] Difference between surface temp and inlet temp: used to estimate T_1 of next node
        !11/15/11, TN: Change max temperature to estimate surface temp at hot end, not center (where energy balance is calculated)
        T1_max  = max(T1_max, T_n_in + (T_1 - T_n_ave))
        !T1_max  = max(T1_max, T_1)      ![K] Calculate highest tube temperature

    ENDDO               !End evaluation of 1 flow path

    h_path_out(j)   = h_n_out           ![J/kg] Keep track of enthalpy outlet of flow path
    P_path_out(j)   = P_out             ![Pa] Keep track of pressure outlet of flow path
    u_n_exit        = max(u_n_exit, u_n)![m/s] Maximum outlet velocity

ENDDO                   !End evaluation of both flow paths

diff_P_path = (P_path_out(1) - P_path_out(2))/P_path_out(1)         ![Pa] Relative difference between outlet pressures
!ENDDO                       !End iteration to ensure equal pressure drop in flow paths

!Energy balance to combine outlet flows (if more than 1): m1h1 + m2h2 = m3h3
mh_sum = 0.d0           ![W] Set initial sum to 0
DO j=1,N_fr
    mh_sum = mh_sum + m_dot_path(j)*h_path_out(j)       ![W] Total mh products for flow routes
ENDDO
h_out_comb  = mh_sum / m_dot_total                      ![J/kg] Calculate final combined outlet enthalpy
h_out_comb  = min(h_out_comb, h_rh_max)

P_out_avg   = min( (sum(P_path_out)/dble(N_fr)/1.e3), 19000.d0)            ![kPa] Average (flow paths) outlet pressure

!*********  REFPROP **********************************************************************
!P(kPa),T(C),enth(kJ/kg),dens(kg/m3),inte(kJ/kg),entr(kJ/kg-K),cp(kJ/kg-K),cond(W/m-K),visc(kg/m-s)
CALL Water_PH(P_out_avg,h_out_comb/1000.,Temp=T_out,dens=rho_out)      !calculate temperature corresponding to outlet enthalpy and pressure
T_out     = T_out + 273.15              !Convert C to K
!*****************************************************************************************

!!Total mass flow rate ALL tubes
!m_dot_in = m_dot_total*dble(N_par)*dble(N_comb)      ![kg/s]

Energy_out  = m_dot_in * (h_out_comb - h_in)         ![W]
eta_rec     = Energy_out / Energy_in                    ![-]
Q_rad_rh    = sum(q_rad)*dble(N_par)*dble(N_comb)/1.E6          ![MW] Total radiative losses from reheater
Q_conv_rh   = sum(q_conv)*dble(N_par)*dble(N_comb)/1.E6         ![MW] Total convective losses from reheater
Q_abs_rh    = sum(q_abs)*dble(N_par)*dble(N_comb)/1.E6          ![MW] Total energy rate absorbed by reheater

!Set Outputs
XOUT(1)     = eta_rec           ![-] Thermal efficiency of receiver
XOUT(2)     = T_out             ![K] Temperature of steam exiting reheater
XOUT(3)     = P_out             ![Pa] Pressure of steam exiting reheater
XOUT(4)     = Energy_out        ![W] Rate of energy transferred to steam in reheater
XOUT(5)     = h_out_comb/1000.d0  ![kJ/kg] Outlet enthalpy
XOUT(6)     = h_in              ![J/kg] Inlet enthalpy
XOUT(7)     = T1_max            ![K] Maximum average temperature of superheater surface
XOUT(8)     = Energy_in         ![W] Flux * receiverArea
XOUT(9)     = P_in - P_out_avg*1000.d0  ![Pa] Pressure drop through superheater
XOUT(10)    = P_out_avg*1000.d0 ![Pa] RH outlet pressure
XOUT(11)    = rho_out           ![kg/m^3] RH outlet density
XOUT(12)    = Q_conv_rh         ![MW] Total convective losses from reheater
XOUT(13)    = Q_rad_rh          ![MW] Total radiative losses from reheater
XOUT(14)    = Q_abs_rh          ![MW] Total energy rate absorbed by reheater
XOUT(15)    = u_n_exit          ![m/s] Exit velocity of steam through reheater

END SUBROUTINE