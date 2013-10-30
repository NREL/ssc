SUBROUTINE PackedBed(TIME,XIN,OUT,PAR,INFO)

!***************************************************************************
! ROCK BED THERMAL STORAGE ROUTINE  (adapted to SOLAR One)
! 
! LAST MODIFIED: 4/93 -- JWT  
!***************************************************************************
! GJK added some intermediate Temperature Nodes for Output on 4/25/2005 (Type 500)
! GJK added heat loss from top and bottom of tank 5/2005 (type 501)
! GJK added thermal mass factor to bottom of tank to simulate concrete foundation (type 502)
!***************************************************************************************
!
! updated to TRNSYS16 standard 11/06 - PS
! updated to .f90 format 4/22/11 - mjw
! Added inputs/outputs to determine useful volume and temperature limits
!*******************************************************************************************

! Updated November/11 -- TN
!*****************************************************************************
! -- Added the ability to use larger number (>33) of nodes.  This required implementing nodal temperature convergence equations inside of
!    this type, rather than relying on the TRNSYS solver to converge the DTDT & T arrays.  Number of nodes is now a parameter.
! -- Added the ability to use a shorter timestep for the thermocline than the simulation.  This allows the thermocline profile to remain accurate
!    while allowing the rest of the code to run at the longer timestep.  A subroutine that accepts the timestep as an input was added to diffeq.for.
! -- Removed option for multiple thermocline iteration.  Could reinstate in the future if necessary.  
! -- Converted from TRNSYS type to subroutine  

! Doc. tables updated 2011-04-26 - MJW
!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Parameters
!    1| HTF                              | HTF fluid number                                                  | none             | none             
!    2| h                                | Height of the rock bed storage tank                               | m                | m                
!    3| a                                | Cross-sectional area                                              | m2               | m2               
!    4| FILL                             | Filler material number                                            | none             | none             
!    5| u                                | loss coefficient                                                  | kJ/hr-m2-K       | kJ/hr-m2-K       
!    6| utop                             | top surface conductance                                           | kJ/hr-m2-K       | kJ/hr-m2-K       
!    7| ubot                             | bottom surface conductance                                        | kJ/hr-m2-K       | kJ/hr-m2-K       
!    8| void                             | rock bed void fraction                                            | none             | none             
!    9| capfac                           | Bottom thermal mass capacitance factor multiplier                 | none             | none             
!   10| Thmin                            | Minimum allowable hot side outlet temperature during discharge    | C                | C                
!   11| Tcmax                            | Maximum allowable cold side outlet temperature during charge      | C                | C                

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Inputs
!    1| thot                             | Temperature into the top of the tank                              | C                | C                
!    2| flowh                            | Flowrate into the top                                             | kg/hr            | kg/hr            
!    3| tcold                            | Temperature into the bottom of the tank                           | C                | C                
!    4| flowc                            | Flowrate into the bottom                                          | kg/hr            | kg/hr            
!    5| tenv                             | Environment temperature                                           | C                | C                

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Outputs
!    1| - no name -                      | Bottom node temperature                                           | C                | C                
!    2| flowh                            | Flowrate out of the bottom                                        | kg/hr            | kg/hr            
!    3| - no name -                      | Top node temperature                                              | C                | C                
!    4| flowc                            | Flowrate out of the top                                           | kg/hr            | kg/hr            
!    5| qenv                             | Environment losses                                                | kJ/hr            | kJ/hr            
!    6| qtank                            | Rate of energy supply                                             | kJ/hr            | kJ/hr            
!    7| delu                             | Internal energy change                                            | kJ               | kJ               
!    8| T_node3                          | Average temperature at node 3 (from top)                          | C                | C                
!    9| T_node7                          | Average temperature at node 7 (from top)                          | C                | C                
!   10| tavg                             | Average temperature                                               | C                | C                
!   11| m_disch_avail                    | Mass flow rate available for discharge                            | kg/hr            | kg/hr            
!   12| T_disch_avail                    | Average temperature of available discharge mass flow              | C                | C                
!   13| m_charge_avail                   | Mass flow rate available for charging                             | kg/hr            | kg/hr            
!   14| T_charge_avail                   | Average temperature of available cold return flow during charge   | C                | C                
   
! USE STATEMENTS
!use TrnsysFunctions
!use TrnsysConstants

IMPLICIT NONE !force explicit declaration of local variables

! TRNSYS DECLARATIONS
DOUBLE PRECISION XIN,OUT,TIME,PAR,TIME0,TFINAL,DELT
INTEGER*4 INFO(15),NP,NI,NO

! SET THE MAXIMUM NUMBER OF PARAMETERS(NP),INPUTS(NI),OUTPUTS(NOUT) THAT MAY BE SUPPLIED FOR THIS TYPE
PARAMETER (NP=23,NI=9,NO=12)

! REQUIRED TRNSYS DIMENSIONS
DIMENSION XIN(NI),OUT(NO),PAR(NP)

! LOCAL VARAIBLE DECLARATIONS
! CHARACTER (len=maxMessageLength) T10_Msg1
INTEGER NODES,J,K,IFLOW, ihlim, iclim, iter, Nodes_break, num_TC, tcn, num_TC_max, MODE, q_iter, TC_limit, ChargeNodes
INTEGER LU_PRINT, LU_FINAL, LU_DESC, div_print, topofhour
DOUBLE PRECISION XNODES,CPA,H,A,P,CPR,RHOR,U,COND,TI,VOL,UA,EFCOND,CAP,CAPN,&
                 THOT,FLOWH,TCOLD,FLOWC,TENV,FLH,FLC,AA,BB,TF,TBAR,TFBAR,TAVG,DELU,QENV,QTANK,UTOP,&
                 UBOT, RHOA, VOID, CAPFAC, UATOP, UABOT,&
                 !4.25.11 mjw added:
                 Thmin, Tcmax, m_disch_avail, T_disch_avail, m_charge_avail, T_charge_avail, fhlim, fclim,&
                 Tctemp, Thtemp, Tprop, specheat, density, conductivity, Qd_fill, Qc_fill, md_tc, md_add, &
                 mc_tc, mc_add, HTF, FILL, pos_print, dx_print, max_T_diff, T_cold0, T_hot0, TC_break, R, tophour,tau,&
                 TC_timestep,E_TES,m_dot_diff0, m_dot_diff, m_dot_prev,&
                 Q_dis_target,&
                 T_TC_max, m_dot_lower, m_dot_upper, m_dot, diff_q_target, q_tol, q_target, q_calc, q_cha_target,&
                 diff_Tcmax, diff_Thmin, t_tol, y_lower, y_upper, T_TC_min, m_dot_low0, m_dot_up0, Th_avail_min, Tc_avail_max,&
                 TFBAR_0, q_charge, q_stored, q_error, q_discharge, UA_HL, Q_loss_total, tol_q, tol_TC, &
                 T_hot_in_min, T_cold_in_max, EES_out, EES_start, EES_stop, T_htr_set, Q_htr_total, Q_htr_max, tank_max_heat, tank_pairs, f_storage
real(8),allocatable::StoreTransfer(:),StoreTransfer0(:),T_ini(:),Diff_T_ave(:),Q_losses(:),T_cout_ave(:),T_hout_ave(:),T_ts_ave(:),Q_htr(:)
real(8),parameter::pi=3.1415926
character::m1*20
logical:: lowmax, highmin, upflag, lowflag, full, know_mdot, mdot_iter, first_write
CHARACTER (len=50) :: PRINT_DESC
CHARACTER*1:: TAB=CHAR(9)						!TAB CHARACTER
! DATA STATEMENTS
!DATA YCHECK/'TE1','MF1','TE1','MF1','TE1'/
!DATA OCHECK/'TE1','MF1','TE1','MF1','PW1','PW1','EN1','TE1','TE1','TE1','MF1','TE1','MF1','TE1'/ 

!DATA T10_Msg1/'The rock bed thermal storage component only allows flow in one direction at a time. &
!               Positive flow into both the top and bottom of the bed were specified.'/    

!--------------------------------------------------------------------------------------------------
! PERFORM LAST CALL MANIPULATIONS 
IF (INFO(8)==-1) THEN

!    DO J=1,Nodes
!        div_print = max(1,jfix(dble(Nodes)/498.d0))
!        IF( (mod(J,div_print)==0.d0).or.(J==Nodes).or.(J==1) )THEN
!            pos_print = H - 0.5*dx_print - dx_print*(J-1)
!            K = J+Nodes
!            WRITE(LU_FINAL, '(E12.5,A1,E15.6,A1,E15.6)')  TIME, TAB, pos_print, TAB, StoreTransfer(2*Nodes+J)
!        ENDIF                 
!    ENDDO
!    close(LU_PRINT)
!    close(LU_DESC)
!    close(LU_FINAL)
    
    deallocate(StoreTransfer,T_ini,Diff_T_ave,Q_losses,T_cout_ave,T_hout_ave,T_ts_ave)


    RETURN 
ENDIF

! PERFORM POST CONVERGENCE MANIPULATIONS 
IF(INFO(13)>0) THEN

    IF(EES_out==1.d0)THEN

        !*************************************************************************
        !Write thermocline temperatures at the top of each hour to text file
        !Used for debugging purposes and not needed for implementation into TRNSYS
        topofhour = mod( (int(time*10.d0)),10 )
        tophour = mod( (time*10.d0),10.d0)
           
        IF((tophour==0).and.(time>=EES_start).and.(time<=EES_stop))THEN
        !IF((time==25).or.(time==2))THEN
            DO J=1,Nodes
                div_print = max(1,jfix(dble(Nodes)/498.d0))
                IF( (mod(J,div_print)==0.d0).or.(J==Nodes).or.(J==1) )THEN
                    pos_print = H - 0.5*dx_print - dx_print*(J-1)
                    K = J+Nodes
                    WRITE(LU_PRINT, '(E12.5,A1,E15.6,A1,E15.6)')  TIME, TAB, pos_print, TAB, StoreTransfer(Nodes+J)
                ENDIF                 
            ENDDO
        ENDIF
        !****************************************************************************

        IF(time==EES_stop)THEN
            DO J=1,Nodes
                div_print = max(1,jfix(dble(Nodes)/498.d0))
                IF( (mod(J,div_print)==0.d0).or.(J==Nodes).or.(J==1) )THEN
                    pos_print = H - 0.5*dx_print - dx_print*(J-1)
                    K = J+Nodes
                    WRITE(LU_FINAL, '(E12.5,A1,E15.6,A1,E15.6)')  TIME, TAB, pos_print, TAB, StoreTransfer(Nodes+J)
                ENDIF                 
            ENDDO
            close(LU_PRINT)
!            close(LU_DESC)
            close(LU_FINAL)     
            EES_out = 0.d0  !Done writing EES file   
        ENDIF
        
    ENDIF
      
!    IF(IFLOW==2)THEN        
!        m_dot = -m_dot  ![kg/hr] Designate a negative mass flow rate for discharging
!    ENDIF
!   
!    !write(21,'(F11.2)')   m_dot
   
    !***End of timestep, so set the temperatures at the beginning of the next timestep equal to the temps
    !*** at the end of this timestep
    StoreTransfer0(1:NODES) = StoreTransfer(NODES+1:2*NODES)
    TFBAR_0 = TFBAR

    RETURN 
ENDIF

! PERFORM FIRST CALL MANIPULATIONS
IF (INFO(7)==-1) THEN
    
    ! TRNSYS FUNCTIONS
    !TIME0   =getSimulationStartTime()
    !TFINAL  =getSimulationStopTime()
    !DELT    =getSimulationTimeStep() 
    
    TIME0   = PAR(22)   ![hr] Simulation start time
    DELT    = PAR(23)   ![hr] Simulation time step
    EES_out = PAR(19)   ![-] Print thermocline file for EES?
    EES_start = PAR(20) ![-] Time to start printing EES file
    EES_stop  = PAR(21) ![-] Time to stop printing EES file
     
    NODES       = PAR(12)           ! [-] Get number of nodes in thermocline.
    !TC_timestep = PAR(16)           ! [hr] Timestep at which to evaluate thermocline.
    num_TC_max  = 10000             ! [-] Maximum number of timesteps that can be evaluated during 1 call
         
    !Allocate arrays based on number of nodes and maximum number of timesteps
    !--The StoreTransfer array contains 3*Nodes values. The values refer to the following information:
        !   1:Nodes             ->  Temperature by node at the beginning of the SUBtime step
        !   Nodes+1:2*Nodes     ->  Temperature by node at the end of the SUBtime step
        !   2*Nodes+1:3*Nodes   ->  Average temperature by node over the SUBtime step
        !   --Nodes are ordered from tank top to tank bottom (1->Nodes : top->bottom)
        !   This array is used to store values for each subtimestep, so at the end of one TRNSYS timestep,
        !   the values will only represent what happened in the last subtime step.  Therefore, the temperatures at
        !   the end of the the subtimestep are useful, while the other values are not.
    !--The StoreTransfer0 array contains Nodes values, representing the temperature at each node
        !   at the start of the TRNSYS timestep.  This array is updated to the temperature at the end of the 
        !   last subtimestep when the TRNSYS timestep has converged
    !--The T_ini array contains Nodes values, representing the initial temperature profile
    !--The Diff_T_ave array contains Nodes values, representing the error at each node.  This array is reset each subtime step
    !--The T_cout_ave array contains num_TC_max values, representing the average temperature of the cold node at each subtime step
    !--The T_hout_ave array contains num_TC_max values, representing the average temperature of the hot node at each subtime step
    !--The T_ts_ave array contains num_TC_max values, representing the average packed bed temperature at each subtime step
        
    allocate(StoreTransfer(3*NODES),StoreTransfer0(NODES),T_ini(NODES),Diff_T_ave(NODES),Q_losses(num_TC_max),T_cout_ave(num_TC_max),T_hout_ave(num_TC_max),T_ts_ave(num_TC_max),Q_htr(num_TC_max))
    
    IF(EES_out == 1.d0)THEN
    !******** Set-up output files for code development and debugging: comment out for "released" code **********
        !Write text file with headers that EES can read
!        IF(NODES < 100)THEN
!            write(unit=m1,fmt="(I2)") NODES
!        ELSEIF(NODES < 1000)THEN
!            write(unit=m1,fmt="(I3)") NODES
!        ELSEIF(NODES < 10000)THEN
!            write(unit=m1,fmt="(I4)") NODES
!        ELSE
!            write(unit=m1,fmt="(I5)") NODES
!        ENDIF
        !PRINT_DESC = " Time"//trim(m1)//""
        
        LU_PRINT = 16   ! Output file for temperature profile
!        LU_DESC = 14    ! Output file describing the number of nodes in simulation and the name of the output file containing the final temperature profile
        LU_FINAL = 12   ! Output file containing the final temperature profile
        !open(LU_PRINT,FILE="C:\Documents and Settings\tneises\My Documents\NREL\Thermal Storage\120301 Stand Alone TC\Modified\Type504.txt")      
        open(LU_PRINT,FILE="Profile_all.txt")      
        !open(LU_DESC,FILE="C:\Documents and Settings\tneises\My Documents\NREL\Thermal Storage\120301 Stand Alone TC\Modified\Description.txt")
!        open(LU_DESC,FILE="Description.txt")
!        write(LU_DESC, '(A24)') trim(m1)//"_Nodes "//trim(m1)//"Nodes.txt"     
        !open(LU_FINAL,FILE="C:\Documents and Settings\tneises\My Documents\NREL\Thermal Storage\120301 Stand Alone TC\Modified\"//trim(m1)//"Nodes.txt")      
        open(LU_FINAL,FILE="Profile_last.txt")      
        write(LU_PRINT, '(I2,A1,I2)') -2, TAB, -3
        write(LU_PRINT, '(A2,A1,A4,A1,A4)') "F1", " ", "Time", " ", "[hr]"
        write(LU_PRINT, '(A2,A1,A3,A1,A3)') "F1", " ", "Pos", " ", "[m]"
        write(LU_PRINT, '(A2,A1,A4,A1,A3)') "F1", " ", "Temp", " ", "[C]"
        write(LU_FINAL, '(I2,A1,I2)') -2, TAB, -3
        write(LU_FINAL, '(A2,A1,A4,A1,A4)') "F1", " ", "Time", " ", "[hr]"
        write(LU_FINAL, '(A2,A1,A3,A1,A3)') "F1", " ", "Pos", " ", "[m]"
        write(LU_FINAL, '(A2,A1,A4,A1,A3)') "F1", " ", "Temp", " ", "[C]"
        !open(19,FILE="C:\Documents and Settings\tneises\My Documents\NREL\Thermal Storage\Type 251 Integration\mdotiter.txt")
        !open(21,FILE="C:\Documents and Settings\tneises\My Documents\NREL\Thermal Storage\Type 251 Integration\average_temp.txt")
    !****************************************************************************************************
    
        first_write = .true.    ! Flag the signals whether initial temperature profile has been written to output file
    ENDIF
    
    RETURN 
ENDIF
	
! PERFORM INITIAL TIMESTEP MANIPULATIONS
IF (TIME<(TIME0+DELT/2.)) THEN
    
    XNODES = DBLE(NODES)        ![-] Convert NODES from integer to real 
    HTF  = PAR(1)               ![-] HTF fluid number
    H    = PAR(2)               ![m] Height of the rock bed storage tank  
    A    = PAR(3)               ![m^2] Cross-sectional area of storage tank
    FILL = PAR(4)               ![-] Filler material number
    U    = PAR(5)               ![kJ/hr-m2-K] loss coefficient   
    ! gjk added 4 new parameters
    UTOP = PAR(6)               ![kJ/hr-m2-K] top surface conductance
    UBOT = PAR(7)               ![kJ/hr-m2-K] bottom surface conductance
    VOID = PAR(8)               ![-] rock bed void fraction
    ! GJK capfac to add thermal mass to bottom of tank
    CAPFAC = PAR(9)             ![-] Bottom thermal mass capacitance factor multiplier
    ! mjw add temperature control parameters
    Thmin = PAR(10)             ![C] Minimum allowable hot side outlet temperature during discharge
    Th_avail_min = Thmin + 5.d0 ![C] Minimum allowable hot side outlet temperature used for availability calcs
    Tcmax = PAR(11)             ![C] Maximum allowable cold side outlet temperature during charge
    Tc_avail_max = Tcmax - 5.d0 ![C] Maximum allowable cold side outlet temperature used for availability calcs
    !Nodes = PAR(12)            ![-] Don't actually reset nodes, just keep as a placeholder to keep track of parameters
    T_hot0 = PAR(13)            ![C] Initial thermocline hot temperature
    T_cold0 = PAR(14)           ![C] Initial thermocline cold temperature
    !TC_break = max(0.d0,min(1.d0,PAR(15)))          ![-] Fraction into tank where thermocline exists (0: entire tank is hot, 1: entire tank is cold)
    TC_break = PAR(15)          ![-] Fraction into tank where thermocline exists (0: entire tank is hot, 1: entire tank is cold)
    T_htr_set = PAR(16)         ![C] Minimum allowable cold tank fluid temperature before auxiliary heater turns on
    tank_max_heat = PAR(17)     ![MW] Capacity of tank heater
    tank_pairs = PAR(18)        ![-] Number of equivalent tank pairs    
    Nodes_break = max(1,min(Nodes-1,int((1.d0 - TC_break)*Nodes)))  ![-] Last hot node
    
!    T_hot_in_min = T_hot0 - 20.d0   ![C] Minimum allowable inlet charging temperature
!    T_cold_in_max = T_cold0 + 20.d0 ![C] Maximum allowable inlet discharging temperature
    T_hot_in_min = 0.9*T_hot0 + 0.1*T_cold0     ![C] Minimum allowable inlet charging temperature
    T_cold_in_max = 0.1*T_hot0 + 0.9*T_cold0    ![C] Maximum allowable inlet discharging temperature
    
    !Define initial thermocline array based on initial hot and cold temperatures and cold fraction
    IF(TC_break < 0.d0)THEN
        T_ini(1:Nodes) = T_hot0
    ELSEIF(TC_break > 1.d0)THEN
        T_ini(1:Nodes) = T_cold0
    ELSE
        T_ini(1:Nodes_break) = T_hot0
        T_ini(Nodes_break+1:Nodes) = T_cold0
    ENDIF
    
    dx_print = H / Nodes        ![m] distance between nodes
    
    !initialize "previous timestep" temperature array
    StoreTransfer0(:) = T_ini(1:NODES)
    !********************************************************************    

    IF(EES_out==1.d0)THEN
        !*************************************************************************
        !Write thermocline temperatures at the top of each hour to text file
        !Used for debugging purposes and not needed for implementation into TRNSYS
        tophour = mod( (time*10.d0),10.d0)
   
        IF((first_write).and.(tophour==0).and.(time>=EES_start).and.(time<=EES_stop))THEN
        !IF((time==25).or.(time==2))THEN
            DO J=1,Nodes
                div_print = max(1,jfix(dble(Nodes)/498.d0))
                IF( (mod(J,div_print)==0.d0).or.(J==Nodes).or.(J==1) )THEN
                    pos_print = H - 0.5*dx_print - dx_print*(J-1)
                    K = J+Nodes
                    WRITE(LU_PRINT, '(E12.5,A1,E15.6,A1,E15.6)')  TIME, TAB, pos_print, TAB, StoreTransfer0(J)
                ENDIF                 
            ENDDO
            first_write = .false.   ! Initial temperature profile has been written, so set flag to "false"
        ENDIF   
       !****************************************************************************
    ENDIF

    !3/1/12, TN: These outputs don't represent anything at this point.  Set to constants?
    OUT(1)  = m_disch_avail
    OUT(2)  = T_disch_avail
    OUT(3)  = m_charge_avail
    OUT(4)  = T_charge_avail

    ! DETERMINE AVERAGE ROCKBED TEMPERATURES AND AVERAGE DERIVATIVES
    TFBAR_0 = 0.d0
    DO K = 1,NODES
        TFBAR_0 = TFBAR_0 + StoreTransfer0(K)  
    ENDDO
    TFBAR_0 = TFBAR_0/DBLE(NODES)       ![C] Spatial average of temps at end of final timestep

    RETURN         !the first timestep is for initialization - exit.
ENDIF

!mjw 4.25.11 Calculate the effective conductance of the thermocline and other fluid properties
!Tprop = StoreTransfer(int(Nodes*1.5))   ![C]
Tprop = 0.5*(T_hot0 + T_cold0)          ![C]
cond = cond_eff(FILL, HTF, VOID, Tprop) ![W/m-K]
CPA = specheat(HTF,Tprop+273.15d0,1.d0) ![kJ/kg-K]
RHOA = density(HTF,Tprop+273.15d0,1.d0) ![kg/m^3]
CPR = Cp_bed(FILL)                      ![kJ/kg-K]
RHOR = Dens_bed(FILL)                   ![kg/m^3]

!Reset all guess (beginning, middle, I guess end doesn't matter, to end of previous timestep)
StoreTransfer(1:Nodes) = StoreTransfer0(:)
StoreTransfer(Nodes+1:2*Nodes) = StoreTransfer0(:)
StoreTransfer(2*Nodes+1:3*Nodes) = StoreTransfer0(:)

! CALCULATE PARAMETER DEPENDENT VALUES
!mjw 4.26.11 Assume a cylindrical tank and calculate perimeter
P = sqrt(A/pi)*2.*pi        ![m] Perimeter of cylinderical tank
VOL = A*H                   ![m^3] Volume of tank
UA = U*P*H/XNODES           ![kJ/hr-K]->[kJ/hr-m2-K]*surface area of node[m^2] (P*H/XNODES)
! gjk add UATOP AND UABOT
UATOP = UTOP*A              ![kJ/hr-K]->[kJ/hr-m2-K]*top surface area[m^2] (A)
UABOT = UBOT*A              ![kJ/hr-K]->[kJ/hr-m2-K]*bottom surface area[m^2] (A)

!11/10/11, TN: Let's calculate some useful system parameters
!R = VOL*VOID*CPA*RHOA / (VOL*(1.-VOID)*CPR*RHOR)    !indication of significance of thermal cap of fluid

EFCOND = COND*A*XNODES/H                            ![W/K]
!CAP = VOL*CPR*RHOR
! gjk add oil to thermal capacity of a node
CAP = VOL*VOID*CPA*RHOA + VOL*(1.-VOID)*CPR*RHOR    ![kJ/K] Tank
E_TES = CAP*(T_hot0 - T_cold0)                      ![kJ] Available energy in completely hot tank
CAPN = CAP/XNODES                                   ![kJ/K] Node

!Need to calculate the tolerance of TC solver by ensuring that during dwelling, energy is conserved:
tol_q = 0.01        ![-]
tol_TC = 0.5*0.5*tol_q*UA / (cond*A/(H/XNODES))
tol_TC = max(1.d-10, min(0.001, tol_TC))

! GET THE CURRENT VALUES OF THE INPUTS
THOT = XIN(1)       ![C] Temperature into the top of the tank (charging)     
FLOWH = XIN(2)/tank_pairs      ![kg/hr] Flowrate into the top (charging)
TCOLD = XIN(3)      ![C] Temperature into the bottom of the tank (discharging)
FLOWC = XIN(4)/tank_pairs      ![kg/hr] Flowrate into the bottom (discharging)
TENV = XIN(5)       ![C] Environmental temperature
MODE = XIN(6)       ![-] Flag for whether call is to check availability
    !Mode = 2: Only check for availability, don't solve thermolcine model
Q_dis_target = XIN(7)/tank_pairs   ![W] Discharge rate required by controller 
Q_cha_target = XIN(8)/tank_pairs   ![W] Charge rate required by controller
f_storage = XIN(9)  ![-] Storage dispatch fraction for timestep
!**************************************************************

!If discharge thermal power rate is specified then
IF(Q_dis_target > 0.d0)THEN
    IFLOW = 2               ![-] Set flow direction to discharge
    know_mdot = .false.     ![-] Need to solve for mass flow
    q_target = Q_dis_target ![W] Target thermal power is discharge target
!If charge thermal power rate is specified then
ELSEIF(Q_cha_target > 0.d0)THEN
    IFLOW = 1               ![-] Set flow direction to charge
    know_mdot = .false.     ![-] Need to solve for mass flow
    q_target = Q_cha_target ![W] Target thermal power is charge target
!If discharge flow rate is specified then
ELSEIF(FLOWC > 0.d0)THEN 
    IFLOW = 2               ![-] Set flow direction to discharge
    know_mdot = .true.      ![-] Know mass flow rate
    m_dot = FLOWC           ![kg/s] Set mass flow rate
!If charge flow rate is specified then
ELSEIF(FLOWH > 0.d0)THEN 
    IFLOW = 1               ![-] Set flow direction to charge
    know_mdot = .true.      ![-] Know mass flow rate
    m_dot = FLOWH           ![kg/s] Set mass flow rate
!"idle" mode    
ELSE                    
    IFLOW = 1               ![-] Set flow direction to charge, although it doesn't matter
    know_mdot = .false.     ![-]
    m_dot = FLOWH           ![kg/s] Set mass flow rate: should be 0
    q_target = 0.d0         ![W] Target thermal power is 0
ENDIF

!***********************************************************************************************************
!mjw 4.25.11 Calulcate the available volumes for charge/discharge given the control temperature limitations
!Updated 11/28/11, TN
!   - The availability check should be completed before the performance simulation, so use StoreTransfer(1:Nodes)
!***********************************************************************************************************
!--The StoreTransfer array contains 3*Nodes values. The values refer to the following information:
!   1:Nodes             ->  Temperature by node at the beginning of the time step
!   Nodes+1:2*Nodes     ->  Temperature by node at the end of the time step
!   2*Nodes+1:3*Nodes   ->  Average temperature by node over the time step
!--Nodes are ordered from tank top to tank bottom (1->Nodes : top->bottom)
iclim=0; ihlim=0; Thtemp=0.d0; Tctemp=0.d0
do k=1,Nodes
    !find the last node where the HTF temperature is above the hot side limit
    if(StoreTransfer(k) > Th_avail_min) then
        ihlim=k         ![-]
        Thtemp = Thtemp + StoreTransfer(k)    ![C]
    endif
    !find the last node where the HTF temperature is below the cold side limit
    if(StoreTransfer(Nodes+1-k) < Tc_avail_max) then
        iclim=k         ![-]
        Tctemp = Tctemp + StoreTransfer(Nodes+1-k)    ![C]
    endif
enddo

fhlim = dble(ihlim)
fclim = dble(iclim)

!Average temperature of TC above limit (assumes uniform props)
Thtemp = Thtemp/dmax1(fhlim,1.d0)   ![C]
!Average temperature of TC below limit
Tctemp = Tctemp/dmax1(fclim,1.d0)   ![C]

!Calculate the fraction of HTF that the hot filler can bring up to temperature
!fhlim = fhlim/XNodes                ![-]
fclim = fclim/XNodes                ![-]
!4/23/12, TN: Account for storage dispatch fraction
fhlim = fhlim - f_storage
ChargeNodes = max(1,jfix(f_storage*Nodes))

!If cold inlet temperature is greater than the cold maximum, then no thermal energy is available for discharge
!IF( TCOLD > Tc_avail_max )THEN  
IF ( TCOLD > T_cold_in_max)THEN
    Qd_fill = 0.d0
    FLOWC = 0.d0
    FLC = CPA*FLOWC     ![kJ/hr-K]
!Otherwise, estimate the maximum thermal energy available for discharge (equation is explained in powerpoint)
ELSE    
    Qd_fill = max(0.d0, vol * fhlim * rhor * cpr * (1.-void) * dmax1(Thtemp - TCOLD, 0.d0) ) + max(0.d0, vol*fhlim*void*rhoa*cpa*dmax1(Thtemp - TCOLD, 0.d0))   ![kJ]
ENDIF

!IF hot inlet temperature is less than hot minimum, then no thermal energy is available for charging
!IF( THOT < Th_avail_min )THEN
IF( THOT < T_hot_in_min )THEN
    Qc_fill = 0.d0
    FLOWH = 0.d0
    FLH = CPA*FLOWH     ![kJ/hr-K]
!Otherwise, estimate the maximum thermal energy available for charge (equation is explained in powerpoint)
ELSE
    Qc_fill = max(0.d0, vol * fclim * rhor * cpr * (1.-void) * dmax1(THOT - Tctemp, 0.d0)) + max(0.d0, vol*fclim*void*rhoa*cpa*dmax1(THOT - Tctemp, 0.d0)) ![kJ]
ENDIF

!******Discharging****************************************************************
!Convert maximum discharge energy to thermal power over timestep
Qd_fill = Qd_fill/(3.6d0*delt)      ![kJ]*[1/hr]*1000[J/kJ]*(1/3600)[hr/s]=>[W] 
!Approximate mass flow available over hour
m_disch_avail = Qd_fill / (cpa*dmax1(Thtemp - TCOLD, 1.d0))*3.6d0       ![J/s]*[kg-K/kJ]*[1/K]*(1/1000)[J/kJ]*3600[s/hr]=>[kg/hr]
!******************************************************************************

!******Charging****************************************************************
Qc_fill = Qc_fill/(3.6d0*delt)      ![kJ]*[1/hr]*1000[J/kJ]*(1/3600)[hr/s]=>[W]
m_charge_avail = Qc_fill / (cpa*dmax1(THOT - Tctemp, 1.d0))*3.6d0       ![J/s]*[kg-K/kJ]*[1/K]*(1/1000)[J/kJ]*3600[s/hr]=>[kg/hr]
!******************************************************************************

!If call to subroutine is only interested in availability, then set outputs and exit
IF(MODE == 2.d0)THEN
    OUT(1)  = m_disch_avail*tank_pairs     ![kg/hr]
    OUT(2)  = StoreTransfer(1)  ![C] -- 3/1/12: Better to use Thtemp here instead?    
    OUT(3)  = m_charge_avail*tank_pairs    ![kg/hr]
    OUT(4)  = StoreTransfer(Nodes)  ![C] -- 3/1/12: Better to use Tctemp here instead?
    RETURN
ENDIF

!If mass flow rate is known, there is still the possibility of that it will over-(discharge) the thermocline,
!     so we need to establish a range of possible mass flow rates for the solver
IF(know_mdot)THEN
    m_dot_lower = 0.d0    
    m_dot_upper = m_dot

!If the charging energy is known, then estimate the mass flow rate required
ELSEIF(IFLOW==2)THEN    !Charging
!    T_TC_max = maxval(StoreTransfer(1:Nodes))       ![C] Maximum temperature in packed bed
!!    m_dot_lower = Q_dis_target / (specheat(HTF,(TCOLD + T_TC_max)/2.d0) * (T_TC_max - TCOLD))*3.6d0     ![kg/hr]: lowest possible mass flow rate to achieve desired power
!!    m_dot_upper = max(m_disch_avail, Q_dis_target*1000.d0 / (specheat(HTF,(TCOLD + Thmin)/2.d0) * (Thmin - TCOLD))*3.6d0 )  ![kg/hr]:highest possible mass flow rate to achieve desired power
!
!    !Using CPA in all the following equations, may as well use it here
!    m_dot_lower = min(Q_dis_target, Qd_fill) / (CPA * (T_TC_max - TCOLD))*3.6d0     ![kg/hr]: lowest possible mass flow rate to achieve desired power
!    m_dot_upper = max(m_disch_avail, min(Q_dis_target, Qd_fill) / (CPA * (Thmin - TCOLD))*3.6d0 )  ![kg/hr]:highest possible mass flow rate to achieve desired power
!
!    IF(m_dot_lower > m_disch_avail)THEN
!        m_dot   = m_disch_avail             ![kg/hr]
!        m_dot_lower = 0.5*m_disch_avail     ![kg/hr]
!    ELSE
!        m_dot   = 0.85*m_dot_lower + 0.15*m_disch_avail     ![kg/hr]
!    ENDIF

! The approximations above are not adequate because it is too difficult to estimate the amount of charging energy before the thermocline is run.  Were experiencing
! cases where actual mass flow rate was outside of bounds.  Therefore, it saves computational time to give a larger range of mass flow rates that improves the chances of  including 
! the actual mass flow rate
    m_dot = min(Q_dis_target, Qd_fill) / (cpa*dmax1(Thtemp - TCOLD, 1.d0))*3.6d0       ![kg/hr]

!If the discharge energy is known, then estimate the mass flow rate required
ELSEIF(IFLOW==1)THEN    !Charging
!    T_TC_min = minval(StoreTransfer(1:Nodes))       ![C] Minimum temperature in packed bed
!!    m_dot_lower = Q_cha_target / (specheat(HTF,(THOT + T_TC_min)/2.d0) * (THOT - T_TC_min))*3.6d0     ![kg/hr]: lowest possible mass flow rate to achieve desired power
!!    m_dot_upper = max(m_disch_avail, Q_cha_target*1000.d0 / (specheat(HTF,(THOT + Tcmax)/2.d0) * (THOT - Tcmax))*3.6d0 )  ![kg/hr]:highest possible mass flow rate to achieve desired power
!
!    !Using CPA in all the following equations, may as well use it here
!    m_dot_lower = min(Q_cha_target, Qc_fill) / (CPA * (THOT - T_TC_min))*3.6d0     ![kg/hr]: lowest possible mass flow rate to achieve desired power
!    m_dot_upper = max(m_disch_avail, min(Q_cha_target, Qc_fill) / (CPA * (THOT - Tcmax))*3.6d0 )  ![kg/hr]:highest possible mass flow rate to achieve desired power
!
!    IF(m_dot_lower > m_charge_avail)THEN
!        m_dot   = m_charge_avail             ![kg/hr]
!        m_dot_lower = 0.5*m_charge_avail     ![kg/hr]
!    ELSE
!        m_dot   = 0.85*m_dot_lower + 0.15*m_charge_avail     ![kg/hr]
!    ENDIF
    
    m_dot = min(Q_cha_target, Qc_fill) / (cpa*dmax1(THOT - Tctemp, 1.d0))*3.6d0       ![kg/hr]

ENDIF

!Set bounds a bit past realistic bounds so if the correct mass flow rate is at the bounds, iteration gets there faster
m_dot_upper = 1.5*m_dot
m_dot_lower = 0.5*m_dot

!Keep track of initial upper and lower mass flow rate estimates
m_dot_low0 = m_dot_lower
m_dot_up0 = m_dot_upper

diff_q_target = 999.d0  ![-] Set difference greater than tolerance
q_iter = 0              ![-] Iteration counter on achieving target (dis)charge energy
TC_limit = 0            ![-] Flag signaling whether current mass flow rate has over-(dis)charged thermocline
upflag = .false.        ![-] Flag signaling that upper limit on mass flow has been found with a corresponding 'diff_q_target'
lowflag = .false.       ![-] Flag signaling that lower limit on mass flow has been found with a corresponding 'diff_q_target'
mdot_iter = .false.     ![-] Flag signaling that allow a mass flow rate was specified, it must be iterated on (when = true)

q_tol = 0.01        ![-] Relative tolerance for energy rate in/out
t_tol = 3.d0        ![C] Absolute tolerance for packed bed filling/depletion limits

DO WHILE( ((abs(diff_q_target)>q_tol).or.(TC_limit/=0)) .and. (q_iter < 40) )

    q_iter = q_iter + 1 ![-] Increase iteration counter
    full = .true.       ![-] Reset target energy flag
    
    !After first run, begin to iterate on mass flow rate
    IF(q_iter > 1)THEN
        IF(TC_limit==1)THEN
            m_dot_upper = m_dot
            upflag = .false.
            m_dot = 0.5*m_dot_upper + 0.5*m_dot_lower 
        ELSEIF(TC_limit==2)THEN
            m_dot_lower = m_dot
            lowflag = .false.
            m_dot = 0.5*m_dot_upper + 0.5*m_dot_lower          
        ELSEIF((upflag).and.(lowflag))THEN
            IF(diff_q_target > q_tol)THEN
                m_dot_upper = m_dot
                y_upper = diff_q_target
            ELSEIF(diff_q_target < q_tol)THEN
                m_dot_lower = m_dot
                y_lower = diff_q_target
            ENDIF   
            m_dot = (y_upper)/(y_upper - y_lower)*(m_dot_lower - m_dot_upper) + m_dot_upper         
        ELSE
            IF(diff_q_target > q_tol)THEN
                m_dot_upper = m_dot
                upflag = .true.
                y_upper = diff_q_target
            ELSEIF(diff_q_target < q_tol)THEN
                m_dot_lower = m_dot
                lowflag = .true.
                y_lower = diff_q_target
            ENDIF
            IF((upflag).and.(lowflag))THEN
                m_dot = (y_upper)/(y_upper - y_lower)*(m_dot_lower - m_dot_upper) + m_dot_upper
            ELSE
                m_dot = 0.5*m_dot_upper + 0.5*m_dot_lower          
            ENDIF
        ENDIF
  
    ENDIF

    !If iterated m_dot approaches lower bound, adjust lower bound to zero
    IF((m_dot-m_dot_low0)/m_dot_low0<0.0005)THEN
        m_dot_low0  = 0.d0
        m_dot_lower = m_dot_low0
        m_dot       = 0.5*m_dot_lower + 0.5*m_dot_upper
    ENDIF
    
    !If iterated m_dot approaches upper bound, increase upper bound
    IF((m_dot_up0-m_dot)/m_dot_up0<0.0005)THEN
        m_dot_up0   = 2.d0*m_dot_up0
        m_dot_upper = m_dot_up0
        m_dot       = 0.5*m_dot_lower + 0.5*m_dot_upper
    ENDIF

    IF(IFLOW==1)    FLOWH = m_dot       ![kg/hr]
    IF(IFLOW==2)    FLOWC = m_dot       ![kg/hr]

    ! COMPUTE CAPACITANCE FLOW RATES
    FLH = CPA*FLOWH     ![kJ/hr-K]
    FLC = CPA*FLOWC     ![kJ/hr-K]

    !Calculate time constant: This value will change every mass flow rate iteration
    tau = (0.632d0) * (CAP/XNODES) / (max(FLH,FLC)) ![kJ/kg] * [hr-K/kJ] => hr
    TC_timestep = tau   ![hr]

    !If calculated timestep is less than TRNSYS timestep, adjust it so that there is a whole number of TC timesteps every TRNSYS timestep
    IF(TC_timestep < DELT)THEN
        !Okay, so if the simulation timestep is not divisible by the thermocline timestep then we need to adjust the TC timestep so it is.  Lower TC timestep to accomplish this
        IF( (DELT - INT(DELT/TC_timestep)*TC_timestep) /= 0.d0 )THEN
        !IF( mod(DELT,TC_timestep) /= 0)THEN
            num_TC = jfix(DELT/TC_timestep) + 1     ![-] Number of thermocline timesteps per simulation timestep
        ELSE
            num_TC = DELT/TC_timestep               ![-]
        ENDIF

        num_TC = min(num_TC_max, num_TC)
        TC_timestep = DELT / dble(num_TC)           ![hr] Corrected timestep (such that the number of TC timesteps in a sim timesteps is an integer) at which to evaluate thermocline 
        
    ELSE        !If calculated timestep is greater than TRNSYS timestep - Maximum TC timestep is the simulation timestep

        num_TC = 1                  ![-] 
        TC_timestep = DELT          ![hr]
        
    ENDIF

    !Reset beginning, average, and end arrays to initial values
    StoreTransfer(1:Nodes) = StoreTransfer0(:)
    StoreTransfer(Nodes+1:2*Nodes) = StoreTransfer0(:)
    StoreTransfer(2*Nodes+1:3*Nodes) = StoreTransfer0(:)
    
    T_ts_ave(:) = 0.d0          ![C] Array storing average packed bed temperature at each timestep
    Q_losses(:) = 0.d0          ![kJ/hr] Array storing summed rate of heat loss at each timestep
    Q_htr(:)    = 0.d0          ![kJ] Array storing summed (nodal) thermal energy required by heater

    !Okay, add outer loop for number of thermocline timesteps in a simulation timestep
    DO tcn = 1,num_TC

        !Set convergence parameters for current run of packed bed simulation
        iter = 0                ![-]
        max_T_diff = 999.d0     ![-]
    
        ! SET COEFFICIENTS FOR ANALYTICAL SOLUTIONS OF INDIVIDUAL NODES
        !0.0005d0
        DO WHILE((iter < 30).and.(max_T_diff > tol_TC))

            iter = iter + 1     ![-] Increase iteration counter
            
            T_ts_ave(tcn) = 0.d0          ![C] Array storing average packed bed temperature at each timestep
            Q_losses(tcn) = 0.d0          ![kJ/hr] Array storing summed rate of heat loss at each timestep
            
            !This is only here for debugging........
            !IF(iter == 30) THEN
            !    continue
            !ENDIF

            !Orginal Type504 code to step through thermocline nodes           
            DO J = 1,NODES
                GO TO (100,150) ,IFLOW

                ! FLOW IS DOWNWARD
                100 continue
                K = J
                IF(K > 1) GO TO 110

                !TOP NODE
                ! gjk ADD UATOP TO AA and BB
                AA = -(FLH+UA+UATOP+EFCOND)/CAPN
                BB = (FLH*THOT+(UA+UATOP)*TENV+EFCOND*StoreTransfer(2*NODES+2))/CAPN
                UA_HL = UA + UATOP      !3/6/12, TN: Keep UA term at every node so losses can be calculated once T_avg is known
                GO TO 200
                110 continue
                IF(K < NODES) GO TO 120

                !BOTTOM NODE
                ! gjk ADD UABOT TO AA and BB
                ! increase capacitance of bottom node BY CAPFAC
                AA = -(FLH+UA+UABOT+EFCOND)/(CAPN*CAPFAC)
                BB = (FLH*StoreTransfer(3*NODES-1)+(UA+UABOT)*TENV+EFCOND*StoreTransfer(3*NODES-1))/(CAPN*CAPFAC)
                UA_HL = UA + UABOT      !3/6/12, TN: Keep UA term at every node so losses can be calculated once T_avg is known
                GO TO 200

                !MIDDLE NODES
                120 continue
                AA = -(FLH+UA+2.d0*EFCOND)/CAPN
                BB = (FLH*StoreTransfer(2*NODES+K-1)+UA*TENV+EFCOND*(StoreTransfer(2*NODES+K-1)+StoreTransfer(2*NODES+K+1)))/CAPN
                UA_HL = UA                 !3/6/12, TN: Keep UA term at every node so losses can be calculated once T_avg is known
                GO TO 200

                !FLOW IS UPWARD
                150 continue
                K = NODES - J + 1
                IF(K < NODES) GO TO 160

                !BOTTOM NODE
                ! gjk ADD UABOT TO AA and BB
                ! increase capacitance of bottom node BY CAPFAC
                AA = -(FLC+UA+UABOT+EFCOND)/(CAPN*CAPFAC)
                BB = (FLC*TCOLD+(UA+UABOT)*TENV+EFCOND*StoreTransfer(3*NODES-1))/(CAPN*CAPFAC)
                UA_HL = UA + UABOT          !3/6/12, TN: Keep UA term at every node so losses can be calculated once T_avg is known
                GO TO 200
                160 continue
                IF(K > 1) GO TO 170

                !TOP NODE
                ! gjk add UATOP TO AA and BB
                AA = -(FLC+UA+UATOP+EFCOND)/CAPN
                BB =  (FLC*StoreTransfer(2*NODES+2)+(UA+UATOP)*TENV+EFCOND*StoreTransfer(2*NODES+2))/CAPN
                UA_HL = UA + UATOP      !3/6/12, TN: Keep UA term at every node so losses can be calculated once T_avg is known
                GO TO 200

                !MIDDLE NODES
                170 continue
                AA = -(FLC+UA+2.d0*EFCOND)/CAPN
                BB = (FLC*StoreTransfer(2*NODES+K+1)+UA*TENV+EFCOND*(StoreTransfer(2*NODES+K+1)+StoreTransfer(2*NODES+K-1)))/CAPN
                UA_HL = UA              !3/6/12, TN: Keep UA term at every node so losses can be calculated once T_avg is known

                !ANALYTICAL SOLUTION
                200 continue
                TI = StoreTransfer(K)
                CALL DIFF_EQ_SUBTS(TIME,TC_timestep,AA,BB,TI,TF,TBAR)
                
                !If final node temperature is below cold limit, then apply heater
                IF(TF < T_htr_set)THEN
                    Q_htr_max = tank_max_heat*1000.d0*TC_timestep*3600.d0   ![MW]*1000(kJ/MJ)*dt(hr)*3600(s/hr)=>kg  Maximum heat rate of tank heater
                    
                    !IF heat has capacity to increase node temperature to setpoint
                    IF( (Q_htr(tcn) + (CAPN*(T_htr_set - TF))) < tank_max_heat )THEN
                        Q_htr(tcn) = (Q_htr(tcn) + (CAPN*(T_htr_set - TF)))   ![kJ]   Thermal energy required by heater to maintain cold limit in tank
                        TF = T_htr_set  ![C] Node hits setpoint
                    
                    !If heater does not have capacity
                    ELSE
                        !Q_htr_node = tank_max_heat - Q_htr(tcn)
                        !CAPN*(TF_new - TF) = Q_htr_node
                        !TF_new = Q_htr_node / CAPN + TF
                        
                        TF = (tank_max_heat - Q_htr(tcn))/CAPN + TF     ![C] Node does not hit setpoint
                    ENDIF
                ENDIF
                
                StoreTransfer(NODES+K) = TF
                !250 continue
                Diff_T_ave(K) = abs(StoreTransfer(2*NODES+K) - TBAR)    ![C] Difference between old average node temp and new average node temp
                StoreTransfer(2*NODES+K) = TBAR                         ![C] Update guess on average node temp now that difference is calculated
                T_ts_ave(tcn) = T_ts_ave(tcn) + TBAR                    ![C] Add average node temps
                Q_losses(tcn) = Q_losses(tcn) + UA_HL * (TBAR - TENV)   ![kJ/hr-K]*[K] -> [kJ/hr] Heat loss
            ENDDO       !End step through thermocline

            T_ts_ave(tcn) = T_ts_ave(tcn)/dble(Nodes)                   ![C] Spatial average of nodal time averaged temps
            max_T_diff = maxval(Diff_T_ave)/290.d0                      ![-] Max nodal relative temperature difference

        ENDDO       !End iteration on average (time) nodal temperatures
        
        T_cout_ave(tcn) = StoreTransfer(3*Nodes)    ![C] Store the average cold outlet temperature of each sub timestep
        T_hout_ave(tcn) = StoreTransfer(2*Nodes+1)  ![C] Store the average hot outlet temperature of each sub timestep
        
        !Check here for over(dis)charge?  Could save time when solving for correct mass flow rate
        
        !After timestep has solved, set initial nodes of next timestep to end nodes of current timestep
        StoreTransfer(1:NODES) = StoreTransfer(NODES+1:2*NODES)
        
    ENDDO

    ! DETERMINE AVERAGE ROCKBED TEMPERATURES AND AVERAGE DERIVATIVES
    TFBAR = 0.d0
    DO K = 1,NODES
        TFBAR = TFBAR + StoreTransfer(NODES+K)  
    ENDDO
    TFBAR = TFBAR/DBLE(NODES)       ![C] Spatial average of temps at end of final timestep

    !TN: Average rockbed temp is now the average (sub timesteps in simulation) of averages (nodal in each sub timestep) of averages (time based in each sub timestep)
    TAVG = 0.d0
    DO tcn=1,num_TC
        TAVG = TAVG + T_ts_ave(tcn)
    ENDDO
    TAVG = TAVG/dble(num_TC)    ![C] Time-average, spatial averaged nodal temperature (how useful is this?)
    !********************************************************************************

    !TN: Outlet temperature to pass should be average (sub timesteps in simulation) of averages (time based in each sub timestep)
    T_disch_avail = 0.d0
    T_charge_avail = 0.d0
    DO tcn=1,num_TC
        T_disch_avail = T_disch_avail + T_hout_ave(tcn)
        T_charge_avail = T_charge_avail + T_cout_ave(tcn)
    ENDDO
    T_disch_avail = T_disch_avail/dble(num_TC)      ![C] Time-averaged discharging OUTLET temperature (this is a CONFUSING name)
    T_charge_avail = T_charge_avail/dble(num_TC)    ![C] Time-averaged charging OUTLET temperature (this is a CONFUSING name)

    !Check if tank has been over-(dis)charged with current mass flow rate
    IF(IFLOW==1)THEN        !Charging, so look for min cold temp hotter than allowed
        diff_Tcmax = StoreTransfer(2*Nodes) - Tcmax

    ELSEIF(IFLOW==2)THEN    !Discharging, so look for max hot temp cooler than allowed
        !diff_Thmin = StoreTransfer(Nodes+1) - Thmin
        !4/23/12, TN: Account for storage discharge fraction
        diff_Thmin = StoreTransfer(Nodes+ChargeNodes) - Thmin

    ENDIF
       
    TC_limit = 0    ![-] Reset Flag
    
    !If solving thermocline for specified mass flow rate:
    IF(know_mdot)THEN
        full = .false.      !Code did not result in target energy (dis)charge because in different solving mode
        
        !Charging, specified mass flow rate
        IF(IFLOW==1)THEN
            q_calc  = m_dot*CPA*(THOT - T_charge_avail)/3.6d0     ![W] Calculated rate of energy charge            
            
            !If specified mass flow caused over-charging and thus iteration:
            IF(mdot_iter)THEN                   
                
                !If within tolerance on max cold temperature then get out
                IF( (diff_Tcmax > -t_tol).and.(diff_Tcmax <= 0.d0) )THEN           
                    !Set convergence criteria to 0 to get out
                    diff_q_target = 0.d0
                    TC_limit = 0
                
                !If min cold temp is greater than allowed then:
                ELSEIF(diff_Tcmax > 0)THEN  
                    TC_limit = 1    ![-] Flag noting that upper limit on mass flow should be set
                
                !Else, if tank is not "close enough" to being fully charged:
                ELSE                        
                    TC_limit = 2    ![-] Flag noting that lower limit on mass flow should be set
                ENDIF
            
            !If first iteration for a specified mass flow rate
            ELSE    
                
                !If tank does NOT overcharge, then get out
                IF(diff_Tcmax <= 0.d0)THEN
                    !Set convergence criteria to 0 to get out  
                    diff_q_target = 0.d0
                    TC_limit = 0
                    
                !Else, know that mass flow rate is too high    
                ELSE    
                    mdot_iter = .true.  ![-] Flag noting that correct mass flow rate must be found via iteration
                    TC_limit = 1        ![-] Flag noting that upper limit on mass flow should be set
                ENDIF
            ENDIF                    
        
        !Discharging, specified mass flow rate
        ELSE
            q_calc  = m_dot*CPA*(T_disch_avail - TCOLD)/3.6d0     ![W]: Calculated rate of energy discharge
            
            !If specified mass flow caused over-charging and thus iteration:
            IF(mdot_iter)THEN   !If specified mass flow caused over-discharging:
            
                !If within tolerance on min hot temperature then get out
                IF( (diff_Thmin >= 0.d0).and.(diff_Thmin < t_tol) )THEN 
                    !Set convergence criteria to 0 to get out
                    diff_q_target = 0.d0
                    TC_limit = 0.d0
                    
                !If max hot temp is less than allowed then:
                ELSEIF(diff_Thmin < 0)THEN  
                    TC_limit = 1    ![-] Flag noting that upper limit on mass flow should be set
                    
                !Else, if tank is not "close enough" to being fully discharged:    
                ELSE                        
                    TC_limit = 2    ![-] Flag noting that lower limit on mass flow should be set
                ENDIF
                
            !If first iteration for a specified mass flow rate    
            ELSE   
            
                !If tank does NOT over-discharge, then get out
                IF(diff_Thmin >= 0.d0)THEN
                    !Set convergence criteria to 0 to get out
                    diff_q_target = 0.d0
                    TC_limit = 0
                    
                !Else, know that mass flow rate is too high    
                ELSE    
                    mdot_iter = .true.  ![-] Flag noting that correct mass flow rate must be found via iteration
                    TC_limit = 1        ![-] Flag noting that upper limit on mass flow should be set
                ENDIF
            ENDIF
        ENDIF
    
    !Only calling model to calculate new thermocline (dwelling, so still need to calculate losses and conduction), get out
    ELSEIF(q_target==0.d0)THEN
        !Set convergence criteria to 0 to get out  
        diff_q_target = 0.d0
        TC_limit = 0
    
    !Solving thermocline for specified charging energy
    ELSEIF(IFLOW==1)THEN
        q_calc  = m_dot*CPA*(THOT - T_charge_avail)/3.6d0     ![kg/hr]*[kJ/kg-K]*[K]*(1000)[J/kJ]*(1/3600)[hr/s]->[W] Calculated rate of energy charge        
        
        !If not removing enough energy and "close enough (but not going over)" to completely filling tank, then get out
        IF((q_calc < q_target).and.(diff_Tcmax > -t_tol).and.(diff_Tcmax <= 0.d0))THEN       
            !Set convergence criteria to 0 to get out
            diff_q_target = 0.d0        ![W]
            TC_limit = 0                ![-]
            full = .false.              ![-] Note that code did not result in target energy rate
                
        ELSEIF(diff_TCmax > 0.d0)THEN    ! Over-charging tank, flag to indicate mass flow is too high
            TC_limit = 1
            
        ELSE        ! Not over-charging tank -> compare calculated to target        
            diff_q_target = (q_calc - q_target)/q_target       ![W] Relative difference between calculated and required rate of energy
        ENDIF            
    
    !Solving thermocline for specified discharging energy
    ELSE
        q_calc  = m_dot*CPA*(T_disch_avail - TCOLD)/3.6d0     ![kg/hr]*[kJ/kg-K]*K*[W/kW][hr/s]=>[W]: Calculated rate of energy discharge
        
        !If not getting enough energy and "close enough (but not going over)" to depleting tank, then get out
        IF((q_calc < q_target).and.(diff_Thmin < t_tol).and.(diff_Thmin >= 0.d0))THEN
            !Set convergence criteria to 0 to get out
            diff_q_target = 0.d0        ![W]
            TC_limit = 0                ![-] 
            full = .false.              ![-] Note that code did not result in target energy rate
        
        ELSEIF(diff_Thmin < 0.d0)THEN   !Over-discharging tank, flag to indicate mass flow is too high
            TC_limit = 1  
                     
        ELSE        ! Not over-discharging tank -> compare calculated to target
            diff_q_target = (q_calc - q_target)/q_target       ![W] Relative difference between calculated and required rate of energy
        ENDIF                    
    ENDIF  
    
ENDDO       !End of iteration on mass flow rate


IF((q_iter == 40).and.( (abs(diff_q_target)>q_tol).or.(TC_limit/=0) ))THEN
    full = .false.
ENDIF

IF(full)THEN        !If within tolerance on target energy rate, then set to target => this is beneficial to the solver (Type 251)
    q_calc = q_target       ![W]
ENDIF

!Calculating some important outputs
Q_loss_total = sum(Q_losses)*DELT/num_TC       ![kJ/hr]*[hr] Heat losses over timestep

IF(IFLOW==1)THEN        !Charging
    m_charge_avail = m_dot  ![kg/hr]
    
    !Energy Balance Calculations
    q_charge = DELT*m_dot*CPA*(THOT - T_charge_avail)      ![hr]*[kg/hr]*[kJ/kg-K]*[K]->[kJ] 
    q_stored = CAP*(TFBAR - TFBAR_0)                       ![kJ/K]*[K]->[kJ]
    ![-] Relative difference.  Scale by m_dot such that the losses don't create huge errors on timesteps with no mass flow rate.
    !q_error  = min(1.d0,m_dot)*(q_charge - q_stored)/max(0.01,q_stored)  
    q_error  = (q_charge - q_stored - q_loss_total)/max(0.01,abs(q_stored))            
                                                                                        
    
ELSE    !Discharging
    m_disch_avail = m_dot   ![kg/hr]
    
    !Energy Balance Calculations
    q_discharge = DELT*m_dot*CPA*(T_disch_avail - TCOLD)      ![hr]*[kg/hr]*[kJ/kg-K]*[K]->[kJ]
    q_stored    = CAP*(TFBAR - TFBAR_0)                       ![kJ/K]*[K]->[kJ]                      
    !q_error     = min(1.d0,m_dot)*(q_discharge - q_stored)/max(0.01,q_stored)           ![-]
    q_error     = (-q_stored - q_discharge - q_loss_total)/max(0.01,abs(q_stored))
    
ENDIF

Q_htr_total = 0.d0
DO tcn=1,num_TC
    Q_htr_total = Q_htr_total + Q_htr(tcn)  ![kJ] Total energy required by heater during timestep
ENDDO

OUT(1)  = m_disch_avail*tank_pairs     ![kg/hr]
OUT(2)  = T_disch_avail     ![C] Discharge temp (HOT)
OUT(3)  = m_charge_avail*tank_pairs    ![kg/hr]
OUT(4)  = T_charge_avail    ![C] Charge temp (COLD)
OUT(5)  = q_calc*tank_pairs            ![W]
OUT(6)  = Q_loss_total*tank_pairs      ![kJ] Heat losses
OUT(7)  = StoreTransfer(Nodes+1)    ![C] Final temperature at hot node
OUT(8)  = StoreTransfer(2*Nodes)    ![C] Final temperature at cold node
OUT(9)  = maxval(StoreTransfer(Nodes+1:2*Nodes))    ![C] Maximum temperature in TC

!reset counters
ihlim = 1
iclim = 1
do k=1,Nodes
    !find the last node where the HTF temperature is above the hot side limit
    !if(StoreTransfer(Nodes+k) > Th_avail_min) then
    if(StoreTransfer(Nodes+k) > T_hot_in_min) then
        ihlim=k         ![-]
    endif
    !find the last node where the HTF temperature is below the cold side limit
    !if(StoreTransfer(2*Nodes+1-k) < Tc_avail_max) then
    if(StoreTransfer(2*Nodes+1-k) < T_cold_in_max) then
        iclim=k         ![-]
    endif
enddo

OUT(10) = dble(ihlim - 1)/dble(Nodes - 1)     ![-] Fraction of depth at which hot temperature decreases below minimum hot temperature limit
OUT(11) = dble(Nodes - iclim)/dble(Nodes - 1) ![-] Fraction of depth at which cold temperature increases above maximum cold temperature limit
OUT(12) = Q_htr_total*tank_pairs                         ![kJ] Total energy required by heater to keep tank above minimum cold temperature

continue

RETURN

contains

    !Density of Bed Material [kg/m3]
    real(8) function dens_bed(BedNum) 
        real(8),intent(in)::BedNum
        select case(int(BedNum))
        case(1); dens_bed = 3800.d0     !1) Taconite
        case(2); dens_bed = 2710.d0     !2) Calcium carbonate
        case(3); dens_bed = 2643.d0     !3) Gravel
        case(4); dens_bed = 2680.d0     !4) Marble
        case(5); dens_bed = 2320.d0     !5) Limestone
        case(6); dens_bed = 7854.d0     !6) Carbon Steel
        case(7); dens_bed = 1515.d0     !7) Sand
        case(8); dens_bed = 2640.d0     !8) Quartzite
        end select
    End Function

    !Bed material heat capacity [kJ/kg-K]
    real(8) function Cp_bed(BedNum)
        real(8),intent(in)::BedNum
        select case(int(BedNum))
        case(1); Cp_bed = 0.651d0       !1) Taconite
        case(2); Cp_bed = 0.835d0       !2) Calcium carbonate
        case(3); Cp_bed = 1.065d0       !3) Gravel ||at average temperature of 335 C!810# + 0.75 * Temp J/kg C orginal expression with temp correction
        case(4); Cp_bed = 0.83d0        !4) Marble
        case(5); Cp_bed = 0.81d0        !5) Limestone
        case(6); Cp_bed = 0.567d0       !6) Carbon steel
        case(7); Cp_bed = 0.8d0         !7) Sand
        case(8); Cp_bed = 1.105d0       !8) Quartzite
        end select
    End Function
    
    !Bed material conductivity [W/m-K]
    real(8) function k_bed(BedNum)
        real(8),intent(in)::BedNum
        select case(int(BedNum))
        case(1); k_bed = 2.1d0          !1) Taconite
        case(2); k_bed = 2.7d0          !2) Calcium carbonate
        case(3); k_bed = 1.8d0          !3) Gravel
        case(4); k_bed = 2.8d0          !4) Marble
        case(5); k_bed = 2.15d0         !5) Limestone
        case(6); k_bed = 48.d0          !6) Carbon steel (average 335
        case(7); k_bed = 0.27d0         !7) Sand
        case(8); k_bed = 5.38d0         !8) Quartzite
        end select
    end function
    
    !mjw Effective conductivity of the bed filler+HTF - A general formulation should be provided for this since 
    !the user will be selecting properties indirectly from drop down menus with material name
    real(8) function Cond_eff(BedNum,HTF,voidfrac,T)
        real(8),intent(in)::BedNum,HTF, voidfrac,T
        real(8)::conductivity
        !PLACEHOLDER FUNCTION ONLY!!!!!
        cond_eff = voidfrac*conductivity(HTF,T + 273.15d0,1.d0) + (1.d0-voidfrac)*k_bed(BedNum)
    end function

END

SUBROUTINE DIFF_EQ_SUBTS(TIME,DELT,AA,BB,TI,TF,TBAR)

!**********************************************************************************
! THIS SUBROUTINE PROVIDES AN ANALYTICAL COLUTION TO DIFFERENTIAL EQUATIONS THAT 
!  CAN BE WRITTEN IN THE FORM DT\DT=A*T+B.
! 11/28/11, TN: This subroutine is the same as differential_eqn except
!               an input for the timestep was added
!**********************************************************************************

IMPLICIT NONE !force explicit declaration of all variables

!    LOCAL VARIABLE DECLARATIONS
DOUBLE PRECISION TBARD,AAD,BBD,TID,TFD
DOUBLE PRECISION TIME,DELT,AA,BB,TI,TF,TBAR

IF(ABS(AA) .GT. 0.)THEN
!    SOLUTION TO DIFFERENTIAL IS EXPONENTIAL
      AAD = AA
      BBD = BB
      TID = TI
      TFD = (TID + BBD/AAD)*DEXP(AAD*DELT) - BBD/AAD
      TBARD = (TID + BBD/AAD)/AAD/DELT*(DEXP(AAD*DELT) - 1.) - BBD/AAD
      TF = TFD
      TBAR = TBARD

ELSE
!    SOLUTION TO DIFFERENTIAL EQUATION IS LINEAR
      TF = BB*DELT + TI
      TBAR = (TF + TI)/2.                 
ENDIF

END SUBROUTINE