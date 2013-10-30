SUBROUTINE TYPE504(TIME,XIN,OUT,T,DTDT,PAR,INFO,ICNTRL,*)

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

! Doc. tables updated 2011-05-03 - MJW
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
!   15| slope_tc                         | Slope of the thermocline portion of the tank                      | C/m              | C/m              

!export this subroutine for its use in external DLLs.
!DEC$ATTRIBUTES DLLEXPORT :: TYPE504
   
! USE STATEMENTS
use TrnsysFunctions
use TrnsysConstants

IMPLICIT NONE !force explicit declaration of local variables

! TRNSYS DECLARATIONS
DOUBLE PRECISION XIN,OUT,TIME,PAR,T,DTDT,TIME0,TFINAL,DELT
INTEGER*4 INFO(15),NP,NI,NO,NDMAX,IUNIT,ITYPE,ICNTRL
CHARACTER*3 OCHECK,YCHECK

! SET THE MAXIMUM NUMBER OF PARAMETERS(NP),INPUTS(NI),OUTPUTS(NOUT),AND DERIVATIVES(ND)
! THAT MAY BE SUPPLIED FOR THIS TYPE
PARAMETER (NP=11,NI=5,NO=15,NDMAX=100)

! REQUIRED TRNSYS DIMENSIONS
DIMENSION XIN(NI),OUT(NO),PAR(NP),YCHECK(NI),OCHECK(NO)
DIMENSION T(*),DTDT(*)

! LOCAL VARAIBLE DECLARATIONS
CHARACTER (len=maxMessageLength) T10_Msg1
INTEGER StorageSize,NODES,ND,J,K,IFLOW, ihlim, iclim, I
DOUBLE PRECISION StoreTransfer(NDMAX),XNODES,CPA,H,A,P,CPR,RHOR,U,COND,TI,VOL,UA,EFCOND,CAP,CAPN,&
                 THOT,FLOWH,TCOLD,FLOWC,TENV,FLH,FLC,AA,BB,TF,TBAR,TFBAR,TAVG,DELU,QENV,QTANK,UTOP,&
                 UBOT, RHOA, VOID, CAPFAC, UATOP, UABOT,&
                 !4.25.11 mjw added:
                 Thmin, Tcmax, m_disch_avail, T_disch_avail, m_charge_avail, T_charge_avail, fhlim, fclim,&
                 Tctemp, Thtemp, Tprop, specheat, density, conductivity, Qd_fill, Qc_fill, md_tc, md_add, &
                 mc_tc, mc_add, HTF, FILL, slope_tc
real(8),parameter::pi=3.1415926

! DATA STATEMENTS
!DATA YCHECK/'TE1','MF1','TE1','MF1','TE1'/
!DATA OCHECK/'TE1','MF1','TE1','MF1','PW1','PW1','EN1','TE1','TE1','TE1','MF1','TE1','MF1','TE1'/ 
DATA IUNIT/0/,ITYPE/0/
DATA T10_Msg1/'The rock bed thermal storage component only allows flow in one direction at a time. &
               Positive flow into both the top and bottom of the bed were specified.'/
      
! TRNSYS FUNCTIONS
TIME0=getSimulationStartTime()
TFINAL=getSimulationStopTime()
DELT=getSimulationTimeStep()

!--------------------------------------------------------------------------------------------------
! SET THE VERSION INFORMATION FOR TRNSYS
IF(INFO(7)==-2) THEN
    INFO(12)=16
    RETURN 1
ENDIF

! PERFORM LAST CALL MANIPULATIONS 
IF (INFO(8)==-1) RETURN 1

! PERFORM POST CONVERGENCE MANIPULATIONS 
IF(INFO(13)>0) THEN
    StorageSize = INFO(5)*3
    CALL getStorageVars(StoreTransfer,StorageSize,INFO)
    NODES = INFO(5)
    StoreTransfer(1:NODES) = StoreTransfer(NODES+1:2*NODES)
    CALL setStorageVars(StoreTransfer,StorageSize,INFO)
    RETURN 1
ENDIF

! PERFORM FIRST CALL MANIPULATIONS
IF (INFO(7)==-1) THEN
    !retrieve unit and type number for this component from the INFO array
    IUNIT=INFO(1)
    ITYPE=INFO(2)
    !retrieve the number of derivatives specified for this component from the INFO array
    ND = INFO(5)
    NODES = ND
    !set some info array variables to tell the trnsys engine how this type is to work
    INFO(6)  = NO   !reserve space in the OUT array using INFO(6)
    INFO(9)  = 1    !set the way in which this component is to be called
    INFO(10) = 0    !set required number of spots in the single precision storage structure
    !reserve spave in the double precision storage structure
    StorageSize = NODES*3
    CALL setStorageSize(StorageSize,INFO)
    !call the type check subroutine to compare what this type requires with what is in the input file. 
    !CALL TYPECK(1,INFO,NI,NP,ND)
    !call the input-output check subroutine to set the correct input and output units.
    !CALL RCHECK(INFO,YCHECK,OCHECK)
    !return to the calling program
    RETURN 1
ENDIF
	
! PERFORM INITIAL TIMESTEP MANIPULATIONS
IF (TIME<(TIME0+DELT/2.)) THEN
    !set the UNIT number for future calls
    IUNIT=INFO(1)
    ITYPE=INFO(2)
    !read parameter values
    NODES = INFO(5)
    XNODES = DBLE(NODES)
    HTF  = PAR(1)
    H    = PAR(2)
    A    = PAR(3)
    FILL = PAR(4)
    U    = PAR(5)
    ! gjk added 4 new parameters
    UTOP = PAR(6)
    UBOT = PAR(7)
    VOID = PAR(8)
    ! GJK capfac to add thermal mass to bottom of tank
    CAPFAC = PAR(9)
    ! mjw add temperature control parameters
    Thmin = PAR(10) 
    Tcmax = PAR(11)   
    
    !mjw do initial estimate of COND
    cond = cond_eff(FILL, HTF, VOID, T(int(.5*Nodes)))
    
    !check the parameters for problems and RETURN if any are found
    IF(XNODES<=0.d0) CALL TYPECK(-5,INFO,0,0,0)
    !IF(CPA<=0.d0)    CALL TYPECK(-4,INFO,0,1,0)
    IF(H<=0.d0)      CALL TYPECK(-4,INFO,0,2,0)
    IF(A<=0.d0)      CALL TYPECK(-4,INFO,0,3,0)
    !IF(P<=0.d0)      CALL TYPECK(-4,INFO,0,4,0)
    !IF(CPR<=0.d0)    CALL TYPECK(-4,INFO,0,5,0)
    !IF(RHOR<=0.d0)   CALL TYPECK(-4,INFO,0,6,0)
    IF(U<=0.d0)      CALL TYPECK(-4,INFO,0,7,0)
    IF(COND<=0.d0)   CALL TYPECK(-4,INFO,0,8,0)
    !initialize storage
    StoreTransfer(NODES+1:2*NODES) = T(1:NODES)
    StoreTransfer(2*NODES+1:3*NODES) = T(1:NODES)
    StoreTransfer(1:NODES) = StoreTransfer(NODES+1:2*NODES)
    CALL setStorageVars(StoreTransfer,StorageSize,INFO)
    TI = SUM(T(1:NODES))/XNODES
    !set the outputs to 0
    OUT(1) = StoreTransfer(3*NODES)
    OUT(2) = XIN(2)
    OUT(3) = StoreTransfer(2*NODES+1)
    OUT(4) = XIN(4)
    OUT(5:7) = 0.d0 !zero energy transfer rates at initial time
    OUT(8) = TI
    OUT(9) = TI
    !mjw 4.25.11
    OUT(10) = TI
    OUT(11) = 0.d0
    OUT(12) = TI
    OUT(13) = 0.d0
    OUT(14) = TI
    OUT(15) = 1.
    RETURN 1         !the first timestep is for initialization - exit.
ENDIF

!--------------------------------------------------------------------------------------------------
! THIS IS AN ITERATIVE CALL TO THIS COMPONENT ***
!--------------------------------------------------------------------------------------------------

! RE-READ THE PARAMETERS IF ANOTHER UNIT OF THIS TYPE HAS BEEN CALLED
10 continue
IF(INFO(1).NE.IUNIT) THEN
    !recall the UNIT and TYPE number
    IUNIT = INFO(1)
    ITYPE = INFO(2)
    !read parameter values
    NODES = INFO(5)
    XNODES = DBLE(NODES)
    HTF  = PAR(1)
    H    = PAR(2)
    A    = PAR(3)
    FILL = PAR(4)
    U    = PAR(5)
    ! gjk added 4 new parameters
    UTOP = PAR(6)
    UBOT = PAR(7)
    VOID = PAR(8)
    ! GJK capfac to add thermal mass to bottom of tank
    CAPFAC = PAR(9)
    ! mjw add temperature control parameters
    Thmin = PAR(10) 
    Tcmax = PAR(11)   
ENDIF

!mjw 4.25.11 Calculate the effective conductance of the thermocline and other fluid properties
Tprop = StoreTransfer(int(Nodes*1.5))
cond = cond_eff(FILL, HTF, VOID, Tprop)
CPA = specheat(HTF,Tprop+273.15d0,1.d0) ![kJ/kg-K]
RHOA = density(HTF,Tprop+273.15d0,1.d0)
CPR = Cp_bed(FILL)  ![kJ/kg-K]
RHOR = Dens_bed(FILL)


! RETRIEVE THE STORAGE VARIABLES
NODES = INFO(5)
StorageSize = NODES*3
CALL getStorageVars(StoreTransfer,StorageSize,INFO)

! CALCULATE PARAMETER DEPENDENT VALUES
!mjw 4.26.11 Assume a cylindrical tank and calculate perimeter
P = sqrt(A/pi)*2.*pi
VOL = A*H
UA = U*P*H/XNODES
! gjk add UATOP AND UABOT
UATOP = UTOP*A
UABOT = UBOT*A

EFCOND = COND*A*XNODES/H
!CAP = VOL*CPR*RHOR
! gjk add oil to thermal capacity of a node
CAP = VOL*VOID*CPA*RHOA + VOL*(1.-VOID)*CPR*RHOR
CAPN = CAP/XNODES

! GET THE CURRENT VALUES OF THE INPUTS
30 continue
THOT = XIN(1)
FLOWH = XIN(2)
TCOLD = XIN(3)
FLOWC = XIN(4)
TENV = XIN(5)
 
! THIS MODEL DOES NOT ALLOW FOR FLOW IN BOTH DIRECTIONS AT ONCE. IF XIN(2) AND XIN(4) ARE BOTH SET, GENERATE ERROR
IF((FLOWH>0.d0).AND.(FLOWC>0.d0)) THEN
    CALL MESSAGES(-1,T10_Msg1,'fatal',IUNIT,ITYPE)
    RETURN 1
ENDIF
 
! COMPUTE CAPACITANCE FLOW RATES
FLH = CPA*FLOWH
FLC = CPA*FLOWC

! DETERMINE FLOW DIRECTION
40 continue
IFLOW = 1
IF(FLOWC > 0.) IFLOW = 2
IF(NODES == 1) GO TO 400

! SET COEFFICIENTS FOR ANALYTICAL SOLUTIONS OF INDIVIDUAL NODES
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
    GO TO 200
    110 continue
    IF(K < NODES) GO TO 120

    !BOTTOM NODE
    ! gjk ADD UABOT TO AA and BB
    ! increase capacitance of bottom node BY CAPFAC
    AA = -(FLH+UA+UABOT+EFCOND)/(CAPN*CAPFAC)
    BB = (FLH*StoreTransfer(3*NODES-1)+(UA+UABOT)*TENV+EFCOND*StoreTransfer(3*NODES-1))/(CAPN*CAPFAC)
    GO TO 200

    !MIDDLE NODES
    120 continue
    AA = -(FLH+UA+2.d0*EFCOND)/CAPN
    BB = (FLH*StoreTransfer(2*NODES+K-1)+UA*TENV+EFCOND*(StoreTransfer(2*NODES+K-1)+StoreTransfer(2*NODES+K+1)))/CAPN
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
    GO TO 200
    160 continue
    IF(K > 1) GO TO 170

    !TOP NODE
    ! gjk add UATOP TO AA and BB
    AA = -(FLC+UA+UATOP+EFCOND)/CAPN
    BB =  (FLC*StoreTransfer(2*NODES+2)+(UA+UATOP)*TENV+EFCOND*StoreTransfer(2*NODES+2))/CAPN
    GO TO 200

    !MIDDLE NODES
    170 continue
    AA = -(FLC+UA+2.d0*EFCOND)/CAPN
    BB = (FLC*StoreTransfer(2*NODES+K+1)+UA*TENV+EFCOND*(StoreTransfer(2*NODES+K+1)+StoreTransfer(2*NODES+K-1)))/CAPN

    !ANALYTICAL SOLUTION
    200 continue
    TI = StoreTransfer(K)
    CALL DIFFERENTIAL_EQN(TIME,AA,BB,TI,TF,TBAR)
    StoreTransfer(NODES+K) = TF
    250 continue
    StoreTransfer(2*NODES+K) = TBAR
ENDDO

! DETERMINE AVERAGE ROCKBED TEMPERATURES AND AVERAGE DERIVATIVES
TFBAR = 0.d0
TAVG = 0.d0
DO K = 1,NODES
    TFBAR = TFBAR + StoreTransfer(NODES+K)
    TAVG = TAVG + StoreTransfer(2*NODES+K)
    DTDT(K) = (StoreTransfer(NODES+K)-StoreTransfer(K))/DELT
ENDDO
TFBAR = TFBAR/DBLE(NODES)
TAVG = TAVG/DBLE(NODES)
GO TO 450

! ONE NODE EQUATIONS
400 GO TO (410,420) ,IFLOW

! FLOW IS DOWNWARD
410   AA = -(FLH+UA)/CAP
BB = (FLH*THOT+UA*TENV)/CAP
GO TO 430

! FLOW IS UPWARD
420 continue
AA = -(FLC+UA)/CAP
BB = (FLC*TCOLD+UA*TENV)/CAP

! SOLUTION
430 continue
TI = StoreTransfer(1)
CALL DIFFERENTIAL_EQN(TIME,AA,BB,TI,TFBAR,TAVG)
StoreTransfer(NODES+1) = TFBAR
StoreTransfer(2*NODES+1) = TAVG
DTDT(1) = (TFBAR-TI)/DELT

! DETERMINE ENERGY RATES AND CHANGE IN INTERNAL ENERGY
450 continue
DELU = CAP*(TFBAR-OUT(9))
!   QENV = UA*DBLE(NODES)*(TAVG-TENV)
! gjk add top and bottom heat losses to loss calc
QENV = UA*REAL(NODES)*(TAVG-TENV)+UATOP*(StoreTransfer(2*NODES+1)-TENV)+UABOT*(StoreTransfer(2*NODES+NODES)-TENV)
QTANK = FLC*(StoreTransfer(2*NODES+1)-TCOLD)

!mjw 4.25.11 Calulcate the available volumes for charge/discharge given the control temperature limitations
!--The StoreTransfer array contains 3*Nodes values. The values refer to the following information:
!   1:Nodes             ->  Temperature by node at the beginning of the time step
!   Nodes+1:2*Nodes     ->  Temperature by node at the end of the time step
!   2*Nodes+1:3*Nodes   ->  Average temperature by node over the time step
!--Nodes are ordered from tank top to tank bottom (1->Nodes : top->bottom)
iclim=0; ihlim=0; Thtemp=0.d0; Tctemp=0.d0
do k=1,Nodes
    !find the last node where the HTF temperature is above the hot side limit
    if(StoreTransfer(Nodes+k) > Thmin) then
        ihlim=k
        Thtemp = Thtemp + StoreTransfer(Nodes+k)
    endif
    !find the last node where the HTF temperature is below the cold side limit
    if(StoreTransfer(2*Nodes+1-k) < Tcmax) then
        iclim=k
        Tctemp = Tctemp + StoreTransfer(2*Nodes+1-k)
    endif
enddo
!What fraction of the total tank height is above/below the limit?
!-hot side
if(ihlim==0 .or. ihlim==1 .or. ihlim==Nodes) then
    fhlim = dble(ihlim)
    Thtemp = StoreTransfer(Nodes+max0(ihlim,1))
else
    fhlim = dble(ihlim+1)-(Thmin - StoreTransfer(Nodes+ihlim+1))/(StoreTransfer(Nodes+ihlim) - StoreTransfer(Nodes+ihlim+1))
    Thtemp = Thtemp + modulo(fhlim,1.d0)*StoreTransfer(Nodes+ihlim+1)
endif
!-cold side
if(iclim==0 .or. iclim==1 .or. iclim==Nodes) then
    fclim = dble(iclim)
    Tctemp = StoreTransfer(2*Nodes + 1 - max0(iclim,1))
else
    fclim = dble(iclim)+(Tcmax - StoreTransfer(2*Nodes+1-iclim))/(StoreTransfer(2*Nodes-iclim) - StoreTransfer(2*Nodes+1-iclim))
    Tctemp = Tctemp + modulo(fclim,1.d0)*StoreTransfer(2*Nodes+2-iclim)
endif

!--Determine the available mass flows and temperatures
Thtemp = Thtemp/dmax1(fhlim,1.d0)   !make the hot/cold temperatures the average
Tctemp = Tctemp/dmax1(fclim,1.d0)
!Calculate the volume of HTF that the hot filler can bring up to temperature
fhlim = fhlim/XNodes
fclim = fclim/XNodes
Qd_fill = vol * fhlim * rhor * cpr * (1.-void) * dmax1(Thtemp - Thmin, 0.d0) ![kJ]
Qc_fill = vol * fclim * rhor * cpr * (1.-void) * dmax1(Tcmax - Tctemp, 0.d0) ![kJ]
!discharging
md_tc = vol * fhlim * void * density(HTF,(Thtemp+Thmin)/2.d0+273.15d0,1.d0) ![kg] HTF mass present in the hot portion of the thermocline
md_add = Qd_fill/((Thtemp - Thmin) * specheat(HTF,(Thtemp+Thmin)/2.d0+273.15d0,1.d0))  ![kg] HTF mass heated up by filler
m_disch_avail = (md_tc + md_add)/delt   ![kg/hr] translated to mass flow rate
!charging
mc_tc = vol* fclim * void * density(HTF,(Tctemp+Tcmax)/2.d0+273.15d0,1.d0)  ![kg] Mass present in the cool portion of the thermocline
mc_add = Qc_fill/((Tcmax - Tctemp) * specheat(HTF,(Tcmax+Tctemp)/2.d0+273.15d0,1.d0))  ![kg] HTF mass cooled by the filler
m_charge_avail = (mc_tc + mc_add)/delt  ![kg/hr] translated to mass flow rate
!temperatures
if(mc_tc + mc_add > 0.d0) then
    T_disch_avail = (Thtemp*mc_tc + (Thmin+Thtemp)/2.d0*mc_add)/(mc_tc + mc_add) ![C] weighted average 
else; T_disch_avail = Thtemp; endif
if(md_tc + md_add > 0.d0) then
    T_charge_avail = (Tctemp*md_tc + (Tcmax+Tctemp)/2.d0*md_add)/(md_tc + md_add)
else; T_charge_avail = Tctemp; endif

!Calculate the temperature gradient of the unusable thermocline portion
slope_tc = (Thmin - Tcmax)/(dmax1(.01d0,1. - (fclim + fhlim)) * h)

! SET THE CURRENT STORAGE VARIABLES
call SetStorageVars(StoreTransfer,StorageSize,INFO)

! SET OUTPUTS
OUT(1) = StoreTransfer(3*NODES)
OUT(2) = FLOWH
OUT(3) = StoreTransfer(2*NODES+1)
OUT(4) = FLOWC
OUT(5) = QENV
OUT(6) = QTANK
OUT(7) = DELU
OUT(8) = StoreTransfer(2*NODES+4)
OUT(9) = StoreTransfer(2*NODES+8)
OUT(10) = TAVG
OUT(11) = m_disch_avail
OUT(12) = T_disch_avail
OUT(13) = m_charge_avail
OUT(14) = T_charge_avail
OUT(15) = slope_tc
RETURN 1

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
        cond_eff = voidfrac*conductivity(HTF,T,1.d0) + (1.d0-voidfrac)*k_bed(BedNum)
    end function

END
