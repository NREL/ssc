! TRNSYS Type 806: Thermal Storage and Dispatch Model based on Excelergy
! ----------------------------------------------------------------------------------------------------------------------
! COPYRIGHT 2007 NATIONAL RENEWABLE ENERGY LABORATORY
!
! This routine models thermal storage and dispatch of the storage, solar field output and the powerplant. 
! It is based on the Excelergy submodel called ThermalStorage.
!
!  ThermalStorage determines how to dispatch the power plant
!
!     TsLogic(p).woSol        Start pb if storage at level without solar in op
!     TsLogic(p).wSol         Start pb if storage at level if solar in operation
!     TsLogic(p).load         Level to run plant at
!  Where
!     p                       TsData(D, h, 0).TouPeriod
!     DayData(D).PeakDay      Peak or off-peak day
!    
!
!     Qtts                    Thermal energy to thermal storage (MWh th)
!     Qfts                    Thermal energy from thermal storage(MWh th)
!     Qtpb                    Thermal energy to power block (MWh th)
!
! Doc. tables updated 7/1/2010 - MJW
!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Parameters
!    1| TsHours                          | Number of equivalent full-load hours of thermal storage           | hours            | hours            
!    2| NumTOU                           | Number of time-of-use periods                                     | none             | none             
!    3| E2TPLF0                          | Turbine part-load electric to thermal conversion (for fossil) - const| none             | none             
!    4| E2TPLF1                          | Turbine part-load electric to thermal conversion (for fossil) - linear| none             | none             
!    5| E2TPLF2                          | Turbine part-load electric to thermal conversion (for fossil) - quad.| none             | none             
!    6| E2TPLF3                          | Turbine part-load electric to thermal conversion (for fossil) - cubic| none             | none             
!    7| E2TPLF4                          | Turbine part-load electric to thermal conversion (for fossil) - quartic| none             | none             
! ...... Loop for 1..(Number of TOU periods) .......
!    8| TSLOGIC(:,1)                     | Dispatch logic without solar                                      | none             | none             
!    9| TSLOGIC(:,2)                     | Dispatch logic with solar                                         | none             | none             
!   10| TSLOGIC(:,3)                     | Dispatch logic - Turbine load fraction                            | none             | none             
! ......//

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Inputs
!    1| Qsf                              | Thermal energy available from the solar field                     | MWt              | MWt              
!    2| TOUPeriod                        | The time-of-use period                                            | none             | none             
!    3| TnkHL                            | Tank heat losses                                                  | MWt              | MWt              
!    4| PTSmax                           | Maximum power rate into the thermal storage                       | MWt              | MWt              
!    5| PFSmax                           | Maximum discharge rate of power from storage                      | MWt              | MWt              
!    6| PTTMAX                           | Maximum ratio of turbine operation                                | none             | none             
!    7| PTTMIN                           | Minimum turbine turn-down fraction                                | none             | none             
!    8| TurSUE                           | Equivalent full-load hours required for turbine startup           | hours            | hours            
!    9| Qdesign                          | Thermal input to the power block under design conditions          | MWt              | MWt              
!   10| HhtfPar                          | TES HTF pump parasitics                                           | MWe              | MWe              
!   11| HhtfParPF                        | Part-load TES HTF pump parasitics - multiplier                    | none             | none             
!   12| HhtfParF0                        | Part-load TES HTF pump parasitics - constant coef                 | none             | none             
!   13| HhtfParF1                        | Part-load TES HTF pump parasitics - linear coef                   | none             | none             
!   14| HhtfParF2                        | Part-load TES HTF pump parasitics - quadratic coef                | none             | none             
!   15| QhtfFreezeProt                   | HTF Freeze Protection Requirement (from 805)                      | MWt              | MWt              

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Outputs
!    1| Qtts                             | Energy to Thermal Storage                                         | MWt              | MWt              
!    2| Qfts                             | Energy from Thermal Storage                                       | MWt              | MWt              
!    3| Ets                              | Energy in Thermal Storage                                         | MWt.hr           | MWt.hr           
!    4| QTsHl                            | Energy losses from Thermal Storage                                | MWt              | MWt              
!    5| Qtpb                             | Energy to the Power Block                                         | MWt              | MWt              
!    6| QTsFull                          | Energy dumped because the thermal storage is full                 | MWt              | MWt              
!    7| Qmin                             | Energy dumped due to minimum load requirement                     | MWt              | MWt              
!    8| Qdump                            | The amount of energy dumped (more than turbine and storage)       | MWt              | MWt              
!    9| QTurSu                           | The energy needed to startup the turbine                          | MWt              | MWt              
!   10| PbStartF                         | Power block startup flag (1 = starting up, 0 = not starting up)   | none             | none             
!   11| HhtfLoad                         | Hot HTF pump load (energy from storage)                           | MWe              | MWe              
!   12| EparHhtf                         | Hot HTF pump parasitics                                           | MWe              | MWe              
!   13| PBMode                           | Power block mode (0 = off, 1 = startup, 2 = running)              | none             | none             
!   14| QhtfFpTES                        | Thermal energy storage freeze protection energy                   | MWt              | MWt              
!   15| QhtfFpHtr                        | Freeze protection provided by auxiliary heater                    | MWt              | MWt              


! SubPrograms
! ----------------------------------------------------------------------------------------------------------------------
! 
! None
!
!
! Required external librairies
! ----------------------------------------------------------------------------------------------------------------------
!
! None
!
!
! Revision history
! ---------------------------------------------------------------------------------------------------------------------
!
! This type was originally written by Nate Blair at NREL in June 2005, for TRNSYS 16. 
! It is based on the Excelergy program by Hank Price.
!
! The code was edited in July 2009 by Mike Wagner at NREL to modify the thermal storage dispatch 
! control scheme.  The modification was made to allow the code to determine dispatch based on 
! desired thermal storage level, instead of dispatching storage to depletion once the power cycle
! begins operation. 

!
! Use of the Storage array
! ----------------------------------------------------------------------------------------------------------------------
!
!PbMode0 = the powerblock mode from the previous timestep
!          = 0 means that the powerblock was completely turned off previously
!		  = 1 means that the powerblock was starting up in the previous timestep
!		  = 2 means that the powerblock was running in the previous timestep
!TurSuE0  turbine startup energy required per timestep
!  
!
! ----------------------------------------------------------------------------------------------------------------------
!

subroutine type806(time,xin,out,t,dtdt,par,info,iCntrl,*)

! Export this subroutine for its use in external DLLs
!dec$attributes dllexport :: type806

use TrnsysConstants ! Use TRNSYS global constants 
use TrnsysFunctions ! Use TRNSYS global functions 

implicit none   ! Force explicit declaration of all variables

! --- TRNSYS declarations ----------------------------------------------------------------------------------------------

integer, parameter :: nMaxTOU = 12
integer :: nI=15, nP=0, nD=0, nO=15, nS=0    ! Nb of inputs, parameters, derivatives, outputs, storage locations

real(8), intent(in)    :: time, xin(*), par(*), t(*)
real(8), intent(inout) :: dtdt(*)
real(8), intent(out)   :: out(*)
integer, intent(inout) :: info(15), iCntrl

integer :: PBmode, PBmode0,NumTOU,TOUPeriod,nstart,timesteps, p, PbStartF, nunits
real(8) :: Qtts, Qfts, Ets, ETs0, QTsHl, Qtpb, QTsFull,Qmin, Qdump, QTurSu,Qsf,QhtfFreezeProt
real(8) :: time0, tFinal, delt, TSnow
real(8) :: TSLogicin(12,3),TSLogic(12,3),TnkHL, TurSuE0, PTSMax, PFSMAX, TurSUE, qdesign, HhtfLoad, ESMax
real(8) :: PTTMAXin, PTTMINin, QTTMAX, QTTMIN
real(8) :: HhtfPar,HhtfParPF,HhtfParF0,HhtfParF1,HhtfParF2,EparHhtf
real(8) :: E2TPLF0, E2TPLF1, E2TPLF2, E2TPLF3, E2TPLF4
real(8) :: TsHours, QhtfFpTES , QhtfFpHtr , TStemp




! --- Messages for this Type -------------------------------------------------------------------------------------------
character(len=maxMessageLength) :: aString, msg(10)

msg(1) = "The current implementation of this Type does not allow multiple instances. Please use only one instance of "  &
         // "this Type in your input file."
msg(2) = "The number of Time of Use periods (par(2)) must be an integer between 1 and nMaxTOU."

! --- Initial call to detect the TRNSYS version for which this Type is written -----------------------------------------

if (info(7) .eq. -2) then

    info(12) = 16   ! This component is a TRNSYS 16 Type
    return 1

endif


! --- Second call in simulation: initialization call (not a simulation call) -------------------------------------------

if (info(7) .eq. -1) then

    ! Always Read parameters
    NumTOU = PAR(2)

    ! Read TRNSYS simulation parameters (they won't change)
    time0 = getSimulationStartTime()
    tFinal = getSimulationStopTime()
    delt = getSimulationTimeStep()


    ! nUnits is initialized to 0 the very first time this routine is called. It is then incremented here below for each 
    ! instance of the Type, to detect multiple instances (no OK currently)
    nUnits = nUnits+1
    ! If multiple units tell user this is not OK with current implementation
    if (nUnits > 2) then
        call Messages(-1,msg(1),'Fatal',info(1),info(2))
        return 1
    endif

    ! Check parameters
    if ( (NumTOU < 1) .or. (NumTOU > nMaxTOU) ) then
        write(aString,'("You have entered ",i," and the maximum value allowed is ",i)') NumTOU,nMaxTOU
        aString = trim(msg(2)) // trim(aString) 
        call Messages(-1,aString,'Fatal',info(1),info(2))
        return 1
    endif

    ! Number of parameters
    nP = 7+NumTOU*3

    info(9) = 1     ! Iterative controller, called at each iteration
    info(6) = nO   ! Set number of outputs

    ! Call the Typeck subroutine to compare what this component requires to what is supplied in the input file (also reserve outputs)
    call typeck(1,info,nI,nP,nD)

    ! Initialize outputs to 0.0
    out(1:nO) = 0.0

    return 1    ! Exit - End of the routine for very first call of the simulation

endif


! --- Very last call in simulation -------------------------------------------------------------------------------------

if (info(8) == -1) then

    return 1    ! Exit - This component does not need to do anything at the very last call
    
endif

! --- Post-convergence call --------------------------------------------------------------------------------------------
if (info(13) == 1) then

!NB NEED TO FIX THIS WITH THE NEW S-ARRAY METHOD SO CAN ALLOW MULTIPLE INSTANCES
    ! Store end-of-timestep temperatures for initial values at next time step
    !!! NOTE: This is only working because we know we will have only one instance of Type 806 !!!

    PbMode0 = PbMode
	ETs0 = Ets
    TurSuE0 = TurSuE0 ! seems like there is just the "prior" variable and not a current timestep one
    return 1    ! Exit - End of the routine for post-convergence calls
    
endif

! --- All simulation calls ---------------------------------------------------------------------------------------------

Qsf       = XIN(1)
TOUperiod = NINT(XIN(2)) 
TnkHL     = XIN(3)
PTSMax    = XIN(4)
PFSMAX    = XIN(5)
PTTMAXin    = XIN(6)
PTTMINin    = XIN(7)
TurSUE    = XIN(8)
Qdesign   = XIN(9)
!ESmax     = XIN(10)
HhtfPar	  = XIN(10)			! Hot HTF pump parasitics coefficient 1.1000
HhtfParPF = XIN(11)	        ! Hot HTF Pump parasitics coefficient 1.000
HhtfParF0 = XIN(12)         ! Hot HTF Pump parasitics coefficient	-0.036
HhtfParF1 = XIN(13)			! Hot HTF Pump parasitics coefficient	0.242
HhtfParF2 = XIN(14)			! Hot HTF Pump parasitics coefficient	0.794
QhtfFreezeProt = XIN(15)			! HTF Freeze Protection 

! Always Read parameters
TsHOURS      = PAR(1)       ! Hours of Thermal Storage
NUMTOU       = PAR(2)
E2TPLF0      = Par(3)       ! | Turbine Part Load Elec  to Thermal (for fossil backup)  | Dimensionless  | Dimensionless
E2TPLF1      = Par(4)       ! | Turbine Part Load Elec  to Thermal (for fossil backup)  | Dimensionless  | Dimensionless
E2TPLF2      = Par(5)       ! | Turbine Part Load Elec  to Thermal (for fossil backup)  | Dimensionless  | Dimensionless
E2TPLF3      = Par(6)       ! | Turbine Part Load Elec  to Thermal (for fossil backup)  | Dimensionless  | Dimensionless
E2TPLF4      = Par(7)       ! | Turbine Part Load Elec  to Thermal (for fossil backup)  | Dimensionless  | Dimensionless

ESMAX = TSHOURS*QDESIGN     !10-4-06 ESMAX now calculated and not an input

nstart = 7
do p = 1, NUMTOU
    TSLogicin(p,1) = PAR(nStart+3*p-2) ! read in the with Sol level for each TOU period
    TSLogicin(p,2) = PAR(nStart+3*p-1) ! read in the without Sol level for each TOU period
    TSlogicin(p,3) = PAR(nstart+3*p)   ! read in the  load level for each TOU period
    TSLogic(p,1) = TSLogicin(p,1) * ESmax
    TSLogic(p,2) = TSLogicin(p,2) * ESmax
    TSLogic(p,3) = Qdesign * (E2TPLF0 + E2TPLF1 * TsLogicin(p,3) + E2TPLF2 * TsLogicin(p,3)**2 + E2TPLF3 * TsLogicin(p,3)**3 + E2TPLF4 * TsLogicin(p,3)**4)
! Check to make sure that dispatch logic operates within the turbine max and min
    If (TSlogicin(p,3).GT.PTTmaxin) TsLogic(p,3) = Qdesign * (E2TPLF0 + E2TPLF1 * PTTmaxin + E2TPLF2 * PTTmaxin**2 + E2TPLF3 * PTTmaxin**3 + E2TPLF4 * PTTmaxin**4)
    If (TSlogicin(p,3).LT.PTTminin) TsLogic(p,3) = Qdesign * (E2TPLF0 + E2TPLF1 * PTTminin + E2TPLF2 * PTTminin**2 + E2TPLF3 * PTTminin**3 + E2TPLF4 * PTTminin**4)
enddo

!Set QTTMAX to be the actual thermal max energy possible to the turbine
QTTMAX = Qdesign * (E2TPLF0 + E2TPLF1 * PTTmaxin + E2TPLF2 * PTTmaxin**2 + E2TPLF3 * PTTmaxin**3 + E2TPLF4 * PTTmaxin**4)
QTTMIN = Qdesign * (E2TPLF0 + E2TPLF1 * PTTminin + E2TPLF2 * PTTminin**2 + E2TPLF3 * PTTminin**3 + E2TPLF4 * PTTminin**4)




!Delt is decimal fraction of hour to produce 1, 2 or 4 timesteps/hr
!NB TimeSteps = 1.0 / Delt
TimeSteps = NINT(1.0 / Delt)
! initialize outputs to 0
Qtts      = 0     !| Energy to Thermal Storage                                      |      MW        |     MW
Qfts      = 0     !| Energy from Thermal Storage                                    |      MW        |     MW
Ets       = 0     !| Energy in Thermal Storage
QTsHl     = 0     !| Energy losses from Thermal Storage
Qtpb      = 0     !| Energy to the Power Block
QTsFull   = 0     !| Energy dumped because the thermal storage is full
Qmin      = 0     !| Indicator of being below minimum operation level
Qdump     = 0     !| The amount of energy dumped (more than turbine and storage)
QTurSu    = 0     !| The energy needed to startup the turbine
PbStartF  = 0     !| is 1 during the period when powerblock starts up otherwise 0
HhtfLoad  = 0     !| Hot HTF pump load (energy from storage)                        | Fraction between 0 and 1
EparHhtf  = 0     !| Hot HTF pump parasitics										|      MWhe 
QhtfFpTES = 0     !| HTF Freeze Protection from Thermal Eneryg Storage				|      MWht HP 12-12-06
QhtfFpHtr = 0	  !| HTF Freeze Protection from Auxiliary Heater					|      MWht 


!   Select Case TsHours  ' Evaluate TsHours - TS dispatch strategy if storage is present
IF ( TsHours.LE.0) Then ! No Storage
                 
      If ((PbMode0.eq.0).OR.(PbMode0.EQ.1)) Then ! if plant is not already operating in last timestep
        
         If (Qsf.GT.0) Then
            If (Qsf.GT.(TurSuE0 * TimeSteps)) Then !  Starts plant as exceeds startup energy needed
               Qtpb = Qsf - TurSuE0 * TimeSteps
               QTurSu = TurSuE0 * TimeSteps
               PbMode = 2
               PbStartF = 1
            Else !  Plant starting up but not enough energy to make it run - will probably finish in the next timestep
               Qtpb = 0
               TurSuE0 = TurSuE0 - Qsf / TimeSteps
               QTurSu = Qsf
               PbMode = 1
               PbStartF = 0
            End If
            
         Else ! No solar field output so still need same amount of energy as before and nothing changes
            TurSuE0 = TurSuE * Qdesign
            PbMode = 0
            PbStartF = 0
         End If
      
      Else ! if the powerblock mode is already 2 (running previous timestep)
      
         If (Qsf.GT.0) Then     ! Plant operated last hour and this one
            Qtpb = Qsf          ! all power goes from solar field to the powerblock
            PbMode = 2          ! powerblock continuing to operate
            PbStartF = 0        ! powerblock did not start during this timestep
         Else                   !  Plant operated last hour but not this one
            Qtpb = 0            ! No energy to the powerblock
            PbMode = 0          ! turned off powrblock
            PbStartF = 0        ! it didn't start this timeperiod 
            TurSuE0 = TurSuE0 - Qsf / TimeSteps ! Qsf is 0 so this statement is confusing
         End If
   
      End If
      
 ! following happens no matter what state the powerblock was in previously      
      HhtfLoad = 0
! This happens after convergence      PbMode0 = PbMode  ! set the value for the next period 
      
      
      If (Qtpb.LT.QTTMIN) Then ! Energy to powerblock less than the minimum that the turbine can run at
          Qmin =  Qtpb         ! The minimum energy (less than the minimum)
          Qtpb = 0             ! Energy to PB is now 0
          PbMode = 0           ! PB turned off
      End If
      
      If (Qtpb.GT.QTTMAX) Then   ! Energy to powerblock greater than what the PB can handle (max)
          Qdump =  Qtpb - QTTMAX ! The energy dumped 
          Qtpb = QTTMAX          ! the energy to the PB is exactly the maximum
      End If
     
	 QhtfFpHtr = QhtfFreezeProt
      
!      Case 1   ' Solergy Dispatch Approach
ELSEIF (TsHours.GT.0)  THEN    
	   p = TouPeriod

 ! initialize a bunch of values
		  QTurSu = 0
		  PbStartF = 0
		  QTsHl = TnkHL ! thermal storage heat losses are equal to the tank losses
		  Qdump = 0
		  Qfts = 0						! HP Added 11-26-06
		  QhtfFpTES = QhtfFreezeProt	! HP Added 12-12-06
      
		  !**********************************************************
		  !******        plant is not already operating         *****
		  !**********************************************************
      
		  If (PbMode0.EQ.0) Then        ! if plant is not already operating nor starting up

!          If  ((((Qsf.GT.0).AND.(ETs0.GE.TsLogic(p,1)).AND.((Qsf+ETs0).GE.TsLogic(p,3))).OR. &
!               ((Qsf.EQ.0).AND.(ETs0.GE.TsLogic(p,2)).AND.(ETs0.GE.TsLogic(p,3))).OR. &
!                (Qsf.GT.PTSmax))) Then
              
          !mjw 1.13.2011
          !If  ((((Qsf.GT.0).AND.(ETs0.GE.TsLogic(p,1)).AND.((Qsf+ETs0).GE.QTTMin)).OR. &
          !     ((Qsf.EQ.0).AND.(ETs0.GE.TsLogic(p,2)).AND.(ETs0.GE.QTTMin)).OR. &
          !      (Qsf.GT.PTSmax))) Then
          
          !mjw 5.8.12 Calculate the startup energy requirement 
          QTurSu = TurSuE * Qdesign * TimeSteps
          !mjw 5.8.12 Account for the startup energy
          !Start the plant if:
          ! -> Solar resource is greater than zero AND total available energy exceeds startup + minimum operation requirement
          ! -> Solar resource is not available AND energy in storage exceeds startup + minimum operation requirement
          ! -> Solar resource exceeds the maximum charge rate and the plant would have to dump energy
          ! Note: this revision gets rid of the requirement that storage energy be above the minimum dispatch fraction in order to start up.
          If  ( &
               ( (Qsf.GT.0).AND.( (Qsf+ETs0-TsLogic(p,1)).GE.(QTTMin+QTurSu) ) ).OR. &
               ( (Qsf.EQ.0).AND.( (ETs0-TsLogic(p,2)).GE.(QTTMin+QTurSu) ) ).OR. &
               (Qsf.GT.PTSmax) ) Then


                !'  Starts plant if any condition is met
                            
				! Assumes Operator started plant during previous time period
				! But TRNSYS cannot do this, so start-up energy is deducted during current timestep.     
				     

				PbMode = 1								! HP Added 11-26-06
				QTurSu = TurSuE * Qdesign * TimeSteps	! HP Added 11-26-06
            
				!Qtpb = TsLogic(p,3) ! set the energy to powerblock equal to the load for this TOU period
				!mjw 5.8.12 Don't automatically set to the dispatch level.. limit by what's available!
				if(Qsf>0.) then
				    Qtpb = dmin1(TsLogic(p,3), Qsf+ETs0-TsLogic(p,1)-QTurSu)
				else
				    Qtpb = dmin1(TsLogic(p,3), Qsf+ETs0-TsLogic(p,2)-QTurSu)
				endif
           
				If (Qsf.GT.Qtpb)  Then ! if solar field output is greater than what the necessary load ?
				   Qtts = Qsf - Qtpb ! the extra goes to thermal storage
               
				   If (Qtts.Gt.PTSmax) Then ! if q to thermal storage exceeds thermal storage max rate Added 9-10-02
					  Qdump = Qtts - PTSmax  ! then dump the excess for this period Added 9-10-02
					  Qtts = PTSmax                   
				   End If                              
				   
!				   Qfts = 0 ! the energy from thermal storage is 0
				   Qfts = QTurSu  ! HP 12-07-06

				Else ! q from solar field not greater than needed by the powerblock
				    Qtts = 0
!				    Qfts = (1 -  Qsf /  Qtpb) * PFSmax ! Added 9-10-02 energy from thermal storage cannot exceed max rate out
! HP 12-07-06	    Qfts = Qfts + (1 -  Qsf /  Qtpb) * PFSmax ! HP Added 11-26-06
				    Qfts = QTurSu + (1 -  Qsf /  Qtpb) * PFSmax ! HP Added 11-26-06
		!		    Qtpb = Qfts + Qsf                ! q to PB sum of thermal storage and solar field Added 9-10-02
                    If (Qfts.GT.PFSmax) Qfts = PFSmax !' Added 1-26-08 ***********
				    Qtpb = Qsf + (1 -  Qsf /  Qtpb) * PFSmax ! HP Added 11-26-06
				End If
            
! HP 12-07-06	Ets = ETs0 + (Qsf - Qtpb) / TimeSteps ! thermal storage energy is initial + what was left 
				Ets = ETs0 - QTurSu + (Qsf - Qtpb) / TimeSteps ! HP Added 12-07-06
				PbMode = 2   ! powerblock is now running
				PbStartF = 1 ! the powerblock turns on during this timeperiod.
            
			 Else !Store energy not enough stored to start plant

				Qtts = Qsf ! everything goes to thermal storage
				Qfts = 0   ! nothing from thermal storage
				Ets = ETs0 + Qtts / TimeSteps  
				Qtpb = 0

			 End If
      
		  !**********************************************************
		  !******        plant is already operating             *****
		  !**********************************************************
      
		  Else        ! Power block operated last period or was starting up
		     !MJW 7/09 Determine the current fractional thermal storage dispatch control
			 if(Qsf.gt.0) then 
			    TStemp = TSLogicin(p,1)
			 else
			    TStemp = TSLogicin(p,2)
			 endif
			    
			 If ((Qsf + max(0.,ETs0-ESmax*TStemp) * TimeSteps).GT.TsLogic(p,3)) Then
         
			 ! If there is sufficient energy to operate at dispatch target output
         
				Qtpb = TsLogic(p,3)
            
				If (Qsf.GT.Qtpb) Then 
				   Qtts = Qsf - Qtpb !extra from what is needed put in thermal storage
               
				! Added 9-10-02
				   If (Qtts.GT.PTSmax) Then  !check if max power rate to storage exceeded            ' Added 9-10-02
					  Qdump = Qtts - PTSmax ! if so, dump extra         ' Added 9-10-02
					  Qtts = PTSmax                  !Added 9-10-02
				   End If                            !Added 9-10-02
               
				   Qfts = 0
            
				Else ! solar field outptu less than what powerblock needs
				   Qtts = 0
				   Qfts = (1 - Qsf / Qtpb) * PFSmax !Added 9-10-02
                   IF (Qfts.gt.PFSMax) Qfts = PFSMax ! Added 1-26-08
				   Qtpb = Qfts + Qsf                ! Added 9-10-02
               
				End If
				
				Ets = ETs0 + (Qsf - Qtpb - Qdump) / TimeSteps ! energy of thermal storage is the extra
            
				! Check to see if throwing away energy HP010701
				If ((Ets.GT.ESmax).AND.(Qtpb.LT.QTTMAX)) Then ! QTTMAX (MWt) - power to turbine max
				   If (((Ets - ESmax) * TimeSteps).LT.(QTTMAX - Qtpb)) Then
					  Qtpb = Qtpb + (Ets - ESmax) * TimeSteps
					  Ets = ESmax
				   Else
					  Ets = Ets - (QTTMAX - Qtpb) / TimeSteps  ! should this be Ets0 instead of Ets on RHS ??
					  Qtpb = QTTMAX
				   End If
				   Qtts = Qsf - Qtpb
                                      
				End If
            
            
			 Else  !Empties TS to dispatch level if above min load level
			 
				If ((Qsf + max(0.,ETs0-ESmax*TStemp) / TimeSteps).GT.QTTMIN) Then  !Modified 7/2009 by MJW
				   Qfts = max(0.,ETs0-ESmax*TStemp)/TimeSteps
				   Qtpb = Qsf + Qfts
				   Qtts = 0
				   Ets = ETs0 - Qfts
				Else
				   Qtpb = 0
				   Qfts = 0
				   Qtts = Qsf
				   Ets = ETs0 + Qtts / TimeSteps
				End If
            
			 End If
    
		  End If
         
		  If (Qtpb.GT.0) Then
			 PbMode = 2
		  Else
			 PbMode = 0
		  End If
      
		  Ets = Ets - (QTsHl + QhtfFpTES)/ TimeSteps ! should this be Ets or ETS0 on the RHS ?
      
		  If (Ets.GT.ESmax) Then ! trying to put in more than storage can handle
			 QTsFull = (Ets - ESmax) * TimeSteps  !this is the amount dumped when storage is completely full
			 Ets = ESmax
			 Qtts = Qtts - QTsFull
         
		  Else
			 QTsFull = 0 ! nothing is dumped if not overfilled
		  End If
      
		  ! Check min and max on turbine
		  If (Qtpb.LT.QTTMIN) Then
			 Qmin = Qtpb
			 Qtpb = 0
			 PbMode = 0
		  Else
			 Qmin = 0
		  End If
      
      
		  HhtfLoad = Qfts / Qdesign
		  PbMode0 = PbMode
!DONE AFTER CONVERGENCE		  ETs0 = .Ets
      
               
ELSE   ! No Storage  NOT SURE WHY THIS IS HERE. IT SHOULD CRASH if the user enters a number other than 0 and 1
      
                     Qtts = 0
                     Qfts = 0
                     Ets = 0
                     Qtpb = Qsf

Endif ! end case on dispatch method





! Thermal Storage Parasitics (Hot Pump) TAKEN FROM PARASITICS SECTION OF EXCELERGY CODE
      
!      If HBypassF = 0 Then          '  Hot pump bypass - when 1 the hot pump is bypassed when the solar field is in operation.

!EparHhtf = HhtfPar * HhtfParPF * (HhtfParF0 + HhtfParF1 * HhtfLoad + HhtfParF2 * (HhtfLoad**2))
EparHhtf = HhtfPar  * (HhtfParF0 + HhtfParF1 * HhtfLoad + HhtfParF2 * (HhtfLoad**2))		!HP Changed 12-12-06 (SAM input accounts for PF)

if (EparHhtf.LT.0.0) Then			!HP Added 12-11-06
	EparHhtf=0
Endif

!HhtfPar	1.1000
!HhtfParPF	1.000
!HhtfParF0	-0.036
!HhtfParF1	0.242
!HhtfParF2	0.794
!      Else                              '  Hot HTF Pumps (HTF from TS to PB)
!       .EparHhtf = 0
!      End If


if (Ets .lt. 0) then
!	out(2) = 0  NB Changed on 6-11-09 because it should just be setting ets to 0 instead of negative
	out(3) = 0
end if

! set outputs
out(1)  = Qtts          !| Energy to Thermal Storage                                      |      MW        |     MW
out(2)  = Qfts          !| Energy from Thermal Storage                                    |      MW        |     MW
out(3)  = Ets           !| Energy in Thermal Storage
out(4)  = QTsHl         !| Energy losses from Thermal Storage
out(5)  = Qtpb          !| Energy to the Power Block
out(6)  = QTsFull       !| Energy dumped because the thermal storage is full
out(7)  = Qmin          !| Indicator of being below minimum operation level
out(8)  = Qdump         !| The amount of energy dumped (more than turbine and storage)
out(9)  = QTurSu        !| The energy needed to startup the turbine
out(10) = PbStartF      !| is 1 during the period when powerblock starts up otherwise 0
out(11) = HhtfLoad      !| Hot HTF pump load (energy from storage)                         | MW?
out(12) = EparHhtf      !| Hot HTF pump parasitics

out(13) = PBMode
out(14) = QhtfFpTES      ! MWht   
out(15) = QhtfFpHtr       ! MWht  !


return 1

end subroutine Type806

