! TRNSYS Type 807: Powerplant, Fossil Backup and Powerplant Parasitics from Excelergy
! ----------------------------------------------------------------------------------------------------------------------
!  COPYRIGHT 2007 NATIONAL RENEWABLE ENERGY LABORATORY
!
! Based on inputs from the dispatch and storage model (Type 806) this model calculates the powerplant output
! and adjusts for temperature corrections. 
!

! Doc. tables updated 7/1/2010 - MJW
!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Parameters
!    1| Qdesign                          | Design Turbine Thermal Input (MWt)                                | MWt              | MWt              
!    2| Edesign                          | Design Turbine Gross Output (MWe)                                 | MWe              | MWe              
!    3| T2EPLF0                          | Turbine Part Load Therm to Elec                                   | none             | none             
!    4| T2EPLF1                          | Turbine Part Load Therm to Elec                                   | none             | none             
!    5| T2EPLF2                          | Turbine Part Load Therm to Elec                                   | none             | none             
!    6| T2EPLF3                          | Turbine Part Load Therm to Elec                                   | none             | none             
!    7| T2EPLF4                          | Turbine Part Load Therm to Elec                                   | none             | none             
!    8| E2TPLF0                          | Turbine Part Load Elec  to Thermal (for fossil backup)            | none             | none             
!    9| E2TPLF1                          | Turbine Part Load Elec  to Thermal (for fossil backup)            | none             | none             
!   10| E2TPLF2                          | Turbine Part Load Elec  to Thermal (for fossil backup)            | none             | none             
!   11| E2TPLF3                          | Turbine Part Load Elec  to Thermal (for fossil backup)            | none             | none             
!   12| E2TPLF4                          | Turbine Part Load Elec  to Thermal (for fossil backup)            | none             | none             
!   13| TempCorrF                        | Temperature Correction Mode (1=wetbulb 2=drybulb basis)           | none             | none             
!   14| TempCorr0                        | Temperature Correction Coefficient 0                              | none             | none             
!   15| TempCorr1                        | Temperature Correction Coefficient 1                              | none             | none             
!   16| TempCorr2                        | Temperature Correction Coefficient 2                              | none             | none             
!   17| TempCorr3                        | Temperature Correction Coefficient 3                              | none             | none             
!   18| TempCorr4                        | Temperature Correction Coefficient 4                              | none             | none             
!   19| TurTesEffAdj                     | Turbine TES Adjustment - Efficiency                               | none             | none             
!   20| TurTesOutAdj                     | Turbine TES Adjustment - Gross Output                             | none             | none             
!   21| MinGrOut                         | Minimum gross electrical output from powerplant                   | none             | none             
!   22| MaxGrOut                         | Maximum gross electrical output from powerplant                   | none             | none             
!   23| NUMTOU                           | Number of time of use periods                                     | none             | none             
! ...... Loop for 1..(Number of TOU periods) .......
!   24| FossilFill(:)                    | Fossil dispatch fraction control                                  |                  |                  
! ......//
!   25| PbFixPar                         | Fixed Power Block Parasitics                                      | MW               | MW               
!   26| BOPPar                           | Balance of Plant Parasitics                                       | MWe              | MWe              
!   27| BOPParPF                         | Balance of Plant Parasitics - multiplier                          | none             | none             
!   28| BOPParF0                         | Balance of Plant Parasitics - constant                            | none             | none             
!   29| BOPParF1                         | Balance of Plant Parasitics - linear term                         | none             | none             
!   30| BOPParF2                         | Balance of Plant Parasitics - quadratic term                      | none             | none             
!   31| CtPar                            | Cooling Tower Parasitics                                          | MWe              | MWe              
!   32| CtParPF                          | Cooling Tower Parasitics - multiplier                             | none             | none             
!   33| CtParF0                          | Cooling Tower Parasitics - constant                               | none             | none             
!   34| CtParF1                          | Cooling Tower Parasitics - linear term                            | none             | none             
!   35| CtParF2                          | Cooling Tower Parasitics - quadratic term                         | none             | none             
!   36| HtrPar                           | Auxiliary heater/boiler operation parasitics                      | MWe              | MWe              
!   37| HtrParPF                         | Auxiliary heater/boiler operation parasitics - multiplier         | none             | none             
!   38| HtrParF0                         | Auxiliary heater/boiler operation parasitics - constant           | none             | none             
!   39| HtrParF1                         | Auxiliary heater/boiler operation parasitics - linear term        | none             | none             
!   40| HtrParF2                         | Auxiliary heater/boiler operation parasitics - quadratic term     | none             | none             
!   41| LHVBoilEff                       | Lower Heating Value Boiler Efficiency                             | none             | none             

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Inputs
!    1| Qtpb                             | Heat to Power Block (output from TS/Dispatch type)                | MWt              | MWt              
!    2| Qfts                             | Heat from Thermal Storage                                         | MWt              | MWt              
!    3| Twetbulb                         | Wet Bulb Temperature                                              | C                | C                
!    4| Tdrybulb                         | Ambient Temperature (dry bulb)                                    | C                | C                
!    5| CtOpF                            | CT Operation Flag (0 = CT par. a function of load, 1 = CT at full/half)| none             | none             
!    6| SFTotPar                         | Solar Field Parasitics (EparSF + EparCHTF + EparAnti)             | MWe              | MWe              
!    7| EparHhtf                         | Thermal Storage Parasitics                                        | MWe              | MWe              
!    8| TOUPeriod                        | Current Time of Use Period                                        | none             | none             

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Outputs
!    1| Enet                             | Net electricity produced, after parasitic loss                    | MWe              | MWe              
!    2| EgrSol                           | Gross electric production from the solar resource                 | MWe              | MWe              
!    3| EMin                             | Solar Electric Generation below minimum required output           | MWe              | MWe              
!    4| Edump                            | Solar Electric Generation that is in excess of powerplant max     | MWe              | MWe              
!    5| Pbload                           | Fraction of current Powerblock output to design output            | none             | none             
!    6| EgrFos                           | Gross electric production from the fossil resource                | MWe              | MWe              
!    7| Egr                              | Gross electricity produced, before parasitic loss                 | MWe              | MWe              
!    8| Qgas                             | Gas Thermal Energy Input                                          | MW               | MW               
!    9| HtrLoad                          | Auxiliary heater load as ratio vs. rated output                   | none             | none             
!   10| Epar                             | Total Parasitics for entire system                                | MWe              | MWe              
!   11| EparPB                           | Fixed Power Block Parasitics                                      | MWe              | MWe              
!   12| EparBOP                          | Balance of Plant Parasitics                                       | MWe              | MWe              
!   13| EparCT                           | Cooling Tower Parasitic Load                                      | MWe              | MWe              
!   14| EparHtr                          | Auxiliary heater parasitic load                                   | MWe              | MWe              
!   15| EparOffLine                      | Parasitics incurred while plant is not producing electricity      | MWe              | MWe              
!   16| EparOnLine                       | Parasitics incurred while plant is producing electricity          | MWe              | MWe              


!
!
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
!

!
! Use of the Storage array
! ----------------------------------------------------------------------------------------------------------------------
!
!
! ----------------------------------------------------------------------------------------------------------------------
!

subroutine TYPE807(time,xin,out,t,dtdt,par,info,iCntrl,*)

! Export this subroutine for its use in external DLLs
!dec$attributes dllexport :: type807

use TrnsysConstants ! Use TRNSYS global constants 
use TrnsysFunctions ! Use TRNSYS global functions 

implicit none   ! Force explicit declaration of all variables

! --- TRNSYS declarations ----------------------------------------------------------------------------------------------

integer, parameter :: nMaxTOU = 12
integer :: nI=8, nP=0, nD=0, nO=16, nS=0    ! Nb of inputs, parameters, derivatives, outputs, storage locations

real(8), intent(in)    :: time, xin(*), par(*), t(*)
real(8), intent(inout) :: dtdt(*)
real(8), intent(out)   :: out(*)
integer, intent(inout) :: info(15), iCntrl

integer :: TempCorrF,CtOpF,NumTOU,p,nUnits, TOUPeriod
real(8) :: Qtpb,Qfts,Twetbulb,Tdrybulb,SFtotPar,EparHhtf
real(8) :: Qdesign, Edesign,T2EPLF0,T2EPLF1,T2EPLF2,T2EPLF3,T2EPLF4,E2TPLF0,E2TPLF1,E2TPLF2,E2TPLF3,E2TPLF4
real(8) :: TempCorr0,TempCorr1,TempCorr2,TempCorr3,TempCorr4,TurTesEffAdj,TurTesOutAdj,MinGrOut,MaxGrOut
real(8) :: FossilFill(12)
real(8) :: PbFixPar,BOPPar,BOPParPF,BOPParF0,BOPParF1,BOPParF2 
real(8) :: CtPar,CtParPF,CtParF0,CtParF1,CtParF2,HtrPar,HtrParPF,HtrParF0,HtrParF1,HtrParF2, LHVBoilEff
real(8) :: time0, tFinal, delt, TSnow

Real(8) :: Enet,EgrSol,Emin,Edump,Pbload,EgrFos,Egr,Qgas,HtrLoad,Epar,EparPB,EparBOP,EparCT,EparHtr,EparOffline,EparOnline

real(8) :: Nth,Nel,TTC,AmbTemp,NTC,GN

! --- Messages for this Type -------------------------------------------------------------------------------------------
character(len=maxMessageLength) :: aString, msg(10)

msg(1) = "The current implementation of this Type does not allow multiple instances. Please use only one instance of "  &
         // "this Type in your input file."
msg(2) = "The number of Fossfill values in the field (par(1)) must be an integer between 1 and nMaxTOU."

! --- Initial call to detect the TRNSYS version for which this Type is written -----------------------------------------

if (info(7) .eq. -2) then

    info(12) = 16   ! This component is a TRNSYS 16 Type
    return 1

endif


! --- Second call in simulation: initialization call (not a simulation call) -------------------------------------------

if (info(7) .eq. -1) then

    ! Always Read parameters
    NUMTOU = PAR(23)

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


    ! Number of Parameters (before FossilFill + FossilFill (numTOU) + pars after Fossilfill)
    nP = 23+NumTOU+17

    info(9) = 1     ! Iterative controller, called at each iteration
    info(6) = nO   ! Set number of outputs

    ! Call the Typeck subroutine to compare what this component requires to what is supplied in the input file (also reserve outputs)
    call typeck(1,info,nI,nP,nD)

    ! Initialize important temperature values
    ! 391C is design outlet loop temperature
    ! 293C is design inlet loop temperature

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

    return 1    ! Exit - End of the routine for post-convergence calls
    
endif

! --- All simulation calls ---------------------------------------------------------------------------------------------


! Read all Inputs 
Qtpb = XIN(1)           !              | Heat to Power Block (output from TS/Dispatch type)      | MW             | MW 
Qfts = XIN(2)           !              | Heat from Thermal Storage                               | MWt            | MWt
Twetbulb = XIN(3)       !              | Wet Bulb Temperature                                    | C              | C
Tdrybulb = XIN(4)       !              | Ambient Temperature (dry bulb)                          | C              | C
CtOpF  = XIN(5)         !              | CT Operation Flag
                        !                      0 = CT parasitics a function of load
                        !                      1 = CT at 50% or 100%
SFTotPar = XIN(6)       !              | Solar Field Parasitics (EparSF + EparCHTF + EparAnti)   | MW             | MW
EparHhtf = XIN(7)       !              | Thermal Storage Parasitics                              | MW             | MW
TOUPeriod = NINT(XIN(8))      !              | Current Time of Use Period

!
! Read in all Parameters
Qdesign      = Par(1)       !         | Design Turbine Thermal Input (MWt)
Edesign      = Par(2)       !         | Design Turbine Gross Output (MWe)
T2EPLF0      = Par(3)       !         | Turbine Part Load Therm to Elec                         | Dimensionless  | Dimensionless
T2EPLF1      = Par(4)       !         | Turbine Part Load Therm to Elec                         | Dimensionless  | Dimensionless
T2EPLF2      = Par(5)       !         | Turbine Part Load Therm to Elec                         | Dimensionless  | Dimensionless
T2EPLF3      = Par(6)       !         | Turbine Part Load Therm to Elec                         | Dimensionless  | Dimensionless
T2EPLF4      = Par(7)       !         | Turbine Part Load Therm to Elec                         | Dimensionless  | Dimensionless
E2TPLF0      = Par(8)       !         | Turbine Part Load Elec  to Thermal (for fossil backup)  | Dimensionless  | Dimensionless
E2TPLF1      = Par(9)       !         | Turbine Part Load Elec  to Thermal (for fossil backup)  | Dimensionless  | Dimensionless
E2TPLF2      = Par(10)      !         | Turbine Part Load Elec  to Thermal (for fossil backup)  | Dimensionless  | Dimensionless
E2TPLF3      = Par(11)      !         | Turbine Part Load Elec  to Thermal (for fossil backup)  | Dimensionless  | Dimensionless
E2TPLF4      = Par(12)      !         | Turbine Part Load Elec  to Thermal (for fossil backup)  | Dimensionless  | Dimensionless
TempCorrF    = NINT(Par(13))!         | Temperature Correction Mode (1=wetbulb 2=drybulb basis) | Dimensionless  | Dimensionless
TempCorr0    = Par(14)      !         | Temperature Correction Coefficient 0                    | Dimensionless  | Dimensionless
TempCorr1    = Par(15)      !         | Temperature Correction Coefficient 1                    | Dimensionless  | Dimensionless
TempCorr2    = Par(16)      !         | Temperature Correction Coefficient 2                    | Dimensionless  | Dimensionless
TempCorr3    = Par(17)      !         | Temperature Correction Coefficient 3                    | Dimensionless  | Dimensionless
TempCorr4    = Par(18)      !         | Temperature Correction Coefficient 4                    | Dimensionless  | Dimensionless
TurTesEffAdj = Par(19)      !         | Turbine TES Adjustment - Efficiency                     | Dimensionless  | Dimensionless
TurTesOutAdj = Par(20)      !         | Turbine TES Adjustment - Gross Output                   | Dimensionless  | Dimensionless
MinGrOut     = Par(21)      !         | Minimum gross electrical output from powerplant         | Fraction of Edesign | Fraction
MaxGrOut     = Par(22)      !         | Maximum gross electrical output from powerplant         | Fraction of Edesign | Fraction
NUMTOU       = NINT(Par(23))!         | Number of time of use periods 
Do p = 1,NUMTOU
 FossilFill(p) = Par(23+p)   !| Fossil Operation: This fills gas use to this fraction for every hour of month
enddo

PbFixPar   = Par(23+numtou+1)  !           | Fixed Power Block Parasitics (24 hr)                    | MW
BOPPar     = Par(23+numtou+2)  !            | Balance of Plant Parasitics ???
BOPParPF   = Par(23+numtou+3)  !            | Balance of Plant Parasitics ???
BOPParF0   = Par(23+numtou+4)  !            | Balance of Plant Parasitics ???
BOPParF1   = Par(23+numtou+5)  !            | Balance of Plant Parasitics ???
BOPParF2   = Par(23+numtou+6)  !            | Balance of Plant Parasitics ???
CtPar      = Par(23+numtou+7)  !            | Cooling Tower Parasitics Coeff ????
CtParPF    = Par(23+numtou+8)  !            | Cooling Tower Parasitics Coeff ????  
CtParF0    = Par(23+numtou+9)  !            | Cooling Tower Parasitics Coeff ????
CtParF1    = Par(23+numtou+10) !           | Cooling Tower Parasitics Coeff ????
CtParF2    = Par(23+numtou+11) !           | Cooling Tower Parasitics Coeff ????
HtrPar     = Par(23+numtou+12) !           | Heater/Boiler Parasitics
HtrParPF   = Par(23+numtou+13) !           | Heater/Boiler Parasitics
HtrParF0   = Par(23+numtou+14) !           | Heater/Boiler Parasitics
HtrParF1   = Par(23+numtou+15) !           | Heater/Boiler Parasitics
HtrParF2   = Par(23+numtou+16) !           | Heater/Boiler Parasitics
LHVBoilEff = Par(23+numtou+17) !           | Lower Heating Value Boiler Efficiency

!  Power Plant Calculation
   
! Design Point Electric Generation (10-1-03)
! EgrSol in MWe gross generation
! Nth - Normalized thermal to power cycle
! Nel - Normalized electric output

  Nth = Qtpb / Qdesign
  Nel = T2EPLF0 + T2EPLF1 * Nth + T2EPLF2 * (Nth**2) + T2EPLF3 * (Nth**3) + T2EPLF4 * (Nth**4)
  EgrSol = Edesign * Nel
      
! Electric Generation Temperature Correction (10-1-03)
! If TempCorrF = 1 then
!    temperature correction is based on the Wet Bulb Temp (make sure Twetbulb is in data set
! If TempCorrF = 2 then
!    temperature correction is based on the Dry Bulb Temp
! Else No Correction

  If ((TempCorrF.eq.1).OR.(TempCorrF.EQ.2)) Then
     If (TempCorrF.EQ.1) Then
        Ttc = Twetbulb
     Else
        Ttc = Tdrybulb
     End If
     Ntc = TempCorr0 + TempCorr1 * Ttc + TempCorr2 * (Ttc**2) + TempCorr3 * (Ttc**3) + TempCorr4 * (Ttc**4)
  Else
     Ttc = 0
     Ntc = 1
  End If
       
  EgrSol = EgrSol * Ntc
      
!  Correction for TES  10-1-03
  If (Qtpb.GT.0) Then
    EgrSol = EgrSol * ((1 - Qfts / Qtpb) + Qfts / Qtpb * TurTesEffAdj)
  End If
      
  Emin = 0.0
  Edump = 0.0
      
  If (EgrSol.LT.(Edesign * MinGrOut)) Then  ! if the solar provided is less than the minimum needed to run the turbine
     If (EgrSol.GT.0.0) Then
        Emin = EgrSol            ! then set the emin equal to the solar provided
     End If
     EgrSol = 0.0
  Else
     If (EgrSol.GT.(Edesign * MaxGrOut)) Then ! if the solar provided is greater= than the maximum used by the turbine
        Edump =  EgrSol - (Edesign * MaxGrOut) ! then dump the extra 
        EgrSol = Edesign * MaxGrOut
     End If
  End If
  
  PbLoad = EgrSol / Edesign ! what is the fraction of the solar output compared to the design point 
   
! FOSSIL BACKUP SECTION

  If (EgrSol.LT.(FossilFill(TOUPeriod) * Edesign)) Then ! if the solar provided is less than the fraction of fossil required.
      EgrFos = Edesign * FossilFill(TOUPeriod) - EgrSol ! then the fossil used is the maximum amount minus the solar provided ???
     !If (FossilFill(TOUPeriod).EQ.0) Then
     if (FossilFill(TOUPeriod).lt.0.99) then  !MJW 11/20/09  decide whether to use full-load turbine model or part load turbine model for Qgas calculation
        GN = (EgrSol + EgrFos) / Edesign
        Qgas = (Qdesign * (E2TPLF0 + E2TPLF1*GN + E2TPLF2*(GN**2) + E2TPLF3*(GN**3) + E2TPLF4*(GN**4)) - Qtpb) / LHVBoilEff ! .9 is boiler LHV Efficiency
     Else
        Qgas = EgrFos * Qdesign / Edesign / LHVBoilEff ! .9 is boiler LHV Efficiency
     End If
  Else
      EgrFos = 0
      Qgas = 0
  End If
         
  HtrLoad = EgrFos / Edesign    !First Order Estimate of the fraction of design output due to fossil
  PbLoad = (EgrSol + EgrFos) / Edesign ! this is the amount of design load met by both fossil and solar
  Egr = EgrFos + EgrSol !the gross electric output is the sum of solar and fossil
      

!  Heater Parasitics
   
  If (HtrLoad.GT.0) Then              ! Solar Field in Operation
!      EparHtr = HtrPar * HtrParPF * (HtrParF0 + HtrParF1 * HtrLoad + HtrParF2 * (HtrLoad**2))	
      EparHtr = HtrPar  * (HtrParF0 + HtrParF1 * HtrLoad + HtrParF2 * (HtrLoad**2))	!HP Sam input accounts for PF 12-12-06
   Else                               ! Heater is not in operation
      EparHtr = 0
   End If

!   Power Block Parasitics
   
    EparPb = PbFixPar               ! Fixed Power Block Parasitics (24 hr)
   
If (PbLoad.GT.0) Then            ! Power Block is in Operation
      
      
	!  Turbine Cycle Plant Parasitics (BOP)
      
!		EparBop = BopPar * BopParPF * (BopParF0 + BopParF1 * PbLoad + BopParF2 * (PbLoad**2))
		EparBop = BopPar * (BopParF0 + BopParF1 * PbLoad + BopParF2 * (PbLoad**2))	!HP Sam input accounts for PF 12-12-06
      
	!  Cooling Tower Parasitics
      
	  If (CtOpF.EQ.0) Then              !  CtOptF - when 1 runs at 50% or 100% only
!		  EparCt = CtPar * CtParPF * (CtParF0 + CtParF1 * PbLoad + CtParF2 * (PbLoad**2))
		  EparCt = CtPar  * (CtParF0 + CtParF1 * PbLoad + CtParF2 * (PbLoad**2))	!HP Sam input accounts for PF 12-12-06
	  Else                              !  Hot HTF Pumps (HTF from TS to PB)
		 If (PbLoad.LE.0.5) Then
!			EparCt = CtPar * CtParPF * 0.5	!HP Sam input accounts for PF 12-12-06
			EparCt = CtPar * 0.5
		 Else
!			EparCt = CtPar * CtParPF	!HP Sam input accounts for PF 12-12-06
			EparCt = CtPar 
		 End If
	  End If
  
Else                                ! Power Block is not in operation

   EparCt = 0                       ! No CT Operation
   EparBop = 0                      ! No BOP Operation
  
End If

! sum all the parasitics including current component, solar field and thermal storage
Epar = SFTotPar + EparHtr + EparHhtf + EparBop + EparCt + EparPb

If (PbLoad.EQ.0) Then
   EparOffLine =  Epar  ! if powerblock not running, then all parasitics are offline parasitics
   EparOnLine = 0
Else
  If ((Egr - Epar).GT.0) Then ! if powerblock running and gross output exceeds parasitics 
      EparOnLine =  Epar
      EparOffLine = 0
  Else ! if powerblock is running but the gross output does NOT exceed parasitics (why is it running then??)
      EparOnLine =  Egr
      EparOffLine =  Epar -  Egr
  End If
End If
   
! Calculate the net plant electric production by subtracting all the parasitics 
! including the solar field and thermal storage 
Enet = Egr - Epar  

   
! set outputs
OUT(1) = Enet        !| Net Electric Energy Production (Gross-Parasitics)              |      MWe       |     MWe
OUT(2) = EgrSol        !| Gross Solar Electric Generation                                |      MW        |     MW
OUT(3) = EMin          !| Solar Electric Generation below minimum powerplant output      |      MW        |     MW
OUT(4) = Edump         !| Solar Electric Generation that is in excess of powerplant max  |      MW        |     MW
OUT(5) = Pbload        !| Fraction of current Powerblock output to design output         | Dimensionless  | Dimensionless
OUT(6) = EgrFos        !| Gross Fossil Electric Generation                               |      MW        |     MW
OUT(7) = Egr           !| Gross Total Electric Generation                                |      MW        |     MW
OUT(8) = Qgas          !| Gas Thermal Energy Input                                       |      MW        |     MW
OUT(9) = HtrLoad       !| Heater Load Factor vs. rated output                            | Dimensionless  | Dimensionless
out(10) = Epar         ! | Total Parasitics for entire system                            |      MW        |     MW
OUT(11) = EparPB        !| Fixed Power Block Parasitics (24 hr)
OUT(12) = EparBOP       !| Balance of Plant Parasitics                                    |      MW        |     MW
OUT(13) = EparCT        !| Cooling Tower Parasitic Loads                                  |      MW        |     MW
OUT(14) = EparHtr       !| Heater Parasitics ???
OUT(15) = EparOffLine   !| Offline Parasitics ???
OUT(16) = EparOnLine    !| Online Parasitics ???
return 1

end subroutine Type807

   
