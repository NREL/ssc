! TRNSYS Type 805: Parabolic Trough Solar Collector Field based on Excelergy
! ----------------------------------------------------------------------------------------------------------------------
! COPYRIGHT 2007 NATIONAL RENEWABLE ENERGY LABORATORY
!
! This routine models a field of parabolic trough collectors. It is based on the Excelergy model.
! The model calculates both the inlet and outlet temperatures and various heat transfer rates.
!
!
! NB 8-20-06 modified the number of collectors to 1 and inserted Hank's heat loss model
!            Note: I've not modified for Patnode changes or Drew's modifications (if there are any)
!            - Changed parameter values to remove "NumofColls" first
!            - Removed Russ Forristall's heat loss inputs
!            - 
! Inputs 
! Doc. tables updated 7/1/2010 - MJW
!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Parameters
!    1| NumHCETypes                      | Number of receiver types used                                     | none             | none             
!    2| Solar_Field_Area                 | Solar Field Area                                                  | m2               | m2               
!    3| Solar_Field_Mult                 | Solar multiple                                                    | none             | none             
!    4| HTFFluid                         | Type of HTF fluid used                                            | none             | none             
! ...... Loop for 1..(Number of receiver types) .......
!    5| HCEtype(:)                       | Number indicating the receiver type                               | none             | none             
!    6| HCEFrac(:)                       | Fraction of field that is this type of HCE                        | none             | none             
!    7| HCEdust(:)                       | Performance degradation due to dust                               | none             | none             
!    8| HCEBelShad(:)                    | Bellows shading (fraction of rad. lost due to bellows)            | none             | none             
!    9| HCEEnvTrans(:)                   | Envelope transmittance                                            | none             | none             
!   10| HCEabs(:)                        | Absorber absorptance                                              | none             | none             
!   11| HCEmisc(:)                       | Miscellaneous emittance                                           | none             | none             
!   12| PerfFac(:)                       | Thermal Performance Factor                                        | none             | none             
!   13| RefMirrAper(:)                   | Mirror aperature width                                            | none             | none             
!   14| HCE_A0(:)                        | Heat loss calculated coefficient                                  | none             | none             
!   15| HCE_A1(:)                        | Heat loss calculated coefficient                                  | none             | none             
!   16| HCE_A2(:)                        | Heat loss calculated coefficient                                  | none             | none             
!   17| HCE_A3(:)                        | Heat loss calculated coefficient                                  | none             | none             
!   18| HCE_A4(:)                        | Heat loss calculated coefficient                                  | none             | none             
!   19| HCE_A5(:)                        | Heat loss calculated coefficient                                  | none             | none             
!   20| HCE_A6(:)                        | Heat loss calculated coefficient                                  | none             | none             
! ......//
!   21| LU_Fl                            | Fluid property file logical unit                                  | none             | none             
!   22| LuFlEr                           | Fluid property error file logical unit                            | none             | none             

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Inputs
!    1| SFTi                             | Solar Field HTF inlet Temperature (if -999, calculated)           | C                | C                
!    2| SolarAz                          | Solar Azimuthal Angle                                             | deg              | deg              
!    3| Insol_Beam_Normal                | Normal BeamInsolation                                             | kJ/hr/m2         | kJ/hr/m2         
!    4| AmbientTemperature               | Ambient Temperature                                               | C                | C                
!    5| WndSpd                           | Wind Speed                                                        | m/s              | m/s              
!    6| Stow_Angle                       | Night-Time Trough Stow Angle                                      | deg              | deg              
!    7| DepAngle                         | Deployment Angle                                                  | deg              | deg              
!    8| IamF0                            | Incident Angle Modifier - 0 factor                                | none             | none             
!    9| IamF1                            | Incident Angle Modifier - 1 factor                                | none             | none             
!   10| IamF2                            | Incident Angle Modifier - 2 factor                                | none             | none             
!   11| Ave_Focal_Length                 | Trough!s Average Focal Length                                     | m                | m                
!   12| Distance_SCA                     | Distance between SCAs in Row                                      | m                | m                
!   13| Row_Distance                     | Distance between Rows of SCAs                                     | m                | m                
!   14| SCA_aper                         | SCA Aperature                                                     | m                | m                
!   15| SfAvail                          | Solar Field Availability                                          | none             | none             
!   16| ColTilt                          | Collector Axis Tilt                                               | deg              | deg              
!   17| ColAz                            | Azimuthal Angle of Collector Axis                                 | deg              | deg              
!   18| NumScas                          | Number of SCAs per Row                                            | none             | none             
!   19| ScaLen                           | Length of Single SCA                                              | m                | m                
!   20| MinHtfTemp                       | Min. Heat Transfer Fluid Temperature                              | C                | C                
!   21| HtfGalArea                       | HTF Fluids in Gallons per Field Area                              | gal/m2           | gal/m2           
!   22| SfPar                            | SCA drives and electronics total parasitic                        | MWe              | MWe              
!   23| SfParPF                          | SCA drives and electronics multiplier                             | none             | none             
!   24| ChtfPar                          | Solar field HTF pump parasitic - factor                           | MWe              | MWe              
!   25| ChtfParPF                        | Solar field HTF pump parasitic - multiplier                       | none             | none             
!   26| CHTFParF0                        | Solar field HTF pump parasitic - equation constant                | none             | none             
!   27| CHTFParF1                        | Solar field HTF pump parasitic - equation linear term             | none             | none             
!   28| CHTFParF2                        | Solar field HTF pump parasitic - equation quadradic term          | none             | none             
!   29| AntiFrPar                        | Fixed anti-freeze pumping parasitic                               | MWe              | MWe              
!   30| Site_Lat                         | Latitude of Solar Plant Site                                      | deg              | rad              
!   31| Site_LongD                       | Longitude of Site                                                 | deg              | rad              
!   32| SHIFT                            | Longitude of Standard Meridian                                    | deg              | rad              
!   33| TurbOutG                         | Gross Turbine Output (SETS THEDESIGN POINT)                       | MWe              | MWe              
!   34| TurbEffG                         | Gross Turbine Eff (SETS THEDESIGN POINT)                          | none             | none             
!   35| SfInTempD                        | Solar Field DesignInlet  Temperature                              | C                | C                
!   36| SfOutTempD                       | Solar Field Design Outlet Temperature                             | C                | C                
!   37| ColType                          | Collector Type                                                    | none             | none             
!   38| TrkTwstErr                       | Tracking Error and Twist                                          | none             | none             
!   39| GeoAcc                           | Geometric Accuracy                                                | none             | none             
!   40| MirRef                           | Mirror Reflectivity                                               | none             | none             
!   41| MirCln                           | Mirror Cleanliness Factor                                         | none             | none             
!   42| ConcFac                          | Concentrator Factor                                               | none             | none             
!   43| SfPipeHl300                      | Solar field piping heat loss at design                            | W/m2             | W/m2             
!   44| SfPipeHl1                        | Solar field piping heat loss at reduced temp. - linear term       | C^(-1)           | C^(-1)           
!   45| SfPipeHl2                        | Solar field piping heat loss at reduced temp. - quadratic term    | C^(-2)           | C^(-2)           
!   46| SfPipeHl3                        | Solar field piping heat loss at reduced temp. - cubic term        | C^(-3)           | C^(-3)           
!   47| SFTempInit                       | Solar Field Initial Temperature                                   | C                | C                

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Outputs
!    1| SfTo                             | Final Outlet temperature of the solar field                       | C                | C                
!    2| SfMassFlow                       | Solar field mass flow rate                                        | kg/hr            | kg/s             
!    3| RecHl                            | Total receiver heat loss                                          | kJ/hr-m2         | W/m2             
!    4| AveSfTemp                        | Average Solar Field Temperature during the timestep               | C                | C                
!    5| SfPipeHlOut                      | Solar Field Pipe Heat Loss (non-receiver loss)                    | kJ/hr-m2         | kJ/hr-m2         
!    6| IAM                              | Incidence Angle Modifier (average over the timestep)              | none             | none             
!    7| Qabsout                          | Heat absorbed by the solar field before thermal loss              | kJ/hr-m2         | W/m2             
!    8| Hour_Angle                       | Hour angle                                                        | rad              | rad              
!    9| Qsf                              | Total thermal power from the solar field after losses             | MWt              | MWt              
!   10| SFTotPar                         | Total Solar Field Parasitics                                      | MWe              | MWe              
!   11| QsfWarmUp                        | Power required or that has contributed to warming the solar field | MWt              | MWt              
!   12| EndLoss                          | Fraction of energy lost due to spillage over the end of the row   | none             | none             
!   13| RowShadow                        | Fraction of energy lost from row-to-row shadowing                 | none             | none             
!   14| ColOptEff                        | Collector Optical Efficiency                                      | none             | none             
!   15| SfOptEff                         | Total Solar Field Optical Efficiency, including receiver optical loss| none             | none             
!   16| SfTI                             | Solar field inlet temperature                                     | C                | C                
!   17| Qdni                             | Total incident irradiation on the field before any losses         | MWt              | MWt              
!   18| EparCHTF                         | Cold HTF Pump Parasitics (HTF flow to Solar Field)                | MWe              | MWe              
!   19| EparSf                           | Parasitics associated with solar field tracking and drives        | MWe              | MWe              
!   20| EparAnti                         | Antifreeze pumping parasitics                                     | MWe              | MWe              
!   21| SolarTime                        | The average solar time for the current timestep                   | hour             | hour             
!   22| SolarAlt                         | The solar elevation angle above the horizon                       | deg              | rad              
!   23| Theta                            | Angle between the aperture plane normal and incident radiation    | deg              | rad              
!   24| CosTheta                         | Multiplying term that scales incident radiation due to angular loss| none             | none             
!   25| TrackAngle                       | Collector tracking angle                                          | deg              | rad              
!   26| Ftrack                           | The fraction of the time period that the field is tracking        | none             | none             
!   27| Qnip                             | Magnitude of the incident radiation (equal to beam normal)        | W/m2             | W/m2             
!   28| QnipCosTh                        | Incident radiation scaled by the cosine loss: "effective radiation"| W/m2             | W/m2             
!   29| Qabs                             | Energy absorbed by the solar field before th. loss, per unit area | W/m2             | W/m2             
!   30| Qcol                             | Energy delivered by the solar field, per unit area                | W/m2             | W/m2             
!   31| QsfAbs                           | Total energy absorbed by the solar field before th. loss          | MWt              | MWt              
!   32| QsfHceHL                         | Total energy lost by the receivers                                | MWt              | MWt              
!   33| QsfPipeHL                        | Total energy lost by the field piping                             | MWt              | MWt              
!   34| QsfWarmup                        | Total energy contributing to solar field warmup                   | MWt              | MWt              
!   35| QhtfFreezeProt                   | Total energy contributing to solar field freeze protection        | MWt              | MWt              
!   36| ColEff                           | Total collector + receiver efficiency, including optics and heat loss| none             | none             
!   37| Qsfnipcosth                      | Total inc. radiation scaled by the cosine loss                    | MWt              | MWt              

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
! This type was originally written by Mary Jane Hale at NREL in Feb. 2005, for TRNSYS 15. 
! It is based on the Excelergy program by Hank Price.
!
! Modifications:
!
! 2005-03-25 - MKu - Converted to TRNSYS 16 and F90
! 2005-06-19 - NB  - Modified extensively and added parasitics
! 2010-06-21 - MJW - Corrected heat loss iteration code
!             
!
! Local variables
! ----------------------------------------------------------------------------------------------------------------------
!
! To be completed
!
!
! Use of the Storage array
! ----------------------------------------------------------------------------------------------------------------------
!
! The storage array is not used in the current implementation. 
! If the Type was to be made compatible with multiple instances, either the storage array (S) or the Output array should
! be used to store information between calls in an independent way for each instance.
!
! ----------------------------------------------------------------------------------------------------------------------
! 

subroutine type805(time,xin,out,t,dtdt,par,info,iCntrl,*)

! Export this subroutine for its use in external DLLs
!dec$attributes dllexport :: type805

use TrnsysConstants ! Use TRNSYS global constants 
use TrnsysFunctions ! Use TRNSYS global functions 
use global_props !MJW 7/09 Use the global property array for user-defined HTF's

implicit none   ! Force explicit declaration of all variables


! --- TRNSYS declarations ----------------------------------------------------------------------------------------------
! Note- MJW 7/09 changed nP from 84 to 86 to add LU_FL, LuFlEr parameters
integer :: nI=47, nP=86, nD=0, nO=37   ! Nb of inputs, parameters, derivatives, outputs, storage locations
integer, parameter :: nS=7 !MJW 8/09 modified storage array

real(8), intent(in)    :: time, xin(*), par(*), t(*)
real(8), intent(inout) :: dtdt(*)
real(8), intent(out)   :: out(*)
real(8) :: stored
integer, intent(inout) :: info(15), iCntrl

dimension :: stored(nS)

! --- Local variables --------------------------------------------------------------------------------------------------

!integer, parameter :: nMaxColType = 6, nMaxHCEType = 4
integer, parameter :: nMaxHCEType = 4
real(8), parameter :: pi = 3.14159265359

!integer :: hr, NumColType, NumHCEtype, HTFFluid
integer :: hr, NumHCEtype, HTFFluid
integer :: nUnits = 0, n, nStart, ITER

integer :: LU_FL !MJW 7/09
integer :: ios, ntot, nufl, i, j, m, actb, qmode !MJW 7/09

real(8) :: B, EOT, DayLightHrs, SunRise, SunSet, DepAngle
real(8) :: DepHr1, DepHr2, DepHr3, DepTime, StwAngle, StwHr1, StwHr2, StwHr3, StwTime, MidTrack, StdTime
real(8) :: IAM, Dec, ColTilt, ColAz, EndGain, EndLoss, SolarNoon, CosTh, PH, RowShadow, ColOptEff, SfOptEff, Qsum_IAM
!real(8) :: ColType(nMaxColType), ColFrac(nMaxColType), TrkTwstErr(nMaxColType), GeoAcc(nMaxColType)
!real(8) :: MirRef(nMaxColType), MirCln(nMaxColType), ConcFac(nMaxColType), ColFactor(nMaxColType)
real(8) :: ColType, ColFrac, TrkTwstErr, GeoAcc,MirRef, MirCln, ConcFac, ColFactor
real(8) :: HCEType(nMaxHCEType), HCEFrac(nMaxHCEType), HCEdust(nMaxHCEType), HCEBelShad(nMaxHCEType)
real(8) :: HCEEnvTrans(nMaxHCEType), HCEabs(nMaxHCEType), HCEmisc(nMaxHCEType), HCEfactor(nMaxHCEType)
real(8) :: HCEflag(nMaxHCEType), PerfFac(nMaxHCEType), RefMirrAper(nMaxHCEType)
real(8) :: HCERecLength(nMaxHCEType),HCENumRecPerAperArea(nMaxHCEType)
real(8) :: HCE_A0(nMaxHCEType), HCE_A1(nMaxHCEType), HCE_A2(nMaxHCEType), HCE_A3(nMaxHCEType)    
real(8) :: HCE_A4(nMaxHCEType), HCE_A5(nMaxHCEType), HCE_A6(nMaxHCEType)   
real(8) :: TurbOutG, TurbEffG, QsfDesign,Site_LongD,WndSpd
real(8) :: SfTi, SfTiO, SfTo, SfToO, SfTi_HL, SfTo_HL, SfTo_hold, IamF0, IamF1, IamF2, Ftrack, NumScas, tim
real(8) :: SfInTemp, SfOutTemp, SfInTempD, SfOutTempD, AveSfTemp, AveSfTemp0, AveSfTemp0Next, AveSfTempD
real(8) :: SfTi_init
real(8) :: H_inD, H_outD
real(8) :: TimeDay, Julian_Day
real(8) :: time0, tFinal, delt, TSnow
real(8) :: sfpar, SfParPF, ChtfPar, CHTFParPF, CHTFParF0, ChtfParF1, ChtfParF2, AntiFrPar
real(8) :: EparSf, EparChtf, EparAnti, SFTotPar,Coleff

real(8) :: Hour_Angle, Solar_Altitude_Angle, Solar_Azimuth_Angle, Solar_Zenith_Angle, Insol_Beam_Normal
real(8) :: Long_Shift, Site_Lat, Stow_Angle, Deploy_Angle, Ave_Focal_Length, Distance_SCA, Solar_Field_Area, Solar_Field_Mult, Row_Distance
real(8) :: SCA_aper, SfAvail, HtfVolGal, Shift, ScaLen, MinHtfTemp, HtfGalArea, Tamb, ColFieldErr, HCEfieldErr
real(8) :: SfMassFlowD, TimeSteps, StartDay, StopDay, HrA, HrB, SolarAlt, SolarTime, SolarAz, AzNum, AzDen
real(8) :: Theta, TrackAngle, RecHL, RecHLout, FDTot, FETot,  Qabs, Qabsout, SfPipeHl1
real(8) :: HLTerm1, HLTerm2, HLTerm3, HLTerm4, HL, HLWind
real(8) :: SfPipeHl2, SfPipeHl3, SfPipeHl, SfPipeHlout, Qhl, QCol, Qnip, QnipCosTh, Qdni, Qsf, QsfAbs, dTemp, SfPipeHl300, QsfHceHl, QsfPipeHl
real(8) :: QHtfFreezeProt, SfMassFlow, SfMassFlowOut, H_thermMin, Ttemp, HtfDfnT, HtfMassKg, QsfWarmup,QsfNipCosTh
real(8) :: H_therminol, H_fluid, T_therminol, Cp_therm, T_Fluid, dThtf, SfLoad, H_thermD, H_therm0, H_therm, H_thermTo, H_thermTi
real(8) :: HtfMasslb, density, specheat

Logical :: CALCSFTi

Logical :: Is_there  !MJW 7/09
character :: checkname*200, tc  !MJW 7/09


! --- Messages for this Type -------------------------------------------------------------------------------------------
character(len=maxMessageLength) :: aString, msg(10)

msg(1) = "The current implementation of this Type does not allow multiple instances. Please use only one instance of "  &
         // "this Type in your input file."
msg(2) = "The number of HCE Types in the field (par(1)) must be an integer between 1 and nMaxHCEType."
!msg(3) = "The number of Collector Types in the field (par(2)) must be an integer between 1 and nMaxColType."
!msg(4) = "The current implementation of this Type only allows hourly time steps. Please change the simulation time step."

!MJW 7/09 Always store current time with global variable ttime
ttime=time

! --- Initial call to detect the TRNSYS version for which this Type is written -----------------------------------------

if (info(7) .eq. -2) then

    info(12) = 16   ! This component is a TRNSYS 16 Type
    return 1

endif


! --- Second call in simulation: initialization call (not a simulation call) -------------------------------------------

if (info(7) .eq. -1) then

    ! Always Read parameters
    NumHCEType = PAR(1)
    Solar_Field_Area = PAR(2)
    Solar_Field_Mult = PAR(3)
!    NumColType = PAR(2)
!NB 10-25-05 HTFFluid         = Par(6)
	HTFFluid = Par(4)
	!MJW 7/09 Read in the fluid property logical unit
	LU_FL = int(PAR(5+16*NumHCEType))
	LuFlEr = int(PAR(6+16*NumHCEType))

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
    if ( (NumHCEType < 1) .or. (NumHCEType > nMaxHCEType) ) then
        write(aString,'("You have entered ",i," and the maximum value allowed is ",i)') NumHCEType,nMaxHCEType
        aString = trim(msg(2)) // trim(aString) 
        call Messages(-1,aString,'Fatal',info(1),info(2))
        return 1
    endif

!    if ( (NumColType < 1) .or. (NumColType > nMaxColType) ) then
!        write(aString,'("You have entered ",i," and the maximum value allowed is ",i)') NumColType,nMaxColType
!        aString = trim(msg(3)) // trim(aString) 
!        call Messages(-1,aString,'Fatal',info(1),info(2))
!        return 1
!    endif

!    if (abs(delt-1.0d0) > minTimeStep/2.0d0) then
!        call Messages(-1,msg(4),'Fatal',info(1),info(2))
!        return 1
!    endif

!    ! Number of inputs
!    nI = 34+NumHCEType*20+1*7
     nP = 6 + NUMHCEType*16     !MJW 7/09 changed 4+ to 6+ for extra LU_FL, LuFlEr parameters

    info(9) = 1     ! Iterative controller, called at each iteration
    info(6) = nO   ! Set number of outputs

    ! Call the Typeck subroutine to compare what this component requires to what is supplied in the input file (also reserve outputs)
    call typeck(1,info,nI,nP,nD)

    ! Initialize important temperature values
    ! 391C is design outlet loop temperature
    ! 293C is design inlet loop temperature

!!NB IS THIS REALLY IMPORTANT TO DO???  SEEMS LIKE A WASTE IF MOST SIMULATIONS START AT MIDNIGHT
!    SfOutTemp = 391.0
!    SfInTemp = 293.0
!!    SfToO=SfOutTemp
!!    SfTiO=SfInTemp
!    SfTo=SfOutTemp
!    SfTi=SfInTemp
!    SfTi_init = 40
    SfTi_init = xin(47)
! INITIALIZE THE DESIGN VALUES - I THINK THESE SHOULD BE INPUTS OR CALCULATED SOMEWHERE ELSE?? WON'T THEY VARY BY SYSTEM
!    SfOutTempD = 391.0
!    SfInTempD = 293.0
    AveSfTemp0 = 100
    AveSfTemp0Next = 100
!    AveSfTempD = (SfOutTempD + SfInTempD)/2.0

!    ! Initialize important enthalpy values at design inlet and outlet loop temperatures
!    H_outD = 1000 * (-18.34 + 1.498 * SfOutTempD + 0.001377 * SfOutTempD**2)
!    H_inD = 1000 * (-18.34 + 1.498 * SfInTempD + 0.001377 * SfInTempD**2)
!     H_outD = H_fluid(SfOutTempD, HtfFluid)
!     H_inD = H_fluid(SfInTempD, HtfFluid)


    ! Initialize outputs to 0.0
    out(1:nO) = 0.0

    !MJW 7/09 Open the warnings file
    inquire(unit=LuFlEr,name=checkname,opened=is_there)
    !open the file for writing
    if(.not.is_there) then
        open(unit=LuFlEr,file=trim(checkname),status="REPLACE",position="REWIND")
    endif
    
    !MJW 8/09 Set the storage array size
    CALL setStorageSize(nS,INFO)
    
    !MJW 8/09 Initialize the storage array
    stored(1) = time0
    stored(2) = tFinal
    stored(3) = delt
    stored(4) = SfTi_init !SfTiO
    stored(5) = SfTi_init !SfToO
    stored(6) = AveSfTemp0
    stored(7) = AveSfTemp0Next
    
    call setStorageVars(stored,nS,INFO)
    
    

    return 1    ! Exit - End of the routine for very first call of the simulation

endif

!-----------------begin MJW 7/09
!   Do all initial timestep manipulations here.  There are no iterations at the initial time
if (time.lt.(getSimulationStartTime() + getSimulationTimeStep()/2.d0)) then
    
    !On the initial timestep call, read in the fluid property file
    call readFluidPropFile(LU_FL)    !MJW 7/09
    fl_flag = 0.  !MJW 7/09 Always reset the fluid warning flag to zero
    fl_ct = 0.  !MJW 7/09 and the warning counter too
    !Return to the calling program
    return 1
endif
!------------------------------------end MJW 7/09

! --- Very last call in simulation -------------------------------------------------------------------------------------

if (info(8) == -1) then

    if(allocated(fprop)) deallocate(fprop) !MJW 7/09
    
    inquire(unit=LuFlEr,opened=is_there)
    if(is_there) close(LuFlEr) !MJW 7/09
    
    return 1    ! Exit 
    
endif


! --- Post-convergence call --------------------------------------------------------------------------------------------

if (info(13) == 1) then

    !MJW 8/09
    stored(4) = SfTi
    stored(5) = SfTo
    stored(6) = AveSfTemp0Next  !AveSfTemp0
    stored(7) = AveSfTemp0Next
        
    call setStorageVars(stored,nS,INFO)
    
!    SfTiO = SfTi
!    SfToO = SfTo
!    AveSfTemp0 = AveSfTemp0Next


    return 1    ! Exit - End of the routine for post-convergence calls
    
endif



!****************************************************************************************************************

! --- All simulation calls ---------------------------------------------------------------------------------------------
! MJW 8/09  Always get stored variables
call getStorageVars(stored,nS,INFO)
time0 = stored(1)
tFinal = stored(2)
delt = stored(3)
SfTiO = stored(4)
SfToO = stored(5)
AveSfTemp0 = stored(6)
AveSfTemp0Next = stored(7)
!---

! Always Read parameters
NumHCEType       = PAR(1)
Solar_Field_Area = PAR(2)
Solar_Field_Mult = PAR(3)	!NB Added on 9-11-06
HTFFluid         = Par(4)
LU_FL            = int(Par(5+16*NumHCEType))  !MJW 7/09
LuFlEr           = int(Par(6+16*NumHCEType))  !MJW 7/09

ColFieldErr = 0.0
HCEfieldErr = 0.0
SfTi_init = XIN(47)

! INPUTS
IF (XIN(1).EQ.-999) THEN
   SFTI = SFTiO  ! use inlet temperature from last time as starting point for calculating inlet temp.
   IF (Time.eq.time0) SFTi = SFti_init
ELSE 
   SFTi = XIN(1) ! Use xin(1) as inlet temperature if connected to inlet temperature 
ENDIF

SolarAz = XIN(2)*pi/180.0
Insol_Beam_Normal = XIN(3)/3.6 ! Direct normal insolation in W/m2
Tamb = XIN(4)
WndSpd = XIN(5)
Stow_Angle = XIN(6) * pi / 180 ! stow angle input converted to radians
DepAngle = XIN(7) * pi / 180.0  ! deploy angle converted to radians (typically 10)

IamF0 = XIN(8)
IamF1 = XIN(9)
IamF2      = XIN(10)
Ave_Focal_Length = XIN(11)
Distance_SCA = XIN(12)
Row_Distance = XIN(13)
SCA_aper   = XIN(14)
SfAvail    = XIN(15)
ColTilt    = XIN(16)*pi/180.  !MJW 7/09
ColAz	   = XIN(17)*pi/180.  !MJW 7/09 -> added *pi/180.
NumScas    = XIN(18)
ScaLen	   = XIN(19)
MinHtfTemp = XIN(20)
HtfGalArea = XIN(21)

!Parasitic inputs
sfpar      = XIN(22)
SfParPF   = XIN(23)
ChtfPar   = XIN(24)
CHTFParPF = XIN(25)
CHTFParF0 = XIN(26)
ChtfParF1 = XIN(27)
ChtfParF2 = XIN(28)
AntiFrPar = XIN(29)
Site_Lat    = XIN(30)*pi/180.0
Site_LongD  = XIN(31)*pi/180.0
SHIFT       = XIN(32)*pi/180.0
TurbOutG    = XIN(33) !Gross Turbine Output (SETS THE DESIGN POINT) 
TurbEffG    = XIN(34) !Gross Turbine Eff (SETS THE DESIGN POINT) 

! Initialize important temperature values
! SfOutTemp is the default design outlet loop temperature (def = 391 C)
! SfInTemp is design inlet loop temperature  (def = 293 C)
SfInTempD = XIN(35)
SfOutTempD = XIN(36)
AveSfTempD = (SfOutTempD + SfInTempD)/2.0
ColType = XIN(37)
TrkTwstErr= XIN(38)
GeoAcc = XIN(39)
MirRef = XIN(40)
MirCln = XIN(41)
ConcFac = XIN(42)
ColFactor = TrkTwstErr * GeoAcc * MirRef * MirCln * ConcFac
ColFieldErr = ColFactor
SfPipeHl300 = XIN(43) ! Solar Field pipe   heat losses at design temp, W/m2
SfPipeHl1 = XIN(44)   ! Solar Field piping heat losses at reduced temp, W/m2
SfPipeHl2 = XIN(45)
SfPipeHl3 = XIN(46)


! Initialize important design values at design inlet and outlet loop temperatures
H_outD = H_fluid(SfOutTempD, HtfFluid)		!Enthalpy of HTF at Collector Outlet [J/kg]
H_inD = H_fluid(SfInTempD, HtfFluid)		!Enthalpy of HTF at Collector Inlet [J/kg]

! *************************************************************
! QsfDesign = TurbOutG / TurbEffG - This is the power block design
! *************************************
!
! Nate Add Solar Multiple to Replace 2.1 in the equation below.
!
! *************************************

!QsfDesign = TurbOutG / TurbEffG * 2.1 !HP added solar multiple but should read in from SAM

QsfDesign = TurbOutG / TurbEffG * Solar_Field_Mult !HP added solar multiple but should read in from SAM

SfMassFlowD = (QsfDesign * 1000000) / (H_outD - H_inD)		!Kg/sec

!Delt is decimal fraction of hour to produce 1, 2 or 4 timesteps/hr
!NB TimeSteps = 1.0 / Delt
TimeSteps = NINT(1.0 / Delt)


Julian_Day = int(Time/24)+1
StartDay = int(Time0/24) + 1
StopDay = int(Tfinal/24) + 1

! Hour of day in standard time
TimeDay = Time - ((Julian_Day-1)*24.0)

if ((TimeDay - int(TimeDay)) == 0.00) then
	TSnow = 1.0
else
	TSnow = 1.0/(TimeDay - int(TimeDay))
endif

! Duffie & Beckman 1.5.3b
B = (Julian_Day-1)*360.0/365.0*pi/180.0

! Eqn of time in minutes
EOT = 229.2 * (0.000075 + 0.001868 * COS(B) - 0.032077 * SIN(B)	- 0.014615 * COS(B*2.0) - 0.04089 * SIN(B*2.0))

! Declination in radians (Duffie & Beckman 1.6.1)
Dec = 23.45 * SIN(360.0*(284.0+Julian_Day)/365.0*pi/180.0) * pi/180.0

! Solar Noon in hours
!SolarNoon = 12 - ((StdLongD - Site_LongD)*180.0/pi) / 15 - EOT / 60
SolarNoon = 12 - ((SHIFT)*180.0/pi) / 15 - EOT / 60

! Number of daylight hours
DayLightHrs = 2.0 / 15.0 * ACOS(-TAN(Site_Lat) * TAN(Dec)) * 180.0 / pi
	
! Sunrise and set in hours
SunRise = SolarNoon - (DayLightHrs / 2.0)
SunSet = SolarNoon + (DayLightHrs / 2.0)

! Deploy & stow times in hours
! Calculations modified by MJW 11/13/2009 to correct bug
DepAngle = dmax1(DepAngle,1.e-6)
DepHr1 = COS(Site_Lat) / TAN(DepAngle)
DepHr2 = -TAN(Dec) * SIN(Site_Lat) / TAN(DepAngle)
DepHr3 = sign(1.d0,tan(pi-DepAngle))*ACOS((DepHr1*DepHr2 + Sqrt(DepHr1*DepHr1-DepHr2*DepHr2+1.0)) / (DepHr1 * DepHr1 + 1.0)) * 180.0 / pi / 15.0
DepTime = SolarNoon + DepHr3

Stow_Angle = dmax1(Stow_Angle,1.e-6)
StwHr1 = COS(Site_Lat) / TAN(Stow_Angle)
StwHr2 = -TAN(Dec) * SIN(Site_Lat) / TAN(Stow_Angle)
!StwHr3 = sign(1.d0,tan(pi-StwAngle))*ACOS((StwHr1*StwHr2 + Sqrt(StwHr1*StwHr1-StwHr2*StwHr2+1.0)) / (StwHr1 * StwHr1 + 1.0)) * 180.0 / pi / 15.0
StwHr3 = sign(1.d0,tan(pi-Stow_Angle))*ACOS((StwHr1*StwHr2 + Sqrt(StwHr1*StwHr1-StwHr2*StwHr2+1.0)) / (StwHr1 * StwHr1 + 1.0)) * 180.0 / pi / 15.0
StwTime = SolarNoon + StwHr3

! HrA and HrB are used for book keeping.  Accomodates 1, 2 and 4 timesteps/hr
HrA = int(TimeDay) - 1.0 + (TSnow-1.0)/TimeSteps
HrB = int(TimeDay) - 1.0 + (TSnow/TimeSteps)

! Ftrack is the fraction of the time period that the field is tracking. MidTrack is time at midpoint of operation

! Solar field operates
if ((HrB > DepTime) .AND. (HrA < StwTime)) then
    ! solar field deploys during time period
    if (HrA < DepTime) then
        Ftrack = (HrB - DepTime) / TimeSteps
        MidTrack = HrB - Ftrack * 0.5 / TimeSteps
    ! Solar field stows during time period
    elseif (HrB > StwTime) then
        Ftrack = (StwTime - HrA) / TimeSteps
        MidTrack = HrA + Ftrack * 0.5 / TimeSteps
    ! solar field operates during entire period
    else
        Ftrack = 1.0
        MidTrack = HrA + 0.5 / TimeSteps
    endif
! solar field doesn't operate
else
    Ftrack = 0.0
    MidTrack = HrA + 0.5 / TimeSteps
endif

StdTime = MidTrack
SolarTime = StdTime+((Shift)*180.0/pi)/15.0+ EOT/60.0

! hour angle (arc of sun) in radians
Hour_Angle=(SolarTime-12.0)*15.0*pi/180.0

! B. Stine equation for Solar Altitude angle in radians
! change to Type 16 output
 SolarAlt = ASIN(SIN(Dec)*SIN(Site_Lat)+COS(Site_Lat)*COS(Dec)*COS(Hour_Angle))

! AzNum and AzDen just checking to prevent error
AzNum = (SIN(Dec)*COS(Site_Lat)-COS(Dec)*COS(Hour_Angle)*SIN(Site_Lat))
AzDen = COS(SolarAlt)

if (abs(AzNum-AzDen)<= 0.0001) then
    AzDen = AzDen + 0.01
endif

! cos theta
! calculation of solar incidence angle for trough
! Stine reference
CosTh = SQRT(1 - (COS(SolarAlt-ColTilt) - COS(ColTilt) * COS(SolarAlt) * (1 - COS(SolarAz -ColAz))) ** 2)

! Theta in radians
Theta = ACOS(CosTh)

! Calculation of Tracking Angle for Trough. Stine Reference
TrackAngle = Atan ( Cos(SolarAlt) * Sin(SolarAz-ColAz) /     &
             (Sin(SolarAlt-ColTilt)+Sin(ColTilt)*Cos(SolarAlt)*(1-Cos(SolarAz-ColAz))) )

! Incident Angle Modifier - IAM
! based on LS-2 Testing at Sandia
! based on Figure 10, p12 SAND94-1884
! K=IAM/cos(IA)
if (CosTh == 0) Then
    IAM = 0
Else
    IAM = IamF0 + IamF1 * Theta / CosTh + IamF2 * Theta * Theta / CosTh
End If


! EndLoss - light that reflects off end of collector plus the light that reflects from one SCA to the next
! Hank's approach - not validated
EndGain = Ave_Focal_Length * Tan(Theta) - Distance_SCA
If (EndGain < 0) Then
    EndGain = 0
End If

! NumScas is # of SCAs in a row; 4 at SEGS
EndLoss = 1 - (Ave_Focal_Length * Tan(Theta) - (NumScas - 1) / NumScas * EndGain) / ScaLen

! Row to Row Shadowing Lossess
PH = pi / 2.0 - TrackAngle

! Row_Distance is distance between rows of SCA; 15m @ SEGS
! SCA_Aper is SCA aperature; 5m LS-2
RowShadow = Abs(Sin(PH)) * Row_Distance / SCA_Aper
   
If ((RowShadow < 0.5) .OR. (SolarAlt < 0)) Then
    RowShadow = 0
Else If (RowShadow > 1) Then
    RowShadow = 1
End If





!*****************************************************************************************************************
!NB BEGIN REPEAT-CODE TO SEARCH FOR CONVERGENCE OF THE SOLAR FIELD OUTLET TEMPERATURE BASED ON INLET TEMPERATURE
!NB THIS LOOP SEEMS TO BE NECESSARY DUE TO THE FACT THAT THE HEAT LOSSES ARE ALL CALCULATED BASED ON THE 
!NB AVERAGE LOOP TEMPERATURE AND NOT THE INLET
!NB THIS REPEAT-CODE SECTION ENDS JUST AFTER CALCULATION OF THE OUTPUT TEMPERATURE
!NB IT REPEATS IF THE NEW OUTLET TEMPERATURE IS MORE THAN .1 DEGREES DIFFERENT THAN
!NB THE PREVIOUSLY CALCULATED OUTLET TEMPERATURE ??? IS THIS ACCURATE ENOUGH????
!NB IF IT HAS TO ITERATE MORE THAN 1000 TIMES, IT BAILS AND ISSUES AN ERROR MESSAGE
ITER = 0
CALCSFTi = .FALSE.
SfTo_hold = 1000

!DO WHILE ((ABS(SfTo - SfTo_hold) > 0.1).AND.(CALCSFTi.EQ..FALSE.)) ! HP Changed 12-08-06
!DO WHILE ((ABS(SfTo - SfTo_hold) > 0.1).OR.(CALCSFTi.EQ..FALSE.)) ! HP Changed AND 12-08-06
! HP Was not recalculating the thermal losses with the final temperatures

DO ! HP Changed 12-08-06 ***************************************************************

	ITER = ITER + 1
	SfTo_hold = SfTo  ! if repeating, set the old SFTo to the _hold for the next iteration.

	IF ((ITER==1).and.(XIN(1).eq.-999)) then   !SFTi is calculated and not an input || MJW added the iteration constraint 6/21/2010
	    CALCSFTi = .TRUE.  ! Set this to true so this loop only is called once
	    SFTi = SFTiO       ! Use the values from last time for the calculations
	    SFTo = SfToO       ! Use the values from last time for the calculations
	ENDIF

	IF (iter >= 10000) then
	    ! Fatal error. NOT SURE IF THIS WILL BE THE RIGHT ERROR MESSAGE BUT FOR NOW...
		call Messages(-1,'CSP Trough model exceeded internal iteration limit.','fatal',info(1),info(2))
	    return 1
	endif

	!********* RECEIVER HEAT LOSS CALCUALATIONS ***************************
	! The following chunk of code calculates the receiver heat loss
	! according to Hank Price's new (4-28-06) heat loss model

	RecHL = 0.0
	HCEfieldErr=0.0

	! --- Calculations for each HCE Type --- 

	! HP Why do we have to do this every iteration???

	do n=1, NumHCEtype

		HCEtype(n)             = PAR(4+ n*16 - 15) !this is a number based on receiver type - not sure it's useful in this new method
		HCEFrac(n)             = PAR(4+ n*16 - 14) !Fraction of field that is this type of HCE
		HCEdust(n)             = PAR(4+ n*16 - 13) !perf degradation due to dust
		HCEBelShad(n)          = PAR(4+ n*16 - 12) !bellows shading (typically .971)
		HCEEnvTrans(n)         = PAR(4+ n*16 - 11) !envelope transmissivity (typically .96)
		HCEabs(n)              = PAR(4+ n*16 - 10) !Absorber absorption (typically .96)
		HCEmisc(n)             = PAR(4+ n*16 - 9) !Miscellaneous emissivity (typically 1.0)
		!!!!!!!!!HCEflag(n)             = PAR(3+ NUMHCEType*20 - 12) !flag to indicate if actual or temp-over-ambient were used to measure properties 
		PerfFac(n)             = PAR(4+ n*16 - 8) !Thermal Performance Factor (typically 1)
		RefMirrAper(n)         = PAR(4+ n*16 - 7) !Mirror aperature (5m for LS2)
		!Removed 11/2009 MJW        HCEMinWind(n)          = PAR(4+ n*16 - 7)  !minimum windspeed 
		!!!!!!!!HCERecLength(n)         = PAR(3+ NUMHCEType*20 - 8)  !receiver length
		!!!!!!!!HCENumRecPerAperArea(n) = PAR(3+ NUMHCEType*20 - 7)  !number of receivers per aperture area
		HCE_A0(n)              = PAR(4+ n*16 - 6)  !heat loss calculated coefficient
		HCE_A1(n)              = PAR(4+ n*16 - 5)  !heat loss calculated coefficient
		HCE_A2(n)              = PAR(4+ n*16 - 4)  !heat loss calculated coefficient
		HCE_A3(n)              = PAR(4+ n*16 - 3)  !heat loss calculated coefficient
		HCE_A4(n)              = PAR(4+ n*16 - 2)  !heat loss calculated coefficient
		HCE_A5(n)              = PAR(4+ n*16 - 1)  !heat loss calculated coefficient
		HCE_A6(n)              = PAR(4+ n*16 )     !heat loss calculated coefficient
										

		If (SfTi == SfTo) then
			SfTo = SfTi + 0.1		!HP Keeps HL curve fits from blowing up
		end if

		HCEfactor(n) = HCEfrac(n) * HCEdust(n) * HCEBelShad(n) * HCEEnvTrans(n) * HCEabs(n) * HCEmisc(n)
		HCEfieldErr  = HCEfieldErr + HCEfactor(n)

		!HLWind = MAX(HCEMinWind(n),WndSpd)	!Keeps curve fits from blowing up.
		                                    !MJW 11/09  Recommend removing this restriction
		HLWind = dmax1(WndSpd, 0.d0) !MJW 6/29/2010 Instead enforce a positive windspeed. Some datasets include negative windspeeds.

		HLTerm1 = (HCE_A0(n)+HCE_A5(n)*(HLWind**0.5))*(SFTo-SFTi)
    
		HLTerm2 = (HCE_A1(n)+HCE_A6(n)*sqrt(HLWind))*((SFTo**2-SFti**2)/2.d0-Tamb*(SFTo-SFTi))
		
		HLTerm3 = ((HCE_A2(n)+HCE_A4(n)*(Insol_Beam_Normal * CosTh * IAM))/3)*(SFTo**3-SFTi**3)

		HLTerm4 = (HCE_A3(n)/4)*(SFTo**4-SFTi**4)

		HL = (HLTerm1 + HLTerm2 + HLTerm3 + HLTerm4)/(SFTo-SFTi)

		! Convert Receiver HL from W/m of receiver to W/m2 of collector aperture
		RecHL = RecHL + (PerfFac(n) * HCEfrac(n) * HL / RefMirrAper(n))
        continue
	enddo 
	! --- Receiver HEAT LOSS Calculations for each HCE Type --- end


	if (RecHL .LT. 0) then ! Check to make sure RecHL are not less than zero.
		RecHL = 0
	endif
		

	!     ' SF Thermal Calc
	!		
	!		Qcol = Qabs - Qhl
	!	
	!		Where:
	!			Qabs = CosTh * Insol_Beam_Normal * SfOptEff * SfAvail
	!			SfOptEff = ColOptEff * RowShadow * EndLoss * IAM
	!			ColOptEff = HCEdust * HCEBelShad * HCEEnvTrans * HCEabs * HCEmisc
	!			Qhl = RecHL + SfPipeHl
	!
	! ColOptEff is the collector optical efficiency; It is a product of factors accounting for twist,
	! geometry, reflectivity, cleanliness, and concentration error. Below, ColOptEff being initialized.

	ColOptEff = ColFieldErr * HCEfieldErr * FTRACK  !MJW Added ftrack -11/23/09
	SfOptEff = ColOptEff * RowShadow * EndLoss * IAM
					
	! Heat absorbed by collectors in W/m2
	Qabs = CosTh * Insol_Beam_Normal * SfOptEff * SfAvail
			
	! Difference between average solar field temperature from previous
	! timestep and ambient temperature
	AVESFTEMP = (SFTI+SFTO)/2.0
	dTemp = AveSfTemp - Tamb

	! Solar Field pipe heat losses at design temp, W/m2
	SfPipeHl = (SfPipeHl3 * dTemp * dTemp * dTemp + SfPipeHl2 * dTemp * dTemp + SfPipeHl1 * dTemp) * SfPipeHl300
	Qhl = RecHL + SfPipeHl
	Qcol = Qabs - Qhl

	if (Qcol < 0) then
		Qcol = 0
	end if

	! Solar Field Thermal Delivery Calculation
	! Uses Data from Qcol to determine solar field output
	Qnip = Insol_Beam_Normal
	QnipCosTh = Qnip*CosTh

	! 
	Qdni = Qnip * Solar_Field_Area / 1000000.0				! incident direct normal radiation on solar field, MWt
	QsfNipCosTh = Qnip*CosTh* Solar_Field_Area / 1000000.0	! radiation in the plane of the collectors, MWt
	QsfAbs = Qabs * Solar_Field_Area / 1000000.0			! energy absorbed by receiver (before thermal losses), MWt
	QsfHceHl = RecHL * Solar_Field_Area / 1000000.0			! energy lost by HCEs/receivers in field, MWt
	QsfPipeHl = SfPipeHl * Solar_Field_Area / 1000000.0		! energy lost by header piping in field, MWt
	Qsf = Qcol * Solar_Field_Area / 1000000.0				! net energy delivered by solar field, MWt

	QHtfFreezeProt = 0
!	QHtfFpTes = 0
!	QHtfFPHtr = 0
	SfMassFlow = 0

	!H_thermMin = 1000 * (-18.34 + 1.498 * MinHtfTemp + 0.001377 * MinHtfTemp**2)
	H_thermMin = H_fluid(MinHtfTemp+273.15, HtfFluid)

	Ttemp = 25.0			!HP Commented 12-11-06

	! dens_fluid(T, fluidnum)
!	HtfDfnT = 67.3091 - 0.020566 * (Ttemp) - 0.000014481 * (Ttemp) ** 2		!HP Commented 12-11-06
	HtfVolGal = HtfGalArea * Solar_Field_Area
!	HtfMassLb = HtfVolGal / 7.48 * (67.3091 - 0.020566 * (Ttemp) - 0.000014481 * (Ttemp) ** 2)		!HP Commented 12-11-06
	HtfMassKg = HtfVolGal / 264.2 * density(dble(HtfFluid),ttemp+273.15,0.d0) !MJW 8.17.2010 dens_fluid(ttemp, HtfFluid)	!HP Added 12-11-06

			
	QsfWarmUp = 0.0

	! this block of code calculates critical temperatures.
	! AveSfTemp - the ave solar field htf temperature,C, at end of timestep
	! SfTi - the loop inlet htf temperature,C, at end of timestep	
	! SfTo - the loop outlet htf temperature,C, at end of timestep
	! SfMassFlow - the htf mass flow rate in or out of entire field, kg/s	
	
	!-------MJW 6/2010
	!The original code didn't correctly allow temperature iteration. The corrected iteration method (see beginning of the 
	!do-loop, MJW comment) sometimes encounters a non-resolvable loop if heat loss + qsf ~= 0. If more than 10 iterations
	!occur, this is the case. Limit the allowed number to 10 and keep with the previously selected solar field mode.
	!**Note that this change has a significant impact on heat loss and annual energy production (~+4% for SAM default case)**
    if(ITER<=10) then        !MJW|| for >10 iterations, the results don't change.
        if (Qsf <= 0) then
            qmode = 0
        else
            qmode = 1
        endif
    endif
    !-------end MJW 6/2010
    
	select case(qmode)
	case(0)
!		AveSfTemp = (T_fluid(H_fluid(AveSfTemp0, HtfFluid) - (Qhl*Solar_Field_Area*3600 / Timesteps / (HTFmasslb/2.2)),HTFFluid))
		AveSfTemp = (T_fluid(H_fluid(AveSfTemp0+273.15, HtfFluid) - (Qhl*Solar_Field_Area*3600 / Timesteps / HTFmassKg),HTFFluid))-273.15
		QsfWarmUp = 0.0

		If (AveSfTemp <= MinHtfTemp) Then
			QHtfFreezeProt = (H_fluid(MinHtfTemp+273.15, HtfFluid) - H_fluid(AveSfTemp+273.15, HtfFluid)) * HTFmassKg / 3600 / 1000000.0
			AveSfTemp = MinHtfTemp
			! QHtfFreezeProt comes from heater or TES
		Else
			QHtfFreezeProt = 0
		End If

		!IF (
		
!		QHtfFPHtr = QHtfFreezeProt
!		QHtfFpTES = QHtfFreezeProt

		AveSfTemp0Next = AveSfTemp

		! Specific Heat of Therminol Oil, J/kg/K (T [C])
		dThtf = (QsfHceHl + QsfPipeHl) * 1000000 / SfMassFlowD / (specheat(dble(HtfFluid), AveSfTemp+273.15, 0.d0)*1000.)
		IF (Xin(1) .EQ. -999) then
		   SfTi = AveSfTemp + dThtf / 2.0 + 0.001
		Endif

		SfTo = AveSfTemp - dThtf / 2.0 - 0.001
		SfLoad = 0
		SfMassFlow = 0

	case(1)

		If ((Qsf > 0).AND.(AveSfTemp < AveSfTempD)) Then
				!NB SEEMS LIKE THIS SECTION HAPPENS IF QSF IS POSITIVE AND PREVIOUS PERIOD WAS BELOW DESIGN TEMPERATURE
			QsfWarmUp = (H_fluid(AveSfTempD+273.15, HtfFluid) - H_fluid(AveSfTemp0+273.15, HtfFluid)) * HtfMassKg / 3600.0 / 1000000.0

			If (Qsf / TimeSteps > QsfWarmUp) Then
				! NB IF THERE IS MORE THAN ENOUGH ENERGY TO REACH THE DESIGN POINT
				Qsf = Qsf - QsfWarmUp * TimeSteps
				AveSfTemp = AveSfTempD
				AveSfTemp0Next = AveSfTempD
					! NB ADDED SFTO CALC
				IF (XIN(1).eq.-999) THEN
				   SFTi = SfInTempD
				ENDIF
				SFTo = (2.0*AVESFTEMP) - SFTI
			Else
				!NB NOT ENOUGH ENERGY TO REACH THE DESIGN POINT
				AveSfTemp = (T_fluid(H_fluid(AveSfTemp0+273.15, HtfFluid) + Qcol * Solar_Field_Area * 3600.0 / TimeSteps / HtfMassKg, HtfFluid))-273.15
				QsfWarmUp = (H_fluid(AveSfTemp+273.15, HtfFluid) - H_fluid(AveSfTemp0+273.15, HtfFluid)) * HtfMassKg / 3600.0 / 1000000.0
				AveSfTemp0Next = AveSfTemp
				Qsf = 0
					!NB ADDED SFTO CALC
				IF (XIN(1).eq.-999) THEN
				   ! HP Changed 11-26-06
				   ! SFTi = SfInTempD   !Unless have the inlet temperature, assume inlet is design point during startup
				   ! SFTi = AveSfTemp - 10.  
				   SFTi = AveSfTemp - QsfWarmUp/QsfDesign*(SfOutTempD-SfInTempD)    
   
				ENDIF
				SFTo = (2.0*AVESFTEMP) - SFTi
			End If

		Else
			!NB IT ISN'T ALLOWED TO EXCEED THE DESIGN POINT IN AVERAGE TEMPERATURE
			AveSfTemp = AveSfTempD
			AveSfTemp0Next = AveSfTempD
			QsfWarmUp = 0.0
			IF (XIN(1).eq.-999) THEN
			   SFTi = SfInTempD
			ENDIF
			!NB ADDED SFTO CALC
			SFTo = (2.0*AVESFTEMP) - SFTI  ! this works either way if SFti is input or calculated (above)
		EndIf

		SfLoad = Qsf / QsfDesign
		SfMassFlow = Qsf * 1000000.0 / (H_fluid(SfTo+273.15, HtfFluid) - H_fluid(SfTi+273.15, HtfFluid)) !anytime Qsf>0
        
	! Here is where we add max and min on solar field???
         
	end select

	!NB HERE IS WHERE THE LOOP ENDS TO ITERATE ON THE OUTLET TEMPERATURE BY RECALCULATING THE HEAT LOSSES, ETC.

If((ABS(SfTo - SfTo_hold) < 0.1).AND.(CALCSFTi.NE..FALSE.)) Exit !HP Added 12-08-06

END DO ! **** HP Changed 12-08-06

!MJW 8/09 Load the average solar field temp into storage for each iteration
stored(7) = AveSfTemp0Next
call setStorageVars(stored,nS,INFO)


!CALCULATE PARASITICS FOR SOLAR FIELD (NOT IN ORIGINAL MJ VERSION)

!   '  Solar field parasitics
        
If (SfLoad.GT.0.01) Then               ! Solar Field in Operation
!   EparSf = SfPar * SfParPF     !  Solar Field (Loc and motor parasitics)
   EparSf = SfPar																		! HP Changed 12-12-06
!   EparChtf = ChtfPar * ChtfParPF * (ChtfParF0 + ChtfParF1 * SfLoad + ChtfParF2 * (SfLoad**2.))
   EparChtf = ChtfPar  * (ChtfParF0 + ChtfParF1 * SfLoad + ChtfParF2 * (SfLoad**2.))	!HP Changed 12-12-06
                                    !  Cold HTF Pumps (HTF flow to Solar Field)
   EparAnti = 0
   
Else                             ! Solar field is not in operation
   EparSf = 0
   EparChtf = 0
   EparAnti = AntiFrPar         !  Antifreeze pumping when field is circulated at night
   
End If
   
SFTotPar = EparSF + EparCHTF + EparAnti


! --- Set the outputs ---
!Convert Mass flow from kg/sec to kg/hr (more standard TRNSYS units)
!SfMassFlowOut = SfMassFlow*3600
SFmassflowout = SFMassFlow
!Convert heat loss outputs from Watts/m2 to kJ/hr-m2 (more standard TRNSYS units)
!RecHLout = RecHl*3.6
RecHLout = RecHl
!SfPipeHlout = SfPipeHl*3.6
SfPipeHlout = SfPipeHl
!Qabsout = Qabs*3.6
Qabsout = Qabs

ColEff = Qcol/dmax1(Qnip, 1.e-6)      !MJW 6/2010: Returns NaN when Qnip==0. Enforce limit

out(1) = SfTo          !C
out(2) = SfMassFlowOut !kg/hr units {info only}
out(3) = RecHlOut      !kj/hr-m^2  {info only}
out(4) = AveSfTemp	   !temperature in C
out(5) = SfPipeHlOut   !kj/hr-m^2 {info only}
out(6) = IAM          !{info only}
out(7) = QabsOut       !W/m^2  {info only}
out(8) = Hour_Angle
out(9) = Qsf          ! MW of output {used in type806}
out(10) = SFTotPar     ! MW of parasitics (Should be subtracted from plant output)  {used in type807}
out(11) = QsfWarmUp   !{info only}
out(12) = EndLoss     !{info only}
out(13) = RowShadow   !{info only}
out(14) = ColOptEff    ! HANK - When you ask for the colleff - Is this it, the optical efficiency?? {info only}
out(15) = SfOptEff      !{info only}
out(16) = SFTi          !{info only}
out(17) = Qdni          !Used in waterfall outputs
out(18) = EparCHTF      !{info only}

out(19) = EparSf     ! Parasitics of the solar field MWhe
out(20) = EparAnti   ! Antifreeze parasitics MWhe

out(21) = SolarTime  ! 
out(22) = SolarAlt*180.d0/pi   ! solar altitude deg
out(23) = Theta*180.d0/pi    ! deg
out(24) = CosTh   ! fraction
out(25) = TrackAngle*180.d0/pi ! deg 
out(26) = Ftrack     ! MJW 11/09 ::  Ftrack is the fraction of the time period that the field is tracking
out(27) = Qnip       ! W/m2 {info only}
out(28) = QnipCosTh  ! W/m2 {info only}
out(29) = Qabs       ! W/m2 {info only}
out(30) = Qcol       ! W/m2 {info only}

out(31) = QsfAbs     ! MWt {info only}
out(32) = QsfHceHL   ! MWt
out(33) = QsfPipeHL  ! MWt
out(34) = QsfWarmup  ! MWt
out(35) = QhtfFreezeProt ! MWt

out(36) = ColEff
out(37) = QsfNipCosTh !MWt
return 1

end subroutine Type805




Double Precision Function HtfHfnT(T) 
implicit none
Double Precision T
   !  enthalpy in btu/lb
   !  temp in F
   HtfHfnT = -19.477 + 0.35271 * T + 0.000178713 * T * T
End Function

Double Precision Function HtfDfnT(T)
implicit none
Double Precision T
   !  density in lbs/ft3
   !  temp in F
   HtfDfnT = 67.3091 - 0.020566 * T - 0.000014481 * T * T
End Function

Double Precision Function HtfTfnH(H)
implicit none
Double Precision H
   !  enthalpy in Btu/lbs
   !  temp in F
   HtfTfnH = 0.000002*(H**3)  - 0.0025*(H**2)  + 2.6377*H  + 54.147
End Function


!Note: additional HTF properties used by this type can be found implemented in TYPE229

!*******************************************************************************

! NOT NEEDED AS ALREADY A BUIL-IN FUNCTION IN FORTRAN
!Double Precision Function Log10(x)
!    Log10 = Log(x) / Log(10)
!End Function
!
!Density of Bed Material [kg/m3]
!Double Precision Function dens_bed(BedNum) !BedNum: 1 = taconite
!!                                  2 = calcium carbonate
!!                                  3 = gravel
!!                                  4 = marble
!!                                  5 = limestone
!!                                  6 = other
!!                                  7 = carbon steel
!!                                  8 = sand
!!                                  9 = quartzite
!implicit none
!INTEGER BedNum
!
!If (BedNum == 1) Then
!        dens_bed = 3800
!    ElseIf (BedNum == 2) Then
!        dens_bed = 2710
!    ElseIf (BedNum == 3) Then
!        dens_bed = 2643
!    ElseIf (BedNum == 4) Then
!        dens_bed = 2680
!    ElseIf (BedNum == 5) Then
!        dens_bed = 2320
!    ElseIf (BedNum == 6) Then
!        dens_bed = 5280
!    ElseIf (BedNum == 7) Then
!        dens_bed = 7854
!    ElseIf (BedNum == 8) Then
!        dens_bed = 1515
!    ElseIf (BedNum == 9) Then
!        dens_bed = 2640
!    End If
!End Function
!
!!Bed material heat capacity
!Double Precision Function Cp_bed(BedNum) !BedNum:  1 = taconite
!!                                 2 = calcium carbonate
!!                                 3 = gravel
!!                                 4 = marble
!!                                 5 = limestone
!!                                 6 = other
!!                                 7 = carbon steel
!!                                 8 = sand
!!                                 9 = quartzite
!implicit none
!Integer BedNum
!
!    If (BedNum == 1) Then
!        Cp_bed = 0.651  !kJ/kg C
!    ElseIf (BedNum == 2) Then
!        Cp_bed = 0.835  !kJ/kg C
!    ElseIf (BedNum == 3) Then
!        Cp_bed = 1.065  !kJ/kg C at average temperature of 335 C
!        !810# + 0.75 * Temp J/kg C orginal expression with temp correction
!    ElseIf (BedNum == 4) Then
!        Cp_bed = 0.83   !kJ/kg C
!    ElseIf (BedNum == 5) Then
!        Cp_bed = 0.81   !kJ/kg C
!    ElseIf (BedNum == 6) Then
!        Cp_bed = 0.651  !kJ/kg C
!    ElseIf (BedNum == 7) Then
!        Cp_bed = 0.567  !kJ/kg C
!    ElseIf (BedNum == 8) Then
!        Cp_bed = 0.8    !kJ/kg C
!    ElseIf (BedNum == 9) Then
!        Cp_bed = 1.105 !kJ/kg C
!    End If
!End Function
!
!!Bed material cost, $/kg
!Double Precision Function cost_bed(BedNum) !BedNum:  1 = taconite
!!                                   2 = calcium carbonate
!!                                   3 = gravel
!!                                   4 = marble
!!                                   5 = limestone
!!                                   6 = other
!!                                   7 = carbon steel
!!                                   8 = sand
!!                                   9 = quartzite
!implicit none
!Integer BedNum
!
!    If (BedNum == 1) Then
!        cost_bed = 0.999
!    ElseIf (BedNum == 2) Then
!        cost_bed = 0.999
!    ElseIf (BedNum == 3) Then
!        cost_bed = 0.999
!    ElseIf (BedNum == 4) Then
!        cost_bed = 0.999
!    ElseIf (BedNum == 5) Then
!        cost_bed = 0.999
!    ElseIf (BedNum == 6) Then
!        cost_bed = 0.999
!    ElseIf (BedNum == 7) Then
!        cost_bed = 0.999
!    ElseIf (BedNum == 8) Then
!        cost_bed = 0.142
!    ElseIf (BedNum == 9) Then
!        cost_bed = 0.142
!    End If
!End Function
!
!
!
