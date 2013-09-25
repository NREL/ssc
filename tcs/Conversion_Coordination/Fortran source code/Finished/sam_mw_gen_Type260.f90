SUBROUTINE TYPE260 (TIME,XIN,OUT,T,DTDT,PAR,INFO,ICNTRL,*)
!************************************************************************
! Object: Generic solar model
! Simulation Studio Model: Type260
! 
! Author: Michael J. Wagner
! Date:	 June 22, 2010

! COPYRIGHT 2010 NATIONAL RENEWABLE ENERGY LABORATORY

!Deck file parameters

! Doc. tables updated 7/1/2010 - MJW
!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Parameters
!    1| LU_arr                           | [int] Array file logical unit                                     | none             | none             
!    2| latitude                        | Site latitude                                                    | deg              | rad              
!    3| theta_stow                       | Solar elevation angle at which the solar field stops operating    | deg              | rad              
!    4| theta_dep                        | Solar elevation angle at which the solar field begins operating   | deg              | rad              
!    5| interp_arr                       | [int] Interpolate the array or find nearest neighbor? (1=interp,2=no)| none             | none             
!    6| rad_type                         | [int] Solar resource radiation type (1=DNI,2=horiz.beam,3=tot.horiz)| none             | none             
!    7| SM                               | Solar multiple                                                    | none             | none             
!    8| T_sfdes                          | Solar field design point temperature (dry bulb)                   | C                | K                
!    9| Irr_des                          | Irradiation design point                                          | W/m2             | W/m2             
!   10| eta_opt_soil                     | Soiling optical derate factor                                     | none             | none             
!   11| eta_opt_gen                      | General/other optical derate                                      | none             | none             
!   12| f_sfhl_ref                       | Reference solar field thermal loss fraction                       | MW/MWcap         | none             
!   13| sfhlQ0                           | Irr-based solar field thermal loss adjustment - constant coef.    | none             | none             
!   14| sfhlQ1                           | Irr-based solar field thermal loss adjustment - linear coef.      | MWt^(-1)         | MWt^(-1)         
!   15| sfhlQ2                           | Irr-based solar field thermal loss adjustment - quadratic coef.   | MWt^(-2)         | MWt^(-2)         
!   16| sfhlQ3                           | Irr-based solar field thermal loss adjustment - cubic coef.       | MWt^(-3)         | MWt^(-3)         
!   17| sfhlT0                           | Temp.-based solar field thermal loss adjustment - constant coef.  | none             | none             
!   18| sfhlT1                           | Temp.-based solar field thermal loss adjustment - linear coef.    | C^(-1)           | C^(-1)           
!   19| sfhlT2                           | Temp.-based solar field thermal loss adjustment - quadratic coef. | C^(-2)           | C^(-2)           
!   20| sfhlT3                           | Temp.-based solar field thermal loss adjustment - cubic coef.     | C^(-3)           | C^(-3)           
!   21| sfhlV0                           | Wind-based solar field thermal loss adjustment - constant coef.   | none             | none             
!   22| sfhlV1                           | Wind-based solar field thermal loss adjustment - linear coef.     | m/s^(-1)         | m/s^(-1)         
!   23| sfhlV2                           | Wind-based solar field thermal loss adjustment - quadratic coef.  | m/s^(-2)         | m/s^(-2)         
!   24| sfhlV3                           | Wind-based solar field thermal loss adjustment - cubic coef.      | m/s^(-2)         | m/s^(-3)         
!   25| Qsf_des                          | Solar field thermal production at design                          | MWt              | MWt              
!   26| W_des                            | Design power cycle gross output                                   | MWe              | MWe              
!   27| eta_des                          | Design power cycle gross efficiency                               | none             | none             
!   28| f_Wmax                           | Maximum over-design power cycle operation fraction                | none             | none             
!   29| f_Wmin                           | Minimum part-load power cycle operation fraction                  | none             | none             
!   30| f_startup                        | Equivalent full-load hours required for power system startup      | hours            | hours            
!   31| eta_LHV                          | Fossil backup lower heating value efficiency                      | none             | none             
!   32| etaQ0                            | Part-load power conversion efficiency adjustment - constant       | none             | none             
!   33| etaQ1                            | Part-load power conversion efficiency adjustment - linear         | MWt^(-1)         | MWt^(-1)         
!   34| etaQ2                            | Part-load power conversion efficiency adjustment - quadratic      | MWt^(-2)         | MWt^(-2)         
!   35| etaQ3                            | Part-load power conversion efficiency adjustment - cubic          | MWt^(-3)         | MWt^(-3)         
!   36| etaQ4                            | Part-load power conversion efficiency adjustment - quartic        | MWt^(-4)         | MWt^(-4)         
!   37| etaT0                            | Temp.-based power conversion efficiency adjustment - constant     | none             | none             
!   38| etaT1                            | Temp.-based power conversion efficiency adjustment - linear       | C^(-1)           | C^(-1)           
!   39| etaT2                            | Temp.-based power conversion efficiency adjustment - quadratic    | C^(-2)           | C^(-2)           
!   40| etaT3                            | Temp.-based power conversion efficiency adjustment - cubic        | C^(-3)           | C^(-3)           
!   41| etaT4                            | Temp.-based power conversion efficiency adjustment - quartic      | C^(-4)           | C^(-4)           
!   42| T_pcdes                          | Power conversion reference temperature                            | C                | K                
!   43| PC_T_corr                        | Power conversion temperature correction mode (1=wetb, 2=dryb)     | none             | none             
!   44| f_Wpar_fixed                     | Fixed capacity-based parasitic loss fraction                      | MWe/MWcap        | none             
!   45| f_Wpar_prod                      | Production-based parasitic loss fraction                          | MWe/MWe          | none             
!   46| Wpar_prodQ0                      | Part-load production parasitic adjustment - constant coef.        | none             | none             
!   47| Wpar_prodQ1                      | Part-load production parasitic adjustment - linear coef.          | MWe^(-1)         | MWe^(-1)         
!   48| Wpar_prodQ2                      | Part-load production parasitic adjustment - quadratic coef.       | MWe^(-2)         | MWe^(-2)         
!   49| Wpar_prodQ3                      | Part-load production parasitic adjustment - cubic coef.           | MWe^(-3)         | MWe^(-3)         
!   50| Wpar_prodT0                      | Temp.-based production parasitic adjustment - constant coef.      | none             | none             
!   51| Wpar_prodT1                      | Temp.-based production parasitic adjustment - linear coef.        | C^(-1)           | C^(-1)           
!   52| Wpar_prodT2                      | Temp.-based production parasitic adjustment - quadratic coef.     | C^(-2)           | C^(-2)           
!   53| Wpar_prodT3                      | Temp.-based production parasitic adjustment - cubic coef.         | C^(-3)           | C^(-3)           
!   54| FLhrs_TES                        | Equivalent full-load hours of storage                             | hours            | hours            
!   55| f_charge                         | Storage charging energy derate                                    | none             | none             
!   56| f_disch                          | Storage discharging energy derate                                 | none             | none             
!   57| f_ETES_0                         | Initial fractional charge level of thermal storage (0..1)         | none             | none             
!   58| f_teshl_ref                      | Reference heat loss from storage per max stored capacity          | kWt/MWhr-stored  | MWt/MWhr-stored  
!   59| teshlX0                          | Charge-based thermal loss adjustment - constant coef.             | none             | none             
!   60| teshlX1                          | Charge-based thermal loss adjustment - linear coef.               | MWhr-stored^(-1) | MWhr-stored^(-1) 
!   61| teshlX2                          | Charge-based thermal loss adjustment - quadratic coef.            | MWhr-stored^(-2) | MWhr-stored^(-2) 
!   62| teshlX3                          | Charge-based thermal loss adjustment - cubic coef.                | MWhr-stored^(-3) | MWhr-stored^(-3) 
!   63| teshlT0                          | Temp.-based thermal loss adjustment - constant coef.              | none             | none             
!   64| teshlT1                          | Temp.-based thermal loss adjustment - linear coef.                | C^(-1)           | C^(-1)           
!   65| teshlT2                          | Temp.-based thermal loss adjustment - quadratic coef.             | C^(-2)           | C^(-2)           
!   66| teshlT3                          | Temp.-based thermal loss adjustment - cubic coef.                 | C^(-3)           | C^(-3)           
!   67| nTOD                             | [int] Number of time-of-dispatch periods in the dispatch schedule | none             | none             

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Inputs
!    1| I_bn                             | Beam-normal (DNI) irradiation                                     | kJ/hr.m^2        | W/m2             
!    2| I_bh                             | Beam-horizontal irradiation                                       | kJ/hr.m^2        | W/m2             
!    3| I_toth                           | Total horizontal irradiation                                      | kJ/hr.m^2        | W/m2             
!    4| shift                            | Shift in longitude from local standard meridian                   | deg              | rad              
!    5| T_db                             | Ambient dry-bulb temperature                                      | C                | K                
!    6| T_wb                             | Ambient wet-bulb temperature                                      | C                | K                
!    7| TOD                              | Time-of-dispatch period                                           | none             | none             
!    8| V_wind                           | Wind velocity                                                     | m/s              | m/s              

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Outputs
!    1| Irr_used                         | Irradiation value used in simulation                              | W/m2             | W/m2             
!    2| Hour_of_day                      | Hour of the day                                                   | hour             | hour             
!    3| Day_of_year                      | Day of the year                                                   | day              | day              
!    4| Declination                      | Declination angle                                                 | deg              | rad              
!    5| SolTime                          | [hour] Solar time of the day                                      | hour             | hour             
!    6| HrAngle                          | Hour angle                                                        | deg              | rad              
!    7| SolAlt                           | Solar elevation angle                                             | deg              | rad              
!    8| SolAz                            | Solar azimuth angle (-180..180, 0deg=South)                       | deg              | rad              
!    9| SF_OptEff                        | Solar field optical efficiency                                    | none             | none             
!   10| f_SFhl_(Qdni)                    | Solar field load-based thermal loss correction                    | none             | none             
!   11| f_SFhl_(Tamb)                    | Solar field temp.-based thermal loss correction                   | none             | none             
!   12| f_SFhl_(V_wind)                  | Solar field wind-based thermal loss correction                    | none             | none             
!   13| Qsf_HL                           | Solar field thermal losses                                        | MWt              | MWt              
!   14| Qsf                              | Solar field delivered thermal power                               | MWt              | MWt              
!   15| Q_Irr                            | Qdni - Solar incident energy, before all losses                   | MWt              | MWt              
!   16| PbMode                           | Power conversion mode                                             | none             | none             
!   17| PbStartF                         | Flag indicating power system startup                              | none             | none             
!   18| Qtpb                             | Thermal energy to the power conversion system                     | MWt              | MWt              
!   19| QTurSu                           | Power conversion startup energy                                   | MWt              | MWt              
!   20| Q_to_TES                         | Thermal energy into storage                                       | MWt              | MWt              
!   21| Q_from_TES                       | Thermal energy from storage                                       | MWt              | MWt              
!   22| E_in_TES                         | Energy in storage                                                 | MWt-hr           | MWt-hr           
!   23| Q_TES_HL                         | Thermal losses from storage                                       | MWt              | MWt              
!   24| Q_dump_TESFull                   | Dumped energy  exceeding storage charge level max                 | MWt              | MWt              
!   25| Q_dump_TESchg                    | Dumped energy exceeding exceeding storage charge rate             | MWt              | MWt              
!   26| Q_dump_uMin                      | Dumped energy from falling below min. operation fraction          | MWt              | MWt              
!   27| Q_dump_tot                       | Total dumped energy                                               | MWt              | MWt              
!   28| Q_fossil                         | thermal energy supplied from aux firing                           | MWt              | MWt              
!   29| Q_gas                            | Energy content of fuel required to supply Qfos                    | MWt              | MWt              
!   30| f_effpc_(Qtpb)                   | Load-based conversion efficiency correction                       | none             | none             
!   31| f_effpc_(Tamb)                   | Temp-based conversion efficiency correction                       | none             | none             
!   32| Eff_pc                           | Adjusted power conversion efficiency                              | none             | none             
!   33| E_gross_solar                    | Power produced from the solar component                           | MWe              | MWe              
!   34| E_gross_fossil                   | Power produced from the fossil component                          | MWe              | MWe              
!   35| E_gross                          | Total gross power production                                      | MWe              | MWe              
!   36| E_par_fixed                      | Fixed parasitic losses                                            | MWe              | MWe              
!   37| E_par_prod                       | Production-based parasitic losses                                 | MWe              | MWe              
!   38| E_parasit                        | Total parasitic losses                                            | MWe              | MWe              
!   39| E_par_Online                     | Online parasitics                                                 | MWe              | MWe              
!   40| E_par_Offline                    | Offline parasitics                                                | MWe              | MWe              
!   41| Enet                             | Net electric output                                               | MWe              | MWe              
      


!-----------------------------------------------------------------------------------------------------------------------
!    TRNSYS acess functions (allow to acess TIME etc.) 
USE TrnsysConstants
USE TrnsysFunctions
use CSPGeneric_tools    !module includes arrays eta(nazm,nzen), azms(nazm), zens(nzen). Also includes functions/subs for this type
!-----------------------------------------------------------------------------------------------------------------------
! required by the multi-dll version of TRNSYS
!DEC$ATTRIBUTES DLLEXPORT :: TYPE260

implicit none 

!TRNSYS declarations
integer*4:: info(15), iunit, itype, icntrl
integer*4,parameter::np=103,ni=8,nout=41,nd=0,ns=3

!Dimension the TRNSYS variables
real(8)::xin(ni),out(nout),par(np),stored(ns),T(nd),dtdt(nd) 

!-----------------------------------------------------------------------------------------------------------------------


!---- User variable declarations----------------------------------------------------------------------------------------

!parameters
integer:: LU_arr, interp_arr, rad_type, nTOD

real(8):: latitude, theta_stow, theta_dep, SM, T_sfdes, Irr_des, eta_opt_soil, eta_opt_gen, f_sfhl_ref, sfhlQ0, &
          sfhlQ1, sfhlQ2, sfhlQ3, sfhlT0, sfhlT1, sfhlT2, sfhlT3, sfhlV0, sfhlV1, sfhlV2, sfhlV3, Qsf_des, W_des, &
          eta_des, f_Wmax, f_Wmin, f_startup, eta_LHV, etaQ0, etaQ1, etaQ2, etaQ3, etaQ4, etaT0, etaT1, etaT2, &
          etaT3, etaT4, T_pcdes, PC_T_corr, f_Wpar_fixed, f_Wpar_prod, Wpar_prodQ0, Wpar_prodQ1, Wpar_prodQ2, Wpar_prodQ3, &
          Wpar_prodT0, Wpar_prodT1, Wpar_prodT2, Wpar_prodT3, FLhrs_TES, f_charge, f_disch, f_ETES_0, f_teshl_ref, &
          teshlX0, teshlX1, teshlX2, teshlX3, teshlT0, teshlT1, teshlT2, teshlT3

!inputs
real(8):: I_bn, I_bh, I_toth, shift, T_db, T_wb, V_wind
real(8),allocatable:: DISwS(:), DISwoS(:), Qdisp(:), Fdisp(:), DIS(:)
!locals
integer:: i, j, k, nrow, ncol, TOD, up1, row_def, col_def, nazm, nzen 
real(8):: time, hour, day_of_year, B, EOT, dec, solarnoon, tsnow, dt, dephr1, dephr2, dephr3, deptime, stwhr1, stwhr2, stwhr3,&
          stwtime, hrA, hrB, ftrack, midtrack, stdtime, solartime, omega, solaralt, solaraz, eta_opt, eta_arr, eta_opt_ref, &
          f_Qsf, Irr, sfhlQ, sfhlT, sfhlV, f_sfhl, Qsf, PbMode, PbMode0, Qtpb, QturSu, TurSuE, TurSuE0, Q_des, Qttmin, Qttmax, &
          Qdump, Qttes, Qftes, Qteshl, PTSmax, EtesA, Etesmax, QtesFull, Etes, Etes0, f_EtesAve, f_teshlX, f_teshlT, Qmin, &
          PbStartF, PFSMax, Qfos, Qgas, QN, TN, etaQ, etaT, etaAdj, Wgr, Wgrsol, Wpar_fixed, Wpar_prodQ, Wpar_prodT, Wpar_prod, &
          Wpar, Enet, WparOn, WparOff, Qsfhl

!constants
real(8),parameter:: pi = 3.14159265
! --- Messages for this Type -------------------------------------------------------------------------------------------
character:: temp*1000
character(len=maxMessageLength) :: aString, msg(10)


! --- Initial call to detect the TRNSYS version for which this Type is written -----------------------------------------
if (info(7) == -2) then
    info(12) = 16   ! This component is a TRNSYS 16 Type
    return 1
endif


! --- Very last call in simulation -------------------------------------------------------------------------------------

if (info(8) == -1) then
    !deallocate all of the local arrays
    deallocate(DISwS, DISwoS, Qdisp, Fdisp, DIS)
    !deallocate the CSPGeneric_tools arrays
    if(allocated(eta)) deallocate(eta,azms,zens,azms1d,zens1d)

    return 1    ! Exit 
endif



! --- Post-convergence call --------------------------------------------------------------------------------------------
if (info(13) == 1) then

    !******************************************************************
    ! Set the system state values for the next timestep 
    !******************************************************************
    stored(1) = Etes        !Energy in thermal storage
    stored(2) = PbMode      !Power block mode
    stored(3) = TurSuE      !Turbine startup energy remaining
    
    call setStorageVars(stored,nS,INFO)

    return 1    ! Exit - End of the routine for post-convergence calls
    
endif


! --- Second call in simulation: initialization call (not a simulation call) -------------------------------------------
!---- read static parameters---
if (info(7) == -1) then
    !******************************************************************************************************************************
    !   Parameters
    !******************************************************************************************************************************
    dt = getSimulationTimeStep()
    i=up1(0)    !initialize parameter counting function
    !-----------------------------------------------------------------------------------------------------------------------------------------
    !Variable    |                     | Description                                                           | Input units      | Output units     
    !-----------------------------------------------------------------------------------------------------------------------------------------
    LU_arr      = int(par(up1(1)))     ! [int] Array file logical unit                                         | none             | none             
    latitude   = par(up1(1))*0.017453 ! Site latitude                                                        | deg              | rad              
    theta_stow  = par(up1(1))*0.017453 ! Solar elevation angle at which the solar field stops operating        | deg              | rad              
    theta_dep   = par(up1(1))*0.017453 ! Solar elevation angle at which the solar field begins operating       | deg              | rad              
    interp_arr  = int(par(up1(1)))     ! [int] Interpolate the array or find nearest neighbor? (1=interp,2=no) | none             | none             
    rad_type    = int(par(up1(1)))     ! [int] Solar resource radiation type (1=DNI,2=horiz.beam,3=tot.horiz)  | none             | none             
    SM          = par(up1(1))          ! Solar multiple                                                        | none             | none             
    T_sfdes     = par(up1(1))+273.15   ! Solar field design point temperature (dry bulb)                       | degC             | K                
    Irr_des     = par(up1(1))          ! Irradiation design point                                              | W/m2             | W/m2             
    eta_opt_soil= par(up1(1))          ! Soiling optical derate factor                                         | none             | none             
    eta_opt_gen = par(up1(1))          ! General/other optical derate                                          | none             | none             
    f_sfhl_ref  = par(up1(1))          ! Reference solar field thermal loss fraction                           | MW/MWcap         | none             
    sfhlQ0      = par(up1(1))          ! Irr-based solar field thermal loss adjustment - constant coef.        | none             | none             
    sfhlQ1      = par(up1(1))          ! Irr-based solar field thermal loss adjustment - linear coef.          | MWt^(-1)         | MWt^(-1)         
    sfhlQ2      = par(up1(1))          ! Irr-based solar field thermal loss adjustment - quadratic coef.       | MWt^(-2)         | MWt^(-2)         
    sfhlQ3      = par(up1(1))          ! Irr-based solar field thermal loss adjustment - cubic coef.           | MWt^(-3)         | MWt^(-3)         
    sfhlT0      = par(up1(1))          ! Temp.-based solar field thermal loss adjustment - constant coef.      | none             | none             
    sfhlT1      = par(up1(1))          ! Temp.-based solar field thermal loss adjustment - linear coef.        | degC^(-1)        | degC^(-1)        
    sfhlT2      = par(up1(1))          ! Temp.-based solar field thermal loss adjustment - quadratic coef.     | degC^(-2)        | degC^(-2)        
    sfhlT3      = par(up1(1))          ! Temp.-based solar field thermal loss adjustment - cubic coef.         | degC^(-3)        | degC^(-3)        
    sfhlV0      = par(up1(1))          ! Wind-based solar field thermal loss adjustment - constant coef.       | none             | none             
    sfhlV1      = par(up1(1))          ! Wind-based solar field thermal loss adjustment - linear coef.         | m/s^(-1)         | m/s^(-1)         
    sfhlV2      = par(up1(1))          ! Wind-based solar field thermal loss adjustment - quadratic coef.      | m/s^(-2)         | m/s^(-2)         
    sfhlV3      = par(up1(1))          ! Wind-based solar field thermal loss adjustment - cubic coef.          | m/s^(-2)         | m/s^(-3)         
    Qsf_des     = par(up1(1))          ! Solar field thermal production at design                              | MWt              | MWt              
    W_des       = par(up1(1))          ! Design power cycle gross output                                       | MWe              | MWe              
    eta_des     = par(up1(1))          ! Design power cycle gross efficiency                                   | none             | none             
    f_Wmax      = par(up1(1))          ! Maximum over-design power cycle operation fraction                    | none             | none             
    f_Wmin      = par(up1(1))          ! Minimum part-load power cycle operation fraction                      | none             | none             
    f_startup   = par(up1(1))          ! Equivalent full-load hours required for power system startup          | hours            | hours            
    eta_LHV     = par(up1(1))          ! Fossil backup lower heating value efficiency                          | none             | none             
    etaQ0       = par(up1(1))          ! Part-load power conversion efficiency adjustment - constant           | none             | none             
    etaQ1       = par(up1(1))          ! Part-load power conversion efficiency adjustment - linear             | MWt^(-1)         | MWt^(-1)         
    etaQ2       = par(up1(1))          ! Part-load power conversion efficiency adjustment - quadratic          | MWt^(-2)         | MWt^(-2)         
    etaQ3       = par(up1(1))          ! Part-load power conversion efficiency adjustment - cubic              | MWt^(-3)         | MWt^(-3)         
    etaQ4       = par(up1(1))          ! Part-load power conversion efficiency adjustment - quartic            | MWt^(-4)         | MWt^(-4)         
    etaT0       = par(up1(1))          ! Temp.-based power conversion efficiency adjustment - constant         | none             | none             
    etaT1       = par(up1(1))          ! Temp.-based power conversion efficiency adjustment - linear           | degC^(-1)        | degC^(-1)        
    etaT2       = par(up1(1))          ! Temp.-based power conversion efficiency adjustment - quadratic        | degC^(-2)        | degC^(-2)        
    etaT3       = par(up1(1))          ! Temp.-based power conversion efficiency adjustment - cubic            | degC^(-3)        | degC^(-3)        
    etaT4       = par(up1(1))          ! Temp.-based power conversion efficiency adjustment - quartic          | degC^(-4)        | degC^(-4)        
    T_pcdes     = par(up1(1))+273.15   ! Power conversion reference temperature                                | degC             | K                
    PC_T_corr   = par(up1(1))          ! Power conversion temperature correction mode (1=wetb, 2=dryb)         | none             | none             
    f_Wpar_fixed= par(up1(1))          ! Fixed capacity-based parasitic loss fraction                          | MWe/MWcap        | none             
    f_Wpar_prod = par(up1(1))          ! Production-based parasitic loss fraction                              | MWe/MWe          | none             
    Wpar_prodQ0 = par(up1(1))          ! Part-load production parasitic adjustment - constant coef.            | none             | none             
    Wpar_prodQ1 = par(up1(1))          ! Part-load production parasitic adjustment - linear coef.              | MWe^(-1)         | MWe^(-1)         
    Wpar_prodQ2 = par(up1(1))          ! Part-load production parasitic adjustment - quadratic coef.           | MWe^(-2)         | MWe^(-2)         
    Wpar_prodQ3 = par(up1(1))          ! Part-load production parasitic adjustment - cubic coef.               | MWe^(-3)         | MWe^(-3)         
    Wpar_prodT0 = par(up1(1))          ! Temp.-based production parasitic adjustment - constant coef.          | none             | none             
    Wpar_prodT1 = par(up1(1))          ! Temp.-based production parasitic adjustment - linear coef.            | degC^(-1)        | degC^(-1)        
    Wpar_prodT2 = par(up1(1))          ! Temp.-based production parasitic adjustment - quadratic coef.         | degC^(-2)        | degC^(-2)        
    Wpar_prodT3 = par(up1(1))          ! Temp.-based production parasitic adjustment - cubic coef.             | degC^(-3)        | degC^(-3)        
    FLhrs_TES   = par(up1(1))          ! Equivalent full-load hours of storage                                 | hours            | hours            
    f_charge    = par(up1(1))          ! Storage charging energy derate                                        | none             | none             
    f_disch     = par(up1(1))          ! Storage discharging energy derate                                     | none             | none             
    f_ETES_0    = par(up1(1))          ! Initial fractional charge level of thermal storage (0..1)             | none             | none             
    f_teshl_ref = par(up1(1))/1000.    ! Reference heat loss from storage per max stored capacity              | kWt/MWhr-stored  | MWt/MWhr-stored  
    teshlX0     = par(up1(1))          ! Charge-based thermal loss adjustment - constant coef.                 | none             | none             
    teshlX1     = par(up1(1))          ! Charge-based thermal loss adjustment - linear coef.                   | MWhr-stored^(-1) | MWhr-stored^(-1) 
    teshlX2     = par(up1(1))          ! Charge-based thermal loss adjustment - quadratic coef.                | MWhr-stored^(-2) | MWhr-stored^(-2) 
    teshlX3     = par(up1(1))          ! Charge-based thermal loss adjustment - cubic coef.                    | MWhr-stored^(-3) | MWhr-stored^(-3) 
    teshlT0     = par(up1(1))          ! Temp.-based thermal loss adjustment - constant coef.                  | none             | none             
    teshlT1     = par(up1(1))          ! Temp.-based thermal loss adjustment - linear coef.                    | degC^(-1)        | degC^(-1)        
    teshlT2     = par(up1(1))          ! Temp.-based thermal loss adjustment - quadratic coef.                 | degC^(-2)        | degC^(-2)        
    teshlT3     = par(up1(1))          ! Temp.-based thermal loss adjustment - cubic coef.                     | degC^(-3)        | degC^(-3)        
    nTOD        = int(par(up1(1)))     ! [int] Number of time-of-dispatch periods in the dispatch schedule     | none             | none             

    !Power block design power
    Q_des = W_des/eta_des   ![MWt]
    !Maximum energy in storage
    Etesmax = FLhrs_TES*Q_des   ![MW-hr]

    !Allocate space for the dispatch fractions according to the value nTOD read in
    allocate(DISwS(nTOD), DISwoS(nTOD), Qdisp(nTOD), Fdisp(nTOD), DIS(nTOD))
        DISwS=0.d0; DISwoS=0.d0; Qdisp=0.d0; Fdisp=0.d0; DIS=0.d0;
    do i=1,nTOD
        DISwS(i) = par(up1(1))*Etesmax  !Time-of-dispatch control for with-solar conditions
        DISwoS(i) = par(up1(1))*Etesmax !Time-of-dispatch control for without-solar conditions
        Qdisp(i) = par(up1(1))*Q_des    !Thermal power conversion control level
        Fdisp(i) = par(up1(1))*Q_des    !Fossil backup control level
    enddo

    !Call to the subroutine that reads in the efficiency azimuth/elevation array (in the CSPGeneric_Tools module)
    call load_gen_table(LU_arr)

    !---- Calculate the "normalized" design-point thermal power. 
    ! This value will be multiplied by the radiation, optical efficiency, and heat loss efficiency to obtain
    ! the solar field thermal output for each timestep.
    !eta_opt_ref = maxval(maxval(eta, 2))*eta_opt_soil*eta_opt_gen
    !---Design point values---  
    !MJW 10.3.11 Calculate the design point efficiency based on the solar position at solstice noon, rather than maxval of the table
    omega = 0.d0 !solar noon
    dec = 23.45d0*pi/180.d0 !declination at summer solstice
    !Solar altitude at noon on the summer solstice
    SolarAlt = asin(sin(dec)*sin(latitude)+cos(latitude)*cos(dec)*cos(omega))
    eta_opt_ref = eta_opt_soil*eta_opt_gen*azzen_interp(0.d0, dmax1(pi/2.-SolarAlt, 0.d0), 1, LU_arr)
    f_Qsf = Qsf_des/(Irr_des*eta_opt_ref*(1.-f_sfhl_ref))    ![MWt/([W/m2] * [-] * [-])]
    
    !Calculate min/max turbine operation rates
    Qttmin = Q_des*f_Wmin
    Qttmax = Q_des*f_Wmax
    !Calculate max TES charging/discharging rates based on solar multiple
    PTSmax = Q_des*dmax1((SM - 1.), 1.d0)
    PFSMax = PTSmax/f_disch*f_Wmax

    !---- Initialize outputs
    out(1:nout) = 0.d0
    info(6)=nout
    call typeck(1,info,nI,nP,nD)

    !---- Set storage array size 
    CALL setStorageSize(nS,INFO)

    !call messages(-1,"The number of inputs in the TYPE250 system geometry data file did not match requirements",'FATAL',INFO(1),INFO(2))

    !Set initial storage values
    stored(1:ns) = 0.d0
    stored(1) = f_ETES_0*Etesmax    ![MW-hr] Initial value in thermal storage. This keeps track of energy in thermal storage, or Etes
    stored(2) = 0.d0                ![-] initial value of power block operation mode PbMode
    stored(3) = f_startup*Q_des     ![MW-hr] Initial value of turbine startup energy TurSuE
    call setStorageVars(stored,nS,info)

    return 1
endif


!******************************************************************************************************************************
!               Time-dependent conditions
!******************************************************************************************************************************
!Variable                        |  Description                                      |  Input units    |  Local units
!---------------------------------------------------------------------------------------------------------------------
I_bn    = XIN(1)/3.6             !  Beam-normal (DNI) irradiation                    |  kJ/hr.m^2      |  W/m2
I_bh    = XIN(2)/3.6             !  Beam-horizontal irradiation                      |  kJ/hr.m^2      |  W/m2
I_toth  = XIN(3)/3.6             !  Total horizontal irradiation                     |  kJ/hr.m^2      |  W/m2
shift   = XIN(4)*0.0174532925    !  Shift in longitude from local standard meridian  |  deg            |  rad
T_db    = XIN(5)+273.15          !  Ambient dry-bulb temperature                     |  C              |  K
T_wb    = XIN(6)+273.15          !  Ambient wet-bulb temperature                     |  C              |  K
TOD     = int(XIN(7))            !  Time-of-dispatch period                          |  none           |  none
V_wind  = XIN(8)                 !  Wind velocity                                    |  m/s            |  m/s

!Choose which irradiation source will be used
select case(rad_type)
case(1)
    Irr = I_bn
case(2)
    Irr = I_bh
case(3)
    Irr = I_toth
end select
!Choose which dispatch set will be used
if(Irr>0.) then
    DIS(:) = DISwS(:)
else
    DIS(:) = DISwoS(:)
endif
!*********** End of input section *********************************************************************************************


!******* Read in stored variables every timestep*******
call getStorageVars(stored,nS,info)
Etes0 = stored(1)        !Energy in thermal storage
PbMode0 = stored(2)       !Power block mode
TurSuE0 = stored(3)       !Turbine startup energy remaining



!------------------------------------------------------------------------------------------------------------
!       Solar field calculations
!------------------------------------------------------------------------------------------------------------
hour = dmod(time,24.)       !hour of the day (1..24)
day_of_year = ceiling(time/24.)  !Day of the year
! Duffie & Beckman 1.5.3b
B = (day_of_year-1)*360.0/365.0*pi/180.0
! Eqn of time in minutes
EOT = 229.2 * (0.000075 + 0.001868 * COS(B) - 0.032077 * SIN(B)	- 0.014615 * COS(B*2.0) - 0.04089 * SIN(B*2.0))
! Declination in radians (Duffie & Beckman 1.6.1)
Dec = 23.45 * SIN(360.0*(284.0+day_of_year)/365.0*pi/180.0) * pi/180.0
! Solar Noon and time in hours
SolarNoon = 12. - ((shift)*180.0/pi) / 15.0 - EOT / 60.0

if ((hour - int(hour)) == 0.00) then
	TSnow = 1.0
else
	TSnow = (hour - floor(hour))/dt + 1.
endif

! Deploy & stow times in hours
! Calculations modified by MJW 11/13/2009 to correct bug
theta_dep = dmax1(theta_dep,1.e-6)
DepHr1 = COS(latitude) / TAN(theta_dep)
DepHr2 = -TAN(Dec) * SIN(latitude) / TAN(theta_dep)
DepHr3 = sign(1.d0,tan(pi-theta_dep))*ACOS((DepHr1*DepHr2 + Sqrt(DepHr1*DepHr1-DepHr2*DepHr2+1.0)) / (DepHr1 * DepHr1 + 1.0)) * 180.0 / pi / 15.0
DepTime = SolarNoon + DepHr3

theta_stow = dmax1(theta_stow,1.e-6)
StwHr1 = COS(latitude) / TAN(theta_stow)
StwHr2 = -TAN(Dec) * SIN(latitude) / TAN(theta_stow)
StwHr3 = sign(1.d0,tan(pi-theta_stow))*ACOS((StwHr1*StwHr2 + Sqrt(StwHr1*StwHr1-StwHr2*StwHr2+1.0)) / (StwHr1 * StwHr1 + 1.0)) * 180.0 / pi / 15.0
StwTime = SolarNoon + StwHr3

! Ftrack is the fraction of the time period that the field is tracking. MidTrack is time at midpoint of operation
HrA = hour-dt
HrB = hour

! Solar field operates
if ((HrB > DepTime) .AND. (HrA < StwTime)) then
    ! solar field deploys during time period
    if (HrA < DepTime) then
        Ftrack = (HrB - DepTime) *dt
        MidTrack = HrB - Ftrack * 0.5 *dt
    ! Solar field stows during time period
    elseif (HrB > StwTime) then
        Ftrack = (StwTime - HrA) *dt
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
SolarAlt = asin(sin(dec)*sin(latitude)+cos(latitude)*cos(dec)*cos(omega))
SolarAz = sign(1.d0, omega)*abs(acos(dmin1(1.d0,(cos(pi/2.-SolarAlt)*sin(latitude)-sin(dec))/(sin(pi/2.-SolarAlt)*cos(latitude)))))

!Get the current optical efficiency
eta_arr = dmax1(azzen_interp(SolarAz, dmax1(pi/2.-SolarAlt, 0.d0), interp_arr, LU_arr)*Ftrack, 0.d0)  !mjw 7.25.11 limit zenith to <90, otherwise the interpolation error message gets called during night hours.
eta_opt = eta_arr*eta_opt_soil*eta_opt_gen

!Evaluate solar feild thermal efficiency derate
sfhlQ = sfhlQ0 + sfhlQ1*(Irr/Irr_des) + sfhlQ2*(Irr/Irr_des)**2 + sfhlQ3*(Irr/Irr_des)**3
sfhlT = sfhlT0 + sfhlT1*(T_db - T_sfdes) + sfhlT2*(T_db - T_sfdes)**2 + sfhlT3*(T_db - T_sfdes)**3
sfhlV = sfhlV0 + sfhlV1*V_wind + sfhlV2*V_wind**2 + sfhlV3*V_wind**3
f_sfhl = 1.d0 - f_sfhl_ref * sfhlQ * sfhlT * sfhlV  !This ratio indicates the sf thermal efficiency
Qsfhl = f_Qsf*Irr*(1.-f_sfhl)   ![MWt]

!Calculate the total solar field thermal output 
Qsf = f_Qsf * f_sfhl * eta_opt * Irr    ![MWt]

!------------------------------------------------------------------------------------------------------------
!       Dispatch calculations
!------------------------------------------------------------------------------------------------------------
! initialize outputs to 0
Qttes      = 0     !| Energy to Thermal Storage 
Qftes      = 0     !| Energy from Thermal Storage
Etes       = 0     !| Energy in Thermal Storage
Qteshl     = 0     !| Energy losses from Thermal Storage
Qtpb       = 0     !| Energy to the Power Block
QtesFull   = 0     !| Energy dumped because the thermal storage is full
Qmin       = 0     !| Indicator of being below minimum operation level
Qdump      = 0     !| The amount of energy dumped (more than turbine and storage)
QTurSu     = 0     !| The energy needed to startup the turbine
PbStartF   = 0     !| is 1 during the period when powerblock starts up otherwise 0
TurSuE = TurSuE0   !| Turbine startup energy for this timestep is equal to the remaining previous energy

!--------Plant dispatch strategy--------------------
IF (FLhrs_TES <= 0.) Then ! No Storage
    If ((PbMode0==0).or.(PbMode0==1)) Then ! if plant is not already operating in last timestep
        If (Qsf>0) Then
            If (Qsf>(TurSuE/dt)) Then !  Starts plant as exceeds startup energy needed
                Qtpb = Qsf - TurSuE/dt
                QTurSu = TurSuE/dt
                PbMode = 2      !Power block mode.. 2=starting up
                PbStartF = 1    !Flag indicating whether the power block starts up in this time period
            Else !  Plant starting up but not enough energy to make it run - will probably finish in the next timestep
                Qtpb = 0
                TurSuE = TurSuE0 - Qsf*dt
                QTurSu = Qsf
                PbMode = 1
                PbStartF = 0
            End If
        Else ! No solar field output so still need same amount of energy as before and nothing changes
            TurSuE = f_startup * Q_des
            PbMode = 0
            PbStartF = 0
        End If
    Else ! if the powerblock mode is already 2 (running previous timestep)
        If (Qsf>0) Then     ! Plant operated last hour and this one
            Qtpb = Qsf          ! all power goes from solar field to the powerblock
            PbMode = 2          ! powerblock continuing to operate
            PbStartF = 0        ! powerblock did not start during this timestep
        Else                   !  Plant operated last hour but not this one
            Qtpb = 0            ! No energy to the powerblock
            PbMode = 0          ! turned off powrblock
            PbStartF = 0        ! it didn't start this timeperiod 
            TurSuE = TurSuE0
        End If
    End If

    ! following happens no matter what state the powerblock was in previously      
    If (Qtpb < Qttmin) Then ! Energy to powerblock less than the minimum that the turbine can run at
        Qmin =  Qtpb         ! The minimum energy (less than the minimum)
        Qtpb = 0             ! Energy to PB is now 0
        PbMode = 0           ! PB turned off
    End If

    If (Qtpb > Qttmax) Then   ! Energy to powerblock greater than what the PB can handle (max)
        Qdump =  Qtpb - Qttmax ! The energy dumped 
        Qtpb = Qttmax          ! the energy to the PB is exactly the maximum
    End If

else    !With thermal storage    

    !--initialize values
    QTurSu = 0.d0
    PbStartF = 0.d0
    Qdump = 0.d0
    Qftes = 0.d0

    if (PbMode0 == 0) then       
    !**********************************************************
    !******        plant is not already operating         *****
    !**********************************************************
        !---Start plant if any of the following conditions are met---
        ! 1.) Solar field output > 0
        !       a.) AND energy in TES exceeds w/ solar TOD fraction
        !       b.) AND energy in TES plus solar field output exceeds turbine fraction
        ! OR
        ! 2.) Solar field is off
        !       a.) AND energy in TES exceeds w/o solar TOD
        !       b.) AND energy in TES exceeds turbine fraction
        ! OR
        ! 3.) Solar field energy exceeds maximum TES charging rate
        !------------------------------------------------------------
        EtesA = dmax1(0.d0, Etes0 - DIS(TOD))
        if ((Qsf + EtesA >= Qdisp(TOD)) .or. (Qsf > PTSmax)) then

            ! Assumes Operator started plant during previous time period
            ! But TRNSYS cannot do this, so start-up energy is deducted during current timestep.     
            PbMode = 1
            QTurSu = f_startup * Q_des /dt

            Qtpb = Qdisp(TOD)       ! set the energy to powerblock equal to the load for this TOU period

            if (Qsf>Qtpb)  then             ! if solar field output is greater than what the necessary load ?
                Qttes = Qsf - Qtpb           ! the extra goes to thermal storage
                Qftes = QTurSu               ! Use the energy from thermal storage to startup the power cycle
                if (Qttes>PTSmax) then       ! if q to thermal storage exceeds thermal storage max rate Added 9-10-02
                    Qdump = Qttes - PTSmax   ! then dump the excess for this period Added 9-10-02
                    Qttes = PTSmax
                endif                              
            else ! Qsf less than the powerblock requirement
                Qttes = 0.d0
                Qftes = QTurSu + (1 -  Qsf /  Qtpb) * dmin1(PFSmax, Q_des)
                if (Qftes>PFSmax) Qftes = PFSmax
                Qtpb = Qsf + (1 -  Qsf /  Qtpb) * dmin1(PFSmax, Q_des)
            endif
                
            Etes = Etes0 - QTurSu + (Qsf - Qtpb) * dt   ! thermal storage energy is initial + what was left 
            PbMode = 2   ! powerblock is now running
            PbStartF = 1 ! the powerblock turns on during this timeperiod.
        else !Store energy not enough stored to start plant
            Qttes = Qsf ! everything goes to thermal storage
            Qftes = 0   ! nothing from thermal storage
            Etes = Etes0 + Qttes * dt  
            Qtpb = 0
        endif

    else       
    !**********************************************************
    !******        plant is already operating             *****
    !**********************************************************

        if ((Qsf + dmax1(0.d0,Etes0-DIS(TOD)) /dt) > Qdisp(TOD)) then ! If there is sufficient energy to operate at dispatch target output

            Qtpb = Qdisp(TOD) 

            If (Qsf>Qtpb) Then 
            Qttes = Qsf - Qtpb !extra from what is needed put in thermal storage
                Qftes = 0
                If (Qttes>PTSmax) Then  !check if max power rate to storage exceeded
                    Qdump = Qttes - PTSmax ! if so, dump extra 
                    Qttes = PTSmax
                endif
            else ! solar field outptu less than what powerblock needs
                Qttes = 0
                Qftes = (1. - Qsf / Qtpb) * dmin1(PFSmax, Q_des)
                if (Qftes>PFSMax) Qftes = dmin1(PFSmax, Q_des)
                Qtpb = Qftes + Qsf
            End If

            Etes = Etes0 + (Qsf - Qtpb - Qdump) *dt  ! energy of thermal storage is the extra

            ! Check to see if throwing away energy 
            If ( (Etes>Etesmax) .AND. (Qtpb<Qttmax) ) Then ! QTTMAX (MWt) - power to turbine max
                If ((Etes - Etesmax)/dt < Qttmax - Qtpb) Then
                    Qtpb = Qtpb + (Etes - Etesmax) /dt
                    Etes = Etesmax
                Else
                    Etes = Etes - (Qttmax - Qtpb) * dt  ! should this be Etes0 instead of Etes on RHS ??
                    Qtpb = Qttmax
                End If
                Qttes = Qsf - Qtpb
            End If

        Else  !Empties tes to dispatch level if above min load level

            If ((Qsf + dmax1(0.,Etes0-DIS(TOD)) * dt) > Qttmin) Then  
                Qftes = dmax1(0.,Etes0-DIS(TOD)) * dt
                Qtpb = Qsf + Qftes
                Qttes = 0
                Etes = Etes0 - Qftes
            Else
                Qtpb = 0
                Qftes = 0
                Qttes = Qsf
                Etes = Etes0 + Qttes * dt
            End If
        End If
    End If

    If (Qtpb>0) Then
        PbMode = 2
    Else
        PbMode = 0
    End If

    !---Calculate TES thermal losses ---
    ! First do the charge-based losses. Use the average charge over the timestep
    f_EtesAve = dmax1((Etes + Etes0)/2./Etesmax, 0.d0)
    f_teshlX = teshlX0 + teshlX1*f_EtesAve + teshlX2*f_EtesAve**2 + teshlX3**3     !Charge adjustment factor
    f_teshlT = teshlT0 + teshlT1*(T_sfdes - T_db) + teshlT2*(T_sfdes - T_db)**2 + teshlT3*(T_sfdes - T_db)**3
    !thermal storage heat losses adjusted by charge level and ambient temp.
    Qteshl = f_teshl_ref*Etesmax*f_teshlX*f_teshlT 

    Etes = dmax1(Etes - Qteshl*dt, 0.d0) ! Adjust the energy in thermal storage according to TES thermal losses

    If (Etes>Etesmax) Then ! trying to put in more than storage can handle
        QtesFull = (Etes - Etesmax)/dt  !this is the amount dumped when storage is completely full
        Etes = Etesmax
        Qttes = Qttes - QtesFull
    Else
        QtesFull = 0 ! nothing is dumped if not overfilled
    End If

    ! Check min and max on turbine
    If (Qtpb<Qttmin) Then
        Qmin = Qtpb
        Qtpb = 0
        PbMode = 0
    Else
        Qmin = 0
    End If

    PbMode0 = PbMode
endif

!------------------------------------------------------------------------------------------------------------
!       Fossil backup
!------------------------------------------------------------------------------------------------------------
if (Qsf < Fdisp(TOD)) then      ! if the solar provided is less than the level stipulated in the fossil control
    Qfos = Fdisp(TOD) - Qsf     ! then the fossil used is the fossil control value minus what's provided by the solar field
    Qgas = Qfos / eta_LHV       ! Calculate the required fossil heat content based on the LHV efficiency
Else
    Qfos = 0
    Qgas = 0
End If
!Adjust the power block energy based on additional fossil backup
Qtpb = Qtpb + Qfos
    
!------------------------------------------------------------------------------------------------------------
!       Power block calculations
!------------------------------------------------------------------------------------------------------------
QN = Qtpb/Q_des             !The normalized thermal energy flow
if(PC_T_corr==1.) then      !Select the dry or wet bulb temperature as the driving difference
    TN = T_wb - T_pcdes
else
    TN = T_db - T_pcdes
endif
!Calculate the load-based and temperature-based efficiency correction factors
etaQ = etaQ0 + etaQ1*QN + etaQ2*QN**2 + etaQ3*QN**3 + etaQ4*QN**4
etaT = etaT0 + etaT1*TN + etaT2*TN**2 + etaT3*TN**3 + etaT4*TN**4
etaAdj = eta_des * etaQ * etaT  !Adjusted power conversion efficiency

if(Qtpb <= 0.) etaAdj = 0.d0  !Set conversion efficiency to zero when the power block isn't operating

!Calculate the gross power
Wgr = Qtpb * etaAdj
!Keep track of what portion is from solar
Wgrsol = (Qtpb - Qfos)*etaAdj


!------------------------------------------------------------------------------------------------------------
!       Parasitics
!------------------------------------------------------------------------------------------------------------
Wpar_fixed = f_Wpar_fixed * W_des       !Fixed parasitic loss based on plant capacity
!Production-based parasitic loss
Wpar_prodQ = Wpar_prodQ0 + Wpar_prodQ1*QN + Wpar_prodQ2*QN**2 + Wpar_prodQ3*QN**3   !Power block part-load correction factor
Wpar_prodT = Wpar_prodT0 + Wpar_prodT1*TN + Wpar_prodT2*TN**2 + Wpar_prodT3*TN**3   !Temperature correction factor
Wpar_prod = f_Wpar_prod * Wgr * Wpar_prodQ * Wpar_prodT 

Wpar = Wpar_fixed + Wpar_prod   !Total parasitic loss

!Keep track of online/offline parasitics
if(Wgr > 0.) then
    WparOn = Wpar
    WparOff = 0.
else
    WparOn = 0.
    WparOff = Wpar
endif

!---Calculate net energy output (Enet<-->Wnet) ---
Enet = Wgr - Wpar


!-------------Set the outputs and return------------------------------------------------------------------------------------
i = up1(0) 
out(up1(1)) = Irr                   ![W/m2] Irradiation value used in simulation
out(up1(1)) = hour                  ![hours] Hour of the day
out(up1(1)) = day_of_year           ![day] Day of the year
out(up1(1)) = Dec*57.2958           ![deg] Declination angle
out(up1(1)) = SolarTime             ![hour] Solar time of the day
out(up1(1)) = omega*57.2958         ![deg] Hour angle
out(up1(1)) = dmax1(SolarAlt*57.2958, 0.d0) ![deg] Solar elevation angle
out(up1(1)) = SolarAz*57.2958       ![deg] Solar azimuth angle (-180..180, 0deg=South)
out(up1(1)) = eta_opt               ![-] Solar field optical efficiency
out(up1(1)) = sfhlQ                 ![-] Solar field load-based thermal loss correction
out(up1(1)) = sfhlT                 ![-] Solar field temp.-based thermal loss correction
out(up1(1)) = sfhlV                 ![-] Solar field wind-based thermal loss correction
out(up1(1)) = f_Qsf*(1.-f_sfhl)*Irr ![MWt] Solar field thermal losses
out(up1(1)) = Qsf                   ![MWt] Solar field delivered thermal power
out(up1(1)) = f_Qsf*Irr             ![MWt] Qdni - Solar incident energy, before all losses
out(up1(1)) = PbMode                ![-] Power conversion mode
out(up1(1)) = PBStartF              ![-] Flag indicating power system startup
out(up1(1)) = Qtpb                  ![MWt] Thermal energy to the power conversion system
out(up1(1)) = QTurSu                ![MWt] Power conversion startup energy
out(up1(1)) = Qttes                 ![MWt] Thermal energy into storage
out(up1(1)) = Qftes                 ![MWt] Thermal energy from storage
out(up1(1)) = Etes                  ![MWt-hr] Energy in storage
out(up1(1)) = Qteshl                ![MWt] Thermal losses from storage
out(up1(1)) = QtesFull              ![MWt] Dumped thermal energy from exceeding storage charge level limitations
out(up1(1)) = Qdump                 ![MWt] Dumped thermal energy from exceeding storage charge rate limitations
out(up1(1)) = Qmin                  ![MWt] Dumped thermal energy from falling below min. operation fraction
out(up1(1)) = QtesFull + Qdump + Qmin ![MWt] Total dumped energy
out(up1(1)) = Qfos                  ![MWt] thermal energy supplied from aux firing
out(up1(1)) = Qgas                  ![MWt] Energy content of fuel required to supply Qfos
out(up1(1)) = etaQ                  ![-] Load-based conversion efficiency correction
out(up1(1)) = etaT                  ![-] Temp-based conversion efficiency correction
out(up1(1)) = etaAdj                ![-] Adjusted power conversion efficiency
out(up1(1)) = Wgrsol                ![MWe] Power produced from the solar component
out(up1(1)) = Wgr - Wgrsol          ![MWe] Power produced from the fossil component
out(up1(1)) = Wgr                   ![MWe] Total gross power production
out(up1(1)) = Wpar_fixed            ![MWe] Fixed parasitic losses
out(up1(1)) = Wpar_prod             ![MWe] Production-based parasitic losses
out(up1(1)) = Wpar                  ![MWe] Total parasitic losses
out(up1(1)) = WparOn                ![MWe] Online parasitics
out(up1(1)) = WparOff               ![MWe] Offline parasitics
out(up1(1)) = Enet                  ![MWe] Net electric output

return 1

end subroutine
