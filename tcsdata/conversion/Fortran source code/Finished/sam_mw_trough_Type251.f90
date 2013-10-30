SUBROUTINE TYPE251 (TIME,XIN,OUT,T,DTDT,PAR,INFO,ICNTRL,*)
!************************************************************************
! Object: Detailed parabolic trough plant controller
! Simulation Studio Model: Type251
! 
! Author: Michael J. Wagner
! Editor: Michael J. Wagner
! Date:	 December 12, 2009
! Modified: April 21, 2011

! COPYRIGHT 2010 NATIONAL RENEWABLE ENERGY LABORATORY

! Doc. tables updated 2011-11-01 - MJW
!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Parameters
!    1| LU_fl                            | Fluid property file logical unit                                  | none             | none             
!    2| TSHOURS                          | Equivalent full-load thermal storage hours                        | hr               | hr               
!    3| NUMTOU                           | Number of time-of-use periods                                     | none             | none             
!    4| is_hx                            | 1=yes, 0=no                                                       | none             | none             
!    5| dT_hot                           | Hot side heat exchanger approach temp                             | C                | C                
!    6| dT_cold                          | Cold side heat exchanger approach temp                            | C                | C                
!    7| hx_config                        | Heat exchanger configuration                                      | none             | none             
!    8| q_max_aux                        | Maximum heat rate of the auxiliary heater                         | MWt              | Wt               
!    9| LHV_eff                          | Fuel LHV efficiency (0..1)                                        | none             | none             
!   10| T_set_aux                        | Aux heater outlet temperature set point                           | C                | K                
!   11| store_fluid                      | material number for the storage fluid (see type229)               | none             | none             
!   12| V_tank_hot_ini                   | Initial hot tank fluid volume                                     | m3               | m3               
!   13| T_tank_hot_ini                   | Initial hot tank fluid temperature                                | C                | K                
!   14| T_tank_cold_ini                  | Initial cold tank fluid temperature                               | C                | K                
!   15| vol_tank                         | Total tank volume, including the unusable volume of HTF at the bottom| m3               | m3               
!   16| h_tank                           | Total height of the tank (height of HTF when tank is full)        | m                | m                
!   17| h_tank_min                       | Minimum htf height in the storage tank                            | m                | m                
!   18| u_tank                           | Loss coefficient from the tank                                    | W/m2.K           | W/m2.K           
!   19| tank_pairs                       | Number of equivalent tank pairs                                   | none             | none             
!   20| cold_tank_Thtr                   | Minimum allowable cold tank HTF temp before aux heater turns on   | C                | K                
!   21| hot_tank_Thtr                    | Minimum allowable hot tank HTF temp before aux heater turns on    | C                | K                
!   22| tank_max_heat                    | Rated heater capacity for tank heating                            | MW               | MW               
!   23| eta_heater_tank                  | Conversion efficiency for electric heater                         | none             | none             
!   24| field_fluid                      | material number for the collector field (see type229)             | none             | none             
!   25| T_field_in_des                   | Field design inlet temperature                                    | C                | K                
!   26| T_field_out_des                  | Field loop outlet design temperature                              | C                | K                
!   27| q_pb_design                      | Design heat input to the power block                              | MWt              | Wt               
!   28| W_pb_design                      | Rated plant capacity                                              | MWe              | We               
!   29| cycle_max_fraction               | Maximum turbine over design operation fraction                    | none             | none             
!   30| cycle_cutoff_frac                | Minimum turbine operation fraction before shutdown                | none             | none             
!   31| solarm                           | Solar multiple                                                    | none             | none             
!   32| PB_pump_coef                     | Pumping power required to move 1kg of HTF through power block flow loop| kW/kg            | kW/kg            
!   33| TES_pump_coef                    | Pumping power required to move 1kg of HTF through storage flow loop| kW/kg            | kW/kg            
!   34| PB_fixed_par                     | fraction of rated gross power consumed at all hours of the year   | none             | none             
!   35| BOP_parVal                       | Balance of plant parasitic                                        | MWe/MWcap        | MWe/MWcap        
!   36| BOP_parPF                        | Balance of plant parasitic multiplier factor                      | none             | none             
!   37| BOP_par0                         | Balance of plant parasitic polynomial constant                    | none             | none             
!   38| BOP_par1                         | Balance of plant parasitic polynomial linear term                 | none             | none             
!   39| BOP_par2                         | Balance of plant parasitic polynomial quadratic term              | none             | none             
!   40| Aux_parVal                       | Aux heater/boiler parasitic                                       | MWe/MWcap        | MWe/MWcap        
!   41| Aux_parPF                        | Aux heater/boiler parasitic multiplier factor                     | none             | none             
!   42| Aur_par0                         | Aux heater/boiler parasitic polynomial constant                   | none             | none             
!   43| Aur_par1                         | Aux heater/boiler parasitic polynomial linear term                | none             | none             
!   44| Aur_par2                         | Aux heater/boiler parasitic polynomial quadratic term             | none             | none             
!   45| T_startup                        | Startup temperature (same as field startup)                       | C                | K                
!   46| fossil_mode                      | Operation mode for the fossil backup {1=Normal, 2=topping}        | none             | none             
!   47| fthr_ok                          | Does the defocus control allow partial defocusing?                | none             | none             
!   48| nSCA                             | The number of SCA's in a single loop                              | none             | none             
!   49| I_bn_des                         | Design point irradiation value                                    | W/m2             | W/m2             
!   50| fc_on                            | DNI forecasting enabled                                           | none             | none             
!   51| q_sby_frac                       | Fraction of thermal power required for standby mode               | none             | none             
!   52| T_init_standby                   | Maximum allowable time for power cycle standby operation          | hr               | hr               
!   53| TES_type                         | Flag indicating thermal storage type {1=2-tank, 2=TC}             | none             | none             
! ...... Loop for 1..(Number of TOU periods) .......
!   54| TSLOGIC_A                        | Dispatch logic without solar                                      | none             | none             
!   55| TSLOGIC_B                        | Dispatch logic with solar                                         | none             | none             
!   56| TSLOGIC_C                        | Dispatch logic - Turbine load fraction                            | none             | none             
!   57| FFRAC                            | Fossil dispatch logic                                             | none             | none             
! ......//

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Inputs
!    1| I_bn                             | Beam normal radiation (input kJ/m2-hr)                            | kJ/m2.hr         | W/m2             
!    2| TOUPeriod                        | Time of use period                                                | none             | none             
!    3| m_dot_field                      | Flow rate from the field                                          | kg/hr            | kg/s             
!    4| m_dot_htf_ref                    | Reference HTF flow rate at design conditions                      | kg/hr            | kg/s             
!    5| T_field_out                      | HTF Temperature from the field                                    | C                | K                
!    6| T_pb_out                         | Fluid temperature from the power block                            | C                | K                
!    7| T_amb                            | Ambient temperature                                               | C                | K                
!    8| m_pb_demand                      | Demand htf flow from the power block                              | kg/hr            | kg/s             
!    9| q_startup                        | Startup energy reported by the collector field                    | MWt              | MWt              
!   10| dnifc                            | Forecast DNI                                                      | W/m2             | W/m2             
!   11| m_disch_avail_in                 | Available TES discharge flow rate from external model             | kg/hr            | kg/s             
!   12| T_disch_avail_in                 | Temperature of available TES discharge flow                       | C                | K                
!   13| m_charge_avail_in                | Available TES charge flow rate from external model                | kg/hr            | kg/s             
!   14| T_charge_avail_in                | Temperature of available TES charge flow                          | C                | K                
!   15| T_TES_cold                       | Actual cold temperature from TES external model                   | C                | K                
!   16| T_TES_hot                        | Actual hot temperature from TES external model                    | C                | K                
!   17| Q_TES_hl_in                      | Reported thermal losses from TES external model                   | kJ/hr            | MWt              

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Outputs
!    1| defocus                          | Defocus instigated by control decisions                           | none             | none             
!    2| cycle_pl_control                 | Part-load control flag - used by Type224                          | none             | none             
!    3| standby_control                  | Standby control flag - used by Type224                            | none             | none             
!    4| m_dot_pb                         | Mass flow rate to the power block                                 | kg/hr            | kg/s             
!    5| T_pb_in                          | HTF Temperature to the power block                                | C                | K                
!    6| T_field_in                       | HTF temperature into the collector field header                   | C                | K                
!    7| no name                          | Mass flow rate of the collector side storage loop                 | kg/hr            | kg/hr            
!    8| no name                          | Mass flow rate of the tank side storage loop                      | kg/hr            | kg/hr            
!    9| Ts_hot                           | Fluid temperature of the hot HTF in the collector loop            | C                | K                
!   10| Ts_cold                          | Fluid temperature of the cold HTF in the collector loop           | C                | K                
!   11| T_tank_hot_in                    | Hot tank inlet temperature                                        | C                | K                
!   12| T_tank_cold_in                   | Cold tank inlet temperature                                       | C                | K                
!   13| v_tank_hotf                      | Available Hot tank HTF volume at the end of the timestep          | m3               | m3               
!   14| T_tank_hotf                      | Hot tank stored HTF temperature at the end of the timestep        | C                | K                
!   15| v_tank_coldf                     | Available Cold tank HTF volume at the end of the timestep         | m3               | m3               
!   16| T_tank_coldf                     | Cold tank stored HTF temperature at the end of the timestep       | C                | K                
!   17| no name                          | Total parasitic power required for tank freeze protection         | MWe              | MWe              
!   18| m_dot_aux                        | Auxiliary heater mass flow rate                                   | kg/hr            | kg/s             
!   19| q_aux_fluid                      | Thermal energy provided to the fluid passing through the aux heater| MWt              | MWt              
!   20| q_aux_fuel                       | Heat content of fuel required to provide aux firing               | MMBTU            | MMBTU            
!   21| no name                          | Total accounted htf volume in storage                             | m3               | m3               
!   22| hx_eff                           | Heat exchanger effectiveness                                      | none             | none             
!   23| m_tank_hotf                      | mass of total fluid in the hot tank                               | kg               | kg               
!   24| m_tank_coldf                     | mass of total fluid in the cold tank                              | kg               | kg               
!   25| no name                          | total mass in the storage system                                  | kg               | kg               
!   26| htf_pump_power                   | pumping power for storage, power block loops                      | MWe              | MWe              
!   27| BOP_par                          | parasitic power as a function of power block load                 | MWe              | MWe              
!   28| no name                          | Fixed parasitic power losses.. for every hour of operation        | MWe              | MWe              
!   29| Aux_par                          | Parasitic power associated with operation of the aux boiler       | MWe              | MWe              
!   30| q_pb                             | Thermal energy to the power block                                 | MWt              | Wt               
!   31| q_loss_tank                      | Tank thermal losses                                               | MWt              | MWt              
!   32| q_to_tes                         | Thermal energy into storage (positive in, negative out)           | MWt              | MWt              
!   33| mode                             | Control strategy operation mode                                   | none             | none             

!-----------------------------------------------------------------------------------------------------------------------
!    TRNSYS acess functions (allow to acess TIME etc.) 
USE TrnsysConstants
USE TrnsysFunctions
use global_props

!-----------------------------------------------------------------------------------------------------------------------
! required by the multi-dll version of TRNSYS
!DEC$ATTRIBUTES DLLEXPORT :: TYPE251

implicit none 

!TRNSYS declarations
real(8):: time
integer*4:: info(15), iunit, itype, icntrl
integer*4,parameter::np=95,ni=17,nout=38,nd=0,ns=10

!Dimension the TRNSYS variables
real(8)::xin(ni),out(nout),par(np),stored(ns),T(nd),dtdt(nd) 

!-----------------------------------------------------------------------------------------------------------------------


!---- User variable declarations----------------------------------------------------------------------------------------
!    PARAMETERS
real(8) :: TSHOURS, cycle_max_fraction, cycle_cutoff_frac,&
           V_tank_hot_ini, T_tank_hot_ini, V_tank_cold_ini, T_tank_cold_ini, T_field_in_des, T_field_out_des, &
           LHV_eff, T_set_aux, vol_tank, h_tank, u_tank, tank_pairs, hot_tank_Thtr, cold_tank_Thtr, &
           eta_heater_tank, q_max_aux, q_pb_design, solarm, dT_hot, dT_cold, store_fluid, field_fluid, h_tank_min, tank_max_heat,&
           W_pb_design, PB_pump_coef, TES_pump_coef, nsca
real(8),allocatable:: TSLOGIC_A(:), TSLOGIC_B(:), TSLOGIC_C(:), FFRAC(:), TSLOGIC(:), TSELECT(:)
integer :: NUMTOU, hx_config, LU_fl, fossil_mode
logical :: is_hx, fthr_ok, fc_on, called_TC

! INPUTS
real(8) :: I_bn, m_dot_field, m_dot_htf_ref, T_field_out, T_pb_out, T_amb, m_pb_demand, q_startup
integer :: TOUPeriod


! OUTPUTS

! Locals
real(8) :: ms_charge_avail, ms_disch_avail, V_tank_hot0, T_tank_hot0, V_tank_cold0, T_tank_cold0, m_dot_aux_avail,&
           density, specheat, dt, c_htf_aux, c_htf_chg, c_htf_dis, c_htf_fld, c_htf_pb, T_tank_hot_in, T_tank_hot_out, T_tank_cold_in, T_tank_cold_out, &
           m_tank_disch_avail, m_tank_charge_avail, Ts_hot, Ts_cold, duty, hx_eff_des, hx_ua, m_avail_tot, hx_eff, q_hx,&
           V_tank_active, qs_disch_avail, qs_charge_avail, qaux_avail, qfield_avail, q_int, T_int, m_int, ms_disch, ms_charge, &
           m_dot_aux, q_aux, m_dot_pb, T_pb_in, defocus, cycle_pl_control, standby_control, T_field_in, T_field_inX, &
           err, tol, q_pb_demand, q_pb_demandX, q_pb, m_tank_charge, m_tank_disch, T_tank_hot_ave, vol_hot_tank_ave,&
           q_loss_hot_tank, T_tank_hotf, V_tank_hotf, q_tank_hot_htr, T_tank_cold_ave, vol_cold_tank_ave, q_loss_cold_tank, &
           T_tank_coldf, v_tank_coldf, q_tank_cold_htr, T_aux_act, q_aux_fluid, q_aux_fuel, demand_var, &
           m_tank_hotf, m_tank_coldf, m_tank_hot0, m_tank_cold0, v_tank_hotfx, v_tank_coldfx, &
           errt, T_tank_hotX, T_tank_coldX, V_tank_hotA, V_tank_coldA, rho_tank_coldX, rho_tank_hotX, msghx1, msghx2, msghx3, msgiter,&
           derr, errx, defocusx, htf_pump_power, PB_fixed_par, BOP_parVal, BOP_parPF, BOP_par0, BOP_par1, BOP_par2, BOP_par,&
           Aux_parVal, Aux_parPF, Aux_par0, Aux_par1, Aux_par2, Aux_par, T_startup, c_pb_ref, m_dot_pb_max, m_dot_dum,&
           ms_charge_max, ms_disch_max, q_demand_aux, c_tes_ave, q_to_tes, ccoef, dnifc, pb_on, pb_on0, I_bn_des,&
           defocus0, q_sby, q_sby_frac, T_standby, T_standby0, T_init_standby, &
           m_disch_avail_in, T_disch_avail_in, m_charge_avail_in, T_charge_avail_in, T_TES_cold, T_TES_hot, Q_tes_hl_in, &
           q_TC_demand, q_sby_storage, q_charge_demand, f_storage
integer :: mode, up1, mode0, iter, itert, TES_type
character::message(3)*300
integer :: i, dumi, tempmode
REAL(8):: PAR_TC(23),XIN_TC(9),OUT_TC(12)

!-----------------------------------------------------------------------------------------------------------------------

! --- Initial call to detect the TRNSYS version for which this Type is written -----------------------------------------
if (info(7) .eq. -2) then
    info(12) = 16   ! This component is a TRNSYS 16 Type
    return 1
endif


! --- Very last call in simulation -------------------------------------------------------------------------------------

if (info(8) == -1) then
    
    IF(TES_type==2)THEN
        CALL PackedBed(TIME,XIN_TC,OUT_TC,PAR_TC,INFO)
    ENDIF

    if(allocated(TSLOGIC_A)) deallocate(TSLOGIC_A, TSLOGIC_B, TSLOGIC_C, FFRAC, TSLOGIC, TSELECT)

    return 1    ! Exit 
    
endif


! --- Post-convergence call --------------------------------------------------------------------------------------------
if (info(13) == 1) then

    !******************************************************************
    ! Set the system state values for the next timestep 
    !******************************************************************
    stored(1) = v_tank_hotf     ![m3] Final hot tank HTF volume
    stored(2) = T_tank_hotf     ![K] Final hot tank HTF temperature
    stored(3) = v_tank_coldf    ![m3] Final cold tank HTF volume
    stored(4) = T_tank_coldf    ![K] Final cold tank HTF temperature
    stored(5) = dble(mode)      !operation mode
    stored(6) = m_tank_hotf     ![kg] hot tank available mass
    stored(7) = m_tank_coldf    ![kg] cold tank available mass
    stored(8) = pb_on           ![-] is the power block running?
    stored(9)= 1.d0             ![-] Defocus always resets to 1.
    stored(10)= T_standby       ![hr] Remaining standby operation time
                    
    call setStorageVars(stored,nS,INFO)
    
    !Print out error messages
    if(msghx1 /= 0.) then
        call messages(-1,trim(message(msghx1)),"Fatal",info(1),info(2))
    endif
    if(msghx2 /= 0.) then
        call messages(-1,trim(message(msghx2)),"Warning",info(1),info(2))
    endif
    if(msghx3 /= 0.) then
        call messages(-1,trim(message(msghx3)),"Warning",info(1),info(2))
    endif
    if(msgiter /= 0.) then
        call messages(-1,trim(message(msgiter)),"Warning",info(1),info(2))
    endif

    !MJW 9.8.2010 :: Call the property range check subroutine with the inlet and outlet HTF temps to make sure they're in the valid range
    call check_htf(field_fluid,T_pb_in)
    call check_htf(field_fluid,T_field_in)
    !MJW 10.21.2010 :: Only call storage check for systems with storage
    if(TSHOURS>0.d0) then
        call check_htf(store_fluid,T_tank_hot_out)
        call check_htf(store_fluid,T_tank_cold_out)
    endif
    
    IF(TES_type==2)THEN
        CALL PackedBed(TIME,XIN_TC,OUT_TC,PAR_TC,INFO)
    ENDIF
    
    return 1    ! Exit - End of the routine for post-convergence calls
    
endif


! --- Second call in simulation: initialization call (not a simulation call) -------------------------------------------
if (info(7) .eq. -1) then
    message(:) = ''
    !set all the messages for this type
    message(1) = "An non-physical solution was found for the heat exchanger size. Please correct temperature specifications."  !Fatal
    message(2) = "The simulation has encountered a non-physical condition in the heat exchanger. Please adjust design values." !Warning
    !message(3) reserved for iteration. Value is set at iteration calculation.  Warning. 

    dt = getSimulationTimeStep()*3600.  ![s]       

    !initialize incremental counter function. This increments once each time
    !it is called with arg 1. handy for reading in parameters without having to manually
    !assign each one a parameter number
    dumi=up1(0)
    
    !---- read static parameters---
    !Logic and control parameters
    LU_fl = int(par(up1(1)))                !Fluid property file logical unit
    TSHOURS=PAR(up1(1))                ![hr] Equivalent full-load thermal storage hours  mjw 6.23. was reading in as INT type
    NUMTOU=int(PAR(up1(1)))                 ![-] Number of time-of-use periods
    
    !heat exchanger parameters
    ![-] Does the system use a heat exchanger?
    if(par(up1(1)) == 1.) then; is_hx=.true.; else; is_hx = .false.; endif 
    dT_hot = par(up1(1))                    ![K] Hot side heat exchanger approach temp
    dT_cold = par(up1(1))                   ![K] Cold side heat exchanger approach temp
    hx_config = int(par(up1(1)))            ![-] Heat exchanger configuration
                                            !{1=Same fluid, 2=Counter-flow, 3=Parallel-flow, 4=Cross-flow}
    !Auxiliary fossil heater parameters
    q_max_aux = par(up1(1))*1.e6            ![Wt] Maximum heat rate of the auxiliary heater
    LHV_eff = par(up1(1))                   ![-] Fuel LHV efficiency (0..1)
    T_set_aux = par(up1(1))+273.15          ![K] Aux heater outlet temperature set point
    
    !Storage tank parameters
    store_fluid = par(up1(1))               ![-] material number for the storage fluid (see type229)
    V_tank_hot_ini = par(up1(1))            ![m3] Initial hot tank fluid volume
    T_tank_hot_ini = par(up1(1))+273.15     ![K] Initial hot tank fluid temperature   
    T_tank_cold_ini = par(up1(1))+273.15    ![K] Initial cold tank fluid temperature
    vol_tank = par(up1(1))                  ![m3] Total tank volume, including the unusable volume of HTF at the bottom
    h_tank = par(up1(1))                    ![m] Total height of the tank (height of HTF when tank is full)
    h_tank_min = par(up1(1))                ![m] Minimum htf height in the storage tank
    u_tank = par(up1(1))                    ![W/m2-K] Loss coefficient from the tank
    tank_pairs = par(up1(1))                ![-] Number of equivalent tank pairs
    cold_tank_Thtr = par(up1(1))+273.15     ![K] Minimum allowable cold tank fluid temperature before auxiliary heater turns on
    hot_tank_Thtr = par(up1(1))+273.15      ![K] Minimum allowable hot tank fluid temperature before auxiliary heater turns on
    tank_max_heat = par(up1(1))             ![MW] Rated heater capacity for tank heating
    eta_heater_tank = par(up1(1))           ![-] Conversion efficiency for electric heater
    
    V_tank_cold_ini = vol_tank - V_tank_hot_ini   ![m3] Initial cold tank fluid volume
    V_tank_active = vol_tank*(1.-2.*h_tank_min/h_tank) ![m3] Active tank volume.. that is, volume above the 
                                                       !minimum fluid level and below the maximum level
    
    !Field parameters
    field_fluid = par(up1(1))               ![-] material number for the collector field (see type229)
    T_field_in_des = par(up1(1))+273.15     ![K] Field design inlet temperature
    T_field_out_des = par(up1(1))+273.15    ![K] Field loop outlet design temperature
    !Check to make sure a fluid mismatch isn't present
    if(.not.is_hx) store_fluid = field_fluid

    !Power block parameters
    q_pb_design = par(up1(1))*1.e6          ![Wt] Design heat input to the power block
    W_pb_design = par(up1(1))*1.e6          ![We] Design electrical output of the power block
    cycle_max_fraction = par(up1(1))        ![-] Maximum turbine over design operation fraction
    cycle_cutoff_frac = par(up1(1))         ![-] Minimum turbine operation fraction
    solarm = par(up1(1))                    ![-] Solar multiple
    PB_pump_coef = par(up1(1))              ![kW/kg] Flow through power block pump power coeffcient
    TES_pump_coef = par(up1(1))             ![kW/kg] Flow through storage pump power coefficient\
    PB_fixed_par = par(up1(1))              ![-] Fraction.. fixed power block losses. Constant throughout year.
    BOP_parVal = par(up1(1))                ![MWe/MWcap] Balance of plant parasitic 
    BOP_parPF = par(up1(1))                 ![-] Balance of plant parasitic multiplier factor
    BOP_par0 = par(up1(1))                  !Balance of plant parasitic polynomial constant
    BOP_par1 = par(up1(1))                  !Balance of plant parasitic polynomial linear term
    BOP_par2 = par(up1(1))                  !Balance of plant parasitic polynomial quadratic term
    Aux_parVal = par(up1(1))                ![MWe/MWcap] Aux heater/boiler parasitic 
    Aux_parPF = par(up1(1))                 ![-] Aux heater/boiler parasitic multiplier factor
    Aux_par0 = par(up1(1))                  !Aux heater/boiler parasitic polynomial constant
    Aux_par1 = par(up1(1))                  !Aux heater/boiler parasitic polynomial linear term
    Aux_par2 = par(up1(1))                  !Aux heater/boiler parasitic polynomial quadratic term
    T_startup = par(up1(1))+273.15          ![K] Startup temperature
    fossil_mode = jfix(par(up1(1))+0.1d0)   ![-] Fossil mode of operation {1=normal, 2="topping"}
    fthr_ok = .false.
    if(par(up1(1))==1.d0) fthr_ok = .true.  ![-] Does the defocus strategy allow for partial defocusing? MJW 12.6.2010
    nSCA = par(up1(1))                      ![-] The number of SCA's in a single loop MJW 12.6.2010
    I_bn_des = par(up1(1))                  ![W/m2] Solar irradiation at design
    fc_on = .false.
    if(par(up1(1))==1.d0) fc_on = .true.    ![-] Forecasting capability enabled? (1=Yes, 0=no)
    q_sby_frac = par(up1(1))                ![-] Fraction of design-point power block energy required for standby operation
    q_sby = q_sby_frac*q_pb_design
    T_init_standby = par(up1(1))*3600.d0    ![hr] Maximum allowable time for power cycle standby operation
    TES_type = int(par(up1(1)))             ![-] Type of thermal storage (1=2-tank, 2=thermocline)
    
    PAR_TC(1) = store_fluid                 ![-] HTF fluid number
    PAR_TC(2) = h_tank                      ![m] Height of rock bed storage tank
    PAR_TC(3) = (vol_tank/tank_pairs)/ h_tank           ![m^2] Cross-sectional area of tank
    PAR_TC(4) = par(up1(1))                 ![-] Filler material number
    PAR_TC(5) = u_tank * 3.6                ![kJ/hr-m2-K] Loss Coefficient -> convert from (W/m2-K)
    
    !Set top and bottom loss coefficients to tank coefficient for SAM
    PAR_TC(6) = PAR_TC(5)
    PAR_TC(7) = PAR_TC(5)
    !PAR_TC(6) = par(up1(1))                 ![kJ/hr-m2-K] Top Surface Loss Coefficient
    !PAR_TC(7) = par(up1(1))                 ![kJ/hr-m2-K] Bottom Surface Loss Coefficient
    !*********************************************************************************
    
    PAR_TC(8) = par(up1(1))                 ![-] Void Fraction
    
    !Set bottom thermal mass capacitance factor multiplier to 1
    PAR_TC(9) = 1.d0
    !PAR_TC(9) = par(up1(1))                 ![-] Bottom thermal mass capacitance factor multiplier
    !*********************************************************************************************
    
    !PAR_TC(10)= T_field_out_des - 273.15d0 - 20.d0    ![C] Minimum allowable hot side outlet temperature during discharge
    !PAR_TC(10)= T_startup - 273.15d0        ![C] 
    !PAR_TC(11)= T_field_in_des - 273.15d0 + 20.d0     ![C] Maximum allowable cold side outlet temperature during charge    
    !PAR_TC(11)= T_startup - 273.15d0
    
    PAR_TC(10)= par(up1(1))                 ![C] Minimum allowable hot side outlet temperature during discharge
    PAR_TC(11)= par(up1(1))                 ![C] Maximum allowable cold side outlet temperature during charge    
    
    PAR_TC(12)= par(up1(1))                 ![-] NODES!!!
    PAR_TC(13)= T_tank_hot_ini - 273.15d0   ![C] Initial thermocline hot temperature
    PAR_TC(14)= T_tank_cold_ini - 273.15d0  ![C] Initial thermocline cold temperature
    PAR_TC(15)= par(up1(1))                 ![-] Fraction into tank where thermocline exists (0: entire tank is hot, 1: entire tank is cold)
    PAR_TC(16)= cold_tank_Thtr - 273.15d0   ![C] Minimum allowable cold tank fluid temperature before auxiliary heater turns on
    PAR_TC(17)= tank_max_heat/tank_pairs    ![MW] Rated heater capacity for tank heating - distribute evenly for each tank pair
    PAR_TC(18)= tank_pairs                  ![-] Number of equivalent tank pairs
    PAR_TC(19)= 0.d0                        ![-] Don't print EES file
    PAR_TC(20)= 0.d0                        ![hr] EES file parameter - don't need it
    PAR_TC(21)= 0.d0                        ![hr] EES file parameter - don't need it
    PAR_TC(22)= getSimulationStartTime()    ![hr] Simulation Start Time
    PAR_TC(23)= getSimulationTimeStep()     ![hr] Simulation Time Step

    !Logic and control parameters, cont...
    allocate(TSLOGIC_A(NUMTOU), TSLOGIC_B(NUMTOU), TSLOGIC_C(NUMTOU), FFRAC(NUMTOU), TSLOGIC(NUMTOU*3), TSELECT(NUMTOU))
    !Initialize TSLOGIC 
    TSLOGIC=0.; TSLOGIC_A=0.; TSLOGIC_B=0.; TSLOGIC_C=0.;

    do i=1,NUMTOU*3
        TSLOGIC(i)=PAR(up1(1))              ![-] Values of TSLOGIC array.. intermediate
    enddo
    do i=1,NUMTOU
        FFRAC(i)=PAR(up1(1))                ![-] Fossil fill fraction control array
    enddo

    !Assign temporary logic array to appropriate structures
    do i=1,NUMTOU
        TSLOGIC_A(i)=TSLOGIC(1+(i-1)*3)
        TSLOGIC_B(i)=TSLOGIC(2+(i-1)*3)
        !Limit turbine operation to max fraction
        TSLOGIC_C(i)=dmax1(dmin1(TSLOGIC(3+(i-1)*3),cycle_max_fraction),cycle_cutoff_frac)
    enddo

    !On the initial timestep call, read in the fluid property file
    call readFluidPropFile(LU_FL)    

    !Calculate heat exchanger size based on parameters
    !hx_ua will be used as the heat exchanger size for simulation calculations
    if((is_hx).and.(TSHOURS>0.d0)) then  !MJW 11.1.2010
        duty = (solarm - 1.0)*q_pb_design
        call hx_size(hx_config,duty,dT_hot,dT_cold,T_field_out_des,T_field_in_des,field_fluid,store_fluid,hx_eff_des,hx_ua,msghx1)
    else
        duty = 0.d0
        hx_eff_des = 0.d0
        hx_ua = 0.d0
        msghx1 = 0
    endif
    
    !Calculate maximum value for power block mass flow
    c_pb_ref = specheat(field_fluid,((T_field_out_des + T_field_in_des)/2.d0),1.d0)*1000. !Reference power block specific heat
    m_dot_pb_max = cycle_max_fraction * q_pb_design/(c_pb_ref*(T_field_out_des - T_field_in_des)) !Maximum power block mass flow rate
    
    !--Calculate the maximum charge/discharge rates for storage  MJW 7.13.2010
    !Charge max is either the power block max flow, or the solar multiple-1 times the design PB mass flow rate
    if(is_hx) then
        ms_charge_max = dmax1(m_dot_pb_max, (solarm - 1.)*q_pb_design/(c_pb_ref*(T_field_out_des - T_field_in_des)))
    else
        !MJW 10.18.2010: If we have direct storage, don't limit the maximum storage charge/discharge rate
        ms_charge_max = 999.*m_dot_pb_max
    endif
    !limit discharge to what the power block can handle
    ms_disch_max = m_dot_pb_max
    
    !---- Initialize outputs
    out(1:nout) = 0.d0
    info(6)=nout


    !---- Set storage array size 
    CALL setStorageSize(nS,INFO)
    !Set initial storage values
    stored(1) = V_tank_hot_ini      ![m3] Initial hot tank HTF volume
    stored(2) = T_tank_hot_ini      ![K] Initial hot tank HTF temperature
    stored(3) = V_tank_cold_ini     ![m3] Initial cold tank HTF volume
    stored(4) = T_tank_cold_ini     ![K] Initial cold tank HTF temperature
    stored(5) = 1.
    stored(6) = V_tank_hot_ini*density(store_fluid,T_tank_hot_ini,1.d0) ![kg] Initial hot tank htf mass
    stored(7) = V_tank_cold_ini*density(store_fluid,T_tank_cold_ini,1.d0) ![kg] Initial cold tank htf mass
    stored(8) = 0.          !power block operating?
    stored(9) = 1.d0               !initial defocus
    stored(10) = T_init_standby          ![hr] set the standby time
    call setStorageVars(stored,nS,info)
    
    IF(TES_type==2)THEN
!        !Set parameters for thermocline (TC) model
!        PAR_TC(1)   = 17.d0     ![-] HTF fluid number
!        PAR_TC(2)   = 20.d0     ![m] Height of rock bed storage tank
!        PAR_TC(3)   = 377.659d0 ![m^2] Cross-sectional area of tank
!        PAR_TC(3)   = 377.659d0*2.d0    
!        PAR_TC(4)   = 7.d0      ![-] Filler material number
!        PAR_TC(5)   = 0.d0      !1.44d0    ![kJ/hr-m2-K] Loss Coefficient
!        PAR_TC(6)   = 0.d0      !0.36d0    ![kJ/hr-m2-K] Top Surface Loss Coefficient
!        PAR_TC(7)   = 0.d0      !0.36d0    ![kJ/hr-m2-K] Bottom Surface Loss Coefficient
!        PAR_TC(8)   = 0.25d0    ![-] Void Fraction
!        PAR_TC(9)   = 1.5d0     ![-] Bottom thermal mass capacitance factor multiplier
!        PAR_TC(10)  = 525.d0    ![C] Minimum allowable hot side outlet temperature during discharge
!        PAR_TC(10)  = T_field_out_des - 273.15d0 - 20.d0    ![C]
!        PAR_TC(11)  = 310.d0    ![C] Maximum allowable cold side outlet temperature during charge
!        PAR_TC(11)  = T_field_in_des - 273.15d0 + 20.d0    ![C]
!        PAR_TC(12)  = 2000.d0   ![-] NODES!!!     
!        PAR_TC(13)  = 574.d0    ![C] Initial thermocline hot temperature
!        PAR_TC(13)  = T_field_out_des - 273.15d0    ![C]
!        PAR_TC(14)  = 290.d0    ![C] Initial thermocline cold temperature
!        PAR_TC(14)  = T_field_in_des - 273.15d0     ![C]
!        PAR_TC(15)  = 2.d0      ![-] Fraction into tank where thermocline exists (0: entire tank is hot, 1: entire tank is cold)
!        PAR_TC(16)  = 0.5
        
!        !Set parameters for thermocline (TC) model
!        PAR_TC(1)   = store_fluid       ![-] HTF fluid number
!        PAR_TC(2)   = h_tank            ![m] Height of rock bed storage tank
!        PAR_TC(3)   = vol_tank / h_tank ![m^2] Cross-sectional area of tank 
!        !PAR_TC(4)   = 7.d0      ![-] Filler material number
!        PAR_TC(5)   = u_tank            ![kJ/hr-m2-K] Loss Coefficient
!        !PAR_TC(6)   = 0.d0      !0.36d0    ![kJ/hr-m2-K] Top Surface Loss Coefficient
!        !PAR_TC(7)   = 0.d0      !0.36d0    ![kJ/hr-m2-K] Bottom Surface Loss Coefficient
!        !PAR_TC(8)   = 0.25d0    ![-] Void Fraction
!        !PAR_TC(9)   = 1.5d0     ![-] Bottom thermal mass capacitance factor multiplier
!        PAR_TC(10)  = T_field_out_des - 273.15d0 - 20.d0    ![C] Minimum allowable hot side outlet temperature during discharge
!        PAR_TC(11)  = T_field_in_des - 273.15d0 + 20.d0     ![C] Maximum allowable cold side outlet temperature during charge
!        !PAR_TC(12)  = 2000.d0   ![-] NODES!!!     
!        PAR_TC(13)  = T_tank_hot_ini - 273.15d0     ![C] Initial thermocline hot temperature
!        PAR_TC(14)  = T_tank_cold_ini - 273.15d0    ![C] Initial thermocline cold temperature
!        !PAR_TC(15)  = 2.d0      ![-] Fraction into tank where thermocline exists (0: entire tank is hot, 1: entire tank is cold)

        CALL PackedBed(TIME,XIN_TC,OUT_TC,PAR_TC,INFO)
    ENDIF

    !Set the convergence coefficient 11.29.2010
    if(TES_type == 1.) then
        ccoef = .9 - .5*(3600.- dt)/3600. !3.5.11
    else
        ccoef = .5      !mjw 5.3.11
    endif

    return 1
endif

IF( (TIME<1.5d0).and.(TES_type==2) ) THEN
    CALL PackedBed(TIME,XIN_TC,OUT_TC,PAR_TC,INFO)
ENDIF

!******************************************************************************************************************************
!               Time-dependent conditions
!******************************************************************************************************************************
I_bn = xin(1)/3.6                   ![W/m2] Beam normal radiation (input kJ/m2-hr)
TOUPeriod = xin(2)                  !Time of use period
m_dot_field = xin(3)/3600.          ![kg/s]  Flow rate from the field
m_dot_htf_ref = xin(4)/3600.        ![kg/s] Reference HTF flow rate at design conditions
T_field_out = xin(5)+273.15         ![K] HTF Temperature from the field
T_pb_out = xin(6)+273.15            ![K] Fluid temperature from the power block
T_amb = xin(7)+273.15               ![K] Ambient temperature
m_pb_demand = xin(8)/3600.          ![kg/s] Demand htf flow from the power block
q_startup = xin(9)                  ![MWt] Startup energy reported by the collector field
dnifc = xin(10)                     ![W/m2] Forecast DNI
!--mjw 4.26.11 inputs for the external TES model
!if(TES_Type/=1) then
!    m_disch_avail_in = xin(11)/3600.d0      ![kg/s] available storage discharge flow rate
!    T_disch_avail_in = xin(12)+273.15d0     ![K] available charge temperature
!    m_charge_avail_in = xin(13)/3600.d0     ![kg/s] available storage charge flow rate
!    T_charge_avail_in = xin(14)+273.15d0    ![K] available discharge temperature
!    T_TES_cold = xin(15)+273.15d0           ![K] Actual cold temperature from TES external model
!    T_TES_hot = xin(16)+273.15d0            ![K] Actual hot temperature from TES
!    Q_TES_hl_in = xin(17)/3.6e6             ![MWt] Reported tank heat losses
!else
IF(TES_Type==1)THEN
    m_disch_avail_in = 0.d0; T_disch_avail_in = 0.d0; m_charge_avail_in = 0.d0
    T_charge_avail_in = 0.d0; T_TES_cold = 0.d0; T_TES_hot = 0.d0; Q_TES_hl_in = 0.d0
endif

!******* Read in stored variables every timestep*******
call getStorageVars(stored,nS,info)

V_tank_hot0 = stored(1)             ![m3] Previous total hot tank HTF volume
T_tank_hot0 = stored(2)             ![K] Previous hot tank HTF temperature
V_tank_cold0 = stored(3)            ![m3] Previous total cold tank HTF volume
T_tank_cold0 = stored(4)            ![K] Previous cold tank HTF temperature
mode0 = int(stored(5))              ![] operation mode
m_tank_hot0 = stored(6)             ![kg] total mass in the hot tank
m_tank_cold0 = stored(7)            ![kg] total mass in the cold tank
pb_on0 = stored(8)                  ![-] Power block was running?
defocus0 = stored(9)                ![-] defocus from the previous iteration
T_standby0 = stored(10)             ![hr] The remaining standby time

T_standby = T_standby0      !10.15.12 tn
!******************************************************************************************************************************
!               Timestep calculations
!******************************************************************************************************************************
if(I_bn.gt.1.e-6) then   !The solar resource is available above 0. W/m^2
    TSELECT=TSLOGIC_B
else                  !Use the dispatch for no solar resource
    TSELECT=TSLOGIC_A
endif

f_storage = TSELECT(TOUperiod)  ![-] Storage dispatch fraction for timestep

!-Account for any available thermal storage
itert = 1
T_tank_hotX = T_tank_hot0
T_tank_coldX = T_tank_cold0
30 continue !iteration loop for storage
itert = itert+1

if(TSHOURS > 0.01) then
    if(TES_type == 1) then !mjw 4.26.11 : 2-tank storage
        !We know the total mass in the tanks, but need to calculate available mass
        !which depends on fluid temperature and minimum volume requirements
        rho_tank_hotX = density(store_fluid,T_tank_hotX,1.d0)       !10-11-12, TN: T_tank_hotX is a guess value for average temperature, i guess
        rho_tank_coldX = density(store_fluid,T_tank_coldX,1.d0)
        V_tank_hot0 = m_tank_hot0/rho_tank_hotX                                 ![m3] The total fluid volume in the hot tank, including unusable volume
        V_tank_cold0 = m_tank_cold0/rho_tank_coldX                              ![m3] The total fluid volume in the cold tank, including unusable volume
        V_tank_hotA = dmax1(V_tank_hot0 - vol_tank*h_tank_min/h_tank,0.d0)      ![m3] The available fluid volume in the hot tank
        V_tank_coldA = dmax1(V_tank_cold0 - vol_tank*h_tank_min/h_tank,0.d0)    ![m3] The available fluid volume in the cold tank
        
        !Calculate the available mass flow rate from the hot and cold tanks
        m_tank_disch_avail = dmax1((V_tank_hotA - V_tank_active*TSELECT(TOUperiod))*rho_tank_hotX/dt, 0.d0)         ![kg/s] Available mass flow rate during discharge cycle
        m_tank_charge_avail = dmax1(V_tank_coldA*rho_tank_coldX/dt,0.d0)      ![kg/s] Available mass flow rate during charge cycle
    elseif(TES_type == 2) then !mjw 4.26.11 : Thermocline storage
        XIN_TC(1)   = T_field_out - 273.15d0    ![C] Charging temperature must come from field outlet
        XIN_TC(2)   = 0.d0                      ![kg/hr] Don't need to know charging flow rate because this call is just checking availability
        XIN_TC(3)   = T_pb_out - 273.15d0       ![C] Discharging temperature must come from power block
        XIN_TC(4)   = 0.d0                      ![kg/hr] Don't need to know discharging flow rate because this call is just check availability
        XIN_TC(5)   = T_amb - 273.15d0          ![C] Ambient temperature 
        XIN_TC(6)   = 2.d0                      ![-] Flag for just checking availability
        XIN_TC(7)   = 0.d0                      ![W] Discharge energy rate demanded
        XIN_TC(8)   = 0.d0                      ![W] Charge energy rate demanded
        XIN_TC(9)   = f_storage                 ![-] Storage dispatch fraction
        
        CALL PackedBed(TIME,XIN_TC,OUT_TC,PAR_TC,INFO)            
   
        m_tank_disch_avail  = OUT_TC(1)/3600.d0     ![kg/s] !Estimated discharge mass flow rate
        Ts_hot              = OUT_TC(2)+273.15d0    ![K]    !Estimated discharge temperature
        m_tank_charge_avail = OUT_TC(3)/3600.d0     ![kg/s] !Estimated charge mass flow rate
        TS_cold             = OUT_TC(4)+273.15d0    ![K]    !Estimated charge temperature
        CONTINUE
    endif
    
    !-Adjust the available storage to correct for the intermediate heat exchanger
    if(is_hx) then
        T_tank_hot_out = T_tank_hotX
        T_tank_cold_out = T_tank_coldX
        !Calculate the field-equivalent mass flow rate produced if all the storage is dispatched.
        !Discharge cycle.. hot flow comes from storage. Cold flow comes from PB outlet
        if(m_tank_disch_avail > 0.) then
            call hx_perf(hx_ua, hx_config, store_fluid, field_fluid, m_tank_disch_avail, T_tank_hot_out, &
                         T_pb_out, dT_hot, dT_cold,hx_eff, T_tank_cold_in, Ts_hot, q_hx, ms_disch_avail, msghx2)
        
!        !Subroutine for heat exchanger performance. Provide the hot side mass flow rate, inlet temps, delta temps.
!subroutine hx_perf(UA, config, fluid_h, fluid_c, m_dot_h, T_h_in, T_c_in, dT_hot, dT_cold,&
!                 !outputs
!                 eta, T_h_out, T_c_out, q_trans, m_dot_c,msg)
       
        else
            ms_disch_avail = 0.
            Ts_hot = T_tank_hot_out     !10.15.12 tn
        endif
        !Calculate the field-equivalent mass flow rate produced if all the cold storage is charged. 
        !Charging cycle.. hot flow comes from field. Cold flow comes from storage.
        if(m_tank_charge_avail > 0.) then
            call hx_reverse(hx_ua, hx_config, field_fluid, store_fluid, m_tank_charge_avail, T_field_out, T_tank_cold_out, dT_hot, dT_cold,&
                            hx_eff, Ts_cold, T_tank_hot_in, q_hx, ms_charge_avail, msghx3)
            !Don't allow message triggering for this hypothetical calculation. We really only care about the performance call.
            msghx3=0.d0
        else
            ms_charge_avail = 0.
            Ts_cold = T_tank_cold_out   !10.15.12 tn
        endif

    else
        IF(TES_type==2)THEN
            ms_disch_avail = m_tank_disch_avail         ![kg/s] Available mass flow rate during discharge cycle
            ms_charge_avail = m_tank_charge_avail      ![kg/s] Available mass flow rate during charge cycle            
        ELSE 
            T_tank_hot_out = T_tank_hotX
            T_tank_cold_out = T_tank_coldX
            ms_disch_avail = m_tank_disch_avail         ![kg/s] Available mass flow rate during discharge cycle
            ms_charge_avail = m_tank_charge_avail      ![kg/s] Available mass flow rate during charge cycle
            Ts_hot = T_tank_hot_out
            Ts_cold = T_tank_cold_out
        ENDIF
    endif
    
    !Limit to the max charge/disch rates MJW 7.13.2010
    ms_disch_avail = dmin1(ms_disch_avail, ms_disch_max)
    ms_charge_avail = dmin1(ms_charge_avail, ms_charge_max)
else
    ms_disch_avail = 0.
    ms_charge_avail = 0.
endif

!-Account for any available auxiliary heater flow
c_htf_aux = specheat(field_fluid,(T_set_aux + T_pb_out)/2.,1.d0)*1000. ![J/kg-K] average specific heat
if(ffrac(TOUPeriod) > 0.) then
    m_dot_aux_avail = dmin1(q_max_aux,ffrac(TOUPeriod)*q_pb_design)/(c_htf_aux*dmax1((T_set_aux - T_pb_out),1.d0))
else
    m_dot_aux_avail = 0.
endif

!mjw 12.8.2010
if(T_field_out < T_startup) m_dot_field = 0.

!Calculate the maximum potentially available mass flow
m_avail_tot = ms_disch_avail + m_dot_aux_avail + m_dot_field

!Calculate the demanded values from the power block
q_pb_demand = q_pb_design*TSLOGIC_C(TOUperiod) !Adjust the power block demand energy for the TOU period 

!Calculate the maximum potentially available heat flows.  Flows are relative to the
!power block outlet temperature, except in the case of the storage charge level and
!the field inlet temperature. This is because the field inlet temp is a function of
!the calculated values of the other temperatures and flow rates, and because mass 
!flow in this case is calculated relative to the field-side cold outlet temperature.

!A small degree of iteration is needed to get the field inlet temperature to converge
!in the case where storage is charging and an intermediate heat exchanger is used.
!For cases with iteration, set the initial guess values
if(m_dot_field > 0.d0) then  !MJW 12.14.2010  base the field inlet temperature only on the mass flow rate from the field
    T_field_inX = T_pb_out
else
    T_field_inX = T_field_out
endif
err = 999.; iter = 0; derr=999.; errx = 9999.; defocusX=1.; tol = 0.001
50 continue  !iterative loop header
iter=iter+1
T_field_in = T_field_inX

!Calculate available heat flows
!10.15.12 tn: Ts_hot and Ts_cold are not always defined. Not a problem here because because the capacitance doesn't matter
!               when no storage is available, but in c++ it needs to be defined each call to type
c_htf_dis = specheat(field_fluid,((Ts_hot + T_pb_out)/2.d0),1.d0)*1000.   !mjw 1.18.2011
c_htf_chg = specheat(field_fluid,((T_field_out + Ts_cold)/2.d0),1.d0)*1000.     !Specific heat for charge flow
c_htf_fld = specheat(field_fluid,((T_field_out + T_field_inX)/2.d0),1.d0)*1000. !mjw 1.18.2011

!**** 10.15.12 TN: T_pb_in is not defined for the first iteration each call to this type
c_htf_pb  = specheat(field_fluid,((T_pb_in + T_pb_out)/2.d0),1.d0)*1000.        !MJW 7.12.2010: Specific heat for the power block

!MJW 7.12.2010: The mass flow should be adjusted to be over design until the demand mass flow rate is met
!Calculate the mass flow rate associated with this load, given the applicable HTF temps
m_dot_dum = dmin1(q_pb_demand/dmax1(c_htf_pb*(T_pb_in - T_pb_out),1.e-6),m_dot_pb_max)
!Recalculate the demand mass flow rate based on this new power block flow rate
if(T_pb_in > T_pb_out + 0.01) then
    q_pb_demandX = m_dot_dum* c_htf_pb*(T_pb_in - T_pb_out)
    !q_pb_demand = dmin1(q_pb_demand, m_dot_dum*c_htf_pb*(T_field_out_des - T_field_in_des)) !mjw 11.4.2010
else
    q_pb_demandX = q_pb_demand
endif

IF(TES_type==2)THEN
    qs_disch_avail = 0.d0
    qs_charge_avail = 0.d0
ELSE
    qs_disch_avail = dmax1(ms_disch_avail*c_htf_dis*(Ts_hot - T_pb_out),0.d0)  
    qs_charge_avail = dmax1(ms_charge_avail*c_htf_chg*(T_field_out - Ts_cold),0.d0) 
ENDIF

qaux_avail = dmax1(m_dot_aux_avail*c_htf_aux*(T_set_aux - T_pb_out),0.d0) 
!qfield_avail = dmax1(m_dot_field*c_htf_fld*(T_field_out - T_field_in_des),0.d0)   !MJW 12.8.2010
qfield_avail = dmax1(m_dot_field*c_htf_fld*(T_field_out - T_field_inX),0.d0) !mjw 1.18.2011

!Initial values
q_int = 0.d0    !Intermediate equivalent power output from the various heat sources
T_int = T_pb_out !Set intermediate temperature to outlet temperature for now
m_int = 0.d0    !Intermediate mass flow from the various heat sources

!Logic to determine if packed bed model has been called during timestep
called_TC = .false.

!---Allocate the flows from the various sources

!Start for the case where the energy available from the field is less than  
!or equal to the design requirement
if(qfield_avail <= q_pb_demandX) then
    !First add the available energy from the field
    q_int = qfield_avail  
    m_int = m_dot_field
    !If energy is coming from the field, adjust the intermediate temperature
    if(q_int > 0.) T_int = T_field_out
    !Next, try to add energy from storage
    
    IF((ms_disch_avail>0.d0).and.(TES_type==2))THEN   
            q_TC_demand = q_pb_demandX - q_int
            
            XIN_TC(1)   = T_field_out - 273.15d0    ![C] Charging temperature must come from field outlet
            XIN_TC(2)   = 0.d0                      ![kg/hr] Don't need to know charging flow rate because this call is just checking availability
            XIN_TC(3)   = T_pb_out - 273.15d0       ![C] Discharging temperature must come from power block
            XIN_TC(4)   = 0.d0                      ![kg/hr] Don't need to know discharging flow rate because this call is just check availability
            XIN_TC(5)   = T_amb - 273.15d0          ![C] Ambient temperature 
            XIN_TC(6)   = 1.d0                      ![-] Flag for just checking availability (1 = calculate packed bed performance)
            XIN_TC(7)   = q_TC_demand               ![W] Discharge energy rate demanded
            XIN_TC(8)   = 0.d0                      ![W] Charge energy rate demanded
            XIN_TC(9)   = f_storage                 ![-] Storage dispatch fraction
            
            CALL PackedBed(TIME,XIN_TC,OUT_TC,PAR_TC,INFO)
            called_TC   = .true.                    ![-] Packed bed model has been called during timestep
            
            qs_disch_avail  = OUT_TC(5)             ![W] Discharge energy rate
            ms_disch_avail  = OUT_TC(1)/3600.d0     ![kg/s] Discharge mass flow rate          
            Ts_hot          = OUT_TC(2)+273.15d0    ![K] Discharge outlet temperature   
            CONTINUE         
    ENDIF
    
    if((qfield_avail + qs_disch_avail) < q_pb_demandX) then
        !If the total amount of energy is less than the design condition, 
        !add all of the remaining energy from storage
        q_int = q_int + qs_disch_avail
        ms_disch = ms_disch_avail
        ms_charge = 0.
        m_int = m_dot_field + ms_disch
        !Adjust the intermediate temperature, if needed
        if(q_int > 0.) T_int = (T_int*m_dot_field + Ts_hot*ms_disch)/(m_int)

        !MJW 11.1.2010: The auxiliary heater can operate in 1 of 2 modes.. 
        ! mode 1: Fossil fraction specifies the minimum pb fraction during the period. Fossil provides the balance.
        ! mode 2: Fossil tops off the available energy. The total fossil contrib. can't exceed the control fraction.
        if (fossil_mode == 1) then
            q_demand_aux = ffrac(TOUPeriod)*q_pb_design
            if(q_int < q_demand_aux) then
                q_aux = q_demand_aux - q_int
                q_int = q_demand_aux
                m_dot_aux = q_aux/(c_htf_aux*dmax1((T_set_aux - T_pb_out),1.d0))
                m_int = m_int + m_dot_aux
                !Adjust the intermediate temperature, if needed
                if(q_int > 0.) T_int = (T_int*(m_dot_field + ms_disch) + T_set_aux*m_dot_aux)/m_int
            else !MJW 1.12.2011
                q_aux = 0.d0
                m_dot_aux = 0.d0
            endif
        else
            !Lastly, add any available auxiliary heat to get to the design point
            if((q_int + qaux_avail) < q_pb_demandX) then
                q_int = q_int + qaux_avail
                m_dot_aux = m_dot_aux_avail
                m_int = m_int + m_dot_aux
                !Adjust the intermediate temperature, if needed
                if(q_int > 0.) T_int = (T_int*(m_dot_field + ms_disch) + T_set_aux*m_dot_aux)/m_int
            else
                !The aux heater was able to produce more energy than what was needed
                q_aux = q_pb_demandX - q_int
                q_int = q_pb_demandX
                !Set the required mass flow rate
                m_dot_aux = q_aux/(c_htf_aux*(T_set_aux - T_pb_out))  
                m_int = m_int + m_dot_aux
                !Adjust the intermediate temperature
                T_int = (m_dot_field*T_field_out + ms_disch*Ts_hot + m_dot_aux*T_set_aux)/m_int
            endif
        endif
        

    else
        !Between the energy available from the field and from storage, the dispatch requirement
        !has been met.  No aux load will be added in this condition              
        q_int = q_pb_demandX
        IF(TES_type==2)THEN
            ms_disch = ms_disch_avail
        ELSE
            ms_disch = (q_pb_demandX - qfield_avail)/(c_htf_dis * (Ts_hot - T_pb_out))  
        ENDIF
        m_int = m_int + ms_disch
        ms_charge = 0.
        m_dot_aux = 0.
        T_int = (m_dot_field*T_field_out + ms_disch*Ts_hot)/m_int
    endif

    !Run a check to see if the energy produced is above the cycle cutout fraction
    if(q_int < q_pb_design*cycle_cutoff_frac) then
        mode = 1
    else
        mode = 2
    endif
    
else 
    !The flow from the field is sufficient to meet the power block demand. If the system has storage
    !the excess flow will be used to charge it.
    
    !MJW 11.3.2010: Need to specify m_int in cases where we've entered "sticky mode" operation
    m_int = m_dot_field
    T_int = T_field_out        
    
    IF((ms_charge_avail>0.d0).and.(TES_type==2))THEN        
        q_charge_demand = qfield_avail - q_pb_demandX
        
        XIN_TC(1)   = T_field_out - 273.15d0    ![C] Charging temperature must come from field outlet
        XIN_TC(2)   = 0.d0                      ![kg/hr] Don't need to know charging flow rate because this call is just checking availability
        XIN_TC(3)   = T_pb_out - 273.15d0       ![C] Discharging temperature must come from power block
        XIN_TC(4)   = 0.d0                      ![kg/hr] Don't need to know discharging flow rate because this call is just check availability
        XIN_TC(5)   = T_amb - 273.15d0          ![C] Ambient temperature 
        XIN_TC(6)   = 1.d0                      ![-] Flag for just checking availability (1 = calculate packed bed performance)
        XIN_TC(7)   = 0.d0                      ![W] Discharge energy rate demanded
        XIN_TC(8)   = q_charge_demand           ![W] Charge energy rate demanded
        XIN_TC(9)   = f_storage                 ![-] Storage dispatch fraction
        
        CALL PackedBed(TIME,XIN_TC,OUT_TC,PAR_TC,INFO)
        called_TC   = .true.                    ![-] Packed bed model has been called during timestep
        
        qs_charge_avail = OUT_TC(5)             ![W] Charge energy rate
        ms_charge_avail = OUT_TC(3)/3600.d0     ![kg/s] Charge mass flow rate          
        Ts_cold         = OUT_TC(4)+273.15d0    ![K] Charging outlet temperature
        CONTINUE
    ENDIF
    
    !Check if the excess flow can be dropped into storage
    if((qfield_avail - q_pb_demandX) > qs_charge_avail) then
        mode = 3
    else
        mode = 4
    endif
    
endif

!Check to make sure mode iteration is not happening
if(info(7)>4) then
    if(info(7)>8) mode = mode0
    if((defocus<1.d0).and.(mode0==4)) mode = 4  !MJW 11.10.2010 don't let it switch from 4 in this case
endif

!MJW 11.4.2010
if(info(7)==0) defocus = 1.d0

!MJW 12.9.2010 -- 
!enforce special considerations for when the turbine is off and wants to restart
if((pb_on0==0.).and.(mode/=1)) then
    !*Look ahead for DNI resource to decide whether cycle should start back up
    if((dnifc<I_bn_des*cycle_cutoff_frac).and.(fc_on)) mode = 1
    !*If it's night time and TES is 0 and power cycle is below cutoff then don't restart the turbine
    if(m_dot_field == 0.) mode = 1
endif

!Do mode-specific calculations here
select case(mode)
case(1)  !mjw 4.21.11 Operation here can either be totally off or running in standby
    !Enter standby mode if the following criteria are met: (1a) Standby time remains and (1b) The available energy from the solar field 
    !..and/or storage is greater than the standby requirement; AND (2a) The power block was previously running; or (2b) it was previously operating in standby
    !Any of the following criteria will send the power block into normal shutdown (not standby mode)
    
    !10.15.2012 TN:** But what if forecasting is not available???
    if(dnifc <= 0.001d0) goto 11    !If solar radiation is not expected (Type 215 only); alternatively, if the sun is below the horizon (Type 15 only)
    if(T_standby0 <= 0.d0) goto 11  !If the standby time has been exhausted
    if(qs_disch_avail + qfield_avail < q_sby) goto 11   !If the total available energy from the field+TES is less than the standby requirement
    if((pb_on0 /= 1.) .and. (T_standby0 == T_init_standby)) goto 11   !If the power block hasn't been running normally or in standby mode in the previous step
        
    !**********************************************
    !********** Standby Operation *****************
    !**********************************************
    
    IF( tshours > 0.d0 )THEN
        
        IF( qfield_avail < q_sby )THEN     ! discharge  
                          
            q_sby_storage   = max(q_sby - qfield_avail, 0.d0)        ![W] Energy required from storage to sustain standby mode
            ms_charge       = 0.d0      ![kg/s] Charge mass flow rate
        
            IF(TES_type == 2)THEN   !thermocline
            
                XIN_TC(1)   = T_field_out - 273.15d0    ![C] Charging temperature must come from field outlet
                XIN_TC(2)   = 0.d0                      ![kg/hr] Don't need to know charging flow rate because this call is just checking availability
                XIN_TC(3)   = T_pb_out - 273.15d0       ![C] Discharging temperature must come from power block
                XIN_TC(4)   = 0.d0                      ![kg/hr] Don't need to know discharging flow rate because this call is just check availability
                XIN_TC(5)   = T_amb - 273.15d0          ![C] Ambient temperature 
                XIN_TC(6)   = 1.d0                      ![-] Flag for just checking availability (1 = calculate packed bed performance)
                XIN_TC(7)   = q_sby_storage             ![W] Discharge energy rate demanded
                XIN_TC(8)   = 0.d0                      ![W] Charge energy rate demanded
                XIN_TC(9)   = f_storage                 ![-] Storage dispatch fraction
                
                CALL PackedBed(TIME,XIN_TC,OUT_TC,PAR_TC,INFO)
                called_TC   = .true.                    ![-] Packed bed model has been called during timestep
                
                ms_disch    = OUT_TC(1)/3600.d0         ![kg/s] Discharge mass flow rate          
                Ts_hot      = OUT_TC(2)+273.15d0        ![K] Discharge outlet temperature
            
            ELSE        !2-tank
                ms_disch = q_sby_storage / (c_htf_dis * (Ts_hot - T_pb_out))        ![kg/s]
            ENDIF                
        
            T_pb_in = (T_field_out*m_dot_field + Ts_hot*ms_disch)/(m_dot_field + ms_disch)
        
        ELSE    ! (q_field_avail > q_sby), need to charge/defocus
            
            ms_disch  = 0.d0            ![kg/s] Discharge mass flow rate
            T_pb_in = (T_field_out*m_dot_field + Ts_hot*ms_disch)/(m_dot_field + ms_disch)  !10.15.12 tn: this equation doesn't need ms_disch parts because it = 0
            ms_charge = dmax1(m_dot_field - q_sby/(c_htf_pb*(T_pb_in - T_pb_out)), 0.d0)    ![kg/s] Calculate mass flow rate that is not used by PB in standby mode
                    
            IF(TES_type == 2)THEN   !thermocline
            
                XIN_TC(1)   = T_field_out - 273.15d0    ![C] Charging temperature must come from field outlet
                XIN_TC(2)   = ms_charge*3600.d0         ![kg/hr] Charging flow rate
                XIN_TC(3)   = T_pb_out - 273.15d0       ![C] Discharging temperature must come from power block
                XIN_TC(4)   = 0.d0                      ![kg/hr] Don't need to know discharging flow rate because this call is just check availability
                XIN_TC(5)   = T_amb - 273.15d0          ![C] Ambient temperature 
                XIN_TC(6)   = 1.d0                      ![-] Flag for just checking availability (1 = calculate packed bed performance)
                XIN_TC(7)   = 0.d0                      ![W] Discharge energy rate demanded
                XIN_TC(8)   = 0.d0                      ![W] Charge energy rate demanded
                XIN_TC(9)   = f_storage                 ![-] Storage dispatch fraction
                
                CALL PackedBed(TIME,XIN_TC,OUT_TC,PAR_TC,INFO)
                called_TC   = .true.                    ![-] Packed bed model has been called during timestep
                
                ms_charge   = OUT_TC(3)/3600.d0     ![kg/s] Charge mass flow rate          
                Ts_cold     = OUT_TC(4)+273.15d0    ![K] Charging outlet temperature
                qs_charge_avail = OUT_TC(5)         ![W] Charging energy rate    
            
            ELSE        !2-tank
            
                !ms_charge = dmin1(ms_charge_avail, dmax1(m_dot_field - q_sby/(c_htf_pb*(T_pb_in - T_pb_out)), 0.d0))  !tn 5.23.11
                ms_charge = dmin1(ms_charge_avail, ms_charge)   !limit charge mass flow rate to the max. available
            
            ENDIF
        
            if(m_dot_field > 0.) then
                !This equation needs a-fixin!
                !defocus = defocus0*((qs_charge_avail+q_sby)/qfield_avail)**ccoef 
                defocus = ((qs_charge_avail+q_sby)/qfield_avail)**ccoef    !TN 4.9.12
            else
                !defocus = defocus0
                defocus = 1.d0  !TN 4.9.12
            endif
        
        ENDIF
    
    ELSE    !tshours == 0
    
        defocus = 1.d0
        m_dot_field = 0.d0
        T_pb_in = T_field_out
        
    ENDIF
    
    m_dot_pb = 0.d0 !the power block code will calculate the mass flow for this mode
   !charge storage with any additional field flow, if possible
    !T_pb_in = (T_field_out*m_dot_field + Ts_hot*ms_disch)/(m_dot_field + ms_disch)
        
    cycle_pl_control = 2. !1=demand, 2=part load
    standby_control = 2. !!1=normal operation, 2=standby, 3=shut down
    !Zero out the fossil backup contribution if we're below the turbine minimum
    q_aux = 0.d0; m_dot_aux = 0.d0
    T_standby = dmax1(T_standby0 - dt, 0.d0) 
     
    !**********************************************
    !********* END Standby Operation **************
    !********************************************** 
            
    goto 12
    11 continue
    
        !**********************************************
        !************* Shutdown ***********************
        !**********************************************
    
        !normal, non-standby operation
        !If below the cutout fraction, check to see if extra energy can be dumped into storage
        IF((m_dot_field>0.d0).and.(ms_charge_avail>0.d0).and.(TES_type==2))THEN
            XIN_TC(1)   = T_field_out - 273.15d0    ![C] Charging temperature must come from field outlet
            XIN_TC(2)   = m_dot_field*3600.d0       ![kg/hr] Charging flow rate
            XIN_TC(3)   = T_pb_out - 273.15d0       ![C] Discharging temperature must come from power block
            XIN_TC(4)   = 0.d0                      ![kg/hr] Don't need to know discharging flow rate because this call is just check availability
            XIN_TC(5)   = T_amb - 273.15d0          ![C] Ambient temperature 
            XIN_TC(6)   = 1.d0                      ![-] Flag for just checking availability (1 = calculate packed bed performance)
            XIN_TC(7)   = 0.d0                      ![W] Discharge energy rate demanded
            XIN_TC(8)   = 0.d0                      ![W] Charge energy rate demanded
            XIN_TC(9)   = f_storage                 ![-] Storage dispatch fraction
            
            CALL PackedBed(TIME,XIN_TC,OUT_TC,PAR_TC,INFO)
            called_TC   = .true.                    ![-] Packed bed model has been called during timestep
            
            ms_charge_avail = OUT_TC(3)/3600.d0     ![kg/s] Charge mass flow rate          
            Ts_cold     = OUT_TC(4)+273.15d0        ![K] Charge outlet temperature
            qs_charge_avail = OUT_TC(5)             ![W] Charging energy rate
            CONTINUE
        ELSEIF((ms_disch>0.d0).and.(TES_type==2))THEN
            called_TC   = .false.   ![-] Have called packed bed earlier assuming discharge, but since PB is off, need to recalculate with idling 
        ENDIF
        
        ms_charge = dmax1(dmin1(ms_charge_avail, m_dot_field),0.d0)
        
        ms_disch = 0.d0
        m_dot_pb = 0.d0
        T_pb_in = T_pb_out
        if(tshours > 0.) then
            if(m_dot_field > 0.) then
                !Defocus according the the amount of charge accepted as compared to what is produced. 
                !defocus = defocus0*(ms_charge/m_dot_field)**ccoef !MJW 12.9.2010
                defocus = (ms_charge/m_dot_field)**ccoef !TN 4.9.12
            else
                !defocus = defocus0
                defocus = 1.d0  !TN 4.9.12
            endif
        else
            !Here we need to allow the field to run normally and artificially force the power block to dump
            !energy.  
            defocus = 1.d0
            m_dot_field = 0.d0
        endif
        cycle_pl_control = 2. !1=demand, 2=part load
        standby_control = 3. !!1=normal operation, 2=standby, 3=shut down
        !MJW 11.3.2010: Zero out the fossil backup contribution if we're below the turbine minimum
        q_aux = 0.d0; m_dot_aux = 0.d0
        T_standby = T_init_standby
        
    12  continue
case(2)
    !Operation below design point. Continue with calculated operation values
    if(fthr_ok) then
        !defocus = dmin1(defocus0*((qs_charge_avail + q_pb_demandX)/dmax1(qfield_avail,1.d0))**ccoef, 1.d0)  !mjw 1.18.2010
        defocus = ((qs_charge_avail + q_pb_demandX)/dmax1(qfield_avail,1.d0))**ccoef    !TN 4.9.12
    else
        !defocus = dmax1(defocus0, defocus0*jfix(nSCA*((qs_charge_avail + q_pb_demandX)/dmax1(qfield_avail, 1.d0))**ccoef)/nSCA)
        defocus = jfix(nSCA*((qs_charge_avail + q_pb_demandX)/dmax1(qfield_avail, 1.d0))**ccoef)/nSCA   !TN 4.9.12
    endif
    m_dot_pb = m_int        !The mass flow rate to the power cycle
    T_pb_in = T_int         ![K] The HTF temperature to the power block
    cycle_pl_control = 2.   !1=demand, 2=part load
    standby_control = 1.    !1=normal operation, 2=standby, 3=shut down
case(3)
    !More thermal energy is available than what can be put into storage
    IF(TES_type==2)THEN
        ms_charge = ms_charge_avail
    ELSE
        ms_charge = qs_charge_avail/(c_htf_fld*(T_field_out - Ts_cold)) 
    ENDIF
    !MJW 11.9.2010: adjust defocus calculation relative to the previous. 0.4 coef tested for speed and convergence
    if(fthr_ok) then
        !defocus = defocus0*((qs_charge_avail + q_pb_demandX)/qfield_avail)**ccoef    !MJW 11.10.2010
        defocus = ((qs_charge_avail + q_pb_demandX)/qfield_avail)**ccoef    !TN 4.9.12
    else
        !defocus = defocus0*jfix(nSCA*((qs_charge_avail + q_pb_demandX)/qfield_avail)**ccoef)/nSCA  !MJW 12.6.2010
        defocus = jfix(nSCA*((qs_charge_avail + q_pb_demandX)/qfield_avail)**ccoef)/nSCA  !TN 4.9.12
    endif
    T_pb_in = T_field_out
    m_dot_pb = dmax1(q_pb_demandX/(c_htf_pb*(T_pb_in - T_pb_out)),0.d0)  !mjw 1.18.2011
    ms_disch = 0.
    cycle_pl_control = 2. !1=demand, 2=part load
    standby_control = 1. !1=normal operation, 2=standby, 3=shut down
    m_dot_aux = 0.
case(4)
    !More energy is available than can be used in the PB, but all of the extra thermal energy can be dropped into storage
    !..storage charge rate limited to max available charge rate  MJW 7.13.2010
    !ms_charge = dmax1(dmin1(qfield_avail - q_pb_demandX, qs_charge_avail)/(c_htf_fld*dmax1(T_field_out - Ts_cold,1.d0)),0.d0)  !MJW 7.15.2010 1.e-6 -> 1.
    IF(TES_type==2)THEN
        ms_charge = ms_charge_avail
    ELSE
        ms_charge = dmax1(dmin1(qfield_avail - q_pb_demandX, qs_charge_avail)/(c_htf_chg*dmax1(T_field_out - Ts_cold,1.d0)),0.d0)  !MJW 3.6.11 changing c_htf
    ENDIF
    if(fthr_ok) then
        !defocus = dmax1(defocus0, defocus0*((qs_charge_avail + q_pb_demandX)/qfield_avail)**ccoef)   !MJW 11.10.2010
        !defocus = dmin1(1.d0, defocus0*((qs_charge_avail + q_pb_demandX)/qfield_avail)**ccoef)  !mjw 3.7.11
        defocus = ((qs_charge_avail + q_pb_demandX)/qfield_avail)**ccoef  !TN 4.9.12
    else
        !defocus = dmax1(defocus0, defocus0*jfix(nSCA*((qs_charge_avail + q_pb_demandX)/qfield_avail)**ccoef)/nSCA)  !MJW 12.6.2010
        !defocus = dmin1(1.d0, defocus0*jfix(nSCA*((qs_charge_avail + q_pb_demandX)/qfield_avail)**ccoef)/nSCA)  !MJW 12.6.2010
        defocus = jfix(nSCA*((qs_charge_avail + q_pb_demandX)/qfield_avail)**ccoef)/nSCA  !TN 4.9.12
    endif
    !m_dot_pb = dmax1(q_pb_demandX/(c_htf_fld*dmax1(T_field_out - T_pb_out,1.d0)),0.d0)      !MJW 7.15.2010 1.e-6 -> 1.
    m_dot_pb = dmax1(q_pb_demandX/(c_htf_pb*dmax1(T_field_out - T_pb_out,1.d0)),0.d0)      !MJW 3.6.11 changing c_htf
    T_pb_in = T_field_out
    ms_disch = 0.
    cycle_pl_control = 2. !1=demand, 2=part load
    standby_control = 1.  !1=normal operation, 2=standby, 3=shut down
    m_dot_aux = 0.
    
    !Rarely, this mode is selected when the solar field outlet temp. drops. Protect from this
    !by enforcing limits on the temperature that can be accepted by the power block. Limit to 25% of 
    !difference between the design field inlet/outlet temp, and turn off operation. MJW 7.15.2010
    if((T_pb_in < T_field_inX + (T_field_out_des-T_field_in_des)*.25).and.(info(7)>15)) mode = 1
    
end select

!Set mode here
stored(5) = mode    !10.15.12 TN: Can't this go after the iteration in the main call?
call setStorageVars(stored,nS,info)


!Calculate the new field inlet temperature. Stick with the same temperature calculation mode as determined on the first call.
!MJW 12.9.2010----
!if(info(7)==0) then  
if((info(7)==0).or.(mode0 /= mode)) then  !mjw 3.29.11
    !if((m_dot_pb + ms_charge - ms_disch) > 0.d0) then 
    if(m_dot_field > 0.) then  !mjw 1.12.2011 Be consistent with the criteria above
        tempmode = 1
    else
        tempmode = 2
    endif
endif
!If during iteration, the mass flow ends up being zero, switch to temperature mode 2
if(m_dot_pb + ms_charge == 0.) tempmode = 2
select case(tempmode)
case(1)
    T_field_inX = (m_dot_pb*T_pb_out + ms_charge*Ts_cold)/(m_dot_pb + ms_charge)
case(2)
    T_field_inX = dmin1(T_field_out,T_startup)
end select
!--------

!convergence error is based on actual field mass flow rate compared to balance of mass flow rates
err = sqrt(abs(((ms_charge + m_dot_pb - ms_disch) - m_dot_field)/dmax1(m_dot_field,1.e-6))**2 + ((T_field_in - T_field_inX)/T_field_in)**2)
derr = abs((err - errX)/errX)
errX = err

!Limit number of iterations
if(iter > 20) then
    !Write the error message in preparation for failure, but don't call the printer yet.. 
    write(message(3),fmt=200) iter,dmin1(err,999.d0),tol
    200 format("After ",I2," iterations, the controller convergence error of ",E8.3," could not resolve below the tolerance of ",F6.4)
    msgiter=3.
    !continue on, don't iterate further
else
    !If the defocus control has changed during the calculations, bypass iteration and recalculate field output.
    !Otherwise, the iteration will continue until the max # has been reached.
    !if(defocus /= defocusX) then
    if(((defocus /= defocusX) .and. (iter > 1)).and.(defocus <= 1.d0)) then  !mjw 1.18.2011 (energy isn't balancing during defocusing with no TES
        defocusX = defocus
        T_field_in = T_field_inX  !MJW 12.8.2010
        continue
    else
        !We are primarily concerned with whether the solution has converged. If the change in the error has stabilized,
        !it is save to move on.
        if(derr < 0.001) then
            continue
        else
        !If the error hasnt stabilized and the absolute error is greater than the tolerance, iterate again
        if(err > tol)THEN
            goto 50
        endif  
        endif
        !no errors occured, convergence acheived. Move on
        msgiter=0.
    endif
endif

if(is_hx) then
    !-----Calculate flows through the heat exchanger
    !Charging cycle: Collector is hot side - Tanks are cold side
    if(ms_charge > 0.001) then
        call hx_perf(hx_ua, hx_config, field_fluid, store_fluid, ms_charge, T_field_out, T_tank_cold_out, &
                     dT_hot, dT_cold, hx_eff, Ts_cold, T_tank_hot_in, q_hx, m_tank_charge, msghx2)
        m_tank_disch = 0.
    !Discharging cycle: Tanks are hot side - Collector loop is cold side
    elseif(ms_disch > 0.001) then
        call hx_reverse(hx_ua, hx_config,  store_fluid, field_fluid, ms_disch, T_tank_hot_out, T_pb_out, &
                        dT_hot, dT_cold, hx_eff, T_tank_cold_in, Ts_hot, q_hx, m_tank_disch, msghx3)
        m_tank_charge = 0.
    else
        !No flow across the storage tank
        hx_eff = 1.
        Ts_cold = T_pb_out
        Ts_hot = T_field_out
        T_tank_cold_in = T_field_in
        T_tank_hot_in = T_field_out
        m_tank_charge = 0. 
        m_tank_disch = 0.   
    endif
else
    IF(TES_type==2)THEN
        hx_eff = 0.d0
    ELSE
        !No heat exchanger
        hx_eff = 0.
        !T_tank_cold_in = T_field_in
        T_tank_cold_in = T_pb_out  !mjw 3.10.11
        T_tank_hot_in = T_field_out
        Ts_cold = T_tank_cold_out
        Ts_hot = T_tank_hot_out
        m_tank_charge = ms_charge
        m_tank_disch = ms_disch
    ENDIF
endif

!-----Update the tanks
if(tshours > 0.) then
    if(TES_type==1) then    !mjw 4.26.11 : 2-tank storage
        !hot tank
        call mixed_tank(& !Inputs and parameters
                              vol_tank,h_tank,h_tank_min,u_tank,tank_pairs,hot_tank_Thtr,tank_max_heat,&
                              m_tank_hot0,T_tank_hot0, m_tank_charge,m_tank_disch,T_tank_hot_in,T_amb,store_fluid,&
                                !outputs
                              T_tank_hot_ave,vol_hot_tank_ave,q_loss_hot_tank,T_tank_hotf,&
                              v_tank_hotf,m_tank_hotf,q_tank_hot_htr)
        !Cold tank
        call mixed_tank(& !Inputs and parameters
                              vol_tank,h_tank,h_tank_min,u_tank,tank_pairs,cold_tank_Thtr,tank_max_heat,&
                              m_tank_cold0,T_tank_cold0, m_tank_disch,m_tank_charge,T_tank_cold_in,T_amb,store_fluid,&
                                !outputs
                              T_tank_cold_ave,vol_cold_tank_ave,q_loss_cold_tank,T_tank_coldf,&
                              v_tank_coldf,m_tank_coldf,q_tank_cold_htr)

        errt = sqrt(((T_tank_hotX - T_tank_hot_ave)/dmax1(T_tank_hot_ave,1.e-6))**2 + ((T_tank_coldX - T_tank_cold_ave)/dmax1(T_tank_cold_ave,1.e-6))**2)
        T_tank_hotX = T_tank_hot_ave
        T_tank_coldX = T_tank_cold_ave  
        if((errt>1.e-5).and.(itert<10)) goto 30 !Iterate to get tank temperature right
    elseif(TES_type==2) then    !mjw 4.26.11 : Thermocline storage
!        T_tank_hotf = T_TES_hot
!        T_tank_coldf = T_TES_cold
!        q_loss_hot_tank = q_TES_hl_in
!        q_loss_cold_tank = 0.d0
!        v_tank_hotf = 0.d0; m_tank_hotf = 0.d0; v_tank_coldf=0.d0; m_tank_coldf=0.d0
        
        
    else; endif
    
    
else
    T_tank_hot_ave = 0. ; vol_hot_tank_ave = 0. ; q_loss_hot_tank = 0. ; T_tank_hotf = 0. ; v_tank_hotf = 0. ; q_tank_hot_htr = 0.
    T_tank_cold_ave = 0. ; vol_cold_tank_ave = 0. ; q_loss_cold_tank = 0. ; T_tank_coldf = 0. ; v_tank_coldf = 0. ; q_tank_cold_htr = 0.
endif 

!-----Update the aux heater
if(qaux_avail>0.) then
                    !Inputs                                                         | Outputs
    call aux_heater(field_fluid, q_max_aux, LHV_eff, T_set_aux, T_pb_out, m_dot_aux, T_aux_act, q_aux_fluid, q_aux_fuel)
else
    T_aux_act = 0. ; q_aux_fluid = 0. ; q_aux_fuel = 0.
endif

defocus = dmin1(defocus0*defocus, 1.d0)         !9-27-12, TWN: Need to send absolute defocus to tower models. Have modified trough model to adjust back to relative

!Store the active mode
stored(5) = mode
stored(9) = defocus  !mjw 1.18.2011
call setStorageVars(stored,nS,info)

100 continue    !why is this here?

!******************************************************************
! Calculate final output values
!******************************************************************
!mjw 12.8.2010
if(m_dot_pb > 0.) then
    pb_on = 1.
else
    pb_on = 0.
endif

!Pumping power
if(is_hx) then
    htf_pump_power = (TES_pump_coef*abs(m_tank_disch-m_tank_charge) + PB_pump_coef*(abs(ms_disch-ms_charge) + m_dot_pb))/1000.  !MW
else
    !If no HX is used, then no separate TES pump is required (MJW added 6-11-2010)
    htf_pump_power = PB_pump_coef*(abs(ms_disch-ms_charge) + m_dot_pb)/1000. !MW
endif

!Variable parasitic power
q_pb = m_dot_pb*(c_htf_pb*(T_pb_in - T_pb_out)) !MJW 7.12.2010: changed specific heat from field-tied to power block-tied 
if(q_pb>0.) then
    BOP_par = W_pb_design*BOP_parVal*BOP_parPF*dmin1(BOP_par0 + BOP_par1*(q_pb/q_pb_design)+BOP_par2*(q_pb/q_pb_design)**2,cycle_max_fraction)/1.e6 !MW
else
    BOP_par = 0.d0
endif
!Aux load parasitic power
if(m_dot_aux > 0.) then
    Aux_par = W_pb_design*Aux_parVal*Aux_parPF*dmin1(Aux_par0 + Aux_par1*(q_aux_fluid/q_pb_design)+Aux_par2*(q_aux_fluid/q_pb_design)**2,cycle_max_fraction)/1.e6 !MW
else
    Aux_par = 0.d0
endif
!calculate the thermal load on TES
if(TSHOURS>0.d0) then
    !c_tes_ave = specheat(field_fluid, (Ts_hot+Ts_cold)/2.d0, 101325.d0)*1000.d0
    !q_to_tes = (ms_charge - ms_disch)*c_tes_ave*(Ts_hot - Ts_cold)/1.e6 ![MWt]  MJW 11.9.2010
    if(ms_disch /= 0.) q_to_tes = -ms_disch*c_htf_dis*(Ts_hot - T_pb_out)*1.e-6 !mjw 1.19.2010
    if(ms_charge /= 0.) q_to_tes = ms_charge*c_htf_chg*(T_field_out - Ts_cold)*1.e-6 !mjw 1.19.2010
    if(ms_disch == 0. .and. ms_charge == 0.) q_to_tes = 0.  !mjw 1.19.2010
else
    q_to_tes = 0.d0
endif

!If packed bed model has not yet been called, call it under "idle" conditions to find losses and temperature profile at next timestep
!Would like to move this to final timestep call, but then couldn't report losses and the such to printer
!New TRNSYS may be able to do this.....
IF((TES_type==2).and.(.not.(called_TC)))THEN
            XIN_TC(1)   = T_field_out - 273.15d0    ![C] Charging temperature must come from field outlet
            XIN_TC(2)   = 0.d0                      ![kg/hr] Charging flow rate
            XIN_TC(3)   = T_pb_out - 273.15d0       ![C] Discharging temperature must come from power block
            XIN_TC(4)   = 0.d0                      ![kg/hr] Don't need to know discharging flow rate because this call is just check availability
            XIN_TC(5)   = T_amb - 273.15d0          ![C] Ambient temperature 
            XIN_TC(6)   = 1.d0                      ![-] Flag for just checking availability (1 = calculate packed bed performance)
            XIN_TC(7)   = 0.d0                      ![W] Discharge energy rate demanded
            XIN_TC(8)   = 0.d0                      ![W] Charge energy rate demanded
            XIN_TC(9)   = f_storage                 ![-] Storage dispatch fraction
            
            CALL PackedBed(TIME,XIN_TC,OUT_TC,PAR_TC,INFO)
            called_TC   = .true.                    ![-] Packed bed model has been called during timestep
ENDIF

!Assign thermocline outputs to variables
IF(TES_type==2)THEN
    q_tank_cold_htr = OUT_TC(12)/(1000.d0*dt)  ![kJ]*(1/1000)[MJ/kJ]*[1/s] => MWe Parasitic input
    q_tank_hot_htr = 0.d0           ![kJ] No minimum hot temp can be enforced in thermocline
    q_loss_hot_tank = OUT_TC(6)/(1000.d0*dt)   ![kJ]*(1/1000)[MJ/kJ]*[1/s] => MWt Heat losses
    q_loss_cold_tank = 0.d0         ![MWt] Only calculate losses from 1 tank
    
    !Storage outputs only relevant to 2-tank storage, set to 0
    !Can these be assigned just once in the info(7) = -1 call?
    T_tank_hot_ave = 0. ; vol_hot_tank_ave = 0. ; T_tank_hotf = 0. ; v_tank_hotf = 0. ;
    T_tank_cold_ave = 0. ; vol_cold_tank_ave = 0. ; T_tank_coldf = 0. ; v_tank_coldf = 0.
        
ENDIF

!outputs
out(1) = defocus 
out(2) = cycle_pl_control 
out(3) = standby_control 
out(4) = m_dot_pb*3600.             !demand var - [kg/hr]  Mass flow rate to the power block
out(5) = T_pb_in - 273.15           ![C] HTF Temperature to the power block
out(6) = T_field_in  - 273.15       ![C] HTF temperature into the collector field header
out(7) = (ms_disch-ms_charge)*3600. ![kg/hr] Mass flow rate of the collector side storage loop
out(8) = (m_tank_disch-m_tank_charge)*3600. ![kg/hr] Mass flow rate of the tank side storage loop
out(9) = Ts_hot - 273.15            ![C] Fluid temperature of the hot HTF in the collector loop
out(10) = Ts_cold - 273.15          ![C] Fluid temperature of the cold HTF in the collector loop
out(11) = T_tank_hot_in - 273.15    ![C] Hot tank inlet temperature
out(12) = T_tank_cold_in - 273.15   ![C] Cold tank inlet temperature
out(13) = v_tank_hotf               ![m3] Available Hot tank HTF volume at the end of the timestep
out(14) = T_tank_hotf - 273.15      ![C] Hot tank stored HTF temperature at the end of the timestep
out(15) = v_tank_coldf              ![m3] Available Cold tank HTF volume at the end of the timestep
out(16) = T_tank_coldf - 273.15     ![C] Cold tank stored HTF temperature at the end of the timestep
out(17) = (q_tank_hot_htr + q_tank_cold_htr)/eta_heater_tank  ![MWe] Total parasitic power required for tank freeze protection
out(18) = m_dot_aux*3600.           ![kg/hr] Auxiliary heater mass flow rate
out(19) = q_aux_fluid               ![MWt] Thermal energy provided to the fluid passing through the aux heater
out(20) = q_aux_fuel                ![MMBTU] Heat content of fuel required to provide aux firing
out(21) = v_tank_hotf + v_tank_coldf  ![m3] Total accounted htf volume in storage
out(22) = hx_eff                    ![-] Heat exchanger effectiveness
out(23) = m_tank_hotf               ![kg] mass of total fluid in the hot tank
out(24) = m_tank_coldf              ![kg] mass of total fluid in the cold tank
out(25) = sum(out(23:24))           ![kg] total mass in the storage system
out(26) = htf_pump_power            ![MWe] pumping power for storage, power block loops
out(27) = BOP_par                   ![MWe] parasitic power as a function of power block load
out(28) = W_pb_design*PB_fixed_par/1.e6  ![MWe] Fixed parasitic power losses.. for every hour of operation
out(29) = Aux_par                   ![MWe] Parasitic power associated with operation of the aux boiler
out(30) = dmax1(q_pb/1.e6,0.d0)     ![MWt] Thermal energy to the power block
out(31) = (q_loss_cold_tank+q_loss_hot_tank) ![MWt] Tank thermal losses
out(32) = q_to_tes                  ![MWt] Thermal energy into (and out of) storage
out(33) = float(mode)               

IF(TES_type==2)THEN
    OUT(34) = OUT_TC(7)             ![C] Final temperature at hot node
    OUT(35) = OUT_TC(8)             ![C] Final temperature at cold node
    OUT(36) = OUT_TC(9)             ![C] Maximum temperature in TC    
    OUT(37) = OUT_TC(10)            ![-] Fraction of depth at which hot temperature decreases below minimum hot temperature limit
    OUT(38) = OUT_TC(11)            ![-] Fraction of depth at which cold temperature increases above maximum cold temperature limit
ELSE
    OUT(34:38) = 0.d0
ENDIF
    

return 1

end subroutine


!Include all this stuff because we need to know the condition of the storage tanks and the 
!heat exchanger during the control decision process.  This will keep the control strategy from
!calling and iterating needlessly. Store the ending tank volume levels for the next timestep, 
!and the available storage level will always be known. The aux heater might also need to be included
!in this component.
!The rankine cycle component might need to be run in supply mode, with the controller trying to get
!as close to design as possible based on the calculations.

!**************************************************************************************************
!**************************************************************************************************

!incrementing function: when called, this function increments one integer value.
!if called with an argument, the counter resets to 0. 
integer function up1(i)
    implicit none
    integer::j,i ; save j
    j=j+1
    if(i==0) j = 0
    up1=j
end function


!Subroutine for storage tanks based on type230
!**************************************************************************************************
!**************************************************************************************************
subroutine mixed_tank(& !Inputs and parameters
                      vol,h,h_min,u,tank_pairs,htr_set_point,max_heat_rate,&
                      m0,T0,m_dot_in,m_dot_out,T_in,T_amb,fluid,&
                        !outputs
                      T_ave,vol_ave,q_loss,T_fin,vol_fin,m_fin,q_heater)
use TrnsysFunctions

!----------Inputs and parameters-------------------------------------------------------------------
!   * vol       - [m3] Total tank volume, including the unusable volume of HTF at the bottom
!   * h         - [m] Total height of the tank (height of HTF when tank is full)
!   * u         - [W/m2-K] Loss coefficient from the tank
!   * tank_pairs- [-] Number of equivalent tank pairs
!   * htr_set_point - [K] Minimum allowable fluid temperature before auxiliary heater turns on
!   * m0        - [kg] total HTF mass in the tank at the end of the last time step
!   * T0        - [K] Temperature of the HTF at the end of the last time step
!   * m_dot_in  - [kg/s] mass flow rate of HTF into the tank
!   * m_dot_out - [kg/s] mass flow rate leaving the tank
!   * T_in      - [K] Temperature of the HTF entering the tank
!   * T_amb     - [K] Temperature of the ambient air
!----------Outputs---------------------------------------------------------------------------------
!   * T_ave     - [K] Average HTF temperature throughout the timestep
!   * vol_ave   - [m3] Average HTF volume level during the timestep
!   * q_loss    - [MWt] Total thermal loss rate from the tank
!   * T_fin     - [K] Temperature of the HTF at the end of the timestep
!   * vol_fin   - [m3] Volume of the HTF at the end of the timestep (total volume)
!   * m_fin     - [kg] total mass at the end of the timestep
!   * q_heater  - [MWt] Total energy consumed by the freeze protection heater
!--------------------------------------------------------------------------------------------------
implicit none

!Inputs
real(8),intent(in):: vol,h,h_min,u,tank_pairs,htr_set_point,max_heat_rate,&
                      m0,T0,m_dot_in,m_dot_out,T_in,T_amb,fluid
!Outputs
real(8),intent(out):: T_ave,vol_ave,q_loss,T_fin,vol_fin,m_fin,q_heater

!Locals
real(8):: pi, dt, acs, vol_min, vol_max, dia, ua, density, rho, m_ave, vol0, &
          h_ave, cp, specheat, B, C, D, CC, DD, AA, BB, G, H1, A1, E, &
          Q_vol, Q_flow
!--------------------------------------------------------------------------------------------------
pi=3.1415926
dt= getSimulationTimeStep()*3600.  ![s]

!Geometrical calculations
vol_min = vol*h_min/h       ![m3] Minimum HTF volume among all tanks
vol_max = vol*(1.-h_min/h)  ![m3] Maximum HTF volume among all tanks
acs = vol/(h*tank_pairs)    ![m2] Cross-sectional area of a single tank
dia = (acs/pi)**0.5*2.      ![m] The diameter of a single tank

!Calculate heat loss coefficient
ua = u*(acs + pi*dia*h)*tank_pairs  !u [W/m2-K] 10.16.12 tn: units should be [W/K]

!Calculate conditions at last time step
rho = density(fluid,T0,1.d0)  !density
cp = specheat(fluid,T0,1.d0)*1000.  ![J/kg-K] specific heat
!m0 = ma0 + vol_min*rho              ![kg] mass of total HTF, Add back on the portion of discarded volume 
vol0 = m0/rho                       ![m3] volume of total htf

!Calculate ending volume levels
m_fin = dmax1(0.001d0, m0 + dt*(m_dot_in - m_dot_out))  ![kg] Available mass at the end of the timestep, mjw 8.3.11 limit to nonzero positive number
m_ave = (m0 + m_fin)/2.d0               ![kg] Average mass 
vol_fin = m_fin/rho                    ![m3] Available volume at the end of the timestep
vol_ave = m_ave/rho                    ![m3] Average volume
h_ave = vol_ave/acs                     ![m] Average HTF height

!Check for no flow
B = m_dot_in + ua/cp
if((dabs(m_dot_in-m_dot_out) < B*1.e-5).or.&
   ((m_dot_in < 0.001).and.(m_dot_out < 0.001))) then
    
    !Equations for no flow or zero net flow
    D = m_dot_in*T_in + (UA/cp)*T_amb
    G = -B/m0
    H1 = 1.0/(dt*(-B))
    A1 = D - B*T0
    E = A1 * exp(dt*G)
    T_fin = (E-D)/(-B)
    T_ave = H1*((E-A1)/G)+D/B
else
    !Equations for unbalanced flow
    C = m_dot_in - m_dot_out
    D = m_dot_in*T_in + ua/cp*T_amb
    CC = T0 - D/B
    DD = dmax1((1.+(C*dt)/m0),0.d0)**(-B/C)  !MJW 9.2.2010 :: limit to positive argument
    T_fin = CC*DD+D/B
    AA = (T0 - D/B)/(C-B)
    BB = (1.+(C*dt)/m0)**(1.-B/C)
    T_ave = AA*(m0/dt)*(BB-1.0) + D/B
endif

!If the temperature of the fluid in the tank is less than the 
!..aux heater set point, heat the fluid in - and passing through - 
!..the tank. MJW
if(T_fin.lt.htr_set_point) then
    !Q_vol=cp*vol_fin*rho/(dt*3600.)*(htr_set_point-T_fin)/1000.  !MW
    Q_vol=cp*vol_fin*rho/dt*(htr_set_point-T_fin)/(1.E6)        !MW  4/30/12 - Fixed unit conversion
    !Q_flow=cp*m_dot_out/3600.*dt/(dt*3600.)*(htr_set_point-T_fin)/1000.  !MW
    Q_flow=cp*m_dot_out*(htr_set_point-T_fin)/(1.E6)            !MW  4/30/12 - Fixed unit conversion
    
    Q_heater = dmin1(Q_flow+Q_vol,max_heat_rate) !MW
    T_fin=T0+dt*dmin1(Q_vol*1.e6,max_heat_rate*1.e6)/(cp*rho*vol_fin)
    T_ave=(T_fin+T0)/2.
else
    Q_heater = 0.
endif

!Calculate losses
q_loss = UA*(T_ave - T_amb)/1.e6    ![MW]


end subroutine

!**************************************************************************************************
!**************************************************************************************************
!Subroutine for sizing a heat exchanger
subroutine hx_size(config,duty,dT_hot,dT_cold,T_h_in,T_h_out,fluid_h,fluid_c,eta,UA,mesg)

use global_props
implicit none

!This subroutine provides the design size for the collector field to storage heat 
!exchanger.
!Inputs required:
!--------------------------------------------------------------------------------------------------
!   * config    - Heat exchanger configuration  {1=Same fluid, 2=Counter-flow, 3=Parallel-flow, 4=Cross-flow, 5=shell-and-tube}
!   * duty      - The heat exchanger duty (MWt)
!   * dT_hot    - Hot side approach temperature [K]
!   * dT_cold   - Cold side approach temperature [K]
!   * T_h_in    - Design hot side inlet temperature [K]
!   * T_h_out   - Design hot side outlet temperature [K]
!   * fluid_h   - Hot side fluid material number [-]
!   * fluid_c   - Cold side fluid material number [-]
!Outputs:
!--------------------------------------------------------------------------------------------------
!   * eta       - Heat exchanger effectiveness at design [-]
!   * UA        - Heat exchanger size [W/K]
!--------------------------------------------------------------------------------------------------

!inputs
real(8),intent(in)::duty,dT_hot,dT_cold,T_h_in,T_h_out,fluid_h,fluid_c
integer,intent(in)::config
!outputs
real(8),intent(out)::eta, UA, mesg
!locals
real(8):: T_ave, specheat, c_c, c_h, T_c_in, T_c_out, m_dot_h, m_dot_c, q_trans,&
          c_dot_max, c_dot_min, c_dot_h, c_dot_c, CR, q_max, NTU, EE, EE1, FF, xx
mesg=0.d0

q_trans = duty     ![W] heat exchanger duty

T_ave = (T_h_in + T_h_out)/2.

!Fluid properties
c_h = specheat(fluid_h,T_ave,1.d0)*1000.
c_c = specheat(fluid_c,T_ave,1.d0)*1000.
!Required temperatures
T_c_out = T_h_in - dT_hot
T_c_in = T_h_out - dT_cold
!Mass flow rates
m_dot_h = q_trans/(c_h*(T_h_in - T_h_out))
m_dot_c = q_trans/(c_c*(T_c_out - T_c_in))
!Capacitance rates
c_dot_h = m_dot_h*c_h
c_dot_c = m_dot_c*c_c
c_dot_max = dmax1(c_dot_h,c_dot_c)
c_dot_min = dmin1(c_dot_h,c_dot_c)
!Capacitance ratio
CR = c_dot_min/c_dot_max
!Maximum possible energy flow
q_max = c_dot_min*(T_h_in - T_c_in)
!Effectiveness
eta = q_trans/q_max

!Heat exchanger configuration correlations
select case(config)
case(1) !Same-fluid     //10-9-12, TN: this doesn't make sense. same fluid would roughly have a capacitance ratio of 1, not 0...
    NTU = -log(1-eta)
case(2) !counter-flow
    !check for no solution
    xx=(1-eta*CR)/(1-eta)
    if(xx < 1.) then
        mesg=1.
    endif
    if (CR /= 1.) then
        NTU = log((1-eta*CR)/(1-eta))/(1-CR)
    else
        NTU = eta/(1-eta)
    endif
case(3) !Parallel-flow
    xx=1.-eta*(1.+CR)
    if(xx < 1.) then
        mesg=1.
    endif

    NTU = log(1.-eta*(1.+CR))/(1.+CR)
case(4) !Cross-flow, unmixed
    xx=1.+log(1.-eta*CR)/CR
    if(xx <= 0.) then
        mesg=1.
    endif

    NTU = -log(1.+log(1.-eta*CR)/CR)
case(5) !Shell-and-tube (assume 1 pass)
    xx=(EE+1.)/(EE-1.)          !10-10-12, TN: EE not defined!
    if(xx < 1.) then
        mesg=1.
    endif
    
    FF = (eta*CR-1.)/(eta - 1.)
    EE1 = (FF-1.)/(FF-CR)
    EE = (2.-EE1*(1.+CR))/(EE1*sqrt(1.+CR*CR))
    NTU = log((EE+1.)/(EE-1.))/sqrt(1.+CR*CR)
end select

!Calculate heat exchanger size
UA = NTU*c_dot_min

end subroutine

!--------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------


!Subroutine for heat exchanger performance. Provide the hot side mass flow rate, inlet temps, delta temps.
subroutine hx_perf(UA, config, fluid_h, fluid_c, m_dot_h, T_h_in, T_c_in, dT_hot, dT_cold,&
                 !outputs
                 eta, T_h_out, T_c_out, q_trans, m_dot_c,msg)

implicit none

!This subroutine models the heat exchanger between the field HTF and the storage HTF
!Inputs required:
!--------------------------------------------------------------------------------------------------
!   * UA        -   Heat exchanger conductance.  Calculated in the SAM interface [W/K]
!   * config    -   Configuration.. {1=Same fluid, 2=Counter-flow, 3=Parallel-flow, 4=Cross-flow, 5=shell-and-tube}
!   * fluid_h   -   Hot side fluid number (see type 229)
!   * fluid_c   -   Cold side fluid number (see type 229)
!   * m_dot_h   -   Hot side mass flow rate [kg/s]
!   * T_h_in    -   Hot side inlet temperature [K]
!   * T_c_in    -   Cold side inlet temperature [K]
!   * dT_hot    -   Hot side approach temperature
!   * dT_cold   -   Cold side approach temperature
!--------------------------------------------------------------------------------------------------
!Outputs provided:
!   * eta       -   Heat exchanger effectiveness [-]
!   * T_h_out   -   Hot side outlet temperature [K]
!   * T_c_out   -   Cold side outlet temperature [K]   
!   * q_trans   -   Total heat transferred between fluids [MWt]
!   * m_dot_c   -   Cold side HTF flow rate [kg/s]
!--------------------------------------------------------------------------------------------------

!Inputs
real(8),intent(in):: UA, fluid_h, fluid_c, m_dot_h, T_h_in, T_c_in, dT_hot, dT_cold
integer,intent(in):: config
!Outputs
real(8),intent(out):: eta, T_h_out, T_c_out, q_trans, m_dot_c, msg
!Locals
real(8):: NTU, CR, c_dot_min, c_dot_max, c_h, c_c, c_dot_h, c_dot_c, specheat, T_ave, q_max, eta1
msg=0.d0

!-------------All configurations
!Calculate outlet temps 
T_h_out = T_c_in + dT_cold
T_c_out = T_h_in - dT_hot

!look up fluid properties. Assume average specific heat based on inlet temperatures of alternate streams
T_ave = (T_h_in + T_c_in)/2.
c_h = specheat(fluid_h,T_ave,1.d0)*1000. ![J/kg-K]
c_c = specheat(fluid_c,T_ave,1.d0)*1000. ![J/kg-K]

!Calculate heat flow
q_trans = dmax1(m_dot_h * c_h * (T_h_in - T_h_out),0.d0)

!Calculate cold side flow
m_dot_c = q_trans/(c_c * (T_c_out - T_c_in))

!Calculate capacity rates
c_dot_c = m_dot_c*c_c
c_dot_h = m_dot_h*c_h

!Calculate min/max rates, ratio
c_dot_min = dmin1(c_dot_c,c_dot_h)
c_dot_max = dmax1(c_dot_c,c_dot_h)
CR = c_dot_min/c_dot_max

!Calculate non-dimensional HX size
NTU = UA/C_dot_min

!Calculate the effectiveness of the heat exchanger 
select case(config)
case(1) !Same fluid
    eta = 1. - exp(-NTU)
case(2) !Counter-flow
    if(CR < 1.) then
        eta = (1.-exp(-NTU*(1.-CR)))/(1.-CR*exp(-NTU*(1.-CR)))
    else
        eta = NTU/(1.+NTU)
    endif
case(3) !Parallel-flow
    eta = (1.-exp(-NTU*(1.+CR)))/(1.+CR)
case(4) !Cross-flow (unmixed)
    eta = 1.-exp(NTU**0.22/CR*(exp(-CR*NTU**0.78)-1.))
case(5) !Shell-and-tube (assume 1 pass)
    eta1 = 2.*(1.+CR+sqrt(1.+CR*CR)*((1.+exp(-NTU*sqrt(1.+CR*CR)))/(1.-exp(-NTU*sqrt(1.+CR*CR)))))**(-1)
    eta = ((1.-eta1*CR)/(1.-eta1)-1.)/((1.-eta1*CR)/(1.-eta1)-CR)
end select

if ((eta <= 0.) .or. (eta > 1.)) then
    msg=2.
endif

q_trans = q_trans*1.e-6     ![MWt]

end subroutine      
              
!--------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------

!Subroutine for heat exchanger performance. Provide the COLD side mass flow rate, inlet temps, delta temps.
!The subroutine will calculate the hot side mass flow rate required to match cold conditions.  Given an 
!amount of cold side fluid, this will calculate the corresponding hot side flow.
subroutine hx_reverse(UA, config, fluid_h, fluid_c, m_dot_c, T_h_in, T_c_in, dT_hot, dT_cold,&
                      !outputs
                      eta, T_h_out, T_c_out, q_trans, m_dot_h,msg)

implicit none

!This subroutine models the heat exchanger between the field HTF and the storage HTF
!Inputs required:
!--------------------------------------------------------------------------------------------------
!   * UA        -   Heat exchanger conductance.  Calculated in the SAM interface [W/K]
!   * config    -   Configuration.. {1=Same fluid, 2=Counter-flow, 3=Parallel-flow, 4=Cross-flow, 5=shell-and-tube}
!   * fluid_h   -   Hot side fluid number (see type 229)
!   * fluid_c   -   Cold side fluid number (see type 229)
!   * m_dot_c   -   Cold side mass flow rate [kg/s]
!   * T_h_in    -   Hot side inlet temperature [K]
!   * T_c_in    -   Cold side inlet temperature [K]
!   * dT_hot    -   Hot side approach temperature
!   * dT_cold   -   Cold side approach temperature
!--------------------------------------------------------------------------------------------------
!Outputs provided:
!   * eta       -   Heat exchanger effectiveness [-]
!   * T_h_out   -   Hot side outlet temperature [K]
!   * T_c_out   -   Cold side outlet temperature [K]   
!   * q_trans   -   Total heat transferred between fluids [MWt]
!   * m_dot_h   -   Hot side mass flow rate [kg/s]
!--------------------------------------------------------------------------------------------------

!Inputs
real(8),intent(in):: UA, fluid_h, fluid_c, m_dot_c, T_h_in, T_c_in, dT_hot, dT_cold
integer,intent(in):: config
!Outputs
real(8),intent(out):: eta, T_h_out, T_c_out, q_trans, m_dot_h, msg
!Locals
real(8):: NTU, CR, c_dot_min, c_dot_max, c_h, c_c, c_dot_h, c_dot_c, specheat, T_ave, q_max, eta1
msg=0.d0

!-------------All configurations
!Calculate outlet temps 
T_h_out = T_c_in + dT_cold
T_c_out = T_h_in - dT_hot

!look up fluid properties. Assume average specific heat based on inlet temperatures of alternate streams
T_ave = (T_h_in + T_c_in)/2.
c_h = specheat(fluid_h,T_ave,1.d0)*1000. ![J/kg-K]
c_c = specheat(fluid_c,T_ave,1.d0)*1000. ![J/kg-K]

!Calculate heat flow
q_trans = dmax1(m_dot_c * c_c * (T_c_out - T_c_in),1.e-6)

!Calculate hot side flow
m_dot_h = q_trans/(c_h * (T_h_in - T_h_out))

!Calculate capacity rates
c_dot_c = m_dot_c*c_c
c_dot_h = m_dot_h*c_h

!Calculate min/max rates, ratio
c_dot_min = dmin1(c_dot_c,c_dot_h)
c_dot_max = dmax1(c_dot_c,c_dot_h)
CR = c_dot_min/dmax1(c_dot_max,1.e-6)

!Calculate non-dimensional HX size
NTU = UA/C_dot_min

!Calculate the effectiveness of the heat exchanger 
select case(config)
case(1) !Same fluid
    eta = 1. - exp(-NTU)
case(2) !Counter-flow
    if(CR < 1.) then
        eta = (1.-exp(-NTU*(1.-CR)))/(1.-CR*exp(-NTU*(1.-CR)))
    else
        eta = NTU/(1.+NTU)
    endif
case(3) !Parallel-flow
    eta = (1.-exp(-NTU*(1.+CR)))/(1.+CR)
case(4) !Cross-flow (unmixed)
    eta = 1.-exp(NTU**0.22/CR*(exp(-CR*NTU**0.78)-1.))
case(5) !Shell-and-tube (assume 1 pass)
    eta1 = 2.*(1.+CR+sqrt(1.+CR*CR)*((1.+exp(-NTU*sqrt(1.+CR*CR)))/(1.-exp(-NTU*sqrt(1.+CR*CR)))))**(-1)
    eta = ((1.-eta1*CR)/(1.-eta1)-1.)/((1.-eta1*CR)/(1.-eta1)-CR)
end select

if ((eta <= 0.) .or. (eta > 1.)) then
    msg=2.
endif

q_trans = q_trans*1.e-6     ![MWt]

end subroutine      

!**************************************************************************************************
!**************************************************************************************************

!Subroutine for auxiliary heater - simplified/modified version of TRNSYS Type6 aux heater
subroutine aux_heater(& !Inputs                                | Outputs
                      fluid, qmax, lhv_eff, T_set, T_in, m_dot, T_out, q_fluid, q_fuel)

!--------------------------------------------------------------------------------------------------
! Model inputs
!   * fluid     - [-] Fluid type
!   * qmax      - [MWt] Maximum heating rate 
!   * lhv_eff   - [-] Lower heating value efficiency of the fuel
!   * T_set     - [K] Fluid outlet temperature set point
!   * T_in      - [K] Inlet fluid temperature
!   * m_dot     - [kg/s] Fluid mass flow rate
!--------------------------------------------------------------------------------------------------
! Model outputs
!   * T_out     - [K] Achieved fluid outlet temperature
!   * q_fluid   - [MW] Thermal energy delivered to the fluid
!   * q_fuel    - [MMBTU] Thermal energy content required in fuel
!--------------------------------------------------------------------------------------------------
use TrnsysFunctions

implicit none

!Inputs
real(8),intent(in):: fluid, qmax, lhv_eff, T_in, m_dot, T_set
!Outputs
real(8),intent(out)::T_out, q_fluid, q_fuel
!Locals
real(8):: specheat, cp, dthr
!--------------------------------------------------------------------------------------------------
dthr = getSimulationTimeStep()

!cp = specheat(fluid,T_in,1.d0)*1000. ![J/kg-K]
cp = specheat(fluid,(T_in+T_set)/2.d0,1.d0)*1000. ![J/kg-K]  MJW 11.3.2010: Use the average temperature for cp

!T_out = dmin1(T_set, (T_in + (qmax*1.e6)/(m_dot*cp)))  ![K]
T_out = dmin1(T_set, (T_in + (qmax)/(m_dot*cp)))    ![K] 10.17.12 tn: don't multiply by 1.e6! already in W
q_fluid = m_dot*cp*(T_out - T_in)/1.e6          ![MWt]
q_fuel = q_fluid/lhv_eff*dthr*3.41214116        ![MMBTU]

end subroutine
