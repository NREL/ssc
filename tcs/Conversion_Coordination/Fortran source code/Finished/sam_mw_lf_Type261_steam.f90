SUBROUTINE TYPE261 (TIME, XIN, OUT, T, DTDT, PAR, INFO, ICNTRL, *)
!************************************************************************
! Object: Linear Fresnel - steam receiver
! Simulation Studio Model: Type261
! 
! Author: Michael J. Wagner
! Date:	 November, 2011

! COPYRIGHT 2011 NATIONAL RENEWABLE ENERGY LABORATORY

! Doc. tables updated 2011-11-09 - MJW
!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Parameters
!    1| NUMTOU                           | Number of time-of-use periods                                     | none             | none             
!    2| TSHOURS                          | Equivalent full-load thermal storage hours                        | hr               | hr               
!    3| q_max_aux                        | Maximum heat rate of the auxiliary heater                         | MW               | W                
!    4| LHV_eff                          | Fuel LHV efficiency (0..1)                                        | none             | none             
!    5| T_set_aux                        | Aux heater outlet temperature set point                           | C                | K                
!    6| T_field_in_des                   | Field design inlet temperature                                    | C                | K                
!    7| T_field_out_des                  | Field loop outlet design temperature                              | C                | K                
!    8| x_b_des                          | Design point boiler outlet steam quality                          | none             | none             
!    9| P_turb_des                       | Design-point turbine inlet pressure                               | bar              | bar              
!   10| fP_hdr_c                         | Average design-point cold header pressure drop fraction           | none             | none             
!   11| fP_sf_boil                       | Design-point pressure drop across the solar field boiler fraction | none             | none             
!   12| fP_boil_to_SH                    | Design-point pressure drop between the boiler and superheater frac| none             | none             
!   13| fP_sf_sh                         | Design-point pressure drop across the solar field superheater fraction| none             | none             
!   14| fP_hdr_h                         | Average design-point hot header pressure drop fraction            | none             | none             
!   15| q_pb_des                         | Design heat input to the power block                              | MW               | kW               
!   16| W_pb_des                         | Rated plant capacity                                              | MW               | kW               
!   17| cycle_max_fraction               | Maximum turbine over design operation fraction                    | none             | none             
!   18| cycle_cutoff_frac                | Minimum turbine operation fraction before shutdown                | none             | none             
!   19| t_sby                            | Low resource standby period                                       | hr               | hr               
!   20| q_sby_frac                       | Fraction of thermal power required for standby                    | none             | none             
!   21| solarm                           | Solar multiple                                                    | none             | none             
!   22| PB_pump_coef                     | Pumping power required to move 1kg of HTF through power block flow loop| kW/kg            | kW/kg            
!   23| PB_fixed_par                     | fraction of rated gross power consumed at all hours of the year   | none             | none             
!   24| BOP_parVal                       | Balance of plant parasitic                                        | MW/MWcap         | MW/MWcap         
!   25| BOP_parPF                        | Balance of plant parasitic multiplier factor                      | none             | none             
!   26| BOP_par0                         | Balance of plant parasitic polynomial constant                    | none             | none             
!   27| BOP_par1                         | Balance of plant parasitic polynomial linear term                 | none             | none             
!   28| BOP_par2                         | Balance of plant parasitic polynomial quadratic term              | none             | none             
!   29| Aux_parVal                       | Aux heater/boiler parasitic                                       | MW/MWcap         | MW/MWcap         
!   30| Aux_parPF                        | Aux heater/boiler parasitic multiplier factor                     | none             | none             
!   31| Aux_par0                         | Aux heater/boiler parasitic polynomial constant                   | none             | none             
!   32| Aux_par1                         | Aux heater/boiler parasitic polynomial linear term                | none             | none             
!   33| Aux_par2                         | Aux heater/boiler parasitic polynomial quadratic term             | none             | none             
!   34| T_startup                        | Startup temperature (same as field startup)                       | C                | K                
!   35| fossil_mode                      | Operation mode for the fossil backup {1=Normal.. 2=supp.. 3=topping}| none             | none             
!   36| I_bn_des                         | Design point irradiation value                                    | W/m2             | W/m2             
!   37| is_sh                            | Does the solar field include a superheating section               | none             | none             
!   38| is_oncethru                      | Flag indicating whether flow is once through with superheat       | none             | none             
!   39| is_multgeom                      | Does the superheater have a different geometry from the boiler {1=yes}?| none             | none             
!   40| nModBoil                         | Number of modules in the boiler section                           | none             | none             
!   41| nModSH                           | Number of modules in the superheater section                      | none             | none             
!   42| nLoops                           | Number of loops                                                   | none             | none             
!   43| eta_pump                         | Feedwater pump efficiency                                         | none             | none             
!   44| latitude                         | Site latitude read from weather file                              | deg              | rad              
!   45| theta_stow                       | stow angle                                                        | deg              | rad              
!   46| theta_dep                        | deploy angle                                                      | deg              | rad              
!   47| m_dot_min                        | Minimum loop flow rate                                            | kg/s             | kg/s             
!   48| T_field_ini                      | Initial field temperature                                         | C                | K                
!   49| T_fp                             | Freeze protection temperature (heat trace activation temperature) | C                | K                
!   50| Pipe_hl_coef                     | Loss coefficient from the header.. runner pipe.. and non-HCE piping| W/m2-K           | W/m2-K           
!   51| SCA_drives_elec                  | Tracking power.. in Watts per SCA drive                           | W/m2             | W/m2             
!   52| ColAz                            | Collector azimuth angle                                           | deg              | rad              
!   53| e_startup                        | Thermal inertia contribution per sq meter of solar field          | kJ/K-m2          | kJ/K-m2          
!   54| T_amb_des_sf                     | Design-point ambient temperature                                  | C                | K                
!   55| V_wind_max                       | Maximum allowable wind velocity before safety stow                | m/s              | m/s              
!   56| LU_HL                            | Logical unit specifying the location of the heat loss information file| none             | none             
!   57| LU_OptTable                      | Logical unit for the optical table file                           | none             | none             
!   58| LU_OptTable_sh                   | Logical unit for the optical table file in superheater            | none             | none             
!   59| LU_warn                          | Logical unit for warnings file                                    | none             | none             
! ...... Loop for 1..9 TOU periods.......
!   60| FFRAC                            | Fossil dispatch logic                                             | none             | none             
! ......//
! ----The following parameters are read in from an external file for both the boiler and superheater sections
! …… Loop for 1..2 Field geometry sections
!   61| A_aperture                       | Reflective aperture area of the collector module                  | m^2              | m^2              
!   62| L_col                            | Active length of the superheater section collector module         | m                | m                
!   63| OptCharType                      | The optical characterization method                               | none             | none             
!   64| IamF0_T                          | Transverse Incident angle modifier 0th order term                 | none             | none             
!   65| IamF1_T                          | Transverse Incident angle modifier 1st order term                 | none             | none             
!   66| IamF2_T                          | Transverse Incident angle modifier 2nd order term                 | none             | none             
!   67| IamF3_T                          | Transverse Incident angle modifier 3rd order term                 | none             | none             
!   68| IamF4_T                          | Transverse Incident angle modifier 4th order term                 | none             | none             
!   69| IamF0_L                          | Longitudinal Incident angle modifier 0th order term               | none             | none             
!   70| IamF1_L                          | Longitudinal Incident angle modifier 1st order term               | none             | none             
!   71| IamF2_L                          | Longitudinal Incident angle modifier 2nd order term               | none             | none             
!   72| IamF3_L                          | Longitudinal Incident angle modifier 3rd order term               | none             | none             
!   73| IamF4_L                          | Longitudinal Incident angle modifier 4th order term               | none             | none             
!   74| TrackingError                    | User-defined tracking error derate                                | none             | none             
!   75| GeomEffects                      | User-defined geometry effects derate                              | none             | none             
!   76| Rho_mirror_clean                 | User-defined clean mirror reflectivity                            | none             | none             
!   77| Dirt_mirror                      | User-defined dirt on mirror derate                                | none             | none             
!   78| Error                            | User-defined general optical error derate                         | none             | none             
!   79| HLCharType                       | Flag indicating the heat loss model type {1=poly.; 2=Forristall}  | none             | none             
!   80| HL_dT_C0                         | Heat loss coefficient - HTF temperature - order 0                 | W/m              | W/m              
!   81| HL_dT_C1                         | Heat loss coefficient - HTF temperature - order 1                 | W/m-K            | W/m-K            
!   82| HL_dT_C2                         | Heat loss coefficient - HTF temperature - order 2                 | W/m-K^2          | W/m-K^2          
!   83| HL_dT_C3                         | Heat loss coefficient - HTF temperature - order 3                 | W/m-K^3          | W/m-K^3          
!   84| HL_dT_C4                         | Heat loss coefficient - HTF temperature - order 4                 | W/m-K^4          | W/m-K^4          
!   85| HL_W_C0                          | Heat loss coefficient adjustment - Wind velocity - order 0        | none             | none             
!   86| HL_W_C1                          | Heat loss coefficient adjustment - Wind velocity - order 1        | 1/(m/s)          | 1/(m/s)          
!   87| HL_W_C2                          | Heat loss coefficient adjustment - Wind velocity - order 2        | 1/(m/s)^2        | 1/(m/s)^2        
!   88| HL_W_C3                          | Heat loss coefficient adjustment - Wind velocity - order 3        | 1/(m/s)^3        | 1/(m/s)^3        
!   89| HL_W_C4                          | Heat loss coefficient adjustment - Wind velocity - order 4        | 1/(m/s)^4        | 1/(m/s)^4        
!   90| D_2                              |  The inner absorber tube diameter                                 | m                | m                
!   91| D_3                              |  The outer absorber tube diameter                                 | m                | m                
!   92| D_4                              |  The inner glass envelope diameter                                | m                | m                
!   93| D_5                              |  The outer glass envelope diameter                                | m                | m                
!   94| D_p                              |  The diameter of the absorber flow plug (optional)                | m                | m                
!   95| Rough                            |  Roughness of the internal surface                                | m                | m                
!   96| Flow_type                        |  The flow type through the absorber                               | none             | none             
!   97| AbsorberMaterial                 |  Absorber material type                                           | none             | none             
! …… Loop for 1..4 receiver variations
!   98| HCE_FieldFrac                    |  The fraction of the field occupied by this HCE type              | none             | none             
!   99| alpha_abs                        |  Absorber absorptance                                             | none             | none             
!  100| epsilon_3                        |  Absorber emittance                                               | none             | none             
!  101| alpha_env                        |  Envelope absorptance                                             | none             | none             
!  102| EPSILON_4                        |  Inner glass envelope emissivities (Pyrex)                        | none             | none             
!  103| Tau_envelope                     |  Envelope transmittance                                           | none             | none             
!  104| GlazingIntactIn                  |  The glazing intact flag {true=0; false=1}                        | none             | none             
!  105| AnnulusGas                       |  Annulus gas type (1=air; 26=Ar; 27=H2)                           | none             | none             
!  106| P_a                              |  Annulus gas pressure                                             | torr             | Pa               
!  107| Design_loss                      |  Receiver heat loss at design                                     | W/m              | W/m              
!  108| Shadowing                        |  Receiver bellows shadowing loss factor                           | none             | none             
!  109| Dirt_HCE                         |  Loss due to dirt on the receiver envelope                        | none             | none             
!---------------
!---------------

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Inputs
!    1| dnifc                            | Forecast DNI                                                      | W/m2             | W/m2             
!    2| I_bn                             | Beam normal radiation (input kJ/m2-hr)                            | kJ/m2.hr         | W/m2             
!    3| T_db                             | Dry bulb air temperature                                          | C                | K                
!    4| T_dp                             | The dewpoint temperature                                          | C                | K                
!    5| P_amb                            | Ambient pressure                                                  | atm              | Pa               
!    6| V_wind                           | Ambient windspeed                                                 | m/s              | m/s              
!    7| m_dot_htf_ref                    | Reference HTF flow rate at design conditions                      | kg/hr            | kg/s             
!    8| m_pb_demand                      | Demand htf flow from the power block                              | kg/hr            | kg/s             
!    9| shift                            | Shift in longitude from local standard meridian                   | deg              | rad              
!   10| SolarAz                          | Solar azimuth angle reported by the Type15 weather file           | deg              | rad              
!   11| T_pb_out                         | Fluid temperature from the power block                            | C                | K                
!   12| TOUPeriod                        | Time of use period                                                | none             | none             

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Outputs
!    1| cycle_pl_control                 | Part-load control flag - used by Type224                          | none             | none             
!    2| dP_tot                           | Total HTF pressure drop                                           | bar              | bar              
!    3| dP_hdr_c                         | Average cold header pressure drop                                 | bar              | bar              
!    4| dP_sf_boil                       | Pressure drop across the solar field boiler                       | bar              | bar              
!    5| dP_boil_to_SH                    | Pressure drop between the boiler and superheater                  | bar              | bar              
!    6| dP_sf_sh                         | Pressure drop across the solar field superheater                  | bar              | bar              
!    7| dP_hdr_h                         | Average hot header pressure drop                                  | bar              | bar              
!    8| E_bal_startup                    | Startup energy consumed                                           | MW               | MW               
!    9| E_field                          | Accumulated internal energy in the entire solar field             | MW-hr            | MW-hr            
!   10| E_fp_tot                         | Freeze protection energy                                          | J                | J                
!   11| eta_opt_ave                      | Collector equivalent optical efficiency                           | none             | none             
!   12| eta_thermal                      | Solar field thermal efficiency (power out/ANI)                    | none             | none             
!   13| eta_sf                           | Total solar field collection efficiency                           | none             | none             
!   14| defocus                          | The fraction of focused aperture area in the solar field          | none             | none             
!   15| m_dot_aux                        | Auxiliary heater mass flow rate                                   | kg/hr            | kg/s             
!   16| m_dot_field                      | Flow rate from the field                                          | kg/hr            | kg/s             
!   17| m_dot_b_tot                      | Flow rate within the boiler section                               | kg/hr            | kg/s             
!   18| m_dot                            | Flow rate in a single loop                                        | kg/s             | kg/s             
!   19| m_dot_to_pb                      | Flow rate delivered to the power block                            | kg/hr            | kg/s             
!   20| P_turb_in                        | Pressure at the turbine inlet                                     | bar              | bar              
!   21| q_loss_piping                    | Pipe heat loss in the hot header and the hot runner               | MW               | MW               
!   22| q_aux_fluid                      | Thermal energy provided to the fluid passing through the aux heater| MW               | MW               
!   23| q_aux_fuel                       | Heat content of fuel required to provide aux firing               | MMBTU            | kW               
!   24| q_dump                           | Dumped thermal energy                                             | MW               | MW               
!   25| q_field_delivered                | Total solar field thermal power delivered                         | MW               | kW               
!   26| q_inc_tot                        | Total power incident on the field                                 | MW               | MW               
!   27| q_loss_rec                       | Total Receiver thermal losses                                     | MW               | MW               
!   28| q_loss_sf                        | Total solar field thermal losses                                  | MW               | MW               
!   29| q_to_pb                          | Thermal energy to the power block                                 | MW               | kW               
!   30| SolarAlt                         | Solar altitude used in optical calculations                       | deg              | rad              
!   31| SolarAz                          | Solar azimuth used in optical calculations                        | deg              | rad              
!   32| phi_t                            | Transversal solar incidence angle                                 | deg              | rad              
!   33| theta_L                          | Longitudinal solar incidence angle                                | deg              | rad              
!   34| standby_control                  | Standby control flag - used by Type224                            | none             | none             
!   35| T_field_in                       | HTF temperature into the collector field header                   | C                | C                
!   36| T_field_out                      | HTF Temperature from the field                                    | C                | C                
!   37| T_loop_out                       | Loop outlet temperature                                           | C                | C                
!   38| T_pb_in                          | HTF Temperature to the power block                                | C                | C                
!   39| W_dot_aux                        | Parasitic power associated with operation of the aux boiler       | MW               | MW               
!   40| W_dot_bop                        | parasitic power as a function of power block load                 | MW               | MW               
!   41| W_dot_col                        | Parasitic electric power consumed by the collectors               | MW               | MW               
!   42| W_dot_fixed                      | Fixed parasitic power losses.. for every hour of operation        | MW               | MW               
!   43| W_dot_pump                       | Required solar field pumping power                                | MW               | MW               

use trnsysconstants
use trnsysfunctions
use CSPGeneric_tools
use water_properties

!DEC$ATTRIBUTES DLLEXPORT :: TYPE261

implicit none

!TRNSYS declarations
real(8):: time, dt
integer*4:: info(15), iunit, itype, icntrl
integer*4,parameter::np=68,ni=12,nout=43,nd=0,ns=100,nea=15

!Dimension the TRNSYS variables
real(8)::xin(ni),out(nout),par(np),stored(ns),T(nd),dtdt(nd) 

!------Variable declarations-------
!----Parameters-----
integer::NUMTOU, fossil_mode, nModBoil, nModSH, nLoops, LU_HL, LU_OptTable, LU_OptTable_sh, OptCharType(2), HLCharType(2), &
	Flow_type(2), LU_warn
real(8)::TSHOURS, q_max_aux, LHV_eff, T_set_aux, T_field_in_des, T_field_out_des, x_b_des, P_turb_des, fP_hdr_c, fP_sf_boil, &
	fP_boil_to_SH, fP_sf_sh, fP_hdr_h, q_pb_des, W_pb_des, cycle_max_fraction, cycle_cutoff_frac, t_sby, q_sby_frac, solarm, PB_pump_coef, &
	PB_fixed_par, BOP_parVal, BOP_parPF, BOP_par0, BOP_par1, BOP_par2, Aux_parVal, Aux_parPF, Aux_par0, Aux_par1, Aux_par2, T_startup, I_bn_des, &
	eta_pump, latitude, theta_stow, theta_dep, m_dot_min, T_field_ini, T_fp, Pipe_hl_coef, SCA_drives_elec, ColAz, e_startup, &
	T_amb_des_sf, V_wind_max, FFRAC(9), A_aperture(2), L_col(2), IamF0_T(2), IamF1_T(2), IamF2_T(2), IamF3_T(2), IamF4_T(2), &
	IamF0_L(2), IamF1_L(2), IamF2_L(2), IamF3_L(2), IamF4_L(2), TrackingError(2), GeomEffects(2), Rho_mirror_clean(2), Dirt_mirror(2), &
	Error(2), HL_dT_C0(2), HL_dT_C1(2), HL_dT_C2(2), HL_dT_C3(2), HL_dT_C4(2), HL_W_C0(2), HL_W_C1(2), HL_W_C2(2), HL_W_C3(2), HL_W_C4(2), D_2(2), D_3(2), D_4(2), D_5(2), &
	D_p(2), Rough(2), AbsorberMaterial(2), HCE_FieldFrac(2,4), alpha_abs(2,4), epsilon_3(2,4,nea), alpha_env(2,4), EPSILON_4(2,4), &
	Tau_envelope(2,4), GlazingIntactIn(2,4), AnnulusGas(2,4), P_a(2,4), Design_loss(2,4), Shadowing(2,4), Dirt_HCE(2,4)
logical::is_sh, is_oncethru, is_multgeom
!----Inputs-----
integer::TOUPeriod
real(8)::dnifc, I_bn, T_db, T_dp, P_amb, V_wind, m_dot_htf_ref, m_pb_demand, shift, SolarAz, T_pb_out
!----Outputs-----
real(8)::cycle_pl_control, dP_tot, dP_hdr_c, dP_sf_boil, dP_boil_to_SH, dP_sf_sh, dP_hdr_h, E_bal_startup, E_field, E_fp_tot, &
	eta_opt_ave, eta_thermal, eta_sf, defocus, m_dot_aux, m_dot_field, m_dot_b_tot, m_dot, m_dot_to_pb, P_turb_in, q_loss_piping, &
	q_aux_fluid, q_aux_fuel, q_dump, q_field_delivered, q_inc_tot, q_loss_rec, q_loss_sf, q_to_pb, SolarAlt, standby_control, T_field_in, T_field_out, &
	T_loop_out, T_pb_in, W_dot_aux, W_dot_bop, W_dot_col, W_dot_fixed, W_dot_pump
	
!---local variables
real(8),allocatable::xarr(:,:),xpar(:), q_inc(:), q_loss(:), q_abs(:), T_ave(:), T_ave0(:), h_ave(:), h_ave0(:), h_in(:), h_out(:), x(:), q_rec(:)
real(8)::D_h(2),A_cs(2), pi, epsilon_3T(2,4,nea), dec, omega, opteff_des(2),&
         L_tot, AP_tot, m_dot_des, phi_t, theta_L, Iam_T(2), Iam_L(2), eta_opt_fixed(2), h_b_in_des, h_b_out_des,&
         fp_sf_tot, dh_b_des, p_loc, c_hl, dT_loc, h_sh_in_des, h_sh_out_des, dh_sh_des, q_inc_tot_des, q_rec_tot_des, &
         q_loss_tot_des, q_abs_tot_des, eta_therm_sf_des, eta_tot_sf_des, e_trans, hour, day_of_year, b, &
         eot, solarnoon, dephr1, dephr2, dephr3, deptime, stwhr1, stwhr2, stwhr3, stwtime, hra, hrb, ftrack, midtrack, stdtime, &
         solartime, m_dot_b_des, h_b_in, h_b_out, m_dotX, err, tol, q_inc_loop, m_dot_lower, m_dot_upper, m_dot_bX, m_dot_b,&
         dh_ot_des, dh_b, dh_sh, q_rec_loss(4), q_rec_abs(4), dum(10), xx(nea), yy(nea), T_sky, skytemp, eps_3, tol_t, err_t, h_aveg, &
         y_upper, y_lower, upmult, h_sh_in, h_sh_out, dh, h_to_pb, T_shX, q_rec_loop, fP_turb_min, m_dot_pb_des, m_dot_pb_max, q_rec_tot, &
         m_dot_max, h_pb_out, err_def, tol_def, m_dot_b_max, defocus0, rc, defocus_lim, eta_optical(2), epsilon_5(2,4), q_aux_avail, &
         q_avail_tot, q_aux, t_sby0, t_sby_now, h_target, h_field_out, rho_fw, dP_basis, T_sys0, h_freeze, P_max, dvar(10), P_check,&
         h_b_recirc, h_pb_out_des, q_abs_tot, h_burn, T_burn
integer::i, ii, j, filelen, li, flag, n, m, k,trkflag, up1, nModTot, iter, iter_t, solvemode, iter_def,gset, LU_opts(2), &
         epsilon_3L(2,4), lxarr(nea)
logical::upflag,lowflag,glazingintact(2,4), is_def, is_pb_on, is_pb_on0
character::test*300

! --- Initial call to detect the TRNSYS version for which this Type is written -----------------------------------------
if (info(7) .eq. -2) then
    info(12) = 16   ! This component is a TRNSYS 16 Type
    flag = 8675309  !flag used during read-in of the system geometry
    return 1
endif

! --- Very last call in simulation -------------------------------------------------------------------------------------
if (info(8) == -1) then
    if(allocated(q_inc)) deallocate(q_inc, q_loss, q_abs, T_ave, x, h_in, h_out, h_ave, h_ave0, T_ave0, q_rec)
    if(allocated(eta)) deallocate(eta,azms,zens,azms1d,zens1d)
    return 1    ! Exit 
    
endif

! --- Post-convergence call --------------------------------------------------------------------------------------------
if (info(13) > 0) then

    !******************************************************************
    ! Set the system state values for the next timestep 
    !******************************************************************
    do i=1,nModTot
        stored(i) = T_ave(i)
    enddo
    stored(nModTot+1) = defocus
    stored(nModTot+2) = t_sby_now
    if(is_pb_on) then
        stored(nModTot+3) = 1.
    else
        stored(nModTot+3) = 0.
    endif
    stored(nModTot+4) = dmin1(T_field_in,T_field_out)+273.15d0    !Last field outlet temperature [K]
    call setStorageVars(stored,nS,INFO)
    
    !if any pressure errors have been recorded, print them out
    dum = P_check(-1.d0,0.d0)
    
    return 1    ! Exit - End of the routine for post-convergence calls
    
endif

! --- Second call in simulation: initialization call (not a simulation call) -------------------------------------------
if (info(7) .eq. -1) then
    !initialize any important variables
    trkflag = 1
    dt = getSimulationTimeStep()*3600.  ![s]
    pi=3.14159265
    !Set the maximum pressure allowed in the steam correlations
    P_max = 190. ![bar]

    !Read in parameters
    NUMTOU = int(par(1)) 		![none] Number of time-of-use periods
    TSHOURS = par(2) 		![hr] Equivalent full-load thermal storage hours
    q_max_aux = par(3)*1e+03 		![kW] Maximum heat rate of the auxiliary heater
    LHV_eff = par(4) 		![none] Fuel LHV efficiency (0..1)
    T_set_aux = par(5)+273.15 		![C] Aux heater outlet temperature set point
    T_field_in_des = par(6)+273.15 		![C] Field design inlet temperature
    T_field_out_des = par(7)+273.15 		![C] Field loop outlet design temperature
    x_b_des = par(8) 		![none] Design point boiler outlet steam quality
    P_turb_des = par(9) 		![bar] Design-point turbine inlet pressure
    fP_hdr_c = par(10) 		![none] Average design-point cold header pressure drop fraction
    fP_sf_boil = par(11) 		![none] Design-point pressure drop across the solar field boiler fraction
    fP_boil_to_SH = par(12) 		![none] Design-point pressure drop between the boiler and superheater frac
    fP_sf_sh = par(13) 		![none] Design-point pressure drop across the solar field superheater fraction
    fP_hdr_h = par(14) 		![none] Average design-point hot header pressure drop fraction
    q_pb_des = par(15)*1000 		![MW] Design heat input to the power block
    W_pb_des = par(16)*1000 		![MW] Rated plant capacity
    cycle_max_fraction = par(17) 		![none] Maximum turbine over design operation fraction
    cycle_cutoff_frac = par(18) 		![none] Minimum turbine operation fraction before shutdown
    t_sby = par(19) 		![hr] Low resource standby period
    q_sby_frac = par(20) 		![none] Fraction of thermal power required for standby
    solarm = par(21) 		![none] Solar multiple
    PB_pump_coef = par(22) 		![kW/kg] Pumping power required to move 1kg of HTF through power block flow loop
    PB_fixed_par = par(23) 		![none] fraction of rated gross power consumed at all hours of the year
    BOP_parVal = par(24) 		![MW/MWcap] Balance of plant parasitic 
    BOP_parPF = par(25) 		![none] Balance of plant parasitic multiplier factor
    BOP_par0 = par(26) 		![none] Balance of plant parasitic polynomial constant
    BOP_par1 = par(27) 		![none] Balance of plant parasitic polynomial linear term
    BOP_par2 = par(28) 		![none] Balance of plant parasitic polynomial quadratic term
    Aux_parVal = par(29) 		![MW/MWcap] Aux heater/boiler parasitic 
    Aux_parPF = par(30) 		![none] Aux heater/boiler parasitic multiplier factor
    Aux_par0 = par(31) 		![none] Aux heater/boiler parasitic polynomial constant
    Aux_par1 = par(32) 		![none] Aux heater/boiler parasitic polynomial linear term
    Aux_par2 = par(33) 		![none] Aux heater/boiler parasitic polynomial quadratic term
    T_startup = par(34)+273.15 		![C] Startup temperature (same as field startup)
    fossil_mode = int(par(35)) 		![none] Operation mode for the fossil backup {1=Normal.. 2=supp.. 3=topping}
    I_bn_des = par(36) 		![W/m2] Design point irradiation value
    is_sh = .false.
    if(par(37)==1.) is_sh = .true. 	!Does the solar field include a superheating section
    is_oncethru = .false.
    if(par(38)==1.) is_oncethru = .true. 	!Flag indicating whether flow is once through with superheat
    is_multgeom = .false.
    if(par(39)==1.) is_multgeom = .true. 	!Does the superheater have a different geometry from the boiler {1=yes}?
    nModBoil = int(par(40)) 		![none] Number of modules in the boiler section
    nModSH = int(par(41)) 		![none] Number of modules in the superheater section
    nLoops = int(par(42)) 		![none] Number of loops 
    eta_pump = par(43) 		![none] Feedwater pump efficiency
    latitude = par(44)*0.0174533 		![deg] Site latitude read from weather file
    theta_stow = par(45)*0.0174533 		![deg] stow angle
    theta_dep = par(46)*0.0174533 		![deg] deploy angle
    m_dot_min = par(47) 		![kg/s] Minimum loop flow rate
    T_field_ini = par(48)+273.15 		![C] Initial field temperature
    T_fp = par(49)+273.15 		![C] Freeze protection temperature (heat trace activation temperature)
    Pipe_hl_coef = par(50) 		![W/m2-K] Loss coefficient from the header.. runner pipe.. and non-HCE piping
    SCA_drives_elec = par(51) 		![W/m2] Tracking power.. in Watts per SCA drive
    ColAz = par(52)*0.0174533 		![deg] Collector azimuth angle
    e_startup = par(53) 		![kJ/K-m2] Thermal inertia contribution per sq meter of solar field
    T_amb_des_sf = par(54)+273.15 		![C] Design-point ambient temperature
    V_wind_max = par(55) 		![m/s] Maximum allowable wind velocity before safety stow
    LU_HL = int(par(56)) 		![none] Logical unit specifying the location of the heat loss information file
    LU_OptTable = int(par(57)) 		![none] Logical unit for the optical table file
    LU_OptTable_sh = int(par(58)) 		![none] Logical unit for the optical table file in superheater
    LU_warn = int(par(59)) 		![none] Logical unit for warnings file
    do i=1,9
	    FFRAC(i) = par(60+(i-1)) 		![none] Fossil dispatch logic
    enddo
    
    !read in the contents of the file, sorted for assignment below..
    !The other system data needs to be read in from the data file
    rewind(LU_HL)
    filelen = 0
    do 
        filelen=filelen+1
        read(unit=LU_HL,fmt="(A)",err=400,eor=400,end=400, advance="YES") test
        if(test(1:1)=="&") then
            read(test(2:4),"(I2)") li
            do i=1,li
                read(unit=LU_HL,fmt="(A)",err=400,eor=400,end=400, advance="YES") test
            enddo
        endif
    enddo

    400   filelen=filelen-1
    
    if(filelen /= 170) then
        call messages(-1,"The number of inputs in the TYPE261 system geometry data file did not match requirements",'FATAL',INFO(1),INFO(2))
        return  !Quit the program here
    endif
    rewind(LU_HL)

    !read in all the information from the file to a temporary arrays
    allocate(xpar(filelen),xarr(4*2,nea))    !limit for epsilon table is nea entries
    !Need room for emittance tables for all HCE combos plus corresponding temperature specs.
    !First array will be temp spec, second will be emittance value
    xpar(:)=0.d0; xarr(:,:)=0.d0; j=0; n=1
    do i=1,filelen
        read(LU_HL,"(A)") test
        if(test(1:1)=="&") then
            xpar(i)=flag
            j=j+1
            read(test(2:4),"(I2)") li
            lxarr(j)=li  !track record length
            do k=1,li
                read(LU_HL,*) xarr(2*j-1,k),xarr(2*j,k)
            enddo
        else
            k=len(trim(test))
            n=1; m=1
            call STRNUM(test(1:k),xpar(i),n,m)
        endif
    enddo
    rewind(LU_HL)        
    
    i = up1(0)
    do i=1,2
        A_aperture(i) = xpar(up1(1)) 		![m^2] Reflective aperture area of the collector module
        L_col(i) = xpar(up1(1)) 		![m] Active length of the superheater section collector module
        OptCharType(i) = int(xpar(up1(1))) 		![none] The optical characterization method 
        IamF0_T(i) = xpar(up1(1)) 		![none] Transverse Incident angle modifier 0th order term
        IamF1_T(i) = xpar(up1(1)) 		![none] Transverse Incident angle modifier 1st order term
        IamF2_T(i) = xpar(up1(1)) 		![none] Transverse Incident angle modifier 2nd order term
        IamF3_T(i) = xpar(up1(1))
        IamF4_T(i) = xpar(up1(1))
        IamF0_L(i) = xpar(up1(1)) 		![none] Longitudinal Incident angle modifier 0th order term
        IamF1_L(i) = xpar(up1(1)) 		![none] Longitudinal Incident angle modifier 1st order term
        IamF2_L(i) = xpar(up1(1)) 		![none] Longitudinal Incident angle modifier 2nd order term
        IAMF3_L(i) = xpar(up1(1))
        IamF4_L(i) = xpar(up1(1))
        TrackingError(i) = xpar(up1(1)) 		![none] User-defined tracking error derate
        GeomEffects(i) = xpar(up1(1)) 		![none] User-defined geometry effects derate
        Rho_mirror_clean(i) = xpar(up1(1)) 		![none] User-defined clean mirror reflectivity
        Dirt_mirror(i) = xpar(up1(1)) 		![none] User-defined dirt on mirror derate
        Error(i) = xpar(up1(1)) 		![none] User-defined general optical error derate 
        HLCharType(i) = int(xpar(up1(1))) 		![none] Flag indicating the heat loss model type {1=poly.; 2=Forristall}
        HL_dT_C0(i) = xpar(up1(1)) 		![W/m] Heat loss coefficient - HTF temperature - order 0
        HL_dT_C1(i) = xpar(up1(1)) 		![W/m-K] Heat loss coefficient - HTF temperature - order 1
        HL_dT_C2(i) = xpar(up1(1)) 		![W/m-K^2] Heat loss coefficient - HTF temperature - order 2
        HL_dT_C3(i) = xpar(up1(1)) 		![W/m-K^3] Heat loss coefficient - HTF temperature - order 3
        HL_dT_C4(i) = xpar(up1(1))      ![W/m-K^4] Heat loss coefficient - HTF temperature - order 4
        HL_W_C0(i) = xpar(up1(1)) 		![none] Heat loss coefficient adjustment - Wind velocity - order 0
        HL_W_C1(i) = xpar(up1(1)) 		![1/(m/s)] Heat loss coefficient adjustment - Wind velocity - order 1
        HL_W_C2(i) = xpar(up1(1)) 		![1/(m/s)^2] Heat loss coefficient adjustment - Wind velocity - order 2
        HL_W_C3(i) = xpar(up1(1)) 		![1/(m/s)^3] Heat loss coefficient adjustment - Wind velocity - order 3
        HL_W_C4(i) = xpar(up1(1))       ![1/(m/s)^4] Heat loss coefficient adjustment - Wind velocity - order 4
	    D_2(i) = xpar(up1(1)) 		![m]  The inner absorber tube diameter
	    D_3(i) = xpar(up1(1)) 		![m]  The outer absorber tube diameter
	    D_4(i) = xpar(up1(1)) 		![m]  The inner glass envelope diameter 
	    D_5(i) = xpar(up1(1)) 		![m]  The outer glass envelope diameter 
	    D_p(i) = xpar(up1(1)) 		![m]  The diameter of the absorber flow plug (optional) 
	    Rough(i) = xpar(up1(1)) 		![m]  Roughness of the internal surface 
	    Flow_type(i) = int(xpar(up1(1))) 		![none]  The flow type through the absorber
        if(Flow_type(i) == 2.d0) then
            D_h(i) = D_2(i) - D_p(i)          ![m] The hydraulic diameter for plug flow
        else
            D_h(i) = D_2(i)                     ![m] The hydraulic diameter for tube flow
            D_p(i) = 0.d0
        endif
        A_cs(i) = pi * (D_2(i)*D_2(i) - D_p(i)*D_p(i)) / 4.  ![m2] The cross-sectional flow area
	    AbsorberMaterial(i) = xpar(up1(1)) 		![none]  Absorber material type
        do j=1,4
            HCE_FieldFrac(i,j) = xpar(up1(1)) 		![none]  The fraction of the field occupied by this HCE type 
            alpha_abs(i,j) = xpar(up1(1)) 		![none]  Absorber absorptance
            epsilon_3(i,j,1) = xpar(up1(1))          ![-] Absorber emissivity
            if(epsilon_3(i,j,1)==flag) then
                epsilon_3l(i,j) = lxarr(trkflag)
                epsilon_3t(i,j,1:size(xarr(trkflag,:)))=xarr(2*trkflag-1,:)
                epsilon_3(i,j,1:size(xarr(trkflag+1,:)))=xarr(2*trkflag,:)
                trkflag=trkflag+1
            else
                epsilon_3l(i,j)=1
            endif
            alpha_env(i,j) = xpar(up1(1)) 		![none]  Envelope absorptance
            EPSILON_4(i,j) = xpar(up1(1)) 		![none]  Inner glass envelope emissivities (Pyrex) 
            EPSILON_5(i,j) = EPSILON_4(i,j) 		![none]  Outer glass envelope emissivities (Pyrex) 

            Tau_envelope(i,j) = xpar(up1(1)) 		![none]  Envelope transmittance
            GlazingIntact(i,j) = .false.
            if(xpar(up1(1))==1.) GlazingIntact(i,j) = .true.  		![none]  The glazing intact flag {1=true; else=false}
            AnnulusGas(i,j) = xpar(up1(1)) 		![none]  Annulus gas type (1=air; 26=Ar; 27=H2)
            P_a(i,j) = xpar(up1(1))      		![torr]  Annulus gas pressure
            Design_loss(i,j) = xpar(up1(1)) 		![W/m]  Receiver heat loss at design
            Shadowing(i,j) = xpar(up1(1)) 		![none]  Receiver bellows shadowing loss factor
            Dirt_HCE(i,j) = xpar(up1(1)) 		![none]  Loss due to dirt on the receiver envelope 
        enddo
        if(.not.is_multgeom) exit !Only read in the second set of values if we're defining multiple geometries
    enddo   

    !Determine whether an incompatible set of inputs has been provided for the boiler quality. For recirc systems, the
    !boiler quality must fall in the range (0..1]. Limit to a minimum of .01 for convergence sake.
    if(.not.is_oncethru) then
        if((x_b_des < .01d0).or.(x_b_des>1.d0)) then 
            write(LU_warn,fmt=101)
            101 format(/,&
                       "For recirculated boiler systems, the specified boiler outlet quality (vapor ",/,&
                       "fraction) must be greater than 0.01 and less than/equal to 1.0")
            call messages(-1,"Boiler quality outside valid range","FATAL",INFO(1),INFO(2))
            return 1
        endif
    endif

    !Allocate any required variables
    if(.not.is_sh) nModSh=0
    nModTot = nModBoil + nModSh
    if(.not.allocated(q_inc)) allocate(q_inc(nModTot), q_loss(nModTot), q_abs(nModTot), T_ave(nModTot), T_ave0(nModTot), h_ave(nModTot), &
                                       h_ave0(nModTot), h_in(nModTot), h_out(nModTot), x(nModTot), q_rec(nModTot))
    !Initialize to zeros
    q_inc=0.d0; q_loss=0.d0; q_rec=0.d0; q_abs=0.d0; T_ave=0.d0; h_ave=0.d0; h_in=0.d0; h_out=0.d0; h_ave0=0.d0; x=0.d0;
    
    !Set any constants
    fP_turb_min = 0.5d0     !Minimum fractional operating pressure at the turbine inlet
    LU_opts(:) = (/LU_OptTable, LU_OptTable_sh/)
    
    !Convert theta_stow
    theta_stow = pi - theta_stow
    
    !---Design point values---
    omega = 0.d0 !solar noon
    dec = 23.45d0*pi/180.d0 !declination at summer solstice
    !Solar altitude at noon on the summer solstice
    SolarAlt = asin(sin(dec)*sin(latitude)+cos(latitude)*cos(dec)*cos(omega))
    !If the optical characterization method is option 1 or 2, load the user's optical table
    do i=1,2
        if(OptCharType(i)<3) call load_gen_table(LU_opts(i))
    enddo
    !Convert the solar angles to collector incidence angles
    call theta_trans(0.d0, (pi/2.-SolarAlt), ColAz, phi_t, theta_L)
    
    !Calculate the design-point efficiency
    do i=1,2
        eta_opt_fixed(i) = TrackingError(i) * GeomEffects(i) * Rho_mirror_clean(i) * Dirt_mirror(i) * Error(i)
        if(.not.is_multgeom) exit
    enddo
    
    do i=1,2
        if(OptCharType(i)==1) then
            !user provides an optical table as a function of solar position
            opteff_des(i) = eta_opt_fixed(i)*dmax1(azzen_interp(0.d0, dmax1(pi/2.-SolarAlt, 0.d0), 1, LU_opts(i)), 0.d0)
            
        elseif(OptCharType(i)==2) then
            !user provides an optical table as a function of collector incidence angles
            opteff_des(i) = eta_opt_fixed(i)*dmax1(azzen_interp(0.d0, dmax1(theta_L, 0.d0), 1, LU_Opts(i)), 0.d0)
            
        elseif(OptCharType(i)==3) then
            !Otherwise, calculate the collector incidence angles for the IAM equations
            Iam_T(i) = IamF0_T(i) + IamF1_T(i)*phi_t + IamF2_T(i)*phi_t**2 + IamF3_T(i)*phi_t**3 + IamF4_T(i)*phi_t**4
            Iam_L(i) = IamF0_L(i) + IamF1_L(i)*theta_L + IamF2_L(i)*theta_L**2 + IamF3_L(i)*theta_L**3 + IamF4_L(i)*theta_L**4
            opteff_des(i) = eta_opt_fixed(i) * Iam_T(i) * Iam_L(i)
        endif
        if(.not.is_multgeom) exit
    enddo
    
    !Calculate the design-point incident energy on each module for a single loop
    do i=1,nModTot
        if((i>nModBoil).and.(is_multgeom)) then
            gset=2
        else
            gset=1
        endif
        q_inc(i) = I_bn_des * A_aperture(gset) /1000.d0       ![kW]
        q_rec(i) = q_inc(i) * opteff_des(gset)
    enddo
    !Calculate the total reference pressure drop across the field
    if(.not.is_sh) then
        fP_boil_to_SH = 0.d0
        fP_sf_sh = 0.d0
    endif
    fP_sf_tot = fP_hdr_c + fP_sf_boil + fP_boil_to_SH + fP_sf_sh + fP_hdr_h
    if(P_turb_des *(1.+fP_sf_tot)> 220.6) then
        write(LU_warn, fmt=102)
        102 format(/,&
                   "The design-point pressure at the inlet of the solar field exceeds the critical ",/,&
                   "pressure (220.6 bar). Review your settings for turbine inlet pressure and solar ",/,&
                   "field pressure drops to maintain reasonable design pressure conditions.")
        call messages(-1,"Input pressure range error. See warnings file.","FATAL",info(1),info(2))
        return 1
    endif
    
    !Reset the pressure check function
    dum = P_check(-2.d0, 0.d0)
    
    !estimate the design-point thermal losses
    
    !---------------"standard" boiler + optional superheater design ---------------------------
    if(.not.is_oncethru) then !Analyze the conventional boiler only/boiler+superheat options
        !calculate boiler inlet/outlet enthalpies
        call water_PQ(P_check(P_max, P_turb_des*(1.+fP_hdr_h+fP_sf_sh+fP_boil_to_SH))*100.,x_b_des,enth=h_b_out_des)
        !Power block outlet/field inlet enthalpy
        call water_TP((T_field_in_des-273.15d0), P_check(P_max, P_turb_des*(1.+fP_sf_tot-fP_hdr_c))*100., enth=h_pb_out_des)
        !Determine the mixed boiler inlet enthalpy
        h_b_in_des = h_pb_out_des*x_b_des + h_b_out_des*(1.-x_b_des)
        dh_b_des = (h_b_out_des - h_b_in_des)/dble(nModBoil)
        do i=1,nModBoil
            !calculate the local pressure in the boiler. Assume a linear pressure drop across each section
            P_loc = P_turb_des * (1. + fP_sf_tot - fP_sf_boil*(1.-dble(i-1)/dble(nModBoil)))
            !Get the temperature and quality at each state in the boiler
            call water_PH(P_check(P_max, P_loc)*100., (h_b_in_des + dh_b_des*dble(i) - dh_b_des/2.), temp = T_ave(i), qual = x(i))
            T_ave(i) = T_ave(i) + 273.15d0  !Convert to [K]
            
            !Calculate the heat loss at each temperature
            if(HLCharType(1)==1) then 
                !Estimate based on the polynomial adjustments provided by the user
                dT_loc = T_ave(i) - T_amb_des_sf
                c_hl = HL_dT_C0(1) + HL_dT_C1(1)*dT_loc + HL_dT_C2(1)*dT_loc**2 + HL_dT_C3(1)*dT_loc**3  ![W/m] Don't adjust for wind speed here
                q_loss(i) = c_hl*L_col(1)/1000.d0 ![kW] Total thermal loss from this collector
            elseif(HLCharType(1)==2) then
                !Estimate based on user-supplied guesses for thermal losses
                q_loss(i) = 0.d0
                do j=1,4
                    q_loss(i) = q_loss(i) + design_loss(1,j)*hce_fieldfrac(1,j)*L_col(1)/1000.d0  !kW
                enddo
            endif
            q_abs(i) = q_rec(i) - q_loss(i)     ![kW]
        enddo
        
        !Same for the superheater, if applicable
        if(is_sh) then
            !decide which geometry set to use
            gset=1
            if(is_multgeom) gset=2
            
            !calculate superheater inlet/outlet enthalpies
            call water_PQ(P_check(P_max, P_turb_des*(1.+fP_hdr_h+fP_sf_sh))*100.,1.d0, enth=h_sh_in_des)
            call water_TP((T_field_out_des-273.15d0), P_check(P_max, P_turb_des*(1.+fP_hdr_h))*100.d0, enth=h_sh_out_des)
            dh_sh_des = (h_sh_out_des - h_sh_in_des)/dble(nModSh)
            do ii=1,nModSh
                i=ii+nModBoil
                !Calculate the local pressure in the superheater. Assume a linear pressure drop
                P_loc = P_turb_des * (1. + fP_hdr_h + fP_sf_sh * (1.-dble(ii-1)/dble(nModSh)))
                !Get the temperature at each state in the boiler
                call water_PH(P_check(P_max, P_loc)*100.d0, (h_sh_in_des + dh_sh_des*dble(ii) - dh_sh_des/2.), temp = T_ave(i))
                T_ave(i) = T_ave(i) + 273.15d0 !Convert to [K]
                
                !Calculate the heat loss at each temperature
                if(HLCharType(gset)==1) then
                    !Polynomials
                    dT_loc = T_ave(i) - T_amb_des_sf
                    c_hl = HL_dT_C0(gset) + HL_dT_C1(gset)*dT_loc + HL_dT_C2(gset)*dT_loc**2 + HL_dT_C3(gset)*dT_loc**3  ![W/m] Don't adjust for wind speed here
                    q_loss(i) = c_hl*L_col(gset)/1000.d0  ![kW] total collector thermal loss
                elseif(HLCharType(gset)==2) then
                    !user supplied guesses for thermal losses - Forristall model
                    q_loss(i) = 0.d0
                    do j=1,4
                        q_loss(i) = q_loss(i) + design_loss(gset,j)*hce_fieldfrac(gset,j)*L_col(gset)/1000.d0   ![kW]
                    enddo
                endif
                q_abs(i) = q_rec(i) - q_loss(i)    ![kW] Total absorbed energy in the collector
            enddo
            
        endif
        
    !------------Once through design -----------------------    
    else !Analyze the once-through boiler+superheater option
        !Calculate the total enthalpy rise across the loop
        call water_TP((T_field_in_des-273.15d0), P_check(P_max, P_turb_des*(1.+fP_sf_tot))*100.d0, enth=h_pb_out_des) 
        call water_TP((T_field_out_des-273.15d0), P_check(P_max, P_turb_des*(1.+fP_hdr_h))*100.d0, enth=h_sh_out_des)
        !enthalpy rise across each collector module
        dh_ot_des = (h_sh_out_des - h_pb_out_des)/dble(nModTot) ![kJ/kg]
        do i=1,nModTot
            !Decide which geometry set to use
            gset=1
            if((i>nModBoil).and.(is_multgeom)) gset=2
            
            !Calculate the local pressure in the loop, assume a linear pressure drop
            P_loc = P_turb_des * (1. + (fP_sf_boil + fP_sf_sh)*(1.-dble(i-1)/dble(nModTot)) + fP_hdr_h)
            !Get the temperature/quality at each state in the loop
            call water_PH(P_check(P_max, P_loc)*100., (h_pb_out_des + dh_ot_des*dble(i) - dh_ot_des/2.), temp = T_ave(i), qual = x(i))
            T_ave(i) = T_ave(i) + 273.15d0 !convert to [K]
            
            !Calculate the heat loss at each temperature
            if(HLCharType(gset)==1) then
                !Polynomials
                dT_loc = T_ave(i) - T_amb_des_sf
                c_hl = HL_dT_C0(gset) + HL_dT_C1(gset)*dT_loc + HL_dT_C2(gset)*dT_loc**2 + HL_dT_C3(gset)*dT_loc**3  ![W/m] Don't adjust for wind speed here
                q_loss(i) = c_hl*L_col(gset)/1000.d0  ![kW] total collector thermal loss
            elseif(HLCharType(gset)==2) then
                !user supplied guesses for thermal losses - Forristall model
                q_loss(i) = 0.d0
                do j=1,4
                    q_loss(i) = q_loss(i) + design_loss(gset,j)*hce_fieldfrac(gset,j)*L_col(gset)/1000.d0   ![kW]
                enddo
            endif
            q_abs(i) = q_rec(i) - q_loss(i)    ![kW] Total absorbed energy in the collector
        enddo    
        
    endif
    
    !Calculate total solar field aperture area
    if(is_multgeom) then
        Ap_tot = (A_aperture(1)*nModBoil + A_aperture(2)*nModSh)*nLoops
    else
        Ap_tot = A_aperture(1)*dble(nModTot * nLoops)
    endif
    
    !estimate piping thermal loss
    q_loss_piping = Ap_tot * Pipe_hl_coef/1000.d0 * ((T_field_in_des + T_field_out_des)/2. - T_amb_des_sf)  !hl coef is [W/m2-K], use average field temp as driving difference
    
    !***********Design solar field thermal power output*********
    q_inc_tot_des = sum(q_inc) * dble(nLoops)
    q_rec_tot_des = sum(q_rec) * dble(nLoops)
    q_loss_tot_des = sum(q_loss) * dble(nLoops) + q_loss_piping
    q_abs_tot_des = q_rec_tot_des - q_loss_tot_des
        
    eta_therm_sf_des = 1. - q_loss_tot_des/q_rec_tot_des    !Design solar field thermal efficiency
    eta_tot_sf_des = (opteff_des(1)*nModBoil*A_aperture(1) + opteff_des(2)*nModSh*A_aperture(2))/(nModBoil*A_aperture(1)+nModSh*A_aperture(2))* eta_therm_sf_des !Design solar field total efficiency

    !Calculate the design-point mass flow rate leaving the solar field
    m_dot_des = q_abs_tot_des/(h_sh_out_des - h_pb_out_des)
    
    !Calculate the design-point mass flow rate in the boiler only (includes recirculation mass)
    if(x_b_des==0.) then
        m_dot_b_des = m_dot_des
    else
        m_dot_b_des = m_dot_des/x_b_des
    endif
    
    !Calculate maximum flow rate to the power block
    m_dot_pb_des = q_pb_des/(h_sh_out_des - h_pb_out_des)
    m_dot_pb_max = m_dot_pb_des * cycle_max_fraction
    m_dot_max = m_dot_pb_max/dble(nLoops)
    m_dot_b_max = m_dot_max/x_b_des
    
    !Convert the thermal inertia term here
    e_trans = e_startup * Ap_tot/dble(nModTot*nLoops)     ![kJ/m2-K] -> [kJ/K] average transient energy per collector
    
    !Check to see if the design provided by the user is reasonable. If not, return a warning message
    if(.not.is_oncethru) then
        !What is the enthalpy rise across the boiler?
        call water_TP((T_field_in_des-273.15d0), P_check(P_max, P_turb_des*(1.+fP_sf_tot))*100.d0, enth=dvar(1)) !solar field inlet
        call water_PQ(P_check(P_max, P_turb_des * (1. + fP_hdr_h + fP_sf_sh + fP_boil_to_SH))*100., x_b_des, enth=dvar(2))  !boiler outlet
        call water_PQ(P_check(P_max, P_turb_des * (1. + fP_hdr_h + fP_sf_sh + fP_boil_to_SH))*100., 1.d0, enth=dvar(7))  !superheater inlet
        dvar(3) = (dvar(2) - dvar(1))/nModBoil  !The enthalpy rise per boiler module.. 
        !Calculate the expected enthalpy rise in the superheater based on ratios of expected energy absorption between the boiler and superheater
        if(is_multgeom) then
            ! = (Expected performance of the superheater modules) / (Expected performance of the boiler modules)
            dvar(10) = (opteff_des(2)*A_aperture(2)*I_bn_des/1000. - sum(q_loss(nModBoil+1:nModTot))/nModSh)/&
                       (opteff_des(1)*A_aperture(1)*I_bn_des/1000. - sum(q_loss(1:nModBoil))/nModBoil)
        else
            !No separate geometry, so approximate the superheater and boiler as the same
            dvar(10) = 1.d0
        endif
        !project this to the superheater modules
        dvar(4) = dvar(7)+dvar(3)*nModSh*dvar(10) !estimated superheater outlet enthalpy
        !check the temperature
        call water_PH(P_turb_des*(1.d0 + fP_boil_to_SH)*100.d0, dvar(4), temp=dvar(5)) 
        dvar(5) = dvar(5) + 273.15d0 !Convert to K
        dvar(6) = dvar(5) - T_field_out_des !difference in temperature between estimated outlet temperature and user-spec
        !what are the superheater design conditions?
        call water_TP((T_field_out_des-273.15d0), P_check(P_max, P_turb_des*(1.+fP_hdr_h))*100., enth=dvar(8)) !superheater outlet
        dvar(9) = (dvar(8) - dvar(7))/(dvar(2) - dvar(1)) * nModBoil
        
        if(dvar(6) > 25.d0) write(LU_warn, fmt=103) nmodboil, nmodsh, (dvar(5)-273.15d0), dvar(6), (T_field_out_des-273.15d0), dvar(9)
        if(dvar(6) < -25.d0) write(LU_warn, fmt=103) nmodboil, nmodsh, (dvar(5)-273.15d0), -dvar(6), (T_field_out_des-273.15d0), dvar(9)
        103 format(/,&
                   "The field layout you selected with ",I2," boiler modules and ",I2," superheater ",/,&
                   "modules results in a projected superheater outlet temperature of ",F6.1,"[C], ",/,&
                   "which is ",F5.1,"[C] away from the design-point value of ",F6.1,"[C]. Based on ",/,&
                   "the specified collector geometry, the ideal fractional number of superheater ",/,&
                   "modules is approximately ",F3.1,". Consider adjusting the design-point steam ",/,&
                   "settings, the module geometry, and/or the module distribution to better match ",/,&
                   "the desired steam conditions.")
        
        !set the limiting steam temperature for enthalpy calculations
        T_burn = dmax1(dvar(5), T_field_out_des) - 273.15d0
    else
        !the value of the design-point temperature is used in the limiting function for enthalpy. set the value to the 
        !field design outlet temperature plus a margin.
        T_burn = T_field_out_des - 273.15d0
    endif
        
    !Calculate the minimum allowable enthalpy before freezing
    call water_TP(5.d0, P_turb_des*cycle_cutoff_frac*100., enth=h_freeze)
    !Calculate the maximum allowable enthalpy before convergence error
    call water_TP(dmin1(T_burn+150., 1000.), P_max*100., enth=h_burn)
    !Set up the enthalpy limit function
    dum = enth_lim(1000.d0, h_freeze, h_burn)

    !---- Initialize outputs
    out(1:nout) = 0.d0
    info(6)=nout
    
    !Deallocate the temporary arrays
    if(allocated(xpar)) deallocate(xpar, xarr)
    
    !---- Set storage array size 
    CALL setStorageSize(nS,INFO)
    
    !Set initial storage values
    do i=1,nModTot
        stored(i)=T_field_ini
    enddo
    stored(nModTot+1) = 1.d0 !defocus
    stored(nModTot+2) = t_sby !initial standby time
    stored(nModTot+3) = 0.  !Was power block on?
    stored(nModTot+4) = T_field_ini
    call setStorageVars(stored,nS,info)
    
    return 1
endif


!******************************************************************************************************************************
!               Time-dependent conditions
!******************************************************************************************************************************

!Read in Inputs
dnifc = xin(1) 		![W/m2] Forecast DNI 
I_bn = xin(2)/3.6 		![kJ/m2.hr] --> [W/m2] Beam normal radiation (input kJ/m2-hr)
T_db = xin(3)+273.15 		![C] --> [K] Dry bulb air temperature
T_dp = xin(4)+273.15 		![C] --> [K] The dewpoint temperature
P_amb = xin(5)*101325.d0 		![atm] --> [Pa] Ambient pressure
V_wind = xin(6) 		![m/s] Ambient windspeed 
m_dot_htf_ref = xin(7)*0.000277778 		![kg/hr] --> [kg/s] Reference HTF flow rate at design conditions
m_pb_demand = xin(8)*0.000277778 		![kg/hr] --> [kg/s] Demand htf flow from the power block
shift = xin(9)*0.0174533 		![deg] --> [rad] Shift in longitude from local standard meridian 
SolarAz = xin(10)*0.0174533 		![deg] --> [rad] Solar azimuth angle reported by the Type15 weather file
T_pb_out = xin(11)+273.15 		![C] --> [K] Fluid temperature from the power block
TOUPeriod = int(xin(12)) 		![none] Time of use period

hour = modulo(time,24.)
T_sky = skytemp(T_db,T_dp,hour)     ![K] Effective sky temperature 

!******* Read in stored variables every timestep*******
call getStorageVars(stored,nS,info)
do i=1,nModTot
    T_ave0(i) = stored(i)
enddo
defocus0 = stored(nModTot + 1)
t_sby0 = stored(nModTot + 2)
is_pb_on0 = .false.
if(stored(nModTot+3)==1) is_pb_on0 = .true.
T_sys0 = stored(nModTot + 4)
!--------------------------------

!Calculations for values once per timestep..
if (info(7)==0) then 
        
    !Optical calculations
    !Time calculations
    hour = modulo(time,24.)       !hour of the day (1..24)  !tn 4.25.11 mod returns a natural number. This messes with the ftrack HrA/HrB calculations
    day_of_year = ceiling(time/24.)  !Day of the year
    ! Duffie & Beckman 1.5.3b
    B = (day_of_year-1)*2.*pi/365.0
    ! Eqn of time in minutes
    EOT = 229.2 * (0.000075 + 0.001868 * COS(B) - 0.032077 * SIN(B)	- 0.014615 * COS(B*2.0) - 0.04089 * SIN(B*2.0))
    ! Declination in radians (Duffie & Beckman 1.6.1)
    Dec = 23.45 * SIN(360.0*(284.0+day_of_year)/365.0*pi/180.0) * pi/180.0
    ! Solar Noon and time in hours
    SolarNoon = 12. - ((shift)*180.0/pi) / 15.0 - EOT / 60.0

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
    HrA = hour-dt/3600.
    HrB = hour

    ! Solar field operates
    if ((HrB > DepTime) .AND. (HrA < StwTime)) then
        ! solar field deploys during time period
        if (HrA < DepTime) then
            Ftrack = (HrB - DepTime) / (dt/3600.)
            MidTrack = HrB - Ftrack * 0.5 *(dt/3600.)
        ! Solar field stows during time period
        elseif (HrB > StwTime) then
            Ftrack = (StwTime - HrA) / (dt/3600.)
            MidTrack = HrA + Ftrack * 0.5 * (dt/3600.)
        ! solar field operates during entire period
        else
            Ftrack = 1.0
            MidTrack = HrA + 0.5 *(dt/3600.)
        endif
    ! solar field doesn't operate
    else
        Ftrack = 0.0
        MidTrack = HrA + 0.5 *(dt/3600.)
    endif
    
    !Maximum wind speed value
    if(V_wind >= V_wind_max) Ftrack = 0.d0
    
    StdTime = MidTrack
    SolarTime = StdTime+((Shift)*180.0/pi)/15.0+ EOT/60.0
    ! hour angle (arc of sun) in radians
    omega = (SolarTime - 12.0)*15.0*pi/180.0
    ! B. Stine equation for Solar Altitude angle in radians
    SolarAlt = asin(sin(dec)*sin(latitude)+cos(latitude)*cos(dec)*cos(omega))
    SolarAz = sign(1.d0, omega)*abs(acos(dmin1(1.d0,(cos(pi/2.-SolarAlt)*sin(latitude)-sin(dec))/(sin(pi/2.-SolarAlt)*cos(latitude)))))
    if (SolarAlt > 0.d0) then
        !Convert the solar angles to collector incidence angles
        call theta_trans(SolarAz, (pi/2.-SolarAlt), ColAz, phi_t, theta_L)
        
        do i=1,2
            if(OptCharType(i)==1) then
                !user provides an optical table as a function of solar position
                eta_optical(i) = eta_opt_fixed(i)*dmax1(azzen_interp(SolarAz, dmax1(pi/2.-SolarAlt, 0.d0), 1, LU_opts(i)), 0.d0)  
                
            elseif(OptCharType(i)==2) then
                !user provides an optical table as a function of collector incidence angles
                eta_optical(i) = eta_opt_fixed(i)*dmax1(azzen_interp(phi_t, dmax1(theta_L, 0.d0), 1, LU_Opts(i)), 0.d0)
                
            elseif(OptCharType(i)==3) then
                !Otherwise, calculate the collector incidence angles for the IAM equations
                Iam_T(i) = IamF0_T(i) + IamF1_T(i)*phi_t + IamF2_T(i)*phi_t**2 + IamF3_T(i)*phi_t**3 + IamF4_T(i)*phi_t**4
                Iam_L(i) = IamF0_L(i) + IamF1_L(i)*theta_L + IamF2_L(i)*theta_L**2 + IamF3_L(i)*theta_L**3 + IamF4_L(i)*theta_L**4
                eta_optical(i) = eta_opt_fixed(i) * Iam_T(i) * Iam_L(i)
                
            endif
            eta_optical(i) = eta_optical(i) * ftrack
            if(.not.is_multgeom) exit
        enddo
    else
        eta_optical = 0.d0
        phi_t = pi/2.
        theta_L = 0.d0
    endif
    
    !Set initial defocus
    defocus = 1.d0 
    is_def = .false.
    err_def = 0.d0
    tol_def = .0001d0
    rc = .7 !relaxation coefficient
    !Set the power block return temperature to a reasonable value since the power block has not yet been called
    T_pb_out = T_field_in_des
    
    !Reset the pressure check function
    dum = P_check(-2.d0, 0.d0)
endif

!--------------------------------------------------------------------------------------------------
!Determine the solar field thermal performance 
!--------------------------------------------------------------------------------------------------
iter_def = 0
10 continue !jump in here on defocus iterations
defocus_lim = dmax1(dmin1(defocus, 1.d0), 0.d0)
eta_opt_ave = 0.d0
do i=1,nModTot
    gset=1
    if((i>nModBoil).and.(is_multgeom)) gset=2
    !Calculate the incident energy on each module
    q_inc(i) = I_bn  * A_aperture(gset) /1000.d0  ![kW]
    !Calculate the energy on the receiver
    q_rec(i) = q_inc(i)*eta_optical(gset)*defocus_lim
    !Average optical efficiency
    eta_opt_ave = eta_opt_ave + eta_optical(gset)*A_aperture(gset)*nLoops/Ap_tot
enddo
q_rec_loop = sum(q_rec)
q_inc_loop = sum(q_inc)

!!For nighttime conditions, calculate the solar field inlet temperature
if((q_rec_loop == 0.d0).and.(FFRAC(TOUPeriod)<cycle_cutoff_frac)) then
    T_pb_out = dmax1(313.15d0, T_sys0)  !Need to limit the inlet temp so that the minimum temp in the loop stays above freezing. 
endif

!Calculate the total reference pressure drop across the field
if(.not.is_sh) then
    fP_boil_to_SH = 0.d0
    fP_sf_sh = 0.d0
endif
fP_sf_tot = fP_hdr_c + fP_sf_boil + fP_boil_to_SH + fP_sf_sh + fP_hdr_h

!Guess the mass flow rate in a loop for iteration. ratio of incident heat/abs heat at design differs by nLoops.. 
m_dotX = dmax1(dmin1(q_rec_loop/q_rec_tot_des * m_dot_des, m_dot_max), m_dot_min)

!Guess the turbine pressure.. turbine inlet pressure is highly insensitive to condenser pressure, so
!simplify the expression to eliminate condenser dependence
P_turb_in = turb_pres_frac(m_dotX*dble(nLoops)/m_dot_pb_des, fossil_mode, ffrac(touperiod), fP_turb_min)*P_turb_des
dP_basis = m_dotX*dble(nLoops)/m_dot_des*P_turb_des

if(is_oncethru .or. (Ftrack<=0.)) then !Run in once-through mode at night since distinct boiler/superheater models are not useful
!------------Once through design -----------------------    
    !Guess the loop inlet/outlet enthalpies
    call water_TP((T_pb_out-273.15d0), P_check(P_max, P_turb_in+dP_basis*(fP_sf_tot-fP_hdr_c))*100., enth=h_b_in)
    h_pb_out = h_b_in   !Set the power block outlet enthalpy to equal the boiler inlet (unlike a recirc system)
    call water_TP((T_field_out_des-273.15d0), P_check(P_max, P_turb_in+dP_basis*fP_hdr_h)*100., enth=h_sh_out)
    !Set the loop inlet enthalpy
    h_in(1) = h_b_in

    !Set up iteration brackets for false-position method
    m_dot_lower = 0.7*m_dot_min
    m_dot_upper = 1.3*m_dot_max 

    upflag = .false.        !Set logic to switch from bisection to false position mode
    lowflag = .false.       !Set logic to switch from bisection to false position mode

    !Set the iteration tolerance (MJW found that it doesn't help to adjust the tolerance based on iteration number)
    tol = 1.e-4

    !Do a rough guess of the receiver enthalpy for the whole loop
    dh = (h_sh_out - h_b_in)/dble(nModTot)
    do i=1,nModTot
        h_ave(i) = h_b_in + dh*dble(i) - dh/2.d0
    enddo

    err = 10.*tol; iter = 0
    do while((dabs(err)>tol) .and. (iter < 50))   !main iteration loop
        iter = iter + 1
        m_dot = m_dotX
        
        !update the turbine pressure and enthalpies
        P_turb_in = P_check(P_max, turb_pres_frac(m_dot*dble(nLoops)/m_dot_pb_des, fossil_mode, ffrac(touperiod), fP_turb_min)*P_turb_des)
        dP_basis = m_dot*dble(nLoops)/m_dot_des*P_turb_des
        !Guess the loop inlet/outlet enthalpies
        call water_TP((T_pb_out-273.15d0), P_check(P_max, P_turb_in+dP_basis*(fP_sf_tot-fP_hdr_c))*100., enth=h_b_in)
        call water_TP((T_field_out_des-273.15d0), P_check(P_max, P_turb_in+dP_basis*fP_hdr_h)*100., enth=h_sh_out)
        !Set the loop inlet enthalpy
        h_in(1) = h_b_in
        
        !initialize
        q_loss(:) = 0.d0
        q_abs(:) = 0.d0
        
        do i=1,nModTot
            !which geometry set?
            gset=1
            if((i>nModBoil).and.(is_multgeom)) gset=2

            !Calculate thermal losses based on temperature guess values
            !calculate the local pressure in the superheater. Assume a linear pressure drop across each section
            P_loc = P_check(P_max, P_turb_in + dP_basis * (fP_hdr_h + (fP_sf_sh + fP_boil_to_sh + fP_sf_boil)*(1.-dble(i-1)/dble(nModTot))))
            
            !Get the temperature at each state in the loop
            call water_PH(P_loc*100., h_ave(i), temp=T_ave(i)); T_ave(i)=T_ave(i)+273.15d0
                        
            !Calculate the heat loss at each temperature
            if(HLCharType(gset)==1) then 
                !Estimate based on the polynomial adjustments provided by the user
                dT_loc = T_ave(i) - T_db
                c_hl = HL_dT_C0(gset) + HL_dT_C1(gset)*dT_loc + HL_dT_C2(gset)*dT_loc**2 + HL_dT_C3(gset)*dT_loc**3  ![W/m] Effect from dT
                if((dabs(HL_W_C0(gset)) + dabs(HL_W_C1(gset)) + dabs(HL_W_C2(gset)) + dabs(HL_W_C3(gset))) > 0.) &
                    c_hl = c_hl * (HL_W_C0(gset) + HL_W_C1(gset)*V_wind + HL_W_C2(gset)*V_wind**2 + HL_W_C3(gset)*V_wind**3) !adjusted based on wind velocity
                q_loss(i) = c_hl*L_col(gset)/1000.d0    ![kW] Total thermal loss from this collector
                q_abs(i) = q_rec(i) - q_loss(i) ![kW] Total absorbed energy in this collector module
            elseif(HLCharType(gset)==2) then
                !Calculate thermal loss from Forristall receiver model (algorithm is found in Type250)
                q_rec_loss(:) = 0.d0
                q_rec_abs(:) = 0.d0
                
                do j=1,4
                    
                    !Only calculate if the HCE fraction is non-zero
                    if(HCE_FieldFrac(gset,j)<=0.) cycle
                
                    !Get emissivity properties
                    if(epsilon_3l(gset,j)>1) then
                        xx(:)=epsilon_3t(gset,j,:)
                        yy(:)=epsilon_3(gset,j,:)
                    else
                        eps_3 = epsilon_3(gset,j,1)
                    endif                    
                
                    !Call the receiver performance model - single point mode
                    !This call uses VP1 as the HTF since 2-phase heat transfer correlations have high uncertainty. The 
                    !only use for the fluid type in the single point model is calculating the convective heat transfer
                    !coefficient between the HTF and inner absorber wall. This is sufficiently high for both HTF and 
                    !steam that substituting the HTF here introduces negligible error.
                    call EvacReceiver(&  !Inputs
                          T_ave(i), 10.d0, T_db, T_sky, v_wind, P_amb, q_inc(i)/L_col(gset)*1000.d0, A_cs(gset), D_2(gset), D_3(gset),D_4(gset), &
                          D_5(gset), D_p(gset), D_h(gset),epsilon_3l(gset,j), xx, yy, nea, L_col(gset), .true., eps_3, Epsilon_4(gset,j), Epsilon_5(gset,j),& 
                          Alpha_abs(gset,j), alpha_env(gset,j), (eta_optical(gset)*defocus_lim*Shadowing(gset,j)*Dirt_HCE(gset,j)), Tau_envelope(gset,j), P_a(gset,j), &
                          Flow_type(gset), AbsorberMaterial(gset), annulusGas(gset,j), glazingIntact(gset,j), 21.d0, info,time,&
                          q_rec_loss(j),q_rec_abs(j),dum(1),dum(2),dum(3))
                    
                    if(isnan(q_rec_abs(j))) then !if not a number, nip in the bud here
                        q_rec_abs(j) = 0.d0
                        q_rec_loss(j) = 0.d0
                    endif
                    
                    !running totals
                    q_loss(i) = q_loss(i) + q_rec_loss(j)*L_col(gset)*HCE_FieldFrac(gset,j)/1000.d0  ![kW]
                    q_abs(i) = q_abs(i) + q_rec_abs(j)*L_col(gset)*HCE_FieldFrac(gset,j)/1000.d0   ![kW]
                enddo
            endif
            
            !Set the inlet enthalpy equal to the outlet of the previous node
            if(i>1) h_in(i) = h_out(i-1)
            
            !calculate the collector outlet enthalpy 
            tol_t = .001d0; err_t = 10.*tol_t; iter_t = 0
            do while ((err_t>tol_t).and.(iter_t<50)) 
                !Calculate the average enthalpy value in the collector module
                h_out(i) = enth_lim(h_in(i) + q_abs(i)/m_dotX - (T_ave(i) - T_ave0(i))*e_trans/dt)
                !update guesses for h_ave and T_ave
                h_aveg = (h_out(i) + h_in(i))/2.d0
                !Update the average temperature for the heat loss calculation
                call water_PH(P_loc*100.d0, h_aveg, temp=T_ave(i)); T_ave(i)=T_ave(i)+273.15d0
                err_t = dabs((h_ave(i) - h_aveg)/h_ave(i))
                h_ave(i) = h_aveg
            enddo
            !predict the next outlet enthalpy
            if(i<nModTot) h_ave(i+1) = enth_lim(h_in(i) + (h_out(i)-h_in(i))*1.5)
        enddo
        
        err = (h_sh_out - h_out(nModTot))/h_sh_out
        
        if((m_dot==m_dot_min).and.(err>0.d0)) exit    !M_dot may already equal m_dot_min while the temperature is still too low, this saves 1 more iteration
        
        !****************************************************   
        !***** Hybrid False Position Iteration Method********  
        !****************************************************
        if((lowflag).and.(upflag))then
            if(err > 0.d0)then
                m_dot_upper = m_dot
                y_upper     = err
            else
                m_dot_lower = m_dot
                y_lower     = err
            endif
                m_dotX = (y_upper)/(y_upper-y_lower)*(m_dot_lower - m_dot_upper) + m_dot_upper
        else
        
            if(err > 0.d0)then      !Prescribed is greater than calculated, so decrease mass flow, so set upper limit
                m_dot_upper = m_dot
                y_upper     = err
                upflag      = .true.
                upmult      = 0.5d0
            else                    !Presribed is less than calculated, so increase mass flow, so set lower limit
                m_dot_lower = m_dot
                y_lower     = err
                lowflag     = .true.
                upmult      = 0.5d0
            endif
            
            if((lowflag).and.(upflag))then  !If results of bracket are defined, use false position
                m_dotX = (y_upper)/(y_upper-y_lower)*(m_dot_lower - m_dot_upper) + m_dot_upper
            else                            !If not, recalculate value based on approximate energy balance
                if(iter<3)then
                    m_dotX = sum(q_abs(:))/(h_sh_out - h_b_in)
                    !m_dotX = dmax1(m_dot_min, min(m_dotX, m_dot_max))
                    m_dotX = dmax1(m_dot_min*.5, dmin1(m_dotX, m_dot_max*1.5))
                else
                    m_dotX = 0.5*m_dot_upper + 0.5*m_dot_lower
                endif
            endif
        endif
        
        !***************************************************************************
        !****** End Hyrbid False Position Iteration Method *************************
        !***************************************************************************
        if(m_dot_lower >= m_dot_b_max) then      !Once the minimum possible m_dot to solve energy balance is greater than maximum allowable, exit loop and go to defocus
            is_def = .true.
            exit
        endif

        if(m_dot_upper <= m_dot_min)then      !Once the maximum possible m_dot to solve energy balance is less than minimum allowable, set to min value and get final T_out
            m_dotX = m_dot_min
            solveMode = 3
        endif
    enddo
    
    !Defocus calculations
    err_def = (m_dotX - m_dot_max)/m_dot_max
    if((.not.is_def).and.(err_def>0.d0)) is_def = .true.
    if(is_def) then
        !calculate new defocus
        defocus = dmin1(1.d0, defocus0 * (1.d0/(err_def + 1.d0))**rc)
        if(dabs(err_def)>tol_def) then !quit iterating if there's no significant change in the defocus
            defocus0 = defocus
            iter_def = iter_def + 1
            if(iter_def<11) goto 10
        endif
    endif    
    
    
    !The boiler mass flow rate is equal to the flow throughout the loop
    m_dot_b_tot = m_dot*dble(nLoops) ![kg/s]
    
else 
!---------------"standard" boiler + optional superheater design ---------------------------
    !Boiler
    !Guess the field inlet enthalpy
    call water_TP((T_pb_out-273.15d0), P_check(P_max, P_turb_in+dP_basis*(fP_sf_tot-fP_hdr_c))*100., enth=h_pb_out)
    
    !boiler outlet conditions
    call water_PQ(P_check(P_max, P_turb_in+dP_basis*(fP_hdr_h+fP_sf_sh+fP_boil_to_SH))*100., x_b_des, enth=h_b_out) !2-phase outlet enthalpy
    call water_PQ(P_check(P_max, P_turb_in+dP_basis*(fP_hdr_h+fP_sf_sh+fP_boil_to_SH))*100., 0.d0, enth=h_b_recirc) !Recirculation enthalpy
    
    !Determine the mixed inlet enthalpy
    h_b_in = h_pb_out*x_b_des + h_b_recirc*(1.-x_b_des)

    !Set the loop inlet enthalpy
    h_in(1) = h_b_in
    
    m_dot_bX = m_dotX/x_b_des
    !Set up iteration brackets for false-position method
    m_dot_lower = 0.7*m_dot_min/x_b_des
    m_dot_upper = 1.3*m_dot_b_max

    upflag = .false.        !Set logic to switch from bisection to false position mode
    lowflag = .false.       !Set logic to switch from bisection to false position mode

    !set iteration tolerance
    tol = 1.e-4
    
    !Do a rough guess of the receiver enthalpy for the boiler
    dh_b = (h_b_out - h_b_in)/dble(nModBoil)
    do i=1,nModBoil
        h_ave(i) = h_b_in + dh_b*dble(i) - dh_b/2.d0
    enddo
    
    err = 10.*tol; iter = 0
    do while((dabs(err)>tol) .and. (iter < 50))   !main iteration loop
        iter = iter + 1
        m_dot_b = m_dot_bX
        !initialize
        q_loss(:) = 0.d0
        q_abs(:) = 0.d0
        
        !Update inlet enthalpy conditions and turbine pressure
        P_turb_in = P_check(P_max, turb_pres_frac(m_dot_b*x_b_des*dble(nLoops)/m_dot_pb_des, fossil_mode, ffrac(touperiod), fP_turb_min)*P_turb_des )
        dP_basis = m_dot_b*dble(nLoops)/m_dot_b_des*P_turb_des
        !field inlet enthalpy
        call water_TP((T_pb_out-273.15d0), P_check(P_max, P_turb_in+dP_basis*(fP_sf_tot-fP_hdr_c))*100., enth=h_pb_out)
        !Update the boiler outlet conditions
        call water_PQ(P_check(P_max, P_turb_in+dP_basis*(fP_hdr_h+fP_sf_sh+fP_boil_to_SH))*100., x_b_des, enth=h_b_out) !2-phase outlet enthalpy
        call water_PQ(P_check(P_max, P_turb_in+dP_basis*(fP_hdr_h+fP_sf_sh+fP_boil_to_SH))*100., 0.d0, enth=h_b_recirc) !Recirculation enthalpy
        
        !Determine the mixed inlet enthalpy
        h_b_in = h_pb_out*x_b_des + h_b_recirc*(1.-x_b_des)
        !Set the loop inlet enthalpy
        h_in(1) = h_b_in
                
        do i=1,nModBoil
            !Calculate thermal losses based on temperature guess values
            !calculate the local pressure in the boiler. Assume a linear pressure drop across each section
            P_loc = P_check(P_max, P_turb_in + dP_basis * (fP_sf_tot - fP_sf_boil*(1.-dble(i-1)/dble(nModBoil))) )
            
            !Get the temperature at each state in the boiler
            call water_PH(P_loc*100., h_ave(i), temp=T_ave(i)); T_ave(i)=T_ave(i)+273.15d0
                        
            !Calculate the heat loss at each temperature
            if(HLCharType(1)==1) then 
                !Estimate based on the polynomial adjustments provided by the user
                dT_loc = T_ave(i) - T_db
                c_hl = HL_dT_C0(1) + HL_dT_C1(1)*dT_loc + HL_dT_C2(1)*dT_loc**2 + HL_dT_C3(1)*dT_loc**3  ![W/m] Effect from dT
                if((dabs(HL_W_C0(1)) + dabs(HL_W_C1(1)) + dabs(HL_W_C2(1)) + dabs(HL_W_C3(1))) > 0.) &
                    c_hl = c_hl * (HL_W_C0(1) + HL_W_C1(1)*V_wind + HL_W_C2(1)*V_wind**2 + HL_W_C3(1)*V_wind**3) !adjusted based on wind velocity
                q_loss(i) = c_hl*L_col(1)/1000.d0    ![kW] Total thermal loss from this collector
                q_abs(i) = q_rec(i) - q_loss(i) ![kW] Total absorbed energy in this collector module
            elseif(HLCharType(1)==2) then
                !Calculate thermal loss from Forristall receiver model (algorithm is found in Type250)
                q_rec_loss(:) = 0.d0
                q_rec_abs(:) = 0.d0
                
                do j=1,4
                    !Only calculate if the HCE fraction is non-zero
                    if(HCE_FieldFrac(1,j)<=0.) cycle
                    
                    !Get emissivity properties
                    if(epsilon_3l(1,j)>1) then
                        xx(:)=epsilon_3t(1,j,:)
                        yy(:)=epsilon_3(1,j,:)
                    else
                        eps_3 = epsilon_3(1,j,1)
                    endif                    
                
                    !Call the receiver performance model - single point mode
                    !This call uses VP1 as the HTF since 2-phase heat transfer correlations have high uncertainty. The 
                    !only use for the fluid type in the single point model is calculating the convective heat transfer
                    !coefficient between the HTF and inner absorber wall. This is sufficiently high for both HTF and 
                    !steam that substituting the HTF here introduces negligible error.
                    call EvacReceiver(&  !Inputs
                          T_ave(i), 10.d0, T_db, T_sky, v_wind, P_amb, q_inc(i)/L_col(1)*1000.d0, A_cs(1), D_2(1), D_3(1),D_4(1), &
                          D_5(1), D_p(1), D_h(1),epsilon_3l(1,j), xx, yy, nea, L_col(1), .true., eps_3, Epsilon_4(1,j), Epsilon_5(1,j),& 
                          Alpha_abs(1,j), alpha_env(1,j), (eta_optical(1)*defocus_lim*Shadowing(1,j)*Dirt_HCE(1,j)), Tau_envelope(1,j), P_a(1,j), &
                          Flow_type(1), AbsorberMaterial(1), annulusGas(1,j), glazingIntact(1,j), 21.d0, info,time,&
                          q_rec_loss(j),q_rec_abs(j),dum(1),dum(2),dum(3))
                    
                    if(isnan(q_rec_abs(j))) then !if not a number, nip in the bud here
                        q_rec_abs(j) = 0.d0
                        q_rec_loss(j) = 0.d0 
                    endif

                    !running totals
                    q_loss(i) = q_loss(i) + q_rec_loss(j)*L_col(1)*HCE_FieldFrac(1,j)/1000.d0  ![kW]
                    q_abs(i) = q_abs(i) + q_rec_abs(j)*L_col(1)*HCE_FieldFrac(1,j)/1000.d0   ![kW]
                enddo
            endif
            
            !Set the inlet enthalpy equal to the outlet of the previous node
            if(i>1) h_in(i) = h_out(i-1)
            
            !calculate the collector outlet enthalpy 
            tol_t = .001d0; err_t = 10.*tol_t; iter_t = 0
            do while ((err_t>tol_t).and.(iter_t<50)) 
                !Calculate the average enthalpy value in the collector module
                h_out(i) = enth_lim(h_in(i) + q_abs(i)/m_dot_bX - (T_ave(i) - T_ave0(i))*e_trans/dt)
                !update guesses for h_ave and T_ave
                h_aveg = (h_out(i) + h_in(i))/2.d0
                !Update the average temperature for the heat loss calculation
                call water_PH(P_loc*100.d0, h_aveg, temp=T_ave(i)); T_ave(i)=T_ave(i)+273.15d0
                err_t = dabs((h_ave(i) - h_aveg)/h_ave(i))
                h_ave(i) = h_aveg
            enddo
            !predict the next outlet enthalpy
            if(i<nModTot) h_ave(i+1) = enth_lim(h_in(i) + (h_out(i)-h_in(i))*1.5)
        enddo
        
        err = (h_b_out - h_out(nModBoil))/h_b_out
        
        if((m_dot_b==m_dot_min/x_b_des).and.(err>0.d0)) exit    !M_dot may already equal m_dot_min while the temperature is still too low, this saves 1 more iteration
        
        !****************************************************   
        !***** Hybrid False Position Iteration Method********  
        !****************************************************
        if((lowflag).and.(upflag))then
            if(err > 0.d0)then
                m_dot_upper = m_dot_b
                y_upper     = err
            else
                m_dot_lower = m_dot_b
                y_lower     = err
            endif
                m_dot_bX = (y_upper)/(y_upper-y_lower)*(m_dot_lower - m_dot_upper) + m_dot_upper
        else
        
            if(err > 0.d0)then      !Prescribed is greater than calculated, so decrease mass flow, so set upper limit
                m_dot_upper = m_dot_b
                y_upper     = err
                upflag      = .true.
                upmult      = 0.5d0
            else                    !Presribed is less than calculated, so increase mass flow, so set lower limit
                m_dot_lower = m_dot_b
                y_lower     = err
                lowflag     = .true.
                upmult      = 0.5d0
            endif
            
            if((lowflag).and.(upflag))then  !If results of bracket are defined, use false position
                m_dot_bX = (y_upper)/(y_upper-y_lower)*(m_dot_lower - m_dot_upper) + m_dot_upper
            else                            !If not, recalculate value based on approximate energy balance
                if(iter<3)then
                    m_dot_bX = sum(q_abs(1:nModBoil))/(h_b_out - h_b_in)
                    !m_dot_bX = dmax1( m_dot_min/x_b_des, min( m_dot_bX, m_dot_b_max))
                    m_dot_bX = dmax1(m_dot_min/x_b_des*.5, dmin1(m_dot_bX, m_dot_b_max*1.5))
                else
                    m_dot_bX = 0.5*m_dot_upper + 0.5*m_dot_lower
                endif
            endif
        endif
        
        !***************************************************************************
        !****** End Hyrbid False Position Iteration Method *************************
        !***************************************************************************
        if(m_dot_lower >= m_dot_b_max) then      !Once the minimum possible m_dot to solve energy balance is greater than maximum allowable, exit loop and go to defocus
            is_def = .true.
            exit
        endif

        if(m_dot_upper <= m_dot_min/x_b_des)then      !Once the maximum possible m_dot to solve energy balance is less than minimum allowable, set to min value and get final T_out
            m_dot_bX = m_dot_min/x_b_des
            solveMode = 3
        endif

    enddo
    !----end of boiler section
    
    !Defocus calculations
    err_def = (m_dot_bX - m_dot_b_max)/m_dot_b_max
    if((.not.is_def).and.(err_def>0.d0)) is_def = .true.
    if(is_def) then
        !calculate new defocus
        defocus = dmin1(1.d0, defocus0 * (1.d0/(err_def + 1.d0))**rc)
        !if((dabs(defocus-defocus0)>tol_def).or.(dabs(err_def)>tol_def)) then !quit iterating if there's no significant change in the defocus
        if(dabs(err_def)>tol_def) then
            defocus0 = defocus
            iter_def = iter_def + 1
            if(iter_def<11) goto 10
        endif
    endif
    
    !Superheater
    if(is_sh) then
        !choose which geometry set to use
        gset=1
        if(is_multgeom) gset=2
        
        !calculate superheater inlet enthalpy
        call water_PQ(P_check(P_max, P_turb_in+dP_basis*(fP_hdr_h+fP_sf_sh))*100., 1.d0, enth=h_sh_in)
        !The superheater outlet enthalpy is constrained according to the steam mass flow produced in the boiler
        call water_TP((T_field_out_des-273.15d0), P_check(P_max, P_turb_in+dP_basis*fP_hdr_h)*100., enth=h_sh_out)
        
        !Set the loop inlet enthalpy
        h_in(nModBoil+1) = h_sh_in
        
        !Do a rough guess of the receiver enthalpy for the boiler
        dh_sh = (h_sh_out - h_sh_in)/dble(nModSh)
        do ii=1,nModSh
            i=ii+nModBoil
            h_ave(i) = h_sh_in + dh_sh*dble(ii) - dh_sh/2.d0
        enddo
        
        m_dot = m_dot_b*x_b_des
        T_shX = T_field_out_des  !Guess the superheater outlet temperature
        
        !iterative loop to get convergence in heat loss
        tol = .01d0; err=10.*tol; iter=0
        
        do while((dabs(err)>tol).and.(iter<5))
            iter = iter + 1
            !initialize
            q_loss(nModBoil+1:nModTot) = 0.d0
            q_abs(nModBoil+1:nModTot) = 0.d0
            
            do ii=1,nModSh
                i=ii+nModBoil
                !Calculate thermal losses based on temperature guess values
                !calculate the local pressure in the superheater. Assume a linear pressure drop across each section
                P_loc = P_check(P_max, P_turb_in + dP_basis * (fP_hdr_h + fP_sf_sh*(1.-dble(ii-1)/dble(nModSh))) )
                
                !Get the temperature at each state in the boiler
                call water_PH(P_loc*100., h_ave(i), temp=T_ave(i)); T_ave(i)=T_ave(i)+273.15d0
                            
                !Calculate the heat loss at each temperature
                if(HLCharType(gset)==1) then 
                    !Estimate based on the polynomial adjustments provided by the user
                    dT_loc = T_ave(i) - T_db
                    c_hl = HL_dT_C0(gset) + HL_dT_C1(gset)*dT_loc + HL_dT_C2(gset)*dT_loc**2 + HL_dT_C3(gset)*dT_loc**3  ![W/m] Effect from dT
                    if((dabs(HL_W_C0(gset)) + dabs(HL_W_C1(gset)) + dabs(HL_W_C2(gset)) + dabs(HL_W_C3(gset))) > 0.) &
                        c_hl = c_hl * (HL_W_C0(gset) + HL_W_C1(gset)*V_wind + HL_W_C2(gset)*V_wind**2 + HL_W_C3(gset)*V_wind**3) !adjusted based on wind velocity
                    q_loss(i) = c_hl*L_col(gset)/1000.d0    ![kW] Total thermal loss from this collector
                    q_abs(i) = q_rec(i) - q_loss(i) ![kW] Total absorbed energy in this collector module
                elseif(HLCharType(gset)==2) then
                    !Calculate thermal loss from Forristall receiver model (algorithm is found in Type250)
                    q_rec_loss(:) = 0.d0
                    q_rec_abs(:) = 0.d0
                    
                    do j=1,4
                        !Only calculate if the HCE fraction is non-zero
                        if(HCE_FieldFrac(gset,j)<=0.) cycle
                        
                        !Get emissivity properties
                        if(epsilon_3l(gset,j)>1) then
                            xx(:)=epsilon_3t(gset,j,:)
                            yy(:)=epsilon_3(gset,j,:)
                        else
                            eps_3 = epsilon_3(gset,j,1)
                        endif                    
                    
                        !Call the receiver performance model - single point mode
                        !This call uses VP1 as the HTF since 2-phase heat transfer correlations have high uncertainty. The 
                        !only use for the fluid type in the single point model is calculating the convective heat transfer
                        !coefficient between the HTF and inner absorber wall. This is sufficiently high for both HTF and 
                        !steam that substituting the HTF here introduces negligible error.
                        call EvacReceiver(&  !Inputs
                              T_ave(i), 10.d0, T_db, T_sky, v_wind, P_amb, q_inc(i)/L_col(gset)*1000.d0, A_cs(gset), D_2(gset), D_3(gset),D_4(gset), &
                              D_5(gset), D_p(gset), D_h(gset),epsilon_3l(gset,j), xx, yy, nea, L_col(gset), .true., eps_3, Epsilon_4(gset,j), Epsilon_5(gset,j),& 
                              Alpha_abs(gset,j), alpha_env(gset,j), (eta_optical(gset)*defocus_lim*Shadowing(gset,j)*Dirt_HCE(gset,j)), Tau_envelope(gset,j), P_a(gset,j), &
                              Flow_type(gset), AbsorberMaterial(gset), annulusGas(gset,j), glazingIntact(gset,j), 21.d0, info,time,&
                              q_rec_loss(j),q_rec_abs(j),dum(1),dum(2),dum(3))

                        if(isnan(q_rec_abs(j))) then !if not a number, nip in the bud here
                            q_rec_abs(j) = 0.d0
                            q_rec_loss(j) = 0.d0
                        endif
                        
                        !running totals
                        q_loss(i) = q_loss(i) + q_rec_loss(j)*L_col(gset)*HCE_FieldFrac(gset,j)/1000.d0  ![kW]
                        q_abs(i) = q_abs(i) + q_rec_abs(j)*L_col(gset)*HCE_FieldFrac(gset,j)/1000.d0   ![kW]
                    enddo
                endif
                
                !Set the inlet enthalpy equal to the outlet of the previous node
                if(ii>1) h_in(i) = h_out(i-1)
                
                !calculate the collector outlet enthalpy 
                tol_t = .001d0; err_t = 10.*tol_t; iter_t = 0
                do while ((err_t>tol_t).and.(iter_t<50)) 
                    !Calculate the average enthalpy value in the collector module
                    h_out(i) = enth_lim(h_in(i) + q_abs(i)/m_dot - (T_ave(i) - T_ave0(i))*e_trans/dt)
                    !update guesses for h_ave and T_ave
                    h_aveg = (h_out(i) + h_in(i))/2.d0
                    !Update the average temperature for the heat loss calculation
                    call water_PH(P_loc*100.d0, h_aveg, temp=T_ave(i)); T_ave(i)=T_ave(i)+273.15d0
                    err_t = dabs((h_ave(i) - h_aveg)/h_ave(i))
                    h_ave(i) = h_aveg
                enddo
            enddo
        
            err = (T_shX - T_ave(nModTot))/T_shX
            T_shX = T_ave(nModTot)
        
        enddo
        
    else
        m_dot_field = m_dot_b*x_b_des*dble(nLoops) ![kg/s] The total field mass flow rate is just the saturated steam coming from the boiler section
    endif
    !----end superheater section
        
    m_dot_b_tot = m_dot_b*dble(nLoops)
endif

!-----------Calculate final solar field values------------
!total effective solar field mass flow rate
m_dot_Field = m_dot*dble(nLoops) ![kg/s]
!Look up temperatures
call water_PH(P_check(P_max, P_turb_in + dP_basis*(fP_sf_tot-fP_hdr_c))*100., h_in(1), temp=T_field_in) ![C]
call water_PH(P_check(P_max, P_turb_in + dP_basis*fP_hdr_h)*100., h_out(nModTot), temp=T_loop_out) ![C]
!piping thermal loss
q_loss_piping = Ap_tot * Pipe_hl_coef/1000.d0 * ((T_field_in + T_loop_out)/2. - (T_db-273.15d0))  !hl coef is [W/m2-K], use average field temp as driving difference
!Given the piping heat/pressure loss, calculate the temperature at the inlet to the power block
if(m_dot>0.d0) then
    h_to_pb = h_out(nModTot) - q_loss_piping/m_dot_field
else
    h_to_pb = h_out(nModTot)
endif
call water_PH(P_turb_in*100., h_to_pb, temp=T_field_out) ![C]
!energies
q_inc_tot = sum(q_inc) * dble(nLoops) / 1000.d0 !Total incident on the collector field
q_rec_tot = sum(q_rec) * dble(nLoops) / 1000.d0 !Total incident on receiver after reflection
q_abs_tot = sum(q_abs) * dble(nLoops) / 1000.d0 !Total absorbed thermal energy
q_loss_rec = sum(q_loss) * dble(nLoops)/1000.d0 ![MW] Thermal losses from the receiver
q_loss_piping = q_loss_piping/1000.  ![MW] Thermal losses from non-receiver piping
q_loss_sf = q_loss_rec + q_loss_piping ![MW] Total solar field losses, receiver + piping loss
q_field_delivered = m_dot_field*dmax1((h_to_pb - h_pb_out), 0.d0) ![kW] Energy balance indicating total energy delivered from the solar field
q_dump = (1.-defocus)*q_rec_tot  ![MW] Total amount of energy dumped by collector defocusing
h_field_out = h_to_pb   !h_field_out is corrected later if fossil energy is supplied in topping mode
if(q_rec_tot>0.d0) then
    eta_thermal = 1. - dmin1(dmax1(q_loss_sf/q_rec_tot, 0.d0), 1.d0)  !thermal efficiency after reflection
else
    eta_thermal = 0.d0
endif
!calculate solar field pressure drops all [bar]
dP_tot = dP_basis*fP_sf_tot
dP_hdr_c = dP_basis*fP_hdr_c
dP_sf_boil = dP_basis*fP_sf_boil
dP_boil_to_SH = dP_basis*fP_boil_to_SH
dP_sf_sh = dP_basis*fP_sf_sh
dP_hdr_h = dP_basis*fP_hdr_h
!Calculate the total change in energy state
E_bal_startup = 0.d0; E_field = 0.d0
do i=1,nModTot
    E_bal_startup = E_bal_startup + (T_ave(i)-T_ave0(i))*e_trans/dt*dble(nLoops)/1000.d0 ![MW]
    E_field = E_field + (T_ave(i) - T_field_ini)*e_trans/dt*dble(nLoops)/1000.d0 ![MW]
enddo

!--------------------------------------------------------------------------------------
! Control logic
!--------------------------------------------------------------------------------------
!Check how much power is available from the aux backup
q_aux_avail = dmin1(q_pb_des*ffrac(touperiod), q_max_aux)

!Fossil mode 3 allows fossil as supplemental temperature boosting. This requires flow from the field.
!it is also possible that the total of the energy from the field plus the topping energy does not 
!produce enough to meet the cycle cutoff fraction, so calculate here how much energy would be contributed
!from the fossil contribution
if(fossil_mode==3.) then
    if(q_field_delivered <= 0.) then
        q_aux_avail = 0.d0
    else
        if(is_sh) then
            call water_TP((T_field_out_des-273.15d0), P_turb_in*100.d0, enth=h_target)
        else
            call water_PQ(P_turb_in*100.d0, x_b_des, enth=h_target)
        endif
        !Thermal requirement for the aux heater
        q_aux_avail = dmax1(0.d0, (h_target - h_field_out)*m_dot_field)
    endif
endif

!Calculate the total available energy for the power cycle as the sum of the energy from the solar field
!and the aux backup
q_avail_tot = q_aux_avail + dmax1(0.d0,q_field_delivered)

!Do we have enough to run the power cycle?
if(q_avail_tot >= q_pb_des*cycle_cutoff_frac) then
    
    if(q_aux_avail > 0.d0) then
        !Calculate the contribution from the aux backup
        select case(int(fossil_mode))
        case(1) !backup minimum level - parallel
            if(is_sh) then
                call water_TP((T_field_out_des-273.15d0), P_turb_in*100.d0, enth=h_target)
            else
                call water_PQ(P_turb_in*100.d0, x_b_des, enth=h_target)
            endif
            q_aux = dmax1(0.d0, q_aux_avail - q_field_delivered)
            m_dot_aux = q_aux/(h_target - h_pb_out)
            if(q_field_delivered > 0.d0) then
                m_dot_to_pb = m_dot_aux + m_dot_field
                h_to_pb = (h_field_out*m_dot_field + h_target*m_dot_aux)/m_dot_to_pb
            else
                m_dot_to_pb = m_dot_aux
                h_to_pb = h_target
            endif
            call water_PH(P_turb_in*100.d0, h_to_pb, temp=T_pb_in)
            q_to_pb = m_dot_to_pb * (h_to_pb - h_pb_out)
            
        case(2) !supplemental parallel
            if(is_sh) then
                call water_TP((T_field_out_des-273.15d0), P_turb_in*100.d0, enth=h_target)
            else
                call water_PQ(P_turb_in*100.d0, x_b_des, enth=h_target)
            endif
            q_aux = dmin1(q_pb_des - q_field_delivered, q_aux_avail)
            !For parallel operation, the result is a weighted mix of the field output and the boiler
            m_dot_aux = q_aux/(h_target - h_pb_out)
            if(q_field_delivered>0.d0) then
                m_dot_to_pb = m_dot_aux + m_dot_field
                h_to_pb = (h_field_out*m_dot_field + h_target*m_dot_aux)/m_dot_to_pb
            else
                m_dot_to_pb = m_dot_aux
                h_to_pb = h_target
            endif
            call water_PH(P_turb_in*100.d0, h_to_pb, temp=T_pb_in)
            q_to_pb = m_dot_to_pb * (h_to_pb - h_b_in)
            
        case(3) !supplemental topping
            !The auxiliary heater is used to bring the steam from the solar field up to the design-point temperature
            !for the power block. The fossil use corresponds to the operation level of the solar field.
            if(is_sh) then
                call water_TP((T_field_out_des-273.15d0), P_turb_in*100.d0, enth=h_target)
            else
                call water_PQ(P_turb_in*100.d0, x_b_des, enth=h_target)
            endif
            !the flow rate through the aux heater is the same as through the field
            m_dot_aux = m_dot_field
            !Thermal requirement for the aux heater
            q_aux = dmin1(dmax1(0.d0, h_target - h_field_out)*m_dot_aux, q_aux_avail)
            !Calculate the enthalpy into the power block
            h_to_pb = h_field_out + q_aux/m_dot_aux
            m_dot_to_pb = m_dot_field
            q_to_pb = m_dot_to_pb * (h_to_pb - h_pb_out)
            call water_PH(P_turb_in*100.d0, h_to_pb, temp=T_pb_in)
        end select
        q_aux_fuel = q_aux/lhv_eff
    else
        !no aux backup, just the solar field
        m_dot_to_pb = m_dot_field
        T_pb_in = T_field_out
        q_to_pb = q_field_delivered
    endif
    standby_control = 1. !We're operating the power block normally
else
    !There isn't enough energy to run the power block
    
    !Do we have enough to do standby?
    if((q_avail_tot > q_pb_des*q_sby_frac).and.(t_sby0>0.).and.(is_pb_on0)) then
        standby_control = 2. !Operate in standby mode
        t_sby_now = dmax1(0.d0, t_sby0-dt/3600.d0)
        q_aux = dmax1(q_sby_frac*q_pb_des - q_field_delivered, 0.d0)
        m_dot_aux = 0.d0 !it's not meaningful to report the aux mass flow rate
        q_aux_fuel = q_aux/lhv_eff
        q_to_pb = q_sby_frac*q_pb_des
        T_pb_in = T_field_in_des
    else
        standby_control = 3. !Turn off the power block
        t_sby_now = t_sby
        q_aux = 0.d0
        m_dot_aux = 0.d0
        q_field_delivered = 0.d0
        q_aux_fuel = 0.d0
        q_to_pb = 0.
        T_pb_in = T_field_out
    endif
    m_dot_to_pb = 0.d0
    
endif

!-----------Calculate final plant values -----------------
!Calculate parasitic values
if(q_aux > 0.d0) then
    W_dot_aux = W_pb_des/1000. * Aux_parVal * Aux_parPF * (Aux_par0 + Aux_par1 * (q_aux/q_pb_des) + Aux_par2 * (q_aux/q_pb_des)**2)
else
    W_dot_aux = 0.d0
endif
!Parasitic power associated with operation of the aux boiler
if(q_to_pb > 0.d0) then
    W_dot_bop = W_pb_des/1000. * BOP_parVal * BOP_parPF * (BOP_par0 + BOP_par1 * (q_to_pb/q_pb_des) + BOP_par2 * (q_to_pb/q_pb_des)**2)
else
    W_dot_bop = 0.d0
endif
!parasitic power as a function of power block load
if(SolarAlt > 0.d0) then
    W_dot_col = ftrack*sca_drives_elec*Ap_tot/1.e6	!Parasitic electric power consumed by the collectors
else
    W_dot_col = 0.d0
endif
W_dot_fixed = pb_fixed_par*W_pb_des/1000.	!Fixed parasitic power losses.. for every hour of operation

!If the power block isn't running, there's only a small pressure differential across the field
if(q_to_pb <= 0.d0) then
    P_turb_in = P_turb_des * turb_pres_frac(cycle_cutoff_frac, fossil_mode, ffrac(touperiod), fP_turb_min) 
    !* cycle_cutoff_frac  mjw 11.15.11 The cycle cutoff restriction occasionally allows low pressures and steaming in the feedwater line. Avoid this.
    dP_hdr_c = dP_basis * fP_hdr_c
    dP_sf_boil = dP_basis * fP_sf_boil
    dP_boil_to_sh = dP_basis * fP_boil_to_sh
    dP_sf_sh = dP_basis * fP_sf_sh
    dP_hdr_h = dP_basis * fP_hdr_h
    dP_tot = dP_basis * (fP_hdr_c + fP_sf_boil + fP_boil_to_sh + fP_sf_sh + fP_hdr_h)
endif

!feedwater pump parasitic
call water_PQ(P_check(P_max, P_turb_in+dP_basis*(fP_hdr_c+fP_sf_boil+fP_boil_to_SH+fP_sf_sh+fP_hdr_h))*100., 0.d0, temp = dum(1)) !limit the temperature to the saturation temp at the given pressure
call water_TP(dmin1(T_pb_out-273.15d0, dum(1)), P_check(P_max, P_turb_in+dP_basis*(fP_hdr_c+fP_sf_boil+fP_boil_to_SH+fP_sf_sh+fP_hdr_h))*100., dens=rho_fw)
if(is_oncethru) then
    W_dot_pump = m_dot_field*dP_tot/rho_fw/eta_pump*.1d0 ![MW]
else
    W_dot_pump = (m_dot_field*(fP_hdr_c+fP_boil_to_Sh+fP_sf_sh+fP_hdr_h)+m_dot_field/x_b_des*fP_sf_boil)*dP_basis/rho_fw/eta_pump*.1d0 ![MW], P_turb is bar
endif

!Solar field efficiency
eta_sf = eta_opt_ave * eta_thermal

!Limit the reported values of solar azimuth/elevation
solaralt = dmax1(0.d0, SolarAlt)
if(solaralt<=0.d0) SolarAz = 0.d0
!part-load control is always 2
cycle_pl_control=2.d0


!Write outputs
out(1) = cycle_pl_control 		![none] Part-load control flag - used by Type224
out(2) = dP_tot 		![bar] Total HTF pressure drop
out(3) = dP_hdr_c 		![bar] Average cold header pressure drop
out(4) = dP_sf_boil 		![bar] Pressure drop across the solar field boiler
out(5) = dP_boil_to_SH 		![bar] Pressure drop between the boiler and superheater
out(6) = dP_sf_sh 		![bar] Pressure drop across the solar field superheater
out(7) = dP_hdr_h 		![bar] Average hot header pressure drop
out(8) = E_bal_startup 		![MW] Startup energy consumed
out(9) = E_field 		![MW-hr] Accumulated internal energy in the entire solar field
out(10) = E_fp_tot 		![J] Freeze protection energy
out(11) = eta_opt_ave 		![none] Collector equivalent optical efficiency
out(12) = eta_thermal 		![none] Solar field thermal efficiency (power out/ANI)
out(13) = eta_sf 		![none] Total solar field collection efficiency
out(14) = defocus 		![none] The fraction of focused aperture area in the solar field
out(15) = m_dot_aux*3600 		![kg/s] --> [kg/hr] Auxiliary heater mass flow rate
out(16) = m_dot_field*3600 		![kg/s] --> [kg/hr] Flow rate from the field
out(17) = m_dot_b_tot*3600 		![kg/s] --> [kg/hr] Flow rate within the boiler section
out(18) = m_dot 		![kg/s] Flow rate in a single loop
out(19) = m_dot_to_pb*3600 		![kg/s] --> [kg/hr] Flow rate delivered to the power block
out(20) = P_turb_in 		![bar] Pressure at the turbine inlet
out(21) = q_loss_piping 		![MW] Pipe heat loss in the hot header and the hot runner
out(22) = q_aux*0.001 		![MW] Thermal energy provided to the fluid passing through the aux heater
out(23) = q_aux_fuel*3.412e-3 		![W] --> [MMBTU] Heat content of fuel required to provide aux firing
out(24) = q_dump 		![MW] Dumped thermal energy
out(25) = q_field_delivered*0.001 		![kW] --> [MW] Total solar field thermal power delivered
out(26) = q_inc_tot 		![MW] Total power incident on the field
out(27) = q_loss_rec 		![MW] Total Receiver thermal losses
out(28) = q_loss_sf 		![MW] Total solar field thermal losses
out(29) = q_to_pb*0.001 		![kW] --> [MW] Thermal energy to the power block
out(30) = SolarAlt*57.2958 		![rad] --> [deg] Solar altitude used in optical calculations
out(31) = SolarAz*57.2958 		![rad] --> [deg] Solar azimuth used in optical calculations
out(32) = phi_t*57.2958 		![rad] --> [deg] Transversal solar incidence angle
out(33) = theta_L*57.2958 		![rad] --> [deg] Longitudinal solar incidence angle
out(34) = standby_control 		![none] Standby control flag - used by Type224
out(35) = T_field_in 		![C] HTF temperature into the collector field header
out(36) = T_field_out 		![C] HTF Temperature from the field
out(37) = T_loop_out 		![C] Loop outlet temperature
out(38) = T_pb_in 		![C] HTF Temperature to the power block
out(39) = W_dot_aux 		![MW] Parasitic power associated with operation of the aux boiler
out(40) = W_dot_bop 		![MW] parasitic power as a function of power block load
out(41) = W_dot_col 		![MW] Parasitic electric power consumed by the collectors
out(42) = W_dot_fixed 		![MW] Fixed parasitic power losses.. for every hour of operation
out(43) = W_dot_pump 		![MW] Required solar field pumping power

return 1

contains
    
    real(8) function turb_pres_frac(m_dot_nd, fmode, ffrac, fP_min)
        !Take a mass flow fraction, fossil backup fraction, fossil fill mode, and minimum turbine fraction
        !and calculate the corresponding fraction of the design point pressure at which the turbine 
        !will operate
        !------------------------------------------------------------------------------------------
        !INPUTS:
        !------------------------------------------------------------------------------------------
        !   *   m_dot_nd        Non-dimensionalized mass flow from the solar field (only!)
        !   *   fmode           Mode of operation for the fossil backup (1, 2, or 3, see doc in code)
        !   *   ffrac           Fossil fill fraction for the current time step
        !   *   fP_min          Minimum allowable non-dimensionalized pressure at the turbine inlet
        !RETURNS:
        !------------------------------------------------------------------------------------------
        !   *   Fraction of the design-point turbine pressure
        !------------------------------------------------------------------------------------------
        
        implicit none
        real(8),intent(in)::m_dot_nd, ffrac, fP_min
        integer,intent(in)::fmode
        
        select case(fmode)
        case(1) !backup minimum level - parallel
            turb_pres_frac = dmax1(fP_min, m_dot_nd, ffrac)
        case(2) !supplemental operation - parallel
            turb_pres_frac = dmax1(fP_min, m_dot_nd, dmin1(1.d0, m_dot_nd + ffrac))
        case(3) !temperature topping mode - series
            turb_pres_frac = dmax1(fP_min, m_dot_nd)
        end select 
        
    end function turb_pres_frac
    
    subroutine theta_trans(alpha_sun, phi_sun, alpha_fix, phi_t, theta)
        
        !Take solar position and convert it into longitudinal and transversal incidence angles
        !Reference: G. Zhu (2011). Incidence Angle Modifier for Parabolic Trough Collector and its 
        !           Measurement at SIMTA. Internal communication, NREL, August, 2011.
        
        !------------------------------------------------------------------------------------------
        !INPUTS:
        !------------------------------------------------------------------------------------------
        !   *   alpha_sun       Solar azimuth angle, range is (-90=E..0=S..+90=W)
        !   *   phi_sun         Solar zenith angle, zero is directly overhead
        !   *   alpha_fix       Angle of rotation of the collector axis. Zero when aligned north-
        !                       south, positive clockwise
        !OUTPUTS:
        !------------------------------------------------------------------------------------------
        !   *   phi_t           Collector angle in the transversal plane
        !   *   theta           Collector angle in the longitudinal plane
        !   *   is_deg          Optional boolean flag for specifying that units are in degrees 
        !                       (default is radians)
        !------------------------------------------------------------------------------------------
        
        implicit none
        real(8),intent(in)::alpha_sun, phi_sun, alpha_fix
        !logical,optional::is_deg
        real(8),intent(out)::phi_t, theta
        real(8)::pi, d2r, alpha_sunX
        
        !Check to see if the user has specified the units to be degree
        !if(.not.present(is_deg)) is_deg = .false.
        pi=3.1415926d0
        !if(is_deg) then
        !    d2r = pi/180.d0
        !else
            d2r = 1.d0
        !endif
        
        !if the sun is below the horizon, return zeros
        if(phi_sun*d2r >= pi/2.) then
            phi_t=0.d0; theta=0.d0
            return
        endif
        
        !Convert the solar azimuth to 0=N
        alpha_sunX = alpha_sun*d2r+pi
        
        !Calculate angles
        phi_t = dabs(atan(tan(phi_sun*d2r)*sin(alpha_sunX*d2r - alpha_fix*d2r)))/d2r !collector angle in transversal plane
        theta = dabs(asin(sin(phi_sun*d2r)*cos(alpha_sunX*d2r - alpha_fix*d2r)))/d2r !collector angle in the longitudinal plane
        
        !check for NaN
        if(isnan(theta) .or. isnan(phi_t)) then
            phi_t = 0.d0; theta=0.d0
        endif
        
        return
        
    end subroutine theta_trans
    

    real(8) function enth_lim(enth, hmin, hmax)
    !Take an enthalpy value and limit it to the valid range. The hmax and hmin values set the enthalpy 
    !range, and these arguments should be set up at the beginning of the simulation. They are otherwise
    !optional arguments.
    
        real(8),intent(in)::enth
        real(8),optional::hmax, hmin
        real(8),save::hmax_s, hmin_s
        if(present(hmax)) hmax_s = hmax
        if(present(hmin)) hmin_s = hmin
        
        if(enth>hmax_s) then
            enth_lim = hmax_s
        elseif(enth<hmin_s) then
            enth_lim = hmin_s
        else
            enth_lim = enth
        endif
    end function enth_lim

end subroutine    

real(8) function P_check(P_max, P)
    implicit none
    real(8),intent(in)::P_max, P
    real(8),save::P_save, P_max_save
    logical,save::is_error
    character::m1*20,m2*20
    
    !Function operates in 3 modes depending on the value of P_max
    !(P_max == -2) Reset the error flag - new time step
    !(P_max == -1) End of time step, print any errors
    !(P_max > 0) Check the pressure, store the highest value for later printing.
    
    if(P_max == -2.) then
        P_save = 0.d0
        is_error = .false.
        P_check = 0.d0
        return
    elseif(P_max == -1.) then
        if(is_error) then
            write(unit=m1,fmt="(F5.1)") P_max_save
            write(unit=m2,fmt="(F5.1)") P_save
            call messages(-1,"A steam pressure of "//trim(m2)//" bar was outside of the valid correlation range and was reset to the maximum value of "//trim(m1)//" bar.", "WARNING",0,260)
        else
            P_save = 0.d0
        endif
        P_check = P
        return
    elseif(P_max > 0.d0) then
        if(P>P_max) then
            is_error = .true.
            if(P>P_save) P_save = P
            P_max_save = P_max
            P_check = P_max
            return
        else
            P_check = P
            return
        endif
    endif
    
end function P_check
