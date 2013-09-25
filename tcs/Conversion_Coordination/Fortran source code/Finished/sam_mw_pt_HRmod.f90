

!************************************************************************************************************
!*                                      Cooling subroutines                                                 *
!************************************************************************************************************
module CSP_cooling_functions
contains

    real(8) function f_dh_evap(P)
        real(8),intent(in)::P
	    !Calculates enthalpy of evaporation given an atmospheric pressure
	    !Input: P -> Pressure [Pa]
	    !Output: Enthalpy [J/kg]
	    f_dh_evap = 2.36230E+06 - 1.35459*P + 0.00000308492*P**2
    end function

    real(8) function f_c_psat(P)
        real(8),intent(in)::P
	    !calculates the specific heat of water at the saturated liquid state as a real(8) function of pressure
	    !Input: P-> Pressure [Pa]
	    !Output: Specific heat [J/kg-K]
	    f_c_psat= 4170.46 + 0.000538088*P - 7.73437E-10*P**2
    end function

    real(8) function f_psat_T(T)
	    real(8),intent(in)::T
	    !Calculates the saturation pressure of steam given a certain temperature
	    !Input: T -> Temperature [C]
	    !Output: Pressure [Pa]
	    f_psat_T = 1125.09 - 19.6444*T + 4.42596*T**2 - 0.0391851*T**3 + 0.000965517*T**4
    end function

    real(8) function f_Tsat_p(P)
	    real(8),intent(in)::P
	    real(8)::Pg, err, T, Tg
	    integer::i
	    save:: Pg, Tg
	    !Calculates the saturation temperature of steam given a certain pressure. Applies to low pressures (condenser)
	    !Input: Pressure [Pa]
	    !Output: T -> Temperature [C]
	    if(P-Pg > 1.) Tg=25.
	    err = 999.; T=9999.; i=0
	    do !iterative loop to solve for Pg = P and return T. T cannot be expressed in terms of P.
	        i=i+1
	        Pg = 1125.09 - 19.6444*Tg + 4.42596*Tg**2 - 0.0391851*Tg**3 + 0.000965517*Tg**4
	        err = (P-Pg)/P
	        T = Tg
	        if((abs(err)<1.e-6).or.(i>30)) exit
	        Tg = T + err*25.
	    enddo
	    f_Tsat_p = T
    end function

    real(8) function f_hw_psat(P)
	    real(8),intent(in)::P
	    !Calculates the enthalpy of water at liquid saturation near ambient pressures
	    !Input: P -> Pressure [Pa]
	    !Output: enthalpy [J/kg]
	    f_hw_psat = 229628.719 + 2.78471579*P - 0.0000111907252*P**2 + 2.12030100E-11*P**3
    end function

    real(8) function f_s_hw_psat(P)
	    real(8),intent(in)::P
	    !Calculates the entropy of water at liquid saturation near ambient pressures
	    !Input: P -> Pressure [Pa]
	    !Output: entropy [J/kg-K]
	    f_s_hw_psat = 779.989872 + 0.00791597131*P - 3.33033640E-08*P**2 + 6.38602593E-14*P**3
    end function

    real(8) function f_rho_P(P)
	    real(8),intent(in)::P
	    !Calculates density of water at liquid saturation near ambient pressures
	    !Input: P -> Pressure [Pa]
	    !Output: density [kg/m3]
	    f_rho_P = 984.079732 - 0.000307058016*P + 5.32272340E-10*P**2
    end function

    real(8) function f_h_air_T(T)
	    real(8),intent(in)::T
	    !Calculates enthalpy of air as a real(8) function of temperature
	    !Input: T -> Temperature [C]
	    !Output: enthalpy [J/kg]
	    f_h_air_T=273474.659 + 1002.9404*T + 0.0326819988*T**2
    end function

    real(8) function T_sat4(P) !Isopentane 
        implicit none
        real(8)::P
        !P is in [bar], T is in [K]
        !Valid for pressures 1.0 < P < 25 bar. Critical point of Isopentane is T=460.4[K] (187.3[C]), P=33.7[bar]
        !This pressure range contains both the likely boiler pressure and condenser pressure for geothermal isopentane cycles
        T_sat4=284.482349 + 20.8848464*P - 1.5898147*P**2 + 0.0655241456*P**3 - 0.0010168822*P**4
    end function

    real(8) function P_sat4(T)  !Isopentane
        implicit none
        real(8)::T,Tk
        Tk = T+273.15d0
        !T is in [C], P is in [Pa]
        !Valid for temperature range from 300[K] to 440 [K]
        P_sat4 = (-99.7450105 + 1.02450484*Tk - 0.00360264243*Tk**2 + 0.00000435512698*Tk**3)*1.e5
    end function
end module


!-------------------------------------------------------------------------------------------

module CSP_HR_mod

contains

    !------------------------------------------------------------------------------------------------------------
    subroutine evap_tower(TT,P_cond_min, n_pl_inc, DeltaT_cw_des, T_approach, P_cycle, eta_ref, T_db, T_wb, P_amb, q_reject, m_dot_water, &
                          W_dot_tot, P_cond, T_cond, f_hrsys)

    !use cooling property functions
    use CSP_cooling_functions
    use water_properties
    implicit none

    real(8):: C_AIR, C_CW, DELTAH_EVAP, DELTAT_CW, DeltaT_cw_des, DP_EVAP, DRIFT_LOSS_FRAC, DT_OUT, eta_ref, ETA_FAN, ETA_FAN_S, &
              ETA_PCW_S, ETA_PUMP, H_FAN_IN, H_FAN_IN_CHK, H_FAN_OUT, H_FAN_OUT_S, H_PCW_IN, H_PCW_OUT, &
              H_PCW_OUT_S, M_DOT_AIR, M_DOT_BLOWDOWN, M_DOT_CW, m_dot_cw_des, M_DOT_DRIFT, blowdown_frac, &
              M_DOT_EVAP, M_DOT_WATER, MASS_RATIO_FAN, P_AMB, P_COND, P_CYCLE, P_RATIO_FAN, Q_REJECT, Q_reject_des, R, RHO_CW, &
              S_FAN_IN, S_PCW_IN, T_APPROACH, T_COND, T_DB, T_FAN_IN, T_FAN_IN_K, T_FAN_OUT, T_FAN_OUT_K, &
              T_WB, W_DOT_CW_PUMP, W_DOT_FAN, W_DOT_TOT, f_hrsys, p_cond_min, n_pl_inc, dh_low, dh_hi
    integer:: i, TT

    !------------------------------------------------------------------------------------------------------------
    !--Inputs
    !   * TT            [-]     Technology type - used to select regression equation set
    !   * P_cond_min    [Pa]    Minimum allowable condenser pressure
    !   * n_pl_inc      [-]     Number of part load heat rejection levels
    !   * DeltaT_cw_des [K]     Cooling water temperature rise across condenser
    !   * T_approach    [K]     Cooling tower approach temperature, difference between cw out and wet bulb temp
    !   * P_cycle       [W]     Rated power block capacity
    !   * eta_ref       [-]     Rated gross conversion efficiency
    !   * T_db          [K]     Dry bulb temperature (converted to C)
    !   * P_amb         [Pa]    Atmospheric pressure
    !------------------------------------------------------------------------------------------------------------
    !--Output
    !   * m_dot_water   [kg/s]  Total cooling tower water usage
    !   * W_dot_tot     [MW]    Total parasitic power for cooling tower model
    !   * P_cond        [Pa]    Condenser steam pressure
    !   * T_cond        [K]     Condenser steam temperature
    !   * f_hrsys       [-]     Fraction of the cooling system operating
    !------------------------------------------------------------------------------------------------------------
    !Unit conversions
    T_db = T_db - 273.15    ![C] Converted dry bulb temp
    T_wb = T_wb - 273.15    ![C] Converted wet bulb temp

    ! Values that can be estimated
    dt_out = 3.                 !Temperature difference at hot side of the condenser
    drift_loss_frac = 0.001     !Drift loss fraction
    blowdown_frac = 0.003       !Blowdown fraction
    dP_evap = 0.37*1.e5         ![Pa] Pressure drop across the condenser and cooling tower
    eta_pump = 0.75             !Total pump efficiency
    eta_pcw_s = 0.8             !Isentropic cooling water pump efficiency
    eta_fan = 0.75              !Fan mechanical efficiency
    eta_fan_s = 0.8             !Fan isentropic efficiency
    P_ratio_fan = 1.0025        !Fan pressure ratio
    mass_ratio_fan = 1.01       !Ratio of air flow to water flow in the cooling tower

    !Cooling water specific heat
    !c_cw = f_c_psat(P_amb)
    call water_TP(dmax1(T_wb, 10.d0), P_amb/100.d0, cp=c_cw); c_cw=c_cw*1000.d0 !Limit ambient temp to 10C to avoid freezing

    !**** Calculations for design conditions
    Q_reject_des = P_cycle*(1./eta_ref-1.)    	    !Heat rejection from the cycle
    m_dot_cw_des = Q_reject_des/(c_cw*DeltaT_cw_des)	!Mass flow rate of cooling water required to absorb the rejected heat
    f_hrsys = 1.d0   !Initial fraction of cooling system operating
    !**** 

    !**** Calculations for performance
    !Calculate the cooling water temp. rise associated with normal cooling system operation
    m_dot_cw = m_dot_cw_des
    DeltaT_cw = Q_reject/(m_dot_cw*c_cw)

    !Condenser saturation temperature
    T_cond = T_wb + DeltaT_cw + dt_out + T_approach

    !Condenser back pressure
    if(TT/=4) then !steam
        !P_cond = f_psat_T(T_cond)
        call water_TQ(T_cond, 1.d0, pres=P_cond); P_cond=P_cond*1000.d0
    else !isopentane
        P_cond = P_sat4(T_cond)
    endif

    !MJW 7.19.2010 :: Cooling system part-load strategy uses the number of part-load increments to determine how the coolign system is
    !partially shut down during under design operation. The condenser pressure is reduced with the cooling system running
    !at full load until it reaches the minimum condenser pressure. The cooling system then incrementally shuts off bays until
    !the condenser temperature/pressure rise above their minimum level. Default cond. pressure is 1.25 inHg (4233 Pa).
    if((P_cond < P_cond_min).and.(TT/=4))then !no lower limit on isopentane
        do i=2,int(n_pl_inc)
            f_hrsys = (1.-float(i-1)/n_pl_inc)
            m_dot_cw = m_dot_cw_des*f_hrsys
            DeltaT_cw = Q_reject/(m_dot_cw*c_cw)
            T_cond = T_wb + DeltaT_cw + dt_out + T_approach
            !P_cond = f_psat_T(T_cond)
            call water_TQ(T_cond, 1.d0, pres=P_cond); P_cond=P_cond*1000.d0
            if(P_cond > P_cond_min) goto 100
        enddo
        !Still below min. fix to min condenser pressure and recalc. temp.
        P_cond = P_cond_min
        !T_cond = f_Tsat_p(P_cond)
        call water_PQ(P_cond/1000.d0, 1.d0, temp=T_cond)
        DeltaT_cw = T_cond - (T_wb + dt_out + T_approach)
        m_dot_cw = q_reject/(DeltaT_cw*c_cw)    
    endif

    100 continue

    !Circulating water pump power
    !h_pcw_in = f_hw_psat(P_amb)     ![J/kg] cw pump inlet enthalpy 
    !s_pcw_in = f_s_hw_psat(P_amb)     ![J/kg-K] cw pump inlet entropy
    !rho_cw = f_rho_P(P_amb)         ![kg/m3] cooling water density in the pump
    call water_TP(T_cond-3.d0, P_amb/1000., enth=h_pcw_in, entr=s_pcw_in, dens=rho_cw)
    h_pcw_in = h_pcw_in*1000.; s_pcw_in = s_pcw_in*1000.
    
    h_pcw_out_s = dP_evap/rho_cw + h_pcw_in                         ![J/kg] isentropic outlet enthalpy.. incompressible fluid
    h_pcw_out = h_pcw_in + (h_pcw_out_s - h_pcw_in)/eta_pcw_s       ![J/kg] Outlet enthalpy accounting for irreversibility
    W_dot_cw_pump = (h_pcw_out - h_pcw_in)*m_dot_cw/eta_pump*1.e-6  ![MW] Cooling water circulating pump power

    !Fan power
    m_dot_air = m_dot_cw*mass_ratio_fan
    T_fan_in = (T_db + T_wb + T_approach)/2
    h_fan_in = f_h_air_T(T_fan_in)

    c_air = 1003.               ![J/kg-K] specific heat of air (This is relatively constant)
    R = 8314./28.97             ![J/kmol-K]/[kg/kmol] Gas constant over the molar mass of air

    T_fan_in_K = T_fan_in + 273.15  !Fan inlet temp, in K
    T_fan_out_K = T_fan_in_K*P_ratio_fan**(R/C_air)    ![K] isentropic temperature rise
    T_fan_out = T_fan_out_K - 273.15    ![C] Convert isentropic temperature rise to deg C
    h_fan_out_s = f_h_air_T(T_fan_out)  ![J/kg] Calculate isentropic enthalpy at fan outlet
    h_fan_out = h_fan_in + (h_fan_out_s - h_fan_in)/eta_fan_s   ![J/kg] Actual enthalpy, accounting for irreversibility

    W_dot_fan = (h_fan_out - h_fan_in)*m_dot_air/eta_fan*1.e-6  ![MW] Fan parasitic power

    !Total cooling tower parasitic power
    W_dot_tot = W_dot_cw_pump + W_dot_fan   ![MW] 

    !Enthalpy of evaporation
    !DELTAh_evap = f_dh_evap(P_amb)
    call water_PQ(P_amb/1000., 0.d0, enth=dh_low)
    call water_PQ(P_amb/1000., 1.d0, enth=dh_hi)
    DELTAh_evap = (dh_hi-dh_low)*1000.

    !Evaporative water loss
    m_dot_evap = Q_reject/Deltah_evap

    !Other water losses
    m_dot_drift = drift_loss_frac*m_dot_cw	!Drift loss fraction, based on cooling water mass flow rate
    m_dot_blowdown = blowdown_frac*m_dot_cw	!Blow down fraction

    !Total power block water usage
    m_dot_water = m_dot_evap + m_dot_drift + m_dot_blowdown

    !Unit conversions
    T_db = T_db + 273.15    ![C] Converted dry bulb temp
    T_wb = T_wb + 273.15    ![C] Converted wet bulb temp
    T_cond = T_cond + 273.15    ![K] Convert to K for output

    end subroutine


    !************************************************************************************************************
    !************************************************************************************************************


    subroutine ACC(TT,P_cond_min, n_pl_inc, T_ITD_des, P_cond_ratio, P_cycle, eta_ref, T_db, P_amb, q_reject, m_dot_air, W_dot_fan, P_cond, T_cond, f_hrsys)

    !use cooling property functions
    use CSP_cooling_functions

    implicit none

    real(8)::dt_fan, eta_ref, eta_fan, eta_fan_s, h_fan_in, h_fan_out, h_fan_out_s, m_dot_air, &
             mm, p_amb, p_cond, p_cond_ratio, p_cycle, q_reject, r, rh, t_cond, t_db, t_fan_in_k, t_fan_out, t_fan_out_k,&
             t_hot_diff, t_itd, w_dot_fan, t_itd_des, c_air, q_reject_des, m_dot_air_des, P_cond_min, n_pl_inc, dT_air, f_hrsys
    integer:: i,TT

    !------------------------------------------------------------------------------------------------------------
    !--Inputs
    !   * TT            [-]     Technology type
    !   * P_cond_min    [Pa]    Minimum allowable condenser pressure
    !   * n_pl_inc      [-]     Number of part load heat rejection levels
    !   * T_ITD         [K]     ACC initial temperature difference, difference between dry bulb and steam inlet temp
    !   * P_cond_ratio  [-]     Condenser air inlet/outlet pressure ratio
    !   * P_cycle       [W]     Rated power block capacity
    !   * eta_ref       [-]     Rated gross conversion efficiency
    !   * T_db          [K]     Dry bulb temperature (converted to C)
    !   * P_amb         [Pa]    Atmospheric pressure
    !------------------------------------------------------------------------------------------------------------
    !--Output
    !   * m_dot_air     [kg/s]  Total ACC air mass flow rate
    !   * W_dot_fan     [MW]    Total parasitic power for ACC model
    !   * P_cond        [Pa]    Condenser steam pressure
    !   * T_cond        [K]     Condenser steam temperature
    !------------------------------------------------------------------------------------------------------------
    !Unit conversions
    T_db = T_db - 273.15        ![C] Converted dry bulb temp

    ! Values that can be estimated
    T_hot_diff = 3.             ![C] Temperature difference between saturation steam and condenser outlet air temp
    eta_fan_s = 0.8             ![-] Fan isentropic efficiency
    eta_fan = .98**3            ![-] Fan mechanical efficiency
    c_air = 1005.               ![J/kg-K] Specific heat of air, relatively constant over dry bulb range

    !**** Calculations for design conditions
    Q_reject_des = P_cycle*(1./eta_ref-1.)    	    !Heat rejection from the cycle
    m_dot_air_des = Q_reject_des/(c_air*(T_ITD_des - T_hot_diff))	
    f_hrsys = 1.
    !**** 

    !Fan power
    dT_air = q_reject/(m_dot_air_des*c_air)
    T_ITD = T_hot_diff + dT_air  ![C] Calculate the actual ITD during off-design operation

    ! Calculated output
    T_cond = T_db + T_ITD	    !Condensation temperature
    if(TT/=4) then !steam
        P_cond =  f_psat_T(T_cond)	!Turbine back pressure
    else !isopentane
        P_cond = P_sat4(T_cond)
    endif

    !MJW 7.19.2010 :: Cooling system part-load strategy uses the number of part-load increments to determine how the coolign system is
    !partially shut down during under design operation. The condenser pressure is reduced with the cooling system running
    !at full load until it reaches the minimum condenser pressure. The cooling system then incrementally shuts off bays until
    !the condenser temperature/pressure rise above their minimum level. Default cond. pressure is 2.0 inHg (6772 Pa).
    if((P_cond < P_cond_min).and.(TT/=4))then !no lower limit on isopentane
        do i=2,int(n_pl_inc)
            f_hrsys = (1.-float(i-1)/n_pl_inc)
            m_dot_air = m_dot_air_des*f_hrsys
            dT_air = q_reject/(m_dot_air*c_air)
            T_cond = T_db + T_hot_diff + dT_air
            P_cond = f_psat_T(T_cond)
            if(P_cond > P_cond_min) goto 100
        enddo
        !Still below min. fix to min condenser pressure and recalc. temp.
        P_cond = P_cond_min
        T_cond = f_Tsat_p(P_cond)
        dT_air = T_cond - (T_db + T_hot_diff)
        m_dot_air = q_reject/(dT_air*c_air)    
    endif

    100 continue
    h_fan_in = f_h_air_T(T_db)  ![J/kg] Fan inlet enthalpy

    mm = 28.97                  ![kg/kmol] molar mass of air
    R = 8314./mm                ![J/kg-K] Gas constant for air

    !These temperature calculations are for the isentropic expansion across the fan, not accounting for heat gain in the ACC
    T_fan_in_K = T_db + 273.15  ![K] Fan inlet temperature
    T_fan_out_K = T_fan_in_K*P_cond_ratio**(R/C_air)
    T_fan_out = T_fan_out_K - 273.15    ![C] Fan outlet temperature
    dT_fan = T_fan_out - T_db   ![C] Difference in temperature including irreversibilities in fan

    h_fan_out_s = f_h_air_T(T_fan_out)	![J/kg] Isentropic fan outlet temperature
    h_fan_out = h_fan_in + (h_fan_out_s - h_fan_in)/eta_fan_s   ![J/kg] Actual fan outlet temperature
    !Total ACC parasitic power
    W_dot_fan = (h_fan_out - h_fan_in)*m_dot_air/eta_fan*1.e-6  ![MW] Fan power

    !Unit conversions
    T_db = T_db + 273.15    ![C] Converted dry bulb temp
    T_cond = T_cond + 273.15    ![K] Convert to K for output

    end subroutine


    !************************************************************************************************************
    !************************************************************************************************************


    subroutine HybridHR(TT, fcall, P_cond_min, n_pl_inc, time, F_wc, F_wcmax, F_wcmin, T_ITD_des, T_approach, dT_cw_ref, P_cond_ratio, P_cycle, eta_ref, T_db, T_wb, &
                        P_amb, q_reject, m_dot_water, W_dot_acfan, W_dot_wctot, W_dot_tot, P_cond, T_cond, f_hrsys)

    !------------------------------------------------------------------------------------------------------------
    !This subroutine models a hybrid wet/dry cooling heat rejection system. In this system, a dry-cooled condenser
    !is responsible for rejecting the thermal load, except a supplemental wet-cooled system is placed in parallel
    !to aid in heat rejection during the hottest hours of the day. The wet cooled system can reject heat based
    !on the wetbulb temperature, and thus will have much lower parasitics in rejecting a fraction of the heat than
    !the dry cooled system will, and the dry cooled system running at normal power will result in a lower 
    !condenser temperature and pressure.
    !
    !Several assumptions are made in the control of this system. The user can specify a cooling distribution factor
    !on the thermal storage page with the other TOU factors. The fraction indicates what the distribution of 
    !the heat rejection load will be. If the fraction is 0.2 for example, then the wet cooling tower will reject
    !20% of the load. 
    !
    !The wet-cooling system is a forced-draft tower, and is sized based on the largest TOU fraction supplied in the 
    !control array. 
    !
    !--Inputs----------------------------------------------------------------------------------------------------
    !   * TT            [-]     Technology type - used to select regression equation set
    !   * P_cond_min    [Pa]    Minimum allowable condenser pressure
    !   * n_pl_inc      [-]     Number of part load heat rejection levels
    !   * time          [-]     hour of the year
    !   * F_wc          [-]     Wet cooling fraction
    !   * F_wcmax       [-]     Maximum annual wet cooling fraction
    !   * F_wcmin       [-]     Minimum annual wet cooling fraction
    !   * T_ITD_des     [K]     ACC initial temperature difference, difference between dry bulb and steam inlet temp
    !   * T_approach    [K]     Wet cooling tower approach temperature, difference between cw out and wet bulb temp
    !   * P_cond_ratio  [-]     Condenser air inlet/outlet pressure ratio
    !   * P_cycle       [W]     Rated power block capacity
    !   * eta_ref       [-]     Rated gross conversion efficiency
    !   * T_db          [K]     Dry bulb temperature (converted to C)
    !   * T_wb          [K]     Wet bulb temperature (converted to C)
    !   * P_amb         [Pa]    Atmospheric pressure
    !   * q_reject      [W]     Total required heat rejection load
    !------------------------------------------------------------------------------------------------------------
    !--Output
    !   * m_dot_water   [kg/s]  Total cooling tower water usage
    !   * W_dot_acfan   [MW]    Total parasitic power for ACC fan
    !   * W_dot_wctot   [MW]    Total parasitic power for cooling tower 
    !   * W_dot_tot     [MW]    Total overall parasitic power
    !   * P_cond        [Pa]    Condenser steam pressure
    !   * T_cond        [K]     Condenser steam temperature
    !------------------------------------------------------------------------------------------------------------

    !use cooling property functions
    use CSP_cooling_functions
    use water_properties
    
    implicit none

    real(8)::F_wc, F_wcmax, F_wcmin, dt_acfan, eta_ref, p_amb, p_cond, p_cond_ratio, p_cycle, &
             q_reject, r, rh, t_cond, t_db, t_hot_diff, t_itd, w_dot_wctot, t_itd_des, c_air, q_reject_des, &
             eta_acfan_s, eta_acfan, m_dot_acair_des, m_dot_acair, h_acfan_in, T_acfan_in_K, T_acfan_out_K, T_acfan_out, &
             h_acfan_out_s,  drift_loss_frac, blowdown_frac, dp_evap, eta_pump, etap_pcw_s, eta_wcfan_s, p_ratio_wcfan, &
             mass_ratio_wcfan, c_cw, q_ac_des, q_wc_des, m_dot_cw_des, q_ac_rej, q_wc_rej, deltat_cw, t_wb, t_approach, &
             w_dot_acfan, h_pcw_in, s_pcw_in, rho_cw, h_pcw_out_s, h_pcw_out, w_dot_cw_pump, m_dot_wcair, t_wcfan_in, &
             eta_pcw_s, eta_wcfan, m_dot_cw, h_wcfan_in, T_wcfan_in_K, T_wcfan_out, t_wcfan_out_k,h_wcfan_out_s, h_wcfan_out, &
             W_dot_wcfan, deltah_evap, m_dot_evap, m_dot_drift, m_dot_blowdown, m_dot_water, h_acfan_out, W_dot_tot, time, dT_cw_ref,&
             P_cond_min, n_pl_inc, f_hrsys, f_hrsysair, f_hrsyswc, T_condwc, T_condair, dT_air, fcall, dh_low, dh_hi
    integer:: i,j, TT
    save::T_hot_diff, eta_acfan_s, eta_acfan, c_air, drift_loss_frac, blowdown_frac, dP_evap, eta_pump, eta_pcw_s, eta_wcfan, &
          eta_wcfan_s, P_ratio_wcfan, mass_ratio_wcfan, Q_reject_des, q_ac_des, m_dot_acair_des, q_wc_des, c_cw, m_dot_cw_des
    !Only call the parameter equations at the beginning of the simulation. Once they're established, they don't need to 
    !be reset each time.
    if(fcall==1.d0) then
        ! Values that can be estimated--------
        !-dry
        T_hot_diff = 3.d0           ![C] Temperature difference between saturation steam and condenser outlet air temp
        eta_acfan_s = 0.8d0         ![-] Fan isentropic efficiency
        eta_acfan = .98d0**3        ![-] Fan mechanical efficiency
        c_air = 1005.               ![J/kg-K] specific heat of air (This is relatively constant)
        R = 286.986538              ![J/kg-K] Gas constant for air = 8314./28.97

        !-wet
        drift_loss_frac = 0.001d0   !Drift loss fraction
        blowdown_frac = 0.003d0     !Blowdown fraction
        dP_evap = 0.37*1.e5         ![Pa] Pressure drop across the condenser and cooling tower
        eta_pump = 0.75d0           !Total pump efficiency
        eta_pcw_s = 0.8d0           !Isentropic cooling water pump efficiency
        eta_wcfan = 0.75d0          !Fan mechanical efficiency
        eta_wcfan_s = 0.8d0         !Fan isentropic efficiency
        P_ratio_wcfan = 1.0025d0    !Fan pressure ratio
        mass_ratio_wcfan = 1.01d0   !Ratio of air flow to water flow in the cooling tower
        
    endif  !TFF - I think the equations below need to be recalculated each time, mainly due to c_cw and m_dot_cw_des

    !**** Calculations for design conditions
    Q_reject_des = P_cycle*(1./eta_ref-1.)    	    !Heat rejection from the cycle
    !-dry
    q_ac_des = Q_reject_des*(1.-F_wcmin)    !Size the ACC to always be able to handle the load that isn't going to the wet cooler
    m_dot_acair_des = q_ac_des/(c_air*(T_ITD_des - T_hot_diff))	
    !-wet
    q_wc_des = Q_reject_des*F_wcmax         !Size the wet cooler to handle the maximum fraction in the control array
    !c_cw = f_c_psat(P_amb)      !Cooling water specific heat
    !**** 

    !Unit conversions
    T_db = T_db - 273.15        ![C] Converted dry bulb temp
    T_wb = T_wb - 273.15
    !c_cw = f_c_psat(P_amb)      !Cooling water specific heat
    call water_TP(dmax1(T_wb, 10.d0), P_amb/100.d0, cp=c_cw); c_cw=c_cw*1000.d0
    m_dot_cw_des = q_wc_des/(c_cw*dT_cw_ref) !Mass flow rate of cooling water required to absorb the rejected heat

    !Calculate the cooling loads
    q_ac_rej = q_reject*(1.-F_wc)
    q_wc_rej = q_reject*F_wc
    f_hrsyswc = 1.
    f_hrsysair = 1.

    !-ACC
    dT_air = q_ac_rej/(m_dot_acair_des*c_air)
    T_ITD = T_hot_diff + dT_air  ![C] Calculate the actual ITD during off-design operation
    !-WC
    DeltaT_cw = q_wc_rej/(m_dot_cw_des*c_cw)

    !***Calculated output
    !Condensation temperature is the maximum of either the wet or dry system cooling stream outlet temperature (plus hot side dT)
    T_condwc = T_wb + DeltaT_cw + T_hot_diff + T_approach
    T_condair = T_db + T_ITD
    if(F_wc>0.) then !MJW 7.23.2010
        T_cond = dmax1(T_condwc, T_condair)     
    else
        T_cond = T_condair
    endif
    if(TT/=4) then !steam
        !P_cond = f_psat_T(T_cond)
        call water_TQ(T_cond, 1.d0, pres=P_cond); P_cond=P_cond*1000.d0
    else !isopentane
        P_cond = P_sat4(T_cond)
    endif

    !MJW 7.19.2010 :: Cooling system part-load strategy uses the number of part-load increments to determine how the coolign system is
    !partially shut down during under design operation. The condenser pressure is reduced with the cooling system running
    !at full load until it reaches the minimum condenser pressure. The cooling system then incrementally shuts off bays until
    !the condenser temperature/pressure rise above their minimum level. Default cond. pressure is 2.0 inHg (6772 Pa).
    i=1; j=1
    if((P_cond < P_cond_min).and.(TT/=4))then !no lower limit on isopentane
        do 
            if(T_condwc > T_condair) then
                i=i+1
                !Reduce just wet cooled
                f_hrsyswc = (1.-float(i-1)/n_pl_inc)
                m_dot_cw = m_dot_cw_des*f_hrsyswc
                DeltaT_cw = q_wc_rej/(m_dot_cw*c_cw)
                T_condwc = T_wb + DeltaT_cw + T_hot_diff + T_approach
            else
                i=i+1
                j=j+1
                !Reduce both wet and dry cooled
                f_hrsysair = (1.-float(j-1)/n_pl_inc)
                m_dot_acair = m_dot_acair_des*f_hrsysair
                dT_air = q_ac_rej/(m_dot_acair*c_air)
                T_condair = T_db + dT_air + T_hot_diff
                !--
                f_hrsyswc = (1.-float(i-1)/n_pl_inc)
                m_dot_cw = m_dot_cw_des*f_hrsyswc
                DeltaT_cw = q_wc_rej/(m_dot_cw*c_cw)
                T_condwc = T_wb + DeltaT_cw + T_hot_diff + T_approach
            endif
            
            if(F_wc>0.) then !MJW 7.23.2010
                T_cond = dmax1(T_condwc, T_condair)     
            else
                T_cond = T_condair
            endif
            !P_cond = f_psat_T(T_cond)
            call water_TQ(T_cond, 1.d0, pres=P_cond); P_cond=P_cond*1000.d0
            if(P_cond > P_cond_min) goto 100
            if((i>=int(n_pl_inc)).or.(j>=int(n_pl_inc))) exit
        enddo
           

        !Still below min. fix to min condenser pressure and recalc. temp.
        P_cond = P_cond_min
        !T_cond = f_Tsat_p(P_cond)
        call water_PQ(P_cond/1000.d0, 1.d0, temp=T_cond)
        if(T_condwc > T_condair) then
            DeltaT_cw = T_cond - (T_wb + T_hot_diff + T_approach)
            m_dot_cw = q_reject/(DeltaT_cw*c_cw)
        else
            dT_air = T_cond - (T_db + T_hot_diff)
            m_dot_acair = q_reject/(dT_air*c_air)    
        endif
    endif

    100 f_hrsys = (f_hrsyswc + f_hrsysair)/2

    !-----ACC Fan power---------
    h_acfan_in = f_h_air_T(T_db)  ![J/kg] Fan inlet enthalpy

    !These temperature calculations are for the isentropic expansion across the fan, not accounting for heat gain in the ACC
    T_acfan_in_K = T_db + 273.15  ![K] Fan inlet temperature
    T_acfan_out_K = T_acfan_in_K*P_cond_ratio**(R/C_air)
    T_acfan_out = T_acfan_out_K - 273.15    ![C] Fan outlet temperature
    dT_acfan = T_acfan_out - T_db   ![C] Difference in temperature including irreversibilities in fan

    h_acfan_out_s = f_h_air_T(T_acfan_out)	![J/kg] Isentropic fan outlet temperature
    h_acfan_out = h_acfan_in + (h_acfan_out_s - h_acfan_in)/eta_acfan_s   ![J/kg] Actual fan outlet temperature
    !Total ACC parasitic power
    W_dot_acfan = (h_acfan_out - h_acfan_in)*m_dot_acair/eta_acfan*1.e-6  ![MW] Fan power


    !-----Wet cooling parasitics --------
    if(q_wc_rej > .001d0) then
        !Circulating water pump power
        !h_pcw_in = f_hw_psat(P_amb)     ![J/kg] cw pump inlet enthalpy 
        !s_pcw_in = f_s_hw_psat(P_amb)     ![J/kg-K] cw pump inlet entropy
        !rho_cw = f_rho_P(P_amb)         ![kg/m3] cooling water density in the pump
        call water_TP(T_cond-3.d0, P_amb/1000., enth=h_pcw_in, entr=s_pcw_in, dens=rho_cw)
        h_pcw_out_s = dP_evap/rho_cw + h_pcw_in                         ![J/kg] isentropic outlet enthalpy.. incompressible fluid
        h_pcw_out = h_pcw_in + (h_pcw_out_s - h_pcw_in)/eta_pcw_s       ![J/kg] Outlet enthalpy accounting for irreversibility
        W_dot_cw_pump = (h_pcw_out - h_pcw_in)*m_dot_cw/eta_pump*1.e-6  ![MW] Cooling water circulating pump power

        !Fan power
        m_dot_wcair = m_dot_cw*mass_ratio_wcfan
        T_wcfan_in = (T_db + T_wb + T_approach)/2.
        h_wcfan_in = f_h_air_T(T_wcfan_in)

        T_wcfan_in_K = T_wcfan_in + 273.15  !Fan inlet temp, in K
        T_wcfan_out_K = T_wcfan_in_K*P_ratio_wcfan**(R/C_air)    ![K] isentropic temperature rise
        T_wcfan_out = T_wcfan_out_K - 273.15    ![C] Convert isentropic temperature rise to deg C
        h_wcfan_out_s = f_h_air_T(T_wcfan_out)  ![J/kg] Calculate isentropic enthalpy at fan outlet
        h_wcfan_out = h_wcfan_in + (h_wcfan_out_s - h_wcfan_in)/eta_wcfan_s   ![J/kg] Actual enthalpy, accounting for irreversibility

        W_dot_wcfan = (h_wcfan_out - h_wcfan_in)*m_dot_wcair/eta_wcfan*1.e-6  ![MW] Fan parasitic power

        !Total wet cooling tower parasitic power
        W_dot_wctot = W_dot_cw_pump + W_dot_wcfan   ![MW] 

        !Enthalpy of evaporation
        !DELTAh_evap = f_dh_evap(P_amb)
        call water_PQ(P_amb/1000., 0.d0, enth=dh_low)
        call water_PQ(P_amb/1000., 1.d0, enth=dh_hi)
        DELTAh_evap = (dh_hi-dh_low)*1000.

        !Evaporative water loss
        m_dot_evap = q_wc_rej/Deltah_evap

        !Other water losses
        m_dot_drift = drift_loss_frac*m_dot_cw	!Drift loss fraction, based on cooling water mass flow rate
        m_dot_blowdown = blowdown_frac*m_dot_cw	!Blow down fraction

        !Total power block water usage
        m_dot_water = m_dot_evap + m_dot_drift + m_dot_blowdown
    else
        !Otherwise set the wet-cooling outputs to zero
        m_dot_water = 0.d0
        W_dot_wctot = 0.d0
    endif
        
    W_dot_tot = W_dot_wctot + W_dot_acfan

    !Unit conversions
    T_db = T_db + 273.15    ![C] Converted dry bulb temp
    T_wb = T_wb + 273.15    ![C] Converted wet bulb temp
    T_cond = T_cond + 273.15    ![K] Convert to K for output


    end subroutine


end module
