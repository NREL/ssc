! ~~~~~~~~~~~~~~~~~ BETA VERSION 0.98 ~~~~~~~~~~~~~~~~~

! This is a beta version - use at your own risk.

! This module contains a number of procedures that return water properties calculated using bicubic interpolation or using REFPROP (not included).
! The general format of the subroutine calls is water_XY, where X and Y correspond to the known independent properties.
! For calling REFPROP, the syntax is water_RP, with the first input being a code specifying which independent properties are specified.
! Note: REFPROP is not supplied with this code and must be purchased from NIST (http://www.nist.gov/srd/nist23.cfm) - the source code for REFPROP must be linked during
! compilation; if REFPROP is not required the water_RP subroutine should be commented out or a compiler error will likely occur.

! Author: John Dyreby [Northland Numerics LLC] <john@nnumerics.com>

module water_properties

USE ISO_C_BINDING, only: C_INT, C_DOUBLE, C_FLOAT

implicit none   ! this applies to all functions and subroutines contained in this module
private         ! public subroutines and variables must be explicitly listed on the line below
public :: water_TQ_f90, water_PQ_f90, water_TP_f90, water_PH_f90, water_PS_f90

! Saturation
! The saturation include file contains the following variables (the fluid name is prefixed to each of the variables):
!   sat_vector_size [integer]
!   sat_array_size [integer]
!   min_sat_temp [DP]
!   max_sat_temp [DP]
!   min_sat_pres [DP]
!   max_sat_pres [DP]
!   sat_temp_vector(sat_vector_size) [DP]
!   sat_pres_vector(sat_vector_size) [DP]
!   sat_temp_coef_array(4,sat_array_size) [SP]
!     - first index is the four coefficients, second is the pressure index
!   sat_pres_coef_array(4,sat_array_size) [SP]
!     - first index is the four coefficients, second is the temperature index
!   sat_vhs_coef_array(3,0:1,4,sat_array_size) [SP]
!     - first index is the property code, second is the saturation state (liq/vap), third is the four coefficients, fourth is the temperature index
!     - 1=volume, 2=enthalpy, 3=entropy
!   sat_prop_coef_array(4,5,0:1,sat_array_size) [SP]
!     - first index is the four coefficients, second is the property code, third is the saturation state (liq/vap), fourth is the temperature index
!     - 1=cv, 2=cp, 3=ssnd, 4=cond, 5=visc
!   sat_deriv_coef_array(4,8,0:1,sat_array_size) [SP]
!     - first index is the four coefficients, second is the property code, third is the saturation state (liq/vap), fourth is the temperature index
!     - 1=dt/ds, 2=dd/ds, 3=dh/ds, 4=dcv/ds, 5=dcp/ds, 6=dssnd/ds, 7=dcond/ds, 8=dvisc/ds (all at constant pressure)
include 'water_sat_data.fdat'

! Subcritical Region
! The subcritical include file contains the following variables (the fluid name is prefixed to each of the variables):
!   pres_vector_size [integer]
!   liquid_entr_vector_size [integer]
!   vapor_entr_vector_size [integer]
!   liquid_array_size [integer]
!   vapor_array_size [integer]
!   sat_array_size [integer]
!   min_temp [DP]
!   max_temp [DP]
!   pres_vector(pres_vector_size) [DP]
!   vapor_entr_index_vector(pres_vector_size) [integer]
!   liquid_entr_index_vector(pres_vector_size) [integer]
!   liquid_entr_values(liquid_entr_vector_size) [DP]
!   vapor_entr_values(vapor_entr_vector_size) [DP]
!   liquid_coef_array(4,4,8,liquid_array_size) [SP]
!   vapor_coef_array(4,4,8,vapor_array_size) [SP]
!       The third index in the coefficient array corresponds to the property, using the following codes:
!       1 = temperature in C                                                                                                                                                                                                                                               
!       2 = density in kg/m3                                                                                                                                                                                                                                               
!       3 = enthalpy in kJ/kg                                                                                                                                                                                                                                              
!       4 = specific heat at constant volume in kJ/kg-K                                                                                                                                                                                                                    
!       5 = specific heat at constant pressure in kJ/kg-K      
!       6 = speed of sound in fluid in m/s                                                                                                                                                                                                            
!       7 = thermal conductivity in W/m-K                                                                                                                                                                                                                                  
!       8 = viscosity in Pa-s                                                                                                                                                                                                                     
include 'water_subcritical_data.fdat'


contains

subroutine water_PS_f90(pres,entr,error_code,temp,dens,vol,inte,enth,cv,cp,cond,visc,qual,ssnd) bind(C)
    ! Subroutine used to determine water properties from bicubic interpolation tables, given pressure and entropy.  Inputs can be an array if outputs are of a conforming shape and size.
    ! Note: this subroutine does not write to water_error_code or water_error_message.
    ! Required Inputs:
    !   pres = pressure (kPa)
    !   entr = entropy (kJ/kg-K)
    ! Required Outputs:
    !   error_code = an integer error code that can be used with the get_error_message subroutine
    ! Optional Outputs:
    !   temp = temperature (C)
    !   dens = density (kg/m3)
    !   vol = specific volume (m3/kg)
    !   inte = internal energy (kJ/kg)
    !   enth = enthalpy (kJ/kg)
    !   cv = specific heat at constant volume (kJ/kg-K)
    !   cp = specific heat at constant pressure (kJ/kg-K)
    !   cond = thermal conductivity (W/m-K)
    !   visc = viscosity (Pa-s)
    !   qual = quality on a mass basis:
    !           q < 0 indicating subcooled liquid
    !           q = 0 indicating saturated liquid
    !           q = 1 indicating saturated vapor
    !           q > 1 indicating superheated vapor
    !           q >= 999 indicating a supercritical state with temperature and pressure greater than the critical point
    !   ssnd = speed of sound in the fluid (m/s)       

    ! Required Argument Declarations
    real(C_DOUBLE), intent(in) :: pres, entr
    integer(C_INT), intent(out) :: error_code
      
    ! Optional Argument Declarations
    real(C_DOUBLE), intent(out) :: temp, dens, vol, inte, enth, cv, cp, cond, visc, qual, ssnd
    
    ! Internal Variable Declarations
    integer(C_INT) :: pres_index, entr_index, row_entr_index, coef_index, sat_temp_index, first_index, first_coef_index, last_coef_index, last_index
    real(C_DOUBLE) :: pres_fraction, entr_fraction, sat_temp, temp_diff, quality
    real(C_DOUBLE), dimension(0:1) :: sat_entr
    real(C_DOUBLE), dimension(2,0:1) :: sat_vhs_values
    real(C_DOUBLE), dimension(2) :: vhs_values
    real(C_DOUBLE), dimension(5) :: prop_values
    
    ! Initialize Error Flag
    error_code = 0
    
    ! Check Pressure Input
    if (pres > water_max_sat_pres .or. pres < water_min_sat_pres) then
        error_code = 4
        return
    end if
    
    ! Use the provided pressure to determine whether the state could be saturated or if it is above the vapor dome.
    if (pres <= water_max_sat_pres) then  ! could be vapor, two-phase, or liquid
        
        ! Determine the saturated liquid and vapor entropy at the given pressure.
        sat_temp = t_sat(pres)
        call sat_find(sat_temp,sat_temp_index,temp_diff)
        sat_entr = ((water_sat_vhs_coef_array(3,:,1,sat_temp_index)*temp_diff+water_sat_vhs_coef_array(3,:,2,sat_temp_index))*temp_diff+water_sat_vhs_coef_array(3,:,3,sat_temp_index))*temp_diff+water_sat_vhs_coef_array(3,:,4,sat_temp_index)
    
        ! Determine the state of the provided pressure and entropy.
        if (entr > sat_entr(1)) then
            
            ! -----VAPOR-----
            
            ! Determine the pres_index row that contains the provided pressure and its fractional position, as well as the index of the first entropy value in that row.
            call pres_find(pres,pres_index,pres_fraction)
            first_index = water_vapor_entr_index_vector(pres_index)
            
            ! Check if the entropy lies in the gap between the vapor dome and the left-most element or if it is fully in the 2D grid.
            if (entr < water_vapor_entr_values(first_index)) then
                ! The given state is in the gap between the vapor dome and the 2D grid.  Calculate its fractional position within the gap and call the appropriate vapor_gap subroutines.
                entr_fraction = (entr-sat_entr(1))/(water_vapor_entr_values(first_index)-sat_entr(1))
                first_coef_index = water_vapor_entr_index_vector(pres_index)-pres_index+1  ! the index corresponding to the first element in the pressure row
                temp = vapor_gap_temp(entr_fraction,pres_fraction,sat_temp,first_coef_index)
                dens = vapor_gap_dens(entr_fraction,pres_fraction,temp_diff,sat_temp_index,first_coef_index)
                vol = 1.0/dens
                enth = vapor_gap_enth(entr_fraction,pres_fraction,temp_diff,sat_temp_index,first_coef_index)
                inte = enth-pres/dens
                cv = vapor_gap_cv(entr_fraction,pres_fraction,temp_diff,sat_temp_index,first_coef_index)
                cp = vapor_gap_cp(entr_fraction,pres_fraction,temp_diff,sat_temp_index,first_coef_index)
                ssnd = vapor_gap_ssnd(entr_fraction,pres_fraction,temp_diff,sat_temp_index,first_coef_index)
                cond = vapor_gap_cond(entr_fraction,pres_fraction,temp_diff,sat_temp_index,first_coef_index)
                visc = vapor_gap_visc(entr_fraction,pres_fraction,temp_diff,sat_temp_index,first_coef_index)
                qual = 10.0
            else
                ! The given state is fully in the vapor region.  Determine the entropy index and check to make sure it is not too large for the given pressure.
                last_index = water_vapor_entr_index_vector(pres_index+1)-1
                row_entr_index = maxloc(water_vapor_entr_values(first_index:last_index), 1, mask=water_vapor_entr_values(first_index:last_index) <= entr)  ! locate the largest value that is smaller than or equal to the given entropy
                entr_index = first_index+row_entr_index-1
                if (entr_index >= last_index) then
                    if (entr == water_vapor_entr_values(last_index)) then
                        ! In the unlikely event that the value at exactly the right side of the last element is specified, adjust the index down one.
                        row_entr_index = row_entr_index-1
                        entr_index = entr_index-1
                    else
                        error_code = 5
                        return
                    end if
                end if
                
                ! Calculate the entropy fraction and coefficient index for this element.
                entr_fraction = (entr-water_vapor_entr_values(entr_index))/(water_vapor_entr_values(entr_index+1)-water_vapor_entr_values(entr_index))
                coef_index = first_index-pres_index+row_entr_index
                
                ! The coefficients for the element containing the point and the fractional position of the point can now be used to calculate dependent properties.  Nested multiplication is used and the equations are explicitly written to make the compiled code faster.
                temp = (((((water_vapor_coef_array(4,4,1,coef_index)*pres_fraction+water_vapor_coef_array(3,4,1,coef_index))*pres_fraction+water_vapor_coef_array(2,4,1,coef_index))*pres_fraction+water_vapor_coef_array(1,4,1,coef_index)*entr_fraction)+((water_vapor_coef_array(4,3,1,coef_index)*pres_fraction+water_vapor_coef_array(3,3,1,coef_index))*pres_fraction+water_vapor_coef_array(2,3,1,coef_index))*pres_fraction+water_vapor_coef_array(1,3,1,coef_index))*entr_fraction+((water_vapor_coef_array(4,2,1,coef_index)*pres_fraction+water_vapor_coef_array(3,2,1,coef_index))*pres_fraction+water_vapor_coef_array(2,2,1,coef_index))*pres_fraction+water_vapor_coef_array(1,2,1,coef_index))*entr_fraction+((water_vapor_coef_array(4,1,1,coef_index)*pres_fraction+water_vapor_coef_array(3,1,1,coef_index))*pres_fraction+water_vapor_coef_array(2,1,1,coef_index))*pres_fraction+water_vapor_coef_array(1,1,1,coef_index)              
                dens = (((((water_vapor_coef_array(4,4,2,coef_index)*pres_fraction+water_vapor_coef_array(3,4,2,coef_index))*pres_fraction+water_vapor_coef_array(2,4,2,coef_index))*pres_fraction+water_vapor_coef_array(1,4,2,coef_index)*entr_fraction)+((water_vapor_coef_array(4,3,2,coef_index)*pres_fraction+water_vapor_coef_array(3,3,2,coef_index))*pres_fraction+water_vapor_coef_array(2,3,2,coef_index))*pres_fraction+water_vapor_coef_array(1,3,2,coef_index))*entr_fraction+((water_vapor_coef_array(4,2,2,coef_index)*pres_fraction+water_vapor_coef_array(3,2,2,coef_index))*pres_fraction+water_vapor_coef_array(2,2,2,coef_index))*pres_fraction+water_vapor_coef_array(1,2,2,coef_index))*entr_fraction+((water_vapor_coef_array(4,1,2,coef_index)*pres_fraction+water_vapor_coef_array(3,1,2,coef_index))*pres_fraction+water_vapor_coef_array(2,1,2,coef_index))*pres_fraction+water_vapor_coef_array(1,1,2,coef_index)              
                vol = 1.0/dens
                enth = (((((water_vapor_coef_array(4,4,3,coef_index)*pres_fraction+water_vapor_coef_array(3,4,3,coef_index))*pres_fraction+water_vapor_coef_array(2,4,3,coef_index))*pres_fraction+water_vapor_coef_array(1,4,3,coef_index)*entr_fraction)+((water_vapor_coef_array(4,3,3,coef_index)*pres_fraction+water_vapor_coef_array(3,3,3,coef_index))*pres_fraction+water_vapor_coef_array(2,3,3,coef_index))*pres_fraction+water_vapor_coef_array(1,3,3,coef_index))*entr_fraction+((water_vapor_coef_array(4,2,3,coef_index)*pres_fraction+water_vapor_coef_array(3,2,3,coef_index))*pres_fraction+water_vapor_coef_array(2,2,3,coef_index))*pres_fraction+water_vapor_coef_array(1,2,3,coef_index))*entr_fraction+((water_vapor_coef_array(4,1,3,coef_index)*pres_fraction+water_vapor_coef_array(3,1,3,coef_index))*pres_fraction+water_vapor_coef_array(2,1,3,coef_index))*pres_fraction+water_vapor_coef_array(1,1,3,coef_index)              
                cv = (((((water_vapor_coef_array(4,4,4,coef_index)*pres_fraction+water_vapor_coef_array(3,4,4,coef_index))*pres_fraction+water_vapor_coef_array(2,4,4,coef_index))*pres_fraction+water_vapor_coef_array(1,4,4,coef_index)*entr_fraction)+((water_vapor_coef_array(4,3,4,coef_index)*pres_fraction+water_vapor_coef_array(3,3,4,coef_index))*pres_fraction+water_vapor_coef_array(2,3,4,coef_index))*pres_fraction+water_vapor_coef_array(1,3,4,coef_index))*entr_fraction+((water_vapor_coef_array(4,2,4,coef_index)*pres_fraction+water_vapor_coef_array(3,2,4,coef_index))*pres_fraction+water_vapor_coef_array(2,2,4,coef_index))*pres_fraction+water_vapor_coef_array(1,2,4,coef_index))*entr_fraction+((water_vapor_coef_array(4,1,4,coef_index)*pres_fraction+water_vapor_coef_array(3,1,4,coef_index))*pres_fraction+water_vapor_coef_array(2,1,4,coef_index))*pres_fraction+water_vapor_coef_array(1,1,4,coef_index)              
                cp = (((((water_vapor_coef_array(4,4,5,coef_index)*pres_fraction+water_vapor_coef_array(3,4,5,coef_index))*pres_fraction+water_vapor_coef_array(2,4,5,coef_index))*pres_fraction+water_vapor_coef_array(1,4,5,coef_index)*entr_fraction)+((water_vapor_coef_array(4,3,5,coef_index)*pres_fraction+water_vapor_coef_array(3,3,5,coef_index))*pres_fraction+water_vapor_coef_array(2,3,5,coef_index))*pres_fraction+water_vapor_coef_array(1,3,5,coef_index))*entr_fraction+((water_vapor_coef_array(4,2,5,coef_index)*pres_fraction+water_vapor_coef_array(3,2,5,coef_index))*pres_fraction+water_vapor_coef_array(2,2,5,coef_index))*pres_fraction+water_vapor_coef_array(1,2,5,coef_index))*entr_fraction+((water_vapor_coef_array(4,1,5,coef_index)*pres_fraction+water_vapor_coef_array(3,1,5,coef_index))*pres_fraction+water_vapor_coef_array(2,1,5,coef_index))*pres_fraction+water_vapor_coef_array(1,1,5,coef_index)              
                ssnd = (((((water_vapor_coef_array(4,4,6,coef_index)*pres_fraction+water_vapor_coef_array(3,4,6,coef_index))*pres_fraction+water_vapor_coef_array(2,4,6,coef_index))*pres_fraction+water_vapor_coef_array(1,4,6,coef_index)*entr_fraction)+((water_vapor_coef_array(4,3,6,coef_index)*pres_fraction+water_vapor_coef_array(3,3,6,coef_index))*pres_fraction+water_vapor_coef_array(2,3,6,coef_index))*pres_fraction+water_vapor_coef_array(1,3,6,coef_index))*entr_fraction+((water_vapor_coef_array(4,2,6,coef_index)*pres_fraction+water_vapor_coef_array(3,2,6,coef_index))*pres_fraction+water_vapor_coef_array(2,2,6,coef_index))*pres_fraction+water_vapor_coef_array(1,2,6,coef_index))*entr_fraction+((water_vapor_coef_array(4,1,6,coef_index)*pres_fraction+water_vapor_coef_array(3,1,6,coef_index))*pres_fraction+water_vapor_coef_array(2,1,6,coef_index))*pres_fraction+water_vapor_coef_array(1,1,6,coef_index)              
                cond = (((((water_vapor_coef_array(4,4,7,coef_index)*pres_fraction+water_vapor_coef_array(3,4,7,coef_index))*pres_fraction+water_vapor_coef_array(2,4,7,coef_index))*pres_fraction+water_vapor_coef_array(1,4,7,coef_index)*entr_fraction)+((water_vapor_coef_array(4,3,7,coef_index)*pres_fraction+water_vapor_coef_array(3,3,7,coef_index))*pres_fraction+water_vapor_coef_array(2,3,7,coef_index))*pres_fraction+water_vapor_coef_array(1,3,7,coef_index))*entr_fraction+((water_vapor_coef_array(4,2,7,coef_index)*pres_fraction+water_vapor_coef_array(3,2,7,coef_index))*pres_fraction+water_vapor_coef_array(2,2,7,coef_index))*pres_fraction+water_vapor_coef_array(1,2,7,coef_index))*entr_fraction+((water_vapor_coef_array(4,1,7,coef_index)*pres_fraction+water_vapor_coef_array(3,1,7,coef_index))*pres_fraction+water_vapor_coef_array(2,1,7,coef_index))*pres_fraction+water_vapor_coef_array(1,1,7,coef_index)              
                visc = (((((water_vapor_coef_array(4,4,8,coef_index)*pres_fraction+water_vapor_coef_array(3,4,8,coef_index))*pres_fraction+water_vapor_coef_array(2,4,8,coef_index))*pres_fraction+water_vapor_coef_array(1,4,8,coef_index)*entr_fraction)+((water_vapor_coef_array(4,3,8,coef_index)*pres_fraction+water_vapor_coef_array(3,3,8,coef_index))*pres_fraction+water_vapor_coef_array(2,3,8,coef_index))*pres_fraction+water_vapor_coef_array(1,3,8,coef_index))*entr_fraction+((water_vapor_coef_array(4,2,8,coef_index)*pres_fraction+water_vapor_coef_array(3,2,8,coef_index))*pres_fraction+water_vapor_coef_array(2,2,8,coef_index))*pres_fraction+water_vapor_coef_array(1,2,8,coef_index))*entr_fraction+((water_vapor_coef_array(4,1,8,coef_index)*pres_fraction+water_vapor_coef_array(3,1,8,coef_index))*pres_fraction+water_vapor_coef_array(2,1,8,coef_index))*pres_fraction+water_vapor_coef_array(1,1,8,coef_index)              
                inte = enth-pres/dens
                qual = 10.0
            end if
        else if (entr >= sat_entr(0)) then
            
            ! ------TWO-PHASE-----
            
            ! Calculate the quality corresponding to the given entropy.
            quality = (entr-sat_entr(0))/(sat_entr(1)-sat_entr(0))
            
            ! Calculate the saturated liquid and vapor values for volume and enthalpy using vectorized nested multiplication, as well as the actual values given the calculated quality.
            sat_vhs_values = ((water_sat_vhs_coef_array(1:2,:,1,sat_temp_index)*temp_diff+water_sat_vhs_coef_array(1:2,:,2,sat_temp_index))*temp_diff+water_sat_vhs_coef_array(1:2,:,3,sat_temp_index))*temp_diff+water_sat_vhs_coef_array(1:2,:,4,sat_temp_index)
            vhs_values = quality*(sat_vhs_values(:,1)-sat_vhs_values(:,0))+sat_vhs_values(:,0)

            ! Return the requested properties that are valid in the two-phase region.
            qual = quality
            temp = sat_temp
            dens = 1.0/vhs_values(1)
            vol = vhs_values(1)
            enth = vhs_values(2)
            inte = vhs_values(2)-pres*vhs_values(1)  ! u = h-P*v

            ! Check to see if properties are requested that are only valid if quality is 0 or 1.
            if (real(quality,C_FLOAT) == 0.0) then  ! round to single precision to catch close values
                prop_values = ((water_sat_prop_coef_array(1,:,0,sat_temp_index)*temp_diff+water_sat_prop_coef_array(2,:,0,sat_temp_index))*temp_diff+water_sat_prop_coef_array(3,:,0,sat_temp_index))*temp_diff+water_sat_prop_coef_array(4,:,0,sat_temp_index)
                cv = prop_values(1)
                cp = prop_values(2)
                ssnd = prop_values(3)
                cond = prop_values(4)
                visc = prop_values(5)
            else if (real(quality,C_FLOAT) == 1.0) then  ! round to single precision to catch close values
                prop_values = ((water_sat_prop_coef_array(1,:,1,sat_temp_index)*temp_diff+water_sat_prop_coef_array(2,:,1,sat_temp_index))*temp_diff+water_sat_prop_coef_array(3,:,1,sat_temp_index))*temp_diff+water_sat_prop_coef_array(4,:,1,sat_temp_index)
                cv = prop_values(1)
                cp = prop_values(2)
                ssnd = prop_values(3)
                cond = prop_values(4)
                visc = prop_values(5)
            else
                cv = 0.0
                cp = 0.0
                ssnd = 0.0
                cond = 0.0
                visc = 0.0
            end if
        else
            
            ! -----LIQUID-----
            
            ! Determine the pres_index row that contains the provided pressure and its fractional position, as well as the index for the last entropy value in that row.
            call pres_find(pres,pres_index,pres_fraction)
            last_index = water_liquid_entr_index_vector(pres_index+1)-1

            ! Check if the entropy lies in the gap between the vapor dome and the right-most element or if it is fully in the 2D grid.
            if (entr > water_liquid_entr_values(last_index)) then
                ! The given state is in the gap between the vapor dome and the 2D grid.  Calculate its fractional position within the gap and call the liquid_gap subroutine.
                entr_fraction = (entr-water_liquid_entr_values(last_index))/(sat_entr(0)-water_liquid_entr_values(last_index))
                !call liquid_gap(pres,pres_index,pres_fraction,sat_temp,sat_temp_index,temp_diff,entr_fraction,temp,dens,vol,inte,enth,cv,cp,cond,visc,qual,ssnd)
                !call liquid_gap_cubic(pres,pres_index,pres_fraction,sat_temp,sat_temp_index,temp_diff,entr,entr_fraction,temp,dens,vol,inte,enth,cv,cp,cond,visc,qual,ssnd)
                last_coef_index = water_liquid_entr_index_vector(pres_index+1)-pres_index-1  ! the index corresponding to the last element in the pressure row
                temp = liquid_gap_temp(entr_fraction,pres_fraction,sat_temp,last_coef_index)                
                dens = liquid_gap_dens(entr_fraction,pres_fraction,temp_diff,sat_temp_index,last_coef_index)
                vol = 1.0/dens
                enth = liquid_gap_enth(entr_fraction,pres_fraction,temp_diff,sat_temp_index,last_coef_index)
                inte = enth-pres/dens
                cv = liquid_gap_cv(entr_fraction,pres_fraction,temp_diff,sat_temp_index,last_coef_index)
                cp = liquid_gap_cp(entr_fraction,pres_fraction,temp_diff,sat_temp_index,last_coef_index)
                ssnd = liquid_gap_ssnd(entr_fraction,pres_fraction,temp_diff,sat_temp_index,last_coef_index)
                cond = liquid_gap_cond(entr_fraction,pres_fraction,temp_diff,sat_temp_index,last_coef_index)
                visc = liquid_gap_visc(entr_fraction,pres_fraction,temp_diff,sat_temp_index,last_coef_index)
                qual = -10.0
            else
                ! The given state is fully in the liquid region.  Determine the entropy index and check to make sure it is not too small for the given pressure.
                first_index = water_liquid_entr_index_vector(pres_index)
                row_entr_index = maxloc(water_liquid_entr_values(first_index:last_index), 1, mask=water_liquid_entr_values(first_index:last_index) <= entr)  ! locate the largest value that is smaller than or equal to the given entropy
                entr_index = first_index+row_entr_index-1
                if (entr_index == last_index .and. entr == water_liquid_entr_values(last_index)) then
                    ! In the unlikely event that the value at exactly the right side of the last element is specified, adjust the index down one.
                    row_entr_index = row_entr_index-1
                    entr_index = entr_index-1
                end if
                if (entr < water_liquid_entr_values(first_index)) then
                    error_code = 6
                    return
                end if

                ! Calculate the entropy fraction and coefficient index for this element.
                entr_fraction = (entr-water_liquid_entr_values(entr_index))/(water_liquid_entr_values(entr_index+1)-water_liquid_entr_values(entr_index))
                coef_index = water_liquid_entr_index_vector(pres_index)-pres_index+row_entr_index

                ! The coefficients for the element containing the point and the fractional position of the point can now be used to calculate dependent properties.  Nested multiplication is used and the equations are explicitly written to make the compiled code faster.
                temp = (((((water_liquid_coef_array(4,4,1,coef_index)*pres_fraction+water_liquid_coef_array(3,4,1,coef_index))*pres_fraction+water_liquid_coef_array(2,4,1,coef_index))*pres_fraction+water_liquid_coef_array(1,4,1,coef_index)*entr_fraction)+((water_liquid_coef_array(4,3,1,coef_index)*pres_fraction+water_liquid_coef_array(3,3,1,coef_index))*pres_fraction+water_liquid_coef_array(2,3,1,coef_index))*pres_fraction+water_liquid_coef_array(1,3,1,coef_index))*entr_fraction+((water_liquid_coef_array(4,2,1,coef_index)*pres_fraction+water_liquid_coef_array(3,2,1,coef_index))*pres_fraction+water_liquid_coef_array(2,2,1,coef_index))*pres_fraction+water_liquid_coef_array(1,2,1,coef_index))*entr_fraction+((water_liquid_coef_array(4,1,1,coef_index)*pres_fraction+water_liquid_coef_array(3,1,1,coef_index))*pres_fraction+water_liquid_coef_array(2,1,1,coef_index))*pres_fraction+water_liquid_coef_array(1,1,1,coef_index)              
                dens = (((((water_liquid_coef_array(4,4,2,coef_index)*pres_fraction+water_liquid_coef_array(3,4,2,coef_index))*pres_fraction+water_liquid_coef_array(2,4,2,coef_index))*pres_fraction+water_liquid_coef_array(1,4,2,coef_index)*entr_fraction)+((water_liquid_coef_array(4,3,2,coef_index)*pres_fraction+water_liquid_coef_array(3,3,2,coef_index))*pres_fraction+water_liquid_coef_array(2,3,2,coef_index))*pres_fraction+water_liquid_coef_array(1,3,2,coef_index))*entr_fraction+((water_liquid_coef_array(4,2,2,coef_index)*pres_fraction+water_liquid_coef_array(3,2,2,coef_index))*pres_fraction+water_liquid_coef_array(2,2,2,coef_index))*pres_fraction+water_liquid_coef_array(1,2,2,coef_index))*entr_fraction+((water_liquid_coef_array(4,1,2,coef_index)*pres_fraction+water_liquid_coef_array(3,1,2,coef_index))*pres_fraction+water_liquid_coef_array(2,1,2,coef_index))*pres_fraction+water_liquid_coef_array(1,1,2,coef_index)              
                vol = 1.0/dens
                enth = (((((water_liquid_coef_array(4,4,3,coef_index)*pres_fraction+water_liquid_coef_array(3,4,3,coef_index))*pres_fraction+water_liquid_coef_array(2,4,3,coef_index))*pres_fraction+water_liquid_coef_array(1,4,3,coef_index)*entr_fraction)+((water_liquid_coef_array(4,3,3,coef_index)*pres_fraction+water_liquid_coef_array(3,3,3,coef_index))*pres_fraction+water_liquid_coef_array(2,3,3,coef_index))*pres_fraction+water_liquid_coef_array(1,3,3,coef_index))*entr_fraction+((water_liquid_coef_array(4,2,3,coef_index)*pres_fraction+water_liquid_coef_array(3,2,3,coef_index))*pres_fraction+water_liquid_coef_array(2,2,3,coef_index))*pres_fraction+water_liquid_coef_array(1,2,3,coef_index))*entr_fraction+((water_liquid_coef_array(4,1,3,coef_index)*pres_fraction+water_liquid_coef_array(3,1,3,coef_index))*pres_fraction+water_liquid_coef_array(2,1,3,coef_index))*pres_fraction+water_liquid_coef_array(1,1,3,coef_index)              
                cv = (((((water_liquid_coef_array(4,4,4,coef_index)*pres_fraction+water_liquid_coef_array(3,4,4,coef_index))*pres_fraction+water_liquid_coef_array(2,4,4,coef_index))*pres_fraction+water_liquid_coef_array(1,4,4,coef_index)*entr_fraction)+((water_liquid_coef_array(4,3,4,coef_index)*pres_fraction+water_liquid_coef_array(3,3,4,coef_index))*pres_fraction+water_liquid_coef_array(2,3,4,coef_index))*pres_fraction+water_liquid_coef_array(1,3,4,coef_index))*entr_fraction+((water_liquid_coef_array(4,2,4,coef_index)*pres_fraction+water_liquid_coef_array(3,2,4,coef_index))*pres_fraction+water_liquid_coef_array(2,2,4,coef_index))*pres_fraction+water_liquid_coef_array(1,2,4,coef_index))*entr_fraction+((water_liquid_coef_array(4,1,4,coef_index)*pres_fraction+water_liquid_coef_array(3,1,4,coef_index))*pres_fraction+water_liquid_coef_array(2,1,4,coef_index))*pres_fraction+water_liquid_coef_array(1,1,4,coef_index)              
                cp = (((((water_liquid_coef_array(4,4,5,coef_index)*pres_fraction+water_liquid_coef_array(3,4,5,coef_index))*pres_fraction+water_liquid_coef_array(2,4,5,coef_index))*pres_fraction+water_liquid_coef_array(1,4,5,coef_index)*entr_fraction)+((water_liquid_coef_array(4,3,5,coef_index)*pres_fraction+water_liquid_coef_array(3,3,5,coef_index))*pres_fraction+water_liquid_coef_array(2,3,5,coef_index))*pres_fraction+water_liquid_coef_array(1,3,5,coef_index))*entr_fraction+((water_liquid_coef_array(4,2,5,coef_index)*pres_fraction+water_liquid_coef_array(3,2,5,coef_index))*pres_fraction+water_liquid_coef_array(2,2,5,coef_index))*pres_fraction+water_liquid_coef_array(1,2,5,coef_index))*entr_fraction+((water_liquid_coef_array(4,1,5,coef_index)*pres_fraction+water_liquid_coef_array(3,1,5,coef_index))*pres_fraction+water_liquid_coef_array(2,1,5,coef_index))*pres_fraction+water_liquid_coef_array(1,1,5,coef_index)              
                ssnd = (((((water_liquid_coef_array(4,4,6,coef_index)*pres_fraction+water_liquid_coef_array(3,4,6,coef_index))*pres_fraction+water_liquid_coef_array(2,4,6,coef_index))*pres_fraction+water_liquid_coef_array(1,4,6,coef_index)*entr_fraction)+((water_liquid_coef_array(4,3,6,coef_index)*pres_fraction+water_liquid_coef_array(3,3,6,coef_index))*pres_fraction+water_liquid_coef_array(2,3,6,coef_index))*pres_fraction+water_liquid_coef_array(1,3,6,coef_index))*entr_fraction+((water_liquid_coef_array(4,2,6,coef_index)*pres_fraction+water_liquid_coef_array(3,2,6,coef_index))*pres_fraction+water_liquid_coef_array(2,2,6,coef_index))*pres_fraction+water_liquid_coef_array(1,2,6,coef_index))*entr_fraction+((water_liquid_coef_array(4,1,6,coef_index)*pres_fraction+water_liquid_coef_array(3,1,6,coef_index))*pres_fraction+water_liquid_coef_array(2,1,6,coef_index))*pres_fraction+water_liquid_coef_array(1,1,6,coef_index)              
                cond = (((((water_liquid_coef_array(4,4,7,coef_index)*pres_fraction+water_liquid_coef_array(3,4,7,coef_index))*pres_fraction+water_liquid_coef_array(2,4,7,coef_index))*pres_fraction+water_liquid_coef_array(1,4,7,coef_index)*entr_fraction)+((water_liquid_coef_array(4,3,7,coef_index)*pres_fraction+water_liquid_coef_array(3,3,7,coef_index))*pres_fraction+water_liquid_coef_array(2,3,7,coef_index))*pres_fraction+water_liquid_coef_array(1,3,7,coef_index))*entr_fraction+((water_liquid_coef_array(4,2,7,coef_index)*pres_fraction+water_liquid_coef_array(3,2,7,coef_index))*pres_fraction+water_liquid_coef_array(2,2,7,coef_index))*pres_fraction+water_liquid_coef_array(1,2,7,coef_index))*entr_fraction+((water_liquid_coef_array(4,1,7,coef_index)*pres_fraction+water_liquid_coef_array(3,1,7,coef_index))*pres_fraction+water_liquid_coef_array(2,1,7,coef_index))*pres_fraction+water_liquid_coef_array(1,1,7,coef_index)              
                visc = (((((water_liquid_coef_array(4,4,8,coef_index)*pres_fraction+water_liquid_coef_array(3,4,8,coef_index))*pres_fraction+water_liquid_coef_array(2,4,8,coef_index))*pres_fraction+water_liquid_coef_array(1,4,8,coef_index)*entr_fraction)+((water_liquid_coef_array(4,3,8,coef_index)*pres_fraction+water_liquid_coef_array(3,3,8,coef_index))*pres_fraction+water_liquid_coef_array(2,3,8,coef_index))*pres_fraction+water_liquid_coef_array(1,3,8,coef_index))*entr_fraction+((water_liquid_coef_array(4,2,8,coef_index)*pres_fraction+water_liquid_coef_array(3,2,8,coef_index))*pres_fraction+water_liquid_coef_array(2,2,8,coef_index))*pres_fraction+water_liquid_coef_array(1,2,8,coef_index))*entr_fraction+((water_liquid_coef_array(4,1,8,coef_index)*pres_fraction+water_liquid_coef_array(3,1,8,coef_index))*pres_fraction+water_liquid_coef_array(2,1,8,coef_index))*pres_fraction+water_liquid_coef_array(1,1,8,coef_index)              
                inte = enth-pres/dens
                qual = -10.0
            end if

        end if
 
    else
        
        ! -----SUPERCRITICAL-----
        error_code = 99
    end if
    
    return
end subroutine water_PS_f90


subroutine water_TP_f90(temp,pres,error_code,dens,vol,inte,enth,entr,cv,cp,cond,visc,qual,ssnd) bind(C)
    ! Subroutine used to determine water properties from bicubic interpolation tables, given temperature and pressure.
    ! Note: this subroutine does not write to water_error_code or water_error_message.
    ! Required Inputs:
    !   temp = temperature (C)
    !   pres = pressure (kPa)
    ! Required Outputs:
    !   error_code = an integer(C_INT) error code that can be used with the get_error_message subroutine
    ! Optional Outputs:
    !   dens = density (kg/m3)
    !   vol = specific volume (m3/kg)
    !   inte = internal energy (kJ/kg)
    !   enth = enthalpy (kJ/kg)
    !   entr = entropy (kJ/kg-K)
    !   cv = specific heat at constant volume (kJ/kg-K)
    !   cp = specific heat at constant pressure (kJ/kg-K)
    !   cond = thermal conductivity (W/m-K)
    !   visc = viscosity (Pa-s)
    !   qual = quality on a mass basis:
    !           q < 0 indicating subcooled liquid
    !           q = 0 indicating saturated liquid
    !           q = 1 indicating saturated vapor
    !           q > 1 indicating superheated vapor
    !           q >= 999 indicating a supercritical state with temperature and pressure greater than the critical point
    !   ssnd = speed of sound in the fluid (m/s)       
    

    ! Required Argument Declarations
    real(C_DOUBLE), intent(in) :: temp, pres
    integer(C_INT), intent(out) :: error_code
    
    ! Optional Argument Declarations
    real(C_DOUBLE), intent(out) :: dens, vol, inte, enth, entr, cv, cp, cond, visc, qual, ssnd
    
    ! Internal Variable Declarations
    integer(C_INT) :: pres_index, entr_index, row_entr_index, sat_temp_index, first_coef_index, last_coef_index, coef_index
    real(C_DOUBLE) :: pres_fraction, entr_fraction, sat_temp, temp_diff, temp_grid
    real(C_DOUBLE), allocatable, dimension(:) :: edge_temps
    real(C_DOUBLE), dimension(4) :: pres_adjusted_coefs
    
    ! Initialize Error Flag
    error_code = 0
    
    ! Check Pressure Input
    if (pres > water_max_sat_pres .or. pres < water_min_sat_pres) then
        error_code = 4
        return
    end if
    
    ! Check Temperature Input
    if (temp < water_min_temp .or. temp > water_max_temp) then
        error_code = 7
        return
    end if
    
    
    
    ! Use the provided pressure to determine whether the state could be saturated or if it is above the vapor dome.
    if (pres <= water_max_sat_pres) then  ! could be vapor, two-phase, or liquid
        
        ! Determine the saturated liquid and vapor entropy at the given pressure.
        sat_temp = t_sat(pres)
        
        ! Check for an indeterminable state.
        if (temp == sat_temp) then  ! the state is indeterminable without quality
            error_code = 8
            return
        end if
        
        ! Determine the state of the provided pressure and entropy.
        if (temp > sat_temp) then
            
            ! -----VAPOR-----
            
            ! Determine the pres_index row that contains the provided pressure and its fractional position, as well as the index of the first element in that row.
            call pres_find(pres,pres_index,pres_fraction)
            first_coef_index = water_vapor_entr_index_vector(pres_index)-pres_index+1
            
            ! Check if the temperature lies in the gap between the vapor dome and the left-most element.
            temp_grid = ((water_vapor_coef_array(4,1,1,first_coef_index)*pres_fraction+water_vapor_coef_array(3,1,1,first_coef_index))*pres_fraction+water_vapor_coef_array(2,1,1,first_coef_index))*pres_fraction+water_vapor_coef_array(1,1,1,first_coef_index)
            if (temp < temp_grid) then
                ! The given state is in the gap between the vapor dome and the 2D grid.  Calculate its fractional position within the gap and call the appropriate vapor_gap subroutines.
                entr_fraction = (temp-sat_temp)/(temp_grid-sat_temp)  ! the fractional position with the gap, assuming linear interpolation
                call sat_find(sat_temp,sat_temp_index,temp_diff)
                dens = vapor_gap_dens(entr_fraction,pres_fraction,temp_diff,sat_temp_index,first_coef_index)
                vol = 1.0/dens
                enth = vapor_gap_enth(entr_fraction,pres_fraction,temp_diff,sat_temp_index,first_coef_index)
                entr = vapor_gap_entr(entr_fraction,temp_diff,sat_temp_index,pres_index)
                inte = enth-pres/dens
                cv = vapor_gap_cv(entr_fraction,pres_fraction,temp_diff,sat_temp_index,first_coef_index)
                cp = vapor_gap_cp(entr_fraction,pres_fraction,temp_diff,sat_temp_index,first_coef_index)
                ssnd = vapor_gap_ssnd(entr_fraction,pres_fraction,temp_diff,sat_temp_index,first_coef_index)
                cond = vapor_gap_cond(entr_fraction,pres_fraction,temp_diff,sat_temp_index,first_coef_index)
                visc = vapor_gap_visc(entr_fraction,pres_fraction,temp_diff,sat_temp_index,first_coef_index)
                qual = 10.0
            else
                ! The given state is fully in the vapor region.  In order to find the correct entropy index for the given temperature it is necessary to calculate the temperature (adjusted for pressure) at the edges of all the elements in the row.
                last_coef_index = water_vapor_entr_index_vector(pres_index+1)-pres_index-1  ! the index corresponding to the last element in the pressure row
                allocate(edge_temps(last_coef_index-first_coef_index+1))
                edge_temps = ((water_vapor_coef_array(4,1,1,first_coef_index:last_coef_index)*pres_fraction+water_vapor_coef_array(3,1,1,first_coef_index:last_coef_index))*pres_fraction+water_vapor_coef_array(2,1,1,first_coef_index:last_coef_index))*pres_fraction+water_vapor_coef_array(1,1,1,first_coef_index:last_coef_index)
                
                ! Search the calculated temperatures to see which one contains the given temperature.
                row_entr_index = maxloc(edge_temps, 1, mask=edge_temps <= temp)  ! locate the largest value that is smaller than or equal to the given temperature
                coef_index = first_coef_index+row_entr_index-1  ! the coefficient index for this element
                
                ! Deallocate the edge temps vector
                deallocate(edge_temps)
                
                ! Solve the cubic equation for entr_fraction given the provided temperature.
                pres_adjusted_coefs = ((water_vapor_coef_array(4,:,1,coef_index)*pres_fraction+water_vapor_coef_array(3,:,1,coef_index))*pres_fraction+water_vapor_coef_array(2,:,1,coef_index))*pres_fraction+water_vapor_coef_array(1,:,1,coef_index)  ! the coefficients for a cubic equation that is only a function of entr_fraction
                call cubic_solution(pres_adjusted_coefs(1),pres_adjusted_coefs(2),pres_adjusted_coefs(3),pres_adjusted_coefs(4),temp,entr_fraction)
                
                ! Calculate the actual entropy from the fractional position (if requested).
                entr_index = water_vapor_entr_index_vector(pres_index)+row_entr_index-1  ! the entropy index for this element
                entr = water_vapor_entr_values(entr_index)+entr_fraction*(water_vapor_entr_values(entr_index+1)-water_vapor_entr_values(entr_index))

                ! The coefficients for the element containing the point and the fractional position of the point can now be used to calculate dependent properties.  Nested multiplication is used and the equations are explicitly written to make the compiled code faster.
                dens = (((((water_vapor_coef_array(4,4,2,coef_index)*pres_fraction+water_vapor_coef_array(3,4,2,coef_index))*pres_fraction+water_vapor_coef_array(2,4,2,coef_index))*pres_fraction+water_vapor_coef_array(1,4,2,coef_index)*entr_fraction)+((water_vapor_coef_array(4,3,2,coef_index)*pres_fraction+water_vapor_coef_array(3,3,2,coef_index))*pres_fraction+water_vapor_coef_array(2,3,2,coef_index))*pres_fraction+water_vapor_coef_array(1,3,2,coef_index))*entr_fraction+((water_vapor_coef_array(4,2,2,coef_index)*pres_fraction+water_vapor_coef_array(3,2,2,coef_index))*pres_fraction+water_vapor_coef_array(2,2,2,coef_index))*pres_fraction+water_vapor_coef_array(1,2,2,coef_index))*entr_fraction+((water_vapor_coef_array(4,1,2,coef_index)*pres_fraction+water_vapor_coef_array(3,1,2,coef_index))*pres_fraction+water_vapor_coef_array(2,1,2,coef_index))*pres_fraction+water_vapor_coef_array(1,1,2,coef_index)              
                vol = 1.0/dens
                enth = (((((water_vapor_coef_array(4,4,3,coef_index)*pres_fraction+water_vapor_coef_array(3,4,3,coef_index))*pres_fraction+water_vapor_coef_array(2,4,3,coef_index))*pres_fraction+water_vapor_coef_array(1,4,3,coef_index)*entr_fraction)+((water_vapor_coef_array(4,3,3,coef_index)*pres_fraction+water_vapor_coef_array(3,3,3,coef_index))*pres_fraction+water_vapor_coef_array(2,3,3,coef_index))*pres_fraction+water_vapor_coef_array(1,3,3,coef_index))*entr_fraction+((water_vapor_coef_array(4,2,3,coef_index)*pres_fraction+water_vapor_coef_array(3,2,3,coef_index))*pres_fraction+water_vapor_coef_array(2,2,3,coef_index))*pres_fraction+water_vapor_coef_array(1,2,3,coef_index))*entr_fraction+((water_vapor_coef_array(4,1,3,coef_index)*pres_fraction+water_vapor_coef_array(3,1,3,coef_index))*pres_fraction+water_vapor_coef_array(2,1,3,coef_index))*pres_fraction+water_vapor_coef_array(1,1,3,coef_index)              
                cv = (((((water_vapor_coef_array(4,4,4,coef_index)*pres_fraction+water_vapor_coef_array(3,4,4,coef_index))*pres_fraction+water_vapor_coef_array(2,4,4,coef_index))*pres_fraction+water_vapor_coef_array(1,4,4,coef_index)*entr_fraction)+((water_vapor_coef_array(4,3,4,coef_index)*pres_fraction+water_vapor_coef_array(3,3,4,coef_index))*pres_fraction+water_vapor_coef_array(2,3,4,coef_index))*pres_fraction+water_vapor_coef_array(1,3,4,coef_index))*entr_fraction+((water_vapor_coef_array(4,2,4,coef_index)*pres_fraction+water_vapor_coef_array(3,2,4,coef_index))*pres_fraction+water_vapor_coef_array(2,2,4,coef_index))*pres_fraction+water_vapor_coef_array(1,2,4,coef_index))*entr_fraction+((water_vapor_coef_array(4,1,4,coef_index)*pres_fraction+water_vapor_coef_array(3,1,4,coef_index))*pres_fraction+water_vapor_coef_array(2,1,4,coef_index))*pres_fraction+water_vapor_coef_array(1,1,4,coef_index)              
                cp = (((((water_vapor_coef_array(4,4,5,coef_index)*pres_fraction+water_vapor_coef_array(3,4,5,coef_index))*pres_fraction+water_vapor_coef_array(2,4,5,coef_index))*pres_fraction+water_vapor_coef_array(1,4,5,coef_index)*entr_fraction)+((water_vapor_coef_array(4,3,5,coef_index)*pres_fraction+water_vapor_coef_array(3,3,5,coef_index))*pres_fraction+water_vapor_coef_array(2,3,5,coef_index))*pres_fraction+water_vapor_coef_array(1,3,5,coef_index))*entr_fraction+((water_vapor_coef_array(4,2,5,coef_index)*pres_fraction+water_vapor_coef_array(3,2,5,coef_index))*pres_fraction+water_vapor_coef_array(2,2,5,coef_index))*pres_fraction+water_vapor_coef_array(1,2,5,coef_index))*entr_fraction+((water_vapor_coef_array(4,1,5,coef_index)*pres_fraction+water_vapor_coef_array(3,1,5,coef_index))*pres_fraction+water_vapor_coef_array(2,1,5,coef_index))*pres_fraction+water_vapor_coef_array(1,1,5,coef_index)              
                ssnd = (((((water_vapor_coef_array(4,4,6,coef_index)*pres_fraction+water_vapor_coef_array(3,4,6,coef_index))*pres_fraction+water_vapor_coef_array(2,4,6,coef_index))*pres_fraction+water_vapor_coef_array(1,4,6,coef_index)*entr_fraction)+((water_vapor_coef_array(4,3,6,coef_index)*pres_fraction+water_vapor_coef_array(3,3,6,coef_index))*pres_fraction+water_vapor_coef_array(2,3,6,coef_index))*pres_fraction+water_vapor_coef_array(1,3,6,coef_index))*entr_fraction+((water_vapor_coef_array(4,2,6,coef_index)*pres_fraction+water_vapor_coef_array(3,2,6,coef_index))*pres_fraction+water_vapor_coef_array(2,2,6,coef_index))*pres_fraction+water_vapor_coef_array(1,2,6,coef_index))*entr_fraction+((water_vapor_coef_array(4,1,6,coef_index)*pres_fraction+water_vapor_coef_array(3,1,6,coef_index))*pres_fraction+water_vapor_coef_array(2,1,6,coef_index))*pres_fraction+water_vapor_coef_array(1,1,6,coef_index)              
                cond = (((((water_vapor_coef_array(4,4,7,coef_index)*pres_fraction+water_vapor_coef_array(3,4,7,coef_index))*pres_fraction+water_vapor_coef_array(2,4,7,coef_index))*pres_fraction+water_vapor_coef_array(1,4,7,coef_index)*entr_fraction)+((water_vapor_coef_array(4,3,7,coef_index)*pres_fraction+water_vapor_coef_array(3,3,7,coef_index))*pres_fraction+water_vapor_coef_array(2,3,7,coef_index))*pres_fraction+water_vapor_coef_array(1,3,7,coef_index))*entr_fraction+((water_vapor_coef_array(4,2,7,coef_index)*pres_fraction+water_vapor_coef_array(3,2,7,coef_index))*pres_fraction+water_vapor_coef_array(2,2,7,coef_index))*pres_fraction+water_vapor_coef_array(1,2,7,coef_index))*entr_fraction+((water_vapor_coef_array(4,1,7,coef_index)*pres_fraction+water_vapor_coef_array(3,1,7,coef_index))*pres_fraction+water_vapor_coef_array(2,1,7,coef_index))*pres_fraction+water_vapor_coef_array(1,1,7,coef_index)              
                visc = (((((water_vapor_coef_array(4,4,8,coef_index)*pres_fraction+water_vapor_coef_array(3,4,8,coef_index))*pres_fraction+water_vapor_coef_array(2,4,8,coef_index))*pres_fraction+water_vapor_coef_array(1,4,8,coef_index)*entr_fraction)+((water_vapor_coef_array(4,3,8,coef_index)*pres_fraction+water_vapor_coef_array(3,3,8,coef_index))*pres_fraction+water_vapor_coef_array(2,3,8,coef_index))*pres_fraction+water_vapor_coef_array(1,3,8,coef_index))*entr_fraction+((water_vapor_coef_array(4,2,8,coef_index)*pres_fraction+water_vapor_coef_array(3,2,8,coef_index))*pres_fraction+water_vapor_coef_array(2,2,8,coef_index))*pres_fraction+water_vapor_coef_array(1,2,8,coef_index))*entr_fraction+((water_vapor_coef_array(4,1,8,coef_index)*pres_fraction+water_vapor_coef_array(3,1,8,coef_index))*pres_fraction+water_vapor_coef_array(2,1,8,coef_index))*pres_fraction+water_vapor_coef_array(1,1,8,coef_index)              
                inte = enth-pres/dens
                qual = 10.0
            end if
        else
            
            ! -----LIQUID-----
                        
            ! Determine the pres_index row that contains the provided pressure and its fractional position, as well as the index of the first element in that row.
            call pres_find(pres,pres_index,pres_fraction)
            last_coef_index = water_liquid_entr_index_vector(pres_index+1)-pres_index-1  ! the index corresponding to the last element in the pressure row

            ! Check if the temperature lies in the gap between the vapor dome and the right-most element.
            temp_grid = (((((water_liquid_coef_array(4,4,1,last_coef_index)*pres_fraction+water_liquid_coef_array(3,4,1,last_coef_index))*pres_fraction+water_liquid_coef_array(2,4,1,last_coef_index))*pres_fraction+water_liquid_coef_array(1,4,1,last_coef_index))+((water_liquid_coef_array(4,3,1,last_coef_index)*pres_fraction+water_liquid_coef_array(3,3,1,last_coef_index))*pres_fraction+water_liquid_coef_array(2,3,1,last_coef_index))*pres_fraction+water_liquid_coef_array(1,3,1,last_coef_index))+((water_liquid_coef_array(4,2,1,last_coef_index)*pres_fraction+water_liquid_coef_array(3,2,1,last_coef_index))*pres_fraction+water_liquid_coef_array(2,2,1,last_coef_index))*pres_fraction+water_liquid_coef_array(1,2,1,last_coef_index))+((water_liquid_coef_array(4,1,1,last_coef_index)*pres_fraction+water_liquid_coef_array(3,1,1,last_coef_index))*pres_fraction+water_liquid_coef_array(2,1,1,last_coef_index))*pres_fraction+water_liquid_coef_array(1,1,1,last_coef_index)
            if (temp > temp_grid) then
                ! The given state is in the gap between the vapor dome and the 2D grid.  Calculate its fractional position within the gap and call the appropriate liquid_gap subroutines.
                entr_fraction = (temp-temp_grid)/(sat_temp-temp_grid)  ! the fractional position with the gap, assuming linear interpolation                
                call sat_find(sat_temp,sat_temp_index,temp_diff)
                dens = liquid_gap_dens(entr_fraction,pres_fraction,temp_diff,sat_temp_index,last_coef_index)
                vol = 1.0/dens
                enth = liquid_gap_enth(entr_fraction,pres_fraction,temp_diff,sat_temp_index,last_coef_index)
                entr = liquid_gap_entr(entr_fraction,temp_diff,sat_temp_index,pres_index)
                inte = enth-pres/dens
                cv = liquid_gap_cv(entr_fraction,pres_fraction,temp_diff,sat_temp_index,last_coef_index)
                cp = liquid_gap_cp(entr_fraction,pres_fraction,temp_diff,sat_temp_index,last_coef_index)
                ssnd = liquid_gap_ssnd(entr_fraction,pres_fraction,temp_diff,sat_temp_index,last_coef_index)
                cond = liquid_gap_cond(entr_fraction,pres_fraction,temp_diff,sat_temp_index,last_coef_index)
                visc = liquid_gap_visc(entr_fraction,pres_fraction,temp_diff,sat_temp_index,last_coef_index)
                qual = -10.0
            else
                ! The given state is fully in the liquid region.  In order to find the correct entropy index for the given temperature it is necessary to calculate the temperature (adjusted for pressure) at the edges of all the elements in the row.
                first_coef_index = water_liquid_entr_index_vector(pres_index)-pres_index+1
                allocate(edge_temps(last_coef_index-first_coef_index+1))
                edge_temps = ((water_liquid_coef_array(4,1,1,first_coef_index:last_coef_index)*pres_fraction+water_liquid_coef_array(3,1,1,first_coef_index:last_coef_index))*pres_fraction+water_liquid_coef_array(2,1,1,first_coef_index:last_coef_index))*pres_fraction+water_liquid_coef_array(1,1,1,first_coef_index:last_coef_index)
                
                ! Search the calculated temperatures to see which one contains the given temperature.
                row_entr_index = maxloc(edge_temps, 1, mask=edge_temps <= temp)  ! locate the largest value that is smaller than or equal to the given temperature
                coef_index = first_coef_index+row_entr_index-1  ! the coefficient index for this element
                
                ! Deallocate the edge temps vector
                deallocate(edge_temps)
                
                ! Solve the cubic equation for entr_fraction given the provided temperature.
                pres_adjusted_coefs = ((water_liquid_coef_array(4,:,1,coef_index)*pres_fraction+water_liquid_coef_array(3,:,1,coef_index))*pres_fraction+water_liquid_coef_array(2,:,1,coef_index))*pres_fraction+water_liquid_coef_array(1,:,1,coef_index)  ! the coefficients for a cubic equation that is only a function of entr_fraction
                call cubic_solution(pres_adjusted_coefs(1),pres_adjusted_coefs(2),pres_adjusted_coefs(3),pres_adjusted_coefs(4),temp,entr_fraction)
                
                ! Calculate the actual entropy from the fractional position (if requested).
                entr_index = water_liquid_entr_index_vector(pres_index)+row_entr_index-1  ! the entropy index for this element
                entr = water_liquid_entr_values(entr_index)+entr_fraction*(water_liquid_entr_values(entr_index+1)-water_liquid_entr_values(entr_index))

                ! The coefficients for the element containing the point and the fractional position of the point can now be used to calculate dependent properties.  Nested multiplication is used and the equations are explicitly written to make the compiled code faster.
                dens = (((((water_liquid_coef_array(4,4,2,coef_index)*pres_fraction+water_liquid_coef_array(3,4,2,coef_index))*pres_fraction+water_liquid_coef_array(2,4,2,coef_index))*pres_fraction+water_liquid_coef_array(1,4,2,coef_index)*entr_fraction)+((water_liquid_coef_array(4,3,2,coef_index)*pres_fraction+water_liquid_coef_array(3,3,2,coef_index))*pres_fraction+water_liquid_coef_array(2,3,2,coef_index))*pres_fraction+water_liquid_coef_array(1,3,2,coef_index))*entr_fraction+((water_liquid_coef_array(4,2,2,coef_index)*pres_fraction+water_liquid_coef_array(3,2,2,coef_index))*pres_fraction+water_liquid_coef_array(2,2,2,coef_index))*pres_fraction+water_liquid_coef_array(1,2,2,coef_index))*entr_fraction+((water_liquid_coef_array(4,1,2,coef_index)*pres_fraction+water_liquid_coef_array(3,1,2,coef_index))*pres_fraction+water_liquid_coef_array(2,1,2,coef_index))*pres_fraction+water_liquid_coef_array(1,1,2,coef_index)              
                vol = 1.0/dens
                enth = (((((water_liquid_coef_array(4,4,3,coef_index)*pres_fraction+water_liquid_coef_array(3,4,3,coef_index))*pres_fraction+water_liquid_coef_array(2,4,3,coef_index))*pres_fraction+water_liquid_coef_array(1,4,3,coef_index)*entr_fraction)+((water_liquid_coef_array(4,3,3,coef_index)*pres_fraction+water_liquid_coef_array(3,3,3,coef_index))*pres_fraction+water_liquid_coef_array(2,3,3,coef_index))*pres_fraction+water_liquid_coef_array(1,3,3,coef_index))*entr_fraction+((water_liquid_coef_array(4,2,3,coef_index)*pres_fraction+water_liquid_coef_array(3,2,3,coef_index))*pres_fraction+water_liquid_coef_array(2,2,3,coef_index))*pres_fraction+water_liquid_coef_array(1,2,3,coef_index))*entr_fraction+((water_liquid_coef_array(4,1,3,coef_index)*pres_fraction+water_liquid_coef_array(3,1,3,coef_index))*pres_fraction+water_liquid_coef_array(2,1,3,coef_index))*pres_fraction+water_liquid_coef_array(1,1,3,coef_index)              
                cv = (((((water_liquid_coef_array(4,4,4,coef_index)*pres_fraction+water_liquid_coef_array(3,4,4,coef_index))*pres_fraction+water_liquid_coef_array(2,4,4,coef_index))*pres_fraction+water_liquid_coef_array(1,4,4,coef_index)*entr_fraction)+((water_liquid_coef_array(4,3,4,coef_index)*pres_fraction+water_liquid_coef_array(3,3,4,coef_index))*pres_fraction+water_liquid_coef_array(2,3,4,coef_index))*pres_fraction+water_liquid_coef_array(1,3,4,coef_index))*entr_fraction+((water_liquid_coef_array(4,2,4,coef_index)*pres_fraction+water_liquid_coef_array(3,2,4,coef_index))*pres_fraction+water_liquid_coef_array(2,2,4,coef_index))*pres_fraction+water_liquid_coef_array(1,2,4,coef_index))*entr_fraction+((water_liquid_coef_array(4,1,4,coef_index)*pres_fraction+water_liquid_coef_array(3,1,4,coef_index))*pres_fraction+water_liquid_coef_array(2,1,4,coef_index))*pres_fraction+water_liquid_coef_array(1,1,4,coef_index)              
                cp = (((((water_liquid_coef_array(4,4,5,coef_index)*pres_fraction+water_liquid_coef_array(3,4,5,coef_index))*pres_fraction+water_liquid_coef_array(2,4,5,coef_index))*pres_fraction+water_liquid_coef_array(1,4,5,coef_index)*entr_fraction)+((water_liquid_coef_array(4,3,5,coef_index)*pres_fraction+water_liquid_coef_array(3,3,5,coef_index))*pres_fraction+water_liquid_coef_array(2,3,5,coef_index))*pres_fraction+water_liquid_coef_array(1,3,5,coef_index))*entr_fraction+((water_liquid_coef_array(4,2,5,coef_index)*pres_fraction+water_liquid_coef_array(3,2,5,coef_index))*pres_fraction+water_liquid_coef_array(2,2,5,coef_index))*pres_fraction+water_liquid_coef_array(1,2,5,coef_index))*entr_fraction+((water_liquid_coef_array(4,1,5,coef_index)*pres_fraction+water_liquid_coef_array(3,1,5,coef_index))*pres_fraction+water_liquid_coef_array(2,1,5,coef_index))*pres_fraction+water_liquid_coef_array(1,1,5,coef_index)              
                ssnd = (((((water_liquid_coef_array(4,4,6,coef_index)*pres_fraction+water_liquid_coef_array(3,4,6,coef_index))*pres_fraction+water_liquid_coef_array(2,4,6,coef_index))*pres_fraction+water_liquid_coef_array(1,4,6,coef_index)*entr_fraction)+((water_liquid_coef_array(4,3,6,coef_index)*pres_fraction+water_liquid_coef_array(3,3,6,coef_index))*pres_fraction+water_liquid_coef_array(2,3,6,coef_index))*pres_fraction+water_liquid_coef_array(1,3,6,coef_index))*entr_fraction+((water_liquid_coef_array(4,2,6,coef_index)*pres_fraction+water_liquid_coef_array(3,2,6,coef_index))*pres_fraction+water_liquid_coef_array(2,2,6,coef_index))*pres_fraction+water_liquid_coef_array(1,2,6,coef_index))*entr_fraction+((water_liquid_coef_array(4,1,6,coef_index)*pres_fraction+water_liquid_coef_array(3,1,6,coef_index))*pres_fraction+water_liquid_coef_array(2,1,6,coef_index))*pres_fraction+water_liquid_coef_array(1,1,6,coef_index)              
                cond = (((((water_liquid_coef_array(4,4,7,coef_index)*pres_fraction+water_liquid_coef_array(3,4,7,coef_index))*pres_fraction+water_liquid_coef_array(2,4,7,coef_index))*pres_fraction+water_liquid_coef_array(1,4,7,coef_index)*entr_fraction)+((water_liquid_coef_array(4,3,7,coef_index)*pres_fraction+water_liquid_coef_array(3,3,7,coef_index))*pres_fraction+water_liquid_coef_array(2,3,7,coef_index))*pres_fraction+water_liquid_coef_array(1,3,7,coef_index))*entr_fraction+((water_liquid_coef_array(4,2,7,coef_index)*pres_fraction+water_liquid_coef_array(3,2,7,coef_index))*pres_fraction+water_liquid_coef_array(2,2,7,coef_index))*pres_fraction+water_liquid_coef_array(1,2,7,coef_index))*entr_fraction+((water_liquid_coef_array(4,1,7,coef_index)*pres_fraction+water_liquid_coef_array(3,1,7,coef_index))*pres_fraction+water_liquid_coef_array(2,1,7,coef_index))*pres_fraction+water_liquid_coef_array(1,1,7,coef_index)              
                visc = (((((water_liquid_coef_array(4,4,8,coef_index)*pres_fraction+water_liquid_coef_array(3,4,8,coef_index))*pres_fraction+water_liquid_coef_array(2,4,8,coef_index))*pres_fraction+water_liquid_coef_array(1,4,8,coef_index)*entr_fraction)+((water_liquid_coef_array(4,3,8,coef_index)*pres_fraction+water_liquid_coef_array(3,3,8,coef_index))*pres_fraction+water_liquid_coef_array(2,3,8,coef_index))*pres_fraction+water_liquid_coef_array(1,3,8,coef_index))*entr_fraction+((water_liquid_coef_array(4,2,8,coef_index)*pres_fraction+water_liquid_coef_array(3,2,8,coef_index))*pres_fraction+water_liquid_coef_array(2,2,8,coef_index))*pres_fraction+water_liquid_coef_array(1,2,8,coef_index))*entr_fraction+((water_liquid_coef_array(4,1,8,coef_index)*pres_fraction+water_liquid_coef_array(3,1,8,coef_index))*pres_fraction+water_liquid_coef_array(2,1,8,coef_index))*pres_fraction+water_liquid_coef_array(1,1,8,coef_index)              
                inte = enth-pres/dens
                qual = -10.0
            end if

!
        end if
!
!        
    else
        
        ! -----SUPERCRITICAL-----
        error_code = 99
    end if
    
    
end subroutine water_TP_f90

subroutine water_PH_f90(pres,enth,error_code,temp,dens,vol,inte,entr,cv,cp,cond,visc,qual,ssnd) bind(C)
    ! Subroutine used to determine water properties from bicubic interpolation tables, given pressure and enthalpy.
    ! Note: this subroutine does not write to water_error_code or water_error_message.
    ! Required Inputs:
    !   pres = pressure (kPa)
    !   enth = enthalpy (kJ/kg)
    ! Required Outputs:
    !   error_code = an integer(C_INT) error code that can be used with the get_error_message subroutine
    ! Optional Outputs:
    !   temp = temperature (C)
    !   dens = density (kg/m3)
    !   vol = specific volume (m3/kg)
    !   inte = internal energy (kJ/kg)
    !   entr = entropy (kJ/kg-K)
    !   cv = specific heat at constant volume (kJ/kg-K)
    !   cp = specific heat at constant pressure (kJ/kg-K)
    !   cond = thermal conductivity (W/m-K)
    !   visc = viscosity (Pa-s)
    !   qual = quality on a mass basis:
    !           q < 0 indicating subcooled liquid
    !           q = 0 indicating saturated liquid
    !           q = 1 indicating saturated vapor
    !           q > 1 indicating superheated vapor
    !           q >= 999 indicating a supercritical state with temperature and pressure greater than the critical point
    !   ssnd = speed of sound in the fluid (m/s)       
    

    ! Required Argument Declarations
    real(C_DOUBLE), intent(in) :: pres, enth
    integer(C_INT), intent(out) :: error_code
    
    ! Optional Argument Declarations
    real(C_DOUBLE), intent(out) :: temp, dens, vol, inte, entr, cv, cp, cond, visc, qual, ssnd
   
    ! Internal Variable Declarations
    integer(C_INT) :: pres_index, entr_index, row_entr_index, sat_temp_index, first_coef_index, last_coef_index, coef_index
    real(C_DOUBLE) :: pres_fraction, entr_fraction, sat_temp, temp_diff, enth_grid, quality
    real(C_DOUBLE), dimension(0:1) :: sat_enth
    real(C_DOUBLE), allocatable, dimension(:) :: edge_enths
    real(C_DOUBLE), dimension(4) :: pres_adjusted_coefs
    real(C_DOUBLE), dimension(3,0:1) :: sat_vhs_values
    real(C_DOUBLE), dimension(3) :: vhs_values
    real(C_DOUBLE), dimension(5) :: prop_values
    
    ! Initialize Error Flag
    error_code = 0
    
    ! Check Pressure Input
    if (pres > water_max_sat_pres .or. pres < water_min_sat_pres) then
        error_code = 4
        return
    end if
    
    
    ! Use the provided pressure to determine whether the state could be saturated or if it is above the vapor dome.
    if (pres <= water_max_sat_pres) then  ! could be vapor, two-phase, or liquid
        
        ! Determine the saturated liquid and vapor enthalpy at the given pressure.
        sat_temp = t_sat(pres)
        call sat_find(sat_temp,sat_temp_index,temp_diff)
        sat_enth = ((water_sat_vhs_coef_array(2,:,1,sat_temp_index)*temp_diff+water_sat_vhs_coef_array(2,:,2,sat_temp_index))*temp_diff+water_sat_vhs_coef_array(2,:,3,sat_temp_index))*temp_diff+water_sat_vhs_coef_array(2,:,4,sat_temp_index)
              
        ! Determine the state of the provided pressure and entropy.
        if (enth > sat_enth(1)) then
            
            ! -----VAPOR-----
            
            ! Determine the pres_index row that contains the provided pressure and its fractional position, as well as the index of the first element in that row.
            call pres_find(pres,pres_index,pres_fraction)
            first_coef_index = water_vapor_entr_index_vector(pres_index)-pres_index+1
            
            ! Check if the temperature lies in the gap between the vapor dome and the left-most element.
            enth_grid = ((water_vapor_coef_array(4,1,3,first_coef_index)*pres_fraction+water_vapor_coef_array(3,1,3,first_coef_index))*pres_fraction+water_vapor_coef_array(2,1,3,first_coef_index))*pres_fraction+water_vapor_coef_array(1,1,3,first_coef_index)
            if (enth < enth_grid) then
                ! The given state is in the gap between the vapor dome and the 2D grid.  Calculate its fractional position within the gap and call the appropriate vapor_gap subroutines.
                entr_fraction = (enth-sat_enth(1))/(enth_grid-sat_enth(1))  ! the fractional position with the gap, assuming linear interpolation
                call sat_find(sat_temp,sat_temp_index,temp_diff)
                temp = vapor_gap_temp(entr_fraction,pres_fraction,sat_temp,first_coef_index)
                dens = vapor_gap_dens(entr_fraction,pres_fraction,temp_diff,sat_temp_index,first_coef_index)
                vol = 1.0/dens
                entr = vapor_gap_entr(entr_fraction,temp_diff,sat_temp_index,pres_index)
                inte = enth-pres/dens
                cv = vapor_gap_cv(entr_fraction,pres_fraction,temp_diff,sat_temp_index,first_coef_index)
                cp = vapor_gap_cp(entr_fraction,pres_fraction,temp_diff,sat_temp_index,first_coef_index)
                ssnd = vapor_gap_ssnd(entr_fraction,pres_fraction,temp_diff,sat_temp_index,first_coef_index)
                cond = vapor_gap_cond(entr_fraction,pres_fraction,temp_diff,sat_temp_index,first_coef_index)
                visc = vapor_gap_visc(entr_fraction,pres_fraction,temp_diff,sat_temp_index,first_coef_index)
                qual = 10.0
            else
                ! The given state is fully in the vapor region.  In order to find the correct entropy index for the given enthalpy it is necessary to calculate the enthalpy (adjusted for pressure) at the edges of all the elements in the row.
                last_coef_index = water_vapor_entr_index_vector(pres_index+1)-pres_index-1  ! the index corresponding to the last element in the pressure row
                allocate(edge_enths(last_coef_index-first_coef_index+1))
                edge_enths = ((water_vapor_coef_array(4,1,3,first_coef_index:last_coef_index)*pres_fraction+water_vapor_coef_array(3,1,3,first_coef_index:last_coef_index))*pres_fraction+water_vapor_coef_array(2,1,3,first_coef_index:last_coef_index))*pres_fraction+water_vapor_coef_array(1,1,3,first_coef_index:last_coef_index)
                
                ! Search the calculated enthalpies to see which one contains the given enthalpy.
                row_entr_index = maxloc(edge_enths, 1, mask=edge_enths <= enth)  ! locate the largest value that is smaller than or equal to the given enthalpy
                coef_index = first_coef_index+row_entr_index-1  ! the coefficient index for this element
                
                ! Deallocate the edge enths vector
                deallocate(edge_enths)
                

                
                ! Solve the cubic equation for entr_fraction given the provided enthalpy.
                pres_adjusted_coefs = ((water_vapor_coef_array(4,:,3,coef_index)*pres_fraction+water_vapor_coef_array(3,:,3,coef_index))*pres_fraction+water_vapor_coef_array(2,:,3,coef_index))*pres_fraction+water_vapor_coef_array(1,:,3,coef_index)  ! the coefficients for a cubic equation that is only a function of entr_fraction
                call cubic_solution(pres_adjusted_coefs(1),pres_adjusted_coefs(2),pres_adjusted_coefs(3),pres_adjusted_coefs(4),enth,entr_fraction)
                
                ! Check to make sure the provided enthalpy is not too large.
                if (entr_fraction > 1.0) then
                    ! Future Improvement: Add a check to see if a valid value for enthalpy is given and adjust things accordingly.
                    error_code = 9
                    return
                end if
                
                
                ! Calculate the actual entropy from the fractional position (if requested).
                entr_index = water_vapor_entr_index_vector(pres_index)+row_entr_index-1  ! the entropy index for this element
                entr = water_vapor_entr_values(entr_index)+entr_fraction*(water_vapor_entr_values(entr_index+1)-water_vapor_entr_values(entr_index))

                ! The coefficients for the element containing the point and the fractional position of the point can now be used to calculate dependent properties.  Nested multiplication is used and the equations are explicitly written to make the compiled code faster.
                temp = (((((water_vapor_coef_array(4,4,1,coef_index)*pres_fraction+water_vapor_coef_array(3,4,1,coef_index))*pres_fraction+water_vapor_coef_array(2,4,1,coef_index))*pres_fraction+water_vapor_coef_array(1,4,1,coef_index)*entr_fraction)+((water_vapor_coef_array(4,3,1,coef_index)*pres_fraction+water_vapor_coef_array(3,3,1,coef_index))*pres_fraction+water_vapor_coef_array(2,3,1,coef_index))*pres_fraction+water_vapor_coef_array(1,3,1,coef_index))*entr_fraction+((water_vapor_coef_array(4,2,1,coef_index)*pres_fraction+water_vapor_coef_array(3,2,1,coef_index))*pres_fraction+water_vapor_coef_array(2,2,1,coef_index))*pres_fraction+water_vapor_coef_array(1,2,1,coef_index))*entr_fraction+((water_vapor_coef_array(4,1,1,coef_index)*pres_fraction+water_vapor_coef_array(3,1,1,coef_index))*pres_fraction+water_vapor_coef_array(2,1,1,coef_index))*pres_fraction+water_vapor_coef_array(1,1,1,coef_index)
                dens = (((((water_vapor_coef_array(4,4,2,coef_index)*pres_fraction+water_vapor_coef_array(3,4,2,coef_index))*pres_fraction+water_vapor_coef_array(2,4,2,coef_index))*pres_fraction+water_vapor_coef_array(1,4,2,coef_index)*entr_fraction)+((water_vapor_coef_array(4,3,2,coef_index)*pres_fraction+water_vapor_coef_array(3,3,2,coef_index))*pres_fraction+water_vapor_coef_array(2,3,2,coef_index))*pres_fraction+water_vapor_coef_array(1,3,2,coef_index))*entr_fraction+((water_vapor_coef_array(4,2,2,coef_index)*pres_fraction+water_vapor_coef_array(3,2,2,coef_index))*pres_fraction+water_vapor_coef_array(2,2,2,coef_index))*pres_fraction+water_vapor_coef_array(1,2,2,coef_index))*entr_fraction+((water_vapor_coef_array(4,1,2,coef_index)*pres_fraction+water_vapor_coef_array(3,1,2,coef_index))*pres_fraction+water_vapor_coef_array(2,1,2,coef_index))*pres_fraction+water_vapor_coef_array(1,1,2,coef_index)              
                vol = 1.0/dens
                cv = (((((water_vapor_coef_array(4,4,4,coef_index)*pres_fraction+water_vapor_coef_array(3,4,4,coef_index))*pres_fraction+water_vapor_coef_array(2,4,4,coef_index))*pres_fraction+water_vapor_coef_array(1,4,4,coef_index)*entr_fraction)+((water_vapor_coef_array(4,3,4,coef_index)*pres_fraction+water_vapor_coef_array(3,3,4,coef_index))*pres_fraction+water_vapor_coef_array(2,3,4,coef_index))*pres_fraction+water_vapor_coef_array(1,3,4,coef_index))*entr_fraction+((water_vapor_coef_array(4,2,4,coef_index)*pres_fraction+water_vapor_coef_array(3,2,4,coef_index))*pres_fraction+water_vapor_coef_array(2,2,4,coef_index))*pres_fraction+water_vapor_coef_array(1,2,4,coef_index))*entr_fraction+((water_vapor_coef_array(4,1,4,coef_index)*pres_fraction+water_vapor_coef_array(3,1,4,coef_index))*pres_fraction+water_vapor_coef_array(2,1,4,coef_index))*pres_fraction+water_vapor_coef_array(1,1,4,coef_index)              
                cp = (((((water_vapor_coef_array(4,4,5,coef_index)*pres_fraction+water_vapor_coef_array(3,4,5,coef_index))*pres_fraction+water_vapor_coef_array(2,4,5,coef_index))*pres_fraction+water_vapor_coef_array(1,4,5,coef_index)*entr_fraction)+((water_vapor_coef_array(4,3,5,coef_index)*pres_fraction+water_vapor_coef_array(3,3,5,coef_index))*pres_fraction+water_vapor_coef_array(2,3,5,coef_index))*pres_fraction+water_vapor_coef_array(1,3,5,coef_index))*entr_fraction+((water_vapor_coef_array(4,2,5,coef_index)*pres_fraction+water_vapor_coef_array(3,2,5,coef_index))*pres_fraction+water_vapor_coef_array(2,2,5,coef_index))*pres_fraction+water_vapor_coef_array(1,2,5,coef_index))*entr_fraction+((water_vapor_coef_array(4,1,5,coef_index)*pres_fraction+water_vapor_coef_array(3,1,5,coef_index))*pres_fraction+water_vapor_coef_array(2,1,5,coef_index))*pres_fraction+water_vapor_coef_array(1,1,5,coef_index)              
                ssnd = (((((water_vapor_coef_array(4,4,6,coef_index)*pres_fraction+water_vapor_coef_array(3,4,6,coef_index))*pres_fraction+water_vapor_coef_array(2,4,6,coef_index))*pres_fraction+water_vapor_coef_array(1,4,6,coef_index)*entr_fraction)+((water_vapor_coef_array(4,3,6,coef_index)*pres_fraction+water_vapor_coef_array(3,3,6,coef_index))*pres_fraction+water_vapor_coef_array(2,3,6,coef_index))*pres_fraction+water_vapor_coef_array(1,3,6,coef_index))*entr_fraction+((water_vapor_coef_array(4,2,6,coef_index)*pres_fraction+water_vapor_coef_array(3,2,6,coef_index))*pres_fraction+water_vapor_coef_array(2,2,6,coef_index))*pres_fraction+water_vapor_coef_array(1,2,6,coef_index))*entr_fraction+((water_vapor_coef_array(4,1,6,coef_index)*pres_fraction+water_vapor_coef_array(3,1,6,coef_index))*pres_fraction+water_vapor_coef_array(2,1,6,coef_index))*pres_fraction+water_vapor_coef_array(1,1,6,coef_index)              
                cond = (((((water_vapor_coef_array(4,4,7,coef_index)*pres_fraction+water_vapor_coef_array(3,4,7,coef_index))*pres_fraction+water_vapor_coef_array(2,4,7,coef_index))*pres_fraction+water_vapor_coef_array(1,4,7,coef_index)*entr_fraction)+((water_vapor_coef_array(4,3,7,coef_index)*pres_fraction+water_vapor_coef_array(3,3,7,coef_index))*pres_fraction+water_vapor_coef_array(2,3,7,coef_index))*pres_fraction+water_vapor_coef_array(1,3,7,coef_index))*entr_fraction+((water_vapor_coef_array(4,2,7,coef_index)*pres_fraction+water_vapor_coef_array(3,2,7,coef_index))*pres_fraction+water_vapor_coef_array(2,2,7,coef_index))*pres_fraction+water_vapor_coef_array(1,2,7,coef_index))*entr_fraction+((water_vapor_coef_array(4,1,7,coef_index)*pres_fraction+water_vapor_coef_array(3,1,7,coef_index))*pres_fraction+water_vapor_coef_array(2,1,7,coef_index))*pres_fraction+water_vapor_coef_array(1,1,7,coef_index)              
                visc = (((((water_vapor_coef_array(4,4,8,coef_index)*pres_fraction+water_vapor_coef_array(3,4,8,coef_index))*pres_fraction+water_vapor_coef_array(2,4,8,coef_index))*pres_fraction+water_vapor_coef_array(1,4,8,coef_index)*entr_fraction)+((water_vapor_coef_array(4,3,8,coef_index)*pres_fraction+water_vapor_coef_array(3,3,8,coef_index))*pres_fraction+water_vapor_coef_array(2,3,8,coef_index))*pres_fraction+water_vapor_coef_array(1,3,8,coef_index))*entr_fraction+((water_vapor_coef_array(4,2,8,coef_index)*pres_fraction+water_vapor_coef_array(3,2,8,coef_index))*pres_fraction+water_vapor_coef_array(2,2,8,coef_index))*pres_fraction+water_vapor_coef_array(1,2,8,coef_index))*entr_fraction+((water_vapor_coef_array(4,1,8,coef_index)*pres_fraction+water_vapor_coef_array(3,1,8,coef_index))*pres_fraction+water_vapor_coef_array(2,1,8,coef_index))*pres_fraction+water_vapor_coef_array(1,1,8,coef_index)              
                inte = enth-pres/dens
                qual = 10.0
            end if
        else if (enth >= sat_enth(0)) then
            
            ! ------TWO-PHASE-----
            
            ! Calculate the quality corresponding to the given entropy.
            quality = (enth-sat_enth(0))/(sat_enth(1)-sat_enth(0))
            
            ! Calculate the saturated liquid and vapor values for volume, enthalpy, and entropy using vectorized nested multiplication, as well as the actual values given the calculated quality.
            sat_vhs_values = ((water_sat_vhs_coef_array(:,:,1,sat_temp_index)*temp_diff+water_sat_vhs_coef_array(:,:,2,sat_temp_index))*temp_diff+water_sat_vhs_coef_array(:,:,3,sat_temp_index))*temp_diff+water_sat_vhs_coef_array(:,:,4,sat_temp_index)
            vhs_values = quality*(sat_vhs_values(:,1)-sat_vhs_values(:,0))+sat_vhs_values(:,0)

            ! Return the requested properties that are valid in the two-phase region.
            qual = quality
            temp = sat_temp
            dens = 1.0/vhs_values(1)
            vol = vhs_values(1)
            entr = vhs_values(3)
            inte = vhs_values(2)-pres*vhs_values(1)  ! u = h-P*v

            ! Check to see if properties are requested that are only valid if quality is 0 or 1.
            if (real(quality,C_FLOAT) == 0.0) then  ! round to single precision to catch close values
                prop_values = ((water_sat_prop_coef_array(1,:,0,sat_temp_index)*temp_diff+water_sat_prop_coef_array(2,:,0,sat_temp_index))*temp_diff+water_sat_prop_coef_array(3,:,0,sat_temp_index))*temp_diff+water_sat_prop_coef_array(4,:,0,sat_temp_index)
                cv = prop_values(1)
                cp = prop_values(2)
                ssnd = prop_values(3)
                cond = prop_values(4)
                visc = prop_values(5)
            else if (real(quality,C_FLOAT) == 1.0) then  ! round to single precision to catch close values
                prop_values = ((water_sat_prop_coef_array(1,:,1,sat_temp_index)*temp_diff+water_sat_prop_coef_array(2,:,1,sat_temp_index))*temp_diff+water_sat_prop_coef_array(3,:,1,sat_temp_index))*temp_diff+water_sat_prop_coef_array(4,:,1,sat_temp_index)
                cv = prop_values(1)
                cp = prop_values(2)
                ssnd = prop_values(3)
                cond = prop_values(4)
                visc = prop_values(5)
            else  ! none of the properties below are available for a two-phase fluid
                cv = 0.0
                cp = 0.0
                ssnd = 0.0
                cond = 0.0
                visc = 0.0
            end if
        else
            
            ! -----LIQUID-----
                        
            ! Determine the pres_index row that contains the provided pressure and its fractional position, as well as the index of the first element in that row.
            call pres_find(pres,pres_index,pres_fraction)
            last_coef_index = water_liquid_entr_index_vector(pres_index+1)-pres_index-1  ! the index corresponding to the last element in the pressure row
            
            ! Check if the enthalpy lies in the gap between the vapor dome and the right-most element.
            enth_grid = (((((water_liquid_coef_array(4,4,3,last_coef_index)*pres_fraction+water_liquid_coef_array(3,4,3,last_coef_index))*pres_fraction+water_liquid_coef_array(2,4,3,last_coef_index))*pres_fraction+water_liquid_coef_array(1,4,3,last_coef_index))+((water_liquid_coef_array(4,3,3,last_coef_index)*pres_fraction+water_liquid_coef_array(3,3,3,last_coef_index))*pres_fraction+water_liquid_coef_array(2,3,3,last_coef_index))*pres_fraction+water_liquid_coef_array(1,3,3,last_coef_index))+((water_liquid_coef_array(4,2,3,last_coef_index)*pres_fraction+water_liquid_coef_array(3,2,3,last_coef_index))*pres_fraction+water_liquid_coef_array(2,2,3,last_coef_index))*pres_fraction+water_liquid_coef_array(1,2,3,last_coef_index))+((water_liquid_coef_array(4,1,3,last_coef_index)*pres_fraction+water_liquid_coef_array(3,1,3,last_coef_index))*pres_fraction+water_liquid_coef_array(2,1,3,last_coef_index))*pres_fraction+water_liquid_coef_array(1,1,3,last_coef_index)
            
            if (enth > enth_grid) then
                ! The given state is in the gap between the vapor dome and the 2D grid.  Calculate its fractional position within the gap and call the appropriate liquid_gap subroutines.
                entr_fraction = (enth-enth_grid)/(sat_enth(0)-enth_grid)  ! the fractional position with the gap, assuming linear interpolation                
                call sat_find(sat_temp,sat_temp_index,temp_diff)
                temp = liquid_gap_temp(entr_fraction,pres_fraction,sat_temp,last_coef_index)
                dens = liquid_gap_dens(entr_fraction,pres_fraction,temp_diff,sat_temp_index,last_coef_index)
                vol = 1.0/dens
                entr = liquid_gap_entr(entr_fraction,temp_diff,sat_temp_index,pres_index)
                inte = enth-pres/dens
                cv = liquid_gap_cv(entr_fraction,pres_fraction,temp_diff,sat_temp_index,last_coef_index)
                cp = liquid_gap_cp(entr_fraction,pres_fraction,temp_diff,sat_temp_index,last_coef_index)
                ssnd = liquid_gap_ssnd(entr_fraction,pres_fraction,temp_diff,sat_temp_index,last_coef_index)
                cond = liquid_gap_cond(entr_fraction,pres_fraction,temp_diff,sat_temp_index,last_coef_index)
                visc = liquid_gap_visc(entr_fraction,pres_fraction,temp_diff,sat_temp_index,last_coef_index)
                qual = -10.0
            else
                ! The given state is fully in the liquid region.  In order to find the correct entropy index for the given enthalpy it is necessary to calculate the enthalpy (adjusted for pressure) at the edges of all the elements in the row.
                first_coef_index = water_liquid_entr_index_vector(pres_index)-pres_index+1
                allocate(edge_enths(last_coef_index-first_coef_index+1))
                edge_enths = ((water_liquid_coef_array(4,1,3,first_coef_index:last_coef_index)*pres_fraction+water_liquid_coef_array(3,1,3,first_coef_index:last_coef_index))*pres_fraction+water_liquid_coef_array(2,1,3,first_coef_index:last_coef_index))*pres_fraction+water_liquid_coef_array(1,1,3,first_coef_index:last_coef_index)
                
                ! Check to make sure the provided enthalpy is not too small.
                if (enth < edge_enths(1)) then
                    error_code = 10
                    deallocate(edge_enths)
                    return
                end if
                
                ! Search the calculated temperatures to see which one contains the given temperature.
                row_entr_index = maxloc(edge_enths, 1, mask=edge_enths <= enth)  ! locate the largest value that is smaller than or equal to the given enthalpy
                coef_index = first_coef_index+row_entr_index-1  ! the coefficient index for this element

                ! Deallocate the edge temps vector
                deallocate(edge_enths)
                

                ! Solve the cubic equation for entr_fraction given the provided temperature.
                pres_adjusted_coefs = ((water_liquid_coef_array(4,:,3,coef_index)*pres_fraction+water_liquid_coef_array(3,:,3,coef_index))*pres_fraction+water_liquid_coef_array(2,:,3,coef_index))*pres_fraction+water_liquid_coef_array(1,:,3,coef_index)  ! the coefficients for a cubic equation that is only a function of entr_fraction
                call cubic_solution(pres_adjusted_coefs(1),pres_adjusted_coefs(2),pres_adjusted_coefs(3),pres_adjusted_coefs(4),enth,entr_fraction)
                
                ! Calculate the actual entropy from the fractional position (if requested).
                entr_index = water_liquid_entr_index_vector(pres_index)+row_entr_index-1  ! the entropy index for this element
                entr = water_liquid_entr_values(entr_index)+entr_fraction*(water_liquid_entr_values(entr_index+1)-water_liquid_entr_values(entr_index))

                ! The coefficients for the element containing the point and the fractional position of the point can now be used to calculate dependent properties.  Nested multiplication is used and the equations are explicitly written to make the compiled code faster.
                temp = (((((water_liquid_coef_array(4,4,1,coef_index)*pres_fraction+water_liquid_coef_array(3,4,1,coef_index))*pres_fraction+water_liquid_coef_array(2,4,1,coef_index))*pres_fraction+water_liquid_coef_array(1,4,1,coef_index)*entr_fraction)+((water_liquid_coef_array(4,3,1,coef_index)*pres_fraction+water_liquid_coef_array(3,3,1,coef_index))*pres_fraction+water_liquid_coef_array(2,3,1,coef_index))*pres_fraction+water_liquid_coef_array(1,3,1,coef_index))*entr_fraction+((water_liquid_coef_array(4,2,1,coef_index)*pres_fraction+water_liquid_coef_array(3,2,1,coef_index))*pres_fraction+water_liquid_coef_array(2,2,1,coef_index))*pres_fraction+water_liquid_coef_array(1,2,1,coef_index))*entr_fraction+((water_liquid_coef_array(4,1,1,coef_index)*pres_fraction+water_liquid_coef_array(3,1,1,coef_index))*pres_fraction+water_liquid_coef_array(2,1,1,coef_index))*pres_fraction+water_liquid_coef_array(1,1,1,coef_index)
                dens = (((((water_liquid_coef_array(4,4,2,coef_index)*pres_fraction+water_liquid_coef_array(3,4,2,coef_index))*pres_fraction+water_liquid_coef_array(2,4,2,coef_index))*pres_fraction+water_liquid_coef_array(1,4,2,coef_index)*entr_fraction)+((water_liquid_coef_array(4,3,2,coef_index)*pres_fraction+water_liquid_coef_array(3,3,2,coef_index))*pres_fraction+water_liquid_coef_array(2,3,2,coef_index))*pres_fraction+water_liquid_coef_array(1,3,2,coef_index))*entr_fraction+((water_liquid_coef_array(4,2,2,coef_index)*pres_fraction+water_liquid_coef_array(3,2,2,coef_index))*pres_fraction+water_liquid_coef_array(2,2,2,coef_index))*pres_fraction+water_liquid_coef_array(1,2,2,coef_index))*entr_fraction+((water_liquid_coef_array(4,1,2,coef_index)*pres_fraction+water_liquid_coef_array(3,1,2,coef_index))*pres_fraction+water_liquid_coef_array(2,1,2,coef_index))*pres_fraction+water_liquid_coef_array(1,1,2,coef_index)              
                vol = 1.0/dens
                cv = (((((water_liquid_coef_array(4,4,4,coef_index)*pres_fraction+water_liquid_coef_array(3,4,4,coef_index))*pres_fraction+water_liquid_coef_array(2,4,4,coef_index))*pres_fraction+water_liquid_coef_array(1,4,4,coef_index)*entr_fraction)+((water_liquid_coef_array(4,3,4,coef_index)*pres_fraction+water_liquid_coef_array(3,3,4,coef_index))*pres_fraction+water_liquid_coef_array(2,3,4,coef_index))*pres_fraction+water_liquid_coef_array(1,3,4,coef_index))*entr_fraction+((water_liquid_coef_array(4,2,4,coef_index)*pres_fraction+water_liquid_coef_array(3,2,4,coef_index))*pres_fraction+water_liquid_coef_array(2,2,4,coef_index))*pres_fraction+water_liquid_coef_array(1,2,4,coef_index))*entr_fraction+((water_liquid_coef_array(4,1,4,coef_index)*pres_fraction+water_liquid_coef_array(3,1,4,coef_index))*pres_fraction+water_liquid_coef_array(2,1,4,coef_index))*pres_fraction+water_liquid_coef_array(1,1,4,coef_index)              
                cp = (((((water_liquid_coef_array(4,4,5,coef_index)*pres_fraction+water_liquid_coef_array(3,4,5,coef_index))*pres_fraction+water_liquid_coef_array(2,4,5,coef_index))*pres_fraction+water_liquid_coef_array(1,4,5,coef_index)*entr_fraction)+((water_liquid_coef_array(4,3,5,coef_index)*pres_fraction+water_liquid_coef_array(3,3,5,coef_index))*pres_fraction+water_liquid_coef_array(2,3,5,coef_index))*pres_fraction+water_liquid_coef_array(1,3,5,coef_index))*entr_fraction+((water_liquid_coef_array(4,2,5,coef_index)*pres_fraction+water_liquid_coef_array(3,2,5,coef_index))*pres_fraction+water_liquid_coef_array(2,2,5,coef_index))*pres_fraction+water_liquid_coef_array(1,2,5,coef_index))*entr_fraction+((water_liquid_coef_array(4,1,5,coef_index)*pres_fraction+water_liquid_coef_array(3,1,5,coef_index))*pres_fraction+water_liquid_coef_array(2,1,5,coef_index))*pres_fraction+water_liquid_coef_array(1,1,5,coef_index)              
                ssnd = (((((water_liquid_coef_array(4,4,6,coef_index)*pres_fraction+water_liquid_coef_array(3,4,6,coef_index))*pres_fraction+water_liquid_coef_array(2,4,6,coef_index))*pres_fraction+water_liquid_coef_array(1,4,6,coef_index)*entr_fraction)+((water_liquid_coef_array(4,3,6,coef_index)*pres_fraction+water_liquid_coef_array(3,3,6,coef_index))*pres_fraction+water_liquid_coef_array(2,3,6,coef_index))*pres_fraction+water_liquid_coef_array(1,3,6,coef_index))*entr_fraction+((water_liquid_coef_array(4,2,6,coef_index)*pres_fraction+water_liquid_coef_array(3,2,6,coef_index))*pres_fraction+water_liquid_coef_array(2,2,6,coef_index))*pres_fraction+water_liquid_coef_array(1,2,6,coef_index))*entr_fraction+((water_liquid_coef_array(4,1,6,coef_index)*pres_fraction+water_liquid_coef_array(3,1,6,coef_index))*pres_fraction+water_liquid_coef_array(2,1,6,coef_index))*pres_fraction+water_liquid_coef_array(1,1,6,coef_index)              
                cond = (((((water_liquid_coef_array(4,4,7,coef_index)*pres_fraction+water_liquid_coef_array(3,4,7,coef_index))*pres_fraction+water_liquid_coef_array(2,4,7,coef_index))*pres_fraction+water_liquid_coef_array(1,4,7,coef_index)*entr_fraction)+((water_liquid_coef_array(4,3,7,coef_index)*pres_fraction+water_liquid_coef_array(3,3,7,coef_index))*pres_fraction+water_liquid_coef_array(2,3,7,coef_index))*pres_fraction+water_liquid_coef_array(1,3,7,coef_index))*entr_fraction+((water_liquid_coef_array(4,2,7,coef_index)*pres_fraction+water_liquid_coef_array(3,2,7,coef_index))*pres_fraction+water_liquid_coef_array(2,2,7,coef_index))*pres_fraction+water_liquid_coef_array(1,2,7,coef_index))*entr_fraction+((water_liquid_coef_array(4,1,7,coef_index)*pres_fraction+water_liquid_coef_array(3,1,7,coef_index))*pres_fraction+water_liquid_coef_array(2,1,7,coef_index))*pres_fraction+water_liquid_coef_array(1,1,7,coef_index)              
                visc = (((((water_liquid_coef_array(4,4,8,coef_index)*pres_fraction+water_liquid_coef_array(3,4,8,coef_index))*pres_fraction+water_liquid_coef_array(2,4,8,coef_index))*pres_fraction+water_liquid_coef_array(1,4,8,coef_index)*entr_fraction)+((water_liquid_coef_array(4,3,8,coef_index)*pres_fraction+water_liquid_coef_array(3,3,8,coef_index))*pres_fraction+water_liquid_coef_array(2,3,8,coef_index))*pres_fraction+water_liquid_coef_array(1,3,8,coef_index))*entr_fraction+((water_liquid_coef_array(4,2,8,coef_index)*pres_fraction+water_liquid_coef_array(3,2,8,coef_index))*pres_fraction+water_liquid_coef_array(2,2,8,coef_index))*pres_fraction+water_liquid_coef_array(1,2,8,coef_index))*entr_fraction+((water_liquid_coef_array(4,1,8,coef_index)*pres_fraction+water_liquid_coef_array(3,1,8,coef_index))*pres_fraction+water_liquid_coef_array(2,1,8,coef_index))*pres_fraction+water_liquid_coef_array(1,1,8,coef_index)              
                inte = enth-pres/dens
                qual = -10.0
            end if


        end if

     
    else
        
        ! -----SUPERCRITICAL-----
        error_code = 99
    end if
    
    
end subroutine water_PH_f90

subroutine water_TQ_f90(temp,qual,error_code,pres,dens,vol,inte,enth,entr,cv,cp,cond,visc,ssnd) bind(C)
    ! Subroutine used to determine water properties from bicubic interpolation tables, given temperature and quality.  Inputs can be an array if outputs are of a conforming shape and size.
    ! Note: this subroutine does not write to water_error_code or water_error_message.
    ! Required Inputs:
    !   temp = temperature (C)
    !   qual = quality (mass basis, between 0 and 1)
    ! Required Outputs:
    !   error_code = an integer(C_INT) error code that can be used with the get_error_message subroutine
    ! Optional Outputs:
    !   pres = pressure (kPa)
    !   dens = density (kg/m3)
    !   vol = specific volume (m3/kg)
    !   inte = internal energy (kJ/kg)
    !   enth = enthalpy (kJ/kg)
    !   entr = entropy (kJ/kg-K)
    !   cv = specific heat at constant volume (kJ/kg-K) [only available at a quality of 0 or 1]
    !   cp = specific heat at constant pressure (kJ/kg-K) [only available at a quality of 0 or 1]
    !   cond = thermal conductivity in the fluid (W/m-K) [only available at a quality of 0 or 1]
    !   visc = viscosity (Pa-s or kg/m-s) [only available at a quality of 0 or 1]            
    !   ssnd = speed of sound in the fluid (m/s) [only available at a quality of 0 or 1]            
    
    ! Required Argument Declarations
    real(C_DOUBLE), intent(in) :: temp, qual
    integer(C_INT), intent(out) :: error_code
            
    ! Optional Argument Declarations
    real(C_DOUBLE), intent(out) :: pres, dens, vol, inte, enth, entr, cv, cp, cond, visc, ssnd
    
    ! Internal Variable Declarations
    integer(C_INT) :: sat_temp_index
    real(C_DOUBLE) :: temp_diff
    real(C_DOUBLE), dimension(3,0:1) :: sat_vhs_values
    real(C_DOUBLE), dimension(3) :: vhs_values
    real(C_DOUBLE), dimension(5) :: prop_values
    
    ! Initialize Error Flag
    error_code = 0
    
    ! Check Quality Input
    if (qual > 1.0 .or. qual < 0.0) then
        error_code = 1
        return
    end if
    
    ! Check Temperature Input
    if (temp > water_max_sat_temp .or. temp < water_min_sat_temp) then
        error_code = 2
        return
    end if
    
    ! Locate the interval containing the given temperature and calculate the difference between the left temperature and the given temperature.
    call sat_find(temp,sat_temp_index,temp_diff)
    
    ! Calculate the saturated liquid and vapor values for volume, enthalpy, and entropy using vectorized nested multiplication.
    sat_vhs_values = ((water_sat_vhs_coef_array(:,:,1,sat_temp_index)*temp_diff+water_sat_vhs_coef_array(:,:,2,sat_temp_index))*temp_diff+water_sat_vhs_coef_array(:,:,3,sat_temp_index))*temp_diff+water_sat_vhs_coef_array(:,:,4,sat_temp_index)
    
    ! Calculate the values of volume, enthalpy, and entropy given quality.
    vhs_values = qual*(sat_vhs_values(:,1)-sat_vhs_values(:,0))+sat_vhs_values(:,0)
    
    ! Return the requested properties that are valid in the two-phase region.
    pres = ((water_sat_pres_coef_array(1,sat_temp_index)*temp_diff+water_sat_pres_coef_array(2,sat_temp_index))*temp_diff+water_sat_pres_coef_array(3,sat_temp_index))*temp_diff+water_sat_pres_coef_array(4,sat_temp_index)
    dens = 1.0/vhs_values(1)
    vol = vhs_values(1)
    enth = vhs_values(2)
    entr = vhs_values(3)
    inte = enth-pres*vol    
    ! Check to see if properties are requested that are only valid if quality is 0 or 1.
    if (real(qual,C_FLOAT) == 0.0) then  ! round to single precision to catch close values
        prop_values = ((water_sat_prop_coef_array(1,:,0,sat_temp_index)*temp_diff+water_sat_prop_coef_array(2,:,0,sat_temp_index))*temp_diff+water_sat_prop_coef_array(3,:,0,sat_temp_index))*temp_diff+water_sat_prop_coef_array(4,:,0,sat_temp_index)
        cv = prop_values(1)
        cp = prop_values(2)
        ssnd = prop_values(3)
        cond = prop_values(4)
        visc = prop_values(5)
    else if (real(qual,C_FLOAT) == 1.0) then  ! round to single precision to catch close values
        prop_values = ((water_sat_prop_coef_array(1,:,1,sat_temp_index)*temp_diff+water_sat_prop_coef_array(2,:,1,sat_temp_index))*temp_diff+water_sat_prop_coef_array(3,:,1,sat_temp_index))*temp_diff+water_sat_prop_coef_array(4,:,1,sat_temp_index)
        cv = prop_values(1)
        cp = prop_values(2)
        ssnd = prop_values(3)
        cond = prop_values(4)
        visc = prop_values(5)
    else  ! none of the properties below are available for a two-phase fluid
        cv = 0.0
        cp = 0.0
        ssnd = 0.0
        cond = 0.0
        visc = 0.0
    end if
    
    return  ! return to the calling program
end subroutine water_TQ_f90

subroutine water_PQ_f90(pres,qual,error_code,temp,dens,vol,inte,enth,entr,cv,cp,cond,visc,ssnd) bind(C)
    ! Subroutine used to determine water properties from bicubic interpolation tables, given pressure and quality.  Inputs can be an array if outputs are of a conforming shape and size.
    ! Note: this subroutine does not write to water_error_code or water_error_message.
    ! Required Inputs:
    !   pres = pressure (kPa)
    !   qual = quality (mass basis, between 0 and 1)
    ! Required Outputs:
    !   error_code = an integer(C_INT) error code that can be used with the get_error_message subroutine
    ! Optional Outputs:
    !   temp = temperature (C)
    !   dens = density (kg/m3)
    !   vol = specific volume (m3/kg)
    !   inte = internal energy (kJ/kg)
    !   enth = enthalpy (kJ/kg)
    !   entr = entropy (kJ/kg-K)
    !   cv = specific heat at constant volume (kJ/kg-K) [only available at a quality of 0 or 1]
    !   cp = specific heat at constant pressure (kJ/kg-K) [only available at a quality of 0 or 1]
    !   cond = thermal conductivity in the fluid (W/m-K) [only available at a quality of 0 or 1]
    !   visc = viscosity (Pa-s or kg/m-s) [only available at a quality of 0 or 1]            
    !   ssnd = speed of sound in the fluid (m/s) [only available at a quality of 0 or 1]            
    
    ! Required Argument Declarations
    real(C_DOUBLE), intent(in) :: pres, qual
    integer(C_INT), intent(out) :: error_code
            
    ! Optional Argument Declarations
    real(C_DOUBLE), intent(out) :: temp, dens, vol, inte, enth, entr, cv, cp, cond, visc, ssnd
    
    ! Internal Variable Declarations
    integer(C_INT) :: sat_temp_index
    real(C_DOUBLE) :: sat_temp, temp_diff
    real(C_DOUBLE), dimension(3,0:1) :: sat_vhs_values
    real(C_DOUBLE), dimension(3) :: vhs_values
    real(C_DOUBLE), dimension(5) :: prop_values
    
    ! Initialize Error Flag
    error_code = 0
    
    ! Check Quality Input
    if (qual > 1.0 .or. qual < 0.0) then
        error_code = 1
        return
    end if
    
    ! Check Pressure Input
    if (pres > water_max_sat_pres .or. pres < water_min_sat_pres) then
        error_code = 3
        return
    end if
    
    ! Calculate the saturation temperature at the given pressure.
    sat_temp = t_sat(pres)

    ! Locate the interval containing the given temperature and calculate the difference between the left temperature and the given temperature.
    call sat_find(sat_temp,sat_temp_index,temp_diff)
    
    ! Calculate the saturated liquid and vapor values for volume, enthalpy, and entropy using vectorized nested multiplication.
    sat_vhs_values = ((water_sat_vhs_coef_array(:,:,1,sat_temp_index)*temp_diff+water_sat_vhs_coef_array(:,:,2,sat_temp_index))*temp_diff+water_sat_vhs_coef_array(:,:,3,sat_temp_index))*temp_diff+water_sat_vhs_coef_array(:,:,4,sat_temp_index)
    
    ! Calculate the values of volume, enthalpy, and entropy given quality.
    vhs_values = qual*(sat_vhs_values(:,1)-sat_vhs_values(:,0))+sat_vhs_values(:,0)
    
    ! Return the requested properties that are valid in the two-phase region.
    temp = sat_temp
    dens = 1.0/vhs_values(1)
    vol = vhs_values(1)
    enth = vhs_values(2)
    entr = vhs_values(3)
    inte = vhs_values(2)-pres*vhs_values(1)
    
    ! Check to see if properties are requested that are only valid if quality is 0 or 1.
    if (real(qual,C_FLOAT) == 0.0) then  ! round to single precision to catch close values
        prop_values = ((water_sat_prop_coef_array(1,:,0,sat_temp_index)*temp_diff+water_sat_prop_coef_array(2,:,0,sat_temp_index))*temp_diff+water_sat_prop_coef_array(3,:,0,sat_temp_index))*temp_diff+water_sat_prop_coef_array(4,:,0,sat_temp_index)
        cv = prop_values(1)
        cp = prop_values(2)
        ssnd = prop_values(3)
        cond = prop_values(4)
        visc = prop_values(5)
    else if (real(qual,C_FLOAT) == 1.0) then  ! round to single precision to catch close values
        prop_values = ((water_sat_prop_coef_array(1,:,1,sat_temp_index)*temp_diff+water_sat_prop_coef_array(2,:,1,sat_temp_index))*temp_diff+water_sat_prop_coef_array(3,:,1,sat_temp_index))*temp_diff+water_sat_prop_coef_array(4,:,1,sat_temp_index)
        cv = prop_values(1)
        cp = prop_values(2)
        ssnd = prop_values(3)
        cond = prop_values(4)
        visc = prop_values(5)
    else  ! none of the properties below are available for a two-phase fluid
        cv = 0.0
        cp = 0.0
        ssnd = 0.0
        cond = 0.0
        visc = 0.0
    end if
    
    return  ! return to the calling program
end subroutine water_PQ_f90

subroutine cubic_solution(coef_1,coef_2,coef_3,coef_4,value,root) bind(C)
    ! Subroutine used to calculate the root (that should lie between 0 and 1) of a cubic equation.  The following is adapted from Numerical Recipes in Fortran, 
    ! with the long real numbers corresponding to precomputed division (eg, 1/3 = 0.33333333...) or multiplicatione (eg, 2*pi = 6.283185307...).
    ! Required Inputs:
    !   coef_1 = the first coefficient, with: (4)*x^3+(3)*x^2+(2)*x+(1) = y
    !   coef_2 = the second coefficient
    !   coef_3 = the third coefficient
    !   coef_4 = the fourth coefficient
    !   value = the cubic equation will be solved for this value (y)
    ! Required Outputss
    !   root = the root of the cubic that ideally lies between 0 and 1

    real(C_DOUBLE), intent(in) :: coef_1,coef_2,coef_3,coef_4,value
    real(C_DOUBLE), intent(out) :: root
    real(C_DOUBLE) :: one_over_fourth_coef, cubic_a, cubic_b, cubic_c, cubic_q, cubic_r, cubic_q_cubed, cubic_r_squared, theta, root_basis, root_1, root_2, root_3, root_a, root_b, min_abs_root

    one_over_fourth_coef = 1.0/coef_4    ! precalculate to reduce division operations
    cubic_a = coef_3*one_over_fourth_coef
    cubic_b = coef_2*one_over_fourth_coef
    cubic_c = (coef_1-value)*one_over_fourth_coef
    cubic_q = (cubic_a*cubic_a-3*cubic_b)*0.11111111111111111111 ! same as dividing by 9
    cubic_r = (2*cubic_a*cubic_a*cubic_a-9*cubic_a*cubic_b+27*cubic_c)*0.0185185185185185185185 ! same as dividing by 54
    cubic_r_squared = cubic_r*cubic_r
    cubic_q_cubed = cubic_q*cubic_q*cubic_q
    if (cubic_r_squared < cubic_q_cubed) then ! there are three roots for the given cubic (only one will lie between 0 and 1, the logic below is used to reduce unnecessary calculations)
        theta = acos(cubic_r/sqrt(cubic_q_cubed))
        root_basis = -2*sqrt(cubic_q)   ! precalculate to reduce sqrt operations
        root_1 = root_basis*cos(theta*0.33333333333333333333)-cubic_a*0.33333333333333333333
        if ((root_1 >= 0.0).and.(root_1 <= 1.0)) then
            root = root_1  ! this is the valid root
        else
            root_2 = root_basis*cos((theta+6.28318530717958647693)*0.33333333333333333333)-cubic_a*0.33333333333333333333
            if ((root_2 >= 0.0).and.(root_2 <= 1.0)) then
                root = root_2 ! this is the valid root
            else
                root_3 = root_basis*cos((theta-6.28318530717958647693)*0.33333333333333333333)-cubic_a*0.33333333333333333333
                if ((root_3 >= 0.0).and.(root_3 <= 1.0)) then
                    root = root_3 ! this is the valid root
                else ! none of the roots were between 0 and 1 - this shouldn't happen but might due to rounding errors if value is right on the edge
                    min_abs_root = min(abs(root_1),abs(root_2),abs(root_3))
                    if (abs(root_1) == min_abs_root) then
                        root = root_1
                    else if (abs(root_2) == min_abs_root) then
                        root = root_2
                    else
                        root = root_3
                    end if
                end if
            end if
        end if
    else    ! there is only one root for the given cubic                        
        root_a = -sign(1.0_C_DOUBLE,cubic_r)*(abs(cubic_r)+sqrt(cubic_r_squared-cubic_q_cubed))**(1.0/3.0)
        if (abs(root_a) < 1e-9) then  ! check for zero (or near zero) value of root_a
            root_b = 0.0
        else
            root_b = cubic_q/root_a
        end if
        root = (root_a+root_b)-cubic_a*0.33333333333333333333
    end if
    
    return
end subroutine cubic_solution


function t_sat(pres)
    ! Returns the saturation temperature at the given pressure.
    ! No bounds checking is performed and the function will not adjust the highest index - it assumes the pressure vector extends beyond the maximum pressure that will be given.
    ! Required Inputs:
    !   pres = pressure (kPa)
    ! Returns:
    !   temp = saturation temperature (C)
    
    real(C_DOUBLE), intent(in) :: pres
    real(C_DOUBLE) :: t_sat
    integer(C_INT) :: index
    real(C_DOUBLE) :: pres_diff
    
    ! Locate the interval containing the given pressure and calculate the difference between the left pressure and the given pressure.
    index = 0
    if (pres >= water_sat_pres_vector(index+64)) index = index+64
    if (pres >= water_sat_pres_vector(index+32)) index = index+32
    if (pres >= water_sat_pres_vector(index+16)) index = index+16
    if (pres >= water_sat_pres_vector(index+8)) index = index+8
    if (pres >= water_sat_pres_vector(index+4)) index = index+4
    if (pres >= water_sat_pres_vector(index+2)) index = index+2
    if (pres >= water_sat_pres_vector(index+1)) index = index+1
    pres_diff = pres-water_sat_pres_vector(index)
    
    ! The saturation temperature corresponding to the given pressure is calculated.
    
    ! Calculate the saturation temperature using nested multiplication.
    t_sat = ((water_sat_temp_coef_array(1,index)*pres_diff+water_sat_temp_coef_array(2,index))*pres_diff+water_sat_temp_coef_array(3,index))*pres_diff+water_sat_temp_coef_array(4,index)    
    return
end function t_sat
    
    

subroutine sat_find(temp,sat_temp_index,temp_diff) bind(C)
    ! Subroutine used to perform a binary search of sat_temp_vector (with length of 127), looking for the index of the element containing the provided temperature.
    ! Note: this suboutine will return 0 if the temperature is less than the lowest value, and if the temperature is exactly equal to the highest value then it will return one less than expected (this is on purpose to catch upper limit cases).
    ! Required Inputs:
    !   temp = saturation temperature
    ! Required Outputs:
    !   sat_temp_index = the element containing the value, referencing the smaller index value (left side of element)
    !   temp_diff = the temperature difference between the given temperature and the smaller element bound
    
    real(C_DOUBLE), intent(in) :: temp
    integer(C_INT), intent(out) :: sat_temp_index
    real(C_DOUBLE), intent(out) :: temp_diff
    integer(C_INT) :: index
    
    ! Locate the interval containing the given temperature.
    index = 0
    if (temp >= water_sat_temp_vector(index+64)) index = index+64
    if (temp >= water_sat_temp_vector(index+32)) index = index+32
    if (temp >= water_sat_temp_vector(index+16)) index = index+16
    if (temp >= water_sat_temp_vector(index+8)) index = index+8
    if (temp >= water_sat_temp_vector(index+4)) index = index+4
    if (temp >= water_sat_temp_vector(index+2)) index = index+2
    if (temp >= water_sat_temp_vector(index+1)) index = index+1
    if (index == 127 .and. temp == water_sat_temp_vector(127)) index = 126
    
    ! Calculate the difference between the left temperature and the given temperature.
    sat_temp_index = index
    temp_diff = temp-water_sat_temp_vector(sat_temp_index)
    return
end subroutine sat_find

subroutine pres_find(pres,pres_index,pres_fraction) bind(C)
    ! Subroutine used to perform a binary search of pres_vector (with length of 127), looking for the index of the element containing the provided pressure.
    ! Note: this suboutine will return 0 if the pressure is less than the lowest value, and if the pressure is exactly equal to the highest value then it will return one less than expected (this is on purpose to catch upper limit cases).
    ! Required Inputs:
    !   pres = pressure
    ! Required Outputs:
    !   pres_index = the element containing the value, referencing the smaller index value (left side of element)
    !   pres_fraction = the fractional position within the element (between 0 and 1)
    
    real(C_DOUBLE), intent(in) :: pres
    integer(C_INT), intent(out) :: pres_index
    real(C_DOUBLE), intent(out) :: pres_fraction
    integer(C_INT) :: index
    
    ! Locate the interval containing the given temperature.
    index = 0
    if (pres >= water_pres_vector(index+64)) index = index+64
    if (pres >= water_pres_vector(index+32)) index = index+32
    if (pres >= water_pres_vector(index+16)) index = index+16
    if (pres >= water_pres_vector(index+8)) index = index+8
    if (pres >= water_pres_vector(index+4)) index = index+4
    if (pres >= water_pres_vector(index+2)) index = index+2
    if (pres >= water_pres_vector(index+1)) index = index+1
    if (index == 127 .and. pres == water_pres_vector(127)) index = 126
    
    ! Calculate the fractional position within the element.
    pres_fraction = (pres-water_pres_vector(index))/(water_pres_vector(index+1)-water_pres_vector(index))
    pres_index = index
    
    return
end subroutine pres_find

function vapor_gap_temp(entr_fraction,pres_fraction,sat_temp,first_coef_index)
    ! Returns the temperature in the vapor gap given the entropy fractional position (uses linear interpolation).
    
    real(C_DOUBLE), intent(in) :: entr_fraction, pres_fraction, sat_temp
    integer(C_INT), intent(in) :: first_coef_index
    real(C_DOUBLE) :: vapor_gap_temp, grid
    
    grid = ((water_vapor_coef_array(4,1,1,first_coef_index)*pres_fraction+water_vapor_coef_array(3,1,1,first_coef_index))*pres_fraction+water_vapor_coef_array(2,1,1,first_coef_index))*pres_fraction+water_vapor_coef_array(1,1,1,first_coef_index)
    vapor_gap_temp = sat_temp+(grid-sat_temp)*entr_fraction
end function vapor_gap_temp

function vapor_gap_dens(entr_fraction,pres_fraction,temp_diff,sat_temp_index,first_coef_index)
    ! Returns the density in the vapor gap given the entropy fractional position (uses linear interpolation).
    
    real(C_DOUBLE), intent(in) :: entr_fraction, pres_fraction, temp_diff
    integer(C_INT), intent(in) :: sat_temp_index, first_coef_index
    real(C_DOUBLE) :: vapor_gap_dens, dome, grid

    dome = 1.0/(((water_sat_vhs_coef_array(1,1,1,sat_temp_index)*temp_diff+water_sat_vhs_coef_array(1,1,2,sat_temp_index))*temp_diff+water_sat_vhs_coef_array(1,1,3,sat_temp_index))*temp_diff+water_sat_vhs_coef_array(1,1,4,sat_temp_index))
    grid = ((water_vapor_coef_array(4,1,2,first_coef_index)*pres_fraction+water_vapor_coef_array(3,1,2,first_coef_index))*pres_fraction+water_vapor_coef_array(2,1,2,first_coef_index))*pres_fraction+water_vapor_coef_array(1,1,2,first_coef_index)
    vapor_gap_dens = dome+(grid-dome)*entr_fraction    
end function vapor_gap_dens

function vapor_gap_enth(entr_fraction,pres_fraction,temp_diff,sat_temp_index,first_coef_index)
    ! Returns the enthalpy in the vapor gap given the entropy fractional position (uses linear interpolation).
    
    real(C_DOUBLE), intent(in) :: entr_fraction, pres_fraction, temp_diff
    integer(C_INT), intent(in) :: sat_temp_index, first_coef_index
    real(C_DOUBLE) :: vapor_gap_enth, dome, grid

    dome = ((water_sat_vhs_coef_array(2,1,1,sat_temp_index)*temp_diff+water_sat_vhs_coef_array(2,1,2,sat_temp_index))*temp_diff+water_sat_vhs_coef_array(2,1,3,sat_temp_index))*temp_diff+water_sat_vhs_coef_array(2,1,4,sat_temp_index)
    grid = ((water_vapor_coef_array(4,1,3,first_coef_index)*pres_fraction+water_vapor_coef_array(3,1,3,first_coef_index))*pres_fraction+water_vapor_coef_array(2,1,3,first_coef_index))*pres_fraction+water_vapor_coef_array(1,1,3,first_coef_index)
    vapor_gap_enth = dome+(grid-dome)*entr_fraction
end function vapor_gap_enth

function vapor_gap_cv(entr_fraction,pres_fraction,temp_diff,sat_temp_index,first_coef_index)
    ! Returns the cv in the vapor gap given the entropy fractional position (uses linear interpolation).
    
    real(C_DOUBLE), intent(in) :: entr_fraction, pres_fraction, temp_diff
    integer(C_INT), intent(in) :: sat_temp_index, first_coef_index
    real(C_DOUBLE) :: vapor_gap_cv, dome, grid

    dome = ((water_sat_prop_coef_array(1,1,1,sat_temp_index)*temp_diff+water_sat_prop_coef_array(2,1,1,sat_temp_index))*temp_diff+water_sat_prop_coef_array(3,1,1,sat_temp_index))*temp_diff+water_sat_prop_coef_array(4,1,1,sat_temp_index)
    grid = ((water_vapor_coef_array(4,1,4,first_coef_index)*pres_fraction+water_vapor_coef_array(3,1,4,first_coef_index))*pres_fraction+water_vapor_coef_array(2,1,4,first_coef_index))*pres_fraction+water_vapor_coef_array(1,1,4,first_coef_index)
    vapor_gap_cv = dome+(grid-dome)*entr_fraction
end function vapor_gap_cv

function vapor_gap_cp(entr_fraction,pres_fraction,temp_diff,sat_temp_index,first_coef_index)
    ! Returns the cp in the vapor gap given the entropy fractional position (uses linear interpolation).
    
    real(C_DOUBLE), intent(in) :: entr_fraction, pres_fraction, temp_diff
    integer(C_INT), intent(in) :: sat_temp_index, first_coef_index
    real(C_DOUBLE) :: vapor_gap_cp, dome, grid

    dome = ((water_sat_prop_coef_array(1,2,1,sat_temp_index)*temp_diff+water_sat_prop_coef_array(2,2,1,sat_temp_index))*temp_diff+water_sat_prop_coef_array(3,2,1,sat_temp_index))*temp_diff+water_sat_prop_coef_array(4,2,1,sat_temp_index)
    grid = ((water_vapor_coef_array(4,1,5,first_coef_index)*pres_fraction+water_vapor_coef_array(3,1,5,first_coef_index))*pres_fraction+water_vapor_coef_array(2,1,5,first_coef_index))*pres_fraction+water_vapor_coef_array(1,1,5,first_coef_index)
    vapor_gap_cp = dome+(grid-dome)*entr_fraction
end function vapor_gap_cp

function vapor_gap_ssnd(entr_fraction,pres_fraction,temp_diff,sat_temp_index,first_coef_index)
    ! Returns the ssnd in the vapor gap given the entropy fractional position (uses linear interpolation).
    
    real(C_DOUBLE), intent(in) :: entr_fraction, pres_fraction, temp_diff
    integer(C_INT), intent(in) :: sat_temp_index, first_coef_index
    real(C_DOUBLE) :: vapor_gap_ssnd, dome, grid

    dome = ((water_sat_prop_coef_array(1,3,1,sat_temp_index)*temp_diff+water_sat_prop_coef_array(2,3,1,sat_temp_index))*temp_diff+water_sat_prop_coef_array(3,3,1,sat_temp_index))*temp_diff+water_sat_prop_coef_array(4,3,1,sat_temp_index)
    grid = ((water_vapor_coef_array(4,1,6,first_coef_index)*pres_fraction+water_vapor_coef_array(3,1,6,first_coef_index))*pres_fraction+water_vapor_coef_array(2,1,6,first_coef_index))*pres_fraction+water_vapor_coef_array(1,1,6,first_coef_index)
    vapor_gap_ssnd = dome+(grid-dome)*entr_fraction
end function vapor_gap_ssnd

function vapor_gap_cond(entr_fraction,pres_fraction,temp_diff,sat_temp_index,first_coef_index)
    ! Returns the cond in the vapor gap given the entropy fractional position (uses linear interpolation).
    
    real(C_DOUBLE), intent(in) :: entr_fraction, pres_fraction, temp_diff
    integer(C_INT), intent(in) :: sat_temp_index, first_coef_index
    real(C_DOUBLE) :: vapor_gap_cond, dome, grid

    dome = ((water_sat_prop_coef_array(1,4,1,sat_temp_index)*temp_diff+water_sat_prop_coef_array(2,4,1,sat_temp_index))*temp_diff+water_sat_prop_coef_array(3,4,1,sat_temp_index))*temp_diff+water_sat_prop_coef_array(4,4,1,sat_temp_index)
    grid = ((water_vapor_coef_array(4,1,7,first_coef_index)*pres_fraction+water_vapor_coef_array(3,1,7,first_coef_index))*pres_fraction+water_vapor_coef_array(2,1,7,first_coef_index))*pres_fraction+water_vapor_coef_array(1,1,7,first_coef_index)
    vapor_gap_cond = dome+(grid-dome)*entr_fraction
end function vapor_gap_cond

function vapor_gap_visc(entr_fraction,pres_fraction,temp_diff,sat_temp_index,first_coef_index)
    ! Returns the visc in the vapor gap given the entropy fractional position (uses linear interpolation).
    
    real(C_DOUBLE), intent(in) :: entr_fraction, pres_fraction, temp_diff
    integer(C_INT), intent(in) :: sat_temp_index, first_coef_index
    real(C_DOUBLE) :: vapor_gap_visc, dome, grid

    dome = ((water_sat_prop_coef_array(1,5,1,sat_temp_index)*temp_diff+water_sat_prop_coef_array(2,5,1,sat_temp_index))*temp_diff+water_sat_prop_coef_array(3,5,1,sat_temp_index))*temp_diff+water_sat_prop_coef_array(4,5,1,sat_temp_index)
    grid = ((water_vapor_coef_array(4,1,8,first_coef_index)*pres_fraction+water_vapor_coef_array(3,1,8,first_coef_index))*pres_fraction+water_vapor_coef_array(2,1,8,first_coef_index))*pres_fraction+water_vapor_coef_array(1,1,8,first_coef_index)
    vapor_gap_visc = dome+(grid-dome)*entr_fraction
end function vapor_gap_visc

function vapor_gap_entr(entr_fraction,temp_diff,sat_temp_index,pres_index)
    ! Returns the visc in the vapor gap given the entropy fractional position (uses linear interpolation).
    
    real(C_DOUBLE), intent(in) :: entr_fraction, temp_diff
    integer(C_INT), intent(in) :: sat_temp_index, pres_index
    real(C_DOUBLE) :: vapor_gap_entr, dome

    dome = ((water_sat_vhs_coef_array(3,1,1,sat_temp_index)*temp_diff+water_sat_vhs_coef_array(3,1,2,sat_temp_index))*temp_diff+water_sat_vhs_coef_array(3,1,3,sat_temp_index))*temp_diff+water_sat_vhs_coef_array(3,1,4,sat_temp_index)
    vapor_gap_entr = dome+(water_vapor_entr_values(water_vapor_entr_index_vector(pres_index))-dome)*entr_fraction
end function vapor_gap_entr

function liquid_gap_temp(entr_fraction,pres_fraction,sat_temp,last_coef_index)
    ! Returns the temperature in the liquid gap given the entropy fractional position (uses linear interpolation).
    
    real(C_DOUBLE), intent(in) :: entr_fraction, pres_fraction, sat_temp
    integer(C_INT), intent(in) :: last_coef_index
    real(C_DOUBLE) :: liquid_gap_temp, grid
    
    grid = (((((water_liquid_coef_array(4,4,1,last_coef_index)*pres_fraction+water_liquid_coef_array(3,4,1,last_coef_index))*pres_fraction+water_liquid_coef_array(2,4,1,last_coef_index))*pres_fraction+water_liquid_coef_array(1,4,1,last_coef_index))+((water_liquid_coef_array(4,3,1,last_coef_index)*pres_fraction+water_liquid_coef_array(3,3,1,last_coef_index))*pres_fraction+water_liquid_coef_array(2,3,1,last_coef_index))*pres_fraction+water_liquid_coef_array(1,3,1,last_coef_index))+((water_liquid_coef_array(4,2,1,last_coef_index)*pres_fraction+water_liquid_coef_array(3,2,1,last_coef_index))*pres_fraction+water_liquid_coef_array(2,2,1,last_coef_index))*pres_fraction+water_liquid_coef_array(1,2,1,last_coef_index))+((water_liquid_coef_array(4,1,1,last_coef_index)*pres_fraction+water_liquid_coef_array(3,1,1,last_coef_index))*pres_fraction+water_liquid_coef_array(2,1,1,last_coef_index))*pres_fraction+water_liquid_coef_array(1,1,1,last_coef_index)
    liquid_gap_temp = grid+(sat_temp-grid)*entr_fraction
end function liquid_gap_temp

function liquid_gap_dens(entr_fraction,pres_fraction,temp_diff,sat_temp_index,last_coef_index)
    ! Returns the density in the liquid gap given the entropy fractional position (uses linear interpolation).
    
    real(C_DOUBLE), intent(in) :: entr_fraction, pres_fraction, temp_diff
    integer(C_INT), intent(in) :: sat_temp_index, last_coef_index
    real(C_DOUBLE) :: liquid_gap_dens, dome, grid

    dome = 1.0/(((water_sat_vhs_coef_array(1,0,1,sat_temp_index)*temp_diff+water_sat_vhs_coef_array(1,0,2,sat_temp_index))*temp_diff+water_sat_vhs_coef_array(1,0,3,sat_temp_index))*temp_diff+water_sat_vhs_coef_array(1,0,4,sat_temp_index))
    grid = (((((water_liquid_coef_array(4,4,2,last_coef_index)*pres_fraction+water_liquid_coef_array(3,4,2,last_coef_index))*pres_fraction+water_liquid_coef_array(2,4,2,last_coef_index))*pres_fraction+water_liquid_coef_array(1,4,2,last_coef_index))+((water_liquid_coef_array(4,3,2,last_coef_index)*pres_fraction+water_liquid_coef_array(3,3,2,last_coef_index))*pres_fraction+water_liquid_coef_array(2,3,2,last_coef_index))*pres_fraction+water_liquid_coef_array(1,3,2,last_coef_index))+((water_liquid_coef_array(4,2,2,last_coef_index)*pres_fraction+water_liquid_coef_array(3,2,2,last_coef_index))*pres_fraction+water_liquid_coef_array(2,2,2,last_coef_index))*pres_fraction+water_liquid_coef_array(1,2,2,last_coef_index))+((water_liquid_coef_array(4,1,2,last_coef_index)*pres_fraction+water_liquid_coef_array(3,1,2,last_coef_index))*pres_fraction+water_liquid_coef_array(2,1,2,last_coef_index))*pres_fraction+water_liquid_coef_array(1,1,2,last_coef_index)        
    liquid_gap_dens = grid+(dome-grid)*entr_fraction
end function liquid_gap_dens

function liquid_gap_enth(entr_fraction,pres_fraction,temp_diff,sat_temp_index,last_coef_index)
    ! Returns the enthalpy in the liquid gap given the entropy fractional position (uses linear interpolation).
    
    real(C_DOUBLE), intent(in) :: entr_fraction, pres_fraction, temp_diff
    integer(C_INT), intent(in) :: sat_temp_index, last_coef_index
    real(C_DOUBLE) :: liquid_gap_enth, dome, grid

    dome = ((water_sat_vhs_coef_array(2,0,1,sat_temp_index)*temp_diff+water_sat_vhs_coef_array(2,0,2,sat_temp_index))*temp_diff+water_sat_vhs_coef_array(2,0,3,sat_temp_index))*temp_diff+water_sat_vhs_coef_array(2,0,4,sat_temp_index)
    grid = (((((water_liquid_coef_array(4,4,3,last_coef_index)*pres_fraction+water_liquid_coef_array(3,4,3,last_coef_index))*pres_fraction+water_liquid_coef_array(2,4,3,last_coef_index))*pres_fraction+water_liquid_coef_array(1,4,3,last_coef_index))+((water_liquid_coef_array(4,3,3,last_coef_index)*pres_fraction+water_liquid_coef_array(3,3,3,last_coef_index))*pres_fraction+water_liquid_coef_array(2,3,3,last_coef_index))*pres_fraction+water_liquid_coef_array(1,3,3,last_coef_index))+((water_liquid_coef_array(4,2,3,last_coef_index)*pres_fraction+water_liquid_coef_array(3,2,3,last_coef_index))*pres_fraction+water_liquid_coef_array(2,2,3,last_coef_index))*pres_fraction+water_liquid_coef_array(1,2,3,last_coef_index))+((water_liquid_coef_array(4,1,3,last_coef_index)*pres_fraction+water_liquid_coef_array(3,1,3,last_coef_index))*pres_fraction+water_liquid_coef_array(2,1,3,last_coef_index))*pres_fraction+water_liquid_coef_array(1,1,3,last_coef_index)        
    liquid_gap_enth = grid+(dome-grid)*entr_fraction
end function liquid_gap_enth

function liquid_gap_cv(entr_fraction,pres_fraction,temp_diff,sat_temp_index,last_coef_index)
    ! Returns the cv in the liquid gap given the entropy fractional position (uses linear interpolation).
    
    real(C_DOUBLE), intent(in) :: entr_fraction, pres_fraction, temp_diff
    integer(C_INT), intent(in) :: sat_temp_index, last_coef_index
    real(C_DOUBLE) :: liquid_gap_cv, dome, grid

    dome = ((water_sat_prop_coef_array(1,1,0,sat_temp_index)*temp_diff+water_sat_prop_coef_array(2,1,0,sat_temp_index))*temp_diff+water_sat_prop_coef_array(3,1,0,sat_temp_index))*temp_diff+water_sat_prop_coef_array(4,1,0,sat_temp_index)
    grid = (((((water_liquid_coef_array(4,4,4,last_coef_index)*pres_fraction+water_liquid_coef_array(3,4,4,last_coef_index))*pres_fraction+water_liquid_coef_array(2,4,4,last_coef_index))*pres_fraction+water_liquid_coef_array(1,4,4,last_coef_index))+((water_liquid_coef_array(4,3,4,last_coef_index)*pres_fraction+water_liquid_coef_array(3,3,4,last_coef_index))*pres_fraction+water_liquid_coef_array(2,3,4,last_coef_index))*pres_fraction+water_liquid_coef_array(1,3,4,last_coef_index))+((water_liquid_coef_array(4,2,4,last_coef_index)*pres_fraction+water_liquid_coef_array(3,2,4,last_coef_index))*pres_fraction+water_liquid_coef_array(2,2,4,last_coef_index))*pres_fraction+water_liquid_coef_array(1,2,4,last_coef_index))+((water_liquid_coef_array(4,1,4,last_coef_index)*pres_fraction+water_liquid_coef_array(3,1,4,last_coef_index))*pres_fraction+water_liquid_coef_array(2,1,4,last_coef_index))*pres_fraction+water_liquid_coef_array(1,1,4,last_coef_index)        
    liquid_gap_cv = grid+(dome-grid)*entr_fraction
end function liquid_gap_cv

function liquid_gap_cp(entr_fraction,pres_fraction,temp_diff,sat_temp_index,last_coef_index)
    ! Returns the cp in the liquid gap given the entropy fractional position (uses linear interpolation).
    
    real(C_DOUBLE), intent(in) :: entr_fraction, pres_fraction, temp_diff
    integer(C_INT), intent(in) :: sat_temp_index, last_coef_index
    real(C_DOUBLE) :: liquid_gap_cp, dome, grid

    dome = ((water_sat_prop_coef_array(1,2,0,sat_temp_index)*temp_diff+water_sat_prop_coef_array(2,2,0,sat_temp_index))*temp_diff+water_sat_prop_coef_array(3,2,0,sat_temp_index))*temp_diff+water_sat_prop_coef_array(4,2,0,sat_temp_index)
    grid = (((((water_liquid_coef_array(4,4,5,last_coef_index)*pres_fraction+water_liquid_coef_array(3,4,5,last_coef_index))*pres_fraction+water_liquid_coef_array(2,4,5,last_coef_index))*pres_fraction+water_liquid_coef_array(1,4,5,last_coef_index))+((water_liquid_coef_array(4,3,5,last_coef_index)*pres_fraction+water_liquid_coef_array(3,3,5,last_coef_index))*pres_fraction+water_liquid_coef_array(2,3,5,last_coef_index))*pres_fraction+water_liquid_coef_array(1,3,5,last_coef_index))+((water_liquid_coef_array(4,2,5,last_coef_index)*pres_fraction+water_liquid_coef_array(3,2,5,last_coef_index))*pres_fraction+water_liquid_coef_array(2,2,5,last_coef_index))*pres_fraction+water_liquid_coef_array(1,2,5,last_coef_index))+((water_liquid_coef_array(4,1,5,last_coef_index)*pres_fraction+water_liquid_coef_array(3,1,5,last_coef_index))*pres_fraction+water_liquid_coef_array(2,1,5,last_coef_index))*pres_fraction+water_liquid_coef_array(1,1,5,last_coef_index)        
    liquid_gap_cp = grid+(dome-grid)*entr_fraction
end function liquid_gap_cp

function liquid_gap_ssnd(entr_fraction,pres_fraction,temp_diff,sat_temp_index,last_coef_index)
    ! Returns the ssnd in the liquid gap given the entropy fractional position (uses linear interpolation).
    
    real(C_DOUBLE), intent(in) :: entr_fraction, pres_fraction, temp_diff
    integer(C_INT), intent(in) :: sat_temp_index, last_coef_index
    real(C_DOUBLE) :: liquid_gap_ssnd, dome, grid

    dome = ((water_sat_prop_coef_array(1,3,0,sat_temp_index)*temp_diff+water_sat_prop_coef_array(2,3,0,sat_temp_index))*temp_diff+water_sat_prop_coef_array(3,3,0,sat_temp_index))*temp_diff+water_sat_prop_coef_array(4,3,0,sat_temp_index)
    grid = (((((water_liquid_coef_array(4,4,6,last_coef_index)*pres_fraction+water_liquid_coef_array(3,4,6,last_coef_index))*pres_fraction+water_liquid_coef_array(2,4,6,last_coef_index))*pres_fraction+water_liquid_coef_array(1,4,6,last_coef_index))+((water_liquid_coef_array(4,3,6,last_coef_index)*pres_fraction+water_liquid_coef_array(3,3,6,last_coef_index))*pres_fraction+water_liquid_coef_array(2,3,6,last_coef_index))*pres_fraction+water_liquid_coef_array(1,3,6,last_coef_index))+((water_liquid_coef_array(4,2,6,last_coef_index)*pres_fraction+water_liquid_coef_array(3,2,6,last_coef_index))*pres_fraction+water_liquid_coef_array(2,2,6,last_coef_index))*pres_fraction+water_liquid_coef_array(1,2,6,last_coef_index))+((water_liquid_coef_array(4,1,6,last_coef_index)*pres_fraction+water_liquid_coef_array(3,1,6,last_coef_index))*pres_fraction+water_liquid_coef_array(2,1,6,last_coef_index))*pres_fraction+water_liquid_coef_array(1,1,6,last_coef_index)        
    liquid_gap_ssnd = grid+(dome-grid)*entr_fraction
end function liquid_gap_ssnd

function liquid_gap_cond(entr_fraction,pres_fraction,temp_diff,sat_temp_index,last_coef_index)
    ! Returns the cond in the liquid gap given the entropy fractional position (uses linear interpolation).
    
    real(C_DOUBLE), intent(in) :: entr_fraction, pres_fraction, temp_diff
    integer(C_INT), intent(in) :: sat_temp_index, last_coef_index
    real(C_DOUBLE) :: liquid_gap_cond, dome, grid

    dome = ((water_sat_prop_coef_array(1,4,0,sat_temp_index)*temp_diff+water_sat_prop_coef_array(2,4,0,sat_temp_index))*temp_diff+water_sat_prop_coef_array(3,4,0,sat_temp_index))*temp_diff+water_sat_prop_coef_array(4,4,0,sat_temp_index)
    grid = (((((water_liquid_coef_array(4,4,7,last_coef_index)*pres_fraction+water_liquid_coef_array(3,4,7,last_coef_index))*pres_fraction+water_liquid_coef_array(2,4,7,last_coef_index))*pres_fraction+water_liquid_coef_array(1,4,7,last_coef_index))+((water_liquid_coef_array(4,3,7,last_coef_index)*pres_fraction+water_liquid_coef_array(3,3,7,last_coef_index))*pres_fraction+water_liquid_coef_array(2,3,7,last_coef_index))*pres_fraction+water_liquid_coef_array(1,3,7,last_coef_index))+((water_liquid_coef_array(4,2,7,last_coef_index)*pres_fraction+water_liquid_coef_array(3,2,7,last_coef_index))*pres_fraction+water_liquid_coef_array(2,2,7,last_coef_index))*pres_fraction+water_liquid_coef_array(1,2,7,last_coef_index))+((water_liquid_coef_array(4,1,7,last_coef_index)*pres_fraction+water_liquid_coef_array(3,1,7,last_coef_index))*pres_fraction+water_liquid_coef_array(2,1,7,last_coef_index))*pres_fraction+water_liquid_coef_array(1,1,7,last_coef_index)        
    liquid_gap_cond = grid+(dome-grid)*entr_fraction
end function liquid_gap_cond

function liquid_gap_visc(entr_fraction,pres_fraction,temp_diff,sat_temp_index,last_coef_index)
    ! Returns the visc in the liquid gap given the entropy fractional position (uses linear interpolation).
    
    real(C_DOUBLE), intent(in) :: entr_fraction, pres_fraction, temp_diff
    integer(C_INT), intent(in) :: sat_temp_index, last_coef_index
    real(C_DOUBLE) :: liquid_gap_visc, dome, grid

    dome = ((water_sat_prop_coef_array(1,5,0,sat_temp_index)*temp_diff+water_sat_prop_coef_array(2,5,0,sat_temp_index))*temp_diff+water_sat_prop_coef_array(3,5,0,sat_temp_index))*temp_diff+water_sat_prop_coef_array(4,5,0,sat_temp_index)
    grid = (((((water_liquid_coef_array(4,4,8,last_coef_index)*pres_fraction+water_liquid_coef_array(3,4,8,last_coef_index))*pres_fraction+water_liquid_coef_array(2,4,8,last_coef_index))*pres_fraction+water_liquid_coef_array(1,4,8,last_coef_index))+((water_liquid_coef_array(4,3,8,last_coef_index)*pres_fraction+water_liquid_coef_array(3,3,8,last_coef_index))*pres_fraction+water_liquid_coef_array(2,3,8,last_coef_index))*pres_fraction+water_liquid_coef_array(1,3,8,last_coef_index))+((water_liquid_coef_array(4,2,8,last_coef_index)*pres_fraction+water_liquid_coef_array(3,2,8,last_coef_index))*pres_fraction+water_liquid_coef_array(2,2,8,last_coef_index))*pres_fraction+water_liquid_coef_array(1,2,8,last_coef_index))+((water_liquid_coef_array(4,1,8,last_coef_index)*pres_fraction+water_liquid_coef_array(3,1,8,last_coef_index))*pres_fraction+water_liquid_coef_array(2,1,8,last_coef_index))*pres_fraction+water_liquid_coef_array(1,1,8,last_coef_index)        
    liquid_gap_visc = grid+(dome-grid)*entr_fraction
end function liquid_gap_visc

function liquid_gap_entr(entr_fraction,temp_diff,sat_temp_index,pres_index)
    ! Returns the entr in the vapor gap given the entropy fractional position (uses linear interpolation).
    
    real(C_DOUBLE), intent(in) :: entr_fraction, temp_diff
    integer(C_INT), intent(in) :: sat_temp_index, pres_index
    real(C_DOUBLE) :: liquid_gap_entr, dome
    integer(C_INT) :: last_index

    dome = ((water_sat_vhs_coef_array(3,0,1,sat_temp_index)*temp_diff+water_sat_vhs_coef_array(3,0,2,sat_temp_index))*temp_diff+water_sat_vhs_coef_array(3,0,3,sat_temp_index))*temp_diff+water_sat_vhs_coef_array(3,0,4,sat_temp_index)
    last_index = water_liquid_entr_index_vector(pres_index+1)-1
    liquid_gap_entr = water_liquid_entr_values(last_index)+(dome-water_liquid_entr_values(last_index))*entr_fraction    
end function liquid_gap_entr

end module water_properties
