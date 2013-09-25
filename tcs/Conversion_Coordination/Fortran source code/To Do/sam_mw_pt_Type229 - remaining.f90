! ----------------------------------------------------------------------------------------------------------------------
!    DO ALL OF THE INITIAL TIMESTEP MANIPULATIONS HERE - THERE ARE NO ITERATIONS AT THE INTIAL TIME
      IF (TIME .LT. (getSimulationStartTime() + getSimulationTimeStep()/2.D0)) THEN

!       SET THE UNIT NUMBER FOR FUTURE CALLS
         IUNIT=INFO(1)
         ITYPE=INFO(2)

        !On the initial timestep call, read in the fluid property file

        call readFluidPropFile(LU_FL)       
        fl_flag = 0. !Always reset the fluid warning flag to zero
        fl_ct = 0.  !and the warning counter too


!       PERFORM ANY REQUIRED CALCULATIONS TO SET THE INITIAL VALUES OF THE OUTPUTS HERE
!		 Specific heat capacity
			OUT(1)=1.53
!		 Density
			OUT(2)=1750.
!		 Viscosity
			OUT(3)=1e-5
!		 Conductivity
			OUT(4)=.5

!       PERFORM ANY REQUIRED CALCULATIONS TO SET THE INITIAL STORAGE VARIABLES HERE
	   STORED(1)=0. !Gjsav

!       PUT THE STORED ARRAY IN THE GLOBAL STORED ARRAY
         CALL setStorageVars(STORED,NSTORED,INFO)

!       RETURN TO THE CALLING PROGRAM
         RETURN 1

      ENDIF
      
      
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!    *** ITS AN ITERATIVE CALL TO THIS COMPONENT ***
! ----------------------------------------------------------------------------------------------------------------------


! The following scheme is used to define the substance number.  Some fluids are
! blocked off for the trough model, and some are blocked off for future use. 
! Any fluid number specified greater than 35 will be a user-defined fluid.
!    1.) Air
!    2.) Stainless_AISI316
!    3.) Water (liquid)
!    4.) Steam
!    5.) CO2
!    6.) Salt (68% KCl, 32% MgCl2)
!    7.) Salt (8% NaF, 92% NaBF4)
!    8.) Salt (25% KF, 75% KBF4)
!    9.) Salt (31% RbF, 69% RbBF4)
!    10.) Salt (46.5% LiF, 11.5%NaF, 42%KF)
!    11.) Salt (49% LiF, 29% NaF, 29% ZrF4)
!    12.) Salt (58% KF, 42% ZrF4)
!    13.) Salt (58% LiCl, 42% RbCl)
!    14.) Salt (58% NaCl, 42% MgCl2)
!    15.) Salt (59.5% LiCl, 40.5% KCl)
!    16.) Salt (59.5% NaF, 40.5% ZrF4)
!    17.) Salt (60% NaNO3, 40% KNO3)
!    18.) Nitrate Salt**
!    19.) Caloria HT 43**
!    20.) Hitec XL**
!    21.) Therminol VP-1**
!    22.) Hitec**
!    23.) Dowtherm Q**
!    24.) Dowtherm RP**
!    26.) Argon (ideal gas properties)
!    27.) Hydrogen (ideal gas properties)
!    28.) -blank-
!    29.) Therminol 66
!    30.) Therminol 59
!    31.) -blank-
!    32.) -blank-
!    33.) -blank-
!    34.) -blank-
!    35.) -blank-
!    36+) User specified (lookup tables)

!     Note that the fluid properties are stored in the FPROP array in the following order:
!*****************************************************************
!|  #    |   1   |   2   |   3   |   4   |   5   |   6   |   7   |
!|-------|-------|-------|-------|-------|-------|-------|-------|
!| Name  |   T   |   Cp  |  rho  |   Mu  |   Nu  |   k   |   h   |
!| Units |   C   |kJ/kg-K| kg/m3 |  Pa-s |  m2-s | W/m-K |  J/kg |
!|-------|-------|-------|-------|-------|-------|-------|-------|
!|   1   |   :   |   :   |   :   |   :   |   :   |   :   |   :   |
!|   2   |   :   |   :   |   :   |   :   |   :   |   :   |   :   |
!*****************************************************************

subroutine check_htf(fnumd,T)

implicit none

real(8),intent(in):: T, fnumd
real(8):: xlo, xhi
integer:: dum, t_warn, fnum

fnum = int(fnumd)

select case(fnum)
case(1:5)
  !No information
  continue
case(6)   !    6.) Salt (68% KCl, 32% MgCl2)
    xlo=699.; xhi=1691.;
    if(T.lt.xlo.or.T.gt.xhi) dum=t_warn(T,xlo,xhi,"Salt (68% KCl, 32% MgCl2)")
case(7)   !    7.) Salt (8% NaF, 92% NaBF4)
    xlo=658.; xhi=969.;
    if(T.lt.xlo.or.T.gt.xhi) dum=t_warn(T,xlo,xhi,"Salt (8% NaF, 92% NaBF4)")
case(8)   !    8.) Salt (25% KF, 75% KBF4)
    xlo=699.; xhi=1691.;
    if(T.lt.xlo.or.T.gt.xhi) dum=t_warn(T,xlo,xhi,"Salt (25% KF, 75% KBF4)")
case(9)   !    9.) Salt (31% RbF, 69% RbBF4)
    xlo=715.; xhi=1343.;
    if(T.lt.xlo.or.T.gt.xhi) dum=t_warn(T,xlo,xhi,"Salt (31% RbF, 69% RbBF4)")
case(10)   !    10.) Salt (46.5% LiF, 11.5%NaF, 42%KF)
    xlo=727.; xhi=1843.;
    if(T.lt.xlo.or.T.gt.xhi) dum=t_warn(T,xlo,xhi,"Salt (46.5% LiF, 11.5%NaF, 42%KF)")
case(11)   !    11.) Salt (49% LiF, 29% NaF, 29% ZrF4)
    xlo=709.; xhi=1673.;
    if(T.lt.xlo.or.T.gt.xhi) dum=t_warn(T,xlo,xhi,"Salt (49% LiF, 29% NaF, 29% ZrF4)")
case(12)   !    12.) Salt (58% KF, 42% ZrF4)
    xlo=773.; xhi=1623.;
    if(T.lt.xlo.or.T.gt.xhi) dum=t_warn(T,xlo,xhi,"Salt (58% KF, 42% ZrF4)")
case(13)   !    13.) Salt (58% LiCl, 42% RbCl)
    xlo=586.; xhi=1323.;
    if(T.lt.xlo.or.T.gt.xhi) dum=t_warn(T,xlo,xhi,"Salt (58% LiCl, 42% RbCl)")
case(14)   !    14.) Salt (58% NaCl, 42% MgCl2)
    xlo=718.; xhi=1738.;
    if(T.lt.xlo.or.T.gt.xhi) dum=t_warn(T,xlo,xhi,"Salt (58% NaCl, 42% MgCl2)")
case(15)   !    15.) Salt (59.5% LiCl, 40.5% KCl)
    xlo=628.; xhi=1673.;
    if(T.lt.xlo.or.T.gt.xhi) dum=t_warn(T,xlo,xhi,"Salt (59.5% LiCl, 40.5% KCl)")
case(16)   !    16.) Salt (59.5% NaF, 40.5% ZrF4)
    xlo=773.; xhi=1623.;
    if(T.lt.xlo.or.T.gt.xhi) dum=t_warn(T,xlo,xhi,"Salt (59.5% NaF, 40.5% ZrF4)")
case(17)   !    17.) Salt (60% NaNO3, 40% KNO3)
    xlo=493.; xhi=866.;     !MJW 10.21.2010: Reduced freezing temp to 220C (493.15K), reference http://www.nrel.gov/csp/troughnet/pdfs/40028.pdf
    if(T.lt.xlo.or.T.gt.xhi) dum=t_warn(T,xlo,xhi,"Salt (60% NaNO3, 40% KNO3)")
case(18)    !Nitrate Salt, [kg/m3]
    xlo=493.; xhi=866.;     !MJW 10.21.2010: Reduced freezing temp to 220C (493.15K), reference http://www.nrel.gov/csp/troughnet/pdfs/40028.pdf
    if(T.lt.xlo.or.T.gt.xhi) dum=t_warn(T,xlo,xhi,"Nitrate Salt (60% NaNO3, 40% KNO3)")
case(19)    !   19.) Caloria HT 43
    xlo=261.; xhi=588.;
    if(T.lt.xlo.or.T.gt.xhi) dum=t_warn(T,xlo,xhi,"Nitrate Salt (60% NaNO3, 40% KNO3)")
case(20)    !   20.) HitecXL 
    xlo=393.; xhi=773.;     !TWN 9.9.2012: Added. Reference: "LOW MELTING POINT MOLTEN SALT HEAT TRANSFER FLUID WITH REDUCED COST", Raade et al.
    if(T.lt.xlo.or.T.gt.xhi) dum=t_warn(T,xlo,xhi,"HitecXL")
case(21)    !   21.) Therminol VP-1
    xlo=285.; xhi=673.;     !TWN 9.4.2012: Added. Reference: Therminol Reference Disk by Solutia: http://www.therminol.com/pages/tools/toolscd.asp 
    if(T.lt.xlo.or.T.gt.xhi) dum=t_warn(T,xlo,xhi,"Therminol VP-1")
case(22)    !   22.) Hitec
    xlo=415.; xhi=811.;     !TWN 9.9.2012: Added. Reference: "LOW MELTING POINT MOLTEN SALT HEAT TRANSFER FLUID WITH REDUCED COST", Raade et al.    
    if(T.lt.xlo.or.T.gt.xhi) dum=t_warn(T,xlo,xhi,"Hitec")
case(23)    !   23.) DowTherm Q                  
    xlo=238.; xhi=603.;     !TWN 9.9.2012: Added. Reference: http://www.dow.com/heattrans/products/synthetic/dowtherm.htm
    if(T.lt.xlo.or.T.gt.xhi) dum=t_warn(T,xlo,xhi,"DowTherm Q")
case(24)    !   24.) DowTherm RP
    ! ***** Low temperature is a place-holder, the correct value is not provided by the reference
    xlo=273.; xhi=623.;     !TWN 9.9.2012: Added. Reference: http://www.dow.com/heattrans/products/synthetic/dowtherm.htm
    if(T.lt.xlo.or.T.gt.xhi) dum=t_warn(T,xlo,xhi,"DowTherm RP")  
case(25:28)
    continue  
case(29)    !   29.) Therminol 66
    xlo=273.; xhi=618.;     !TWN 9.4.2012: Added. Reference: Therminol Reference Disk by Solutia: http://www.therminol.com/pages/tools/toolscd.asp
    if(T.lt.xlo.or.T.gt.xhi) dum=t_warn(T,xlo,xhi,"Therminol 66")  
case(30)    !   30.) Therminol 59
    xlo=228.; xhi=588.;     !TWN 9.4.2012: Added. Reference: Therminol Reference Disk by Solutia: http://www.therminol.com/pages/tools/toolscd.asp
    if(T.lt.xlo.or.T.gt.xhi) dum=t_warn(T,xlo,xhi,"Therminol 59")  
case(31:)
    continue !no informaion
end select


end subroutine


! 12-10-9, TN: Current plan is to NOT convert the following functions to c++

!
!Enthalpy of Salt (English Units), [Btu/lbm]
Double Precision Function H_engl_salt(T) !T [F]
implicit none
Double Precision T
H_engl_salt = 0.345 * T + 0.0000114 * T*T
End Function


!Volumetric Thermal Expansion Coefficient for Nitrate Salt, [1/C]
Double Precision Function beta_salt(T) !T [C]
implicit none
Double Precision T, density
beta_salt = 0.636 / density(18.d0, T, 0.d0)
End Function

!Vapor Pressure of Therminol Oil [kPa]
Double Precision Function Pvap_therminol(T) !T [C]
implicit none
Double Precision T
Pvap_therminol = 10**-11.13 * T**5.448
End Function

!Vapor Pressure of Dowtherm Q [W/mK]
Double Precision Function Pvap_Dowtherm_Q(T) !T [C]
implicit none
Double Precision T
Pvap_Dowtherm_Q = -6238.9915 + 0.000000037388014 * T**5.5254072     !Hank 10-2-03
End Function

!Vapor Pressure of Dowtherm RP [Pa]
Double Precision Function Pvap_Dowtherm_RP(T) !T [C]
implicit none
Double Precision T
Pvap_Dowtherm_RP = -2863.8678 + 2.7661827E-12 * T**6.894446     !Hank 10-2-03                       !Hank 10-2-03
End Function