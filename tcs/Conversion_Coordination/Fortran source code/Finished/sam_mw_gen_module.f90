!************************************************************************
! Object: Generic solar model helper subroutines, functions, and arrays
! Simulation Studio Model: Type260
! 
! Author: Michael J. Wagner
! Date:	 June 22, 2010

! COPYRIGHT 2010 NATIONAL RENEWABLE ENERGY LABORATORY

!------------------------------------------------------------------------------------------------------------
!This file contains declarations for the following arrays:
!   * eta       | [-] Optical efficiency array, function of (azimuth, zenith) solar position
!   * azms      | [rad] Array containing the user-specified solar azimuth angles corresponding to the eta array
!   * zens      | [rad] Array containing the user-specified solar zenith angles corresponding to the eta array
!
!This file contains the following subroutines and functions
!   * load_gen_table    | Reads in the generic optical table, allocating and assigning the eta, azms, and zens arrays
!   * azzen_interp      | Handles interpolation within the eta array given a solar position
!   * bounds            | Handler function for locateb and huntb - manages the use of the interpolation codes
!   * locateb           | Interpolation code that locates the upper and lower bound using the 'locate' searching method
!   * huntb             | Interpolation code that locates the upper and lower bound using the 'hunt' searching method   
!------------------------------------------------------------------------------------------------------------

!************************************************************************************************************
!************************************************************************************************************


module CSPGeneric_tools
    real(8),allocatable:: eta(:,:,:), azms(:,:), zens(:,:), azms1d(:), zens1d(:),&
                          eta_temp(:,:,:), azms_temp(:,:), zens_temp(:,:)
    integer::lu_map(4)

contains

    !********************************************************************************************************

    subroutine load_gen_table(LU)
    use TrnsysFunctions
    !--------------------------------------------------------------------------------------------------------
    !INPUTS: 
    !   * LU    | Logical unit assigned to the generic optical array file
    !--------------------------------------------------------------------------------------------------------

    implicit none

    real(8):: pi
    integer:: LU, nazm, nzen, i, j, k, LU_ind
    character:: temp*1000, df
    pi = 3.14159265
    !--------------------------------------------------------------------------------------------------------
    !This subroutine reads in the generic efficiency table from a file. The file can either be in .csv format 
    !or in tab-delimited format.
    rewind(LU)

    !---find the number of azimuth angles
    nazm=0
    !Read in the first/next line of the file.. this has one more column than there are array values
    read(LU,'(A)',err=105) temp
    !Determine the delimiter format
    df=","
    i = index(trim(temp),df)
    !If we can't find a comma, try to find a tab
    if (i==0) then
        df=achar(9)    !look for ascii character TAB (#9)
        i = index(trim(temp),df)
        if (i==0) stop !call messages(...)
    endif

    do 
        !Is there a column delimiter?
        i = index(trim(temp),df)
        !Check to see if we've read in everything
        if((len(trim(temp))<=0).or.(i==0)) exit
        !otherwise increment the column counter
        nazm = nazm+1
        !chop the temp string and continue looping
        temp = temp(i+1:)
    enddo

    !---find the number of zenith angles
    nzen=0
    rewind(LU)
    do
        !Read in each row of text
        read(LU,'(A)',end=100) temp
        !if there is no problem, skip to 101 and increment the row counter        
            goto 101    
        100 exit    !We've hit the end of the file.. exit the loop
        101 nzen = nzen+1
    enddo
    nzen = nzen - 1 !Remove the header row
    
    !---allocate the arrays
    if(.not.allocated(eta)) then
        allocate(eta(4,nazm,nzen),azms(4,nazm),zens(4,nzen),azms1d(nazm),zens1d(nzen))
        !zero arrays
        eta=0.d0; azms=-9999.d0; zens=-9999.d0; azms1d=0.d0; zens1d=0.d0;
        lu_map(:) = -1
    endif
    !--If the arrays are not large enough for the new table, reallocate
    if((nazm>size(azms(1,:))) .or. (nzen>size(zens(1,:))) ) then
        if(allocated(eta_temp)) deallocate(eta_temp, azms_temp, zens_temp)
        allocate(eta_temp(4,nazm,nzen), azms_temp(4,nazm), zens_temp(4,nzen))
        !Set to zero
        eta_temp = 0.d0; azms_temp = 0.d0; zens_temp = 0.d0;
        !Store old values in temp array
        do k=1,4
            do i=1,size(azms(1,:))
                azms_temp(k,i) = azms(k,i)
                do j=1,size(zens(1,:))
                    if(i==1) zens_temp(k,j) = zens(k,j)
                    eta_temp(k,i,j) = eta(k,i,j)
                enddo        
            enddo
        enddo
        !Deallocate and reallocate original arrays
        deallocate(eta, azms, zens, azms1d, zens1d)
        !Reallocate with new dimensions
        allocate(eta(4,nazm,nzen), azms(4,nazm), zens(4,nzen),azms1d(nazm),zens1d(nzen))
        !zero new arrays
        eta=0.d0; azms=0.d0; zens=0.d0; azms1d=0.d0; zens1d=0.d0;
        !Fill back in the data
        eta(:,:,:) = eta_temp(:,:,:)
        azms(:,:) = azms_temp(:,:)
        zens(:,:) = zens_temp(:,:)
        !Deallocate temporary arrays
        deallocate(eta_temp, azms_temp, zens_temp)
    endif
    
    !Add this array to the logical unit map.. keep track of the different array data separately
    do i=1,4
        if(lu_map(i)<0) then
            lu_map(i) = LU
            LU_ind = i
            exit
        endif
        if(i==4) goto 105
    enddo
    
    !Read in the data array
    rewind(LU)
    !First get all the azimuth angles in the first line
    read(LU,'(A)') temp
    !Get rid of the first column of the first row.. it's junk
    temp = temp(index(trim(temp),df)+1:)
    do j=1,nazm
        i = index(trim(temp),df)
        !If we're only left with the last value (i=0) then reset i to be the end+1
        if(i==0) i=len(trim(temp))+1
        read(temp(:i-1),*) azms(LU_ind, j)
        temp = temp(i+1:)
    enddo
    
    !Now read in each row, the first column is the zenith angle
    do j=1,nzen
        read(LU,'(A)') temp
        !Get the first column.. the zenith angle
        i = index(trim(temp),df)
        read(temp(:i-1),*) zens(LU_ind, j)
        temp = temp(i+1:)
        
        !now loop through the rest of the azimuths (k) for this particular zenith (j)
        do k=1,nazm
            i = index(trim(temp),df)
            !If we're only left with the last value (i=0) then reset i to be the end+1
            if(i==0) i=len(trim(temp))+1
            read(temp(:i-1),*) eta(LU_ind, k,j)
            temp = temp(i+1:)
        enddo
    enddo
    
    !Make sure the azms and zens array contain angles in radians..
    if(abs(maxval(zens) - minval(zens)) > pi) then
        do i=1,nzen
            zens(LU_ind, i) = zens(LU_ind, i)*pi/180.
        enddo
        do i=1,nazm
            azms(LU_ind, i) = azms(LU_ind, i)*pi/180.
        enddo
    endif
    
    !Warning messages
    goto 999
        105 call messages(-1,"There was a problem reading in the efficiency array. Make sure the file is not empty.",'FATAL',0,260)
    999 continue 

    close(LU)

    end subroutine
    !--------------------------------------------------------------------------------------------------------

    !**************************************************************************************************
    !**  The following functions are used to interpolate user-defined material property tables.      **
    !**  "huntb" and "locateb" code is adapted from Numerical Recipes, 3rd Edition.                  **
    !**************************************************************************************************
    
    !--------------------------------------------------------------------------------------------------------
    !--------------------------------------------------------------------------------------------------------
    real(8) function azzen_interp(az, zen, mode, LU)
    !azimuth [rad]
    !zenith [rad]
    !mode : (1-interpolate, 2-no interpolation)
    
    use TrnsysFunctions
    
    implicit none
    
    real(8):: az, zen, az_zenlb, az_zenub
    logical:: ascnd
    integer:: mode, i, j, k, lba, uba, lbz, ubz, nazm, nzen, lena, lenz, asav, zsav, b1, b2, LU_ind, LU
    save:: asav, zsav
    !--------------------------------------------------------------------------------------------------------

    !Which data set are we using?
    do i=1,4
        if(LU==lu_map(i)) then
            LU_ind = i
            exit
        endif
        if(i==4) then
            call messages(-1,"There was a problem finding the appropriate lookup table in the azzen_interp function.","FATAL",0,260)
            return
        endif
    enddo
    
    !Determine the number of azimuth and zenith entries
    nazm = 0; nzen = 0
    do i=1,size(azms(LU_ind,:))
        if(azms(LU_ind,i)==-9999.d0) exit
        nazm = nazm+1
    enddo
    do i=1,size(zens(LU_ind,:))
        if(zens(LU_ind,i)==-9999.d0) exit
        nzen = nzen+1
    enddo
    !nazm = size(azms(LU_ind,:)); nzen = size(zens(LU_ind,:))
    
    !Start by finding the surrounding values in the table
    azms1d = azms(LU_ind,:); zens1d = zens(LU_ind,:)
    !interpolate to find the upper and lower brackets
    !azimuth first
    call bounds(az, nazm, azms1d, lba, uba, asav)
    !next, zenith
    call bounds(zen, nzen, zens1d, lbz, ubz, zsav)
    
    !warn if interpolating outside the array bounds
    if(az<minval(azms(LU_ind,:))) goto 101
    if(az>maxval(azms(LU_ind,:))) goto 101
    if(zen<minval(zens(LU_ind,:))) goto 101
    if(zen>maxval(zens(LU_ind,:))) goto 101
    goto 102
        101 call messages(-1,"The azimuth/elevation table is interpolating outside the array bounds","NOTICE",0,260)
        
    102 select case(mode)
    case(1) !Interpolate
        az_zenlb = eta(LU_ind,lba,lbz) + (az - azms(LU_ind,lba))/(azms(LU_ind,uba) - azms(LU_ind,lba))*(eta(LU_ind,uba,lbz)-eta(LU_ind,lba,lbz))
        az_zenub = eta(LU_ind,lba,ubz) + (az - azms(LU_ind,lba))/(azms(LU_ind,uba) - azms(LU_ind,lba))*(eta(LU_ind,uba,ubz)-eta(LU_ind,lba,ubz))
        azzen_interp = az_zenlb + (zen - zens(LU_ind,lbz))/(zens(LU_ind,ubz) - zens(LU_ind,lbz))*(az_zenub - az_zenlb)
    case(2) !Don't interpolate
        !Just find the closest table angle the corresponds to the requested solar position
        if(abs(az - azms(LU_ind,lba)) < abs(azms(LU_ind,uba) - az)) then
            b1 = lba
        else
            b1 = uba
        endif
        if(abs(zen - zens(LU_ind,lbz)) < abs(zens(LU_ind,ubz) - zen)) then
            b2 = lbz
        else
            b2 = ubz
        endif
        azzen_interp = eta(LU_ind,b1,b2)
    end select
    
    end function

    !--------------------------------------------------------------------------------------------------------
    !--------------------------------------------------------------------------------------------------------
    subroutine bounds(x,n,xx,lb,ub,jsav)

        !********************************************************************************************
        !* Given a value x, return the interpolation brackets, using data xx of size n. Save       **
        !* previous table location as jsav, and return it to the subroutine when calling.          **
        !********************************************************************************************
        integer,intent(in)::n
        integer,intent(inout)::jsav
        integer  cor, j, dj, lb, ub
        double precision x,xx(n)

        !Estimated limit for using hunt vs locate methods
        dj=min(1,nint(n**.25))  

        !Decide which interpolation method to use
        if (cor.eq.1) then  
            call huntb(x,n,xx,dj,cor,lb,jsav)  !We anticipate a location close to the previous
        else  
            call locateb(x,n,xx,dj,cor,lb,jsav)  !The anticipated location is not near the previous
        endif

        ub = lb+1 !The lower bound is always between 1,(n-1), so the upper bound can be n if needed

    end subroutine


    !--------------------------------------------------------------------------------------------------------
    !--------------------------------------------------------------------------------------------------------


    subroutine locateb(x,n,xx,dj,cor,j,jsav)

        !********************************************************************************************
        !* Given a value x, return a value j such that x is (insofar as possible) centered in the   *
        !* subrange xx[j..j+mm-1], where xx is the stored pointer.  The values in xx must be mono-  *
        !* tonic, either increasing or decreasing.  The returned value is not less than 0, nor      *
        !* greater than n-1.                                                                        *
        !********************************************************************************************
        implicit none

        integer,intent(in):: n, dj
        integer,intent(out):: cor, j
        integer,intent(inout):: jsav
        double precision,intent(in):: x,xx(*) 
        integer ju, jm, jl, mm
        logical ascnd

        !mm is the number of abscissas used to bracket the interpolation value
        !note for linear interpolation, mm is always equal to 2
        mm=2

        if ((n.lt.2).or.(mm.lt.2.).or.(mm.gt.n)) &
            print *,"LOCATE size error"

        ascnd=.FALSE.
        if (xx(n).ge.xx(1)) ascnd=.TRUE. !True if ascending order table, false otherwise

        jl=1; ju=n;           !Initialize lower, upper bound
                            
        do 
            if((ju-jl).le.1) exit
            jm=(ju+jl)/2            !Compute a midpoint
            if((x.ge.xx(jm)).eq.ascnd) then  !Replace either the lower limit or upper limit
                jl=jm
            else
                ju=jm
            endif
        enddo                     !Repeat until the test condition is satisfied

        !Set the flag that determines which interpolation method to use in the next time step
        cor=1;  if(abs(jl-jsav).gt.dj) cor=0; 

        jsav=jl  !Save the lower bound

        j=max(1,min(n-1,jl)) !find the lower bound, must be between 1 and n-1

    end subroutine

    !--------------------------------------------------------------------------------------------------------
    !--------------------------------------------------------------------------------------------------------

    subroutine huntb(x,n,xx,dj,cor,j,jsav)

        !********************************************************************************************
        !* Given a value x, return a value j such that x is (insofar as possible) centered in the  **
        !* subrange xx(j..j+mm-1), where xx is the stored data array.  The values in xx must be    **
        !* monatonic, either increasing or decreasing.  The returned value is not less than 0, nor **
        !* greater than n-1.                                                                       **
        !********************************************************************************************
        implicit none

        integer,intent(in):: n, dj
        integer,intent(out):: cor, j
        integer,intent(inout):: jsav
        double precision,intent(in):: x, xx(n)
        integer ju, jm, jl, mm, inc
        logical ascnd

        inc=1  !Set the initial increment

        !mm is the number of abscissas used to bracket the interpolation value
        !note for linear interpolation, mm is always equal to 2
        mm=2

        if ((n.lt.2).or.(mm.lt.2.).or.(mm.gt.n)) &
         print *,"LOCATE size error"

        ascnd=.FALSE.
        if (xx(n).ge.xx(1)) ascnd=.TRUE. !True if ascending order table, false otherwise

        if((jl.lt.1).or.(jl.gt.n)) then  !Check to see if the guess values are useful
            jl=1  !if not, define the lower and upper bounds as the min/max abscissas of the array
            ju=n
        else
            if(x.ge.xx(jl).eq.ascnd) then
                do
                    ju=jl+inc
                    if(ju.ge.n) then
                        ju=n
                        exit    !off end of table
                    elseif((x.lt.xx(ju)).eq.ascnd) then
                        exit    !Found bracket
                    else
                        jl=ju
                        inc=inc+inc  !Double increment and try again
                    endif
                enddo
            else
                ju=jl
                do
                    jl=jl-inc
                    if(jl.le.1) then
                        jl=1
                        exit  !Off end of table
                    elseif((x.ge.xx(jl)).eq.ascnd) then
                        exit  !Found bracket
                    else
                        ju=jl
                        inc=inc+inc  !Double increment and try again
                    endif
                enddo
            endif
        endif
        do while((ju-jl).gt.1)
            jm=(ju+jl)/2
            if((x.ge.xx(jm)).eq.ascnd) then
                jl=jm
            else
                ju=jm
            endif
        enddo

        !Set the flag that determines which interpolation method to use in the next time step
        cor=1;  if(abs(jl-jsav).gt.dj) cor=0; 

        jsav=jl  !Save the lower bound

        j=max(1,min(n-mm+1,jl-((mm-2)/2))) !find the lower bound, must be between 1 and n-1

    end subroutine
    !--------------------------------------------------------------------------------------------------------


end module

