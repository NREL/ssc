!**************************************************************************************************
!**  The following functions are used to interpolate user-defined material property tables.      **
!**  Code is adapted from Numerical Recipes, 3rd Edition.                                        **
!**************************************************************************************************

subroutine interp(x,n,xx,yy,jsav,y)

    !********************************************************************************************
    !* Given a value x, return an interpolated value y, using data xx and yy of size n. Save   **
    !* previous table location as jsav, and return it to the subroutine when calling.          **
    !********************************************************************************************
    integer,intent(in)::n
    integer,intent(inout)::jsav
    integer  cor, j, dj
    double precision x,xx(n),yy(n)
    double precision,intent(out)::y

    !Estimated limit for using hunt vs locate methods
    dj=min(1,nint(n**.25))  

    !Decide which interpolation method to use
    if (cor.eq.1) then  
        call hunt(x,n,xx,dj,cor,j,jsav)  !We anticipate a location close to the previous
    else  
        call locate(x,n,xx,dj,cor,j,jsav)  !The anticipated location is not near the previous
    endif

    !Calculate the interpolated value
    y = yy(j)+((x-xx(j))/(xx(j+1)-xx(j)))*(yy(j+1)-yy(j))

end subroutine


!--------------------------------------------------------------------------------------------


subroutine locate(x,n,xx,dj,cor,j,jsav)

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

    j=max(1,min(n-mm+1,jl-((mm-2)/2))) !find the lower bound, must be between 1 and n-1

end subroutine

!--------------------------------------------------------------------------------------------

subroutine hunt(x,n,xx,dj,cor,j,jsav)

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