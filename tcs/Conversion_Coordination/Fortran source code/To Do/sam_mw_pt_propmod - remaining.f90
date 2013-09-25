integer function t_warn(X,XLO,XHI,substance)
use global_props
implicit none

double precision,intent(in)::X,XHI,XLO
character(len=*),intent(in)::substance

    fl_ct=fl_ct+1.
    if((fl_ct.gt.50.).and.(fl_flag.eq.0.)) then
    
        write (LuFlEr,fmt=100) trim(substance),X,XLO,XHI,ttime
        fl_flag = 1.  !Note that the warning has now been written. Don't write again.
        
    endif
    100   format("WARNING: The property range for ",A,&
                 " has been exceeded more than 50 times. The value provided was ",F6.1,&
                 ", and the lower and upper limits are ",&
                 F6.1," and ",F6.1,", respectively. (time: ",F6.1,")")
    t_warn=0
end function
