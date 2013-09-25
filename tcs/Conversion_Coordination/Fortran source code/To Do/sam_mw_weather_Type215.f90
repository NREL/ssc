SUBROUTINE TYPE215 (TIME,XIN,OUT,T,DTDT,PAR,INFO,ICNTRL,*)
!************************************************************************
! Object: SAM Weather file processor
! Simulation Studio Model: Type215
! 
! Author: Michael J. Wagner
! Editor: Michael J. Wagner
! Date:	 November 15, 2010
! Modified: December 3, 2010

! COPYRIGHT 2010 NATIONAL RENEWABLE ENERGY LABORATORY

!NOTES:
! This type is written exclusively for use with the SAM modules. The expected format
! for the data input is relatively strict compared with Type 9 (see table below). If the user requires additional
! flexibility in data input, use of Type 9 is recommended. 
!   - The input file entries must be comma-delimited. 
!   - The first header line should be comma-separated and include the following:
!     ***Station#/ID#, Location name, State, time zone, latitude, longitude, elevation, timestep, start year, start time (hr:min:sec)***
!
! ---Note regarding weather file data instantaneous vs. averaged convention:---
! This weather file reader expects all values in the weather file to be instantaneous measurements at the time reported
! in the weather file. The instantaneous values are considered valid over a time span equal to the time step and centered
! equally about the instantaneous time value. For example, the weather values reported at time 1.5 for a time step of 1.0
! are valid from time = 1.0 to time = 2.0. 
! This convention approximates the cumulative solar irradiation values to be equal to their average value over the duration
! of the time step. 
!---Psychrometric properties---
! Psych properties specified in the weather file must include both dry-bulb temperature and atmospheric pressure. In addition, 
! one or more of the following must also be specified: Dew-point temperature, wet-bulb temperature, relative humidity.
! The columns with no data should be formatted with all values equal to -999. The reader will call the psych algorithm to 
! determine unspecified values.

!Data input file format:
!-----------------------------------------------
!Column |   Item                    | Units
!-----------------------------------------------
! 1     | Dry-bulb temperature      | C
! 2     | Dew-point temperature     | C
! 3     | Wet-bulb temperature      | C
! 4     | Relative humidity         | %
! 5     | Wind speed                | m/s
! 6     | Wind direction            | deg
! 7     | Atmospheric pressure      | mbar
! 8     | Global horiz. radiation   | W/m2
! 9     | Direct normal radiation   | W/m2
! 10    | Diffuse horiz. radiation  | W/m2
! 11    | Albedo                    | none
! 12    | Snow depth                | m
!-----------------------------------------------

! Doc. tables updated 2010-12-09 - MJW
!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Parameters
!    1| LU_w                             | Assigned logical unit for the weather file                        | none             | none             
!    2| Interp                           | Interpolation flag - 0=No, 1=Yes                                  | none             | none             
!    3| def_psych_mode                   | Default psych property mode                                       | none             | none             
!    4| rhog                             | Ground reflectance (if constant)                                  | none             | none             
!    5| rhog_snow                        | Ground reflectance with snow (if constant)                        | none             | none             
!    6| fcvar                            | Forecast variable (1=GHI, 2=DNI, 3=DHI)                           | none             | none             
!    7| fchrs                            | Forecast hours                                                    | hr               | hr               

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Inputs

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Outputs
!    1| day_of_year                      | Day of the year                                                   | day              | day              
!    2| hour                             | Hour of the day                                                   |  hr              |  hr              
!    3| T_db                             | Dry-bulb temperature                                              |  C               |  C               
!    4| T_dp                             | Dew-point temperature                                             |  C               |  C               
!    5| T_wb                             | Wet-bulb temperature                                              |  C               |  C               
!    6| RH                               | Relative humidity                                                 |  %               |  %               
!    7| v_wind                           | Wind speed                                                        |  m/s             |  m/s             
!    8| wind_dir                         | Wind direction                                                    | deg              | deg              
!    9| P_atm                            | Atmospheric pressure                                              |  mbar            |  mbar            
!   10| GHI                              | Global horiz. radiation                                           |  W/m2            |  W/m2            
!   11| DNI                              | Direct normal radiation                                           |  W/m2            |  W/m2            
!   12| DHI                              | Diffuse horiz. radiation                                          |  W/m2            |  W/m2            
!   13| albedo                           | Ground reflectance                                                | none             | none             
!   14| snow_depth                       | Snow depth                                                        | m                | m                
!   15| shift                            | The shift in time from the standard local meridian                | deg              | deg              
!   16| latitude                         | Plant latitude                                                    | deg              | deg              
!   17| longitude                        | Plant longitude                                                   | deg              | deg              
!   18| tmzone                           | Time zone in hours                                                | hr               | hr               
!   19| site_elevation                   | Site elevation above sea level                                    | m                | m                
!   20| T_mains                          | Mains water temperature                                           | C                | C                
!   21| Month                            | Month of the year                                                 | none             | none             
!   22| Hour_of_month                    | Hour of the month                                                 | hr               | hr               
!   23| day_of_month                     | Day of the month                                                  | day              | day              
!   24| time_last                        | TRNSYS time at the last weather file read                         | hr               | hr               
!   25| time_next                        | TRNSYS time at the next weather file read                         | hr               | hr               
!   26| forecast                         | Forecasted average resource level over next fchrs hours           | W/m2             | W/m2             

!-----------------------------------------------------------------------------------------------------------------------
!    TRNSYS acess functions (allow to acess TIME etc.) 
USE TrnsysConstants
USE TrnsysFunctions
use CSPGeneric_tools !Contains array searching algorithms

!-----------------------------------------------------------------------------------------------------------------------
! required by the multi-dll version of TRNSYS
!DEC$ATTRIBUTES DLLEXPORT :: TYPE215

implicit none 

!TRNSYS declarations
real(8):: time
integer*4:: info(15), iunit, itype, icntrl
integer*4,parameter::np=7,ni=0,nout=27,nd=0,ns=0 

!Dimension the TRNSYS variables
real(8)::xin(ni),out(nout),par(np),stored(ns),T(nd),dtdt(nd) 

logical:: is_there, interp
integer:: LU_w, i, j, ios, tmzone, commas(20), nwf, nwfcol, pmode, wbmode, IE, def_psych_mode, index, index0,&
          lb, ub, pstat, index1, monthhr(12), j_max, j_min, idb, n, c1, c2, fcvar, nfc, errcall
real(8):: x, latitude, longitude, elevation, dttrn, dtwf, timecalc0, timecalc, timetrn, tstart, tstop, step, &
          shift, psydat(9), timewflast, timewfnow, timewfnext, timedtlb, timedtub, sol_elev, sol_zen, sol_azi,&
          T_mains, month, hour_of_month, day_of_year, day_of_month, offset, mains_ratio, annual_ave_T, dt_max, &
          month_ave_T(0:13), xday, lag, nwf_pm(12), rhog, rhog_snow, time_last, time_next, hour_of_day, wfsthr, &
          wfstmin, wfstsec, timewfstart, wfstyear, fchrs
real(8),parameter:: small=1.e-6
real(8),allocatable:: wfdata(:,:), wftime(:), wfmonth(:), forecast(:)
character(len=100):: fname, statid, location, state, tmztxt, lattxt, longtxt, elevtxt
character:: temptxt*500


!-----------------------------------------------------------------------------------------------------------------------

!Define TRNSYS time variables in seconds
dttrn = getSimulationTimeStep()*3600.d0  !Trnsys reported time step [s]  
timetrn = time*3600.d0              !Trnsys reported time [s]
timecalc = timetrn - dttrn/2.d0        !Trnsys calculation time [s]


! --- Initial call to detect the TRNSYS version for which this Type is written -----------------------------------------
if (info(7) .eq. -2) then
    info(12) = 16   ! This component is a TRNSYS 16 Type
    return 1
endif


! --- Very last call in simulation -------------------------------------------------------------------------------------

if (info(8) == -1) then
    if(allocated(wfdata)) deallocate(wfdata,wftime,wfmonth,forecast)
    if(allocated(eta)) deallocate(eta,azms,zens,azms1d,zens1d)
    return 1    ! Exit 
endif



! --- Post-convergence call --------------------------------------------------------------------------------------------
if (info(13) == 1) then

    !******************************************************************
    ! Set the system state values for the next timestep 
    !******************************************************************
    !call setStorageVars(stored,nS,INFO)
    
    return 1    ! Exit - End of the routine for post-convergence calls
    
endif


! --- Second call in simulation: initialization call (not a simulation call) -------------------------------------------
!---- read static parameters---
if (info(7) .eq. -1) then
    !******************************************************************************************************************************
    !               Plant configuration inputs
    !******************************************************************************************************************************
    !    1| LU_w                             | Assigned logical unit for the weather file                        | none             | none             
    LU_w = int(PAR(1))
    !    2| def_psych_mode                   | Default psych property mode                                       | none             | none             
    def_psych_mode = int(PAR(2))
    ! Interpolate values?
    interp = .false.
    if(PAR(3)==1) interp = .true.
    !Ground reflectance
    rhog = PAR(4)
    !Ground reflectance with snow
    rhog_snow = PAR(5)
    !Forecast variable (1=GHI, 2=DNI, 3=DHI)
    fcvar = jfix(PAR(6)+.1d0)  !2
    !Forecast hours [s]
    fchrs = PAR(7)*3600.  !3
    
    !Make sure the weather file is connected and available
    inquire(unit=LU_w, opened=is_there)
    if(.not.is_there) then
        call messages(-1, "Error accessing weather file", INFO(1), INFO(2))
        return 1
    endif
    
    errcall = 0 !mjw 9.2.11 Flag to make sure the psych property warning message is only printed once

    !Station/ID#, Location name, State, time zone, latitude, longitude, elevation, timestep
    !parse header for comma positions
    rewind(LU_w)
    read(LU_w, fmt='(A)') temptxt
    j=0; commas(:)=0
    do i=1,len(trim(temptxt))
        if(temptxt(i:i)==',') then 
            j=j+1
            commas(j)=i
        endif
    enddo
    !Check to make sure all the header info was supplied
    if(j<9) call messages(-1,"The header information in the weather file is incomplete. Please refer to Type215 documentation for required header fields and format","FATAL",INFO(1), INFO(2))
    !Load header data
    statid = temptxt(1:commas(1)-1)
    location = temptxt(commas(1)+1:commas(2)-1)
    state = temptxt(commas(2)+1:commas(3)-1)
    read(temptxt(commas(3)+1:commas(4)-1), fmt=*) tmzone
    read(temptxt(commas(4)+1:commas(5)-1), fmt=*) latitude
    read(temptxt(commas(5)+1:commas(6)-1), fmt=*) longitude
    read(temptxt(commas(6)+1:commas(7)-1), fmt=*) elevation
    read(temptxt(commas(7)+1:commas(8)-1), fmt=*) dtwf
    read(temptxt(commas(8)+1:commas(9)-1), fmt=*) wfstyear
    !Parse the starting time from the format hr:min:sec
    temptxt = temptxt(commas(9)+1:commas(10)-1)
    do i=1,len(trim(temptxt))
        if(temptxt(i:i)==":") temptxt(i:i)=' '
    enddo
    read(temptxt,fmt=*) wfsthr, wfstmin, wfstsec
    timewfstart = wfsthr*3600.d0 + wfstmin*60.d0 + wfstsec  ![s] starting time of the weather file data
    
    !Find out how many entries there are
    nwf=0
    do 
        read(LU_w, fmt='(A1)',iostat=ios) temptxt
        if(ios/=0) exit
        nwf = nwf+1
    enddo
    rewind(LU_w); read(LU_w, fmt=*) !Go to the second line

    !Allocate space for the data array
    nwfcol = 12
    if(.not.allocated(wfdata)) allocate(wfdata(nwfcol,nwf),wftime(nwf),wfmonth(nwf),forecast(nwf))
    !Read in all the entries
    do i=1,nwf
        read(LU_w, fmt=*) (wfdata(j,i), j=1,nwfcol)
    enddo
    rewind(LU_w)
    
    !Preprocess the albedo column based on what's provided
    do i=1,nwf
        if((wfdata(11,i)<0.).or.(wfdata(11,i)>1.)) then !Check if the albedo column is nonsense
            !Check if the ground has snow cover or not
            if(wfdata(12,i) > 0.) then
                wfdata(11,i) = rhog_snow
            else
                wfdata(11,i) = rhog
            endif
        endif
    enddo
    
    !----Calculate the forecast at each timestep
    !Calculate the number of timesteps that the forecast will include
    nfc = int(fchrs/dtwf+0.01)
    !first step
    forecast(1) = sum(wfdata(7+fcvar,2:nfc+1))/float(nfc)
    do i=2,nwf
        if(i+nfc < nwf) then
            j = i+nfc
        else
            j = nwf
        endif
        forecast(i) = forecast(i-1)+wfdata(7+fcvar,j)/float(nfc) - wfdata(7+fcvar, i)/float(nfc)
    enddo
    
    !----Calculate psych properties so that all are defined----
    idb = 1     !Save the dry-bulb temperature index for later use
    !Convert time to time of the year in seconds
    do i=1,nwf
        !weather file time: reconstruct from header info
        !wftime(i)=(wfdata(1,i)-1.)*86400. + wfdata(2,i)
        wftime(i) = timewfstart + float(i-1)*dtwf
    enddo
    !First, try to use the default psych mode
    if(wfdata(def_psych_mode+1,1)/=-999.) then
        select case(def_psych_mode)
        case(1)
            pmode = 3
        case(2)
            pmode = 1
        case(3)
            pmode = 2
        end select
    else    !The default psych mode tries to use unavailable data 
        if(wfdata(2,1)/=-999.) then  !Dew-point temperature
            pmode = 3
        elseif(wfdata(3,1)/=-999.) then  !Wet-bulb temperature
            pmode = 1
        elseif(wfdata(4,1)/=-999.) then  !Relative humidity
            pmode = 2
        else
            call messages(-1,"Weather file specifies only two psychrometric properties while three are required. "//&
                             "In addition to Dry-bulb temp. and atmospheric pressure, specify dew-point temp, wet-"//&
                             "bulb temp, or relative humidity","FATAL",INFO(1),INFO(2))
        endif
    endif
    ie=0
    do i=1,nwf
        !psydat(:)=0.d0
        psydat(1) = wfdata(7,i)*0.001D0/1.01325D0 ![] Atmospheric pressure
        psydat(2) = wfdata(1,i) ![] Dry-bulb temperature
        wbmode = 1
        select case(pmode)
        case(1) !Specify wet-bulb temperature
            psydat(3) = wfdata(3,i)
            wbmode = 0
        case(2) !Specify relative humidity
            psydat(4) = wfdata(4,i)/100.
        case(3) !Specify dew-point temperature
            psydat(5) = wfdata(2,i)
        end select
        call psychrometrics(time,info,1,pmode,wbmode,psydat,ie,pstat,*100)
        100 select case(pmode)
        case(1)
            wfdata(4,i) = dmax1(dmin1(psydat(4)*100., 100.d0), 0.d0)  !mjw 9.2.11 Some sets of values can return unreasonable numbers
            !mjw 9.2.11 Some combinations of twb and tdb return nonsensical values for twb. If this happens, estimate the value using other data.
            if((psydat(5)<-100.).or.(psydat(5)>psydat(2))) then
                if(errcall==0) then
                    call messages(-1,"TRNSYS detected a problem with the psycrometric properties provided in the weather file. Non-physical wet-bulb/dry-bulb temperature combination.","WARNING",info(1),info(2))
                    errcall = 1
                endif
            else
                goto 101
            endif
            if(psydat(5)<-100.) then
                if(i>1) then
                    psydat(5) = wfdata(2,i-1) !estimate as the value from the previous time step
                else
                    psydat(5) = psydat(3) !estimate as the wet bulb temperature
                endif
            endif
            101 continue
            wfdata(2,i) = dmin1(psydat(5),psydat(2)) !the dewpoint temperature should never exceed the dry bulb temperature
        case(2)
            wfdata(3,i) = psydat(3)
            wfdata(2,i) = psydat(5)
        case(3)
            wfdata(3,i) = psydat(3)
            wfdata(4,i) = psydat(4)*100.
        end select
        if(errorfound()) return 1
    enddo      
     
    !Calculate shift
    shift = longitude - 15.*tmzone ![deg] This convention is consistent with Type16 
    
    !Time start/stop/step info
    tstart = getSimulationStartTime()*3600.d0 ![s]
    tstop = getSimulationStopTime()*3600.d0  ![s]
    step = getSimulationTimeStep()*3600.d0  ![s]    !TRNSYS time step
    
    !---------Set other required values
    !declare the array of starting hours for each month
    monthhr = (/0,744,1416,2160,2880,3624,4344,5088,5832,6552,7296,8016/)    !Hour of the year on which the new month starts
    !Associate each weather file entry with a particular month
    do i=1,nwf
        j = jfix((wftime(i)+.1d0)/3600.d0) !Hour of the year
        if(j<monthhr(2)) then
            month = 1.d0
        elseif((j>=monthhr(2)).and.(j<monthhr(3))) then
            month = 2.d0
        elseif((j>=monthhr(3)).and.(j<monthhr(4))) then
            month = 3.d0
        elseif((j>=monthhr(4)).and.(j<monthhr(5))) then
            month = 4.d0
        elseif((j>=monthhr(5)).and.(j<monthhr(6))) then
            month = 5.d0
        elseif((j>=monthhr(6)).and.(j<monthhr(7))) then
            month = 6.d0
        elseif((j>=monthhr(7)).and.(j<monthhr(8))) then
            month = 7.d0
        elseif((j>=monthhr(8)).and.(j<monthhr(9))) then
            month = 8.d0
        elseif((j>=monthhr(9)).and.(j<monthhr(10))) then
            month = 9.d0
        elseif((j>=monthhr(10)).and.(j<monthhr(11))) then
            month = 10.d0
        elseif((j>=monthhr(11)).and.(j<monthhr(12))) then
            month = 11.d0
        elseif(j>=monthhr(12)) then
            month = 12.d0
        endif
        wfmonth(i) = month       
    enddo

    !Calculate the average monthly dry bulb temperatures
    nwf_pm = (/31.d0,28.d0,31.d0,30.d0,31.d0,30.d0,31.d0,31.d0,30.d0,31.d0,30.d0,31.d0/)
    do i=1,12
        nwf_pm(i) = nwf_pm(i)*24.d0*3600.d0/dtwf  ![real] Calculate the number of weather file entries per month
    enddo
    do i=1,nwf
        j = jfix(wfmonth(i)+.1) !integer indicating the current month
        month_ave_T(j) = month_ave_T(j) + wfdata(idb,i)/nwf_pm(j)
    enddo
    !Specify wrap-around values
    month_ave_T(0) = month_ave_T(12)
    month_ave_T(13) = month_ave_T(1)
    !Calculate the annual average temperature
    annual_ave_T = sum(month_ave_T)/12.d0
    
    !---- Initialize outputs
    out(1:nout) = 0.d0
    info(6)=nout
    call typeck(1,info,nI,nP,nD)


    !---- Set storage array size 
    CALL setStorageSize(nS,INFO)

    !return 1
    !The same calculations apply for initialization as for each time step iteration. Thus, instead of duplicating code
    !the best thing is to continue on and run through all the calculations instead of returning. This only works if
    !we don't have to set any values for storage. Right now the storage array size is zero and storage isn't used.
endif

!!Initial call - set storage variables
!if(timetrn<dttrn/2.d0) then
!    return 1
!endif

!-----------------Every iteration-------------------

!Get stored variables
!call GetStorageVars(stored, nS, INFO)

!Determine the beginning data index based on the start time and the time information provided in the weather file
call bounds(timecalc+small,nwf,wftime,lb,ub,index0)
index = index0+1
index1 = index+1

!Define all of the weather file time values used
if(index0 == 1) then
    timewflast = 0.d0
    timewfnow = wftime(index)
    timewfnext = wftime(index1)
elseif(index0 == nwf-1) then
    index1 = index
    timewflast = wftime(index0)
    timewfnow = wftime(index)
    timewfnext = wftime(index1)
else
    index = index0+1
    timewflast = wftime(index0)
    timewfnow = wftime(index)
    timewfnext = wftime(index1)
endif

!Keep track of the TRNSYS time at the last weather file read
time_last = (timewflast - dtwf/2.d0)/3600.d0
time_next = time_last + dtwf/3600.d0

!Calculate all of the weather file information to report
if(interp) then
    !Interpolate the values
    do i=1,nwfcol
        out(i+2) = (timecalc - timewflast)/(timewfnow - timewflast)*(wfdata(i,index) - wfdata(i,index0)) + wfdata(i,index0)
    enddo
else
    timedtlb = timecalc - dttrn/2.d0
    timedtub = timecalc + dttrn/2.d0
    if(timewflast + dtwf/2.d0 - timedtlb >= 0.d0) then
        !The starting time (lower bound) of the current timestep lies within the last weather file data period
        if(timewflast + dtwf/2.d0 - timedtub >= 0.d0) then
            !The ending time of the current timestep also lies within the last weather file data period
            do i=1,nwfcol
                out(i+2) = wfdata(i,index0)
            enddo
        else
            !The ending time of the current timestep falls in the current weather file period, while the beginning time falls in the previous.
            !The reported value will be a weighted average of the two portions of the current time step (each falling in a different weather file period).
            do i=1,nwfcol
                out(i+2) = wfdata(i,index0)*(timewflast + dtwf/2.d0 - timedtlb)/dttrn + wfdata(i,index)*(timedtub - (timewflast + dtwf/2.d0))/dttrn
            enddo
        endif
    else
        !The lower bound of the current timestep falls within the current weather file data period
        if(timewfnow + dtwf/2.d0 - timedtub >= 0.d0) then
            !The ending time for the current timestep lies in the current weather file data period
            do i=1,nwfcol
                out(i+2) = wfdata(i,index)
            enddo
        else
            !The ending time for the current timestep lies in the next weather file data period while the beginning time falls in the current.
            !The reported value will be a weighted average of the two portions of the current time step (each falling in a different weather file period).
            do i=1,nwfcol
                out(i+2) = wfdata(i,index)*(timewfnow + dtwf/2.d0 - timedtlb)/dttrn + wfdata(i,index1)*(timedtub - (timewfnow + dtwf/2.d0))/dttrn
            enddo
        endif
    endif

endif

!------------Calculate the other weather file outputs
!month of the year
do i=1,12
    month = dble(13-i)
    if(timetrn/3600.d0 >= dble(monthhr(13-i))) exit
enddo
!hour of the month
hour_of_month = timetrn/3600.d0 - dble(monthhr(nint(month)))
!Day of the year
day_of_year = jfix(timetrn/86400.d0) + 1.d0
!Day of the month
day_of_month = day_of_year - dble(monthhr(nint(month)))/24.d0
!Hour of the day in TRNSYS time
hour_of_day = dmod(time,24.)

!-----Calculate the mains water temperature
!based on an algorithm by Craig Christensen and Jay Burch of NREL
j_min=1
j_max=1
do J=2,12
  if(MONTH_AVE_T(j) > MONTH_AVE_T(j-1)) then
     j_max=j
  endif

  if(MONTH_AVE_T(j) < MONTH_AVE_T(j-1)) then
     j_min=j
  endif
enddo
OFFSET=6.
MAINS_RATIO=0.4+0.01*((ANNUAL_AVE_T*1.8+32.)-44.)
DT_MAX=(MONTH_AVE_T(J_MAX)-MONTH_AVE_T(J_MIN))*1.8
XDAY=DMOD(TIME,8760.D0)/24.
LAG=35.-((ANNUAL_AVE_T*1.8+32.)-44.)
T_MAINS=((ANNUAL_AVE_T*1.8+32.)+OFFSET)+MAINS_RATIO*(DT_MAX/2.)*DSIN((0.986*(XDAY-15-LAG)-90)*0.017453292)
T_MAINS=(T_MAINS-32.)/1.8


!-----Set the remaining output values
out(1) = day_of_year
out(2) = timewflast/3600.d0 !hr
out(nwfcol+3) = shift       !deg
out(nwfcol+4) = latitude
out(nwfcol+5) = longitude
out(nwfcol+6) = tmzone      !hr
out(nwfcol+7) = elevation   !m
out(nwfcol+8) = T_mains     !C
out(nwfcol+9) = Month
out(nwfcol+10) = Hour_of_month
out(nwfcol+11) = day_of_month
out(nwfcol+12) = time_last  !hr
out(nwfcol+13) = time_next
out(nwfcol+14) = hour_of_day
out(nwfcol+15) = forecast(index0)  !W/m2  Forecasted for the next X hours

return 1

end subroutine