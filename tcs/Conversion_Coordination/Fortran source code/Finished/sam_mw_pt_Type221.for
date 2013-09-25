      subroutine TYPE221(time,xin,out,t,dtdt,par,info,icntrl,*)

C****************************************************************************
c Trnsys Component for Heliostat Field 
c Created by Michael Wagner
c University of Wisconsin - Madison
c Solar Energy Lab
c First revised: 12/3/2008
c Last revision: 5/22/2009
! COPYRIGHT 2009 NATIONAL RENEWABLE ENERGY LABORATORY
c ---------|
c Component is based on and modified from:
c STEC library TYPE394
c Created by Robert Pitz-Paal
c Energy Technology Division
c DLR
c ----------|
C****************************************************************************

! Doc. tables updated 7/1/2010 - MJW
!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Parameters
!    1| IUFELD                           | Logical unit no of field efficiency input data file               | none             | none             
!    2| NEL                              | Number of zenith angle data points in file                        | none             | none             
!    3| NAZ                              | Number of azimuth angle data points in file                       | none             | none             
!    4| NHEL                             | Number of heliostats in the field                                 | none             | none             
!    5| QSTART                           | Electric work for starting up one heliostat                       | kWe-hr           | kJ               
!    6| PRUN                             | Electric power for tracking one heliostat                         | kWe              | kJ/hr            
!    7| VMAX                             | Maximum tolerable wind speed                                      | m/s              | m/s              
!    8| hel_stow_deploy                  | Heliostat field stow/deploy solar elevation angle                 | deg              | deg              

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Inputs
!    1| VWIND                            | Wind velocity                                                     | m/s              | m/s              
!    2| Field_control                    | Field defocus control                                             | none             | none             
!    3| THETA                            | Solar zenith angle                                                | deg              | deg              
!    4| PHI                              | Solar azimuth angle                                               | deg              | deg              

!--------------------------------------------------------------------------------------------------------------------------------------------
! Nb  | Variable                         | Description                                                       | Input units      | Local units      
!--------------------------------------------------------------------------------------------------------------------------------------------
!Outputs
!    1| PPARASi                          | Parasitic tracking/startup power                                  | MWe              | kJ/hr            
!    2| ETAFELD                          | Total field efficiency                                            | none             | none             


C****************************************************************************
      
C    TRNSYS acess functions (allow to acess TIME etc.) 
      USE TrnsysConstants
      USE TrnsysFunctions

C-----------------------------------------------------------------------------------------------------------------------
C    REQUIRED BY THE MULTI-DLL VERSION OF TRNSYS
      !DEC$ATTRIBUTES DLLEXPORT :: TYPE221				
C-----------------------------------------------------------------------------------------------------------------------
C-----------------------------------------------------------------------------------------------------------------------
C    TRNSYS DECLARATIONS
      IMPLICIT NONE			!REQUIRES THE USER TO DEFINE ALL VARIABLES BEFORE USING THEM

	real(8):: XIN, OUT, time, par, stored, t, dtdt
	integer:: info(15), np, ni, no, nd, iunit, itype, icntrl, ns
	CHARACTER*3 OCHECK		!AN ARRAY TO BE FILLED WITH THE CORRECT VARIABLE TYPES FOR THE OUTPUTS
	CHARACTER*3 YCHECK		!AN ARRAY TO BE FILLED WITH THE CORRECT VARIABLE TYPES FOR THE INPUTS
C-----------------------------------------------------------------------------------------------------------------------

C-----------------------------------------------------------------------------------------------------------------------
C    USER DECLARATIONS - SET THE MAXIMUM NUMBER OF PARAMETERS (NP), INPUTS (NI),
C    OUTPUTS (NO), AND DERIVATIVES (ND) THAT MAY BE SUPPLIED FOR THIS TYPE
      PARAMETER (NP=8,NI=4,NO=2,ND=0,NS=2)
C-----------------------------------------------------------------------------------------------------------------------

C-----------------------------------------------------------------------------------------------------------------------
C    REQUIRED TRNSYS DIMENSIONS
      DIMENSION XIN(NI),OUT(NO),PAR(NP),YCHECK(NI),OCHECK(NO),
	1   STORED(NS),T(ND),DTDT(ND)
      INTEGER NITEMS, IUFELD, NX(2), NEL, NAZ, NHEL, ios, i
C-----------------------------------------------------------------------------------------------------------------------
C-----------------------------------------------------------------------------------------------------------------------
C    ADD DECLARATIONS AND DEFINITIONS FOR THE USER-VARIABLES HERE

	DOUBLE PRECISION X(2),
     & VWIND,		!Wind Speed      m/s 
     & Field_control,		!Control Parameter(range from 0 to 1)
     & THETA,		!Solar zenith angle 
     & PHI,		!Solar azimuth
     & QSTART,	!Electric work for Startup of one heliostat (KJ)    	
     & PRUN,		!Electric power for tracking one heliostat (kJ/h)   
     & VMAX,		!Maximum tolerable wind speed m/s
     & GAMOLD,
     & VWOLD,
     & PPARASI,
     & ETAFELD,
     & DELT,
     & TIME0,
     & TIMEF,
     & hel_stow_deploy  !Solar angle stow/deploy for heliostat startup/shutdown
     
      LOGICAL is_there, is_open
      character::test*200
                                                 	
C    SET THE INPUT AND OUTPUT ARRAYS
      DATA YCHECK/'VE1','CF1','DG1','DG1'/
      DATA OCHECK/'PW1','CF1'/

C--------------------------------------------------------------------------------------------------
C    GET GLOBAL TRNSYS SIMULATION VARIABLES
      TIME0 = getSimulationStartTime()
      DELT  = getSimulationTimeStep()
	TIMEF = getSimulationStopTime()

C--------------------------------------------------------------------------------------------------
C    SET THE VERSION INFORMATION FOR TRNSYS
      IF(INFO(7).EQ.-2) THEN
	  INFO(12)=16
	  RETURN 1
	ENDIF

C-----------------------------------------------------------------------------------------------------------------------
C    DO ALL THE VERY LAST CALL OF THE SIMULATION MANIPULATIONS HERE
      IF (INFO(8).EQ.-1) THEN
	   RETURN 1
	ENDIF
C-----------------------------------------------------------------------------------------------------------------------

C-----------------------------------------------------------------------------------------------------------------------
C    PERFORM ANY POST-CONVERGENCE MANIPULATIONS THAT ARE REQUIRED HERE
      IF (INFO(13).GT.0) THEN
	   STORED(1) = ETAFELD  !This timestep's field efficiency
	   STORED(2) = XIN(1)  !This timestep's wind speed
	   CALL setStorageVars(STORED,NS,INFO)
	   RETURN 1
	ENDIF
C
C-----------------------------------------------------------------------------------------------------------------------

C--------------------------------------------------------------------------------------------------
C    PERFORM FIRST CALL MANIPULATIONS
      IF (INFO(7).EQ.-1) THEN
        !retrieve unit and type number for this component from the INFO array
        IUNIT=INFO(1)
	  ITYPE=INFO(2)
        !set some info array variables to tell the trnsys engine how this type is to work
        INFO(6)  = NO   !reserve space in the OUT array using INFO(6)
        INFO(9)  = 1    !set the way in which this component is to be called
	  INFO(10) = 0    !set required number of spots in the single precision storage structure
        !reserve space in the double precision storage structure
	  CALL setStorageSize(NS,INFO)

        IUFELD  = PAR(1)


        !We need to check to make sure that the fluxmap file has been successfully
        !created by the PTGEN program.  Do this by counting the number of lines 
        !in the file to make sure that it is not zero or close to zero.
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        i=0 !initialize counting variable
        do 
          i=i+1
          if (i.gt.10) goto 10
          read(unit=iufeld,fmt="(A)",err=10,eor=10,end=10, 
     &         advance="YES") test
        enddo

10      continue

        if(i.le.2) then
          call messages(-1,
     &                "TRNSYS did not find the field efficiency file"
     &               ,'FATAL',INFO(1),INFO(2))
          stop  !Quit the program here, returning through the hierarchy is cumbersome
        endif
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  

        rewind(iufeld)        

        !return to the calling program
        RETURN 1
      ENDIF

C--------------------------------------------------------------------------------------------------
C    PERFORM INITIAL TIMESTEP MANIPULATIONS
      IF (TIME.LT.(TIME0+DELT/2.d0)) THEN
        !set the UNIT number for future calls
        IUNIT=INFO(1)
	  ITYPE=INFO(2)
        !read the first parameter values - there are other parameters used directly in the
	  !code without first being set to local variables.

c      Logical Unit No of Field efficiency input data (File)
        IUFELD  = PAR(1)
c      Number of zenith angle data points in field efficiency file
        NEL     = PAR(2)
c      Number of azimuth angle data points in field efficiency file
        NAZ     = PAR(3)
c      Number of Heliostats      
        NHEL    = PAR(4)          
c       Electric work for Startup of one heliostat (kWe-hr)      
        QSTART  = PAR(5)*3600. !Convert kWe-hr to kJ
c       Electric power for tracking one heliostat (kWe)      
        PRUN    = PAR(6)*3600.  !Convert kWe to kJ/h
c       Maximum tolerable wind speed m/s
        VMAX    = PAR(7)   
c       Solar Stow/deploy angle for heliostat startup           
        hel_stow_deploy = PAR(8)

	  !check the parameters for problems and RETURN if any are found

        !initialize storage variables
	  STORED(1) = 0.0  !Set initial field efficiency stored variable to zero
	  STORED(2) = XIN(2)
	  CALL setStorageVars(STORED,NS,INFO)


	  !set the outputs to appropriate initial values.
	  OUT(1) = 0.d0
        OUT(2) = 0.d0  !XIN(3)

        RETURN 1         !the first timestep is for initialization - exit.
      ENDIF
C-----------------------------------------------------------------------------------------------------------------------

C--------------------------------------------------------------------------------------------------
C    THIS IS AN ITERATIVE CALL TO THIS COMPONENT ***
C--------------------------------------------------------------------------------------------------

C    RE-READ THE PARAMETERS IF ANOTHER UNIT OF THIS TYPE HAS BEEN CALLED SINCE THE LAST 
C    TIME THEY WERE READ IN
      IF(INFO(1).NE.IUNIT) THEN
        !recall the UNIT and TYPE number
        IUNIT = INFO(1)
	  ITYPE = INFO(2)
        !read parameter values
        IUFELD  = PAR(1)
        NEL     = PAR(2)
        NAZ     = PAR(3)
        NHEL    = PAR(4)          
        QSTART  = PAR(5)*3600. !Convert kWe-hr to kJ
        PRUN    = PAR(6)*3600.  !Convert kWe to kJ/h
        VMAX    = PAR(7)   
        hel_stow_deploy = PAR(8)
	ENDIF

C--------------------------------------------------------------------------------------------------
C    GET AND CHECK INPUT VALUES

c     Wind Speed      m/s
      VWIND = XIN(1) 
c     Control Parameter ( range from 0 to 1; 0=off, 1=all on)
      Field_control = xin(2)
      IF(Field_control.GT.1.0) Field_control=1.0
      IF(Field_control.LT.0.d0)   Field_control=0.d0
c      Solar zenith angle (is delivered by Standard trnsys type 15)
      THETA = XIN(3)
C       No tracking before sunrise and after sunset
      IF(THETA.GE.90) Field_control=0.d0
c     Solar azimuth (is delivered by Standard trnsys type 16 'Solar Radiation Processor')
      PHI   = XIN(4)
      if(PHI<0.d0) PHI = PHI+360.d0  !mjw 6.21.11
      
c Control Parameter Field_control and windspeed of last time step is retrieved from S-Array
	CALL getStorageVars(STORED,NS,INFO)
      GAMOLD  = STORED(1)
      VWOLD   = STORED(2)
      
c Parasitics for startup or shutdown
      PPARASI =0.d0
cc   ********startup by setting of control paramter (Field_control 0-> 1)  
      IF( (Field_control.gt.1.e-4 .AND. GAMOLD.lt.1.e-4) .OR.
cc   ********startup by setting of control paramter (Field_control 1->0 ) 
     #    (Field_control.lt.1.e-4 .AND. GAMOLD.ge.1.e-4) .OR.
c    shutdown by too high wind speed     
     #    (Field_control.gt.1.e-4 .AND. VWIND.GE.VMAX ).OR.
c    startup after too high windspeed     
     # ((GAMOLD.GT.1.e-4).AND.(VWOLD.GE.VMAX).AND.(VWIND.LT.VMAX))) THEN
c   Parasitics for startup/shutdown     
        PPARASI = NHEL * QSTART/DELT 
      end if          
      
c  Parasitics for tracking      
      IF(VWIND.LT.VMAX .AND. VWOLD.LT.VMAX) 
     # PPARASI = PPARASI + NHEL * PRUN * Field_control
      
c Read Field efficiency value from data file 
C (example field efficiency data are reported  for example in 
c the Second Generation study of Becker/Klimas)
c Interpolate to find the efficiency value using dynamicdata call.
      NX(1) = NEL
      NX(2) = NAZ
      X(1)  = THETA
      X(2)  = PHI
      CALL dynamicdata(IUFELD,2,NX,1,X,ETAFELD,INFO,*100)     
100   ETAFELD = DMIN1(DMAX1(ETAFELD,0.),1.0)
      IF((THETA.GE.90.).or.((90.-THETA).LT.DMAX1(hel_stow_deploy,0.1)))  !MJW
     & ETAFELD=1.e-6
      IF (VWIND.LT.VMAX) THEN
        ETAFELD = DMAX1(ETAFELD*Field_control,1.e-6)
      ELSE 
        ETAFELD = 1.e-6
      ENDIF
 
c Output Parameters are set:
c      Parasitic power for tracking (kJ/h)
      OUT(1) = PPARASi/3.6e6  !Convert from kJ/hr to MW
c     field efficiency
      OUT(2) = ETAFELD
                                
      continue
      return 1
      end
