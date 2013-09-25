!----------------------------------------------------------------------------------------------------------------------
!                   Analytical view factor subroutines for Type 232
!                   *Reference : Engineering Equation Solver. 
!----------------------------------------------------------------------------------------------------------------------


!----------------------------------------------------------------------------------------------------------------------

!View factor calculcations for the given geometry

!This programs returns the view factors from each panel 1-4, counted from one outer panel to the other side (assuming symmetric setup), to its surroundings.
!The calculations can only be made for receivers with four panels which are of equal size.

subroutine panelviewfactors(N,theta,R,H,L,F_A_B,F_A_C,F_A_D,F_A_O,F_A_L,F_B_O,F_B_L)
implicit none

!----------------------------------------------------------------------------------------------------------------------
!-Inputs
!   * N         |   Number of nodes in the vertical direction
!   * theta     |   Angle of the extent of the cylinder (i.e. 180deg for a half-cylinder)
!   * R         |   Radius of the receiver surface cylinder
!   * H         |   Panel height
!   * L         |   height of the upper lip (equal to the vertical direction offset of the panels to the aperture)
!----------------------------------------------------------------------------------------------------------------------
!-outputs
!   * F_A_B     |   View factors between nodes of panel A and nodes of panel B
!   * F_A_C     |   View factors between nodes of panel A and nodes of panel C
!   * F_A_D     |   View factors between nodes of panel A and nodes of panel D
!   * F_A_O     |   View factors between nodes of panel A and opening (O)
!   * F_A_L     |   View factors between nodes of panel A and lip (L)
!   * F_B_O     |   View factors between nodes of panel B and opening (O)
!   * F_B_L     |   View factors between nodes of panel B and opening (L)
!----------------------------------------------------------------------------------------------------------------------


integer,intent(in):: N
real(8),intent(in):: theta, R, H, L
real(8),intent(out),dimension(N):: F_A_B,F_A_C,F_A_D,F_A_O,F_A_L,F_B_O,F_B_L
real(8):: alpha, H_N, W, c, z, phi_1, phi_2, phi_3, phi_4, phi_5, a_1, a_2, a_3, a_4, zero, pi, F3D_30
integer:: i

zero = 0.d0; pi=3.1415926

!Geometric relations
alpha	= theta/4
H_N	= H/N                               !height of a panel node
W = 2*R*sin(alpha/2)                    !panel width if panels have equal size
c = 2*R*sin(PI-2*alpha)                 !distance between the vertical aperture edges if the aperature is considered to be at the outer edges of the outer panels
z = R*cos(PI-2*alpha)                   !distance between the aperature plane and the centerline of the panel circle
phi_1 = PI-alpha                        !angle between two adjacent panels
phi_2 = PI-2*alpha                      !angle between two non-adjacent panels with one panel in between
phi_3 = PI-3*alpha                      !angle between two non-adjacent panels with two panels in between
phi_4 = (theta-alpha)/2.                !angle between the aperture plane and an outer panel (1)
phi_5 = alpha/2.                        !angle between the aperture plane and an inner panel (2)
a_1 = W/(2.*cos(alpha))                 !distance from inner panel edge to intersection edge of both panel planes for view factor calculation of panels with one panel in between
a_2 = R*sin(alpha)/sin((pi-3.*alpha)/2.)    !distance from inner panel edge to intersection edge of both panel planes for view factor calculation of panels with two panels in between
a_3 = (R+z)/sin(alpha/2.)-W             !distance from panel 2 to intersection edge of panel planes 2 and a
a_4	= (R+z)/tan(alpha/2.)-c/2.          !distance from panel a to intersection edge of panel plane 2 and a

i=0
do 
    if(i>=N) exit

	i=i+1
	
	!View factors between nodes of panel A and nodes of panel B
	F_A_B(i) = F3D_30(zero,W,zero,H_N,dble(i-1)*H_N,i*H_N,zero,W,phi_1)
	!View factors between nodes of panel A and nodes of panel C
	F_A_C(i) = F3D_30(a_1,a_1+W,zero,H_n,dble(i-1)*H_n,i*H_n,a_1,a_1+W,phi_2)
	!View factors between nodes of panel A and nodes of panel D
	F_A_D(i) = F3D_30(a_2,a_2+W,zero,H_n,dble(i-1)*H_n,i*H_n,a_2,a_2+W,phi_3)
	!View factors between nodes of panel A and opening (O)
	F_A_O(N-i+1) = F3D_30(zero,W,dble(i-1)*H_n,i*H_n,L,H,zero,c,phi_4)
	!View factors between nodes of panel A and lip (L)
	F_A_L(N-i+1) = F3D_30(zero,W,dble(i-1)*H_n,i*H_n,zero,L,zero,c,phi_4)
	!View factors between nodes of panel B and opening (O)
	F_B_O(N-i+1) = F3D_30(a_3,a_3+W,dble(i-1)*H_n,i*H_n,L,H,a_4,a_4+c,phi_5)
	!View factors between nodes of panel B and opening (L)
	F_B_L(N-i+1) = F3D_30(a_3,a_3+W,dble(i-1)*H_n,i*H_n,zero,L,a_4,a_4+c,phi_5)
enddo

end subroutine

!----------------------------------------------------------------------------------------------------------------------
real(8) function F3D_30(x_1,x_2,y_1,y_2,eta_1,eta_2,z_1,z_2,theta)
    implicit none
    real(8)::G_1_1_1, G_1_1_2, G_1_2_1, G_1_2_2, G_2_1_1, G_2_1_2, G_2_2_1, G_2_2_2, x_1, x_2, y_1, y_2, eta_1,&
             eta_2, z_1, z_2, theta, F_1_2, G3D30

!Function for calculating the view factor for rectangles with parallel and perpendicular edges and 
!with an arbitrary angle theta between their intersecting planes, the rectangles can't be flush in 
!the direction of the intersection line.
!Reference: http://www.me.utexas.edu/~howell/sectionc/C-17.html.
    G_1_1_1 = G3D30(x_1,y_1,eta_1,z_1,z_2,theta)
    G_1_1_2 = G3D30(x_1,y_1,eta_2,z_1,z_2,theta)
    G_1_2_1 = G3D30(x_1,y_2,eta_1,z_1,z_2,theta)
    G_1_2_2 = G3D30(x_1,y_2,eta_2,z_1,z_2,theta)
    G_2_1_1 = G3D30(x_2,y_1,eta_1,z_1,z_2,theta)
    G_2_1_2 = G3D30(x_2,y_1,eta_2,z_1,z_2,theta)
    G_2_2_1 = G3D30(x_2,y_2,eta_1,z_1,z_2,theta)
    G_2_2_2 = G3D30(x_2,y_2,eta_2,z_1,z_2,theta)
    F_1_2   = (-G_1_1_1+G_2_1_1+G_1_2_1-G_2_2_1+G_1_1_2-G_2_1_2-G_1_2_2+G_2_2_2)/((x_2-x_1)*(y_2-y_1))
    F3D_30 = F_1_2
end function

!----------------------------------------------------------------------------------------------------------------------

real(8) function  G3D30(x,y,eta,xi_1,xi_2,alpha)
    implicit none
    
    real(8)::x,y,eta,xi_1,xi_2,alpha,calG
    
    IF (y==eta) THEN
        y = y + 1e-06 ![m]
    ENDIF
    IF ((x==0) .and. (xi_1==0)) THEN
        x = 1e-06 ![m]
    ENDIF
	
    G3D30 = calG(x,y,eta,xi_1,xi_2,alpha)
end function

!----------------------------------------------------------------------------------------------------------------------

real(8) function calG(x,y,eta,xi_1,xi_2,theta)
    real(8):: x,y,eta,xi_1,xi_2,theta, G, pi, step, xi, v1, v2, v3, dx32, dx21, dv32, vexp, err, tol, minstep, xi0
    integer:: i, n
    logical:: recalc
    pi=3.1415926; recalc = .false.
    tol = 1.e-6 !The allowed slop between the predicted value and the calculated value of v1
    minstep = 1.e-9*(xi_2 - xi_1)   !The minimum allowable step
    
    !This function integrates the expression for G using a flexible step size. The step size for the next step is based on 
    !the magnitude of the second derivative G''. This is evaluated by considering the difference between the predicted 
    !position of the current value based on the previous 2 values and the actual value obtained from the equation. 
    
    step = minstep  !Start with a small step
    
    xi=xi_1; i=0; n=0; G=0.d0; xi0 = xi
    do 
        i=i+1; n=n+1
        
        !Do the following if we aren't recalculating the step    
        if(.not.recalc) then
            v3 = v2 !v3 is equal to the second to last calculated value of v1
            v2 = v1 !v2 is equal to the last calculated value of v1
            dx32 = dx21 !The step size from the previous iteration
        endif
        dx21 = step !The current step size
        
        !Evaluate the expression at xi
        v1 = ((x-xi*cos(theta))*cos(theta)-xi*(sin(theta))**2)/((x**2-2*x*xi*cos(theta)+xi**2)**(.5)*&
               (sin(theta))**2)*atan((eta-y)/(x**2-2*x*xi*cos(theta)+xi**2)**(.5))+ cos(theta)/&
               ((eta-y)*(sin(theta))**2)*((xi**2*(sin(theta))**2+(eta-y)**2)**(.5)*atan((x-xi*cos(theta))/&
               (xi**2*(sin(theta))**2+(eta-y)**2)**(.5))-xi*sin(theta)*atan((x-xi*cos(theta))/(sin(theta))))&
               + xi/(2*(eta-y))*log((x**2-2*x*xi*cos(theta)+xi**2+(eta-y)**2)/(x**2-2*x*xi*cos(theta)+xi**2))
        
        !Do this only on the first iteration
        if(i==1) then
            v2=v1
            v3=v1
            dx32 = step
            dx21 = step
        endif
        
        !Calculate the slope of the previous 2 points
        dv32 = (v2 - v3)/dx32
        !Calculate the predicted current point v1 based on the previous slope
        if(i>2) then
            vexp = v2 + dx21*dv32
        else
            vexp = v1
        endif
        !Determine the relative error in the predicted value
        err = dabs((vexp-v1)/v1)/tol
        
        !If the error exceeds a certain value, we need to recalculate
        if((err>1.).and.(step>minstep)) then
            !Adjust the step size. If the error is greater than 1 (equal to the tolerance), then we need to decrease the step size. 
            if(i>2) step = dmax1(step * 10**(1.d0-err), minstep)
            recalc = .true.
            xi=dmin1(xi0+step,xi_2) !go back and reset the current xi based on the previous xi + the adjusted step
            i=i-1
        else
            !Integrate based on the current iteration. We have to average the value of 'v' over the entire step
            G = G+(v1+v2)/2.d0*step
            
            !Adjust the step size for the next iteration. If the error is greater than 0, then we need to decrease the step size. 
            if(i>2) step = dmax1(step * 10**(1.d0-err), minstep)
        
            recalc = .false.
        
            if(xi>=xi_2) exit
        
            xi0 = xi    !keep track of the last xi position
            xi = dmin1(xi+step,xi_2)
        endif
        
    enddo
    
    !Use the integral to evaluate calG
    calG = - (eta-y)*(sin(theta))**2/(2*pi)*G
    
    
end function



!----------------------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------


!View factor calculation via Monte Carlo Ray Tracing
!technique for the cavity receiver model
!by Lukas Feierabend

!This file includes the “point-in-polygon” routines adapted from: 
!   http://rosettacode.org/wiki/Point_in_polygon_(ray_casting_algorithm)

!----------------------------------------------------------------------------------------------------------------------
module Points_Module  
    implicit none   

    type point     
        real :: x, y  
    end type point  
 
    interface operator (-)     
        module procedure pt_sub  
    end interface   

    interface len     
        module procedure pt_len  
    end interface   

    public :: point  
    private :: pt_sub, pt_len 

    contains   

    function pt_sub(a, b) result(c)    
        type(point), intent(in) :: a, b    
        type(point) :: c     
        c = point(a%x - b%x, a%y - b%y)  
    end function pt_sub   

    function pt_len(a) result(l)    
        type(point), intent(in) :: a    
        real :: l     
        l = sqrt((a%x)**2 + (a%y)**2)  
    end function pt_len 

end module Points_Module
!----------------------------------------------------------------------------------------------------------------------
module Polygons  
    use Points_Module  
    implicit none   
    type polygon     
        type(point), dimension(:), allocatable :: points     
        integer, dimension(:), allocatable :: vertices  
    end type polygon 

    contains   
    
    function create_polygon(pts, vt)    
        type(polygon) :: create_polygon    
        type(point), dimension(:), intent(in) :: pts    
        integer, dimension(:), intent(in) :: vt     
        integer :: np, nv     
        np = size(pts,1)    
        nv = size(vt,1)     
        allocate(create_polygon%points(np), create_polygon%vertices(nv))    
        create_polygon%points = pts    
        create_polygon%vertices = vt   
    end function create_polygon   
    
    subroutine free_polygon(pol)    
        type(polygon), intent(inout) :: pol     
        deallocate(pol%points, pol%vertices)   
    end subroutine free_polygon 
    
end module Polygons
!----------------------------------------------------------------------------------------------------------------------
module Ray_Casting_Algo  
    use Polygons  
    implicit none   
    real, parameter, private :: eps = 0.00001  
    private :: ray_intersects_seg 
    contains   
    function ray_intersects_seg(p0, a0, b0) result(intersect)    
        type(point), intent(in) :: p0, a0, b0    
        logical :: intersect     
        type(point) :: a, b, p    
        real :: m_red, m_blue     
        p = p0    ! let variable "a" be the point with smallest y coordinate    
        if ( a0%y > b0%y ) then       
            b = a0       
            a = b0    
        else       
            a = a0       
            b = b0    
        endif     
        if ( (p%y == a%y) .or. (p%y == b%y) ) p%y = p%y + eps     
        intersect = .false.     
        if ( (p%y > b%y) .or. (p%y < a%y) ) return    
        if ( p%x > max(a%x, b%x) ) return     
        if ( p%x < min(a%x, b%x) ) then       
            intersect = .true.    
        else       
            if ( abs(a%x - b%x) > tiny(a%x) ) then          
                m_red = (b%y - a%y) / (b%x - a%x)       
            else          
                m_red = huge(m_red)       
            endif       
            if ( abs(a%x - p%x) > tiny(a%x) ) then          
                m_blue = (p%y - a%y) / (p%x - a%x)       
            else          
                m_blue = huge(m_blue)       
            endif       
            if ( m_blue >= m_red ) then          
                intersect = .true.       
            else          
                intersect = .false.       
            endif    
        endif   
    end function ray_intersects_seg   

    function point_is_inside(p, pol) result(inside)    
        logical :: inside    
        type(point), intent(in) :: p    
        type(polygon), intent(in) :: pol     
        integer :: i, cnt, pa, pb     
        cnt = 0    
        do i = lbound(pol%vertices,1), ubound(pol%vertices,1), 2       
            pa = pol%vertices(i)       
            pb = pol%vertices(i+1)       
            if ( ray_intersects_seg(p, pol%points(pa), pol%points(pb)) ) cnt = cnt + 1    
        enddo     
        inside = .true.    
        if ( mod(cnt, 2) == 0 ) then       
            inside = .false.    
        endif   
    end function point_is_inside 
end module Ray_Casting_Algo
!----------------------------------------------------------------------------------------------------------------------

!----------------------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------

subroutine OuterPanel_Floor(N_rays,N_nodes,H_rec,H_lip,R_rec,RecAngle,F_AF)
! view factor from all the nodes of an outer panel to the floor surface
use Points_Module  
use Ray_Casting_Algo
implicit none
! Inputs
! N_nodes   number of panel H_node in the vertical direction
! H_rec     receiver panel height
! R_rec     radius of circle where the common panel edges are located
! RecAngle  section of circle which is covered by panels

! Outputs
! F_AF      view factor array for panel A

integer::N_nodes,N_rays,ict,hit,i
double precision::alpha,RecAngle,H_node,H_rec,H_lip,W,c,R_rec,R1,R2,&
                  x,y,yv(5),zv(5),yvR(4),zvR(4),Ptheta,theta,Pphi,phi,y_i,z_i
double precision,dimension(N_nodes),intent(out)::F_AF                                  
double precision,parameter::pi=3.14159265
type(polygon):: targetpolygon
type(point):: pts(5),p  
logical::inside
integer,dimension(N_nodes)::hits   


! number of rays to generate
!N_rays = 500000

! Geometric calculations
alpha = RecAngle*0.25               ! angle coverage of the receiver circle by each of the 4 panels
H_node = H_rec/dble(N_nodes)        ! height of a panel node
W = 2.d0*R_rec*sin(alpha/2.d0)        ! panel width if panels have equal size
c = 2.d0*R_rec*sin(pi-2.d0*alpha) 	! distance between the vertical aperture edges if the aperature is considered to be at the outer edges of the outer panels

        
! Vertices of the floor polygon
yv = (/0.d0, W, W*(1.d0+cos(alpha)), W+2.d0*R_rec*sin(alpha)*cos(alpha+acos(R_rec*sin(alpha)/W)), c*cos(3.d0/2.d0*alpha)/)
zv = (/0.d0, 0.d0, W*sin(alpha), 2.d0*R_rec*sin(alpha)*sin(alpha+acos(R_rec*sin(alpha)/W)), c*sin(3.d0/2.d0*alpha)/)
! Create the polygon 
pts = (/ point(yv(1),zv(1)), point(yv(2),zv(2)), point(yv(3),zv(3)), point(yv(4),zv(4)), point(yv(5),zv(5))/) 
targetpolygon = create_polygon(pts, (/ 1,2, 2,3, 3,4, 4,5, 5,1 /) )
! Vertices of the bounding box for the polygon
yvR = (/0.d0, W*(1.d0+cos(alpha)), W*(1.d0+cos(alpha)), 0.d0/)
zvR = (/0.d0, 0.d0, c*sin(3.d0/2.d0*alpha), c*sin(3.d0/2.d0*alpha)/)

!Initialize random number generation
CALL RANDOM_SEED
!Initialize ray and hit counters
ict = 0
hits = 0
do  
    ! ray counter
    ict = ict+1
    if(ict>N_rays) exit 	
    do i = 1,N_nodes
        ! randomly selected ray direction
        call RANDOM_NUMBER(Ptheta)  ! uniformly distributed random number between 0 and 1
        call RANDOM_NUMBER(Pphi)    ! uniformly distributed random number between 0 and 1        	    
        theta= asin(sqrt(Ptheta))	! determine the polar angle             
        phi=Pphi*2.d0*pi             ! determine the azimuthal angle
        
        ! check if ray goes through floor plane
        if ((pi/2.0.LE.phi.AND.phi.LE.3.0/2.0*pi).OR.(theta.EQ.0.0)) then   
            hit=0                                    ! 0 indicates that the ray does not hit the surface
        else
            ! randomly selected ray origin
            call RANDOM_NUMBER(R1)
            call RANDOM_NUMBER(R2)
            x = (dble(i)-1.0+R1)*H_node                      
            y = R2*W
            ! determine the intersection point        
            y_i = y+tan(phi)*(H_rec-x)                   
            z_i = (H_rec-x)/(cos(phi)*tan(theta))
            if (y_i<yvR(1)) then
                hit=0
            elseif (y_i>yvR(2)) then
                hit=0
            elseif (z_i<zvR(1)) then
                hit=0
            elseif (z_i>zvR(4)) then
                hit=0
            else     
                p = point(y_i,z_i)
                inside = point_is_inside(p, targetpolygon)                      
                if(inside) then
                    hit=1
                else
                    hit=0
                endif
            endif    
        ! hit counter
        if (hit.EQ.1) hits(i) = hits(i)+1  
        endif
    enddo
enddo

do i = 1,N_nodes
    F_AF(N_nodes+1-i) = DBLE(hits(i))/DBLE(ict)
enddo

end subroutine


subroutine InnerPanel_Floor(N_rays,N_nodes,H_rec,H_lip,R_rec,RecAngle,F_BF)
! view factor from all the nodes of an inner panel to the floor surface
use Points_Module  
use Ray_Casting_Algo
implicit none
! Inputs
! N_nodes   number of panel H_node in the vertical direction
! H_rec     receiver panel height
! R_rec     radius of circle where the common panel edges are located
! RecAngle  section of circle which is covered by panels

! Outputs
! F_BF      view factor array for panel A
integer::N_nodes,N_rays,ict,hit,i
double precision::alpha,RecAngle,H_node,H_rec,H_lip,W,c,R_rec,R1,R2,&
                  x,y,yv(5),zv(5),yvR(4),zvR(4),Ptheta,theta,Pphi,phi,y_i,z_i
double precision,dimension(N_nodes),intent(out)::F_BF
double precision,parameter::pi=3.14159265
type(polygon):: targetpolygon
type(point):: pts(5),p  
logical::inside
integer,dimension(N_nodes)::hits    

! number of rays to generate
!N_rays = 500000

! Geometric calculations
alpha = RecAngle*0.25               ! angle coverage of the receiver circle by each of the 4 panels
H_node = H_rec/dble(N_nodes)        ! height of a panel node
W = 2.d0*R_rec*sin(alpha/2.d0)        ! panel width if panels have equal size
c = 2.d0*R_rec*sin(pi-2.d0*alpha) 	! distance between the vertical aperture edges if the aperature is considered to be at the outer edges of the outer panels

        
! Vertices of the floor polygon
yv = (/-W*sin(pi/2.d0-alpha), 0.d0, W, W*(1.d0+cos(alpha)), W+2.d0*R_rec*sin(alpha)*cos(alpha+acos(R_rec*sin(alpha)/W))/)
zv = (/W*cos(pi/2.d0-alpha), 0.d0, 0.d0, W*sin(alpha), 2.d0*R_rec*sin(alpha)*sin(alpha+acos(R_rec*sin(alpha)/W))/)
! Create the polygon 
pts = (/ point(yv(1),zv(1)), point(yv(2),zv(2)), point(yv(3),zv(3)), point(yv(4),zv(4)), point(yv(5),zv(5))/) 
targetpolygon = create_polygon(pts, (/ 1,2, 2,3, 3,4, 4,5, 5,1 /) )
! Vertices of the bounding box for the polygon
yvR = (/-W*sin(pi/2.d0-alpha), W*(1.d0+cos(alpha)), W*(1.d0+cos(alpha)), -W*sin(pi/2.d0-alpha)/)
zvR = (/0.d0, 0.d0, 2.d0*R_rec*sin(alpha)*sin(alpha+acos(R_rec*sin(alpha)/W)),& 
        2.d0*R_rec*sin(alpha)*sin(alpha+acos(R_rec*sin(alpha)/W))/)

!Initialize random number generation
CALL RANDOM_SEED
!Initialize ray and hit counters
ict = 0
hits = 0
do  
    ! ray counter
    ict = ict+1
    if(ict>N_rays) exit 	
    do i = 1,N_nodes
        ! randomly selected ray direction
        call RANDOM_NUMBER(Ptheta)  ! uniformly distributed random number between 0 and 1
        call RANDOM_NUMBER(Pphi)    ! uniformly distributed random number between 0 and 1        	    
        theta= asin(sqrt(Ptheta))	! determine the polar angle             
        phi=Pphi*2.d0*pi             ! determine the azimuthal angle
        
        ! check if ray goes through floor plane
        if ((pi/2.0.LE.phi.AND.phi.LE.3.0/2.0*pi).OR.(theta.EQ.0.0)) then   
            hit=0                                    ! 0 indicates that the ray does not hit the surface
        else
            ! randomly selected ray origin
            call RANDOM_NUMBER(R1)
            call RANDOM_NUMBER(R2)
            x = (dble(i)-1.0+R1)*H_node                      
            y = R2*W
            ! determine the intersection point        
            y_i = y+tan(phi)*(H_rec-x)                   
            z_i = (H_rec-x)/(cos(phi)*tan(theta))
            if (y_i<yvR(1)) then
                hit=0
            elseif (y_i>yvR(2)) then
                hit=0
            elseif (z_i<zvR(1)) then
                hit=0
            elseif (z_i>zvR(4)) then
                hit=0
            else     
                p = point(y_i,z_i)
                inside = point_is_inside(p, targetpolygon)                      
                if(inside) then
                    hit=1
                else
                    hit=0
                endif
            endif    
        ! hit counter
        if (hit.EQ.1) hits(i) = hits(i)+1  
        endif
    enddo
enddo

do i = 1,N_nodes
    F_BF(N_nodes+1-i) = DBLE(hits(i))/DBLE(ict)
enddo

end subroutine


subroutine Lip_Ceiling(N_rays,N_nodes,H_rec,H_lip,R_rec,RecAngle,F_LCE)
! view factor from the receiver lip surface to the ceiling surface
use Points_Module  
use Ray_Casting_Algo
implicit none
! Inputs
! N_nodes   number of panel H_node in the vertical direction
! H_rec     receiver panel height
! R_rec     radius of circle where the common panel edges are located
! RecAngle  section of circle which is covered by panels

! Outputs
! F_LCE      view factor array for panel A

double precision::alpha,RecAngle,H_node,H_rec,H_lip,W,c,z,R_rec,R1,R2,&
                  x,y,yv(5),zv(5),yvR(4),zvR(4),Ptheta,theta,Pphi,phi,y_i,z_i
double precision,intent(out)::F_LCE                   
integer::N_nodes,N_rays,ict,hit,hits,i
double precision,parameter::pi=3.14159265
type(polygon):: targetpolygon
type(point):: pts(5),p  
logical::inside   

! number of rays to generate
!N_rays = 500000

! Geometric calculations
alpha = RecAngle*0.25              ! angle coverage of the receiver circle by each of the 4 panels
H_node = H_rec/dble(N_nodes)        ! height of a panel node
W = 2.d0*R_rec*sin(alpha/2.d0)      ! panel width if panels have equal size
c = 2.d0*R_rec*sin(pi-2.d0*alpha)   ! distance between the vertical aperture edges if the aperature is considered to be at the outer edges of the outer panels
z = R_rec*cos(pi-2.d0*alpha)        ! distance between the aperature plane and the centerline of the panel circle
        
! Vertices of the floor polygon
yv = (/c, 0.5*c+R_rec*sin(alpha), 0.5*c, W*cos(1.5*alpha), 0.d0/)
zv = (/0.d0, z+R_rec*cos(alpha), z+R_rec, W*sin(1.5*alpha), 0.d0/)
! Create the polygon 
pts = (/ point(yv(1),zv(1)), point(yv(2),zv(2)), point(yv(3),zv(3)), point(yv(4),zv(4)), point(yv(5),zv(5))/) 
targetpolygon = create_polygon(pts, (/ 1,2, 2,3, 3,4, 4,5, 5,1 /) )
! Vertices of the bounding box for the polygon
yvR = (/0.d0, c, c, 0.d0/)
zvR = (/0.d0, 0.d0, z+R_rec, z+R_rec/)

!Initialize random number generation
CALL RANDOM_SEED
!Initialize ray and hit counters
ict = 0
hits = 0
do  
    ! ray counter
    ict = ict+1
    if(ict>N_rays) exit 	
    ! randomly selected ray direction
    call RANDOM_NUMBER(Ptheta)  ! uniformly distributed random number between 0 and 1
    call RANDOM_NUMBER(Pphi)    ! uniformly distributed random number between 0 and 1        	    
    theta= asin(sqrt(Ptheta))	! determine the polar angle             
    phi=Pphi*2.d0*pi             ! determine the azimuthal angle      
    ! check if ray goes through floor plane
    if ((pi/2.0.LE.phi.AND.phi.LE.3.0/2.0*pi).OR.(theta.EQ.0.0)) then   
        hit=0                                    ! 0 indicates that the ray does not hit the surface
    else
        ! randomly selected ray origin
        call RANDOM_NUMBER(R1)
        call RANDOM_NUMBER(R2)
        x = R1*H_lip
        y = R2*c
        ! determine the intersection point        
        y_i = y+tan(phi)*(H_lip-x)                   
        z_i = (H_lip-x)/(cos(phi)*tan(theta))
        if (y_i<yvR(1)) then
            hit=0
        elseif (y_i>yvR(2)) then
            hit=0
        elseif (z_i<zvR(1)) then
            hit=0
        elseif (z_i>zvR(4)) then
            hit=0
        else     
            p = point(y_i,z_i)
            inside = point_is_inside(p, targetpolygon)                      
            if(inside) then
                hit=1
            else
                hit=0
            endif
        endif    
        ! hit counter
        if (hit.EQ.1) hits = hits+1  
    endif   
enddo

F_LCE = DBLE(hits)/DBLE(ict)

end subroutine


subroutine Lip_Floor(N_rays,N_nodes,H_rec,H_lip,R_rec,RecAngle,F_LF)
! view factor from the receiver lip surface to the floor surface
use Points_Module  
use Ray_Casting_Algo
implicit none
! Inputs
! N_nodes   number of panel H_node in the vertical direction
! H_rec     receiver panel height
! R_rec     radius of circle where the common panel edges are located
! RecAngle  section of circle which is covered by panels

! Outputs
! F_LF      view factor array for panel A

double precision::alpha,RecAngle,H_node,H_rec,H_lip,W,c,z,R_rec,R1,R2,&
                  x,y,yv(5),zv(5),yvR(4),zvR(4),Ptheta,theta,Pphi,phi,y_i,z_i
double precision,intent(out)::F_LF                  
integer::N_nodes,N_rays,ict,hit,hits,i
double precision,parameter::pi=3.14159265
type(polygon):: targetpolygon
type(point):: pts(5),p  
logical::inside   

! number of rays to generate
!N_rays = 500000

! Geometric calculations
alpha = RecAngle*0.25              ! angle coverage of the receiver circle by each of the 4 panels
H_node = H_rec/dble(N_nodes)        ! height of a panel node
W = 2.d0*R_rec*sin(alpha/2.d0)      ! panel width if panels have equal size
c = 2.d0*R_rec*sin(pi-2.d0*alpha)   ! distance between the vertical aperture edges if the aperature is considered to be at the outer edges of the outer panels
z = R_rec*cos(pi-2.d0*alpha)        ! distance between the aperature plane and the centerline of the panel circle
        
! Vertices of the floor polygon
yv = (/c, 0.5*c+R_rec*sin(alpha), 0.5*c, W*cos(1.5*alpha), 0.d0/)
zv = (/0.d0, z+R_rec*cos(alpha), z+R_rec, W*sin(1.5*alpha), 0.d0/)
! Create the polygon 
pts = (/ point(yv(1),zv(1)), point(yv(2),zv(2)), point(yv(3),zv(3)), point(yv(4),zv(4)), point(yv(5),zv(5))/) 
targetpolygon = create_polygon(pts, (/ 1,2, 2,3, 3,4, 4,5, 5,1 /) )
! Vertices of the bounding box for the polygon
yvR = (/0.d0, c, c, 0.d0/)
zvR = (/0.d0, 0.d0, z+R_rec, z+R_rec/)

!Initialize random number generation
CALL RANDOM_SEED
!Initialize ray and hit counters
ict = 0
hits = 0
do  
    ! ray counter
    ict = ict+1
    if(ict>N_rays) exit 	
    ! randomly selected ray direction
    call RANDOM_NUMBER(Ptheta)  ! uniformly distributed random number between 0 and 1
    call RANDOM_NUMBER(Pphi)    ! uniformly distributed random number between 0 and 1        	    
    theta= asin(sqrt(Ptheta))	! determine the polar angle             
    phi=Pphi*2.d0*pi             ! determine the azimuthal angle      
    ! check if ray goes through floor plane
    if ((pi/2.0.LE.phi.AND.phi.LE.3.0/2.0*pi).OR.(theta.EQ.0.0)) then   
        hit=0                                    ! 0 indicates that the ray does not hit the surface
    else
        ! randomly selected ray origin
        call RANDOM_NUMBER(R1)
        call RANDOM_NUMBER(R2)
        x = R1*H_lip
        y = R2*c
        ! determine the intersection point        
        y_i = y+tan(phi)*(H_rec-x)                   
        z_i = (H_rec-x)/(cos(phi)*tan(theta))
        if (y_i<yvR(1)) then
            hit=0
        elseif (y_i>yvR(2)) then
            hit=0
        elseif (z_i<zvR(1)) then
            hit=0
        elseif (z_i>zvR(4)) then
            hit=0
        else     
            p = point(y_i,z_i)
            inside = point_is_inside(p, targetpolygon)                      
            if(inside) then
                hit=1
            else
                hit=0
            endif
        endif    
        ! hit counter
        if (hit.EQ.1) hits = hits+1  
    endif   
enddo

F_LF = DBLE(hits)/DBLE(ict)

end subroutine


subroutine Opening_Ceiling(N_rays,N_nodes,H_rec,H_lip,R_rec,RecAngle,F_OCE)
! view factor from the receiver aperture surface to the ceiling surface
use Points_Module  
use Ray_Casting_Algo
implicit none
! Inputs
! N_nodes   number of panel H_node in the vertical direction
! H_rec     receiver panel height
! R_rec     radius of circle where the common panel edges are located
! RecAngle  section of circle which is covered by panels

! Outputs
! F_OCE      view factor array for panel A

double precision::alpha,RecAngle,H_node,H_rec,H_lip,W,c,z,R_rec,R1,R2,&
                  x,y,yv(5),zv(5),yvR(4),zvR(4),Ptheta,theta,Pphi,phi,y_i,z_i
double precision,intent(out)::F_OCE                   
integer::N_nodes,N_rays,ict,hit,hits,i
double precision,parameter::pi=3.14159265
type(polygon):: targetpolygon
type(point):: pts(5),p  
logical::inside   

! number of rays to generate
!N_rays = 500000

! Geometric calculations
alpha = RecAngle*0.25              ! angle coverage of the receiver circle by each of the 4 panels
H_node = H_rec/dble(N_nodes)        ! height of a panel node
W = 2.d0*R_rec*sin(alpha/2.d0)      ! panel width if panels have equal size
c = 2.d0*R_rec*sin(pi-2.d0*alpha)   ! distance between the vertical aperture edges if the aperature is considered to be at the outer edges of the outer panels
z = R_rec*cos(pi-2.d0*alpha)        ! distance between the aperature plane and the centerline of the panel circle
        
! Vertices of the floor polygon
yv = (/c, 0.5*c+R_rec*sin(alpha), 0.5*c, W*cos(1.5*alpha), 0.d0/)
zv = (/0.d0, z+R_rec*cos(alpha), z+R_rec, W*sin(1.5*alpha), 0.d0/)
! Create the polygon 
pts = (/ point(yv(1),zv(1)), point(yv(2),zv(2)), point(yv(3),zv(3)), point(yv(4),zv(4)), point(yv(5),zv(5))/) 
targetpolygon = create_polygon(pts, (/ 1,2, 2,3, 3,4, 4,5, 5,1 /) )
! Vertices of the bounding box for the polygon
yvR = (/0.d0, c, c, 0.d0/)
zvR = (/0.d0, 0.d0, z+R_rec, z+R_rec/)

!Initialize random number generation
CALL RANDOM_SEED
!Initialize ray and hit counters
ict = 0
hits = 0
do  
    ! ray counter
    ict = ict+1
    if(ict>N_rays) exit 	
    ! randomly selected ray direction
    call RANDOM_NUMBER(Ptheta)  ! uniformly distributed random number between 0 and 1
    call RANDOM_NUMBER(Pphi)    ! uniformly distributed random number between 0 and 1        	    
    theta= asin(sqrt(Ptheta))	! determine the polar angle             
    phi=Pphi*2.d0*pi             ! determine the azimuthal angle      
    ! check if ray goes through floor plane
    if ((pi/2.0.LE.phi.AND.phi.LE.3.0/2.0*pi).OR.(theta.EQ.0.0)) then   
        hit=0                                    ! 0 indicates that the ray does not hit the surface
    else
        ! randomly selected ray origin
        call RANDOM_NUMBER(R1)
        call RANDOM_NUMBER(R2)
        x = R1*(H_rec-H_lip)
        y = R2*c
        ! determine the intersection point        
        y_i = y+tan(phi)*(H_rec-x)                   
        z_i = (H_rec-x)/(cos(phi)*tan(theta))
        if (y_i<yvR(1)) then
            hit=0
        elseif (y_i>yvR(2)) then
            hit=0
        elseif (z_i<zvR(1)) then
            hit=0
        elseif (z_i>zvR(4)) then
            hit=0
        else     
            p = point(y_i,z_i)
            inside = point_is_inside(p, targetpolygon)                      
            if(inside) then
                hit=1
            else
                hit=0
            endif
        endif    
        ! hit counter
        if (hit.EQ.1) hits = hits+1  
    endif   
enddo

F_OCE = DBLE(hits)/DBLE(ict)

end subroutine


subroutine Opening_Floor(N_rays,N_nodes,H_rec,H_lip,R_rec,RecAngle,F_OF)
! view factor from the receiver aperture surface to the floor surface
use Points_Module  
use Ray_Casting_Algo
implicit none
! Inputs
! N_nodes   number of panel H_node in the vertical direction
! H_rec     receiver panel height
! R_rec     radius of circle where the common panel edges are located
! RecAngle  section of circle which is covered by panels

! Outputs
! F_OF      view factor array for panel A

double precision::alpha,RecAngle,H_node,H_rec,H_lip,W,c,z,R_rec,R1,R2,&
                  x,y,yv(5),zv(5),yvR(4),zvR(4),Ptheta,theta,Pphi,phi,y_i,z_i
double precision,intent(out)::F_OF                   
integer::N_nodes,N_rays,ict,hit,hits,i
double precision,parameter::pi=3.14159265
type(polygon):: targetpolygon
type(point):: pts(5),p  
logical::inside   

! number of rays to generate
!N_rays = 500000

! Geometric calculations
alpha = RecAngle*0.25              ! angle coverage of the receiver circle by each of the 4 panels
H_node = H_rec/dble(N_nodes)        ! height of a panel node
W = 2.d0*R_rec*sin(alpha/2.d0)      ! panel width if panels have equal size
c = 2.d0*R_rec*sin(pi-2.d0*alpha)   ! distance between the vertical aperture edges if the aperature is considered to be at the outer edges of the outer panels
z = R_rec*cos(pi-2.d0*alpha)        ! distance between the aperature plane and the centerline of the panel circle
        
! Vertices of the floor polygon
yv = (/c, 0.5*c+R_rec*sin(alpha), 0.5*c, W*cos(1.5*alpha), 0.d0/)
zv = (/0.d0, z+R_rec*cos(alpha), z+R_rec, W*sin(1.5*alpha), 0.d0/)
! Create the polygon 
pts = (/ point(yv(1),zv(1)), point(yv(2),zv(2)), point(yv(3),zv(3)), point(yv(4),zv(4)), point(yv(5),zv(5))/) 
targetpolygon = create_polygon(pts, (/ 1,2, 2,3, 3,4, 4,5, 5,1 /) )
! Vertices of the bounding box for the polygon
yvR = (/0.d0, c, c, 0.d0/)
zvR = (/0.d0, 0.d0, z+R_rec, z+R_rec/)

!Initialize random number generation
CALL RANDOM_SEED
!Initialize ray and hit counters
ict = 0
hits = 0
do  
    ! ray counter
    ict = ict+1
    if(ict>N_rays) exit 	
    ! randomly selected ray direction
    call RANDOM_NUMBER(Ptheta)  ! uniformly distributed random number between 0 and 1
    call RANDOM_NUMBER(Pphi)    ! uniformly distributed random number between 0 and 1        	    
    theta= asin(sqrt(Ptheta))	! determine the polar angle             
    phi=Pphi*2.d0*pi             ! determine the azimuthal angle      
    ! check if ray goes through floor plane
    if ((pi/2.0.LE.phi.AND.phi.LE.3.0/2.0*pi).OR.(theta.EQ.0.0)) then   
        hit=0                                    ! 0 indicates that the ray does not hit the surface
    else
        ! randomly selected ray origin
        call RANDOM_NUMBER(R1)
        call RANDOM_NUMBER(R2)
        x = H_lip + R1*(H_rec-H_lip)
        y = R2*c
        ! determine the intersection point        
        y_i = y+tan(phi)*(H_rec-x)                   
        z_i = (H_rec-x)/(cos(phi)*tan(theta))
        if (y_i<yvR(1)) then
            hit=0
        elseif (y_i>yvR(2)) then
            hit=0
        elseif (z_i<zvR(1)) then
            hit=0
        elseif (z_i>zvR(4)) then
            hit=0
        else     
            p = point(y_i,z_i)
            inside = point_is_inside(p, targetpolygon)                      
            if(inside) then
                hit=1
            else
                hit=0
            endif
        endif    
        ! hit counter
        if (hit.EQ.1) hits = hits+1  
    endif   
enddo

F_OF = DBLE(hits)/DBLE(ict)

end subroutine

