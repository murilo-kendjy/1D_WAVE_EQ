!
! This module sets the system conditions
! such as rope/wave density, dissipative force etc
! 

module system_conditions

use precision
use secundary
use r_inp   

contains

    function ext_F(n) result(F)
    !*****************************************************************************
    !
    ! Set the external forces F(u,t).
    !
    !   (1:Nx-Nx2-NxL) -> The interval on the first rope
    !   (Nx1+1:Nx-Nx2) -> The interval on the second rope (or joint)
    !   (Nx1+NxL+1:Nx) -> Interval on the third rope (or second in case joint = 0)
    !
    implicit none
    integer, intent(in) :: n
    ! OUT
    real(dp) :: F

    select case (n)
        case(10)
            !   Constant and proportional to gravity 
            !   Depends on the wave amplitude for the sine case
            !   Assuming A=0.5
            F= -9.8/20
        case default
            !   No external forces
            !
            F=0
    end select

    end function ext_F


    function beta_var(Nx1, Nx2, NxL, Nx, n) result(beta)
    !*****************************************************************************
    !
    ! Set the magnitude of the dissipative force
    !
    !   (1:Nx-Nx2-NxL) -> The interval on the first rope
    !   (Nx1+1:Nx-Nx2) -> The interval on the second rope (or joint)
    !   (Nx1+NxL+1:Nx) -> Interval on the third rope (or second in case joint = 0)
    !
    implicit none
    integer, intent(in) :: n
    integer, intent(in) :: Nx1, Nx2, NxL, Nx
    ! OUT
    real(dp),dimension(Nx) :: beta

    ! Local
    real(dp), dimension(Nx1) :: beta1    ! Interval on the first rope
    real(dp), dimension(NxL) :: beta2    ! Interval on the second rope (or joint)
    real(dp), dimension(Nx2) :: beta3    ! Interval on the third rope (or second in case joint = 0)

    select case (n)
        case(4)
            !   Custom
            !
            beta1= 0.5
            beta2= 0.5
            beta3= 0.5
        case(10)
            !   Custom
            !
            beta1= 0
            beta2= 0
            beta3= 0
        case default
            !   Null
            !
            beta1= 0
            beta2= 0
            beta3= 0
    end select

    beta(1:Nx-Nx2-NxL)= beta1
    beta(Nx1+1:Nx-Nx2)= beta2
    beta(Nx1+NxL+1:Nx)= beta3

    if (any(beta < 0)) then
        write ( *, '(a)' ) ' WARNING '
        write ( *, '(a)' ) 'The proportional constant beta < 0'
    end if
    
    end function beta_var


    function mu_var(Nx1, Nx2, NxL, Nx, x_vec, n) result(mu)
    !*****************************************************************************
    !
    ! Set the density on the ropes
    !
    implicit none
    real(dp), parameter :: pi= 3.141592653589793
    integer, intent(in) :: n
    integer, intent(in) :: Nx1, Nx2, NxL, Nx
    real(dp),dimension(Nx), intent(in) :: x_vec
    ! OUT
    real(dp),dimension(Nx) :: mu

    ! Local
    real(dp), dimension(Nx1) :: mu1    ! Interval on the first rope
    real(dp), dimension(NxL) :: mu2    ! Interval on the second rope (or joint)
    real(dp), dimension(Nx2) :: mu3    ! Interval on the third rope (or second in case joint = 0)
    real(dp), dimension(Nx) :: x

    select case (n)
        case(1)
            !   mu1 = mu2 < mu3
            !
            mu1= 1
            mu2= 1
            mu3= 5
        case(2)
            !   mu1 = mu2 > mu3
            !
            mu1= 5
            mu2= 5
            mu3= 1
        case(3)
            !   Variable on the joint
            !
            call r8vec_linspace(NxL, real(-0.5,kind=dp), real(0.5,kind=dp), x(Nx1+1:Nx-Nx2))
            mu1= 1
            mu2= (-4*x(Nx1+1:Nx-Nx2)+2.5)*(4*x(Nx1+1:Nx-Nx2)+2.5)
            mu3= 1
        case(4)
            !   All ropes different density
            !
            mu1= 1
            mu2= 1+5*x_vec(Nx1+1:Nx-Nx2)**2
            mu3= 1
        case(10)
            !   Custom
            !
            ! call r8vec_linspace(Nx, real(0,kind=dp), real(2*pi,kind=dp), x)
            ! mu1= 1 + 2*sin(x(1:Nx-Nx2-NxL))**2
            ! mu2= 1 + 2*cos(x(Nx1+1:Nx-Nx2))**2
            ! mu3= 1 + 2*tan(x(Nx1+NxL+1:Nx))**2
            mu1= 1
            mu2= 1
            mu3= 1
        case default
            !   Constant and same all ropes
            !
            mu1= 1
            mu2= 1
            mu3= 1
    end select

    mu(1:Nx-Nx2-NxL)= mu1
    mu(Nx1+1:Nx-Nx2)= mu2
    mu(Nx1+NxL+1:Nx)= mu3

    if (any(mu <= 0.2)) then
        write ( *, '(a)' ) ' WARNING '
        write ( *, '(a)' ) 'The ropes density mu is too low'
        stop
    end if

    end function mu_var

end module system_conditions
