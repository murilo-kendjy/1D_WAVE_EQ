! 
! This module sets the boundary and initial conditions
! 

module boundary_conditions

use precision
use r_inp
use secundary

contains

    subroutine cond_u_x1(Nx, u, n)
    !*****************************************************************************
    !
    ! evaluates U at the boundary X1.
    !
    implicit none
    integer :: n
    integer, intent(in) :: Nx   ! Número de pontos
    real(dp), dimension(Nx), intent(out) :: u

    select case(n)
        case(1)
            !   Loose left side
            !
            u(1)=u(2)
        case default
            !   Fixed left size u(0,t)
            !
            u(1)=0
    end select

    end subroutine cond_u_x1


    subroutine cond_u_x2(Nx, u, n)
    !*****************************************************************************
    !
    ! evaluates U at the boundary X2.
    !
    implicit none
    integer :: n
    integer, intent(in) :: Nx
    real(dp), dimension(Nx), intent(out) :: u


    select case(n)
        case(1)
            !   Loose right side
            ! 
            u(Nx)=u(Nx-1)
        case default
            !   Fixed right side u(L,t)   
            !
            u(Nx)= 0
    end select

    end subroutine cond_u_x2
    

    subroutine cond_u_t(Nx, x_vec, u, n)
    !*****************************************************************************
    !
    ! evaluates U at the initial time T1.
    !
    implicit none
    integer :: n
    real(dp), parameter :: pi= 3.141592653589793
    integer, intent(in) :: Nx
    real(dp), dimension(Nx), intent(in) :: x_vec
    real(dp), dimension(Nx), intent(out) :: u

    select case(n)
        case(1)
            !   Senoidal
            !
            u(1:Nx)= 0.25*sin(2*pi*x_vec)
        case(2)
            !   Central triangular wave
            !
            u(1:Nx/2)= 0.5*x_vec(1:Nx/2)/x_vec(Nx)
            u(Nx/2:Nx)= 0.5*(1-x_vec(Nx/2:Nx)/x_vec(Nx))
        case(10)
            !   Custom
            !
            u= 0 
        case default
            !   No deformation
            !
            u= 0   
    end select

    end subroutine cond_u_t


    subroutine cond_ut_t(Nx, ut_t, n)
    !*****************************************************************************
    !
    ! evaluates dU/dT at the initial time T1.
    !
    implicit none
    integer :: n
    integer, intent(in) :: Nx
    real(dp), parameter :: pi= 3.141592653589793
    real(dp), dimension(Nx), intent(out) :: ut_t

    ! Local
    integer :: p
    real(dp), dimension(Nx) :: x
    

    select case(n)
        case(1)
            !   No initial velocity
            !
            ut_t= 0
        case(2)
            !   Senoidal (pulse) wave for 1/2 of te rope
            !
            p= int(Nx/2)    ! Make sure the interval is integer
            call r8vec_linspace(p, real(0,kind=dp), pi, x(:p))
            ut_t(:p)= 0.25*sin(x(:p))
        case(3)
            !   Senoidal (pulse) wave for 1/4 of te rope
            !
            p= int(Nx/4)    ! Make sure the interval is integer
            call r8vec_linspace(p, real(0,kind=dp), pi, x(:p))
            ut_t(:p)= 0.25*sin(x(:p))
        case(10)
            !   Custom
            !
            p= int(Nx/5)    ! Make sure the interval is integer
            call r8vec_linspace(p, real(0,kind=dp), pi, x(:p))
            ut_t(:p)= 0.25*sin(x(:p))
        case default
            !   Senoidal (pulse) wave for 1/8 of te rope
            !
            p= int(Nx/8)    ! Make sure the interval is integer
            call r8vec_linspace(p, real(0,kind=dp), pi, x(:p))
            ut_t(:p)= 0.25*sin(x(:p))
    end select

    end subroutine cond_ut_t

end module boundary_conditions
