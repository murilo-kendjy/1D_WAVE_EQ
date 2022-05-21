!
!   This module sets the coeficients of the equation:
!   eg: -For the explicit method-
! 

module coeficients

use precision
use system_conditions
use boundary_conditions

contains

    subroutine coef_exp(Nx, dt, dx, T, mu, beta, cp, cu, cd, ce, lambda, j, ut_t, n)
    !*****************************************************************************
    !
    !   Set the coeficients for the explicit method
    !
    !   for  j=1
    !       u(i,j+1)=cp*u(i,j)
    !               +cd*u(i-1,j)
    !               +cu*u(i+1,j)
    !               + lambda
    !   for j>1
    !       u(i,j+1)=cp*u(i,j)
    !               +cd*u(i-1,j)
    !               +cu*u(i+1,j)
    !               -ce*u(i,j-1)
    !               + lambda
    !
    implicit none
    integer :: j
    integer :: n
    integer, intent(in):: Nx
    real(dp), intent(in) :: dt, dx, T
    real(dp), dimension(Nx), intent(in):: mu, beta, ut_t
    real(dp), dimension(Nx), intent(out) :: cp, cu, cd, ce, lambda

    !   Local
    real(dp) :: F
    real(dp), dimension(Nx) :: v, alpha, gamma, c1, c2

    F= ext_F(n)

    v(:)= sqrt(T/mu)
    alpha(:)= v*(dt/dx)
    gamma(:)= beta/(2*mu)
    c1(:)=1/(1+gamma*dt)
    c2(:)=1-gamma*dt

    if (j == 1) then
        cp(:)= ( c1/(1+c1*c2) ) * 2*(1-alpha**2)   !   u(i,j)
        cu(:)= ( c1/(1+c1*c2) ) *      alpha**2    !   u(i+1,j)
        cd(:)= ( c1/(1+c1*c2) ) *      alpha**2    !   u(i-1,j)
        ce(:)= 0
        lambda(:)= ( c1/(1+c1*c2) ) * (c2*2*dt*ut_t + dt**2/mu*F)
    else
        cp(:)= c1*2*(1-alpha**2)   !   u(i,j)
        cu(:)= c1*     alpha**2    !   u(i-1,j)
        cd(:)= c1*     alpha**2    !   u(i+1,j)
        ce(:)= c1*c2               !   u(i,j-1)
        lambda(:)= ( c1/(1+c1*c2) ) * (dt**2/mu*F)
    end if

    end subroutine coef_exp
    

    subroutine coef_imp(Nx, dt, dx, T, cp, cu, cd, &
        cp2, cu2, lambda, mu, beta, j, ut_t, n)
    !*****************************************************************************
    !
    !   Set the coeficients for the implicit method
    !
    implicit none
    integer :: j
    integer :: n
    integer :: Nx
    real(dp) :: dx, dt, T
    real(dp), dimension(Nx) :: mu, beta, ut_t
    real(dp), dimension(Nx), intent(out) :: cp, cu, cd  ! Left side u(i,j+1)
    real(dp), dimension(Nx),intent(out) :: cp2, cu2, lambda    ! Right side u(i,j-1) u(i,j)

    !   Local
    real(dp) :: F
    real(dp), dimension(Nx) :: v, alpha, gamma, c1

    F= ext_F(n)
    
    v(:)= sqrt(T/mu)
    alpha(:)= v*(dt/dx)
    gamma(:)= beta/(2*mu)

    c1(:)= 2*gamma*dt
    
    if (j == 1) then
        ! Left side
        cp(:)= (1 + alpha**2 - c1/2)    ! u(i,j+1)
        cu(:)= -alpha**2/2              ! u(i-1,j+1)
        cd(:)= -alpha**2/2              ! u(i+1,j+1)
        ! Right side
        cp2(:)= (2-c1)/2                ! u(i,j)
        lambda(:)= (1-c1)*(dt*ut_t) + dt**2*F/(2*mu)   ! Cte
        ! For i=1 and Nx we have different conditions
        cp(1)= (1 + alpha(1)**2/2 - c1(1)/2)
        cp(Nx)= (1 + alpha(Nx)**2/2 - c1(Nx)/2)
    else
        ! Left side
        cp(:)= (1 + 2*alpha**2)     ! u(i,j+1)
        cu(:)= -alpha**2            ! u(i-1,j+1)
        cd(:)= -alpha**2            ! u(i+1,j+1)
        ! Right side
        cp2(:)= (2-c1)              ! u(i,j)
        cu2(:)= (-1+c1)             ! u(i,j-1)
        lambda(:)= dt**2*F/mu       ! Cte
        ! For i=1 and Nx we have different conditions
        cp(1)= (1 + alpha(1)**2)
        cp(Nx)= (1 + alpha(Nx)**2)
    end if

    cd(1)= 0
    cu(Nx)= 0
    
    end subroutine coef_imp
    
end module coeficients
