! 
! This module evaluates the EDP values in time
! 

module EDP

use precision
use r_inp
use system_conditions
use boundary_conditions
use coeficients

contains

    subroutine wave_alpha(T, Nx, mu, dt, dx)
    !*****************************************************************************
    !
    ! Check the stability of the system in case of using explicit method
    !

    implicit none
    integer, intent(in) :: Nx
    real(dp), intent(in) :: dx, dt, T
    real(dp), dimension(Nx), intent(in) :: mu
    ! Local
    real(dp), dimension(Nx) :: v1, alpha

    v1= sqrt(T/mu)

    alpha= v1*(dt/dx)

    if (any(abs(alpha) > 1)) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The stability condition ALPHA < 1 fails.'
      write ( *, '(a)' ) '  Results may be inaccurate.'
    else
      write ( *, * ) ' '
      write ( *, '(a,g14.6)' ) '  Stability condition ALPHA = C * DT / DX, OK'
      write ( *, * ) ' '
    end if

    end subroutine wave_alpha

    subroutine wave_step_exp(Nx, Nt, j, cp, cu, cd, ce, lambda, u, nxi, nxf)
    !*****************************************************************************
    !
    ! Computes the wave equation for the explicit method
    !
    implicit none
    integer :: j
    integer :: nxi, nxf
    integer, intent(in) :: Nx, Nt
    real(dp), dimension(Nx) :: cp, cu, cd, ce, lambda
    real(dp), dimension(Nx,Nt), intent(out) :: u ! General soluction vector u(i,j)

    if (j == 1) then 
      u(2:Nx-1,j+1) = cp(2:Nx-1)*u(2:Nx-1,j)    &
                    + cu(2:Nx-1)*u(3:Nx,j)      &
                    + cd(2:Nx-1)*u(1:Nx-2,j)    &
                    + lambda(2:Nx-1)
    else
      u(2:Nx-1,j+1) = cp(2:Nx-1)*u(2:Nx-1,j)    &
                    + cu(2:Nx-1)*u(3:Nx,j)      &
                    + cd(2:Nx-1)*u(1:Nx-2,j)    &
                    - ce(2:Nx-1)*u(2:Nx-1,j-1)  &  
                    + lambda(2:Nx-1)
    end if

    call cond_u_x1(Nx, u(:,j+1), nxi)
    call cond_u_x2(Nx, u(:,j+1), nxf)

    end subroutine wave_step_exp


    subroutine wave_step_imp(Nx, Nt, j, cp2, cu2, lambda, u, u2)
      !*****************************************************************************
      !
      ! Computes the wave equation for the implicit method
      !
      implicit none
      integer :: j
      integer, intent(in) :: Nx, Nt
      real(dp), dimension(Nx) :: cp2, cu2, lambda
      real(dp), dimension(Nx,Nt) :: u  ! General soluction vector u(i,j+1)
      real(dp), dimension(Nx), intent(out) :: u2  ! Soluction vector for the right side 
  
      if (j == 1) then 
        u2 =  cp2*u(:,j) + lambda
      else
        u2 =  cp2*u(:,j)    &
            + cu2*u(:,j-1)  &
            + lambda
      end if
    
      end subroutine wave_step_imp

end module EDP
