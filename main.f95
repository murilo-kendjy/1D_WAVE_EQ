
program main

use precision
use secundary
use r_inp
use system_conditions
use boundary_conditions
use coeficients
use tridag
use EDP


!*****************************************************************************
!
! Main program for the 1D wave equation
!
implicit none
integer :: i, j
integer :: Nx1, Nx2, NxL  ! number of dicretized points on each rope
real(dp) :: x1, x2  ! initial and final position
real(dp) :: dx, dt  ! space and time intervals based on number os points
real(dp), dimension(:), allocatable :: x_vec, t_vec ! space and time discretized vectors
real(dp), dimension(:), allocatable :: u_t ! initial condition u_t (deformation)
real(dp), dimension(:), allocatable :: ut_t  ! initial condition ut_t (velocity)
real(dp), dimension(:), allocatable :: u2 ! soluction vector for u(j+1)
real(dp), dimension(:), allocatable :: mu, beta ! system conditions 
real(dp), dimension(:), allocatable :: cp, cu, cd, ce, lambda ! system coeficients
real(dp), dimension(:), allocatable :: cp2, cu2, cd2 ! system coeficients
real(dp), dimension(:,:), allocatable :: u  ! matrix wave solution
character( len = 255 ) data_filename

write( *, * ) ' '
write( *, *) '-***-Program Start-***-'
write( *, *) ' '

print *, 'Enter data file name (no extesion)'
read *, data_filename 

data_filename  = trim(adjustl(data_filename)) // '.inp'

call r_variables(data_filename)

!
! Check inputs
!
if (xr1 < 0 .or. xr2 < 0 .or. L < 0 .or. xr1 == 0 .and. xr2 == 0 &
    .and. L == 0) then
  write( *, *) ' '
  write( *, *) '-Length input problem-'
  write( *, *) '-Abort-'
  stop
else if (t1 > t2) then
  write( *, *) ' '
  write( *, *) '-Time input problem-'
  write( *, *) '-Abort-'
  stop
else
  write( *, *) ' '
  write( *, *) '-Inputs ok-'
  write( *, *) ' '
end if

!
! Stability is not a problem
! but we want a quadratic matrix
!
if (mtd == 2) Nt= Nx

allocate(x_vec(Nx),t_vec(Nt))
allocate(u_t(Nx), ut_t(Nx))
allocate(u(Nx,Nt), u2(Nx))
allocate(mu(Nx), beta(Nx), lambda(Nx))
allocate(cp(Nx), cu(Nx), cd(Nx), ce(Nx))
allocate(cp2(Nx), cu2(Nx), cd2(Nx))

! 
! Define the start/end point
! 
x1=0
x2=xr1+xr2+L

!  
! Evaluates the number of points on the ropes
! 
Nx1= int(xr1*Nx/(x2))
Nx2= int(xr2*Nx/(x2))
NxL= Nx-Nx1-Nx2

!
! Creates linearized vectors
!
call r8vec_linspace(Nx, x1, x2, x_vec)
call r8vec_linspace(Nt, t1, t2, t_vec)

!
! Iteration interval
!
dx= x_vec(2) - x_vec(1)
dt= t_vec(2) - t_vec(1)

!
! Defines the system conditions
!
mu= mu_var(Nx1, Nx2, NxL, Nx, x_vec, n)
beta= beta_var(Nx1, Nx2, NxL, Nx, n)

!
!  Initial conditions
!
call cond_u_t(Nx, x_vec, u(:,1), nxt)
call cond_ut_t(Nx, ut_t, nxtt)

select case(mtd)
  case (1)
    !
    ! Stability test
    !
    call wave_alpha(T, Nx, mu, dt, dx)
    !
    ! Calculates de wave equation from t=1 to t=Nt-1
    ! using explicit method
    !
    do j=1, Nt-1
        call coef_exp(Nx, dt, dx, T, mu, beta, cp, cu, cd, ce, lambda, j, ut_t, n)
        call wave_step_exp(Nx, Nt, j, cp, cu, cd, ce, lambda, u, nxi, nxf)
    end do
  case default
    !
    ! Calculates de wave equation from t=1 to t=Nt-1
    ! using implicit method
    !
    do j=1, Nt-1
      call coef_imp(Nx, dt, dx, T, cp, cu, cd, &
        cp2, cu2, lambda, mu, beta, j, ut_t, n)
      call wave_step_imp(Nx, Nt, j, cp2, cu2, lambda, u, u2)
      ! write(*,*) u2
      call tridagr(Nx, cd, cp, cu, u2, u(:,j+1))
      ! write(*,*) u(:,j+1)
      call cond_u_x1(Nx, u(:,j+1), nxi)
      call cond_u_x2(Nx, u(:,j+1), nxf)
    end do
  end select

!
!  Write the solution matrix to a file
!

write(data_filename, '( "Data_[", I0, "-", I0, "-", I0, "-", I0, "-", I0, "-", I0, "]_", &
& "|", I0, "|", I0, "|", I0, "|", I0, "|", I0, "|" )' ) &
   mtd, n, nxi, nxf, nxt, nxtt, Nx, Nt, Nx1, Nx2, NxL

data_filename= trim(data_filename) // ".dat"

call r8mat_write(data_filename, Nx, Nt, u)

write ( *, '(a)' ) ' '
write ( *, '(a)' ) ' System/Boundary parameters written as [mtd-n-nxi-nxf-nxt-nxtt] '
write ( *, '(a)' ) ' Number of points parameters written as \Nx\Nt\Nx1\Nx2\NxL\ '
write ( *, '(a)' ) ' Matrix soluction u(x,t) data written to "' // trim(data_filename) // '".'

! 
!  Write x and t vectors
! 
write(data_filename, '( "Data_Vecs_[", I0, "-", I0,"]" )' )   &
      Nx, Nt

data_filename= trim(data_filename) // ".dat"

open (unit=11, file = data_filename, form='formatted', status='unknown')

  
    if (Nx <= Nt) then
      do i=1, Nx
        write(11,'(2F12.8)') x_vec(i), t_vec(i)
      end do
      do j=Nx+1, Nt
        write(11,'(A,F12.8)') repeat(' ', 12), t_vec(j)
      end do
    else
      do j=1, Nt
        write(11,'(2F12.8)') x_vec(j), t_vec(j)
      end do
      do i=Nt+1, Nx
        write(11,'(A,F12.8)') repeat(' ', 12), t_vec(i)
      end do
    end if

write ( *, '(a)' ) ' '
write ( *, '(a)' ) ' Number of points parameters written as (Nx|Nt) '
write ( *, '(a)' ) '  Data Vectors X,T data written to "' // trim(data_filename) // '".'

close(unit = 11, status = 'keep')  

write( *, *) ' '
write( *, *) '-***-Program End-***-'
write( *, *) ' '

end program main
