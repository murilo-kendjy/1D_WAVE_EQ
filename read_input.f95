!
! This module read the inputs from input.inp
!

module r_inp

use precision

implicit none
real(dp), save :: T
real(dp), save :: xr1
real(dp), save :: xr2
real(dp), save :: L
real(dp), save :: t1
real(dp), save :: t2
integer, save :: Nx
integer, save :: Nt
integer, save :: mtd
integer, save :: n
integer, save :: nxi
integer, save :: nxf
integer, save :: nxt
integer, save :: nxtt
         
contains

    ! 
    ! Read data
    !  
    subroutine r_variables(arq)
    !*****************************************************************************

    implicit none
    character(len=*), intent(in) :: arq

    open(1, file=arq, form='formatted', status='old')
    read(1,*) T
    read(1,*) xr1
    read(1,*) xr2
    read(1,*) L
    read(1,*) Nx
    read(1,*) t1
    read(1,*) t2
    read(1,*) Nt
    read(1,*) mtd
    read(1,*) n
    read(1,*) nxi
    read(1,*) nxf
    read(1,*) nxt
    read(1,*) nxtt

    close(unit=1, status='keep')
    
    ! 
    ! Adimensionalização
    ! 
    
    end subroutine r_variables
 
 end module r_inp
