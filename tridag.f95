module tridag

use precision

contains

    subroutine tridagr( nx, a, b, c, r, u)
    !*****************************************************************************
    !
    !   a = upper diagonal
    !   b = principal diagonal
    !   c = down diagonal
    !   A.u=r 
    !   u= soluction vector
    !
    implicit none
    integer, intent(in) :: nx
    real(dp), dimension(nx), intent(in)  :: a, b, c, r
    real(dp), dimension(nx), intent(out) :: u
    real(dp), dimension(nx) :: gama

    !   Local
    integer  :: j  
    real(dp) :: bet
    
    !   nx = size(r)

    bet  = b(1)
    u(1) = r(1) / bet

    do j= 2, nx
        gama(j) = c(j-1) / bet
        bet     = b(j) - a(j) * gama(j)
        u(j)    = ( r(j) - a(j) * u(j-1) ) / bet
    end do

    do j= nx-1, 1, -1
        u(j) = u(j) - gama(j+1) * u(j+1)
    end do

    end subroutine tridagr
    
end module tridag
