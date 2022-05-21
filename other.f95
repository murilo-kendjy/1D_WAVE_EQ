! This code takes FD1D_WAVE as base
! from https://people.sc.fsu.edu/~jburkardt/f_src/fd1d_wave/fd1d_wave.html
! 
! This high modified version evaluates the wave function taking 
! dissipative forces along a rope 
!

module secundary
    
use precision

contains

subroutine get_unit ( iunit )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is a value between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is a value between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 October 2008
!    04 June 2021
!
!  Author:
!
!    John Burkardt
!    Murilo Kendjy
!
!  Parameters:
!
!    Output, integer(ip2) IUNIT, the free unit number.
!
    implicit none
    integer(ip2) :: i
    integer(ip2) :: ios
    integer(ip2) :: iunit
    logical :: lopen

    iunit = 0

    do i = 1, 99

    if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then
        inquire ( unit = i, opened = lopen, iostat = ios )
        if ( ios == 0 ) then
            if ( .not. lopen ) then
                iunit = i
                return
            end if
        end if
    end if

    end do

end subroutine get_unit


subroutine r8mat_write( output_filename, m, n, table)

!*****************************************************************************80
!
!! R8MAT_WRITE writes an R8MAT file.
!
!  Discussion:
!
!    An R8MAT is an array of R8 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2009
!    04 June 2021
!
!  Author:
!
!    John Burkardt
!    Murilo Kendjy
!
!  Parameters:
!
!    Input, character ( len = * ) OUTPUT_FILENAME, the output file name.
!
!    Input, integer(ip2) M, the spatial dimension.
!
!    Input, integer(ip2) N, the number of points.
!
!    Input, real(dp) TABLE(M,N), the data.
!
    implicit none
    integer :: m, n, j
    integer(ip2) :: output_status
    integer(ip2) :: output_unit
    real(dp), dimension(m,n) :: table
    character(len = *) :: output_filename
    character(len = 60) :: string
    
    !  Open the file
    call get_unit ( output_unit )

    open ( unit = output_unit, file = output_filename, &
    status = 'replace', iostat = output_status )

    if ( output_status /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8MAT_WRITE - Fatal error!'
        write ( *, '(a,i8)' ) '  Could not open the output file "' // &
            trim ( output_filename ) // '" on unit ', output_unit
        output_unit = -1
        stop
    end if

    !  Create a format string
    !
    !  For less precision in the output file, try:
    !
    !                                            '(', m, 'g', 14, '.', 6, ')'

    if ( 0 < m .and. 0 < n ) then
        write ( string, '(a1,i8,a1,i8,a1,i8,a1)' ) '(', m, 'f', 12, '.', 8, ')'
        !  Write the data
        do j = 1, n
            write ( output_unit, string ) table(1:m,j)
        end do

    end if

    !  Close the file
    close ( unit = output_unit )

end subroutine r8mat_write


subroutine r8vec_linspace ( n, a_first, a_last, a )

!*****************************************************************************80
!
!! R8VEC_LINSPACE creates a vector of linearly spaced values.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 March 2011
!    04 June 2021
!
!  Author:
!
!    John Burkardt
!    Murilo Kendjy
!
!  Parameters:
!
!    Input, integer(ip2) N, the number of entries in the vector.
!
!    Input, real(dp) A_FIRST, A_LAST, the first and last entries.
!
!    Output, real(dp) A(N), a vector of linearly spaced data.
!
    implicit none
    integer :: n
    integer :: i
    real(dp), dimension(n) :: a
    real(dp) :: a_first
    real(dp) :: a_last
    
    if ( n == 1 ) then
        a(1) = ( a_first + a_last ) / 2
    else
        do i = 1, n
            a(i) = ( real ( n - i,     kind = dp ) * a_first &
                    + real (     i - 1, kind = dp ) * a_last ) &
                    / real ( n     - 1, kind = dp )
        end do
    end if
    
end subroutine r8vec_linspace

end module secundary
