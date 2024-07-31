! positional_formatting.f90 --
!     Provide a simple facility for "positional formatting"
!
!     Preliminary version
!
!     Note:
!     This works for Intel Fortran oneAPI (ifx), but not for gfortran
!     (13.2.0)
!     By changing the automatic allocation to an explicit allocation,
!     I got gfortran to work as well.
!
!     Note 2:
!     It does not work correctly yet, but I am getting there.
!
!     TODO:
!     - Extend with logical variables - besides T and F also words "true" and "false"
!     - Function returning a string
!     - OO interface
!     - Customisation of the default format
!     - Inlined formats
!     - Better error handling - extra argument?
!     - Documentation
!     - Proper test program
!     - fpm support
!
module positional_formatting
    implicit none

    private :: upv

    interface format_string
        module procedure format_string_argn
    end interface

    type upv
        class(*), allocatable :: value
    end type upv
contains

! format_string_argn --
!     Provide an easy interface to the underlying routine
!
! Arguments:
!     string                 Output string
!     format                 Template for the output
!     arg1                   Argument of any scalar type
!     ...
!
! Note:
!    Using array(1)%value = arg1 did not work with gfortran
!
subroutine format_string_argn( string, format, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9 )
    character(len=:), allocatable, intent(out) :: string
    character(len=*), intent(in)               :: format
    class(*), intent(in), optional             :: arg1, arg2, arg3, arg4, arg5, arg6, arg7,arg8, arg9

    type(upv), dimension(9)                    :: array
    integer                                    :: arg_count

    if ( present(arg1) ) then
        arg_count = 1
        allocate( array(1)%value, source = arg1 )
    endif

    if ( present(arg2) ) then
        arg_count = 2
        allocate( array(2)%value, source = arg2 )
    endif

    if ( present(arg3) ) then
        arg_count = 3
        allocate( array(3)%value, source = arg3 )
    endif

    if ( present(arg4) ) then
        arg_count = 4
        allocate( array(4)%value, source = arg4 )
    endif

    if ( present(arg5) ) then
        arg_count = 5
        allocate( array(5)%value, source = arg5 )
    endif

    if ( present(arg6) ) then
        arg_count = 6
        allocate( array(6)%value, source = arg6 )
    endif

    if ( present(arg7) ) then
        arg_count = 7
        allocate( array(7)%value, source = arg7 )
    endif

    if ( present(arg8) ) then
        arg_count = 8
        allocate( array(8)%value, source = arg8 )
    endif

    if ( present(arg9) ) then
        arg_count = 9
        allocate( array(9)%value, source = arg9 )
    endif

    call format_string_array( string, format, array(1:arg_count) )
end subroutine format_string_argn

! format_string_array --
!     Actual implementation of the positional formatting
!
! Arguments:
!     string                 Output string
!     format                 Template for the output
!     array                  Array of class(*) arguments
!
subroutine format_string_array( string, format, array )
    character(len=:), allocatable, intent(out) :: string
    character(len=*), intent(in)               :: format
    type(upv), dimension(:), intent(in)        :: array

    character(len=40)                          :: value_string
    integer                                    :: idx, k, length, start

    length = len(format)
    start  = 1

    string = ''

    do
        k = index( format(start:length-1), '{' )

        if ( k > 0 ) then

            k = k + start
            string = string // format(start:k-2)


            if ( format(k:k) == '{' ) then
                string = string // '{'
            else
                read( format(k:k), * ) idx

                if ( idx > size(array) ) then
                    ! Problem found!
                    string = 'ERROR! Positional argument out of range'
                    exit
                endif

                !
                ! Format this argument
                !
                select type ( value => array(idx)%value )
                    type is (integer)
                        write(value_string,'(g0)') value
                        string = string // trim(value_string)

                    type is (real)
                        write(value_string,'(g0)') value
                        string = string // trim(value_string)

                    type is (real(kind=kind(1.0d0)))
                        write(value_string,'(g0)') value
                        string = string // trim(value_string)

                    type is (logical)
                        write(value_string,'(g0)') value
                        string = string // trim(value_string)

                    type is (character(len=*))
                        string = string // value

                    class default
                        string = 'ERROR! Unsupported type'
                end select
            endif

            k = k + 1  ! Skip the rest of the positional argument - should check for "}"
        else
            string = string // trim(format(start:))
            exit
        endif

        start = k + 1

    enddo

end subroutine format_string_array

end module positional_formatting
