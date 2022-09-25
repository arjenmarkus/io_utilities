! progressbar.f90 --
!     Straightforward module to display ASCII progress bars in different styles
!
!     Notes:
!     - You should set a few things before the progress bar is displayed, even though
!       most parameters have a default.
!     - The module provides no method to determine the width of the console - that
!       may be possible with OS-specific means.
!     - For the Intel Fortran compiler it is necessary to use the flush statement.
!
module progress_bar
    use iso_c_binding, only: c_carriage_return
    use iso_fortran_env, only: output_unit

    implicit none

    private

    public :: progress_bar_finish, progress_bar_setup, progress_bar_next, progress_bar_value
    !
    ! Define the styles
    !
    integer, parameter, public    :: progress_keep       = 0
    integer, parameter, public    :: progress_continuous = 1
    integer, parameter, public    :: progress_cyclic     = 2
    integer, parameter, public    :: progress_delimited  = 3
    integer, parameter, public    :: progress_rotating   = 4
    integer, parameter, public    :: progress_no_cursor  = 5

    !
    ! Define the options for showing the value
    !
    integer, parameter, public    :: progress_none          = 0
    integer, parameter, public    :: progress_value         = 1
    integer, parameter, public    :: progress_value_maximum = 2

    !
    ! Define options
    !
    character(len=1)              :: leading_char  = '.'
    character(len=1)              :: trailing_char = '*'
    character(len=:), allocatable :: prefix_string
    character(len=:), allocatable :: suffix_string
    character(len=:), allocatable :: form_value
    character(len=:), allocatable :: form_both
    integer                       :: show_value    = progress_none

    !
    ! Parameters
    !
    real                          :: maximum_value  = 100.0
    integer                       :: progress_style = -1
    integer                       :: progress_width = 50
    integer                       :: position       =  0
    integer                       :: direction      =  1

    character(len=4)              :: cyclic_chars = '-\|/'

contains

! progress_bar_reset --
!     Reset all options to the default values
!
! Arguments:
!     None
!
subroutine progress_bar_reset()

    position       =  0
    direction      =  1
    progress_style = -1
    progress_width = 50
    maximum_value  = 100.0

    leading_char   = '.'
    trailing_char  = '*'
    prefix_string  = ''
    suffix_string  = ''
    form_value     = '(f6.2)'
    form_both      = '(f6.2,''/'',f6.2)'
    show_value     = progress_none

end subroutine progress_bar_reset

! progress_bar_finish --
!     Convenience routine to move the cursor to the next line
!
! Arguments:
!     None
!
subroutine progress_bar_finish()

    position       =  0
    direction      =  1

    write( output_unit, '(a)' ) ''

end subroutine progress_bar_finish

! progress_bar_setup --
!     Set the style and other parameters for the progress bar
!
! Arguments:
!     style              What style for the progress bar, note: progrees_keep allows you to change settings and keep others
!     max_value          Scale for values passed to progress_bar_value (calculate the length)
!     leading            Character to use before the cursor
!     trailing           Character to use to show the position of the cursor
!     prefix             Text to be shown before the progress bar
!     suffix             Text to be shown after the progress bar
!     show               Option to show the actual values (none, value and maximum value)
!     width              Width of the area for the cursor
!     format_value       Format to be used for the value (without the maximum)
!     format_both        Format to be used for the value and the maximum
!
subroutine progress_bar_setup( style, max_value, leading, trailing, prefix, suffix, show, width, format_value, format_both )
    integer, intent(in)                    :: style
    real, intent(in), optional             :: max_value
    character(len=*), intent(in), optional :: leading
    character(len=*), intent(in), optional :: trailing
    character(len=*), intent(in), optional :: prefix
    character(len=*), intent(in), optional :: suffix
    integer, intent(in), optional          :: show
    integer, intent(in), optional          :: width
    character(len=*), intent(in), optional :: format_value
    character(len=*), intent(in), optional :: format_both

    !
    ! Check style
    !
    if ( all ( style /= &
         [progress_keep, progress_continuous, progress_cyclic, progress_delimited, progress_rotating, progress_no_cursor] ) ) then
        progress_style = progress_continuous
    else
        if ( style /= progress_keep ) then
            call progress_bar_reset
            progress_style = style
        endif
    endif

    !
    ! Store the various values
    !
    if ( present(max_value) ) then
        maximum_value = max_value
    endif

    if ( present(leading) ) then
        leading_char = leading
    endif

    if ( present(trailing) ) then
        trailing_char = trailing
    endif

    if ( present(prefix) ) then
        prefix_string = prefix
    else
        prefix_string = ''
    endif

    if ( present(suffix) ) then
        suffix_string = suffix
    else
        suffix_string = ''
    endif

    if ( present(show) ) then
        show_value = show
    endif

    if ( present(width) ) then
        progress_width = width
    endif

    if ( present(format_value) ) then
        form_value = format_value
    endif

    if ( present(format_both) ) then
        form_both = format_both
    endif

    position       =  0
    direction      =  1

end subroutine progress_bar_setup

! progress_bar_next --
!     Simply write the next character
!
! Arguments:
!     None
!
! Note:
!     Only applicable if the progress bar does not show a value.
!     If it should show a value, simply return
!
subroutine progress_bar_next()

    if ( show_value /= progress_none ) then
        return
    endif

    select case ( progress_style )
        case( progress_continuous )
            write( output_unit, '(a)', advance = 'no' ) trailing_char
            flush( output_unit )

        case( progress_cyclic )
            if ( position == 0 ) then
                direction = 1
            endif
            if ( position == progress_width ) then
                direction = -1
                position  = position + direction
            endif

            write( output_unit, '(6a)', advance = 'no' ) &
                prefix_string, repeat( leading_char, max( 0, position ) ), trailing_char, &
                               repeat( leading_char, max( 0, progress_width - position - 1 ) ), &
                suffix_string, c_carriage_return
            flush( output_unit )

            position = position + direction

        case( progress_delimited )
            if ( position < progress_width ) then
                write( output_unit, '(6a)', advance = 'no' ) &
                    prefix_string, repeat( leading_char, max( 0, position ) ), trailing_char, &
                                   repeat( ' ', max( 0, progress_width - position - 1 ) ), &
                    suffix_string, c_carriage_return
                flush( output_unit )
                position = position + 1
            endif

        case( progress_rotating )
            position = 1 + mod( position, 4 )

            write( output_unit, '(4a)', advance = 'no' ) &
                prefix_string, cyclic_chars(position:position), suffix_string, c_carriage_return
            flush( output_unit )

        case default
            !
            ! Simply ignore this possibility
            !
    end select
end subroutine progress_bar_next

! progress_bar_value --
!     Show the progress bar based on a value
!
! Arguments:
!     value           The value to be used for the position of the cursor
!
! Note:
!     Only applicable if the progress bar does not show a value.
!     If it should not show a value, simply return
!
subroutine progress_bar_value( value )
    real, intent(in)  :: value

    character(len=80) :: value_string

    select case ( show_value )
        case ( progress_none )
            return

        case ( progress_value )
            write( value_string, form_value ) value

        case ( progress_value_maximum )
            write( value_string, form_both ) value, maximum_value

        case default
            !
            ! Simply ignore this possibility
            !
    end select

    select case ( progress_style )
        case( progress_delimited )
            position = progress_width * max( 0.0, min( value, maximum_value ) ) / maximum_value
            write( output_unit, '(7a)', advance = 'no' ) &
                prefix_string, repeat( leading_char, max( 0, position ) ), trailing_char, &
                               repeat( ' ',          max( 0, progress_width - position - 1 ) ), &
                suffix_string, trim(value_string), c_carriage_return
            flush( output_unit )

        case( progress_no_cursor )
                write( output_unit, '(5a)', advance = 'no' ) &
                    prefix_string, trim(value_string), c_carriage_return

        case default
            !
            ! Simply ignore this possibility
            !
    end select
end subroutine progress_bar_value

end module progress_bar
