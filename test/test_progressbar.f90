! test_progressbar.f90 --
!     Test the progress bar module
!
!     Notes:
!     - SLEEP() is an extension that is supported by gfortran and Intel Fortran
!     - Using Intel Fortran you need to flush the output
!     - It does not matter whether you use output_unt or error_unit
!
!     TODO:
!     - Cyclic: At the end of the bar, the asterisk disappears. Now it seems to stay there too long
!     - Style: [....*   ]
!     - Style: [....*...], neither cyclic
!       or should that be part of handling a sufffix?
!
program test_progressbar
    use progress_bar

    integer :: i, style

    write( *, '(a)' ) 'Test program for the progressbar module', ''
    write( *, '(a)' ) 'Chose the style:'
    write( *, '(a)' ) '1. Continuous bar (sequence of # characters'
    write( *, '(a)' ) '2. Rotating cursor'
    write( *, '(a)' ) '3. Limited area, being filled up'
    write( *, '(a)' ) '4. Cycling cursor within a limited area'
    write( *, '(a)' ) '5. Limited area with surrounding text'
    write( *, '(a)' ) '6. Increasing percentage'
    write( *, '(a)' ) '7. Increasing value with maximum'

    read( *, * ) style

    if ( style < 1 .or. style > 7 ) then
        style = 1
        write( *, '(a)' ) 'Out of range ... using default style 1'
    endif

    write( *, '(a)' ) 'Note: progressing one step per second'

    select case( style )
        case( 1 )
            call progress_bar_setup( progress_continuous )

        case( 2 )
            call progress_bar_setup( progress_rotating, prefix = "Working ... " )

        case( 3 )
            call progress_bar_setup( progress_delimited, prefix = "Working ... ", width = 10 )

        case( 4 )
            call progress_bar_setup( progress_cyclic, prefix = "Working ... ", width = 10 )

        case( 5 )
            call progress_bar_setup( progress_delimited, prefix = "Progress [", suffix = ']', width = 10 )

        case( 6 )
            call progress_bar_setup( progress_no_cursor, prefix = "Progress: ", show = progress_value_maximum, &
                     format_both = '(f5.1, "/", f5.0)' )
        case( 7 )
            call progress_bar_setup( progress_delimited, prefix = "Progress: ", show = progress_value_maximum, &
                     max_value = 150.0 )
    end select

    if ( style < 6 ) then
        !
        ! Styles between 1 to 5 do not use any input for the next step
        !
        do i = 1,40
            call sleep(1)
            call progress_bar_next
        enddo

    else
        !
        ! Styles 6 and 7 require a value to be diplayed
        !
        do i = 1,40
            call sleep(1)
            call progress_bar_value( 2.3*i )
        enddo
    endif
end program test_progressbar
