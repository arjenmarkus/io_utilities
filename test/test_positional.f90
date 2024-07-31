! test_positional.f90 --
!     Simple test program for positional formatting
!
program test_positional
    use positional_formatting

    implicit none

    character(len=:), allocatable :: output
    character(len=80)             :: format

    format = 'No substitution'
    call format_string( output, format, '1' ); write(*,*) '>>', output, '<<'

    format = '{1} substitution - string'
    call format_string( output, format, 'AA' ); write(*,*) '>>', output, '<<'

    format = '{1} substitution - integer'
    call format_string( output, format, 23 ); write(*,*) '>>', output, '<<'

    format = '{1} substitution - real'
    call format_string( output, format, 42.0 ); write(*,*) '>>', output, '<<'

    format = '{1} substitution - double precision real'
    call format_string( output, format, 42.0d0 ); write(*,*) '>>', output, '<<'

    format = '{1} substitution - logical'
    call format_string( output, format, .true. ); write(*,*) '>>', output, '<<'

    format = '{1} substitution - complex'
    call format_string( output, format, (1.0,2.0) ); write(*,*) '>>', output, '<<'

    format = 'Two substitutions - {1} and again {1}'
    call format_string( output, format, 'AA' ); write(*,*) '>>', output, '<<'

    format = 'Two substitutions - {2} and another one, {1}'
    call format_string( output, format, 'AA', 3 ); write(*,*) '>>', output, '<<'

    format = 'Two substitutions - {2} and another one, {1}, one argument missing'
    call format_string( output, format, 3 ); write(*,*) '>>', output, '<<'

end program test_positional
