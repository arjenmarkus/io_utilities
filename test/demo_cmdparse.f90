! demo_cmdparse.f90 --
!     Demo program from the documentation
!
program demo_cmdparse

    use cmdparse

    type(cmdparser)        :: parser
    integer                :: idx
    character(len=10)      :: cmd

    character(len=10)      :: svalue
    integer                :: ivalue
    real(kind=kind(1.0d0)) :: rvalue1, rvalue2

    character(len=40), dimension(3) :: cmds = &
        [ 'string (a)value              ',   &
          'int-value (i)value           ',   &
          'two-reals (f)value1 (f)value2']

    call parser%commands( cmds )
    call parser%set_input( 'example.inp' )

    do while ( parser%has_next() )
       call parser%next( idx, cmd )

       select case ( idx ) ! Or indeed cmd ...
           case (1)
               call parser%get_arg( 1, svalue )
               write(*,*) 'String value: ', svalue

           case (2)
               call parser%get_arg( 1, ivalue )
               write(*,*) 'Integer value: ', ivalue

           case (3)
               call parser%get_arg( 1, rvalue1 )
               call parser%get_arg( 2, rvalue2 )
               write(*,*) 'Real values: ', rvalue1, rvalue2
        end select
    enddo

end program demo_cmdparse
