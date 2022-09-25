! test_cmdparse.f90 --
!     Simple test program for the command parser
!
!     Usage:
!     1. The commands are contained in an input file - line by line
!     2. Pattern:
!        do while ( parser%has_next() )
!            call parser%next( ... )
!            ... handle the command
!        enddo
!     3. The template commands consist of a command name (single word) and zero or arguments
!     4. The arguments are encoded as:
!        (a)keyword - a string argument (identified as "keyword")
!        (i)keyword - an integer argument
!        (f)keyword - a real argument
!
program test_cmdparse
    use cmdparse

    type(cmdparser) :: parser

    character(len=40), dimension(4) :: cmds = &
        [ 'report-file (a)filename   ',   &
          'repeat (i)times           ',   &
          'allocate (i)chunks (i)size',   &
          'set-random (i)times       '    ]

    character(len=40)               :: cmd, string
    integer                         :: idx
    integer                         :: i, intvalue

    logical                         :: exists
    integer                         :: lun

    !
    ! Check if the input file exists, otherwise write an example
    !
    write( *, '(a)' ) 'Simple test program for cmpdarse module', ''
    inquire( file = "test_cmdparse.inp", exist = exists )
    if ( .not. exists ) then
        open( newunit = lun, file = "test_cmdparse.inp" )
        write( lun, '(a)' ) &
"# test.inp --", &
"#     Simple test input file", &
"", &
"report-file 'test.out'   # We need a file to report to ...", &
"repeat 10", &
"#repeat A 10", &
"allocate 10 10000", &
"#allocate", &
"", &
"#unknown-command"
        close( lun )

        write( *, '(a)' ) 'Input file test_cmdparse.inp, as it did not exist'
    else
        write( *, '(a)' ) 'Using existing input file test_cmdparse.inp'
    endif

    !
    ! Set up the parser and process the commands
    !
    call parser%commands( cmds )
    call parser%set_input( "test_cmdparse.inp" )

    do while ( parser%has_next() )
        call parser%next( idx, cmd )
        write( *, * ) trim(cmd), ':'
        do i = 1,2
            call parser%get_arg( i, string )
            call parser%get_arg( i, intvalue )
            write( *, * ) idx, string, intvalue
        enddo
    enddo

    call parser%use_keyboard( .true., 'what do you want to do? ' )
    if ( parser%has_next() ) then
        call parser%next( idx, cmd )
        write( *, * ) trim(cmd), ':'
    endif
end program test_cmdparse
