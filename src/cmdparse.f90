! cmdparse.f90 --
!     Simple command parser
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
module cmdparse
    use iso_fortran_env
    implicit none

    private
    public :: cmdparser

    integer, parameter :: maxargs = 10

    type :: cmdparser
        private
        character(len=80), dimension(:), allocatable :: template ! Template for valid commands
        character(len=80), dimension(maxargs)        :: char_arg
        integer, dimension(maxargs)                  :: int_arg
        real(kind=kind(1.0d0)), dimension(maxargs)   :: real_arg
        integer                                      :: number_args
        integer                                      :: cmd_index_cmd
        character(len=80)                            :: cmd_name
        character(len=80)                            :: line
        character(len=:), allocatable                :: prompt
        logical                                      :: keyboard  = .false.
        integer                                      :: lu_input  = 0
        integer                                      :: lu_report = output_unit
    contains
        procedure :: commands     => commands_parser
        procedure :: use_keyboard => use_keyboard_input
        procedure :: set_input    => set_input_file
        procedure :: set_output   => set_report_file
        procedure :: has_next     => has_next_parser
        procedure :: next         => next_parser
        generic   :: get_arg      => get_char, get_int, get_real
        procedure :: get_char     => get_char_arg
        procedure :: get_int      => get_int_arg
        procedure :: get_real     => get_real_arg
    end type

contains

! commands_parser --
!     Set up the parser: set the templates for the commands
!
! Arguments:
!     cmdp                Command parser
!     template            Array of strings defining the valid commands
!
subroutine commands_parser( cmdp, template )
    class(cmdparser), intent(inout)            :: cmdp
    character(len=*), dimension(:), intent(in) :: template

    cmdp%template = template
end subroutine commands_parser

! use_keyboard_input --
!     Use the keyboard and a specified prompt (can be toggled)
!
! Arguments:
!     cmdp                Command parser
!     keyboard            Use the keyboard instead of a file?
!     prompt              Prompt to use
!
subroutine use_keyboard_input( cmdp, keyboard, prompt )
    class(cmdparser), intent(inout)        :: cmdp
    logical, intent(in)                    :: keyboard
    character(len=*), intent(in), optional :: prompt

    cmdp%keyboard = keyboard
    if ( present(prompt) ) then
        cmdp%prompt   = prompt
    else
        if ( .not. allocated(cmdp%prompt) ) then
            cmdp%prompt   = '>'
        endif
    endif
end subroutine use_keyboard_input

!     Set the input file (will be openend)
!
! Arguments:
!     cmdp                Command parser
!     filename            Name of the file to be read
!
subroutine set_input_file( cmdp, filename )
    class(cmdparser), intent(inout) :: cmdp
    character(len=*), intent(in)   :: filename

    integer                        :: ierr

    open( newunit = cmdp%lu_input, file = filename, status = 'old', iostat = ierr )

    if ( ierr /= 0 ) then
        write( cmdp%lu_report, '(2a)' ) 'Input file does not exist or could not be opened: ', trim(filename)
        write( cmdp%lu_report, '(2a)' ) 'Program stopped'
        error stop
    endif
end subroutine set_input_file

! set_report_file --
!     Set the LU-number to be used for reporting
!
! Arguments:
!     cmdp                Command parser
!     lu_report           LU-number to be used
!
subroutine set_report_file( cmdp, lu_report )
    class(cmdparser), intent(inout) :: cmdp
    integer, intent(in)            :: lu_report

    cmdp%lu_report = lu_report
end subroutine set_report_file

! has_next_parser --
!     Return whether there is a command remaining
!
! Arguments:
!     cmdp                Command parser
!
logical function has_next_parser( cmdp )
    class(cmdparser), intent(inout) :: cmdp

    integer                        :: k, ierr

    if ( cmdp%keyboard ) then
        write( OUTPUT_UNIT, '(a,1x)', advance = 'no') trim(cmdp%prompt)
    else
        if ( cmdp%lu_input == 0 ) then
            write( cmdp%lu_report, '(a)' ) 'No input file defined - program stopped'
            error stop
        endif
    endif

    if ( .not. allocated( cmdp%template ) ) then
        write( cmdp%lu_report, '(a)' ) 'No command templates defined - program stopped'
        error stop
    endif

    has_next_parser = .false.

    do
        if ( cmdp%keyboard ) then
            read( INPUT_UNIT,    '(a)', iostat = ierr ) cmdp%line
        else
            read( cmdp%lu_input, '(a)', iostat = ierr ) cmdp%line
        endif

        if ( ierr /= 0 ) then
            has_next_parser = .false.
            exit
        endif

        k = index( cmdp%line, '#' )
        if ( k > 0 ) then
            if ( cmdp%line(1:k-1) == ' ' ) then
                cycle
            else
                cmdp%line = cmdp%line(1:k-1)
            endif
        endif

        if ( cmdp%line /= ' ' ) then
            has_next_parser = .true.
            exit
        endif
    enddo
end function has_next_parser

! next_parser --
!     Return the information on the next command
!
! Arguments:
!     cmdp                Command parser
!     idx                 Index of the command in the template list
!     command             Command name (either is useable to identify the command)
!
! Note:
!     If the command is not recognised or invalid, the index is set to -1 and
!     the command name to "?"
!
subroutine next_parser( cmdp, idx, command )
    class(cmdparser), intent(inout)   :: cmdp
    integer, intent(out)              :: idx
    character(len=*), intent(out)     :: command

    character(len=10)                 :: arg_type
    character(len=len(cmdp%cmd_name)) :: cmd
    character(len=len(cmdp%char_arg)) :: arg, tmpl
    integer                           :: i, iarg, ierr, k

    idx     = -1
    command = '?'

    cmdp%char_arg(:)  = ' '
    cmdp%int_arg(:)   = -huge(i)
    cmdp%real_arg(:)  = -huge(1.0d0)

    read( cmdp%line, * ) cmd

    do i = 1,size(cmdp%template)
        k = index( cmdp%template(i), trim(cmd) )
        if ( k == 1 ) then
            idx     = i
            command = cmd
            exit
        endif
    enddo

    if ( idx == -1 ) then
        write( cmdp%lu_report, '(2a)' ) 'Error: Command not recognised: ', trim(cmd)
        write( cmdp%lu_report, '(2a)' ) '       Complete command line:  ', trim(cmdp%line)
        return
    endif

    !
    ! Now identify the arguments - recognise number and type one by one
    !
    do iarg = 1,maxargs
        read( cmdp%template(idx), *, iostat = ierr ) cmd, (tmpl, i = 1,iarg )

        !
        ! No more arguments?
        !
        if ( ierr /= 0 ) then
            exit
        endif

        cmdp%number_args = iarg

        !
        ! Get the corresponding argument from the input
        !
        read( cmdp%line, *, iostat = ierr ) cmd, (arg, i = 1,iarg )

        if ( ierr /= 0 ) then
            write( cmdp%lu_report, '(2a)' ) 'Error: Too few arguments: ', trim(cmdp%line)
            write( cmdp%lu_report, '(2a)' ) '       Required:          ', trim(display(cmdp%template(idx)))
            idx     = -1
            command = '?'
            exit
        endif

        !
        ! Identify the type
        !
        ierr = 0
        select case ( tmpl(1:3) )
            case ( '(a)', '(c)' )
                cmdp%char_arg(iarg) = arg
            case ( '(i)' )
                arg_type = 'integer'
                read( arg, *, iostat = ierr ) cmdp%int_arg(iarg)
            case ( '(f)' )
                arg_type = 'real'
                read( arg, *, iostat = ierr ) cmdp%real_arg(iarg)
            case default
                write( cmdp%lu_report, '(4a)' ) 'Error: Unknown type of argument: ', trim(tmpl(1:3)), ' - ',tmpl(4:)
        end select
        if ( ierr /= 0 ) then
            write( cmdp%lu_report, '(2a)' ) 'Error: Argument of the wrong type: ', trim(arg)
            write( cmdp%lu_report, '(2a)' ) '       Required:                   ', trim(arg_type)
            idx     = -1
        endif
    enddo

end subroutine next_parser

function display( string )
    character(len=*), intent(in) :: string
    character(len=len(string))   :: display

    integer                      :: k

    display = string

    do
        k = index( display, '(' )
        if ( k /= 0 ) then
            display = display(1:k-1) // display(k+3:)
        else
            exit
        endif
    enddo
end function display

! get_char_arg --
!     Return the nth character argument
!
! Arguments:
!     cmdp                Command parser
!     n                   Index of the argument
!     string              String value of the argument
!
subroutine get_char_arg( cmdp, n, string )
    class(cmdparser), intent(inout) :: cmdp
    integer, intent(in)             :: n
    character(len=*), intent(out)   :: string

    if ( n < 1 .or. n > maxargs ) then
        string = '??'
    else
        string = cmdp%char_arg(n)
    endif
end subroutine get_char_arg

! get_int_arg --
!     Return the nth integer argument
!
! Arguments:
!     cmdp                Command parser
!     n                   Index of the argument
!     intvalue            Integer value of the argument
!
subroutine get_int_arg( cmdp, n, intvalue )
    class(cmdparser), intent(inout) :: cmdp
    integer, intent(in)             :: n
    integer, intent(out)            :: intvalue

    if ( n < 1 .or. n > maxargs ) then
        intvalue = -huge(intvalue)
    else
        intvalue = cmdp%int_arg(n)
    endif
end subroutine get_int_arg

! get_real_arg --
!     Return the nth real argument
!
! Arguments:
!     cmdp                Command parser
!     n                   Index of the argument
!     realvalue           Real value of the argument
!
subroutine get_real_arg( cmdp, n, realvalue )
    class(cmdparser), intent(inout)    :: cmdp
    integer, intent(in)                :: n
    real(kind=kind(1.d0)), intent(out) :: realvalue

    if ( n < 1 .or. n > maxargs ) then
        realvalue = -huge(realvalue)
    else
        realvalue = cmdp%real_arg(n)
    endif
end subroutine get_real_arg

end module cmdparse
