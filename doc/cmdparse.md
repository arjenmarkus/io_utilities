# cmdparse - module for parsing a minimalistic command language

The `cmdparse` module provides a straightforward mechanism for reading commands from an input file
or the keyboard:

 * Valid commands with their arguments are defined beforehand, so that the parser can check the validity.
 * The type of the expected input argument is patr of the definition.
 * The arguments are stored when a command has bee nread and can be retrieved independently.


## ROUTINES AND DATA TYPES

The command parser is an object of type `cmdparser` that is initialised via the subroutine (method) `commands`:

```fortran
    type(cmdparser) :: parser

    call parser%commands( cmds )
```
The argument `cmds` is an array of strings. Each string defines a new command via the name of the command
and zero, one or more arguments:

```fortran
    character(len=40), dimension(4) :: cmds = &
        [ 'report-file (a)filename   ',   &
          'repeat (i)times           ',   &
          'allocate (i)chunks (i)size',   &
          'set-random (i)times       '    ]
```

The type of the arguments can be:

 * "(a)" - a string of arbitrary length (well, up to 80 characters in the current implementation)
 * "(i)" - an integer (of default kind)
 * "(f)" - a double-precision real value (note: no single-precision at this moment)

The arguments can be retrieved via the method `get_arg`:

```fortran
    call parser%get_arg( idx, varname )
```
where `idx` is the index of the argument and `varname` the variable that will be given the value of
the argument.

Other methods are:

 * Set the input file for retrieving the next commands (the file is opened to a new unit number):

```fortran
    call parser%set_input( filename )
```

 * Set the report file for simple logging (the report file is assumed to be opened to this unit number):

```fortran
    call parser%set_report( lurep )
```

 * Check if there is a new command in the input:

```fortran

    have_next = parser%has_next()
```

 * Retrieve the next command from the input (file or keyboard):

```fortran
    integer            :: idx
    character(len=...) :: cmd

    call parser%next( idx, cmd )
```
   The argument `idx` is the index of the command in the template and `cmd` is the name of the command.
   Either can be used to take action.

 * Switch between input file and keyboard:

```fortran
    logical            :: keyboard
    character(len=...) :: prompt

    call parser%set_keyboard( keyboard, prompt )
```
   The `prompt` argument is optional and defaults to ">". If `keyboard` is set to true, then the next
   command is read from the keyboard.

## TYPICAL USAGE

To illustrate the use, here is a trivial but complete, program:

```fortran
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
```

