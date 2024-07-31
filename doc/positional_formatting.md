# positional_formatting - module for formatting strings based on Python-like format strings

The purpose of the module **positional_formatting** is to construct a string with the values of variables
filled in a more flexible way than is possible with the classical format string or statement, that is,
you can indicate the value to fill in ("interpolate") by means of an index, instead of the strict
ordering.

Here is a simple example of how it can be used:

```fortran
    use positional_formatting

    implicit none

    character(len=:), allocatable :: output
    character(len=80)             :: format

    format = '{1}, the name is {1}'
    call format_string( output, format, 'Fortran' ); write(*,*) '>>', output, '<<'
    format = '{3} = {2} / {1}'
    call format_string( output, format, 7, 42, 6 ); write(*,*) '>>', output, '<<'
```

This would produce the output:

```
>>Fortran, the name is Fortran<<
>>6 = 42 / 7<<
```

Arguments to the `format_string` subroutine are generic (can be of any -- basic -- type) and they are filled in in the
output based on the index, using an edit description that fits the actual type.

The module can be used for instance to localise output, where the ordering of values to be filled in may not be the
same from one natural language to the next but also for easily constructing log messages: there is no need to
explicitly create a format string, for instance. (This type of usage would be made easier with a version that
writes directly to a file or screen, see the note.)


**Note:** In its present form it is merely a prototype. More flexibility is probably required, if it is to be used in actual
programs.

## ROUTINES

The module currently provides the following routine:

```fortran
subroutine format_string( string, format, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9 )
    character(len=:), allocatable, intent(out) :: string
    character(len=*), intent(in)               :: format
    class(*), intent(in), optional             :: arg1, arg2, arg3, arg4, arg5, arg6, arg7,arg8, arg9
end subroutine format_string
```

The `string` argument contains the formatted string on return.

The `format` argument contains the format string to be used. It may contain indices of the form `{1}` to indicate
which argument to use. If the format string refers to an argument that was *not* included in the call, an error
message is used as output. This is also true for arguments that are not one of the basic types (supporting
derived types is one possible extension).

All other arguments can be of any (basic) type, but you should not use the dummy argument names as keywords:

```fortran
call format_string( string, format, arg9 = z )
```

will cause the routine to assume there are indeed nine actual arguments, whereas arguments 1 to 8 are not present.
The logic will fail in this case.

