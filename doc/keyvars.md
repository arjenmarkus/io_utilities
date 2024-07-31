# keyvars - module for reading and writing INI-file like files

The `keyvars` module was inspired by the observations that auxiliary programs that you create
for doing your job, such as converting one type of file into another, because that is more conventient
for the task at hand, often require a small amount of input, say four or five parameters. It is easy
enough to use a few read statements to read them from a file, but that often means fairly little
checking, because it is "just" an auxiliary program, to be used by yourself only and documentation
is more often than not only contained in the source code (or more honestly: the source code is the
documentation).

To solve the problem of documenting the input and checking that all is there, at least partly,
you can use this module:

 * You specify which variables are to be read from the input file.
 * Each input variable is associated with a short meaningful name and a short description.
 * Reading the data is handled by a single subroutine call, as is the reporting of the values that
   were read and providing support.
 * The input file format is either that of a simple INI file or of an INI file without sections.
   A "#" indicates the start of a comment. Here is a simple illustration:
```text
# Example of an input file for keyvars
# Text after a # is comment
#
int = 2
real = 1.234
char = A string of several words
```
where space around the equal sign are not significant.

Here is a simple example of how to use the module:

```fortran
program demo
    use keyvars

    implicit none

    integer :: x
    real    :: y
    character(len=20) :: string

    x = -1
    y = -1.0

    call get_values( 'keyvars.inp', [keyvar("int",  x, "Integer value"), &
                                     keyvar("real", y, "Real value"), &
                                     keyvar("char", string, "Some text")] )

    write(*,*) 'x = ', x
    write(*,*) 'y = ', y
    write(*,*) 'string = ', string

end program demo
```

The subroutine `get_values` first looks at the command-line arguments:
 * Arguments `-?, /?, -h, -help, --help` are all recognised as a request for displaying some
   help text. It will print the keywords and the description and then stop.
 * Arguments `-i, -input, --input` must be followed by the name of the input file the user
   wants to use. It then reads that particular file instead of the file name that was passed.
 * Arguments `-o, -output, --output` indicate that the user wants a template of the input.
   The argument must be followed by the name of that file. The routine will simply write the
   template and stop. The template includes the descriptions as comments.

   The array of variables (!) is formed via the `keyvar` function. Basically, this function
   returns a `keyvar_data` value that contains all there is to know about the variable.
   For its proper functioning it stores a pointer to the variable.

*Note:* The algorithm was implemented without any consideration of duplicate keys either
in the array or the input file. What happens if a key is not unique therefore depends on the
exact implementation.

## ROUTINES
There are two public routines:

```fortran
    call get_values( filename, args )
```
This subroutine reads the given input file and stores the values it found in the variables
defined in the array `args`. More details provided above.

Arguments:
 * `character(len=*) :: filename` - Name of the file to be read, unless overwritten via the `-input` option.
 * `type(keyvar_data), dimension(:) :: args` - Array specifying the variables as well as the associated keywords and descriptive texts. Construct
   or fill it using the `keyvar` function.

```fortran
    arg = keyvar( keyword, var, description )
```
This function returns a value of type `keyvar_data`. It fills the components
of this derived type with a pointer to the variable `var`, so that it can be
set automatically. The variable itself is one of the basic types: integer,
single and double precision real, logical or a character string of any length.

Arguments:
 * `character(len=*) :: keyword` - The keyword by which the variable is to be found in the input.
 * `integer/real/double precision/character(len=*)/logical, target :: var` - The variable to be assigned a value from the input file.
 * `character(len=*) :: description`` - The description of the variable, serves as documentation in the template.

*Notes:*
 * The actual argument to `var`should have the `target` attribute, as it will be pointed to.
 * The compiler will not check the presence of the `target` attribute and the program may work if it is not present, but it would violate the standard.

```fortran
    arg = keyvar( section, keyword, var, description )
```
Alternative form of the `keyvar` function. In this case you can specify the section the variable
should come from. Section names appear in the input file as `[section]`.

Arguments:
 * `character(len=*) :: section` - The section the keyword belongs to.
 * `character(len=*) :: keyword` - The keyword by which the variable is to be found in the input.
 * `integer/real/double precision/character(len=*)/logical :: var` - The variable to be assigned a value from the input file.
 * `character(len=*) :: description`` - The description of the variable, serves as documentation in the template.

*Notes:*
 * The module uses deferred-length characters and this requires a recent version of the Fortran
   compiler. For `gfortran` you need at least 4.9.
 * The implementation does not care about duplicate keys, as remarked above. This basically means
   that what happens if you use duplicate keys, depends on the details of the implementation and
   is therefore not documented here.
