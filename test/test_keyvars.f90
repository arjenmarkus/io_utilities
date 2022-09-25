! test_keyvars.f90 --
!     Straightforward test program for the keyvars module
!
program test_keyvars
    use keyvars

    implicit none

    integer :: x
    real    :: y
    character(len=20) :: string

    logical                         :: exists
    integer                         :: lun

    !
    ! Check if the input file exists, otherwise write an example
    !
    write( *, '(a)' ) 'Simple test program for keyvars module', ''
    inquire( file = "test_keyvars.inp", exist = exists )
    if ( .not. exists ) then
        open( newunit = lun, file = "test_keyvars.inp" )
        write( lun, '(a)' ) &
"# Example of an input file for keyvars", &
"# Lines starting with # are comments, anything after a # is regarded as comment as well", &
"#", &
"int = 2", &
"real = 1.234", &
"char = Would this work?"
        close( lun )

        write( *, '(a)' ) 'Input file test_keyvars.inp, as it did not exist'
    else
        write( *, '(a)' ) 'Using existing input file test_keyvars.inp'
    endif

    x = -1
    y = -1.0

    call get_values( 'test_keyvars.inp', [keyvar("int",  x, "Integer value"), &
                                          keyvar("real", y, "Real value"), &
                                          keyvar("char", string, "Some text")] )

    write(*,*) 'x = ', x
    write(*,*) 'y = ', y
    write(*,*) 'string = ', string
end program test_keyvars
