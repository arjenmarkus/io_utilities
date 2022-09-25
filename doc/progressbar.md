# progress_bar - module for displaying ASCII-based progress bars

The purpose of the module **progress_bar** is to display a progress bar in the console, so that
the user can see the progress of a long calculation or operation. Various styles are supported that
are selected via a set-up routine.

Here is a simple example of how it can be used:

```fortran
    use progress_bar

    integer :: i

    call progress_bar_setup( progress_rotating, prefix = "Working ... " )

    do i = 1,40
        call sleep(1) ! Sleep causes delay in displaying the next step
        call progress_bar_next
    enddo
```

The style `progress_rotating` presents a rotating cursor (a succession of '-', '/', '|', '\'). Via the
optional arguments in the routine `progress_bar_setup` you can specify the precise layout of the progress bar,
such as including text before or after the progress bar.

## ROUTINES

The module provides the following routines:

```fortran
subroutine progress_bar_setup( style, max_value, leading, trailing, prefix, suffix, show, width, format_value, format_both )
    integer, intent(in)                    :: style         ! What style for the progress bar
    real, intent(in), optional             :: max_value     ! Scale for values passed to progress_bar_value (calculate the length)
    character(len=*), intent(in), optional :: leading       ! Character to use before the cursor
    character(len=*), intent(in), optional :: trailing      ! Character to use to show the position of the cursor
    character(len=*), intent(in), optional :: prefix        ! Text to be shown before the progress bar
    character(len=*), intent(in), optional :: suffix        ! Text to be shown after the progress bar
    integer, intent(in), optional          :: show          ! Option to show the actual values (none, value and maximum value)
    integer, intent(in), optional          :: width         ! Width of the area for the cursor
    character(len=*), intent(in), optional :: format_value  ! Format to be used for the value (without the maximum)
    character(len=*), intent(in), optional :: format_both   ! Format to be used for the value and the maximum
end subroutine progress_bar_setup
```

The available styles are:

 * progress_continuous - the cursor is simply appended to the previous ones, no additional text is shown.
 * progress_cyclic - the cursor runs up and down a limited range: `[...*......]`
 * progress_delimited - the cursor runs from left to right a limited range: `[...*      ]`
 * progress_rotating - the cursor is a rotating line character
 * progress_no_cursor - the progress bar consists of the value and possibly the maximum value: ` 12.0/100.0`

The option `show` takes the following values:

 * progress_none - do not show the value next to the progress bar
 * progress_value - show the value next to the progress bar, using the `format_value` format string
 * progress_value_maximum - show the value as well as the maximum value next to the progress bar, using the `format_both` format string

Note: the "style" progress_keep allows you to keep the previously defined options and change selected ones only.
The progress bar will start anew, though.


```fortran
subroutine progress_bar_next()
    ! No arguments
end subroutine progress_bar_next
```

Use this to display the "next" step, if the style does not support showing a value or if the `show` option is `progress_none`.


```fortran
subroutine progress_bar_value( value )
    real, intent(in) :: value
end subroutine progress_bar_value
```

Use this to display the progress bar based on a value. This is only implemented for the styles `progress_delimited` and
`progress_no_cursor`, as it does not seem to make sense for the other styles.


```fortran
subroutine progress_bar_finish()
    ! No arguments
end subroutine progress_bar_finish
```

Use this routine to finish the progress bar, effectively writing a new line character to the console, so that
a next write statement starts on a new line.

