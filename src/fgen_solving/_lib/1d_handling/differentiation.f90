module fgen_1d_differentiation
   !  1D differentiation

   use fgen_char_conversions, only: int_to_char
   use fgen_1d_linear_spline, only: differentiate1d_linear
   use fgen_1d_handling_options, only: OneDimensionalHandlingOption, LinearSpline

   implicit none
   private

   public :: differentiate_1d

contains

   subroutine differentiate_1d( &
      time_bounds, &
      time_units, &
      y_bounds, &
      y_units, &
      kind_interpolation, &
      res_values_bounds, &
      res_units, &
      res_kind_interpolation &
      )
      ! Differentiate
      !
      ! This could be a pure function,
      ! if we could get around the circularity
      ! and use Timeseries as the return type.
      ! This seems like it would be very difficult
      ! so not attempting it now.

      real(kind=8), dimension(:), intent(in) :: time_bounds
      ! The time-values at the bounds of each time step.

      character(len=*), intent(in) :: time_units
      ! Units of the time-values

      real(kind=8), dimension(size(time_bounds)), intent(in) :: y_bounds
      ! The y-values at the bounds of each time step.

      character(len=*), intent(in) :: y_units
      ! Units of the y-values

      integer(kind(OneDimensionalHandlingOption)), intent(in) :: kind_interpolation
      ! Kind of interpolation to assume while differentiating.

      real(kind=8), dimension(size(time_bounds)), intent(out) :: res_values_bounds
      ! Resulting values at bounds

      character(len=:), allocatable, intent(out) :: res_units
      ! Result's units

      integer(kind(OneDimensionalHandlingOption)), intent(out) :: res_kind_interpolation
      ! Result's kind of interpolation
      !
      ! This allows us to round-trip integration and differentiation
      ! (to within a constant of integration).

      if (kind_interpolation == LinearSpline) then
         call differentiate1d_linear(time_bounds, y_bounds, res_values_bounds, res_kind_interpolation)
      else
         print *, "Differentiation not implemented for kind_interpolation="//int_to_char(kind_interpolation)
         error stop 1
      end if

      res_units = y_units//" / "//time_units

   end subroutine differentiate_1d

end module fgen_1d_differentiation
