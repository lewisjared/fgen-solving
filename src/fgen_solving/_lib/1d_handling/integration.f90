module fgen_1d_integration
   ! 1D Integration

   use fgen_char_conversions, only: int_to_char
   use fgen_1d_linear_spline, only: integrate1d_linear
   use fgen_1d_next, only: integrate1d_next
   use fgen_1d_handling_options, only: OneDimensionalHandlingOption, LinearSpline, Next, Previous
   use fgen_1d_previous, only: integrate1d_previous

   implicit none
   private

   public :: integrate1d

contains

   subroutine integrate1d( &
      time_bounds, &
      time_units, &
      y_bounds, &
      y_units, &
      kind_interpolation, &
      c, &
      res_values_bounds, &
      res_units, &
      res_kind_interpolation &
      )
      ! Integrate
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
      ! Kind of interpolation to assume while integrating.

      real(kind=8), optional :: c
      ! Constant of integration, C
      !
      ! Default: 0.0
      !
      ! This is added to the integral to allow for a non-zero start.

      real(kind=8), dimension(size(time_bounds)), intent(out) :: res_values_bounds
      ! Resulting values at bounds

      character(len=:), allocatable, intent(out) :: res_units
      ! Result's units

      integer(kind(OneDimensionalHandlingOption)), intent(out) :: res_kind_interpolation
      ! Result's kind of interpolation
      !
      ! This allows us to round-trip integration and differentiation
      ! (to within a constant of integration).

      real(kind=8) :: a_c

      real(kind=8), dimension(size(time_bounds) - 1) :: window_integrals
      integer :: i

      if (present(c)) then
         a_c = c
      else
         a_c = 0.0D0
      end if

      if (kind_interpolation == LinearSpline) then
         call integrate1d_linear(time_bounds, y_bounds, window_integrals, res_kind_interpolation)
      elseif (kind_interpolation == Next) then
         call integrate1d_next(time_bounds, y_bounds, window_integrals, res_kind_interpolation)
      elseif (kind_interpolation == Previous) then
         call integrate1d_previous(time_bounds, y_bounds, window_integrals, res_kind_interpolation)
      else
         print *, "Integration not implemented for kind_interpolation="//int_to_char(kind_interpolation)
         error stop 1
      end if

      res_values_bounds(1) = a_c

      do i = 1, size(res_values_bounds) - 1

         res_values_bounds(i + 1) = res_values_bounds(i) + window_integrals(i)

      end do

      res_units = y_units//" "//time_units

   end subroutine integrate1d

end module fgen_1d_integration
