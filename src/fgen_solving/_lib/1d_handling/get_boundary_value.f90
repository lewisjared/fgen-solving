module fgen_1d_get_boundary_value
   ! Logic that defines the value to be used at the boundary of 1D arrays

   use fgen_char_conversions, only: int_to_char
   use fgen_1d_next, only: get_boundary_value_known_index1d_next
   use fgen_1d_handling_options, only: OneDimensionalHandlingOption
   use fgen_1d_handling_options, only: CubicSpline
   use fgen_1d_handling_options, only: LinearSpline
   use fgen_1d_handling_options, only: Next
   use fgen_1d_handling_options, only: NotSpecified
   use fgen_1d_handling_options, only: Previous
   use fgen_1d_handling_options, only: QuadraticSpline
   use fgen_1d_previous, only: get_boundary_value_known_index1d_previous

   implicit none
   private

   public :: get_boundary_value_known_index_1d

contains

   function get_boundary_value_known_index_1d(y, idx, kind_interpolation) result(boundary_value)
      ! Get the boundary value, given we already know the index we're interested in.

      real(kind=8), dimension(:), intent(in) :: y
      ! Array of values from which to retrieve the value

      integer, intent(in) :: idx
      ! Index of the match in the x-values for which to retrieve the value

      integer(kind(OneDimensionalHandlingOption)), intent(in) :: kind_interpolation
      ! Interpolation kind to assume
      !
      ! This defines the logic for how the value is returned.

      real(kind=8) :: boundary_value
      ! Value at the boundary

      if (kind_interpolation == LinearSpline) then
         ! Spline, just return the value at the boundary
         boundary_value = y(idx)

      elseif (kind_interpolation == Next) then
         boundary_value = get_boundary_value_known_index1d_next(y=y, idx=idx)

      elseif (kind_interpolation == NotSpecified) then
         ! Just return the value at the boundary
         boundary_value = y(idx)

      elseif (kind_interpolation == Previous) then
         boundary_value = get_boundary_value_known_index1d_previous(y=y, idx=idx)

      else
         print *, "get_boundary_value_known_index1d not implemented for kind_interpolation="//int_to_char(kind_interpolation)
         error stop 1

      end if

   end function get_boundary_value_known_index_1d

end module fgen_1d_get_boundary_value
