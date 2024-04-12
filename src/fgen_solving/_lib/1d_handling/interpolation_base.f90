module fgen_1d_interpolation_base
   ! Base class for 1D interpolation

   use fgen_base_finalizable, only: BaseFinalizable
   use fgen_char_conversions, only: float_to_char
   use fgen_utils, only: searchsorted

   implicit none
   private

   public :: BaseInterpolation1D

   type, abstract, extends(BaseFinalizable) :: BaseInterpolation1D
      ! Base class for 1D interpolators

      logical :: allow_extrapolation
      ! Whether to allow extrapolation or not

   contains

      private

      procedure, public :: find_segment
      ! Check if extrapolation needs to be performed
      !
      ! Also returns information about
      ! what kind of extrapolation needs to be performed

      procedure(derived_type_interpolate), public, deferred :: interpolate
      ! Calculate interpolated value

   end type BaseInterpolation1D

   interface

      function derived_type_interpolate(self, time_target) result(y_target)
         import :: BaseInterpolation1D

         class(BaseInterpolation1D), intent(in) :: self

         real(kind=8), intent(in) :: time_target

         real(kind=8) :: y_target

      end function derived_type_interpolate

   end interface

contains

   subroutine find_segment( &
      self, &
      time_target, &
      time_segment_bounds, &
      end_segment_idx, &
      needs_extrap_backward, &
      needs_extrap_forward, &
      on_boundary &
      )

      class(BaseInterpolation1D), intent(in) :: self

      real(kind=8), intent(in) :: time_target
      real(kind=8), dimension(:), intent(in) :: time_segment_bounds
      integer, intent(out) :: end_segment_idx
      logical, intent(out) :: needs_extrap_backward
      logical, intent(out) :: needs_extrap_forward
      logical, optional, intent(out) :: on_boundary

      logical :: needs_extrap

      end_segment_idx = searchsorted(time_segment_bounds, time_target)

      needs_extrap_forward = (end_segment_idx == size(time_segment_bounds) + 1)

      ! Check if we can fast return because there is an exact match
      if (.not. needs_extrap_forward) then
         if (time_target == time_segment_bounds(end_segment_idx)) then
            ! We're on the boundary of a segment, fast return
            needs_extrap_backward = .false.
            if (present(on_boundary)) then
               on_boundary = .true.
            end if

            return
         end if
      end if

      if (present(on_boundary)) then
         on_boundary = .false.
      end if

      needs_extrap_backward = ( &
                              (.not. needs_extrap_forward) &
                              .and. (end_segment_idx == 1) &
                              )
      needs_extrap = needs_extrap_backward .or. needs_extrap_forward

      if (needs_extrap .and. (.not. self%allow_extrapolation)) then
         if (needs_extrap_backward) then
            print *, "Extrapolation is not allowed and time_target is " &
               //"before the start of the interpolation range. time_target=" &
               //float_to_char(time_target, '(f10.5)')//". " &
               //"start of interpolation range=" &
               //float_to_char(time_segment_bounds(1), '(f10.5)')
         else
            print *, "Extrapolation is not allowed and time_target is " &
               //"after the end of the interpolation range. time_target=" &
               //float_to_char(time_target, '(f10.5)')//". " &
               //"end of interpolation range=" &
               //float_to_char(time_segment_bounds(size(time_segment_bounds)), '(f10.5)')
         end if

         error stop 1

      end if

   end subroutine find_segment

end module fgen_1d_interpolation_base
