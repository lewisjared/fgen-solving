module fgen_1d_next
   ! Next-value 1D interpolation
   !
   ! The interpolated value is always equal to the next value in the array
   ! from which to interpolate.
   !
   ! This can be confusing to think about.
   !
   ! At the boundaries (i.e time(i)) we return values(i).
   ! For other values of time_target between time(i) and time(i + 1),
   ! we always take y(i + 1) (i.e. we always take the 'next' value).
   ! As a result,
   ! y_target = y(i + 1) for time(i) < time_target <= time(i + 1)
   !
   ! If helpful, we have drawn a picture of how this works below.
   ! Symbols:
   ! - time: y-value selected for this time-value
   ! - i: closed (i.e. inclusive) boundary
   ! - o: open (i.e. exclusive) boundary
   !
   ! y(4):                                    oxxxxxxxxxxxxxxxxxxxxxxxxxx
   ! y(3):                        oxxxxxxxxxxxi
   ! y(2):            oxxxxxxxxxxxi
   ! y(1): xxxxxxxxxxxi
   !       -----------|-----------|-----------|-----------|--------------
   !               time(1)     time(2)     time(3)     time(4)
   !
   ! One other way to think about this is
   ! that the y-values are shifted to the left compared to the time-values.
   ! As a result, y(1) is only used for (backward) extrapolation,
   ! it isn't actually used in the interpolation domain at all.

   use fgen_array_helpers, only: check_arrays_same_shape, check_is_monotonic
   use fgen_1d_interpolation_base, only: BaseInterpolation1D
   use fgen_1d_handling_options, only: OneDimensionalHandlingOption, LinearSpline

   implicit none
   private

   public :: Interp1DNext
   public :: get_boundary_value_known_index1d_next
   ! Differentiation not implemented yet
   public :: integrate1d_next

   type, extends(BaseInterpolation1D) :: Interp1DNext
      ! Next-value 1D interpolator

      real(kind=8), dimension(:), allocatable :: time
      ! time-values to use for interpolation

      real(kind=8), dimension(:), allocatable :: y
      ! y-values to use for interpolation

   contains

      procedure, public :: build, finalize, interpolate

   end type Interp1DNext

   interface Interp1DNext
      module procedure :: constructor
   end interface Interp1DNext

contains

   function constructor(time, y, allow_extrapolation) result(self)

      real(kind=8), dimension(:), intent(in) :: time
      real(kind=8), dimension(:), intent(in) :: y
      logical, intent(in) :: allow_extrapolation

      type(Interp1DNext) :: self

      call self%build(time=time, y=y, allow_extrapolation=allow_extrapolation)

   end function constructor

   subroutine build(self, time, y, allow_extrapolation)

      class(Interp1DNext), intent(inout) :: self

      real(kind=8), dimension(:), intent(in) :: time
      real(kind=8), dimension(:), intent(in) :: y
      logical, intent(in) :: allow_extrapolation

      call check_is_monotonic(time)
      call check_arrays_same_shape(time, y)

      allocate (self%time, mold=time)
      allocate (self%y, mold=y)

      self%time = time
      self%y = y
      self%allow_extrapolation = allow_extrapolation

   end subroutine build

   function interpolate(self, time_target) result(y_target)
      use fgen_utils, only: searchsorted

      class(Interp1DNext), intent(in) :: self

      real(kind=8), intent(in) :: time_target

      real(kind=8) :: y_target

      integer :: end_segment_idx
      logical :: needs_extrap_backward, needs_extrap_forward, on_boundary

      call self%find_segment( &
         time_target=time_target, &
         time_segment_bounds=self%time, &
         end_segment_idx=end_segment_idx, &
         needs_extrap_backward=needs_extrap_backward, &
         needs_extrap_forward=needs_extrap_forward, &
         on_boundary=on_boundary &
         )

      ! Edge cases :)
      if (on_boundary) then
         y_target = get_boundary_value_known_index1d_next( &
                    self%y, end_segment_idx &
                    )
         return
      end if

      if (needs_extrap_backward) then
         y_target = self%y(1)
         return
      end if

      if (needs_extrap_forward) then
         y_target = self%y(size(self%y))
         return
      end if

      ! time_target < time(end_segment_idx), hence return y(end_segment_idx)
      y_target = self%y(end_segment_idx)

   end function interpolate

   subroutine finalize(self)

      class(Interp1DNext), intent(inout) :: self

      if (allocated(self%time)) then
         deallocate (self%time)
      end if

      if (allocated(self%y)) then
         deallocate (self%y)
      end if

   end subroutine finalize

   function get_boundary_value_known_index1d_next(y, idx) result(boundary_value)
      ! Get the boundary value, given we already know the index we're interested in.
      !
      ! Keeping in mind our right-open intervals,
      ! we return the value from idx + 1.
      ! (also see the diagram at the top of the module)
      ! The exception is the last boundary,
      ! which we treat as closed at both ends.

      real(kind=8), dimension(:), intent(in) :: y
      ! Array of values from which to retrieve the value

      integer, intent(in) :: idx
      ! Index of the match in the time-values for which to retrieve the value

      real(kind=8) :: boundary_value
      ! Value at the boundary

      boundary_value = y(idx)

   end function get_boundary_value_known_index1d_next

   subroutine integrate1d_next( &
      time_bounds, &
      y_bounds, &
      window_integrals, &
      res_kind_interpolation &
      )
      ! Integrate assuming a next kind of interpolation
      !
      ! This just returns the integral across each window,
      ! it does not return the cumulative integral!!!
      !
      ! In other words, this is a very low-level function.
      ! In general, you will want to be using higher-level interfaces
      ! that can handle constants of integration, cumulative integration etc.

      real(kind=8), dimension(:), intent(in) :: time_bounds
      ! The time-values at the bounds of each time step.

      real(kind=8), dimension(size(time_bounds)), intent(in) :: y_bounds
      ! The y-values at the bounds of each time step.

      real(kind=8), dimension(size(time_bounds) - 1), intent(out) :: window_integrals
      ! Integral within each window.

      integer(kind(OneDimensionalHandlingOption)), intent(out) :: res_kind_interpolation
      ! Result's kind of interpolation

      integer :: i
      real(kind=8) :: time_step_size

      do i = 1, size(time_bounds) - 1

         time_step_size = time_bounds(i + 1) - time_bounds(i)

         ! Next integration so we can ignore y_bounds(i)
         window_integrals(i) = y_bounds(i + 1)*time_step_size

      end do

      res_kind_interpolation = LinearSpline

   end subroutine integrate1d_next

end module fgen_1d_next
