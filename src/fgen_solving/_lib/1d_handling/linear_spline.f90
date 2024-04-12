module fgen_1d_linear_spline
   ! LinearSpline 1D interpolation
   !
   ! The interpolated value is
   ! derived from a linear interpolation of the two points
   ! to either side of time_target.
   !
   ! The resulting curve is therefore only zero-order continuous.

   use fgen_array_helpers, only: check_arrays_same_shape, check_is_monotonic
   use fgen_1d_interpolation_base, only: BaseInterpolation1D
   use fgen_1d_handling_options, only: OneDimensionalHandlingOption, Previous, QuadraticSpline

   implicit none
   private

   public :: Interp1DLinearSpline
   ! No get_boundary_value_known_index1d_linear_spline
   ! function because this is a spline,
   ! so no special logic is required (you just return the value).
   public :: differentiate1d_linear
   public :: integrate1d_linear

   type, extends(BaseInterpolation1D) :: Interp1DLinearSpline
      ! LinearSpline 1D interpolator

      real(kind=8), dimension(:), allocatable :: time
      ! time-values to use for interpolation

      real(kind=8), dimension(:), allocatable :: y
      ! y-values to use for interpolation

   contains

      procedure, public :: build, finalize, interpolate

   end type Interp1DLinearSpline

   interface Interp1DLinearSpline
      module procedure :: constructor
   end interface Interp1DLinearSpline

contains

   function constructor(time, y, allow_extrapolation) result(self)

      real(kind=8), dimension(:), intent(in) :: time
      real(kind=8), dimension(:), intent(in) :: y
      logical, intent(in) :: allow_extrapolation

      type(Interp1DLinearSpline) :: self

      call self%build(time=time, y=y, allow_extrapolation=allow_extrapolation)

   end function constructor

   subroutine build(self, time, y, allow_extrapolation)

      class(Interp1DLinearSpline), intent(inout) :: self

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

      class(Interp1DLinearSpline), intent(in) :: self

      real(kind=8), intent(in) :: time_target

      real(kind=8) :: y_target

      integer :: end_segment_idx, time_size
      logical :: needs_extrap_backward, needs_extrap_forward, on_boundary
      real(kind=8) :: time1, y1, time2, y2, m

      call self%find_segment( &
         time_target=time_target, &
         time_segment_bounds=self%time, &
         end_segment_idx=end_segment_idx, &
         needs_extrap_backward=needs_extrap_backward, &
         needs_extrap_forward=needs_extrap_forward, &
         on_boundary=on_boundary &
         )

      if (on_boundary) then
         ! Fast return
         y_target = self%y(end_segment_idx)
         return
      end if

      if (needs_extrap_backward) then

         ! Use first two points
         time1 = self%time(1)
         y1 = self%y(1)

         time2 = self%time(2)
         y2 = self%y(2)

      elseif (needs_extrap_forward) then

         ! Use last two points
         time_size = size(self%time)

         time1 = self%time(time_size - 1)
         y1 = self%y(time_size - 1)

         time2 = self%time(time_size)
         y2 = self%y(time_size)

      else

         ! Use points surrounding time_target
         time1 = self%time(end_segment_idx - 1)
         y1 = self%y(end_segment_idx - 1)

         time2 = self%time(end_segment_idx)
         y2 = self%y(end_segment_idx)

      end if

      m = (y2 - y1)/(time2 - time1)
      y_target = m*(time_target - time1) + y1

   end function interpolate

   subroutine finalize(self)

      class(Interp1DLinearSpline), intent(inout) :: self

      if (allocated(self%time)) then
         deallocate (self%time)
      end if

      if (allocated(self%y)) then
         deallocate (self%y)
      end if

   end subroutine finalize

   subroutine differentiate1d_linear( &
      time_bounds, &
      y_bounds, &
      derivatives_at_bounds, &
      res_kind_interpolation &
      )
      ! Differentiate assuming a linear kind of interpolation
      !
      ! This is a very low-level function.
      ! In general, you will want to be using higher-level interfaces
      ! that can handle constants of integration, cumulative integration etc.

      real(kind=8), dimension(:), intent(in) :: time_bounds
      ! The time-values at the bounds of each time step.

      real(kind=8), dimension(size(time_bounds)), intent(in) :: y_bounds
      ! The y-values at the bounds of each time step.

      real(kind=8), dimension(size(time_bounds)), intent(out) :: derivatives_at_bounds
      ! Derivatives at each value in ``time_bounds``
      !
      ! We currently assume contiguous bounds.

      integer(kind(OneDimensionalHandlingOption)), intent(out) :: res_kind_interpolation
      ! Result's kind of interpolation

      integer :: i, stimeb

      stimeb = size(time_bounds)
      do i = 1, stimeb - 1

         derivatives_at_bounds(i) = ( &
                                    (y_bounds(i + 1) - y_bounds(i)) &
                                    /(time_bounds(i + 1) - time_bounds(i)) &
                                    )

      end do

      ! No information to do anything other
      ! than assume that the gradient at the last bound is continuous.
      derivatives_at_bounds(stimeb) = derivatives_at_bounds(stimeb - 1)

      res_kind_interpolation = Previous

   end subroutine differentiate1d_linear

   subroutine integrate1d_linear( &
      time_bounds, &
      y_bounds, &
      window_integrals, &
      res_kind_interpolation &
      )
      ! Integrate assuming a linear kind of interpolation
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
      real(kind=8) :: time_step_size, y2, y1

      do i = 1, size(time_bounds) - 1

         time_step_size = time_bounds(i + 1) - time_bounds(i)
         y2 = y_bounds(i + 1)
         y1 = y_bounds(i)

         window_integrals(i) = ( &
                               0.5D0*time_step_size*(y2 - y1) &
                               + y1*time_step_size &
                               )

      end do

      res_kind_interpolation = QuadraticSpline

   end subroutine integrate1d_linear

end module fgen_1d_linear_spline
