module fgen_1d_interpolation
   ! 1D interpolation

   use fgen_char_conversions, only: int_to_char
   use fgen_1d_interpolation_base, only: BaseInterpolation1D
   use fgen_1d_linear_spline, only: Interp1DLinearSpline
   use fgen_1d_next, only: Interp1DNext
   use fgen_1d_handling_options, only: OneDimensionalHandlingOption, LinearSpline, Next, Previous
   use fgen_1d_previous, only: Interp1DPrevious

   implicit none
   private

   public :: create_interp_1d

contains

   function create_interp_1d(time, y, kind_interpolation, allow_extrapolation) result(res)
      ! Create a 1D-interpolator

      real(kind=8), dimension(:), intent(in) :: time
      ! time-values to use for the interpolation

      real(kind=8), dimension(:), intent(in) :: y
      ! y-values to use for the interpolation

      integer(kind(OneDimensionalHandlingOption)), intent(in) :: kind_interpolation
      ! Kind of interpolator we wish to create

      logical, optional :: allow_extrapolation
      ! Should the interpolator also support extrapolation?
      !
      ! Default: .false.
      !
      ! If .true. the interpolator will also be able to extrapolate.
      ! If .false., errors will be raised
      ! if you try to use your interpolator for extrapolation.

      class(BaseInterpolation1D), allocatable ::  res
      ! Resulting object

      logical :: a_allow_extrapolation

      if (present(allow_extrapolation)) then
         a_allow_extrapolation = allow_extrapolation
      else
         a_allow_extrapolation = .false.
      end if

      if (kind_interpolation == LinearSpline) then

         allocate (res, source=Interp1DLinearSpline(time, y, a_allow_extrapolation))

      elseif (kind_interpolation == Previous) then

         allocate (res, source=Interp1DPrevious(time, y, a_allow_extrapolation))

      elseif (kind_interpolation == Next) then

         allocate (res, source=Interp1DNext(time, y, a_allow_extrapolation))

      else

         print *, "Interpolation not supported for kind_interpolation=" &
            //int_to_char(kind_interpolation, "(i5.0)")
         error stop 1

      end if

   end function create_interp_1d

end module fgen_1d_interpolation
