module fgen_timeseries

   use fgen_array_helpers, only: check_arrays_same_shape
   use fgen_base_finalizable, only: BaseFinalizable
   use fgen_1d_get_boundary_value, only: get_boundary_value_known_index_1d
   use fgen_char_conversions, only: float_1darray_to_char, float_to_char, int_to_char, write_after_whitespace
   use fgen_1d_differentiation, only: differentiate_1d
   use fgen_1d_integration, only: integrate1d
   use fgen_1d_interpolation, only: create_interp_1d
   use fgen_1d_interpolation_base, only: BaseInterpolation1D
   use fgen_1d_handling_options, only: OneDimensionalHandlingOption, LinearSpline, Next, NotSpecified, Previous, QuadraticSpline
   use fgen_time, only: TimeAxis
   use fgen_utils, only: searchsorted
   use fgen_values_bounded, only: ValuesBounded

   implicit none
   private

   integer, parameter, public :: INDENT_COMPONENTS_REPR = 4
   ! Number of spaces to use when indenting attributes in repr

   public Timeseries

   type, extends(BaseFinalizable) :: Timeseries
      ! Timeseries handling container

      character(len=:), allocatable :: name
      ! Name of the timeseries

      type(ValuesBounded), allocatable :: values
      ! Values of the timeseries

      type(TimeAxis), allocatable :: time
      ! Time axis of the timeseries

      integer(kind(OneDimensionalHandlingOption)) :: spline = NotSpecified
      ! Spline to use when converting from discrete to continuous values.
      !
      ! Default: NotSpecified
      !
      ! This is required to perform operations like differentiation and integration
      ! without approximation errors.
      !
      ! TODO: rename this.
      ! It isn't a spline in all cases so better to use kind_interpolation.

   contains

      private

      procedure, public :: build, finalize
      procedure, public :: repr
      procedure, public :: get_time_units
      procedure, public :: get_values_units
      procedure, public :: get_value_at_time
      procedure, public :: interpolate
      procedure, public :: interpolate_single
      procedure, public :: differentiate
      procedure, public :: integrate
      procedure :: get_interpolator

   end type Timeseries

   interface Timeseries
      module procedure :: constructor
   end interface Timeseries

contains

   function constructor(name, values, time, spline) result(self)

      character(len=*), intent(in) :: name
      type(ValuesBounded), intent(in) :: values
      type(TimeAxis), intent(in) :: time
      integer(kind(OneDimensionalHandlingOption)), optional, intent(in) :: spline

      type(Timeseries) :: self

      call self%build(name=name, values=values, time=time, spline=spline)

   end function constructor

   subroutine build(self, name, values, time, spline)

      class(Timeseries), intent(inout) :: self

      character(len=*), intent(in) :: name
      type(ValuesBounded), intent(in) :: values
      type(TimeAxis), intent(in) :: time
      integer(kind(OneDimensionalHandlingOption)), optional, intent(in) :: spline

      call check_arrays_same_shape(values%values, time%values)

      allocate (character(len_trim(name)) :: self%name)
      self%name = trim(name)

      allocate (self%values, source=values)
      allocate (self%time, source=time)

      if (present(spline)) then
         self%spline = spline
      end if

   end subroutine build

   subroutine finalize(self)

      class(Timeseries), intent(inout) :: self

      if (allocated(self%name)) then
         deallocate (self%name)
      end if

      if (allocated(self%values)) then
         deallocate (self%values)
      end if

      if (allocated(self%time)) then
         deallocate (self%time)
      end if

   end subroutine finalize

   function repr(self, max_length, format_string_int, format_string_real) result(res)
      ! Get character representation of self

      class(Timeseries), intent(in) :: self

      integer, optional :: max_length
      ! Max length of repr
      !
      ! Default: 880
      !
      ! Only required if you are going to make a really long string.
      ! You can tell if you need to make this bigger
      ! because you wil get an end of record runtime error.

      character(len=*), optional :: format_string_int
      ! Format string to use for formatting integers
      !
      ! Default: '(i3.0)'

      character(len=*), optional :: format_string_real
      ! Format string to use for formatting reals
      !
      ! Default: '(f10.5)'

      character(len=:), allocatable :: res
      ! Representation of self

      integer :: a_max_length
      logical :: a_uniform_size

      character(len=:), allocatable :: a_format_string_int
      character(len=:), allocatable :: a_format_string_real
      character(len=:), allocatable :: tmp

      if (present(max_length)) then
         a_max_length = max_length
      else
         a_max_length = 880
      end if

      if (present(format_string_int)) then
         a_format_string_int = format_string_int
      else
         a_format_string_int = '(i3.0)'
      end if

      if (present(format_string_real)) then
         a_format_string_real = format_string_real
      else
         a_format_string_real = '(f10.5)'
      end if

      ! Hard-code for now, not sure why you wouldn't want this
      a_uniform_size = .true.

      allocate (character(a_max_length) :: tmp)

      write (tmp, '(a)') 'name='//self%name
      call write_after_whitespace( &
         new_line('a')//'spline=' &
         //int_to_char(self%spline, format_string=a_format_string_int), &
         tmp &
         )
      call write_after_whitespace( &
         new_line('a')//'values %'//new_line('a') &
         //self%values%repr(indent=INDENT_COMPONENTS_REPR), &
         tmp &
         )
      call write_after_whitespace( &
         new_line('a')//'time %'//new_line('a') &
         //self%time%repr(indent=INDENT_COMPONENTS_REPR), &
         tmp &
         )

      res = trim(tmp)

   end function repr

   function get_time_units(self) result(tunits)
      ! The time units
      !
      ! This allows us to get the time units without having to create
      ! a TimeAxis instance in Python.
      ! Bit of a hack, but also not the worst solution.

      class(Timeseries), intent(in) :: self

      character(len=:), allocatable :: tunits

      allocate (character(len_trim(self%time%units)) :: tunits)

      tunits = self%time%units

   end function get_time_units

   function get_values_units(self) result(vunits)
      ! The values units
      !
      ! This allows us to get the values units without having to create
      ! a valuesAxis instance in Python.
      ! Bit of a hack, but also not the worst solution.

      class(Timeseries), intent(in) :: self

      character(len=:), allocatable :: vunits

      allocate (character(len_trim(self%values%units)) :: vunits)

      vunits = self%values%units

   end function get_values_units

   function get_value_at_time(self, time, exp_unit) result(value_at_time)
      ! Get value from :attr:`self % values` that applies at time ``time``

      class(Timeseries), intent(in) :: self

      real(kind=8), intent(in) :: time
      ! Time for which to get the value

      character(len=*), optional, intent(in) :: exp_unit
      ! Expected unit of the value
      !
      ! If not supplied, no check of the unit is done.
      ! If supplied and the units of :attr:`self % values%` doesn't match,
      ! an error is raised.

      real(kind=8) :: value_at_time
      ! Value from :attr:`self % values` at time ``time``.

      integer :: idx

      idx = searchsorted(self%time%values, time, exact=.true.)

      if (present(exp_unit)) then
         if (self%values%units /= exp_unit) then
            print *, "Values are not in expected units. " &
               //"self % values % units="//self%values%units//". " &
               //"exp_unit="//exp_unit
            error stop 1
         end if
      end if

      if (idx == 0) then
         print *, "Time not found. time="//float_to_char(time)//". " &
            //"self % time % values="//float_1darray_to_char(self%time%values)
         error stop 1
      end if

      value_at_time = get_boundary_value_known_index_1d( &
                      y=self%values%values, &
                      idx=idx, &
                      kind_interpolation=self%spline &
                      )

   end function get_value_at_time

   function interpolate(self, time_axis_new, allow_extrapolation) result(res)
      ! Interpolate onto a new time axis

      class(Timeseries), intent(in) :: self

      type(TimeAxis), target, intent(in) :: time_axis_new
      ! Time axis onto which to interpolate

      logical, optional, intent(in) :: allow_extrapolation
      ! Should extrapolation be allowed during the interpolation?
      !
      ! Default: .false.

      type(Timeseries) :: res
      ! The interpolated :obj:`Timeseries`

      logical :: a_allow_extrapolation

      class(BaseInterpolation1D), allocatable ::  interpolator
      integer :: i
      real(kind=8), dimension(size(time_axis_new%values)) :: values_new
      real(kind=8) :: value_last_bound_new
      type(ValuesBounded), target, save :: values_out

      if (present(allow_extrapolation)) then
         a_allow_extrapolation = allow_extrapolation
      else
         a_allow_extrapolation = .false.
      end if

      if (self%time%units /= time_axis_new%units) then
         print *, "Conversion of time units is not implemented"
         error stop 1
      end if

      allocate (interpolator, source=self%get_interpolator(allow_extrapolation=a_allow_extrapolation))

      do i = 1, size(time_axis_new%values)

         values_new(i) = interpolator%interpolate(time_axis_new%values(i))

      end do

      value_last_bound_new = interpolator%interpolate(time_axis_new%value_last_bound)

      values_out = ValuesBounded( &
                   values=values_new, &
                   value_last_bound=value_last_bound_new, &
                   units=self%values%units &
                   )

      res = Timeseries( &
            name=self%name, &
            values=values_out, &
            time=time_axis_new, &
            spline=self%spline &
            )

   end function interpolate

   function interpolate_single(self, time, allow_extrapolation) result(res)
      ! Interpolate a value for a single point in time

      class(Timeseries), intent(in) :: self

      real(kind=8), intent(in) :: time
      ! Time value for which to get the interpolated value

      logical, optional, intent(in) :: allow_extrapolation
      ! Should extrapolation be allowed during the interpolation?
      !
      ! Default: .false.

      real(kind=8) :: res
      ! The interpolated value

      logical :: a_allow_extrapolation
      class(BaseInterpolation1D), allocatable ::  interpolator

      if (present(allow_extrapolation)) then
         a_allow_extrapolation = allow_extrapolation
      else
         a_allow_extrapolation = .false.
      end if

      allocate ( &
         interpolator, &
         source=self%get_interpolator( &
         allow_extrapolation=a_allow_extrapolation &
         ) &
         )

      res = interpolator%interpolate(time)

   end function interpolate_single

   function get_interpolator(self, allow_extrapolation) result(interpolator)
      ! Get interpolator that matches self
      !
      ! TODO: Caching this would speed things up
      ! if there were lots of repeated interpolation calls.
      ! By how much is a different question.
      ! Not sure how easy or difficult that caching would be though.
      ! We would probably need to somehow freeze the `Timeseries` objects
      ! to ensure the data hasn't changed (or check for changes or something).

      class(Timeseries), intent(in) :: self

      logical, intent(in) :: allow_extrapolation
      ! Should the interpolator allow extrapolation?

      class(BaseInterpolation1D), allocatable :: interpolator
      ! Interpolator that matches self

      real(kind=8), dimension(size(self%time%values) + 1) :: time_interp_in
      real(kind=8), dimension(size(self%time%values) + 1) :: values_interp_in

      time_interp_in(1:size(time_interp_in) - 1) = self%time%values
      time_interp_in(size(time_interp_in)) = self%time%value_last_bound

      values_interp_in(1:size(values_interp_in) - 1) = self%values%values
      values_interp_in(size(values_interp_in)) = self%values%value_last_bound

      allocate (interpolator, source=create_interp_1d( &
                time=time_interp_in, &
                y=values_interp_in, &
                kind_interpolation=self%spline, &
                allow_extrapolation=allow_extrapolation &
                ))

   end function get_interpolator

   function differentiate(self, name_res) result(res)
      ! Differentiate

      class(Timeseries), intent(in) :: self

      character(len=*), intent(in), optional :: name_res
      ! Name of the result
      !
      ! Default: derivative

      type(Timeseries) :: res
      ! Derivative

      character(len=:), allocatable :: a_name_res

      real(kind=8), dimension(size(self%values%values) + 1) :: res_values_bounds
      character(len=:), allocatable :: res_values_units
      integer(kind(OneDimensionalHandlingOption)) :: res_kind_interpolation

      ! Try to switch to using allocatable here,
      ! see https://gitlab.com/magicc/fgen/-/issues/42
      type(ValuesBounded), target, save :: res_values
      type(TimeAxis), target, save :: res_time

      if (present(name_res)) then
         a_name_res = name_res
      else
         a_name_res = "derivative"
      end if

      call differentiate_1d( &
         time_bounds=self%time%get_bounds(), &
         time_units=self%time%units, &
         y_bounds=self%values%get_bounds(), &
         y_units=self%values%units, &
         kind_interpolation=self%spline, &
         res_values_bounds=res_values_bounds, &
         res_units=res_values_units, &
         res_kind_interpolation=res_kind_interpolation &
         )

      ! If we switch to allocatable, we might not need this.
      ! See https://gitlab.com/magicc/fgen/-/issues/42
      if (allocated(res_values%values)) then
         call res_values%finalize()
      end if

      call res_values%build( &
         values=res_values_bounds(1:size(res_values_bounds) - 1), &
         value_last_bound=res_values_bounds(size(res_values_bounds)), &
         units=res_values_units &
         )

      ! Copy method would be very handy here
      res_time = TimeAxis( &
                 values=self%time%values, &
                 value_last_bound=self%time%value_last_bound, &
                 units=self%time%units &
                 )

      call res%build( &
         name=a_name_res, &
         values=res_values, &
         time=res_time, &
         spline=res_kind_interpolation &
         )

   end function differentiate

   function integrate(self, c, name_res) result(res)
      ! Integrate

      class(Timeseries), intent(in) :: self

      real(kind=8), optional :: c
      ! Constant of integration, C
      !
      ! Default: 0.0
      !
      ! This is added to the integral to allow for a non-zero start.

      character(len=*), intent(in), optional :: name_res
      ! Name of the result
      !
      ! Default: integral

      type(Timeseries) :: res
      ! Integral

      character(len=:), allocatable :: a_name_res

      real(kind=8), dimension(size(self%values%values) + 1) :: res_values_bounds
      character(len=:), allocatable :: res_values_units
      integer(kind(OneDimensionalHandlingOption)) :: res_kind_interpolation

      type(ValuesBounded), target, save :: res_values
      type(TimeAxis), target, save :: res_time

      if (present(name_res)) then
         a_name_res = name_res
      else
         a_name_res = "integral"
      end if

      call integrate1d( &
         time_bounds=self%time%get_bounds(), &
         time_units=self%time%units, &
         y_bounds=self%values%get_bounds(), &
         y_units=self%values%units, &
         kind_interpolation=self%spline, &
         c=c, &
         res_values_bounds=res_values_bounds, &
         res_units=res_values_units, &
         res_kind_interpolation=res_kind_interpolation &
         )

      if (allocated(res_values%values)) then
         call res_values%finalize()
      end if

      call res_values%build( &
         values=res_values_bounds(1:size(res_values_bounds) - 1), &
         value_last_bound=res_values_bounds(size(res_values_bounds)), &
         units=res_values_units &
         )

      ! Copy method would be very handy here
      res_time = TimeAxis( &
                 values=self%time%values, &
                 value_last_bound=self%time%value_last_bound, &
                 units=self%time%units &
                 )

      call res%build( &
         name=a_name_res, &
         values=res_values, &
         time=res_time, &
         spline=res_kind_interpolation &
         )

   end function integrate

end module fgen_timeseries
