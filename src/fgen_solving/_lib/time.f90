module fgen_time

   use fgen_array_helpers, only: check_is_monotonic
   use fgen_char_conversions, only: float_to_char
   use fgen_values_bounded, only: ValuesBounded

   implicit none
   private

   public TimeAxis

   type, extends(ValuesBounded) :: TimeAxis
      ! Time axis
      !
      ! This is just a :class:`ValuesBounded`,
      ! with the extra restriction
      ! that the values must be strictly monotonically increasing.
      !
      ! The values are the values (i.e. points) which define the time axis.
      !
      ! ``value_last_bound`` defines the end bound of the last time step.

   contains

      private

      procedure, public :: build

   end type TimeAxis

   interface TimeAxis
      module procedure :: constructor
   end interface TimeAxis

contains

   function constructor(values, value_last_bound, units) result(self)

      real(kind=8), dimension(:), intent(in) :: values
      real(kind=8), intent(in) :: value_last_bound
      character(len=*), intent(in) :: units

      type(TimeAxis) :: self

      call self%build(values=values, value_last_bound=value_last_bound, units=units)

   end function constructor

   subroutine build(self, values, value_last_bound, units)

      class(TimeAxis), intent(inout) :: self

      real(kind=8), dimension(:), intent(in) :: values
      real(kind=8), intent(in) :: value_last_bound
      character(len=*), intent(in) :: units

      ! Extra checks above what are in ValuesBounded initialiser
      call check_is_monotonic(values, increasing=.true., strict=.true.)
      if (.not. (value_last_bound > values(size(values)))) then
         print *, "value_last_bound must be greater than the last value in values. " &
            //"Received value_last_bound="//float_to_char(value_last_bound) &
            //". values(size(values))="//float_to_char(values(size(values)))
         error stop 1
      end if

      allocate (self%values, source=values)

      self%value_last_bound = value_last_bound

      allocate (character(len_trim(units)) :: self%units)
      self%units = units

   end subroutine build

end module fgen_time
