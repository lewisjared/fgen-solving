module fgen_values_bounded

   use fgen_base_finalizable, only: BaseFinalizable

   implicit none
   private

   public ValuesBounded

   type, extends(BaseFinalizable) :: ValuesBounded
      ! Container for values that supports retrieving bounds too
      !
      ! The bounds are defined by the values,
      ! with the :attr:`last_bound_value`
      ! resolving the only remaining ambiguity
      ! about what value to use for the last bound.
      ! When handling time values,
      ! this lets us define the extent of the last time step.
      ! When handling other values,
      ! it provides a key piece of information needed
      ! to do interpolation/integration/differentiation unambiguosly.
      !
      ! However, you must remember that this container is quite low-level.
      ! As a result, it does not provide all the information required
      ! to do operations,
      ! such as interpolation/integration/differentiation,
      ! unambiguosly.
      ! For example, it does not specify whether the bounds are
      ! open, closed, half-open, half-closed etc.
      ! That information has to come from other classes/information.
      ! For example, the kind of interpolation
      ! (which does specify whether the bounds are open, closed etc.
      ! see :obj:`libfgen.fgen_1d_handling_options.OneDimensionalHandlingOption`
      ! and related functionality).
      !
      ! The design means that the bounds are tightly coupled to the values.
      ! This is deliberate, as it significantly simplifes our life.
      ! For example, one result of this construction is
      ! that the bounds are always contiguous.
      ! In other words, we can have a bounds concept and API
      ! without the headaches of having to handle arbitrary bounds and gaps.
      ! As far as we can tell, this container will support
      ! all the model-related situations we can think of.
      ! For example, if you want to skip time steps,
      ! just have a massive middle time step then throw the value away later.
      !
      ! Of course, such a tight coupling means that some things are not possible.
      ! However, this is model land, that we control,
      ! so we think the tight coupling is ok,
      ! particularly given that
      ! this isn't meant to be an all-purpose data container
      ! that can handle arbitrary data from any source
      ! (which is a much more difficult problem because
      ! that does require arbitary bound handling).
      !
      ! One other consequence of this container's structure is
      ! that we can’t have bounds which start before the first value
      ! (in the case of time, this means the start of the first timestep
      ! is always equal to the first value).
      ! However, we can't think of a situation in which that is needed
      ! (and excluding this possibility makes life much simpler).

      real(8), allocatable, dimension(:) :: values
      ! Values

      real(8) :: value_last_bound
      ! Value to use for bounds(2, size(bounds)) i.e. the last value to use in the bounds array
      !
      ! Required to avoid ambiguity in this value.

      character(len=:), allocatable :: units
      ! Units of :attr:`values` and :attr:`value_last_bound`

   contains

      private

      procedure, public :: build, build_from_values, finalize, repr, get_bounds

   end type ValuesBounded

   interface ValuesBounded
      module procedure :: constructor
   end interface ValuesBounded

contains

   function constructor(values, value_last_bound, units) result(self)

      real(kind=8), dimension(:), intent(in) :: values
      real(kind=8), intent(in) :: value_last_bound
      character(len=*), intent(in) :: units

      type(ValuesBounded) :: self

      call self%build(values=values, value_last_bound=value_last_bound, units=units)

   end function constructor

   subroutine build(self, values, value_last_bound, units)

      class(ValuesBounded), intent(inout) :: self

      real(kind=8), dimension(:), intent(in) :: values
      real(kind=8), intent(in) :: value_last_bound
      character(len=*), intent(in) :: units

      allocate (self%values, source=values)

      self%value_last_bound = value_last_bound

      allocate (character(len_trim(units)) :: self%units)
      self%units = units

   end subroutine build

   subroutine build_from_values(self, values, units)

      class(ValuesBounded), intent(inout) :: self

      real(kind=8), dimension(:), intent(in) :: values
      character(len=*), intent(in) :: units

      real(kind=8) :: value_last_bound
      integer :: sv

      sv = size(values)
      if (sv < 2) then
         print *, "Requires at least two points to intialise from values. Received: ", values
      end if

      value_last_bound = values(sv) + (values(sv) - values(sv - 1))

      call self%build(values=values, value_last_bound=value_last_bound, units=units)

   end subroutine build_from_values

   subroutine finalize(self)

      class(ValuesBounded), intent(inout) :: self

      if (allocated(self%values)) then
         deallocate (self%values)
      end if

      self%value_last_bound = 0.0D0

      if (allocated(self%units)) then
         deallocate (self%units)
      end if

   end subroutine finalize

   function repr(self, indent) result(res)
      ! Get character representation of self
      use fgen_char_conversions, only: MAX_LENGTH_CHARACTERS
      use fgen_char_conversions, only: float_1darray_to_char
      use fgen_char_conversions, only: float_to_char
      use fgen_char_conversions, only: write_after_whitespace

      class(ValuesBounded), intent(in) :: self

      integer, optional, intent(in) :: indent

      character(len=:), allocatable :: res
      ! Representation of self

      integer :: a_indent

      character(len=:), allocatable :: tmp

      if (present(indent)) then
         a_indent = indent
      else
         a_indent = 0
      end if

      allocate (character(MAX_LENGTH_CHARACTERS) :: tmp)

      if (a_indent > 0) then

         write (tmp, '(a)') ' '
         call write_after_whitespace( &
            'values='//float_1darray_to_char(self%values), &
            tmp, &
            right_pad=a_indent + 1 &
            )

      else

         write (tmp, '(a)') 'values='//float_1darray_to_char(self%values)

      end if

      call write_after_whitespace(new_line('a'), tmp)
      call write_after_whitespace( &
         'value_last_bound=' &
         //float_to_char(self%value_last_bound), &
         tmp, &
         right_pad=a_indent + 1 &
         )

      call write_after_whitespace(new_line('a'), tmp)
      call write_after_whitespace( &
         'units='//self%units, &
         tmp, &
         right_pad=a_indent + 1 &
         )

      res = trim(tmp)

   end function repr

   function get_bounds(self) result(bounds)

      class(ValuesBounded), intent(in) :: self

      real(kind=8), dimension(size(self%values) + 1) :: bounds

      integer :: sv

      sv = size(self%values)

      bounds(1:sv) = self%values
      bounds(sv + 1) = self%value_last_bound

   end function get_bounds

end module fgen_values_bounded
