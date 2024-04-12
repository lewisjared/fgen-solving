!!!
! Wrapper for ``fgen_values_bounded``
!
! In combination with ``fgen_values_bounded_manager``,
! this allows the ``ValuesBounded`` derived type
! to be exposed to Python.
!!!
module fgen_values_bounded_w

   ! Standard library requirements
   use iso_c_binding, only: c_loc, c_ptr

   ! First-party requirements from the module we're wrapping
   use fgen_values_bounded, only: ValuesBounded
   use fgen_values_bounded_manager, only: &
      manager_get_free_instance => get_free_instance_number, &
      manager_instance_finalize => instance_finalize, &
      manager_get_instance => get_instance

   implicit none
   private

   public :: get_free_instance_number, &
             instance_build, &
             instance_finalize

   ! Statment declarations for getters and setters
   public :: iget_values
   public :: iget_values_shape  ! Helper for iget_values
   public :: iset_values
   public :: iget_value_last_bound
   public :: iset_value_last_bound
   public :: iget_units
   public :: iget_units_length  ! Helper for iget_units
   public :: iset_units

   ! Statement declarations for methods
   public :: i_repr
   public :: i_repr_length  ! Helper for i_repr
   public :: i_get_bounds
   public :: i_get_bounds_shape  ! Helper for i_get_bounds

contains

   function get_free_instance_number() result(instance_index)

      integer :: instance_index

      instance_index = manager_get_free_instance()

   end function get_free_instance_number

   ! Build methods
   !
   ! These are a bit like Python class methods,
   ! but they build/intialise/set up the class
   ! rather than returning a new instance.
   subroutine instance_build( &
      instance_index, &
      values, &
      value_last_bound, &
      units &
      )

      integer, intent(in) :: instance_index

      real(8), dimension(:), intent(in) :: values
      ! Passing of values

      real(8), intent(in) :: value_last_bound
      ! Passing of value_last_bound

      character(len=*), intent(in) :: units
      ! Passing of units

      type(ValuesBounded), pointer :: instance

      call manager_get_instance(instance_index, instance)

      call instance%build( &
         values=values, &
         value_last_bound=value_last_bound, &
         units=units &
         )

   end subroutine instance_build

   ! Finalisation
   subroutine instance_finalize(instance_index)

      integer, intent(in) :: instance_index

      call manager_instance_finalize(instance_index)

   end subroutine instance_finalize

   ! Attributes getters and setters
   ! Wrapping values
   ! Strategy: WrappingStrategyArrayDeferredSize(
   !     magnitude_suffix='_m',
   !     shape_suffix='_shape',
   !     shape_callable_suffix='_shape',
   ! )
   subroutine iget_values_shape( &
      instance_index, &
      nd, &
      dtype, &
      dshape &
      )
      ! Get information about values
      !
      ! This allows the array to be retrieved in a second step

      integer, intent(in) :: instance_index

      integer, intent(out) :: nd
      ! Number of dimensions in the array
      !
      ! If the array is not allocated, this will be returned as zero.

      character(len=20), intent(out) :: dtype
      ! Data type of the array

      integer, dimension(10), intent(out) :: dshape
      ! Shape of the array
      !
      ! If you have an array with more than 10-dimensions,
      ! an error will be raised.

      type(ValuesBounded), pointer :: instance

      call manager_get_instance(instance_index, instance)

      if (.not. allocated(instance%values)) then
         print *, "instance % values is not allocated"
         nd = 0
         return
      end if

      dtype = "real(8)"
      nd = rank(instance%values)
      if (nd > 10) then
         print *, "Array has more than 10 dimensions, wrapping won't work"
         error stop 1
      end if
      dshape(1:nd) = shape(instance%values)

   end subroutine iget_values_shape

   subroutine iget_values( &
      instance_index, &
      values &
      )

      integer, intent(in) :: instance_index

      real(8), dimension(:), intent(inout) :: values
      ! Returning of values.
      !
      ! This argument is `intent(inout)`,
      ! i.e. an array must be passed in from Python, which this subroutine then fills.

      type(ValuesBounded), pointer :: instance

      call manager_get_instance(instance_index, instance)

      values = instance%values

   end subroutine iget_values

   subroutine iset_values( &
      instance_index, &
      values &
      )

      integer, intent(in) :: instance_index

      real(8), dimension(:), intent(in) :: values
      ! Passing of values

      type(ValuesBounded), pointer :: instance

      call manager_get_instance(instance_index, instance)

      instance%values = values

   end subroutine iset_values

   ! Wrapping value_last_bound
   ! Strategy: WrappingStrategyDefault(
   !     magnitude_suffix='_m',
   ! )
   subroutine iget_value_last_bound( &
      instance_index, &
      value_last_bound &
      )

      integer, intent(in) :: instance_index

      real(8), intent(out) :: value_last_bound
      ! Returning of value_last_bound

      type(ValuesBounded), pointer :: instance

      call manager_get_instance(instance_index, instance)

      value_last_bound = instance%value_last_bound

   end subroutine iget_value_last_bound

   subroutine iset_value_last_bound( &
      instance_index, &
      value_last_bound &
      )

      integer, intent(in) :: instance_index

      real(8), intent(in) :: value_last_bound
      ! Passing of value_last_bound

      type(ValuesBounded), pointer :: instance

      call manager_get_instance(instance_index, instance)

      instance%value_last_bound = value_last_bound

   end subroutine iset_value_last_bound

   ! Wrapping units
   ! Strategy: WrappingStrategyCharacterDeferredSize(
   !     length_suffix='_length',
   !     length_callable_suffix='_length',
   ! )
   subroutine iget_units_length( &
      instance_index, &
      units_length &
      )

      integer, intent(in) :: instance_index

      integer, intent(out) :: units_length
      ! units's length

      type(ValuesBounded), pointer :: instance

      call manager_get_instance(instance_index, instance)

      units_length = len(instance%units)

   end subroutine iget_units_length

   subroutine iget_units( &
      instance_index, &
      n, &
      units &
      )

      integer, intent(in) :: instance_index

      integer, intent(in) :: n

      character(len=n), intent(out) :: units
      ! Returning of units

      type(ValuesBounded), pointer :: instance

      call manager_get_instance(instance_index, instance)

      units = instance%units

   end subroutine iget_units

   subroutine iset_units( &
      instance_index, &
      units &
      )

      integer, intent(in) :: instance_index

      character(len=*), intent(in) :: units
      ! Passing of units

      type(ValuesBounded), pointer :: instance

      call manager_get_instance(instance_index, instance)

      instance%units = units

   end subroutine iset_units

   ! Wrapped methods
   ! Wrapping res
   ! Strategy: WrappingStrategyCharacterDeferredSize(
   !     length_suffix='_length',
   !     length_callable_suffix='_length',
   ! )
   subroutine i_repr_length( &
      instance_index, &
      res_length &
      )

      integer, intent(in) :: instance_index

      integer, intent(out) :: res_length
      ! Length of res

      type(ValuesBounded), pointer :: instance

      call manager_get_instance(instance_index, instance)

      res_length = len(instance%repr( &
                       ))

   end subroutine i_repr_length

   subroutine i_repr( &
      instance_index, &
      n, &
      res &
      )

      integer, intent(in) :: instance_index

      integer, intent(in) :: n
      ! Length of res

      character(len=n), intent(out) :: res
      ! Returning of res

      type(ValuesBounded), pointer :: instance

      call manager_get_instance(instance_index, instance)

      res = instance%repr( &
            )

   end subroutine i_repr

   ! Wrapping bounds
   ! Strategy: WrappingStrategyArrayDeferredSize(
   !     magnitude_suffix='_m',
   !     shape_suffix='_shape',
   !     shape_callable_suffix='_shape',
   ! )
   subroutine i_get_bounds_shape( &
      instance_index, &
      nd, &
      dtype, &
      dshape &
      )
      ! Get information about the array returned by get_bounds
      !
      ! This allows the array to be retrieved in a second step.

      integer, intent(in) :: instance_index

      integer, intent(out) :: nd
      ! Number of dimensions in the array
      !
      ! If the array is not allocated, this will be returned as zero.

      character(len=20), intent(out) :: dtype
      ! Data type of the array

      integer, dimension(10), intent(out) :: dshape
      ! Shape of the array
      !
      ! If you have an array with more than 10-dimensions,
      ! an error will be raised.

      type(ValuesBounded), pointer :: instance
      real(kind=8), dimension(:), allocatable :: bounds

      call manager_get_instance(instance_index, instance)

      print *, "WARNING: Wrapping of methods returning deferred shape arrays " &
         //"calls the calculation twice, performance may be an issue"
      bounds = instance%get_bounds( &
               )

      dtype = "real(kind=8)"
      nd = rank(bounds)
      if (nd > 10) then
         print *, "Array has more than 10 dimensions, wrapping won't work"
         error stop 1
      end if

      dshape(1:nd) = shape(bounds)

   end subroutine i_get_bounds_shape

   subroutine i_get_bounds( &
      instance_index, &
      bounds &
      )

      integer, intent(in) :: instance_index

      real(kind=8), dimension(:), intent(inout) :: bounds
      ! Returning of bounds.
      !
      ! This argument is `intent(inout)`,
      ! i.e. an array must be passed in from Python, which this subroutine then fills.

      type(ValuesBounded), pointer :: instance

      call manager_get_instance(instance_index, instance)

      bounds = instance%get_bounds( &
               )

   end subroutine i_get_bounds

end module fgen_values_bounded_w
