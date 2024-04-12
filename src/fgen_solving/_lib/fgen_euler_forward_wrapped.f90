!!!
! Wrapper for ``fgen_euler_forward``
!
! In combination with ``fgen_euler_forward_manager``,
! this allows the ``EulerForwardStepper`` derived type
! to be exposed to Python.
!!!
module fgen_euler_forward_w

   ! Standard library requirements
   use iso_c_binding, only: c_loc, c_ptr

   ! First-party requirements from the module we're wrapping
   use fgen_euler_forward, only: EulerForwardStepper
   use fgen_euler_forward_manager, only: &
      manager_get_free_instance => get_free_instance_number, &
      manager_instance_finalize => instance_finalize, &
      manager_get_instance => get_instance

   implicit none
   private

   public :: get_free_instance_number, &
             instance_build, &
             instance_finalize

   ! Statment declarations for getters and setters
   public :: iget_step_size
   public :: iset_step_size

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
      step_size &
      )

      integer, intent(in) :: instance_index

      real(kind=8), intent(in) :: step_size
      ! Passing of step_size

      type(EulerForwardStepper), pointer :: instance

      call manager_get_instance(instance_index, instance)

      call instance%build( &
         step_size=step_size &
         )

   end subroutine instance_build

   ! Finalisation
   subroutine instance_finalize(instance_index)

      integer, intent(in) :: instance_index

      call manager_instance_finalize(instance_index)

   end subroutine instance_finalize

   ! Attributes getters and setters
   ! Wrapping step_size
   ! Strategy: WrappingStrategyDefault(
   !     magnitude_suffix='_m',
   ! )
   subroutine iget_step_size( &
      instance_index, &
      step_size &
      )

      integer, intent(in) :: instance_index

      real(kind=8), intent(out) :: step_size
      ! Returning of step_size

      type(EulerForwardStepper), pointer :: instance

      call manager_get_instance(instance_index, instance)

      step_size = instance%step_size

   end subroutine iget_step_size

   subroutine iset_step_size( &
      instance_index, &
      step_size &
      )

      integer, intent(in) :: instance_index

      real(kind=8), intent(in) :: step_size
      ! Passing of step_size

      type(EulerForwardStepper), pointer :: instance

      call manager_get_instance(instance_index, instance)

      instance%step_size = step_size

   end subroutine iset_step_size

end module fgen_euler_forward_w
