!!!
! Wrapper for ``fgen_rk``
!
! In combination with ``fgen_rk_manager``,
! this allows the ``RK4Stepper`` derived type
! to be exposed to Python.
!!!
module fgen_rk_w

   ! Standard library requirements
   use iso_c_binding, only: c_loc, c_ptr

   ! First-party requirements from the module we're wrapping
   use fgen_rk, only: RK4Stepper
   use fgen_rk_manager, only: &
      manager_get_free_instance => get_free_instance_number, &
      manager_instance_finalize => instance_finalize, &
      manager_get_instance => get_instance

   implicit none
   private

   public :: get_free_instance_number, &
             instance_build, &
             instance_finalize

   ! Statment declarations for getters and setters
   public :: iget_rtol
   public :: iset_rtol
   public :: iget_atol
   public :: iset_atol
   public :: iget_h
   public :: iset_h

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
      rtol, &
      atol, &
      h &
      )

      integer, intent(in) :: instance_index

      real(kind=8), intent(in) :: rtol
      ! Passing of rtol

      real(kind=8), intent(in) :: atol
      ! Passing of atol

      real(kind=8), intent(in) :: h
      ! Passing of h

      type(RK4Stepper), pointer :: instance

      call manager_get_instance(instance_index, instance)

      call instance%build( &
         rtol=rtol, &
         atol=atol, &
         h=h &
         )

   end subroutine instance_build

   ! Finalisation
   subroutine instance_finalize(instance_index)

      integer, intent(in) :: instance_index

      call manager_instance_finalize(instance_index)

   end subroutine instance_finalize

   ! Attributes getters and setters
   ! Wrapping rtol
   ! Strategy: WrappingStrategyDefault(
   !     magnitude_suffix='_m',
   ! )
   subroutine iget_rtol( &
      instance_index, &
      rtol &
      )

      integer, intent(in) :: instance_index

      real(kind=8), intent(out) :: rtol
      ! Returning of rtol

      type(RK4Stepper), pointer :: instance

      call manager_get_instance(instance_index, instance)

      rtol = instance%rtol

   end subroutine iget_rtol

   subroutine iset_rtol( &
      instance_index, &
      rtol &
      )

      integer, intent(in) :: instance_index

      real(kind=8), intent(in) :: rtol
      ! Passing of rtol

      type(RK4Stepper), pointer :: instance

      call manager_get_instance(instance_index, instance)

      instance%rtol = rtol

   end subroutine iset_rtol

   ! Wrapping atol
   ! Strategy: WrappingStrategyDefault(
   !     magnitude_suffix='_m',
   ! )
   subroutine iget_atol( &
      instance_index, &
      atol &
      )

      integer, intent(in) :: instance_index

      real(kind=8), intent(out) :: atol
      ! Returning of atol

      type(RK4Stepper), pointer :: instance

      call manager_get_instance(instance_index, instance)

      atol = instance%atol

   end subroutine iget_atol

   subroutine iset_atol( &
      instance_index, &
      atol &
      )

      integer, intent(in) :: instance_index

      real(kind=8), intent(in) :: atol
      ! Passing of atol

      type(RK4Stepper), pointer :: instance

      call manager_get_instance(instance_index, instance)

      instance%atol = atol

   end subroutine iset_atol

   ! Wrapping h
   ! Strategy: WrappingStrategyDefault(
   !     magnitude_suffix='_m',
   ! )
   subroutine iget_h( &
      instance_index, &
      h &
      )

      integer, intent(in) :: instance_index

      real(kind=8), intent(out) :: h
      ! Returning of h

      type(RK4Stepper), pointer :: instance

      call manager_get_instance(instance_index, instance)

      h = instance%h

   end subroutine iget_h

   subroutine iset_h( &
      instance_index, &
      h &
      )

      integer, intent(in) :: instance_index

      real(kind=8), intent(in) :: h
      ! Passing of h

      type(RK4Stepper), pointer :: instance

      call manager_get_instance(instance_index, instance)

      instance%h = h

   end subroutine iset_h

end module fgen_rk_w
