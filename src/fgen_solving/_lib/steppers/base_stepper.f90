module fgen_base_stepper

   use fgen_base_finalizable, only: BaseFinalizable

   implicit none
   private

   public :: BaseStepper, integrable, Running, Failure, Finished

   abstract interface
      function integrable(t, y)
         ! A function that can be integrated
         ! Represents the time derivative of the state `y` at time `t`
         real(kind=8), intent(in) :: t
         real(kind=8), intent(in), dimension(:) :: y
         real(kind=8), dimension(:), allocatable :: integrable
      end function
   end interface

   enum, bind(c)
      enumerator :: Running = 1
      enumerator :: Failure = 2
      enumerator :: Finished = 3
   end enum

   type, abstract, extends(BaseFinalizable) :: BaseStepper
      ! Base class for a stepper that can be used as part of solving ODEs.
      !
      ! This is based on scipy's ODESolver class
      ! (scipy.integrate._ivp.base.ODESolver).
      ! A concrete stepper must implement the `step` and `prepare_impl` routines.
      !
      ! The stepper holds onto its own state.
      ! We would prefer not to have an object that holds its own state.
      ! However, making the state an attribute of the stepper
      ! allows the stepper to optimise in a number of ways.
      ! As a result, we tradeoff speed for comprehensibility
      ! and our ability to debug.

      procedure(integrable), pointer, nopass :: integrand
      ! Function that represents the time derivative of the state `y` at time `t` (
      !
      ! func(t, y) = dy/dt evaluated at time=t

      real(kind=8) :: t
      ! Current time.
      !
      ! Will be unset if no steps have been made.

      real(kind=8), dimension(:), allocatable :: y
      ! Current state of the stepper

      integer :: status

   contains
      private
      procedure, public :: prepare

      procedure(derived_type_prepare), private, deferred :: prepare_impl
      ! Stepper-specific preparation functionality
      !
      ! This routine could be a concrete routine which does nothing.

      procedure(derived_type_step), public, deferred :: step
      ! Step the stepper forward by a single step

   end type BaseStepper

   interface
      subroutine derived_type_step(self)
         import :: BaseStepper

         class(BaseStepper), intent(inout) :: self

      end subroutine derived_type_step

      subroutine derived_type_prepare(self, integrand, t0, y0)
         import :: BaseStepper

         procedure(integrable) :: integrand
         real(kind=8), intent(in) :: t0
         real(kind=8), dimension(:), intent(in) :: y0

         class(BaseStepper), intent(inout) :: self

      end subroutine derived_type_prepare

   end interface

contains

   subroutine prepare(self, integrand, t0, y0)
      ! Prepare a stepper to begin solving an integrand

      procedure(integrable) :: integrand
      ! Integrand to integrate

      real(kind=8), intent(in) :: t0
      ! Starting time

      real(kind=8), dimension(:), intent(in) :: y0
      ! State at t=t0 i.e. initial state

      class(BaseStepper), intent(inout) :: self

      self%status = Running
      self%integrand => integrand
      self%y = y0
      self%t = t0

      call self%prepare_impl(integrand, t0, y0)

   end subroutine prepare

end module fgen_base_stepper
