module fgen_euler_forward
   ! Implementation of a stepper that follow's the Euler forward step method
   !
   ! This is actually a special case of Runge-Kutta, but implementing it like
   ! this is way easier to understand, the general solution can wait.

   use fgen_base_stepper, only: BaseStepper, integrable, Finished

   implicit none
   private

   public EulerForwardStepper

   type, extends(BaseStepper) :: EulerForwardStepper
      ! Stepper which uses the Euler forward method

      real(8) :: step_size
      ! Step size to use when solving
      !
      ! This is called `h` in many implementations and textbooks.

   contains

      procedure :: build
      procedure :: step
      procedure :: prepare_impl
      procedure :: finalize

   end type EulerForwardStepper

   interface EulerForwardStepper
      module procedure :: constructor
   end interface EulerForwardStepper

contains

   function constructor(step_size) result(self)

      real(kind=8), optional :: step_size

      type(EulerForwardStepper) :: self

      call self%build(step_size=step_size)

   end function constructor

   subroutine build(self, step_size)

      class(EulerForwardStepper), intent(inout) :: self

      real(kind=8), optional :: step_size

      real(kind=8) :: a_step_size

      if (present(step_size)) then
         a_step_size = step_size
      else
         a_step_size = 1.0D0
      end if

      self%step_size = a_step_size

   end subroutine build

   subroutine prepare_impl(self, integrand, t0, y0)
      procedure(integrable) :: integrand
      real(kind=8), intent(in) :: t0
      ! Time at which the initial state, `y0`, applies
      real(kind=8), dimension(:), intent(in) :: y0
      ! Intial state

      class(EulerForwardStepper), intent(inout) :: self

      ! Don't need to do anything

   end subroutine prepare_impl

   subroutine step(self)
      class(EulerForwardStepper), intent(inout) :: self

      real(kind=8), dimension(size(self%y)) :: y_new
      real(kind=8) :: t_new

      y_new = self%y + self%integrand(self%t, self%y)*self%step_size
      t_new = self%t + self%step_size

      self%t = t_new
      self%y = y_new

   end subroutine step

   subroutine finalize(self)

      class(EulerForwardStepper), intent(inout) :: self

      if (associated(self%integrand)) then
         nullify (self%integrand)
      end if

   end subroutine finalize

end module fgen_euler_forward
