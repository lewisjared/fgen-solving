module fgen_rk

   use fgen_base_stepper, only: BaseStepper, integrable, Finished

   implicit none
   private

   public RK4Stepper

   type, extends(BaseStepper) :: RK4Stepper
      real(8) :: h
      ! Current step size
      real(8) :: atol
      ! Absolute tolerance for error when calculating step size
      real(8) :: rtol
      ! Relative tolerance for error when calculating step size
   contains

      procedure :: build
      procedure :: step
      procedure :: prepare_impl
      procedure :: finalize

   end type RK4Stepper

   interface RK4Stepper
      module procedure :: constructor

   end interface RK4Stepper

contains

   function constructor(rtol, atol, h) result(self)

      real(kind=8), intent(in) :: rtol, atol
      real(kind=8), intent(in), optional :: h

      type(RK4Stepper) :: self

      call self%build(rtol=rtol, atol=atol, h=h)

   end function constructor

   subroutine build(self, rtol, atol, h)

      class(RK4Stepper), intent(inout) :: self

      real(kind=8), intent(in) :: rtol, atol
      ! relative and absolute tolerances
      ! Used to determine if a step is accepted or rejected (not currently implemented)

      real(kind=8), intent(in), optional :: h
      ! Selected step size
      ! If not provided, the optimal step size will be determined at runtime using an algorithm described
      ! in Hairer et al. See the `select_initial_step` function

      self%rtol = rtol
      self%atol = atol

      if (present(h)) then
         self%h = h
      else
         ! This will be updated with a quasi-optimal value
         ! during the preperation phase of the stepper
         self%h = 0D0
      end if

   end subroutine build

   subroutine prepare_impl(self, integrand, t0, y0)
      procedure(integrable) :: integrand
      real(kind=8), intent(in) :: t0
      ! Time at which the initial state, `y0`, applies
      real(kind=8), dimension(:), intent(in) :: y0
      ! Intial state

      class(RK4Stepper), intent(inout) :: self

      ! For invalid step sizes (value less than on equal to zero) determine the initial step size heuristically
      if (self%h <= 0D0) then
         self%h = select_initial_step(integrand, t0, y0, integrand(t0, y0), self%rtol, self%atol)
      end if
   end subroutine prepare_impl

   function norm(x)
      ! Calculate RMS norm of a vector
      real(8), dimension(:), intent(in) :: x
      real(8) :: norm

      norm = NORM2(x)/(size(x)**0.5)
   end function norm

   function select_initial_step(integrand, t0, y0, dydt0, rtol, atol) result(initial_step)
      ! Select a good initial step
      !
      ! Described in E. Hairer, S. P. Norsett G. Wanner, "Solving Ordinary Differential
      !           Equations I: Nonstiff Problems", Sec. II.4.
      ! Hairer et al used a few magic constants in in this function. The named constants below are our interpretation
      ! of what they mean.
      procedure(integrable) :: integrand
      real(kind=8), intent(in) :: t0, rtol, atol
      real(kind=8), dimension(:), intent(in) :: y0, dydt0

      real(8), dimension(size(y0)) :: scale, y1, dydt1
      real(8) :: h0, h1, d0, d1, d2, initial_step
      integer:: order = 4 ! RK45

      real(kind=8), parameter :: numerical_error_min_step_size = 1e-6
      real(kind=8), parameter :: initial_magnitude_size_fraction = 0.01
      real(kind=8), parameter :: second_derivative_size_fraction = 0.01
      real(kind=8), parameter :: second_derivative_rel_initial_guess_size_fraction = 1e-3
      real(kind=8), parameter :: first_derivative_factor = 100

      scale = atol + abs(y0)*rtol

      d0 = norm(y0/scale)
      d1 = norm(dydt0/scale)
      if (d0 < 1e-5 .or. d1 < 1e-5) then
         h0 = numerical_error_min_step_size
      else
         h0 = initial_magnitude_size_fraction*d0/d1
      end if

      y1 = y0 + h0*dydt0
      dydt1 = integrand(t0 + h0, y1)
      d2 = norm((dydt1 - dydt0)/scale)/h0

      if (d1 <= 1e-15 .and. d2 <= 1e-15) then
         h1 = max(numerical_error_min_step_size, h0*second_derivative_rel_initial_guess_size_fraction)
      else
         h1 = (second_derivative_size_fraction/max(d1, d2))**(1D0/(order + 1D0))
      end if

      initial_step = min(first_derivative_factor*h0, h1)
   end function select_initial_step

   subroutine step(self)
      class(RK4Stepper), intent(inout) :: self

      real(8), dimension(size(self%y)) :: y_new

      real(kind=8) :: h, t_new
      logical :: step_accepted

      step_accepted = .FALSE.
      h = self%h

      do while (.not. step_accepted)
         t_new = self%t + h

         y_new = rk4(self%t, self%y, self%integrand, h)

         ! naive stepping where we don't change the step size
         step_accepted = .TRUE.
      end do

      self%t = t_new
      self%y = y_new
      self%h = h

   end subroutine step

   function rk4(t, y, integrand, h) result(y_out)
      ! Given values for the variables y(1:n), current time t and an integrand of form dy / dt = f(y, t), use the
      ! fourth-order Runge-Kutta method to advance the solution over an interval h and return y(t + h)
      real(kind=8), intent(in) :: h, t, y(:)
      procedure(integrable) :: integrand
      real(kind=8) :: y_out(size(y))

      real(kind=8) :: hh
      real(kind=8), dimension(size(y)) :: k1, k2, k3, k4

      hh = h*0.5

      k1 = integrand(t, y)
      k2 = integrand(t + hh, y + hh*k1)
      k3 = integrand(t + hh, y + hh*k2)
      k4 = integrand(t + h, y + h*k3)

      ! Accumulate increments with proper weights.
      y_out = y + h/6*(k1 + 2*k2 + 2*k3 + k4)
   end function rk4

   subroutine finalize(self)

      class(RK4Stepper), intent(inout) :: self

      if (associated(self%integrand)) then
         nullify (self%integrand)
      end if

   end subroutine finalize

end module fgen_rk
