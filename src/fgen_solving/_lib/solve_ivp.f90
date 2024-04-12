module fgen_solve_ivp
   ! Solving of initial-value problems (IVPs)

   use fgen_base_stepper, only: BaseStepper, integrable, Running, Failure, Finished
   use fgen_interpolation, only: linear_interpolation
   use fgen_time, only: TimeAxis
   use fgen_utils, only: searchsorted, is_monotonic

   implicit none
   private

   public :: IVPSolvingResult
   public :: solve_ivp

   type :: ParamsTEval
      ! Storage of parameters during solving

      real(kind=8) :: next_t_to_record
      real(kind=8) :: stepper_previous_t
      real(kind=8), dimension(:), allocatable :: stepper_previous_y
      integer :: previous_recorded_t_idx
      integer :: solved_until_t_idx
      integer :: stepper_steps

   contains

   end type ParamsTEval

   type :: IVPSolvingResult
      ! Solving result container

      logical :: success
      ! Was the solving completed successfully?

      integer :: stepper_steps
      ! How many steps did the stepper takes as part of the calculation?

      real(kind=8), dimension(:), allocatable :: t
      ! The time axis of the result

      real(kind=8), dimension(:, :), allocatable :: y
      ! The solved values
      !
      ! i.e. the solution y to the ODE dy/dt = f(y, t)
      ! which we have solved.

   contains

   end type IVPSolvingResult

contains

   subroutine solve_ivp( &
      stepper, &
      integrand, &
      t0, &
      y0, &
      t_eval, &
      result, &
      max_stepper_steps, &
      solve_until_end_time_bounds &
      )
      ! Solve initial-value problem (IVP)

      ! TODO: check if/why this has the target attribute
      class(BaseStepper), intent(inout) :: stepper
      ! Stepper to use to perform the integration

      procedure(integrable) :: integrand
      ! Function to integrate

      real(kind=8), intent(in) :: t0
      ! Initial time step

      real(kind=8), intent(in), dimension(:) :: y0
      ! Intial state at t=t0

      type(TimeAxis), intent(in) :: t_eval
      ! Timesteps to evalulate the IVP on
      !
      ! The result is linearly interpolated onto
      ! `t_eval % values` from the values at the time points the stepper
      ! actually visited.

      class(IVPSolvingResult), intent(out) :: result
      ! Container to hold the results

      integer, intent(in), optional:: max_stepper_steps
      ! Maximum number of steps that can be performed by the stepper
      !
      ! Default: 1e6

      logical, intent(in), optional :: solve_until_end_time_bounds
      ! Should we solve until the end of time % bounds?
      !
      ! Default: .true.
      !
      ! If .false. we only solve until the last value in t_eval % values
      !

      ! Note to developers:
      ! At the moment, this subroutine does two things: all the stepping/stopping logic
      ! and all of the writing of results into some output. As a result, it is relatively complicated.
      ! If you find yourself trying to alter or debug it, it may be worth refactoring it first to
      ! make it easier to test and handle. Specifically, it might be worth separating out the logic
      ! related to stepping the stepper from the logic related to putting (or not putting) the stepper's
      ! steps into the results. That could be done in a number of ways.

      integer :: a_max_stepper_steps
      logical :: a_solve_until_end_time_bounds

      logical :: inputs_valid
      type(ParamsTEval) :: params_t_eval
      integer :: y0_size
      logical :: reached_end_t_eval_values

      if (present(max_stepper_steps)) then
         a_max_stepper_steps = max_stepper_steps
      else
         ! Must be a bounded value
         a_max_stepper_steps = 1e6
      end if

      if (present(solve_until_end_time_bounds)) then
         a_solve_until_end_time_bounds = solve_until_end_time_bounds
      else
         a_solve_until_end_time_bounds = .true.
      end if

      inputs_valid = check_inputs(t0=t0, t_eval=t_eval)
      if (.not. inputs_valid) then
         result%success = .False.
         stepper%status = Failure
         return
      end if

      call prepare_to_solve_ivp( &
         integrand=integrand, &
         t0=t0, &
         y0=y0, &
         t_eval=t_eval, &
         stepper=stepper, &
         finished_flag=reached_end_t_eval_values, &
         y0_size=y0_size, &
         result=result, &
         params_t_eval=params_t_eval &
         )

      do while (stepper%status == Running)

         call do_stepper_step( &
            max_stepper_steps=a_max_stepper_steps, &
            params_t_eval=params_t_eval, &
            stepper=stepper &
            )

         if (stop_solving(stepper)) then
            ! Fast exit if any issue
            print *, "Stopping solving because an issue was raised in do_stepper_step"
            exit
         end if

         if (stepper%t < params_t_eval%next_t_to_record) then
            ! Nothing new to record, so we can cycle
            call prepare_next_cycle(params_t_eval=params_t_eval, stepper=stepper)
            cycle

         end if

         call record_stepper_step( &
            stepper=stepper, &
            t_eval=t_eval, &
            result=result, &
            params_t_eval=params_t_eval, &
            reached_end_t_eval_values=reached_end_t_eval_values &
            )

         if (.not. reached_end_t_eval_values) then

            call prepare_next_cycle(params_t_eval=params_t_eval, stepper=stepper)
            cycle

         end if

         if (a_solve_until_end_time_bounds) then

            call run_until_last_bound( &
               last_bound=t_eval%value_last_bound, &
               max_stepper_steps=a_max_stepper_steps, &
               params_t_eval=params_t_eval, &
               stepper=stepper &
               )

            ! Make sure that the stepper is not still running
            ! (if it is, solve_ivp has no signal to stop).
            if (stepper%status == Running) then
               print *, "How is the stepper still running?! Preventing infinite loop"
               error stop 1
            end if

         else
            ! All done
            stepper%status = Finished

         end if

      end do

      ! Finalise
      deallocate (params_t_eval%stepper_previous_y)
      result%stepper_steps = params_t_eval%stepper_steps
      result%success = stepper%status == Finished

   end subroutine solve_ivp

   function check_inputs( &
      t0, &
      t_eval &
      ) result(inputs_valid)
      ! Check that the inputs for solve_ivp are valid

      real(kind=8), intent(in) :: t0
      type(TimeAxis), intent(in) :: t_eval

      logical :: inputs_valid

      inputs_valid = .true.

      ! Only need to check against t_eval(1)
      ! as TimeAxis objects only support monotonic values
      if (t_eval%values(1) < t0) then
         print *, "t_eval % values(1) is before t0. t_eval % values(1)=", &
            t_eval%values(1), &
            " t0=", t0
         inputs_valid = .false.
      end if

   end function check_inputs

   subroutine prepare_to_solve_ivp( &
      integrand, &
      t0, &
      y0, &
      t_eval, &
      stepper, &
      finished_flag, &
      y0_size, &
      result, &
      params_t_eval &
      )
      ! Preparation/set-up steps for solve_ivp

      procedure(integrable) :: integrand
      real(kind=8), intent(in) :: t0
      real(kind=8), intent(in), dimension(:) :: y0
      type(TimeAxis), intent(in) :: t_eval
      class(BaseStepper), intent(inout) :: stepper
      logical, intent(out) :: finished_flag
      integer, intent(out) :: y0_size
      class(IVPSolvingResult), intent(out) :: result
      type(ParamsTEval), intent(out) :: params_t_eval

      call stepper%prepare(integrand, t0, y0)

      finished_flag = .false.
      y0_size = size(y0)

      allocate (result%t(size(t_eval%values)))
      allocate (result%y(size(result%t), y0_size))

      allocate (params_t_eval%stepper_previous_y(y0_size))

      ! Can hard-code first element as TimeAxis only supports monotonic values
      params_t_eval%next_t_to_record = t_eval%values(1)
      params_t_eval%previous_recorded_t_idx = 0
      params_t_eval%stepper_previous_t = t0
      params_t_eval%stepper_previous_y = y0
      params_t_eval%solved_until_t_idx = 0
      params_t_eval%stepper_steps = 0

   end subroutine prepare_to_solve_ivp

   subroutine do_stepper_step( &
      max_stepper_steps, &
      params_t_eval, &
      stepper &
      )
      integer, intent(in) :: max_stepper_steps
      type(ParamsTEval), intent(inout) :: params_t_eval
      class(BaseStepper), intent(inout) :: stepper

      integer :: current_stepper_steps

      current_stepper_steps = params_t_eval%stepper_steps

      if (current_stepper_steps >= max_stepper_steps) then
         ! Taking another step will lead to too many steps, so fast exit
         stepper%status = Failure
         return
      end if

      call stepper%step()
      ! Note that stepper step occured
      params_t_eval%stepper_steps = params_t_eval%stepper_steps + 1

      if (stop_solving(stepper)) then
         ! stepper issue, so should stop solving
         return
      end if

   end subroutine do_stepper_step

   function stop_solving(stepper) result(stop_solving_flag)

      class(BaseStepper), intent(in) :: stepper

      logical :: stop_solving_flag

      stop_solving_flag = stepper%status /= Running

   end function stop_solving

   subroutine prepare_next_cycle(params_t_eval, stepper)
      type(ParamsTEval), intent(inout) :: params_t_eval
      class(BaseStepper), intent(in) :: stepper

      ! Keep a note of the stepper's previous state
      ! so we can use it for interpolation if needed.
      params_t_eval%stepper_previous_t = stepper%t
      params_t_eval%stepper_previous_y = stepper%y

   end subroutine prepare_next_cycle

   subroutine record_stepper_step( &
      stepper, &
      t_eval, &
      result, &
      params_t_eval, &
      reached_end_t_eval_values &
      )
      ! Record timestep(s)
      !
      ! We are careful in case the stepper
      ! has taken a step big enough to go
      ! over more than one index in t_eval.

      class(BaseStepper), intent(in) :: stepper
      type(TimeAxis), intent(in) :: t_eval
      class(IVPSolvingResult), intent(inout) :: result
      type(ParamsTEval), intent(inout) :: params_t_eval
      logical, intent(inout) :: reached_end_t_eval_values

      integer :: latest_solved_idx
      integer :: first_unsolved_idx
      integer :: increment_idx
      integer :: i

      latest_solved_idx = params_t_eval%previous_recorded_t_idx
      ! We only search t_eval % values for times after ones we've already solved
      ! (this saves searching in values we know we don't care about).
      first_unsolved_idx = params_t_eval%previous_recorded_t_idx + 1
      increment_idx = searchsorted( &
                      t_eval%values(first_unsolved_idx:), &
                      stepper%t, &
                      exact=.false. &
                      )

      ! Offset by -1 because searchsorted returns the index
      ! where you could insert the value and preserve order.
      ! i.e. if stepper % t is less than all values in t_eval % values(first_unsolved_idx:),
      ! searchsorted returns 1, so the increment we want is zero
      ! because we haven't solved any additional steps
      ! (beyond ones we've already solved).
      ! Same logic applies for all searchsorted values greater than one.
      increment_idx = increment_idx - 1

      params_t_eval%solved_until_t_idx = latest_solved_idx + increment_idx

      do i = first_unsolved_idx, params_t_eval%solved_until_t_idx, 1
         ! Linearly interpolate all the values
         ! at points in t_eval % values
         ! we have now solved.
         result%t(i) = t_eval%values(i)
         result%y(i, :) = linear_interpolation( &
                          x1=params_t_eval%stepper_previous_t, &
                          y1=params_t_eval%stepper_previous_y, &
                          x2=stepper%t, &
                          y2=stepper%y, &
                          xout=t_eval%values(i) &
                          )

      end do

      if (params_t_eval%solved_until_t_idx < size(t_eval%values)) then
         ! Record where we're up to, i.e. prepare for next step
         params_t_eval%next_t_to_record = t_eval%values(params_t_eval%solved_until_t_idx + 1)
         params_t_eval%previous_recorded_t_idx = params_t_eval%solved_until_t_idx

      else

         reached_end_t_eval_values = .true.

      end if

   end subroutine record_stepper_step

   subroutine run_until_last_bound( &
      last_bound, &
      max_stepper_steps, &
      params_t_eval, &
      stepper &
      )
      real(kind=8), intent(in) :: last_bound
      integer, intent(in) :: max_stepper_steps
      type(ParamsTEval), intent(inout) :: params_t_eval
      class(BaseStepper), intent(inout) :: stepper

      do while (.not. stepper%t >= last_bound)

         call do_stepper_step( &
            max_stepper_steps=max_stepper_steps, &
            params_t_eval=params_t_eval, &
            stepper=stepper &
            )

         if (stop_solving(stepper)) then
            return
         end if

      end do

      stepper%y = linear_interpolation( &
                  x1=params_t_eval%stepper_previous_t, &
                  y1=params_t_eval%stepper_previous_y, &
                  x2=stepper%t, &
                  y2=stepper%y, &
                  xout=last_bound &
                  )
      stepper%t = last_bound
      stepper%status = Finished

   end subroutine run_until_last_bound

end module fgen_solve_ivp
