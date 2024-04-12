module fgen_1d_handling_options
   ! 1D handling options
   !
   ! This module holds the options for our one-dimensional data handling.
   ! These options define what to do when performing operations
   ! (like interpolation, integration and differentiation)
   ! that require assumptions about how to go from discrete data
   ! to continuous data.
   !
   ! Underpinning this module is the idea that we are working with data
   ! where the x-axis is a time axis.
   ! That is why you will see time rather than x throughout this module.
   ! This assumption allows us to assume that the x-/time-axis
   ! is stricly monotonically increasing,
   ! which simplifies a lot of our processing and logic
   ! (and means we don't have to check for this strict montonicity
   ! before every operation).
   !
   ! Each option can be combined with each operation.
   ! For example, a linear spline can be combined with interpolation.
   ! The implementations live in in the same folder as this file,
   ! in files named `<option>.f90`, for example, `linear_spline.f90`.
   ! The files named `<operation>.f90`, provide uniform helper functions
   ! to perform these operations for different handling options
   ! (without having to import all the entire underlying implementation modules).
   !
   ! The operations are designed to operate on
   ! low-level data structures (pure arrays, for example).
   ! Higher-level objects, such as :obj:`libfgen.fgen_timeseries.Timeseries`
   ! should wrap these lower-level operations
   ! both to preserve the flexibility of these lower-level operations
   ! but also to avoid circular dependencies.
   !
   ! For further information on the assumptions
   ! which underpin these modules,
   ! see the docstring at the top of each `<option>.f90` file.

   implicit none
   private

   ! Note: `OneDimensionalHandlingOption` starts with One
   ! because it can't start with 1
   ! (so we can't call it `1DHandlingOption`)
   public :: OneDimensionalHandlingOption
   public :: CubicSpline
   public :: LinearSpline
   public :: Next
   public :: NotSpecified
   public :: Previous
   public :: QuadraticSpline

   enum, bind(c)
      enumerator :: OneDimensionalHandlingOption = 0
      enumerator :: NotSpecified = -1
      enumerator :: LinearSpline = 1
      enumerator :: QuadraticSpline = 2
      enumerator :: CubicSpline = 3
      enumerator :: Previous = 10
      enumerator :: Next = 11
   end enum

contains

end module fgen_1d_handling_options
