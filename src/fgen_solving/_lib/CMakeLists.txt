# ~~~
# The name of the extension modules to build, excluding file extensions.
# It is required that the yaml and the f90 file share the same name and are in the same folder
# (i.e. they only differ by file extension).
# fgen will raise an error if this isn't the case.
# ~~~
set(
  EXTENSION_MODULES
  fgen_1d_handling_options
  fgen_euler_forward
  fgen_rk
  fgen_time
  fgen_timeseries
  fgen_values_bounded
)

# Helper variables to shorten the file definitions
set(py_project_directory ${CMAKE_CURRENT_SOURCE_DIR}/src/fgen_solving)
set(extension_directory ${py_project_directory}/_lib)

foreach(module ${EXTENSION_MODULES})
  # ~~~
  # Run fgen generate on a module.
  # This will only be run if the YAML/fortran files changes.
  # TODO: this currently ignores the fact that a python file is generated as
  # it doesn't play a role in the build process
  # ~~~

  # cmake-format: off
  add_custom_command(
    OUTPUT "${extension_directory}/${module}_manager.f90"
           "${extension_directory}/${module}_wrapped.f90"
    COMMAND fgen generate -f ${extension_directory}/${module}.yaml
      --extension fgen_solving._lib
      --wrapper-directory ${extension_directory}/
      --python-directory ${py_project_directory}/
    DEPENDS
             "${extension_directory}/${module}.yaml"
    VERBATIM
  )
  # cmake-format: on

  list(
    APPEND
    ANCILLARY_FORTRAN_SOURCES
    "${extension_directory}/${module}_manager.f90"
  )
  list(
    APPEND
    WRAPPED_FORTRAN_SOURCES
    "${extension_directory}/${module}_wrapped.f90"
  )
endforeach()

# Additional sources needed to support the wrappers
list(
  APPEND
  ANCILLARY_FORTRAN_SOURCES
  ${extension_directory}/solve_ivp.f90
  ${extension_directory}/time.f90
  ${extension_directory}/timeseries.f90
  ${extension_directory}/values_bounded.f90
  ${extension_directory}/1d_handling/differentiation.f90
  ${extension_directory}/1d_handling/get_boundary_value.f90
  ${extension_directory}/1d_handling/integration.f90
  ${extension_directory}/1d_handling/interpolation.f90
  ${extension_directory}/1d_handling/interpolation_base.f90
  ${extension_directory}/1d_handling/linear_spline.f90
  ${extension_directory}/1d_handling/next.f90
  ${extension_directory}/1d_handling/options.f90
  ${extension_directory}/1d_handling/previous.f90
  ${extension_directory}/steppers/base_stepper.f90
  ${extension_directory}/steppers/euler_forward.f90
  ${extension_directory}/steppers/rk4.f90
)
