#[[.rst:
Find fgen
---------------

Makes the fgen project available.

Imported Targets
^^^^^^^^^^^^^^^^

This module provides the following imported target, if found:

``fgen::fgen``
  The fgen fortran library


Result Variables
^^^^^^^^^^^^^^^^

This module will define the following variables:

``FGEN_FOUND``
  True if the fgen library is available

``FGEN_SOURCE_DIR``
  Path to the source directory of the fgen project,
  only set if the project is included as source (i.e. via ``fetch`` or ``subproject`` methods).

``FGEN_BINARY_DIR``
  Path to the binary directory of the fgen project,
  only set if the project is included as source (i.e. via ``fetch`` or ``subproject`` methods).

Cache variables
^^^^^^^^^^^^^^^

The following cache variables may be set to influence the library detection:

``FGEN_FIND_METHOD``
  Methods to find or make the project available. Available methods are
  - ``cmake``: Try to find via CMake config file
  - ``subproject``: Use source in subprojects directory
  - ``fetch``: Fetch the source from the upstream gitlab repository

``FGEN_DIR``
  Used for searching the CMake config file

``FGEN_SUBPROJECT``
  Directory to find the fgen subproject, relative to the project root (defaults to "subprojects/fgen")

#]]

set(_lib "fgen")
set(_pkg "FGEN")
set(_url "https://gitlab.com/magicc/fgen")
set(_hash "9e67821488da3f66c87c3a47a93c8cfc4dfe0274")  # origin/remove-solving

if(NOT DEFINED "${_pkg}_FIND_METHOD")
  if(DEFINED "${PROJECT_NAME}-dependency-method")
    set("${_pkg}_FIND_METHOD" "${${PROJECT_NAME}-dependency-method}")
  else()
    set("${_pkg}_FIND_METHOD" "cmake" "subproject" "fetch")
  endif()
  set("_${_pkg}_FIND_METHOD")
endif()

foreach(method ${${_pkg}_FIND_METHOD})
  if(TARGET "${_lib}::${_lib}")
    break()
  endif()

  if("${method}" STREQUAL "cmake")
    message(STATUS "${_lib}: Find installed package")
    if(DEFINED "${_pkg}_DIR")
      set("_${_pkg}_DIR")
      set("${_lib}_DIR" "${_pkg}_DIR")
    endif()
    find_package("${_lib}" CONFIG)
    if("${_lib}_FOUND")
      message(STATUS "${_lib}: Found installed package")
      break()
    endif()
  endif()

  if("${method}" STREQUAL "subproject")
    if(NOT DEFINED "${_pkg}_SUBPROJECT")
      set("_${_pkg}_SUBPROJECT")
      set("${_pkg}_SUBPROJECT" "subprojects/${_lib}")
    endif()
    set("${_pkg}_SOURCE_DIR" "${PROJECT_SOURCE_DIR}/${${_pkg}_SUBPROJECT}")
    # This binary path is similar to that from FetchContent
    set("${_pkg}_BINARY_DIR" "${PROJECT_BINARY_DIR}/_deps/${_lib}-build")
    if(EXISTS "${${_pkg}_SOURCE_DIR}/CMakeLists.txt")
      message(STATUS "Include ${_lib} from ${${_pkg}_SUBPROJECT}")
      add_subdirectory(
              "${${_pkg}_SOURCE_DIR}"
              "${${_pkg}_BINARY_DIR}"
      )

      add_library("${_lib}::${_lib}" INTERFACE IMPORTED)
      target_link_libraries("${_lib}::${_lib}" INTERFACE "${_lib}")

      # We need the module directory in the subproject before we finish the configure stage
      if(NOT EXISTS "${${_pkg}_BINARY_DIR}/include")
        make_directory("${${_pkg}_BINARY_DIR}/include")
      endif()

      break()
    endif()
  endif()

  if("${method}" STREQUAL "fetch")
    message(STATUS "Retrieving ${_lib} from ${_url}")
    include(FetchContent)
    FetchContent_Declare(
            "${_lib}"
            GIT_REPOSITORY "${_url}"
            GIT_TAG "${_hash}"
    )
    FetchContent_MakeAvailable("${_lib}")

    add_library("${_lib}::${_lib}" INTERFACE IMPORTED)
    target_link_libraries("${_lib}::${_lib}" INTERFACE "${_lib}")

    # We need the module directory in the subproject before we finish the configure stage
    FetchContent_GetProperties("${_lib}" SOURCE_DIR "${_pkg}_SOURCE_DIR")
    FetchContent_GetProperties("${_lib}" BINARY_DIR "${_pkg}_BINARY_DIR")
    if(NOT EXISTS "${${_pkg}_BINARY_DIR}/include")
      make_directory("${${_pkg}_BINARY_DIR}/include")
    endif()

    break()
  endif()

endforeach()

if(TARGET "${_lib}::${_lib}")
  set("${_pkg}_FOUND" TRUE)
else()
  set("${_pkg}_FOUND" FALSE)
endif()

if(DEFINED "_${_pkg}_SUBPROJECT")
  unset("${_pkg}_SUBPROJECT")
  unset("_${_pkg}_SUBPROJECT")
endif()
if(DEFINED "_${_pkg}_DIR")
  unset("${_lib}_DIR")
  unset("_${_pkg}_DIR")
endif()
if(DEFINED "_${_pkg}_FIND_METHOD")
  unset("${_pkg}_FIND_METHOD")
  unset("_${_pkg}_FIND_METHOD")
endif()
unset(_lib)
unset(_pkg)
unset(_url)
