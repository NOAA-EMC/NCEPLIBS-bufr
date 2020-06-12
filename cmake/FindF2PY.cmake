# MAT This code is based, loosely, on the FindF2PY.cmake file from scikit
#.rst:
#
# The purpose of the F2PY –Fortran to Python interface generator– project is to provide a
# connection between Python and Fortran languages.
#
# F2PY is a Python package (with a command line tool f2py and a module f2py2e) that facilitates
# creating/building Python C/API extension modules that make it possible to call Fortran 77/90/95
# external subroutines and Fortran 90/95 module subroutines as well as C functions; to access Fortran
# 77 COMMON blocks and Fortran 90/95 module data, including allocatable arrays from Python.
#
# For more information on the F2PY project, see http://www.f2py.com/.
#
# The following variables are defined:
#
# ::
#
#   F2PY_EXECUTABLE      - absolute path to the F2PY executable
#
# ::
#
#   F2PY_VERSION_STRING  - the version of F2PY found
#   F2PY_VERSION_MAJOR   - the F2PY major version
#   F2PY_VERSION_MINOR   - the F2PY minor version
#   F2PY_VERSION_PATCH   - the F2PY patch version
#
#
# .. note::
#
#   By default, the module finds the F2PY program associated with the installed NumPy package.
#

# Path to the f2py executable
find_package(Python COMPONENTS Interpreter)

find_program(F2PY_EXECUTABLE NAMES "f2py${Python_VERSION_MAJOR}.${Python_VERSION_MINOR}"
                                   "f2py-${Python_VERSION_MAJOR}.${Python_VERSION_MINOR}"
                                   "f2py${Python_VERSION_MAJOR}"
                                   "f2py"
                                   )

if(F2PY_EXECUTABLE)
   # extract the version string
   execute_process(COMMAND "${F2PY_EXECUTABLE}" -v
                     OUTPUT_VARIABLE F2PY_VERSION_STRING
                     OUTPUT_STRIP_TRAILING_WHITESPACE)
   if("${F2PY_VERSION_STRING}" MATCHES "^([0-9]+)(.([0-9+]))?(.([0-9+]))?$")
      set(F2PY_VERSION_MAJOR "${CMAKE_MATCH_1}")
      set(F2PY_VERSION_MINOR "${CMAKE_MATCH_3}")
      set(F2PY_VERSION_PATCH "${CMAKE_MATCH_5}")
   endif()

   # Now we need to test if we can actually use f2py and what its suffix is

   include(try_f2py_compile)
   try_f2py_compile(
      ${CMAKE_CURRENT_LIST_DIR}/check_compiler_support/test.F90
      DETECT_F2PY_SUFFIX
      )

endif ()

# handle the QUIET and REQUIRED arguments and set F2PY_FOUND to TRUE if
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(F2PY
   REQUIRED_VARS F2PY_EXECUTABLE F2PY_SUFFIX
   VERSION_VAR F2PY_VERSION_STRING
   )

mark_as_advanced(F2PY_EXECUTABLE F2PY_SUFFIX)

if (F2PY_FOUND)
   include(UseF2Py)
endif ()
