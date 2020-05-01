# Most users of this software do not (should not?) have permissions to
# install in the cmake default of /usr/local (or equiv on other os's).
# Below, the default is changed to a directory within the build tree
# unless the user explicitly sets CMAKE_INSTALL_PREFIX in the cache.
if (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    set (CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}/install" CACHE PATH "default install path" FORCE )
    message(STATUS "*** Setting default install prefix to ${CMAKE_INSTALL_PREFIX}.")
    message(STATUS "*** Override with -DCMAKE_INSTALL_PREFIX=<path>.")
endif()

# Bring in ecbuild
list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/@ecbuild/cmake")
#set (BUILD_SHARED_LIBS OFF)
option(BUILD_SHARED_LIBS "Build the shared library" OFF)
set (ECBUILD_2_COMPAT_VALUE OFF)
include (ecbuild_system NO_POLICY_SCOPE)


list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/check_compiler_support")
include ("${CMAKE_Fortran_COMPILER_ID}")
include (check_fortran_support)
include (esma_check_if_debug)
include (esma_set_this)
include (esma_add_subdirectories)
include (esma_add_library)
include (esma_generate_automatic_code)
include (esma_create_stub_component)
include (esma_fortran_generator_list)

find_package(ImageMagick)
if (NOT ImageMagick_FOUND)
   message(STATUS "NOTE: ImageMagick was not found. This will prevent using LaTeX and some postprocessing utilities from running, but does not affect the build")
endif ()

find_package(LATEX)
# These are all the bits of LaTeX that UseLatex needs. As it's confusing
# how LATEX_FOUND from find_package(LATEX) is set, we test all the bits
# that UseLatex requires
#
# Also, UseLatex assumes ImageMagick is installed. While this is always
# nice (and technically required to generate plots with GEOS plotting 
# utilities, it's not necessary to *build*
if (LATEX_FOUND AND LATEX_PDFLATEX_FOUND AND LATEX_BIBTEX_FOUND AND LATEX_MAKEINDEX_FOUND AND ImageMagick_FOUND)
   # If they are all found, set LATEX_FOUND to TRUE...
   set (LATEX_FOUND TRUE)

   # ...and then set up for protex and UseLatex
   include (UseProTeX)
   set (protex_flags -g -b -f)

   set (LATEX_COMPILER pdflatex)
   include (UseLatex)
else ()
   set (LATEX_FOUND FALSE)
endif ()

if (APPLE)
  include(osx_extras)
endif ()

# OpenMP support
find_package (OpenMP)

# Threading support
set(CMAKE_THREAD_PREFER_PTHREAD TRUE)
set(THREADS_PREFER_PTHREAD_FLAG TRUE)
find_package(Threads REQUIRED)

# Position independent code
set(CMAKE_POSITION_INDEPENDENT_CODE ON)


# MPI Support - only invoked from Fortran sources in GEOS-5.
# But some BASEDIR packages use MPI from C/C++.
set(MPI_DETERMINE_LIBRARY_VERSION TRUE)
find_package (MPI REQUIRED)

if (APPLE)
  if (DEFINED ENV{MKLROOT})
    set (MKL_Fortran)
    find_package (MKL REQUIRED)
  else ()
    if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
      #USE FRAMEWORK
      message(STATUS "Found macOS and gfortran, using framework Accelerate")
      link_libraries("-framework Accelerate")
    endif ()
  endif ()
else ()
  find_package (MKL REQUIRED)
endif ()

option (ESMA_ALLOW_DEPRECATED "suppress warnings about deprecated features" ON)

# Baselibs ...
include (FindBaselibs)

enable_testing()
set (CMAKE_INSTALL_MESSAGE LAZY)

# This is a "stub" macro to detect building within an ESMA project (for MAPL standalone)
macro (esma)

endmacro ()

find_package(GitInfo)

find_package(F2PY)
