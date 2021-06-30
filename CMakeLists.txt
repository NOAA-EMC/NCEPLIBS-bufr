# This is the main CMake file for NCEPLIBS-bufr.
#
# Mark Potts, Kyle Gerheiser, Rahul Mahajan, Ed Hartnett, Jeff Ator
cmake_minimum_required(VERSION 3.15)

file(STRINGS "VERSION" pVersion)

project(
    bufr
    VERSION ${pVersion}
    LANGUAGES C Fortran)

# Handle user options.
option(ENABLE_DOCS "Enable generation of doxygen-based documentation." OFF)
option(ENABLE_PYTHON "Enable building python module 'ncepbufr'" OFF)
option(BUILD_SHARED_LIBS "Enable building shared libraries" OFF)

set(MASTER_TABLE_DIR "${CMAKE_INSTALL_PREFIX}/tables" CACHE STRING "Installation location of Master BUFR Tables")

if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE
      "Release"
      CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
                                               "MinSizeRel" "RelWithDebInfo")
endif()

include(GNUInstallDirs)
enable_testing()

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

if(ENABLE_PYTHON)
  if(APPLE)
    set(Python3_FIND_FRAMEWORK "LAST")
  endif()
  find_package(Python3 REQUIRED COMPONENTS Interpreter)
endif()

if(APPLE)
  # The linker on macOS does not include `common symbols` by default
  # Passing the -c flag includes them and fixes an error with undefined symbols
  set(CMAKE_Fortran_ARCHIVE_FINISH "<CMAKE_RANLIB> -c <TARGET>")
endif()

# Determine the kinds upfront (both src/ and test/ need them).
list(APPEND kinds "4_DA" "8_DA" "d_DA")
list(APPEND kinds "4" "8" "d")

# Set common to the package compiler flags
if(CMAKE_C_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_C_FLAGS_DEBUG "-O0")
elseif(CMAKE_C_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_C_FLAGS_DEBUG "-ggdb -Wall -O0")
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(fortran_8_flags "-r8 -i8")
  set(fortran_d_flags "-r8")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -traceback")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(fortran_8_flags "-fdefault-real-8 -fdefault-integer-8")
  set(fortran_d_flags "-fdefault-real-8")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -fbacktrace")
  set(CMAKE_Fortran_FLAGS_DEBUG "-ggdb -Wall -O0")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
  add_compile_definitions("GNU=1")
endif()

set(fortran_8_DA_flags ${fortran_8_flags})
set(fortran_d_DA_flags ${fortran_d_flags})

set(underscore_def "UNDERSCORE")
set(c_8_defs "F77_INTSIZE_8")
set(c_8_DA_defs ${c_8_defs})

# For gfortran-10+ backward compatibility
if(${CMAKE_Fortran_COMPILER_ID} MATCHES "^(GNU)$" AND ${CMAKE_Fortran_COMPILER_VERSION} VERSION_GREATER_EQUAL 10)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -w -fallow-argument-mismatch -fallow-invalid-boz")
endif()

add_subdirectory(src)
add_subdirectory(utils)

if(ENABLE_PYTHON)
  add_subdirectory(python)
endif()

add_subdirectory(tables)

add_subdirectory(test)

# Generate doxygen documentation if desired.
if(ENABLE_DOCS)
  find_package(Doxygen REQUIRED)
  add_subdirectory(docs)
endif()
