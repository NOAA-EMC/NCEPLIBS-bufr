# This is the main CMake file for NCEPLIBS-bufr.
#
# Mark Potts, Kyle Gerheiser, Rahul Mahajan, Ed Hartnett
cmake_minimum_required(VERSION 3.15)

file(STRINGS "VERSION" pVersion)

project(
    bufr
    VERSION ${pVersion}
    LANGUAGES C Fortran)

if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE
      "Release"
      CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
                                               "MinSizeRel" "RelWithDebInfo")
endif()

if(NOT CMAKE_C_COMPILER_ID MATCHES "^(Intel|GNU|Clang|AppleClang)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_C_COMPILER_ID}")
endif()

if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|GNU)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_Fortran_COMPILER_ID}")
endif()

include(GNUInstallDirs)

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

option(ENABLE_TESTS "Enable Tests" OFF)

# Determine the kinds upfront (both src/ and test/ need them).
# Both GNU and Intel support DYNAMIC_ALLOCATION
list(APPEND kinds "4_DA" "8_DA" "d_DA")
# Only Intel supports STATIC_ALLOCATION
if(CMAKE_C_COMPILER_ID MATCHES "^(Intel)$")
  list(APPEND kinds "4" "8" "d")
endif()

# Set common to the package compiler flags
if(CMAKE_C_COMPILER_ID MATCHES "^(GNU)$")
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
endif()

# For gfortran-10+ backward compatibility
if(${CMAKE_Fortran_COMPILER_ID} MATCHES "^(GNU)$" AND ${CMAKE_Fortran_COMPILER_VERSION} VERSION_GREATER_EQUAL 10)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -w -fallow-argument-mismatch -fallow-invalid-boz")
endif()

set(fortran_8_DA_flags ${fortran_8_flags})
set(fortran_d_DA_flags ${fortran_d_flags})

add_subdirectory(src)
add_subdirectory(utils)

if(ENABLE_TESTS)
  enable_testing()
  add_subdirectory(test)
endif()

# Determine whether or not to generate documentation.
option(ENABLE_DOCS "Enable generation of doxygen-based documentation." OFF)
if(ENABLE_DOCS)
  find_package(Doxygen REQUIRED)
endif()
add_subdirectory(docs)  
