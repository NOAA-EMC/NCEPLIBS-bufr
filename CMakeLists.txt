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

include(GNUInstallDirs)

option(ENABLE_TESTS "Enable Tests" ON)

# Determine the kinds upfront (both src/ and test/ need them).
# Both GNU and Intel support DYNAMIC_ALLOCATION
list(APPEND kinds "4_DA" "8_DA" "d_DA")
# Only Intel supports STATIC_ALLOCATION
if(CMAKE_C_COMPILER_ID MATCHES "^(Intel)$")
  list(APPEND kinds "4" "8" "d")
endif()

add_subdirectory(src)

if(ENABLE_TESTS)
  enable_testing()
  add_subdirectory(test)
endif()
