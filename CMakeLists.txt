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
option(BUILD_UTILS "Enable building utilities" ON)
option(BUILD_TESTING "Enable automated testing" ON)

# Developers can use this option to specify a local directory which
# holds the test files. They will be copied instead of fetching the
# files via FTP.
SET(TEST_FILE_DIR "." CACHE STRING "Check this directory for test files before using FTP.")
message(STATUS "Finding test data files in directory ${TEST_FILE_DIR}.")

set(MASTER_TABLE_DIR "${CMAKE_INSTALL_PREFIX}/tables" CACHE STRING "Installation location of Master BUFR Tables")

string(LENGTH "${MASTER_TABLE_DIR}" _lenmtd)
set(_lenslmax 60)  # max number of chars of ${MASTER_TABLE_DIR} to store on a single line of a Fortran90 or C source code file
if(${_lenmtd} LESS_EQUAL ${_lenslmax})
  set(MASTER_TABLE_DIR_F "${MASTER_TABLE_DIR}")
  set(MASTER_TABLE_DIR_C "${MASTER_TABLE_DIR}")
else() # generate source code lines in chunks of ${_lenslmax} chars each
  set(_mtd_f "")
  set(_mtd_c "")
  set(_len2go ${_lenmtd})
  set(_start 0)
  while(${_len2go} GREATER ${_lenslmax})
    string(SUBSTRING "${MASTER_TABLE_DIR}" ${_start} ${_lenslmax} _chunk)
    string(APPEND _mtd_f "${_chunk}" "' // &\n'")
    string(APPEND _mtd_c "${_chunk}" "\" \n\t\t\"")
    math(EXPR _len2go "${_len2go} - ${_lenslmax}")
    math(EXPR _start "${_start} + ${_lenslmax}")
  endwhile()
  string(SUBSTRING "${MASTER_TABLE_DIR}" ${_start} -1 _chunk)
  string(APPEND _mtd_f "${_chunk}")
  string(APPEND _mtd_c "${_chunk}")
  set(MASTER_TABLE_DIR_F "${_mtd_f}")
  set(MASTER_TABLE_DIR_C "${_mtd_c}")
endif()

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

if(APPLE AND NOT "${CMAKE_RANLIB}" MATCHES "^.*(llvm-ranlib)$")
  # The linker on macOS does not include `common symbols` by default
  # Passing the -c flag includes them and fixes an error with undefined symbols
  set(CMAKE_Fortran_ARCHIVE_FINISH "<CMAKE_RANLIB> -c <TARGET>")
  set(CMAKE_C_ARCHIVE_FINISH "<CMAKE_RANLIB> -c <TARGET>")
endif()

# Only build the _4 version of the library
list(APPEND kinds "4")

# Set common to the package compiler flags
if(CMAKE_C_COMPILER_ID MATCHES "^(Intel|IntelLLVM)$")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -traceback")
  set(CMAKE_C_FLAGS_DEBUG "-O0")
  set(CMAKE_C_FLAGS_RELEASE "-O3")
elseif(CMAKE_C_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g")
  set(CMAKE_C_FLAGS_DEBUG "-ggdb -Wall -O0")
  set(CMAKE_C_FLAGS_RELEASE "-O3")
endif()

# Even if we only build _4 version of the library, we still need the _8 and _d flags when building the test codes
if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|IntelLLVM)$")
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

add_subdirectory(src)

if(BUILD_UTILS)
  add_subdirectory(utils)
endif()

if(ENABLE_PYTHON)
  add_subdirectory(python)
endif()

add_subdirectory(tables)

# Turn on unit testing.
if(BUILD_TESTING)
  include(CTest)
  add_subdirectory(test)
endif()

# Generate doxygen documentation if desired.
if(ENABLE_DOCS)
  find_package(Doxygen REQUIRED)
  add_subdirectory(docs)
endif()
