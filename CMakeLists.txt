cmake_minimum_required(VERSION 3.15)

file(STRINGS "VERSION" pVersion)

project(
    bufr 
    VERSION ${pVersion}
    LANGUAGES C Fortran)

set(${PROJECT_NAME}_VERSION ${PROJECT_VERSION} CACHE INTERNAL "${PROJECT_NAME} version number")

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

add_compile_definitions(UNDERSCORE)
if(APPLE)
  add_compile_definitions(APPLE)
elseif(UNIX)
  add_compile_definitions(LINUX)
endif()

if(CMAKE_C_COMPILER_ID MATCHES "^(Intel)$")
  include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/Intel.cmake)
  set(IntelComp true )
elseif(CMAKE_C_COMPILER_ID MATCHES "^(GNU|Clang|AppleClang)$")
  set(GNUComp true )
  include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/GNU.cmake)
endif()

set(fortran_8_flags ${FREAL8} ${FINT8})
set(shared_flags  ${DEBINFO} ${FOPT3} ${TRACEBACK} ${AVX2})
set(fortran_8_DA_flags ${FREAL8} ${FINT8})
set(fortran_d_DA_flags ${FREAL8})
set(c_SS_flags ${MMMED} ${SHARED})

STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "RelWithDebInfo" BUILD_RELEASE)
STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "RELEASE" BUILD_RELEASE)
STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "PRODUCTION" BUILD_PRODUCTION)
STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "DEBUG" BUILD_DEBUG)

option(GSIBUILD "Build only libs for GSI" ON)

set(definitions "UNDERSCORE" "NFILES=32" "MAXCD=250" "MAXNC=600" "MXNAF=3")
set(c_DA_allocation_def "DYNAMIC_ALLOCATION")
set(c_nonDA_allocation_def "STATIC_ALLOCATION")

set(c_4_DA_flags "-DUNDERSCORE")
set(c_8_DA_definitions "F77_INTSIZE_8")
set(c_d_DA_flags "-DUNDERSCORE")
set(fortran_4_DA_flags)
set(c_4_flags  "-DUNDERSCORE")
set(c_8_flags  "-DUNDERSCORE")
set(c_8_definitions "F77_INTSIZE_8")
set(c_d_flags "-DUNDERSCORE")
set(fortran_4_flags)
if(IntelComp)
      set(c_DA_allocation_def "DYNAMIC_ALLOCATION")
      set(c_nonDA_allocation_def "STATIC_ALLOCATION")
      message("Intel Compiler flags set")
elseif(GNUComp)
    message("GNU Compiler flags set")
#   if(NOT GSIBUILD)
#     message(FATAL_ERROR, GNU compiler not configured for non-GSIBUILD)
#   endif()
    set(allocation_def "DYNAMIC_ALLOCATION")
    set(shared_fortran_flags ${UNROLL} ${NOTAB})
else()
    message("unknown compiler!")
endif()


file(GLOB fortran_src ${CMAKE_CURRENT_SOURCE_DIR}/src/*.f ${CMAKE_CURRENT_SOURCE_DIR}/src/*.F)
file(GLOB c_src ${CMAKE_CURRENT_SOURCE_DIR}/src/*.c)

if(GSIBUILD)
  set(kinds "4" "d" "8")
else()
  set(kinds "4" "d" "8" "SS" "4_DA" "8_DA" "d_DA")
endif()

foreach(kind ${kinds})
  set(lib_name ${PROJECT_NAME}_${kind})
  set(versioned_lib_name ${PROJECT_NAME}_v${PROJECT_VERSION}_${kind})

  # different compiler definitions for Intel in DA vs non-DA
  # -DDYNAMIC_ALLOCATION when compiled with DA and -DSTATIC_ALLOCATION
  # check current kind and if it has 'DA' in it then set compiler def

  if(IntelComp)
    string(FIND ${kind} "DA" isDA)
    if(isDA GREATER_EQUAL 0)
      set(allocation_def ${c_DA_allocation_def})
    else()
      set(allocation_def ${c_nonDA_allocation_def})
    endif()
  endif()

  add_library(${lib_name}_f OBJECT ${fortran_src})
  set_target_properties(${lib_name}_f PROPERTIES POSITION_INDEPENDENT_CODE ON)
  set_target_properties(${lib_name}_f PROPERTIES
    COMPILE_OPTIONS "${shared_flags};${shared_fortran_flags};${fortran_${kind}_flags}")
  set_target_properties(${lib_name}_f PROPERTIES
    COMPILE_DEFINITIONS "${allocation_def}")

  add_library(${lib_name}_c OBJECT ${c_src})
  set_target_properties(${lib_name}_c PROPERTIES POSITION_INDEPENDENT_CODE ON)
  set_target_properties(${lib_name}_c PROPERTIES
#   COMPILE_OPTIONS "${shared_flags}")
    COMPILE_OPTIONS "${shared_flags};${c_${kind}_flags}")
  set_target_properties(${lib_name}_c PROPERTIES
    COMPILE_DEFINITIONS "${c_${kind}_definitions};${allocation_def}")

  set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include_${kind}")
  set_target_properties(${lib_name}_f PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")

  add_library(${lib_name} STATIC $<TARGET_OBJECTS:${lib_name}_f> $<TARGET_OBJECTS:${lib_name}_c>)

  set_target_properties(${lib_name} PROPERTIES OUTPUT_NAME "${versioned_lib_name}")

  target_include_directories(${lib_name} PUBLIC
    $<BUILD_INTERFACE:${module_dir}>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_PREFIX}/include_${kind}>)

  install(TARGETS ${lib_name}
    EXPORT ${PROJECT_NAME}-config
    RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX}/bin
    LIBRARY DESTINATION ${CMAKE_INSTALL_PREFIX}/lib
    ARCHIVE DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)
  install(DIRECTORY ${module_dir} DESTINATION ${CMAKE_INSTALL_PREFIX})
endforeach()

install(EXPORT ${PROJECT_NAME}-config 
        DESTINATION ${CMAKE_INSTALL_PREFIX}/lib/cmake/${PROJECT_NAME})

