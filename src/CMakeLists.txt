cmake_minimum_required(VERSION 3.15)

set(definitions "UNDERSCORE" "NFILES=32" "MAXCD=250" "MAXNC=600" "MXNAF=3")
add_compile_definitions(definitions)

if(InteComp)
  set (FREAL8 "-r8")
  set (FINT8 "-i8")
  set (TRACEBACK "-traceback")
  set (MMMED "-mcmodel=medium")
elseif(GNUComp)
  set (FREAL8 "-fdefault-double-8" "-fdefault-real-8")
  set (FINT8 "-fdefault-integer-8")
  set (TRACEBACK "-fbacktrace")
  set (MMMED "")
endif()

set(fortran_8_flags ${FREAL8} ${FINT8} ${TRACEBACK})
set(shared_flags  ${DEBINFO} ${FOPT3} ${AVX2})
set(fortran_8_DA_flags ${FREAL8} ${FINT8} ${TRACEBACK})
set(fortran_d_DA_flags ${FREAL8} ${TRACEBACK})
set(c_SS_flags ${MMMED} ${SHARED})


set(c_DA_allocation_def "DYNAMIC_ALLOCATION")
set(c_nonDA_allocation_def "STATIC_ALLOCATION")

set(c_8_DA_definitions "F77_INTSIZE_8")
set(fortran_4_DA_flags)
set(c_8_definitions "F77_INTSIZE_8")
set(fortran_4_flags)

include("list_of_files.cmake")
set(kinds "4" "d" "8" "4_DA" "8_DA" "d_DA")

foreach(kind ${kinds})
  set(lib_name ${PROJECT_NAME}_${kind})

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

  add_library(${lib_name}_f STATIC OBJECT ${fortran_src})
  set_target_properties(${lib_name}_f PROPERTIES
    COMPILE_OPTIONS "${shared_flags};${shared_fortran_flags};${fortran_${kind}_flags}")
  set_target_properties(${lib_name}_f PROPERTIES
    COMPILE_DEFINITIONS "${allocation_def}")

  add_library(${lib_name}_c STATIC OBJECT ${c_src})
  set_target_properties(${lib_name}_c PROPERTIES
    COMPILE_OPTIONS "${shared_flags};${c_${kind}_flags}")

  set_target_properties(${lib_name}_c PROPERTIES
    COMPILE_DEFINITIONS "${c_${kind}_definitions};${allocation_def}")

  set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include_${kind}")
  set_target_properties(${lib_name}_f PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")

  add_library(${lib_name} STATIC $<TARGET_OBJECTS:${lib_name}_f> $<TARGET_OBJECTS:${lib_name}_c>)
  add_library(${PROJECT_NAME}::${lib_name} ALIAS ${lib_name})

  target_include_directories(${lib_name} INTERFACE
    $<BUILD_INTERFACE:${module_dir}>
    $<INSTALL_INTERFACE:include_${kind}>)

  list(APPEND LIB_TARGETS ${lib_name})
  install(DIRECTORY ${module_dir} DESTINATION ${CMAKE_INSTALL_PREFIX})

endforeach()

install(
  TARGETS ${LIB_TARGETS}
  EXPORT ${PROJECT_NAME}Exports
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib)

### Package config
include(CMakePackageConfigHelpers)
set(CONFIG_INSTALL_DESTINATION lib/cmake/${PROJECT_NAME})

export(EXPORT ${PROJECT_NAME}Exports
  NAMESPACE ${PROJECT_NAME}::
  FILE ${PROJECT_NAME}-targets.cmake)

configure_package_config_file(
  ${CMAKE_SOURCE_DIR}/cmake/PackageConfig.cmake.in ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config.cmake
  INSTALL_DESTINATION ${CONFIG_INSTALL_DESTINATION})
install(FILES ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config.cmake
  DESTINATION ${CONFIG_INSTALL_DESTINATION})

write_basic_package_version_file(
  ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY AnyNewerVersion)
install(FILES ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
  DESTINATION ${CONFIG_INSTALL_DESTINATION})

install(EXPORT ${PROJECT_NAME}Exports
  NAMESPACE ${PROJECT_NAME}::
  FILE ${PROJECT_NAME}-targets.cmake
  DESTINATION ${CONFIG_INSTALL_DESTINATION})
