# Set library specific flags
if(CMAKE_C_COMPILER_ID MATCHES "^(Intel)$")
  set(c_8_defs "F77_INTSIZE_8")
endif()

set(c_8_DA_defs ${c_8_defs})

# Create the bvers.f file with the version from VERSION file.
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/bvers.f.in ${CMAKE_CURRENT_BINARY_DIR}/bvers.f @ONLY)

# Extract BUFRLIB parameters
file(READ bufrlib.prm _bufrlib_prm_str)
foreach(_var IN ITEMS MAXNC MXNAF)
    if(_bufrlib_prm_str MATCHES "${_var} = ([0-9]+)")
      list(APPEND c_defs ${_var}=${CMAKE_MATCH_1})
    else()
      message(FATAL_ERROR "Unable to parse variable ${_var} value from file: src/bufrlib.prm")
    endif()
endforeach()

list(APPEND underscore_def UNDERSCORE)

include(TestBigEndian)
test_big_endian(IS_BIG_ENDIAN)
if(IS_BIG_ENDIAN)
  list(APPEND endian_def BIG_ENDIAN)
else()
  list(APPEND endian_def LITTLE_ENDIAN)
endif()

include("list_of_files.cmake")

foreach(kind ${kinds})
  set(lib_name ${PROJECT_NAME}_${kind})
  set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include_${kind}")

  # determine ALLOCATION based on kind
  if(${kind} MATCHES "^([4|8|d]_DA)$")
    set(allocation_def "DYNAMIC_ALLOCATION")
  else()
    set(allocation_def "STATIC_ALLOCATION")
  endif()

  add_library(${lib_name}_f STATIC OBJECT ${fortran_src})
  set_target_properties(${lib_name}_f PROPERTIES COMPILE_FLAGS
                                                 "${fortran_${kind}_flags}")
  target_compile_definitions(${lib_name}_f PUBLIC "${allocation_def}")
  target_compile_definitions(${lib_name}_f PUBLIC "${underscore_def}")
  target_compile_definitions(${lib_name}_f PRIVATE "${endian_def}")

  add_library(${lib_name}_c STATIC OBJECT ${c_src})
  set_target_properties(${lib_name}_c PROPERTIES COMPILE_FLAGS
                                                 "${c_${kind}_flags}")
  target_compile_definitions(${lib_name}_c PUBLIC "${allocation_def}")
  target_compile_definitions(${lib_name}_c PUBLIC "${underscore_def}")
  target_compile_definitions(${lib_name}_c PRIVATE "${endian_def}")
  target_compile_definitions(${lib_name}_c PRIVATE "${c_defs}")
  target_compile_definitions(${lib_name}_c PRIVATE "${c_${kind}_defs}")
  target_include_directories(${lib_name}_c PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

  set_target_properties(${lib_name}_f PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")

  add_library(${lib_name} STATIC $<TARGET_OBJECTS:${lib_name}_f>
                                 $<TARGET_OBJECTS:${lib_name}_c>)
  add_library(${PROJECT_NAME}::${lib_name} ALIAS ${lib_name})

  target_include_directories(${lib_name} PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>)

  target_include_directories(${lib_name} INTERFACE
    $<BUILD_INTERFACE:${module_dir}>
    $<INSTALL_INTERFACE:include_${kind}>)

  target_compile_definitions(${lib_name} PUBLIC "${underscore_def}")
  target_compile_definitions(${lib_name} PUBLIC "${allocation_def}")

  list(APPEND LIB_TARGETS ${lib_name})
  install(DIRECTORY ${module_dir} DESTINATION ${CMAKE_INSTALL_PREFIX})
  install(FILES ${c_hdr} DESTINATION ${CMAKE_INSTALL_PREFIX}/include_${kind})
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
