set(CMAKE_INCLUDE_CURRENT_DIR ON)

# Create the bvers.f file
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/bvers.f.in ${CMAKE_CURRENT_BINARY_DIR}/bvers.f @ONLY)

# Create the bfrini.f90 file
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/bfrini.f90.in ${CMAKE_CURRENT_BINARY_DIR}/bfrini.f90 @ONLY)

# Create the bufrlib.h file
set(VAR_REGEX_DYNAMIC "INTEGER ::")
set(VAR_REGEX_STATIC "PARAMETER ")
foreach(_var IN ITEMS NFILES MAXCD MAXNC MXNAF)
  foreach(_alloc IN ITEMS DYNAMIC STATIC)
    file(STRINGS modv_${_var}.F _${_var}_tempstr REGEX ${VAR_REGEX_${_alloc}})
    if(_${_var}_tempstr MATCHES "${_var} = ([0-9]+)")
      set(${_var}_${_alloc} ${CMAKE_MATCH_1})
    elseif(${_var} MATCHES "^(NFILES|MAXCD)$" OR ${_alloc} MATCHES "^(STATIC)$")
      message(FATAL_ERROR "Unable to parse variable ${_var}_${_alloc} value from file: modv_${_var}.F")
    endif()
  endforeach()
endforeach()
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/bufrlib.h.in ${CMAKE_CURRENT_BINARY_DIR}/bufrlib.h @ONLY)

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
  set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include/bufr_${kind}")

  # determine ALLOCATION based on kind
  if(${kind} MATCHES "^([4|8|d]_DA)$")
    set(allocation_def "DYNAMIC_ALLOCATION")
  else()
    set(allocation_def "STATIC_ALLOCATION")
  endif()

  add_library(${lib_name}_f OBJECT ${fortran_src})
  set_target_properties(${lib_name}_f PROPERTIES COMPILE_FLAGS
                                                 "${fortran_${kind}_flags}")
  target_compile_definitions(${lib_name}_f PUBLIC "${allocation_def}")
  target_compile_definitions(${lib_name}_f PRIVATE "${endian_def}")

  add_library(${lib_name}_c OBJECT ${c_src})
  set_target_properties(${lib_name}_c PROPERTIES COMPILE_FLAGS
                                                 "${c_${kind}_flags}")
  target_compile_definitions(${lib_name}_c PUBLIC "${allocation_def}")
  target_compile_definitions(${lib_name}_c PUBLIC "${underscore_def}")
  target_compile_definitions(${lib_name}_c PRIVATE "${c_${kind}_defs}")

  set_target_properties(${lib_name}_f PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")

  add_library(${lib_name} $<TARGET_OBJECTS:${lib_name}_f>
                          $<TARGET_OBJECTS:${lib_name}_c>)
  add_library(${PROJECT_NAME}::${lib_name} ALIAS ${lib_name})

  target_include_directories(${lib_name} PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>)

  target_include_directories(${lib_name} INTERFACE
    $<BUILD_INTERFACE:${module_dir}>
    $<INSTALL_INTERFACE:include/bufr_${kind}>)

  target_compile_definitions(${lib_name} PUBLIC "${underscore_def}")
  target_compile_definitions(${lib_name} PUBLIC "${allocation_def}")

  list(APPEND LIB_TARGETS ${lib_name})
  install(DIRECTORY ${module_dir} DESTINATION ${CMAKE_INSTALL_PREFIX}/include)
  install(FILES ${c_hdr} DESTINATION ${CMAKE_INSTALL_PREFIX}/include/bufr_${kind})
endforeach()

install(
  TARGETS ${LIB_TARGETS}
  EXPORT ${PROJECT_NAME}Exports
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})

# Package config
include(CMakePackageConfigHelpers)
set(CONFIG_INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME})

export(EXPORT ${PROJECT_NAME}Exports
  NAMESPACE ${PROJECT_NAME}::
  FILE ${PROJECT_NAME}-targets.cmake)

configure_package_config_file(
  ${PROJECT_SOURCE_DIR}/cmake/PackageConfig.cmake.in ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config.cmake
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
