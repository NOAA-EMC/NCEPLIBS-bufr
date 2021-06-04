# Create the debufr.c file
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/debufr.c.in ${CMAKE_CURRENT_BINARY_DIR}/debufr.c @ONLY)

list(APPEND _utils_srcs
  binv.f
  cmpbqm.f
  readbp.f
  readmp.f
  sinv.f
  gettab.f90
  split_by_subset.f90
)

foreach(_src ${_utils_srcs})
  get_filename_component(_name "${_src}" NAME_WE)
  set( _exec "${_name}.x")
  add_executable(${_exec} ${_src})
  add_dependencies(${_exec} bufr::bufr_4_DA)
  target_link_libraries(${_exec} PRIVATE bufr::bufr_4_DA)
  list(APPEND _utils_execs ${_exec})
endforeach()

# the debufr utility uses the "8_DA" (8-byte integer, 8-byte real, dynamic allocation)
# build of the library
set(_exec debufr.x)
add_executable(${_exec} ${CMAKE_CURRENT_BINARY_DIR}/debufr.c debufr.f)
add_dependencies(${_exec} bufr::bufr_8_DA)
set_property(SOURCE ${CMAKE_CURRENT_BINARY_DIR}/debufr.c APPEND_STRING PROPERTY COMPILE_DEFINITIONS
                                                                                "${underscore_def}"
                                                                                "${c_8_DA_defs}")
set_property(SOURCE debufr.f APPEND_STRING PROPERTY COMPILE_FLAGS
                                                    "${fortran_8_DA_flags}")
target_include_directories(${_exec} PRIVATE "${PROJECT_BINARY_DIR}/src")
set_property(TARGET ${_exec} PROPERTY LINKER_LANGUAGE C)
target_link_libraries(${_exec} PRIVATE bufr::bufr_8_DA)
list(APPEND _utils_execs ${_exec})

install(TARGETS ${_utils_execs}
        RUNTIME
        DESTINATION bin
        COMPONENT utilities)
