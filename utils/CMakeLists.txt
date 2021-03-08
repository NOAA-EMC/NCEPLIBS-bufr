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
add_executable(${_exec} debufr.c debufr.f)
add_dependencies(${_exec} bufr::bufr_8_DA)
set_property(SOURCE debufr.c APPEND_STRING PROPERTY COMPILE_DEFINITIONS
                                                    "${underscore_def}"
                                                    "${c_8_DA_defs}")
set_property(SOURCE debufr.f APPEND_STRING PROPERTY COMPILE_FLAGS
                                                    "${fortran_8_DA_flags}")
target_include_directories(${_exec} PRIVATE "${PROJECT_BINARY_DIR}/src")
target_link_libraries(${_exec} PRIVATE bufr::bufr_8_DA)
if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  target_link_options(${_exec} PRIVATE "-nofor-main")
endif()
list(APPEND _utils_execs ${_exec})

install(TARGETS ${_utils_execs}
        RUNTIME
        DESTINATION bin
        COMPONENT utilities)
