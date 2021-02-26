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

add_library(_lib OBJECT debufr.f)
set(_exec debufr.x)
add_executable(${_exec} debufr.c)
target_compile_definitions(${_exec} PUBLIC "UNDERSCORE")
add_dependencies(${_exec} bufr::bufr_4_DA)
target_link_libraries(${_exec} PRIVATE $<TARGET_OBJECTS:_lib>)
target_link_libraries(${_exec} PRIVATE bufr::bufr_4_DA)
list(APPEND _utils_execs ${_exec})

install(TARGETS ${_utils_execs}
        RUNTIME
        DESTINATION bin
        COMPONENT utilities)
