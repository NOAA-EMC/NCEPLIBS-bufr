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
#set(_lib debufr.o)
#add_library(${_lib} OBJECT debufr.f)
#set_target_properties(${_lib} PROPERTIES COMPILE_FLAGS
#                                         "${fortran_8_DA_flags}")
#set(_exec debufr.x)
#add_executable(${_exec} debufr.c)
#target_compile_definitions(${_exec} PUBLIC "${underscore_def}")
#target_compile_definitions(${_exec} PUBLIC "${c_8_DA_defs}")
#add_dependencies(${_exec} bufr::bufr_8_DA)
#target_link_libraries(${_exec} PRIVATE $<TARGET_OBJECTS:${_lib}>)
#target_link_libraries(${_exec} PRIVATE bufr::bufr_8_DA)
#list(APPEND _utils_execs ${_exec})

install(TARGETS ${_utils_execs}
        RUNTIME
        DESTINATION bin
        COMPONENT utilities)
