# Create the debufr.c and sinv.f90 files
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/debufr.c.in ${CMAKE_CURRENT_BINARY_DIR}/debufr.c @ONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/sinv.f90.in ${CMAKE_CURRENT_BINARY_DIR}/sinv.f90 @ONLY)

list(APPEND _utils_srcs
  binv.f90
  cmpbqm.f90
  ${CMAKE_CURRENT_BINARY_DIR}/sinv.f90
  gettab.f90
  readbp.f90
  readmp.f90
  split_by_subset.f90
  xbfmg.c
)

foreach(_src ${_utils_srcs})
  get_filename_component(_exec "${_src}" NAME_WE)
  get_filename_component(_ext "${_src}" LAST_EXT)
  
  add_executable(${_exec} ${_src})
  add_dependencies(${_exec} bufr::bufr_4)
  if(${_ext} MATCHES ".c")
    set_property(SOURCE ${_src} APPEND_STRING PROPERTY COMPILE_DEFINITIONS "${underscore_def}")
    target_include_directories(${_exec} PRIVATE "${PROJECT_BINARY_DIR}/src")
    set_property(TARGET ${_exec} PROPERTY LINKER_LANGUAGE C)
  endif()
  target_link_libraries(${_exec} PRIVATE bufr::bufr_4)
  list(APPEND _utils_execs ${_exec})
endforeach()

# the debufr utility uses the "8" (8-byte integer, 8-byte real) build of the library
set(_exec debufr)
add_executable(${_exec} ${CMAKE_CURRENT_BINARY_DIR}/debufr.c debufr.f)
add_dependencies(${_exec} bufr::bufr_8)
set_property(SOURCE ${CMAKE_CURRENT_BINARY_DIR}/debufr.c APPEND_STRING PROPERTY COMPILE_DEFINITIONS
             "${underscore_def}" "${c_8_defs}")
set_property(SOURCE debufr.f APPEND_STRING PROPERTY COMPILE_FLAGS "${fortran_8_flags}")
target_include_directories(${_exec} PRIVATE "${PROJECT_BINARY_DIR}/src")
set_property(TARGET ${_exec} PROPERTY LINKER_LANGUAGE C)
target_link_libraries(${_exec} PRIVATE bufr::bufr_8)
list(APPEND _utils_execs ${_exec})

install(TARGETS ${_utils_execs}
        RUNTIME
        DESTINATION bin
        COMPONENT utilities)
