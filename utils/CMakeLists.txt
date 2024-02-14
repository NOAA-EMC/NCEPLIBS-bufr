# This is the cmake build file for the utils directory of the
# NCEPLIBS-bufr project.
#
# Rahul Mahajan, Jeff Ator, Ed Hartnett

# Create the debufr.c and sinv.F90 files
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/debufr.c.in ${CMAKE_CURRENT_BINARY_DIR}/debufr.c @ONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/sinv.F90.in ${CMAKE_CURRENT_BINARY_DIR}/sinv.F90 @ONLY)

# Source code for utilities.
list(APPEND _utils_srcs
  binv.F90
  cmpbqm.F90
  ${CMAKE_CURRENT_BINARY_DIR}/sinv.F90
  gettab.F90
  readbp.F90
  readmp.F90
  split_by_subset.F90
  xbfmg.c
)

# Build each utility.
foreach(_src ${_utils_srcs})
  get_filename_component(_exec "${_src}" NAME_WE)
  get_filename_component(_ext "${_src}" LAST_EXT)
  
  add_executable(${_exec} ${_src})
  add_dependencies(${_exec} bufr::bufr_4)
  if(${_ext} MATCHES ".c")
    target_include_directories(${_exec} PRIVATE "${PROJECT_BINARY_DIR}/src")
    set_property(TARGET ${_exec} PROPERTY LINKER_LANGUAGE C)
  endif()
  target_link_libraries(${_exec} PRIVATE bufr::bufr_4)
  list(APPEND _utils_execs ${_exec})
endforeach()

# The debufr utility consists of both a C source file and a Fortran source file.
set(_exec debufr)
add_executable(${_exec} ${CMAKE_CURRENT_BINARY_DIR}/debufr.c debufr.F90)
add_dependencies(${_exec} bufr::bufr_4)
target_include_directories(${_exec} PRIVATE "${PROJECT_BINARY_DIR}/src")
set_property(TARGET ${_exec} PROPERTY LINKER_LANGUAGE C)
target_link_libraries(${_exec} PRIVATE bufr::bufr_4)
list(APPEND _utils_execs ${_exec})

# Install the utilities.
install(TARGETS ${_utils_execs}
        RUNTIME
        DESTINATION bin
        COMPONENT utilities)
