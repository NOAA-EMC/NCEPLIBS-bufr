# This is the cmake build file for the python directory of the
# NCEPLIBS-bufr project.
#
# Alex Richert, Rahul Mahajan

set(_PYVER "${Python3_VERSION_MAJOR}.${Python3_VERSION_MINOR}")

file( COPY ncepbufr DESTINATION . )
add_custom_target(python_mod ALL)
add_dependencies(python_mod bufr_4)

add_custom_command(
  TARGET python_mod
  POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E rm -f "${CMAKE_CURRENT_BINARY_DIR}/_bufrlib.cpython-*.so"
)

find_program(F2PY_EXECUTABLE NAMES f2py3 f2py)

add_custom_command(
  TARGET python_mod
  POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E env
                   LDFLAGS=-L${CMAKE_BINARY_DIR}/src LIBS=-lbufr_4
                   ${F2PY_EXECUTABLE} --verbose
                   -c "${CMAKE_CURRENT_SOURCE_DIR}/_bufrlib.pyf"
                   -m _bufrlib
                   -L${CMAKE_BINARY_DIR}/src
                   -lbufr_4
  DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/_bufrlib.pyf"
)

install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/ncepbufr
  DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR}/python${_PYVER}/site-packages
)
install(
  DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/
  DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR}/python${_PYVER}/site-packages
  FILES_MATCHING
  PATTERN "_bufrlib*.so"
  PATTERN "CMakeFiles" EXCLUDE
)
install(
  PROGRAMS ${CMAKE_CURRENT_SOURCE_DIR}/utils/prepbufr2nc
  DESTINATION ${CMAKE_INSTALL_FULL_BINDIR}
)

if(ENABLE_DOCS)
  # Uses pdoc (https://github.com/BurntSushi/pdoc)
  find_program(PDOC_EXECUTABLE pdoc REQUIRED)
  add_custom_target(python_docs ALL
    ${CMAKE_COMMAND} -E env PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}:$ENV{PYTHONPATH}
    ${PDOC_EXECUTABLE} -o ../docs/html/python ./ncepbufr
  )
  add_dependencies(python_docs doc python_mod)
endif()

# Add tests
set(pytest_list checkpoint gps misc prepbufr rad satwnd write)
foreach(pytestname ${pytest_list})
  add_test(
    NAME test_pyncepbufr_${pytestname}
    COMMAND
      ${CMAKE_COMMAND} -E env PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}:$ENV{PYTHONPATH}
      ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test/test_${pytestname}.py
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/test/testfiles
  )
endforeach()
