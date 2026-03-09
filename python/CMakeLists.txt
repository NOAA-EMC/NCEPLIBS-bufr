# This is the cmake build file for the python directory of the
# NCEPLIBS-bufr project.
#
# Alex Richert, Rahul Mahajan

set(_PYVER "${Python3_VERSION_MAJOR}.${Python3_VERSION_MINOR}")

file(COPY ncepbufr DESTINATION .)

# Check NumPy version
execute_process(
  COMMAND ${Python3_EXECUTABLE} -c "import numpy; print(numpy.__version__)"
  OUTPUT_VARIABLE NUMPY_VERSION
  OUTPUT_STRIP_TRAILING_WHITESPACE
  ERROR_QUIET
)

if(NOT NUMPY_VERSION)
  message(FATAL_ERROR "NumPy not found. Please install numpy.")
endif()

message(STATUS "NumPy version: ${NUMPY_VERSION}")

# Set library path and link flags
set(BUFR_LIB_DIR "${CMAKE_BINARY_DIR}/src")
set(BUFR_LIB_NAME "bufr_4")

# Build Python extension module
if(NUMPY_VERSION VERSION_GREATER_EQUAL "2.0.0")
  # NumPy 2.0+ with meson backend
  message(STATUS "Using meson backend for f2py")

  # Set environment variables for meson to find the library
  add_custom_target(python_mod ALL
    COMMAND ${CMAKE_COMMAND} -E env
            "LDFLAGS=-L${BUFR_LIB_DIR} -l${BUFR_LIB_NAME} -Wl,-rpath,${BUFR_LIB_DIR}"
            ${Python3_EXECUTABLE} -m numpy.f2py
            -c "${CMAKE_CURRENT_SOURCE_DIR}/_bufrlib.pyf"
            -m _bufrlib
            --backend meson
    DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/_bufrlib.pyf" bufr_4
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Building Python module _bufrlib with f2py (meson backend)"
  )
else()
  # NumPy < 2.0 with distutils backend
  message(STATUS "Using distutils backend for f2py")

  find_program(F2PY_EXECUTABLE
    NAMES f2py f2py${_PYVER} f2py${Python3_VERSION_MAJOR}
    DOC "f2py executable"
  )

  if(F2PY_EXECUTABLE)
    message(STATUS "Found f2py: ${F2PY_EXECUTABLE}")
    set(F2PY_COMMAND ${F2PY_EXECUTABLE})
  else()
    message(STATUS "Using python -m numpy.f2py")
    set(F2PY_COMMAND ${Python3_EXECUTABLE} -m numpy.f2py)
  endif()

  add_custom_target(python_mod ALL
    COMMAND ${F2PY_COMMAND}
            -c "${CMAKE_CURRENT_SOURCE_DIR}/_bufrlib.pyf"
            -m _bufrlib
            -L${BUFR_LIB_DIR}
            -l${BUFR_LIB_NAME}
    DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/_bufrlib.pyf" bufr_4
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Building Python module _bufrlib with f2py (distutils backend)"
  )
endif()

# Install Python package
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
  # Generate documentation
  find_program(PDOC_EXECUTABLE pdoc REQUIRED)
  add_custom_target(python_docs ALL
    ${CMAKE_COMMAND} -E env PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}:$ENV{PYTHONPATH}
    ${PDOC_EXECUTABLE} -o ../docs/html/python ./ncepbufr
  )
  add_dependencies(python_docs doc python_mod)
endif()

if(BUILD_TESTING)
  # Add tests
  set(pytest_list misc write outtest12 outtest13)
  foreach(pytestname ${pytest_list})
    add_test(
      NAME test_pyncepbufr_${pytestname}
      COMMAND
        ${CMAKE_COMMAND} -E env PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}:$ENV{PYTHONPATH}
        ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test/test_${pytestname}.py
      WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/test/testfiles
    )
  endforeach()
endif()
