# This is the cmake build file for the python directory of the
# NCEPLIBS-bufr project.
#
# Rahul Mahajan

configure_file(setup.py.in setup.py @ONLY)

file( COPY ncepbufr utils DESTINATION . )

# Library installation directory
set(_PYVER "${Python3_VERSION_MAJOR}.${Python3_VERSION_MINOR}")
set(_build_dir "${CMAKE_BINARY_DIR}/python/site-packages")

# Build the extension module for use in install tree
add_custom_target(python_mod ALL)

if( NOT \$ENV{DESTDIR} STREQUAL \"\" )
    set( __root \"--root=\$ENV{DESTDIR}\" )
endif()
add_custom_command(
    TARGET python_mod
    COMMAND ${Python3_EXECUTABLE} setup.py install
        \${__root}
        --prefix=${CMAKE_INSTALL_PREFIX}
        --install-lib=${_build_dir}
        --record=${CMAKE_BINARY_DIR}/extra_install.txt
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS setup.py.in
)

if(ENABLE_DOCS)
  # Uses pdoc (https://github.com/BurntSushi/pdoc)
  find_program(PDOC_EXECUTABLE pdoc REQUIRED)
  set(ENV{PYTHONPATH} ${_build_dir})
  add_custom_command(COMMAND ${PDOC_EXECUTABLE} -o ../docs/html/python ./ncepbufr WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
endif()

install(DIRECTORY ${_build_dir} DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR}/python${_PYVER})

# Add tests
add_test(NAME test_pyncepbufr_checkpoint
         COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test/test_checkpoint.py
         WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/test/testfiles)
list(APPEND _python_tests test_pyncepbufr_checkpoint)

add_test(NAME test_pyncepbufr_gps
         COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test/test_gps.py
         WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/test/testfiles)
list(APPEND _python_tests test_pyncepbufr_gps)

add_test(NAME test_pyncepbufr_prepbufr
         COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test/test_prepbufr.py
         WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/test/testfiles)
list(APPEND _python_tests test_pyncepbufr_prepbufr)

add_test(NAME test_pyncepbufr_rad
         COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test/test_rad.py
         WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/test/testfiles)
list(APPEND _python_tests test_pyncepbufr_rad)

add_test(NAME test_pyncepbufr_satwnd
         COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test/test_satwnd.py
         WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/test/testfiles)
list(APPEND _python_tests test_pyncepbufr_satwnd)

add_test(NAME test_pyncepbufr_write
         COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test/test_write.py
         WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/test/testfiles)
list(APPEND _python_tests test_pyncepbufr_write)

add_test(NAME test_pyncepbufr_test
         COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test/test.py
         WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/test/testfiles)
list(APPEND _python_tests test_pyncepbufr_test)

foreach(_test ${_python_tests})
  set_tests_properties(${_test}
    PROPERTIES ENVIRONMENT "PYTHONPATH=${_build_dir}:$ENV{PYTHONPATH}")
endforeach()
