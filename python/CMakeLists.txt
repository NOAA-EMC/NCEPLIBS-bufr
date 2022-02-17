configure_file(setup.py.in setup.py @ONLY)

file( COPY ncepbufr utils DESTINATION . )

# Library installation directory
execute_process(COMMAND ${Python3_EXECUTABLE} -c "from __future__ import print_function; import sys; print(sys.version[:3], end='')"
                  OUTPUT_VARIABLE _PYVER)
set(_install_dir "${CMAKE_INSTALL_FULL_LIBDIR}/python${_PYVER}/site-packages")

# Build the extension module for use in install tree
install(CODE "message(\"Building Python extension modules:
                                      ${Python3_EXECUTABLE} setup.py build\")
              execute_process(COMMAND ${Python3_EXECUTABLE} setup.py build
                              WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})")

# Call distutils for installation
install(CODE "if( NOT \$ENV{DESTDIR} STREQUAL \"\" )
                set( __root \"--root=\$ENV{DESTDIR}\" )
              endif()
              message(\"Installing Python modules:
                                      ${Python3_EXECUTABLE} -m pip install \${__root}
                                        --prefix=${CMAKE_INSTALL_PREFIX}
                                        --target=${_install_dir}
              execute_process(COMMAND ${Python3_EXECUTABLE} -m pip install \${__root}
                                        --prefix=${CMAKE_INSTALL_PREFIX}
                                        --target=${_install_dir}
                              WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})")

if(ENABLE_DOCS)
  # Uses pdoc (https://github.com/BurntSushi/pdoc)
  find_program(PDOC_EXECUTABLE pdoc REQUIRED)
  install(CODE "message(\"Building Python API documentation:
  ${PDOC_EXECUTABLE} -o ../docs/html/python' ./ncepbufr\")
                set(ENV{PYTHONPATH} ${_install_dir})
                execute_process(COMMAND ${PDOC_EXECUTABLE} -o ../docs/html/python ./ncepbufr
                                WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})")
endif()

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
    PROPERTIES ENVIRONMENT "PYTHONPATH=${_install_dir}:$ENV{PYTHONPATH}")
endforeach()
