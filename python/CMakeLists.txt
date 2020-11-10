configure_file(setup.py.in setup.py @ONLY)

file( COPY ncepbufr utils DESTINATION . )

# Build the extension module for use in install tree with RPATH pointing to install tree
install(CODE "message(\"Building Python extension modules:
${Python3_EXECUTABLE} setup.py build_ext --rpath ${CMAKE_INSTALL_PREFIX}/${INSTALL_LIB_DIR}\")
              execute_process(COMMAND ${Python3_EXECUTABLE} setup.py build_ext --rpath ${CMAKE_INSTALL_PREFIX}/${INSTALL_LIB_DIR}
                              WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})")

# Library installation directory
execute_process(COMMAND ${Python3_EXECUTABLE} -c "from __future__ import print_function; import sys; print(sys.version[:3], end='')"
                  OUTPUT_VARIABLE _PYVER)
set( __install_lib "--install-lib=${CMAKE_INSTALL_FULL_LIBDIR}/python${_PYVER}/site-packages" )

# Call distutils for installation
install(CODE "if( NOT \$ENV{DESTDIR} STREQUAL \"\" )
                set( __root \"--root=\$ENV{DESTDIR}\" )
              endif()
              message(\"Installing Python modules:
${Python3_EXECUTABLE} setup.py install \${__root}
                                       ${__install_lib}
                                       --prefix=${CMAKE_INSTALL_PREFIX}
                                       --record=${CMAKE_BINARY_DIR}/extra_install.txt\")
              execute_process(COMMAND ${Python3_EXECUTABLE} setup.py install
                                        \${__root}
                                        --prefix=${CMAKE_INSTALL_PREFIX}
                                        ${__install_lib}
                                        --record=${CMAKE_BINARY_DIR}/extra_install.txt
                              WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})")

if(ENABLE_DOCS)
  find_program(PDOC_EXECUTABLE pdoc)
  # Uses pdoc (https://github.com/BurntSushi/pdoc)
  if (PDOC_EXECUTABLE)
    add_custom_target( pydoc ALL
                      COMMAND ${PDOC_EXECUTABLE} --html --html-no-source --overwrite --html-dir 'py-docs' ncepbufr
                      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                      COMMENT "Generating Python API Documentation with pdoc" VERBATIM)
  endif()
endif()

add_test(
  NAME    test_checkpoint
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test/test_checkpoint.py ${CMAKE_BINARY_DIR}/test/testfiles/prepbufr)
list(APPEND _python_tests test_checkpoint)

foreach(_test ${_python_tests})
  set_tests_properties(${_test}
    PROPERTIES ENVIRONMENT "PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}/build/lib.macosx-10.15-x86_64-3.8:$ENV{PYTHONPATH}")
endforeach()
