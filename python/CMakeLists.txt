configure_file(setup.py.in setup.py @ONLY)

add_custom_target(pyncepbufr_build ALL
                  COMMAND ${Python3_EXECUTABLE} setup.py build_ext --inplace
                  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                  DEPENDS setup.py.in bufr_4_DA)

file( COPY ncepbufr utils DESTINATION . )

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

#if(ENABLE_DOCS)
#   # Uses pdoc (https://github.com/BurntSushi/pdoc)
#   # to create html docs from docstrings in python source.
#   pdoc --html --html-no-source --overwrite --html-dir 'docs' ncepbufr
#endif()
