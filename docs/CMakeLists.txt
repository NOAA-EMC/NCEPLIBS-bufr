# This is the CMake file for building the docs directory of NCEPLIBS-bufr.
#
# Ed Hartnett 10/16/20

if(ENABLE_DOCS)

  set(abs_top_srcdir "${CMAKE_SOURCE_DIR}")
  set(config_srcdir "${CMAKE_CURRENT_BINARY_DIR}/../src")

  # Create doxyfile.
  configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)

  # Build documentation with target all.
  add_custom_target(doc ALL
    ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Generating API Documentation with Doxygen" VERBATIM)

endif(ENABLE_DOCS)

