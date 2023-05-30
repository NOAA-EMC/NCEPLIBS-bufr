# This is the cmake build file for the tables directory of the
# NCEPLIBS-bufr project.
#
# Jeff Ator

# This is the list of tables.
foreach(type "TableB" "TableD" "CodeFlag")
  foreach(version RANGE 13 40)
    list(APPEND bufr_tables bufrtab.${type}_STD_0_${version})
  endforeach()
  list(APPEND bufr_tables bufrtab.${type}_LOC_0_7_1)
endforeach()

# Link BUFR tables in the build area
foreach(FILENAME ${bufr_tables})
  execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
                   ${CMAKE_CURRENT_SOURCE_DIR}/${FILENAME}
                   ${CMAKE_CURRENT_BINARY_DIR}/${FILENAME} )
endforeach(FILENAME)

# Install BUFR tables during installation
install(FILES ${bufr_tables} DESTINATION ${MASTER_TABLE_DIR})
