list(APPEND bufr_tables
  bufrtab.CodeFlag_LOC_0_7_1
  bufrtab.CodeFlag_STD_0_13
  bufrtab.CodeFlag_STD_0_14
  bufrtab.CodeFlag_STD_0_15
  bufrtab.CodeFlag_STD_0_16
  bufrtab.CodeFlag_STD_0_17
  bufrtab.CodeFlag_STD_0_18
  bufrtab.CodeFlag_STD_0_19
  bufrtab.CodeFlag_STD_0_20
  bufrtab.CodeFlag_STD_0_21
  bufrtab.CodeFlag_STD_0_22
  bufrtab.CodeFlag_STD_0_23
  bufrtab.CodeFlag_STD_0_24
  bufrtab.CodeFlag_STD_0_25
  bufrtab.CodeFlag_STD_0_26
  bufrtab.CodeFlag_STD_0_27
  bufrtab.CodeFlag_STD_0_28
  bufrtab.CodeFlag_STD_0_29
  bufrtab.CodeFlag_STD_0_30
  bufrtab.CodeFlag_STD_0_31
  bufrtab.CodeFlag_STD_0_32
  bufrtab.CodeFlag_STD_0_33
  bufrtab.CodeFlag_STD_0_34
  bufrtab.CodeFlag_STD_0_35
  bufrtab.CodeFlag_STD_0_36
  bufrtab.CodeFlag_STD_0_37
  bufrtab.CodeFlag_STD_0_38
  bufrtab.TableB_LOC_0_7_1
  bufrtab.TableB_STD_0_13
  bufrtab.TableB_STD_0_14
  bufrtab.TableB_STD_0_15
  bufrtab.TableB_STD_0_16
  bufrtab.TableB_STD_0_17
  bufrtab.TableB_STD_0_18
  bufrtab.TableB_STD_0_19
  bufrtab.TableB_STD_0_20
  bufrtab.TableB_STD_0_21
  bufrtab.TableB_STD_0_22
  bufrtab.TableB_STD_0_23
  bufrtab.TableB_STD_0_24
  bufrtab.TableB_STD_0_25
  bufrtab.TableB_STD_0_26
  bufrtab.TableB_STD_0_27
  bufrtab.TableB_STD_0_28
  bufrtab.TableB_STD_0_29
  bufrtab.TableB_STD_0_30
  bufrtab.TableB_STD_0_31
  bufrtab.TableB_STD_0_32
  bufrtab.TableB_STD_0_33
  bufrtab.TableB_STD_0_34
  bufrtab.TableB_STD_0_35
  bufrtab.TableB_STD_0_36
  bufrtab.TableB_STD_0_37
  bufrtab.TableB_STD_0_38
  bufrtab.TableD_LOC_0_7_1
  bufrtab.TableD_STD_0_13
  bufrtab.TableD_STD_0_14
  bufrtab.TableD_STD_0_15
  bufrtab.TableD_STD_0_16
  bufrtab.TableD_STD_0_17
  bufrtab.TableD_STD_0_18
  bufrtab.TableD_STD_0_19
  bufrtab.TableD_STD_0_20
  bufrtab.TableD_STD_0_21
  bufrtab.TableD_STD_0_22
  bufrtab.TableD_STD_0_23
  bufrtab.TableD_STD_0_24
  bufrtab.TableD_STD_0_25
  bufrtab.TableD_STD_0_26
  bufrtab.TableD_STD_0_27
  bufrtab.TableD_STD_0_28
  bufrtab.TableD_STD_0_29
  bufrtab.TableD_STD_0_30
  bufrtab.TableD_STD_0_31
  bufrtab.TableD_STD_0_32
  bufrtab.TableD_STD_0_33
  bufrtab.TableD_STD_0_34
  bufrtab.TableD_STD_0_35
  bufrtab.TableD_STD_0_36
  bufrtab.TableD_STD_0_37
  bufrtab.TableD_STD_0_38
)

# Link BUFR tables in the build area
foreach(FILENAME ${bufr_tables})
  execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
                   ${CMAKE_CURRENT_SOURCE_DIR}/${FILENAME}
                   ${CMAKE_CURRENT_BINARY_DIR}/${FILENAME} )
endforeach(FILENAME)

# Install BUFR tables during installation
install(FILES ${bufr_tables} DESTINATION ${MASTER_TABLE_DIR})

