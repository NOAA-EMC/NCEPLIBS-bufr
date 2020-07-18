list(APPEND test_IN_srcs
  test_IN_1.f
  test_IN_2.f
  test_IN_3.f
  test_IN_4.f
  test_IN_5.f
  )

list(APPEND test_OUT_srcs
  test_OUT_1.f
  test_OUT_2.f
  )

list(APPEND test_OUT_srcs_DA_ONLY
  test_OUT_3.f
)

list(APPEND test_IN_bufr_data
  IN_1 IN_2 IN_3 IN_4 IN_5
  bufrtab.CodeFlag_LOC_0_7_1
  bufrtab.CodeFlag_STD_0_13
  bufrtab.TableB_LOC_0_7_1
  bufrtab.TableB_STD_0_13
  bufrtab.TableB_STD_0_14
  bufrtab.TableD_LOC_0_7_1
  bufrtab.TableD_STD_0_13
  bufrtab.TableD_STD_0_14
  )

list(APPEND test_OUT_bufr_data
  OUT_1 OUT_2 OUT_3
  OUT_1_bufrtab OUT_2_bufrtab OUT_3_bufrtab
  )

list(APPEND test_bufr_data
  ${test_IN_bufr_data}
  ${test_OUT_bufr_data}
  )

# Create local testfiles directory
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testfiles)

# Soft-link test_bufr_data to local testfiles directory
foreach(FILENAME ${test_bufr_data})
  execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
    ${CMAKE_CURRENT_SOURCE_DIR}/testfiles/${FILENAME}
    ${CMAKE_CURRENT_BINARY_DIR}/testfiles/${FILENAME} )
endforeach(FILENAME)

# Install test_wrapper.sh
execute_process( COMMAND ${CMAKE_COMMAND} -E copy
  ${CMAKE_CURRENT_SOURCE_DIR}/test_wrapper.sh
  ${CMAKE_BINARY_DIR}/bin/test_wrapper.sh )

function(bufr_add_test TESTNAME EXENAME)
  add_test(NAME ${TESTNAME}
           COMMAND ${CMAKE_BINARY_DIR}/bin/test_wrapper.sh ${EXENAME})
endfunction()

foreach(test_src ${test_IN_srcs})
  string(REPLACE ".f" "" testPref ${test_src})
  foreach(kind ${kinds})
    set(test     ${testPref}_${kind})
    set(test_exe ${test}.x)
    add_executable(${test_exe} ${test_src})
    add_dependencies(${test_exe} bufr_${kind})
    target_link_libraries(${test_exe} PUBLIC bufr_${kind})
    add_test(NAME ${test} COMMAND ${test_exe})
  endforeach()
endforeach()

foreach(test_src ${test_OUT_srcs})
  string(REPLACE ".f" "" testPref ${test_src})
  foreach(kind ${kinds})
    set(test     ${testPref}_${kind})
    set(test_exe ${test}.x)
    add_executable(${test_exe} ${test_src})
    add_dependencies(${test_exe} bufr_${kind})
    target_link_libraries(${test_exe} PUBLIC bufr_${kind})
    bufr_add_test(${test} ${test_exe})
  endforeach()
endforeach()

#foreach(test_src ${test_OUT_srcs_DA_ONLY})
#  string(REPLACE ".f" "" testPref ${test_src})
#  foreach(kind ${kinds})
#    if(${kind} MATCHES "^([4|8|d]_DA)$")
#      set(test     ${testPref}_${kind})
#      set(test_exe ${test}.x)
#      add_executable(${test_exe} ${test_src})
#      add_dependencies(${test_exe} bufr_${kind})
#      target_link_libraries(${test_exe} PUBLIC bufr_${kind})
#      bufr_add_test(${test} ${test_exe})
#    endif()
#  endforeach()
#endforeach()
