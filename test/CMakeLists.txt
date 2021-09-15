# Fetch test data from: https://ftp.emc.ncep.noaa.gov/static_files/public/bufr.tar
set(BUFR_URL "https://ftp.emc.ncep.noaa.gov/static_files/public")
if(${PROJECT_VERSION} VERSION_GREATER_EQUAL 11.6.0)
  set(BUFR_TAR "bufr-11.6.0.tar")
else()
  set(BUFR_TAR "bufr.tar")
endif()

if(NOT EXISTS "${CMAKE_CURRENT_BINARY_DIR}/${BUFR_TAR}")
  message(STATUS "Downloading bufr test files...")
  file(DOWNLOAD
    ${BUFR_URL}/${BUFR_TAR}
    ${CMAKE_CURRENT_BINARY_DIR}/${BUFR_TAR}
    SHOW_PROGRESS
    STATUS status
    INACTIVITY_TIMEOUT 30
    )

  list(GET status 0 status_num)

  if(NOT status_num EQUAL 0 OR NOT EXISTS ${CMAKE_CURRENT_BINARY_DIR}/${BUFR_TAR})
    # Remove empty file if download doesn't complete
    file(REMOVE ${CMAKE_CURRENT_BINARY_DIR}/${BUFR_TAR})
    message(STATUS "Could not download bufr test files, not building tests")
    return()
  endif()
  
endif()

add_custom_target(get_bufr_test_data ALL DEPENDS ${BUFR_TAR})
add_custom_command(
  TARGET get_bufr_test_data
  POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E chdir ${CMAKE_CURRENT_BINARY_DIR} tar xvf ${BUFR_TAR}
  COMMAND ${CMAKE_COMMAND} -E chdir ${CMAKE_CURRENT_BINARY_DIR} rm -rf testfiles
  COMMAND ${CMAKE_COMMAND} -E chdir ${CMAKE_CURRENT_BINARY_DIR} mv bufr/testfiles testfiles
  COMMAND ${CMAKE_COMMAND} -E chdir ${CMAKE_CURRENT_BINARY_DIR} rm -rf bufr)

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testrun)

list(APPEND test_IN_srcs
  test_IN_1.f
  test_IN_2.f
  test_IN_3.f
  test_IN_4.f
  test_IN_5.f
  test_IN_6.f
  )

list(APPEND test_OUT_1_srcs
  test_OUT_1.f
  )

list(APPEND test_OUT_2_srcs
  test_OUT_2.f
  )

list(APPEND test_OUT_3_srcs
  test_OUT_3.f
)

list(APPEND test_OUT_4_srcs
  test_OUT_4.f
)

list(APPEND test_OUT_5_srcs
  test_OUT_5.f
)

list(APPEND test_OUT_6_srcs
  test_OUT_6.f
)

list(APPEND test_c_interface_srcs
  test_c_interface.c
  test_c_interface_2.c
)

# Install testing scripts
list(APPEND test_scripts
  test_wrapper_IN.sh
  test_wrapper_OUT.sh
  test_gettab.sh
  test_split_by_subset.sh
  test_debufr.sh
)

foreach(test_script ${test_scripts})
  execute_process( COMMAND ${CMAKE_COMMAND} -E copy
    ${CMAKE_CURRENT_SOURCE_DIR}/test_scripts/${test_script}
    ${CMAKE_BINARY_DIR}/bin/${test_script} )
endforeach()

function(bufr_add_test TESTNAME EXENAME)
  add_test(NAME ${TESTNAME}
           COMMAND ${CMAKE_BINARY_DIR}/bin/test_wrapper_OUT.sh ${EXENAME})
endfunction()

function(bufr_add_test_preAPX TESTNAME EXENAME)
  add_test(NAME ${TESTNAME}
           COMMAND ${CMAKE_BINARY_DIR}/bin/test_wrapper_OUT.sh ${EXENAME} "Y")
endfunction()

list(APPEND test_kinds_4 "4")

list(APPEND test_kinds_8 "8")

# IN tests
foreach(test_src ${test_IN_srcs})
  string(REPLACE ".f" "" testPref ${test_src})
  foreach(kind ${test_kinds_4})
    set(test     ${testPref}_${kind})
    set(test_exe ${test}.x)
    add_executable(${test_exe} ${test_src})
    set_target_properties(${test_exe} PROPERTIES COMPILE_FLAGS
                                                 "${fortran_${kind}_flags}")
    add_dependencies(${test_exe} bufr_${kind})
    target_link_libraries(${test_exe} PUBLIC bufr::bufr_${kind})
    add_test(NAME ${test} COMMAND ${CMAKE_BINARY_DIR}/bin/test_wrapper_IN.sh ${test_exe})
  endforeach()
endforeach()

# OUT_1, OUT_3, OUT_4, and OUT_5 tests
foreach(test_src ${test_OUT_1_srcs} ${test_OUT_3_srcs} ${test_OUT_4_srcs} ${test_OUT_5_srcs})
  string(REPLACE ".f" "" testPref ${test_src})
  foreach(kind ${test_kinds_4})
    set(test     ${testPref}_${kind})
    set(test_exe ${test}.x)
    add_executable(${test_exe} ${test_src})
    set_target_properties(${test_exe} PROPERTIES COMPILE_FLAGS
                                                 "${fortran_${kind}_flags}")
    add_dependencies(${test_exe} bufr_${kind})
    target_link_libraries(${test_exe} PRIVATE bufr::bufr_${kind})
    bufr_add_test(${test} ${test_exe})
  endforeach()
endforeach()

# OUT_2 tests; these use the bufr_add_test_preAPX function
foreach(test_src ${test_OUT_2_srcs})
  string(REPLACE ".f" "" testPref ${test_src})
  foreach(kind ${test_kinds_4})
    set(test     ${testPref}_${kind})
    set(test_exe ${test}.x)
    add_executable(${test_exe} ${test_src})
    set_target_properties(${test_exe} PROPERTIES COMPILE_FLAGS
                                                 "${fortran_${kind}_flags}")
    add_dependencies(${test_exe} bufr_${kind})
    target_link_libraries(${test_exe} PRIVATE bufr::bufr_${kind})
    bufr_add_test_preAPX(${test} ${test_exe})
  endforeach()
endforeach()

# OUT_6 tests; these are for "8" (8-byte integer, 8-byte real) builds only
foreach(test_src ${test_OUT_6_srcs})
  string(REPLACE ".f" "" testPref ${test_src})
  foreach(kind ${test_kinds_8})
    set(test     ${testPref}_${kind})
    set(test_exe ${test}.x)
    add_executable(${test_exe} ${test_src})
    set_target_properties(${test_exe} PROPERTIES COMPILE_FLAGS
                                                 "${fortran_${kind}_flags}")
    add_dependencies(${test_exe} bufr_${kind})
    target_link_libraries(${test_exe} PRIVATE bufr::bufr_${kind})
    bufr_add_test(${test} ${test_exe})
  endforeach()
endforeach()

# c_interface tests
foreach(test_src ${test_c_interface_srcs})
  string(REPLACE ".c" "" testPref ${test_src})
  foreach(kind ${test_kinds_4})
    set(test     ${testPref}_${kind})
    set(test_exe ${test}.x)
    add_executable(${test_exe} ${test_src})
    add_dependencies(${test_exe} bufr_${kind})
    target_link_libraries(${test_exe} PRIVATE bufr::bufr_${kind})
    add_test(NAME ${test} COMMAND ${CMAKE_BINARY_DIR}/test/${test_exe})
  endforeach()
endforeach()

# Test utilities

# Test debufr.x utility
add_test(NAME test_debufr_1
  COMMAND ${CMAKE_BINARY_DIR}/bin/test_debufr.sh "${CMAKE_BINARY_DIR}/utils/debufr.x -t ../tables" "testfiles/data/debufr_1" "testrun/debufr_1.run" "testfiles/testoutput/debufr_1.out"
  )
add_test(NAME test_debufr_2
  COMMAND ${CMAKE_BINARY_DIR}/bin/test_debufr.sh "${CMAKE_BINARY_DIR}/utils/debufr.x -t testfiles/data -f bufrtab.031 -c" "testfiles/data/debufr_2" "testrun/debufr_2.run" "testfiles/testoutput/debufr_2.out"
  )

# Test gettab.x utility
add_test(NAME test_gettab
  COMMAND ${CMAKE_BINARY_DIR}/bin/test_gettab.sh "${CMAKE_BINARY_DIR}/utils/gettab.x testfiles/data/1bamua" "testrun/1bamua.table.run" "testfiles/testoutput/1bamua.table.out"
  )

# Test split_by_subset.x utility
add_test(NAME test_split_by_subset
  COMMAND ${CMAKE_BINARY_DIR}/bin/test_split_by_subset.sh "${CMAKE_BINARY_DIR}/utils/split_by_subset.x testfiles/data/satwndbufr" "testfiles/testoutput/satwndbufr_split"
  )
