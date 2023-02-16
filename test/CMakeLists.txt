# This is the cmake build file for the tests directory of the
# NCEPLIBS-bufr project.
#
# Rahul Mahajan, Jeff Ator, Ed Hartnett

# This function builds, links, and runs a test program.
function(create_test name kind)
  add_executable(${name}_${kind} ${name}.F90)
  set_target_properties(${name}_${kind} PROPERTIES COMPILE_FLAGS "${fortran_${kind}_flags}")
  target_compile_definitions(${name}_${kind} PUBLIC -DKIND_${kind})
  target_link_libraries(${name}_${kind} PRIVATE bufr_4)
  add_dependencies(${name}_${kind} bufr_4)
  add_test(NAME ${name}_${kind} COMMAND ${name}_${kind})
endfunction()

# Fetch test data from: https://ftp.emc.ncep.noaa.gov/static_files/public/bufr.tar
set(BUFR_URL "https://ftp.emc.ncep.noaa.gov/static_files/public")
if(${PROJECT_VERSION} VERSION_GREATER_EQUAL 11.6.0)
  set(BUFR_TAR "bufr-11.6.0.tgz")
else()
  set(BUFR_TAR "bufr.tar")
endif()

# If the TEST_FILE_DIR was specified, look for our test data files
# there before FTPing them. Developers can keep all test files on
# their machines, and save the time of downloading them every time.
if(NOT ${TEST_FILE_DIR} STREQUAL ".")
  if (EXISTS ${TEST_FILE_DIR}/${BUFR_TAR})
    message(STATUS "Copying file ${TEST_FILE_DIR}/${BUFR_TAR} to test data directory.")
    FILE(COPY ${TEST_FILE_DIR}/${BUFR_TAR}
      DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
  endif()
endif()

# Get the test data if we don't have it.
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

add_custom_target(get_bufr_test_data ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${BUFR_TAR})
add_custom_command(
  TARGET get_bufr_test_data
  POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E chdir ${CMAKE_CURRENT_BINARY_DIR} tar xzf ${BUFR_TAR}
  COMMAND ${CMAKE_COMMAND} -E chdir ${CMAKE_CURRENT_BINARY_DIR} rm -rf testfiles
  COMMAND ${CMAKE_COMMAND} -E chdir ${CMAKE_CURRENT_BINARY_DIR} mv bufr/testfiles testfiles
  COMMAND ${CMAKE_COMMAND} -E chdir ${CMAKE_CURRENT_BINARY_DIR} rm -rf bufr)

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testrun)

list(APPEND test_IN_srcs
  test_IN_3.F
  test_IN_6.F
  test_IN_7.F
  test_IN_8.F90
  )

list(APPEND test_OUT_2_srcs
  test_OUT_2.F
  )

list(APPEND test_OUT_other_srcs
  test_OUT_1.F
  test_OUT_3.F
  test_OUT_4.F
  test_OUT_5.F
  test_OUT_6.F
  test_OUT_7.F90
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
  test_binv.sh
  test_sinv.sh
  test_readbp.sh
  test_readmp.sh
  test_cmpbqm.sh
  test_split_by_subset.sh
  test_debufr.sh
  test_xbfmg.sh
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

list(APPEND test_kinds 4 8 d)

set(fortran_8_defs INTSIZE_8)

# Add tests which don't require output log processing.
foreach(kind ${test_kinds})
  create_test(intest1 ${kind})
  create_test(intest2 ${kind})
  create_test(intest4 ${kind})
endforeach()

# IN tests
foreach(test_src ${test_IN_srcs})
  string(REGEX REPLACE "(.F90|.F)" "" testPref ${test_src})
  foreach(kind ${test_kinds})
    set(test     ${testPref}_${kind})
    set(test_exe ${test}.x)
    add_executable(${test_exe} ${test_src})
    set_target_properties(${test_exe} PROPERTIES COMPILE_FLAGS
                                                 "${fortran_${kind}_flags}")
    target_compile_definitions(${test_exe} PRIVATE "${fortran_${kind}_defs}")
    add_dependencies(${test_exe} bufr_4)
    target_link_libraries(${test_exe} PUBLIC bufr::bufr_4)
    add_test(NAME ${test} COMMAND ${CMAKE_BINARY_DIR}/bin/test_wrapper_IN.sh ${test_exe})
  endforeach()
endforeach()

# OUT_2 tests; these use the bufr_add_test_preAPX function
foreach(test_src ${test_OUT_2_srcs})
  string(REPLACE ".F" "" testPref ${test_src})
  foreach(kind ${test_kinds})
    set(test     ${testPref}_${kind})
    set(test_exe ${test}.x)
    add_executable(${test_exe} ${test_src})
    set_target_properties(${test_exe} PROPERTIES COMPILE_FLAGS
                                                 "${fortran_${kind}_flags}")
    target_compile_definitions(${test_exe} PRIVATE "${fortran_${kind}_defs}")
    add_dependencies(${test_exe} bufr_4)
    target_link_libraries(${test_exe} PRIVATE bufr::bufr_4)
    bufr_add_test_preAPX(${test} ${test_exe})
  endforeach()
endforeach()

# all other OUT tests
foreach(test_src ${test_OUT_other_srcs})
  string(REGEX REPLACE "(.F90|.F)" "" testPref ${test_src})
  foreach(kind ${test_kinds})
    set(test     ${testPref}_${kind})
    set(test_exe ${test}.x)
    add_executable(${test_exe} ${test_src})
    set_target_properties(${test_exe} PROPERTIES COMPILE_FLAGS
                                                 "${fortran_${kind}_flags}")
    target_compile_definitions(${test_exe} PRIVATE "${fortran_${kind}_defs}")
    add_dependencies(${test_exe} bufr_4)
    target_link_libraries(${test_exe} PRIVATE bufr::bufr_4)
    bufr_add_test(${test} ${test_exe})
  endforeach()
endforeach()

# c_interface tests
foreach(test_src ${test_c_interface_srcs})
  string(REPLACE ".c" "" testPref ${test_src})
  set(test     ${testPref}_4)
  set(test_exe ${test}.x)
  add_executable(${test_exe} ${test_src})
  add_dependencies(${test_exe} bufr_4)
  target_link_libraries(${test_exe} PRIVATE bufr::bufr_4 m)
  add_test(NAME ${test} COMMAND ${CMAKE_BINARY_DIR}/test/${test_exe})
endforeach()

# Test debufr utility
set(db_flags_1 "-t ../tables")
set(db_flags_2 "-t testfiles/data -f bufrtab.031 -c")
foreach(db_num RANGE 1 2)
  set(db_case debufr_${db_num})
  add_test(NAME test_${db_case} COMMAND ${CMAKE_BINARY_DIR}/bin/test_debufr.sh
           "${CMAKE_BINARY_DIR}/utils/debufr ${db_flags_${db_num}}" "testfiles/data/${db_case}"
           "testrun/${db_case}.run" "testfiles/testoutput/${db_case}.out")
endforeach()

# Test gettab utility
add_test(NAME test_gettab
  COMMAND ${CMAKE_BINARY_DIR}/bin/test_gettab.sh "${CMAKE_BINARY_DIR}/utils/gettab testfiles/data/1bamua" "testrun/1bamua.table.run" "testfiles/testoutput/1bamua.table.out"
  )

# Test sinv utility
add_test(NAME test_sinv 
  COMMAND ${CMAKE_BINARY_DIR}/bin/test_sinv.sh "${CMAKE_BINARY_DIR}/utils/sinv testfiles/data/satwndbufr ../tables" "testrun/sinv.out"  "testfiles/testoutput/sinv.out"
  )

# Test binv utility
add_test(NAME test_binv  
  COMMAND ${CMAKE_BINARY_DIR}/bin/test_binv.sh "${CMAKE_BINARY_DIR}/utils/binv testfiles/data/satwndbufr" "testrun/binv.out"  "testfiles/testoutput/binv.out"
  )

# Test split_by_subset utility
add_test(NAME test_split_by_subset
  COMMAND ${CMAKE_BINARY_DIR}/bin/test_split_by_subset.sh "${CMAKE_BINARY_DIR}/utils/split_by_subset testfiles/data/satwndbufr" "testfiles/testoutput/satwndbufr_split"
  )

# Test readbp utility
add_test(NAME test_readbp 
  COMMAND ${CMAKE_BINARY_DIR}/bin/test_readbp.sh "${CMAKE_BINARY_DIR}/utils/readbp -n testfiles/data/prepbufr2.ref" "testrun/readbp.out" "testfiles/testoutput/readbp.out"
  )

# Test readmp utility
add_test(NAME test_readmp 
  COMMAND ${CMAKE_BINARY_DIR}/bin/test_readmp.sh "${CMAKE_BINARY_DIR}/utils/readmp testfiles/data/prepbufr2.ref q" "testrun/readmp.out" "testfiles/testoutput/readmp.out"
  )

# Test cmpbqm utility
add_test(NAME test_cmpbqm 
  COMMAND ${CMAKE_BINARY_DIR}/bin/test_cmpbqm.sh "${CMAKE_BINARY_DIR}/utils/cmpbqm testfiles/data/prepbufr" "testrun/cmpbqm.out" "testfiles/testoutput/cmpbqm.out"
  )

# Test xbfmg utility
set(xb_flags_1 "-g")
set(xb_flags_2 "")
foreach(xb_num RANGE 1 2)
  set(xb_case xbfmg_${xb_num})
  add_test(NAME test_${xb_case} COMMAND ${CMAKE_BINARY_DIR}/bin/test_xbfmg.sh
           "${CMAKE_BINARY_DIR}/utils/xbfmg ${xb_flags_${xb_num}}" "testfiles/data"
           "testfiles/testoutput/xbfmg" "${xb_case}")
endforeach()

