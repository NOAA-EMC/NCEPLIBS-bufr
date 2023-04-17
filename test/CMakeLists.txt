# This is the cmake build file for the tests directory of the
# NCEPLIBS-bufr project.
#
# Rahul Mahajan, Jeff Ator, Ed Hartnett

# These are the kinds of tests we're going to build and run.
list(APPEND test_kinds 4 8 d)

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

# This function builds, links, and runs an ordinary C test.
function(create_c_test_plain name)
  set(testID "${name}")
  add_executable(${testID} ${name}.c)
  target_compile_definitions(${testID} PUBLIC -DKIND_${kind})
  target_link_libraries(${testID} PRIVATE bufr::bufr_4)
  target_include_directories(${testID} PRIVATE "${CMAKE_BINARY_DIR}/src")
  add_dependencies(${testID} bufr_4)
  add_test(NAME ${testID} COMMAND ${testID})
endfunction()

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

# This function builds, links, and runs an ordinary Fortran test.
function(create_test_plain name kind)
  set(testID "${name}_${kind}")
  add_executable(${testID} ${name}.F90)
  set_target_properties(${testID} PROPERTIES COMPILE_FLAGS "${fortran_${kind}_flags}")
  target_compile_definitions(${testID} PUBLIC -DKIND_${kind})
  target_link_libraries(${testID} PRIVATE bufr_4)
  add_dependencies(${testID} bufr_4)
  add_test(NAME ${testID} COMMAND ${testID})
endfunction()

# This function builds, links, and runs an intest or outtest program.
function(create_test name kind num)
  set(testID "${name}${num}_${kind}")
  add_executable(${testID} ${name}${num}.F90)
  set_target_properties(${testID} PROPERTIES COMPILE_FLAGS "${fortran_${kind}_flags}")
  target_compile_definitions(${testID} PUBLIC -DKIND_${kind})
  target_link_libraries(${testID} PRIVATE bufr_4)
  add_dependencies(${testID} bufr_4)
  if(${name} MATCHES "^intest")
    add_test(NAME ${testID} COMMAND ${testID})
  else()
    add_test(NAME ${testID} COMMAND ${CMAKE_BINARY_DIR}/bin/test_outtest.sh ${testID} ${num})
  endif()
endfunction()

# This is for testing bort().
foreach(kind ${test_kinds})
  add_executable(test_bort_${kind} test_bort.F90)
  set_target_properties(test_bort_${kind} PROPERTIES COMPILE_FLAGS "${fortran_${kind}_flags}")
  # Need to turn off a warning for test_bort, so it can do stupid things needed to fail.
  set_target_properties(test_bort_${kind} PROPERTIES COMPILE_FLAGS -Wno-character-truncation)
  target_compile_definitions(test_bort_${kind} PUBLIC -DKIND_${kind})
  target_link_libraries(test_bort_${kind} PRIVATE bufr_4)
  add_dependencies(test_bort_${kind} bufr_4)
endforeach()

# Test the borts. This runs test script run_test_bort.sh, which
# repeatedly calls program test_bort, and expects each time for the
# program to be aborted.
file(COPY "${CMAKE_SOURCE_DIR}/test/run_test_bort.sh"
    DESTINATION ${CMAKE_BINARY_DIR}/test
    FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
add_test(NAME run_test_bort COMMAND ${CMAKE_BINARY_DIR}/test/run_test_bort.sh)

# These are the C API tests.
list(APPEND test_c_interface_srcs
  test_c_interface.c
  test_c_interface_2.c
)

# test_c_misc test.
set(test_name test_c_misc)
add_executable(${test_name} ${test_name}.c)
add_dependencies(${test_name} bufr_4)
target_link_libraries(${test_name} PRIVATE bufr::bufr_4)
add_test(NAME ${test_name} COMMAND ${CMAKE_BINARY_DIR}/test/${test_name})

create_c_test_plain(test_c_crwbmg)

# Install testing scripts
list(APPEND test_scripts
  test_outtest.sh
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

# Create Fortran API tests, plus any other tests we want to run.
foreach(kind ${test_kinds})
  create_test_plain(test_misc ${kind})
  foreach(innum RANGE 1 10)
    create_test(intest ${kind} ${innum})
  endforeach()
  foreach(outnum RANGE 1 9)
    create_test(outtest ${kind} ${outnum})
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

# These are the utilities to be tested.
list(APPEND utils
  debufr
  gettab
  sinv
  binv
  split_by_subset
  readbp
  readmp
  cmpbqm
  xbfmg
)

# utility tests
foreach(util ${utils})
  set(test test_${util})
  add_test(NAME ${test} COMMAND ${CMAKE_BINARY_DIR}/bin/${test}.sh)
endforeach()
