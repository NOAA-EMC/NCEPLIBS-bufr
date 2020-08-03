# Fetch test data from: https://ftp.emc.ncep.noaa.gov/static_files/public/bufr.tar
set(BUFR_URL "https://ftp.emc.ncep.noaa.gov/static_files/public")
set(BUFR_TAR "bufr.tar")

find_program(CURL_pgm curl)
mark_as_advanced(CURL_pgm)

if(CURL_pgm)

  add_custom_command(
    OUTPUT ${BUFR_TAR}
    COMMENT "(curl) downloading ${BUFR_URL}/${BUFR_TAR}"
    COMMAND ${CURL_pgm}
            --silent --show-error --fail
            --retry 0
            --connect-timeout 30
            --output ${BUFR_TAR}
            ${BUFR_URL}/${BUFR_TAR} )

else()

  find_program(WGET_pgm wget)
  mark_as_advanced(WGET_pgm)

  if(WGET_pgm)

    add_custom_command(
      OUTPUT ${BUFR_TAR}
      COMMENT "(wget) downloading ${BUFR_URL}/${BUFR_TAR}"
      COMMAND ${WGET_pgm}
              -nv
              -t 0
              -T 30
              -O ${BUFR_TAR}
              ${BUFR_URL}/${BUFR_TAR} )

  else()

    message(SEND_ERROR "Could not find curl or wget.\nCannot download test data from server.\nPlease obtain the test data by other means and place it in the build directory!")
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

list(APPEND test_IN_srcs
  test_IN_1.f
  test_IN_2.f
  test_IN_3.f
  test_IN_4.f
  test_IN_5.f
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

# IN tests data
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

# OUT tests data
list(APPEND test_OUT_bufr_data
  OUT_1 OUT_2 OUT_3
  OUT_1_bufrtab OUT_2_bufrtab OUT_3_bufrtab
  OUT_2_preAPX
  )

# Install test_wrapper.sh
execute_process( COMMAND ${CMAKE_COMMAND} -E copy
  ${CMAKE_CURRENT_SOURCE_DIR}/test_wrapper.sh
  ${CMAKE_BINARY_DIR}/bin/test_wrapper.sh )

function(bufr_add_test TESTNAME EXENAME)
  add_test(NAME ${TESTNAME}
           COMMAND ${CMAKE_BINARY_DIR}/bin/test_wrapper.sh ${EXENAME})
endfunction()

function(bufr_add_test_preAPX TESTNAME EXENAME)
  add_test(NAME ${TESTNAME}
           COMMAND ${CMAKE_BINARY_DIR}/bin/test_wrapper.sh ${EXENAME} "Y")
endfunction()

# Tests are only guarenteed to work for _4 libraries.
# Both GNU and Intel support DYNAMIC_ALLOCATION
list(APPEND test_kinds "4_DA")
# Only Intel supports STATIC_ALLOCATION
if(CMAKE_C_COMPILER_ID MATCHES "^(Intel)$")
  list(APPEND test_kinds "4")
endif()

# IN tests
foreach(test_src ${test_IN_srcs})
  string(REPLACE ".f" "" testPref ${test_src})
  foreach(kind ${test_kinds})
    set(test     ${testPref}_${kind})
    set(test_exe ${test}.x)
    add_executable(${test_exe} ${test_src})
    set_target_properties(${test_exe} PROPERTIES COMPILE_FLAGS
                                                 "${fortran_${kind}_flags}")
    add_dependencies(${test_exe} bufr_${kind})
    target_link_libraries(${test_exe} PUBLIC bufr::bufr_${kind})
    add_test(NAME ${test} COMMAND ${test_exe})
  endforeach()
endforeach()

# OUT_1 tests
foreach(test_src ${test_OUT_1_srcs})
  string(REPLACE ".f" "" testPref ${test_src})
  foreach(kind ${test_kinds})
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

# OUT_2 tests
foreach(test_src ${test_OUT_2_srcs})
  string(REPLACE ".f" "" testPref ${test_src})
  foreach(kind ${test_kinds})
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

# OUT_3 tests only run with DA library and Intel
if(CMAKE_C_COMPILER_ID MATCHES "^(Intel)$")
foreach(test_src ${test_OUT_3_srcs})
  string(REPLACE ".f" "" testPref ${test_src})
  foreach(kind ${test_kinds})
    if(${kind} MATCHES "^([4|8|d]_DA)$")
      set(test     ${testPref}_${kind})
      set(test_exe ${test}.x)
      add_executable(${test_exe} ${test_src})
      set_target_properties(${test_exe} PROPERTIES COMPILE_FLAGS
                                                   "${fortran_${kind}_flags}")
      add_dependencies(${test_exe} bufr_${kind})
      target_link_libraries(${test_exe} PRIVATE bufr::bufr_${kind})
      bufr_add_test(${test} ${test_exe})
    endif()
  endforeach()
endforeach()
endif()
