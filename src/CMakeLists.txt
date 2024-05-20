# This is the CMake build file for the src directory of NCEPLIBS-bufr.
#
# Kyle Gerheiser, Jeff Ator
set(CMAKE_INCLUDE_CURRENT_DIR ON)

list(APPEND fortran_src
  modules_vars.F90 modules_arrs.F90 bitmaps.F90 blocks.F90 borts.F90 cktaba.f
  compress.F90 conwin.f copydata.F90 dumpdata.F90 dxtable.F90
  errwrt.F90 fstag.f getabdb.f gettagpr.f
  getwin.f ifbget.f fxy.F90 iokoper.f ipks.f
  invcon.f invmrg.f invtag.f invwin.f
  jumplink.F90 mastertable.F90 missing.F90 mesgbc.f mesgbf.f mrginv.f
  nevn.f newwin.f nvnwin.f nwords.f nxtwin.f openbt.F90
  parstr.f parusr.f parutg.f ftbvs.F90 pktdd.f posapx.f rcstpl.f
  readwritemg.F90 readwritesb.F90 readwriteval.F90 rewnbf.f rsvfvm.f s013vals.F90 tankrcpt.F90
  standard.F90 strcln.f string.f
  trybump.f ufbcup.f ufbevn.f ufbget.f ufbin3.f ufbinx.f
  memmsgs.F90 ufbovr.f ufbqcd.f ufbqcp.f
  ufbtab.f ciencode.F90 cidecode.F90
  ups.f uptdd.f usrtpl.f arallocf.F90 openclosebf.F90
  bufr_interface.F90 bufr_c2f_interface.F90 x4884.F90 bufrlib.F90)

list(APPEND c_src
  arallocc.c cpmstabs.c cread.c crwbmg.c cfe.c icvidx.c restd.c stseq.c)

list(APPEND c_hdr
  cread.h mstabs.h bufr_interface.h bufrlib.h)

# Create the misc.F90 file and bfrini.F90 files
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/misc.F90.in ${CMAKE_CURRENT_BINARY_DIR}/misc.F90 @ONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/bfrini.F90.in ${CMAKE_CURRENT_BINARY_DIR}/bfrini.F90 @ONLY)
list(APPEND fortran_src ${CMAKE_CURRENT_BINARY_DIR}/misc.F90 ${CMAKE_CURRENT_BINARY_DIR}/bfrini.F90)

include(TestBigEndian)
test_big_endian(IS_BIG_ENDIAN)
if(IS_BIG_ENDIAN)
  list(APPEND endian_def BIG_ENDIAN)
else()
  list(APPEND endian_def LITTLE_ENDIAN)
endif()

foreach(kind ${kinds})
  set(lib_name ${PROJECT_NAME}_${kind})
  set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include/bufr_${kind}")

  add_library(${lib_name}_f OBJECT ${fortran_src})
  set_target_properties(${lib_name}_f PROPERTIES COMPILE_FLAGS
                                                 "${fortran_${kind}_flags}")
  target_compile_definitions(${lib_name}_f PRIVATE "${endian_def}")

  add_library(${lib_name}_c OBJECT ${c_src})
  set_target_properties(${lib_name}_c PROPERTIES COMPILE_FLAGS
                                                 "${c_${kind}_flags}")
  target_compile_definitions(${lib_name}_c PRIVATE "${c_${kind}_defs}")

  set_target_properties(${lib_name}_f PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")

  add_library(${lib_name} $<TARGET_OBJECTS:${lib_name}_f>
                          $<TARGET_OBJECTS:${lib_name}_c>)
  add_library(${PROJECT_NAME}::${lib_name} ALIAS ${lib_name})

  target_include_directories(${lib_name} PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>)

  target_include_directories(${lib_name} INTERFACE
    $<BUILD_INTERFACE:${module_dir}>
    $<INSTALL_INTERFACE:include/bufr_${kind}>)

  list(APPEND LIB_TARGETS ${lib_name})
  install(DIRECTORY ${module_dir} DESTINATION ${CMAKE_INSTALL_PREFIX}/include)
  install(FILES ${c_hdr} DESTINATION ${CMAKE_INSTALL_PREFIX}/include/bufr_${kind})
endforeach()

install(
  TARGETS ${LIB_TARGETS}
  EXPORT ${PROJECT_NAME}Exports
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})

# Package config
include(CMakePackageConfigHelpers)
set(CONFIG_INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME})

export(EXPORT ${PROJECT_NAME}Exports
  NAMESPACE ${PROJECT_NAME}::
  FILE ${PROJECT_NAME}-targets.cmake)

configure_package_config_file(
  ${PROJECT_SOURCE_DIR}/cmake/PackageConfig.cmake.in ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config.cmake
  INSTALL_DESTINATION ${CONFIG_INSTALL_DESTINATION})
install(FILES ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config.cmake
  DESTINATION ${CONFIG_INSTALL_DESTINATION})

write_basic_package_version_file(
  ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY AnyNewerVersion)
install(FILES ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
  DESTINATION ${CONFIG_INSTALL_DESTINATION})

install(EXPORT ${PROJECT_NAME}Exports
  NAMESPACE ${PROJECT_NAME}::
  FILE ${PROJECT_NAME}-targets.cmake
  DESTINATION ${CONFIG_INSTALL_DESTINATION})
