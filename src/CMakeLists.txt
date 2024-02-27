# This is the CMake build file for the src directory of NCEPLIBS-bufr.
#
# Kyle Gerheiser, Jeff Ator
set(CMAKE_INCLUDE_CURRENT_DIR ON)

list(APPEND fortran_src
  modules_vars.F90 modules_arrs.F90
  adn30.f atrcpt.f blocks.F90 borts.F90 cadn30.f capit.f chekstab.f cktaba.f closmg.f
  cmpmsg.f cmsgini.f cnved4.f codflg.f conwin.f copybf.f copymg.f copysb.f cpbfdx.f cpdxmm.f cpymem.f
  cpyupd.f datebf.f datelen.f digit.f drfini.f drstpl.f dumpbf.f dxdump.f dxinit.f dxmini.f elemdx.f
  errwrt.F90 exitbufr.f fstag.f getabdb.f getbmiss.f getcfmng.f getlens.f getntbe.f gets1loc.f gettagpr.f
  gettagre.f gettbh.f getvalnb.f getwin.f hold4wlc.f i4dy.f ibfms.f icbfms.f icmpdx.f icopysb.f
  idn30.f idxmsg.f ifbget.f ifxy.f igetdate.f igetfxy.f igetmxby.f igetntbi.f igetntbl.f igetprm.f
  igetrfel.f igetsc.f igettdi.f imrkopr.f inctab.f invcon.f invmrg.f invtag.f invwin.f iok2cpy.f
  iokoper.f ipkm.f ipks.f ireadmg.f ireadmm.f ireadns.f ireadsb.f ishrdx.f isize.f istdesc.f iupb.f
  iupbs01.f iupbs3.f iupm.f iupvs01.f jstnum.f lcmgdf.f lmsg.f lstjpb.f makestab.f maxout.f
  mesgbc.f mesgbf.f minimg.f mrginv.f msgfull.f msgini.f msgupd.f msgwrt.f mtfnam.f mtinfo.f mvb.f
  nemdefs.f nemock.f nemspecs.f nemtab.f nemtba.f nemtbax.f nemtbb.f nemtbd.f nenubd.f nevn.f newwin.f
  nmsub.f nmwrd.f numbck.f numtab.f numtbd.f nvnwin.f nwords.f nxtwin.f openbt.f openmb.f openmg.f pad.f
  padmsg.f parstr.f parusr.f parutg.f pkb.f pkbs1.f pkc.F90 pkftbv.f pktdd.f posapx.f rcstpl.f rdbfdx.f
  rdcmps.f rdmemm.f rdmems.f rdmgsb.f rdmsgw.F90 rdmtbb.f rdmtbd.f rdmtbf.f rdtree.f rdusdx.f readdx.f
  readerme.f readlc.f readmg.f readmm.f readns.f reads3.f readsb.f rewnbf.f rsvfvm.f rtrcpt.f
  rtrcptb.f seqsdx.f setblock.F90 setbmiss.f setvalnb.f sntbbe.f sntbde.f sntbfe.f status.f stbfdx.f
  stdmsg.f stndrd.f stntbi.f stntbia.f strbtm.f strcln.f strcpt.f string.f strnum.F90 strsuc.F90 tabent.f
  tabsub.f trybump.f ufbcnt.f ufbcpy.f ufbcup.f ufbdmp.f ufbevn.f ufbget.f ufbin3.f ufbint.f ufbinx.f
  ufbmem.f ufbmex.f ufbmms.f ufbmns.f ufbovr.f ufbpos.f ufbqcd.f ufbqcp.f ufbrep.f ufbrms.f ufbrp.f
  ufbrw.f ufbseq.f ufbsp.f ufbstp.f ufbtab.f ufbtam.f ufdump.f upb.f upbb.f upc.F90 upds3.f upftbv.f
  ups.f uptdd.f usrtpl.f wrcmps.f wrdxtb.f writcp.f writdx.f writlc.f writsa.f writsb.f
  wrtree.f wtstat.f arallocf.f ardllocf.f closbf.f ireadmt.f irev.F90 isetprm.f openbf.f pkvs01.f
  fortran_open.F90 fortran_close.F90 bufr_interface.F90 pkb8.F90 upb8.F90 up8.F90 setim8b.F90
  bufr_c2f_interface.F90 x48.F90 x84.F90 bufrlib.F90)

list(APPEND c_src
  arallocc.c ardllocc.c cpmstabs.c cread.c crwbmg.c cfe.c
  icvidx.c restd.c stseq.c wrdesc.c)

list(APPEND c_hdr
  cread.h mstabs.h bufr_interface.h)

# Create the bvers.f file and bfrini.F90 files
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/bvers.f.in ${CMAKE_CURRENT_BINARY_DIR}/bvers.f @ONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/bfrini.F90.in ${CMAKE_CURRENT_BINARY_DIR}/bfrini.F90 @ONLY)
list(APPEND fortran_src ${CMAKE_CURRENT_BINARY_DIR}/bvers.f ${CMAKE_CURRENT_BINARY_DIR}/bfrini.F90)

# Create the bufrlib.h file
foreach(_var IN ITEMS maxnc mxnaf maxcd nfiles)
  file(STRINGS modules_vars.F90 _${_var}_tempstr REGEX "integer[a-z ,]*:: ${_var}")
  if(_${_var}_tempstr MATCHES "${_var} = ([0-9]+)")
    string(TOUPPER ${_var} _varuc)
    set(${_varuc} ${CMAKE_MATCH_1})
  else()
    message(FATAL_ERROR "Unable to parse variable ${_var} value from file: modules_vars.F90")
  endif()
endforeach()
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/bufrlib.h.in ${CMAKE_CURRENT_BINARY_DIR}/bufrlib.h @ONLY)
list(APPEND c_hdr ${CMAKE_CURRENT_BINARY_DIR}/bufrlib.h)

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
