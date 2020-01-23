cmake_minimum_required(VERSION 3.15)
project(bufr VERSION 11.3.0)
set(${PROJECT_NAME}_VERSION ${PROJECT_VERSION} CACHE INTERNAL "${PROJECT_NAME} version number")
enable_language (Fortran)

if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
    "Choose the type of build, options are: PRODUCTION Debug Release."
    FORCE)
endif()

if(CMAKE_CXX_COMPILER_ID STREQUAL "Intel")
  set(IntelComp true )
elseif(CMAKE_CXX_COMPILER_ID MATCHES "GNU*" OR CMAKE_CXX_COMPILER_ID MATCHES "Clang*")
  set(GNUComp true )
elseif(CMAKE_CXX_COMPILER_ID MATCHES "pgc*")
  set(PGIComp true )
endif()

STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "RelWithDebInfo" BUILD_RELEASE)
STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "RELEASE" BUILD_RELEASE)
STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "PRODUCTION" BUILD_PRODUCTION)
STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "DEBUG" BUILD_DEBUG)

option(GSIBUILD "Build only libs for GSI" ON)

set(definitions "UNDERSCORE" "NFILES=32" "MAXCD=250" "MAXNC=600" "MXNAF=3")

if( (BUILD_RELEASE) OR (BUILD_PRODUCTION) )
  if(IntelComp)
    #shared with C and Fortran
    set(shared_flags "-g" "-traceback" "-O3" "-axCORE-AVX2")

    set(c_DA_allocation_def "DYNAMIC_ALLOCATION")
    set(c_4_DA_flags)    
    set(c_8_DA_definitions "F77_INTSIZE_8")
    set(c_d_DA_flags)
    
    set(fortran_4_DA_flags)
    set(fortran_8_DA_flags "-r8" "-i8")
    set(fortran_d_DA_flags "-r8")
    
    set(c_nonDA_allocation_def "STATIC_ALLOCATION")
    set(c_4_flags)
    set(c_8_flags)
    set(c_8_definitions "F77_INTSIZE_8")
    set(c_d_flags)
    set(c_SS_flags "-mcmodel=medium" "-shared")

    set(fortran_4_flags)
    set(fortran_8_flags "-r8 -i8 ")
    set(fortran_d_flags "-r8")
    set(fortran_SS_flags "-mcmodel=medium" "-shared")

  elseif(GNUComp)
    set(allocation_def "DYNAMIC_ALLOCATION")
    set(shared_flags "-O3" "-ggdb" "-Wall")

    set(c_d_flags)
    set(c_4_flags)
    set(c_8_flags)

    set(shared_fortran_flags "-funroll-loops")
    set(fortran_4_flags)
    set(fortran_d_flags "-fdefault-real-8")
    set(fortran_8_flags "-fdefault-integer-8" "-fdefault-real-8")
  else()
    message("unknown compiler!")
    exit()
  endif()
endif()


file(GLOB fortran_src ${CMAKE_CURRENT_SOURCE_DIR}/src/*.f ${CMAKE_CURRENT_SOURCE_DIR}/src/*.F)
file(GLOB c_src ${CMAKE_CURRENT_SOURCE_DIR}/src/*.c)

if(GSIBUILD)
  set(kinds "4" "d" "8")
else()
  set(kinds "4" "d" "8" "SS" "4_DA" "8_DA" "d_DA")
endif()

foreach(kind ${kinds})
  set(lib_name ${PROJECT_NAME}_${kind})
  set(versioned_lib_name ${PROJECT_NAME}_v${PROJECT_VERSION}_${kind})
  add_library(${lib_name} STATIC ${fortran_src} ${c_src})
  set_target_properties(${lib_name} PROPERTIES OUTPUT_NAME "${versioned_lib_name}")

  # different compiler definitions for Intel in DA vs non-DA
  # -DDYNAMIC_ALLOCATION when compiled with DA and -DSTATIC_ALLOCATION
  # check current kind and if it has 'DA' in it then set compiler def
 
  if(IntelComp)
    string(FIND ${kind} "DA" isDA)
    if(isDA GREATER_EQUAL 0)
      set(allocation_def ${c_DA_allocation_def})
    else()
      set(allocation_def ${c_nonDA_allocation_def})
    endif()
  endif()

  target_compile_definitions(${lib_name} PRIVATE "${allocation_def}")
  
  set_source_files_properties(${c_src} PROPERTIES
    COMPILE_OPTIONS "${shared_flags};${c_${kind}_flags}")
  set_source_files_properties(${c_src} PROPERTIES
    COMPILE_DEFINITIONS "${c_definitions};${c_${kind}_definitions}")
  
  set_source_files_properties(${fortran_src} PROPERTIES
    COMPILE_OPTIONS "${shared_flags};${shared_fortran_flags};${fortran_${kind}_flags}")
  
  set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include_${kind}")
  set_target_properties(${lib_name} PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")
  
  target_include_directories(${lib_name} PUBLIC
    $<BUILD_INTERFACE:${module_dir}>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_PREFIX}/include_${kind}}>)

  install(TARGETS ${lib_name}
    EXPORT ${PROJECT_NAME}-config
    RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX}/bin
    LIBRARY DESTINATION ${CMAKE_INSTALL_PREFIX}/lib
    ARCHIVE DESTINATION ${CMAKE_INSTALL_PREFIX}/lib) 
  install(DIRECTORY ${module_dir} DESTINATION ${CMAKE_INSTALL_PREFIX})
  install(EXPORT ${PROJECT_NAME}-config DESTINATION ${CMAKE_INSTALL_PREFIX})
endforeach()

