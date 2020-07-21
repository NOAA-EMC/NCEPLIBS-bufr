if(CMAKE_C_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "-g -traceback")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")

elseif(CMAKE_C_COMPILER_ID MATCHES "^(GNU|Clang|AppleClang)$")
  set(CMAKE_C_FLAGS_DEBUG "-ggdb -Wall -O0")
  set(CMAKE_Fortran_FLAGS "-g -fbacktrace")
  set(CMAKE_Fortran_FLAGS_DEBUG "-ggdb -Wall -O0")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
endif()

set ( CMAKE_CXX_STANDARD 14 )

add_library("bufrlib_c" STATIC bufrlib_c_wrapper.f90)
target_link_libraries("bufrlib_c" bufr_4_DA)

include_directories(c_wrapper)

add_executable("bufrlib_c_test" main.cpp bufrlib_c_wrapper.f90)
target_link_libraries("bufrlib_c_test" bufr_4_DA)