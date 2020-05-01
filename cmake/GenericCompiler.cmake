set (GEOS_Fortran_FLAGS_DEBUG   "${GEOS_Fortran_Debug_Flags} ${common_Fortran_flags} ${GEOS_Fortran_Debug_FPE_Flags} ${ALIGNCOM}")
set (GEOS_Fortran_FLAGS_RELEASE "${GEOS_Fortran_Release_Flags} ${common_Fortran_flags} ${GEOS_Fortran_Release_FPE_Flags} ${ALIGNCOM}")
set (GEOS_Fortran_FLAGS_VECT    "${GEOS_Fortran_Vect_Flags} ${common_Fortran_flags} ${GEOS_Fortran_Vect_FPE_Flags} ${ALIGNCOM}")

set (CMAKE_Fortran_FLAGS_DEBUG   "${GEOS_Fortran_FLAGS_DEBUG}" CACHE STRING "Debug Fortran flags" FORCE )
set (CMAKE_Fortran_FLAGS_RELEASE "${GEOS_Fortran_FLAGS_RELEASE}" CACHE STRING "Release Fortran flags" FORCE )
