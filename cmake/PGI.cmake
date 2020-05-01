if (CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 17.10)
  message(FATAL_ERROR "${CMAKE_Fortran_COMPILER_ID} version must be at least 17.10!")
endif()

set (FREAL8 "-r8")
set (FINT8 "-fdefault-integer-8")
set (PP    "-Mpreprocess")
set (MISMATCH "")
# This doesn't seem to work at the moment
set (EXTENDED_SOURCE "-Mextend")
set (FIXED_SOURCE "-Mfixed")
set (DISABLE_FIELD_WIDTH_WARNING)
set (BYTERECLEN)
set (BIG_ENDIAN "-Mbyteswapio")
set (CRAY_POINTER "")
set (BACKSLASH_STRING "-Mbackslash")
set (TRACEBACK "-traceback")

####################################################

add_definitions(-D__PGI) # Needed for LANL CICE
add_definitions(-D__PGI) # Needed for LANL CICE

set(CMAKE_EXE_LINKER_FLAGS "-pgc++libs -tp=px-64" CACHE INTERNAL "" FORCE)

# Common Fortran Flags
# --------------------
set (common_Fortran_flags "${BACKSLASH_STRING}")
set (common_Fortran_fpe_flags "-Ktrap=fp -tp=px-64")

# GEOS Debug
# ----------
set (GEOS_Fortran_Debug_Flags "-O0 -g -Kieee -Minfo=all -Mbounds ${TRACEBACK} -Mchkfpstk -Mchkstk -Mdepchk")
set (GEOS_Fortran_Debug_FPE_Flags "${common_Fortran_fpe_flags}")

# GEOS Release
# ------------
set (GEOS_Fortran_Release_Flags "-fast -Kieee -g")
set (GEOS_Fortran_Release_FPE_Flags "${common_Fortran_fpe_flags}")

# NOTE: No idea how to handle GPU with CMake

# GEOS Vectorize
# --------------
# Until good options can be found, make vectorize equal common flags
set (GEOS_Fortran_Vect_Flags ${GEOS_Fortran_Release_Flags})
set (GEOS_Fortran_Vect_FPE_Flags ${GEOS_Fortran_Release_FPE_Flags})

# Common variables for every compiler
include(GenericCompiler)

set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DpgiFortran")
