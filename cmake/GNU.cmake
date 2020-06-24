if (CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 5.3)
  message(FATAL_ERROR "${CMAKE_Fortran_COMPILER_ID} version must be at least 5.3!")
endif()

set (FOPT0 "-O0")
set (FOPT1 "-O1")
set (FOPT2 "-O2")
set (FOPT3 "-O3")
set (FOPT4 "-O3")
set (AVX2 "-O3")
set (DEBINFO "-g")
set (DEBFULL "-ggdb")
set (WARNALL "-Wall")
set (UNROLL "-funroll-loops")
set (NOTAB "-Wno-tabs")

set (FPE0 "")
set (FPE3 "")
set (FP_MODEL_SOURCE "")
set (FP_MODEL_STRICT "")
set (FP_MODEL_CONSISTENT "")

set (OPTREPORT0 "")
set (OPTREPORT5 "")
set (fdbl8 "-fdefault-double-8")
set (frl8 "-fdefault-real-8")
set (FREAL8 ${frl8} ${fdbl8})
set (FINT8 "-fdefault-integer-8")

set (MMMED "")
set (MMSML "")
set (MMLGE "")
set (PP    "-cpp")
set (FREEFORM    "-ffree-form")
set (SHARED "")
set (COMPSHARED "")
set (COMPSTATIC "-static-libgfortran")
set (XHOST "")
set (MISMATCH "")
#set (BIG_ENDIAN "-fconvert=swap") # This doesn't seem to work at the moment
#set (LITTLE_ENDIAN "") # Not sure
set (EXTENDED_SOURCE "-ffixed-line-length-132")
set (FIXED_SOURCE "-ffixed-form")
set (DISABLE_FIELD_WIDTH_WARNING "")
set (CRAY_POINTER "-fcray-pointer")
set (MCMODEL "")
set (HEAPARRAYS "")
set (BYTERECLEN "-frecord-marker=4")
set (ALIGNCOM "-falign-commons")
set (TRACEBACK "-fbacktrace")
set (NOOLD_MAXMINLOC "")
set (REALLOC_LHS "")
set (ARCH_CONSISTENCY "")
set (FTZ "")
set (ALIGN_ALL "")
set (NO_ALIAS "")

set (NO_RANGE_CHECK "-fno-range-check")

####################################################

add_definitions(-D__GFORTRAN__)

# Common Fortran Flags
# --------------------
set (common_Fortran_flags "-ffree-line-length-none ${NO_RANGE_CHECK} -Wno-missing-include-dirs ${TRACEBACK}")
set (common_Fortran_fpe_flags "-ffpe-trap=zero,overflow ${TRACEBACK}")

