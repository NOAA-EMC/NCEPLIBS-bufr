set (FOPT0 "-O0")
set (FOPT1 "-O1")
set (FOPT2 "-O2")
set (FOPT3 "-O3")
set (FOPT4 "-O4")
set (AVX2 "-axCORE-AVX2")
set (DEBINFO "-g")
set (DEBFULL "-debug full")
set (WARNALL "-warn all")

set (FPE0 "-fpe0")
set (FPE3 "-fpe3")
set (FP_MODEL_SOURCE "-fp-model source")
set (FP_MODEL_STRICT "-fp-model strict")
set (FP_MODEL_CONSISTENT "-fp-model consistent")

set (OPTREPORT0 "-qopt-report0")
set (OPTREPORT5 "-qopt-report5")

set (FREAL8 "-r8")
set (FINT8 "-i8")

set (MMMED "-mcmodel=medium")
set (MMSML "-mcmodelsmall")
set (MMLGE "-mcmodel=large")
set (PP    "-fpp -FR")
set (FREEFORM    "-free")
set (SHARED "-shared")
set (COMPSHARED "-shared-intel")
set (COMPSTATIC "")
set (XHOST "-xHOST")
set (MISMATCH "")
set (BIG_ENDIAN "-convert big_endian")
set (LITTLE_ENDIAN "-convert little_endian")
set (EXTENDED_SOURCE "-extend_source")
set (FIXED_SOURCE "-fixed")
set (DISABLE_FIELD_WIDTH_WARNING "-diag-disable 8291")
set (DISABLE_GLOBAL_NAME_WARNING "-diag-disable 5462")
set (CRAY_POINTER "")
set (MCMODEL "-mcmodel medium -shared-intel")
set (HEAPARRAYS "-heap-arrays 32")
set (BYTERECLEN "-assume byterecl")
set (ALIGNCOM "-align dcommons")
set (TRACEBACK "-traceback")
set (NOOLD_MAXMINLOC "-assume noold_maxminloc")
set (REALLOC_LHS "-assume realloc_lhs")
set (ARCH_CONSISTENCY "-fimf-arch-consistency=true")
set (FTZ "-ftz")
set (ALIGN_ALL "-align all")
set (NO_ALIAS "-fno-alias")

set (NO_RANGE_CHECK "")

#add_definitions(-DHAVE_SHMEM)

####################################################

# Common Fortran Flags
# --------------------
set (common_Fortran_flags "${TRACEBACK} ${REALLOC_LHS}")
set (common_Fortran_fpe_flags "${FPE0} ${FP_MODEL_SOURCE} ${HEAPARRAYS} ${NOOLD_MAXMINLOC}")

