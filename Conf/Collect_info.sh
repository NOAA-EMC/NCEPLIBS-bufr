 collect_info() {
 local setx_status=${-//[^x]/}
 [[ -n $setx_status ]] && set +x

  local lname=$1
  local precsn=$2
  local msgline=$3
  local msginfo=$4

  local info
  local oneline

  USEMPI=false
  grep -i mpi <<< "$COMP" &> /dev/null && USEMPI=true

#  libver=${lname~~}_VER
  [[ $precsn == "-" ]] && {
    cflg=CFLAGS
    fflg=FFLAGS
    libfile=${lname}Lib
    NCLIBVER="${lname,,} (${lname~~}) ${!libver}"
  } || {
    cflg=CFLAGS${precsn%/*}
    [[ -z "${!cflg}" ]] && cflg=CFLAGS
    fflg=FFLAGS${precsn%/*}
    libfile=${lname}Lib${precsn#*/}
    NCLIBVER="${lname,,} (${lname~~} _${precsn%/*}) ${!libver}"
  }
  mpicflg=MPI"$cflg"
  mpifflg=MPI"$fflg"

  $USEMPI && mpiver=$(mpirun --version | grep Version | sed 's/://' | \
                      tr -s " " | sed 's/^.* Version //; s/ Build.*$//')

  COMPILED="compiled by"
  $CC --version | grep " (ICC) " &> /dev/null   &&
  $FC --version | grep " (IFORT) " &> /dev/null && {
     ccstr=$($CC --version | grep " (ICC) ")
     cfstr=$($FC --version | grep " (IFORT) ")
     COMPILER="${ccstr/(ICC)*/(ICC)}, $cfstr"
  } || {
     $CC --version | grep " (.*GCC.*) " &> /dev/null &&
     $FC --version | grep " (.*GCC.*) " &> /dev/null && {
        ccstr=$($CC --version | grep " (.*GCC.*) ")
        cfstr=$($FC --version | grep " (.*GCC.*) ")
        COMPILER="${ccstr/ (*GCC*)*/}, $cfstr"
     } || {
        ccstr=$($CC --version | grep -E 'ICC|GCC')
        cfstr=$($FC --version | grep -E 'IFORT|GCC')
        COMPILER="$ccstr, $cfstr"
     }
  }
  [[ -z "$USECC" || ${USECC,,} == no ]] && { COMPILER="$cfstr"; }
  [[ -z "$USEFC" || ${USEFC,,} == no ]] && { COMPILER="$ccstr"; }
  
  $USEMPI && { COMPILER="<mpi $mpiver> $COMPILER"; }
  (( ${#COMPILER} > 50 )) && COMPILER=${COMPILER% (*)*}

  DATETIME="$(date +'on %x at %X')"

  oneline="nclibver = '$NCLIBVER',"
  oneline="$oneline"$'\n'"compiled = ' $COMPILED',"
  oneline="$oneline"$'\n'"compiler = ' $COMPILER',"
  oneline="$oneline"$'\n'"datetime = ' $DATETIME'"

  info="*** ${lname} information ***"
  info=$info$'\n'"LIBRARY NAME: ${lname~~}"
  info=$info$'\n'"LIBRARY VERSION: ${!libver}"
  [[ $precsn != "-" ]] && {
    info=$info$'\n'"LIBRARY PRECISION: (_${precsn%/*})"
  }
  info=$info$'\n'"LIBRARY FILE NAME: ${!libfile}"
  info=$info$'\n'"*** building information ***"
  info=$info$'\n'"BUILDING TIME: $(date)"
  info=$info$'\n'"SYSTEM/PLATFORM: $(uname -srm)"
  [[ -n "$USECC" && ${USECC,,} != no ]] && {
    $USEMPI && {
      info=$info$'\n'"C COMPILER NAME: $MPICC"
      info=$info$'\n'"C COMPILER VERSION: $($MPICC --version | head -1)"
      info=$info$'\n'"MPICFLAGS: "${!mpicflg}
    } || {
      info=$info$'\n'"C COMPILER NAME: $CC"
      info=$info$'\n'"C COMPILER VERSION: $($CC --version | head -1)"
      info=$info$'\n'"CFLAGS: "${!cflg}
    }
  }
  [[ -n "$USEFC" && ${USEFC,,} != no ]] && {
    $USEMPI && {
      info=$info$'\n'"FORTRAN COMPILER NAME: $MPIFC"
      info=$info$'\n'"FORTRAN COMPILER VERSION: $($MPIFC --version | head -1)"
      info=$info$'\n'"MPIFFLAGS: "${!mpifflg}
    } || {
      info=$info$'\n'"FORTRAN COMPILER NAME: $FC"
      info=$info$'\n'"FORTRAN COMPILER VERSION: $($FC --version | head -1)"
      info=$info$'\n'"FFLAGS: "${!fflg}
    }
  }

  $USEMPI && { info=$info$'\n'"MPI VERSION: $mpiver"; }

  [[ -n "$DEPS" ]] && { info=$info$'\n'"DEPENDENCES: $DEPS"; }

  eval $msgline="\"$oneline\""
  eval $msginfo="\"$info\""

 [[ -n $setx_status ]] && set -x
 }
