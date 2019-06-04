# *** manually set environments (for gnu compiler) of bufr ***

 : ${USERMODE:=false}  # user mode (USERMODE) is closed by default
                       # set env var USERMODE to "true" to active it
 ${USERMODE} && {
    echo "Environment set by user"
# On theia/cray, user can load environment
#   module load gcc/6.2.0
# Or set environment on specific platform
    gcc_version=6.2.0
    gcc_topdir=/apps/gcc/$gcc_version
    export PATH=$gcc_topdir/bin:$PATH
    export LIBRARY_PATH=$gcc_topdir/lib64:$LIBRARY_PATH
    export LD_LIBRARY_PATH=$gcc_topdir/lib64:$LD_LIBRARY_PATH
    export INCLUDE=$gcc_topdir/include:$INCLUDE
    export MANPATH=$gcc_topdir/share/man:$MANPATH
 }

 ANCHORDIR=..
 export COMP=gnu
 export BUFR_VER=v11.2.0
 export BUFR_SRC=
 export BUFR_LIB4_DA=$ANCHORDIR/libbufr_${BUFR_VER}_4_64_DA.a
 export BUFR_LIBd_DA=$ANCHORDIR/libbufr_${BUFR_VER}_d_64_DA.a
 export BUFR_LIB8_DA=$ANCHORDIR/libbufr_${BUFR_VER}_8_64_DA.a
 export BUFR_LIB4=$ANCHORDIR/libbufr_${BUFR_VER}_4_64.a
 export BUFR_LIB8=$ANCHORDIR/libbufr_${BUFR_VER}_8_64.a
 export BUFR_LIBd=$ANCHORDIR/libbufr_${BUFR_VER}_d_64.a
 export BUFR_LIBs=$ANCHORDIR/libbufr_${BUFR_VER}_s_64.a

 export CC=gcc
 export FC=gfortran
 export CPP=cpp
 export OMPCC="$CC -fopenmp"
 export OMPFC="$FC -fopenmp"
 export MPICC=mpigcc
 export MPIFC=mpigfortran

 export DEBUG="-g -O0"
 export CFLAGS="-O3 -fPIC"
 export FFLAGS="-O3 -fPIC"
 export FREEFORM="-ffree-form"
 export FPPCPP="-cpp"
 export CPPFLAGS="-P -traditional-cpp"
 export MPICFLAGS="-O3 -fPIC"
 export MPIFFLAGS="-O3 -fPIC"
 export MODPATH="-J"
 export I4R4=""
 export I4R8="-fdefault-real-8"
 export I8R8="-fdefault-integer-8 -fdefault-real-8"

 export CPPDEFS=""
 export CFLAGSDEFS="-DUNDERSCORE -DLINUX"
 export FFLAGSDEFS=""

 export USECC="YES"
 export USEFC="YES"
 export DEPS=""
