# *** manually set environments (for intel compiler) of bufr ***

 : ${USERMODE:=false}  # user mode (USERMODE) is closed by default
                       # set env var USERMODE to "true" to active it
 ${USERMODE} && {
    echo "Environment set by user"
# On theia/cray, user can load environment
#   module load intel/18.0.1.163
# Or set environment on specific platform
    intel_version=2018.1.163
    intel_topdir=/apps/intel/compilers_and_libraries_$intel_version
    source $intel_topdir/linux/bin/compilervars.sh intel64
 }

 ANCHORDIR=..
 export COMP=ips
 export BUFR_VER=v10.2.5
 export BUFR_SRC=
 export BUFR_LIB4_DA=$ANCHORDIR/libbufr_${BUFR_VER}_4_64_DA.a
 export BUFR_LIBd_DA=$ANCHORDIR/libbufr_${BUFR_VER}_d_64_DA.a
 export BUFR_LIB8_DA=$ANCHORDIR/libbufr_${BUFR_VER}_8_64_DA.a
 export BUFR_LIB4=$ANCHORDIR/libbufr_${BUFR_VER}_4_64.a
 export BUFR_LIB8=$ANCHORDIR/libbufr_${BUFR_VER}_8_64.a
 export BUFR_LIBd=$ANCHORDIR/libbufr_${BUFR_VER}_d_64.a
 export BUFR_LIBs=$ANCHORDIR/libbufr_${BUFR_VER}_s_64.a

 export CC=icc
 export FC=ifort
 export CPP=cpp
 export OMPCC="$CC -qopenmp"
 export OMPFC="$FC -qopenmp"
 export MPICC=mpiicc
 export MPIFC=mpiifort

 export DEBUG="-g -O0"
 export CFLAGS="-O3 -fPIC"
 export FFLAGS="-O3 -fPIC"
 export FPPCPP="-cpp"
 export FREEFORM="-free"
 export CPPFLAGS="-P -traditional-cpp"
 export MPICFLAGS="-O3 -fPIC"
 export MPIFFLAGS="-O3 -fPIC"
 export MODPATH="-module "
 export I4R4="-integer-size 32 -real-size 32"
 export I4R8="-integer-size 32 -real-size 64"
 export I8R8="-integer-size 64 -real-size 64"

 export CPPDEFS=""
 export CFLAGSDEFS="-DUNDERSCORE -DLINUX"
 export FFLAGSDEFS=""

 export USECC="YES"
 export USEFC="YES"
 export DEPS=""
