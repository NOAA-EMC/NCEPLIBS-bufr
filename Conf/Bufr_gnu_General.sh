# *** manually set environments (for gnu compiler) of bufr ***

# !!! module environment (*THEIA*) !!!
 module load gcc/6.2.0

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
 export CFLAGS="-O3 -DUNDERSCORE -DLINUX -fPIC"
 export FFLAGS="-cpp -O3 -fPIC"
 export CPPFLAGS="-P -traditional-cpp"
 export MPICFLAGS="-O3 -DUNDERSCORE -DLINUX -fPIC"
 export MPIFFLAGS="-cpp -O3 -fPIC"
 export I4R4=""
 export I4R8="-fdefault-real-8"
 export I8R8="-fdefault-integer-8 -fdefault-real-8"

 export CPPDEFS=""
 export CFLAGSDEFS=""
 export FFLAGSDEFS=""

 export USECC="YES"
 export USEFC="YES"
 export DEPS=""
