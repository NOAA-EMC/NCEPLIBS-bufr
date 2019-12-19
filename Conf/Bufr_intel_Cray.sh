# *** for WCOSS Cray (intel) ***

 export CC=cc
 export FC=ftn
 export OMPCC="$CC -qopenmp"
 export OMPFC="$FC -qopenmp"
 export MPICC=mpiicc
 export MPIFC=mpiifort

 export DEBUG="-g -traceback -O0"
 export CFLAGS="-g -traceback -O3 -axCORE-AVX2 -fPIC"
 export FFLAGS="-g -traceback -O3 -axCORE-AVX2 -fPIC"
 export FPPCPP="-cpp"
 export FREEFORM="-free"
 export MPICFLAGS="-g -traceback -O3 -axCORE-AVX2 -fPIC"
 export MPIFFLAGS="-g -traceback -O3 -axCORE-AVX2 -fPIC"
 export MODPATH="-module "
 export I4R4="-integer-size 32 -real-size 32"
 export I4R8="-integer-size 32 -real-size 64"
 export I8R8="-integer-size 64 -real-size 64"

 export CFLAGSDEFS="-DUNDERSCORE"
 export CF77INTSIZE="-DF77_INTSIZE_8"
 export FFLAGSDEFS=""

 export USECC="YES"
 export USEFC="YES"
 export DEPS=""
