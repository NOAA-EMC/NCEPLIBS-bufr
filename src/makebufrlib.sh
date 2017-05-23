#!/bin/sh
##########################################################################################
#
#   PURPOSE:   This script uses the make utility to update the BUFR archive libraries
#              (libbufr*.a).  It reads a list of source files in the library and then
#              automatically generates a makefile that will be used to update the archive
#              libraries.  The make command is then executed for each build of the archive
#              library, where the archive library name and compilation flags are passed to
#              the makefile through environment variables.
#
#   REMARKS:   Only source files that have been modified since the last library update are
#              recompiled and replaced in the object archive libraries.  The make utility
#              determines this from the file modification times.  New source files are
#              also compiled and added to the object archive libraries.
#
##########################################################################################

#-------------------------------------------------------------------------------
# To compile with debugging flags, export DEBUG=YES before calling this script

[ -z "$DEBUG" ] && DEBUG=NO

#-------------------------------------------------------------------------------
# Determine the OS and hostname of the local machine.

OS=`uname`
hncc1="`hostname | cut -c1`"
if [ $OS = "Linux" ]
then
    export FC=ftn
    export CC=cc
    export AFLAGS=""
    case ${COMP:?} in
    intel)
        fflags_base="-g -traceback"
        cflags_base="-g -traceback -DUNDERSCORE"
        flag64int="-i8"
        flag64flt="-r8"
        if [ $DEBUG != "YES" ]
        then
            fflags_base="${fflags_base} -O3"
            cflags_base="${cflags_base} -O3"
        fi
        if [ ${hncc1} = "l" -o ${hncc1} = "s" ]  # luna or surge
        then
            fflags_base="${fflags_base} -axCORE-AVX2"
            cflags_base="${cflags_base} -axCORE-AVX2"
        elif [ ${hncc1} = "t" -o ${hncc1} = "g" ]  # tide or gyre
        then
            export FC=ifort
            export CC=icc
        fi
        ;;
    *)
        fflags_base=""
        cflags_base="-DUNDERSCORE"
        flag64int="-s integer64"
        flag64flt="-s real64"
        if [ $DEBUG != "YES" ]
        then
            fflags_base="${fflags_base} -O2"
            cflags_base="${cflags_base} -O2"
        fi
        ;;
    esac
else
    >&2 echo "Don't know how to build BUFRLIB for OS = $OS"
    exit 1
fi

#-------------------------------------------------------------------------------
# Determine the byte-ordering scheme used by the local machine.

cat > endiantest.c << ENDIANTEST

#include <stdio.h>

void fill(p, size) char *p; int size; {
	char *ab= "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	int i;

	for (i=0; i<size; i++) p[i]= ab[i];
}

void endian(byte_size) int byte_size; {
	int j=0;
	unsigned int mask, i, c;

	mask=0;
	for (i=1; i<=(unsigned)byte_size; i++) mask= (mask<<1)|1;
	fill((char *)&j, (int) sizeof(j));
	for (i=1; i<=sizeof(j); i++) {
	    c=((j>>(byte_size*(sizeof(j)-i)))&mask);
	    putchar(c==0 ? '?' : (char)c);
	}
	printf("\n");
}

int cprop() {
	/* Properties of type char */
	char c;
	int byte_size;

	c=1; byte_size=0;
	do { c<<=1; byte_size++; } while(c!=0);

	return byte_size;
}

main()
{
	int byte_size;

	byte_size= cprop();
	endian(byte_size);
}
ENDIANTEST

$CC -o endiantest endiantest.c

if [ `./endiantest | cut -c1` = "A" ]
then
    byte_order=BIG_ENDIAN
else
    byte_order=LITTLE_ENDIAN
fi
echo
echo "byte_order is $byte_order"
echo

rm -f endiantest.c endiantest

#-------------------------------------------------------------------------------
# Get the version number for this build from the bvers.f source file.

version=v`grep CVERSTR bvers.f | grep '=' | cut -f2 -d\'` 

echo "version is $version"
echo

#-------------------------------------------------------------------------------
#  Cycle through each type of array configuration 

for array_type in DYNAMIC STATIC
do

    if [ ${array_type} = "DYNAMIC" ]
    then
        xtag="_DA"
    else
        xtag=""
    fi

    # -------------------------------------------------------------------------------
    # Set the Fortran define flags.

    fflags_defs="-D${byte_order} -D${array_type}_ALLOCATION"

    # -------------------------------------------------------------------------------
    # Set the C define flags.

    cflags_defs="-D${array_type}_ALLOCATION"
    for bprm in MAXNC MXNAF
    do
        bprmval=`grep " ${bprm} = " bufrlib.prm | cut -f2 -d= | cut -f2 -d" "`
        cflags_defs="${cflags_defs} -D${bprm}=${bprmval}"
    done

    # -------------------------------------------------------------------------------
    # Generate a list of object files that correspond to the
    # list of Fortran ( *.f ) files in the current directory.

    # We need to ensure that the Fortran files are compiled in this order:
    #   1. any modv_*.F files (modules containing variables)
    #   2. any moda_*.F files (modules containing array declarations)
    #   3. all remaining *.F and *.f files

    OBJS=""

    modvlist="`ls -1 modv_*.F`"
    modalist="`ls -1 moda_*.F`"
    allrem="`ls -1 *.F *.f | grep -v mod[av]_`"

    for i in $modvlist $modalist $allrem
    do
        obj=`basename $i .F`
        obj=`basename ${obj} .f`
        OBJS="$OBJS ${obj}.o"
    done

    # -------------------------------------------------------------------------------
    # Generate a list of object files that corresponds to the
    # list of C ( .c ) files in the current directory.

    for i in `ls *.c`
    do
        obj=`basename $i .c`
        OBJS="$OBJS ${obj}.o"
    done

    # -------------------------------------------------------------------------------
    # If a makefile exists, remove it because we're going to generate a new one (below)
    # in case the object file list has changed since the last time we compiled.

    if [ -f make.libbufr ] 
    then
        rm -f make.libbufr
    fi

    # -------------------------------------------------------------------------------
    # Generate a new makefile (make.libbufr), with the updated object list,
    # from this herefile.

cat > make.libbufr << EOF
SHELL=/bin/sh

\$(LIB):	\$(LIB)( ${OBJS} )

.F.a:
	\$(FC) -c \$(FFLAGS) \$<
	ar -ruv \$(AFLAGS) \$@ \$*.o
	rm -f \$*.o

.f.a:
	\$(FC) -c \$(FFLAGS) \$<
	ar -ruv \$(AFLAGS) \$@ \$*.o
	rm -f \$*.o

.c.a:
	\$(CC) -c \$(CFLAGS) \$<
	ar -ruv \$(AFLAGS) \$@ \$*.o
	rm -f \$*.o
EOF

    # ---------------------------------------------------------------------------
    # Update libbufr_4_64.a (4-byte REAL, 4-byte INT, 64-bit compilation)

    if [ ${hncc1} = "l" -o ${hncc1} = "s" ]  # luna or surge
    then
        export LIB="../${COMP}/libbufr_${version}_4_64${xtag}.a"
        mkdir -p $(dirname $LIB)
    else
        export LIB="../libbufr_${version}_4_64${xtag}.a"
    fi
    echo
    echo "Updating ${LIB} with ${array_type} arrays..."
    echo
    export FFLAGS="${fflags_base} ${fflags_defs}"
    export CFLAGS="${cflags_base} ${cflags_defs}"
    make -f make.libbufr
    err_make=$?
    [ $err_make -ne 0 ]  && exit 99

    # ---------------------------------------------------------------------------
    # Update libbufr_8_64.a (8-byte REAL, 8-byte INT, 64-bit compilation)

    if [ ${hncc1} = "l" -o ${hncc1} = "s" ]  # luna or surge
    then
        export LIB="../${COMP}/libbufr_${version}_8_64${xtag}.a"
        mkdir -p $(dirname $LIB)
    else
        export LIB="../libbufr_${version}_8_64${xtag}.a"
    fi
    echo
    echo "Updating ${LIB} with ${array_type} arrays..."
    echo
    export FFLAGS="${fflags_base} ${fflags_defs} ${flag64flt} ${flag64int}"
    export CFLAGS="${cflags_base} -DF77_INTSIZE_8 ${cflags_defs}"
    make -f make.libbufr
    err_make=$?
    [ $err_make -ne 0 ]  && exit 99

    # ---------------------------------------------------------------------------
    # Update libbufr_d_64.a (8-byte REAL, 4-byte INT, 64-bit compilation)

    if [ ${hncc1} = "l" -o ${hncc1} = "s" ]  # luna or surge
    then
        export LIB="../${COMP}/libbufr_${version}_d_64${xtag}.a"
        mkdir -p $(dirname $LIB)
    else
        export LIB="../libbufr_${version}_d_64${xtag}.a"
    fi
    echo
    echo "Updating ${LIB} with ${array_type} arrays..."
    echo
    export FFLAGS="${fflags_base} ${fflags_defs} ${flag64flt}"
    export CFLAGS="${cflags_base} ${cflags_defs}"
    make -f make.libbufr
    err_make=$?
    [ $err_make -ne 0 ]  && exit 99

done

#-------------------------------------------------------------------------------
# Clean up and check how we did.

rm -f make.libbufr *.mod

if [ \( \( ${hncc1} = "l" -o ${hncc1} = "s" \) -a \( -s ../${COMP}/libbufr_${version}_s_64.a \) \) \
         -o \
     \( \( ${hncc1} = "t" -o ${hncc1} = "g" \) -a \( -s ../libbufr_${version}_d_64.a \) \) ]; then
   echo
   echo "SUCCESS: The script updated all BUFR archive libraries"
   echo
else
   echo
   echo "FAILURE: The script did NOT update all BUFR archive libraries"
   echo
fi
