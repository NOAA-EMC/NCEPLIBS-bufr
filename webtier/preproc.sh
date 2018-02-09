#!/bin/sh

#-------------------------------------------------------------------------------
#     Determine the byte-ordering scheme used by the local machine.

cat > endiantest.c << ENDIANTEST

#include <stdio.h>

#define Order(x)\
	fill((char *)&x, sizeof(x)); \
	for (i=1; i<=sizeof(x); i++) { \
	    c=((x>>(byte_size*(sizeof(x)-i)))&mask); \
	    putchar(c==0 ? '?' : (char)c); \
	} \
	printf("\n");

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
	Order(j);
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

rm -f endiantest.c endiantest
    
#-------------------------------------------------------------------------------
#     Preprocess any Fortran *.F files into corresponding *.f files.

BNFS=""

for i in `ls *.F`
do
  bn=`basename $i .F`
  bnf=${bn}.f
  BNFS="$BNFS $bnf"
  cpp -P -C -D$byte_order -DNORMAL_BUILD -DDYNAMIC_ALLOCATION $i $bnf
done

#-------------------------------------------------------------------------------
#     Use some of the preprocessed Fortran global variable modules to generate
#     corresponding define flags for the C compiler.

cflags_defs="-DDYNAMIC_ALLOCATION"
for gvar in NFILES MAXCD
do
  gvarval=`grep " ${gvar} = " modv_${gvar}.f | cut -f2 -d= | cut -f2 -d" "`
  cflags_defs="${cflags_defs} -D${gvar}=${gvarval}"
done

#-------------------------------------------------------------------------------
#     Generate the bufrlib.prm header file.

cpp -P -C -DNORMAL_BUILD -DDYNAMIC_ALLOCATION bufrlib.PRM bufrlib.prm

#-------------------------------------------------------------------------------
#     Use the bufrlib.prm header file to generate a few additional corresponding
#     define flags for the C compiler.

for bprm in MAXNC MXNAF
do
  bprmval=`grep " ${bprm} = " bufrlib.prm | cut -f2 -d= | cut -f2 -d" "`
  cflags_defs="${cflags_defs} -D${bprm}=${bprmval}"
done

#-------------------------------------------------------------------------------
#     Print (to standard output) the define flags for the C compiler.
echo ${cflags_defs}
