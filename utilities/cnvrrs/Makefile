MXMRG =	$(shell grep "MXMRG = " mod_mrg.f | cut -f2 -d= | cut -f2 -d" ")

cnvrrs:		realclean
	icc -g -O2 -DUNDERSCORE -DMXMRG=$(MXMRG) -c *.c
	ifort -g -O2 -nofor-main -o cnvrrs *.o mod*.f [a-ln-z]*.f \
		/home/decdev/decod_shared/lib/bufr/v11.0.0/libbufr_v11.0.0_4_64.a
	make clean

clean:
	rm -f *.o *.mod

realclean:	clean
	rm -f cnvrrs
