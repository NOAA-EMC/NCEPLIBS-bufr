CSRC	= cdebufr.c prtusage.c

FSRC	= shtbinfo.f fdebufr.f openbt.f

debufr:		$(CSRC) $(FSRC)
	icc -g -traceback -O3 -DUNDERSCORE -c $(CSRC)
	ifort -g -traceback -O3 -nofor-main -o debufr $(FSRC) *.o /meso/save/Jeff.Ator/bufrlib/NCEPLIBS-bufr/libbufr_v11.3.0_4_64_DA.a
#	ifort -O3 -nofor-main -o debufr $(FSRC) *.o $(BUFR_LIB4_DA)

install:
	mv debufr ../exec

clean:
	rm -f *.o *.mod

realclean:	clean
	rm -f debufr
