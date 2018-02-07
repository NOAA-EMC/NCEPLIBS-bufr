CSRC	= cdebufr.c prtusage.c

FSRC	= shtbinfo.f fdebufr.f openbt.f

debufr:		$(CSRC) $(FSRC)
	icc -O3 -DUNDERSCORE -c $(CSRC)
	ifort -O3 -nofor-main -o debufr $(FSRC) *.o $(BUFR_LIB4_DA)

install:
	mv debufr ../exec

clean:
	rm -f *.o *.mod

realclean:	clean
	rm -f debufr
