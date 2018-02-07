CSRC	= cdebufr.c

FSRC	= shtbinfo.f fdebufr.f openbt.f

debufr:		$(CSRC) $(FSRC)
	icc -O3 -mcmodel=medium -shared-intel -DUNDERSCORE -c $(CSRC)
	ifort -O3 -nofor-main -mcmodel=medium -shared-intel -o debufr $(FSRC) cdebufr.o -L/nwprod/lib -lbufr_s_64

clean:
	rm -f *.o *.mod

realclean:	clean
	rm -f debufr
