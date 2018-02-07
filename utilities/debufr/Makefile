debufr:
	ncepxlc -O3 -c cdebufr.c
	ncepxlf -O3 -o debufr fdebufr.f cdebufr.o -L/nwprod/lib -lbufr_s_64

clean:
	rm -f *.o debufr
