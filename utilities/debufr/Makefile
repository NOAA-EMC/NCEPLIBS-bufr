debufr:
	icc -O3 -DUNDERSCORE -c cdebufr.c
	ifort -O3 -nofor-main -mcmodel=medium -shared-intel -o debufr fdebufr.f cdebufr.o -L/nwprod/lib -lbufr_v10.2.0_s_64

clean:
	rm -f *.o debufr
