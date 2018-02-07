xbfmg:	clean
	icc -g -O2 -DUNDERSCORE -c xbfmg.c
	ifort -g -O2 -nofor-main -o xbfmg *.o -L/nwprod/lib -lbufr_4_64

clean:
	rm -f *.o xbfmg
