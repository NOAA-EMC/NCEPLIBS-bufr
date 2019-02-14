xg2mg:	clean
	icc -g -O2 -DUNDERSCORE -c xg2mg.c
	ifort -g -O2 -nofor-main -o xg2mg *.o ${BUFR_LIB4}

clean:
	rm -f *.o xg2mg
