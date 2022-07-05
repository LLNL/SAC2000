CC=gcc
CFLAGS=$(SACCFLAGS)

.c.o :
	$(CC) -c $(CFLAGS) $<

LIB  = ../../../lib/libsac.a

LIBOBJS = cmprs6.o dcomp.o gseRead20.o GSEWaveforms.o gseWrite20.o

library:  $(LIBOBJS)
	ar r ../../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o
	
cmprs6.o:       cmprs6.c gse.h
dcomp.o:        dcomp.c gse.h 
gseRead20.o:    gseRead20.c gse.h 
GSEWaveforms.o: GSEWaveforms.c gse.h
gseWrite20.o:   gseWrite20.c  gse.h
