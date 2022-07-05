CC=gcc
CFLAGS=$(SACCFLAGS)

.c.o :
	$(CC) -c $(CFLAGS) $<

LIB  = ../../../lib/libseismgr.a

LIBOBJS = cssbIO.o cssRead2_8FlatFiles.o cssReadFlatFiles.o cssWriteFlatFiles.o

library:  $(LIBOBJS)
	ar r ../../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o
	
cssbIO.o:              cssbIO.c cssb.h
cssRead2_8FlatFiles.o: cssRead2_8FlatFiles.c 
cssReadFlatFiles.o:    cssReadFlatFiles.c 
cssWriteFlatFiles.o:   cssWriteFlatFiles.c 
