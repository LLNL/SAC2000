CC=gcc
CFLAGS=$(SACCFLAGS)

.c.o :
	$(CC) -c $(CFLAGS) $<

LIB  = ../../../lib/libsac.a

LIBOBJS = smMemory.o 

library:  $(LIBOBJS)
	ar r ../../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o
	
smMemory.o:                smMemory.c smMemory.h
