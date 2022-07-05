CC=gcc
CFLAGS=$(SACCFLAGS)

.c.o :
	$(CC) -c $(CFLAGS) $<

LIB  = ../../../lib/libsac.a


LIBOBJS = gcChecks.o gcCollect.o

library:  $(LIBOBJS)
	ar r ../../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o
	
gcChecks.o:  gcChecks.c gc.h
gcCollect.o: gcCollect.c gc.h 
