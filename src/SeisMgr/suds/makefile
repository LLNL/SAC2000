CC=gcc
CFLAGS=$(SACCFLAGS)

.c.o :
	$(CC) -c $(CFLAGS) $<

LIB  = ../../../lib/libsac.a

LIBOBJS = sudsDataConversions.o sudsIO.o sudsListOps.o sudsReading.o \
          sudsToCSSlist.o sudsWriting.o

library:  $(LIBOBJS)
	ar r ../../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o
	
sudsDataConversions.o:   sudsDataConversions.c
sudsIO.o:                sudsIO.c sudsIO.h
sudsListOps.o:           sudsListOps.c sudsListOps.h
sudsReading.o:           sudsReading.c sudsReading.h
sudsToCSSlist.o:         sudsToCSSlist.c
sudsWriting.o:           sudsWriting.c sudsWriting.h
