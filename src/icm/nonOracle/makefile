CC=gcc
CFLAGS=$(SACCFLAGS)

.c.a :
	$(CC) -c $(CFLAGS) $<
	@ ar r $@ $*.o
	@ rm -f $*.o

LIB  = ../../../lib/libSacOracleStubFuncs.a

LIBOBJS = $(LIB)(dbaseCommand.o)

library:  $(LIBOBJS)

clean:
	/bin/rm -f *.o 

dbaseCommand.o: dbaseCommand.c
