CC=gcc
CFLAGS=$(SACCFLAGS)

.c.o :
	$(CC) -c -DNDEBUG $(CFLAGS) $<

LIB  = ../../../lib/sac.a


LIBOBJS = ndcTransfer.o fap.o fir.o paz.o unscaled_response.o odfftr.o csserror.o polar.o lagrange.o

library:  $(LIBOBJS)
	ar r ../../../lib/libsac.a $(LIBOBJS)

clean: 
	rm -rf *.o
