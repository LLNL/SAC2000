CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB  = ../../lib/libnnm.a

LIBOBJS = xnnmc.o xwritenn.o 

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 

xnnmc.o:     xnnmc.c $(INC)/mach.h 
xwritenn.o:  xwritenn.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
             $(INC)/mem.h $(INC)/nnm.h 
