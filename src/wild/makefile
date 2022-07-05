CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB  = ../../lib/libwild.a

LIBOBJS = getdir.o lfilec.o lwildc.o wildch.o wildfl.o

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 

getdir.o:  getdir.c $(INC)/mach.h 
lfilec.o:  lfilec.c $(INC)/mach.h $(INC)/wild.h 
lwildc.o:  lwildc.c $(INC)/mach.h $(INC)/wild.h 
wildch.o:  wildch.c $(INC)/wild.h 
wildfl.o:  wildfl.c $(INC)/mach.h $(INC)/dfm.h 
