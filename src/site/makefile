CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB  = ../../lib/libsite.a

LIBOBJS = initsite.o xsitecom.o xtestsite.o 

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 

initsite.o:   initsite.c $(INC)/mach.h $(INC)/site.h 
xsitecom.o:   xsitecom.c $(INC)/mach.h $(INC)/site.h 
xtestsite.o:  xtestsite.c $(INC)/mach.h $(INC)/site.h 
