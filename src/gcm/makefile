CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB  = ../../lib/libgcm.a

LIBOBJS = xbegindevice.o xenddevices.o xgcmc.o xsgf.o  xvspac.o 

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 



xbegindevice.o: xbegindevice.c $(INC)/mach.h 
xenddevices.o:  xenddevices.c $(INC)/mach.h 
xgcmc.o:        xgcmc.c $(INC)/mach.h 
xsgf.o:         xsgf.c $(INC)/mach.h 
xvspac.o:       xvspac.c $(INC)/mach.h 
