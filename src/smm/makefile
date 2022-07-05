CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB  = ../../lib/libsmm.a

INC1 = $(INC)/mach.h $(INC)/smm.h
INC2 = $(INC1) $(INC)/dfm.h $(INC)/hdr.h

LIBOBJS = inismm.o ptp.o xmarkptp.o xmarktimes.o \
          xmarkvalue.o xrms.o xsmmc.o 

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 

inismm.o:      inismm.c $(INC1) 
ptp.o:         ptp.c 
xmarkptp.o:    xmarkptp.c   $(INC2) $(INC)/mem.h $(INC)/eam.h 
xmarktimes.o:  xmarktimes.c $(INC2) $(INC)/mem.h 
xmarkvalue.o:  xmarkvalue.c $(INC2) $(INC)/mem.h 
xrms.o:        xrms.c       $(INC2) $(INC)/lhf.h $(INC)/mem.h 
xsmmc.o:       xsmmc.c      $(INC1) 
