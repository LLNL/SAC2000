CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB    = ../../lib/libbot.a


LIBOBJS = capf.o chpf.o clipdp.o crname.o indexa.o \
          indexb.o indexc.o lequal.o locdp.o \
          modcase.o nequal.o terminate.o upcase.o wrlist.o 
	     
library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 
    
    
capf.o:       capf.c $(INC)/mach.h $(INC)/eam.h 
chpf.o:       chpf.c $(INC)/mach.h $(INC)/eam.h 
clipdp.o:     clipdp.c $(INC)/mach.h 
crname.o:     crname.c $(INC)/mach.h 
indexa.o:     indexa.c 
indexb.o:     indexb.c 
indexc.o:     indexc.c 
lequal.o:     lequal.c $(INC)/mach.h 
locdp.o:      locdp.c $(INC)/mach.h 
modcase.o:    modcase.c 
nequal.o:     nequal.c $(INC)/mach.h 
terminate.o:  terminate.c
upcase.o:     upcase.c 
wrlist.o:     wrlist.c $(INC)/mach.h 
