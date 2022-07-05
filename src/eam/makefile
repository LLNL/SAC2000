CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB  = ../../lib/libeam.a

LIBOBJS = inieam.o whpf1.o xapk.o xeamc.o xoapf.o xohpf.o xwhpf.o 

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 


inieam.o: inieam.c $(INC)/mach.h $(INC)/eam.h 
whpf1.o:  whpf1.c $(INC)/mach.h $(INC)/eam.h 
xapk.o:   xapk.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
          $(INC)/mem.h $(INC)/eam.h 
xeamc.o:  xeamc.c $(INC)/mach.h $(INC)/eam.h 
xoapf.o:  xoapf.c $(INC)/mach.h $(INC)/eam.h 
xohpf.o:  xohpf.c $(INC)/mach.h $(INC)/eam.h 
xwhpf.o:  xwhpf.c $(INC)/mach.h $(INC)/eam.h 
