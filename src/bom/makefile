CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC) 

.c.o :
	$(CC) -c $(SACCFLAGS) $(SAC_INCLUDES) $<

LIB  = ../../lib/libbom.a


LIBOBJS = getbfl.o inibom.o relbfl.o xaddf.o xboec.o \
          xbomc.o xdivf.o xmerge.o xmulf.o xsubf.o

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)
		   

clean:
	/bin/rm -f *.o
    
getbfl.o:  getbfl.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
           $(INC)/mem.h $(INC)/bom.h 
inibom.o:  inibom.c $(INC)/mach.h $(INC)/bom.h 
relbfl.o:  relbfl.c $(INC)/mach.h $(INC)/dfm.h $(INC)/bom.h \
           $(INC)/mem.h 
xaddf.o:   xaddf.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
           $(INC)/mem.h $(INC)/bom.h 
xboec.o:   xboec.c $(INC)/mach.h $(INC)/exm.h $(INC)/bom.h 
xbomc.o:   xbomc.c $(INC)/mach.h $(INC)/bom.h 
xdivf.o:   xdivf.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
           $(INC)/mem.h $(INC)/bom.h 
xmerge.o:  xmerge.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
           $(INC)/mem.h $(INC)/bom.h 
xmulf.o:   xmulf.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
           $(INC)/mem.h $(INC)/bom.h 
xsubf.o:   xsubf.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
           $(INC)/mem.h $(INC)/bom.h 
