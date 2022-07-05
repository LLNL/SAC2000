CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB  = ../../lib/libspe.a

INC1 = $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h $(INC)/mem.h $(INC)/spe.h

LIBOBJS = inispe.o xcor.o  xmem.o    xmlm.o     xpcor.o \
          xpds.o   xppe.o  xpspe.o   xquitspe.o xrcor.o xspe.o \
          xspec.o  xwcor.o xwhiten.o xwspe.o 

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 

inispe.o:    inispe.c   $(INC)/mach.h $(INC)/spe.h 
xcor.o:      xcor.c     $(INC1) 
xmem.o:      xmem.c     $(INC1) 
xmlm.o:      xmlm.c     $(INC1) 
xpcor.o:     xpcor.c    $(INC1) $(INC)/gem.h $(INC)/gam.h 
xpds.o:      xpds.c     $(INC1) 
xppe.o:      xppe.c     $(INC1) $(INC)/gem.h $(INC)/gam.h 
xpspe.o:     xpspe.c    $(INC1) $(INC)/gem.h $(INC)/gam.h 
xquitspe.o:  xquitspe.c $(INC)/mach.h $(INC)/mem.h $(INC)/spe.h 
xrcor.o:     xrcor.c    $(INC)/mach.h 
xspe.o:      xspe.c     $(INC1) 
xspec.o:     xspec.c    $(INC)/mach.h $(INC)/spe.h 
xwcor.o:     xwcor.c    $(INC1) 
xwhiten.o:   xwhiten.c  $(INC)/icm.h $(INC)/mach.h $(INC)/hdr.h $(INC)/mem.h $(INC)/spe.h
xwspe.o:     xwspe.c    $(INC)/mach.h $(INC)/hdr.h $(INC)/mem.h $(INC)/spe.h 
