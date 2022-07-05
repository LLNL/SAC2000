CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB  = ../../lib/libuom.a

INC1 = $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h $(INC)/mem.h

LIBOBJS = dif2.o dif3.o dif5.o iniuom.o \
          xabs.o xadd.o xdif.o xdiv.o xexp.o \
          xexp10.o xint.o xlog.o xlog10.o xmul.o xsqr.o \
          xsqrt.o xsub.o xuomc.o 


library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 

dif2.o:   dif2.c 
dif3.o:   dif3.c 
dif5.o:   dif5.c 
iniuom.o: iniuom.c $(INC)/mach.h $(INC)/uom.h 
xabs.o:   xabs.c   $(INC1) 
xadd.o:   xadd.c   $(INC1) $(INC)/uom.h 
xdif.o:   xdif.c   $(INC1) $(INC)/uom.h 
xdiv.o:   xdiv.c   $(INC1) $(INC)/uom.h 
xexp.o:   xexp.c   $(INC1) 
xexp10.o: xexp10.c $(INC1) 
xint.o:   xint.c   $(INC1)  $(INC)/uom.h
xlog.o:   xlog.c   $(INC1) 
xlog10.o: xlog10.c $(INC1) 
xmul.o:   xmul.c   $(INC1) $(INC)/uom.h 
xsqr.o:   xsqr.c   $(INC1) 
xsqrt.o:  xsqrt.c  $(INC1) 
xsub.o:   xsub.c   $(INC1) $(INC)/uom.h 
xuomc.o:  xuomc.c  $(INC)/mach.h $(INC)/uom.h 
