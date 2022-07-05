CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB  = ../../lib/libscm.a

INC1 = $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h $(INC)/mem.h
INC2 = $(INC1) $(INC)/scm.h

LIBOBJS = iniscm.o lifite.o lifitu.o linear.o rotate.o \
          wigint.o xabsgl.o xdecimate.o xinterpolate.o xlinefit.o xpowgl.o xquantize.o \
          xreverse.o xrglitches.o xrmean.o xrotate.o xrq.o \
          xrtr.o xscmc.o xsmooth.o xstretch.o xtaper.o 

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 

iniscm.o:        iniscm.c $(INC)/mach.h $(INC)/scm.h 
lifite.o:        lifite.c 
lifitu.o:        lifitu.c 
linear.o:        linear.c 
rotate.o:        rotate.c $(INC)/mach.h 
wigint.o:        wigint.c 
xabsgl.o:        xabsgl.c $(INC)/mach.h
xdecimate.o:     xdecimate.c $(INC2) $(INC)/fir.h 
xinterpolate.o:  xinterpolate.c $(INC2)  
xlinefit.o:      xlinefit.c $(INC1) 
xpowgl.o:        xpowgl.c $(INC)/mach.h
xquantize.o:     xquantize.c $(INC2)  
xreverse.o:      xreverse.c $(INC1) 
xrglitches.o:    xrglitches.c $(INC2) 
xrmean.o:        xrmean.c $(INC1) 
xrotate.o:       xrotate.c $(INC2) 
xrq.o:           xrq.c $(INC2) 
xrtr.o:          xrtr.c $(INC1) 
xscmc.o:         xscmc.c $(INC)/mach.h $(INC)/scm.h 
xsmooth.o:       xsmooth.c $(INC2) 
xstretch.o:      xstretch.c $(INC2) 
xtaper.o:        xtaper.c $(INC1) $(INC)/sam.h 
