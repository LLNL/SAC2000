CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB  = ../../lib/libsam.a


INC1 = $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h $(INC)/mem.h
INC2 = $(INC1) $(INC)/sam.h

LIBOBJS = chkpha.o cpft.o estpha.o fdhp.o fdlp.o fdbp.o fdbr.o \
          fdplot.o filtb.o filterdesign.o \
          filtk.o fdWhitenWrite.o fdWriteFiles.o inisam.o rfir.o spcval.o toamph.o \
          torlim.o unwrap.o xbenioff.o xbp.o xbr.o xcorrelate.o xconvolve.o xdft.o \
          xdivomega.o xenvelope.o xfir.o xhan.o xhilbert.o xhp.o \
          xidft.o xkeepam.o xkhronhite.o xlp.o xmulomega.o xpsp.o xrsp.o \
          xsamc.o xunwr.o xwnr.o xwsp.o

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 

chkpha.o:        chkpha.c 
cpft.o:          cpft.c 
estpha.o:        estpha.c 
fdhp.o:          fdhp.c $(INC2) $(INC)/dfir.h
fdlp.o:          fdlp.c $(INC2) $(INC)/dfir.h
fdbp.o:          fdbp.c $(INC2) $(INC)/dfir.h
fdbr.o:          fdbr.c $(INC2) $(INC)/dfir.h
fdplot.o:        fdplot.c $(INC)/mach.h $(INC)/mem.h $(INC)/gem.h $(INC)/gam.h $(INC)/sam.h
filtb.o:         filtb.c 
filterdesign.o:  filterdesign.c 
filtk.o:         filtk.c 
fdWhitenWrite.o: fdWhitenWrite.c $(INC)/dfm.h $(INC)/hdr.h $(INC)/mem.h $(INC)/sam.h $(INC)/extfunc.h
fdWriteFiles.o:  fdWriteFiles.c $(INC)/dfm.h $(INC)/hdr.h $(INC)/mem.h $(INC)/sam.h $(INC)/extfunc.h
inisam.o:        inisam.c $(INC)/mach.h $(INC)/sam.h 
rfir.o:          rfir.c $(INC)/mach.h 
spcval.o:        spcval.c 
toamph.o:        toamph.c $(INC)/mach.h
torlim.o:        torlim.c 
unwrap.o:        unwrap.c 
xbenioff.o:      xbenioff.c $(INC1) 
xbp.o:           xbp.c $(INC2) 
xbr.o:           xbr.c $(INC2) 
xcorrelate.o:    xcorrelate.c $(INC2) 
xconvolve.o:     xconvolve.c $(INC2) 
xdft.o:          xdft.c $(INC2) 
xdivomega.o:     xdivomega.c $(INC1) 
xenvelope.o:     xenvelope.c $(INC2) 
xfir.o:          xfir.c $(INC2) $(INC)/fir.h 
xhan.o:          xhan.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h  $(INC)/mem.h 
xhilbert.o:      xhilbert.c $(INC2) 
xhp.o:           xhp.c $(INC2) 
xidft.o:         xidft.c $(INC2) 
xkeepam.o:       xkeepam.c $(INC1) 
xkhronhite.o:    xkhronhite.c $(INC2) 
xlp.o:           xlp.c $(INC2) 
xmulomega.o:     xmulomega.c $(INC1) 
xpsp.o:          xpsp.c $(INC2) $(INC)/gem.h $(INC)/gam.h 
xrsp.o:          xrsp.c $(INC2) 
xsamc.o:         xsamc.c $(INC)/mach.h $(INC)/sam.h 
xunwr.o:         xunwr.c $(INC2) 
xwnr.o:          xwnr.c $(INC2)  
xwsp.o:          xwsp.c $(INC2)
