CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB  = ../../lib/libgam.a

LIBOBJS = dispid.o disppk.o disppkLandscape.o getxlm.o getylm.o inigam.o \
          markvert.o markwf.o pcmcur.o pcmrpl.o pcrrpl.o pcxcur.o \
          pcxop1.o pcxop2.o pcxope.o pcxops.o pcxrpl.o pkchar.o \
          pkdet.o pkeval.o pkfilt.o pkfunc.o wavfrm.o xfid.o xfilenumber.o \
          xfitxy.o xgamc.o xp.o xp1.o xp2.o xpc.o xplotalpha.o \
          xpicks.o xplotpm.o xplotxy.o xplotdy.o xppk.o xprint.o xsetdevice.o xylim.o

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 



dispid.o:          dispid.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                   $(INC)/mem.h $(INC)/gem.h $(INC)/gam.h 
disppk.o:          disppk.c $(INC)/mach.h $(INC)/hdr.h $(INC)/lhf.h \
                   $(INC)/gem.h $(INC)/gam.h 
disppkLandscape.o: disppkLandscape.c $(INC)/mach.h $(INC)/hdr.h $(INC)/lhf.h \
                   $(INC)/gem.h $(INC)/gam.h
getxlm.o:          getxlm.c $(INC)/mach.h $(INC)/gam.h $(INC)/hdr.h 
getylm.o:          getylm.c $(INC)/mach.h $(INC)/gam.h $(INC)/dfm.h \
                   $(INC)/hdr.h $(INC)/mem.h 
inigam.o:          inigam.c $(INC)/mach.h $(INC)/gem.h $(INC)/gam.h 
markvert.o:        markvert.c 
markwf.o:          markwf.c 
pcmcur.o:          pcmcur.c $(INC)/mach.h $(INC)/gam.h $(INC)/gem.h 
pcmrpl.o:          pcmrpl.c $(INC)/mach.h $(INC)/gam.h $(INC)/gem.h 
pcrrpl.o:          pcrrpl.c $(INC)/mach.h $(INC)/gem.h $(INC)/gam.h 
pcxcur.o:          pcxcur.c $(INC)/mach.h $(INC)/gam.h $(INC)/gem.h 
pcxop1.o:          pcxop1.c $(INC)/mach.h $(INC)/gam.h $(INC)/gem.h 
pcxop2.o:          pcxop2.c $(INC)/mach.h $(INC)/gam.h $(INC)/gem.h 
pcxope.o:          pcxope.c $(INC)/mach.h $(INC)/gam.h $(INC)/gem.h 
pcxops.o:          pcxops.c $(INC)/mach.h 
pcxrpl.o:          pcxrpl.c $(INC)/mach.h $(INC)/gam.h $(INC)/gem.h 
pkchar.o:          pkchar.c $(INC)/mach.h 
pkdet.o:           pkdet.c $(INC)/mach.h $(INC)/eam.h 
pkeval.o:          pkeval.c $(INC)/mach.h $(INC)/eam.h 
pkfilt.o:          pkfilt.c $(INC)/mach.h $(INC)/eam.h 
pkfunc.o:          pkfunc.c $(INC)/mach.h $(INC)/eam.h 
wavfrm.o:          wavfrm.c 
xfid.o:            xfid.c $(INC)/mach.h $(INC)/gam.h 
xfilenumber.o:	   xfilenumber.c $(INC)/mach.h $(INC)/gam.h
xfitxy.o:          xfitxy.c $(INC)/mach.h $(INC)/gem.h $(INC)/gam.h \
                   $(INC)/dfm.h $(INC)/hdr.h $(INC)/mem.h 
xgamc.o:           xgamc.c $(INC)/mach.h $(INC)/gam.h 
xp.o:              xp.c $(INC)/mach.h $(INC)/gem.h $(INC)/gam.h \
                   $(INC)/dfm.h $(INC)/hdr.h $(INC)/mem.h 
xp1.o:             xp1.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                   $(INC)/mem.h $(INC)/gem.h $(INC)/gam.h 
xp2.o:             xp2.c $(INC)/mach.h $(INC)/gem.h $(INC)/gam.h \
                   $(INC)/dfm.h $(INC)/hdr.h $(INC)/mem.h 
xpc.o:             xpc.c $(INC)/mach.h $(INC)/gem.h $(INC)/gam.h 
xpicks.o:          xpicks.c $(INC)/mach.h $(INC)/gam.h 
xplotpm.o:         xplotpm.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                   $(INC)/mem.h $(INC)/gem.h $(INC)/gam.h 
xplotxy.o:         xplotxy.c $(INC)/mach.h $(INC)/gem.h $(INC)/gam.h \
                   $(INC)/dfm.h $(INC)/hdr.h $(INC)/mem.h $(INC)/xyz.h
xppk.o:            xppk.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                   $(INC)/mem.h $(INC)/gem.h $(INC)/eam.h $(INC)/gam.h 
xprint.o:          xprint.c $(INC)/gd2.h $(INC)/msg.h
xsetdevice.o:      xsetdevice.c $(INC)/mach.h $(INC)/gdm.h $(INC)/gam.h 
xylim.o:           xylim.c $(INC)/mach.h $(INC)/gam.h 
xplotalpha.o:      xplotalpha.c
xplotdy.o:         xplotdy.c $(INC)/mach.h $(INC)/gem.h $(INC)/gam.h \
                   $(INC)/dfm.h $(INC)/hdr.h $(INC)/mem.h $(INC)/xyz.h 
