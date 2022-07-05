CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB  = ../../lib/libpl.a

LIBOBJS = centxt.o incat.o inicol.o inigem.o inilin.o iniwidth.o \
          logdta.o pl2d.o plalpha.o plblank.o plcalwvtrans.o plclip.o \
          pldta.o plgrid.o plhome.o plmap.o plnocl.o plplab.o \
          plrest.o plsave.o pltext.o xlinax.o xlogax.o ylinax.o ylogax.o

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 

centxt.o:        centxt.c $(INC)/mach.h $(INC)/gem.h 
incat.o:         incat.c 
inicol.o:        inicol.c $(INC)/mach.h 
inigem.o:        inigem.c $(INC)/mach.h $(INC)/gem.h 
inilin.o:        inilin.c $(INC)/mach.h 
iniwidth.o:      iniwidth.c $(INC)/mach.h $(INC)/gem.h
logdta.o:        logdta.c $(INC)/mach.h 
pl2d.o:          pl2d.c $(INC)/mach.h $(INC)/gem.h 
plalpha.o:       plalpha.c $(INC)/mach.h $(INC)/gem.h $(INC)/gam.h \
                 $(INC)/dfm.h $(INC)/hdr.h $(INC)/mem.h
plblank.o:       plblank.c $(INC)/mach.h $(INC)/gem.h 
plcalwvtrans.o:  plcalwvtrans.c $(INC)/mach.h $(INC)/gem.h 
plclip.o:        plclip.c $(INC)/mach.h $(INC)/gem.h 
pldta.o:         pldta.c $(INC)/mach.h $(INC)/gem.h  $(INC)/gdm.h
plgrid.o:        plgrid.c $(INC)/mach.h $(INC)/gem.h 
plhome.o:        plhome.c $(INC)/mach.h 
plmap.o:         plmap.c $(INC)/mach.h $(INC)/gem.h 
plnocl.o:        plnocl.c $(INC)/mach.h $(INC)/gem.h 
plplab.o:        plplab.c $(INC)/mach.h $(INC)/gem.h 
plrest.o:        plrest.c $(INC)/mach.h $(INC)/gem.h 
plsave.o:        plsave.c $(INC)/mach.h $(INC)/gem.h 
pltext.o:        pltext.c $(INC)/gem.h
xlinax.o:        xlinax.c $(INC)/mach.h $(INC)/gem.h 
xlogax.o:        xlogax.c $(INC)/mach.h $(INC)/gem.h 
ylinax.o:        ylinax.c $(INC)/mach.h $(INC)/gem.h 
ylogax.o:        ylogax.c $(INC)/mach.h $(INC)/gem.h 
