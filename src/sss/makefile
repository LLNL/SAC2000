CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB  = ../../lib/libsss.a

INC1 = $(INC)/mach.h $(INC)/sss.h
INC2 = $(INC)/dfm.h $(INC)/hdr.h $(INC)/mem.h

LIBOBJS = definelimits.o inisss.o timecrossing.o vmcalc.o vmdly.o vmline.o \
          xaddstack.o xchangestack.o xdeletestack.o xdeltacheck.o xdistanceaxis.o \
          xdistancewind.o xglobalstack.o xincrementsta.o xliststack.o \
          xplotrecords.o xplotstack.o xquitsss.o xsss.o xsssc.o xsumstack.o \
          xtimeaxis.o xtimewindow.o xvelocitymode.o xvelocityrose.o xwritestack.o \
          xzerostack.o xtraveltime.o rscursor.o timeadj.o velocityadj.o phaseadj.o \
          xphase.o iaspmodel.o libtau.o ttint.o

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 

definelimits.o:   definelimits.c  $(INC)/mach.h 
inisss.o:         inisss.c        $(INC1) $(INC)/hdr.h $(INC)/tt.h
vmcalc.o:         vmcalc.c        $(INC1) $(INC2) $(INC)/com.h 
vmdly.o:          vmdly.c         $(INC1) $(INC2) 
vmline.o:         vmline.c        $(INC1) $(INC2) $(INC)/gem.h 
timecrossing.o:   timecrossing.c
xaddstack.o:      xaddstack.c     $(INC1) $(INC2) 
xchangestack.o:   xchangestack.c  $(INC1) $(INC2) 
xdeletestack.o:   xdeletestack.c  $(INC1) $(INC2)  
xdeltacheck.o:    xdeltacheck.c   $(INC1) 
xdistanceaxis.o:  xdistanceaxis.c $(INC1) 
xdistancewind.o:  xdistancewind.c $(INC1) 
xglobalstack.o:   xglobalstack.c  $(INC1) 
xincrementsta.o:  xincrementsta.c $(INC1) $(INC2) 
xliststack.o:     xliststack.c    $(INC1) $(INC2) 
xplotrecords.o:   xplotrecords.c  $(INC1) $(INC)/gem.h $(INC)/gam.h $(INC2)  $(INC)/tt.h
xplotstack.o:     xplotstack.c    $(INC1) $(INC)/gem.h $(INC)/gam.h $(INC2) 
xquitsss.o:       xquitsss.c      $(INC1) $(INC)/mem.h 
xsss.o:           xsss.c          $(INC1) $(INC)/dfm.h $(INC)/hdr.h 
xsssc.o:          xsssc.c         $(INC1) 
xsumstack.o:      xsumstack.c     $(INC1) $(INC2) $(INC)/gem.h $(INC)/gam.h 
xtimeaxis.o:      xtimeaxis.c     $(INC1) 
xtimewindow.o:    xtimewindow.c   $(INC1) 
xvelocitymode.o:  xvelocitymode.c $(INC1) 
xvelocityrose.o:  xvelocityrose.c $(INC1) $(INC)/gam.h  
xwritestack.o:    xwritestack.c   $(INC1) $(INC)/hdr.h $(INC)/mem.h 
xzerostack.o:     xzerostack.c    $(INC1) $(INC)/hdr.h $(INC)/mem.h 
xtraveltime.o:    xtraveltime.c   $(INC1) $(INC2) $(INC)/tt.h 
rscursor.o:       rscursor.c      $(INC)/mach.h $(INC)/gem.h $(INC)/gam.h $(INC)/mem.h
ttint.o:          ttint.c         $(INC)/mach.h $(INC)/gem.h
timeadj.o:        timeadj.c       $(INC)/mach.h $(INC)/tt.h $(INC)/mem.h
xphase.o:         xphase.c        $(INC1) $(INC)/tt.h 
iaspmodel.o:      iaspmodel.c     $(INC1) $(INC)/dfm.h $(INC)/hdr.h  $(INC)/mem.h $(INC)/tt.h 
velocityadj.o:    velocityadj.c
phaseadj.o:       phaseadj.c
libtau.o:         libtau.c ttlim.h
