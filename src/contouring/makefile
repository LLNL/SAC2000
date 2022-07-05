CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB  = ../../lib/libcontouring.a


LIBOBJS = alloclabels.o   allocpoints.o   allocsegments.o \
          calccontlab1.o  calccontlab4.o \
          calccontlabs.o  calccontrlink.o calccontsegs.o \
	  calccontticks.o calclastpoint.o calcsegangle.o \
          fastcontdata.o  getcontlabel.o  getcontpoint.o \
	  getcontrlink.o  getcontseg.o    getcontseglab.o \
          initcontattr.o  linkcontsegs.o  listcontsegs.o \
	  markcontlabel.o mergecontsegs.o newcontlabel.o \
          newcontpoint.o  newcontseg.o    nextcontseg.o plotcontdata.o \
          plotcontsegs.o  pointsequal.o   putcontlabel.o \
	  putcontpoint.o  putcontrlink.o  putcontseg.o \
          putcontseglab.o releaselabels.o releasepoints.o \
	  releasesegmen.o setcontdatali.o


library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 

alloclabels.o:    alloclabels.c $(INC)/mem.h 
allocpoints.o:    allocpoints.c $(INC)/mem.h 
allocsegments.o:  allocsegments.c $(INC)/mem.h 
calccontlab1.o:   calccontlab1.c $(INC)/mach.h $(INC)/contouring.h 
calccontlab4.o:   calccontlab4.c $(INC)/mach.h $(INC)/contouring.h 
calccontlabs.o:   calccontlabs.c $(INC)/mach.h $(INC)/contouring.h 
calccontrlink.o:  calccontrlink.c $(INC)/mach.h $(INC)/contouring.h 
calccontsegs.o:   calccontsegs.c $(INC)/mach.h $(INC)/gtm.h $(INC)/contouring.h 
calccontticks.o:  calccontticks.c $(INC)/mach.h $(INC)/contouring.h 
calclastpoint.o:  calclastpoint.c 
calcsegangle.o:   calcsegangle.c $(INC)/mach.h 
fastcontdata.o:   fastcontdata.c $(INC)/mach.h $(INC)/gtm.h $(INC)/contouring.h 
getcontlabel.o:   getcontlabel.c $(INC)/mach.h $(INC)/mem.h $(INC)/contouring.h 
getcontpoint.o:   getcontpoint.c $(INC)/mach.h $(INC)/mem.h $(INC)/contouring.h 
getcontrlink.o:   getcontrlink.c $(INC)/mach.h $(INC)/mem.h $(INC)/contouring.h 
getcontseg.o:     getcontseg.c $(INC)/mach.h $(INC)/mem.h $(INC)/contouring.h 
getcontseglab.o:  getcontseglab.c $(INC)/mach.h $(INC)/mem.h $(INC)/contouring.h 
initcontattr.o:   initcontattr.c $(INC)/mach.h $(INC)/contouring.h 
linkcontsegs.o:   linkcontsegs.c $(INC)/mach.h $(INC)/contouring.h 
listcontsegs.o:   listcontsegs.c $(INC)/mach.h $(INC)/contouring.h 
markcontlabel.o:  markcontlabel.c $(INC)/mach.h $(INC)/contouring.h 
mergecontsegs.o:  mergecontsegs.c $(INC)/mach.h $(INC)/contouring.h 
newcontlabel.o:   newcontlabel.c $(INC)/mach.h $(INC)/mem.h $(INC)/contouring.h 
newcontpoint.o:   newcontpoint.c $(INC)/mach.h $(INC)/mem.h $(INC)/contouring.h 
newcontseg.o:     newcontseg.c $(INC)/mach.h $(INC)/mem.h $(INC)/contouring.h 
nextcontseg.o:    nextcontseg.c $(INC)/mach.h $(INC)/mem.h $(INC)/contouring.h 
plotcontdata.o:   plotcontdata.c $(INC)/mach.h $(INC)/contouring.h 
plotcontsegs.o:   plotcontsegs.c $(INC)/mach.h $(INC)/contouring.h  $(INC)/gem.h
pointsequal.o:    pointsequal.c $(INC)/mach.h 
putcontlabel.o:   putcontlabel.c $(INC)/mach.h $(INC)/mem.h $(INC)/contouring.h 
putcontpoint.o:   putcontpoint.c $(INC)/mach.h $(INC)/mem.h $(INC)/contouring.h 
putcontrlink.o:   putcontrlink.c $(INC)/mach.h $(INC)/mem.h $(INC)/contouring.h 
putcontseg.o:     putcontseg.c $(INC)/mach.h $(INC)/mem.h $(INC)/contouring.h 
putcontseglab.o:  putcontseglab.c $(INC)/mach.h $(INC)/mem.h $(INC)/contouring.h 
releaselabels.o:  releaselabels.c $(INC)/mach.h $(INC)/mem.h $(INC)/contouring.h 
releasepoints.o:  releasepoints.c $(INC)/mach.h $(INC)/mem.h $(INC)/contouring.h 
releasesegmen.o:  releasesegmen.c $(INC)/mach.h $(INC)/mem.h $(INC)/contouring.h 
setcontdatali.o:  setcontdatali.c $(INC)/mach.h $(INC)/contouring.h 
