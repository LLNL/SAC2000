CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB  = ../../lib/libfks.a

LIBOBJS = inifks.o xfksc.o xgmtmap.o     xmap.o \
          xbbfk.o  xbeam.o calcoffsets.o calcBeamOffsets.o

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 


inifks.o:          inifks.c $(INC)/mach.h $(INC)/fks.h
xfksc.o:           xfksc.c $(INC)/mach.h $(INC)/gam.h $(INC)/fks.h
xmap.o:            xmap.c $(INC)/mach.h $(INC)/gem.h $(INC)/gam.h $(INC)/hdr.h $(INC)/dfm.h $(INC)/fks.h
xgmtmap.o:         xgmtmap.c $(INC)/mach.h $(INC)/hdr.h $(INC)/dfm.h $(INC)/fks.h
xbbfk.o:           xbbfk.c $(INC)/mach.h $(INC)/mem.h $(INC)/hdr.h $(INC)/dfm.h $(INC)/fks.h
xbeam.o:           xbeam.c $(INC)/mach.h $(INC)/mem.h $(INC)/hdr.h $(INC)/dfm.h $(INC)/fks.h
calcoffsets.o:     calcoffsets.c $(INC)/complex.h $(INC)/proto.h $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h $(INC)/mem.h $(INC)/fks.h
calcBeamOffsets.o: calcBeamOffsets.c $(INC)/complex.h $(INC)/proto.h $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h $(INC)/mem.h $(INC)/fks.h
