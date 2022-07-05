CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB  = ../../lib/libtop.a

LIBOBJS = executecomman.o findcommand.o initcommon.o initsac.o saccommands.o 

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 

executecomman.o: executecomman.c $(INC)/mach.h $(INC)/comlists.h $(INC)/site.h 
findcommand.o:   findcommand.c $(INC)/mach.h $(INC)/comlists.h $(INC)/site.h 
initcommon.o:    initcommon.c $(INC)/mach.h $(INC)/mem.h $(INC)/csf.h $(INC)/gdm.h $(INC)/dfm.h  $(INC)/cnv.h $(INC)/vars.h $(INC)/dload.h
initsac.o:       initsac.c $(INC)/mach.h $(INC)/exm.h $(INC)/gam.h  $(INC)/sam.h
saccommands.o:   saccommands.c $(INC)/mach.h $(INC)/exm.h $(INC)/cnd.h
