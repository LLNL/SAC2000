CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<


LIB  = ../../lib/libdatafilelist.a

LIBOBJS = nextinputfile.o selectinputfi.o setinputmode.o 

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 

nextinputfile.o:  nextinputfile.c $(INC)/mach.h $(INC)/datafilelist.h 
selectinputfi.o:  selectinputfi.c $(INC)/mach.h $(INC)/datafilelist.h 
setinputmode.o:   setinputmode.c $(INC)/mach.h $(INC)/datafilelist.h 
