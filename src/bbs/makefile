CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o:
	$(CC) $(CFLAGS) -c  $< 

LIBOBJS = createbbs.o deletebbs.o getbbv.o inibbs.o readbbf.o \
          setbbv.o unsetbbv.o writebbf.o

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)
 
clean:
	/bin/rm -f *.o 
	    
createbbs.o:  createbbs.c $(INC)/mach.h $(INC)/bbs.h 
deletebbs.o:  deletebbs.c $(INC)/mach.h $(INC)/bbs.h 
getbbv.o:     getbbv.c $(INC)/mach.h $(INC)/bbs.h 
inibbs.o:     inibbs.c $(INC)/mach.h $(INC)/bbs.h 
readbbf.o:    readbbf.c $(INC)/mach.h $(INC)/mem.h $(INC)/bbs.h 
setbbv.o:     setbbv.c $(INC)/mach.h $(INC)/bbs.h 
unsetbbv.o:   unsetbbv.c $(INC)/mach.h $(INC)/bbs.h 
writebbf.o:   writebbf.c $(INC)/mach.h $(INC)/bbs.h 
