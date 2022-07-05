CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) -I/usr/X11R5/include $<

LIB  = ../../lib/libgpm.a

LIBOBJS = begingraphic.o 

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)


clean:
	/bin/rm -f *.o 

begingraphic.o:  begingraphic.c 
