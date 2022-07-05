CFLAGS = $(SACCFLAGS)

CC = gcc

.c.o:
	$(CC) $(CFLAGS) -c  $< 


LIBOBJS    = allamb.o relamb.o iniam.o reaamb.o

library: $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)


clean:
	/bin/rm -f *.o 

allamb.o:  allamb.c 
iniam.o:   iniam.c 
relamb.o:  relamb.c 
reaamb.o:  reaamb.c 
