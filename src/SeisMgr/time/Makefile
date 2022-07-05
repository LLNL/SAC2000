CC=gcc
CFLAGS=$(SACCFLAGS)
.c.o:
	$(CC) $(CFLAGS) -c  $< 

library:  timefuncs.o
	ar -r ../../../lib/libsac.a *.o	   

clean:
	/bin/rm -f *.o
	
timefuncs.o: timefuncs.c timefuncs.h
