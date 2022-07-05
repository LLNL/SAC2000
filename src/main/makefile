CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o:
	$(CC) $(CFLAGS) -c $(SAC_INCLUDES) $< 

sac.o:  sac.c ../../inc/mach.h ../../inc/exm.h 

clean:
	/bin/rm -f *.o 
