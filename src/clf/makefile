CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB  = ../../lib/libclf.a

LIBOBJS = fndelcl.o ldelcl.o lnumcl.o lnxtcl.o nfndcl.o putcl.o 


library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 



fndelcl.o: fndelcl.c
ldelcl.o:  ldelcl.c $(INC)/mach.h 
lnumcl.o:  lnumcl.c 
lnxtcl.o:  lnxtcl.c 
nfndcl.o:  nfndcl.c $(INC)/mach.h 
putcl.o:   putcl.c 
