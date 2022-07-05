CC=gcc
CFLAGS=$(SACCFLAGS)
.c.o:
	$(CC) $(CFLAGS) -c  $< 

OBJS =  cssArchitecture.o cssListOps.o cssTableDump.o dbFillWfdisc.o \
          dblErrors.o dblGetDataSubs.o dblUserData.o enlarge.o g2tofloat.o \
	  isValid.o byteswap.o cancomp.o dblReadE1.o

library:  $(OBJS)
	ar -r ../../../lib/libsac.a *.o	   

clean:
	/bin/rm -f *.o

{OBJS}: cssArchitecture.h cssListOps.h cssListStrucs.h cssStrucs.h dblErrors.h \
        dblGetDataSubs.h dblPublicDefs.h dblUserData.h
		
cssArchitecture.o: cssArchitecture.c
cssListOps.o:      cssListOps.c
cssTableDump.o:    cssTableDump.c
dbFillWfdisc.o:    dbFillWfdisc.c
dblErrors.o:       dblErrors.c
dblGetDataSubs.o:  dblGetDataSubs.c
dblUserData.o:     dblUserData.c
enlarge.o:         enlarge.c
g2tofloat.o:       g2tofloat.c
isValid.o:         isValid.c
byteswap.o:	   byteswap.c
cancomp.o:	   cancomp.c 
dblReadE1.o:	   dblReadE1.c
