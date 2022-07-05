CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB    = ../../lib/libmsg.a

LIBOBJS = apcmsg.o     apcmsg2.o    apcmsgnum.o apfmsg.o apimsg.o \
          autooutmsg.o clrmsg.o     getsmsg.o   aplmsg.o \
          inimsg.o     inquiremsg.o outmsg.o    pltmsg.o sacmsg.o \
          sendmesg.o   setmsg.o     typmsg.o    wrtmsg.o

library: $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)



clean:
	/bin/rm -f *.o 


apcmsg.o:      apcmsg.c $(INC)/mach.h $(INC)/msg.h 
apcmsg2.o:     apcmsg2.c $(INC)/mach.h $(INC)/msg.h 
apcmsgnum.o:   apcmsgnum.c $(INC)/mach.h 
apfmsg.o:      apfmsg.c $(INC)/mach.h $(INC)/msg.h 
apimsg.o:      apimsg.c $(INC)/mach.h $(INC)/msg.h 
aplmsg.o:      aplmsg.c $(INC)/mach.h $(INC)/msg.h 
autooutmsg.o:  autooutmsg.c $(INC)/mach.h $(INC)/msg.h 
clrmsg.o:      clrmsg.c $(INC)/mach.h $(INC)/msg.h 
getsmsg.o:     getsmsg.c $(INC)/mach.h $(INC)/msg.h 
inimsg.o:      inimsg.c $(INC)/mach.h $(INC)/msg.h 
inquiremsg.o:  inquiremsg.c $(INC)/mach.h $(INC)/msg.h 
outmsg.o:      outmsg.c $(INC)/mach.h $(INC)/msg.h 
pltmsg.o:      pltmsg.c $(INC)/mach.h $(INC)/gem.h $(INC)/msg.h 
sacmsg.o:      sacmsg.c $(INC)/mach.h $(INC)/msg.h 
sendmesg.o:    sendmesg.c $(INC)/mach.h $(INC)/msg.h 
setmsg.o:      setmsg.c $(INC)/mach.h $(INC)/msg.h 
typmsg.o:      typmsg.c $(INC)/mach.h $(INC)/msg.h 
wrtmsg.o:      wrtmsg.c $(INC)/mach.h $(INC)/msg.h 
