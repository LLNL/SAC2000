CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB  = ../../lib/libcnd.a


LIBOBJS = getclun.o getdolen.o ldolist.o skipdo.o skipif.o \
          xbreak.o xcndc.o xdo.o xelse.o xelseif.o xenddo.o \
          xendif.o xif.o xwhile.o 

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 


getclun.o:  getclun.c $(INC)/mach.h $(INC)/cpf.h 
getdolen.o: getdolen.c $(INC)/mach.h $(INC)/cnd.h 
ldolist.o:  ldolist.c $(INC)/mach.h $(INC)/cnd.h 
skipdo.o:   skipdo.c $(INC)/mach.h $(INC)/cnd.h 
skipif.o:   skipif.c $(INC)/mach.h $(INC)/cnd.h 
xbreak.o:   xbreak.c $(INC)/mach.h $(INC)/cnd.h 
xcndc.o:    xcndc.c $(INC)/mach.h $(INC)/cnd.h 
xdo.o:      xdo.c $(INC)/mach.h $(INC)/cnd.h $(INC)/cpf.h 
xelse.o:    xelse.c $(INC)/mach.h $(INC)/cnd.h 
xelseif.o:  xelseif.c $(INC)/mach.h $(INC)/cnd.h 
xenddo.o:   xenddo.c $(INC)/mach.h $(INC)/cnd.h 
xendif.o:   xendif.c $(INC)/mach.h $(INC)/cnd.h 
xif.o:      xif.c $(INC)/mach.h $(INC)/cnd.h 
xwhile.o:   xwhile.c $(INC)/mach.h $(INC)/cnd.h $(INC)/cpf.h 
