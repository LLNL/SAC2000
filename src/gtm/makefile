CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) -I/usr/X11R5/include $<

LIB  = ../../lib/libgtm.a

LIBOBJS = calwvtransfo.o \
          getvport.o getworld.o inigtm.o line.o \
          polyline.o rectangle.o setsymbolgap.o setsymbolnum.o \
          setsymbolsiz.o setvport.o setvportrat.o setworld.o \
          symbol.o vporttoworld.o worldcursor.o worlddraw.o \
          worldline.o worldmove.o worldpolylin.o worldsector.o \
          worldtovport.o xaxis.o xlinaxis.o xlogaxis.o yaxis.o \
          ylinaxis.o ylogaxis.o 

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 



calwvtransfo.o:  calwvtransfo.c $(INC)/mach.h $(INC)/gtm.h 
getvport.o:      getvport.c $(INC)/mach.h $(INC)/gtm.h 
getworld.o:      getworld.c $(INC)/mach.h $(INC)/gtm.h 
inigtm.o:        inigtm.c $(INC)/mach.h $(INC)/gtm.h 
line.o:          line.c 
polyline.o:      polyline.c 
rectangle.o:     rectangle.c 
setsymbolgap.o:  setsymbolgap.c $(INC)/mach.h $(INC)/gtm.h 
setsymbolnum.o:  setsymbolnum.c $(INC)/mach.h $(INC)/gtm.h 
setsymbolsiz.o:  setsymbolsiz.c $(INC)/mach.h $(INC)/gtm.h 
setvport.o:      setvport.c $(INC)/mach.h $(INC)/gtm.h 
setvportrat.o:   setvportrat.c $(INC)/mach.h $(INC)/gtm.h 
setworld.o:      setworld.c $(INC)/mach.h $(INC)/gtm.h 
symbol.o:        symbol.c $(INC)/mach.h $(INC)/gtm.h 
vporttoworld.o:  vporttoworld.c $(INC)/mach.h $(INC)/gtm.h 
worldcursor.o:   worldcursor.c $(INC)/mach.h $(INC)/gtm.h 
worlddraw.o:     worlddraw.c $(INC)/mach.h $(INC)/gtm.h $(INC)/gdm.h 
worldline.o:     worldline.c 
worldmove.o:     worldmove.c $(INC)/mach.h $(INC)/gtm.h 
worldpolylin.o:  worldpolylin.c 
worldsector.o:   worldsector.c $(INC)/mach.h 
worldtovport.o:  worldtovport.c $(INC)/mach.h $(INC)/gtm.h 
xaxis.o:         xaxis.c $(INC)/mach.h $(INC)/gtm.h 
xlinaxis.o:      xlinaxis.c $(INC)/mach.h $(INC)/gtm.h 
xlogaxis.o:      xlogaxis.c $(INC)/mach.h $(INC)/gtm.h 
yaxis.o:         yaxis.c $(INC)/mach.h $(INC)/gtm.h 
ylinaxis.o:      ylinaxis.c $(INC)/mach.h $(INC)/gtm.h 
ylogaxis.o:      ylogaxis.c $(INC)/mach.h $(INC)/gtm.h 
