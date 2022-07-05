CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) -I/usr/X11R5/include $<

LIB  = ../../lib/libgem.a

LIBOBJS = inisym.o xaxes.o xbeginframe.o xbeginwindow.o \
          xcolor.o xendframe.o xgemc.o xgrid.o xgt.o xlct.o xline.o \
          xplab.o xqdp.o xsym.o xticks.o xtitle.o \
          xtsize.o xwait.o xwidth.o xwindow.o xxdiv.o xxgrid.o xxlab.o \
          xydiv.o xygrid.o xylab.o 

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o

inisym.o:        inisym.c $(INC)/mach.h 
xaxes.o:         xaxes.c $(INC)/mach.h $(INC)/gem.h 
xbeginframe.o:   xbeginframe.c $(INC)/gem.h
xbeginwindow.o:  xbeginwindow.c $(INC)/mach.h $(INC)/gdm.h 
xcolor.o:        xcolor.c $(INC)/mach.h $(INC)/gem.h 
xendframe.o:     xendframe.c
xgemc.o:         xgemc.c $(INC)/mach.h $(INC)/gem.h 
xgrid.o:         xgrid.c $(INC)/mach.h $(INC)/gem.h 
xgt.o:           xgt.c $(INC)/mach.h $(INC)/gem.h $(INC)/hdr.h  $(INC)/gam.h $(INC)/gdm.h 
xlct.o:          xlct.c $(INC)/gdm.h
xline.o:         xline.c $(INC)/mach.h $(INC)/gem.h 
xplab.o:         xplab.c $(INC)/mach.h $(INC)/gem.h 
xqdp.o:          xqdp.c $(INC)/mach.h $(INC)/gem.h 
xsym.o:          xsym.c $(INC)/mach.h $(INC)/gem.h 
xticks.o:        xticks.c $(INC)/mach.h $(INC)/gem.h 
xtitle.o:        xtitle.c $(INC)/mach.h $(INC)/gem.h 
xtsize.o:        xtsize.c $(INC)/mach.h $(INC)/gem.h 
xwait.o:         xwait.c $(INC)/mach.h $(INC)/gam.h 
xwidth.o:        xwidth.c $(INC)/gem.h 
xwindow.o:       xwindow.c $(INC)/mach.h $(INC)/gdm.h 
xxdiv.o:         xxdiv.c $(INC)/mach.h $(INC)/gem.h 
xxgrid.o:        xxgrid.c $(INC)/mach.h $(INC)/gem.h 
xxlab.o:         xxlab.c $(INC)/mach.h $(INC)/gem.h 
xydiv.o:         xydiv.c $(INC)/mach.h $(INC)/gem.h 
xygrid.o:        xygrid.c $(INC)/mach.h $(INC)/gem.h 
xylab.o:         xylab.c $(INC)/mach.h $(INC)/gem.h 
