CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

OS := $(shell uname)

ifeq ($(OS), Darwin)
   X11_INCLUDE=-I/usr/X11R6/include
else
   X11_INCLUDE=-I/usr/include/X11
endif

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $(X11_INCLUDE) -I/usr/X11R5/include  $<

LIB  = ../../lib/libgd2.a

LIBOBJS = begindevice2.o beginframe2.o  beginSGFtemp.o beginwindow2.o \
          calc_loc2.o    cbar_window2.o changectable2.o \
          createwindo2.o cursor2.o      cursortext2.o draw2.o \
          enddevice2.o   endframe2.o    endSGFtemp.o erase2.o fill_clrbar2.o \
          fill_image2.o  flushbuffer2.o getalphainf2.o \
          getdevicein2.o getdevicera2.o get_geometry2.o getratio2.o \
          getwindowst2.o hardwaretex2.o initdevice2.o move2.o \
          put_image2.o   setcolor2.o     setctable2.o setlinestyl2.o setsgfdir.o \
          setsgfnumber.o setsgfprefix.o setsgfsize.o \
	  settextangl2.o setwidth2.o    settextsize2.o

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 

begindevice2.o:  begindevice2.c 
beginframe2.o:   beginframe2.c $(INC)/mach.h $(INC)/gd2.h 
beginwindow2.o:  beginwindow2.c 
beginSGFtemp.o:  beginSGFtemp.c $(INC)/gdm.h $(INC)/gem.h $(INC)/gd2.h
calc_loc2.o:     calc_loc2.c $(INC)/gd2.h
cbar_window2.o:  cbar_window2.c $(INC)/gem.h $(INC)/gd2.h
changectable2.o: changectable2.c $(INC)/mach.h $(INC)/gd2.h $(INC)/sgfcolor.h
createwindo2.o:  createwindo2.c $(INC)/mach.h 
cursor2.o:       cursor2.c 
cursortext2.o:   cursortext2.c 
draw2.o:         draw2.c $(INC)/mach.h $(INC)/gd2.h 
enddevice2.o:    enddevice2.c 
endframe2.o:     endframe2.c $(INC)/mach.h $(INC)/gd2.h 
endSGFtemp.o:    endSGFtemp.c $(INC)/gdm.h $(INC)/gd2.h
erase2.o:        erase2.c 
fill_clrbar2.o:  fill_clrbar2.c $(INC)/mach.h $(INC)/gd2.h
fill_image2.o:   fill_image2.c $(INC)/mach.h $(INC)/gd2.h
flushbuffer2.o:  flushbuffer2.c $(INC)/mach.h $(INC)/gd2.h 
getalphainf2.o:  getalphainf2.c 
getdevicein2.o:  getdevicein2.c $(INC)/mach.h $(INC)/gd2.h 
getdevicera2.o:  getdevicera2.c $(INC)/mach.h 
get_geometry2.o: get_geometry2.c $(INC)/mach.h $(INC)/gd2.h
getratio2.o:     getratio2.c $(INC)/mach.h $(INC)/gd2.h 
getwindowst2.o:  getwindowst2.c $(INC)/mach.h 
hardwaretex2.o:  hardwaretex2.c $(INC)/mach.h $(INC)/gd2.h 
initdevice2.o:   initdevice2.c $(INC)/mach.h $(INC)/gd2.h 
move2.o:         move2.c $(INC)/mach.h $(INC)/gd2.h 
put_image2.o:    put_image2.c $(INC)/mach.h $(INC)/gd2.h
setcolor2.o:     setcolor2.c $(INC)/mach.h $(INC)/gd2.h 
setctable2.o:    setctable2.c $(INC)/gd2.h
setlinestyl2.o:  setlinestyl2.c $(INC)/mach.h $(INC)/gd2.h 
setsgfdir.o:     setsgfdir.c $(INC)/mach.h $(INC)/gd2.h 
setsgfnumber.o:  setsgfnumber.c $(INC)/mach.h $(INC)/gd2.h 
setsgfprefix.o:  setsgfprefix.c $(INC)/mach.h $(INC)/gd2.h 
setsgfsize.o:    setsgfsize.c $(INC)/mach.h $(INC)/gd2.h 
settextsize2.o:  settextsize2.c $(INC)/mach.h $(INC)/gd2.h 
setwidth2.o:     setwidth2.c $(INC)/mach.h $(INC)/gd2.h 
settextangl2.o:  settextangl2.c $(INC)/mach.h $(INC)/gd2.h 
