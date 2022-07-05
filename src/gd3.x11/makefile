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

LIB  = ../../lib/libgd3.x11.a

LIBOBJS = begindevice3.o  beginframe3.o  beginwindow3.o calc_loc3.o    cbar_window3.o \
  	  changectable3.o createwindo3.o cursor3.o      cursortext3.o  dispatcheve3.o draw3.o \
          enddevice3.o    endframe3.o    erase3.o       fill_clrbar3.o fill_image3.o flushbuffer3.o getalphainf3.o \
          getdevicein3.o  getdevicera3.o getratio3.o    getwindowst3.o get_geometry3.o \
          initdevice3.o   make_label3.o  move3.o        put_image3.o   setcolor3.o setctable3.o \
          setlinestyl3.o  setpsctable3.o settextsize3.o setwidth3.o    drawpoly3.o

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 



begindevice3.o:  begindevice3.c $(INC)/gd3.x11.h
beginframe3.o:   beginframe3.c  erase3.o
beginwindow3.o:  beginwindow3.c $(INC)/gd3.x11.h
changectable3.o: changectable3.c $(INC)/gd3.x11.h
calc_loc3.o:     calc_loc3.c
cbar_window3.o:  cbar_window3.c $(INC)/gem.h
createwindo3.o:  createwindo3.c	$(INC)/gd3.x11.h
cursor3.o:       cursor3.c $(INC)/gd3.x11.h
cursortext3.o:   cursortext3.c $(INC)/gd3.x11.h
dispatcheve3.o:  dispatcheve3.c $(INC)/gd3.x11.h
draw3.o:         draw3.c $(INC)/gd3.x11.h
drawpoly3.o:     drawpoly3.c $(INC)/gd3.x11.h 
enddevice3.o:    enddevice3.c $(INC)/gd3.x11.h
endframe3.o:     endframe3.c
erase3.o:        erase3.c $(INC)/gd3.x11.h
fill_clrbar3.o:  fill_clrbar3.c $(INC)/gd3.x11.h
fill_image3.o:   fill_image3.c $(INC)/gd3.x11.h
flushbuffer3.o:  flushbuffer3.c $(INC)/gd3.x11.h
getalphainf3.o:  getalphainf3.c
getdevicein3.o:  getdevicein3.c	$(INC)/gd3.x11.h
getdevicera3.o:  getdevicera3.c $(INC)/gd3.x11.h
getratio3.o:     getratio3.c $(INC)/gd3.x11.h
getwindowst3.o:  getwindowst3.c  $(INC)/gd3.x11.h
get_geometry3.o: get_geometry3.c $(INC)/gd3.x11.h
initdevice3.o:   initdevice3.c $(INC)/gd3.x11.h
make_label3.o:   make_label3.c
move3.o:         move3.c $(INC)/gd3.x11.h
put_image3.o:    put_image3.c $(INC)/gd3.x11.h
setcolor3.o:     setcolor3.c $(INC)/gd3.x11.h
setctable3.o:    setctable3.c $(INC)/gd3.x11.h
setlinestyl3.o:  setlinestyl3.c
setpsctable3.o:  setpsctable3.c
settextsize3.o:  settextsize3.c
setwidth3.o:     setwidth3.c
