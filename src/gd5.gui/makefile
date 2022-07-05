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
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $(X11_INCLUDE) -I/usr/X11R5/include $<

LIB  = ../../lib/libgd5.gui.a

LIBOBJS = begindevice5.o beginframe5.o beginwindow5.o calc_loc5.o cbar_window5.o \
	  changectable5.o createwindo5.o cursor5.o cursortext5.o dispatcheve5.o draw5.o \
          enddevice5.o endframe5.o erase5.o fill_clrbar5.o fill_image5.o flushbuffer5.o getalphainf5.o \
          getdevicein5.o getdevicera5.o getratio5.o getwindowst5.o get_geometry5.o \
          initdevice5.o move5.o put_image5.o setcolor5.o setctable5.o \
          setlinestyl5.o setpsctable5.o settextsize5.o setwidth5.o drawpoly5.o

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o


begindevice5.o:  begindevice5.c $(INC)/gd5.gui.h
beginframe5.o:   beginframe5.c  erase5.o
beginwindow5.o:  beginwindow5.c $(INC)/gd5.gui.h
changectable5.o: changectable5.c $(INC)/gd5.gui.h
calc_loc5.o:     calc_loc5.c
cbar_window5.o:  cbar_window5.c $(INC)/gem.h
createwindo5.o:  createwindo5.c	$(INC)/gd5.gui.h
cursor5.o:       cursor5.c $(INC)/gd5.gui.h
cursortext5.o:   cursortext5.c $(INC)/gd5.gui.h
dispatcheve5.o:  dispatcheve5.c $(INC)/gd5.gui.h
draw5.o:         draw5.c $(INC)/gd5.gui.h
drawpoly5.o:     drawpoly5.c $(INC)/gd5.gui.h 
enddevice5.o:    enddevice5.c $(INC)/gd5.gui.h
endframe5.o:     endframe5.c
erase5.o:        erase5.c $(INC)/gd5.gui.h
fill_clrbar5.o:  fill_clrbar5.c $(INC)/gd5.gui.h
fill_image5.o:   fill_image5.c $(INC)/gd5.gui.h
flushbuffer5.o:  flushbuffer5.c $(INC)/gd5.gui.h
getalphainf5.o:  getalphainf5.c
getdevicein5.o:  getdevicein5.c	$(INC)/gd5.gui.h
getdevicera5.o:  getdevicera5.c $(INC)/gd5.gui.h
getratio5.o:     getratio5.c $(INC)/gd5.gui.h
getwindowst5.o:  getwindowst5.c  $(INC)/gd5.gui.h
get_geometry5.o: get_geometry5.c $(INC)/gd5.gui.h
initdevice5.o:   initdevice5.c $(INC)/gd5.gui.h
move5.o:         move5.c $(INC)/gd5.gui.h
put_image5.o:    put_image5.c $(INC)/gd5.gui.h
setcolor5.o:     setcolor5.c $(INC)/gd5.gui.h
setctable5.o:    setctable5.c $(INC)/gd5.gui.h
setlinestyl5.o:  setlinestyl5.c
setpsctable5.o:  setpsctable5.c
settextsize5.o:  settextsize5.c
setwidth5.o:     setwidth5.c
