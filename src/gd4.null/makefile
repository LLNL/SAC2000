CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) -I/usr/X11R5/include $<

LIB  = ../../lib/libgd4.null.a

LIBOBJS = begindevice4.o beginframe4.o beginwindow4.o \
	  createwindo4.o cursor4.o cursortext4.o \
	  draw4.o enddevice4.o endframe4.o erase4.o \
	  flushbuffer4.o getalphainf4.o \
	  getdevicein4.o \
	  getdevicera4.o getratio4.o getwindowst4.o initdevice4.o \
	  move4.o setcolor4.o setctable4.o \
	  setlinestyl4.o settextsize4.o setwidth4.o \
	  drawpoly4.o batchon4.o batchoff4.o batchshow4.o

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 

batchon4.o:      batchon4.c
batchshow4.o:    batchshow4.c
batchoff4.o:     batchoff4.c
begindevice4.o:  begindevice4.c
beginframe4.o:   beginframe4.c 
beginwindow4.o:  beginwindow4.c
createwindo4.o:  createwindo4.c
cursor4.o:       cursor4.c
cursortext4.o:   cursortext4.c
draw4.o:         draw4.c
drawpoly4.o:     drawpoly4.c
enddevice4.o:    enddevice4.c
endframe4.o:     endframe4.c
erase4.o:        erase4.c
flushbuffer4.o:  flushbuffer4.c 
getalphainf4.o:  getalphainf4.c
getdevicein4.o:  getdevicein4.c
getdevicera4.o:  getdevicera4.c
getratio4.o:     getratio4.c
getwindowst4.o:  getwindowst4.c
initdevice4.o:   initdevice4.c
move4.o:         move4.c
setcolor4.o:     setcolor4.c
setctable4.o:    setctable4.c
setlinestyl4.o:  setlinestyl4.c
settextsize4.o:  settextsize4.c
setwidth4.o:     setwidth4.c


