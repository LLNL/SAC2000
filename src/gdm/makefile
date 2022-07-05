CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) -I/usr/X11R5/include $<

LIB  = ../../lib/libgdm.a

LIBOBJS = adj_geometry.o begindevice.o begindevices.o beginframe.o \
          beginwindow.o calstatus.o calvspace.o changectable.o convcolornam.o \
          convcolornum.o createwindow.o cursor0.o cursoroff.o cursoron.o \
          cursortext.o draw.o enddevice.o endframe.o endgraphics.o erase.o \
          fill_colorbar.o fill_image.o flushbuffer.o getalphainfo.o \
          getdevicein1.o getdevicenam.o getdevicerat.o getlinestyle.o \
          getmaxdevice.o getratio.o getstatus.o getstringsiz.o \
          gettextjust.o gettextsize.o getvspace.o getvspacetyp.o getwindowsta.o \
          get_geometry.o inigdm.o initctable.o move.o loadctable.o put_image.o \
          readctable.o setcolor.o setcolorname.o \
          setlinestyle.o setlinewidth.o setpsctable.o settextangle.o settextfont.o \
          settextjust.o settextsize.o settexttype.o setvspacecli.o setvspacetyp.o \
          softwaretext.o stubs1.o text.o

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 




adj_geometry.o:  adj_geometry.c
begindevice.o:   begindevice.c $(INC)/mach.h 
begindevices.o:  begindevices.c $(INC)/mach.h $(INC)/gdm.h $(INC)/gem.h
beginframe.o:    beginframe.c $(INC)/mach.h $(INC)/gdm.h $(INC)/gam.h 
beginwindow.o:   beginwindow.c $(INC)/mach.h $(INC)/gdm.h 
calstatus.o:     calstatus.c $(INC)/mach.h $(INC)/gdm.h 
calvspace.o:     calvspace.c $(INC)/mach.h $(INC)/gdm.h 
changectable.o:  changectable.c $(INC)/mach.h
convcolornam.o:  convcolornam.c $(INC)/mach.h $(INC)/gdm.h 
convcolornum.o:  convcolornum.c $(INC)/mach.h $(INC)/gdm.h 
createwindow.o:  createwindow.c $(INC)/mach.h $(INC)/gdm.h 
cursor0.o:       cursor0.c $(INC)/mach.h $(INC)/gdm.h 
cursoroff.o:     cursoroff.c $(INC)/mach.h $(INC)/gdm.h 
cursoron.o:      cursoron.c $(INC)/mach.h $(INC)/gdm.h 
cursortext.o:    cursortext.c $(INC)/mach.h $(INC)/gdm.h 
draw.o:          draw.c $(INC)/mach.h $(INC)/gdm.h 
enddevice.o:     enddevice.c $(INC)/mach.h $(INC)/gdm.h 
endframe.o:      endframe.c $(INC)/mach.h $(INC)/gdm.h $(INC)/gd2.h
endgraphics.o:   endgraphics.c $(INC)/mach.h $(INC)/gdm.h 
erase.o:         erase.c $(INC)/mach.h $(INC)/gdm.h 
fill_colorbar.o: fill_colorbar.c $(INC)/mach.h $(INC)/gdm.h
fill_image.o:    fill_image.c $(INC)/mach.h $(INC)/gdm.h
flushbuffer.o:   flushbuffer.c $(INC)/mach.h $(INC)/gdm.h 
getalphainfo.o:  getalphainfo.c $(INC)/mach.h $(INC)/gdm.h 
getdevicein1.o:  getdevicein1.c $(INC)/mach.h
getdevicenam.o:  getdevicenam.c $(INC)/mach.h $(INC)/gdm.h 
getdevicerat.o:  getdevicerat.c $(INC)/mach.h $(INC)/gdm.h 
getlinestyle.o:  getlinestyle.c $(INC)/mach.h $(INC)/gdm.h 
getmaxdevice.o:  getmaxdevice.c $(INC)/mach.h $(INC)/gdm.h 
getratio.o:      getratio.c $(INC)/mach.h $(INC)/gdm.h 
getstatus.o:     getstatus.c $(INC)/mach.h $(INC)/gdm.h 
getstringsiz.o:  getstringsiz.c $(INC)/mach.h $(INC)/gdm.h 
gettextjust.o:   gettextjust.c $(INC)/mach.h $(INC)/gdm.h 
gettextsize.o:   gettextsize.c $(INC)/mach.h $(INC)/gdm.h 
getvspace.o:     getvspace.c $(INC)/mach.h $(INC)/gdm.h 
getvspacetyp.o:  getvspacetyp.c $(INC)/mach.h $(INC)/gdm.h 
getwindowsta.o:  getwindowsta.c $(INC)/mach.h $(INC)/gdm.h 
get_geometry.o:  get_geometry.c
inigdm.o:        inigdm.c $(INC)/mach.h $(INC)/gdm.h 
initctable.o:    initctable.c $(INC)/mach.h $(INC)/gdm.h 
loadctable.o:    loadctable.c $(INC)/mach.h $(INC)/gdm.h 
move.o:          move.c $(INC)/mach.h $(INC)/gdm.h 
put_image.o:     put_image.c $(INC)/mach.h $(INC)/gdm.h
readctable.o:    readctable.c $(INC)/mach.h 
setcolor.o:      setcolor.c $(INC)/mach.h $(INC)/gdm.h 
setcolorname.o:  setcolorname.c $(INC)/mach.h $(INC)/gdm.h 
setlinestyle.o:  setlinestyle.c $(INC)/mach.h $(INC)/gdm.h 
setlinewidth.o:  setlinewidth.c $(INC)/mach.h  $(INC)/gem.h $(INC)/gdm.h
setpsctable.o:   setpsctable.c $(INC)/mach.h $(INC)/gdm.h $(INC)/gam.h
settextangle.o:  settextangle.c $(INC)/mach.h $(INC)/gdm.h 
settextfont.o:   settextfont.c $(INC)/mach.h $(INC)/gdm.h 
settextjust.o:   settextjust.c $(INC)/mach.h $(INC)/gdm.h 
settextsize.o:   settextsize.c $(INC)/mach.h $(INC)/gdm.h 
settexttype.o:   settexttype.c $(INC)/mach.h $(INC)/gdm.h 
setvspacecli.o:  setvspacecli.c $(INC)/mach.h $(INC)/gdm.h 
setvspacetyp.o:  setvspacetyp.c $(INC)/mach.h $(INC)/gdm.h 
softwaretext.o:  softwaretext.c $(INC)/mach.h $(INC)/gdm.h 
text.o:          text.c $(INC)/mach.h $(INC)/gdm.h $(INC)/gem.h

