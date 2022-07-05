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
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $(X11_INCLUDE) $<

LIB  = ../../lib/libxyz.a

LIBOBJS = adjust_height.o adjust_width.o \
          calcfftsize.o calcloc.o calcsize.o cbar_window.o \
	  flipdata.o getdata.o inixyz.o label_cbar.o \
	  linear_interp.o plotimage.o scaleimage.o scallop.o \
          smooth.o spcgrm.o specplot.o spectrogram.o subtract.o \
          vfxyz.o window_data.o writezdata.o xcontour.o \
	  xgrayscale.o ximage.o xscallop.o xspectrogram.o \
          xxyzc.o xyzcleanup.o xzcolors.o xzlabels.o \
	  xzlevels.o xzlines.o xzticks.o zfft.o
          

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 

adjust_height.o: adjust_height.c $(INC)/proto.h
adjust_width.o:  adjust_width.c $(INC)/proto.h
calcfftsize.o:   calcfftsize.c $(INC)/spectrogram.h 
calcloc.o:       calcloc.c
calcsize.o:      calcsize.c
cbar_window.o:   cbar_window.c
flipdata.o:      flipdata.c 
getdata.o:       getdata.c $(INC)/mach.h $(INC)/spectrogram.h $(INC)/specdata.h $(INC)/mem.h 
inixyz.o:        inixyz.c $(INC)/xyz.h 
label_cbar.o:    label_cbar.c
linear_interp.o: linear_interp.c 
plotimage.o:     plotimage.c $(INC)/mach.h $(INC)/gtm.h $(INC)/gd3.x11.h $(INC)/gdm.h
scaleimage.o:    scaleimage.c $(INC)/mach.h
scallop.o:       scallop.c $(INC)/mach.h
smooth.o:        smooth.c $(INC)/mach.h 
specplot.o:      specplot.c
spcgrm.o:        spcgrm.c 
spectrogram.o:   spectrogram.c $(INC)/mach.h $(INC)/mem.h $(INC)/spectrogram.h $(INC)/specdata.h 
subtract.o:      subtract.c $(INC)/mach.h
vfxyz.o:         vfxyz.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h $(INC)/mem.h 
window_data.o:   window_data.c
writezdata.o:    writezdata.c 
xcontour.o:      xcontour.c $(INC)/mach.h $(INC)/gem.h $(INC)/gam.h $(INC)/mem.h $(INC)/xyz.h 
xgrayscale.o:    xgrayscale.c $(INC)/mem.h $(INC)/xyz.h 
ximage.o:        ximage.c $(INC)/xyz.h
xscallop.o:      xscallop.c $(INC)/mach.h $(INC)/mem.h
xspectrogram.o:  xspectrogram.c $(INC)/mach.h $(INC)/mem.h 
xxyzc.o:         xxyzc.c 
xyzcleanup.o:    xyzcleanup.c $(INC)/xyz.h 
xzcolors.o:      xzcolors.c $(INC)/mach.h $(INC)/contouring.h 
xzlabels.o:      xzlabels.c $(INC)/mach.h $(INC)/contouring.h 
xzlevels.o:      xzlevels.c $(INC)/mach.h $(INC)/contouring.h 
xzlines.o:       xzlines.c $(INC)/mach.h $(INC)/contouring.h 
xzticks.o:       xzticks.c $(INC)/mach.h $(INC)/contouring.h 
zfft.o:          zfft.c 
