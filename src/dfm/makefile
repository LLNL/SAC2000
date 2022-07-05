CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB  = ../../lib/libdfm.a


LIBOBJS = cleardfl.o cnvfmt.o      cnvfre.o     crsac.o \
          decont.o   defcut.o      defmem.o     detnum.o \
	  gennames.o getatw.o      getprefs.o   getwfdiscs.o \
          inidfm.o   inisnf.o      iztypeMessage.o lfilesok.o \
          makeuniq.o rdci.o        readcfl.o    readfl.o   sort.o \
          synch.o    vblist.o      vfeven.o     vflist.o \
          vfmaxn.o   vfmax.o       vfrng.o      vfspec.o   vftime.o \
          wrci.o     xch.o         xconv.o      xcopyhdr.o xcuter.o \
	  xdatagen.o xdeletechannel.o xdfmc.o   xHeaderWindow.o \
          xlh.o      xpickauthor.o xpickphase.o xpickprefs.o \
          xr.o       xrtab.o       xrcss.o      xrerr.o    xrh.o \
          xrsdd.o    xsort.o       xsynch.o     xw.o       xwh.o \
	  xwild.o    updatedfl.o   xrsuds.o     xwcss.o \
          readsuds.o xrgse.o       readgse.o    xwgse.o    xwtab.o
          

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 

cleardfl.o:       cleardfl.c $(INC)/mach.h $(INC)/dfm.h $(INC)/mem.h 
cnvfmt.o:         cnvfmt.c 
cnvfre.o:         cnvfre.c 
crsac.o:          crsac.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h 
decont.o:         decont.c $(INC)/mach.h 
defcut.o:         defcut.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h 
defmem.o:         defmem.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h 
detnum.o:         detnum.c 
getwfdiscs.o:     getwfdiscs.c
gennames.o:       gennames.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h 
getatw.o:         getatw.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h 
getprefs.o:       getprefs.c $(INC)/mach.h $(INC)/dfm.h
sort.o:           sort.c 
inidfm.o:         inidfm.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h 
inisnf.o:         inisnf.c $(INC)/mach.h $(INC)/snf.h 
iztypeMessage.o:  iztypeMessage.c $(INC)/msg.h $(INC)/extfunc.h
lfilesok.o:       lfilesok.c $(INC)/mach.h $(INC)/dfm.h \
                  $(INC)/hdr.h $(INC)/mem.h
makeuniq.o:       makeuniq.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h 
rdci.o:           rdci.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h 
readcfl.o:        readcfl.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h $(INC)/arrival.h
readfl.o:         readfl.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h $(INC)/sddhdr.h
synch.o:          synch.c 
updatedfl.o:      updatedfl.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/dload.h $(INC)/extfunc.h
vblist.o:         vblist.c $(INC)/mach.h $(INC)/bom.h 
vfeven.o:         vfeven.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h 
vflist.o:         vflist.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h 
vfmaxn.o:         vfmaxn.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h 
vfmax.o:          vfmax.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h 
vfrng.o:          vfrng.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h 
vfspec.o:         vfspec.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h 
vftime.o:         vftime.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h 
wrci.o:           wrci.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h 
xch.o:            xch.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h $(INC)/lhf.h 
xconv.o:          xconv.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h 
xcopyhdr.o:       xcopyhdr.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h $(INC)/lhf.h 
xcuter.o:         xcuter.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h 
xdatagen.o:       xdatagen.c $(INC)/mach.h $(INC)/dfm.h 
xdeletechannel.o: xdeletechannel.c $(INC)/mach.h $(INC)/dfm.h
xdfmc.o:          xdfmc.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h 
xHeaderWindow.o:  xHeaderWindow.c
xlh.o:            xlh.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h $(INC)/lhf.h 
xpickauthor.o:	  xpickauthor.c $(INC)/mach.h $(INC)/dfm.h
xpickphase.o:	  xpickphase.c $(INC)/mach.h $(INC)/dfm.h
xpickprefs.o:     xpickprefs.c
xr.o:             xr.c $(INC)/mach.h $(INC)/dfm.h 
xrtab.o:          xrtab.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h 
xrcss.o:          xrcss.c $(INC)/mach.h $(INC)/com.h $(INC)/dfm.h 
xrerr.o:          xrerr.c $(INC)/mach.h $(INC)/exm.h $(INC)/dfm.h \
                  $(INC)/hdr.h $(INC)/mem.h 
xrh.o:            xrh.c $(INC)/mach.h $(INC)/dfm.h 
xrsdd.o:          xrsdd.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h $(INC)/sddhdr.h
xsort.o:          xsort.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/lhf.h
xsynch.o:         xsynch.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h 
xw.o:             xw.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h $(INC)/sddhdr.h
xwh.o:            xwh.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h 
xwild.o:          xwild.c $(INC)/mach.h $(INC)/dfm.h $(INC)/wild.h 
xrsuds.o:         xrsuds.c $(INC)/mach.h $(INC)/dfm.h $(INC)/wild.h
xwcss.o:          xwcss.c $(INC)/mach.h $(INC)/dfm.h $(INC)/wild.h
readsuds.o:       readsuds.c $(INC)/mach.h $(INC)/dfm.h $(INC)/wild.h
xrgse.o:          xrgse.c $(INC)/mach.h $(INC)/dfm.h $(INC)/wild.h
readgse.o:        readgse.c $(INC)/mach.h $(INC)/dfm.h $(INC)/wild.h
xwgse.o:          xwgse.c $(INC)/mach.h $(INC)/dfm.h $(INC)/msg.h
xwtab.o:          xwtab.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                  $(INC)/mem.h $(INC)/gem.h $(INC)/gdm.h $(INC)/com.h
