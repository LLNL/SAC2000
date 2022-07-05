CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB    = ../../lib/libucf.a

LIBOBJS = basenm.o byteswap.o cnvatf.o cnvati.o cnvfta.o cnvita.o \
          copy.o   copydouble.o copyi.o copykc.o \
          ddttm.o  delims.o distaz.o evallogical.o extrma.o fill.o \
          getyw.o  getxw.o idttm.o idttmf.o incdat.o \
          inctim.o inctimf.o initok.o kadate.o kadttm.o katime.o \
          kidate.o kijdat.o lbsrch.o ldttm.o linrng.o ljust.o \
          nccomp.o next2.o ophelp.o poptok.o \
          sorti.o  srtndx.o timecheck.o \
          tokdel.o tosecs.o wapf.o wrcom.o wrhelp.o \
          wrindx.o 

library: $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 

basenm.o:       basenm.c $(INC)/mach.h 
byteswap.o:     byteswap.c
cnvatf.o:       cnvatf.c 
cnvati.o:       cnvati.c $(INC)/mach.h $(INC)/cnv.h
cnvfta.o:       cnvfta.c $(INC)/mach.h 
cnvita.o:       cnvita.c $(INC)/mach.h 
copy.o:         copy.c $(INC)/mach.h 
copydouble.o:   copydouble.c 
copyi.o:        copyi.c $(INC)/mach.h 
copykc.o:       copykc.c $(INC)/mach.h 
ddttm.o:        ddttm.c $(INC)/mach.h 
delims.o:       delims.c $(INC)/mach.h $(INC)/tpf.h 
distaz.o:       distaz.c $(INC)/mach.h 
evallogical.o:  evallogical.c $(INC)/mach.h 
extrma.o:       extrma.c $(INC)/mach.h
fill.o:         fill.c 
getyw.o:        getyw.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h $(INC)/mem.h 
getxw.o:        getxw.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h $(INC)/mem.h
idttm.o:        idttm.c 
idttmf.o:       idttmf.c
incdat.o:       incdat.c 
inctim.o:       inctim.c 
inctimf.o:      inctimf.c
initok.o:       initok.c $(INC)/mach.h $(INC)/tok.h 
kadate.o:       kadate.c $(INC)/mach.h 
kadttm.o:       kadttm.c $(INC)/mach.h 
katime.o:       katime.c $(INC)/mach.h 
kidate.o:       kidate.c $(INC)/mach.h 
kijdat.o:       kijdat.c $(INC)/mach.h 
lbsrch.o:       lbsrch.c 
ldttm.o:        ldttm.c $(INC)/mach.h $(INC)/hdr.h 
linrng.o:       linrng.c 
ljust.o:        ljust.c $(INC)/mach.h 
nccomp.o:       nccomp.c $(INC)/mach.h 
next2.o:        next2.c 
ophelp.o:       ophelp.c $(INC)/mach.h 
poptok.o:       poptok.c $(INC)/mach.h $(INC)/tok.h 
sorti.o:        sorti.c $(INC)/mach.h 
srtndx.o:       srtndx.c $(INC)/mach.h 
timecheck.o:    timecheck.c
tokdel.o:       tokdel.c $(INC)/mach.h $(INC)/tok.h 
tosecs.o:       tosecs.c 
wapf.o:         wapf.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h $(INC)/mem.h $(INC)/eam.h 
wrcom.o:        wrcom.c $(INC)/mach.h $(INC)/com.h 
wrhelp.o:       wrhelp.c $(INC)/mach.h 
wrindx.o:       wrindx.c $(INC)/mach.h 
