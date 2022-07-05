CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB    = ../../lib/libco.a

LIBOBJS =    ext_init.o  getfline.o getline.o izshft.o \
             tokenize.o zauxfile.o zbasename.o zclose.o zcloses.o \
             zclosec.o zdest.o zdestf.o zfiles.o \
             zgetc.o  zgetgd.o zgimsg.o zgpmsg.o \
             complex.o zgtmsg.o zgwindowsize.o zinfo.o zinquire.o \
             zmemad.o znfile.o znfiles.o zopen.o zopens.o zopenc.o zputc.o \
             zquit.o  zrabs.o zrun.o zrunname.o zruntext.o \
             zsleep.o fstrncpy.o subscpy.o strscpy.o math.o backspace.o \
             zsysop.o zwabs.o zwabs2.o zwprmt.o zload.o zexecute.o

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)


clean:
	/bin/rm -f *.o 

ext_init.o:      ext_init.c
backspace.o:     backspace.c
math.o:          math.c
complex.o:       complex.c
strscpy.o:       strscpy.c
subscpy.o:       subscpy.c
fstrncpy.o:      fstrncpy.c
getfline.o:      getfline.c 
getline.o:       getline.c 
izshft.o:        izshft.c 
tokenize.o:      tokenize.c
zauxfile.o:      zauxfile.c $(INC)/mach.h 
zbasename.o:     zbasename.c 
zclose.o:        zclose.c $(INC)/mach.h 
zcloses.o:       zcloses.c $(INC)/mach.h 
zclosec.o:       zclosec.c 
zdest.o:         zdest.c $(INC)/mach.h 
zdestf.o:        zdestf.c $(INC)/mach.h 
zfiles.o:        zfiles.c 
zgetc.o:         zgetc.c 
zgetgd.o:        zgetgd.c 
zgimsg.o:        zgimsg.c
zgpmsg.o:        zgpmsg.c stdu.h 
zgtmsg.o:        zgtmsg.c stdu.h 
zgwindowsize.o:  zgwindowsize.c 
zinfo.o:         zinfo.c 
zinquire.o:      zinquire.c 
zmemad.o:        zmemad.c stdu.h 
znfile.o:        znfile.c $(INC)/mach.h 
znfiles.o:       znfiles.c $(INC)/mach.h 
zopen.o:         zopen.c $(INC)/mach.h 
zopens.o:        zopens.c $(INC)/mach.h 
zopenc.o:        zopenc.c 
zputc.o:         zputc.c 
zquit.o:         zquit.c $(INC)/mach.h $(INC)/exm.h 
zrabs.o:         zrabs.c 
zrun.o:          zrun.c 
zrunname.o:      zrunname.c 
zruntext.o:      zruntext.c 
zsleep.o:        zsleep.c 
zsysop.o:        zsysop.c 
zwabs.o:         zwabs.c 
zwabs2.o:        zwabs2.c
zwprmt.o:        zwprmt.c stdu.h 
zload.o:         zload.c 
zexecute.o:      zexecute.c 
