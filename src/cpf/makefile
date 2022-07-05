CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB  = ../../lib/libcpf.a


LIBOBJS = cerr.o   cfmt.o cresp.o csinit.o cspop.o \
          cspush.o cszero.o ctype.o gc.o getarg.o ictok.o \
          inicom.o inicsf.o initcomlists.o initpf.o iniusr.o lcchar.o \
          lccl.o   lcdfl.o lcia.o lcidi.o lcint.o lcirc.o \
          lcircp.o lckey.o lckeyExact.o lclist.o lclog.o lclog2.o \
          lcmore.o lcquot.o lcra.o \
          lcreal.o lcrest.o lcrrcp.o lcrtw.o lctok.o \
          lkchar.o lkcharExact.o lkentries.o lkia.o lkint.o \
          lkirc.o  lklist.o lklog.o lklog2.o lklogc.o \
          lklogi.o lklogr.o lklogra.o lkquot.o lkra.o lkreal.o lkrest.o \
          lkrrc.o  lkrrcp.o lkrtw.o lkt.o non_num_com.o pcmsg.o \
          savearg.o sctok.o setcomlist.o tokens.o xclog.o \
          xclogr.o xcrrcp.o xcrtw.o

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 

cerr.o:         cerr.c $(INC)/mach.h $(INC)/com.h 
cfmt.o:         cfmt.c $(INC)/mach.h $(INC)/com.h 
cresp.o:        cresp.c $(INC)/mach.h $(INC)/com.h $(INC)/exm.h 
csinit.o:       csinit.c $(INC)/mach.h $(INC)/csf.h 
cspop.o:        cspop.c $(INC)/mach.h $(INC)/csf.h $(INC)/exm.h 
cspush.o:       cspush.c $(INC)/mach.h $(INC)/csf.h 
cszero.o:       cszero.c $(INC)/mach.h $(INC)/csf.h 
ctype.o:        ctype.c $(INC)/mach.h $(INC)/com.h 
getarg.o:       getarg.c $(INC)/mach.h $(INC)/com.h
gc.o:           gc.c $(INC)/mach.h $(INC)/csf.h $(INC)/com.h 
ictok.o:        ictok.c $(INC)/mach.h $(INC)/com.h 
inicom.o:       inicom.c $(INC)/mach.h $(INC)/com.h 
inicsf.o:       inicsf.c $(INC)/mach.h $(INC)/csf.h 
initcomlists.o: initcomlists.c $(INC)/mach.h $(INC)/comlists.h $(INC)/exm.h 
initpf.o:       initpf.c $(INC)/mach.h $(INC)/tpf.h 
iniusr.o:       iniusr.c $(INC)/mach.h $(INC)/usr.h 
lcchar.o:       lcchar.c $(INC)/mach.h $(INC)/com.h 
lccl.o:         lccl.c $(INC)/mach.h $(INC)/com.h 
lcdfl.o:        lcdfl.c $(INC)/mach.h $(INC)/com.h 
lcia.o:         lcia.c $(INC)/mach.h $(INC)/com.h 
lcidi.o:        lcidi.c $(INC)/mach.h $(INC)/com.h
lcint.o:        lcint.c $(INC)/mach.h $(INC)/com.h 
lcirc.o:        lcirc.c $(INC)/mach.h $(INC)/com.h 
lcircp.o:       lcircp.c $(INC)/mach.h $(INC)/com.h 
lckey.o:        lckey.c $(INC)/mach.h $(INC)/com.h 
lckeyExact.o:   lckeyExact.c $(INC)/mach.h $(INC)/com.h
lclist.o:       lclist.c $(INC)/mach.h $(INC)/com.h 
lclog.o:        lclog.c $(INC)/mach.h $(INC)/com.h 
lclog2.o:       lclog2.c $(INC)/mach.h 
lcmore.o:       lcmore.c $(INC)/mach.h $(INC)/com.h 
lcquot.o:       lcquot.c $(INC)/mach.h $(INC)/com.h 
lcra.o:         lcra.c $(INC)/mach.h $(INC)/com.h 
lcreal.o:       lcreal.c $(INC)/mach.h $(INC)/com.h 
lcrest.o:       lcrest.c $(INC)/mach.h $(INC)/com.h 
lcrrcp.o:       lcrrcp.c $(INC)/mach.h $(INC)/com.h 
lcrtw.o:        lcrtw.c $(INC)/mach.h $(INC)/dfm.h 
lctok.o:        lctok.c $(INC)/mach.h $(INC)/com.h 
lkchar.o:       lkchar.c $(INC)/mach.h $(INC)/com.h 
lkcharExact.o:  lkcharExact.c $(INC)/mach.h $(INC)/com.h
lkentries.o:    lkentries.c $(INC)/mach.h 
lkia.o:         lkia.c $(INC)/mach.h $(INC)/com.h 
lkint.o:        lkint.c $(INC)/mach.h $(INC)/com.h 
lkirc.o:        lkirc.c $(INC)/mach.h $(INC)/com.h 
lklist.o:       lklist.c $(INC)/mach.h $(INC)/com.h 
lklog.o:        lklog.c $(INC)/mach.h $(INC)/com.h 
lklog2.o:       lklog2.c $(INC)/mach.h 
lklogc.o:       lklogc.c $(INC)/mach.h $(INC)/com.h 
lklogi.o:       lklogi.c $(INC)/mach.h $(INC)/com.h 
lklogr.o:       lklogr.c $(INC)/mach.h $(INC)/com.h 
lklogra.o:      lklogra.c $(INC)/mach.h $(INC)/com.h
lkquot.o:       lkquot.c $(INC)/mach.h $(INC)/com.h 
lkra.o:         lkra.c $(INC)/mach.h $(INC)/com.h 
lkreal.o:       lkreal.c $(INC)/mach.h $(INC)/com.h 
lkrest.o:       lkrest.c $(INC)/mach.h $(INC)/com.h 
lkrrc.o:        lkrrc.c $(INC)/mach.h $(INC)/com.h 
lkrrcp.o:       lkrrcp.c $(INC)/mach.h $(INC)/com.h 
lkrtw.o:        lkrtw.c $(INC)/mach.h 
lkt.o:          lkt.c $(INC)/mach.h $(INC)/dfm.h
non_num_com.o:  non_num_com.c
pcmsg.o:        pcmsg.c 
savearg.o:      savearg.c $(INC)/mach.h $(INC)/com.h
sctok.o:        sctok.c $(INC)/mach.h $(INC)/com.h 
setcomlist.o:   setcomlist.c $(INC)/mach.h $(INC)/comlists.h 
tokens.o:       tokens.c $(INC)/mach.h $(INC)/tpf.h 
xclog.o:        xclog.c $(INC)/mach.h 
xclogr.o:       xclogr.c $(INC)/mach.h 
xcrrcp.o:       xcrrcp.c $(INC)/mach.h 
xcrtw.o:        xcrtw.c $(INC)/mach.h 
