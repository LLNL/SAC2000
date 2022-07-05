CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) -DNDEBUG $<

LIB  = ../../lib/libicm.a
 	
LIBOBJS = acc.o balance.o bbdisp.o bbvel.o benbog.o ckinst.o \
          clh.o clz.o cmh.o cmz.o csh.o csz.o \
          dcpft.o dewit.o dseis.o dss.o dwwssn.o ekalp6.o \
          ekasp2.o elmag.o eyeomg.o gbalp.o gbasp.o general.o \
          getins.o getran.o getroots.o gsref.o hfslpwb.o hqr.o hs3.o iniicm.o \
          lll.o llsn.o lrsmlp.o lrsmsp.o noress.o noresshf.o \
          oldbb.o oldkir.o polezero.o portable.o predfl.o prewit.o \
          ptbllp.o redkir.o reftek.o rs7.o rsk.o rsl.o \
          rsm.o rstn.o sandia.o snla3.o sro.o taper.o \
          transfer.o vel.o wa.o wabn.o wiech.o wwlpbn.o \
          wwsp.o wwspbn.o xicmc.o xprewit.o xtransfer.o ykalp.o ykasp.o \
          EvrespGateway.o EVRESPnames.o 

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

ndc:
	cd NDC ; make library ;

clean:
	/bin/rm -f *.o; cd oracle; make clean; cd ../nonOracle; make clean; cd ../NDC; make clean


acc.o:            acc.c $(INC)/mach.h 
balance.o:        balance.c
bbdisp.o:         bbdisp.c $(INC)/mach.h 
bbvel.o:          bbvel.c $(INC)/mach.h 
benbog.o:         benbog.c $(INC)/mach.h 
ckinst.o:         ckinst.c $(INC)/mach.h 
clh.o:            clh.c 
clz.o:            clz.c 
cmh.o:            cmh.c 
cmz.o:            cmz.c 
csh.o:            csh.c 
csz.o:            csz.c 
dcpft.o:          dcpft.c 
dewit.o:          dewit.c 
dseis.o:          dseis.c $(INC)/mach.h 
dss.o:            dss.c $(INC)/mach.h 
dwwssn.o:         dwwssn.c $(INC)/mach.h 
ekalp6.o:         ekalp6.c $(INC)/mach.h 
ekasp2.o:         ekasp2.c $(INC)/mach.h 
elmag.o:          elmag.c $(INC)/mach.h 
eyeomg.o:         eyeomg.c $(INC)/mach.h 
gbalp.o:          gbalp.c $(INC)/mach.h 
gbasp.o:          gbasp.c $(INC)/mach.h 
general.o:        general.c $(INC)/mach.h 
getins.o:         getins.c $(INC)/mach.h 
getran.o:         getran.c 
getroots.o:       getroots.c $(INC)/wtofd.h
gsref.o:          gsref.c $(INC)/mach.h 
hfslpwb.o:        hfslpwb.c $(INC)/mach.h 
hqr.o:            hqr.c $(INC)/wtofd.h
hs3.o:            hs3.c $(INC)/mach.h 
iniicm.o:         iniicm.c $(INC)/mach.h $(INC)/icm.h 
lll.o:            lll.c $(INC)/mach.h 
llsn.o:           llsn.c $(INC)/mach.h 
lrsmlp.o:         lrsmlp.c $(INC)/mach.h 
lrsmsp.o:         lrsmsp.c $(INC)/mach.h 
noress.o:         noress.c $(INC)/mach.h 
noresshf.o:       noresshf.c $(INC)/mach.h 
oldbb.o:          oldbb.c $(INC)/mach.h 
oldkir.o:         oldkir.c $(INC)/mach.h 
polezero.o:       polezero.c $(INC)/mach.h 
portable.o:       portable.c $(INC)/mach.h 
predfl.o:         predfl.c 
prewit.o:         prewit.c 
ptbllp.o:         ptbllp.c $(INC)/mach.h 
redkir.o:         redkir.c $(INC)/mach.h 
reftek.o:         reftek.c 
rs7.o:            rs7.c 
rsk.o:            rsk.c 
rsl.o:            rsl.c 
rsm.o:            rsm.c 
rstn.o:           rstn.c $(INC)/mach.h 
sandia.o:         sandia.c $(INC)/mach.h 
snla3.o:          snla3.c $(INC)/mach.h 
sro.o:            sro.c $(INC)/mach.h 
taper.o:          taper.c $(INC)/mach.h 
transfer.o:       transfer.c $(INC)/mach.h 
vel.o:            vel.c $(INC)/mach.h 
wa.o:             wa.c $(INC)/mach.h 
wabn.o:           wabn.c $(INC)/mach.h 
wiech.o:          wiech.c $(INC)/mach.h 
wwlpbn.o:         wwlpbn.c $(INC)/mach.h 
wwsp.o:           wwsp.c $(INC)/mach.h 
wwspbn.o:         wwspbn.c $(INC)/mach.h 
xicmc.o:          xicmc.c $(INC)/mach.h $(INC)/icm.h 
xprewit.o:        xprewit.c $(INC)/complex.h $(INC)/proto.h $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h $(INC)/mem.h $(INC)/icm.h $(INC)/spe.h
xtransfer.o:      xtransfer.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h $(INC)/mem.h $(INC)/icm.h 
ykalp.o:          ykalp.c $(INC)/mach.h 
ykasp.o:          ykasp.c $(INC)/mach.h 
EvrespGateway.o:  EvrespGateway.c
EVRESPnames.o:    EVRESPnames.c 

