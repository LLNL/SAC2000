CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB  = ../../lib/libdbh.a

LIBOBJS = afr.o alias.o apply.o append.o autcor.o beroots.o bilin2.o \
          buroots.o c1roots.o c2roots.o chebparm.o crit.o \
          crscor.o cutoffs.o decim.o design.o dfr.o dircor.o edecim.o fft.o \
          firtrn.o gauss.o getResponse.o iirfilter.o inspect.o levin.o levinD.o \
          linpack.o lp.o lptbp.o lptbr.o lpthp.o mem.o mlm.o \
          phaseshift.o overlp.o pds.o pef.o random.o xapiir.o rms.o shift.o \
          spectr.o step.o unit.o warp.o wiener.o window.o winmov.o \
          zero.o zshft.o pltplr.o cpolar.o 

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 

afr.o:          afr.c
alias.o:        alias.c 
append.o:       append.c 
apply.o:        apply.c 
autcor.o:       autcor.c 
beroots.o:      beroots.c
bilin2.o:       bilin2.c
buroots.o:      buroots.c
c1roots.o:      c1roots.c
c2roots.o:      c2roots.c
chebparm.o:     chebparm.c
crit.o:         crit.c 
crscor.o:       crscor.c 
cutoffs.o:      cutoffs.c 
decim.o:        decim.c 
design.o:       design.c
dfr.o:          dfr.c
dircor.o:       dircor.c 
edecim.o:       edecim.c 
fft.o:          fft.c 
firtrn.o:       firtrn.c 
getResponse.o:  getResponse.c
gauss.o:        gauss.c 
iirfilter.o:    iirfilter.c
inspect.o:      inspect.c
levin.o:        levin.c 
levinD.o:       levinD.c
linpack.o:      linpack.c
lp.o:           lp.c 
lptbp.o:        lptbp.c 
lptbr.o:        lptbr.c 
lpthp.o:        lpthp.c 
mem.o:          mem.c 
mlm.o:          mlm.c 
overlp.o:       overlp.c 
pds.o:          pds.c 
pef.o:          pef.c 
phaseshift.o:   phaseshift.c
random.o:       random.c 
xapiir.o:       xapiir.c $(INC)/mach.h 
rms.o:          rms.c 
shift.o:        shift.c  
spectr.o:       spectr.c 
step.o:         step.c 
unit.o:         unit.c 
warp.o:         warp.c 
wiener.o:       wiener.c 
window.o:       window.c 
winmov.o:       winmov.c 
zero.o:         zero.c 
zshft.o:        zshft.c
pltplr.o:       pltplr.c $(INC)/mach.h $(INC)/gem.h $(INC)/gam.h
cpolar.o:       cpolar.c
