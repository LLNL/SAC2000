CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c  $(CFLAGS) -I../../inc $<

LIB    = ../../lib/libdff.a

LIBOBJS = formhv.o formmarker.o getfhv.o getfil.o gethv.o \
          getihv.o getkhv.o getlhv.o getnfiles.o getnhv.o \
          hdrfld.o inihdr.o inilhf.o lgahdr.o markhdr.o \
          newhdr.o newstn.o putfil.o rddta.o rdhdr.o \
          rdsac.o rdsdta.o rdsegy.o rdshdr.o rdxdrdta.o rsac1.o rsac2.o \
          setfhv.o map_hdr_in.o map_hdr_out.o map_chdr_in.o \
          map_chdr_out.o setihv.o setkhv.o setlhv.o setnfiles.o setnhv.o setrng.o \
          updhdr.o wrsac.o wrxdr.o xdrhdr.o wsac0.o \
          wsac1.o wsac2.o wrsdd.o wrsegy.o

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)



clean:
	/bin/rm -f *.o 

formhv.o:       formhv.c $(INC)/mach.h $(INC)/hdr.h $(INC)/lhf.h 
formmarker.o:   formmarker.c $(INC)/mach.h $(INC)/hdr.h 
getfhv.o:       getfhv.c $(INC)/mach.h $(INC)/hdr.h $(INC)/lhf.h 
getfil.o:       getfil.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                $(INC)/mem.h 
gethv.o:        gethv.c $(INC)/mach.h $(INC)/dfm.h 
getihv.o:       getihv.c $(INC)/mach.h $(INC)/hdr.h $(INC)/lhf.h 
getkhv.o:       getkhv.c $(INC)/mach.h $(INC)/hdr.h $(INC)/lhf.h 
getlhv.o:       getlhv.c $(INC)/mach.h $(INC)/hdr.h $(INC)/lhf.h 
getnfiles.o:    getnfiles.c $(INC)/mach.h $(INC)/dfm.h 
getnhv.o:       getnhv.c $(INC)/mach.h $(INC)/hdr.h $(INC)/lhf.h 
hdrfld.o:       hdrfld.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                $(INC)/lhf.h 
inihdr.o:       inihdr.c $(INC)/mach.h $(INC)/hdr.h 
inilhf.o:       inilhf.c $(INC)/mach.h $(INC)/hdr.h $(INC)/lhf.h 
lgahdr.o:       lgahdr.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                $(INC)/mem.h 
markhdr.o:      markhdr.c $(INC)/mach.h $(INC)/hdr.h 
map_hdr_in.o:   map_hdr_in.c $(INC)/mach.h $(INC)/hdr.h
map_hdr_out.o:  map_hdr_out.c $(INC)/mach.h $(INC)/hdr.h
map_chdr_in.o:  map_chdr_in.c $(INC)/mach.h $(INC)/hdr.h
map_chdr_out.o: map_chdr_out.c $(INC)/mach.h $(INC)/hdr.h
newhdr.o:       newhdr.c $(INC)/mach.h $(INC)/hdr.h 
newstn.o:       newstn.c $(INC)/mach.h $(INC)/snf.h 
putfil.o:       putfil.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                $(INC)/mem.h 
rddta.o:        rddta.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                $(INC)/mem.h 
rdhdr.o:        rdhdr.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                $(INC)/mem.h 
rdsac.o:        rdsac.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                $(INC)/mem.h 
rdsdta.o:       rdsdta.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                $(INC)/mem.h  $(INC)/sddhdr.h
rdsegy.o:       rdsegy.c $(INC)/dfm.h $(INC)/hdr.h $(INC)/mem.h \
                $(INC)/segy.h $(INC)/segy2sac.h
rdshdr.o:       rdshdr.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                $(INC)/mem.h  $(INC)/sddhdr.h
rdxdrdta.o:     rdxdrdta.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                $(INC)/mem.h  $(INC)/sddhdr.h
rsac1.o:        rsac1.c $(INC)/mach.h $(INC)/hdr.h 
rsac2.o:        rsac2.c $(INC)/mach.h $(INC)/hdr.h 
setfhv.o:       setfhv.c $(INC)/mach.h $(INC)/hdr.h $(INC)/lhf.h 
setihv.o:       setihv.c $(INC)/mach.h $(INC)/hdr.h $(INC)/lhf.h 
setkhv.o:       setkhv.c $(INC)/mach.h $(INC)/hdr.h $(INC)/lhf.h 
setlhv.o:       setlhv.c $(INC)/mach.h $(INC)/hdr.h $(INC)/lhf.h 
setnfiles.o:    setnfiles.c $(INC)/mach.h $(INC)/dfm.h 
setnhv.o:       setnhv.c $(INC)/mach.h $(INC)/hdr.h $(INC)/lhf.h  
setrng.o:       setrng.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                $(INC)/mem.h $(INC)/gam.h 
updhdr.o:       updhdr.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                $(INC)/mem.h 
wrsac.o:        wrsac.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                $(INC)/mem.h 
wrxdr.o:        wrxdr.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                $(INC)/mem.h 
xdrhdr.o:       xdrhdr.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                $(INC)/mem.h 
wsac0.o:        wsac0.c $(INC)/mach.h $(INC)/hdr.h 
wsac1.o:        wsac1.c $(INC)/mach.h $(INC)/hdr.h 
wsac2.o:        wsac2.c $(INC)/mach.h $(INC)/hdr.h 
wrsdd.o:        wrsdd.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h \
                $(INC)/mem.h $(INC)/sddhdr.h
wrsegy.o:       wrsegy.c $(INC)/hdr.h $(INC)/segy.h
