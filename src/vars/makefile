CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB    = ../../lib/libvars.a

INC1 = $(INC)/vars.h $(INC)/nvars.h

LIBOBJS = allocatevnode.o convlistname.o createvlist.o decodevdesc.o deletev.o \
          deletevlist.o encodevdesc.o encodeventry.o encodevnil.o existsv.o \
          existsvlist.o findvnil.o getventry.o getvlist.o getvlist2.o \
          getvvstring.o increasenlist.o initializevar.o initvlist.o inivars.o nextvlist.o \
          putventry.o putvvstring.o readvfile.o releasevnode.o setvreadonly.o \
          splitvname.o writevfile.o putvFILEptr.o getvFILEptr.o

library: $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)


clean:
	/bin/rm -f *.o 

allocatevnode.o: allocatevnode.c $(INC1) $(INC)/mem.h 
convlistname.o:  convlistname.c  $(INC1) $(INC)/mach.h 
createvlist.o:   createvlist.c   $(INC1) $(INC)/mach.h $(INC)/mem.h 
decodevdesc.o:   decodevdesc.c 
deletev.o:       deletev.c       $(INC1) $(INC)/mem.h 
deletevlist.o:   deletevlist.c   $(INC1) $(INC)/mach.h $(INC)/mem.h 
encodevdesc.o:   encodevdesc.c 
encodeventry.o:  encodeventry.c  $(INC1) $(INC)/mem.h 
encodevnil.o:    encodevnil.c    $(INC1) $(INC)/mem.h 
existsv.o:       existsv.c       $(INC1) $(INC)/mem.h 
existsvlist.o:   existsvlist.c   $(INC1) $(INC)/mem.h 
findvnil.o:      findvnil.c      $(INC1) $(INC)/mem.h 
getventry.o:     getventry.c     $(INC1) $(INC)/mem.h 
getvFILEptr.o:   getvFILEptr.c   $(INC)/vars.h 
getvlist.o:      getvlist.c      $(INC1) 
getvlist2.o:     getvlist2.c     $(INC1) 
getvvstring.o:   getvvstring.c   $(INC1) $(INC)/mem.h 
increasenlist.o: increasenlist.c $(INC1) $(INC)/mem.h
initializevar.o: initializevar.c $(INC1) $(INC)/mem.h 
initvlist.o:     initvlist.c     $(INC1) $(INC)/mem.h 
inivars.o:       inivars.c       $(INC1) $(INC)/mem.h 
nextvlist.o:     nextvlist.c     $(INC1) $(INC)/mem.h 
putvFILEptr.o:   putvFILEptr.c   $(INC)/vars.h
putventry.o:     putventry.c     $(INC1) $(INC)/mem.h 
putvvstring.o:   putvvstring.c   $(INC1) $(INC)/mem.h 
readvfile.o:     readvfile.c     $(INC1) $(INC)/mem.h 
releasevnode.o:  releasevnode.c  $(INC1) $(INC)/mem.h 
setvreadonly.o:  setvreadonly.c  $(INC1) $(INC)/mem.h 
splitvname.o:    splitvname.c    $(INC1) $(INC)/mach.h  
writevfile.o:    writevfile.c    $(INC1) $(INC)/mem.h 
