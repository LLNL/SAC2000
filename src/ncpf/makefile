CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB  = ../../lib/libncpf.a

LIBOBJS = appendstring.o changestring.o closemacro.o deletestring.o executemacro.o \
          getembedded.o getmacroinfo.o getnumericarg.o getstringargs.o indexs.o inicpf.o \
          macrokeyword.o macroline.o macroordered.o macropreambl.o macrostatus.o \
          nstrlensp.o openmacro.o getepoch.o gettime.o \
          prependstring.o processembed.o processfunc.o processline.o processnum.o processstring.o \
          setmacrolev.o setmacrostat.o

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 

appendstring.o:   appendstring.c 
changestring.o:   changestring.c $(INC)/mach.h 
closemacro.o:     closemacro.c $(INC)/mach.h $(INC)/cpf.h 
deletestring.o:   deletestring.c $(INC)/mach.h 
executemacro.o:   executemacro.c $(INC)/mach.h 
getembedded.o:    getembedded.c $(INC)/mach.h 
getmacroinfo.o:   getmacroinfo.c $(INC)/mach.h $(INC)/cpf.h 
getnumericarg.o:  getnumericarg.c $(INC)/mach.h 
getstringargs.o:  getstringargs.c $(INC)/mach.h 
getepoch.o:       getepoch.c 
gettime.o:        gettime.c $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h $(INC)/mem.h
indexs.o:         indexs.c 
inicpf.o:         inicpf.c $(INC)/mach.h $(INC)/cpf.h 
macrokeyword.o:   macrokeyword.c $(INC)/mach.h $(INC)/cpf.h 
macroline.o:      macroline.c $(INC)/mach.h $(INC)/cpf.h 
macroordered.o:   macroordered.c $(INC)/mach.h $(INC)/cpf.h 
macropreambl.o:   macropreambl.c $(INC)/mach.h $(INC)/cpf.h 
macrostatus.o:    macrostatus.c $(INC)/mach.h $(INC)/cpf.h 
nstrlensp.o:      nstrlensp.c 
openmacro.o:      openmacro.c $(INC)/mach.h $(INC)/cpf.h 
prependstring.o:  prependstring.c $(INC)/mach.h 
processembed.o:   processembed.c $(INC)/mach.h $(INC)/cpf.h 
processfunc.o:    processfunc.c $(INC)/mach.h $(INC)/cpf.h 
processline.o:    processline.c $(INC)/mach.h $(INC)/cpf.h 
processnum.o:     processnum.c $(INC)/mach.h $(INC)/cpf.h 
processstring.o:  processstring.c $(INC)/mach.h $(INC)/cpf.h 
setmacrolev.o:    setmacrolev.c $(INC)/mach.h $(INC)/cpf.h 
setmacrostat.o:   setmacrostat.c $(INC)/mach.h $(INC)/cpf.h 
