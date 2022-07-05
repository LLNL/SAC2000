CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB  = ../../lib/libextfuncs.a

LIBOBJS = getahdr.o  getehdr.o  getfhdr.o  getlhdr.o getnhdr.o makehdr.o \
          setahdr.o  setehdr.o  setfhdr.o  setlhdr.o setnhdr.o fgetahdr.o fgetehdr.o \
          fgetfhdr.o fgetlhdr.o fgetnhdr.o fsetahdr.o fsetehdr.o fsetfhdr.o \
          fsetlhdr.o fsetnhdr.o getfield.o fextlib.o

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 

getahdr.o:  getahdr.c $(INC)/extfunc.h
getehdr.o:  getehdr.c $(INC)/extfunc.h
getfhdr.o:  getfhdr.c $(INC)/extfunc.h
getlhdr.o:  getlhdr.c $(INC)/extfunc.h
getnhdr.o:  getnhdr.c $(INC)/extfunc.h
makehdr.o:  makehdr.c $(INC)/extfunc.h
setahdr.o:  setahdr.c $(INC)/extfunc.h
setehdr.o:  setehdr.c $(INC)/extfunc.h
setfhdr.o:  setfhdr.c $(INC)/extfunc.h
setlhdr.o:  setlhdr.c $(INC)/extfunc.h
setnhdr.o:  setnhdr.c $(INC)/extfunc.h
fgetahdr.o:  fgetahdr.c $(INC)/extfunc.h
fgetehdr.o:  fgetehdr.c $(INC)/extfunc.h
fgetfhdr.o:  fgetfhdr.c $(INC)/extfunc.h
fgetlhdr.o:  fgetlhdr.c $(INC)/extfunc.h
fgetnhdr.o:  fgetnhdr.c $(INC)/extfunc.h
fsetahdr.o:  fsetahdr.c $(INC)/extfunc.h
fsetehdr.o:  fsetehdr.c $(INC)/extfunc.h
fsetfhdr.o:  fsetfhdr.c $(INC)/extfunc.h
fsetlhdr.o:  fsetlhdr.c $(INC)/extfunc.h
fsetnhdr.o:  fsetnhdr.c $(INC)/extfunc.h
getfield.o:  getfield.c
fextlib.o:  fextlib.c
