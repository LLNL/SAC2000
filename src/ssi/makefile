CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB  = ../../lib/libssi.a

LIBOBJS = CSStoSAC.o DBheaderToSac.o DBwfToSac.o OnOrOff.o SacHeaderToDB.o \
          SeisMgrCode.o SeisMgrToSac.o alignFiles.o deleteAllSacFiles.o \
	  inissi.o prefPicksToHeader.o rollback.o sacToSeisMgr.o uniqueStaAndChan.o \
	  xblend.o xcommit.o xcutim.o xreaddb.o xrollback.o xtablname.o

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 

CSStoSAC.o:	     CSStoSAC.c          $(INC)/complex.h $(INC)/proto.h $(INC)/mach.h  $(INC)/dfm.h $(INC)/hdr.h $(INC)/mem.h 
DBheaderToSac.o:     DBheaderToSac.c     $(INC)/SacHeader.h $(INC)/hdr.h $(INC)/dfm.h
DBwfToSac.o:	     DBwfToSac.c         $(INC)/cssStrucs.h $(INC)/complex.h $(INC)/proto.h  $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h $(INC)/mem.h  $(INC)/extfunc.h
OnOrOff.o:	     OnOrOff.c
SacHeaderToDB.o:     SacHeaderToDB.c     $(INC)/hdr.h $(INC)/dfm.h $(INC)/SacHeader.h
SeisMgrCode.o:	     SeisMgrCode.c       $(INC)/complex.h $(INC)/proto.h  $(INC)/mach.h $(INC)/exm.h
SeisMgrToSac.o:	     SeisMgrToSac.c      $(INC)/hdr.h $(INC)/dfm.h $(INC)/ssi.h  
alignFiles.o:        alignFiles.c        $(INC)/dfm.h $(INC)/ssi.h
deleteAllSacFiles.o: deleteAllSacFiles.c $(INC)/complex.h $(INC)/proto.h  $(INC)/mach.h $(INC)/dfm.h $(INC)/hdr.h $(INC)/mem.h
inissi.o:	     inissi.c            $(INC)/ssi.h 
prefPicksToHeader.o: prefPicksToHeader.c $(INC)/proto.h $(INC)/hdr.h  $(INC)/dfm.h 
rollback.o:	     rollback.c          $(INC)/dfm.h $(INC)/ssi.h  
sacToSeisMgr.o:      sacToSeisMgr.c      $(INC)/hdr.h $(INC)/dfm.h $(INC)/ssi.h  $(INC)/mem.h  
uniqueStaAndChan.o:  uniqueStaAndChan.c  $(INC)/extfunc.h $(INC)/cssStrucs.h  
xblend.o:	     xblend.c
xcommit.o:           xcommit.c           $(INC)/ssi.h $(INC)/msg.h
xcutim.o:	     xcutim.c            $(INC)/dfm.h $(INC)/ssi.h $(INC)/mem.h  $(INC)/hdr.h $(INC)/lhf.h 
xreaddb.o:	     xreaddb.c           $(INC)/cssStrucs.h $(INC)/dblPublicDefs.h   $(INC)/proto.h $(INC)/mach.h $(INC)/dfm.h
xrollback.o:	     xrollback.c         $(INC)/dfm.h $(INC)/ssi.h $(INC)/msg.h  
xtablname.o:	     xtablname.c       
