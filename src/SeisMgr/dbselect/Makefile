
CC=gcc

# InstantClient Directories.
ICSDKHOME=$(ORACLE_HOME)/sdk/
ICLIBHOME=$(ORACLE_HOME)/


PROC=$(ICSDKHOME)proc
SO_EXT=.dylib
I_SYM=-I

CCINCLUDES= $(I_SYM)$(ICSDKHOME)include

# Pre-compiler Flags.
PRECOMP_INCLUDE=$(I_SYM). $(SYS_INCLUDE)
PRECOMPPUBH=$(ICSDKHOME)include
SYS_INCLUDE=sys_include=\($(PRECOMPPUBH),/usr/include\)

# Compiler Flags.
OPTIMIZE=
LDPATHFLAG=-L
CCFLAGS=-fno-common -Wno-unused -fPIC -m64 -Wno-implicit-int

ICLIBPATH=$(LDPATHFLAG)$(ICLIBHOME)
PFLAGS=$(CCINCLUDES) $(SPFLAGS) $(LPFLAGS)
CFLAGS=$(GFLAG) $(OPTIMIZE) $(CDEBUG) $(CCFLAGS) $(PFLAGS) $(USRFLAGS) $(SACCFLAGS)





.SUFFIXES: .pc .c .o

.c.o:
	$(CC) $(CFLAGS)  -c  $< 

.pc.c:
	proc  $< 

OBJS =  dbselect.o dbBuildSQLstring.o dbQuerySL.o \
        dbPtsInside.o dbDefaults.o dbBuildSearchlink.o dbHelp.o \
        dbHelpQuery.o

library:  $(OBJS)
	ar -r ../../../lib/libSMOracleFuncs.a *.o	   

clean:
	/bin/rm -f *.o dbHelpQuery.c dbQuerySL.c *.lis


{OBJS}: dbBuildSearchlink.h dbBuildSQLstring.h dbDefaults.h dbErrorHandler.h \
        dbPtsInside.h dbselect.h


dbBuildSearchlink.o: dbBuildSearchlink.c
dbBuildSQLstring.o:  dbBuildSQLstring.c
dbDefaults.o:        dbDefaults.c
dbHelp.o:            dbHelp.c
dbHelpQuery.o:       dbHelpQuery.c
dbPtsInside.o:       dbPtsInside.c
dbQuerySL.o:         dbQuerySL.c
dbselect.o:          dbselect.c
