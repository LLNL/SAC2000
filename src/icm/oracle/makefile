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
#SPFLAGS=-DMAC_OSX -D_GNU_SOURCE -DSLTS_ENABLE -DSLMXMX_ENABLE -D_REENTRANT -DNS_THREADS -DSS_64BIT_SERVER -DORAX86_64 -DBIT64 -DMACHINE64 -DSLS8NATIVE -DSLU8NATIVE -m64 -D_BCERT_API_ -DRSA_PLATFORM=RSA_MAC_X86_64_DARWIN
CCFLAGS=-fno-common -Wno-unused -fPIC -m64 -Wno-implicit-int

ICLIBPATH=$(LDPATHFLAG)$(ICLIBHOME)
PFLAGS=$(CCINCLUDES) $(SPFLAGS) $(LPFLAGS)
CFLAGS=$(GFLAG) $(OPTIMIZE) $(CDEBUG) $(CCFLAGS) $(PFLAGS) $(USRFLAGS) $(SACCFLAGS)





.SUFFIXES: .pc .c .o


library:  dbFuncs.o dbaseCommand.o
	ar -r ../../../lib/libSacOracleFuncs.a *.o
clean:
	/bin/rm -f *.o dbFuncs.c dbaseCommand.o



dbFuncs.c:  dbFuncs.pc
	proc dbFuncs.pc
	
dbFuncs.o:  dbFuncs.c
	$(CC) -c $(CFLAGS)  dbFuncs.c

dbaseCommand.o: dbaseCommand.c
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) dbaseCommand.c 
