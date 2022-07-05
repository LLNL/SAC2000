CC=gcc

export SACCFLAGS=-D_FORTIFY_SOURCE=0
OUT_LIB_DIR := $(shell mkdir -p ../lib)

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

# Libraries.
PROLDLIBS=$(LDCLIENTLIBS) $(THREADLIBS)
LDCLIENTLIBS=$(ICLIBPATH) $(LLIBCLNTSH) $(LDLIBS)
LLIBCLNTSH=$(LDLIBFLAG)$(LIBCLNTSHNAME)
LDLIBFLAG=-l
LIBCLNTSHNAME=clntsh
LDLIBS=$(EXSYSLIBS) $(MATHLIB) $(USRLIBS)
EXSYSLIBS=-ldl
MATHLIB=-lm
THREADLIBS=-lpthread

INSTALL_BINDIR = /usr/local/bin
INSTALL_AUXDIR = /usr/local/sac/misc

SYSTEM_LIBS = -L/usr/X11/lib -L$(ORACLE_HOME) -L../lib -lX11 -lm -ldl

ALWAYS_NEEDS     = main/sac.o ../lib/libsac.a ../lib/libSMnonOracleFuncs.a  
ORACLE_FUNCS     = ../lib/libSacOracleFuncs.a  ../lib/libSMOracleFuncs.a
NON_DB_STUBS     = ../lib/libSacOracleStubFuncs.a

DB_SAC: $(ALWAYS_NEEDS) $(ORACLE_FUNCS) $(LIB_DIRS) main/sac.o
	make -s UPDATE_LIBS
	cd SeisMgr; $(MAKE) DATABASE_LIB; 
	cd icm/oracle; $(MAKE) library
	gcc $(CFLAGS)  $(SACCFLAGS) -o ../bin/sac \
	$(ALWAYS_NEEDS) $(ORACLE_FUNCS) $(PROLDLIBS)  $(LDFLAGS) \
	$(SYSTEM_LIBS) -lSacOracleFuncs -lSMOracleFuncs


NON_DB_SAC: $(ALWAYS_NEEDS) $(LIB_DIRS) $(NON_DB_STUBS) main/sac.o
	make -s UPDATE_LIBS
	gcc $(SACCFLAGS) -o ../bin/sacndb $(ALWAYS_NEEDS) $(NON_DB_STUBS)   $(SYSTEM_LIBS) -lSMnonOracleFuncs -lSacOracleStubFuncs

all: 
	$(MAKE) DB_SAC
	$(MAKE) NON_DB_SAC
	
install: ../bin/sac ../bin/sacndb
	/bin/cp ../bin/sac ../bin/sacndb $(INSTALL_BINDIR)
	/bin/cp -r ../misc/* $(INSTALL_AUXDIR)
	/bin/cp -r ../datagen $(INSTALL_AUXDIR)
	
LIB_DIRS = amf bbs bom bot clf cnd co coda contouring cpf datafilelist dbh \
              dff dfm eam exm extfuncs fks gam gcm gd2 gd3.x11 gd4.null \
              gd5.gui gdm gem gpm gtm history icm msg ncpf nnm \
	      pl sam scm site smm spe ssi sss top ucf uom vars wild xyz icm/NDC

SACIO_DIRS = amf bbs bot co dff msg ucf vars

main/sac.o:
	cd main; $(MAKE) sac.o

SAC_IO_LIB:
	@ echo Checking SACIO directories for modified source files...
	for i in $(SACIO_DIRS); do ( if [ -d $$i ]; then cd $$i ; $(MAKE) iolib; fi; ) ; done;
	
../lib/libSMnonOracleFuncs.a:
	cd SeisMgr; $(MAKE) SEISMGR_LIB
	
../lib/libSMOracleFuncs.a:
	cd SeisMgr; $(MAKE) DATABASE_LIB; 

../lib/libSacOracleFuncs.a:
	cd icm/oracle; $(MAKE) library

../lib/libevresp.a:
	cd evalresp; $(MAKE) library
	
../lib/libSacOracleStubFuncs.a: SeisMgr/smNoDBstubs.o icm/nonOracle/dbaseCommand.o
	cd SeisMgr; $(MAKE) NON_DATABASE_STUB; cd ../icm/nonOracle; $(MAKE) library	
		      
../lib/libsac.a:	      
	for i in $(LIB_DIRS); do ( if [ -d $$i ]; then cd $$i ; $(MAKE) library; fi; ) ; done;


clean:
	@ echo Removing executables and libraries...
	@ /bin/rm -f ../lib/*.a ../bin/*sac*
	@ echo Removing all object files...
	@ /bin/rm -f *.o ; 
	@ cd SeisMgr; $(MAKE) -s clean; 
	@ cd icm/oracle; $(MAKE) -s clean; 
	@ cd evalresp; $(MAKE) -s clean; 
	@ (for i in $(LIB_DIRS); do ( if [ -d $$i ]; then cd $$i ; $(MAKE) -s clean; fi; ) ; done; )
	@ cd main; $(MAKE) -s clean; 
	
	
UPDATE_LIBS:
	echo Checking libraries for modified source files...	      
	cd SeisMgr; $(MAKE) SEISMGR_LIB
	cd evalresp; $(MAKE) library
	cd exm; touch xabout.c  #Update the date in the greeting.
	cd SeisMgr; $(MAKE) NON_DATABASE_STUB; cd ../icm/nonOracle; $(MAKE) library	
	for i in $(LIB_DIRS); do ( if [ -d $$i ]; then cd $$i ; $(MAKE) library; fi; ) ; done;
	
