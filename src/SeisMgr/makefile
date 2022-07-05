CC=gcc
CFLAGS=$(SACCFLAGS)

.c.o:
	$(CC) $(CFLAGS) -c  $< 


DIRS = cssb cssListOps gc gse sacIO smMemory suds time

SEISMGR_LIB: smDataIO.o smWorkSets.o stringfun.o
	for i in $(DIRS); do ( if [ -d $$i ]; then cd $$i ; make library; fi; ) ; done; \
	ar -r ../../lib/libSMnonOracleFuncs.a smDataIO.o smWorkSets.o stringfun.o


DATABASE_LIB: smDBdata.o
	cd dbselect; make library; cd ..; ar -r ../../lib/libSMOracleFuncs.a smDBdata.o

NON_DATABASE_STUB: smNoDBstubs.o
	ar -r ../../lib/libSacOracleStubFuncs.a smNoDBstubs.o

clean:
	@ /bin/rm -f *.o;
	@ cd dbselect; make clean;
	for i in $(DIRS); do ( if [ -d $$i ]; then cd $$i ; make clean; fi; ) ; done;




smDataIO.o:    smDataIO.c smDataIO.h
smWorkSets.o:  smWorkSets.c
stringfun.o:   stringfun.c stringfun.h
smDBdata.o:    smDBdata.c
smNoDBstubs.o: smNoDBstubs.c
