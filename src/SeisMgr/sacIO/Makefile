CC=gcc
CFLAGS=$(SACCFLAGS)
.c.o:
	$(CC) $(CFLAGS) -c  $< 

library:  dbConversions.o sacInput.o sacOutput.o 
	ar -r ../../../lib/libsac.a *.o	   

clean:
	/bin/rm -f *.o
	
dbConversions.o:  dbConversions.c  dbConversions.h   SacHeader.h
sacInput.o:       sacInput.c  SacHeader.h
sacOutput.o:      sacOutput.c  SacHeader.h
