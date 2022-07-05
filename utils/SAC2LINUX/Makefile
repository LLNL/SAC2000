CC = gcc
#CFLAGS = -O2 
CFLAGS = -g
INCLUDE = .
all : saclinux2sun sacsun2linux 

.c.o: 
	$(CC) -I$(INCLUDE) $(CFLAGS) -c $*.c

saclinux2sun : saclinux2sun.o sac.h
	$(CC) $(CFLAGS) -o saclinux2sun saclinux2sun.o

sacsun2linux : sacsun2linux.o sac.h
	$(CC) $(CFLAGS) -o sacsun2linux sacsun2linux.o

clean : 
	rm -f *.o saclinux2sun sacsun2linux
