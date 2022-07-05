OS := $(shell uname)

all:
	cd src && make all
	
clean:
	cd src && make clean
