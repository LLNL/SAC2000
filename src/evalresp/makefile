CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<

LIB  = ../../lib/libevalresp.a

LIBOBJS = regexp.o      regerror.o regsub.o      calc_fctns.o  resp_fctns.o \
          file_ops.o     evresp.o   alloc_fctns.o parse_fctns.o print_fctns.o \
          string_fctns.o error_fctns.o evr_spline.o

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)

clean:
	/bin/rm -f *.o 
	  
alloc_fctns.o:   alloc_fctns.c
calc_fctns.o:    calc_fctns.c
error_fctns.o:   error_fctns.c
evresp.o:        evresp.c
file_ops.o:      file_ops.c
parse_fctns.o:   parse_fctns.c
print_fctns.o:   print_fctns.c
regerror.o:      regerror.c
regexp.o:        regexp.c
regsub.o:        regsub.c
resp_fctns.o:    resp_fctns.c
string_fctns.o:  string_fctns.c
evr_spline.o:    evr_spline.c
