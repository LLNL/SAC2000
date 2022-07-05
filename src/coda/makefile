CC=gcc
CFLAGS=$(SACCFLAGS)
INC = ../../inc
SAC_INCLUDES = -I$(INC)

.c.o :
	$(CC) -c $(CFLAGS) $(SAC_INCLUDES) $<


LIB  = ../../lib/libcoda.a

LIBOBJS = xcoda.o pickwindows.o get_input.o calc_envelopes.o \
          unary_op.o C_firtrn.o C_overlp.o C_zshft.o C_fft.o \
	  binary_op.o C_wigint.o C_xapiir.o C_apply.o C_warp.o \
          C_design.o C_bilin2.o C_buroots.o C_cutoffs.o C_lptbp.o \
	  C_fstrncpy.o C_lptbr.o C_lp.o C_lpthp.o calc_codaGF.o \
          calc_coda_amplitudes.o fit_coda_amp.o calc_moment_magnitude.o \
	  plotspec.o calc_energy.o fit_coda_params.o send_output.o

library:  $(LIBOBJS)
	ar r ../../lib/libsac.a $(LIBOBJS)


clean:
	/bin/rm -f *.o 

get_input.o:             get_input.c  coda.h
calc_envelopes.o:        calc_envelopes.c coda.h
unary_op.o:              unary_op.c coda.h
C_firtrn.o:              C_firtrn.c coda.h
C_zshft.o:               C_zshft.c coda.h
C_overlp.o:              C_overlp.c coda.h
C_fft.o:                 C_fft.c coda.h
binary_op.o:             binary_op.c coda.h
C_wigint.o:              C_wigint.c coda.h
C_xapiir.o:              C_xapiir.c coda.h
C_apply.o:               C_apply.c coda.h
C_warp.o:                C_warp.c coda.h
C_design.o:              C_design.c coda.h
C_bilin2.o:              C_bilin2.c coda.h
C_buroots.o:             C_buroots.c coda.h
C_cutoffs.o:             C_cutoffs.c coda.h
C_lptbp.o:               C_lptbp.c coda.h
C_lp.o:                  C_lp.c coda.h
C_lpthp.o:               C_lpthp.c coda.h
C_lptbr.o:               C_lptbr.c coda.h
C_fstrncpy.o:            C_fstrncpy.c coda.h
C_complex.o:             C_complex.c coda.h
calc_codaGF.o:           calc_codaGF.c coda.h
calc_coda_amplitudes.o:  calc_coda_amplitudes.c coda.h
fit_coda_amp.o:          fit_coda_amp.c coda.h
calc_moment_magnitude.o: calc_moment_magnitude.c coda.h
pickwindows.o:           pickwindows.c coda.h
plotspec.o:	         plotspec.c coda.h
calc_energy.o:	         calc_energy.c coda.h
send_output.o:	         send_output.c coda.h
fit_coda_params.o:	 fit_coda_params.c coda.h
  
