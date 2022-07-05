FFLAGS=$(SACFFLAGS) $(TEMPFFLAGS)
CFLAGS=$(SACCFLAGS) $(TEMPCFLAGS)
GRAPHICSLIBS = -lsacf -Bdynamic -lX11 -lsocket
all: sgflist sgfmerge sgfplot sgftops sgftoai
sgftops: sgftops.c
	cc $(CFLAGS) -o sgftops sgftops.c 
sgftoai: sgftoai.c
	cc $(CFLAGS) -o sgftoai sgftoai.c -lm 
sgflist:  sgflist.f mach
	f77 $(FFLAGS) -o sgflist sgflist.f -lsacf 
sgfmerge:  sgfmerge.f mach 
	f77 $(FFLAGS) -o sgfmerge sgfmerge.f -lsacf 
sgfplot:  sgfplot.f mach 
	f77 $(FFLAGS) -o sgfplot sgfplot.f $(GRAPHICSLIBS) 
clean: 
	rm -f sgftops sgftoai sgflist sgfmerge sgfplot
