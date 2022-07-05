/* ../../inc/contouring.h */

#define	MACTIONDRAW	2
#define	MACTIONLABEL	10
#define	MACTIONMOVE	1
#define	MACTIONTICK	3
#define	MLABELCANDIDATE	1
#define	MLABELREJECTED	2
#define	MLABELSELECTED	3
#define	MLINKCLOSED	(-2)
#define	MLINKOPEN	(-1)
#define	MSEGLABELCOMPLE	3
#define	MSEGLABELELIMIN	2
#define	MSEGLABELINCOMP	1
#define	MZLEVELS	40



struct t_cmcontouring {
	int maxsegments, numsegments, indexlevels, indexstarts, indexstops, 
	 maxpoints, numpoints, indexpoints, indexlinks, indexrlinks, indexaction, 
	 ixdatastart, ixdatastop, iydatastart, iydatastop;
	float zlevels[MZLEVELS];
	int nzlevels;
	float zlevellist[MZLEVELS];
	int nzlevellist;
	float zlevelminimum, zlevelmaximum, zlevelincrement;
	int nznumlevels, lines[MZLEVELS], nlines, linelist[MZLEVELS], 
	 nlinelist;
	float zregionlist[MZLEVELS];
	int nzregionlist, ncolorlist, iticklist[MZLEVELS], nticklist;
	float ticklength, tickspacing;
	int lticksdown;
	int indexseglabelst, indexseglabelnu, indexseglabelfi, maxlabels, 
	 numlabels, indexlabelpoint, indexlabeltype, indexlabelangle, 
	 indexlabeltext, nlabellist;
	float minlabelspacing, maxlabelspacing, deslabelspacing, desiredangle, 
	 widthlabels, heightlabels;
	}	cmcontouring;
struct t_kmcontouring {
	char klistname[MCPFN+1], kdatamode[9], klevelmode[9], klinemode[9], 
	 klinetype[9], kcolormode[9], kcolorlist[MZLEVELS][9], ktickmode[9], 
	 klabelmode[9], klabellist[MZLEVELS][17], klabel[MZLEVELS][17];
	byte kdecimal;
	}	kmcontouring;



#ifdef DOINITS
int *const Iticklist = &cmcontouring.iticklist[0] - 1;
int *const Linelist = &cmcontouring.linelist[0] - 1;
int *const Lines = &cmcontouring.lines[0] - 1;
float *const Zlevellist = &cmcontouring.zlevellist[0] - 1;
float *const Zlevels = &cmcontouring.zlevels[0] - 1;
float *const Zregionlist = &cmcontouring.zregionlist[0] - 1;
#else
extern int *const Iticklist;
extern int *const Linelist;
extern int *const Lines;
extern float *const Zlevellist;
extern float *const Zlevels;
extern float *const Zregionlist;
#endif

