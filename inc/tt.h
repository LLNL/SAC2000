/* ../../inc/tt.h */

#define	MTTLEN	5
#define	MTTRD	2
#define	MXTT	60
#define TTKILOMETER     1       /* indicates distance in kilometers */
#define	TTDEGREE	2	/* indicates distance in degrees */



struct t_kmtt {
	char kttnm[MXTT][6], kttmodl[MXTT][9], kttrd[MTTRD][9], krdph[6], 
	 kphases[MXTT][6], kmodel[9], **kphaseNames;
	}	kmtt;
struct t_cmtt {
	int	lttm, 		/* 1 means plot travel time curves in prs, 0 means don't */
		ltteven[MXTT], 	/* 1 means data is evenly spaced */
		lttplt[MXTT],
		lpreviousModel, /* 1 means a model was used last time tt command used. maf 960829 */
		npreviousFileNames ;	/* number of files used last time. maf 960829 */
	char	* previousFileNames ;	/* space delimited list of file names used last time. maf 960829 */
	float	xttfirst[MXTT], 
		xttdel[MXTT], 
		ttdist, 
		ttdep;
	int ittunit;	/* distance units (TTDEGREE or TTKILOMETER) */
	int lrdtt;
	int nttrd ,
		 nhlines ;	/* number of header lines to skip.  maf 970808 */
	float rdvel;
	int nrdph, 
		nphases, 	/* number of phases, eg P, S, Pn etc. */
		nttm, 
		ndxtty[MXTT], 	/* indicates the beginning of a tt data curve in cmmem.sacmem */
		ndxttx[MXTT], 	/* same for x axis */
		nttpt[MXTT];
	}	cmtt;


#ifdef DOINITS
int *const Ltteven = &cmtt.ltteven[0] - 1;
int *const Lttplt = &cmtt.lttplt[0] - 1;
int *const Ndxttx = &cmtt.ndxttx[0] - 1;
int *const Ndxtty = &cmtt.ndxtty[0] - 1;
int *const Nttpt = &cmtt.nttpt[0] - 1;
float *const Xttdel = &cmtt.xttdel[0] - 1;
float *const Xttfirst = &cmtt.xttfirst[0] - 1;
#else
extern int *const Ltteven;
extern int *const Lttplt;
extern int *const Ndxttx;
extern int *const Ndxtty;
extern int *const Nttpt;
extern float *const Xttdel;
extern float *const Xttfirst;
#endif

