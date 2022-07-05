/* ../../inc/dfm.h */

#include "mach.h"
#include "dblPublicDefs.h"
#include "cssStrucs.h"
#include "sacIO.h"

#define MAXSORTFIELDS	5
#define	MCOMP	2
#define	MCUTER	3
#define	MDGSUB	3
#define	MHDRCO	20
#define	MPICK	18
#define	MRWFMT	5
#define COMMIT	0
#define RECALL	1
#define ROLLBACK 2
#define BINARY	0
#define ASCII	1

typedef enum{Ascending, Descending} SortOrder;

/* TWODAYS in seconds is 48 hours times 3600 seconds per hour. maf 970908 */
#define TWODAYS ( 48 * 3600 )

enum readFlag { RDB , HIGH , LOW } ;

struct t_cmdfr {
	int nstart[MDFL], nfillb[MDFL], nstop[MDFL], nfille[MDFL], 
	 ntotal[MDFL], nxsdd[MDFL];
}	cmdfr;

struct t_cmdfm {
	int ndfl, idflc;
	int ndsndx[MDFL], ndsflcnt ;
	int ncdir;
	int lechof;
	int ndxhdr[MDFL], ndxdta[MDFL][MCOMP], nlndta[MDFL], ncomp[MDFL];
	int nwfl;
	int lovrrq, lcut;
	float ocut[2];
	int icuter;
	int nrwfmt, iwfmt, icfmt[2], /* nsamp, */ ipckn, ipckz, 
	 ipckg, ipckb, ipcke, ipcko, ipcka, ipckt0, ipckt1, ipckt2, ipckt3, 
	 ipckt4, ipckt5, ipckt6, ipckt7, ipckt8, ipckt9, ipckf, ipckhd[MPICK];
	int lround;
	int icatco[MHDRCO], itemco[MHDRCO];
	int ldfree;
	int idgsub, ndgfil;
	int lshift ,	/* TRUE if rcss should perform a time shift */
	     lscale ;	/* TRUE if rcss should calibrate data */
	MagType  nMagSpec ; /* specify which magnitude to read in rcss. maf 970206 */
	int  iauthors ; /* Number of authors names to search for in .arrival css file. maf 970409 */
	int nSortOrder ; /* added for SORT command. maf 980812 */
	int larray ;	/* true if ARRAY option for READCSS is on */
	int lrascii ;	/* true if RCSS reads ascii flat files, else reads CSSB */
        char ltype[5];  /* optional: specify an output type for writecss ganz 20131009 */
	int lwascii ;   /* true if WCSS writes ascii flat files, else writes CSSB */
	int ldata ;	/* true if waveform committed in COMMIT command. maf 980915 */
        int lpref ;	/* true if CSS picks should be read through the preferences */

	/* for COMMIT / ROLLBACK options. 0: commit, 1: recall trace, 2: rollback */
	int icomORroll ;

	/* ASCEND / DESCEND for SORT */
	SortOrder idirection[ MAXSORTFIELDS ] ;

	/* for COMMIT option on deletechannel. */
	int lcommit ;

	/* added for rdsegy, 000327 */
	int iztype ;

        /* added for writetable */
        int liftype ;
        int lheader ;

	/* added to fix data transfer from SAC buffers to CSS buffers. */
	int ltrust ;
	enum readFlag nreadflag ;
	int lread ;
	int  nfilesFirst ;

        int lcm6 ;  /* GSE CM6 compressed format instead of integer format */
}	cmdfm;


struct t_kmdfm {
	char kdfl[MAXCHARS], kdflrq[MAXCHARS], krddir[MCPFN+1], kwrdir[MCPFN+1],
	     krdcssdir[MCPFN+1] ;
	int lstation;		/* gain became station.  maf 961216 */
	int lchannel;
	char kstation[7];	/* " */
	char kchannel[9];
	int lbandw;
	char kbandw[9];
	int lorient;
	char korient[9], kdirnm[MCPFN+1], kwfl[MAXCHARS], ksuffx[MDFL][3], 
	 kcut[2][9], kcuter[MCUTER][9], kecbdf[9], kecmem[9], krwfmt[MRWFMT][9], 
	 kcfile[2][MCPFN+1], kcfmt[2][9], kpick[MPICK][9], kdcont[MCMSG+1], kdform[MCMSG+1], 
	 kdgsub[MDGSUB][9], kdgfil[MCMSG+1];

	/* The following were added to facilitate rcss reading 
	   picks from .arrival file. maf 970409 */
	char kprefsFileName[ MCPFN ] ;
	char **kauthors ;
	char ktPh[10][9] , ktAu[10][16] ;

	/* Added for COMMIT and ROLLBACK options. */
	/* char kcomORroll[3][9] ; */

	/* BINARY or ASCII options */
	char kbinORasc[2][9] ;

	/* Added for the sort command.  maf 980812 */
	char ksort[ MAXSORTFIELDS ][ 9 ] ;
}	kmdfm;


#ifdef DOINITS
int *const Icatco = &cmdfm.icatco[0] - 1;
int *const Icfmt = &cmdfm.icfmt[0] - 1;
int *const Ipckhd = &cmdfm.ipckhd[0] - 1;
int *const Itemco = &cmdfm.itemco[0] - 1;
int *const Ncomp = &cmdfm.ncomp[0] - 1;
int *const Ndsndx = &cmdfm.ndsndx[0] - 1;
int *const Ndxhdr = &cmdfm.ndxhdr[0] - 1;
int *const Nfillb = &cmdfr.nfillb[0] - 1;
int *const Nfille = &cmdfr.nfille[0] - 1;
int *const Nlndta = &cmdfm.nlndta[0] - 1;
int *const Nstart = &cmdfr.nstart[0] - 1;
int *const Nstop = &cmdfr.nstop[0] - 1;
int *const Ntotal = &cmdfr.ntotal[0] - 1;
int *const Nxsdd = &cmdfr.nxsdd[0] - 1;
float *const Ocut = &cmdfm.ocut[0] - 1;
int const wfHeader = -1 ;
int const allHeader = 0 ;
int const eventHeader = 1 ;
float const MaxMem = 0.3 ;
#else
extern int *const Icatco;
extern int *const Icfmt;
extern int *const Ipckhd;
extern int *const Itemco;
extern int *const Ncomp;
extern int *const Ndsndx;
extern int *const Ndxhdr;
extern int *const Nfillb;
extern int *const Nfille;
extern int *const Nlndta;
extern int *const Nstart;
extern int *const Nstop;
extern int *const Ntotal;
extern int *const Nxsdd;
extern float *const Ocut;
extern int const wfHeader ;
extern int const allHeader ;
extern int const eventHeader ;
extern float MaxMem ;
#endif

