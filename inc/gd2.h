/* ../../inc/gd2.h */

#define	JFBMAX	5000
#define	JFBSIZ	(JFBMAX + 100)
#define	MOPCOL	(-4)
#define	MOPDON	(-2)
#define	MOPFIL	(-10)
#define	MOPFRT	(-11)
#define	MOPHWS	(-6)
#define	MOPHWT	(-5)
#define	MOPLIN	(-7)
#define	MOPMOV	(-3)
#define	MOPNUL	(-1)
#define	MOPSIZ	(-8)
#define	MOPTAN	(-12)
#define	MOPWIDTH	(-9)
#define MOPCIMAGE (-13)
#define	XW	32000.
#define	YW	24000.



struct t_kmgd2 {
	char kname2[9], kfnamb[MCPFN+1], kfdir[MCPFN+1], sizetype[9],
	     kfdirStore[MCPFN+1] ,  /* store value of kfdir for use later */
	     kfilename[ MCPFN+1 ] ; /* name of last SGF file written. */
	}	kmgd2;
struct t_cmgd2 {
	int itype2, nfbuf;
	short int mfbuf[JFBSIZ];
	int jfbpnt, jfdpnt, jfun, nfnamb, nfdir, nfdirStore;
	int lfnum;
	int nfnum;
	float sizevalue;
	int encodesize;
	int lover ; /* TRUE overwrites sgf files. */
	}	cmgd2;




#ifdef DOINITS
short *const Mfbuf = &cmgd2.mfbuf[0] - 1;
#else
extern short *const Mfbuf;
#endif

