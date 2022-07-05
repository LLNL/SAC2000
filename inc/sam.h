/* ../../inc/sam.h */

#include "mach.h"
#define	MFFT	16777216
#define	MPROTYP	6
#define	MRIR	17
#define	MTAPTP	5
#define	MTPIIR	4
#define	MWINTP	5
#define	NDATPTS	501
#define	NIMPPTS	1000
#define	NPTLEN	3



struct t_cmsam {
	int ifwd, ibwd;
	int lrlim, lwmean, lspfwd, lspbwd;
	char knmfir[MCPFN+1];
	int lrqrec;
	int itplp, npollp;
	float cflp;
	int npaslp;
	float tbwlp, atnlp;
	int itphp, npolhp;
	float cfhp;
	int npashp;
	float tbwhp, atnhp;
	int itpbp, npolbp;
	float cfbp1, cfbp2;
	int npasbp;
	float tbwbp, atnbp;
	int itpbr, npolbr;
	float cfbr1, cfbr2;
	int npasbr;
	float tbwbr, atnbr;
	int ncwien,
		lmu,		/* 1 if wienmu is set, 0 else.  maf 960723 */
		lepsilon;	/* 1 if epsilon is set, 0 else. maf 960723 */
	float	wienwb,
		wienwe,
		wienmu,		/* mu (adaption parameter) used in wiener() */
		epsilon,	/* maf 960723 */
		ortwwi[2];
	int nsptpl;
	int lramph, lrspe, lwspov;
	int nwspfl;
	int lwamph, lwrlim, lwspc1, lwspc2;
	int npsppp;
	int lpamph, lprlim, lpspc1, lpspc2;
	int ixspin, iyspin;
	float widtap;
	int lrtwwi, lunwfz;
	int nunwfz;
	float vunwit, vunwct, extsam[17];
	int ntaptp, itaptp;
	float cutkhr;
	int nwin;
	int lwinln;
	float winln;
	int iwintp, imast;
	float fddelta;
	}	cmsam;
struct t_kmsam {
	char ktpiir[MTPIIR][9], kprotyp[MPROTYP][4], krtwwi[2][9], ksptpl[8][9], 
	 krsps1[9], krsps2[9], kwsptp[9], kwspfl[MAXCHARS], kwsps1[9], kwsps2[9], 
	 kpsptp[9], kpspl1[17], kpspl2[17], ktaptp[MTAPTP][9], kxtsam[6][9], 
	 kwintp[MWINTP][9];
	}	kmsam;


#ifdef DOINITS
float *const Extsam = &cmsam.extsam[0] - 1;
float *const Ortwwi = &cmsam.ortwwi[0] - 1;
#else
extern float *const Extsam;
extern float *const Ortwwi;
#endif

