/* ../../inc/spe.h */

#define	MLNPE	100
#define	MPREWH	12
#define	MPSPTP	5
#define	MWINTP	5
#define MINPOW  2048	/* minimum power of 2 for spectral windows. maf 980602 */



struct t_cmspe {
	int lfile;
	int ndxdat, nlndat;
	float samfrq;
	int ndxcor, ndxpe, ndxspe, ndxaux;
	int lcor;
	int iwncor, numwin;
	float winlen;
	int nwinln;
	int lsnumw, lprewh;
	int nprerq, nprewh, junk;
	float cprewh[MPREWH + 1];
	int nlncor, nlnfft, nlnspe;
	int lspe, lpcspec;
	float pcspec;
	int iwnpds;
	float specpds;
	int nlgpds, nlgmem, nlgmlm;
	int lresl;
	float resl;
	int lcl;
	float clu, cll;
	int lrqcl, lspeid;
	int npsptp, ipsptp;
	float extspe[20];
	/* first power of 2 >= number of datapoints. maf 980527 */
	int firstPowerOf2 ;
	}	cmspe;
struct t_kmspe {
	char kscale[11], kwintp[MWINTP][9], kpsptp[MPSPTP][9], kpspl1[17], 
	 kpspl2[17], kpspl3[17], knmcor[MCPFN+1], knmspe[MCPFN+1], kermsg[131], 
	 kxtspe[10][9];
	}	kmspe;


#ifdef DOINITS
float *const Cprewh = &cmspe.cprewh[0] - 1;
float *const Extspe = &cmspe.extspe[0] - 1;
#else
extern float *const Cprewh;
extern float *const Extspe;
#endif

