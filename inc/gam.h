/* ../../inc/gam.h */

#define	MFIDFM	5
#define	MFIDLC	5
#define	MFIDNM	10
#define	MFIDTP	5
#define	MOP1	20
#define	MOP2	20
#define	MOPE	100
#define	MOPN	10
#define	MOPNLI	50
#define	MOPT	10
#define	MP2LOC	5
#define	MPKNAM	13
#define	MPKTYP	3
#define	MYLIM	100
#define MDEFAULT 0
#define MGREY   1
#define MCOLOR  2


struct t_cmgam {
	int lppkpp;
	int nppkpp;
	int lppkut, lppkrl;
	float vppkrl;
	int lmkall, lsavelocs, lp2abs;
	int ip2loc;
	float tsp2;
	int nope, nop1, nop2, nopn, nopt;
	int lrplrq, lpcfil;
	float scamac, rotmac, xopnli[MOPNLI], yopnli[MOPNLI], pcdegi;
	int npcpsi;
	int lglori;
	float xori, yori, xcdp, ycdp, xcen, ycen, xcdpe, ycdpe, pcalen, 
	 pcamul, pcaang;
	int lpcavi, lpcafi;
	int icsize;
	int lbdrrq;
	int nopei, nhtick, nvtick;
	int lbdron, ldsppk;
	int ipktyp[MPKNAM];
	float pkwdth, pkhgth, tspk, tsfid;
	int lfidrq;	/* 1 or 0 if fileid is on or off respectively */
	int lfinorq;	/* 1 or 0 if filenumber is on or off respectively. maf 970204 */
	int nfidtp, ifidtp, nfidlc, ifidlc;
	float xfidlc, yfidlc;
	int nfidnm, nfidst, nfidtx, ifidfm, nfidfm, iur, iul, ilr, 
	 ill;
	int lwaitr, lwaite, lrtwxl;
	float ortwxl[2];
	int nylim;
	float ylims[MYLIM][2], rngmin, rngmax, fidbdr, xrect[4], yrect[4], 
	 extgam[11];
        int cmap;
	}	cmgam;
struct t_kmgam {
	char kp2loc[MP2LOC][9], kope[MOPE][3];
	byte kop1[MOP1], kop2[MOP2], kopn[MOPN], kopt[MOPT];
	char kopetx[81];
	byte kopqui, kopdel, kopmac, kopbe, kopee, kopcmt;
	char kpcfil[MCPFN+1], kpcmac[MCPFN+1], kpcfsu[5], kpcmsu[5], kopnli[MOPNLI][9], 
	 kpknam[MPKNAM][9], kpktyp[MPKTYP][9], kfidtp[MFIDTP][9], kfidlc[MFIDLC][9], 
	 kfidnm[MFIDNM][9], kfidst[MFIDNM][9], kfidtx[MFIDNM][41], kfidfm[MFIDFM][9], 
	 krtwxl[2][9], kylims[MYLIM][9], kgddef[9], kxtgam[9][9];
	}	kmgam;


#ifdef DOINITS
float *const Extgam = &cmgam.extgam[0] - 1;
int *const Ipktyp = &cmgam.ipktyp[0] - 1;
byte *const Kop1 = &kmgam.kop1[0] - 1;
byte *const Kop2 = &kmgam.kop2[0] - 1;
byte *const Kopn = &kmgam.kopn[0] - 1;
byte *const Kopt = &kmgam.kopt[0] - 1;
float *const Ortwxl = &cmgam.ortwxl[0] - 1;
float *const Xopnli = &cmgam.xopnli[0] - 1;
float *const Xrect = &cmgam.xrect[0] - 1;
float *const Yopnli = &cmgam.yopnli[0] - 1;
float *const Yrect = &cmgam.yrect[0] - 1;
#else
extern float *const Extgam;
extern int *const Ipktyp;
extern byte *const Kop1;
extern byte *const Kop2;
extern byte *const Kopn;
extern byte *const Kopt;
extern float *const Ortwxl;
extern float *const Xopnli;
extern float *const Xrect;
extern float *const Yopnli;
extern float *const Yrect;
#endif

