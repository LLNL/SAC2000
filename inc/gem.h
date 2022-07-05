/* ../../inc/gem.h */

#define	MCMGEM	611
#define	MCPTXT	144
#define	MICOL	10
#define	MILINE	10
#define	MIPEN	10
#define	MISYM	20
#define	MKMGEM	323
#define	MPLAB	25
#define	MSISYM	8
#define	MSYM	16
#define	MSYMTB	40
#define	MTXSIZ	4
#define	MUNSYM	1
#define	MWIDTH	10
#define MAXPRNTRNAMELEN  128



struct t_cmgem {
	int lxlim;
	float ximn, ximx;
	int lylim;
	float yimn, yimx, ximnu, ximxu, yimnu, yimxu, xvspmn, xvspmx, 
	 yvspmn, yvspmx, vspreq, ximnz, ximxz, yimnz, yimxz, xpmn, xpmx, 
	 ypmn, ypmx, xpmnu, xpmxu, ypmnu, ypmxu, xmpip1, xmpip2, ympip1, 
	 ympip2;
	int lxfudg;
	float xfudg;
	int lyfudg;
	float yfudg;
	int lxrev, lyrev;
	int ixint, iyint;
	int lxfull, lyfull, lloglb, lnice, lxdiv;
	float xdiv;
	int lnxdiv;
	int nxdiv;
	int lydiv;
	float ydiv;
	int lnydiv;
	int nydiv;
	int lxpowr, lypowr, ltopax, lbotax, lrigax, llefax, ltoptc, 
	 lbottc, lrigtc, lleftc, lxgen;
	float xfirst, xdelta;
	int lygen;
	float yfirst, ydelta, axwtop, axwbot, axwrig, axwlef, tsaxis;
	int lfloor;
	float floor;
	int lflusd, lrqclp, ltqdp;
	int ntqdp;
	int lnull;
	float vnull;
	int lbdr, lxgrd;
	int ixgrd;
	int lygrd;
	int iygrd;
	int ltitl;
	int ntitl;
	float tstitl;
	int ititlp;
	int lxlab;
	int nxlab;
	float tsxlab;
	int ixlabp;
	int lylab;
	int nylab;
	float tsylab;
	int iylabp, nplab;
	int lplab[MPLAB], lplabl[MPLAB];
	float xplabl[MPLAB-(0)+1], yplabl[MPLAB-(0)+1], tsplab[MPLAB-(0)+1], 
	 taplab[MPLAB-(0)+1];
	int lline;
	int icline;
	int liline;
	int iiline[MILINE], niline, jiline, isklin;
	int lsym;
	int isym;
	int lisym;
	int iisym[MISYM], nisym, jisym;
	float symsz, symsp;
	int lpen;
	int ipen;
	int lipen;
	int iipen[MIPEN], nipen, jipen, iskpen;
	int lcol;
	int icol;
	int licol;
	int iicol[MICOL], nicol, jicol, iskcol, ibacol;
	int lwidth;
	int iwidth, iswidth, isymwidth;
	int liwidth;
	int iiwidth[MWIDTH], iskwidth, isskwidth, niwidth, jiwidth;
	float skdevfudge, chwid, chht, tscur, txsiz[MTXSIZ], otxsiz[MTXSIZ];
	int igtfnt;
	float tsdef, txrat, otxrat;
	int lframe;
	float dtxsiz[MTXSIZ], dtxrat;
	int lfqdp;
	int nfqdp;
	float extgem[43];
	int ilin, ilog, isolid, idot, ithin;
	float vert, horz;
	int itiny, ismall, imed, ilarge, itop, ibot, iright, ileft, 
	 itjlef, itjrig, itjbot, itjtop, itjcen;
	int lnwsym, lscsym;
	int jsyml1, jsym1b, jsym1e;
	int ldbsym;
	int jsyml2, jsym2b, jsym2e, isyml1[MSYM], isyml2[MSYM], nsymlc[MSISYM + 1];
	int ldrsym[MSYMTB];
	float xsymtb[MSYMTB], ysymtb[MSYMTB], fac[9];
	int lprint ;	/* TRUE if user wants to print a plot */
	int lSGFtemp ; /* TRUE if SGF turned on exclusively for PRINT option */
	}	cmgem;
struct t_kmgem {
	char ksides[4][9], ktitl[145], kxlab[145], kylab[145], kplab[MPLAB][145], 
	 ktxsiz[MTXSIZ][9], kgtqua[9], khjust[9], kvjust[9], ktxpos[5][9], 
	 ktxori[2][9], kxtgem[20][9], kptrName[ MAXPRNTRNAMELEN+1 ] ;
	byte kfac[9];
	}	kmgem;

/* structures used for saving/restoring graphics environment */
int lgems;

struct t_cmgem cmgemsav;

struct t_kmgem kmgemsav;



#ifdef DOINITS
float *const cmgema = (float*)&cmgem.lxlim;
char (*const kmgema)[9] = (char(*)[9])kmgem.ksides;
float *const xvsp = (float*)&cmgem.xvspmn;
float *const yvsp = (float*)&cmgem.yvspmn;

/* float *const Cmgema = &cmgema[0] - 1; */
float *const Cmgema = (float*)(&cmgem.lxlim - 1);

float *const Dtxsiz = &cmgem.dtxsiz[0] - 1;
float *const Extgem = &cmgem.extgem[0] - 1;
float *const Fac = &cmgem.fac[0] - 1;
int *const Iicol = &cmgem.iicol[0] - 1;
int *const Iiline = &cmgem.iiline[0] - 1;
int *const Iipen = &cmgem.iipen[0] - 1;
int *const Iisym = &cmgem.iisym[0] - 1;
int *const Iiwidth = &cmgem.iiwidth[0] - 1;
int *const Isyml1 = &cmgem.isyml1[0] - 1;
int *const Isyml2 = &cmgem.isyml2[0] - 1;
byte *const Kfac = &kmgem.kfac[0] - 1;
int *const Ldrsym = &cmgem.ldrsym[0] - 1;
int *const Lplab = &cmgem.lplab[0] - 1;
int *const Lplabl = &cmgem.lplabl[0] - 1;
int *const Nsymlc = &cmgem.nsymlc[0] - 1;
float *const Otxsiz = &cmgem.otxsiz[0] - 1;
float *const Txsiz = &cmgem.txsiz[0] - 1;
float *const Xsymtb = &cmgem.xsymtb[0] - 1;

/* float *const Xvsp = &xvsp[0] - 1; */
float *const Xvsp = (float*)(&cmgem.xvspmn - 1);
float *const Ysymtb = &cmgem.ysymtb[0] - 1;

/* float *const Yvsp = &yvsp[0] - 1; */
float *const Yvsp = (float*)(&cmgem.yvspmn - 1);
#else
extern float *const cmgema;
extern char (*const kmgema)[9];
extern float *const xvsp;
extern float *const yvsp;
extern float *const Cmgema;
extern float *const Dtxsiz;
extern float *const Extgem;
extern float *const Fac;
extern int *const Iicol;
extern int *const Iiline;
extern int *const Iipen;
extern int *const Iisym;
extern int *const Iiwidth;
extern int *const Isyml1;
extern int *const Isyml2;
extern byte *const Kfac;
extern int *const Ldrsym;
extern int *const Lplab;
extern int *const Lplabl;
extern int *const Nsymlc;
extern float *const Otxsiz;
extern float *const Txsiz;
extern float *const Xsymtb;
extern float *const Xvsp;
extern float *const Ysymtb;
extern float *const Yvsp;
#endif


