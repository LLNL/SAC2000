/* ../../inc/sss.h */

#define	MDWUN	5
#define	MSFL	MDFL
#define	MVM	2
#define	MVMTP	5
#define	RKMPERDG	111.11



struct t_cmsss {
	int	lincl[MSFL];
	float	wt[MSFL];	/* ? weight */
	int	lpol[MSFL];	/* logical for instrument polarity */
	float	dst[MSFL], 	/* distance values of data points. accessed by Dst */
		beginTime[MSFL],/* these 2 store begin and ennd from hdr.h in array so */
		endTime[MSFL],	/* the data from all files can be present at once, maf 960701 */
		dlyt[MSFL], 	/* ? delay time, accessed by Dlyt */
		dlyn[MSFL], 
		dlyvm[MSFL], 
		dlyti[MSFL], 
		dlyni[MSFL], 
		dlytg, 
		dlyng, 
		wtg;
	int	lpolg;
	float	dstg, dlytig, dlynig;
	int	lsrc;
	float	srcfac;
	int 	ndxsum,
		nlnsum,
		ndwun,
		idwun,
		idwop;			/* distance window option: default is 1
					   1 = use min and max from data file
					   2 = use min from file, fix width
					   3 = fix data window from v1 to v2 */
	float	dwwid, dwlim[2];	/* distance window width and limit */
	int 	idaop;			/* distance axis option: default is 1
					   1 = fixed axis
					   2 = scaled axis	*/
	float	dalen,			/* distance axis length */
		dasca,			/* distance axis scale */
		del,			/* distance axis delta t */
		twlim[2]; 		/* distance axis limits */
	int	ltwlim;
	int	itaop;
	float	talen, tasca;
	int	lnorm,
		lnarli,
		lrslab,
		lrswt,
		lrspol,
		lrslin,
		lOriginDefault, 	/* sets location of origin in prs. formerly llefor, maf 961004.*/
		lrscur;			/* 1 means cursor on mode in prs */
	int	lPlottingTT ;		/* 1 when actually plotting travel time. maf 961004 */
        int	lorient;
	float	xpfrac, xpsize, axdel;
	int	laspect, lpswt, lpspl, lpssum, lpsper;
	int	npsper;
	int	lroset;
	int	iroset, nvmtp;
	int	lvm[MVM];
	int	ivm[MVM];
	float	vapp[MVM], t0vm[MVM], vappi[MVM], t0vmi[MVM];
	int	ntvm[MVM];
	float	tvm[MVM][2];
	int	ndvm[MVM];
	float	dvm[MVM][2];
	int	lcvapp[MVM], lct0vm[MVM];
	int	inmo, irefr;
	int	lxlabreq, lxlabdef, lylabreq, lylabdef;
	}	cmsss;
struct t_kmsss {
	char knmsum[MCPFN+1], kdwun[MDWUN][9], knmlab[9], kvmtp[MVMTP][9], 
	 kxlabreq[MCMSG+1], kylabreq[MCMSG+1];
	}	kmsss;


#ifdef DOINITS
/* allows C arrays to be used in FORTRAN-like expressions 
   for example:  cmsss.dst[0] can be accessed by Dst[1]. 
   this is convenient because in FORTRAN the array starts
   at 1 while in C the array starts at 0. */
float *const Dlyn = &cmsss.dlyn[0] - 1;
float *const Dlyni = &cmsss.dlyni[0] - 1;
float *const Dlyt = &cmsss.dlyt[0] - 1;
float *const Dlyti = &cmsss.dlyti[0] - 1;
float *const Dlyvm = &cmsss.dlyvm[0] - 1;
float *const Dst = &cmsss.dst[0] - 1;
float *const Dwlim = &cmsss.dwlim[0] - 1;
int *const Ivm = &cmsss.ivm[0] - 1;
int *const Lct0vm = &cmsss.lct0vm[0] - 1;
int *const Lcvapp = &cmsss.lcvapp[0] - 1;
int *const Lincl = &cmsss.lincl[0] - 1;
int *const Lpol = &cmsss.lpol[0] - 1;
int *const Lvm = &cmsss.lvm[0] - 1;
int *const Ndvm = &cmsss.ndvm[0] - 1;
int *const Ntvm = &cmsss.ntvm[0] - 1;
float *const T0vm = &cmsss.t0vm[0] - 1;
float *const T0vmi = &cmsss.t0vmi[0] - 1;
float *const Tbegin = &cmsss.beginTime[0] - 1;	/* maf 960701 */
float *const Tend = &cmsss.endTime[0] - 1;	/* maf 960701 */
float *const Twlim = &cmsss.twlim[0] - 1;
float *const Vapp = &cmsss.vapp[0] - 1;
float *const Vappi = &cmsss.vappi[0] - 1;
float *const Wt = &cmsss.wt[0] - 1;
#else
extern float *const Dlyn;
extern float *const Dlyni;
extern float *const Dlyt;
extern float *const Dlyti;
extern float *const Dlyvm;
extern float *const Dst;
extern float *const Dwlim;
extern int *const Ivm;
extern int *const Lct0vm;
extern int *const Lcvapp;
extern int *const Lincl;
extern int *const Lpol;
extern int *const Lvm;
extern int *const Ndvm;
extern int *const Ntvm;
extern float *const T0vm;
extern float *const T0vmi;
extern float *const Tbegin;	/* maf 960701 */
extern float *const Tend;	/* maf 960701 */
extern float *const Twlim;
extern float *const Vapp;
extern float *const Vappi;
extern float *const Wt;
#endif

