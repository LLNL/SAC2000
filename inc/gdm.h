/* ../../inc/gdm.h */

#define	MCTSIZE	256
#define	MGD	5
#define	MWINDOWS 10
#define NPSCIMAGE 237


struct t_kmgdm {
	char kgdnam[MGD][13], ctname[MCTSIZE - 1-(0)+1][9], kgtfn[4][41];
	}	kmgdm;
struct t_cmgdm {
	int lgdon[MGD];
	int igdtyp[MGD];
	int lactiv, lpasiv, lcur;
	int igdcur, iflcur[MGD], igdhc, iflhc[MGD], igdtxt;
	int lginit, lbegf;
	int iwindow;
	float xwindowmin[MWINDOWS], xwindowmax[MWINDOWS], ywindowmin[MWINDOWS], 
	 ywindowmax[MWINDOWS];
	int lvsful;
	float vsrat, xvs[2], yvs[2];
	int lvsclip, ltsoft;
	float thgt, twidth, tangle;
	int ihjust, ivjust, itcol;
	int lfhard;
	int icolor, nctsize;
        int npscimage;
	float ctred[MCTSIZE - 1-(0)+1], ctgreen[MCTSIZE - 1-(0)+1], ctblue[MCTSIZE - 1-(0)+1];
	int iline;
	float xold, yold;
	int iold, nfont, iofset, ispace, iyoff, iyhigh, maxstr;
	float fnthit, fntwid, fntrot;
	short int ascstr[128], stxmin[128], stxmax[128], stroke[3252];
        int lgui;
	}	cmgdm;



#ifdef DOINITS
short *const Ascstr = &cmgdm.ascstr[0] - 1;
int *const Iflcur = &cmgdm.iflcur[0] - 1;
int *const Iflhc = &cmgdm.iflhc[0] - 1;
int *const Igdtyp = &cmgdm.igdtyp[0] - 1;
int *const Lgdon = &cmgdm.lgdon[0] - 1;
short *const Stroke = &cmgdm.stroke[0] - 1;
short *const Stxmax = &cmgdm.stxmax[0] - 1;
short *const Stxmin = &cmgdm.stxmin[0] - 1;
float *const Xvs = &cmgdm.xvs[0] - 1;
float *const Xwindowmax = &cmgdm.xwindowmax[0] - 1;
float *const Xwindowmin = &cmgdm.xwindowmin[0] - 1;
float *const Yvs = &cmgdm.yvs[0] - 1;
float *const Ywindowmax = &cmgdm.ywindowmax[0] - 1;
float *const Ywindowmin = &cmgdm.ywindowmin[0] - 1;
#else
extern short *const Ascstr;
extern int *const Iflcur;
extern int *const Iflhc;
extern int *const Igdtyp;
extern int *const Lgdon;
extern short *const Stroke;
extern short *const Stxmax;
extern short *const Stxmin;
extern float *const Xvs;
extern float *const Xwindowmax;
extern float *const Xwindowmin;
extern float *const Yvs;
extern float *const Ywindowmax;
extern float *const Ywindowmin;
#endif

