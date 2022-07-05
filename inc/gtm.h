/* ../../inc/gtm.h */

#define	MSISYM	8
#define	MSYM	16
#define	MSYMTB	40
#define	MUNSYM	1



struct t_cmgtm {
	float xwcmin, xwcmax, ywcmin, ywcmax, xvpmin, xvpmax, yvpmin, 
	 yvpmax, xmpwv1, xmpwv2, ympwv1, ympwv2;
	int lvpclip;
	float xvpold, yvpold;
	int ivpold, isym;
	float symsz, symgap;
	int lxdiv;
	float xdiv;
	int lnxdiv;
	int nxdiv;
	float xnicsp;
	int lydiv;
	float ydiv;
	int lnydiv;
	int nydiv;
	float ynicsp;
	int lnwsym, lscsym;
	int jsyml1, jsym1b, jsym1e;
	int ldbsym;
	int jsyml2, jsym2b, jsym2e, isyml1[MSYM], isyml2[MSYM], nsymlc[MSISYM + 1];
	int ldrsym[MSYMTB];
	float xsymtb[MSYMTB], ysymtb[MSYMTB], fac[9];
	}	cmgtm;
struct t_kmgtm {
	byte kfac[9];
	}	kmgtm;


#ifdef DOINITS
float *const xvp = (float*)&cmgtm.xvpmin;
float *const xwc = (float*)&cmgtm.xwcmin;
float *const yvp = (float*)&cmgtm.yvpmin;
float *const ywc = (float*)&cmgtm.ywcmin;

       	/* OFFSET Vectors w/subscript range: 1 to dimension */
/* float *const Xvp = &xvp[0] - 1; */
float *const Xvp = (float*)(&cmgtm.xvpmin - 1);

/* float *const Xwc = &xwc[0] - 1; */
float *const Xwc = (float*)(&cmgtm.xwcmin - 1);

/* float *const Yvp = &yvp[0] - 1; */
float *const Yvp = (float*)(&cmgtm.yvpmin - 1);

/* float *const Ywc = &ywc[0] - 1; */
float *const Ywc = (float*)(&cmgtm.ywcmin - 1);
#else
extern float *const xvp;
extern float *const xwc;
extern float *const yvp;
extern float *const ywc;
extern float *const Xvp;
extern float *const Xwc;
extern float *const Yvp;
extern float *const Ywc;
#endif

