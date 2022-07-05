/* ../../inc/dfir.h */

#define	MBANDS	10
#define	MFILT	255
#define	MSIZE1	(MFILT/2 + 2)
#define	MSIZE2	(16*MSIZE1)
#define	MSIZE3	(2*MBANDS)
#define	MXNSECTS	30



struct t_cmdfir {
	double pi2, ad[MSIZE1], dev, x[MSIZE1], y[MSIZE1];
	float grid[MSIZE2], des[MSIZE2], wt[MSIZE2], alpha[MSIZE1];
	int iext[MSIZE1], nfcns, ngrid;
	float h[MSIZE1];
	int infilt, nfilt, jtype, nbands, ngrden;
	float edgehz[MSIZE3], edge[MSIZE3], fx[MBANDS], wtx[MBANDS], deviat[MBANDS], 
	 delta;
	}	cmdfir;
struct t_cmfir2 {
	char kfilnm[9];
	int nlnwr1;
	char ktype[9];
	int nid;
	char kid[81];
	float dt;
	int ncoeff;
	}	cmfir2;
struct t_cmfir3 {
	int nsects;
	float sn[3*MXNSECTS], sd[3*MXNSECTS];
	char filttype[3];
	}	cmfir3;


#ifdef DOINITS
double *const Ad = &cmdfir.ad[0] - 1;
float *const Alpha = &cmdfir.alpha[0] - 1;
float *const Des = &cmdfir.des[0] - 1;
float *const Deviat = &cmdfir.deviat[0] - 1;
float *const Edge = &cmdfir.edge[0] - 1;
float *const Edgehz = &cmdfir.edgehz[0] - 1;
float *const Fx = &cmdfir.fx[0] - 1;
float *const Grid = &cmdfir.grid[0] - 1;
float *const H = &cmdfir.h[0] - 1;
int *const Iext = &cmdfir.iext[0] - 1;
float *const Sd = &cmfir3.sd[0] - 1;
float *const Sn = &cmfir3.sn[0] - 1;
/* float *const Wt = &cmdfir.wt[0] - 1; */
float *const Wtx = &cmdfir.wtx[0] - 1;
double *const X = &cmdfir.x[0] - 1;
double *const Y = &cmdfir.y[0] - 1;
#else
extern double *const Ad;
extern float *const Alpha;
extern float *const Des;
extern float *const Deviat;
extern float *const Edge;
extern float *const Edgehz;
extern float *const Fx;
extern float *const Grid;
extern float *const H;
extern int *const Iext;
extern float *const Sd;
extern float *const Sn;
/* extern float *const Wt; */
extern float *const Wtx;
extern double *const X;
extern double *const Y;
#endif

