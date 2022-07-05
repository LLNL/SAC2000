/* ../../inc/icm.h */

#define	MAXFP	10
#define	MAXIP	1
#define	MAXKP	2
#define	MINSTR	50



struct t_cmicm {
	float fpfrom[MAXFP];
	int lfpfrom[MAXFP];
	int ipfrom[MAXIP];
	int lipfrom[MAXIP], lkpfrom[MAXKP];
	float fpto[MAXFP];
	int lfpto[MAXFP];
	int ipto[MAXIP];
	int lipto[MAXIP], lkpto[MAXKP], lfreql, lprew;
	int iprew;
	float freq[4];
	int ninstr;
	int lfd ;
	}	cmicm;
struct t_kmicm {
	char kpfrom[MAXKP][MCPFN+1], kpto[MAXKP][MCPFN+1], kinstr[MINSTR][9];
	}	kmicm;


#ifdef DOINITS
float *const Fpfrom = &cmicm.fpfrom[0] - 1;
float *const Fpto = &cmicm.fpto[0] - 1;
float *const Freq = &cmicm.freq[0] - 1;
int *const Ipfrom = &cmicm.ipfrom[0] - 1;
int *const Ipto = &cmicm.ipto[0] - 1;
int *const Lfpfrom = &cmicm.lfpfrom[0] - 1;
int *const Lfpto = &cmicm.lfpto[0] - 1;
int *const Lipfrom = &cmicm.lipfrom[0] - 1;
int *const Lipto = &cmicm.lipto[0] - 1;
int *const Lkpfrom = &cmicm.lkpfrom[0] - 1;
int *const Lkpto = &cmicm.lkpto[0] - 1;
#else
extern float *const Fpfrom;
extern float *const Fpto;
extern float *const Freq;
extern int *const Ipfrom;
extern int *const Ipto;
extern int *const Lfpfrom;
extern int *const Lfpto;
extern int *const Lipfrom;
extern int *const Lipto;
extern int *const Lkpfrom;
extern int *const Lkpto;
#endif

