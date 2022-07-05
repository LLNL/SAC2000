/* ../../inc/eam.h */

struct t_cmeam {
	float c1, c2, c3, c4, c5, c6, c7, c8;
	int i3, i4, i5, i6, i8, i9;
	float d5, d8, d9;
	int lhpfop;
	FILE *nhpfun;
	int lhpfic;
	int nhpfic[2], npyear, npjday, npmon, npday, nphour, npmin;
	int lpphas;
	float psecs;
	int lsphas;
	float ssecs;
	int lampx;
	float ampx, prx;
	int lfini;
	float fmp;
	int lapfop;
	FILE *napfun;
	float pkcmpa, pkcmpi, pkseci;
	int npkyr, npkjdy, npkhr, npkmn;
	float pksecs, pkampl, dtwf[5], awf[5];
	int lpfgmt, lpfstd, lichpf;
	int ichpf[2];
	int lvalpk;
	float exteam[19];
	}	cmeam;
struct t_kmeam {
	char khpfnm[MCPFN+1], kstid[9], kpwave[9], kswave[9], kapfnm[MCPFN+1], 
	 kpkid[9], kpkev[17], kpkst[9], kpksrc[9], kpkrid[9], kpkfmt[9], 
	 kxteam[10][9];
	}	kmeam;


#ifdef DOINITS
float *const Awf = &cmeam.awf[0] - 1;
float *const Dtwf = &cmeam.dtwf[0] - 1;
float *const Exteam = &cmeam.exteam[0] - 1;
int *const Ichpf = &cmeam.ichpf[0] - 1;
int *const Nhpfic = &cmeam.nhpfic[0] - 1;
#else
extern float *const Awf;
extern float *const Dtwf;
extern float *const Exteam;
extern int *const Ichpf;
extern int *const Nhpfic;
#endif

