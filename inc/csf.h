/* ../../inc/csf.h */

#define	MARG	20
#define	MCS	600
#define	MLEV	5
#define	MTOK	200



struct t_cmcsf {
	int nlev, nlvusd[MLEV], nlvlen[MLEV], ntop, nmvtop, ncur, 
	 nbot, nmvbot;
	int lcsemp;
	int ncs, narg, narg1, narg2, ntokdl, nmsgdl, neoc, nstype[MTOK], 
	 ntok;
	float extcsf[20];
	}	cmcsf;
struct t_kmcsf {
	char knmmac[MCPFN+1], karg1[MARG][9], karg2[MARG][9], kprmtd[9], knmlev[MLEV][MCPFN+1], 
	 kcs[MCS][9], kinv[2][9], keoc[2][9], kpass[9], keocs[9], kappnd[9], 
	 kcmt[9], kmac[2][9], ksingl[9], ktext[9], kdel[9], kquery[9], 
	 klabl[9], ksvout[9][9], ktokdl[10][9], kmsgdl[10][9], ktok[MTOK][9], 
	 kxtcsf[10][9];
	}	kmcsf;


#ifdef DOINITS
float *const Extcsf = &cmcsf.extcsf[0] - 1;
int *const Nlvlen = &cmcsf.nlvlen[0] - 1;
int *const Nlvusd = &cmcsf.nlvusd[0] - 1;
int *const Nstype = &cmcsf.nstype[0] - 1;
#else
extern float *const Extcsf;
extern int *const Nlvlen;
extern int *const Nlvusd;
extern int *const Nstype;
#endif

