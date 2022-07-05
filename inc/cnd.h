/* ../../inc/cnd.h */

#define	MDOLEVEL	10
#define	MIFLEVEL	10



struct t_cnd {
	int niflevel;
	int lifresp[MIFLEVEL];
	int ndolevel, ndolines[MDOLEVEL], ndotype[MDOLEVEL], idoin1[MDOLEVEL], 
	 idoin2[MDOLEVEL];
	}	cnd;
struct t_kcnd {
	char kdovar[MDOLEVEL][MCPFN+1], kdolist[MDOLEVEL][MCPFN+1], kdoname[MDOLEVEL][MCPFN+1];
	}	kcnd;


#ifdef DOINITS
int *const Idoin1 = &cnd.idoin1[0] - 1;
int *const Idoin2 = &cnd.idoin2[0] - 1;
int *const Lifresp = &cnd.lifresp[0] - 1;
int *const Ndolines = &cnd.ndolines[0] - 1;
int *const Ndotype = &cnd.ndotype[0] - 1;
#else
extern int *const Idoin1;
extern int *const Idoin2;
extern int *const Lifresp;
extern int *const Ndolines;
extern int *const Ndotype;
#endif

