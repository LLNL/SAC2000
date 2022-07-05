/* ../../inc/usr.h */

#define	MLUSR	10
#define	MUSR	30
#define	MVUSR	100



struct t_cmusr {
	int nusr, ndxusr[MUSR], numusr[MUSR];
	float vusr[MVUSR];
	int nlusr;
	int lusr[MLUSR];
	}	cmusr;
struct t_kmusr {
	char kusr[MUSR][9], klusr[MLUSR][9];
	}	kmusr;


#ifdef DOINITS
int *const Lusr = &cmusr.lusr[0] - 1;
int *const Ndxusr = &cmusr.ndxusr[0] - 1;
int *const Numusr = &cmusr.numusr[0] - 1;
float *const Vusr = &cmusr.vusr[0] - 1;
#else
extern int *const Lusr;
extern int *const Ndxusr;
extern int *const Numusr;
extern float *const Vusr;
#endif

