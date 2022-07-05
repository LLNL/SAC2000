/* ../../inc/uom.h */

#define	MDIFTP	5
#define	MUOCON	MDFL



struct t_cmuom {
	float conadd[MUOCON], consub[MUOCON], conmul[MUOCON], condiv[MUOCON];
	int ndiftp, idiftp;
	int ltrap;
	float extuom[400];
	}	cmuom;
struct t_kmuom {
	char kdiftp[MDIFTP][9];
	}	kmuom;


#ifdef DOINITS
float *const Conadd = &cmuom.conadd[0] - 1;
float *const Condiv = &cmuom.condiv[0] - 1;
float *const Conmul = &cmuom.conmul[0] - 1;
float *const Consub = &cmuom.consub[0] - 1;
float *const Extuom = &cmuom.extuom[0] - 1;
#else
extern float *const Conadd;
extern float *const Condiv;
extern float *const Conmul;
extern float *const Consub;
extern float *const Extuom;
#endif
