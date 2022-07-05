/* ../../inc/fir.h */

#define	MFIR	255



struct t_kmfir {
	char kidfir[81];
	}	kmfir;
struct t_cmfir {
	int ncfir;
	float cfir[MFIR], dtfir;
	}	cmfir;



#ifdef DOINITS
float *const Cfir = &cmfir.cfir[0] - 1;
#else
extern float *const Cfir;
#endif

