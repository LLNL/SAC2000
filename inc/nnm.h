/* ../../inc/nnm.h */

struct t_kmnnm {
	char kwritenn[MCPFN+1];
	}	kmnnm;
struct t_cmnnm {
	int numpoints, numfiles;
	float headerarray[MDFL];
	}	cmnnm;


#ifdef DOINITS
float *const Headerarray = &cmnnm.headerarray[0] - 1;
#else
extern float *const Headerarray;
#endif

