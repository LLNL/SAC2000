/* ../../inc/smm.h */

#define	MVEL	10



struct t_cmsmm {
	int lmtw;
	float omtw[2];
	int nvel;
	float vel[MVEL];
	int ldistr;
	float distr;
	int loriginr;
	float originr;
	int lgmt;
	int iodttm[6];
	float value;
	int lgedata;
	float winlen;
	int lnoisemtw;
	float onoisemtw[2];
	int irmspick;
	}	cmsmm;
struct t_kmsmm {
	char kmtw[2][9], ktmark[9], kvmark[9], kpmark[9], knoisemtw[2][9];
	}	kmsmm;


#ifdef DOINITS
int *const Iodttm = &cmsmm.iodttm[0] - 1;
float *const Omtw = &cmsmm.omtw[0] - 1;
float *const Onoisemtw = &cmsmm.onoisemtw[0] - 1;
float *const Vel = &cmsmm.vel[0] - 1;
#else
extern int *const Iodttm;
extern float *const Omtw;
extern float *const Onoisemtw;
extern float *const Vel;
#endif

