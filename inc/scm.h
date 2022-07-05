/* ../../inc/scm.h */

#define	MQGAIN	8
#define	MRGLMT	3
#define	MRGLTP	2



struct t_cmscm {
	float usraz, usrang;
	int lnpreq;
	float rqqcon, rqrcon, rqccon, dtnew, eps;
	int lbreq;
	float breq;
	int lnreq;
	int nreq, iqgain[MQGAIN + 1];
	float qlevel;
	int nqmant, nstrfc;
	int lstrfi, lmean;
	int nhalf, irgltp, irglmt;
	float thold;
	int lrglwin;
	float orglwin[2];
	int ndecfc;
	int ldecfi;
	}	cmscm;
struct t_kmscm {
	char krottp[9], krgltp[MRGLTP][9], krglmt[MRGLMT][9], krglwin[2][9];
	}	kmscm;


#ifdef DOINITS
int *const Iqgain = &cmscm.iqgain[0] - 1;
float *const Orglwin = &cmscm.orglwin[0] - 1;
#else
extern int *const Iqgain;
extern float *const Orglwin;
#endif

