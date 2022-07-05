/* ../../inc/tpf.h */

#define	MCPS	8
#define	MDL	8



struct t_cmtpf {
	int jtok1, ntokdl, nmsgdl;
	float exttpf[20];
	}	cmtpf;
struct t_kmtpf {
	byte ktok1[MCPS], kbl1, ktokdl[MDL], kmsgdl[MDL];
	char kxttpf[10][9];
	}	kmtpf;


#ifdef DOINITS
float *const Exttpf = &cmtpf.exttpf[0] - 1;
byte *const Kmsgdl = &kmtpf.kmsgdl[0] - 1;
byte *const Ktok1 = &kmtpf.ktok1[0] - 1;
byte *const Ktokdl = &kmtpf.ktokdl[0] - 1;
#else
extern float *const Exttpf;
extern byte *const Kmsgdl;
extern byte *const Ktok1;
extern byte *const Ktokdl;
#endif

