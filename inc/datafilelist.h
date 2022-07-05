/* ../../inc/datafilelist.h */

struct t_kmdatafilelist {
	char kselectmode[9];
	}	kmdatafilelist;
struct t_cmdatafilelist {
	int nentries, iselect[MDFL], nselect, jselect;
	}	cmdatafilelist;


#ifdef DOINITS
int *const Iselect = &cmdatafilelist.iselect[0] - 1;
#else
extern int *const Iselect;
#endif

