/* ../../inc/xyz.h */

#define	MZLLIST	40
#define	MZREGIONS	(2*MZLLIST + 1)



struct t_cmxyz {
	int lcleanup;
	float zllist[MZLLIST];
	int nzllist;
	int lzllist, lzlmin;
	float zlmin;
	int lzlmax;
	float zlmax;
	int lzlinc;
	float zlinc;
	int lzlines;
	int izlines[MZLLIST], nzlines;
	float zregions[MZREGIONS];
	int nzregions;
	int laspect;
	}	cmxyz;


#ifdef DOINITS
int *const Izlines = &cmxyz.izlines[0] - 1;
float *const Zllist = &cmxyz.zllist[0] - 1;
float *const Zregions = &cmxyz.zregions[0] - 1;
#else
extern int *const Izlines;
extern float *const Zllist;
extern float *const Zregions;
#endif

