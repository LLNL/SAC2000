/* ../../inc/vars.h */

#define	BADDATABLOCKFLAG       1207
#define	BADINPUT	       1210
#define	BADVALUETYPE	       1204
#define	BIGVALUE	       65536
#define	MAXCENAME	128
#define	MAXCVNAME	128
#define	MAXLEVELS	10
#define	MAXVARS	400
#define	MAXVARSEXCEEDED	1202
#define	MLEVELS	10
#define	NOCURRENTLIST	1209
#define	NOTIMPLEMENTED	1206
#define	NOTVARSFILE	1208
#define	VALUEAPPLICATION       31
#define	VALUEBYTE	8
#define	VALUECOMPLEX	12
#define	VALUEDATA	2
#define	VALUEDBLCOMPLEX	13
#define	VALUEDOUBLE	11
#define	VALUEINTEGER	4
#define	VALUELIST	1
#define	VALUELOGICAL	14
#define	VALUENIL	0
#define	VALUEREAL	10
#define	VALUESHORTINT	6
#define	VALUESTRING	3
#define	VALUESYMBOL	15
#define	VALUEUNSBYTE	9
#define	VALUEUNSINT	5
#define	VALUEUNSSHORTINT	7
#define	VARNODELETE	1205
#define	VARNOTFOUND	1201
#define	VARSINCR	32
#define	VARSLISTEXISTS	1211
#define	VARSLISTNOTFOUND	1203
#define	VARSTERMINATOR	0
#define	VARSVERSION	1
#define NVFILELIST     10
#define NVFILEINC       5



struct t_kmvars {
	byte vabsflag, vlistdelim, cfill1, cfill2;
	char varsname[MAXVARS][MAXCVNAME+1], varsidcode[5], valuename[MAXCVNAME+1];
	}	kmvars;
struct t_cmvars {
	int varslength[MAXVARS], numvars, varsindex[MAXVARS], varsnilindex[MAXVARS];
	int varsmodified[MAXVARS], varsindirect[MAXVARS], lvarsinit;
	int ncvarsname[MAXVARS], currentnode, varsnode, varsnode1, descindex, 
	 desclength, valuelength, valuetype, namelength;
	int deleteflag, readonlyflag, indirectflag, sharedflag, 
	 reservedflag, applflag1, applflag2;
	}	cmvars;
struct t_kmgetvlist {
	char sublistnames[MAXLEVELS][MAXCENAME+1];
	}	kmgetvlist;
struct t_cmgetvlist {
	int indexsave[MAXLEVELS], nlevelsgt;
	}	cmgetvlist;
struct t_cmcopyvlist {
	int node1savecp[MAXLEVELS], node2savecp[MAXLEVELS], indexsavecp[MAXLEVELS], 
	 nlevelscp;
	}	cmcopyvlist;
struct t_kmprintvlist {
	char varssavepr[MLEVELS][MAXCVNAME+1];
	}	kmprintvlist;
struct t_cmprintvlist {
	int indexsavepr[MLEVELS], nlevelspr;
	}	cmprintvlist;

struct varsfile {
        char *varsname;
        char *variable;
        FILE *value;
        };

struct t_varsfile {
        int nallocated;
        int nentries;
        struct varsfile *filelist;
        } vfilelist;
          


#ifdef DOINITS
int *const Indexsave = &cmgetvlist.indexsave[0] - 1;
int *const Indexsavecp = &cmcopyvlist.indexsavecp[0] - 1;
int *const Indexsavepr = &cmprintvlist.indexsavepr[0] - 1;
int *const Ncvarsname = &cmvars.ncvarsname[0] - 1;
int *const Node1savecp = &cmcopyvlist.node1savecp[0] - 1;
int *const Node2savecp = &cmcopyvlist.node2savecp[0] - 1;
int *const Varsindex = &cmvars.varsindex[0] - 1;
int *const Varsindirect = &cmvars.varsindirect[0] - 1;
int *const Varslength = &cmvars.varslength[0] - 1;
int *const Varsmodified = &cmvars.varsmodified[0] - 1;
int *const Varsnilindex = &cmvars.varsnilindex[0] - 1;
#else
extern int *const Indexsave;
extern int *const Indexsavecp;
extern int *const Indexsavepr;
extern int *const Ncvarsname;
extern int *const Node1savecp;
extern int *const Node2savecp;
extern int *const Varsindex;
extern int *const Varsindirect;
extern int *const Varslength;
extern int *const Varsmodified;
extern int *const Varsnilindex;
#endif

