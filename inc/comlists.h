/* ../../inc/comlists.h */

#define	MCOMNAMES	800
#define	MEXTCOMNAMES	100
#define	MODULEEXTCOM	100
#define	MPROCESSES	5



struct t_cmcomlists {
	int icomlist, icomliststart[MPROCESSES], ncomlistentries[MPROCESSES], 
	 icommodule[MCOMNAMES], icomindex[MCOMNAMES], nextcomnames, iextcomindex[MEXTCOMNAMES];
	}	cmcomlists;
struct t_kmcomlists {
	char kcomnames[MCOMNAMES][9], kextcomnames[MEXTCOMNAMES][9];
	}	kmcomlists;


#ifdef DOINITS
int *const Icomindex = &cmcomlists.icomindex[0] - 1;
int *const Icomliststart = &cmcomlists.icomliststart[0] - 1;
int *const Icommodule = &cmcomlists.icommodule[0] - 1;
int *const Iextcomindex = &cmcomlists.iextcomindex[0] - 1;
int *const Ncomlistentries = &cmcomlists.ncomlistentries[0] - 1;
#else
extern int *const Icomindex;
extern int *const Icomliststart;
extern int *const Icommodule;
extern int *const Iextcomindex;
extern int *const Ncomlistentries;
#endif

