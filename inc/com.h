/* ../../inc/com.h */

#define	MCOM	200
#define MKARGS  100



struct t_kmcom {
	char kcom[MCOM][9];	/* command stack, MCOM commands, 8 chars int plus '\0' */
        char **kargs;		/* arguments to accompany commands in kcom. */
        int nkargs;		/* number of arguments. */
        int nkargs_allocated;	/* number of allocated spaces for arguments. */
	}   kmcom;

struct t_cmcom {
	int jcom, 		/* indexes a command from kcom and argument from kargs */
		ncom,		/* number of commands in stack. */
		itypcm[MCOM];

	float	flnum[MCOM];

	int inumbr, 
		ialpha, 
		icont, 
		ncerr;
	}   cmcom;


#ifdef DOINITS
float *const Flnum = &cmcom.flnum[0] - 1;
int *const Itypcm = &cmcom.itypcm[0] - 1;
#else
extern float *const Flnum;
extern int *const Itypcm;
#endif

