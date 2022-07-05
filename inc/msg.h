/* ../../inc/msg.h */

#define	MCOMMANDS	4
#define	MERRORS	1
#define	MFMSG	500
#define	MLIMSG	5
#define	MMACROS	5
#define	MOUTPUT	3
#define	MPROCESSED	6
#define	MTPMSG	6
#define	MUNITS	5
#define	MWARNINGS	2



struct t_kmmsg {
	char	ktpmsg[MTPMSG][9], 
		klimsg[MLIMSG][MCMSG+1],	/* list of messages */
		kfmsg[MFMSG][MCMSG+1];
	}   kmmsg;
struct t_cmmsg {
	int nummsg, itpmsg, nlimsg, nchmsg;
	int autoout;
	int nunits;
        FILE *iunits[MUNITS];
	int lsend[MUNITS][MTPMSG];
	int nfmsg, ifmsg[MFMSG];
	}	cmmsg;


#ifdef DOINITS
int *const Ifmsg = &cmmsg.ifmsg[0] - 1;
#else
extern int *const Ifmsg;
extern int *const Iunits;
#endif

