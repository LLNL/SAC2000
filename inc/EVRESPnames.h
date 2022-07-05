#ifndef TRUE
#define TRUE   1
#endif
#ifndef FALSE
#define FALSE   0
#endif


/* These are the keywords recognized by the routines in the EVRESPnames module */
/*  Transfer from or transfer to  */
enum Direction { FROM, TO };


/*  The types of evalresp sub-parameters  */
enum EVparam { CHANNEL, STATION, NETWORK, LOCID, DATE, TIME, TYPE, FILENAME, NAME_FROM_DB};


/* Prototypes for ANSI C */
#ifdef __STDC__

/* These are the function prototypes for this module */
void setStationName(char *, enum Direction);
void setNetworkName(char *, enum Direction);
void setChannelName(char *, enum Direction);
void setLocidName  (char *, enum Direction);
void setFileName(char *name, enum Direction dir);
void setUseDBName(int value, enum Direction dir);

char *getStationName(enum Direction);
char *getNetworkName(enum Direction);
char *getChannelName(enum Direction);
char *getLocidName  (enum Direction);
char *getFileName(enum Direction dir);
int getUseDBName(enum Direction dir);
int isSet(enum EVparam, enum Direction);
void clearEVRESPstrucs(void);

void setTransferDirection(int);
int getTransferDirection(void);

void setDate(char *, enum Direction);
void setTime(char *, enum Direction);
int getYear(enum Direction);
int getJday(enum Direction);
int getHour(enum Direction);
int getMinute(enum Direction);
int getSecond(enum Direction);

void setType(char *, enum Direction);
char *getType(enum Direction);

#else

/* Prototypes for K&R C */
void setStationName();
void setNetworkName();
void setChannelName();
void setLocidName();

char *getStationName();
char *getNetworkName();
char *getChannelName();
char *getLocidName();

int isSet();
void clearEVRESPstrucs();

void setTransferDirection();
int getTransferDirection();

void setDate();
void setTime();
int getYear();
int getJday();
int getHour(Direction);
int getMinute();
int getSecond();

void setType();
char *getType();

#endif
