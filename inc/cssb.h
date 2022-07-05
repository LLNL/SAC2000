#ifndef CSSB_H
#define CSSB_H

#include "dblUserData.h"

/* Define code for type of machine suds is being compiled on. */
/* 'X' is for Sparc architecture */
#define MACHINE         'X'

#define NOFLOAT         -999.0
#define NONNREAL        -1.0
#define NOINT           -1
#define NOTIME          -9999999999.999 
#define NOENDTIME       +9999999999.999
#define NOSTRG          "-"
#define NOOTHER         0.0


/* Structure types or identifiers */
#define AFFILIATION     33
#define ARRIVAL         34
#define ASSOC           35
#define EVENT           36
#define GREGION         37
#define INSTRUMENT      38
#define LASTID          39
#define NETMAG          40
#define NETWORK         41
#define ORIGERR         42
#define ORIGIN          43
#define SENSOR          44
#define SITE            45
#define SITECHAN        46
#define SREGION         47
#define STAMAG          48
#define STASSOC         49
#define WFDISC          50
#define WFTAG           51
#define UDCOMMENT       52
#define DATACOM         53
#define UDMATRIX        54
#define REMARK          55
#define SACDATA         56

#define ST_MAGIC   'S'        /* magic character for sync in structtag */
#define SPARC      'X'        /* architecture byte for sparc */
#define PC         '6'        /* architecture byte for 8086  */
typedef struct {
   char sync;                 /* Must be ST_MAGIC. If not present, error exists. */
   char machine;              /* code for machine writing binary file for use */
   short id_struct;           /* structure identifier: numbers defined above */
   int len_struct;           /* structure length in bytes for fast reading */
   int len_data;             /* length of data following structure in bytes */
} CSSBTAG;


typedef struct{
   int Datalen;
   enum dataType Type;
   int CommentLen;
   int Reference;
} DataCom;

typedef struct{
   int nrows;
   int ncols;
   int CommentLen;
} UDmatrix;   

int WriteCSSBfile(const char *WorkSetName, const char *fname, int Verbose, int Version, const char *tType);
int ReadCSSBfile(const char *fname, const char *WorkSetName, int Replace, int MaxTraces,
                 int Verbose, double MaxPhysMem, int takeEvid );

int WriteCSSflatFiles(const char *WorkSetName, const char *basename);


char **TokenLineToStrgList(char* line, int* Ntokens, char* Delimit);
void FreeStringArray(char** S, int Nlines);
int cssReadFlatFiles(char* Root, char *WorkSetName, int Replace, int MaxWaveforms,
                    char** Station, int Nstation, 
                    char** Channel, int Nchannel, 
                    char** Band, int Nband, 
                    char** Code, int Ncode,
                    char** Orientation, int Norientation, 
                    char** Author, int Nauthor,
                    char** Filelist, int Nfiles,
		    char** Phaselist, int Nphases,
                    int verbose, double MaxPhysMem, int *takeEvid);


int InList(char* str, char** List, int Nitems) ;
int InOneCharList(char* chan, char** List, int Nitems, int pos) ;
int FileExists(char *dir, char *dfile) ;


#endif


