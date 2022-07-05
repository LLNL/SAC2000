#ifndef SACIO_H
#define SACIO_H

#include "SacHeader.h"


#ifndef FALSE
#       define FALSE 0
#       define TRUE !FALSE
#endif /* FALSE */

FILE *sacfile;
static char sacErrorStrg[300];
static char strgDef[] = "-12345";
static int intDef = -12345;
static float fltDef = -12345.0;

static int PREPICK = 1;
static int  POSTPICK = 2;
static int  CUTPOINT = 3;

typedef  enum {Any, MsMag, MlMag, MbMag} MagType;


int  AridJoinsWithWfid(DBlist tree, int arid, int wfid);





extern int sacLoadDataFromFiles(char *specifier, int SkipData,
                                char* WorkSetName , int takeEvid );

extern sacSACdata *sacInput(char *filename, struct SACheader *header, ...);
extern void sacHeaderFromCSS(DBlist tree, struct SACheader *header, struct wfdiscList *w,
                             int RefTimeType, double *RefTime, MagType Mtype);
extern int sacWriteSacFile(DBlist tree, struct wfdiscList *w, char 
		                                       *dir, char *fname);
int sacLoadFromHeaderAndData(struct SACheader *header, sacSACdata *data,
                             char *WorkSetName, int Replace, int index,
                             int UpdateData , int takeEvid );

#endif
