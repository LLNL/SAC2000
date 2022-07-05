#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "cssb.h"
#include "../time/timefuncs.h"
#include "../cssListOps/cssStrucs.h"
#include "../cssListOps/cssListStrucs.h"
#include "../cssListOps/dblUserData.h"
#include "../cssListOps/dblPublicDefs.h"
#include "../cssListOps/cssListOps.h"
#include "../cssListOps/dblErrors.h"
#include "../smDataIO.h"
#include "../stringfun.h"
#include "../cssListOps/cssArchitecture.h"

int CSSfltDefined(float value);
int CSSdblDefined(double value);
int CSSlngDefined(int value);
int CSSchrDefined(char *value);
void gcCollect ( DBlist dblList );
void byteswap ( void *x, int n );

static char* MakeFileName(const char* basename, const char* tableName)
{
   char* fname;
   int n = strlen(basename) + strlen(tableName) + 4;
   fname = (char*) malloc( n );
   if ( basename[ 0 ] != '/' ){
      strcpy(fname,"./");
      strcat(fname, basename);
   }
   else
      strcpy(fname, basename);
   strcat(fname, ".");
   strcat(fname, tableName);
   return fname;
}
/* ---------------------------------------------------------------- */



static void StrAppend(char *buffer, char *fmt, int len, char *str)
{
   char OneItem[100];
   sprintf(OneItem,fmt, str);
   if(strlen(OneItem) > len)
      printf("Truncating (%s) to %d characters...\n",OneItem, len);

   strncat(buffer, OneItem, len);
   strcat(buffer, " ");
}
/* ---------------------------------------------------------------- */



static void DblAppend(char *buffer, char *fmt, int len, double val)
{
   char OneItem[100];
   char msg[100];

   sprintf(OneItem,fmt, val);
   strncat(buffer, OneItem, len);
   strcat(buffer, " ");

   if(strlen(OneItem) > len){
      sprintf(msg,"%s truncated to (", OneItem);
      strncat(msg, OneItem, len);
      printf("%s) in order to fit format requirements.\n",msg);
   }
      
}
/* ---------------------------------------------------------------- */



static void LngAppend(char *buffer, char *fmt, int len, int val)
{
   char OneItem[100];
   char msg[100];
   sprintf(OneItem,fmt, val);
   strncat(buffer, OneItem, len);
   strcat(buffer, " ");

   if(strlen(OneItem) > len){
      sprintf(msg,"%d truncated to (", val);
      strncat(msg, OneItem, len);
      printf("%s) in order to fit format requirements.\n",msg);
   }

}
/* ---------------------------------------------------------------- */
      






static int WriteAffiliationRecord(struct affiliationList *af, FILE* ptr, int ver)
{
   char buffer[70];
   if(!CSSchrDefined(af->element->sta) || !CSSchrDefined(af->element->net) ){
      printf("Skipping affiliation record because of null keys...\n");
      return 1;
   }
   *buffer = '\0';
   StrAppend(buffer, "%-8s", 8,    af->element->net);
   StrAppend(buffer, "%-6s", 6,    af->element->sta);
   
   if ( ver == 39 ) {
   	DblAppend(buffer, "%17.5f", 17, af->element->time);
   	DblAppend(buffer, "%17.5f", 17, af->element->endtime);
   }

   StrAppend(buffer, "%-17s", 17,  af->element->lddate);

   if( fprintf(ptr, "%s\n", buffer) == EOF )return 0;
   return 1;    
}
/* ---------------------------------------------------------------- */

      


static int WriteArrivalRecord(struct arrivalList *ar, FILE* ptr, int ver)
{
   char buffer[250];



   if(!CSSchrDefined(ar->element->sta) || !CSSchrDefined(ar->element->chan) ||
      !CSSdblDefined(ar->element->time) )
      if(!CSSlngDefined(ar->element->arid) ){
         printf("Skipping arrival record because of missing key...\n");
         return 1;
      }

   *buffer = '\0';
   StrAppend(buffer, "%-6s", 6,    ar->element->sta);
   DblAppend(buffer, "%17.5f", 17, ar->element->time);
   if( ver == 39 ) {
   	LngAppend(buffer, "%9d", 9,     ar->element->arid);
   } else {
   	LngAppend(buffer, "%8d", 8,     ar->element->arid);
   }

   LngAppend(buffer, "%8d", 8,     ar->element->jdate);

   if( ver == 39 ) {
   	LngAppend(buffer, "%9d", 9,     ar->element->stassid);
   } else {
   	LngAppend(buffer, "%8d", 8,     ar->element->stassid);
   }

   LngAppend(buffer, "%8d", 8,     ar->element->chanid);
   StrAppend(buffer, "%-8s", 8,    ar->element->chan);
   StrAppend(buffer, "%-8s", 8,    ar->element->iphase);
   StrAppend(buffer, "%-1s", 1,    ar->element->stype);
   DblAppend(buffer, "%6.3f", 6,   ar->element->deltim);
   DblAppend(buffer, "%7.2f", 7,   ar->element->delaz);
   DblAppend(buffer, "%7.2f", 7,   ar->element->azimuth);
   DblAppend(buffer, "%7.2f", 7,   ar->element->slow);
   DblAppend(buffer, "%7.2f", 7,   ar->element->delslo);
   DblAppend(buffer, "%7.2f", 7,   ar->element->ema);
   DblAppend(buffer, "%7.3f", 7,   ar->element->rect);

   if( ver == 39 ) {
   	DblAppend(buffer, "%11.2f", 11, ar->element->amp);
   } else {
   	DblAppend(buffer, "%10.1f", 10, ar->element->amp);
   }

   DblAppend(buffer, "%7.2f", 7,   ar->element->per);
   DblAppend(buffer, "%7.2f", 7,   ar->element->logat);
   StrAppend(buffer, "%-1s", 1,    ar->element->clip);
   StrAppend(buffer, "%-2s", 2,    ar->element->fm);
   DblAppend(buffer, "%10.2f", 10, ar->element->snr);
   StrAppend(buffer, "%-1s", 1,    ar->element->qual);
   StrAppend(buffer, "%-15s", 15,  ar->element->auth);

   if( ver == 39 ) {
   	LngAppend(buffer, "%9d", 9,     ar->element->commid);
   } else {
   	LngAppend(buffer, "%8d", 8,     ar->element->commid);
   }

   StrAppend(buffer, "%-17s", 17,  ar->element->lddate);

   if( fprintf(ptr, "%s\n", buffer) == EOF )return 0;

   return 1;    
}
/* ---------------------------------------------------------------- */




static int WriteAssocRecord(struct assocList *as, FILE* ptr, int ver)
{
   char buffer[180];

   if(!CSSlngDefined(as->element->arid) ||!CSSlngDefined(as->element->orid) ){
         printf("Skipping assoc record because of missing key...\n");
         return 1;
   }

   *buffer = '\0';
   if( ver == 39 ) {
   	LngAppend(buffer, "%9d", 9,     as->element->arid);
   	LngAppend(buffer, "%9d", 9,     as->element->orid);
   } else {
	LngAppend(buffer, "%8d", 8,     as->element->arid);
   	LngAppend(buffer, "%8d", 8,     as->element->orid);
   }

   StrAppend(buffer, "%-6s", 6,    as->element->sta);
   StrAppend(buffer, "%-8s", 8,    as->element->phase);
   
   if( ver == 39 ) {
   	DblAppend(buffer, "%4.1f", 4,   as->element->belief);
   } else {
	/* The 3.0 specification has a bug. If belief is undefined its
	   value is -1.0. However this value written using format %4.2f
	   will be -1.00 which does not fit in 4 columns.
	*/
	   if(as->element->belief > 0)
	      	DblAppend(buffer, "%4.2f", 4,   as->element->belief);
	   else
	      	DblAppend(buffer, "%4.1f", 4,   as->element->belief);
   }

   DblAppend(buffer, "%8.3f", 8,   as->element->delta);
   DblAppend(buffer, "%7.2f", 7,   as->element->seaz);
   DblAppend(buffer, "%7.2f", 7,   as->element->esaz);
   DblAppend(buffer, "%8.3f", 8,   as->element->timeres);
   StrAppend(buffer, "%-1s", 1,    as->element->timedef);
   DblAppend(buffer, "%7.1f", 7,   as->element->azres);
   StrAppend(buffer, "%-1s", 1,    as->element->azdef);
   DblAppend(buffer, "%7.2f", 7,   as->element->slores);
   StrAppend(buffer, "%-1s", 1,    as->element->slodef);
   DblAppend(buffer, "%7.1f", 7,   as->element->emares);
   DblAppend(buffer, "%6.3f", 6,   as->element->wgt);
   StrAppend(buffer, "%-15s", 15,  as->element->vmodel);

   if( ver == 39 ) {
   	LngAppend(buffer, "%9d", 9,     as->element->commid);
   } else {
   	LngAppend(buffer, "%8d", 8,     as->element->commid);
   }

   StrAppend(buffer, "%-17s", 17,  as->element->lddate);

   if( fprintf(ptr, "%s\n", buffer) == EOF )return 0;


   return 1;
}
/* ---------------------------------------------------------------- */





static int WriteEventRecord(struct eventList *ev, FILE* ptr, int ver)
{
   char buffer[100];

   if(!CSSlngDefined(ev->element->evid) ){
         printf("Skipping event record because of missing key...\n");
         return 1;
   }

   *buffer = '\0';
   if( ver == 39 ) {
   	LngAppend(buffer, "%9d", 9,     ev->element->evid);
   	StrAppend(buffer, "%-32s", 32,  ev->element->evname);
   	LngAppend(buffer, "%9d", 9,     ev->element->prefor);
   } else {
   	LngAppend(buffer, "%8d", 8,     ev->element->evid);
   	StrAppend(buffer, "%-15s", 15,  ev->element->evname);
   	LngAppend(buffer, "%8d", 8,     ev->element->prefor);
   }

   StrAppend(buffer, "%-15s", 15,  ev->element->auth);
 
   if( ver == 39 ) {
   	LngAppend(buffer, "%9d", 9,     ev->element->commid);
   } else {
   	LngAppend(buffer, "%8d", 8,     ev->element->commid);
   }
   
   StrAppend(buffer, "%-17s", 17,  ev->element->lddate);

   if( fprintf(ptr, "%s\n", buffer) == EOF )return 0;

   return 1;
}
/* ---------------------------------------------------------------- */




static int WriteGregionRecord(struct gregionList *gr, FILE* ptr, int ver)
{
   char buffer[80];

   *buffer = '\0';
   LngAppend(buffer, "%8d", 8,     gr->element->grn);
   StrAppend(buffer, "%-40s", 40,  gr->element->grname);
   StrAppend(buffer, "%-17s", 17,  gr->element->lddate);

   if( fprintf(ptr, "%s\n", buffer) == EOF )return 0;

   return 1;
}
/* ---------------------------------------------------------------- */




static int WriteInstrumentRecord(struct instrumentList *in, FILE* ptr, int ver)
{
   char buffer[250];

   if(!CSSlngDefined(in->element->inid) ){
         printf("Skipping instrument record because of missing key...\n");
         return 1;
   }

   *buffer = '\0';
   LngAppend(buffer, "%8d", 8,     in->element->inid);
   StrAppend(buffer, "%-50s", 50,  in->element->insname);
   StrAppend(buffer, "%-6s", 6,    in->element->instype);
   StrAppend(buffer, "%-1s", 1,    in->element->band);
   StrAppend(buffer, "%-1s", 1,    in->element->digital);
   DblAppend(buffer, "%11.7f", 11, in->element->samprate);
   DblAppend(buffer, "%16.6f", 16, in->element->ncalib);
   DblAppend(buffer, "%16.6f", 16, in->element->ncalper);
   StrAppend(buffer, "%-64s", 64,  in->element->dir);
   StrAppend(buffer, "%-32s", 32,  in->element->dfile);
   StrAppend(buffer, "%-6s", 6,    in->element->rsptype);
   StrAppend(buffer, "%-17s", 17,  in->element->lddate);


   if( fprintf(ptr, "%s\n", buffer) == EOF )return 0;

   return 1;
}
/* ---------------------------------------------------------------- */





static int WriteOrigerrRecord(struct origerrList *oe, FILE* ptr, int ver)
{
   char buffer[260];

   if(!CSSlngDefined(oe->element->orid) ){
         printf("Skipping origerr record because of missing key...\n");
         return 1;
   }


   *buffer = '\0';
   if( ver == 39 ) {
   	LngAppend(buffer, "%9d", 9,     oe->element->orid);
   } else {
   	LngAppend(buffer, "%8d", 8,     oe->element->orid);
   }
   
   DblAppend(buffer, "%15.4f", 15, oe->element->sxx);
   DblAppend(buffer, "%15.4f", 15, oe->element->syy);
   DblAppend(buffer, "%15.4f", 15, oe->element->szz);
   DblAppend(buffer, "%15.4f", 15, oe->element->stt);
   DblAppend(buffer, "%15.4f", 15, oe->element->sxy);
   DblAppend(buffer, "%15.4f", 15, oe->element->sxz);
   DblAppend(buffer, "%15.4f", 15, oe->element->syz);
   DblAppend(buffer, "%15.4f", 15, oe->element->stx);
   DblAppend(buffer, "%15.4f", 15, oe->element->sty);
   DblAppend(buffer, "%15.4f", 15, oe->element->stz);
   DblAppend(buffer, "%9.4f", 9,   oe->element->sdobs);
   DblAppend(buffer, "%9.4f", 9,   oe->element->smajax);
   DblAppend(buffer, "%9.4f", 9,   oe->element->sminax);
   DblAppend(buffer, "%6.2f", 6,   oe->element->strike);
   DblAppend(buffer, "%9.4f", 9,   oe->element->sdepth);

   if( ver == 39 ) {
   	DblAppend(buffer, "%6.3f", 6,   oe->element->stime);
   } else {
   	DblAppend(buffer, "%8.2f", 8,   oe->element->stime);
   }

   DblAppend(buffer, "%5.3f", 5,   oe->element->conf);

   if( ver == 39 ) {
   	LngAppend(buffer, "%9d", 9,     oe->element->commid);
   } else {
   	LngAppend(buffer, "%8d", 8,     oe->element->commid);
   }

   StrAppend(buffer, "%-17s", 17,  oe->element->lddate);


   if( fprintf(ptr, "%s\n", buffer) == EOF )return 0;

   return 1;
}
/* ---------------------------------------------------------------- */





static int WriteOriginRecord(struct originList *orig, FILE* ptr, int ver)
{
   char buffer[250];

   if(!CSSdblDefined(orig->element->lat) || !CSSdblDefined(orig->element->lon) ||
      !CSSdblDefined(orig->element->depth) || !CSSdblDefined(orig->element->time) )
     if(!CSSlngDefined(orig->element->orid) ){
         printf("Skipping origin record because of missing key...\n");
         return 1;
     }

   *buffer = '\0';

   if( ver == 39 ) {
   	DblAppend(buffer, "%11.6f", 11,   orig->element->lat);
   	DblAppend(buffer, "%11.6f", 11,   orig->element->lon);
   } else {
   	DblAppend(buffer, "%9.4f", 9,   orig->element->lat);
   	DblAppend(buffer, "%9.4f", 9,   orig->element->lon);
   }

   DblAppend(buffer, "%9.4f", 9,   orig->element->depth);
   DblAppend(buffer, "%17.5f", 17, orig->element->time);
   
   if( ver == 39 ) {
   	LngAppend(buffer, "%9d", 9,     orig->element->orid);
   	LngAppend(buffer, "%9d", 9,     orig->element->evid);
   } else {
   	LngAppend(buffer, "%8d", 8,     orig->element->orid);
   	LngAppend(buffer, "%8d", 8,     orig->element->evid);
   }

   LngAppend(buffer, "%8d", 8,     orig->element->jdate);
   LngAppend(buffer, "%4d", 4,     orig->element->nass);
   LngAppend(buffer, "%4d", 4,     orig->element->ndef);
   LngAppend(buffer, "%4d", 4,     orig->element->ndp);
   LngAppend(buffer, "%8d", 8,     orig->element->grn);
   LngAppend(buffer, "%8d", 8,     orig->element->srn);
   StrAppend(buffer, "%-7s", 7,    orig->element->etype);
   DblAppend(buffer, "%9.4f", 9,   orig->element->depdp);
   StrAppend(buffer, "%-1s", 1,    orig->element->dtype);
   DblAppend(buffer, "%7.2f", 7,   orig->element->mb);

   if( ver == 39 ) {
   	LngAppend(buffer, "%9d", 9,     orig->element->mbid);
   } else {
   	LngAppend(buffer, "%8d", 8,     orig->element->mbid);
   }

   DblAppend(buffer, "%7.2f", 7,   orig->element->ms);

   if( ver == 39 ) {
   	LngAppend(buffer, "%9d", 9,     orig->element->msid);
   } else {
   	LngAppend(buffer, "%8d", 8,     orig->element->msid);
   }

   DblAppend(buffer, "%7.2f", 7,   orig->element->ml);

   if( ver == 39 ) {
   	LngAppend(buffer, "%9d", 9,     orig->element->mlid);
  } else {
   	 LngAppend(buffer, "%8d", 8,     orig->element->mlid);
   }

   StrAppend(buffer, "%-15s", 15,  orig->element->algorithm);
   StrAppend(buffer, "%-15s", 15,  orig->element->auth);

   if( ver == 39 ) {
   	LngAppend(buffer, "%9d", 9,     orig->element->commid);
   } else {
   	LngAppend(buffer, "%8d", 8,     orig->element->commid);
   }

   StrAppend(buffer, "%-17s", 17,  orig->element->lddate);


   if( fprintf(ptr, "%s\n", buffer) == EOF )return 0;

   return 1;
}
/* ---------------------------------------------------------------- */






static int WriteRemarkRecord(struct remarkList *re, FILE* ptr, int ver)
{
   char buffer[120];

   *buffer = '\0';

   if( ver == 39 ) {
   	LngAppend(buffer, "%9d", 9,     re->element->commid);
   } else {
   	LngAppend(buffer, "%8d", 8,     re->element->commid);
   }

   LngAppend(buffer, "%8d", 8,     re->element->lineno);
   StrAppend(buffer, "%-80s", 80,  re->element->remark);
   StrAppend(buffer, "%-17s", 17,  re->element->lddate);
   if( fprintf(ptr, "%s\n", buffer) == EOF )return 0;


   return 1;
}
/* ---------------------------------------------------------------- */





static int WriteSensorRecord(struct sensorList *se, FILE* ptr, int ver)
{
   char buffer[160];

   if(!CSSdblDefined(se->element->time) || !CSSchrDefined(se->element->sta) ||
      !CSSchrDefined(se->element->chan) ){
         printf("Skipping sensor record because of missing key...\n");
         return 1;
     }

   *buffer = '\0';
   StrAppend(buffer, "%-6s", 6,    se->element->sta);
   StrAppend(buffer, "%-8s", 8,    se->element->chan);
   DblAppend(buffer, "%17.5f", 17, se->element->time);
   DblAppend(buffer, "%17.5f", 17, se->element->endtime);
   LngAppend(buffer, "%8d", 8,     se->element->inid);
   LngAppend(buffer, "%8d", 8,     se->element->chanid);
   LngAppend(buffer, "%8d", 8,     se->element->jdate);
   DblAppend(buffer, "%16.6f", 16, se->element->calratio);
   DblAppend(buffer, "%16.6f", 16, se->element->calper);

   if( ver == 39 ) {
   	DblAppend(buffer, "%16.2f", 16,   se->element->tshift);
   } else {
   	DblAppend(buffer, "%6.2f", 6,   se->element->tshift);
   }

   StrAppend(buffer, "%-1s", 1,    se->element->instant);
   StrAppend(buffer, "%-17s", 17,  se->element->lddate);


   if( fprintf(ptr, "%s\n", buffer) == EOF )return 0;


   return 1;
}
/* ---------------------------------------------------------------- */





static int WriteSiteRecord(struct siteList *si, FILE* ptr, int ver)
{
   char buffer[160];

   if(!CSSchrDefined(si->element->sta) || !CSSlngDefined(si->element->ondate) ){
      printf("Skipping site record because of missing key...\n");
      return 1;
   }

   *buffer = '\0';
   StrAppend(buffer, "%-6s", 6,    si->element->sta);
   LngAppend(buffer, "%8d", 8,     si->element->ondate);
   LngAppend(buffer, "%8d", 8,     si->element->offdate);

   if( ver == 39 ) {
   	DblAppend(buffer, "%11.6f", 11,   si->element->lat);
   	DblAppend(buffer, "%11.6f", 11,   si->element->lon);
   } else {
   	DblAppend(buffer, "%9.4f", 9,   si->element->lat);
   	DblAppend(buffer, "%9.4f", 9,   si->element->lon);
   }

   DblAppend(buffer, "%9.4f", 9,   si->element->elev);
   StrAppend(buffer, "%-50s", 50,  si->element->staname);
   StrAppend(buffer, "%-4s", 4,    si->element->statype);
   StrAppend(buffer, "%-6s", 6,    si->element->refsta);
   DblAppend(buffer, "%9.4f", 9,   si->element->dnorth);
   DblAppend(buffer, "%9.4f", 9,   si->element->deast);
   StrAppend(buffer, "%-17s", 17,  si->element->lddate);


   if( fprintf(ptr, "%s\n", buffer) == EOF )return 0;

   return 1;
}
/* ---------------------------------------------------------------- */





static int WriteSitechanRecord(struct sitechanList *sc, FILE* ptr, int ver)
{
   char buffer[150];

   if(!CSSchrDefined(sc->element->sta) || !CSSchrDefined(sc->element->chan) ||
      !CSSlngDefined(sc->element->ondate) ){
      printf("Skipping sitechan record because of missing key...\n");
      return 1;
   }

   *buffer = '\0';
   StrAppend(buffer, "%-6s", 6,    sc->element->sta);
   StrAppend(buffer, "%-8s", 8,    sc->element->chan);
   LngAppend(buffer, "%8d", 8,     sc->element->ondate);
   LngAppend(buffer, "%8d", 8,     sc->element->chanid);
   LngAppend(buffer, "%8d", 8,     sc->element->offdate);
   StrAppend(buffer, "%-4s", 4,    sc->element->ctype);
   DblAppend(buffer, "%9.4f", 9,   sc->element->edepth);
   DblAppend(buffer, "%6.1f", 6,   sc->element->hang);
   DblAppend(buffer, "%6.1f", 6,   sc->element->vang);
   StrAppend(buffer, "%-50s", 50,  sc->element->descrip);
   StrAppend(buffer, "%-17s", 17,  sc->element->lddate);


   if( fprintf(ptr, "%s\n", buffer) == EOF )return 0;

   return 1;
}
/* ---------------------------------------------------------------- */





static int WriteStassocRecord(struct stassocList *sa, FILE* ptr, int ver)
{
   char format [] = "      "
                    "   %8d \n";
   char buffer[200];


   *buffer = '\0';
   LngAppend(buffer, "%8d", 8,     sa->element->stassid);
   StrAppend(buffer, "%-6s", 6,    sa->element->sta);
   StrAppend(buffer, "%-7s", 7,    sa->element->etype);
   StrAppend(buffer, "%-32s", 32,  sa->element->location);
   DblAppend(buffer, "%7.2f", 7,   sa->element->dist);
   DblAppend(buffer, "%7.2f", 7,   sa->element->azimuth);
   DblAppend(buffer, "%9.4f", 9,   sa->element->lat);
   DblAppend(buffer, "%9.4f", 9,   sa->element->lon);
   DblAppend(buffer, "%9.4f", 9,   sa->element->depth);
   DblAppend(buffer, "17.5f", 17,  sa->element->time);
   DblAppend(buffer, "%7.2f", 7,   sa->element->imb);
   DblAppend(buffer, "%7.2f", 7,   sa->element->ims);
   DblAppend(buffer, "%7.2f", 7,   sa->element->iml);
   StrAppend(buffer, "%-15s", 15,  sa->element->auth);
   LngAppend(buffer, "%8d", 8,     sa->element->commid);
   StrAppend(buffer, "%-17s", 17,  sa->element->lddate);


   if( fprintf(ptr, "%s\n", buffer) == EOF )return 0;

   return 1;
}


/* ---------------------- NEW NOW SUPPORTS S4/I4  ------------------------- */
static int WriteWfdiscRecord(const char *basename, struct wfdiscList *w, FILE* ptr, int ver, const char *type )
{
   char datafile[100];
   char tmp[11];
   FILE* fptr;
   int ptsOut;
   char buffer[400];
   char *slashPtr;
   char tType[5];
   char nType[5];
   int swap=0;

   strcpy(nType, type);
   if(!CSSchrDefined(w->element->sta) || !CSSchrDefined(w->element->chan) ||
      !CSSdblDefined(w->element->time) )
      if(!CSSlngDefined(w->element->wfid) ){
         printf("Skipping wfdisc record because of missing key...\n");
         return 1;
      }


   sprintf(datafile, "%s%s%08d.w", basename, w->element->sta, w->element->wfid);


   slashPtr = strrchr(datafile, '/');
   if( slashPtr ){
      *slashPtr = '\0';
      strcpy(w->element->dfile, slashPtr + 1);
      strcpy(w->element->dir, datafile );
      *slashPtr = '/';
   }
   else{
      strcpy(w->element->dir, ".");
      strcpy(w->element->dfile, datafile);
   }

   if(w->seis->i && w->seis->r)
      strcpy(tType, "ri");
   else{
      if( IsBigendin() )
         strcpy(tType, "t4");
      else
         strcpy(tType, "f4" );
   }
   /*
   If type = s4 and on an little endian machine, swap bytes
   If type = i4 and on an big endian machine, swap bytes.
   
   */
 

  if (strcmp(nType,"s4") == 0 ){
         if( !IsBigendin() ) swap=1; 
         strcpy(tType, type);
   }
   else if (strcmp(tType,"i4") == 0){
   	 if( IsBigendin() ) swap=1;
   	 strcpy(tType, type);
   }

   strcpy(w->element->dattype, tType );

   w->element->foff = 0;

   *buffer = '\0';
   StrAppend(buffer, "%-6s", 6,    w->element->sta);
   StrAppend(buffer, "%-8s", 8,    w->element->chan);
   DblAppend(buffer, "%17.5f", 17, w->element->time);

   if( ver == 39 ) {
   	LngAppend(buffer, "%9d", 9,     w->element->wfid);
   } else {
   	LngAppend(buffer, "%8d", 8,     w->element->wfid);
   }

   LngAppend(buffer, "%8d", 8,     w->element->chanid);
   LngAppend(buffer, "%8d", 8,     w->element->jdate);
   DblAppend(buffer, "%17.5f", 17, w->element->endtime);
   LngAppend(buffer, "%8d", 8,     w->element->nsamp);
   DblAppend(buffer, "%11.7f", 11, w->element->samprate);
   DblAppend(buffer, "%16.6f", 16, w->element->calib);
   DblAppend(buffer, "%16.6f", 16, w->element->calper);
   StrAppend(buffer, "%-6s", 6,    w->element->instype);
   StrAppend(buffer, "%-1s", 1,    w->element->segtype);
   StrAppend(buffer, "%-2s", 2,    w->element->dattype);
   StrAppend(buffer, "%-1s", 1,    w->element->clip);
   StrAppend(buffer, "%-64s", 64,  w->element->dir);
   StrAppend(buffer, "%-32s", 32,  w->element->dfile);
   LngAppend(buffer, "%10d", 10,   w->element->foff);

   if( ver == 39 ) {
   	LngAppend(buffer, "%9d", 9,     w->element->commid);
   } else {
   	LngAppend(buffer, "%8d", 8,     w->element->commid);
   }

   StrAppend(buffer, "%-17s", 17,  w->element->lddate);

   buffer[ 283 ] = '\0' ;

   if( fprintf(ptr, "%s\n", buffer) == EOF )return 0;

   if(!w->seis->i){
      printf("No data associated with wfdisc struct.\n");
      return 1;
   }

   if( !(fptr = fopen(datafile,"wb")) ){
      printf("Error: Could not open (%s).\n", datafile);
      return 0;
   }

   if (strcmp(tType,"s4") == 0 || strcmp(tType,"i4") == 0) {
	int* s4array;
        int is4;
        
        float* tmp = w->seis->r ? w->seis->r : w->seis->i;
   	s4array = (int*) malloc( sizeof(int) * w->element->nsamp) ;
        for (is4 = 0; is4 < w->element->nsamp; is4++){
           s4array[is4] = (int)(tmp[is4]); 
        /*
        ** Swap bytes if needed.
        */
           if ( swap )
              byteswap ( &s4array[is4], sizeof(int) );
        }	
	
	ptsOut = fwrite(s4array, sizeof(int), w->element->nsamp, fptr);
        free(s4array);
	if(ptsOut != w->element->nsamp){
	   printf("Error writing (%s).\n", datafile);
	   return 0;
	}
        
        
    } 
    else
    {
	   if(w->seis->r){
	      ptsOut = fwrite(w->seis->r, sizeof(float), w->element->nsamp, fptr);
	      if(ptsOut != w->element->nsamp){
		 printf("Error writing (%s).\n", datafile);
                 fclose(fptr);
		 return 0;
	      }
	   }
	   ptsOut = fwrite(w->seis->i, sizeof(float), w->element->nsamp, fptr);
	   if(ptsOut != w->element->nsamp){
	      printf("Error writing (%s).\n", datafile);
	      fclose(fptr);
	      return 0;
	   }
   }
   fclose(fptr);

   return 1;
}
/* ---------------------------------------------------------------- */


/* ---------------------------------------------------------------- */



static int WriteWftagRecord(struct wftagList *wt, FILE* ptr, int ver)
{
   char buffer[70];


   *buffer = '\0';

   StrAppend(buffer, "%-8s",   8, wt->element->tagname);

   if( ver == 39 ) {
   	LngAppend(buffer, "%9d",    9, wt->element->tagid);
	LngAppend(buffer, "%9d",    9, wt->element->wfid);
   } else {
   	LngAppend(buffer, "%8d",    8, wt->element->tagid);
   	LngAppend(buffer, "%8d",    8, wt->element->wfid);
   }

   StrAppend(buffer, "%-17s", 17, wt->element->lddate);

   if( fprintf(ptr, "%s\n", buffer) == EOF )return 0;

   return 1;
}
/* ---------------------------------------------------------------- */








static int WriteSacdataRecord(struct sacdataList *sd, FILE* ptr, int ver)
{
   char buffer[400];


   *buffer = '\0';
   StrAppend(buffer, "%-8s",8,     sd->element->userdata.label[0]);
   LngAppend(buffer, "%8d", 8,     sd->element->userdata.value[0]);
   StrAppend(buffer, "%-8s",8,     sd->element->userdata.label[1]);
   LngAppend(buffer, "%8d", 8,     sd->element->userdata.value[1]);
   StrAppend(buffer, "%-8s",8,     sd->element->userdata.label[2]);
   LngAppend(buffer, "%8d", 8,     sd->element->userdata.value[2]);
   LngAppend(buffer, "%8d", 8,     sd->element->userdata.value[3]);
   LngAppend(buffer, "%8d", 8,     sd->element->userdata.value[4]);
   LngAppend(buffer, "%8d", 8,     sd->element->userdata.value[5]);
   LngAppend(buffer, "%8d", 8,     sd->element->userdata.value[6]);
   LngAppend(buffer, "%8d", 8,     sd->element->userdata.value[7]);
   LngAppend(buffer, "%8d", 8,     sd->element->userdata.value[8]);
   LngAppend(buffer, "%8d", 8,     sd->element->userdata.value[9]);
   LngAppend(buffer, "%8d", 8,     sd->element->synthflag);
   LngAppend(buffer, "%8d", 8,     sd->element->lpspol);
   LngAppend(buffer, "%8d", 8,     sd->element->iztype);
   LngAppend(buffer, "%8d", 8,     sd->element->idep);
   LngAppend(buffer, "%8d", 8,     sd->element->iftype);
      if( ver == 39 ) {
   	   LngAppend(buffer, "%9d", 9,     sd->element->wfid);
   }else {
   	   LngAppend(buffer, "%8d", 8,     sd->element->wfid);
   }
   LngAppend(buffer, "%8d", 8,     sd->element->nsnpts);
   LngAppend(buffer, "%8d", 8,     sd->element->nxsize);
   LngAppend(buffer, "%8d", 8,     sd->element->nysize);
   LngAppend(buffer, "%8d", 8,     sd->element->leven);
   DblAppend(buffer, "%16.6f", 16, sd->element->fmt);
   DblAppend(buffer, "%16.6f", 16, sd->element->sb);
   DblAppend(buffer, "%16.6f", 16, sd->element->sdelta);
   DblAppend(buffer, "%16.6f", 16, sd->element->xminimum);
   DblAppend(buffer, "%16.6f", 16, sd->element->xmaximum);
   DblAppend(buffer, "%16.6f", 16, sd->element->yminimum);
   DblAppend(buffer, "%16.6f", 16, sd->element->ymaximum);


   if( fprintf(ptr, "%s\n", buffer) == EOF )return 0;

   return 1;
}
/* ---------------------------------------------------------------- */








static int HasAffiliationStructs(DBlist tree )
{
   struct affiliationList *af = 0;
   af = (struct affiliationList *) dblNextTableInstance(af, tree, dbl_LIST_AFFILIATION);
   if(af) return 1;
   return 0;
}
/* ---------------------------------------------------------------- */




static int HasArrivalStructs(DBlist tree )
{
   struct arrivalList *ar = 0;
   ar = (struct arrivalList *) dblNextTableInstance(ar, tree, dbl_LIST_ARRIVAL);
   if(ar) return 1;
   return 0;
}
/* ---------------------------------------------------------------- */



static int HasAssocStructs(DBlist tree )
{
   struct assocList *as = 0;
   as = (struct assocList *) dblNextTableInstance(as, tree, dbl_LIST_ASSOC);
   if(as) return 1;
   return 0;
}
/* ---------------------------------------------------------------- */



static int HasEventStructs(DBlist tree )
{
   struct eventList *ev = 0;
   ev = (struct eventList *) dblNextTableInstance(ev, tree, dbl_LIST_EVENT);
   if(ev) return 1;
   return 0;
}
/* ---------------------------------------------------------------- */



static int HasGregionStructs(DBlist tree )
{
   struct gregionList *gr = 0;
   gr = (struct gregionList *)dblNextTableInstance(gr, tree, dbl_LIST_GREGION);
   if(gr) return 1;
   return 0;
}
/* ---------------------------------------------------------------- */



static int HasInstrumentStructs(DBlist tree )
{
   struct instrumentList *in = 0;
   in = (struct instrumentList *) dblNextTableInstance(in, tree, dbl_LIST_INSTRUMENT);
   if(in) return 1;
   return 0;
}
/* ---------------------------------------------------------------- */

   




static int HasOrigerrStructs(DBlist tree )
{
   struct origerrList *oe = 0;
   oe = (struct origerrList *) dblNextTableInstance(oe, tree, dbl_LIST_ORIGERR);
   if(oe) return 1;
   return 0;
}
/* ---------------------------------------------------------------- */




static int HasOriginStructs(DBlist tree )
{
   struct originList *orig = 0;
   orig = (struct originList *) dblNextTableInstance(orig, tree, dbl_LIST_ORIGIN);
   if(orig) return 1;
   return 0;
}
/* ---------------------------------------------------------------- */




static int HasRemarkStructs(DBlist tree )
{
   struct remarkList *re = 0;
   re = (struct remarkList *) dblNextTableInstance(re, tree, dbl_LIST_REMARK);
   if(re) return 1;
   return 0;
}
/* ---------------------------------------------------------------- */




static int HasSensorStructs(DBlist tree )
{
   struct sensorList *se = 0;
   se = (struct sensorList *) dblNextTableInstance(se, tree, dbl_LIST_SENSOR);
   if(se) return 1;
   return 0;
}
/* ---------------------------------------------------------------- */




static int HasSiteStructs(DBlist tree )
{
   struct siteList *si = 0;
   si = (struct siteList *) dblNextTableInstance(si, tree, dbl_LIST_SITE);
   if(si) return 1;
   return 0;
}
/* ---------------------------------------------------------------- */




static int HasSitechanStructs(DBlist tree )
{
   struct sitechanList *sc = 0;
   sc = (struct sitechanList *) dblNextTableInstance(sc, tree, dbl_LIST_SITECHAN);
   if(sc) return 1;
   return 0;
}
/* ---------------------------------------------------------------- */





static int HasStassocStructs(DBlist tree )
{
   struct stassocList *sa = 0;
   sa = (struct stassocList *) dblNextTableInstance(sa, tree, dbl_LIST_STASSOC);
   if(sa) return 1;
   return 0;
}
/* ---------------------------------------------------------------- */





static int HasWfdiscStructs(DBlist tree )
{
   struct wfdiscList *w = 0;
   w = (struct wfdiscList *) dblNextTableInstance(w, tree, dbl_LIST_WFDISC);
   if(w) return 1;
   return 0;
}
/* ---------------------------------------------------------------- */




static int HasWftagStructs(DBlist tree )
{
   struct wftagList *w = 0;
   w = (struct wftagList *) dblNextTableInstance(w, tree, dbl_LIST_WFTAG);
   if(w) return 1;
   return 0;
}
/* ---------------------------------------------------------------- */




static int HasSacdataStructs(DBlist tree )
{
   struct sacdataList *sd = 0;
   sd = (struct sacdataList *) dblNextTableInstance(sd, tree, dbl_LIST_SACDATA);
   if(sd) return 1;
   return 0;
}
/* ---------------------------------------------------------------- */




int WriteCSSflatFiles(const char *WorkSetName, const char *basename, int Version, const char *tType)
{
   FILE *ptr;
   struct affiliationList *af = 0;
   struct arrivalList *ar     = 0;
   struct assocList *as       = 0;
   struct eventList *ev       = 0;
   struct gregionList *gr     = 0;
   struct instrumentList *in  = 0;
   struct origerrList *oe     = 0;
   struct originList *orig      = 0;
   struct remarkList *re      = 0;
   struct sensorList *se      = 0;
   struct siteList *si        = 0;
   struct sitechanList *sc    = 0;
   struct stassocList *sa     = 0;
   struct wfdiscList *w       = 0;
   struct wftagList *wt       = 0;
   struct sacdataList *sd     = 0;
   DBlist tree;
   char*  FileName;
   

   if(!basename || ! strlen(basename)){
      printf("Invalid basename!\n");
      return 0;
   }


   if(!WorkSetName || !strlen(WorkSetName) ){
      printf("Invalid worksetname! Cannot write CSS binary data to file.\n");
      return 0;
   }

   if(!smChangeDefaultWorksetByName( (char*)WorkSetName )){
      printf("Workset %s is empty orig does not exist!\n", WorkSetName);
      return 0;
   }
   tree = smGetDefaultTree();
   if(!tree)return 0;

   gcCollect ( tree ) ;

   if(HasAffiliationStructs( tree ) ){
      FileName = MakeFileName( basename, "affiliation");
      if(! (ptr = fopen( FileName, "w" ) ) ){
         printf("ERROR: Unable to open output file (%s)\n",FileName);
         return 0;
      }
   
      do{
         if(!(af = (struct affiliationList*) dblNextTableInstance(af, tree, dbl_LIST_AFFILIATION) ) ) break;
         if(!WriteAffiliationRecord(af, ptr, Version) ){
            fclose(ptr);
            free(FileName);
            return 0;
         }
      }while(af);
      fclose(ptr);
      free(FileName);
   }



   if(HasArrivalStructs( tree ) ){
      FileName = MakeFileName( basename, "arrival");
      if(! (ptr = fopen( FileName, "w" ) ) ){
         printf("ERROR: Unable to open output file (%s)\n",FileName);
         return 0;
      }
   
      do{
         if(!(ar = (struct arrivalList*) dblNextTableInstance(ar, tree, dbl_LIST_ARRIVAL) ) ) break;
         if(!WriteArrivalRecord(ar, ptr, Version) ){
            fclose(ptr);
            free(FileName);
            return 0;
         }
      }while(ar);
      fclose(ptr);
      free(FileName);
   }



   if(HasAssocStructs( tree ) ){
      FileName = MakeFileName( basename, "assoc");
      if(! (ptr = fopen( FileName, "w" ) ) ){
         printf("ERROR: Unable to open output file (%s)\n",FileName);
         return 0;
      }
   
      do{
         if(!(as = (struct assocList*) dblNextTableInstance(as, tree, dbl_LIST_ASSOC) ) ) break;
         if(!WriteAssocRecord(as, ptr, Version) ){
            fclose(ptr);
            free(FileName);
            return 0;
         }
      }while(as);
      fclose(ptr);
      free(FileName);
   }


   if(HasEventStructs( tree ) ){
      FileName = MakeFileName( basename, "event");
      if(! (ptr = fopen( FileName, "w" ) ) ){
         printf("ERROR: Unable to open output file (%s)\n",FileName);
         return 0;
      }
   
      do{
         if(!(ev = (struct eventList*) dblNextTableInstance(ev, tree, dbl_LIST_EVENT) ) ) break;
         if(!WriteEventRecord(ev, ptr, Version) ){
            fclose(ptr);
            free(FileName);
            return 0;
         }
      }while(ev);
      fclose(ptr);
      free(FileName);
   }


   if(HasGregionStructs( tree ) ){
      FileName = MakeFileName( basename, "gregion");
      if(! (ptr = fopen( FileName, "w" ) ) ){
         printf("ERROR: Unable to open output file (%s)\n",FileName);
         return 0;
      }
   
      do{
         if(!(gr = (struct gregionList*) dblNextTableInstance(gr, tree, dbl_LIST_GREGION) ) ) break;
         if(!WriteGregionRecord(gr, ptr, Version) ){
            fclose(ptr);
            free(FileName);
            return 0;
         }
      }while(gr);
      fclose(ptr);
      free(FileName);
   }



   if(HasInstrumentStructs( tree ) ){
      FileName = MakeFileName( basename, "instrument");
      if(! (ptr = fopen( FileName, "w" ) ) ){
         printf("ERROR: Unable to open output file (%s)\n",FileName);
         return 0;
      }
   
      do{
         if(!(in = (struct instrumentList*) dblNextTableInstance(in, tree, dbl_LIST_INSTRUMENT) ) ) break;
         if(!WriteInstrumentRecord(in, ptr, Version) ){
            fclose(ptr);
            free(FileName);
            return 0;
         }
      }while(in);
      fclose(ptr);
      free(FileName);
   }



   if(HasOrigerrStructs( tree ) ){
      FileName = MakeFileName( basename, "origerr");
      if(! (ptr = fopen( FileName, "w" ) ) ){
         printf("ERROR: Unable to open output file (%s)\n",FileName);
         return 0;
      }
   
      do{
         if(!(oe = (struct origerrList*) dblNextTableInstance(oe, tree, dbl_LIST_ORIGERR) ) ) break;
         if(!WriteOrigerrRecord(oe, ptr, Version) ){
            fclose(ptr);
            free(FileName);
            return 0;
         }
      }while(oe);
      fclose(ptr);
      free(FileName);
   }



   if(HasOriginStructs( tree ) ){
      FileName = MakeFileName( basename, "origin");
      if(! (ptr = fopen( FileName, "w" ) ) ){
         printf("ERROR: Unable to open output file (%s)\n",FileName);
         return 0;
      }
   
      do{
         if(!(orig = (struct originList*) dblNextTableInstance(orig, tree, dbl_LIST_ORIGIN) ) ) break;
         if(!WriteOriginRecord(orig, ptr, Version) ){
            fclose(ptr);
            free(FileName);
            return 0;
         }
      }while(orig);
      fclose(ptr);
      free(FileName);
   }


   if(HasRemarkStructs( tree ) ){
      FileName = MakeFileName( basename, "remark");
      if(! (ptr = fopen( FileName, "w" ) ) ){
         printf("ERROR: Unable to open output file (%s)\n",FileName);
         return 0;
      }
   
      do{
         if(!(re = (struct remarkList*) dblNextTableInstance(re, tree, dbl_LIST_REMARK) ) ) break;
         if(!WriteRemarkRecord(re, ptr, Version) ){
            fclose(ptr);
            free(FileName);
            return 0;
         }
      }while(re);
      fclose(ptr);
      free(FileName);
  }



   if(HasSensorStructs( tree ) ){
      FileName = MakeFileName( basename, "sensor");
      if(! (ptr = fopen( FileName, "w" ) ) ){
         printf("ERROR: Unable to open output file (%s)\n",FileName);
         return 0;
      }
   
      do{
         if(!(se = (struct sensorList*) dblNextTableInstance(se, tree, dbl_LIST_SENSOR) ) ) break;
         if(!WriteSensorRecord(se, ptr, Version) ){
            fclose(ptr);
            free(FileName);
            return 0;
         }
      }while(se);
      fclose(ptr);
      free(FileName);
   }


   if(HasSiteStructs( tree ) ){
      FileName = MakeFileName( basename, "site");
      if(! (ptr = fopen( FileName, "w" ) ) ){
         printf("ERROR: Unable to open output file (%s)\n",FileName);
         return 0;
      }
   
      do{
         if(!(si = (struct siteList*) dblNextTableInstance(si, tree, dbl_LIST_SITE) ) ) break;
         if(!WriteSiteRecord(si, ptr, Version) ){
            fclose(ptr);
            free(FileName);
            return 0;
         }
      }while(si);
      fclose(ptr);
      free(FileName);
   }



   if(HasSitechanStructs( tree ) ){
      FileName = MakeFileName( basename, "sitechan");
      if(! (ptr = fopen( FileName, "w" ) ) ){
         printf("ERROR: Unable to open output file (%s)\n",FileName);
         return 0;
      }
   
      do{
         if(!(sc = (struct sitechanList*) dblNextTableInstance(sc, tree, dbl_LIST_SITECHAN) ) ) break;
         if(!WriteSitechanRecord(sc, ptr, Version) ){
            fclose(ptr);
            free(FileName);
            return 0;
         }
      }while(sc);
      fclose(ptr);
      free(FileName);
   }



   if(HasStassocStructs( tree ) ){
      FileName = MakeFileName( basename, "stassoc");
      if(! (ptr = fopen( FileName, "w" ) ) ){
         printf("ERROR: Unable to open output file (%s)\n",FileName);
         return 0;
      }
   
      do{
         if(!(sa = (struct stassocList*) dblNextTableInstance(sa, tree, dbl_LIST_SITECHAN) ) ) break;
         if(!WriteStassocRecord(sa, ptr, Version) ){
            fclose(ptr);
            free(FileName);
            return 0;
         }
      }while(sa);
      fclose(ptr);
      free(FileName);
   }




   if(HasWfdiscStructs( tree ) ){
      FileName = MakeFileName( basename, "wfdisc");
      if(! (ptr = fopen( FileName, "w" ) ) ){
         printf("ERROR: Unable to open output file (%s)\n",FileName);
         return 0;
      }
   
      do{
         if(!(w = (struct wfdiscList*) dblNextTableInstance(w, tree, dbl_LIST_WFDISC) ) ) break;
         if(!WriteWfdiscRecord(basename, w, ptr, Version, tType) ){   /* , "s4"*/
            fclose(ptr);
            free(FileName);
            return 0;
         }
      }while(w);
      fclose(ptr);
      free(FileName);
   }



   if(HasWftagStructs( tree ) ){
      FileName = MakeFileName( basename, "wftag");
      if(! (ptr = fopen( FileName, "w" ) ) ){
         printf("ERROR: Unable to open output file (%s)\n",FileName);
         return 0;
      }
   
      do{
         if(!(wt = (struct wftagList*) dblNextTableInstance(wt, tree, dbl_LIST_WFTAG) ) ) break;
         if(!WriteWftagRecord(wt, ptr, Version) ){
            fclose(ptr);
            free(FileName);
            return 0;
         }
      }while(wt);
      fclose(ptr);
      free(FileName);
   }



   if(HasSacdataStructs( tree ) ){
      FileName = MakeFileName( basename, "sacdata");
      if(! (ptr = fopen( FileName, "w" ) ) ){
         printf("ERROR: Unable to open output file (%s)\n",FileName);
         return 0;
      }
   
      do{
         if(!(sd = (struct sacdataList*) dblNextTableInstance(sd, tree, dbl_LIST_SACDATA) ) ) break;
         if(!WriteSacdataRecord(sd, ptr, Version) ){
            fclose(ptr);
            free(FileName);
            return 0;
         }
      }while(sd);
      fclose(ptr);
      free(FileName);
   }


   return 1;
}
/* ------------------------------------------------------- */




