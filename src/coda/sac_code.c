#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "sac_header.h"


static struct sac_header sac_header;
struct sac_header *sh = &sac_header;
float *fhdrptr;
int *ihdrptr;
char *chdrptr;
int *lhdrptr;

#define   RAND_MAX        32767
static int iseed = 5268;

#define TRUE 1
#define FALSE 0

int indexb_(string)
     char *string;
{
  int length;

  length = strcspn(string," ");
  return(length);
}

void rand_(result,rand_max,rand_min)
     float *result;
     float *rand_max;
     float *rand_min;
{
  *result = 0.0;

  while ((*result >= *rand_max) || (*result <= *rand_min)){
    *result = *rand_max * (rand()/(RAND_MAX+1.0));
  }

  /*  fprintf(stderr,"RAND_MAX = %f result=%f \n",RAND_MAX*1.0,*result); */
}



newhdr_()
{
  initialize_header();
}

void wsac1_(char* outfile,float* data,int *npts, float* begin, float* delta,int* nerr)
{ 
  FILE *wfp;
  int i,status,length;
  int hdr_size=sizeof(struct sac_header);
 
  length = strcspn(outfile," ");
  //  outfile[length] = '\0';
  wfp = fopen(outfile,"w"); /* file to be generated */
  if(wfp == NULL) {
    printf("ERROR: opening file %s \n",outfile);
    exit(-1);
  }

  sh->NPTS = *npts;
  sh->DELTA = *delta;
  sh->B = *begin;

  sh->E = sh->NPTS * sh->DELTA;
  sh->LEVEN = TRUE;

  fwrite(sh,hdr_size,1,wfp); /* write the header */
      
  fwrite(data,sizeof(float),sh->NPTS,wfp); /* write data */
	
  fclose(wfp);
}

wsac0_(outfile,xdata,ydata,nerr)
     char *outfile;
     float  *xdata;
     float *ydata;
     int *nerr;
{ 
  FILE *wfp;
  int i,status,length;
  int hdr_size=sizeof(struct sac_header);
 
  length = strcspn(outfile," ");
  outfile[length] = '\0';
  wfp = fopen(outfile,"w"); /* file to be generated */
  if(wfp == NULL) {
    printf("ERROR: opening file %s \n",outfile);
    exit(-1);
  }

  sh->E = sh->NPTS * sh->DELTA;

  fwrite(sh,hdr_size,1,wfp); /* write the header */
      
  fwrite(ydata,sizeof(float),sh->NPTS,wfp); /* write y data */

  if (sh->LEVEN != TRUE) 
    fwrite(xdata,sizeof(float),sh->NPTS,wfp); /* write x data if necessary*/
	
  fclose(wfp);
}

void rsac1_(fname,data,np,beg,delta,maxpts,nerr)
char *fname;
float *data;
int *np; 
float *beg;
float *delta;
int *maxpts;
int *nerr;
{
  FILE *fp;  /* file to be read in */
  int i,status,length;

 initialize_header();

 length = strcspn(fname," ");
 fname[length] = '\0';
 fp = fopen(fname,"r");
 if(fp == NULL) printf("ERROR: opening file %s %d \n",fname,length);
   
 status = fread(sh,sizeof(struct sac_header),1,fp); /*read the header*/
 if (status != 1) printf("ERROR: Bad file header.\n");
 if(sh->NPTS > *maxpts) {
   printf("ERROR: Too many data points %d (%d) \n",sh->NPTS,maxpts);
   exit(1);
 }
  /* Read in data points, and load them into an array */
  status = fread(data,sizeof(float),sh->NPTS,fp);
  if (status != sh->NPTS) 
    printf("ERROR: Data read unsuccessful.\n");

  *np = sh->NPTS;
  *delta = sh->DELTA;
  *beg = sh->B;


fclose(fp);

}

void getfhv_(name,value,nerr)
     char *name;
     float *value;
     int nerr;
{
  nerr = find_header_var(name);
  if (fhdrptr != NULL)
    *value = *fhdrptr;
  else
    nerr = -1;
}

void setfhv_(name,value,nerr)
     char *name;
     float *value;
     int nerr;
{
  nerr = find_header_var(name);
  if (fhdrptr != NULL)
    *fhdrptr = *value;
  else
    nerr = -1;
}

void setnhv_(name,value,nerr)
     char *name;
     int *value;
     int nerr;
{
  nerr = find_header_var(name);
  if (ihdrptr != NULL)
    *ihdrptr = *value;
  else
    nerr = -1;
}

void getnhv_(name,value,nerr)
     char *name;
     int *value;
     int nerr;
{
  nerr = find_header_var(name);
  if (ihdrptr != NULL)
    *value = *ihdrptr;
  else
    nerr = -1;
}

void getkhv_(name,value,nerr)
     char *name;
     char *value;
     int nerr;
{
  nerr = find_header_var(name);
  if (chdrptr != NULL)
    *value = *chdrptr;
  else
    nerr = -1;
}

void setkhv_(name,value,nerr)
     char *name;
     char *value;
     int nerr;
{
  nerr = find_header_var(name);
  if (chdrptr != NULL)
    *chdrptr = *value;
  else
    nerr = -1;
}

void setlhv_(name,value,nerr)
     char *name;
     int *value;
     int nerr;
{
  nerr = find_header_var(name);
  if ((*value == TRUE) || (*value == FALSE)) {
    if (ihdrptr != NULL)
      *ihdrptr = *value;
    else
      nerr = -1;
  }
  else {
    fprintf(stderr, "ERROR! setlhv: logical value must be TRUE or FALSE \n");
    nerr = -2;
  }
}

void getlhv_(name,value,nerr)
     char *name;
     int *value;
     int nerr;
{
  nerr = find_header_var(name);
  if (ihdrptr != NULL)
    *value = *ihdrptr;
  else
    nerr = -1;
}


int find_header_var(name)
     char *name;
{
  int length;
  length = strcspn(name," ");

  if (!strncasecmp(name,"DELTA",length))
    fhdrptr = &sh->DELTA;
  else if (!strncasecmp(name,"DEPMIN",length))
    fhdrptr = &sh->DEPMIN;
  else if (!strncasecmp(name,"DEPMAX",length))
    fhdrptr = &sh->DEPMAX;
  else if (!strncasecmp(name,"SCALE",length))
    fhdrptr = &sh->SCALE;
  else if (!strncasecmp(name,"ODELTA",length))
    fhdrptr = &sh->ODELTA;
  else if (!strncasecmp(name,"B",length))
    fhdrptr = &sh->B;
  else if (!strncasecmp(name,"E",length))
    fhdrptr = &sh->E;
  else if (!strncasecmp(name,"O",length))
    fhdrptr = &sh->O;
  else if (!strncasecmp(name,"A",length))
    fhdrptr = &sh->A;
  else if (!strncasecmp(name,"INT0",length))
    fhdrptr = &sh->INT0;
   else if (!strncasecmp(name,"T0",length))
     fhdrptr = &sh->T0;
   else if (!strncasecmp(name,"T1",length))
     fhdrptr = &sh->T1;
   else if (!strncasecmp(name,"T2",length))
     fhdrptr = &sh->T2;
   else if (!strncasecmp(name,"T3",length))
     fhdrptr = &sh->T3;
   else if (!strncasecmp(name,"T4",length))
     fhdrptr = &sh->T4;
   else if (!strncasecmp(name,"T5",length))
     fhdrptr = &sh->T5;
   else if (!strncasecmp(name,"T6",length))
     fhdrptr = &sh->T6;
   else if (!strncasecmp(name,"T7",length))
     fhdrptr = &sh->T7;
   else if (!strncasecmp(name,"T8",length))
     fhdrptr = &sh->T8;
   else if (!strncasecmp(name,"T9",length))
     fhdrptr = &sh->T9;
   else if (!strncasecmp(name,"F",length))
     fhdrptr = &sh->F;
   else if (!strncasecmp(name,"RESP0",length))
     fhdrptr = &sh->RESP0;
   else if (!strncasecmp(name,"RESP1",length))
     fhdrptr = &sh->RESP1;
   else if (!strncasecmp(name,"RESP2",length))
     fhdrptr = &sh->RESP2;
   else if (!strncasecmp(name,"RESP3",length))
     fhdrptr = &sh->RESP3;
   else if (!strncasecmp(name,"RESP4",length))
     fhdrptr = &sh->RESP4;
   else if (!strncasecmp(name,"RESP5",length))
     fhdrptr = &sh->RESP5;
   else if (!strncasecmp(name,"RESP6",length))
     fhdrptr = &sh->RESP6;
   else if (!strncasecmp(name,"RESP7",length))
     fhdrptr = &sh->RESP7;
   else if (!strncasecmp(name,"RESP8",length))
     fhdrptr = &sh->RESP8;
   else if (!strncasecmp(name,"RESP9",length))
     fhdrptr = &sh->RESP9;
   else if (!strncasecmp(name,"STLA",length))
     fhdrptr = &sh->STLA;
   else if (!strncasecmp(name,"STLO",length))
     fhdrptr = &sh->STLO;
   else if (!strncasecmp(name,"STEL",length))
     fhdrptr = &sh->STEL;
   else if (!strncasecmp(name,"STDP",length))
     fhdrptr = &sh->STDP;
   else if (!strncasecmp(name,"EVLA",length))
     fhdrptr = &sh->EVLA;
   else if (!strncasecmp(name,"EVLO",length))
     fhdrptr = &sh->EVLO;
   else if (!strncasecmp(name,"EVEL",length))
     fhdrptr = &sh->EVEL;
   else if (!strncasecmp(name,"EVDP",length))
     fhdrptr = &sh->EVDP;
   else if (!strncasecmp(name,"UN0",length))
     fhdrptr = &sh->UN0;
   else if (!strncasecmp(name,"USER0",length))
     fhdrptr = &sh->USER0;
   else if (!strncasecmp(name,"USER1",length))
     fhdrptr = &sh->USER1;
   else if (!strncasecmp(name,"USER2",length))
     fhdrptr = &sh->USER2;
   else if (!strncasecmp(name,"USER3",length))
     fhdrptr = &sh->USER3;
   else if (!strncasecmp(name,"USER4",length))
     fhdrptr = &sh->USER4;
   else if (!strncasecmp(name,"USER5",length))
     fhdrptr = &sh->USER5;
   else if (!strncasecmp(name,"USER6",length))
     fhdrptr = &sh->USER6;
   else if (!strncasecmp(name,"USER7",length))
     fhdrptr = &sh->USER7;
   else if (!strncasecmp(name,"USER8",length))
     fhdrptr = &sh->USER8;
   else if (!strncasecmp(name,"USER9",length))
     fhdrptr = &sh->USER9;
   else if (!strncasecmp(name,"DIST",length))
     fhdrptr = &sh->DIST;
   else if (!strncasecmp(name,"AZ",length))
     fhdrptr = &sh->AZ;
   else if (!strncasecmp(name,"BAZ",length))
     fhdrptr = &sh->BAZ;
   else if (!strncasecmp(name,"GCARC",length))
     fhdrptr = &sh->GCARC;
   else if (!strncasecmp(name,"INT1",length))
     fhdrptr = &sh->INT1;
   else if (!strncasecmp(name,"INT2",length))
     fhdrptr = &sh->INT2;
   else if (!strncasecmp(name,"DEPMEN",length))
     fhdrptr = &sh->DEPMEN;
   else if (!strncasecmp(name,"CMPAZ",length))
     fhdrptr = &sh->CMPAZ;
   else if (!strncasecmp(name,"CMPINC",length))
     fhdrptr = &sh->CMPINC;
   else if (!strncasecmp(name,"UN1",length))
     fhdrptr = &sh->UN1;
   else if (!strncasecmp(name,"UN2",length))
     fhdrptr = &sh->UN2;
   else if (!strncasecmp(name,"UN3",length))
     fhdrptr = &sh->UN3;
   else if (!strncasecmp(name,"UN4",length))
     fhdrptr = &sh->UN4;
   else if (!strncasecmp(name,"UN5",length))
     fhdrptr = &sh->UN5;
   else if (!strncasecmp(name,"UN6",length))
     fhdrptr = &sh->UN6;
   else if (!strncasecmp(name,"UN7",length))
     fhdrptr = &sh->UN7;
   else if (!strncasecmp(name,"UN8",length))
     fhdrptr = &sh->UN8;
   else if (!strncasecmp(name,"UN9",length))
     fhdrptr = &sh->UN9;
   else if (!strncasecmp(name,"UN10",length))
     fhdrptr = &sh->UN10;
   else if (!strncasecmp(name,"UN11",length))
     fhdrptr = &sh->UN11;

   else if (!strncasecmp(name,"NZYEAR",length))
     ihdrptr = &sh->NZYEAR;
   else if (!strncasecmp(name,"NZJDAY",length))
     ihdrptr = &sh->NZJDAY;
   else if (!strncasecmp(name,"NZHOUR",length))
     ihdrptr = &sh->NZHOUR;
   else if (!strncasecmp(name,"NZMIN",length))
     ihdrptr = &sh->NZMIN;
   else if (!strncasecmp(name,"NZSEC",length))
     ihdrptr = &sh->NZSEC;
   else if (!strncasecmp(name,"NZMSEC",length))
     ihdrptr = &sh->NZMSEC;
   else if (!strncasecmp(name,"NVHDR",length))
     ihdrptr = &sh->NVHDR;
   else if (!strncasecmp(name,"INT3",length))
     ihdrptr = &sh->INT3;
   else if (!strncasecmp(name,"INT4",length))
     ihdrptr = &sh->INT4;
   else if (!strncasecmp(name,"NPTS",length))
     ihdrptr = &sh->NPTS;
   else if (!strncasecmp(name,"INT5",length))
     ihdrptr = &sh->INT5;
   else if (!strncasecmp(name,"INT6",length))
     ihdrptr = &sh->INT6;
   else if (!strncasecmp(name,"UN12",length))
     ihdrptr = &sh->UN12;
   else if (!strncasecmp(name,"UN13",length))
     ihdrptr = &sh->UN13;
   else if (!strncasecmp(name,"UN14",length))
     ihdrptr = &sh->UN14;
   else if (!strncasecmp(name,"IFTYPE",length))
     ihdrptr = &sh->IFTYPE;
   else if (!strncasecmp(name,"IDEP",length))
     ihdrptr = &sh->IDEP;
   else if (!strncasecmp(name,"IZTYPE",length))
     ihdrptr = &sh->IZTYPE;
   else if (!strncasecmp(name,"UN15",length))
     ihdrptr = &sh->UN15;
   else if (!strncasecmp(name,"IINST",length))
     ihdrptr = &sh->IINST;
   else if (!strncasecmp(name,"ISTREG",length))
     ihdrptr = &sh->ISTREG;
   else if (!strncasecmp(name,"IEVREG",length))
     ihdrptr = &sh->IEVREG;
   else if (!strncasecmp(name,"IEVTYP",length))
     ihdrptr = &sh->IEVTYP;
   else if (!strncasecmp(name,"IQUAL",length))
     ihdrptr = &sh->IQUAL;
   else if (!strncasecmp(name,"ISYNTH",length))
     ihdrptr = &sh->ISYNTH;
   else if (!strncasecmp(name,"UN16",length))
     ihdrptr = &sh->UN16;
   else if (!strncasecmp(name,"UN17",length))
     ihdrptr = &sh->UN17;
   else if (!strncasecmp(name,"UN18",length))
     ihdrptr = &sh->UN18;
   else if (!strncasecmp(name,"UN19",length))
     ihdrptr = &sh->UN19;
   else if (!strncasecmp(name,"UN20",length))
     ihdrptr = &sh->UN20;
   else if (!strncasecmp(name,"UN21",length))
     ihdrptr = &sh->UN21;
   else if (!strncasecmp(name,"UN22",length))
     ihdrptr = &sh->UN22;
   else if (!strncasecmp(name,"UN23",length))
     ihdrptr = &sh->UN23;
   else if (!strncasecmp(name,"UN24",length))
     ihdrptr = &sh->UN24;
   else if (!strncasecmp(name,"UN25",length))
     ihdrptr = &sh->UN25;
   else if (!strncasecmp(name,"LEVEN",length))
     ihdrptr = (int *) &sh->LEVEN;
   else if (!strncasecmp(name,"LOVROK",length))
     ihdrptr = (int *) &sh->LOVROK;
   else if (!strncasecmp(name,"LPSPOL",length))
     ihdrptr = (int *) &sh->LPSPOL;
   else if (!strncasecmp(name,"LCALDA",length))
     ihdrptr = (int *) &sh->LCALDA;

   else if (!strncasecmp(name,"KSTNM",length))
     chdrptr = (char *) &sh->KSTNM;
   else if (!strncasecmp(name,"KEVNM",length))
     chdrptr = (char *) &sh->KEVNM;
   else if (!strncasecmp(name,"KHOLE",length))
     chdrptr = (char *) &sh->KHOLE;
   else if (!strncasecmp(name,"KO",length))
     chdrptr = (char *) &sh->KO;
   else if (!strncasecmp(name,"KA",length))
     chdrptr = (char *) &sh->KA;
   else if (!strncasecmp(name,"KT0",length))
     chdrptr = (char *) &sh->KT0;
   else if (!strncasecmp(name,"KT1",length))
     chdrptr = (char *) &sh->KT1;
   else if (!strncasecmp(name,"KT2",length))
     chdrptr = (char *) &sh->KT2;
   else if (!strncasecmp(name,"KT3",length))
     chdrptr = (char *) &sh->KT3;
   else if (!strncasecmp(name,"KT4",length))
     chdrptr = (char *) &sh->KT4;
   else if (!strncasecmp(name,"KT5",length))
     chdrptr = (char *) &sh->KT5;
   else if (!strncasecmp(name,"KT6",length))
     chdrptr = (char *) &sh->KT6;
   else if (!strncasecmp(name,"KT7",length))
     chdrptr = (char *) &sh->KT7;
   else if (!strncasecmp(name,"KT8",length))
     chdrptr = (char *) &sh->KT8;
   else if (!strncasecmp(name,"KT9",length))
     chdrptr = (char *) &sh->KT9;
   else if (!strncasecmp(name,"KF",length))
     chdrptr = (char *) &sh->KF;
   else if (!strncasecmp(name,"KUSER0",length))
     chdrptr = (char *) &sh->KUSER0;
   else if (!strncasecmp(name,"KUSER1",length))
     chdrptr = (char *) &sh->KUSER1;
   else if (!strncasecmp(name,"KUSER2",length))
     chdrptr = (char *) &sh->KUSER2;
   else if (!strncasecmp(name,"KCMPNM",length))
     chdrptr = (char *) &sh->KCMPNM;
   else if (!strncasecmp(name,"KNETWK",length))
     chdrptr = (char *) &sh->KNETWK;
   else if (!strncasecmp(name,"KDATRD",length))
     chdrptr = (char *) &sh->KDATRD;
   else if (!strncasecmp(name,"KINST",length))
       chdrptr = (char *) &sh->KINST;
   else {
       fhdrptr = NULL;
       ihdrptr = NULL;
       chdrptr = NULL;
       return(-1);
     }
  return(0);
}   

initialize_header()

{
  int i;
  
  fhdrptr = (float *) sh;
  for(i=0;i<N_FHDR;i++) {
    *fhdrptr = -12345.0;
    fhdrptr++;
  }

  ihdrptr = (int *) fhdrptr;
  for(i=0;i<N_IHDR;i++) {
    *ihdrptr = -12345;
    ihdrptr++;
  }
  
  lhdrptr = (int *) ihdrptr;
  for(i=0;i<N_LHDR;i++) {
    *lhdrptr = -12345;
    lhdrptr++;
  }

  chdrptr = (char *) lhdrptr;
  for(i=0;i<N_CHDR;i++) {
    sprintf(chdrptr,"-12345");
    chdrptr+= 8;
  }

  sh->NVHDR = 6;
  sh->IFTYPE = 1; 
  sh->LEVEN = TRUE;
  sh->LOVROK = TRUE;
  sh->LPSPOL=-12345;
  sh->LCALDA=-12345;
  sh->UN26=-12345;
}



