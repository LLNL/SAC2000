#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>

#ifndef __STDC__
#  include <sys/types.h>
#  include <unistd.h>
#endif

#include <sys/param.h>
#include "../../inc/complex.h"
#undef MIN  /* dont use the MIN defined in sys/param.h, use the one in proto.h */
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"


#include "../../inc/cssb.h"
#include "../../inc/smDataIO.h"
#include "../../inc/smMemory.h"

int dblGetNumWaveformsInMemory(DBlist tree);
void dblTableOfContents(DBlist dblList, FILE *unit);

static char **MakeFileList(char *DirectoryString, char *Files, int *Nfiles)
{
   char *Directory;
   char delimit[] = " \t\n";
   char cmd[500];
   char fileName[200];
   int L;
   int AddSlash = 0;
   char *token;
   char ** FileList = 0;
   FILE *FilesFound;
   FILE *testFile;


   *Nfiles = 0;
   if(!Files)return 0;


   if(DirectoryString){
       Directory = strtok(DirectoryString, delimit);
       if(Directory && strlen(Directory) ){
	  L = strlen(Directory);
	  if(Directory[L-1] != '/')AddSlash = 1;
       }
   }
   
   token = strtok(Files, delimit);
   while(token){
      strcpy(cmd, "ls ");
      if(Directory){
	 strcat(cmd, Directory);
	 if(AddSlash)strcat(cmd, "/");
      }
      strcat(cmd, token);
      FilesFound = popen(cmd, "r");
      while(fscanf(FilesFound,"%s",fileName) != EOF){
         if( (testFile = fopen(fileName,"r") ) ){
            FileList = (char**) realloc(FileList, ((*Nfiles) + 1) * sizeof(char*) );
	    FileList[*Nfiles] = (char*) malloc(strlen(fileName) + 1);
	    strcpy(FileList[*Nfiles], fileName);
	    (*Nfiles)++;
            fclose(testFile);
	 }
      }
      pclose(FilesFound);

      token = strtok(0, delimit);

   }

   return FileList;
}
/* ------------------------------------------------------------------- */



void /*FUNCTION*/ readcfl( lmore, kdirin, kdirin_s, kdflin, 
	 kdflin_s, ndflin, Verbose, isASCII, MaxMem, nerr)
int lmore;
char *kdirin;   int kdirin_s;
char *kdflin;   int kdflin_s;
int Verbose;
int isASCII;
float MaxMem;
int ndflin, *nerr;
{
   char *CurrentWfdiscFile;
   char kwfdisc[MCPFN+1];
   char *wfdiscdir = NULL;
   char tmpBandString[2];
   char tmpOrientString[2];
   char kstationsel[7] ;
   char kchannelsel[9] ;
   char *wfdiscroots;
   int lensave; 
   int nwfdiscr;
   int nwfdiscs; 
   int iwfdisc; 
   int wfdisclist_s; 
   int wflexpnd;
   int ic1wfdisc;
   int ic2wfdisc;
   int takeEvid = TRUE ;
   char *wfdisclist   = NULL;
   int filesReturned = 0;
   DBlist tree;

   /* These are arguments to cssReadFlatFiles function */
   char** Station     = 0;
   int Nstation       = 0;
   char** Channel     = 0;
   int Nchannel       = 0;
   char** Band        = 0;
   int Nband          = 0;
   char** Code        = 0;
   int NCode          = 0;
   char** Orientation = 0;
   int Norientation   = 0;
   char** Author      = 0;
   int Nauthor        = 0;
   char** Filelist    = 0;
   int Nfiles         = 0;
   char** Phaselist   = 0;
   int Nphases        = 0;
   int Replace        = 0;
   char **CSSBFileList= 0;
   char *Root;              /* Basename of wfdisc file */
   char *WorkSetName;
   int MaxTraces      = MDFL;
   int j;

	/*=====================================================================
	 * PURPOSE:  To read a filelist of css formatted files into memory.
	 *=====================================================================
	 * INTPUT ARGUMENTS:
	 *    ldata:   Set to .TRUE. if header and data are to be read. [l]
	 *             Set to .FALSE. if only header (no data) are to be read.
	 *    lmore:   Set to .TRUE. if these files are to be appended to the
	 *             ones in memory, .FALSE. if they are to replace them. [l]
	 *    kdirin:  Default input directory to use if filename does not
	 *             contain a directory part. [c]
	 *    kdflin:  Input file list. [cl]
	 *    ndflin:  Number of entries in kdflin. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 1301, 1320
	 *=====================================================================
	 * MODULE/LEVEL: DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    DFM:     MDFL, KECBDF, NCOMP, NLNDTA
	 *             KGAIN, KBAND, KORIENT, KECMEM
	 *    HDR:     MHDR
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    DFM:     NDFL, KDFL, NDXHDR, NDXDTA
	 *             NDSDFL, KDSDFL
	 *    MEM:     SACMEM
	 *    HDR:     All.
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LCKEY, LCDFL,
	 *             ZGTFUN, CLEARDFL, ALLAMB, RDHDR, ZCLOSE, DEFMEM,
	 *             RDDTA, GTOUTM, SETRNG
	 *             Data-set storage subroutines.
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    980922:  New version using SeisMgr API.
	 *=====================================================================
	 * COMMENT:  
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;


	if( lmore ) {
	   if( !cmdfm.ltrust )
		cmdfm.nreadflag = LOW ;
	   else if( cmdfm.nreadflag == RDB )
		cmdfm.nreadflag = HIGH ;
	}
	else {
	   cmdfm.nreadflag = HIGH ;
           smClearDefaultTree( );
	}
           
           
        WorkSetName = smGetDefaultWorksetName();

        if(isASCII){
           /* pull the wfdisc file names out of the input file list--store in
              wfdiscroots and remove them from input file list (kdflin)
              wfdiscroots should be a char * and a blank delimited string of
              names will be returned (allocated in this routine) */
           getwfdiscs(&wfdiscroots, &nwfdiscr, kdflin, ndflin, nerr);
           if( *nerr != 0 ) goto L_8888;
     
           Filelist = TokenLineToStrgList(kdflin, &Nfiles, 0);
           ndflin -= nwfdiscr;

           /* if no filenames are left in the filelist after extracting the
              wfdisc names assume all files are to be read.  */

           if(ndflin == 0) kdflin[1] = '*';

           /* Process wildcards if any in wfdisc name(s). Process wfdiscroots
              through wildfl, returning wfdisclist and notfound. Allocate some
              temporary memory to get the returned file list in */

           wfdisclist_s = MAXCHARS;
           if((wfdisclist = (char *)malloc(wfdisclist_s)) == NULL){
              *nerr = 0301;
              goto L_8888;
  	   }
           memset(wfdisclist,' ',wfdisclist_s - 1);
           *(wfdisclist+wfdisclist_s-1) = '\0';

           wildfl(kdirin, kdirin_s, wfdiscroots, strlen(wfdiscroots)+1,
                  nwfdiscr, wfdisclist, wfdisclist_s, &nwfdiscs, &wflexpnd);
           free(wfdiscroots);

           if(nwfdiscs == 0){
              printf("No wfdisc file(s) found in the specified directory\n");
              *nerr = 1301;
              goto L_8888;
    	   }

	   /* - extract simple name from dataset pathname */
           if((lensave = nstrlensp( kdirin,kdirin_s )) > 0){
              wfdiscdir = malloc(lensave+1);
              strncpy(wfdiscdir,kdirin,lensave);
              wfdiscdir[lensave] = '\0';
           }else{
              wfdiscdir = malloc(2);
              strcpy(wfdiscdir,".");
              lensave = 1;
           }        

	}


/* set up to read file list from .wfdiscs selecting on stationsel, bandwsel,
and orientsel. At this point Directory to read from is in (NULL-terminated)
wfdiscdir */



	if( kmdfm.lstation ){
	    strcpy ( kstationsel , kmdfm.kstation ) ;
            if(strcmp(kstationsel, "*") )  /* Null Station acts as * */
               Station = TokenLineToStrgList(kstationsel, &Nstation, 0);

	}

        if( kmdfm.lchannel ){   /* channel selection.  maf 970403 */
            strcpy ( kchannelsel , kmdfm.kchannel ) ;
            if(strcmp(kchannelsel, "*") )
               Channel = TokenLineToStrgList(kchannelsel, &Nchannel, 0);
        }

	if( kmdfm.lbandw ){
            tmpBandString[0] = toupper(kmdfm.kbandw[0]);
            tmpBandString[1] = '\0';
            if(kmdfm.kbandw[0] != '*') 
               Band = TokenLineToStrgList(tmpBandString, &Nband, 0);
	}

	if( kmdfm.lorient ){
            tmpOrientString[0] = toupper(kmdfm.korient[0]);
            tmpOrientString[1] = '\0';
            if(kmdfm.korient[0] != '*')
               Orientation = TokenLineToStrgList(tmpOrientString,
                                                 &Norientation, 0);
	}

	/* determine value of takeEvid */
	if( cmdfm.nreadflag != LOW && cmdfm.ltrust == TRUE )
	    takeEvid = TRUE ;
	else
	    takeEvid = FALSE ;

        if(isASCII){

           ic1wfdisc = 0;

           for( iwfdisc = 0; iwfdisc < nwfdiscs; iwfdisc++){

	      fstrncpy( kwfdisc, MCPFN, " ", 1);

              if( !( lnxtcl( wfdisclist, strlen( wfdisclist ) + 1,
                             &ic1wfdisc, &ic2wfdisc ) ) )
              {
		 printf("error reading wfdisc--readcfl\n");
	 	 *nerr = 901;
	      }

              memset(kwfdisc,' ',MCPFN);
              kwfdisc[MCPFN] = '\0';
              strncpy(kwfdisc,wfdisclist+ic1wfdisc-1,ic2wfdisc-ic1wfdisc+1);
              CurrentWfdiscFile = strtok(kwfdisc, " ");

              filesReturned += cssReadFlatFiles(CurrentWfdiscFile, WorkSetName,
                                                Replace,     MaxTraces,  
                                                Station,     Nstation, 
                                                Channel,     Nchannel, 
                                                Band,        Nband, 
                                                Code,        NCode, 
                                                Orientation, Norientation, 
                                                Author,      Nauthor,
		                                Filelist,    Nfiles, 
                                                Phaselist,   Nphases,
                                                Verbose,     MaxMem,
                                                &takeEvid );

             if( !takeEvid )
                cmdfm.nreadflag = LOW ;
	   }
           FreeStringArray(Station,     Nstation);
           FreeStringArray(Channel,     Nchannel);
           FreeStringArray(Band,        Nband);
           FreeStringArray(Code,        Nchannel);
           FreeStringArray(Orientation, Norientation);
           FreeStringArray(Author,      Nauthor);
           FreeStringArray(Filelist,    Nfiles);
           FreeStringArray(Phaselist,   Nphases);
	}
	else{
	   CSSBFileList = MakeFileList(kdirin, kdflin, &Nfiles);
	   for(j=0;j<Nfiles;j++){
              filesReturned += ReadCSSBfile(CSSBFileList[j], WorkSetName,
                                            Replace, MaxTraces, Verbose,
                                            MaxMem, takeEvid );
              tree = smGetDefaultTree();
              if(dblGetNumWaveformsInMemory(tree) >= MaxTraces)
                 break;
	      if(smFracPhysMemUsed() > MaxMem)
		 break;
 	   }
           if(Verbose)printf("\n");
           FreeStringArray(CSSBFileList,     Nfiles);
	}
        
        tree = smGetDefaultTree();
        if(filesReturned){
           if(Verbose)
              dblTableOfContents(tree, stdout);
	}
	else{
	   printf("PROBLEM: No traces returned from SeisMgr!\n");
	   *nerr = 0114;
	   goto L_8888;
	}
        
        tree = smGetDefaultTree();
	cmdfm.lread = TRUE ;
        SeisMgrToSac ( tree , !lmore , nerr, Verbose , TRUE ) ;
	cmdfm.lread = FALSE ;


L_8888:
        if(wfdiscdir != NULL)  free(wfdiscdir);
        if(wfdisclist != NULL) free(wfdisclist);

	return;


}

