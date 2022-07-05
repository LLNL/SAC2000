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

int gseRead20(char *fileName, char *WorkSetName, int Replace, int MaxWaveforms, int verbose,
              double MaxPhysMem );
void dblTableOfContents(DBlist dblList, FILE *unit);
int dblGetNumWaveformsInMemory(DBlist tree);


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



void readgse( int lmore, char* kdirin, int kdirin_s, char* kdflin, int kdflin_s, int ndflin, int Verbose, int isASCII, float MaxMem, int* nerr)
{
   int filesReturned = 0;
   DBlist tree;
   int Replace        = 0;

   int Nfiles         = 0;
   char **GSEFileList= 0;
   char *WorkSetName;
   int MaxTraces      = MDFL;
   int j;

	/*=====================================================================
	 * PURPOSE:  To read a set of GSE files into memory.
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


	if( !lmore ) 
           smClearDefaultTree( );
           
           
        WorkSetName = smGetDefaultWorksetName();


	
	GSEFileList = MakeFileList(kdirin, kdflin, &Nfiles);
	for(j=0;j<Nfiles;j++){
           filesReturned += gseRead20(GSEFileList[j], WorkSetName, Replace,  
                                          MaxTraces, Verbose, MaxMem);                                          
           tree = smGetDefaultTree();
           if(dblGetNumWaveformsInMemory(tree) >= MaxTraces)
                 break;
	   if(smFracPhysMemUsed() > MaxMem)
		 break;
 	}
        if(Verbose)printf("\n");
        FreeStringArray(GSEFileList,     Nfiles);
	
        
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
	cmdfm.nreadflag = LOW ;
	cmdfm.lread = TRUE ;
        SeisMgrToSac ( tree , !lmore , nerr, Verbose , FALSE ) ;
	cmdfm.lread = FALSE ;


L_8888:

	return;


}

