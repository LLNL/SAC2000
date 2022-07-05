#include <stdio.h>
#include "coda.h"

main(argc,argv)
     int     argc;
     char    *argv[];
{

  FILE *fpout;
  int i,nerr,length,nchar,ifstdout=1,j,intdata,ndata;
  static char infile1[200], infile2[200], inputfile[200];
  float input[50];
  float data1[MAXDATA],data2[MAXDATA];
  int max = MAXDATA;
  int horizontals=0;
  int vertical=1;
  struct envelope envelopes[MAXBANDS];
  int nbands;
  float begin, delta, npts, dist;



  if (argc <= 1) {
    fprintf(stderr,"Usage: coda inputfile datafile1 [datafile2] \n");
    exit(-1);
  }

  sscanf(argv[1],"%s",inputfile);
  sscanf(argv[2],"%s",infile1);

  if (argc == 4) {
    sscanf(argv[3],"%s",infile2);
    horizontals=1;
    vertical=0;
  }

  /* read input file */
  get_input(inputfile, &envelopes,&nbands);

/* read header line */

  initialize_header();

   rsac1_(infile1,&data1[0],&ndata,&begin,&delta,&max,&nerr );

   getfhv_("DIST",&dist,&nerr);
       unary_op(&data1, &ndata, "mul", 1e9, &delta);   

   if(horizontals) {
     rsac1_(infile2,&data2[0],&ndata,&begin,&delta,&max,&nerr );
     unary_op(&data2, &ndata, "mul", 1e9, &delta);
   }
   calc_envelopes(&data1[0],&data2[0],ndata,horizontals,&envelopes,nbands,begin,delta,&npts,dist);

   calc_coda_amplitudes(&envelopes,nbands,dist,begin);

   calc_moment_magnitude(&envelopes,nbands);
}
