#include <stdio.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mem.h"
#include "coda.h"


int binary_op (float* data1, float* data2, int n, char* op);
void C_xapiir(float* data, int nsamps, char* aproto, double trbndw, double a, int iord, char* type,  double flo, double fhi, double ts, int passes);
int unary_op (float* data, int* npts, char* op, float arg, float* delta);
void wsac1_(char* outfile,float* data,int *npts, float* begin, float* delta,int* nerr);


void calc_envelopes(int nchannels,int ndata,int *file_pointers,struct envelope *envelopes,int* nbands,float begin,
                   float delta,int* npts,float dist,char* evid)
{
  int i,ichan,orig_npts,new_npts,namelength;
  float *databuf1, *databuf2;
  float orig_delta;
  char filename[200];

  databuf1 = (float *) malloc(ndata*sizeof(float));
  databuf2 = (float *) malloc(ndata*sizeof(float));

  fprintf(stderr,"Computing envelopes: ndata= %d nchannels= %d\n",ndata,nchannels);
  for(i=1;i<=*nbands;i++) {
    //       fprintf(stderr,"Computing envelope for band %d of %d (%f-%f) \n", i, *nbands, envelopes[i].freq_low, envelopes[i].freq_high);
    if(envelopes[i].freq_high > ((1/delta)/2)) {
      fprintf(stderr, "Frequency %d %f is greater than nyquist! Skip this band \n",i,envelopes[i].freq_high);
      *nbands = *nbands -1;
    }

    // form envelopes and average all of the channels
    for(ichan=0;ichan<nchannels;ichan++) {
//      fprintf(stderr,"Computing envelope for channel %d of %d\n",ichan,nchannels);
      binary_op(cmmem.sacmem[file_pointers[ichan]],databuf1,ndata,"copy");
      C_xapiir(databuf1, ndata, "BU", 0., 0., 4, "BP",(envelopes+i)->freq_low, envelopes[i].freq_high, delta, 2);
      unary_op(databuf1,&ndata,"envelope",0.0,&delta);
      unary_op(databuf1,&ndata,"log10",0.0,&delta);
      if( i == 0 ) {
	binary_op(databuf1,databuf2,ndata,"copy");
      } else {
	binary_op(databuf2,databuf1,ndata,"addf");
      }
    }
    unary_op(databuf2,&ndata,"div",(float) nchannels,&delta);

    orig_npts = ndata;
    orig_delta = delta;
    unary_op(databuf1,&ndata,"interpolate",DELTA_GF,&delta);
    new_npts = ndata-5;
    ndata = orig_npts;
    *npts = orig_npts;
    unary_op(databuf1,&new_npts,"smooth",3.0,&delta);
    envelopes[i].envelope_data = (float *) malloc(new_npts*sizeof(float));
    binary_op(databuf1,envelopes[i].envelope_data,new_npts,"copy");
    sprintf(filename, "%s.env_%2.2f-%2.2f",evid,envelopes[i].freq_low,envelopes[i].freq_high);
    envelopes[i].number_of_points = new_npts;
    envelopes[i].GFdelta = DELTA_GF;
    namelength = strcspn(filename," ");
    wsac1_(filename,databuf1,&new_npts,&begin,&delta,&nerr);
    delta = orig_delta;
  }
  free(databuf1);
  free(databuf2);
}
