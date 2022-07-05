void window_data(array,nxsize,nysize,wdata,jxstart,jxstop,jystart,jystop)
float *array;
int nxsize, nysize;
float *wdata;
int jxstart, jxstop, jystart, jystop;
{

  int i, j;

  for (i = jystart-1; i < jystop; i++){
    for (j = jxstart-1; j < jxstop; j++){
      *(wdata++) = *(array + (i*nxsize) + j);
    }
  }

  return;
}
