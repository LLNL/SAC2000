/* ../../inc/mem.h */

/*  old initializations
#ifdef DOINITS
int *const isacmem = (int*)cmmem.sacmem;

int *const Isacmem = (int*)&cmmem.sacmem[0] - 1;

float *const Sacmem = &cmmem.sacmem[0] - 1;
#else
extern int *const isacmem;
extern int *const Isacmem;
extern float *const Sacmem;
#endif
   old initializations */

/* These dynamic pointers now must be initialized and
   managed in routines like iniam, allamb, reaamb.
   Isacmem and Sacmem will no inter be needed, I
   think.                                        */

#ifdef DOINITS
  struct t_cmmem cmmem = { 0, NULL };
#else
  extern struct t_cmmem cmmem;
#endif
