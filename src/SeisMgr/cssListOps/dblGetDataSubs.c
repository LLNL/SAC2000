#include <stdio.h>
#include <stdlib.h>

#include "cssListStrucs.h"
#include "dblPublicDefs.h"
#include "cssListOps.h"
#include "../dbselect/dbDefaults.h"
#include "../smMemory/smMemory.h"
#include "dblGetDataSubs.h"
#include "cssArchitecture.h"

#define REG register

void g2tofloat (REG unsigned char *from, REG float *to, REG int num) ;
void Convert2(void *in);
void Convert4(void *in);
void Convert8(void *in);

void swapShortArray( short *ptr, int NPTS )
{
    int idx ;
    for( idx = 0; idx < NPTS; ++idx ) 
       Convert2( (void*) ( ptr + idx ) );
} 
/* ------------------------------------------------------------ */   




void swapFloatArray( float *vArr , int NPTS )
{
   unsigned int idx;
    for( idx = 0; idx < NPTS; ++idx ) 
       Convert4( (void*) ( vArr+idx) );
}  
/* ------------------------------------------------------------ */   



void swapLongArray( int *vArr , int NPTS )
{
   unsigned int idx; 
    for( idx = 0; idx < NPTS; ++idx ) 
       Convert4( (void*) ( vArr+idx) );
}  
/* ------------------------------------------------------------ */   



void swapDoubleArray( double *vArr , int NPTS )
{
   unsigned int idx;
    for( idx = 0; idx < NPTS; ++idx ) 
       Convert8( (void*) ( vArr+idx) );
}  
/* ------------------------------------------------------------ */   



int dblGetE1 ( int NPTS , FILE *fptr , struct wfdiscList * wfStruc )
{
    int *intPtr , nerr , jdx ;
    float    *fltPtr ;

    intPtr = (int * ) smMalloc ( NPTS * sizeof ( int ) ) ;
    fltPtr = (float * ) smMalloc ( NPTS * sizeof ( float ) ) ;
    if ( !intPtr || !fltPtr ) {
	if ( intPtr ) smFree ( intPtr ) ;
	if ( fltPtr )  smFree ( fltPtr ) ;
	return 1 ;
    }

    enlarge ( fptr, NPTS, intPtr , &nerr ) ;
    if ( nerr != 0 ) {
	smFree ( intPtr ) ;
	smFree ( fltPtr ) ;
	return 2 ;
    }

    for( jdx = 0; jdx < NPTS; jdx++ ){
	fltPtr[ jdx ] = (float)( intPtr[jdx] ) ;
    }

    smFree ( intPtr ) ;

    wfStruc->element->nsamp = NPTS ;
    wfStruc->seis->i = fltPtr;
    wfStruc->seis->Cmplx = 0;

    return 0 ;

} /* end dblGetE1 */



int dblGetS2I2G2 ( int NPTS ,
                    FILE *fptr ,
                    struct wfdiscList * wfStruc ,
                    char * pType )
{
    short * shortPtr ;
    int idx ;
    int ptsRead ;
    float * fltPtr ;
    int bigendin = IsBigendin() ;

    shortPtr = (short *) smMalloc ( NPTS * sizeof ( short ) ) ;
    fltPtr   = (float *) smMalloc ( NPTS * sizeof ( float ) ) ;
    if ( !shortPtr || !fltPtr ) {
        if ( shortPtr ) smFree ( shortPtr ) ;
        if ( fltPtr )   smFree ( fltPtr )   ;
        return 1 ;
    }

    ptsRead = fread ( (void *) shortPtr , sizeof(short) , NPTS , fptr ) ;
    if ( ptsRead != NPTS ) {
        smFree ( shortPtr ) ;
        smFree ( shortPtr ) ;
        return 2 ;
    }

    if ( *pType == 's' || *pType == 'i' ) {     /* s2 or i2 data */
        if( ( *pType == 's' && !bigendin ) || ( *pType == 'i' && bigendin ) )
            swapShortArray( shortPtr, NPTS ) ;

        for ( idx = 0 ; idx < NPTS ; idx++ )
            fltPtr[ idx ] = (float) ( shortPtr[ idx ] ) ;
    }
    else        /* g2 Noress gain-ranged data */
        g2tofloat((unsigned char*)shortPtr,fltPtr, (int) ptsRead);

    wfStruc->element->nsamp = ptsRead;
    smFree(shortPtr);
    wfStruc->seis->i = fltPtr;
    wfStruc->seis->Cmplx = 0;

    return 0 ;

} /* end dblGetS2I2G2 */




int dblGetS3 ( int NPTS , FILE * fptr , struct wfdiscList *wfStruc )
{
    unsigned char threeByte[3] ;
    int jdx ;
    int ret ;
    float *fltPtr;

    union
    {
	unsigned char bval[4] ;
	int ival ;
    } u ;

    fltPtr = (float *) smMalloc(NPTS * sizeof(float) );
    if( !fltPtr )
        return 1 ;

    for ( jdx = 0 ; jdx < NPTS ; jdx++ ) 
    {
	ret = fread ( (void *) threeByte , 1 , 3 , fptr ) ;
	if ( ret != 3 ) 
	{
	    /* error handling */
	    smFree ( fltPtr ) ;
	    return 2 ;
	}


        if( IsBigendin() ){

	   u.bval[1] = threeByte[0] ;
           u.bval[2] = threeByte[1] ;
           u.bval[3] = threeByte[2] ;
           if ( u.bval[ 1 ] < 127 )
                u.bval[ 0 ] = 0 ;
           else
                u.bval[ 0 ] = 255 ;
        }
        else{
	   u.bval[2] = threeByte[0] ;
           u.bval[1] = threeByte[1] ;
           u.bval[0] = threeByte[2] ;
           if ( u.bval[ 2 ] < 127 )
                u.bval[ 3 ] = 0 ;
           else
                u.bval[ 3 ] = 255 ;
        }


	fltPtr[ jdx ] = (float) (u.ival) ;

    } /* end for */

    wfStruc->element->nsamp = NPTS ;
    wfStruc->seis->i = fltPtr;
    wfStruc->seis->Cmplx = 0;

    return 0 ;
} /* end dblGetS3 */



int dblGetT4F4( int NPTS, FILE *fptr, struct wfdiscList *wfStruc,
                 char *pType )
{
    int ptsRead;
    float *fltPtr;
    int bigendin = IsBigendin() ;

    fltPtr = (float *) smMalloc(NPTS * sizeof(float) );
    if(!fltPtr)
	return 1 ;

    ptsRead = fread( (void *) fltPtr, sizeof(float), NPTS, fptr);
    if(ptsRead != NPTS) {
	smFree ( fltPtr ) ;
	return 2 ;
    }

    if( ( *pType == 't' && !bigendin ) || ( *pType == 'f' && bigendin ) )
	swapFloatArray( (void*) fltPtr , NPTS ) ;

    wfStruc->element->nsamp = ptsRead;
    wfStruc->seis->i = fltPtr;
    wfStruc->seis->Cmplx = 0;

    return 0 ;

} /* end dblGetT4F4 */




int dblGetS4I4 ( int NPTS , FILE *fptr , struct wfdiscList *wfStruc ,
                  char *pType )
{
    int jdx ;
    int ptsRead, *intPtr;
    float *fltPtr;
    int bigendin = IsBigendin() ;

    fltPtr = (float *) smMalloc(NPTS * sizeof(float) );
    intPtr = (int *) smMalloc(NPTS * sizeof(int) );
    if(!fltPtr || !intPtr) {
	if ( fltPtr )  smFree ( fltPtr )  ;
	if ( intPtr ) smFree ( intPtr ) ;
        return 1 ;
    }

    ptsRead = fread( (void *) intPtr, sizeof(int), NPTS, fptr);
    if(ptsRead != NPTS){
	smFree ( fltPtr ) ;
	smFree ( intPtr ) ;
        return 2 ;
    }

    if( ( *pType == 's' && !bigendin ) || ( *pType == 'i' && bigendin ) )
        swapLongArray( (void*) intPtr , NPTS ) ;

    wfStruc->element->nsamp = ptsRead;
    for(jdx=0;jdx<ptsRead;jdx++) *(fltPtr +jdx) = *(intPtr + jdx);
    smFree(intPtr);
    wfStruc->seis->i = fltPtr;
    wfStruc->seis->Cmplx = 0;

    return 0 ;

} /* end dblGetS4I4 */




int dblGetT8F8 ( int NPTS , FILE * fptr , struct wfdiscList *wfStruc ,
                  char *pType )
{
    int idx ,
	ptsRead,
        bigendin = IsBigendin() ;
    float  * fltPtr ;
    double * dblPtr ;

    fltPtr = (float *) smMalloc(NPTS * sizeof(float) );
    dblPtr = (double*) smMalloc(NPTS * sizeof(double));
    if(!fltPtr || !dblPtr) {
	if ( fltPtr ) smFree ( fltPtr ) ;
	if ( dblPtr ) smFree ( dblPtr ) ;
        return 1 ;
    }

    ptsRead = fread( (void *) dblPtr, sizeof(double), NPTS, fptr);
    if(ptsRead != NPTS) {
	smFree ( fltPtr ) ;
        smFree ( dblPtr ) ;
        return 2 ;
    }

    if( ( *pType == 't' && !bigendin ) || ( *pType == 'f' && bigendin ) )
        swapDoubleArray( dblPtr , NPTS ) ;

    for ( idx = 0 ; idx < NPTS ; idx++ )
	fltPtr[ idx ] = dblPtr [ idx ] ;

    smFree ( dblPtr ) ;

    wfStruc->element->nsamp = ptsRead;
    wfStruc->seis->i = fltPtr;
    wfStruc->seis->Cmplx = 0;

    return 0 ;

} /* end dblGetT8F8 */






int dblGetRealImagFloats ( int NPTS , FILE * fptr , struct wfdiscList *wfStruc )
{
    int idx , ptsRead;
    float  * fltRPtr ;
    float  * fltIPtr ;

    fltRPtr = (float *) smMalloc(NPTS * sizeof(float) );
    fltIPtr = (float*)  smMalloc(NPTS * sizeof(float));
    if(!fltRPtr || !fltIPtr) {
	if ( fltRPtr ) smFree ( fltRPtr ) ;
	if ( fltIPtr ) smFree ( fltIPtr ) ;
        return 1 ;
    }

    ptsRead = fread( (void *) fltRPtr, sizeof(float), NPTS, fptr);
    if(ptsRead != NPTS) {
	smFree ( fltRPtr ) ;
        smFree ( fltIPtr ) ;
        return 2 ;
    }

    ptsRead = fread( (void *) fltIPtr, sizeof(float), NPTS, fptr);
    if(ptsRead != NPTS) {
	smFree ( fltRPtr ) ;
        smFree ( fltIPtr ) ;
        return 2 ;
    }


    wfStruc->element->nsamp = ptsRead;
    wfStruc->seis->r = fltRPtr;
    wfStruc->seis->i = fltIPtr;
    wfStruc->seis->Cmplx = 1;

    return 0 ;

} /* end dblGetRealImagFloats */

