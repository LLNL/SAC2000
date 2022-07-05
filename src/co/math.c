#include <stdlib.h>

double fabs();

int min(a,b)
int a, b;
{

   return ( a < b ? a : b );
}

int max(a,b)
int a, b;
{

  return ( a > b ? a : b );
}

double fmin(a,b)
double a, b;
{

  return ( a < b ? a : b );
}

double fmax(a,b)
double a, b;
{

  return ( a > b ? a : b );
}

int isign(a,b)
int a, b;
{

  return ( b >= 0L ? abs(a) : -(abs(a)) );
}

double sign(a,b)
double a, b;
{

  return ( b >= 0.0 ? fabs(a) : -(fabs(a)) );
}

double powi(b,x)
double b;
int x;

{
  double temp;
  int i;


  if ( b == 0.0 ) return( (double) 0.0 );
  if ( x == 0L ) return( (double) 1.0 ) ;

  if ( x > 0L ) {
    temp = b;
    for ( i = x-1; i > 0L; i-- ) temp *= b;
    return temp;
  }

  if ( x < 0L ) {
    temp = 1.0 / b;
    for ( i = x+1; i < 0L; i++ ) temp *= (1.0/b);
    return temp;
  }
  return 0;
}

int ipow(b,x)
int b, x;

{
  int temp, i;


  if ( b == 0L ) return 0L;
  if ( b == 1L ) return 1L;

  if ( x < 0L ) return 0L;
  if ( x == 0L ) return 1L;
  if ( x == 1L ) return b;

  temp = b;
  for ( i = x-1; i > 0L; i--) temp *= b;
  
  return temp;
}

