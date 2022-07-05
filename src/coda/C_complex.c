#include <math.h>
#include "coda_complex.h"

double aimag(c)
complexf c;
{
  return(c.im);
}

complexf cmplxadd(c1,c2)
complexf c1,c2;
{
  c1.re += c2.re;
  c1.im += c2.im;
  return(c1);
}

double cmplxtof(c)
complexf c;
{
  return(c.re);
}

complexf cmplxcj(c)
complexf c;
{
  c.im = -c.im;
  return(c);
}

complexf cmplxmul(c1,c2)
complexf c1,c2;
{
  complexf c3;
  
  c3.re = (c1.re * c2.re) - (c1.im * c2.im);
  c3.im = (c1.re * c2.im) + (c1.im * c2.re);
  return(c3);
}

complexf flttocmplx(d1,d2)
double d1,d2;
{
  complexf c;
  c.re = d1;
  c.im = d2;
  return(c);
}

complexf cmplxsub(c1,c2)
complexf c1,c2;
{
  c1.re -= c2.re;
  c1.im -= c2.im;
  return(c1);
}

double cmplxabs(c)
complexf c;
{
  return(sqrt((double)((c.re*c.re) + (c.im*c.im))));
} 

double cmplxang(c)
complexf c;
{
  double d;

  if ( c.im < 1.0e-14 && c.im > -1.0e-14 )
        d = acos(c.re/cmplxabs(c));
  else if (c.re < 1.0e-14 && c.re > -1.0e-14)
        d = asin(c.im/cmplxabs(c));
  else {
        d = atan(c.im/c.re);
        if(c.re < 0.0) {
          if(c.im < 0.0) d -= 3.1415926536;
          else           d += 3.1415926536;
        }
  }

  return(d);
}

complexf cmplxsqrt(c)
complexf c;
{
  double sqrtsave, angle;

  sqrtsave = sqrt(cmplxabs(c));
  angle = cmplxang(c);

  c.re = sqrtsave*cos(angle/2.0);
  c.im = sqrtsave*sin(angle/2.0);

  if(c.re < 0.0) {
    c.re = -c.re;
    c.im = -c.im;
  }
  else if (c.re < 1.0e-14 && c.re > -1.0e-14 && c.im < 0.0)
          c.im = -c.im;

  return(c);
}

complexf cmplxdiv(c1,c2)
complexf c1,c2;
{
  complexf c;
  float f;

  if (c2.re == 0.0 && c2.im == 0.0) {
    printf("complex divide by zero-cmplxdiv\n");
    exit(1);
  }

  f = c2.re*c2.re + c2.im*c2.im;

  c.re = (c1.re*c2.re + c1.im*c2.im)/f;
  c.im = (c2.re*c1.im - c1.re*c2.im)/f;

  return(c);
}

complexf cmplxlog(c)
complexf c;
{
  complexf c1;

  c1.re = log(cmplxabs(c));
  c1.im = cmplxang(c);

  return(c1);
}

complexf cmplxexp(c)
complexf c;
{
  double d;

  if(c.re == 0.0) d = 1.0;
  else            d = exp(c.re);

  if(c.im == 0.0){
    c.re = d;
    return(c);
  }

  c.re = d*cos(c.im);
  c.im = d*sin(c.im);

  return(c);
}

complexf cmplxpow(c,d)
complexf c;
double d;
{
  if(c.re == 0.0 && c.im == 0.0) return(c);

  c = cmplxlog(c);
  c.re = d*c.re;
  c.im = d*c.im;

  return(cmplxexp(c));
}

complexf cmplxneg(c)
complexf c;
{
  c.re = -c.re;
  c.im = -c.im;
  return(c);
}
