/*
 * Copyright 1990 Science Applications International Corporation.
 *
 * NAME
 *	complex -- Routines for manipulation of complex data types
 * 
 * FILE 
 *	complex.c
 *
 * SYNOPSIS
 *
 *	complex
 *	cset (a, b)
 *	double a;
 *	double b;
 *
 *	complex
 *	cadd (a, b)
 *	complex a;
 *	complex b;
 *
 *	complex
 *	csub (a, b)
 *	complex a;
 *	complex b;
 *
 *	complex
 *	cmult (a, b)
 *	complex a;
 *	complex b;
 *
 *	complex
 *	cdiv (a, b)
 *	complex a;
 *	complex b;
 *
 *	complex
 *	conj (a)
 *	complex a;
 *
 *	double
 *	cabs (a)
 *	complex a;
 *
 *	complex
 *	csqrt (a)
 *	complex a;
 *
 *	complex
 *	rcmult (r, a)
 *	double r;
 *	complex a;
 *
 *	double
 *	cmult_real (a, b)
 *	complex	a;
 *	complex	b;
 *
 *	double
 *	cmult_imag (a, b)
 *	complex	a;
 *	complex	b;
 *	
 * DESCRIPTION
 *
 *	cset () -- Returns a complex number with specified real and imaginary
 *		   parts
 *
 *	cadd () -- Returns the complex sum of two complex numbers
 *
 *	csub () -- Returns the complex difference of two complex numbers
 *
 *	cmult () -- Returns the complex product of two complex numbers
 *
 *	cdiv () -- Returns the complex quotient of two complex numbers
 *
 *	conj () -- Returns the complex conjugate of a complex number
 *
 *	cabs () -- Returns the complex absolute value (modulus) of a complex
 *		   number
 *
 *	csqrt () -- Returns the complex sqrt of a complex number
 *
 *	rcmult () -- Returns the product of a real number and a complex number
 *
 *	cmult_real () -- Returns the real part of the product of two complex
 *			 numbers
 *
 *	cmult_imag () -- Returns the imaguinary part of the product of two 
 *			 complex numbers

 * DIAGNOSTICS
 *	Describe any "gotcha's", hidden pitfalls, or subtle assumptions.
 *	Specify error conditions and results.
 *
 * FILES
 *	cmplx.h
 *
 * NOTES
 *	Scratch pad - modification history, plans for enhancements.
 * 
 * SEE ALSO
 *
 * AUTHOR
 *	Darrin Wahl
 *
 */

#include "cmplx.h"


complex cset (double a, double b)
{
	complex c;
	
	c.r = a;
	c.i = b;
	
	return (c);
}


complex cadd (complex a, complex b)
{
	complex c;
	
	c.r = a.r + b.r;
	c.i = a.i + b.i;

	return (c);
}


complex
csub (a, b)
complex	a;
complex	b;
{
	complex	c;
	
	c.r = a.r - b.r;
	c.i = a.i - b.i;

	return (c);
}


complex
cmult (a, b)
complex a;
complex b;
{
	complex	c;
	
	c.r = a.r * b.r - a.i * b.i;
	c.i = a.i * b.r + a.r * b.i;

	return (c);
}       		
	

complex 
cdiv (a, b)
complex a;
complex b;
{
	complex	c;
	double	r, den;
	
	if (fabs (b.r) >= fabs (b.i))
	{
		r = b.i / b.r;
		den = b.r + r * b.i;
	}
	else
	{
		r = b.r / b.i;
		den = b.i + r * b.r;
	}
	
	c.r = (a.r + r * a.i) / den;
	c.i = (a.i - r * a.r) / den;
       
	return (c);
}


complex
conj (a)
complex	a;
{
	complex c;
	
	c.r = a.r;
	c.i = -a.i;
	
	return (c);
}


double
cabs (a)
complex	a;
{
	double	x, y, ans, temp;
	
	x = fabs (a.r);
	y = fabs (a.i);
	
	if (x == 0.0)
	{
		ans = y;
	}
	else if (y == 0.0)
	{
		ans = x;
	}
	else if (x > y)
	{
		temp = y / x;
		ans = x * sqrt (1.0 + temp * temp);
	}
	else
	{
		temp = x / y;
		ans = y * sqrt (1.0 + temp * temp);
	}
	
	return (ans);
}


complex
csqrt (a)
complex a;
{
	complex	c;
	double	x, y, w, r;
	
	if ((a.r == 0.0) && (a.i == 0.0))
	{
		c.r = c.i = 0.0;
		return (c);
	}
	else
	{
		x = fabs (a.r);
		y = fabs (a.i);
		
		if (x >= y)
		{
			r = y / x;
			w = sqrt (x) * sqrt (0.5 * (1.0 * sqrt (1.0 + r * r)));
		}
		else
		{
			r = x / y;
			w = sqrt (y) * sqrt (0.5 * (1.0 * sqrt (1.0 + r * r)));
		}
		
		if (a.r >= 0.0)
		{
			c.r = w;
			c.i = a.i / (2.0 * w);
		}
		else
		{
			c.i = (a.i >= 0.0) ? w : -w;
			c.r = a.i / (2.0 * c.i);
		}
		
		return (c);
	}
}


complex
rcmult (r, a)
double	r;
complex	a;
{
	complex c;
	
	c.r = r * a.r;
	c.i = r * a.i;
	
	return (c);
}

	
	
double
cmult_real (a, b)
complex	a;
complex	b;
{
	return (a.r * b.r - a.i * b.i);
}


double
cmult_imag (a, b)
complex	a;
complex	b;
{
	return (a.i * b.r + a.r * b.i);
}








