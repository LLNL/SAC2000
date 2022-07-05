
#include "libresponse.h"

void
lagrange (f, xi, n, x, fx)    /* evenly spaced , in order */
double	*f, *xi;
int	n;
double	x, *fx;
{
	int	i, k;
	double	prod;

	*fx = 0.0;
	for (k = 0; k < n; k++)
	{
		prod = 1.0;
		for (i = 0; i < n; i++)
		{
			if (i != k)
				prod = prod * (x - xi[i]) / (xi[k] - xi[i]);
		}
		*fx = *fx + prod * f[k];
	}
}


