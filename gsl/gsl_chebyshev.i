%module "gsl/gsl-chebyshev"

%multiple_values;

%{
#include <gsl/gsl_math.h>
#include <gsl/gsl_chebyshev.h>
%}

typedef struct
{
	double * c;		/* coefficients  c[0] .. c[order] */
	size_t order;		/* order of expansion             */
	double a;		/* lower interval point           */
	double b;		/* upper interval point           */
	size_t order_sp;
	double * f;
} gsl_cheb_series;

extern gsl_cheb_series * gsl_cheb_alloc (const size_t N);
extern void gsl_cheb_free (gsl_cheb_series * CS);
extern int gsl_cheb_init (gsl_cheb_series * CS, const gsl_function * F, const double A, const double B);
extern double gsl_cheb_eval (const gsl_cheb_series * CS, double X);
extern int gsl_cheb_eval_err (const gsl_cheb_series * CS, const double X, double * OUTPUT, double * OUTPUT);
extern double gsl_cheb_eval_n (const gsl_cheb_series * CS, size_t ORDER, double X);
extern int gsl_cheb_eval_n_err (const gsl_cheb_series * CS, const size_t ORDER, const double X, double * OUTPUT, double * OUTPUT);
extern int gsl_cheb_calc_deriv (gsl_cheb_series * DERIV, const gsl_cheb_series * CS);
extern int gsl_cheb_calc_integ (gsl_cheb_series * INTEG, const gsl_cheb_series * CS);
