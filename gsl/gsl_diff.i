%module "gsl/gsl-diff"

%multiple_values;

%{
#include <gsl/gsl_diff.h>
%}

extern int gsl_diff_central (const gsl_function *F, double X,
			     double *OUTPUT, double *OUTPUT);
extern int gsl_diff_forward (const gsl_function *F, double X,
			     double *OUTPUT, double *OUTPUT);
extern int gsl_diff_backward (const gsl_function *F, double X,
			      double *OUTPUT, double *OUTPUT);
