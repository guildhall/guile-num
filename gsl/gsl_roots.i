%module "gsl/gsl-roots"

%multiple_values;

%{
#include <gsl/gsl_roots.h>
%}

extern const gsl_root_fsolver_type * gsl_root_fsolver_bisection;
extern const gsl_root_fsolver_type * gsl_root_fsolver_brent;
extern const gsl_root_fsolver_type * gsl_root_fsolver_falsepos;
extern const gsl_root_fdfsolver_type * gsl_root_fdfsolver_newton;
extern const gsl_root_fdfsolver_type * gsl_root_fdfsolver_secant;
extern const gsl_root_fdfsolver_type * gsl_root_fdfsolver_steffenson;

extern gsl_root_fsolver * gsl_root_fsolver_alloc (const gsl_root_fsolver_type * T);
extern void gsl_root_fsolver_free (gsl_root_fsolver * s);

extern gsl_root_fdfsolver * gsl_root_fdfsolver_alloc (const gsl_root_fdfsolver_type * T);
extern void gsl_root_fdfsolver_free (gsl_root_fdfsolver * s);

extern int gsl_root_fsolver_set (gsl_root_fsolver * S, gsl_function * F, double X_LOWER, double X_UPPER);
extern int gsl_root_fdfsolver_set (gsl_root_fdfsolver * S, gsl_function_fdf * FDF, double ROOT);
extern const char * gsl_root_fsolver_name (const gsl_root_fsolver * S);
extern const char * gsl_root_fdfsolver_name (const gsl_root_fdfsolver * S);

extern int gsl_root_fsolver_iterate (gsl_root_fsolver * S);
extern int gsl_root_fdfsolver_iterate (gsl_root_fdfsolver * S);
extern double gsl_root_fsolver_root (const gsl_root_fsolver * S);
extern double gsl_root_fdfsolver_root (const gsl_root_fdfsolver * S);
extern double gsl_root_fsolver_x_lower (const gsl_root_fsolver * S);
extern double gsl_root_fsolver_x_upper (const gsl_root_fsolver * S);
extern int gsl_root_test_interval (double X_LOWER, double X_UPPER, double EPSABS, double EPSREL);
extern int gsl_root_test_delta (double X1, double X0, double EPSREL, double EPSABS);
extern int gsl_root_test_residual (double F, double EPSABS);

%scheme %{
(define my-so (dynamic-link "gsl/libguile-gsl-roots.so"))
(dynamic-call "SWIG_init" my-so)
%}
