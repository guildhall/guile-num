%module "gsl/gsl-fit"

%multiple_values;

%{
#include <gsl/gsl_fit.h>
#include <gsl/gsl_multifit.h>
%}

extern int gsl_fit_linear (const double * X, const size_t XSTRIDE, const double * Y, const size_t YSTRIDE, size_t N, double * C0, double * C1, double * OUTPUT, double * OUTPUT, double * OUTPUT, double * OUTPUT);
extern int gsl_fit_wlinear (const double * X, const size_t XSTRIDE, const double * W, const size_t WSTRIDE, const double * Y, const size_t YSTRIDE, size_t N, double * OUTPUT, double * OUTPUT, double * OUTPUT, double * OUTPUT, double * OUTPUT, double * OUTPUT);
extern int gsl_fit_linear_est (double X, double C0, double C1, double C00, double C01, double C11, double *OUTPUT, double *OUTPUT);
extern int gsl_fit_mul (const double * X, const size_t XSTRIDE, const double * Y, const size_t YSTRIDE, size_t N, double * C1, double * OUTPUT, double * OUTPUT);
extern int gsl_fit_wmul (const double * X, const size_t XSTRIDE, const double * W, const size_t WSTRIDE, const double * Y, const size_t YSTRIDE, size_t N, double * C1, double * OUTPUT, double * OUTPUT);
extern int gsl_fit_mul_est (double X, double C1, double C11, double *Y, double *OUTPUT);

extern gsl_multifit_linear_workspace * gsl_multifit_linear_alloc (size_t N, size_t P);
extern void gsl_multifit_linear_free (gsl_multifit_linear_workspace * WORK);
extern int gsl_multifit_linear (const gsl_matrix * X, const gsl_vector * Y, gsl_vector * C, gsl_matrix * COV, double * OUTPUT, gsl_multifit_linear_workspace * WORK);
extern int gsl_multifit_wlinear (const gsl_matrix * X, const gsl_vector * W, const gsl_vector * Y, gsl_vector * C, gsl_matrix * COV, double * OUTPUT, gsl_multifit_linear_workspace * WORK);

%scheme %{
(define my-so (dynamic-link "gsl/libguile-gsl-fit.so"))
(dynamic-call "SWIG_init" my-so)
%}
