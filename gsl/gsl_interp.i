%module "gsl/gsl-interp"

%multiple_values;

%{
#include <gsl/gsl_interp.h>
#include <gsl/gsl_spline.h>
%}

extern gsl_interp * gsl_interp_alloc (const gsl_interp_type * T, size_t SIZE);
extern int gsl_interp_init (gsl_interp * INTERP, const double XA[], const double YA[], size_t SIZE);
extern void gsl_interp_free (gsl_interp * INTERP);

extern const gsl_interp_type * gsl_interp_linear;
extern const gsl_interp_type * gsl_interp_polynomial;
extern const gsl_interp_type * gsl_interp_cspline;
extern const gsl_interp_type * gsl_interp_cspline_periodic;
extern const gsl_interp_type * gsl_interp_akima;
extern const gsl_interp_type * gsl_interp_akima_periodic;

extern const char * gsl_interp_name (const gsl_interp * INTERP);
extern unsigned int gsl_interp_min_size (const gsl_interp * INTERP);

extern size_t gsl_interp_bsearch (const double x_array[], double X, size_t INDEX_LO, size_t INDEX_HI);
extern gsl_interp_accel * gsl_interp_accel_alloc (void);
extern size_t gsl_interp_accel_find (gsl_interp_accel * A, const double x_array[], size_t SIZE, double X);
extern void gsl_interp_accel_free (gsl_interp_accel* A);

extern double gsl_interp_eval (const gsl_interp * INTERP, const double XA[], const double YA[], double X, gsl_interp_accel * A);
extern int gsl_interp_eval_e (const gsl_interp * INTERP, const double XA[], const double YA[], double X, gsl_interp_accel * A, double * OUTPUT);
extern double gsl_interp_eval_deriv (const gsl_interp * INTERP, const double XA[], const double YA[], double X, gsl_interp_accel * A);
extern int gsl_interp_eval_deriv_e (const gsl_interp * INTERP, const double XA[], const double YA[], double X, gsl_interp_accel * A, double * OUTPUT);
extern double gsl_interp_eval_deriv2 (const gsl_interp * INTERP, const double XA[], const double YA[], double X, gsl_interp_accel * A);
extern int gsl_interp_eval_deriv2_e (const gsl_interp * INTERP, const double XA[], const double YA[], double X, gsl_interp_accel * A, double * OUTPUT);
extern double gsl_interp_eval_integ (const gsl_interp * INTERP, const double XA[], const double YA[], double A, double B, gsl_interp_accel * A);
extern int gsl_interp_eval_integ_e (const gsl_interp * INTERP, const double XA[], const double YA[],  double A, double B, gsl_interp_accel * A, double * OUTPUT);

extern gsl_spline * gsl_spline_alloc (const gsl_interp_type * T, size_t N);
extern int gsl_spline_init (gsl_spline * SPLINE, const double xa[], const double YA[], size_t SIZE);
extern void gsl_spline_free (gsl_spline * SPLINE);
extern double gsl_spline_eval (const gsl_spline * SPLINE, double X, gsl_interp_accel * A);
extern int gsl_spline_eval_e (const gsl_spline * SPLINE, double X, gsl_interp_accel * A, double * OUTPUT);
extern double gsl_spline_eval_deriv (const gsl_spline * SPLINE, double X, gsl_interp_accel * A);
extern int gsl_spline_eval_deriv_e (const gsl_spline * SPLINE, double X, gsl_interp_accel * A, double * OUTPUT);
extern double gsl_spline_eval_deriv2 (const gsl_spline * SPLINE, double X, gsl_interp_accel * A);
extern int gsl_spline_eval_deriv2_e (const gsl_spline * SPLINE, double X, gsl_interp_accel * A, double * OUTPUT);
extern double gsl_spline_eval_integ (const gsl_spline * SPLINE, double A, double B, gsl_interp_accel * ACC);
extern int gsl_spline_eval_integ_e (const gsl_spline * SPLINE, double A, double B, gsl_interp_accel * ACC, double * OUTPUT);

%scheme %{
(define my-so (dynamic-link "libguile-gsl-interp.la"))
(dynamic-call "SWIG_init" my-so)
%}
