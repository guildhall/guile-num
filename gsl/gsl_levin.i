%module "gsl/gsl-levin"

%multiple_values;

%{
#include <gsl/gsl_sum.h>
%}

typedef struct {
	size_t size;
	size_t i;                     /* position in array */
	size_t terms_used;            /* number of calls */
	double sum_plain;
	double *q_num;
	double *q_den;
	double *dq_num;
	double *dq_den;
	double *dsum;
} gsl_sum_levin_u_workspace;

typedef struct {
	size_t size;
	size_t i;                     /* position in array */
	size_t terms_used;            /* number of calls */
	double sum_plain;
	double *q_num;
	double *q_den;
	double *dsum;
} gsl_sum_levin_utrunc_workspace;

extern gsl_sum_levin_u_workspace * gsl_sum_levin_u_alloc (size_t N);
extern void gsl_sum_levin_u_free (gsl_sum_levin_u_workspace * W);
extern int gsl_sum_levin_u_accel (const double * ARRAY, size_t ARRAY_SIZE, gsl_sum_levin_u_workspace * W, double * OUTPUT, double * OUTPUT);

extern gsl_sum_levin_utrunc_workspace * gsl_sum_levin_utrunc_alloc (size_t N);
extern void gsl_sum_levin_utrunc_free (gsl_sum_levin_utrunc_workspace * W);
extern int gsl_sum_levin_utrunc_accel (const double * ARRAY, size_t ARRAY_SIZE, gsl_sum_levin_utrunc_workspace * W, double * OUTPUT , double * OUTPUT);

%scheme %{
(define my-so (dynamic-link "gsl/libguile-gsl-levin.la"))
(dynamic-call "SWIG_init" my-so)
%}
