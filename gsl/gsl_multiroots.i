%module "gsl/gsl-multiroots"

%multiple_values;

%{
#include <gsl/gsl_multiroots.h>
%}

typedef struct {
	const gsl_multiroot_fsolver_type * type;
	gsl_multiroot_function * function ;
	gsl_vector * x ;
	gsl_vector * f ;
	gsl_vector * dx ;
	void *state;
} gsl_multiroot_fsolver;

typedef struct {
	const gsl_multiroot_fdfsolver_type * type;
	gsl_multiroot_function_fdf * fdf ;
	gsl_vector * x;
	gsl_vector * f;
	gsl_matrix * J;
	gsl_vector * dx;
	void *state;
} gsl_multiroot_fdfsolver;


extern const gsl_multiroot_fsolver_type * gsl_multiroot_fsolver_dnewton;
extern const gsl_multiroot_fsolver_type * gsl_multiroot_fsolver_broyden;
extern const gsl_multiroot_fsolver_type * gsl_multiroot_fsolver_hybrid;
extern const gsl_multiroot_fsolver_type * gsl_multiroot_fsolver_hybrids;

extern gsl_multiroot_fsolver * gsl_multiroot_fsolver_alloc (const gsl_multiroot_fsolver_type * T, size_t N);
extern int gsl_multiroot_fsolver_set (gsl_multiroot_fsolver * S, gsl_multiroot_function * F, gsl_vector * X);
extern void gsl_multiroot_fsolver_free (gsl_multiroot_fsolver * S);
extern const char * gsl_multiroot_fsolver_name (const gsl_multiroot_fsolver * S);

extern int gsl_multiroot_fsolver_iterate (gsl_multiroot_fsolver * S);
extern gsl_vector * gsl_multiroot_fsolver_root (const gsl_multiroot_fsolver * S);
extern gsl_vector * gsl_multiroot_fsolver_f (const gsl_multiroot_fsolver * S);
extern gsl_vector * gsl_multiroot_fsolver_dx (const gsl_multiroot_fsolver * S);

extern const gsl_multiroot_fdfsolver_type * gsl_multiroot_fdfsolver_newton;
extern const gsl_multiroot_fdfsolver_type * gsl_multiroot_fdfsolver_gnewton;
extern const gsl_multiroot_fdfsolver_type * gsl_multiroot_fdfsolver_hybridj;
extern const gsl_multiroot_fdfsolver_type * gsl_multiroot_fdfsolver_hybridsj;

extern gsl_multiroot_fdfsolver * gsl_multiroot_fdfsolver_alloc (const gsl_multiroot_fdfsolver_type * T, size_t N);
extern int gsl_multiroot_fdfsolver_set (gsl_multiroot_fdfsolver * S, gsl_multiroot_function_fdf * FDF, gsl_vector * X);
extern void gsl_multiroot_fdfsolver_free (gsl_multiroot_fdfsolver * S);
extern const char * gsl_multiroot_fdfsolver_name (const gsl_multiroot_fdfsolver * S);

extern int gsl_multiroot_fdfsolver_iterate (gsl_multiroot_fdfsolver * S);
extern gsl_vector * gsl_multiroot_fdfsolver_root (const gsl_multiroot_fdfsolver * S);
extern  gsl_vector * gsl_multiroot_fdfsolver_f (const gsl_multiroot_fdfsolver * S);
extern gsl_vector * gsl_multiroot_fdfsolver_dx (const gsl_multiroot_fdfsolver * S);


extern int gsl_multiroot_test_delta (const gsl_vector * DX, const gsl_vector * X, double EPSABS, double EPSREL);
extern int gsl_multiroot_test_residual (const gsl_vector * F, double EPSABS);

%scheme %{
(define my-so (dynamic-link "gsl/libguile-gsl-multiroots.so"))
(dynamic-call "SWIG_init" my-so)
%}
