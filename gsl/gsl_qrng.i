%module "gsl/gsl-qrng"

%{
#include <gsl/gsl_qrng.h>
%}

extern gsl_qrng * gsl_qrng_alloc (const gsl_qrng_type * T, unsigned int D);
extern void gsl_qrng_free (gsl_qrng * Q);
extern void gsl_qrng_init (gsl_qrng * Q);
extern int gsl_qrng_get (const gsl_qrng * Q, double X[]);
extern const char * gsl_qrng_name (const gsl_qrng * Q);
extern size_t gsl_qrng_size (const gsl_qrng * Q);
extern void * gsl_qrng_state (const gsl_qrng * Q);
extern int gsl_qrng_memcpy (gsl_qrng * DEST, const gsl_qrng * SRC);
extern gsl_qrng * gsl_qrng_clone (const gsl_qrng * Q);

extern const gsl_qrng_type * gsl_qrng_niederreiter_2;
extern const gsl_qrng_type * gsl_qrng_sobol;

%scheme %{
(define my-so (dynamic-link "gsl/libguile-gsl-qrng.so"))
(dynamic-call "SWIG_init" my-so)
%}
