%module "gsl/gsl-block"

%{
#include <gsl/gsl_block.h>
%}

extern gsl_block * gsl_block_alloc (size_t N);
extern gsl_block * gsl_block_calloc (size_t N);
extern void gsl_block_free (gsl_block * B);
extern size_t gsl_block_size (const gsl_block * b);
extern double * gsl_block_data (const gsl_block * b);

extern gsl_block_complex * gsl_block_complex_alloc (size_t N);
extern gsl_block_complex * gsl_block_complex_calloc (size_t N);
extern void gsl_block_complex_free (gsl_block_complex * B);
extern size_t gsl_block_complex_size (const gsl_block_complex * b);
extern double * gsl_block_complex_data (const gsl_block_complex * b);

%scheme %{
(define my-so (dynamic-link "gsl/libguile-gsl-block.la"))
(dynamic-call "SWIG_init" my-so)
%}
