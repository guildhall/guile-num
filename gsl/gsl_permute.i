%module "gsl/gsl-permute"

%{
#include <gsl/gsl_permute.h>
#include <gsl/gsl_vector.h>
%}

extern gsl_permutation * gsl_permutation_alloc (size_t N);
extern gsl_permutation * gsl_permutation_calloc (size_t N);
extern void gsl_permutation_init (gsl_permutation * P);
extern void gsl_permutation_free (gsl_permutation * P);
extern int gsl_permutation_memcpy (gsl_permutation * DEST, const gsl_permutation * SRC);
extern size_t gsl_permutation_get (const gsl_permutation * P, const size_t I);
extern int gsl_permutation_swap (gsl_permutation * P, const size_t I, const size_t J);
extern size_t gsl_permutation_size (const gsl_permutation * P);
extern size_t * gsl_permutation_data (const gsl_permutation * P);
extern int gsl_permutation_valid (gsl_permutation * P);
extern void gsl_permutation_reverse (gsl_permutation * P);
extern int gsl_permutation_inverse (gsl_permutation * INV, const gsl_permutation * P);
extern int gsl_permutation_next (gsl_permutation * P);
extern int gsl_permutation_prev (gsl_permutation * P);
extern int gsl_permute (const size_t * P, double * DATA, size_t STRIDE, size_t N);
extern int gsl_permute_inverse (const size_t * P, double * DATA, size_t STRIDE, size_t N);
extern int gsl_permute_vector (const gsl_permutation * P, gsl_vector * V);
extern int gsl_permute_vector_inverse (const gsl_permutation * P, gsl_vector * V);
extern int gsl_permutation_mul (gsl_permutation * P, const gsl_permutation * PA, const gsl_permutation * PB);
// extern int gsl_permutation_fwrite (FILE * STREAM, const gsl_permutation * P);
// extern int gsl_permutation_fread (FILE * STREAM, gsl_permutation * P);
// extern int gsl_permutation_fprintf (FILE * STREAM, const gsl_permutation * P, const char *FORMAT);
extern int gsl_permutation_fscanf (FILE * STREAM, gsl_permutation * P);
extern int gsl_permutation_linear_to_canonical (gsl_permutation * Q, const gsl_permutation * P);
extern int gsl_permutation_canonical_to_linear (gsl_permutation * P, const gsl_permutation * Q);
extern size_t gsl_permutation_inversions (const gsl_permutation * P);
extern size_t gsl_permutation_linear_cycles (const gsl_permutation * P);
extern size_t gsl_permutation_canonical_cycles (const gsl_permutation * Q);

%scheme %{
(define my-so (dynamic-link "gsl/libguile-gsl-permute.so"))
(dynamic-call "SWIG_init" my-so)
%}
