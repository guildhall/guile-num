%module "gsl/gsl-combination"

%{
#include <gsl/gsl_combination.h>
%}

extern gsl_combination * gsl_combination_alloc (size_t N, size_t K);
extern gsl_combination *gsl_combination_calloc (const size_t N, const size_t K);
extern void gsl_combination_init_first (gsl_combination * C);
extern void gsl_combination_init_last (gsl_combination * C);
extern void gsl_combination_free (gsl_combination * C);
extern size_t gsl_combination_get (const gsl_combination * C, const size_t I);
extern size_t gsl_combination_n (const gsl_combination * C);
extern size_t gsl_combination_k (const gsl_combination * C);
extern size_t * gsl_combination_data (const gsl_combination * C);
extern int gsl_combination_valid (gsl_combination * C);
extern int gsl_combination_next (gsl_combination * C);
extern int gsl_combination_prev (gsl_combination * C);
extern int gsl_combination_fwrite (FILE * STREAM, const gsl_combination * C);
extern int gsl_combination_fread (FILE * STREAM, gsl_combination * C);int gsl_combination_fprintf (FILE * STREAM, const gsl_combination * C, const char *FORMAT);
extern int gsl_combination_fscanf (FILE * STREAM, gsl_combination * C);
