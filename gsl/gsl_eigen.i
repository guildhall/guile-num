%module "gsl/gsl-eigen"

%{
#include <gsl/gsl_eigen.h>
%}

typedef enum {
	GSL_EIGEN_SORT_VAL_ASC,
	GSL_EIGEN_SORT_VAL_DESC,
	GSL_EIGEN_SORT_ABS_ASC,
	GSL_EIGEN_SORT_ABS_DESC
} gsl_eigen_sort_t;

extern gsl_eigen_symm_workspace * gsl_eigen_symm_alloc (const size_t N);
extern void gsl_eigen_symm_free (gsl_eigen_symm_workspace * W);
extern int gsl_eigen_symm (gsl_matrix * A, gsl_vector * EVAL, gsl_eigen_symm_workspace * W);

extern gsl_eigen_symmv_workspace * gsl_eigen_symmv_alloc (const size_t N);
extern int gsl_eigen_symmv (gsl_matrix * A, gsl_vector * EVAL, gsl_matrix * EVEC, gsl_eigen_symmv_workspace * W);

extern gsl_eigen_herm_workspace * gsl_eigen_herm_alloc (const size_t N);
extern void gsl_eigen_herm_free (gsl_eigen_herm_workspace * W);
extern int gsl_eigen_herm (gsl_matrix_complex * A, gsl_vector * EVAL, gsl_eigen_herm_workspace * W);

extern gsl_eigen_hermv_workspace * gsl_eigen_hermv_alloc (const size_t N);
extern void gsl_eigen_hermv_free (gsl_eigen_hermv_workspace * W);
extern int gsl_eigen_hermv (gsl_matrix_complex * A, gsl_vector * EVAL, gsl_matrix_complex * EVEC, gsl_eigen_hermv_workspace * W);

extern int gsl_eigen_symmv_sort (gsl_vector * EVAL, gsl_matrix * EVEC, gsl_eigen_sort_t SORT_TYPE);
extern int gsl_eigen_hermv_sort (gsl_vector * EVAL, gsl_matrix_complex * EVEC, gsl_eigen_sort_t SORT_TYPE);

%scheme %{
(define my-so (dynamic-link "gsl/libguile-gsl-eigen.la"))
(dynamic-call "SWIG_init" my-so)
%}
