%module "gsl/sortvector"

%{
#include <gsl/gsl_sort_vector.h>
%}

%include "gsl_sort_vector_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-sort-vector.la"))
(dynamic-call "scm_init_gsl_sortvector_module" my-so)
%}
