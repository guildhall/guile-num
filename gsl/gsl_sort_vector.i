%module "gsl/gsl-sort-vector"

%{
#include <gsl/gsl_sort_vector.h>
%}

%include "gsl_sort_vector_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-sort-vector.la"))
(dynamic-call "SWIG_init" my-so)
%}
