%module "gsl/gsl-sort"

%{
#include <gsl/gsl_sort.h>
%}

%include "gsl_sort_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-sort.la"))
(dynamic-call "SWIG_init" my-so)
%}
