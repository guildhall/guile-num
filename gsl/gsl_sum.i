%module "gsl/gsl-sum"

%multiple_values;

%{
#include <gsl/gsl_sum.h>
%}

%include "gsl_sum_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-levin.la"))
(dynamic-call "SWIG_init" my-so)
%}
