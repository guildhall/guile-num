%module "gsl/sum"

%multiple_values;

%{
#include <gsl/gsl_sum.h>
%}

%include "gsl_sum_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-levin.la"))
(dynamic-call "scm_init_gsl_sum_module" my-so)
%}
