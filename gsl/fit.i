%module "gsl/fit"

%multiple_values;

%{
#include <gsl/gsl_fit.h>
%}

%include "gsl_fit_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-fit.la"))
(dynamic-call "scm_init_gsl_fit_module" my-so)
%}
