%module "gsl/multifit"

%multiple_values;

%{
#include <gsl/gsl_multifit.h>
%}

%include "gsl_multifit_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-multifit.la"))
(dynamic-call "scm_init_gsl_multifit_module" my-so)
%}
