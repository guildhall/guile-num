%module "gsl/permute"

%{
#include <gsl/gsl_permute.h>
#include <gsl/gsl_vector.h>
%}

%include "gsl_permute_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-permute.la"))
(dynamic-call "scm_init_gsl_permute_module" my-so)
%}
