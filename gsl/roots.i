%module "gsl/roots"

%multiple_values;

%{
#include <gsl/gsl_roots.h>
%}

%include "gsl_roots_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-roots.la"))
(dynamic-call "scm_init_gsl_roots_module" my-so)
%}
