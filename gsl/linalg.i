%module "gsl/linalg"

%{
#include <gsl/gsl_linalg.h>
%}

%include "gsl_linalg_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-linalg.la"))
(dynamic-call "scm_init_gsl_linalg_module" my-so)
%}
