%module "gsl/integration"
%include "typemaps.i"

%multiple_values;

%{
#include <gsl/gsl_integration.h>
%}

%include "gsl_integration_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-integration.la"))
(dynamic-call "scm_init_gsl_integration_module" my-so)
%}
