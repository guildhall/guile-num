%module "gsl/gsl-integration"

%multiple_values;

%{
#include <gsl/gsl_integration.h>
%}

%include "gsl_integration_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-integration.la"))
(dynamic-call "SWIG_init" my-so)
%}
