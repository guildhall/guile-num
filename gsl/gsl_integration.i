%module "gsl/gsl-integration"

%multiple_values;

%{
#include <gsl/gsl_integration.h>
%}

%include "gsl_integration.inc"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-integration.la"))
(dynamic-call "SWIG_init" my-so)
%}
