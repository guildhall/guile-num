%module "gsl/gsl-fit"

%multiple_values;

%{
#include <gsl/gsl_fit.h>
%}

%include "gsl_fit.inc"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-fit.la"))
(dynamic-call "SWIG_init" my-so)
%}
