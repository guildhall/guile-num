%module "gsl/gsl-combination"

%{
#include <gsl/gsl_combination.h>
%}

%include "gsl_combination.inc"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-combination.la"))
(dynamic-call "SWIG_init" my-so)
%}
