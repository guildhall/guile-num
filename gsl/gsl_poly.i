%module "gsl/gsl-poly"

%{
#include <gsl/gsl_poly.h>
%}

%include "gsl_poly.inc"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-poly.la"))
(dynamic-call "SWIG_init" my-so)
%}
