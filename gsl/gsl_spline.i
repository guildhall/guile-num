%module "gsl/gsl-spline"

%multiple_values;

%{
#include <gsl/gsl_spline.h>
%}

%include "gsl_spline.inc"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-spline.la"))
(dynamic-call "SWIG_init" my-so)
%}
