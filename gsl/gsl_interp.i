%module "gsl/gsl-interp"

%multiple_values;

%{
#include <gsl/gsl_interp.h>
#include <gsl/gsl_spline.h>
%}

%include "gsl_interp.inc"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-interp.la"))
(dynamic-call "SWIG_init" my-so)
%}
