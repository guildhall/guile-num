%module "gsl/gsl-chebyshev"

%multiple_values;

%{
#include <gsl/gsl_math.h>
#include <gsl/gsl_chebyshev.h>
%}

%include "gsl_chebyshev.inc"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-chebyshev.la"))
(dynamic-call "SWIG_init" my-so)
%}
