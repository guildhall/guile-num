%module "gsl/gsl-min"

%multiple_values;

%{
#include <gsl/gsl_min.h>
%}

%include "gsl_min_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-min.la"))
(dynamic-call "SWIG_init" my-so)
%}