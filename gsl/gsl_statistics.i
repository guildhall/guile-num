%module "gsl/gsl-statistics"

%{
#include <gsl/gsl_statistics.h>
%}

%multiple_values;

%include "gsl_statistics.inc"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-statistics.la"))
(dynamic-call "SWIG_init" my-so)
%}
