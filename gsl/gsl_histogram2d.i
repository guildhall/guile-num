%module "gsl/gsl-histogram2d"

%{
#include <stdio.h>
#include <gsl/gsl_histogram2d.h>
%}

%multiple_values;

%include "gsl_histogram2d.inc"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-histogram2d.la"))
(dynamic-call "SWIG_init" my-so)
%}
