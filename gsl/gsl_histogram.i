%module "gsl/gsl-histogram"

%{
#include <stdio.h>
#include <gsl/gsl_histogram.h>
%}

%multiple_values;

%include "gsl_histogram_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-histogram.la"))
(dynamic-call "SWIG_init" my-so)
%}
