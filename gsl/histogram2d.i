%module "gsl/histogram2d"

%{
#include <stdio.h>
#include <gsl/gsl_histogram2d.h>
%}

%multiple_values;

%include "gsl_histogram2d_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-histogram2d.la"))
(dynamic-call "scm_init_gsl_histogram2d_module" my-so)
%}
