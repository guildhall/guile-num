%module "gsl/gsl-randist"

%{
#include <gsl/gsl_randist.h>
%}

%multiple_values;

%include "gsl_randist.inc"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-randist.la"))
(dynamic-call "SWIG_init" my-so)
%}
