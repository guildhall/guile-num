%module "gsl/gsl-roots"

%multiple_values;

%{
#include <gsl/gsl_roots.h>
%}

%include "gsl_roots.inc"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-roots.la"))
(dynamic-call "SWIG_init" my-so)
%}
