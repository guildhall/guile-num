%module "gsl/gsl-linalg"

%{
#include <gsl/gsl_linalg.h>
%}

%include "gsl_linalg.inc"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-linalg.la"))
(dynamic-call "SWIG_init" my-so)
%}
