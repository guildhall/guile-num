%module "gsl/gsl-permute"

%{
#include <gsl/gsl_permute.h>
#include <gsl/gsl_vector.h>
%}

%include "gsl_permute.inc"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-permute.la"))
(dynamic-call "SWIG_init" my-so)
%}
