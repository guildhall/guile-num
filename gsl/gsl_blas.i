%module "gsl/gsl-blas"

%{
#include <gsl/gsl_blas.h>
%}


%include "gsl_blas.inc"


%scheme %{
(define my-so (dynamic-link "libguile-gsl-blas.la"))
(dynamic-call "SWIG_init" my-so)
%}
