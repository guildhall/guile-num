%module "gsl/gsl-qrng"

%{
#include <gsl/gsl_qrng.h>
%}

%include "gsl_qrng.inc"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-qrng.la"))
(dynamic-call "SWIG_init" my-so)
%}
