%module "gsl/qrng"

%{
#include <gsl/gsl_qrng.h>
%}

%include "gsl_qrng_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-qrng.la"))
(dynamic-call "scm_init_gsl_qrng_module" my-so)
%}
