%module "gsl/gsl-linalg"

%{
#include <gsl/gsl_linalg.h>
%}

%include "gsl_linalg_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-linalg.la"))
(dynamic-call "SWIG_init" my-so)
%}
