%module "gsl/eigen"

%{
#include <gsl/gsl_eigen.h>
%}

%include "gsl_eigen_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-eigen.la"))
(dynamic-call "scm_init_gsl_eigen_module" my-so)
%}
