%module "gsl/interp"

%multiple_values;

%{
#include <gsl/gsl_interp.h>
#include <gsl/gsl_spline.h>
%}

%include "gsl_interp_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-interp.la"))
(dynamic-call "scm_init_gsl_interp_module" my-so)
%}
