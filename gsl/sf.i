%module "gsl/sf"

%{
#include <gsl/gsl_sf.h>
%}

typedef enum {GSL_PREC_DOUBLE, GSL_PREC_SINGLE, GSL_PREC_APPROX} gsl_mode_t;

%include "gsl_sf_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-sf.la"))
(dynamic-call "scm_init_gsl_sf_module" my-so)
%}
