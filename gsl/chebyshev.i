%module "gsl/chebyshev"

%multiple_values;

%{
#include <gsl/gsl_math.h>
#include <gsl/gsl_chebyshev.h>
%}

%include "gsl_chebyshev_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-chebyshev.la"))
(dynamic-call "scm_init_gsl_chebyshev_module" my-so)
%}
