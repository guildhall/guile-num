%module "gsl/randist"

%{
#include <gsl/gsl_randist.h>
%}

%multiple_values;

%include "gsl_randist_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-randist.la"))
(dynamic-call "scm_init_gsl_randist_module" my-so)
%}
