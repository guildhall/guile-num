%module "gsl/statistics"

%{
#include <gsl/gsl_statistics.h>
%}

%multiple_values;

%include "gsl_statistics_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-statistics.la"))
(dynamic-call "scm_init_gsl_statistics_module" my-so)
%}
