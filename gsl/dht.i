%module "gsl/dht"

%multiple_values;

%{
#include <gsl/gsl_dht.h>
%}

%include "gsl_dht_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-dht.la"))
(dynamic-call "scm_init_gsl_dht_module" my-so)
%}
