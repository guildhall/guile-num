%module "gsl/gsl-dht"

%multiple_values;

%{
#include <gsl/gsl_dht.h>
%}

%include "gsl_dht_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-dht.la"))
(dynamic-call "SWIG_init" my-so)
%}
