%module "gsl/gsl-dht"

%multiple_values;

%{
#include <gsl/gsl_dht.h>
%}

%include "gsl_dht.inc"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-dht.la"))
(dynamic-call "SWIG_init" my-so)
%}
