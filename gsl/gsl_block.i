%module "gsl/gsl-block"

%{
#include <gsl/gsl_block.h>
%}

%include "gsl_block_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-block.la"))
(dynamic-call "SWIG_init" my-so)
%}
