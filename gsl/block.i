%module "block"

%{
#include <gsl/gsl_block.h>
%}

%include "gsl_block_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-block.la"))
(dynamic-call "scm_init_gsl_block_module" my-so)
%}
