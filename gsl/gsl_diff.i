%module "gsl/gsl-diff"

%multiple_values;

%{
#include <gsl/gsl_diff.h>
%}

%include <gsl_diff_inc.i>

%scheme %{
(define my-so (dynamic-link "libguile-gsl-diff.la"))
(dynamic-call "SWIG_init" my-so)
%}
