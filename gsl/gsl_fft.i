%module "gsl/gsl-fft"

%{
#include <gsl/gsl_fft_complex.h>
#include <gsl/gsl_fft_complex_float.h>
#include <gsl/gsl_fft_halfcomplex.h>
#include <gsl/gsl_fft_halfcomplex_float.h>
#include <gsl/gsl_fft_real.h>
#include <gsl/gsl_fft_real_float.h>
%}

%include "gsl_fft.inc"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-fft.la"))
(dynamic-call "SWIG_init" my-so)
%}
