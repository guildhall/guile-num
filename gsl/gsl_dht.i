%module "gsl/gsl-dht"

%multiple_values;

%{
#include <gsl/gsl_dht.h>
%}

extern gsl_dht * gsl_dht_alloc (size_t SIZE);
extern int gsl_dht_init (gsl_dht * T, double NU, double XMAX);
extern gsl_dht * gsl_dht_new (size_t SIZE, double NU, double XMAX);
extern void gsl_dht_free (gsl_dht * T);
extern int gsl_dht_apply (const gsl_dht * T, double * F_IN, double * F_OUT);
extern double gsl_dht_x_sample (const gsl_dht * T, int N);
extern double gsl_dht_k_sample (const gsl_dht * T, int N);

%scheme %{
(define my-so (dynamic-link "gsl/libguile-gsl-dht.so"))
(dynamic-call "SWIG_init" my-so)
%}
