%module "gsl/gsl-rng"

%{
#include <gsl/gsl_rng.h>

size_t gsl_rng_type_len(void)
{
	const gsl_rng_type **t, **t0;
	size_t len;
          
	t0 = gsl_rng_types_setup ();
	len = 0;
	for (t = t0; *t != 0; t++) {
		len++;
	}
	return len;
}
	

const gsl_rng_type * gsl_rng_type_ref(size_t i)
{
	const gsl_rng_type **t0;
          
	if (i < 0 || i >= gsl_rng_type_len())
		return NULL;

	t0 = gsl_rng_types_setup ();
	return t0[i];
}

const char * gsl_rng_type_name(const gsl_rng_type * t)
{
	return t->name;
}

%}

size_t gsl_rng_type_len(void);
const gsl_rng_type * gsl_rng_type_ref(size_t i);
const char * gsl_rng_type_name(const gsl_rng_type * T);

%include "gsl_rng_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-rng.la"))
(dynamic-call "SWIG_init" my-so)
%}
