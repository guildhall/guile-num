%module "gsl/gsl-vector"

%{
#include <gsl/gsl_complex.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_permute.h>
%}

%{

size_t gsl_vector_int_length (gsl_vector_int * v)
{
	return v->size;
}

gsl_block_int * gsl_vector_int_block (gsl_vector_int * v)
{
	return v->block;
}

int * gsl_vector_int_data (gsl_vector_int * v)
{
	return v->data;
}

size_t gsl_vector_length (gsl_vector * v)
{
	return v->size;
}

gsl_block * gsl_vector_block (gsl_vector * v)
{
	return v->block;
}

double * gsl_vector_data (gsl_vector * v)
{
	return v->data;
}

size_t gsl_vector_complex_length (gsl_vector_complex * v)
{
	return v->size;
}

gsl_block_complex * gsl_vector_complex_block (gsl_vector_complex * v)
{
	return v->block;
}

double * gsl_vector_complex_data (gsl_vector_complex * v)
{
	return v->data;
}

void gsl_vector_complex_set_real_imag (gsl_vector_complex * v,
				       size_t i, double re, double im)
{
	gsl_complex zp;

	GSL_SET_COMPLEX(&zp, re, im);
	gsl_vector_complex_set(v, i, zp);
}

%}

size_t gsl_vector_int_length (gsl_vector_int * v);
gsl_block_int * gsl_vector_int_block (gsl_vector_int * v);
int * gsl_vector_int_data (gsl_vector_int * v);
size_t gsl_vector_length (gsl_vector * v);
gsl_block * gsl_vector_block (gsl_vector * v);
double * gsl_vector_data (gsl_vector * v);
size_t gsl_vector_complex_length (gsl_vector_complex * v);
gsl_block_complex * gsl_vector_complex_block (gsl_vector_complex * v);
double * gsl_vector_complex_data (gsl_vector_complex * v);
void gsl_vector_complex_set_real_imag (gsl_vector_complex * v,
				       size_t i, double re, double im);

%include "gsl_vector.inc"

%scheme %{
(define my-so (dynamic-link "libguile-gsl-vector.la"))
(dynamic-call "SWIG_init" my-so)
%}
