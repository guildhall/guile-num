%module "gsl/gsl-matrix"

%{
#include <gsl/gsl_matrix.h>
%}

%{
size_t gsl_matrix_int_rows (gsl_matrix_int * m)
{
	return m->size1;
}

size_t gsl_matrix_int_cols (gsl_matrix_int * m)
{
	return m->size2;
}

size_t gsl_matrix_rows (gsl_matrix * m)
{
	return m->size1;
}

size_t gsl_matrix_cols (gsl_matrix * m)
{
	return m->size2;
}

size_t gsl_matrix_complex_rows (gsl_matrix_complex * m)
{
	return m->size1;
}

size_t gsl_matrix_complex_cols (gsl_matrix_complex * m)
{
	return m->size2;
}

void gsl_matrix_complex_set_real_imag (gsl_matrix_complex * v,
				       size_t i, size_t j,
				       double re, double im)
{
	gsl_complex zp;

	GSL_SET_COMPLEX(&zp, re, im);
	gsl_matrix_complex_set(v, i, j, zp);
}


%}

size_t gsl_matrix_int_rows (gsl_matrix_int * M);
size_t gsl_matrix_int_cols (gsl_matrix_int * M);

size_t gsl_matrix_rows (gsl_matrix * M);
size_t gsl_matrix_cols (gsl_matrix * M);

size_t gsl_matrix_complex_rows (gsl_matrix_complex * M);
size_t gsl_matrix_complex_cols (gsl_matrix_complex * M);

%include "gsl_matrix.inc"


%scheme %{
(define my-so (dynamic-link "libguile-gsl-matrix.la"))
(dynamic-call "SWIG_init" my-so)
%}
