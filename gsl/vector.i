%module "gsl/vector"

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

%include "gsl_vector_inc.i"

%scheme %{
(use-modules (gsl math))

(define my-so (dynamic-link "libguile-gsl-vector.la"))
(dynamic-call "scm_init_gsl_vector_module" my-so)

(export vector->gsl-vector-int
	gsl-vector-int->vector
	vector->gsl-vector
	gsl-vector->vector
	vector->gsl-vector-complex
	gsl-vector-complex->vector)

;;; Vector conversion functions
(define (vector->gsl-vector-int v)
  (let* ((len (uniform-vector-length v))
	 (w (gsl-vector-int-calloc len)))
    (do ((i 0 (+ i 1)))
	((= i len) w)
      (gsl-vector-int-set w i (uniform-vector-ref v i)))))

(define (gsl-vector-int->vector v)
  (let* ((len (gsl-vector-int-length v))
	 (w (make-uniform-vector len -1)))
    (do ((i 0 (+ i 1)))
	((= i len) w)
      (uniform-vector-set! w i (gsl-vector-int-get v i)))))

(define (vector->gsl-vector v)
  (let* ((len (uniform-vector-length v))
	 (w (gsl-vector-calloc len)))
    (do ((i 0 (+ i 1)))
	((= i len) w)
      (gsl-vector-set w i (uniform-vector-ref v i)))))

(define (gsl-vector->vector v)
  (let* ((len (gsl-vector-length v))
	 (w (make-uniform-vector len 1/3)))
    (do ((i 0 (+ i 1)))
	((= i len) w)
      (uniform-vector-set! w i (gsl-vector-get v i)))))

(define (vector->gsl-vector-complex v)
  (let* ((len (uniform-vector-length v))
	 (w (gsl-vector-complex-calloc len))
	 (z (gsl-complex-alloc)))
    (do ((i 0 (+ i 1)))
	((= i len) w)
      (let* ((z (uniform-vector-ref v i))
	     (re (real-part z))
	     (im (imag-part z)))
	(gsl-vector-complex-set-real-imag w i re im)))))

(define (gsl-vector-complex->vector v)
  (let ((w (make-uniform-vector (gsl-vector-complex-length v) 0+i)))
    (do ((i 0 (+ i 1)))
	((= i (gsl-vector-complex-length v)) w)
      (let ((z (gsl-vector-complex-get v i)))
	(uniform-vector-set! w i (gsl-complex->complex z))))))
%}
