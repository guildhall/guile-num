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


extern gsl_vector_int * gsl_vector_int_alloc (size_t N);
extern gsl_vector_int * gsl_vector_int_calloc (size_t N);
extern void gsl_vector_int_free (gsl_vector_int * V);
size_t gsl_vector_int_length (gsl_vector_int * V);
gsl_block_int * gsl_vector_int_block (gsl_vector_int * V);
int * gsl_vector_int_data (gsl_vector_int * V);
extern int gsl_vector_int_get (const gsl_vector_int * V, size_t I);
extern void gsl_vector_int_set (gsl_vector_int * V, size_t I, int X);
extern void gsl_vector_int_set_all (gsl_vector_int * V, int X);
extern void gsl_vector_int_set_zero (gsl_vector_int * V);
extern int gsl_vector_int_set_basis (gsl_vector_int * V, size_t I);
extern int gsl_vector_int_memcpy (gsl_vector_int * DEST, const gsl_vector_int * SRC);
extern int gsl_vector_int_swap (gsl_vector_int * V, gsl_vector_int * W);
extern int gsl_vector_int_swap_elements (gsl_vector_int * V, size_t i, size_t j);
extern int gsl_vector_int_reverse (gsl_vector_int * V);
extern int gsl_vector_int_add (gsl_vector_int * A, const gsl_vector_int * B);
extern int gsl_vector_int_sub (gsl_vector_int * A, const gsl_vector_int * B);
extern int gsl_vector_int_mul (gsl_vector_int * A, const gsl_vector_int * B);
extern int gsl_vector_int_div (gsl_vector_int * A, const gsl_vector_int * B);
extern int gsl_vector_int_isnull (const gsl_vector_int * V);



extern gsl_vector * gsl_vector_alloc (size_t N);
extern gsl_vector * gsl_vector_calloc (size_t N);
extern void gsl_vector_free (gsl_vector * V);
size_t gsl_vector_length (gsl_vector * V);
gsl_block * gsl_vector_block (gsl_vector * V);
double * gsl_vector_data (gsl_vector * V);
extern double gsl_vector_get (const gsl_vector * V, size_t I);
extern void gsl_vector_set (gsl_vector * V, size_t I, double X);
extern void gsl_vector_set_all (gsl_vector * V, double X);
extern void gsl_vector_set_zero (gsl_vector * V);
extern int gsl_vector_set_basis (gsl_vector * V, size_t I);
extern int gsl_vector_memcpy (gsl_vector * DEST, const gsl_vector * SRC);
extern int gsl_vector_swap (gsl_vector * V, gsl_vector * W);
extern int gsl_vector_swap_elements (gsl_vector * V, size_t i, size_t j);
extern int gsl_vector_reverse (gsl_vector * V);
extern int gsl_vector_add (gsl_vector * A, const gsl_vector * B);
extern int gsl_vector_sub (gsl_vector * A, const gsl_vector * B);
extern int gsl_vector_mul (gsl_vector * A, const gsl_vector * B);
extern int gsl_vector_div (gsl_vector * A, const gsl_vector * B);
extern int gsl_vector_scale (gsl_vector * A, const double X);
extern int gsl_vector_add_constant (gsl_vector * A, const double X);
extern double gsl_vector_max (const gsl_vector * V);
extern double gsl_vector_min (const gsl_vector * V);
extern int gsl_vector_isnull (const gsl_vector * V);

extern gsl_vector_complex * gsl_vector_complex_alloc (size_t N);
extern gsl_vector_complex * gsl_vector_complex_calloc (size_t N);
extern void gsl_vector_complex_free (gsl_vector_complex * V);
size_t gsl_vector_complex_length (gsl_vector_complex * V);
gsl_block * gsl_vector_complex_block (gsl_vector_complex * V);
double * gsl_vector_complex_data (gsl_vector_complex * V);

extern gsl_complex gsl_vector_complex_get (const gsl_vector_complex * V, size_t I);
extern void gsl_vector_complex_set (gsl_vector_complex * V, size_t I, gsl_complex X);
void gsl_vector_complex_set_real_imag (gsl_vector_complex * V, size_t I, double RE, double IM);
extern void gsl_vector_complex_set_all (gsl_vector_complex * V, gsl_complex X);
extern void gsl_vector_complex_set_zero (gsl_vector_complex * V);
extern int gsl_vector_complex_set_basis (gsl_vector_complex * V, size_t I);
extern int gsl_vector_complex_memcpy (gsl_vector_complex * DEST, const gsl_vector_complex * SRC);
extern int gsl_vector_complex_swap (gsl_vector_complex * V, gsl_vector_complex * W);
extern int gsl_vector_complex_swap_elements (gsl_vector_complex * V, size_t i, size_t j);
extern int gsl_vector_complex_reverse (gsl_vector_complex * V);
extern int gsl_vector_complex_add (gsl_vector_complex * A, const gsl_vector_complex * B);
extern int gsl_vector_complex_sub (gsl_vector_complex * A, const gsl_vector_complex * B);
extern int gsl_vector_complex_mul (gsl_vector_complex * A, const gsl_vector_complex * B);
extern int gsl_vector_complex_div (gsl_vector_complex * A, const gsl_vector_complex * B);
extern int gsl_vector_complex_scale (gsl_vector_complex * A, const double X);
extern int gsl_vector_complex_add_constant (gsl_vector_complex * A, const gsl_complex X);
extern double gsl_vector_complex_max (const gsl_vector_complex * V);
extern double gsl_vector_complex_min (const gsl_vector_complex * V);
extern int gsl_vector_complex_isnull (const gsl_vector_complex * V);

extern void gsl_sort_vector (gsl_vector * V);
extern int gsl_sort_vector_index (gsl_permutation * P, const gsl_vector * V);
extern void gsl_sort_vector_smallest (double * DEST, size_t K, const gsl_vector * V);
extern void gsl_sort_vector_largest (double * DEST, size_t K, const gsl_vector * V);
extern void gsl_sort_vector_smallest_index (size_t * P, size_t K, const gsl_vector * V);
extern void gsl_sort_vector_largest_index (size_t * P, size_t K, const gsl_vector * V);

%scheme %{
(use-modules (gsl gsl-math))

(define my-so (dynamic-link "gsl/libguile-gsl-vector.la"))
(dynamic-call "SWIG_init" my-so)

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
