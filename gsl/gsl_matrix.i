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

void gsl_matrix_complex_set_real_imag (gsl_matrix_complex * v,
				       size_t i, size_t j,
				       double re, double im);

%include "gsl_matrix.inc"


%scheme %{
(use-modules (gsl gsl-math))

(define my-so (dynamic-link "libguile-gsl-matrix.la"))
(dynamic-call "SWIG_init" my-so)

(export matrix->gsl-matrix-int
	gsl-matrix-int->matrix
	matrix->gsl-matrix
	gsl-matrix->matrix
	matrix->gsl-matrix-complex
        gsl-matrix-complex->matrix)

;;; Matrix conversion functions
(define (matrix->gsl-matrix-int v)
  (let* ((rows (car (array-dimensions v)))
	 (cols (cadr (array-dimensions v)))
	 (w (gsl-matrix-int-calloc rows cols)))
    (do ((i 0 (+ i 1)))
	((= i rows) w)
      (do ((j 0 (+ j 1)))
	  ((= j cols))
	(gsl-matrix-int-set w i j (array-ref v i j))))))

(define (gsl-matrix-int->matrix v)
  (let* ((rows (gsl-matrix-int-rows v))
	 (cols (gsl-matrix-int-cols v))
	 (w (make-uniform-array -1 rows cols)))
    (do ((i 0 (+ i 1)))
	((= i rows) w)
      (do ((j 0 (+ j 1)))
	  ((= j cols))
	(array-set! w (gsl-matrix-int-get v i j) i j)))))

(define (matrix->gsl-matrix v)
  (let* ((rows (car (array-dimensions v)))
	 (cols (cadr (array-dimensions v)))
	 (w (gsl-matrix-calloc rows cols)))
    (do ((i 0 (+ i 1)))
	((= i rows) w)
      (do ((j 0 (+ j 1)))
	  ((= j cols))
	(gsl-matrix-set w i j (array-ref v i j))))))

(define (gsl-matrix->matrix v)
  (let* ((rows (gsl-matrix-rows v))
	 (cols (gsl-matrix-cols v))
	 (w (make-uniform-array 1/3 rows cols)))
    (do ((i 0 (+ i 1)))
	((= i rows) w)
      (do ((j 0 (+ j 1)))
	  ((= j cols))
	(array-set! w (gsl-matrix-get v i j) i j)))))

(define (matrix->gsl-matrix-complex v)
  (let* ((rows (car (array-dimensions v)))
	 (cols (cadr (array-dimensions v)))
	 (w (gsl-matrix-complex-calloc rows cols)))
    (do ((i 0 (+ i 1)))
	((= i rows) w)
      (do ((j 0 (+ j 1)))
	  ((= j cols))
	(let* ((z (array-ref v i j))
	       (re (real-part z))
	       (im (imag-part z)))
	  (gsl-matrix-complex-set-real-imag w i j re im))))))

(define (gsl-matrix-complex->matrix v)
  (let* ((rows (gsl-matrix-complex-rows v))
	 (cols (gsl-matrix-complex-cols v))
	 (w (make-uniform-array 0+i rows cols)))
    (do ((i 0 (+ i 1)))
	((= i rows) w)
      (do ((j 0 (+ j 1)))
	  ((= j cols))
	(let ((z (gsl-matrix-complex-get v i j)))
	  (array-set! w (gsl-complex->complex z) i j))))))
%}
