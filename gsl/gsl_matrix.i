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
extern gsl_matrix_int * gsl_matrix_int_alloc (size_t N1, size_t N2);
extern gsl_matrix_int * gsl_matrix_int_calloc (size_t N1, size_t N2);
extern void gsl_matrix_int_free (gsl_matrix_int * M);
extern int * gsl_matrix_int_ptr(gsl_matrix_int * m, const size_t i, const size_t j);
extern int gsl_matrix_int_get (const gsl_matrix_int * M, size_t I, size_t J);
extern void gsl_matrix_int_set (gsl_matrix_int * M, size_t I, size_t J, int X);
extern void gsl_matrix_int_set_all (gsl_matrix_int * M, int X);
extern void gsl_matrix_int_set_zero (gsl_matrix_int * M);
extern void gsl_matrix_int_set_identity (gsl_matrix_int * M);
extern gsl_vector_int_view gsl_matrix_int_row (gsl_matrix_int * M, size_t I);
extern gsl_vector_int_view gsl_matrix_int_column (gsl_matrix_int * M, size_t J);
extern gsl_vector_int_view gsl_matrix_int_diagonal (gsl_matrix_int * M);
extern gsl_vector_int_view gsl_matrix_int_subdiagonal (gsl_matrix_int * M, size_t K);
extern gsl_vector_int_view gsl_matrix_int_superdiagonal (gsl_matrix_int * M, size_t K);
extern int gsl_matrix_int_memcpy (gsl_matrix_int * DEST, const gsl_matrix_int * SRC);
extern int gsl_matrix_int_swap (gsl_matrix_int * M1, gsl_matrix_int * M2);
extern int gsl_matrix_int_get_row (gsl_vector_int * V, const gsl_matrix_int * M, size_t I);
extern int gsl_matrix_int_get_col (gsl_vector_int * V, const gsl_matrix_int * M, size_t J);
extern int gsl_matrix_int_set_row (gsl_matrix_int * M, size_t I, const gsl_vector_int * V);
extern int gsl_matrix_int_set_col (gsl_matrix_int * M, size_t J, const gsl_vector_int * V);
extern int gsl_matrix_int_swap_rows (gsl_matrix_int * M, size_t I, size_t J);
extern int gsl_matrix_int_swap_columns (gsl_matrix_int * M, size_t I, size_t J);
extern int gsl_matrix_int_transpose_memcpy (gsl_matrix_int * DEST,
	                                const gsl_matrix_int * SRC);
extern int gsl_matrix_int_transpose (gsl_matrix_int * M);
extern int gsl_matrix_int_add (gsl_matrix_int * A, const gsl_matrix_int * B);
extern int gsl_matrix_int_sub (gsl_matrix_int * A, const gsl_matrix_int * B);
extern int gsl_matrix_int_mul_elements (gsl_matrix_int * A, const gsl_matrix_int * B);
extern int gsl_matrix_int_div_elements (gsl_matrix_int * A, const gsl_matrix_int * B);
extern int gsl_matrix_int_scale (gsl_matrix_int * A, const double X);
extern int gsl_matrix_int_add_constant (gsl_matrix_int * A, const double X);
extern int gsl_matrix_int_max (const gsl_matrix_int * M);

size_t gsl_matrix_rows (gsl_matrix * M);
size_t gsl_matrix_cols (gsl_matrix * M);
extern gsl_matrix * gsl_matrix_alloc (size_t N1, size_t N2);
extern gsl_matrix * gsl_matrix_calloc (size_t N1, size_t N2);
extern void gsl_matrix_free (gsl_matrix * M);
extern double * gsl_matrix_ptr(gsl_matrix * m, const size_t i, const size_t j);
extern double gsl_matrix_get (const gsl_matrix * M, size_t I, size_t J);
extern void gsl_matrix_set (gsl_matrix * M, size_t I, size_t J, double X);
extern void gsl_matrix_set_all (gsl_matrix * M, double X);
extern void gsl_matrix_set_zero (gsl_matrix * M);
extern void gsl_matrix_set_identity (gsl_matrix * M);
extern gsl_vector_view gsl_matrix_row (gsl_matrix * M, size_t I);
extern gsl_vector_view gsl_matrix_column (gsl_matrix * M, size_t J);
extern gsl_vector_view gsl_matrix_diagonal (gsl_matrix * M);
extern gsl_vector_view gsl_matrix_subdiagonal (gsl_matrix * M, size_t K);
extern gsl_vector_view gsl_matrix_superdiagonal (gsl_matrix * M, size_t K);
extern int gsl_matrix_memcpy (gsl_matrix * DEST, const gsl_matrix * SRC);
extern int gsl_matrix_swap (gsl_matrix * M1, gsl_matrix * M2);
extern int gsl_matrix_get_row (gsl_vector * V, const gsl_matrix * M, size_t I);
extern int gsl_matrix_get_col (gsl_vector * V, const gsl_matrix * M, size_t J);
extern int gsl_matrix_set_row (gsl_matrix * M, size_t I, const gsl_vector * V);
extern int gsl_matrix_set_col (gsl_matrix * M, size_t J, const gsl_vector * V);
extern int gsl_matrix_swap_rows (gsl_matrix * M, size_t I, size_t J);
extern int gsl_matrix_swap_columns (gsl_matrix * M, size_t I, size_t J);
extern int gsl_matrix_transpose_memcpy (gsl_matrix * DEST,
	                                const gsl_matrix * SRC);
extern int gsl_matrix_transpose (gsl_matrix * M);
extern int gsl_matrix_add (gsl_matrix * A, const gsl_matrix * B);
extern int gsl_matrix_sub (gsl_matrix * A, const gsl_matrix * B);
extern int gsl_matrix_mul_elements (gsl_matrix * A, const gsl_matrix * B);
extern int gsl_matrix_div_elements (gsl_matrix * A, const gsl_matrix * B);
extern int gsl_matrix_scale (gsl_matrix * A, const double X);
extern int gsl_matrix_add_constant (gsl_matrix * A, const double X);
extern double gsl_matrix_max (const gsl_matrix * M);

size_t gsl_matrix_complex_rows (gsl_matrix_complex * M);
size_t gsl_matrix_complex_cols (gsl_matrix_complex * M);
extern gsl_matrix_complex * gsl_matrix_complex_alloc (size_t N1, size_t N2);
extern gsl_matrix_complex * gsl_matrix_complex_calloc (size_t N1, size_t N2);
extern void gsl_matrix_complex_free (gsl_matrix_complex * M);
extern gsl_complex * gsl_matrix_complex_ptr(gsl_matrix_complex * m, const size_t i, const size_t j);
extern gsl_complex gsl_matrix_complex_get (const gsl_matrix_complex * M, size_t I, size_t J);
extern void gsl_matrix_complex_set (gsl_matrix_complex * M, size_t I, size_t J, gsl_complex X);
void gsl_matrix_complex_set_real_imag (gsl_matrix_complex * M, size_t I, size_t J, double RE, double IM);
extern void gsl_matrix_complex_set_all (gsl_matrix_complex * M, gsl_complex X);
extern void gsl_matrix_complex_set_zero (gsl_matrix_complex * M);
extern int gsl_matrix_complex_memcpy (gsl_matrix_complex * DEST, const gsl_matrix_complex * SRC);

%scheme %{
(use-modules (gsl gsl-math))

(define my-so (dynamic-link "gsl/libguile-gsl-matrix.so"))
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
