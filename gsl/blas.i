%module "gsl/blas"

%{
#include <gsl/gsl_blas.h>
%}

%include "gsl_blas_inc.i"

%scheme %{
(use-modules (math array-fun)
             (gsl math)
	     (gsl errno)
             (gsl vector)
             (gsl matrix))

(define my-so (dynamic-link "libguile-gsl-blas.la"))
(dynamic-call "scm_init_gsl_blas_module" my-so)

(export blas-dgemv
	blas-zgemv
	blas-dgemm
	blas-zgemm
        ddot
	zdotu
	zdotc)

;;; Matrix - vector multiplication
(define (blas-dgemv trans alpha A x beta y)
  (if (not (real? alpha))
      (error "alpha must be a real number"))
  (if (not (real-matrix? A))
      (error "A must be a real matrix"))
  (if (not (real-vector? x))
      (error "x must be a real vector"))
  (if (not (real? beta))
      (error "beta must be a real number"))
  (if (not (real-vector? y))
      (error "y must be a real vector"))
  (let* ((trans (cond ((eq? trans 'no-transpose) (CblasNoTrans))
		      ((eq? trans 'transpose) (CblasTrans))
		      ((eq? trans 'conjugate) (CblasConjTrans))
		      (else (error "unknown trans parameter"))))
	 (A (matrix->gsl-matrix A))
	 (x (vector->gsl-vector x))
	 (y (vector->gsl-vector y))
	 (errno (gsl-blas-dgemv trans alpha A x beta y))
	 (result (gsl-vector->vector y)))
    (gsl-matrix-free A)
    (gsl-vector-free x)
    (gsl-vector-free y)
    (cond ((= errno 0) result)
	  (else (error (gsl-strerror errno))))))

(define (blas-zgemv trans alpha A x beta y)
  (if (not (complex? alpha))
      (error "alpha must be a complex number"))
  (if (not (complex-matrix? A))
      (error "A must be a complex matrix"))
  (if (not (complex-vector? x))
      (error "x must be a complex vector"))
  (if (not (complex? beta))
      (error "beta must be a complex number"))
  (if (not (complex-vector? y))
      (error "y must be a complex vector"))
  (let* ((trans (cond ((eq? trans 'no-transpose) (CblasNoTrans))
		      ((eq? trans 'transpose) (CblasTrans))
		      ((eq? trans 'conjugate) (CblasConjTrans))
		      (else (error "unknown trans parameter"))))
	 (alpha (complex->gsl-complex alpha))
	 (a (matrix->gsl-matrix-complex a))
	 (x (vector->gsl-vector-complex x))
	 (beta (complex->gsl-complex beta))
	 (y (vector->gsl-vector-complex y))
	 (errno (gsl-blas-zgemv trans alpha a x beta y))
	 (result (gsl-vector-complex->vector y)))
    (gsl-complex-free alpha)
    (gsl-matrix-complex-free a)
    (gsl-vector-complex-free x)
    (gsl-complex-free beta)
    (gsl-vector-complex-free y)
    (cond ((= errno 0) result)
	  (else (error (gsl-strerror errno))))))


;;; Matrix - matrix multiplication
(define (blas-dgemm transA transB alpha A B beta C)
  (if (not (real? alpha))
      (error "alpha must be a real number"))
  (if (not (real-matrix? A))
      (error "A must be a real matrix"))
  (if (not (real-matrix? B))
      (error "B must be a real matrix"))
  (if (not (real? beta))
      (error "beta must be a real number"))
  (if (not (real-matrix? C))
      (error "C must be a real matrix"))
  (let* ((transA (cond ((eq? transA 'no-transpose) (CblasNoTrans))
		      ((eq? transA 'transpose) (CblasTrans))
		      ((eq? transA 'conjugate) (CblasConjTrans))
		      (else (error "unknown transA parameter"))))
	 (transB (cond ((eq? transB 'no-transpose) (CblasNoTrans))
		       ((eq? transB 'transpose) (CblasTrans))
		       ((eq? transB 'conjugate) (CblasConjTrans))
		       (else (error "unknown transB parameter"))))
	(A (matrix->gsl-matrix A))
	(B (matrix->gsl-matrix B))
	(C (matrix->gsl-matrix C))
	(errno (gsl-blas-dgemm transA transB alpha A B beta C))
	(result (gsl-matrix->matrix C)))
    (gsl-matrix-free A)
    (gsl-matrix-free B)
    (gsl-matrix-free C)
    (cond ((= errno 0) result)
	  (else (error (gsl-strerror errno))))))

(define (blas-zgemm transA transB alpha A B beta C)
  (if (not (complex? alpha))
      (error "alpha must be a complex number"))
  (if (not (complex-matrix? A))
      (error "A must be a complex matrix"))
  (if (not (complex-matrix? B))
      (error "B must be a complex matrix"))
  (if (not (complex? beta))
      (error "beta must be a complex number"))
  (if (not (complex-matrix? C))
      (error "C must be a complex matrix"))
  (let* ((transA (cond ((eq? transA 'no-transpose) (CblasNoTrans))
		      ((eq? transA 'transpose) (CblasTrans))
		      ((eq? transA 'conjugate) (CblasConjTrans))
		      (else (error "unknown transA parameter"))))
	 (transB (cond ((eq? transB 'no-transpose) (CblasNoTrans))
		       ((eq? transB 'transpose) (CblasTrans))
		       ((eq? transB 'conjugate) (CblasConjTrans))
		       (else (error "unknown transB parameter"))))
	 (ra (complex->gsl-complex alpha))
	 (rA (matrix->gsl-matrix-complex A))
	 (rB (matrix->gsl-matrix-complex B))
	 (rb (complex->gsl-complex beta))
	 (rC (matrix->gsl-matrix-complex C))
	 (errno (gsl-blas-zgemm transA transB ra rA rB rb rC))
	 (result (gsl-matrix-complex->matrix rC)))
    (gsl-complex-free ra)
    (gsl-matrix-complex-free rA)
    (gsl-matrix-complex-free rB)
    (gsl-complex-free rb)
    (gsl-matrix-complex-free rC)
    (cond ((= errno 0) result)
	  (else (error (gsl-strerror errno))))))

(define (ddot x y)
  "x^T * y where x and y are real vectors."
  (if (not (and (uniform-vector? x)
		(uniform-vector? y)))
      (error "arguments must be uniform vectors"))
  (let ((x (vector->gsl-vector x))
	(y (vector->gsl-vector y)))
    (let ((result (gsl-blas-ddot x y)))
      (gsl-vector-free x)
      (gsl-vector-free y)
      (cadr result))))

(define (zdotu x y)
  "x^T * y where x and y are complex vectors."
  (if (not (and (uniform-vector? x)
		(uniform-vector? y)))
      (error "arguments must be uniform vectors"))
  (let ((x (vector->gsl-vector-complex (ensure-complex-array x)))
	(y (vector->gsl-vector-complex (ensure-complex-array y)))
	(z (gsl-complex-alloc)))
    (let ((errno (gsl-blas-zdotu x y z))
	  (result (gsl-complex->complex z)))
      (gsl-vector-complex-free x)
      (gsl-vector-complex-free y)
      (gsl-vector-complex-free z)
      result)))

(define (zdotc x y)
  "x^H * y where x and y are complex vectors."
  (if (not (and (uniform-vector? x)
		(uniform-vector? y)))
      (error "arguments must be uniform vectors"))
  (let* ((x (vector->gsl-vector-complex (ensure-complex-array x)))
	 (y (vector->gsl-vector-complex (ensure-complex-array y)))
	 (z (gsl-complex-alloc)))
    (let ((errno (gsl-blas-zdotc x y z))
	  (result (gsl-complex->complex z)))
      (gsl-vector-complex-free x)
      (gsl-vector-complex-free y)
      (gsl-vector-complex-free z)
      result)))
%}