%module "gsl/gsl-blas"

%{
#include <gsl/gsl_blas.h>
%}

/* Typedefs */
typedef size_t CBLAS_INDEX_t;
typedef enum CBLAS_ORDER {
	CblasRowMajor=101, CblasColMajor=102
} CBLAS_ORDER_t;
typedef enum CBLAS_TRANSPOSE {
	CblasNoTrans=111, CblasTrans=112, CblasConjTrans=113
} CBLAS_TRANSPOSE_t;
typedef enum CBLAS_UPLO {
	CblasUpper=121, CblasLower=122
} CBLAS_UPLO_t;
typedef enum CBLAS_DIAG {
	CblasNonUnit=131, CblasUnit=132
} CBLAS_DIAG_t;
typedef enum CBLAS_SIDE {
	CblasLeft=141, CblasRight=142
} CBLAS_SIDE_t;

/* Level 1 BLAS */
extern int gsl_blas_ddot (const gsl_vector * X,
			  const gsl_vector * Y,
			  double * OUTPUT);
extern int gsl_blas_zdotu (const gsl_vector_complex * X,
			   const gsl_vector_complex * Y,
			   gsl_complex * OUTPUT);
extern int gsl_blas_zdotc (const gsl_vector_complex * X,
			   const gsl_vector_complex * Y,
			   gsl_complex * OUTPUT);
extern double gsl_blas_dnrm2 (const gsl_vector * X);
extern double gsl_blas_dznrm2 (const gsl_vector_complex * X);
extern double gsl_blas_dasum (const gsl_vector * X);
extern double gsl_blas_dzasum (const gsl_vector_complex * X);
extern CBLAS_INDEX_t gsl_blas_idamax (const gsl_vector * X);
extern CBLAS_INDEX_t gsl_blas_izamax (const gsl_vector_complex * X);
extern int gsl_blas_dswap (gsl_vector * X, gsl_vector * Y);
extern int gsl_blas_zswap (gsl_vector_complex * X, gsl_vector_complex * Y);
extern int gsl_blas_dcopy (const gsl_vector * X, gsl_vector * Y);
extern int gsl_blas_zcopy (const gsl_vector_complex * X,
			   gsl_vector_complex * Y);
extern int gsl_blas_daxpy (double ALPHA, const gsl_vector * X, gsl_vector * Y);
extern int gsl_blas_zaxpy (const gsl_complex ALPHA,
			   const gsl_vector_complex * X,
			   gsl_vector_complex * Y);
extern void gsl_blas_dscal (double ALPHA, gsl_vector * X);
extern void gsl_blas_zscal (const gsl_complex ALPHA, gsl_vector_complex * X);
extern int gsl_blas_drotg (double a[], double b[], double c[], double s[]);
extern int gsl_blas_drot (gsl_vector * X, gsl_vector * Y,
		   const double C, const double S);
extern int gsl_blas_drotmg (double d1[], double d2[], double b1[],
		     double B2, double P[]);
extern int gsl_blas_drotm (gsl_vector * X, gsl_vector * Y,
		    const double P[]);

/* Level 2 BLAS */
extern int gsl_blas_dgemv (CBLAS_TRANSPOSE_t TRANSA, double ALPHA,
			   const gsl_matrix * A, const gsl_vector * X,
			   double BETA, gsl_vector * Y);
extern int gsl_blas_zgemv (CBLAS_TRANSPOSE_t TRANSA, const gsl_complex ALPHA,
			   const gsl_matrix_complex * A,
			   const gsl_vector_complex * X,
			   const gsl_complex BETA,
			   gsl_vector_complex * Y);
extern int gsl_blas_dtrmv (CBLAS_UPLO_t UPLO, CBLAS_TRANSPOSE_t TRANSA,
			   CBLAS_DIAG_t DIAG, const gsl_matrix * A,
			   gsl_vector * X);
extern int gsl_blas_ztrmv (CBLAS_UPLO_t UPLO, CBLAS_TRANSPOSE_t TRANSA,
			   CBLAS_DIAG_t DIAG, const gsl_matrix_complex * A,
			   gsl_vector_complex * X);
extern int gsl_blas_dtrsv (CBLAS_UPLO_t UPLO, CBLAS_TRANSPOSE_t TRANSA,
			   CBLAS_DIAG_t DIAG, const gsl_matrix * A,
			   gsl_vector * X);
extern int gsl_blas_ztrsv (CBLAS_UPLO_t UPLO, CBLAS_TRANSPOSE_t TRANSA,
			   CBLAS_DIAG_t DIAG, const gsl_matrix_complex * A,
			   gsl_vector_complex *X);
extern int gsl_blas_dsymv (CBLAS_UPLO_t UPLO, double ALPHA,
			   const gsl_matrix * A, const gsl_vector * X,
			   double BETA, gsl_vector * Y);
extern int gsl_blas_zhemv (CBLAS_UPLO_t UPLO, const gsl_complex ALPHA,
			   const gsl_matrix_complex * A,
			   const gsl_vector_complex * X,
			   const gsl_complex BETA, gsl_vector_complex * Y);
extern int gsl_blas_dger (double ALPHA, const gsl_vector * X,
			  const gsl_vector * Y, gsl_matrix * A);
extern int gsl_blas_zgeru (const gsl_complex ALPHA,
			   const gsl_vector_complex * X,
			   const gsl_vector_complex * Y,
			   gsl_matrix_complex * A);
extern int gsl_blas_zgerc (const gsl_complex ALPHA,
			   const gsl_vector_complex * X,
			   const gsl_vector_complex * Y,
			   gsl_matrix_complex * A);
extern int gsl_blas_dsyr (CBLAS_UPLO_t UPLO, double ALPHA,
			  const gsl_vector * X,
			  gsl_matrix * A);
extern int gsl_blas_zher (CBLAS_UPLO_t UPLO, double ALPHA,
			  const gsl_vector_complex * X,
			  gsl_matrix_complex * A);
extern int gsl_blas_dsyr2 (CBLAS_UPLO_t UPLO, double ALPHA,
			   const gsl_vector * X,
			   const gsl_vector * Y, gsl_matrix * A);
extern int gsl_blas_zher2 (CBLAS_UPLO_t UPLO, const gsl_complex ALPHA,
			   const gsl_vector_complex * X,
			   const gsl_vector_complex * Y,
			   gsl_matrix_complex * A);

/* Level 3 BLAS */
extern int gsl_blas_dgemm (CBLAS_TRANSPOSE_t TRANSA,
			   CBLAS_TRANSPOSE_t TRANSB,
			   double ALPHA,
			   const gsl_matrix * A,
			   const gsl_matrix * B,
			   double BETA,
			   gsl_matrix * C);
extern int gsl_blas_zgemm (CBLAS_TRANSPOSE_t TRANSA,
			   CBLAS_TRANSPOSE_t TRANSB,
			   const gsl_complex ALPHA,
			   const gsl_matrix_complex * A,
			   const gsl_matrix_complex * B, const
			   gsl_complex BETA,
			   gsl_matrix_complex * C);
extern int gsl_blas_dsymm (CBLAS_SIDE_t SIDE,
			   CBLAS_UPLO_t UPLO,
			   double ALPHA,
			   const gsl_matrix * A,
			   const gsl_matrix * B,
			   double BETA,
			   gsl_matrix * C);
extern int gsl_blas_zsymm (CBLAS_SIDE_t SIDE,
			   CBLAS_UPLO_t UPLO,
			   const gsl_complex ALPHA,
			   const gsl_matrix_complex * A,
			   const gsl_matrix_complex * B,
			   const gsl_complex BETA,
			   gsl_matrix_complex * C);
extern int gsl_blas_zhemm (CBLAS_SIDE_t SIDE,
			   CBLAS_UPLO_t UPLO,
			   const gsl_complex ALPHA,
			   const gsl_matrix_complex * A,
			   const gsl_matrix_complex * B,
			   const gsl_complex BETA,
			   gsl_matrix_complex * C);
extern int gsl_blas_dtrmm (CBLAS_SIDE_t SIDE,
			   CBLAS_UPLO_t UPLO,
			   CBLAS_TRANSPOSE_t TRANSA,
			   CBLAS_DIAG_t DIAG,
			   double ALPHA,
			   const gsl_matrix * A,
			   gsl_matrix * B);
extern int gsl_blas_ztrmm (CBLAS_SIDE_t SIDE,
			   CBLAS_UPLO_t UPLO,
			   CBLAS_TRANSPOSE_t TRANSA,
			   CBLAS_DIAG_t DIAG,
			   const gsl_complex ALPHA,
			   const gsl_matrix_complex * A,
			   gsl_matrix_complex * B);
extern int gsl_blas_dtrsm (CBLAS_SIDE_t SIDE,
			   CBLAS_UPLO_t UPLO,
			   CBLAS_TRANSPOSE_t TRANSA,
			   CBLAS_DIAG_t DIAG,
			   double ALPHA,
			   const gsl_matrix * A,
			   gsl_matrix * B);
extern int gsl_blas_ztrsm (CBLAS_SIDE_t SIDE,
			   CBLAS_UPLO_t UPLO,
			   CBLAS_TRANSPOSE_t TRANSA,
			   CBLAS_DIAG_t DIAG,
			   const gsl_complex ALPHA,
			   const gsl_matrix_complex * A,
			   gsl_matrix_complex * B);
extern int gsl_blas_dsyrk (CBLAS_UPLO_t UPLO,
			   CBLAS_TRANSPOSE_t TRANS,
			   double ALPHA,
			   const gsl_matrix * A,
			   double BETA,
			   gsl_matrix * C);
extern int gsl_blas_zsyrk (CBLAS_UPLO_t UPLO,
			   CBLAS_TRANSPOSE_t TRANS,
			   const gsl_complex ALPHA,
			   const gsl_matrix_complex * A,
			   const gsl_complex BETA,
			   gsl_matrix_complex * C);
extern int gsl_blas_zherk (CBLAS_UPLO_t UPLO,
			   CBLAS_TRANSPOSE_t TRANS,
			   double ALPHA,
			   const gsl_matrix_complex * A,
			   double BETA,
			   gsl_matrix_complex * C);
extern int gsl_blas_dsyr2k (CBLAS_UPLO_t UPLO,
			    CBLAS_TRANSPOSE_t TRANS,
			    double ALPHA,
			    const gsl_matrix * A,
			    const gsl_matrix * B,
			    double BETA,
			    gsl_matrix * C);
extern int gsl_blas_zsyr2k (CBLAS_UPLO_t UPLO,
			    CBLAS_TRANSPOSE_t TRANS,
			    const gsl_complex ALPHA,
			    const gsl_matrix_complex * A,
			    const gsl_matrix_complex * B,
			    const gsl_complex BETA,
			    gsl_matrix_complex *C);
extern int gsl_blas_zher2k (CBLAS_UPLO_t UPLO,
			    CBLAS_TRANSPOSE_t TRANS,
			    const gsl_complex ALPHA,
			    const gsl_matrix_complex * A,
			    const gsl_matrix_complex * B,
			    double BETA,
			    gsl_matrix_complex * C);

%scheme %{
(use-modules (math array-fun)
             (gsl gsl-math)
             (gsl gsl-vector)
             (gsl gsl-matrix))

(define my-so (dynamic-link "gsl/libguile-gsl-blas.la"))
(dynamic-call "SWIG_init" my-so)

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
	 (a (complex->gsl-complex alpha))
	 (A (matrix->gsl-matrix-complex A))
	 (B (matrix->gsl-matrix-complex B))
	 (b (complex->gsl-complex beta))
	 (C (matrix->gsl-matrix-complex C))
	 (errno (gsl-blas-zgemm transA transB a A B b C))
	 (result (gsl-matrix-complex->matrix C)))
    (gsl-complex-free a)
    (gsl-matrix-complex-free A)
    (gsl-matrix-complex-free B)
    (gsl-complex-free b)
    (gsl-matrix-complex-free C)
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
