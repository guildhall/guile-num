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
