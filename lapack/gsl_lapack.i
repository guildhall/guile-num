%module "lapack/lapack"

%header %{
#include <gsl/gsl_complex_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>

#define ROWS(x) (x->size2)
#define COLS(x) (x->size1)

extern void dgesv_(int * N, int * NRHS, double * A, int * LDA,
		   int * IPIV,
		   double * B, int * LDB, int * OUTPUT);
extern void zgesv_(int * N, int * NRHS, gsl_complex * A, int * LDA,
		   int * IPIV,
		   gsl_complex * B, int * LDB, int * OUTPUT);
extern void dgels_(char * TRANS, int * M, int * N, int *  NRHS,
		   double * A, int * LDA,
		   double * B, int * LDB,
		   double * WORK, int * LWORK, int * INFO);
extern void zgels_(char * TRANS, int * M, int * N, int *  NRHS,
		   gsl_complex * A, int * LDA,
		   gsl_complex * B, int * LDB,
		   gsl_complex * WORK, int * LWORK, int * INFO);
%}

%wrapper %{

int gsl_dgesv(int n, int nrhs, double * a, int lda, int * ipiv,
	      double * b, int ldb)
{
	int info;
	int i;

	dgesv_(&n, &nrhs, a, &lda, ipiv, b, &ldb, &info);
	for (i = 0; i < n; i++) {
		ipiv[i]--;
	}
	return info;
}

int gsl_zgesv(int n, int nrhs, gsl_complex * a, int lda, int * ipiv,
	      gsl_complex * b, int ldb)
{
	int info;
	int i;

	zgesv_(&n, &nrhs, a, &lda, ipiv, b, &ldb, &info);
	for (i = 0; i < n; i++) {
		ipiv[i]--;
	}
	return info;
}

int gsl_dgels(int transpose, gsl_matrix * a, gsl_matrix * b)
{
	int m, n;
	int lda, ldb;
	int nrhs;
	int lwork;
	gsl_vector *work;
	int nb = 16;
	int info;
	char trans;

	m = ROWS(a);
	n = COLS(a);
	lda = m;
	ldb = ROWS(b);
	nrhs = COLS(b);
	if (transpose)
		trans = 'T';
	else
		trans = 'N';

	lwork = 1 + m * n + (m * n + nrhs ) * nb;
	work = gsl_vector_alloc(lwork);
	dgels_(&trans, &m, &n, &nrhs,
	       gsl_matrix_ptr(a, 0, 0), &lda,
	       gsl_matrix_ptr(b, 0, 0), &ldb,
	       gsl_vector_ptr(work, 0), &lwork,
	       &info);
	if (transpose)
		ROWS(b) = m;
	else
		ROWS(b) = n;
	/* NOTE: remaining rows contain sum of squares for the solution */
	gsl_vector_free(work);

	return info;
}

int gsl_zgels(int conjugate, gsl_matrix_complex * a, gsl_matrix_complex * b)
{
	int m, n;
	int lda, ldb;
	int nrhs;
	int lwork;
	gsl_vector_complex *work;
	int nb = 4;
	int info;
	char trans;

	m = a->size2;
	n = a->size1;
	lda = m;
	ldb = b->size2;
	nrhs = b->size1;
	if (conjugate)
		trans = 'C';
	else
		trans = 'N';

	lwork = 1 + m * n + (m * n + nrhs ) * nb;
	work = gsl_vector_complex_alloc(lwork);
	zgels_(&trans, &m, &n, &nrhs,
	       gsl_matrix_complex_ptr(a, 0, 0), &lda,
	       gsl_matrix_complex_ptr(b, 0, 0), &ldb,
	       gsl_vector_complex_ptr(work, 0), &lwork,
	       &info);
	gsl_vector_complex_free(work);

	return info;
}
%}

int gsl_dgesv(int n, int nrhs,
	      double * a, int lda,
	      int * ipiv,
	      double * b, int ldb);
int gsl_zgesv(int n, int nrhs,
	      gsl_complex * a, int lda,
	      int * ipiv,
	      gsl_complex * b, int ldb);
int gsl_dgels(int transpose, gsl_matrix * a, gsl_matrix * b);
int gsl_zgels(int conjugate, gsl_matrix_complex * a, gsl_matrix_complex * b);

%scheme %{
;;; (load-extension "libguile-lapack" "SWIG_init")
(define my-so (dynamic-link "lapack/libguile-lapack.so"))
(dynamic-call "SWIG_init" my-so)
%}
