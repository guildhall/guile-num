/* Guile interface to LAPACK
   Copyright (C) 2002   Arno W. Peters <a.w.peters@ieee.org>
  
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or (at
   your option) any later version.
  
   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.
  
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA
*/

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
(use-modules (math array-fun)
             (gsl gsl-math)
             (gsl gsl-vector)
	     (gsl gsl-matrix))

(define my-so (dynamic-link "lapack/libguile-lapack.so"))
(dynamic-call "SWIG_init" my-so)

(export dgesv
	zgesv
	dgels
	zgels)

(define (lapack-permutation-vector->matrix v)
  (let* ((len (uniform-vector-length v))
	 (P   (gsl-matrix-int-alloc len len)))
    (gsl-matrix-int-set-identity P)
    (do ((i 0 (+ i 1)))
	((= i len) (gsl-matrix-int->matrix P))
      (gsl-matrix-int-swap-columns P i (uniform-vector-ref v i)))))


(define (dgesv A B)
  (let* ((n (array-rows A))
	 (nrhs (array-columns B))
	 (lda n)
	 (ldb n)
	 (IPIV (make-uniform-array 1 n))
	 (a (matrix->gsl-matrix (transpose-array A 1 0)))
	 (b (matrix->gsl-matrix (transpose-array B 1 0)))
	 (ipiv (vector->gsl-vector-int IPIV))
	 (errno (gsl-dgesv n nrhs
			   (gsl-matrix-ptr a 0 0) lda
			   (gsl-vector-int-data ipiv)
			   (gsl-matrix-ptr b 0 0) ldb)))
    (let ((P (lapack-permutation-vector->matrix (gsl-vector-int->vector ipiv)))
	  (LU (transpose-array (gsl-matrix->matrix a) 1 0))
	  (X (transpose-array (gsl-matrix->matrix b) 1 0)))
      (gsl-matrix-free a)
      (gsl-matrix-free b)
      (gsl-vector-int-free ipiv)
      (cond ((= errno 0) (values X LU P))
	    ((< errno 0) (error "one of the arguments had an illegal value"))
	    (else
	     (error "factor U is singular, no solution can be computed"))))))

(define (zgesv A B)
  (let* ((n (array-rows A))
	 (nrhs (array-columns B))
	 (lda n)
	 (ldb n)
	 (IPIV (make-uniform-array 1 n))
	 (a (matrix->gsl-matrix-complex (transpose-array A 1 0)))
	 (b (matrix->gsl-matrix-complex (transpose-array B 1 0)))
	 (ipiv (vector->gsl-vector-int IPIV))
	 (errno (gsl-zgesv n nrhs
			   (gsl-matrix-complex-ptr a 0 0) lda
			   (gsl-vector-int-data ipiv)
			   (gsl-matrix-complex-ptr b 0 0) ldb)))
    (let ((P (lapack-permutation-vector->matrix (gsl-vector-int->vector ipiv)))
	  (LU (transpose-array (gsl-matrix-complex->matrix a) 1 0))
	  (X (transpose-array (gsl-matrix-complex->matrix b) 1 0)))
      (gsl-matrix-complex-free a)
      (gsl-matrix-complex-free b)
      (gsl-vector-int-free ipiv)
      (cond ((= errno 0) (values X LU P))
	    ((< errno 0) (error "one of the arguments had an illegal value"))
	    (else
	     (error "factor U is singular, no solution can be computed"))))))

(define (dgels trans A B)
  (let* ((a (matrix->gsl-matrix (transpose-array A 1 0)))
	 (b (matrix->gsl-matrix (transpose-array B 1 0)))
	 (errno (gsl-dgels trans a b)))
    (let ((LQ (transpose-array (gsl-matrix->matrix a) 1 0))
	  (X  (transpose-array (gsl-matrix->matrix b) 1 0)))
      (gsl-matrix-free a)
      (gsl-matrix-free b)
      (cond ((= errno 0) (values X LQ))
	    (else (error "one of the arguments had an illegal value"))))))

(define (zgels trans A B)
  (let* ((a (matrix->gsl-matrix-complex (transpose-array A 1 0)))
	 (b (matrix->gsl-matrix-complex (transpose-array B 1 0)))
	 (errno (gsl-zgels trans a b)))
    (let ((LQ (transpose-array (gsl-matrix-complex->matrix a) 1 0))
	  (X  (transpose-array (gsl-matrix-complex->matrix b) 1 0)))
      (gsl-matrix-complex-free a)
      (gsl-matrix-complex-free b)
      (cond ((= errno 0) (values X LQ))
	    (else (error "one of the arguments had an illegal value"))))))

%}
